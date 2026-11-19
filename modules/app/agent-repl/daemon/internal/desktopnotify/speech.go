package desktopnotify

import (
	"context"
	"encoding/json"
	"fmt"
	"strings"
	"sync"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/standdown"
	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

// opSpeak is the operation a banner's reading aloud carries.
const opSpeak = "daemon.desktopnotify.speak"

// EnvSayCmd overrides the speech program's binary, exactly as EnvChimeCmd
// does the chime's: the platform's argv is unchanged, so a test points it at
// a recorder and no test ever speaks on the host.
const EnvSayCmd = "AGENT_REPL_SAY_CMD"

// EnvAudioOutputCmd overrides the program the current audio output is read
// with. Its argv is unchanged, so a test points it at a stand-in that prints
// a scripted device and no test ever reads the host's audio.
const EnvAudioOutputCmd = "AGENT_REPL_AUDIO_OUTPUT_CMD"

// audioOutputBound bounds one read of the current audio output. The read
// (system_profiler SPAudioDataType -json) took 70-80ms over five runs at a
// load average of 7 (measured 2026-11-18), so three times the slowest.
const audioOutputBound = 240 * time.Millisecond

// The speech program's measured cost, which sayBound is sized from: `say`
// took 1.02-1.04s to start and render a one-word line, and read a 28-word
// banner in 10.4s, 0.37s a word (measured 2026-11-18 at a load average of 7).
const (
	sayStartup = 1050 * time.Millisecond
	sayPerWord = 370 * time.Millisecond
)

// sayBound bounds one reading of text aloud: three times its expected time,
// the program's start plus its words at the measured rate. A run past it is a
// hung program.
func sayBound(text string) time.Duration {
	words := len(strings.Fields(text))
	return 3 * (sayStartup + time.Duration(words)*sayPerWord)
}

// Transport is how an audio device is attached, as CoreAudio names it.
type Transport string

// The transports CoreAudio reports, as system_profiler spells them.
const (
	TransportBluetooth Transport = "coreaudio_device_type_bluetooth"
	TransportBuiltIn   Transport = "coreaudio_device_type_builtin"
	TransportUSB       Transport = "coreaudio_device_type_usb"
)

// AudioOutput is the device the host plays its sound through now: CoreAudio's
// default output device.
type AudioOutput struct {
	Name      string
	Transport Transport
}

// ListeningDevice reports whether out is a device the user listens on away
// from the screen, the headphone half of the condition a banner is read aloud
// on. THIS IS THE ONE PREDICATE: today it is AirPods only (a Bluetooth device
// named for them), and widening it to other headphones changes this function
// alone.
func ListeningDevice(out AudioOutput) bool {
	return out.Transport == TransportBluetooth && strings.Contains(strings.ToLower(out.Name), "airpods")
}

// WifiStanding is the daemon's own persistent-wifi standing, as its
// controller last published it (persistentwifi.Controller.Topic). Speech
// reads the standing, never re-derives it.
type WifiStanding interface {
	// Latest answers the last standing published, and false before the first.
	Latest() (*agentreplv1.PersistentWifiState, bool)
}

// SpeechPlatform names a platform's speech and audio-output programs. A
// platform with no Say speaks nothing.
type SpeechPlatform struct {
	// Say is the speech program's binary; its argv is `-- TEXT`.
	Say string
	// AudioOutput is the program the current audio output is read with, and
	// AudioOutputArgs its argv.
	AudioOutput     string
	AudioOutputArgs []string
}

// supported reports whether the platform can speak at all.
func (p SpeechPlatform) supported() bool { return p.Say != "" }

// SpeechPlatformFor answers the speech programs for a GOOS: `say` and
// system_profiler's CoreAudio report on macOS, the system's own binaries.
// Any other platform speaks nothing, which is no error: its banners are drawn
// as ever.
func SpeechPlatformFor(goos string) SpeechPlatform {
	if goos != "darwin" {
		return SpeechPlatform{}
	}
	return SpeechPlatform{
		Say:             "/usr/bin/say",
		AudioOutput:     "/usr/sbin/system_profiler",
		AudioOutputArgs: []string{"SPAudioDataType", "-json"},
	}
}

// WithOverrides answers p with each program an override names put in its
// place: say for the speech program, audioOutput for the audio read.
func (p SpeechPlatform) WithOverrides(say, audioOutput string) SpeechPlatform {
	if !p.supported() {
		return p
	}
	if say != "" {
		p.Say = say
	}
	if audioOutput != "" {
		p.AudioOutput = audioOutput
	}
	return p
}

// Speech reads a made banner aloud IF AND ONLY IF BOTH hold: the daemon's
// persistent wifi mode is on, and the current audio output is a listening
// device (ListeningDevice). Otherwise it says nothing, and the banner is
// drawn as ever. Every run goes through the Runner the banner backend uses.
type Speech struct {
	goos     string
	platform SpeechPlatform
	runner   Runner
	standing WifiStanding

	// unsupported records the platform speaking nothing once.
	unsupported sync.Once
}

// NewSpeech builds the speech for a GOOS's platform. It panics on a missing
// collaborator, because a half-wired speech is a boot defect.
func NewSpeech(goos string, platform SpeechPlatform, runner Runner, standing WifiStanding) *Speech {
	if runner == nil || standing == nil {
		panic("desktopnotify: NewSpeech requires a Runner and a WifiStanding")
	}
	return &Speech{goos: goos, platform: platform, runner: runner, standing: standing}
}

// Silence is why a banner was not read aloud.
type Silence string

// The reasons a banner is not read aloud, each the condition that decided it.
const (
	SilenceUnsupported       Silence = "speech_unsupported"
	SilenceWifiUnread        Silence = "persistent_wifi_unread"
	SilenceWifiOff           Silence = "persistent_wifi_off"
	SilenceOutputUnreadable  Silence = "audio_output_unreadable"
	SilenceNoListeningDevice Silence = "no_listening_device"
)

// decide answers whether banner is read aloud, and the silence that decided
// it not to be, recording the decision through log. Persistent wifi is read
// first: it is the daemon's own standing and runs no program.
func (s *Speech) decide(ctx context.Context, log dlog.Logger) (bool, Silence) {
	if !s.platform.supported() {
		s.unsupported.Do(func() {
			log.Info(opSpeak, "reading banners aloud is unsupported on this platform; no banner is ever read aloud", dlog.Context{
				"goos": s.goos, "decided_by": string(SilenceUnsupported),
			})
		})
		return false, SilenceUnsupported
	}
	state, ok := s.standing.Latest()
	if !ok {
		log.Info(opSpeak, "the persistent-wifi standing is not yet read; the banner is not read aloud", dlog.Context{
			"decided_by": string(SilenceWifiUnread),
		})
		return false, SilenceWifiUnread
	}
	if state.GetOn() == nil {
		log.Info(opSpeak, "persistent wifi mode is off; the banner is not read aloud", dlog.Context{
			"decided_by": string(SilenceWifiOff),
		})
		return false, SilenceWifiOff
	}
	out, err := s.readOutput(ctx)
	if err != nil {
		log.Error(opSpeak, "the current audio output could not be read; the banner is not read aloud", dlog.Context{
			"program": s.platform.AudioOutput, "bound_ms": audioOutputBound.Milliseconds(),
			"cause": err.Error(), "decided_by": string(SilenceOutputUnreadable),
		})
		return false, SilenceOutputUnreadable
	}
	if !ListeningDevice(out) {
		log.Info(opSpeak, "the current audio output is no listening device; the banner is not read aloud", dlog.Context{
			"device": out.Name, "transport": string(out.Transport), "decided_by": string(SilenceNoListeningDevice),
		})
		return false, SilenceNoListeningDevice
	}
	log.Info(opSpeak, "persistent wifi mode is on and a listening device is the audio output; reading the banner aloud", dlog.Context{
		"device": out.Name, "transport": string(out.Transport), "decided_by": "persistent_wifi_on_and_listening_device",
	})
	return true, ""
}

// Speak reads banner aloud when decide says so, and returns once it is read,
// its program failed, or its bound passed. Every outcome is recorded
// through log. The notifier calls it on a goroutine of its own, only once the
// banner is made, so speech never holds or delays a banner.
func (s *Speech) Speak(ctx context.Context, banner Banner, log dlog.Logger) {
	speak, _ := s.decide(ctx, log)
	if !speak {
		return
	}
	text := SpokenText(banner)
	bound := sayBound(text)
	_, err := runWithin(ctx, bound, s.runner, s.platform.Say, []string{"--", text}, nil)
	switch {
	case standdown.CutShort(ctx, err):
		log.Info(opSpeak, "the daemon stood down while the banner was read aloud", dlog.Context{"cause": err.Error()})
	case err != nil:
		log.Error(opSpeak, "the speech program failed; the banner was drawn but not read aloud in full", dlog.Context{
			"program": s.platform.Say, "bound_ms": bound.Milliseconds(), "cause": err.Error(),
		})
	default:
		log.Debug(opSpeak, "the banner was read aloud", nil)
	}
}

// SpokenText is what a banner reads aloud: its title and its body, as drawn,
// each on its own line, with any italic letters read as the plain ones.
func SpokenText(banner Banner) string {
	lines := make([]string, 0, 2)
	for _, line := range []string{banner.Title, banner.Body} {
		if line != "" {
			lines = append(lines, Plain(line))
		}
	}
	return strings.Join(lines, "\n")
}

// readOutput reads CoreAudio's default output device through the platform's
// audio-output program, bounded.
func (s *Speech) readOutput(ctx context.Context) (AudioOutput, error) {
	stdout, err := runWithin(ctx, audioOutputBound, s.runner, s.platform.AudioOutput, s.platform.AudioOutputArgs, nil)
	if err != nil {
		return AudioOutput{}, err
	}
	return parseAudioOutput(stdout)
}

// spAudioReport is the part of system_profiler's SPAudioDataType report a
// default output device is read from.
type spAudioReport struct {
	SPAudioDataType []struct {
		Items []struct {
			Name          string `json:"_name"`
			DefaultOutput string `json:"coreaudio_default_audio_output_device"`
			Transport     string `json:"coreaudio_device_transport"`
		} `json:"_items"`
	} `json:"SPAudioDataType"`
}

// spaudioYes is how the report marks the default output device.
const spaudioYes = "spaudio_yes"

// parseAudioOutput reads the one default output device from the report. A
// report that is not JSON, names no default output, or names more than one is
// an error: the output is never guessed.
func parseAudioOutput(stdout string) (AudioOutput, error) {
	var report spAudioReport
	if err := json.Unmarshal([]byte(stdout), &report); err != nil {
		return AudioOutput{}, fmt.Errorf("decode the audio report: %w", err)
	}
	var found []AudioOutput
	for _, group := range report.SPAudioDataType {
		for _, item := range group.Items {
			if item.DefaultOutput == spaudioYes {
				found = append(found, AudioOutput{Name: item.Name, Transport: Transport(item.Transport)})
			}
		}
	}
	switch len(found) {
	case 0:
		return AudioOutput{}, fmt.Errorf("the audio report names no default output device")
	case 1:
		return found[0], nil
	default:
		return AudioOutput{}, fmt.Errorf("the audio report names %d default output devices: %+v", len(found), found)
	}
}
