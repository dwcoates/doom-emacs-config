package desktopnotify

import (
	"context"
	"encoding/json"
	"fmt"
	"strings"
	"sync"
	"time"

	"agent-repld/internal/audiooutput"
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

// EnvBluetoothCmd overrides the program a Bluetooth output's device class is
// read with, exactly as EnvAudioOutputCmd does the audio output's.
const EnvBluetoothCmd = "AGENT_REPL_BLUETOOTH_CMD"

// audioOutputBound bounds one read of the current audio output. The read
// (`agent-repld audio-output`) took 40-80ms over five runs at a load average
// of 8.4 (measured 2026-11-18), so three times the slowest.
const audioOutputBound = 240 * time.Millisecond

// bluetoothBound bounds one read of the Bluetooth devices' classes. The read
// (system_profiler SPBluetoothDataType -json) took 60-80ms over three runs at
// a load average of 8 (measured 2026-11-18), so three times the slowest.
const bluetoothBound = 240 * time.Millisecond

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

// The Bluetooth device classes system_profiler reports (device_minorType)
// that are worn: the Class of Device's audio minor classes for headphones and
// for a wearable headset.
const (
	BluetoothHeadphones = "Headphones"
	BluetoothHeadset    = "Headset"
)

// AudioOutput is the device the host plays its sound through now: CoreAudio's
// default output device and, for a Bluetooth one, its device class.
type AudioOutput struct {
	audiooutput.Device
	// BluetoothClass is a Bluetooth output's device class as system_profiler
	// reports it (device_minorType), empty for any other output and for a
	// Bluetooth output whose class could not be found.
	BluetoothClass string
}

// PrivateListeningDevice reports whether out is a device only its wearer
// hears: the headphone half of the condition a banner is read aloud on. THIS
// IS THE ONE PREDICATE, and widening or narrowing what counts changes this
// function alone.
//
// Owner ruling, 2026-11-18: "Just airpods, unless we can programmatically
// identify some or all headphone types. We just don't want it to be speakers
// that play for everyone to hear." So only a kind macOS positively
// identifies as worn counts:
//   - the built-in output with its headphone jack as its data source, never
//     its internal speakers;
//   - a Bluetooth output whose device class is headphones or a headset, which
//     is how AirPods and Apple's other earbuds and headphones report.
//
// Everything else is a speaker and is silent: another Bluetooth class
// (loudspeaker, portable audio, hands-free), a Bluetooth output whose class
// is unknown, and every USB, HDMI, DisplayPort, AirPlay or other output. A
// kind that cannot be positively identified is never guessed to be worn.
func PrivateListeningDevice(out AudioOutput) bool {
	switch out.Transport {
	case audiooutput.TransportBuiltIn:
		return out.DataSource == audiooutput.DataSourceHeadphones
	case audiooutput.TransportBluetooth, audiooutput.TransportBluetoothLE:
		return out.BluetoothClass == BluetoothHeadphones || out.BluetoothClass == BluetoothHeadset
	default:
		return false
	}
}

// WifiStanding is the daemon's own persistent-wifi standing, as its
// controller last published it (persistentwifi.Controller.Topic). Speech
// reads the standing, never re-derives it.
type WifiStanding interface {
	// Latest answers the last standing published, and false before the first.
	Latest() (*agentreplv1.PersistentWifiState, bool)
}

// SpeechPlatform names a platform's speech programs. A platform with no Say
// speaks nothing.
type SpeechPlatform struct {
	// Say is the speech program's binary; its argv is `-- TEXT`.
	Say string
	// AudioOutput is the program the current audio output is read with, and
	// AudioOutputArgs its argv: it prints one audiooutput.Device.
	AudioOutput     string
	AudioOutputArgs []string
	// Bluetooth is the program the Bluetooth devices' classes are read with,
	// and BluetoothArgs its argv.
	Bluetooth     string
	BluetoothArgs []string
}

// supported reports whether the platform can speak at all.
func (p SpeechPlatform) supported() bool { return p.Say != "" }

// SpeechPlatformFor answers the speech programs for a GOOS: on macOS `say`,
// the daemon's own `audio-output` verb (selfExe) for CoreAudio's default
// output, and system_profiler's Bluetooth report for a Bluetooth output's
// class. Any other platform speaks nothing, which is no error: its banners
// are drawn as ever.
//
// THE BLUETOOTH CLASS IS READ FROM system_profiler, NOT IOBluetooth: a
// command-line process that asks IOBluetooth for a device's class is killed
// by macOS's privacy check for want of a Bluetooth usage description
// (measured 2026-11-18, exit 134), while system_profiler reports the same
// class for every connected device.
func SpeechPlatformFor(goos, selfExe string) SpeechPlatform {
	if goos != "darwin" {
		return SpeechPlatform{}
	}
	return SpeechPlatform{
		Say:             "/usr/bin/say",
		AudioOutput:     selfExe,
		AudioOutputArgs: []string{audiooutput.Verb},
		Bluetooth:       "/usr/sbin/system_profiler",
		BluetoothArgs:   []string{"SPBluetoothDataType", "-json"},
	}
}

// WithOverrides answers p with each program an override names put in its
// place: say for the speech program, audioOutput and bluetooth for the reads.
func (p SpeechPlatform) WithOverrides(say, audioOutput, bluetooth string) SpeechPlatform {
	if !p.supported() {
		return p
	}
	for _, o := range []struct {
		field *string
		value string
	}{{&p.Say, say}, {&p.AudioOutput, audioOutput}, {&p.Bluetooth, bluetooth}} {
		if o.value != "" {
			*o.field = o.value
		}
	}
	return p
}

// Speech reads a made banner aloud IF AND ONLY IF ALL THREE hold: the
// daemon's persistent wifi mode is on, the lid is heard closed, and the
// current audio output is a private listening device
// (PrivateListeningDevice). Otherwise it says nothing, and the banner is
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
	SilenceUnsupported      Silence = "speech_unsupported"
	SilenceWifiUnread       Silence = "persistent_wifi_unread"
	SilenceWifiOff          Silence = "persistent_wifi_off"
	SilenceLidNotClosed     Silence = "lid_not_closed"
	SilenceOutputUnreadable Silence = "audio_output_unreadable"
	SilenceNoPrivateDevice  Silence = "no_private_listening_device"
)

// decide answers whether banner is read aloud, and the silence that decided
// it not to be, recording the decision through log. Persistent wifi and the
// lid are read first: both are the daemon's own standing and run no program.
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
	if state.GetLidClosed() == nil {
		// Open, a machine with no lid, a lid not yet heard, and a lid the
		// daemon cannot hear are all not heard closed.
		log.Info(opSpeak, "the lid is not heard closed; the banner is not read aloud", dlog.Context{
			"lid": lidName(state), "decided_by": string(SilenceLidNotClosed),
		})
		return false, SilenceLidNotClosed
	}
	out, err := s.readOutput(ctx)
	if err != nil {
		log.Error(opSpeak, "the current audio output could not be read; the banner is not read aloud", dlog.Context{
			"cause": err.Error(), "decided_by": string(SilenceOutputUnreadable),
		})
		return false, SilenceOutputUnreadable
	}
	fields := dlog.Context{
		"device": out.Name, "transport": out.Transport, "data_source": out.DataSource, "bluetooth_class": out.BluetoothClass,
	}
	if !PrivateListeningDevice(out) {
		fields["decided_by"] = string(SilenceNoPrivateDevice)
		log.Info(opSpeak, "the current audio output is no private listening device; the banner is not read aloud", fields)
		return false, SilenceNoPrivateDevice
	}
	fields["decided_by"] = "persistent_wifi_on_lid_closed_and_private_listening_device"
	log.Info(opSpeak, "persistent wifi mode is on, the lid is closed and a private listening device is the audio output; reading the banner aloud", fields)
	return true, ""
}

// lidName names the standing's lid arm for the decision record, "unheard"
// while none is assigned.
func lidName(state *agentreplv1.PersistentWifiState) string {
	switch state.GetLid().(type) {
	case *agentreplv1.PersistentWifiState_LidOpen:
		return "open"
	case *agentreplv1.PersistentWifiState_LidClosed:
		return "closed"
	case *agentreplv1.PersistentWifiState_NoLid:
		return "no_lid"
	case *agentreplv1.PersistentWifiState_LidUnwatched:
		return "unwatched"
	case nil:
		return "unheard"
	default:
		panic(fmt.Sprintf("desktopnotify: unknown lid arm %T", state.GetLid()))
	}
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

// readOutput reads CoreAudio's default output device and, for a Bluetooth
// one, its device class, each through its program and under its bound. The
// error names the program and its bound.
func (s *Speech) readOutput(ctx context.Context) (AudioOutput, error) {
	stdout, err := runWithin(ctx, audioOutputBound, s.runner, s.platform.AudioOutput, s.platform.AudioOutputArgs, nil)
	if err != nil {
		return AudioOutput{}, fmt.Errorf("%s (bound %v): %w", s.platform.AudioOutput, audioOutputBound, err)
	}
	device, err := audiooutput.Decode(stdout)
	if err != nil {
		return AudioOutput{}, err
	}
	out := AudioOutput{Device: device}
	if device.Transport != audiooutput.TransportBluetooth && device.Transport != audiooutput.TransportBluetoothLE {
		return out, nil
	}
	address, ok := audiooutput.BluetoothAddress(device.UID)
	if !ok {
		// A Bluetooth output whose UID names no address has no class to
		// find: it is unidentified, and so a speaker.
		return out, nil
	}
	report, err := runWithin(ctx, bluetoothBound, s.runner, s.platform.Bluetooth, s.platform.BluetoothArgs, nil)
	if err != nil {
		return AudioOutput{}, fmt.Errorf("%s (bound %v): %w", s.platform.Bluetooth, bluetoothBound, err)
	}
	out.BluetoothClass, err = bluetoothClass(report, address)
	if err != nil {
		return AudioOutput{}, err
	}
	return out, nil
}

// spBluetoothReport is the part of system_profiler's SPBluetoothDataType
// report a connected device's class is read from: each connected device is
// one object keyed by its name.
type spBluetoothReport struct {
	SPBluetoothDataType []struct {
		Connected []map[string]struct {
			Address   string `json:"device_address"`
			MinorType string `json:"device_minorType"`
		} `json:"device_connected"`
	} `json:"SPBluetoothDataType"`
}

// bluetoothClass answers the class of the connected device at address, and
// "" when no connected device is there. A report that is not JSON is an
// error.
func bluetoothClass(stdout, address string) (string, error) {
	var report spBluetoothReport
	if err := json.Unmarshal([]byte(stdout), &report); err != nil {
		return "", fmt.Errorf("decode the Bluetooth report: %w", err)
	}
	for _, controller := range report.SPBluetoothDataType {
		for _, named := range controller.Connected {
			for _, device := range named {
				if strings.EqualFold(device.Address, address) {
					return device.MinorType, nil
				}
			}
		}
	}
	return "", nil
}
