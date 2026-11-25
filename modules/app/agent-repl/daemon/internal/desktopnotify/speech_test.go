package desktopnotify

import (
	"context"
	"encoding/binary"
	"errors"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"sync"
	"testing"
	"time"

	"agent-repld/internal/audiooutput"
	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

// fakeStanding answers one scripted persistent-wifi standing.
type fakeStanding struct {
	state *agentreplv1.PersistentWifiState
	ok    bool
}

func (s fakeStanding) Latest() (*agentreplv1.PersistentWifiState, bool) { return s.state, s.ok }

// modeOn answers a standing with persistent wifi mode on and its lid as
// setLid leaves it, unassigned (not yet heard) when setLid sets nothing.
func modeOn(setLid func(*agentreplv1.PersistentWifiState)) fakeStanding {
	state := &agentreplv1.PersistentWifiState{Mode: &agentreplv1.PersistentWifiState_On{On: &agentreplv1.PersistentWifiModeOn{}}}
	setLid(state)
	return fakeStanding{state: state, ok: true}
}

// wifiOn is the standing a banner is read aloud under (the mode on and the
// lid closed), and wifiOff the mode off with the lid closed.
var (
	wifiOn = modeOn(func(s *agentreplv1.PersistentWifiState) {
		s.Lid = &agentreplv1.PersistentWifiState_LidClosed{LidClosed: &agentreplv1.PersistentWifiLidClosed{}}
	})
	wifiOff = fakeStanding{state: &agentreplv1.PersistentWifiState{
		Mode: &agentreplv1.PersistentWifiState_Off{Off: &agentreplv1.PersistentWifiModeOff{}},
		Lid:  &agentreplv1.PersistentWifiState_LidClosed{LidClosed: &agentreplv1.PersistentWifiLidClosed{}},
	}, ok: true}
)

// speechRun is one program run the speech runner saw.
type speechRun struct {
	bin      string
	args     []string
	deadline time.Time
}

// speechRunner answers the audio read and the speech program by binary, and
// records every run, so no test runs `say` or reads the host's audio.
type speechRunner struct {
	mu      sync.Mutex
	runs    []speechRun
	answers map[string]speechAnswer
}

type speechAnswer struct {
	stdout string
	err    error
	// renders, when set, is the audio a render writes to its `-o` file.
	renders []byte
	// hang runs until the run's bound ends it, then answers what a killed
	// program does.
	hang bool
	// onRun, when set, is called as the program runs (the daemon standing
	// down under it).
	onRun func()
}

// errKilled is what a killed program answers.
var errKilled = errors.New("signal: killed")

func (r *speechRunner) Run(ctx context.Context, bin string, args []string, _ func()) (string, error) {
	deadline, _ := ctx.Deadline()
	r.mu.Lock()
	r.runs = append(r.runs, speechRun{bin: bin, args: args, deadline: deadline})
	answer, ok := r.answers[bin]
	r.mu.Unlock()
	if !ok {
		panic("speechRunner: no answer scripted for " + bin)
	}
	if answer.renders != nil {
		if err := os.WriteFile(args[1], answer.renders, 0o600); err != nil {
			panic(err)
		}
	}
	if answer.onRun != nil {
		answer.onRun()
	}
	if answer.hang {
		<-ctx.Done()
		return "", errKilled
	}
	return answer.stdout, answer.err
}

// renderedPath answers the file the reading was rendered to, "" when no
// render ran.
func (r *speechRunner) renderedPath() string {
	for _, run := range r.ran() {
		if run.bin == darwinSpeech.Say {
			return run.args[1]
		}
	}
	return ""
}

// played answers every run of the player.
func (r *speechRunner) played() []speechRun {
	var out []speechRun
	for _, run := range r.ran() {
		if run.bin == darwinSpeech.Player {
			out = append(out, run)
		}
	}
	return out
}

// rate22050 and rate44100 are 80-bit extended sample rates as an AIFF COMM
// chunk holds them, the first read from a file `say -o` rendered.
var (
	rate22050 = []byte{0x40, 0x0d, 0xac, 0x44, 0, 0, 0, 0, 0, 0}
	rate44100 = []byte{0x40, 0x0e, 0xac, 0x44, 0, 0, 0, 0, 0, 0}
)

// aiffFile is an AIFF file of form ("AIFF" or "AIFC") with an odd-sized
// chunk before its COMM chunk of frames at rate, the COMM chunk cut to
// commLen bytes when commLen is under 18.
func aiffFile(form string, frames uint32, rate []byte, commLen int) []byte {
	comm := make([]byte, 18)
	binary.BigEndian.PutUint16(comm[0:2], 1)
	binary.BigEndian.PutUint32(comm[2:6], frames)
	binary.BigEndian.PutUint16(comm[6:8], 16)
	copy(comm[8:18], rate)
	comm = comm[:commLen]
	var body []byte
	body = append(body, form...)
	body = append(body, chunk("FVER", []byte{1, 2, 3})...)
	body = append(body, chunk("COMM", comm)...)
	return append(append([]byte("FORM"), be32(uint32(len(body)))...), body...)
}

// chunk is one AIFF chunk, padded to an even length.
func chunk(id string, data []byte) []byte {
	out := append(append([]byte(id), be32(uint32(len(data)))...), data...)
	if len(data)%2 == 1 {
		out = append(out, 0)
	}
	return out
}

func be32(v uint32) []byte { return binary.BigEndian.AppendUint32(nil, v) }

// renderedAudio is the audio a scripted render writes: 2s at 22050Hz.
var renderedAudio = aiffFile("AIFC", 44100, rate22050, 18)

// rendered is the speech program's clean render of renderedAudio.
var rendered = speechAnswer{renders: renderedAudio}

func (r *speechRunner) ran() []speechRun {
	r.mu.Lock()
	defer r.mu.Unlock()
	return append([]speechRun(nil), r.runs...)
}

// spoken answers the text of every run of the speech program.
func (r *speechRunner) spoken() []string {
	var out []string
	for _, run := range r.ran() {
		if run.bin == darwinSpeech.Say {
			out = append(out, run.args[len(run.args)-1])
		}
	}
	return out
}

var darwinSpeech = SpeechPlatformFor("darwin", "/bin/agent-repld")

// deviceLine is the audio-output verb's line for device.
func deviceLine(device audiooutput.Device) string {
	var b strings.Builder
	if err := audiooutput.Encode(&b, device); err != nil {
		panic(err)
	}
	return b.String()
}

// The devices a test's audio output reads.
var (
	airpods  = audiooutput.Device{Name: "Dodge’s AirPods Pro", Transport: audiooutput.TransportBluetooth, UID: "F8-CB-15-F0-AE-6C:output"}
	speakers = audiooutput.Device{Name: "MacBook Pro Speakers", Transport: audiooutput.TransportBuiltIn, UID: "BuiltInSpeakerDevice", DataSource: audiooutput.DataSourceInternalSpeakers}
)

// bluetoothReport is a system_profiler Bluetooth report with one connected
// device at address of class, and one device of another class not connected.
func bluetoothReport(address, class string) string {
	return `{"SPBluetoothDataType":[{"controller_properties":{"controller_address":"84:2F:57:A5:57:FA"},` +
		`"device_connected":[{"Dodge’s AirPods Pro":{"device_address":"` + address + `","device_minorType":"` + class + `"}}],` +
		`"device_not_connected":[{"Kitchen":{"device_address":"74:8F:3C:18:4E:EE","device_minorType":"Loudspeaker"}}]}]}`
}

var airpodsReport = bluetoothReport("F8:CB:15:F0:AE:6C", BluetoothHeadphones)

// newSpeechRunner scripts the audio output the verb prints (and, for a
// Bluetooth one, the Bluetooth report), the speech program's answer and a
// player that plays cleanly.
func newSpeechRunner(audio speechAnswer, say speechAnswer) *speechRunner {
	return &speechRunner{answers: map[string]speechAnswer{
		darwinSpeech.AudioOutput: audio,
		darwinSpeech.Bluetooth:   {stdout: airpodsReport},
		darwinSpeech.Say:         say,
		darwinSpeech.Player:      {},
	}}
}

// onAirpods and onSpeakers are the audio output's answers for each device.
var (
	onAirpods  = speechAnswer{stdout: deviceLine(airpods)}
	onSpeakers = speechAnswer{stdout: deviceLine(speakers)}
)

// quietSpeech is the speech of a notifier test about something else: macOS
// with persistent wifi off, so it runs nothing.
func quietSpeech() *Speech {
	return NewSpeech("darwin", darwinSpeech, &speechRunner{}, wifiOff)
}

type speechFixture struct {
	notifier *Notifier
	speech   *Speech
	backend  *fakeBackend
	runner   *speechRunner
	log      *dlog.TestLogger
}

func newSpeechFixture(goos string, standing WifiStanding, runner *speechRunner) speechFixture {
	log := dlog.NewTestLogger()
	f := speechFixture{backend: &fakeBackend{}, runner: runner, log: log}
	f.speech = NewSpeech(goos, SpeechPlatformFor(goos, "/bin/agent-repld"), runner, standing)
	f.notifier = New(Deps{
		Focus: NewFocus(log), Backend: f.backend, Chime: &fakeChime{},
		Speech: f.speech, Clicks: &fakeClicks{}, Workspaces: named, Log: log,
	})
	return f
}

// postOne posts one banner and joins every goroutine it started.
func (f speechFixture) postOne() {
	f.notifier.Post("ws1", KindTurnEnded, func(context.Context, string) Banner {
		return Banner{Title: "✅ ws-name", Body: Italic("Fixed the bug.")}
	})
	f.notifier.wg.Wait()
}

func TestSpeechReadsTheBannerAloudWhenEveryConditionHolds(t *testing.T) {
	// Arrange
	f := newSpeechFixture("darwin", wifiOn, newSpeechRunner(onAirpods, rendered))

	// Act
	f.postOne()

	// Assert
	if got := f.runner.spoken(); !reflect.DeepEqual(got, []string{"✅ ws-name\nFixed the bug."}) {
		t.Fatalf("spoke %q, want the banner's title and plain body", got)
	}
	r, ok := dlog.FindRecord(f.log.Records(), dlog.RecordQuery{Level: "info", Operation: opSpeak})
	if !ok || r.Context["decided_by"] != "persistent_wifi_on_lid_closed_and_private_listening_device" || r.Context["device"] != "Dodge’s AirPods Pro" || r.Context["bluetooth_class"] != BluetoothHeadphones || r.Context["kind"] != KindTurnEnded {
		t.Fatalf("decision record = %+v (found %v), want every condition, the device and the kind", r, ok)
	}
}

// TestSpeechIsSilentUnlessEveryConditionHolds pins each condition that keeps a
// banner from being read aloud, and that its banner is drawn all the same.
func TestSpeechIsSilentUnlessEveryConditionHolds(t *testing.T) {
	cases := []struct {
		name      string
		goos      string
		standing  WifiStanding
		audio     speechAnswer
		wantLevel string
		wantBy    Silence
		wantLid   string
		wantRuns  int
	}{
		{name: "persistent wifi off", goos: "darwin", standing: wifiOff, wantLevel: "info", wantBy: SilenceWifiOff},
		{name: "the lid open", goos: "darwin", standing: modeOn(func(s *agentreplv1.PersistentWifiState) {
			s.Lid = &agentreplv1.PersistentWifiState_LidOpen{LidOpen: &agentreplv1.PersistentWifiLidOpen{}}
		}), audio: onAirpods, wantLevel: "info", wantBy: SilenceLidNotClosed, wantLid: "open"},
		{name: "a machine with no lid", goos: "darwin", standing: modeOn(func(s *agentreplv1.PersistentWifiState) {
			s.Lid = &agentreplv1.PersistentWifiState_NoLid{NoLid: &agentreplv1.PersistentWifiNoLid{}}
		}), audio: onAirpods, wantLevel: "info", wantBy: SilenceLidNotClosed, wantLid: "no_lid"},
		{name: "the lid not yet heard", goos: "darwin", standing: modeOn(func(*agentreplv1.PersistentWifiState) {}), audio: onAirpods, wantLevel: "info", wantBy: SilenceLidNotClosed, wantLid: "unheard"},
		{name: "the lid unwatched", goos: "darwin", standing: modeOn(func(s *agentreplv1.PersistentWifiState) {
			s.Lid = &agentreplv1.PersistentWifiState_LidUnwatched{LidUnwatched: &agentreplv1.PersistentWifiLidUnwatched{Detail: "no IOKit port"}}
		}), audio: onAirpods, wantLevel: "info", wantBy: SilenceLidNotClosed, wantLid: "unwatched"},
		{name: "persistent wifi not yet read", goos: "darwin", standing: fakeStanding{}, wantLevel: "info", wantBy: SilenceWifiUnread},
		{name: "airpods absent", goos: "darwin", standing: wifiOn, audio: onSpeakers, wantLevel: "info", wantBy: SilenceNoPrivateDevice, wantRuns: 1},
		{name: "the audio read fails", goos: "darwin", standing: wifiOn, audio: speechAnswer{err: errors.New("agent-repld: signal: killed")}, wantLevel: "error", wantBy: SilenceOutputUnreadable, wantRuns: 1},
		{name: "the audio output is unreadable", goos: "darwin", standing: wifiOn, audio: speechAnswer{stdout: "not json"}, wantLevel: "error", wantBy: SilenceOutputUnreadable, wantRuns: 1},
		{name: "a platform with no speech", goos: "linux", standing: wifiOn, wantLevel: "info", wantBy: SilenceUnsupported},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newSpeechFixture(tc.goos, tc.standing, newSpeechRunner(tc.audio, rendered))

			// Act
			f.postOne()

			// Assert
			if got := f.runner.spoken(); len(got) != 0 {
				t.Fatalf("spoke %q, want silence", got)
			}
			if got := len(f.runner.ran()); got != tc.wantRuns {
				t.Fatalf("ran %d programs, want %d", got, tc.wantRuns)
			}
			if got := len(f.backend.banners()); got != 1 {
				t.Fatalf("posted %d banners, want the banner drawn as ever", got)
			}
			r, ok := dlog.FindRecord(f.log.Records(), dlog.RecordQuery{Level: tc.wantLevel, Operation: opSpeak})
			if !ok || r.Context["decided_by"] != string(tc.wantBy) {
				t.Fatalf("decision record = %+v (found %v), want %s decided by %s", r, ok, tc.wantLevel, tc.wantBy)
			}
			if tc.wantLid != "" && r.Context["lid"] != tc.wantLid {
				t.Fatalf("decision record = %+v, want lid %q", r, tc.wantLid)
			}
			if tc.audio.err != nil && !strings.Contains(r.Context["cause"].(string), tc.audio.err.Error()) {
				t.Fatalf("failure record = %+v, want the read's cause", r)
			}
		})
	}
}

func TestSpeechRecordsAnUnsupportedPlatformOnce(t *testing.T) {
	// Arrange
	f := newSpeechFixture("linux", wifiOn, &speechRunner{})

	// Act
	f.postOne()
	f.postOne()

	// Assert
	n := 0
	for _, r := range f.log.Records() {
		if r.Operation == opSpeak {
			n++
		}
	}
	if n != 1 {
		t.Fatalf("recorded speech %d times over two banners, want the unsupported platform once", n)
	}
}

// withinBound reports whether a run's deadline is bound from when it ran,
// between started and now.
func withinBound(run speechRun, started time.Time, bound time.Duration) bool {
	return !run.deadline.IsZero() && !run.deadline.Before(started.Add(bound)) && !run.deadline.After(time.Now().Add(bound))
}

// TestSpeechRendersThenPlays pins a clean reading's two steps: the text
// rendered to a temporary AIFF file under the render bound, that file played
// under a bound sized from its own audio, and the reading recorded once.
func TestSpeechRendersThenPlays(t *testing.T) {
	// Arrange
	f := newSpeechFixture("darwin", wifiOn, newSpeechRunner(onAirpods, rendered))
	text := "✅ ws-name\nFixed the bug."
	started := time.Now()

	// Act
	f.postOne()

	// Assert
	path := f.runner.renderedPath()
	var render speechRun
	for _, run := range f.runner.ran() {
		if run.bin == darwinSpeech.Say {
			render = run
		}
	}
	t.Run("the render writes the text to an aiff file in the temporary directory", func(t *testing.T) {
		if !reflect.DeepEqual(render.args, []string{"-o", path, "--", text}) || filepath.Dir(path) != filepath.Clean(os.TempDir()) || filepath.Ext(path) != ".aiff" {
			t.Fatalf("say argv %q, want -o FILE.aiff in %s and the text behind --", render.args, os.TempDir())
		}
	})
	t.Run("the render runs under the render bound", func(t *testing.T) {
		if !withinBound(render, started, renderBound(text)) {
			t.Fatalf("say ran under deadline %v, want its bound %v", render.deadline, renderBound(text))
		}
	})
	t.Run("the player plays the rendered file", func(t *testing.T) {
		played := f.runner.played()
		if len(played) != 1 || !reflect.DeepEqual(played[0].args, []string{path}) {
			t.Fatalf("played %+v, want the rendered file once", played)
		}
	})
	t.Run("the play runs under a bound sized from the rendered audio", func(t *testing.T) {
		played := f.runner.played()
		if len(played) != 1 || !withinBound(played[0], started, playBound(2*time.Second)) {
			t.Fatalf("played %+v, want the bound of 2s of audio, %v", played, playBound(2*time.Second))
		}
	})
	t.Run("the reading is recorded with its length and its audio's", func(t *testing.T) {
		r, ok := dlog.FindRecord(f.log.Records(), dlog.RecordQuery{Level: "info", Message: "the banner was read aloud"})
		if !ok || r.Context["audio_ms"] != int64(2000) || r.Context["duration_ms"] == nil {
			t.Fatalf("reading record = %+v (found %v), want audio_ms 2000 and duration_ms", r, ok)
		}
	})
	t.Run("the rendered file is removed", func(t *testing.T) {
		if _, err := os.Stat(path); !errors.Is(err, os.ErrNotExist) {
			t.Fatalf("stat %s = %v, want it removed", path, err)
		}
	})
}

// TestSpeechStepOutcomes pins how each step of a reading ends when it does
// not run cleanly: recorded with its step, the banner out all the same, the
// play never run after a failed render, and the rendered file removed.
func TestSpeechStepOutcomes(t *testing.T) {
	const tiny = time.Millisecond
	failed := errors.New("exit status 1")
	passedMessage := "a step of the banner's reading ran past its bound and was killed; the banner was drawn but not read aloud in full"
	failedMessage := "the speech program failed; the banner was drawn but not read aloud in full"
	stoodDownMessage := "the daemon stood down while the banner was read aloud"
	cases := []struct {
		name        string
		say, play   speechAnswer
		standDown   string // the step whose run the daemon stands down under
		wantLevel   string
		wantMessage string
		wantStep    string
		wantCause   string
		wantAudioMS any
		wantPlays   int
	}{
		{name: "the render's bound passes", say: speechAnswer{renders: renderedAudio, hang: true},
			wantLevel: "error", wantMessage: passedMessage, wantStep: stepRender, wantCause: "killed when its bound of 1ms passed: signal: killed"},
		{name: "the play's bound passes", say: rendered, play: speechAnswer{hang: true},
			wantLevel: "error", wantMessage: passedMessage, wantStep: stepPlay, wantCause: "killed when its bound of 1ms passed: signal: killed", wantAudioMS: int64(2000), wantPlays: 1},
		{name: "the speech program fails", say: speechAnswer{renders: renderedAudio, err: failed},
			wantLevel: "error", wantMessage: failedMessage, wantStep: stepRender, wantCause: failed.Error()},
		{name: "the player fails", say: rendered, play: speechAnswer{err: failed},
			wantLevel: "error", wantMessage: failedMessage, wantStep: stepPlay, wantCause: failed.Error(), wantAudioMS: int64(2000), wantPlays: 1},
		{name: "the rendered audio is unreadable", say: speechAnswer{renders: []byte("not audio")},
			wantLevel: "error", wantMessage: "the rendered reading's length could not be read; the banner is not read aloud",
			wantStep: stepRender, wantCause: "not an AIFF file: no FORM AIFF or AIFC header"},
		{name: "the daemon stands down during the render", say: speechAnswer{renders: renderedAudio, err: errKilled}, standDown: stepRender,
			wantLevel: "info", wantMessage: stoodDownMessage, wantStep: stepRender, wantCause: errKilled.Error()},
		{name: "the daemon stands down during the play", say: rendered, play: speechAnswer{err: errKilled}, standDown: stepPlay,
			wantLevel: "info", wantMessage: stoodDownMessage, wantStep: stepPlay, wantCause: errKilled.Error(), wantAudioMS: int64(2000), wantPlays: 1},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			runner := newSpeechRunner(onAirpods, tc.say)
			runner.answers[darwinSpeech.Player] = tc.play
			f := newSpeechFixture("darwin", wifiOn, runner)
			if tc.say.hang {
				f.speech.renderBound = func(string) time.Duration { return tiny }
			}
			if tc.play.hang {
				f.speech.playBound = func(time.Duration) time.Duration { return tiny }
			}
			bin := map[string]string{stepRender: darwinSpeech.Say, stepPlay: darwinSpeech.Player}
			if tc.standDown != "" {
				answer := runner.answers[bin[tc.standDown]]
				answer.onRun = f.notifier.cancel
				runner.answers[bin[tc.standDown]] = answer
			}

			// Act
			f.postOne()

			// Assert
			if got := len(f.backend.banners()); got != 1 {
				t.Fatalf("posted %d banners, want the banner out", got)
			}
			r, ok := dlog.FindRecord(f.log.Records(), dlog.RecordQuery{Level: tc.wantLevel, Message: tc.wantMessage})
			if !ok || r.Context["step"] != tc.wantStep || r.Context["program"] != bin[tc.wantStep] || r.Context["cause"] != tc.wantCause {
				t.Fatalf("outcome record = %+v (found %v), want step %s, its program and cause %q", r, ok, tc.wantStep, tc.wantCause)
			}
			if r.Context["audio_ms"] != tc.wantAudioMS {
				t.Fatalf("outcome record audio_ms = %v, want %v", r.Context["audio_ms"], tc.wantAudioMS)
			}
			if got := len(f.runner.played()); got != tc.wantPlays {
				t.Fatalf("played %d times, want %d", got, tc.wantPlays)
			}
			if _, ok := dlog.FindRecord(f.log.Records(), dlog.RecordQuery{Message: "the banner was read aloud"}); ok {
				t.Fatal("recorded the banner read aloud, want no clean reading")
			}
			path := f.runner.renderedPath()
			if _, err := os.Stat(path); path == "" || !errors.Is(err, os.ErrNotExist) {
				t.Fatalf("stat %q = %v, want the rendered file removed", path, err)
			}
		})
	}
}

// TestSpeechRecordsEachBound pins that a step's outcome record carries the
// bound its run was held to.
func TestSpeechRecordsEachBound(t *testing.T) {
	failed := errors.New("exit status 1")
	cases := []struct {
		name      string
		say, play speechAnswer
		want      time.Duration
	}{
		{name: "the render's bound", say: speechAnswer{renders: renderedAudio, err: failed}, want: renderBound("✅ ws-name\nFixed the bug.")},
		{name: "the play's bound", say: rendered, play: speechAnswer{err: failed}, want: playBound(2 * time.Second)},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			runner := newSpeechRunner(onAirpods, tc.say)
			runner.answers[darwinSpeech.Player] = tc.play
			f := newSpeechFixture("darwin", wifiOn, runner)

			// Act
			f.postOne()

			// Assert
			r, ok := dlog.FindRecord(f.log.Records(), dlog.RecordQuery{Level: "error", Operation: opSpeak})
			if !ok || r.Context["bound_ms"] != tc.want.Milliseconds() {
				t.Fatalf("failure record = %+v (found %v), want bound_ms %d", r, ok, tc.want.Milliseconds())
			}
		})
	}
}

func TestSpeechRunsOnlyOnceTheBannerIsMade(t *testing.T) {
	// Arrange
	f := newSpeechFixture("darwin", wifiOn, newSpeechRunner(onAirpods, rendered))
	ranBeforeMade := false
	f.backend.onPost = func() { ranBeforeMade = len(f.runner.ran()) > 0 }

	// Act
	f.postOne()

	// Assert
	if ranBeforeMade {
		t.Fatal("speech ran before the banner was handed to the platform")
	}
	if got := len(f.runner.spoken()); got != 1 {
		t.Fatalf("spoke %d times, want once", got)
	}
}

func TestSpeechSaysNothingForABannerNotMade(t *testing.T) {
	// Arrange
	f := newSpeechFixture("darwin", wifiOn, newSpeechRunner(onAirpods, rendered))
	f.backend.err = errors.New("exit status 64")

	// Act
	f.postOne()

	// Assert
	if got := f.runner.ran(); len(got) != 0 {
		t.Fatalf("ran %+v for a banner not made, want nothing", got)
	}
}

func TestSpeechSaysNothingOnceTheDaemonStandsDown(t *testing.T) {
	// Arrange: Close begins while the banner is handed over, before it is made.
	f := newSpeechFixture("darwin", wifiOn, newSpeechRunner(onAirpods, rendered))
	closed := make(chan struct{})
	f.backend.onPost = func() {
		go func() { f.notifier.Close(); close(closed) }()
		<-f.notifier.ctx.Done()
	}

	// Act
	f.notifier.Post("ws1", KindTurnEnded, titledBy)
	<-closed

	// Assert
	if got := f.runner.ran(); len(got) != 0 {
		t.Fatalf("ran %+v while standing down, want nothing", got)
	}
	if _, ok := hasRecord(f.log, "info", "the daemon is standing down; the banner is not read aloud"); !ok {
		t.Fatal("no record of the reading given up for the stand-down")
	}
}

// TestEveryKindOfBannerIsReadAloud drives every way a banner is raised and
// pins that each is read aloud alike, through the notifier's one made edge.
func TestEveryKindOfBannerIsReadAloud(t *testing.T) {
	cases := []struct {
		name  string
		raise func(n *Notifier)
	}{
		{name: "an agent notification", raise: func(n *Notifier) { n.Raise("ws1", "permission_requested", "Run the tests?") }},
		{name: "an agent-requested banner", raise: func(n *Notifier) { n.Notify("ws1", "Look here") }},
		{name: "a turn end", raise: func(n *Notifier) {
			endings := fakeEndings{"t1": {Answer: "done"}}
			NewTurnBanners(TurnDeps{Endings: endings, Poster: n, Summaries: &fakeSummaries{}, Log: dlog.NewTestLogger()}).OnTurnEnded("ws1", "t1", wsm.CloseCompleted)
		}},
		{name: "a merge put in line", raise: func(n *Notifier) { NewMergeBanners(n).Queued("ws1", "the branch") }},
		{name: "a merge resolved", raise: func(n *Notifier) { NewMergeBanners(n).Resolved("ws1", MergeLanded, "landed") }},
		{name: "a deploy mediated", raise: func(n *Notifier) { NewMergeBanners(n).Mediated("ws1", "escalated") }},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newSpeechFixture("darwin", wifiOn, newSpeechRunner(onAirpods, rendered))

			// Act
			tc.raise(f.notifier)
			f.notifier.wg.Wait()

			// Assert
			banners := f.backend.banners()
			if len(banners) != 1 {
				t.Fatalf("posted %d banners, want one", len(banners))
			}
			if got := f.runner.spoken(); !reflect.DeepEqual(got, []string{SpokenText(banners[0])}) {
				t.Fatalf("spoke %q, want the banner %+v read aloud", got, banners[0])
			}
		})
	}
}

func TestPrivateListeningDevice(t *testing.T) {
	bluetooth := func(class string) AudioOutput {
		return AudioOutput{Device: audiooutput.Device{Name: "a device", Transport: audiooutput.TransportBluetooth}, BluetoothClass: class}
	}
	cases := []struct {
		name string
		out  AudioOutput
		want bool
	}{
		{name: "airpods: bluetooth headphones", out: AudioOutput{Device: airpods, BluetoothClass: BluetoothHeadphones}, want: true},
		{name: "a bluetooth headset", out: bluetooth(BluetoothHeadset), want: true},
		{name: "bluetooth le headphones", out: AudioOutput{Device: audiooutput.Device{Transport: audiooutput.TransportBluetoothLE}, BluetoothClass: BluetoothHeadphones}, want: true},
		{name: "the built-in headphone jack", out: AudioOutput{Device: audiooutput.Device{Transport: audiooutput.TransportBuiltIn, DataSource: audiooutput.DataSourceHeadphones}}, want: true},
		{name: "the built-in speakers", out: AudioOutput{Device: speakers}, want: false},
		{name: "the built-in output with no data source", out: AudioOutput{Device: audiooutput.Device{Transport: audiooutput.TransportBuiltIn}}, want: false},
		{name: "a bluetooth loudspeaker", out: bluetooth("Loudspeaker"), want: false},
		{name: "bluetooth portable audio", out: bluetooth("Portable Audio"), want: false},
		{name: "a bluetooth hands-free device", out: bluetooth("Hands-Free"), want: false},
		{name: "an unidentifiable bluetooth output", out: bluetooth(""), want: false},
		{name: "airpods by name with no class", out: AudioOutput{Device: airpods}, want: false},
		{name: "a usb output", out: AudioOutput{Device: audiooutput.Device{Transport: audiooutput.TransportUSB, DataSource: audiooutput.DataSourceHeadphones}}, want: false},
		{name: "an hdmi output", out: AudioOutput{Device: audiooutput.Device{Transport: audiooutput.TransportHDMI}}, want: false},
		{name: "a displayport display", out: AudioOutput{Device: audiooutput.Device{Transport: audiooutput.TransportDisplayPort}}, want: false},
		{name: "an airplay output", out: AudioOutput{Device: audiooutput.Device{Transport: audiooutput.TransportAirPlay}}, want: false},
		{name: "an unknown transport", out: AudioOutput{Device: audiooutput.Device{Transport: "virt"}, BluetoothClass: BluetoothHeadphones}, want: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := PrivateListeningDevice(tc.out)

			// Assert
			if got != tc.want {
				t.Fatalf("PrivateListeningDevice(%+v) = %v, want %v", tc.out, got, tc.want)
			}
		})
	}
}

// TestSpeechReadsTheOutputsKind pins how the audio output's kind is read: a
// Bluetooth output's class from the report, joined on its address, and an
// unidentifiable one read as a speaker.
func TestSpeechReadsTheOutputsKind(t *testing.T) {
	cases := []struct {
		name      string
		device    audiooutput.Device
		bluetooth speechAnswer
		wantSpeak bool
		wantLevel string
		wantRuns  int
	}{
		{name: "connected airpods are read aloud to", device: airpods, bluetooth: speechAnswer{stdout: airpodsReport}, wantSpeak: true, wantLevel: "info", wantRuns: 4},
		{name: "the address is matched in any case", device: airpods, bluetooth: speechAnswer{stdout: bluetoothReport("f8:cb:15:f0:ae:6c", BluetoothHeadphones)}, wantSpeak: true, wantLevel: "info", wantRuns: 4},
		{name: "a connected loudspeaker is silent", device: airpods, bluetooth: speechAnswer{stdout: bluetoothReport("F8:CB:15:F0:AE:6C", "Loudspeaker")}, wantLevel: "info", wantRuns: 2},
		{name: "an output the report does not show connected is unidentifiable and silent", device: airpods, bluetooth: speechAnswer{stdout: bluetoothReport("AA:BB:CC:DD:EE:FF", BluetoothHeadphones)}, wantLevel: "info", wantRuns: 2},
		{name: "a bluetooth uid naming no address is unidentifiable and silent", device: audiooutput.Device{Name: "AirPods", Transport: audiooutput.TransportBluetooth, UID: "odd"}, wantLevel: "info", wantRuns: 1},
		{name: "the built-in headphone jack reads no bluetooth report", device: audiooutput.Device{Transport: audiooutput.TransportBuiltIn, DataSource: audiooutput.DataSourceHeadphones}, wantSpeak: true, wantLevel: "info", wantRuns: 3},
		{name: "a failed bluetooth read is silent and recorded", device: airpods, bluetooth: speechAnswer{err: errors.New("system_profiler: signal: killed")}, wantLevel: "error", wantRuns: 2},
		{name: "an unreadable bluetooth report is silent and recorded", device: airpods, bluetooth: speechAnswer{stdout: "not json"}, wantLevel: "error", wantRuns: 2},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			runner := &speechRunner{answers: map[string]speechAnswer{
				darwinSpeech.AudioOutput: {stdout: deviceLine(tc.device)},
				darwinSpeech.Bluetooth:   tc.bluetooth,
				darwinSpeech.Say:         rendered,
				darwinSpeech.Player:      {},
			}}
			f := newSpeechFixture("darwin", wifiOn, runner)

			// Act
			f.postOne()

			// Assert
			if got := len(f.runner.spoken()) == 1; got != tc.wantSpeak {
				t.Fatalf("spoke %q, want spoken=%v", f.runner.spoken(), tc.wantSpeak)
			}
			if got := len(f.runner.ran()); got != tc.wantRuns {
				t.Fatalf("ran %+v, want %d runs", f.runner.ran(), tc.wantRuns)
			}
			if _, ok := dlog.FindRecord(f.log.Records(), dlog.RecordQuery{Level: tc.wantLevel, Operation: opSpeak}); !ok {
				t.Fatalf("no %s record of the decision", tc.wantLevel)
			}
		})
	}
}

func TestSpeechReadsEachProgramUnderItsBound(t *testing.T) {
	// Arrange
	f := newSpeechFixture("darwin", wifiOn, newSpeechRunner(onAirpods, rendered))
	started := time.Now()

	// Act
	f.postOne()

	// Assert
	bounds := map[string]time.Duration{darwinSpeech.AudioOutput: audioOutputBound, darwinSpeech.Bluetooth: bluetoothBound}
	for _, run := range f.runner.ran() {
		bound, ok := bounds[run.bin]
		if !ok {
			continue
		}
		if run.deadline.IsZero() || run.deadline.Before(started.Add(bound)) || run.deadline.After(time.Now().Add(bound)) {
			t.Fatalf("%s ran under deadline %v, want its bound %v", run.bin, run.deadline, bound)
		}
	}
}

func TestBluetoothClass(t *testing.T) {
	cases := []struct {
		name    string
		stdout  string
		address string
		want    string
		wantErr string
	}{
		{name: "the connected device's class", stdout: airpodsReport, address: "F8:CB:15:F0:AE:6C", want: BluetoothHeadphones},
		{name: "a device not connected has no class", stdout: airpodsReport, address: "74:8F:3C:18:4E:EE", want: ""},
		{name: "no devices", stdout: `{"SPBluetoothDataType":[{}]}`, address: "F8:CB:15:F0:AE:6C", want: ""},
		{name: "not json", stdout: "garbage", address: "F8:CB:15:F0:AE:6C", wantErr: "decode the Bluetooth report"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, err := bluetoothClass(tc.stdout, tc.address)

			// Assert
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("bluetoothClass err = %v, want %q", err, tc.wantErr)
				}
				return
			}
			if err != nil || got != tc.want {
				t.Fatalf("bluetoothClass = %q, %v, want %q", got, err, tc.want)
			}
		})
	}
}

func TestRenderBound(t *testing.T) {
	cases := []struct {
		name string
		text string
		want time.Duration
	}{
		{name: "no words is the program's start", text: "", want: 3 * sayStartup},
		{name: "each word adds its time", text: "two words", want: 3 * (sayStartup + 2*sayPerWord)},
		{name: "lines count their words alike", text: "one\ntwo three", want: 3 * (sayStartup + 3*sayPerWord)},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := renderBound(tc.text)

			// Assert
			if got != tc.want {
				t.Fatalf("renderBound(%q) = %v, want %v", tc.text, got, tc.want)
			}
		})
	}
}

func TestPlayBound(t *testing.T) {
	cases := []struct {
		name  string
		audio time.Duration
		want  time.Duration
	}{
		{name: "no audio is the player's start", audio: 0, want: 3 * playerStartup},
		{name: "the audio's length adds itself", audio: 6130 * time.Millisecond, want: 3 * (playerStartup + 6130*time.Millisecond)},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := playBound(tc.audio)

			// Assert
			if got != tc.want {
				t.Fatalf("playBound(%v) = %v, want %v", tc.audio, got, tc.want)
			}
		})
	}
}

func TestAIFFDuration(t *testing.T) {
	header := func(form string) []byte { return append([]byte("FORM\x00\x00\x00\x0e"), form...) }
	sayFrames, sayRate := 56926.0, 22050.0 // read from a file `say -o` rendered
	cases := []struct {
		name    string
		data    []byte
		want    time.Duration
		wantErr string
	}{
		{name: "an aiff-c file as say renders it", data: aiffFile("AIFC", 56926, rate22050, 18), want: time.Duration(sayFrames / sayRate * float64(time.Second))},
		{name: "a plain aiff file", data: aiffFile("AIFF", 44100, rate44100, 18), want: time.Second},
		{name: "no frames is no audio", data: aiffFile("AIFF", 0, rate44100, 18), want: 0},
		{name: "not a form file", data: []byte("RIFF\x00\x00\x00\x04WAVE"), wantErr: "not an AIFF file"},
		{name: "a form of another kind", data: header("8SVX"), wantErr: "not an AIFF file"},
		{name: "an empty file", data: nil, wantErr: "not an AIFF file"},
		{name: "no comm chunk", data: append(header("AIFF"), chunk("FVER", []byte{1, 2})...), wantErr: "no COMM chunk"},
		{name: "a chunk running past the file", data: append(header("AIFF"), "FVER\x00\x00\x01\x00"...), wantErr: "no COMM chunk"},
		{name: "a comm chunk cut short", data: aiffFile("AIFF", 44100, rate44100, 10), wantErr: "the COMM chunk is cut short"},
		{name: "a zero sample rate", data: aiffFile("AIFF", 44100, make([]byte, 10), 18), wantErr: "is not a positive finite number"},
		{name: "a negative sample rate", data: aiffFile("AIFF", 44100, append([]byte{0xc0}, rate44100[1:]...), 18), wantErr: "is not a positive finite number"},
		{name: "an infinite sample rate", data: aiffFile("AIFF", 44100, []byte{0x7f, 0xff, 0x80, 0, 0, 0, 0, 0, 0, 0}, 18), wantErr: "is not a positive finite number"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, err := aiffDuration(tc.data)

			// Assert
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("aiffDuration err = %v, want %q", err, tc.wantErr)
				}
				return
			}
			if err != nil || got != tc.want {
				t.Fatalf("aiffDuration = %v, %v, want %v", got, err, tc.want)
			}
		})
	}
}

func TestRenderedLengthOfAMissingFile(t *testing.T) {
	// Act
	_, err := renderedLength(filepath.Join(t.TempDir(), "absent.aiff"))

	// Assert
	if !errors.Is(err, os.ErrNotExist) {
		t.Fatalf("renderedLength err = %v, want the file missing", err)
	}
}

func TestSpokenText(t *testing.T) {
	cases := []struct {
		name   string
		banner Banner
		want   string
	}{
		{name: "title over body", banner: Banner{Title: "ws", Body: "Run the tests?"}, want: "ws\nRun the tests?"},
		{name: "an italic body read plain", banner: Banner{Title: "✅ ws", Body: Italic("Done.")}, want: "✅ ws\nDone."},
		{name: "no body", banner: Banner{Title: "ws"}, want: "ws"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := SpokenText(tc.banner)

			// Assert
			if got != tc.want {
				t.Fatalf("SpokenText(%+v) = %q, want %q", tc.banner, got, tc.want)
			}
		})
	}
}

func TestSpeechPlatformFor(t *testing.T) {
	cases := []struct {
		name string
		goos string
		want SpeechPlatform
	}{
		{name: "macOS speaks through say and reads CoreAudio through the daemon's own verb", goos: "darwin", want: SpeechPlatform{
			Say: "/usr/bin/say", Player: "/usr/bin/afplay", AudioOutput: "/bin/agent-repld", AudioOutputArgs: []string{audiooutput.Verb},
			Bluetooth: "/usr/sbin/system_profiler", BluetoothArgs: []string{"SPBluetoothDataType", "-json"},
		}},
		{name: "linux speaks nothing", goos: "linux", want: SpeechPlatform{}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := SpeechPlatformFor(tc.goos, "/bin/agent-repld")

			// Assert
			if !reflect.DeepEqual(got, tc.want) {
				t.Fatalf("SpeechPlatformFor(%s) = %+v, want %+v", tc.goos, got, tc.want)
			}
		})
	}
}

func TestSpeechPlatformWithOverrides(t *testing.T) {
	cases := []struct {
		name                          string
		platform                      SpeechPlatform
		say, player, audio, bluetooth string
		want                          [4]string
	}{
		{name: "every program overridden", platform: darwinSpeech, say: "/fake/say", player: "/fake/chime", audio: "/fake/audio", bluetooth: "/fake/bt", want: [4]string{"/fake/say", "/fake/chime", "/fake/audio", "/fake/bt"}},
		{name: "none overridden", platform: darwinSpeech, want: [4]string{"/usr/bin/say", "/usr/bin/afplay", "/bin/agent-repld", "/usr/sbin/system_profiler"}},
		{name: "one overridden", platform: darwinSpeech, bluetooth: "/fake/bt", want: [4]string{"/usr/bin/say", "/usr/bin/afplay", "/bin/agent-repld", "/fake/bt"}},
		{name: "the player overridden", platform: darwinSpeech, player: "/fake/chime", want: [4]string{"/usr/bin/say", "/fake/chime", "/bin/agent-repld", "/usr/sbin/system_profiler"}},
		{name: "an unsupported platform stays silent", platform: SpeechPlatform{}, say: "/fake/say", player: "/fake/chime", audio: "/fake/audio", bluetooth: "/fake/bt"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := tc.platform.WithOverrides(tc.say, tc.player, tc.audio, tc.bluetooth)

			// Assert
			if [4]string{got.Say, got.Player, got.AudioOutput, got.Bluetooth} != tc.want {
				t.Fatalf("WithOverrides = %+v, want %v", got, tc.want)
			}
		})
	}
}

func TestNewSpeechRefusesAHalfWiredSpeech(t *testing.T) {
	cases := []struct {
		name     string
		runner   Runner
		standing WifiStanding
	}{
		{name: "no runner", standing: wifiOn},
		{name: "no standing", runner: &speechRunner{}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Assert
			defer func() {
				if recover() == nil {
					t.Fatal("NewSpeech accepted a half-wired speech")
				}
			}()

			// Act
			NewSpeech("darwin", darwinSpeech, tc.runner, tc.standing)
		})
	}
}

func TestLidName(t *testing.T) {
	cases := []struct {
		name string
		lid  func(*agentreplv1.PersistentWifiState)
		want string
	}{
		{name: "open", lid: func(s *agentreplv1.PersistentWifiState) {
			s.Lid = &agentreplv1.PersistentWifiState_LidOpen{LidOpen: &agentreplv1.PersistentWifiLidOpen{}}
		}, want: "open"},
		{name: "closed", lid: func(s *agentreplv1.PersistentWifiState) {
			s.Lid = &agentreplv1.PersistentWifiState_LidClosed{LidClosed: &agentreplv1.PersistentWifiLidClosed{}}
		}, want: "closed"},
		{name: "no lid", lid: func(s *agentreplv1.PersistentWifiState) {
			s.Lid = &agentreplv1.PersistentWifiState_NoLid{NoLid: &agentreplv1.PersistentWifiNoLid{}}
		}, want: "no_lid"},
		{name: "unwatched", lid: func(s *agentreplv1.PersistentWifiState) {
			s.Lid = &agentreplv1.PersistentWifiState_LidUnwatched{LidUnwatched: &agentreplv1.PersistentWifiLidUnwatched{}}
		}, want: "unwatched"},
		{name: "not yet heard", lid: func(*agentreplv1.PersistentWifiState) {}, want: "unheard"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			state := modeOn(tc.lid).state

			// Act
			got := lidName(state)

			// Assert
			if got != tc.want {
				t.Fatalf("lidName = %q, want %q", got, tc.want)
			}
		})
	}
}
