package desktopnotify

import (
	"context"
	"errors"
	"reflect"
	"strings"
	"sync"
	"testing"
	"time"

	"agent-repld/internal/audiooutput"
	"agent-repld/internal/dlog"
	"agent-repld/internal/sourcescan"
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
}

func (r *speechRunner) Run(ctx context.Context, bin string, args []string, _ func()) (string, error) {
	deadline, _ := ctx.Deadline()
	r.mu.Lock()
	r.runs = append(r.runs, speechRun{bin: bin, args: args, deadline: deadline})
	answer, ok := r.answers[bin]
	r.mu.Unlock()
	if !ok {
		panic("speechRunner: no answer scripted for " + bin)
	}
	return answer.stdout, answer.err
}

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
// Bluetooth one, the Bluetooth report) and the speech program's answer.
func newSpeechRunner(audio speechAnswer, say speechAnswer) *speechRunner {
	return &speechRunner{answers: map[string]speechAnswer{
		darwinSpeech.AudioOutput: audio,
		darwinSpeech.Bluetooth:   {stdout: airpodsReport},
		darwinSpeech.Say:         say,
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
	backend  *fakeBackend
	runner   *speechRunner
	log      *dlog.TestLogger
}

func newSpeechFixture(goos string, standing WifiStanding, runner *speechRunner) speechFixture {
	log := dlog.NewTestLogger()
	f := speechFixture{backend: &fakeBackend{}, runner: runner, log: log}
	f.notifier = New(Deps{
		Focus: NewFocus(log), Backend: f.backend, Chime: &fakeChime{},
		Speech: NewSpeech(goos, SpeechPlatformFor(goos, "/bin/agent-repld"), runner, standing),
		Clicks: &fakeClicks{}, Workspaces: named, Log: log,
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
	f := newSpeechFixture("darwin", wifiOn, newSpeechRunner(onAirpods, speechAnswer{}))

	// Act
	f.postOne()

	// Assert
	if got := f.runner.spoken(); !reflect.DeepEqual(got, []string{"✅ ws-name\nFixed the bug."}) {
		t.Fatalf("spoke %q, want the banner's title and plain body", got)
	}
	runs := f.runner.ran()
	if runs[len(runs)-1].args[0] != "--" {
		t.Fatalf("say argv %q, want the text behind --", runs[len(runs)-1].args)
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
			f := newSpeechFixture(tc.goos, tc.standing, newSpeechRunner(tc.audio, speechAnswer{}))

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

// TestSpeechFailureLeavesTheBannerOut pins that a speech program that fails
// or times out costs only the reading: the banner is out, and the failure is
// recorded with the bound the run was held to.
func TestSpeechFailureLeavesTheBannerOut(t *testing.T) {
	cases := []struct {
		name string
		err  error
	}{
		{name: "say fails", err: errors.New("say: exit status 1")},
		{name: "say times out", err: context.DeadlineExceeded},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newSpeechFixture("darwin", wifiOn, newSpeechRunner(onAirpods, speechAnswer{err: tc.err}))
			started := time.Now()

			// Act
			f.postOne()

			// Assert
			if got := len(f.backend.banners()); got != 1 {
				t.Fatalf("posted %d banners, want the banner out", got)
			}
			r, ok := dlog.FindRecord(f.log.Records(), dlog.RecordQuery{Level: "error", Message: "the speech program failed; the banner was drawn but not read aloud in full"})
			if !ok || r.Context["cause"] != tc.err.Error() || r.Context["program"] != darwinSpeech.Say {
				t.Fatalf("failure record = %+v (found %v), want the program and its cause", r, ok)
			}
			runs := f.runner.ran()
			say := runs[len(runs)-1]
			bound := sayBound("✅ ws-name\nFixed the bug.")
			if say.deadline.IsZero() || say.deadline.Before(started.Add(bound)) || say.deadline.After(time.Now().Add(bound)) {
				t.Fatalf("say ran under deadline %v, want its bound %v", say.deadline, bound)
			}
		})
	}
}

func TestSpeechRunsOnlyOnceTheBannerIsMade(t *testing.T) {
	// Arrange
	f := newSpeechFixture("darwin", wifiOn, newSpeechRunner(onAirpods, speechAnswer{}))
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
	f := newSpeechFixture("darwin", wifiOn, newSpeechRunner(onAirpods, speechAnswer{}))
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
	f := newSpeechFixture("darwin", wifiOn, newSpeechRunner(onAirpods, speechAnswer{}))
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
			f := newSpeechFixture("darwin", wifiOn, newSpeechRunner(onAirpods, speechAnswer{}))

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

// TestSpeechHasOneChokePoint pins that every banner reaches the platform
// through the one Backend.Post call, and is read aloud from its made edge
// alone: no kind of banner can speak, or skip speaking, on its own.
func TestSpeechHasOneChokePoint(t *testing.T) {
	cases := []struct {
		substr string
		want   []string
	}{
		{substr: "Backend.Post(", want: []string{"notifier.go"}},
		{substr: "n.speak(", want: []string{"notifier.go"}},
		{substr: ".Speak(", want: []string{"notifier.go"}},
	}
	for _, tc := range cases {
		t.Run(tc.substr, func(t *testing.T) {
			// Act
			got := sourcescan.Sites(t, tc.substr)

			// Assert
			if !reflect.DeepEqual(got, tc.want) {
				t.Fatalf("%q is called in %v, want only %v", tc.substr, got, tc.want)
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
		{name: "connected airpods are read aloud to", device: airpods, bluetooth: speechAnswer{stdout: airpodsReport}, wantSpeak: true, wantLevel: "info", wantRuns: 3},
		{name: "the address is matched in any case", device: airpods, bluetooth: speechAnswer{stdout: bluetoothReport("f8:cb:15:f0:ae:6c", BluetoothHeadphones)}, wantSpeak: true, wantLevel: "info", wantRuns: 3},
		{name: "a connected loudspeaker is silent", device: airpods, bluetooth: speechAnswer{stdout: bluetoothReport("F8:CB:15:F0:AE:6C", "Loudspeaker")}, wantLevel: "info", wantRuns: 2},
		{name: "an output the report does not show connected is unidentifiable and silent", device: airpods, bluetooth: speechAnswer{stdout: bluetoothReport("AA:BB:CC:DD:EE:FF", BluetoothHeadphones)}, wantLevel: "info", wantRuns: 2},
		{name: "a bluetooth uid naming no address is unidentifiable and silent", device: audiooutput.Device{Name: "AirPods", Transport: audiooutput.TransportBluetooth, UID: "odd"}, wantLevel: "info", wantRuns: 1},
		{name: "the built-in headphone jack reads no bluetooth report", device: audiooutput.Device{Transport: audiooutput.TransportBuiltIn, DataSource: audiooutput.DataSourceHeadphones}, wantSpeak: true, wantLevel: "info", wantRuns: 2},
		{name: "a failed bluetooth read is silent and recorded", device: airpods, bluetooth: speechAnswer{err: errors.New("system_profiler: signal: killed")}, wantLevel: "error", wantRuns: 2},
		{name: "an unreadable bluetooth report is silent and recorded", device: airpods, bluetooth: speechAnswer{stdout: "not json"}, wantLevel: "error", wantRuns: 2},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			runner := &speechRunner{answers: map[string]speechAnswer{
				darwinSpeech.AudioOutput: {stdout: deviceLine(tc.device)},
				darwinSpeech.Bluetooth:   tc.bluetooth,
				darwinSpeech.Say:         {},
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
	f := newSpeechFixture("darwin", wifiOn, newSpeechRunner(onAirpods, speechAnswer{}))
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

func TestSayBound(t *testing.T) {
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
			got := sayBound(tc.text)

			// Assert
			if got != tc.want {
				t.Fatalf("sayBound(%q) = %v, want %v", tc.text, got, tc.want)
			}
		})
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
			Say: "/usr/bin/say", AudioOutput: "/bin/agent-repld", AudioOutputArgs: []string{audiooutput.Verb},
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
		name                  string
		platform              SpeechPlatform
		say, audio, bluetooth string
		want                  [3]string
	}{
		{name: "every program overridden", platform: darwinSpeech, say: "/fake/say", audio: "/fake/audio", bluetooth: "/fake/bt", want: [3]string{"/fake/say", "/fake/audio", "/fake/bt"}},
		{name: "none overridden", platform: darwinSpeech, want: [3]string{"/usr/bin/say", "/bin/agent-repld", "/usr/sbin/system_profiler"}},
		{name: "one overridden", platform: darwinSpeech, bluetooth: "/fake/bt", want: [3]string{"/usr/bin/say", "/bin/agent-repld", "/fake/bt"}},
		{name: "an unsupported platform stays silent", platform: SpeechPlatform{}, say: "/fake/say", audio: "/fake/audio", bluetooth: "/fake/bt"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := tc.platform.WithOverrides(tc.say, tc.audio, tc.bluetooth)

			// Assert
			if [3]string{got.Say, got.AudioOutput, got.Bluetooth} != tc.want {
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
