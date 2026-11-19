package desktopnotify

import (
	"context"
	"errors"
	"reflect"
	"strings"
	"sync"
	"testing"
	"time"

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

// wifiOn and wifiOff are the two standings a test reads.
var (
	wifiOn  = fakeStanding{state: &agentreplv1.PersistentWifiState{Mode: &agentreplv1.PersistentWifiState_On{On: &agentreplv1.PersistentWifiModeOn{}}}, ok: true}
	wifiOff = fakeStanding{state: &agentreplv1.PersistentWifiState{Mode: &agentreplv1.PersistentWifiState_Off{Off: &agentreplv1.PersistentWifiModeOff{}}}, ok: true}
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

var darwinSpeech = SpeechPlatformFor("darwin")

// audioReport is a system_profiler report whose default output is NAME over
// TRANSPORT.
func audioReport(name string, transport Transport) string {
	return `{"SPAudioDataType":[{"_items":[` +
		`{"_name":"MacBook Pro Microphone","coreaudio_default_audio_input_device":"spaudio_yes","coreaudio_device_transport":"coreaudio_device_type_builtin"},` +
		`{"_name":"` + name + `","coreaudio_default_audio_output_device":"spaudio_yes","coreaudio_device_transport":"` + string(transport) + `"}` +
		`],"_name":"coreaudio_device"}]}`
}

var (
	airpodsReport  = audioReport("Dodge's AirPods Pro", TransportBluetooth)
	speakersReport = audioReport("MacBook Pro Speakers", TransportBuiltIn)
)

// newSpeechRunner scripts the audio read's and the speech program's answers.
func newSpeechRunner(audio speechAnswer, say speechAnswer) *speechRunner {
	return &speechRunner{answers: map[string]speechAnswer{
		darwinSpeech.AudioOutput: audio,
		darwinSpeech.Say:         say,
	}}
}

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
		Speech: NewSpeech(goos, SpeechPlatformFor(goos), runner, standing),
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

func TestSpeechReadsTheBannerAloudWhenBothConditionsHold(t *testing.T) {
	// Arrange
	f := newSpeechFixture("darwin", wifiOn, newSpeechRunner(speechAnswer{stdout: airpodsReport}, speechAnswer{}))

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
	if !ok || r.Context["decided_by"] != "persistent_wifi_on_and_listening_device" || r.Context["device"] != "Dodge's AirPods Pro" || r.Context["kind"] != KindTurnEnded {
		t.Fatalf("decision record = %+v (found %v), want both conditions, the device and the kind", r, ok)
	}
}

// TestSpeechIsSilentUnlessBothConditionsHold pins each condition that keeps a
// banner from being read aloud, and that its banner is drawn all the same.
func TestSpeechIsSilentUnlessBothConditionsHold(t *testing.T) {
	cases := []struct {
		name      string
		goos      string
		standing  WifiStanding
		audio     speechAnswer
		wantLevel string
		wantBy    Silence
		wantRuns  int
	}{
		{name: "persistent wifi off", goos: "darwin", standing: wifiOff, wantLevel: "info", wantBy: SilenceWifiOff},
		{name: "persistent wifi not yet read", goos: "darwin", standing: fakeStanding{}, wantLevel: "info", wantBy: SilenceWifiUnread},
		{name: "airpods absent", goos: "darwin", standing: wifiOn, audio: speechAnswer{stdout: speakersReport}, wantLevel: "info", wantBy: SilenceNoListeningDevice, wantRuns: 1},
		{name: "the audio read fails", goos: "darwin", standing: wifiOn, audio: speechAnswer{err: errors.New("system_profiler: signal: killed")}, wantLevel: "error", wantBy: SilenceOutputUnreadable, wantRuns: 1},
		{name: "the audio report is unreadable", goos: "darwin", standing: wifiOn, audio: speechAnswer{stdout: "not json"}, wantLevel: "error", wantBy: SilenceOutputUnreadable, wantRuns: 1},
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
			if tc.audio.err != nil && r.Context["cause"] != tc.audio.err.Error() {
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
			f := newSpeechFixture("darwin", wifiOn, newSpeechRunner(speechAnswer{stdout: airpodsReport}, speechAnswer{err: tc.err}))
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
	f := newSpeechFixture("darwin", wifiOn, newSpeechRunner(speechAnswer{stdout: airpodsReport}, speechAnswer{}))
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
	f := newSpeechFixture("darwin", wifiOn, newSpeechRunner(speechAnswer{stdout: airpodsReport}, speechAnswer{}))
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
	f := newSpeechFixture("darwin", wifiOn, newSpeechRunner(speechAnswer{stdout: airpodsReport}, speechAnswer{}))
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
			f := newSpeechFixture("darwin", wifiOn, newSpeechRunner(speechAnswer{stdout: airpodsReport}, speechAnswer{}))

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

func TestListeningDevice(t *testing.T) {
	cases := []struct {
		name string
		out  AudioOutput
		want bool
	}{
		{name: "airpods over bluetooth", out: AudioOutput{Name: "Dodge's AirPods Pro", Transport: TransportBluetooth}, want: true},
		{name: "airpods max over bluetooth", out: AudioOutput{Name: "AirPods Max", Transport: TransportBluetooth}, want: true},
		{name: "the name in any case", out: AudioOutput{Name: "my airpods", Transport: TransportBluetooth}, want: true},
		{name: "other bluetooth headphones", out: AudioOutput{Name: "WH-1000XM5", Transport: TransportBluetooth}, want: false},
		{name: "built-in speakers", out: AudioOutput{Name: "MacBook Pro Speakers", Transport: TransportBuiltIn}, want: false},
		{name: "an airpods name off bluetooth", out: AudioOutput{Name: "AirPods", Transport: TransportUSB}, want: false},
		{name: "no transport", out: AudioOutput{Name: "AirPods"}, want: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := ListeningDevice(tc.out)

			// Assert
			if got != tc.want {
				t.Fatalf("ListeningDevice(%+v) = %v, want %v", tc.out, got, tc.want)
			}
		})
	}
}

func TestParseAudioOutput(t *testing.T) {
	cases := []struct {
		name    string
		stdout  string
		want    AudioOutput
		wantErr string
	}{
		{name: "the default output", stdout: airpodsReport, want: AudioOutput{Name: "Dodge's AirPods Pro", Transport: TransportBluetooth}},
		{name: "not json", stdout: "garbage", wantErr: "decode the audio report"},
		{name: "no default output", stdout: `{"SPAudioDataType":[{"_items":[{"_name":"Mic","coreaudio_default_audio_input_device":"spaudio_yes"}]}]}`, wantErr: "names no default output device"},
		{name: "no devices", stdout: `{"SPAudioDataType":[]}`, wantErr: "names no default output device"},
		{name: "two default outputs", stdout: `{"SPAudioDataType":[{"_items":[{"_name":"A","coreaudio_default_audio_output_device":"spaudio_yes"},{"_name":"B","coreaudio_default_audio_output_device":"spaudio_yes"}]}]}`, wantErr: "names 2 default output devices"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, err := parseAudioOutput(tc.stdout)

			// Assert
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("parseAudioOutput err = %v, want %q", err, tc.wantErr)
				}
				return
			}
			if err != nil || got != tc.want {
				t.Fatalf("parseAudioOutput = %+v, %v, want %+v", got, err, tc.want)
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
		{name: "macOS speaks through say and reads CoreAudio's report", goos: "darwin", want: SpeechPlatform{Say: "/usr/bin/say", AudioOutput: "/usr/sbin/system_profiler", AudioOutputArgs: []string{"SPAudioDataType", "-json"}}},
		{name: "linux speaks nothing", goos: "linux", want: SpeechPlatform{}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := SpeechPlatformFor(tc.goos)

			// Assert
			if !reflect.DeepEqual(got, tc.want) {
				t.Fatalf("SpeechPlatformFor(%s) = %+v, want %+v", tc.goos, got, tc.want)
			}
		})
	}
}

func TestSpeechPlatformWithOverrides(t *testing.T) {
	cases := []struct {
		name            string
		platform        SpeechPlatform
		say, audio      string
		wantSay, wantAO string
	}{
		{name: "both overridden", platform: darwinSpeech, say: "/fake/say", audio: "/fake/audio", wantSay: "/fake/say", wantAO: "/fake/audio"},
		{name: "none overridden", platform: darwinSpeech, wantSay: "/usr/bin/say", wantAO: "/usr/sbin/system_profiler"},
		{name: "an unsupported platform stays silent", platform: SpeechPlatform{}, say: "/fake/say", audio: "/fake/audio"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := tc.platform.WithOverrides(tc.say, tc.audio)

			// Assert
			if got.Say != tc.wantSay || got.AudioOutput != tc.wantAO {
				t.Fatalf("WithOverrides = %+v, want say %q and audio %q", got, tc.wantSay, tc.wantAO)
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
