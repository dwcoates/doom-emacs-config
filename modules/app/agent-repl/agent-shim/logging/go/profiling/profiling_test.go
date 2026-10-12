package profiling

import (
	"encoding/json"
	"os"
	"path/filepath"
	"strconv"
	"syscall"
	"testing"
	"time"
)

const fixturePath = "../../../../proto/vocab/profile-session.json"

type sessionFixture struct {
	RootEnv     string `json:"root_env"`
	DefaultRoot string `json:"default_root"`
	ActiveFile  string `json:"active_file"`
	ArmedDir    string `json:"armed_dir"`
	Signal      string `json:"signal"`
	MaxSeconds  int    `json:"max_seconds"`
	Sessions    []struct {
		ID    string `json:"id"`
		Valid bool   `json:"valid"`
	} `json:"sessions"`
}

func loadFixture(t *testing.T) sessionFixture {
	t.Helper()
	raw, err := os.ReadFile(fixturePath)
	if err != nil {
		t.Fatalf("read %s: %v", fixturePath, err)
	}
	var f sessionFixture
	if err := json.Unmarshal(raw, &f); err != nil {
		t.Fatalf("parse %s: %v", fixturePath, err)
	}
	return f
}

// fakeTimer records the max-duration stop a profile armed.
type fakeTimer struct{ stopped bool }

func (f *fakeTimer) Stop() bool { f.stopped = true; return true }

// harness is a Profiler over a temp root with recorded events and captured
// max-duration stops.
type harness struct {
	root   string
	p      *Profiler
	events []Event
	armed  []func()
}

func newHarness(t *testing.T) *harness {
	t.Helper()
	h := &harness{root: t.TempDir()}
	p, err := New(Options{
		Runtime: "daemon",
		Root:    h.root,
		Events:  func(e Event) { h.events = append(h.events, e) },
		After: func(_ time.Duration, f func()) Timer {
			h.armed = append(h.armed, f)
			return &fakeTimer{}
		},
	})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	h.p = p
	t.Cleanup(func() { p.Stop("test_cleanup") })
	return h
}

func (h *harness) open(t *testing.T, session string) {
	t.Helper()
	if err := os.WriteFile(filepath.Join(h.root, ActiveFile), []byte(session+"\n"), 0o644); err != nil {
		t.Fatal(err)
	}
}

func (h *harness) close(t *testing.T) {
	t.Helper()
	if err := os.Remove(filepath.Join(h.root, ActiveFile)); err != nil {
		t.Fatal(err)
	}
}

func (h *harness) kinds() []EventKind {
	out := make([]EventKind, len(h.events))
	for i, e := range h.events {
		out[i] = e.Kind
	}
	return out
}

func requireNonEmpty(t *testing.T, path string) {
	t.Helper()
	info, err := os.Stat(path)
	if err != nil {
		t.Fatalf("stat %s: %v", path, err)
	}
	if info.Size() == 0 {
		t.Fatalf("%s is empty, want a profile", path)
	}
}

func TestTheContractMatchesTheSharedFixture(t *testing.T) {
	// Arrange
	f := loadFixture(t)

	// Act
	sig, _ := Signal.(syscall.Signal)

	// Assert
	if f.RootEnv != RootEnvironment || f.ActiveFile != ActiveFile || f.MaxSeconds != int(MaxDuration/time.Second) {
		t.Fatalf("constants = %q %q %v, fixture %q %q %ds", RootEnvironment, ActiveFile, MaxDuration, f.RootEnv, f.ActiveFile, f.MaxSeconds)
	}
	if f.ArmedDir != ArmedDir {
		t.Fatalf("ArmedDir = %q, fixture %q", ArmedDir, f.ArmedDir)
	}
	if f.DefaultRoot != "~/"+DefaultRoot {
		t.Fatalf("DefaultRoot = %q, fixture %q", DefaultRoot, f.DefaultRoot)
	}
	if f.Signal != "SIGWINCH" || sig != syscall.SIGWINCH {
		t.Fatalf("Signal = %v, fixture %q", Signal, f.Signal)
	}
}

func TestValidSessionJudgesEveryFixtureSession(t *testing.T) {
	for _, tt := range loadFixture(t).Sessions {
		t.Run(tt.ID, func(t *testing.T) {
			// Act
			got := ValidSession(tt.ID)

			// Assert
			if got != tt.Valid {
				t.Fatalf("ValidSession(%q) = %v, want %v", tt.ID, got, tt.Valid)
			}
		})
	}
}

func TestRoot(t *testing.T) {
	tests := []struct {
		name    string
		env     string
		home    string
		want    string
		wantErr bool
	}{
		{name: "the override wins", env: "/override", home: "/home/u", want: "/override"},
		{name: "the default is under home", home: "/home/u", want: "/home/u/.cache/agent-repl/profiles"},
		{name: "no home and no override is refused", wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			getenv := func(k string) string {
				if k == RootEnvironment {
					return tt.env
				}
				return ""
			}

			// Act
			got, err := Root(getenv, tt.home)

			// Assert
			if (err != nil) != tt.wantErr || got != tt.want {
				t.Fatalf("Root = %q, %v; want %q, error %v", got, err, tt.want, tt.wantErr)
			}
		})
	}
}

func TestNewRefusesIncompleteOptions(t *testing.T) {
	events := func(Event) {}
	tests := []struct {
		name string
		opts Options
	}{
		{name: "no runtime", opts: Options{Root: "/r", Events: events}},
		{name: "no root", opts: Options{Runtime: "daemon", Events: events}},
		{name: "no events", opts: Options{Runtime: "daemon", Root: "/r"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			_, err := New(tt.opts)

			// Assert
			if err == nil {
				t.Fatal("New accepted incomplete options")
			}
		})
	}
}

func TestReconcileStartsAProfileIntoTheActiveSession(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.open(t, "s1")

	// Act
	h.p.Reconcile()

	// Assert
	if h.p.Session() != "s1" || len(h.events) != 1 || h.events[0].Kind != Started {
		t.Fatalf("session %q, events %v; want s1 and one start", h.p.Session(), h.kinds())
	}
	want := filepath.Join(h.root, "s1", "daemon-"+strconv.Itoa(os.Getpid())+".cpu.pprof")
	if h.events[0].Files[0] != want {
		t.Fatalf("cpu profile = %q, want %q", h.events[0].Files[0], want)
	}
}

func TestReconcileWithTheSessionClosedWritesBothProfiles(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.open(t, "s1")
	h.p.Reconcile()
	h.close(t)

	// Act
	h.p.Reconcile()

	// Assert
	stop := h.events[len(h.events)-1]
	if stop.Kind != Stopped || stop.Reason != "session_closed" || len(stop.Files) != 2 {
		t.Fatalf("last event = %+v, want a session_closed stop with two files", stop)
	}
	for _, f := range stop.Files {
		requireNonEmpty(t, f)
	}
	if h.p.Session() != "" {
		t.Fatalf("session = %q after the stop, want none", h.p.Session())
	}
}

func TestReconcileOnAChangedSessionMovesTheProfile(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.open(t, "s1")
	h.p.Reconcile()
	h.open(t, "s2")

	// Act
	h.p.Reconcile()

	// Assert
	got := h.kinds()
	if len(got) != 3 || got[1] != Stopped || h.events[1].Reason != "session_changed" || got[2] != Started || h.p.Session() != "s2" {
		t.Fatalf("events %v, session %q; want start, session_changed stop, start into s2", got, h.p.Session())
	}
}

func TestReconcileIsIdempotentForTheRunningSession(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.open(t, "s1")
	h.p.Reconcile()

	// Act
	h.p.Reconcile()

	// Assert
	if len(h.events) != 1 {
		t.Fatalf("events %v, want the one start", h.kinds())
	}
}

func TestReconcileWithNoSessionStartsNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.p.Reconcile()

	// Assert
	if len(h.events) != 0 || h.p.Session() != "" {
		t.Fatalf("events %v, session %q; want nothing", h.kinds(), h.p.Session())
	}
}

func TestReconcileRefusesAnInvalidSessionLoudly(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.open(t, "../escape")

	// Act
	h.p.Reconcile()

	// Assert
	if len(h.events) != 1 || h.events[0].Kind != Failed || h.events[0].Err == nil || h.p.Session() != "" {
		t.Fatalf("events %+v; want one failure and nothing started", h.events)
	}
}

func TestTheMaxDurationStopEndsTheProfile(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.open(t, "s1")
	h.p.Reconcile()

	// Act
	h.armed[0]()

	// Assert
	stop := h.events[len(h.events)-1]
	if stop.Kind != Stopped || stop.Reason != "max_duration" || h.p.Session() != "" {
		t.Fatalf("last event = %+v, session %q; want a max_duration stop", stop, h.p.Session())
	}
}

func TestAnEarlierProfilesMaxDurationStopLeavesALaterOneRunning(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.open(t, "s1")
	h.p.Reconcile()
	h.open(t, "s2")
	h.p.Reconcile()

	// Act
	h.armed[0]()

	// Assert
	if h.p.Session() != "s2" || len(h.events) != 3 {
		t.Fatalf("session %q, events %v; want s2 still running", h.p.Session(), h.kinds())
	}
}

func TestStopWithNothingRunningRecordsNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.p.Stop("exit")

	// Assert
	if len(h.events) != 0 {
		t.Fatalf("events %v, want none", h.kinds())
	}
}

func TestWatchReconcilesOnTheSignal(t *testing.T) {
	// Arrange: events arrive on a channel, because the loop runs on its own
	// goroutine.
	root := t.TempDir()
	if err := os.WriteFile(filepath.Join(root, ActiveFile), []byte("s1\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	events := make(chan Event, 4)
	p, err := New(Options{Runtime: "daemon", Root: root, Events: func(e Event) { events <- e },
		After: func(time.Duration, func()) Timer { return &fakeTimer{} }})
	if err != nil {
		t.Fatal(err)
	}
	ch := make(chan os.Signal, 1)
	done := make(chan struct{})
	finished := make(chan struct{})
	go func() { watch(p, done, ch); close(finished) }()

	// Act
	ch <- Signal
	started := <-events
	close(done)
	<-finished
	stopped := <-events

	// Assert
	if started.Kind != Started || stopped.Kind != Stopped || stopped.Reason != "exit" {
		t.Fatalf("events %+v then %+v; want a start on the signal and an exit stop", started, stopped)
	}
}

func TestWatchStopsTheProfileWhenDone(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.open(t, "s1")
	h.p.Reconcile()
	done := make(chan struct{})
	close(done)

	// Act
	watch(h.p, done, make(chan os.Signal))

	// Assert
	stop := h.events[len(h.events)-1]
	if stop.Kind != Stopped || stop.Reason != "exit" {
		t.Fatalf("last event = %+v, want an exit stop", stop)
	}
}

func TestWatchHoldsTheArmedMarkerWhileItListens(t *testing.T) {
	// Arrange
	root := t.TempDir()
	p, err := New(Options{Runtime: "store", Root: root, Events: func(Event) {}})
	if err != nil {
		t.Fatal(err)
	}
	marker, err := p.arm()

	// Act
	_, statErr := os.Stat(marker)

	// Assert
	if err != nil || statErr != nil || marker != filepath.Join(root, ArmedDir, "store-"+strconv.Itoa(os.Getpid())) {
		t.Fatalf("arm = %q, %v; stat %v; want the runtime's marker on disk", marker, err, statErr)
	}
}

func TestWatchRemovesTheArmedMarkerWhenItStops(t *testing.T) {
	// Arrange
	root := t.TempDir()
	p, err := New(Options{Runtime: "store", Root: root, Events: func(Event) {}})
	if err != nil {
		t.Fatal(err)
	}
	done := make(chan struct{})
	close(done)

	// Act
	Watch(p, done)

	// Assert
	entries, err := os.ReadDir(filepath.Join(root, ArmedDir))
	if err != nil || len(entries) != 0 {
		t.Fatalf("armed directory = %v, %v; want it empty once Watch returned", entries, err)
	}
}

func TestDisarmRecordsAFailedRemoval(t *testing.T) {
	// Arrange: a marker path that is a non-empty directory cannot be removed.
	root := t.TempDir()
	var got []Event
	p, err := New(Options{Runtime: "store", Root: root, Events: func(e Event) { got = append(got, e) }})
	if err != nil {
		t.Fatal(err)
	}
	marker := filepath.Join(root, "stuck")
	if err := os.MkdirAll(filepath.Join(marker, "inside"), 0o755); err != nil {
		t.Fatal(err)
	}

	// Act
	p.disarm(marker)

	// Assert
	if len(got) != 1 || got[0].Kind != Failed {
		t.Fatalf("events = %+v, want the failed removal recorded", got)
	}
}
