package harness

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agentrepl/testrun/testenv"
)

// fakeWindowAPI records the route's operations and answers them.
type fakeWindowAPI struct {
	calls []string
	// failOp makes that operation fail.
	failOp string
}

func (f *fakeWindowAPI) Do(op, dir string) error {
	f.calls = append(f.calls, op)
	if op == f.failOp {
		return fmt.Errorf("the daemon refused %s", op)
	}
	return nil
}

// fakeReducer answers a canned file list per call, in order, and records the
// directories it read.
type fakeReducer struct {
	answers [][]string
	cpu     float64
	err     error
	reads   [][]string
}

func (f *fakeReducer) Reduce(meta string, counters ...string) ([]string, float64, error) {
	f.reads = append(f.reads, append([]string{meta}, counters...))
	if f.err != nil {
		return nil, 0, f.err
	}
	files := f.answers[0]
	f.answers = f.answers[1:]
	return files, f.cpu, nil
}

type recorderFixture struct {
	r      *impactRecorder
	api    *fakeWindowAPI
	red    *fakeReducer
	emits  []testenv.ImpactWindow
	fails  []string
	emitEr error
}

func newRecorderFixture(t *testing.T, answers ...[]string) *recorderFixture {
	t.Helper()
	f := &recorderFixture{api: &fakeWindowAPI{}, red: &fakeReducer{answers: answers, cpu: 0.5}}
	f.r = newImpactRecorder(t.TempDir(), f.red,
		func(w testenv.ImpactWindow) error { f.emits = append(f.emits, w); return f.emitEr },
		func(format string, args ...any) { f.fails = append(f.fails, fmt.Sprintf(format, args...)) })
	return f
}

func (f *recorderFixture) window(test string) *testenv.ImpactWindow {
	for i := range f.emits {
		if f.emits[i].Test == test {
			return &f.emits[i]
		}
	}
	return nil
}

func TestTopLevelFoldsSubtestsIntoTheirTest(t *testing.T) {
	tests := []struct{ name, want string }{
		{"TestA", "TestA"},
		{"TestA/sub/deeper", "TestA"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := topLevel(tt.name); got != tt.want {
				t.Fatalf("topLevel(%q) = %q, want %q", tt.name, got, tt.want)
			}
		})
	}
}

func TestImpactRecorderCutsOneWindowPerTestOnASharedDaemon(t *testing.T) {
	// Arrange: the boot reduction, then A's window, then B's.
	f := newRecorderFixture(t, []string{"m/boot.go"}, []string{"m/a.go"}, []string{"m/b.go"})
	// Act
	f.r.UseShared("TestA", "d1", f.api)
	f.r.UseShared("TestB", "d1", f.api)
	f.r.Finish()
	// Assert
	wantCalls := "meta,counters,clear,counters,clear,counters"
	if got := strings.Join(f.api.calls, ","); got != wantCalls {
		t.Fatalf("route calls = %s, want %s: boot cut once, each window cleared on its test's first use and written when the next test takes over", got, wantCalls)
	}
	if a := f.window("TestA"); a == nil || strings.Join(a.Executed, ",") != "m/a.go,m/boot.go" && strings.Join(a.Executed, ",") != "m/boot.go,m/a.go" {
		t.Fatalf("TestA's window = %+v, want its own file and the boot's", a)
	}
	if b := f.window("TestB"); b == nil || len(b.Executed) != 2 {
		t.Fatalf("TestB's window = %+v, want its own file and the boot's", b)
	}
	if boot := f.window(""); boot == nil || boot.CPUSeconds != 0.5 || len(boot.Executed) != 0 {
		t.Fatalf("the boot's window = %+v, want its cost and no reach", boot)
	}
	if len(f.fails) != 0 {
		t.Fatalf("failures = %v, want none", f.fails)
	}
}

func TestImpactRecorderOpensASharedWindowOncePerTest(t *testing.T) {
	// Arrange
	f := newRecorderFixture(t, []string{"m/boot.go"}, []string{"m/a.go"})
	// Act: one test views the daemon twice, as a test with two views does.
	f.r.UseShared("TestA", "d1", f.api)
	f.r.UseShared("TestA", "d1", f.api)
	f.r.Finish()
	// Assert
	if got := strings.Join(f.api.calls, ","); got != "meta,counters,clear,counters" {
		t.Fatalf("route calls = %s, want a single clear for the test's one window", got)
	}
}

func TestImpactRecorderKeepsEachSharedDaemonsWindowApart(t *testing.T) {
	// Arrange: two daemons (the canonical and a group's), each booted once.
	f := newRecorderFixture(t, []string{"m/boot1.go"}, []string{"m/boot2.go"}, []string{"m/one.go"}, []string{"m/two.go"})
	other := &fakeWindowAPI{}
	// Act
	f.r.UseShared("TestA", "d1", f.api)
	f.r.UseShared("TestA", "d2", other)
	f.r.Finish()
	// Assert
	a := f.window("TestA")
	if a == nil || len(a.Executed) != 4 {
		t.Fatalf("TestA's window = %+v, want both daemons' windows and both boots", a)
	}
	if strings.Join(f.api.calls, ",") != "meta,counters,clear,counters" || strings.Join(other.calls, ",") != "meta,counters,clear,counters" {
		t.Fatalf("calls = %v and %v, want each daemon cut on its own", f.api.calls, other.calls)
	}
}

func TestImpactRecorderReducesATestsOwnDaemonDirectory(t *testing.T) {
	cases := []struct {
		name           string
		writeCounters  bool
		wantIncomplete bool
	}{
		{"a daemon that left gracefully has its counters", true, false},
		{"a daemon that was killed wrote none", false, true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newRecorderFixture(t, []string{"m/own.go"})
			dir, err := f.r.OwnDaemon("TestFresh")
			if err != nil {
				t.Fatal(err)
			}
			if tc.writeCounters {
				if err := os.WriteFile(filepath.Join(dir, "covcounters.abc.1.2"), []byte("x"), 0o644); err != nil {
					t.Fatal(err)
				}
			}
			// Act
			f.r.Finish()
			// Assert
			w := f.window("TestFresh")
			if w == nil || (w.Incomplete != "") != tc.wantIncomplete {
				t.Fatalf("window = %+v, want incomplete=%v", w, tc.wantIncomplete)
			}
			if tc.writeCounters && strings.Join(w.Executed, ",") != "m/own.go" {
				t.Fatalf("window = %+v, want the daemon's reduced files", w)
			}
			if _, err := os.Stat(dir); !errors.Is(err, os.ErrNotExist) {
				t.Fatalf("the raw counter directory %s is still there (%v), want it removed once reduced", dir, err)
			}
		})
	}
}

func TestImpactRecorderClosesTheOwnDaemonOfTheTestBeforeTheNext(t *testing.T) {
	// Arrange
	f := newRecorderFixture(t, []string{"m/a.go"})
	dir, err := f.r.OwnDaemon("TestA")
	if err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "covcounters.x"), nil, 0o644); err != nil {
		t.Fatal(err)
	}
	// Act: the next test starts a daemon of its own.
	if _, err := f.r.OwnDaemon("TestB"); err != nil {
		t.Fatal(err)
	}
	// Assert
	if a := f.window("TestA"); a == nil || strings.Join(a.Executed, ",") != "m/a.go" {
		t.Fatalf("TestA's window = %+v, want it closed when TestB took over", a)
	}
	if f.window("TestB") != nil {
		t.Fatal("TestB's window was closed before TestB ended")
	}
}

func TestImpactRecorderFailsLoudly(t *testing.T) {
	cases := []struct {
		name  string
		setup func(f *recorderFixture)
		act   func(f *recorderFixture)
		want  string
	}{
		{"a daemon that refuses the boot cut", func(f *recorderFixture) { f.api.failOp = "meta" },
			func(f *recorderFixture) { f.r.UseShared("TestA", "d1", f.api) }, "the daemon refused meta"},
		{"a daemon that refuses to clear", func(f *recorderFixture) { f.api.failOp = "clear" },
			func(f *recorderFixture) { f.r.UseShared("TestA", "d1", f.api) }, "the daemon refused clear"},
		{"a boot that cannot be reduced", func(f *recorderFixture) { f.red.err = errors.New("covdata exploded") },
			func(f *recorderFixture) { f.r.UseShared("TestA", "d1", f.api) }, "reduce a shared daemon's boot window: covdata exploded"},
		{"a window the daemon cannot write", nil,
			func(f *recorderFixture) {
				f.r.UseShared("TestA", "d1", f.api)
				f.api.failOp = "counters"
				f.r.Finish()
			}, "close TestA's window on a shared daemon: the daemon refused counters"},
		{"a window that cannot be reduced", nil,
			func(f *recorderFixture) {
				f.r.UseShared("TestA", "d1", f.api)
				f.red.err = errors.New("covdata exploded")
				f.r.Finish()
			}, "reduce TestA's window on a shared daemon: covdata exploded"},
		{"a window that cannot be recorded", func(f *recorderFixture) { f.emitEr = errors.New("disk full") },
			func(f *recorderFixture) {
				f.r.UseShared("TestA", "d1", f.api)
				f.r.Finish()
			}, "record TestA's window: disk full"},
		{"an own daemon's counters that cannot be reduced", func(f *recorderFixture) { f.red.err = errors.New("covdata exploded") },
			func(f *recorderFixture) {
				dir, _ := f.r.OwnDaemon("TestA")
				_ = os.WriteFile(filepath.Join(dir, "covcounters.x"), nil, 0o644)
				f.r.Finish()
			}, "reduce TestA's own daemon's counters"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newRecorderFixture(t, []string{"m/boot.go"}, []string{"m/a.go"})
			if tc.setup != nil {
				tc.setup(f)
			}
			// Act
			tc.act(f)
			// Assert
			found := false
			for _, msg := range f.fails {
				found = found || strings.Contains(msg, tc.want)
			}
			if !found {
				t.Fatalf("failures = %q, want one containing %q", f.fails, tc.want)
			}
		})
	}
}

func TestRecorderIsInertUnlessTheRunRecords(t *testing.T) {
	t.Setenv(testenv.ImpactOut, "")
	if ImpactEnabled() || recorder() != nil || coverageTeardownArmed() {
		t.Fatal("a run that names no impact file recorded, or armed the coverage teardown")
	}
	if got := daemonProgramName(); got != "agent-repld" {
		t.Fatalf("daemonProgramName() = %q, want the ordinary build", got)
	}
}

func TestARecordingRunStartsTheCoverBuildAndArmsTheTeardown(t *testing.T) {
	t.Setenv(testenv.ImpactOut, filepath.Join(t.TempDir(), "w.jsonl"))
	if !ImpactEnabled() || !coverageTeardownArmed() {
		t.Fatal("a run that names an impact file did not record")
	}
	if got := daemonProgramName(); got != "agent-repld-impact" {
		t.Fatalf("daemonProgramName() = %q, want the cover build", got)
	}
}

func TestImpactRecorderMarksATestIncompleteWhenItsDaemonIsGoneAtWindowClose(t *testing.T) {
	// Arrange: the daemon refuses the window write, as a daemon that has left does.
	f := newRecorderFixture(t, []string{"m/boot.go"})
	f.r.UseShared("TestA", "d1", f.api)
	f.api.failOp = "counters"

	// Act
	f.r.Finish()

	// Assert: the problem is reported, and the window says it is incomplete
	// so the runner never records the test, instead of the run failing.
	w := f.window("TestA")
	if w == nil || !strings.Contains(w.Incomplete, "close TestA's window on a shared daemon") {
		t.Fatalf("window = %+v, want it incomplete with the cause", w)
	}
	if len(f.fails) != 1 {
		t.Fatalf("reports = %v, want the one problem said", f.fails)
	}
}

func TestImpactRecorderKeepsOneTestsProblemsOutOfTheNextWindow(t *testing.T) {
	// Arrange
	f := newRecorderFixture(t, []string{"m/boot.go"}, []string{"m/b.go"})
	f.r.UseShared("TestA", "d1", f.api)
	f.api.failOp = "counters"
	f.r.UseShared("TestB", "d1", f.api)
	f.api.failOp = ""

	// Act
	f.r.Finish()

	// Assert
	if a, b := f.window("TestA"), f.window("TestB"); a == nil || a.Incomplete == "" || b == nil || b.Incomplete != "" {
		t.Fatalf("windows A=%+v B=%+v, want only A incomplete", a, b)
	}
}

func TestImpactRecorderTouchClosesTheLastTestsWindowWithoutOpeningOne(t *testing.T) {
	// Arrange
	f := newRecorderFixture(t, []string{"m/boot.go"}, []string{"m/a.go"})
	f.r.UseShared("TestA", "d1", f.api)

	// Act: the next test only views a daemon it started itself.
	f.r.Touch("TestB")

	// Assert: A's window closed, B opened none, and no route call was made for B.
	if a := f.window("TestA"); a == nil {
		t.Fatal("TestA's window was not closed when TestB took over")
	}
	if got := strings.Join(f.api.calls, ","); got != "meta,counters,clear,counters" {
		t.Fatalf("route calls = %s, want none beyond A's own window", got)
	}
}

func TestADaemonATestStartsItselfIsNotCutThroughItsRoute(t *testing.T) {
	// Arrange: a view of a daemon the test owns, whose address answers nothing.
	f := newRecorderFixture(t)
	d := &Daemon{Addr: "127.0.0.1:1"}

	// Act: no route call is attempted, so no connection is refused.
	f.r.useDaemon("TestOwn", d)

	// Assert
	if _, cut := f.r.shared[d.Addr]; cut || f.r.current != "TestOwn" || len(f.fails) != 0 {
		t.Fatalf("recorder = cut %v, current %q, problems %v, want the test current and no shared daemon cut", cut, f.r.current, f.fails)
	}
}

func TestASuiteOwnedDaemonIsCutThroughItsRoute(t *testing.T) {
	// Arrange: a suite-owned daemon whose address answers nothing, so the
	// refused connection is the proof the route was asked.
	f := newRecorderFixture(t)
	d := &Daemon{Addr: "127.0.0.1:1", suiteOwned: true}

	// Act
	f.r.useDaemon("TestShared", d)

	// Assert
	if len(f.fails) == 0 || !strings.Contains(f.fails[0], "coverage window meta on 127.0.0.1:1") {
		t.Fatalf("problems = %v, want the route asked of the suite-owned daemon", f.fails)
	}
}
