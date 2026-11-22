package harness

// THE IMPACT RECORDER cuts the daemons' coverage counters into one window per
// top-level test, for the test runner's coverage-based test impact selection
// (testrun/internal/impact), which runs in SHADOW MODE: it records and
// measures, and never skips a test.
//
// It is INERT unless the run names a file (testenv.ImpactOut): nothing is
// cut, no daemon is asked anything, and the processes are the ordinary ones.
//
// WHY A WINDOW HOLDS EXACTLY ONE TEST. A recorded unit runs with
// -test.parallel=1, so the process runs one top-level test at a time, and the
// harness switches windows only when a DIFFERENT top-level test first asks it
// for a daemon (View, StartDaemon): the previous test's work is over by then,
// and no other test's work has begun. Every daemon a test can reach comes
// through one of those two calls, so a test that never calls either reached
// no daemon, and its window is empty.
//
// TWO KINDS OF DAEMON, TWO CUTS.
//
//   - The suite's SHARED daemons (the canonical daemon and each group's) serve
//     many tests in turn. Each is cut through its coverage window route
//     (internal/server.CoverageWindowRoute): its boot's counters are written
//     once and credited to every test that uses it; each test's window begins
//     by clearing the counters (the first time the test views the daemon) and
//     ends by writing them.
//   - A test's OWN daemons (StartDaemon on the test) run under their own
//     GOCOVERDIR, which the process writes whole as it leaves: the daemon
//     exits gracefully at the test's end (coverageTeardownArmed), and its
//     directory is the test's window. A daemon killed instead writes no
//     counters, so the window says it is incomplete, and the runner never
//     records the test.
//
// Each window is reduced AT ONCE to the set of files it executed, and the raw
// counters are removed: nothing but file names leaves this file.

import (
	"fmt"
	"net/http"
	"net/url"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"

	"agentrepl/testrun/testenv"
)

// coverageWindowsEnv serves a daemon its coverage window route
// (server.Deps.TestCoverageWindows).
const coverageWindowsEnv = "AGENT_REPL_TEST_COVERAGE_WINDOWS"

// coverageWindowRoute is internal/server.CoverageWindowRoute.
const coverageWindowRoute = "/_test/coverage-window"

// ImpactEnabled reports whether this run records the files each top-level
// test executed.
func ImpactEnabled() bool { return os.Getenv(testenv.ImpactOut) != "" }

// coverageTeardownArmed is whether a daemon must leave through SIGTERM at its
// test's end, so the process writes its counters: a coverage run, and an
// impact-recording run.
func coverageTeardownArmed() bool { return CoverageEnabled() || ImpactEnabled() }

// covReducer is what an impact recorder reduces coverage with
// (testenv.CovReducer).
type covReducer interface {
	Reduce(metaDir string, counterDirs ...string) ([]string, float64, error)
}

// windowAPI is a shared daemon's coverage window route as the recorder asks
// it, so the recorder is tested without a cover-built daemon.
type windowAPI interface {
	Do(op, dir string) error
}

// httpWindowAPI asks a daemon's route over its loopback address.
type httpWindowAPI struct {
	addr   string
	client *http.Client
}

func (a httpWindowAPI) Do(op, dir string) error {
	q := url.Values{"op": {op}}
	if dir != "" {
		q.Set("dir", dir)
	}
	resp, err := a.client.Post("http://"+a.addr+coverageWindowRoute+"?"+q.Encode(), "text/plain", nil)
	if err != nil {
		return fmt.Errorf("harness: coverage window %s on %s: %w", op, a.addr, err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusNoContent {
		return fmt.Errorf("harness: coverage window %s on %s answered %s", op, a.addr, resp.Status)
	}
	return nil
}

// sharedCov is one shared daemon's coverage state.
type sharedCov struct {
	api     windowAPI
	metaDir string
	boot    []string
	// opened are the top-level tests whose window on this daemon is open.
	opened map[string]bool
}

// impactRecorder owns the windows of one test process.
type impactRecorder struct {
	root    string // scratch directory for counters and meta-data, removed with the run root
	reducer covReducer
	emit    func(testenv.ImpactWindow) error
	// report says a recording problem loudly, on the process's stderr. It
	// never fails the run: shadow mode must not stand between a change and its
	// landing, so the test whose window the problem touched is marked
	// incomplete instead (problem), which the runner never records.
	report func(format string, args ...any)
	// pending are the problems of the current test's window so far.
	pending []string

	mu      sync.Mutex
	current string
	shared  map[string]*sharedCov
	// used are the shared daemons the current test opened a window on.
	used []*sharedCov
	// fresh are the GOCOVERDIRs of the current test's own daemons.
	fresh []string
	seq   int
}

func newImpactRecorder(root string, reducer covReducer, emit func(testenv.ImpactWindow) error, report func(string, ...any)) *impactRecorder {
	return &impactRecorder{root: root, reducer: reducer, emit: emit, report: report, shared: map[string]*sharedCov{}}
}

// problem reports a recording problem and marks the current test's window
// incomplete.
func (r *impactRecorder) problem(format string, args ...any) {
	msg := fmt.Sprintf(format, args...)
	r.report("%s", msg)
	r.pending = append(r.pending, msg)
}

// topLevel is the top-level test a (sub)test name belongs to.
func topLevel(name string) string {
	top, _, _ := strings.Cut(name, "/")
	return top
}

func (r *impactRecorder) dir(kind string) (string, error) {
	r.seq++
	d := filepath.Join(r.root, fmt.Sprintf("%s-%03d", kind, r.seq))
	if err := os.MkdirAll(d, 0o755); err != nil {
		return "", fmt.Errorf("harness: make the impact scratch directory %s: %w", d, err)
	}
	return d, nil
}

// switchTo closes the current test's window when test is a different
// top-level test, and makes test current.
func (r *impactRecorder) switchTo(test string) {
	if r.current == test {
		return
	}
	r.closeLocked()
	r.current = test
}

// UseShared is the first call of a test that views the shared daemon named
// key (api serves its window route): it cuts the daemon's boot on the daemon's
// first use and opens the test's window on it.
func (r *impactRecorder) UseShared(test, key string, api windowAPI) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.switchTo(test)
	s := r.shared[key]
	if s == nil {
		s = &sharedCov{api: api, opened: map[string]bool{}}
		bootCPU, err := r.cutBoot(s)
		if err != nil {
			r.problem("%v", err)
			return
		}
		r.shared[key] = s
		// The boot's own cost is the recording's overhead, and nobody's reach:
		// each test that uses the daemon is credited the files it executed.
		if err := r.emit(testenv.ImpactWindow{CPUSeconds: bootCPU}); err != nil {
			r.problem("harness: record a shared daemon's boot cost: %v", err)
		}
	}
	if s.opened[test] {
		return
	}
	if err := s.api.Do("clear", ""); err != nil {
		r.problem("%v", err)
		return
	}
	s.opened[test] = true
	r.used = append(r.used, s)
}

// cutBoot writes the daemon's meta-data and the counters its boot made, and
// reduces them to the files its boot executed.
func (r *impactRecorder) cutBoot(s *sharedCov) (float64, error) {
	meta, err := r.dir("meta")
	if err != nil {
		return 0, err
	}
	counters, err := r.dir("boot")
	if err != nil {
		return 0, err
	}
	if err := s.api.Do("meta", meta); err != nil {
		return 0, err
	}
	if err := s.api.Do("counters", counters); err != nil {
		return 0, err
	}
	s.metaDir = meta
	var cpu float64
	s.boot, cpu, err = r.reducer.Reduce(meta, counters)
	if err != nil {
		return 0, fmt.Errorf("harness: reduce a shared daemon's boot window: %w", err)
	}
	return cpu, os.RemoveAll(counters)
}

// Touch makes test current without opening a window on any daemon.
func (r *impactRecorder) Touch(test string) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.switchTo(test)
}

// OwnDaemon is the start of a daemon the test runs for itself: it answers
// the GOCOVERDIR the daemon writes its counters under.
func (r *impactRecorder) OwnDaemon(test string) (string, error) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.switchTo(test)
	d, err := r.dir("own")
	if err != nil {
		return "", err
	}
	r.fresh = append(r.fresh, d)
	return d, nil
}

// Finish closes the last test's window, at the end of the process's tests.
func (r *impactRecorder) Finish() {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.closeLocked()
	r.current = ""
}

// closeLocked ends the current test's window: it writes each used shared
// daemon's counters and reduces them with every own daemon's directory,
// appends the window, and removes the raw counters.
func (r *impactRecorder) closeLocked() {
	if r.current == "" {
		return
	}
	w := testenv.ImpactWindow{Test: r.current}
	executed := map[string]bool{}
	add := func(files []string) {
		for _, f := range files {
			executed[f] = true
		}
	}
	for _, s := range r.used {
		counters, err := r.dir("win")
		if err == nil {
			err = s.api.Do("counters", counters)
		}
		if err != nil {
			r.problem("harness: close %s's window on a shared daemon: %v", r.current, err)
			continue
		}
		files, cpu, err := r.reducer.Reduce(s.metaDir, counters)
		if err != nil {
			r.problem("harness: reduce %s's window on a shared daemon: %v", r.current, err)
			continue
		}
		add(files)
		add(s.boot)
		w.CPUSeconds += cpu
		if err := os.RemoveAll(counters); err != nil {
			r.problem("harness: remove %s's raw counters %s: %v", r.current, counters, err)
		}
	}
	for _, dir := range r.fresh {
		files, cpu, incomplete, err := r.reduceOwn(dir)
		if err != nil {
			r.problem("harness: reduce %s's own daemon's counters %s: %v", r.current, dir, err)
			continue
		}
		if incomplete != "" {
			w.Incomplete = incomplete
		}
		add(files)
		w.CPUSeconds += cpu
		if err := os.RemoveAll(dir); err != nil {
			r.problem("harness: remove %s's raw counters %s: %v", r.current, dir, err)
		}
	}
	for f := range executed {
		w.Executed = append(w.Executed, f)
	}
	if len(r.pending) > 0 {
		w.Incomplete = strings.Join(r.pending, "; ")
	}
	r.pending = nil
	if err := r.emit(w); err != nil {
		r.problem("harness: record %s's window: %v", r.current, err)
	}
	for _, s := range r.shared {
		delete(s.opened, r.current)
	}
	r.used, r.fresh = nil, nil
}

// reduceOwn reduces a test's own daemon's GOCOVERDIR. A directory with
// meta-data and no counters is a daemon that never left gracefully, which the
// window reports as incomplete.
func (r *impactRecorder) reduceOwn(dir string) (files []string, cpu float64, incomplete string, err error) {
	counters, err := filepath.Glob(filepath.Join(dir, "covcounters.*"))
	if err != nil {
		return nil, 0, "", err
	}
	if len(counters) == 0 {
		return nil, 0, "a daemon the test started was killed before it wrote its coverage counters", nil
	}
	files, cpu, err = r.reducer.Reduce(dir, dir)
	return files, cpu, "", err
}

// impact is this process's recorder, nil unless the run records.
var (
	impactOnce sync.Once
	impactRec  *impactRecorder
)

// recorder answers the process's recorder, built on the first call.
func recorder() *impactRecorder {
	if !ImpactEnabled() {
		return nil
	}
	impactOnce.Do(func() {
		reducer, err := testenv.DefaultCovReducer()
		if err != nil {
			fmt.Fprintf(os.Stderr, "harness: impact recording is off for this process (shadow mode never fails a run): %v\n", err)
			return
		}
		impactRec = newImpactRecorder(RunRoot(), reducer,
			func(w testenv.ImpactWindow) error { return testenv.AppendImpactWindow(os.Getenv, w) },
			func(format string, args ...any) {
				fmt.Fprintf(os.Stderr, "harness: impact recording problem (shadow mode never fails a run; the test is recorded as incomplete): "+format+"\n", args...)
			})
	})
	return impactRec
}

// impactUseShared opens test t's window on the shared daemon d, when the run
// records.
//
// ONLY A SUITE-OWNED DAEMON IS CUT THROUGH ITS ROUTE. A daemon a test started
// for itself is stopped by that test's own cleanups, before the next test (or
// the end of the process) closes the test's window, so asking its route then
// is asking a daemon that is gone. Its counters are read from its own
// GOCOVERDIR instead (OwnDaemon), and viewing it only makes the test current.
func impactUseShared(t testing.TB, d *Daemon) {
	if r := recorder(); r != nil {
		r.useDaemon(topLevel(t.Name()), d)
	}
}

// useDaemon is impactUseShared on the recorder r.
func (r *impactRecorder) useDaemon(test string, d *Daemon) {
	if !d.suiteOwned {
		r.Touch(test)
		return
	}
	r.UseShared(test, d.Addr, httpWindowAPI{addr: d.Addr, client: d.HTTP()})
}

// impactOwnDaemonEnv answers the environment a daemon started for test t
// carries when the run records: its own counter directory and its window
// route. Nil otherwise.
func impactOwnDaemonEnv(t testing.TB) []string {
	r := recorder()
	if r == nil {
		return nil
	}
	dir, err := r.OwnDaemon(topLevel(t.Name()))
	if err != nil {
		t.Fatalf("%v", err)
	}
	return []string{"GOCOVERDIR=" + dir}
}

// ImpactFinish closes the last window of this process; Main calls it after
// the tests and before the shared daemons leave.
func ImpactFinish() {
	if r := recorder(); r != nil {
		r.Finish()
	}
}

// daemonProgramName is the roster name of the daemon build this run's tests
// start: the cover-built one when the run records which files each test
// executes.
func daemonProgramName() string {
	if ImpactEnabled() {
		return testenv.ImpactProgram("agent-repld")
	}
	return "agent-repld"
}
