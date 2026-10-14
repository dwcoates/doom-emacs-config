package life

import (
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"slices"
	"strings"
	"syscall"
	"testing"
	"time"

	"agentrepl/testrun/reap"
)

// recLog records every line logged.
type recLog struct{ info, errs []string }

func (l *recLog) Infof(format string, args ...any) {
	l.info = append(l.info, fmt.Sprintf(format, args...))
}
func (l *recLog) Errorf(format string, args ...any) {
	l.errs = append(l.errs, fmt.Sprintf(format, args...))
}

func has(lines []string, sub string) bool {
	return slices.ContainsFunc(lines, func(s string) bool { return strings.Contains(s, sub) })
}

// brokenPipe is a pipe whose reader is gone.
func brokenPipe(t *testing.T) *os.File {
	t.Helper()
	r, w, err := os.Pipe()
	if err != nil {
		t.Fatal(err)
	}
	r.Close()
	t.Cleanup(func() { w.Close() })
	return w
}

func TestFallbackWriterWritesToItsPrimaryWhileItIsRead(t *testing.T) {
	// Arrange.
	r, w, err := os.Pipe()
	if err != nil {
		t.Fatal(err)
	}
	defer r.Close()
	defer w.Close()
	path := filepath.Join(t.TempDir(), "run.log")
	fw := &FallbackWriter{Primary: w, Path: path}

	// Act.
	_, err = fw.Write([]byte("line\n"))

	// Assert.
	buf := make([]byte, 5)
	if _, readErr := r.Read(buf); err != nil || readErr != nil || string(buf) != "line\n" {
		t.Fatalf("Write() = %v, read %q (%v), want the line on the primary", err, buf, readErr)
	}
	if _, statErr := os.Stat(path); !os.IsNotExist(statErr) {
		t.Fatal("the fallback file was written while the primary was read")
	}
}

func TestFallbackWriterAppendsToTheRunLogOnceItsRelayIsGone(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "run.log")
	if err := os.WriteFile(path, []byte("before\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	fw := &FallbackWriter{Primary: brokenPipe(t), Path: path}

	// Act.
	_, err1 := fw.Write([]byte("first\n"))
	_, err2 := fw.Write([]byte("second\n"))

	// Assert.
	raw, readErr := os.ReadFile(path)
	if err1 != nil || err2 != nil || readErr != nil || string(raw) != "before\nfirst\nsecond\n" {
		t.Fatalf("Write() = %v, %v; run log %q (%v)", err1, err2, raw, readErr)
	}
}

func TestFallbackWriterWithNoRunLogAnswersTheBrokenPipe(t *testing.T) {
	// Arrange.
	fw := &FallbackWriter{Primary: brokenPipe(t)}

	// Act.
	_, err := fw.Write([]byte("x\n"))

	// Assert.
	if !errors.Is(err, syscall.EPIPE) {
		t.Fatalf("Write() error = %v, want EPIPE", err)
	}
}

func TestFallbackWriterAnswersAnyOtherPrimaryFailure(t *testing.T) {
	// Arrange.
	f, err := os.Open(os.DevNull)
	if err != nil {
		t.Fatal(err)
	}
	defer f.Close()
	path := filepath.Join(t.TempDir(), "run.log")
	fw := &FallbackWriter{Primary: f, Path: path}

	// Act.
	_, err = fw.Write([]byte("x\n"))

	// Assert.
	if err == nil || errors.Is(err, syscall.EPIPE) {
		t.Fatalf("Write() error = %v, want the read-only primary's failure", err)
	}
	if _, statErr := os.Stat(path); !os.IsNotExist(statErr) {
		t.Fatal("a failure other than a broken relay fell back to the run log")
	}
}

func TestWardenMessages(t *testing.T) {
	tests := []struct {
		name      string
		in        string
		wantRoots []string
		wantDone  bool
		wantErr   string
	}{
		{name: "roots then done", in: "root /tmp/a\nroot /tmp/b\ndone\n", wantRoots: []string{"/tmp/a", "/tmp/b"}, wantDone: true},
		{name: "a lifeline cut short", in: "root /tmp/a\n", wantRoots: []string{"/tmp/a"}},
		{name: "nothing", in: ""},
		{name: "a line that is no message", in: "root /tmp/a\nhello\n", wantRoots: []string{"/tmp/a"}, wantErr: `the runner wrote "hello", which is no warden message`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			roots, done, err := wardenMessages(strings.NewReader(tt.in))

			// Assert.
			if !slices.Equal(roots, tt.wantRoots) || done != tt.wantDone {
				t.Fatalf("wardenMessages() = %q, %v", roots, done)
			}
			if (tt.wantErr == "") != (err == nil) || (err != nil && err.Error() != tt.wantErr) {
				t.Fatalf("wardenMessages() error = %v, want %q", err, tt.wantErr)
			}
		})
	}
}

// fakeWarden is a Warden whose reap and release a test scripts.
type fakeWarden struct {
	reaped     []reap.Members
	reapErr    error
	released   []int
	releaseErr error
	log        recLog
}

func (f *fakeWarden) warden() Warden {
	return Warden{
		Runner: 77, Since: time.Unix(1760400000, 0), Sentinel: 76,
		Reap: func(m reap.Members) (reap.Result, error) {
			f.reaped = append(f.reaped, m)
			return reap.Result{}, f.reapErr
		},
		Release: func(runner int) (bool, error) {
			f.released = append(f.released, runner)
			return true, f.releaseErr
		},
		Log: &f.log,
	}
}

func TestWardenLeavesARunnerThatSaidDone(t *testing.T) {
	// Arrange.
	f := &fakeWarden{}

	// Act.
	code := f.warden().Watch(strings.NewReader("root /tmp/x\ndone\n"))

	// Assert.
	if code != 0 || len(f.reaped) != 0 || len(f.released) != 0 || len(f.log.errs) != 0 {
		t.Fatalf("Watch() = %d, reaped %v, released %v, errors %q; want nothing done", code, f.reaped, f.released, f.log.errs)
	}
}

func TestWardenReapsAndReleasesARunnerThatDied(t *testing.T) {
	// Arrange.
	root := filepath.Join(t.TempDir(), "tr-1")
	if err := os.MkdirAll(root, 0o755); err != nil {
		t.Fatal(err)
	}
	f := &fakeWarden{}

	// Act.
	code := f.warden().Watch(strings.NewReader("root " + root + "\n"))

	// Assert.
	if code != 0 {
		t.Fatalf("Watch() = %d, errors %q", code, f.log.errs)
	}
	if len(f.reaped) != 1 {
		t.Fatalf("reaped %d times, want once", len(f.reaped))
	}
	m := f.reaped[0]
	if m.Session != 77 || !m.Since.Equal(time.Unix(1760400000, 0)) || !slices.Contains(m.Dirs, root) || !slices.Contains(m.Except, 76) || !slices.Contains(m.Except, os.Getpid()) {
		t.Fatalf("reaped %+v, want the runner's session, its root, and never the sentinel or the warden", m)
	}
	if !slices.Equal(f.released, []int{77}) {
		t.Fatalf("released %v, want the runner's disk", f.released)
	}
	if _, err := os.Stat(root); !os.IsNotExist(err) {
		t.Fatalf("the dead run's root survives (stat err = %v)", err)
	}
	if !has(f.log.errs, "the runner pid 77 died without releasing its run") || !has(f.log.info, "the warden of run 77 reaped it and released what it held") {
		t.Fatalf("log = %q / %q, want the death at ERROR and the cleanup at INFO", f.log.errs, f.log.info)
	}
}

func TestWardenReapsARunnerWhoseLifelineSaidNonsense(t *testing.T) {
	// Arrange.
	f := &fakeWarden{}

	// Act.
	code := f.warden().Watch(strings.NewReader("hello\ndone\n"))

	// Assert.
	if code != 0 || len(f.reaped) != 1 || !has(f.log.errs, "could not read its runner's lifeline") {
		t.Fatalf("Watch() = %d, reaped %d, errors %q; want the run reaped as dead", code, len(f.reaped), f.log.errs)
	}
}

func TestWardenFailures(t *testing.T) {
	boom := errors.New("boom")
	tests := []struct {
		name    string
		arrange func(f *fakeWarden)
		in      func(root string) string
		// released says the release still ran.
		released bool
	}{
		{name: "the reap fails", arrange: func(f *fakeWarden) { f.reapErr = boom }, in: func(root string) string { return "root " + root + "\n" }, released: true},
		{name: "the release fails", arrange: func(f *fakeWarden) { f.releaseErr = boom }, in: func(root string) string { return "root " + root + "\n" }, released: true},
		{name: "a root cannot be resolved", arrange: func(*fakeWarden) {}, in: func(string) string { return "root /no/such/parent/tr-1\n" }, released: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			root := filepath.Join(t.TempDir(), "tr-1")
			if err := os.MkdirAll(root, 0o755); err != nil {
				t.Fatal(err)
			}
			f := &fakeWarden{}
			tt.arrange(f)

			// Act.
			code := f.warden().Watch(strings.NewReader(tt.in(root)))

			// Assert.
			if code != 1 {
				t.Fatalf("Watch() = %d, want 1", code)
			}
			if tt.released != (len(f.released) == 1) {
				t.Fatalf("released %v", f.released)
			}
			if _, err := os.Stat(root); err != nil {
				t.Fatalf("a root was removed after a failure (stat err = %v)", err)
			}
		})
	}
}

func TestExitCode(t *testing.T) {
	// Arrange.
	exit3 := exec.Command("/bin/sh", "-c", "exit 3").Run()
	killed := exec.Command("/bin/sh", "-c", "kill -9 $$").Run()
	tests := []struct {
		name    string
		err     error
		want    int
		wantLog string
	}{
		{name: "a clean exit", err: nil, want: 0},
		{name: "an exit status", err: exit3, want: 3},
		{name: "a signal", err: killed, want: 137, wantLog: "the runner pid 9 was ended by killed"},
		{name: "a failed wait", err: errors.New("wait failed"), want: 1, wantLog: "wait for the runner pid 9: wait failed"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			log := &recLog{}

			// Act.
			got := exitCode(tt.err, 9, log)

			// Assert.
			if got != tt.want {
				t.Fatalf("exitCode() = %d, want %d", got, tt.want)
			}
			if tt.wantLog != "" && !has(log.errs, tt.wantLog) {
				t.Fatalf("errors = %q, want %q", log.errs, tt.wantLog)
			}
		})
	}
}

// listTable is a table that only lists.
type listTable struct {
	procs []reap.Proc
	err   error
}

func (l listTable) List() ([]reap.Proc, error)                  { return l.procs, l.err }
func (listTable) Signal(int, syscall.Signal) error              { return nil }
func (listTable) AwaitExit([]int, time.Duration) ([]int, error) { return nil, nil }

func TestStartOf(t *testing.T) {
	start := time.Unix(1760400000, 0)
	tests := []struct {
		name    string
		table   listTable
		wantErr string
	}{
		{name: "listed", table: listTable{procs: []reap.Proc{{Pid: 9, Start: start}}}},
		{name: "not listed", table: listTable{}, wantErr: "the runner pid 9 is not in the process table"},
		{name: "the listing fails", table: listTable{err: errors.New("ps failed")}, wantErr: "read when the runner started: ps failed"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got, err := startOf(tt.table, 9)

			// Assert.
			if tt.wantErr != "" {
				if err == nil || err.Error() != tt.wantErr {
					t.Fatalf("startOf() error = %v, want %q", err, tt.wantErr)
				}
				return
			}
			if err != nil || !got.Equal(start) {
				t.Fatalf("startOf() = %v, %v", got, err)
			}
		})
	}
}

func TestStartRunnerRefusesAProcessThatDoesNotLeadItsSession(t *testing.T) {
	// Arrange: this test process was not started by a sentinel.
	if sid, _ := syscall.Getsid(0); sid == os.Getpid() {
		t.Skip("the test process leads its own session")
	}

	// Act.
	_, err := StartRunner(listTable{}, nil, WardenCommand{}, &recLog{})

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "it was not started by testrun's sentinel") {
		t.Fatalf("StartRunner() error = %v, want the refusal", err)
	}
}

// pipeRunner is a Runner whose warden's lifeline is a pipe the test reads.
func pipeRunner(t *testing.T, reapErr error) (*Runner, *os.File, *recLog) {
	t.Helper()
	r, w, err := os.Pipe()
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { r.Close() })
	log := &recLog{}
	return &Runner{
		Own:    reap.Members{What: "run 77", Session: 77, Except: []int{77, 76, 78}},
		log:    log,
		reap:   func(reap.Members) (reap.Result, error) { return reap.Result{}, reapErr },
		warden: w,
	}, r, log
}

func TestFinish(t *testing.T) {
	boom := errors.New("boom")
	closed := make(chan struct{})
	close(closed)
	tests := []struct {
		name       string
		code       int
		reapErr    error
		releaseErr error
		gone       chan struct{}
		want       int
		wantLog    string
	}{
		{name: "a clean run", code: 0, gone: make(chan struct{}), want: 0},
		{name: "a failed suite", code: 3, gone: make(chan struct{}), want: 3},
		{name: "a failed reap", code: 0, reapErr: boom, gone: make(chan struct{}), want: 1},
		{name: "a failed release", code: 0, releaseErr: boom, gone: make(chan struct{}), want: 1, wantLog: "boom"},
		{name: "a cut run", code: 0, gone: closed, want: 1, wantLog: "the run was cut (its sentinel is gone); it reaped its session 77"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			r, lifeline, log := pipeRunner(t, tt.reapErr)

			// Act.
			got := r.Finish(tt.code, tt.gone, func() error { return tt.releaseErr })

			// Assert.
			if got != tt.want {
				t.Fatalf("Finish() = %d, want %d", got, tt.want)
			}
			if tt.wantLog != "" && !has(log.errs, tt.wantLog) {
				t.Fatalf("errors = %q, want %q", log.errs, tt.wantLog)
			}
			msgs, done, err := wardenMessages(lifeline)
			if err != nil || !done || len(msgs) != 0 {
				t.Fatalf("the warden was told %q, done %v (%v); want done", msgs, done, err)
			}
		})
	}
}

func TestFinishAnswersAWardenThatCannotBeTold(t *testing.T) {
	// Arrange.
	r, lifeline, log := pipeRunner(t, nil)
	lifeline.Close()
	if err := r.warden.Close(); err != nil {
		t.Fatal(err)
	}

	// Act.
	got := r.Finish(0, make(chan struct{}), func() error { return nil })

	// Assert.
	if got != 1 || !has(log.errs, `tell the warden "done"`) {
		t.Fatalf("Finish() = %d, errors %q; want the failed telling", got, log.errs)
	}
}

func TestRootTellsTheWardenAndWidensTheNet(t *testing.T) {
	// Arrange.
	r, lifeline, _ := pipeRunner(t, nil)

	// Act.
	err := r.Root("/tmp/tr-1")

	// Assert.
	if err != nil || !slices.Equal(r.Members().Dirs, []string{"/tmp/tr-1"}) {
		t.Fatalf("Root() = %v, dirs %q", err, r.Members().Dirs)
	}
	if err := r.Done(); err != nil {
		t.Fatal(err)
	}
	roots, done, err := wardenMessages(lifeline)
	if err != nil || !done || !slices.Equal(roots, []string{"/tmp/tr-1"}) {
		t.Fatalf("the warden read %q, done %v (%v)", roots, done, err)
	}
}

func TestCancelOnLifelineCancelsAndReapsWhenTheSentinelGoes(t *testing.T) {
	// Arrange.
	r, _, log := pipeRunner(t, nil)
	reaped := make(chan reap.Members, 1)
	r.reap = func(m reap.Members) (reap.Result, error) { reaped <- m; return reap.Result{}, nil }
	gone := make(chan struct{})
	cancelled := make(chan struct{})
	r.CancelOnLifeline(gone, func() { close(cancelled) })

	// Act.
	close(gone)

	// Assert.
	<-cancelled
	m := <-reaped
	if m.Session != 77 || !slices.Contains(m.Except, 76) {
		t.Fatalf("reaped %+v, want the session, never the sentinel", m)
	}
	if !has(log.info, "the sentinel is gone (the run was cut)") {
		t.Fatalf("info = %q", log.info)
	}
}
