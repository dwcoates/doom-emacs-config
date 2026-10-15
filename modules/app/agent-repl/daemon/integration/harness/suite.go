package harness

import (
	"fmt"
	"io"
	"os"
	"strings"
	"sync"
	"testing"
)

// SUITE IS THE TEST A WHOLE SUITE PROCESS IS. A fixture that outlives any one
// test (AGENTS.md "A case may share an expensive fixture, never a slice
// another case reads") is built and owned by it: its temp dirs live under the
// run root, its cleanups run after the last test and before the run root is
// removed, and anything it fails turns the process's exit code red.
//
// It is a testing.TB in everything the harness asks of one. A Fatal or a Skip
// cannot end a goroutine the testing package does not own, so they PANIC with
// SuiteFatal or SuiteSkip; the code that builds a shared fixture recovers
// them on its own goroutine and hands the outcome to every test that asks
// for the fixture (RecoverSuiteBuild). A Fatal anywhere else crashes the
// process, which is the loudest report there is.

// SuiteFatal is what Suite().Fatalf panics with.
type SuiteFatal struct{ Message string }

func (f SuiteFatal) Error() string { return f.Message }

// SuiteSkip is what Suite().Skipf panics with: a precondition the whole
// fixture lacks.
type SuiteSkip struct{ Reason string }

// SuiteTB is the suite process as a testing.TB. The embedded TB is nil: it
// supplies testing.TB's unexported method, and any method SuiteTB does not
// define panics on use rather than doing something quietly wrong.
type SuiteTB struct {
	testing.TB
	out io.Writer

	mu       sync.Mutex
	errors   []string
	cleanups []func()
	tempDirs int
}

var (
	suiteOnce sync.Once
	suiteTB   *SuiteTB
)

// Suite is this process's SuiteTB.
func Suite() *SuiteTB {
	suiteOnce.Do(func() { suiteTB = newSuiteTB(os.Stderr) })
	return suiteTB
}

func newSuiteTB(out io.Writer) *SuiteTB { return &SuiteTB{out: out} }

// Name is what the suite's records, slots and artifacts are filed under.
func (s *SuiteTB) Name() string { return "Suite" }

func (s *SuiteTB) Helper() {}

func (s *SuiteTB) Log(args ...any) { s.Logf("%s", fmt.Sprint(args...)) }

func (s *SuiteTB) Logf(format string, args ...any) {
	fmt.Fprintf(s.out, "suite: "+format+"\n", args...)
}

func (s *SuiteTB) Error(args ...any) { s.Errorf("%s", fmt.Sprint(args...)) }

func (s *SuiteTB) Errorf(format string, args ...any) {
	msg := fmt.Sprintf(format, args...)
	s.mu.Lock()
	s.errors = append(s.errors, msg)
	s.mu.Unlock()
	fmt.Fprintf(s.out, "suite: FAIL: %s\n", msg)
}

func (s *SuiteTB) Fail() { s.Errorf("the suite was failed with no message") }

func (s *SuiteTB) Failed() bool {
	s.mu.Lock()
	defer s.mu.Unlock()
	return len(s.errors) > 0
}

func (s *SuiteTB) Fatal(args ...any) { s.Fatalf("%s", fmt.Sprint(args...)) }

func (s *SuiteTB) Fatalf(format string, args ...any) {
	s.Errorf(format, args...)
	panic(SuiteFatal{Message: fmt.Sprintf(format, args...)})
}

func (s *SuiteTB) FailNow() { s.Fatalf("the suite was stopped with no message") }

func (s *SuiteTB) Skip(args ...any) { s.Skipf("%s", fmt.Sprint(args...)) }

func (s *SuiteTB) Skipf(format string, args ...any) {
	panic(SuiteSkip{Reason: fmt.Sprintf(format, args...)})
}

func (s *SuiteTB) SkipNow() { s.Skipf("the suite's fixture was skipped with no reason") }

func (s *SuiteTB) Skipped() bool { return false }

// Cleanup runs f after the suite's last test, last-registered-first.
func (s *SuiteTB) Cleanup(f func()) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.cleanups = append(s.cleanups, f)
}

// TempDir is a fresh directory under the run root, removed with it.
func (s *SuiteTB) TempDir() string {
	root := RunRoot()
	if root == "" {
		s.Fatalf("harness: the suite has no run root; the suite must run through harness.MainAt or WithRunRoot")
	}
	s.mu.Lock()
	s.tempDirs++
	n := s.tempDirs
	s.mu.Unlock()
	dir, err := os.MkdirTemp(root, fmt.Sprintf("suite-%03d-", n))
	if err != nil {
		s.Fatalf("harness: suite temp dir: %v", err)
	}
	return dir
}

// Setenv would change every test's environment at once.
func (s *SuiteTB) Setenv(key, _ string) {
	s.Fatalf("harness: Setenv(%s) on the suite; a fixture shared by every test never changes their environment", key)
}

// finishSuite finishes the suite's SuiteTB if any test asked for it, and
// answers the exit code it adds.
func finishSuite() int {
	if suiteTB == nil {
		return 0
	}
	return suiteTB.finish()
}

// finish runs the suite's cleanups, last-registered-first, and answers the
// exit code the suite adds: 1 when anything failed it, 0 otherwise. A cleanup
// that panics is recorded as a failure and the rest still run, so one broken
// teardown never leaves the others' processes behind.
func (s *SuiteTB) finish() int {
	s.mu.Lock()
	cleanups := s.cleanups
	s.cleanups = nil
	s.mu.Unlock()
	for i := len(cleanups) - 1; i >= 0; i-- {
		func() {
			defer func() {
				if r := recover(); r != nil {
					if _, fatal := r.(SuiteFatal); !fatal {
						s.Errorf("a suite cleanup panicked: %v", r)
					}
				}
			}()
			cleanups[i]()
		}()
	}
	if !s.Failed() {
		return 0
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	fmt.Fprintf(s.out, "suite: FAIL: the suite's shared fixtures failed %d time(s):\n  %s\n", len(s.errors), strings.Join(s.errors, "\n  "))
	return 1
}

// RecoverSuiteBuild runs build, which builds a shared fixture on Suite(), and
// answers how it ended: nil, the SuiteFatal that stopped it, or the SuiteSkip
// that stopped it. Any other panic is not the suite's to explain and is
// re-raised.
func RecoverSuiteBuild(build func()) (fatal *SuiteFatal, skip *SuiteSkip) {
	defer func() {
		r := recover()
		switch v := r.(type) {
		case nil:
		case SuiteFatal:
			fatal = &v
		case SuiteSkip:
			skip = &v
		default:
			panic(r)
		}
	}()
	build()
	return nil, nil
}
