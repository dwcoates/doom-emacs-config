package harness

import (
	"fmt"
	"runtime"
	"strings"
	"sync"
	"testing"
)

// RecordingTB is a test that code under test may fail without failing the
// real test: it records Errorf and Fatalf, ends the calling goroutine on a
// Fatal exactly as the testing package does (so run the act through Within),
// and keeps its cleanups for the test to run when it chooses (RunCleanups).
// Every other method is the real test's.
//
// It exists for the tests OF test machinery (a sweep, a shared fixture, a
// guard) whose subject is precisely that some test gets failed.
type RecordingTB struct {
	testing.TB
	name string

	mu       sync.Mutex
	errors   []string
	cleanups []func()
}

// NewRecordingTB is a RecordingTB named name over the real test t.
func NewRecordingTB(t *testing.T, name string) *RecordingTB {
	return &RecordingTB{TB: t, name: name}
}

func (r *RecordingTB) Name() string { return r.name }

func (r *RecordingTB) Helper() {}

func (r *RecordingTB) Cleanup(f func()) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.cleanups = append(r.cleanups, f)
}

func (r *RecordingTB) Errorf(format string, args ...any) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.errors = append(r.errors, fmt.Sprintf(format, args...))
}

func (r *RecordingTB) Fatalf(format string, args ...any) {
	r.Errorf(format, args...)
	runtime.Goexit()
}

func (r *RecordingTB) Skipf(format string, args ...any) {
	r.Errorf("SKIP: "+format, args...)
	runtime.Goexit()
}

func (r *RecordingTB) Failed() bool {
	r.mu.Lock()
	defer r.mu.Unlock()
	return len(r.errors) > 0
}

// RunCleanups runs the recorded cleanups last-registered-first, as the
// testing package would at the test's end.
func (r *RecordingTB) RunCleanups() {
	r.mu.Lock()
	cleanups := r.cleanups
	r.cleanups = nil
	r.mu.Unlock()
	for i := len(cleanups) - 1; i >= 0; i-- {
		cleanups[i]()
	}
}

// Failures is every recorded failure, one per line; a Skipf is recorded with
// a "SKIP: " prefix.
func (r *RecordingTB) Failures() string {
	r.mu.Lock()
	defer r.mu.Unlock()
	return strings.Join(r.errors, "\n")
}

// Within runs act on its own goroutine, so a Fatalf or Skipf ends act and not
// the test, and answers whether act ran to its end.
func Within(act func()) (completed bool) {
	done := make(chan bool, 1)
	go func() {
		finished := false
		defer func() { done <- finished }()
		act()
		finished = true
	}()
	return <-done
}
