package deploy

import (
	"context"
	"errors"
	"strings"
	"sync"
	"testing"
)

// scriptedRunner answers each command from a script keyed by its argv, and
// records every argv it was asked to run. It is how launchctl and the build
// commands are driven with NO real process behind them.
type scriptedRunner struct {
	mu      sync.Mutex
	answers map[string]runAnswer
	calls   [][]string
	dirs    []string
}

type runAnswer struct {
	out  string
	code int
	err  error
}

func (r *scriptedRunner) Run(_ context.Context, dir string, argv []string) (string, int, error) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.calls = append(r.calls, append([]string(nil), argv...))
	r.dirs = append(r.dirs, dir)
	a := r.answers[strings.Join(argv, " ")]
	return a.out, a.code, a.err
}

func (r *scriptedRunner) Calls() [][]string {
	r.mu.Lock()
	defer r.mu.Unlock()
	return append([][]string(nil), r.calls...)
}

func newLaunchctl(answers map[string]runAnswer) (*Launchctl, *scriptedRunner) {
	runner := &scriptedRunner{answers: answers}
	return &Launchctl{Binary: "launchctl", UID: 501, Dir: "/", Runner: runner}, runner
}

func TestLaunchctlPrint(t *testing.T) {
	tests := []struct {
		name       string
		answer     runAnswer
		wantLoaded bool
		wantPID    int
		wantErr    string
	}{
		{name: "a running service", answer: runAnswer{out: "gui/501/x = {\n\tpid = 4242\n}\n"}, wantLoaded: true, wantPID: 4242},
		{name: "loaded, not running", answer: runAnswer{out: "gui/501/x = {\n\tstate = waiting\n}\n"}, wantLoaded: true},
		{name: "not in the domain", answer: runAnswer{out: "Could not find service \"x\" in domain for user gui: 501", code: 113}},
		{name: "exit 113 is not loaded whatever launchctl prints", answer: runAnswer{out: "Bad request.", code: 113}},
		{name: "any other failure is an error", answer: runAnswer{out: "Operation not permitted", code: 1}, wantErr: "exited 1"},
		{name: "a run that could not start is an error", answer: runAnswer{err: errors.New("exec: not found")}, wantErr: "not found"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			l, _ := newLaunchctl(map[string]runAnswer{"launchctl print gui/501/x": tc.answer})

			// Act
			loaded, pid, err := l.Print(context.Background(), "x")

			// Assert
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("Print error = %v, want %q", err, tc.wantErr)
				}
				return
			}
			if err != nil || loaded != tc.wantLoaded || pid != tc.wantPID {
				t.Fatalf("Print = %v, %d, %v; want %v, %d", loaded, pid, err, tc.wantLoaded, tc.wantPID)
			}
		})
	}
}

func TestLaunchctlVerbs(t *testing.T) {
	tests := []struct {
		name string
		call func(l *Launchctl) error
		argv string
	}{
		{name: "kickstart restarts in place", call: func(l *Launchctl) error { return l.Kickstart(context.Background(), "x") }, argv: "launchctl kickstart -k gui/501/x"},
		{name: "bootout leaves the domain", call: func(l *Launchctl) error { return l.Bootout(context.Background(), "x") }, argv: "launchctl bootout gui/501/x"},
		{name: "bootstrap loads the plist", call: func(l *Launchctl) error { return l.Bootstrap(context.Background(), "/p/x.plist") }, argv: "launchctl bootstrap gui/501 /p/x.plist"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			l, runner := newLaunchctl(map[string]runAnswer{})

			// Act
			err := tc.call(l)

			// Assert
			if err != nil {
				t.Fatalf("verb: %v", err)
			}
			calls := runner.Calls()
			if len(calls) != 1 || strings.Join(calls[0], " ") != tc.argv {
				t.Fatalf("argv = %v, want %q", calls, tc.argv)
			}
		})
	}
}

func TestALaunchctlVerbThatExitsNonZeroFails(t *testing.T) {
	// Arrange
	l, _ := newLaunchctl(map[string]runAnswer{"launchctl kickstart -k gui/501/x": {out: "no such service", code: 3}})

	// Act
	err := l.Kickstart(context.Background(), "x")

	// Assert
	if err == nil || !strings.Contains(err.Error(), "no such service") {
		t.Fatalf("Kickstart = %v, want the exit and launchctl's words", err)
	}
}

func TestLaunchctlBootoutAnswers(t *testing.T) {
	tests := []struct {
		name          string
		answer        runAnswer
		wantErr       bool
		wantNotLoaded bool
	}{
		{name: "a loaded service is booted out", answer: runAnswer{}},
		{name: "exit 113 is a service the domain no longer holds", answer: runAnswer{out: "Boot-out failed: 113: Could not find specified service", code: 113}, wantErr: true, wantNotLoaded: true},
		{name: "any other exit is a failed bootout", answer: runAnswer{out: "Boot-out failed: 5: Input/output error", code: 5}, wantErr: true},
		{name: "a run that could not start is a failed bootout", answer: runAnswer{err: errors.New("exec: not found")}, wantErr: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			l, _ := newLaunchctl(map[string]runAnswer{"launchctl bootout gui/501/x": tc.answer})

			// Act
			err := l.Bootout(context.Background(), "x")

			// Assert
			if (err != nil) != tc.wantErr || errors.Is(err, ErrServiceNotLoaded) != tc.wantNotLoaded {
				t.Fatalf("Bootout = %v; want error %v, not-loaded %v", err, tc.wantErr, tc.wantNotLoaded)
			}
		})
	}
}
