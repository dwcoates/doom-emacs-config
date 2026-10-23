package command

import (
	"context"
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

// expiredContext is a context whose deadline has passed.
func expiredContext(t *testing.T) context.Context {
	t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), 0)
	t.Cleanup(cancel)
	<-ctx.Done()
	return ctx
}

func TestBoundedSetsItsWaitDelay(t *testing.T) {
	// Act.
	cmd := Bounded(context.Background(), "/bin/echo", "x")

	// Assert.
	if cmd.WaitDelay != WaitDelay {
		t.Fatalf("WaitDelay = %s, want %s", cmd.WaitDelay, WaitDelay)
	}
}

func TestBoundedKillsTheCommandAtItsBound(t *testing.T) {
	// Arrange.
	ctx, cancel := context.WithTimeout(context.Background(), 50*time.Millisecond)
	defer cancel()

	// Act.
	_, err := Bounded(ctx, "/bin/sleep", "300").Output()

	// Assert.
	if got := Outcome(ctx, 50*time.Millisecond, "sleep 300", "", nil, err); got == nil || got.Error() != "sleep 300 did not answer within 50ms: context deadline exceeded" {
		t.Fatalf("Outcome() = %v, want the bound's failure", got)
	}
}

func TestLine(t *testing.T) {
	tests := []struct {
		name string
		args []string
		want string
	}{
		{name: "no arguments", want: "ps"},
		{name: "arguments", args: []string{"-axEo", "pid="}, want: "ps -axEo pid="},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := Line("ps", tt.args)

			// Assert.
			if got != tt.want {
				t.Fatalf("Line() = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestUnansweredError(t *testing.T) {
	tests := []struct {
		name string
		hint string
		want string
	}{
		{name: "without a hint", want: "ps did not answer within 1s: context deadline exceeded"},
		{name: "with a hint", hint: "the disk manager may be wedged", want: "ps did not answer within 1s; the disk manager may be wedged: context deadline exceeded"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			err := &Unanswered{Line: "ps", Bound: time.Second, Hint: tt.hint, Cause: context.DeadlineExceeded}

			// Act.
			got := err.Error()

			// Assert.
			if got != tt.want {
				t.Fatalf("Error() = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestOutcome(t *testing.T) {
	tests := []struct {
		name    string
		expired bool
		printed string
		err     error
		wantErr string
	}{
		{name: "an answer is no error", printed: "listed\n"},
		{name: "a failure names the command and what it printed", printed: "refused\n", err: errors.New("exit status 1"), wantErr: "hdiutil info: exit status 1: refused"},
		{name: "a failure that printed nothing names the command", err: errors.New("exit status 1"), wantErr: "hdiutil info: exit status 1"},
		{name: "a command its bound killed did not answer", expired: true, err: errors.New("signal: killed"), wantErr: "hdiutil info did not answer within 1s; wedged: context deadline exceeded"},
		{name: "a command that answered as its bound passed did not answer", expired: true, printed: "listed\n", wantErr: "hdiutil info did not answer within 1s; wedged: context deadline exceeded"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			ctx := context.Background()
			if tt.expired {
				ctx = expiredContext(t)
			}

			// Act.
			err := Outcome(ctx, time.Second, "hdiutil info", "wedged", []byte(tt.printed), tt.err)

			// Assert.
			got := ""
			if err != nil {
				got = err.Error()
			}
			if got != tt.wantErr {
				t.Fatalf("Outcome() = %q, want %q", got, tt.wantErr)
			}
		})
	}
}

func TestOutcomeKeepsItsCausesInTheChain(t *testing.T) {
	// Arrange.
	own := errors.New("exit status 1")

	// Act.
	unanswered := Outcome(expiredContext(t), time.Second, "ps", "", nil, own)
	failed := Outcome(context.Background(), time.Second, "ps", "", nil, own)

	// Assert.
	var u *Unanswered
	if !errors.As(unanswered, &u) || !errors.Is(unanswered, context.DeadlineExceeded) {
		t.Errorf("unanswered = %v, want an *Unanswered carrying context.DeadlineExceeded", unanswered)
	}
	if !errors.Is(failed, own) {
		t.Errorf("failed = %v, want the command's own error in its chain", failed)
	}
}

// TestEveryBoundedCommandGoesThroughBoundedAndOutcome holds every bounded
// command of testrun to the one shape: no source but this package's starts a
// command under a context, and every source starting one through Bounded
// judges it through Outcome.
func TestEveryBoundedCommandGoesThroughBoundedAndOutcome(t *testing.T) {
	// Arrange.
	var bounded []string

	// Act.
	err := filepath.WalkDir("../..", func(path string, d fs.DirEntry, err error) error {
		if err != nil || d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return err
		}
		src, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		body := string(src)
		if strings.Contains(body, "exec.CommandContext(") && filepath.Base(path) != "bounded.go" {
			t.Errorf("%s starts a command under a context by hand; want command.Bounded", path)
		}
		if strings.Contains(body, "command.Bounded(") {
			bounded = append(bounded, path)
			if !strings.Contains(body, "command.Outcome(") {
				t.Errorf("%s judges a bounded command by hand; want command.Outcome", path)
			}
		}
		return nil
	})

	// Assert.
	if err != nil {
		t.Fatal(err)
	}
	if len(bounded) < 2 {
		t.Fatalf("found command.Bounded in %v, want the RAM disk's two files", bounded)
	}
}
