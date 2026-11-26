package merge

import (
	"context"
	"errors"
	"slices"
	"testing"
	"time"
)

// counter is a sequence for a bare fake git.
func counter() func() int {
	n := 0
	return func() int { n++; return n }
}

func TestAStoppedGateStartsNoCommand(t *testing.T) {
	// Arrange.
	inner := newFakeGit(counter())
	gate := newGatedGit(inner, time.Now)
	gate.stop()

	// Act.
	_, err := gate.ResolveRef(context.Background(), "/repo", "HEAD")

	// Assert.
	if !errors.Is(err, errMergeStopping) || slices.Contains(inner.calls, "resolve_ref") {
		t.Fatalf("ResolveRef = %v with calls %v, want errMergeStopping and no git", err, inner.calls)
	}
}

func TestStoppingAnIdleGateAnswersAtOnce(t *testing.T) {
	// Arrange.
	gate := newGatedGit(newFakeGit(counter()), time.Now)

	// Act.
	idle, running := gate.stop()

	// Assert.
	select {
	case <-idle:
	default:
		t.Fatal("an idle gate's stop did not answer at once")
	}
	if running.name != "" {
		t.Fatalf("an idle gate names %q in flight, want none", running.name)
	}
}

func TestStoppingAGateAnswersWhenItsCommandEnds(t *testing.T) {
	// Arrange: a command held in flight.
	inner := newFakeGit(counter())
	entered, release := inner.hold("resolve_ref")
	gate := newGatedGit(inner, time.Now)
	done := make(chan struct{})
	go func() {
		gate.ResolveRef(context.Background(), "/repo", "HEAD")
		close(done)
	}()
	<-entered

	// Act.
	idle, running := gate.stop()

	// Assert: the stop names the command and answers only once it ends.
	if running.name != "ResolveRef" {
		t.Fatalf("the stop names %q in flight, want ResolveRef", running.name)
	}
	select {
	case <-idle:
		t.Fatal("the stop answered while the command still ran")
	default:
	}
	close(release)
	<-done
	<-idle
}

func TestAStoppedGateRefusesThePathScopedCommands(t *testing.T) {
	// Arrange.
	tests := []struct {
		name string
		call func(g *gatedGit) error
	}{
		{name: "PathClean", call: func(g *gatedGit) error {
			_, err := g.PathClean(context.Background(), "/repo", "/repo/a.md")
			return err
		}},
		{name: "CommitPath", call: func(g *gatedGit) error {
			_, err := g.CommitPath(context.Background(), "/repo", "/repo/a.md", "m")
			return err
		}},
		{name: "MoveWorktree", call: func(g *gatedGit) error {
			return g.MoveWorktree(context.Background(), "/repo", "/a", "/b")
		}},
		{name: "RenameBranch", call: func(g *gatedGit) error {
			return g.RenameBranch(context.Background(), "/repo", "a", "b")
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			gate := newGatedGit(newFakeGit(counter()), time.Now)
			gate.stop()

			// Act.
			err := tt.call(gate)

			// Assert.
			if !errors.Is(err, errMergeStopping) {
				t.Fatalf("%s on a stopped gate = %v, want errMergeStopping", tt.name, err)
			}
		})
	}
}

func TestAStoppedGateCountsNoCommits(t *testing.T) {
	// Arrange.
	inner := newFakeGit(counter())
	gate := newGatedGit(inner, time.Now)
	gate.stop()

	// Act.
	_, err := gate.CountCommits(context.Background(), "/repo", "master", "feature")

	// Assert.
	if !errors.Is(err, errMergeStopping) || slices.Contains(inner.calls, "count_commits") {
		t.Fatalf("CountCommits = %v with calls %v, want errMergeStopping and no git", err, inner.calls)
	}
}

func TestAnOpenGatePassesCountCommitsThrough(t *testing.T) {
	// Arrange.
	inner := newFakeGit(counter())
	gate := newGatedGit(inner, time.Now)

	// Act.
	_, err := gate.CountCommits(context.Background(), "/repo", "master", "feature")

	// Assert.
	if err != nil || !slices.Contains(inner.calls, "count_commits") {
		t.Fatalf("CountCommits = %v with calls %v, want the inner git asked", err, inner.calls)
	}
}

// blockingCounter is a fake git whose CountCommits parks until release
// closes, announcing itself on entered, and records the context it was given.
type blockingCounter struct {
	*fakeGit
	entered, release chan struct{}
	// endedCut is the error of the command's context once it was released.
	endedCut error
}

func (b *blockingCounter) CountCommits(ctx context.Context, dir, base, tip string) (uint32, error) {
	close(b.entered)
	<-b.release
	b.endedCut = ctx.Err()
	return 7, nil
}

func TestACommandInFlightIsNotCutByTheRunsCancellation(t *testing.T) {
	// Arrange: a command held in flight under the run's context.
	inner := &blockingCounter{fakeGit: newFakeGit(counter()), entered: make(chan struct{}), release: make(chan struct{})}
	gate := newGatedGit(inner, time.Now)
	ctx, cancel := context.WithCancelCause(context.Background())
	defer cancel(nil)
	type answer struct {
		n   uint32
		err error
	}
	answered := make(chan answer, 1)
	go func() {
		n, err := gate.CountCommits(ctx, "/repo", "master", "feature")
		answered <- answer{n, err}
	}()
	<-inner.entered

	// Act: the run is abandoned while the command runs, then git finishes.
	cancel(&abandonment{cause: CauseWorkspaceClosed})
	close(inner.release)
	got := <-answered

	// Assert: git ran whole and its answer is the run's.
	if inner.endedCut != nil {
		t.Fatalf("the command in flight ended under a cut context (%v), want it never cut", inner.endedCut)
	}
	if got.err != nil || got.n != 7 {
		t.Fatalf("CountCommits = %d, %v, want the inner git's 7 and no error", got.n, got.err)
	}
}

func TestAGateRefusesACommandOnceTheRunIsCancelled(t *testing.T) {
	// Arrange.
	tests := []struct {
		name  string
		check func(err error) bool
	}{
		{name: "the refusal is the gate's", check: func(err error) bool {
			var refused *gitRefused
			return errors.As(err, &refused)
		}},
		{name: "the refusal carries the abandonment", check: func(err error) bool {
			var ab *abandonment
			return errors.As(err, &ab) && ab.cause == CauseWorkspaceClosed
		}},
		{name: "the refusal reads as a cancellation", check: func(err error) bool {
			return errors.Is(err, context.Canceled)
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			inner := newFakeGit(counter())
			gate := newGatedGit(inner, time.Now)
			ctx, cancel := context.WithCancelCause(context.Background())
			cancel(&abandonment{cause: CauseWorkspaceClosed})

			// Act.
			_, err := gate.CountCommits(ctx, "/repo", "master", "feature")

			// Assert.
			if !tt.check(err) || slices.Contains(inner.calls, "count_commits") {
				t.Fatalf("CountCommits = %v with calls %v, want refused before git", err, inner.calls)
			}
		})
	}
}

func TestARefusedCommandLeavesNoCommandInFlight(t *testing.T) {
	// Arrange.
	gate := newGatedGit(newFakeGit(counter()), time.Now)
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	_, _ = gate.CountCommits(ctx, "/repo", "master", "feature")

	// Act.
	idle, running := gate.stop()

	// Assert.
	select {
	case <-idle:
	default:
		t.Fatal("a gate whose command was refused did not stop at once")
	}
	if running.name != "" {
		t.Fatalf("a refused command is named in flight (%q), want none", running.name)
	}
}
