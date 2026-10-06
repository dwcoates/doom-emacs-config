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
