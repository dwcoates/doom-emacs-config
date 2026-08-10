package sessioncontroller

import (
	"context"
	"errors"
	"fmt"
	"testing"

	"claude-repld/internal/errclass"
)

// ---------------------------------------------------------------------------
// THE TERMINAL FENCE. A bring-up refused for a vanished resume target is
// published once and then short-circuited, never re-run. See vanishedresume.go.
// ---------------------------------------------------------------------------

// errVanishedTarget is the spawner's terminal refusal, classified exactly as
// the server's resume gate classifies it: a wrapped cause carrying the
// vanished-target sentinel.
var errVanishedTarget = fmt.Errorf("server: session s1: resume target uuid-gone has no transcript in this daemon's config dir: %w", errclass.ErrResumeTargetVanished)

// TestVanishedResumeTargetFencesTheSession: the first terminal refusal publishes
// a card and spawns nothing further.
func TestVanishedResumeTargetFencesTheSession(t *testing.T) {
	// Arrange.
	h := newEscapeHarness(t)
	h.spawner.err = errVanishedTarget

	// Act.
	_, err := h.m.ensure(context.Background(), "ws")

	// Assert.
	if !errors.Is(err, errclass.ErrResumeTargetVanished) {
		t.Fatalf("ensure = %v, want the terminal vanished-resume refusal", err)
	}
	if len(h.failureCards()) != 1 {
		t.Fatalf("failure cards = %d, want exactly one standing card", len(h.failureCards()))
	}
	if !h.log.contains("bring-up TERMINALLY FAILED") {
		t.Fatal("the terminal disposition was not reported")
	}
}

// TestASecondOpenShortCircuitsWithoutBringingUp: this is the defect itself —
// the ensure machinery used to re-run the identical doomed bring-up on every
// open, sweep and reattach.
func TestASecondOpenShortCircuitsWithoutBringingUp(t *testing.T) {
	// Arrange — one terminal refusal, already fenced.
	h := newEscapeHarness(t)
	h.spawner.err = errVanishedTarget
	if _, err := h.m.ensure(context.Background(), "ws"); err == nil {
		t.Fatal("the first ensure succeeded; the harness must refuse it terminally")
	}
	spawnsAtFence := len(h.spawner.calls)

	// Act.
	_, err := h.m.ensure(context.Background(), "ws")

	// Assert.
	if !errors.Is(err, errclass.ErrResumeTargetVanished) {
		t.Fatalf("second ensure = %v, want the same standing refusal", err)
	}
	if len(h.spawner.calls) != spawnsAtFence {
		t.Fatalf("EnsureShim calls went %d -> %d past the fence; nothing may be brought up again",
			spawnsAtFence, len(h.spawner.calls))
	}
	if !h.log.contains("SHORT-CIRCUITED by the vanished-resume fence") {
		t.Fatal("the short-circuit was not recorded")
	}
}

// TestTheFenceCardIsPublishedOnce: a session refused N times owes the user one
// account of it, not N.
func TestTheFenceCardIsPublishedOnce(t *testing.T) {
	// Arrange.
	h := newEscapeHarness(t)
	h.spawner.err = errVanishedTarget

	// Act — three opens.
	for i := 0; i < 3; i++ {
		if _, err := h.m.ensure(context.Background(), "ws"); err == nil {
			t.Fatalf("ensure #%d succeeded; the harness must refuse it terminally", i+1)
		}
	}

	// Assert.
	if got := len(h.failureCards()); got != 1 {
		t.Fatalf("failure cards = %d, want one", got)
	}
	if got := h.warn.count("bring-up TERMINALLY FAILED"); got != 1 {
		t.Fatalf("terminal reports = %d, want exactly one loud record", got)
	}
}

// TestTheShortCircuitIsNotReportedAsAnError: the failure was reported once; the
// refusals that follow are diagnostic detail, not new faults.
func TestTheShortCircuitIsNotReportedAsAnError(t *testing.T) {
	// Arrange.
	h := newEscapeHarness(t)
	h.spawner.err = errVanishedTarget
	if _, err := h.m.ensure(context.Background(), "ws"); err == nil {
		t.Fatal("the first ensure succeeded; the harness must refuse it terminally")
	}

	// Act.
	if _, err := h.m.ensure(context.Background(), "ws"); err == nil {
		t.Fatal("the fenced ensure succeeded; it must return the standing refusal")
	}

	// Assert.
	if h.warn.contains("SHORT-CIRCUITED by the vanished-resume fence") {
		t.Fatal("the short-circuit took the warn channel; it belongs at debug")
	}
	if !h.log.contains("level=debug") {
		t.Fatal("the short-circuit was not tagged as a debug record")
	}
}

// TestRestartRechecksTheDiskAndUnfences: the way out is an explicit user
// action, and it re-derives the verdict rather than asserting one.
func TestRestartRechecksTheDiskAndUnfences(t *testing.T) {
	// Arrange — a fenced session whose transcript then reappears (the spawner
	// stops refusing).
	h := newEscapeHarness(t)
	h.spawner.err = errVanishedTarget
	if _, err := h.m.ensure(context.Background(), "ws"); err == nil {
		t.Fatal("the first ensure succeeded; the harness must refuse it terminally")
	}
	h.spawner.err = nil

	// Act.
	err := h.m.RestartSession(context.Background(), "ws")

	// Assert.
	if err != nil {
		t.Fatalf("RestartSession = %v, want the restart to bring the session back up", err)
	}
	if !h.log.contains("vanished-resume fence CLEARED") {
		t.Fatal("the restart did not clear the fence")
	}
	if h.m.vanishedResumeFenced("s1") {
		t.Fatal("the session is still fenced after a bring-up that succeeded")
	}
}

// TestRestartRefencesWhenTheTranscriptIsStillGone: clearing the fence entitles
// the bring-up to look again, and nothing more.
func TestRestartRefencesWhenTheTranscriptIsStillGone(t *testing.T) {
	// Arrange — a fenced session whose transcript is still missing.
	h := newEscapeHarness(t)
	h.spawner.err = errVanishedTarget
	if _, err := h.m.ensure(context.Background(), "ws"); err == nil {
		t.Fatal("the first ensure succeeded; the harness must refuse it terminally")
	}

	// Act.
	err := h.m.RestartSession(context.Background(), "ws")

	// Assert.
	if !errors.Is(err, errclass.ErrResumeTargetVanished) {
		t.Fatalf("RestartSession = %v, want the same terminal refusal", err)
	}
	if !h.m.vanishedResumeFenced("s1") {
		t.Fatal("the session was not re-fenced; the next open would loop again")
	}
}

// TestAnOrdinarySpawnFailureIsNotFenced: the fence applies to the terminal
// disposition alone. A transient spawn failure keeps its retries.
func TestAnOrdinarySpawnFailureIsNotFenced(t *testing.T) {
	// Arrange.
	h := newEscapeHarness(t)
	h.spawner.err = errors.New("spawn: transient failure")

	// Act.
	if _, err := h.m.ensure(context.Background(), "ws"); err == nil {
		t.Fatal("the ensure succeeded; the harness must fail the spawn")
	}
	if _, err := h.m.ensure(context.Background(), "ws"); err == nil {
		t.Fatal("the second ensure succeeded; the harness must fail the spawn")
	}

	// Assert.
	if h.m.vanishedResumeFenced("s1") {
		t.Fatal("a transient spawn failure fenced the session; only a vanished resume target may")
	}
	if len(h.spawner.calls) != 2 {
		t.Fatalf("EnsureShim calls = %d, want both attempts to have reached the spawner", len(h.spawner.calls))
	}
}

// TestTheFenceCardIsTerminal: the card must state that its failure has no
// closing edge, because that is the only thing on the wire a client can read to
// stop its own automatic re-open loop.
func TestTheFenceCardIsTerminal(t *testing.T) {
	// Arrange.
	h := newEscapeHarness(t)
	h.spawner.err = errVanishedTarget

	// Act.
	if _, err := h.m.ensure(context.Background(), "ws"); err == nil {
		t.Fatal("the ensure succeeded; the harness must refuse it terminally")
	}

	// Assert.
	cards := h.failureCards()
	if len(cards) != 1 {
		t.Fatalf("failure cards = %d, want exactly one standing card", len(cards))
	}
	if !errclass.IsTerminal(cards[0]) {
		t.Fatalf("fence card lifecycle = %T, want the terminal arm", cards[0].GetLifecycle())
	}
}
