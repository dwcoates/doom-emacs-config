package server

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/errclass"
)

// ---------------------------------------------------------------------------
// AN UNWIRED WORKSPACE IS CLASSIFIED BY CALLER.
//
// Emacs fans background machinery — resyncs, kept health probes, sweep passes —
// across every workspace it knows about, and after a daemon bounce most of them
// are legitimately unwired. Reporting each of those refusals as a failure put
// dozens of error-shaped lines and nacks in front of a user with nothing to act
// on. Direct user-initiated commands keep the loud nack, because there the
// refusal IS the feedback.
// ---------------------------------------------------------------------------

// unwiredResyncer refuses with the no-live-driver sentinel, exactly as
// sessiondrv does for a workspace that has not been brought up.
type unwiredResyncer struct{}

func (unwiredResyncer) Resync(string, uint64) error { return errclass.ErrNoLiveDriver }

// brokenResyncer fails for a reason that is nobody's routine expectation.
type brokenResyncer struct{}

func (brokenResyncer) Resync(string, uint64) error {
	return errors.New("the retained ring is corrupt")
}

// newResyncHandler builds a handler over one Resyncer, capturing the log.
func newResyncHandler(t *testing.T, r Resyncer, lines *[]string) *commandHandler {
	t.Helper()
	h, err := newCommandHandler(&fakePrompts{}, &fakeMerges{}, &fakeLifecycle{}, r, &fakeSessionCmds{}, nil, nil,
		func(f string, a ...any) { *lines = append(*lines, fmt.Sprintf(f, a...)) })
	if err != nil {
		t.Fatalf("newCommandHandler: %v", err)
	}
	return h
}

func TestResyncOnAnUnwiredWorkspaceIsCalm(t *testing.T) {
	// Arrange — a BACKGROUND caller: Emacs re-syncing every workspace it knows
	// about after a bounce.
	var logged []string
	h := newResyncHandler(t, unwiredResyncer{}, &logged)

	// Act.
	err := h.Resync(context.Background(), "/w", "r1", &frontendv1.ResyncCmd{FromSeq: 1})

	// Assert — no nack, so no failure card and no error-shaped ack.
	if err != nil {
		t.Fatalf("Resync on an unwired workspace = %v, want a calm no-op", err)
	}
}

func TestResyncOnAnUnwiredWorkspaceSaysSoPlainly(t *testing.T) {
	// Arrange — calm is not silent: the skip is still accounted for.
	var logged []string
	h := newResyncHandler(t, unwiredResyncer{}, &logged)

	// Act.
	h.Resync(context.Background(), "/w", "r1", &frontendv1.ResyncCmd{FromSeq: 1})

	// Assert.
	for _, l := range logged {
		if strings.Contains(l, "the workspace has no live driver") {
			return
		}
	}
	t.Fatalf("no calm skip line logged; lines=%v", logged)
}

func TestAGenuineResyncFailureStaysLoud(t *testing.T) {
	// Arrange — nothing here weakens the loud path for a real caller error.
	var logged []string
	h := newResyncHandler(t, brokenResyncer{}, &logged)

	// Act.
	err := h.Resync(context.Background(), "/w", "r1", &frontendv1.ResyncCmd{FromSeq: 1})

	// Assert.
	if err == nil {
		t.Fatal("a genuine resync failure was quieted")
	}
}

func TestInterruptOnAnUnwiredWorkspaceStaysLoud(t *testing.T) {
	// Arrange — a DIRECT user action. The user pressed stop; a silent no-op
	// would leave a pressed control doing nothing with no explanation.
	p := &fakePrompts{err: errclass.ErrNoLiveDriver}
	h, err := newCommandHandler(p, &fakeMerges{}, &fakeLifecycle{}, nil, &fakeSessionCmds{}, nil, nil, nil)
	if err != nil {
		t.Fatalf("newCommandHandler: %v", err)
	}

	// Act.
	err = h.Interrupt(context.Background(), "/w", "r1", &frontendv1.InterruptCmd{ConfirmAgents: true})

	// Assert.
	if err == nil {
		t.Fatal("a user-commanded interrupt on an unwired workspace returned no error")
	}
}
