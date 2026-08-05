package server

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/errclass"
	frontend "claude-repld/internal/frontend"
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

// unwiredResyncer refuses with the no-live-controller sentinel, exactly as
// sessioncontroller does for a workspace that has not been brought up.
type unwiredResyncer struct{}

func (unwiredResyncer) ResyncForGeneration(string, string, string, uint64) error {
	return errclass.ErrNoLiveSessionController
}

// brokenResyncer fails for a reason that is nobody's routine expectation.
type brokenResyncer struct{}

func (brokenResyncer) ResyncForGeneration(string, string, string, uint64) error {
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

// An unwired workspace is NO LONGER a resync exception. Its conversation is
// served from durable store history (sessioncontroller/durablereplay.go), so a
// resyncer that still reports "no live session controller" is reporting that
// the durable path itself could not answer — a failure, not a routine state.
func TestResyncThatCannotServeAnUnwiredWorkspaceNacks(t *testing.T) {
	// Arrange.
	var logged []string
	h := newResyncHandler(t, unwiredResyncer{}, &logged)

	// Act.
	err := h.Resync(context.Background(), "/w", "r1", &frontendv1.ResyncCmd{FromSeq: 1})

	// Assert — silence would be indistinguishable from an empty conversation.
	if err == nil {
		t.Fatal("Resync that could not serve an unwired workspace returned no error")
	}
}

func TestResyncWithNoResyncerWiredNacks(t *testing.T) {
	// Arrange — the command exists, so something must answer it.
	var logged []string
	h := newResyncHandler(t, nil, &logged)

	// Act.
	err := h.Resync(context.Background(), "/w", "r1", &frontendv1.ResyncCmd{FromSeq: 1})

	// Assert.
	if err == nil {
		t.Fatal("Resync with no resyncer wired returned no error")
	}
}

func TestAFailedResyncIsLoggedWithItsCause(t *testing.T) {
	// Arrange.
	var logged []string
	h := newResyncHandler(t, brokenResyncer{}, &logged)

	// Act.
	h.Resync(context.Background(), "/w", "r1", &frontendv1.ResyncCmd{FromSeq: 1})

	// Assert.
	for _, l := range logged {
		if strings.Contains(l, "resync ws=/w request_id=r1 session= generation= from_seq=1 FAILED") && strings.Contains(l, "the retained ring is corrupt") {
			return
		}
	}
	t.Fatalf("no failed-resync line carrying the cause; lines=%v", logged)
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

type supersededResyncer struct{}

func (supersededResyncer) ResyncForGeneration(string, string, string, uint64) error {
	return errclass.ErrSessionSuperseded
}

func TestStaleResyncCommandAckIsClassifiedSuperseded(t *testing.T) {
	var logged []string
	h := newResyncHandler(t, supersededResyncer{}, &logged)
	ack := frontend.Dispatch(context.Background(), func(string, ...any) {}, h, nil, &frontendv1.FrontendCommand{
		RequestId: "r-stale", Workspace: "/w",
		Command: &frontendv1.FrontendCommand_Resync{Resync: &frontendv1.ResyncCmd{SessionId: "retired", ControllerGenerationId: "old"}},
	})
	if ack.GetFailure().GetErrorType() != string(errclass.TypeSessionSuperseded) {
		t.Fatalf("failure type = %q, want %q", ack.GetFailure().GetErrorType(), errclass.TypeSessionSuperseded)
	}
}

func TestInterruptOnAnUnwiredWorkspaceStaysLoud(t *testing.T) {
	// Arrange — a DIRECT user action. The user pressed stop; a silent no-op
	// would leave a pressed control doing nothing with no explanation.
	p := &fakePrompts{err: errclass.ErrNoLiveSessionController}
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
