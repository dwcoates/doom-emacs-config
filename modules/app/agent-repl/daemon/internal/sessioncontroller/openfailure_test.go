package sessioncontroller

import (
	"errors"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/errclass"
)

// errOpenBringUp is a bring-up failure that arrived after its open was acked.
var errOpenBringUp = errors.New("the workspace never became driveable")

func TestRecordOpenFailurePublishesAStartFailedCard(t *testing.T) {
	// Arrange — the open command acks acceptance now, so this is the ONLY
	// surface a late bring-up failure has left to reach the user on.
	h := newEscapeHarness(t, blocked())
	if _, err := h.m.bringUp("ws"); err != nil {
		t.Fatalf("bringUp: %v", err)
	}

	// Act.
	h.m.RecordOpenFailure("ws", errOpenBringUp)

	// Assert.
	if !h.hasCard(errclass.TypeSessionStartFailed) {
		t.Fatalf("no start-failed card for a late open failure; cards=%v", h.failureCards())
	}
}

func TestRecordOpenFailureNamesTheCause(t *testing.T) {
	// Arrange — a card that only announces a failure tells the user nothing the
	// tab color did not already.
	h := newEscapeHarness(t, blocked())
	if _, err := h.m.bringUp("ws"); err != nil {
		t.Fatalf("bringUp: %v", err)
	}

	// Act.
	h.m.RecordOpenFailure("ws", errOpenBringUp)

	// Assert.
	for _, c := range h.failureCards() {
		if errclass.TypeName(c) == string(errclass.TypeSessionStartFailed) {
			if !strings.Contains(c.GetDetail(), errOpenBringUp.Error()) {
				t.Fatalf("start-failed detail = %q, want the open's own cause", c.GetDetail())
			}
			return
		}
	}
	t.Fatal("no start-failed card")
}

// resumeDetailedError is a bring-up failure that carries the typed continuity
// evidence an exact resume holds — the evidence the command nack used to hand
// the client before the open acked on acceptance.
type resumeDetailedError struct{ detail *frontendv1.SessionResumeFailure }

func (e *resumeDetailedError) Error() string { return "the named conversation has no transcript" }

func (e *resumeDetailedError) SessionResumeFailureDetail() *frontendv1.SessionResumeFailure {
	return e.detail
}

func TestRecordOpenFailurePreservesTypedResumeEvidence(t *testing.T) {
	// Arrange — flattening this to prose would lose exactly the evidence that
	// tells the user WHICH conversation could not be restored.
	h := newEscapeHarness(t, blocked())
	if _, err := h.m.bringUp("ws"); err != nil {
		t.Fatalf("bringUp: %v", err)
	}
	failure := &resumeDetailedError{detail: &frontendv1.SessionResumeFailure{
		ClaudeSessionId: "claude-lost",
		Cwd:             "ws",
	}}

	// Act.
	h.m.RecordOpenFailure("ws", failure)

	// Assert.
	for _, c := range h.failureCards() {
		if detail := c.GetKind().GetSessionResumeFailed().GetDetail(); detail != nil {
			if detail.GetClaudeSessionId() != "claude-lost" {
				t.Fatalf("resume evidence = %v, want claude-lost", detail)
			}
			return
		}
	}
	t.Fatalf("no typed session-resume-failed card; cards=%v", h.failureCards())
}

func TestRecordOpenFailureRefusesAHibernatedSession(t *testing.T) {
	// Arrange — the revival gate is an expected outcome with its own pushed
	// SessionView. Carding it would show a continuity error for a session that
	// is merely asleep.
	h := newEscapeHarness(t, blocked())
	if _, err := h.m.bringUp("ws"); err != nil {
		t.Fatalf("bringUp: %v", err)
	}

	// Act.
	h.m.RecordOpenFailure("ws", errclass.ErrSessionHibernated)

	// Assert.
	if h.hasCard(errclass.TypeSessionStartFailed) {
		t.Fatalf("a hibernated session was carded as a start failure; cards=%v", h.failureCards())
	}
}

func TestRecordOpenFailureIsLoudWithNoControllerToPublishOnto(t *testing.T) {
	// Arrange — the bring-up may have torn its own controller down. The failure
	// is still real, and going quiet about it is the one thing forbidden.
	h := newEscapeHarness(t)

	// Act.
	h.m.RecordOpenFailure("ws", errOpenBringUp)

	// Assert.
	if !h.log.contains("no live controller to publish a failure card onto") {
		t.Fatalf("the unpublishable open failure left no loud line; log=%v", h.log.lines)
	}
}

func TestRecordOpenFailureIsLoudWithNoCause(t *testing.T) {
	// Arrange — a failure with no cause names nothing and cannot be published,
	// which is a caller bug rather than a runtime condition.
	h := newEscapeHarness(t, blocked())
	if _, err := h.m.bringUp("ws"); err != nil {
		t.Fatalf("bringUp: %v", err)
	}

	// Act.
	h.m.RecordOpenFailure("ws", nil)

	// Assert.
	if !h.log.contains("RecordOpenFailure called with") {
		t.Fatalf("a causeless open failure left no loud line; log=%v", h.log.lines)
	}
}
