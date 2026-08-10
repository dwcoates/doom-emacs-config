package sessioncontroller

import (
	"errors"
	"fmt"
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

func TestRecordOpenFailureFencedRefusalFirstIsWarn(t *testing.T) {
	// Arrange — an open attempt against a session the vanished-resume fence
	// already fenced refuses before any controller exists, same as the
	// fenced-with-no-controller shape.
	h := newEscapeHarness(t)
	fenced := fmt.Errorf("session-controller: wrap: %w", errclass.ErrResumeTargetVanished)

	// Act.
	h.m.RecordOpenFailure("ws", fenced)

	// Assert — the FIRST fenced open refusal this boot is loud at WARN, naming
	// the fence, not at ERROR.
	if h.warn.count("refused by the vanished-resume fence") != 1 {
		t.Fatalf("first fenced open refusal was not reported exactly once at warn; warn=%v", h.warn.lines)
	}
}

func TestRecordOpenFailureFencedRefusalSubsequentIsDebug(t *testing.T) {
	// Arrange — the fence stands and the caller keeps retrying the open, which
	// is the exact 9-errors-in-one-window shape the defect names.
	h := newEscapeHarness(t)
	fenced := fmt.Errorf("session-controller: wrap: %w", errclass.ErrResumeTargetVanished)
	h.m.RecordOpenFailure("ws", fenced)

	// Act.
	h.m.RecordOpenFailure("ws", fenced)
	h.m.RecordOpenFailure("ws", fenced)

	// Assert — only the first arrival took the WARN channel; the later two are
	// demoted to debug (tagged, not re-warned), and still name the fence.
	if h.warn.count("refused by the vanished-resume fence") != 1 {
		t.Fatalf("a later fenced open refusal was warned again; warn=%v", h.warn.lines)
	}
	if h.log.count("refused by the vanished-resume fence again") != 2 {
		t.Fatalf("later fenced open refusals were not logged at debug; log=%v", h.log.lines)
	}
	if !h.log.contains("level=debug") {
		t.Fatalf("later fenced open refusal did not carry a debug level tag; log=%v", h.log.lines)
	}
}

func TestRecordOpenFailureNonFenceStaysError(t *testing.T) {
	// Arrange — a bring-up failure that is NOT the vanished-resume fence's own
	// refusal must keep its ERROR loudness exactly as before; only the fence
	// path is demoted.
	h := newEscapeHarness(t)

	// Act.
	h.m.RecordOpenFailure("ws", errOpenBringUp)

	// Assert.
	if !h.log.contains("no live controller to publish a failure card onto") {
		t.Fatalf("a non-fence open failure with no controller lost its loud line; log=%v", h.log.lines)
	}
	if h.log.contains("refused by the vanished-resume fence") {
		t.Fatalf("a non-fence open failure was misclassified as a fence refusal; log=%v", h.log.lines)
	}
}

func TestRecordOpenFailureFencedRefusalCountResetsPerBoot(t *testing.T) {
	// Arrange — "per boot" means a fresh Manager (a restarted daemon) warns
	// once again rather than inheriting the previous boot's count.
	h1 := newEscapeHarness(t)
	fenced := fmt.Errorf("session-controller: wrap: %w", errclass.ErrResumeTargetVanished)
	h1.m.RecordOpenFailure("ws", fenced)
	h1.m.RecordOpenFailure("ws", fenced)
	if h1.warn.count("refused by the vanished-resume fence") != 1 {
		t.Fatalf("setup: expected exactly one warn in the first boot; warn=%v", h1.warn.lines)
	}

	// Act — a new Manager stands in for the next boot.
	h2 := newEscapeHarness(t)
	h2.m.RecordOpenFailure("ws", fenced)

	// Assert.
	if h2.warn.count("refused by the vanished-resume fence") != 1 {
		t.Fatalf("the new boot's first fenced open refusal was not warned; warn=%v", h2.warn.lines)
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
