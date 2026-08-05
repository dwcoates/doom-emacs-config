package server

import (
	"errors"
	"fmt"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// resumeDetail pulls the wire evidence off a classified error.
func resumeDetail(t *testing.T, err error) *frontendv1.SessionResumeFailure {
	t.Helper()
	var detailed interface {
		SessionResumeFailureDetail() *frontendv1.SessionResumeFailure
	}
	if !errors.As(err, &detailed) {
		t.Fatalf("error carries no resume detail: %v", err)
	}
	return detailed.SessionResumeFailureDetail()
}

// missingTranscript is the failure a restart meets when its conversation's
// transcript is gone: the exact shape observed in the field.
func missingTranscript() *ResumeTranscriptMissingError {
	return &ResumeTranscriptMissingError{
		ResumeID:          "a60aa989-5397-4d26-8f09-059b65fe3856",
		CWD:               "/ws/evalposition",
		ConfigDir:         "/home/u/.claude-chesscom",
		ResolvedConfigDir: "/home/u/.claude-chesscom",
		TranscriptPath:    "/home/u/.claude-chesscom/projects/-ws-evalposition/a60aa989.jsonl",
		SearchedPaths:     []string{"/home/u/.claude-chesscom/projects/-ws-evalposition/a60aa989.jsonl"},
	}
}

// THE REGRESSION: a restart whose transcript is missing reached the user as
// internal.unclassified, so an unopenable workspace gave no reason at all.
func TestRestartClassifiesAMissingTranscript(t *testing.T) {
	// Arrange
	wrapped := fmt.Errorf("server: restarting the session for workspace %q: %w", "/ws/evalposition", missingTranscript())

	// Act
	classified := restartResumeEstablishment().classify(wrapped)

	// Assert
	if resumeDetail(t, classified).GetTranscriptUnavailable() == nil {
		t.Fatal("classified restart failure does not name transcript-unavailable as its cause")
	}
}

// The searched path is the actionable half: it tells the reader WHERE the
// daemon looked, which is how a config-root mismatch is spotted.
func TestRestartClassificationCarriesTheSearchedPath(t *testing.T) {
	// Arrange
	wrapped := fmt.Errorf("wrapped: %w", missingTranscript())

	// Act
	got := resumeDetail(t, restartResumeEstablishment().classify(wrapped)).GetTranscriptUnavailable().GetSearchedPaths()

	// Assert
	if len(got) != 1 || got[0] != missingTranscript().SearchedPaths[0] {
		t.Fatalf("searched_paths = %v, want the single path the gate reported", got)
	}
}

// A restart is an AUTOMATIC restore, not a create: the frontend words the two
// differently, and a restart shown as a create would misdescribe what the user
// did.
func TestRestartClassificationReportsAnAutomaticRestore(t *testing.T) {
	// Arrange
	wrapped := fmt.Errorf("wrapped: %w", missingTranscript())

	// Act
	detail := resumeDetail(t, restartResumeEstablishment().classify(wrapped))

	// Assert
	if detail.GetAutomaticRestore() == nil {
		t.Fatalf("attempt = %T, want automatic_restore", detail.GetAttempt())
	}
}

// The conversation identity comes off the error, so the detail identifies the
// failure even though the restart boundary holds no registry record.
func TestRestartClassificationCarriesTheConversationIdentity(t *testing.T) {
	// Arrange
	wrapped := fmt.Errorf("wrapped: %w", missingTranscript())

	// Act
	detail := resumeDetail(t, restartResumeEstablishment().classify(wrapped))

	// Assert
	if detail.GetClaudeSessionId() != missingTranscript().ResumeID || detail.GetCwd() != missingTranscript().CWD {
		t.Fatalf("identity = (%s, %s), want the error's conversation and cwd",
			detail.GetClaudeSessionId(), detail.GetCwd())
	}
}

// An unrelated restart failure must NOT be dressed up as a continuity failure:
// with no conversation identity to report, it is left for its own classifier.
func TestRestartLeavesAnUnrelatedFailureAlone(t *testing.T) {
	// Arrange
	plain := errors.New("server: stopping the orphaned shim: permission denied")

	// Act
	classified := restartResumeEstablishment().classify(plain)

	// Assert
	var detailed interface {
		SessionResumeFailureDetail() *frontendv1.SessionResumeFailure
	}
	if errors.As(classified, &detailed) {
		t.Fatalf("unrelated restart failure was classified as a resume failure: %v", classified)
	}
}
