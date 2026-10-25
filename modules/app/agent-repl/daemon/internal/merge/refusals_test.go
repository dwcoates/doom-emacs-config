package merge

import (
	"agent-repld/internal/dlog"
	"errors"
	"fmt"
	"strings"
	"testing"
)

// TestRefusedReportsTheArm covers the seam the server maps: a refusal carries
// the arm name the contract owes it.
func TestRefusedReportsTheArm(t *testing.T) {
	// Arrange: one refusal.
	err := refuse(ArmAlreadyQueued, theWorkspace, "it already holds place %d", 2)

	// Act.
	refusal, refused := Refused(err)

	// Assert.
	if !refused || refusal.Arm != ArmAlreadyQueued {
		t.Fatalf("Refused answered (%q, %v), want the already_queued arm", refusal.Arm, refused)
	}
}

// TestRefusedSeesThroughAWrappedError covers the wrapping every caller does on
// the way out: the arm must survive it.
func TestRefusedSeesThroughAWrappedError(t *testing.T) {
	// Arrange: a wrapped refusal.
	err := fmt.Errorf("enqueueing: %w", refuse(ArmSessionDeleted, theWorkspace, "it was deleted"))

	// Act.
	refusal, refused := Refused(err)

	// Assert.
	if !refused || refusal.Arm != ArmSessionDeleted {
		t.Fatalf("Refused answered (%q, %v) for a wrapped refusal", refusal.Arm, refused)
	}
}

// TestRefusedIgnoresAnOrdinaryFailure covers the distinction the server needs:
// a store failure is not a refusal the user asked for.
func TestRefusedIgnoresAnOrdinaryFailure(t *testing.T) {
	// Arrange: an ordinary error.
	err := errors.New("the database is gone")

	// Act.
	_, refused := Refused(err)

	// Assert.
	if refused {
		t.Fatal("an ordinary failure was reported as a refusal")
	}
}

// TestRefusalCarriesItsReason covers what the server puts after the arm: the
// sentence a user reads.
func TestRefusalCarriesItsReason(t *testing.T) {
	// Arrange: a refusal with a composed reason.
	err := refuse(ArmNoLayoutFacts, theWorkspace, "no creation job records its geometry")

	// Act.
	message := err.Error()

	// Assert.
	if !strings.Contains(message, "no creation job records its geometry") {
		t.Fatalf("the refusal reads %q, want it to carry its reason", message)
	}
	if !strings.Contains(message, string(theWorkspace)) {
		t.Fatalf("the refusal reads %q, want it to name the workspace", message)
	}
}

func TestLogRefusalRecordsAtInfoWithTheRefusal(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	refusal := refuse(ArmNameTaken, theWorkspace, "the name is taken")

	// Act
	logRefusal(log, "daemon.merge.enqueue", "refused an async merge whose name is taken", dlog.Context{"workspace": "w1"}, refusal)

	// Assert
	record, found := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: "daemon.merge.enqueue"})
	if !found || record.Context["error"] != refusal.Error() || record.Context["workspace"] != "w1" {
		t.Fatalf("record = (%+v, %v), want the refusal at INFO with the caller's fields", record, found)
	}
}
