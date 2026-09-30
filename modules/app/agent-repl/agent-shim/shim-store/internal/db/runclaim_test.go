package db

import (
	"errors"
	"fmt"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
)

// ---- the shell run claim: persistence ----

func runClaim(taskID, run string) *storev1.ShellRunClaim {
	return &storev1.ShellRunClaim{VendorTaskId: taskID, Run: &conversationv1.AgentActivityId{Value: run}}
}

// writeClaimsOK writes one interactive batch carrying entries and claims,
// which must succeed.
func writeClaimsOK(t *testing.T, d *DB, claims []*storev1.ShellRunClaim, entries ...*storev1.StoreEntry) WriteResult {
	t.Helper()
	result, err := d.WriteBatch(ctx(), "test-producer", WriteInteractive,
		&storev1.EntryBatch{Entries: entries, ShellRunClaims: claims}, nil)
	if err != nil {
		t.Fatalf("WriteBatch: %v", err)
	}
	return result
}

// launchedIn books the run's launching call as an activity row of `book`,
// as the producer that read the call does.
func launchedIn(t *testing.T, d *DB, book, run string) {
	t.Helper()
	writeOK(t, d, pageEntry("w-"+run, "activity:"+run, book, frameItem(activityFrame(book, run, prose()))))
}

func TestWriteBatchPersistsAShellRunClaim(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	result := writeClaimsOK(t, d, []*storev1.ShellRunClaim{runClaim("b1", "toolu_run")})

	// Assert
	if got := scalar[string](t, d, `SELECT run_id FROM shell_run_claim WHERE vendor_task_id = 'b1'`); got != "toolu_run" {
		t.Fatalf("shell_run_claim run = %q, want toolu_run", got)
	}
	if result.Claims != 1 {
		t.Fatalf("Claims = %d, want 1", result.Claims)
	}
}

func TestWriteBatchAbsorbsARestatedShellRunClaim(t *testing.T) {
	// Arrange: the shim restates a claim at every detachment fact it reads.
	d, _ := newStore(t)
	writeClaimsOK(t, d, []*storev1.ShellRunClaim{runClaim("b1", "toolu_run")})

	// Act
	writeClaimsOK(t, d, []*storev1.ShellRunClaim{runClaim("b1", "toolu_run")})

	// Assert
	if got := scalar[int](t, d, `SELECT COUNT(*) FROM shell_run_claim`); got != 1 {
		t.Fatalf("shell_run_claim rows = %d, want 1", got)
	}
}

func TestWriteBatchRefusesAMalformedShellRunClaimWhole(t *testing.T) {
	tests := []struct {
		name  string
		claim *storev1.ShellRunClaim
		field string
	}{
		{name: "unset", claim: nil, field: "shell_run_claims[0]"},
		{name: "empty task id", claim: runClaim("", "toolu_run"), field: "shell_run_claims[0].vendor_task_id"},
		{name: "unset run", claim: &storev1.ShellRunClaim{VendorTaskId: "b1"}, field: "shell_run_claims[0].run"},
		{name: "empty run", claim: runClaim("b1", ""), field: "shell_run_claims[0].run"},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			d, _ := newStore(t)

			// Act
			_, err := d.WriteBatch(ctx(), "test-producer", WriteInteractive, &storev1.EntryBatch{
				Entries:        []*storev1.StoreEntry{pageEntry("w1", "u1", "agent-main", frameItem(activityFrame("agent-main", "act-1", prose())))},
				ShellRunClaims: []*storev1.ShellRunClaim{test.claim},
			}, nil)

			// Assert
			if !errors.Is(err, ErrInvalid) || RefusalField(err) != test.field {
				t.Fatalf("err = %v (field %q), want ErrInvalid naming %s", err, RefusalField(err), test.field)
			}
			if got := scalar[int](t, d, `SELECT COUNT(*) FROM entry`); got != 0 {
				t.Fatalf("entry rows = %d, want 0: a refused batch commits nothing", got)
			}
		})
	}
}

// ---- the shell run claim: the lookup ----

func TestShellRunClaimsAnswersTheOwningBookFromTheRunsActivityRow(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	launchedIn(t, d, "agent-sub", "toolu_run")
	writeClaimsOK(t, d, []*storev1.ShellRunClaim{runClaim("b1", "toolu_run")})

	// Act
	claims, err := d.ShellRunClaims(ctx(), []string{"b1"})

	// Assert
	if err != nil {
		t.Fatalf("ShellRunClaims: %v", err)
	}
	if len(claims) != 1 || claims[0] != (ClaimedRun{VendorTaskID: "b1", Run: "toolu_run", Owner: "agent-sub"}) {
		t.Fatalf("claims = %+v, want b1 -> toolu_run owned by agent-sub", claims)
	}
}

func TestShellRunClaimsLeavesTheOwnerEmptyWhileTheCallIsNotOnRecord(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	writeClaimsOK(t, d, []*storev1.ShellRunClaim{runClaim("b1", "toolu_run")})

	// Act
	claims, err := d.ShellRunClaims(ctx(), []string{"b1"})

	// Assert
	if err != nil {
		t.Fatalf("ShellRunClaims: %v", err)
	}
	if len(claims) != 1 || claims[0].Owner != "" {
		t.Fatalf("claims = %+v, want one claim with no owner yet", claims)
	}
}

func TestShellRunClaimsOmitsAnUnclaimedTaskID(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	writeClaimsOK(t, d, []*storev1.ShellRunClaim{runClaim("b1", "toolu_run")})

	// Act
	claims, err := d.ShellRunClaims(ctx(), []string{"b1", "b-unclaimed"})

	// Assert
	if err != nil {
		t.Fatalf("ShellRunClaims: %v", err)
	}
	if len(claims) != 1 || claims[0].VendorTaskID != "b1" {
		t.Fatalf("claims = %+v, want only b1's claim", claims)
	}
}

func TestShellRunClaimsAnswersEveryRunThatClaimsOneTaskID(t *testing.T) {
	// Arrange: two runs claim one spool; the caller refuses to choose.
	d, _ := newStore(t)
	writeClaimsOK(t, d, []*storev1.ShellRunClaim{runClaim("b1", "toolu_one"), runClaim("b1", "toolu_two")})

	// Act
	claims, err := d.ShellRunClaims(ctx(), []string{"b1"})

	// Assert
	if err != nil {
		t.Fatalf("ShellRunClaims: %v", err)
	}
	if len(claims) != 2 || claims[0].Run != "toolu_one" || claims[1].Run != "toolu_two" {
		t.Fatalf("claims = %+v, want both runs in order", claims)
	}
}

func TestShellRunClaimsRefusesAnIncompleteRequestBeforeReading(t *testing.T) {
	tests := []struct {
		name  string
		ids   []string
		field string
	}{
		{name: "no ids", ids: nil, field: "vendor_task_ids"},
		{name: "an empty id", ids: []string{"b1", ""}, field: "vendor_task_ids[1]"},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			d, _ := newStore(t)

			// Act
			_, err := d.ShellRunClaims(ctx(), test.ids)

			// Assert
			if !errors.Is(err, ErrInvalid) || RefusalField(err) != test.field {
				t.Fatalf("err = %v (field %q), want ErrInvalid naming %s", err, RefusalField(err), test.field)
			}
		})
	}
}

func TestTheClaimLookupBuildsNoAutomaticIndex(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	plan := queryPlan(t, d, fmt.Sprintf(shellRunClaimsSQL, "?"), "b1")

	// Assert
	assertNoAutomaticIndex(t, "the claim lookup", plan)
}

func TestShellRunClaimsReportsAStorageFailureOnAClosedDatabase(t *testing.T) {
	// Arrange
	d, s := newStore(t)
	if err := d.Close(); err != nil {
		t.Fatalf("close: %v", err)
	}

	// Act
	_, err := d.ShellRunClaims(ctx(), []string{"b1"})

	// Assert
	if !errors.Is(err, ErrStorage) {
		t.Fatalf("error = %v, want ErrStorage", err)
	}
	s.assertLogged(t, "error", "reading the claims of 1 vendor task id(s)")
}
