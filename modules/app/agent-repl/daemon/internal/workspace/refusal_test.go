package workspace

import (
	"errors"
	"fmt"
	"testing"

	"claude-repld/internal/dlog"
)

func TestRefusalRendersTheLedgerMessage(t *testing.T) {
	// Arrange.
	refusal := &Refusal{Rpc: "CloseWorkspace", Arm: "blocked", Reason: "a turn is running"}

	// Act.
	got := refusal.Error()

	// Assert.
	want := "intended arm: CloseWorkspaceError.blocked: a turn is running"
	if got != want {
		t.Fatalf("Error() = %q, want %q", got, want)
	}
}

func TestRefusalWithoutAnRpcRendersThePlaceholder(t *testing.T) {
	// Arrange: a refusal raised by a shared helper leaves the rpc to the
	// handler that surfaces it.
	refusal := &Refusal{Arm: ArmWorkspaceRefMismatch, Reason: "dir disagrees"}

	// Act.
	got := refusal.Error()

	// Assert.
	want := "intended arm: <Rpc>Error.workspace_ref_mismatch: dir disagrees"
	if got != want {
		t.Fatalf("Error() = %q, want %q", got, want)
	}
}

func TestWithRpcDoesNotMutateTheOriginal(t *testing.T) {
	// Arrange: two handlers naming one shared refusal must not overwrite each
	// other.
	shared := &Refusal{Arm: ArmUnknownWorkspace, Reason: "gone"}

	// Act.
	named := shared.WithRpc("OpenWorkspace")

	// Assert.
	if shared.Rpc != "" {
		t.Fatalf("the shared refusal's rpc = %q, want it left empty", shared.Rpc)
	}
	if named.Rpc != "OpenWorkspace" {
		t.Fatalf("the named refusal's rpc = %q, want OpenWorkspace", named.Rpc)
	}
}

func TestAsRefusalFindsAWrappedRefusal(t *testing.T) {
	// Arrange.
	wrapped := fmt.Errorf("create: %w", &Refusal{Arm: ArmNoSlug, Reason: "no words"})

	// Act.
	refusal, ok := AsRefusal(wrapped)

	// Assert.
	if !ok || refusal.Arm != ArmNoSlug {
		t.Fatalf("AsRefusal(%v) = (%v, %v), want the no_slug refusal", wrapped, refusal, ok)
	}
}

func TestAsRefusalRejectsAnOrdinaryError(t *testing.T) {
	// Arrange. Act.
	_, ok := AsRefusal(errors.New("disk on fire"))

	// Assert.
	if ok {
		t.Fatal("AsRefusal(ordinary error) reported a refusal")
	}
}

func TestRefuseLogsTheTypedRefusalAtInfo(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()

	// Act.
	refuse(log, "Interrupt", ArmNoSession, "no live session", false)

	// Assert.
	records := log.Records()
	if len(records) != 1 {
		t.Fatalf("records = %v, want exactly one", records)
	}
	if records[0].Level != "info" || records[0].Operation != "daemon.refusal.typed" {
		t.Fatalf("record = %+v, want an info record under daemon.refusal.typed", records[0])
	}
}

func TestRefuseNeverLogsTheUnlandedArmOperation(t *testing.T) {
	// Arrange: only server.UnlandedArm may warn under that operation, so the
	// log stays usable for reconciling ERROR-ARMS.md.
	log := dlog.NewTestLogger()

	// Act.
	refuse(log, "Interrupt", ArmNoSession, "no live session", false)

	// Assert.
	for _, record := range log.Records() {
		if record.Operation == "daemon.refusal.unlanded_arm" {
			t.Fatalf("record = %+v, want no unlanded-arm record from a verb refusal", record)
		}
		if record.Level == "warn" {
			t.Fatalf("record = %+v, want no warning from a typed refusal", record)
		}
	}
}

func TestRefuseWithLogsTheTypedRefusalAtInfo(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()

	// Act.
	refuseWith(log, "CreateWorkspace", ArmBaseRefUnresolved, "no such ref", false,
		map[string]any{"ref": "origin/nope"})

	// Assert.
	records := log.Records()
	if len(records) != 1 {
		t.Fatalf("records = %v, want exactly one", records)
	}
	if records[0].Level != "info" || records[0].Operation != "daemon.refusal.typed" {
		t.Fatalf("record = %+v, want an info record under daemon.refusal.typed", records[0])
	}
}

func TestRefuseWithRecordsTheArmsFieldsInTheLog(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()

	// Act.
	refuseWith(log, "CreateWorkspace", ArmBaseRefUnresolved, "no such ref", false,
		map[string]any{"ref": "origin/nope"})

	// Assert.
	records := log.Records()
	if len(records) != 1 {
		t.Fatalf("records = %v, want exactly one", records)
	}
	if got := records[0].Context["arm_ref"]; got != "origin/nope" {
		t.Fatalf("context[arm_ref] = %v, want the ref the refusal named", got)
	}
}

func TestRefuseWithCarriesTheArmsOwnFieldValues(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()

	// Act.
	r := refuseWith(log, "CreateWorkspace", ArmBaseRefUnresolved, "no such ref", false,
		map[string]any{"ref": "origin/nope"})

	// Assert.
	if got := r.Fields["ref"]; got != "origin/nope" {
		t.Fatalf("Fields[\"ref\"] = %v, want the ref the refusal named", got)
	}
}

func TestRefuseWithDoesNotShareTheCallersFieldMap(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	fields := map[string]any{"ref": "origin/nope"}

	// Act.
	r := refuseWith(log, "CreateWorkspace", ArmBaseRefUnresolved, "no such ref", false, fields)
	fields["ref"] = "mutated"

	// Assert.
	if got := r.Fields["ref"]; got != "origin/nope" {
		t.Fatalf("Fields[\"ref\"] = %v, want the value at the refusal site", got)
	}
}

func TestWithRpcKeepsTheArmsFieldValues(t *testing.T) {
	// Arrange.
	r := &Refusal{Arm: ArmBaseRefUnresolved, Reason: "no such ref",
		Fields: map[string]any{"ref": "origin/nope"}}

	// Act.
	renamed := r.WithRpc("CreateWorkspace")

	// Assert.
	if got := renamed.Fields["ref"]; got != "origin/nope" {
		t.Fatalf("WithRpc dropped the arm's fields: %v", renamed.Fields)
	}
}
