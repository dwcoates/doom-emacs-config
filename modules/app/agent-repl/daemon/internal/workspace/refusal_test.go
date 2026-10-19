package workspace

import (
	"context"
	"errors"
	"fmt"
	"slices"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/sourcescan"
	"agent-repld/internal/wsm"
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

func TestRefuseUnrestorableRefusesUnderTheUnrestorableArm(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()

	// Act.
	got := refuseUnrestorable(log, "OpenWorkspace", wsm.Workspace{Dir: "/w/gone", Branch: "feature/gone"}, "both gone")

	// Assert.
	if got.Rpc != "OpenWorkspace" || got.Arm != ArmWorktreeUnrestorable || got.Reason != "both gone" {
		t.Fatalf("refusal = %+v, want OpenWorkspace's worktree_unrestorable saying both gone", got)
	}
}

func TestRefuseUnrestorableCarriesTheRecordedDirAndBranch(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()

	// Act.
	got := refuseUnrestorable(log, "OpenWorkspace", wsm.Workspace{Dir: "/w/gone", Branch: "feature/gone"}, "both gone")

	// Assert.
	if got.Fields["dir"] != "/w/gone" || got.Fields["branch"] != "feature/gone" {
		t.Fatalf("fields = %v, want the recorded dir and branch", got.Fields)
	}
}

// TestOnlyRefuseUnrestorableRaisesTheUnrestorableArm pins refuseUnrestorable as
// the one place this package raises worktree_unrestorable, so no refusal of it
// can leave out the evidence the arm carries.
func TestOnlyRefuseUnrestorableRaisesTheUnrestorableArm(t *testing.T) {
	// Arrange, Act.
	got := functionsReferencing(t, "ArmWorktreeUnrestorable")

	// Assert.
	if !slices.Equal(got, []string{"refuseUnrestorable"}) {
		t.Fatalf("functions naming ArmWorktreeUnrestorable = %v, want only refuseUnrestorable", got)
	}
}

func TestRefusedByFamilyAnswersOnlyTheFamilyRules(t *testing.T) {
	tests := []struct {
		name string
		err  error
		want bool
	}{
		{name: "open children", err: fmt.Errorf("close: %w", errOpenChildren), want: true},
		{name: "a parent not open", err: fmt.Errorf("open: %w", errParentNotOpen), want: true},
		{name: "an ordinary failure", err: errors.New("disk I/O error"), want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			log := dlog.NewTestLogger()

			// Act
			got := refusedByFamily(log, "daemon.workspace.test", tt.err)

			// Assert
			_, recorded := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: "daemon.workspace.test"})
			if got != tt.want || recorded != tt.want {
				t.Fatalf("refusedByFamily = %v (recorded %v), want %v", got, recorded, tt.want)
			}
		})
	}
}

func TestRequireOpenParent(t *testing.T) {
	tests := []struct {
		name      string
		err       error
		wantErr   bool
		wantLevel string
	}{
		{name: "an open parent", err: nil},
		{name: "a parent that is not open", err: errParentNotOpen, wantErr: true, wantLevel: dlog.LevelInfo},
		{name: "a read that failed", err: errors.New("disk I/O error"), wantErr: true, wantLevel: dlog.LevelError},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.db.openParentErr = tt.err

			// Act
			err := f.verbs.(*verbs).requireOpenParent(context.Background(), f.log.logger, "daemon.workspace.test", "parent")

			// Assert
			if (err != nil) != tt.wantErr || (tt.err != nil && !errors.Is(err, tt.err)) {
				t.Fatalf("requireOpenParent = %v, want %v", err, tt.err)
			}
			if tt.wantErr {
				if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: tt.wantLevel, Operation: "daemon.workspace.test"}); !ok {
					t.Fatalf("no %s record under the operation", tt.wantLevel)
				}
			}
			if !slices.Equal(f.db.openParentAsked, []ids.WorkspaceID{"parent"}) {
				t.Fatalf("asked about %v, want [parent]", f.db.openParentAsked)
			}
		})
	}
}

func TestEveryParentPreCheckGoesThroughRequireOpenParent(t *testing.T) {
	// Act.
	direct := sourcescan.Count(t, "v.deps.DB.RequireOpenParent(")
	shared := sourcescan.Count(t, "v.requireOpenParent(ctx, ")

	// Assert.
	if direct != 1 || shared != 2 {
		t.Fatalf("direct reads = %d, shared calls = %d; want Open and Create both through the one helper", direct, shared)
	}
}
