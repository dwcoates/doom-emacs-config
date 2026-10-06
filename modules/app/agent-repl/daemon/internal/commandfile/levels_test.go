package commandfile

import (
	"context"
	"errors"
	"testing"

	"claude-repld/internal/merge"
	"claude-repld/internal/workspace"
)

// TestARefusedEntryIsAnAnswerAndAFailedOneIsAFault pins the level a refused
// entry and its file's quarantine are recorded at: a typed refusal is the
// verb's ANSWER to what the command asked (INFO); any other failure to act is
// a fault (WARN).
func TestARefusedEntryIsAnAnswerAndAFailedOneIsAFault(t *testing.T) {
	tests := []struct {
		name      string
		err       error
		wantLevel string
	}{
		{
			name: "a verb's typed refusal",
			err: &workspace.Refusal{Rpc: "CreateWorkspace", Arm: workspace.ArmInsideTemporaryDirectory,
				Reason: "/private/tmp/scratch is inside the temporary directory /private/tmp; agent-repl does not register temporary folders"},
			wantLevel: "info",
		},
		{name: "the merge orchestrator's typed refusal", err: &merge.RefusalError{Arm: merge.ArmNotPaused, Reason: "arranged"}, wantLevel: "info"},
		{name: "a failure that is no refusal", err: errors.New("the registry could not be read"), wantLevel: "warn"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			f.verbs.err = tt.err
			path := f.write(t, "workspace_commands_create.json",
				`[{"type":"create","git_root":"/repo","prompt":"do a thing"}]`)

			// Act.
			err := f.ingress.ApplyFile(context.Background(), path)

			// Assert.
			if !errors.Is(err, ErrQuarantined) {
				t.Fatalf("ApplyFile = %v, want the file quarantined", err)
			}
			entry := f.record(t, opEntry, tt.wantLevel)
			if entry.Context["cause"] == nil {
				t.Fatalf("entry record %+v carries no cause", entry)
			}
			quarantine := f.record(t, opQuarantine, tt.wantLevel)
			if quarantine.Context["refused"] != 1 {
				t.Fatalf("quarantine record %+v, want refused=1", quarantine)
			}
		})
	}
}

// TestAFileWithAnyFaultedEntryIsQuarantinedAtWarn pins that one fault among
// answers keeps the file's quarantine a warning: the answers do not launder
// the fault.
func TestAFileWithAnyFaultedEntryIsQuarantinedAtWarn(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", "/tree/w1")
	f.merge.pauseErr = &merge.RefusalError{Arm: merge.ArmAlreadyPaused, Reason: "arranged"}
	f.verbs.err = errors.New("the registry could not be read")
	path := f.write(t, "workspace_commands_mixed.json",
		`[{"type":"merge_pause","project_dir":"/tree/w1"},{"type":"create","git_root":"/repo","prompt":"do a thing"}]`)

	// Act.
	err := f.ingress.ApplyFile(context.Background(), path)

	// Assert.
	if !errors.Is(err, ErrQuarantined) {
		t.Fatalf("ApplyFile = %v, want the file quarantined", err)
	}
	f.record(t, opEntry, "info")
	f.record(t, opEntry, "warn")
	f.record(t, opQuarantine, "warn")
}
