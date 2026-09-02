//go:build integration

package integration

import (
	"os"
	"path/filepath"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/integration/harness"
)

// commandfile_test.go exercises the command-file ingress
// ($AGENT_REPL_STATE_DIR/output/workspace_commands_*.json), per SPEC.md and
// daemon/internal/commandfile/api.go's documented layout
// (daemon/AGENTS.md "State root layout": `output/workspace_commands_*.json`).
// Every command maps onto the SAME internal path as its rpc, so this suite
// asserts the SAME visible effects the rpc suites assert (roster rows, shim
// spawns, StartTurn) rather than re-deriving a second vocabulary.

// commandfileWrite drops one command file into the daemon's ingress
// directory under its exact contracted name.
func commandfileWrite(t *testing.T, d *harness.Daemon, name, body string) string {
	t.Helper()
	path := filepath.Join(d.StateDir, "output", name)
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatalf("mkdir the command-file ingress dir: %v", err)
	}
	if err := os.WriteFile(path, []byte(body), 0o644); err != nil {
		t.Fatalf("write %s: %v", path, err)
	}
	return path
}

// commandfileRowByName finds a roster row by its display name, anywhere in
// the repository grouping.
func commandfileRowByName(r *frontendv1.WorkspaceRoster, name string) *frontendv1.RosterRow {
	for _, s := range r.GetRepository().GetSections() {
		for _, row := range s.GetRows().GetRows() {
			if row.GetName().GetText() == name {
				return row
			}
			for _, c := range row.GetChildren() {
				if c.GetName().GetText() == name {
					return c
				}
			}
		}
	}
	return nil
}

func TestACreateEntryMaterializesAWorkspaceExactlyLikeCreateWorkspace(t *testing.T) {
	// Arrange
	repo := harness.NewRepo(t)
	d := harness.StartDaemon(t, harness.Opts{})
	roster := d.WatchRoster()

	// Act
	commandfileWrite(t, d, "workspace_commands_create.json",
		`[{"type":"create","git_root":"`+repo.Dir+`","name":"cmdfile-ws","prompt":"do the commandfile thing"}]`)

	// Assert: a workspace materializes exactly as CreateWorkspace would —
	// registered under the repository grouping, a real branch and worktree.
	got := awaitRoster(t, d, roster, "the command-file create's workspace row", func(r *frontendv1.WorkspaceRoster) bool {
		return commandfileRowByName(r, "cmdfile-ws") != nil
	})
	row := commandfileRowByName(got, "cmdfile-ws")
	ws := row.GetWorkspace().GetWorkspace()
	if ws.GetId() == "" {
		t.Fatalf("materialized row = %v, want a workspace identity", row)
	}
	if !repo.HasBranch("cmdfile-ws") {
		t.Fatalf("branches = %v, want cmdfile-ws materialized", repo.Branches())
	}

	// Assert: the initial prompt was submitted, exactly like CreateWorkspace.
	shim := d.Shim(ws)
	req := shim.ExpectStartTurn()
	if got := text(req.GetSaid()); got != "do the commandfile thing" {
		t.Fatalf("StartTurn.said = %q, want the command file's prompt", got)
	}
}

func TestAMergeEntryEnqueuesTheNamedWorkspace(t *testing.T) {
	// Arrange: a workspace with real layout facts, via CreateWorkspace (the
	// merge_test.go helper — same package, same conventions).
	repo := harness.NewRepo(t)
	d := harness.StartDaemon(t, harness.Opts{SelfRepo: repo.Dir})
	repoRef := mergeRepositoryRef(t, d, repo)
	f := mergeCreateChild(t, d, repoRef, "cmdfile-merge", "do the thing", nil)
	roster := d.WatchRoster()

	// Act
	commandfileWrite(t, d, "workspace_commands_merge.json",
		`[{"type":"merge","workspace":"`+f.ws.GetId()+`"}]`)

	// Assert: the roster shows the merge enqueued/running, exactly as
	// MergeWorkspace would.
	got := awaitRoster(t, d, roster, "the command-file merge enqueued", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, f.ws.GetId())
		return row.GetMergeEnqueuing() != nil || row.GetMerging() != nil ||
			row.GetMergeQueued() != nil || row.GetMergeConflict() != nil
	})
	row := rosterRow(got, f.ws.GetId())
	if row.GetNone() != nil || row.GetReady() != nil {
		t.Fatalf("command-file-merged workspace roster status = %v, want a merge arm", row)
	}
}

func TestAPromptEntrySubmitsToTheNamedWorkspace(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})

	// Act
	commandfileWrite(t, f.d, "workspace_commands_prompt.json",
		`[{"type":"prompt","workspace":"`+f.ws.GetId()+`","prompt":"hello from a command file"}]`)

	// Assert: delivered exactly like SubmitPrompt, stamped with the
	// legacy-host-prompt origin the command-file channel always carries.
	req := f.shim.ExpectStartTurn()
	if got := text(req.GetSaid()); got != "hello from a command file" {
		t.Fatalf("StartTurn.said = %q, want the command file's prompt", got)
	}
	if req.GetOrigin() != conversationv1.PromptOrigin_PROMPT_ORIGIN_LEGACY_HOST_PROMPT {
		t.Fatalf("StartTurn.origin = %v, want PROMPT_ORIGIN_LEGACY_HOST_PROMPT", req.GetOrigin())
	}
}

func TestAMalformedCommandFileIsQuarantinedAndLoggedNeverIngested(t *testing.T) {
	// Arrange
	d := harness.StartDaemon(t, harness.Opts{})
	path := commandfileWrite(t, d, "workspace_commands_bad.json", `{not even an array`)

	// Act / Assert: the file leaves its original place...
	d.AwaitFileGone(path)

	// ...and lands in quarantine rather than being silently dropped or
	// ingested.
	quarantined := filepath.Join(d.StateDir, "output", "quarantine", "workspace_commands_bad.json")
	if _, err := os.Stat(quarantined); err != nil {
		t.Fatalf("stat %s = %v, want the malformed file quarantined there", quarantined, err)
	}

	// Assert: logged.
	d.AwaitRunLogOperation("daemon.commandfile.quarantine")
	d.ExpectWarnings("daemon.commandfile.quarantine")
}

func TestIngestionIsAtomicAHalfWrittenFileIsNotClaimedUntilComplete(t *testing.T) {
	// Arrange
	d := harness.StartDaemon(t, harness.Opts{})
	partial := `[{"type":"task-cre`
	path := commandfileWrite(t, d, "workspace_commands_atomic.json", partial)

	// Act / Assert: a half-written file is left alone while still young —
	// never claimed on a truncated snapshot.
	d.ExpectFileUnchanged(path, partial, 150*time.Millisecond)

	// Act: complete the write before the file ages out.
	if err := os.WriteFile(path, []byte(`[{"type":"task-create","title":"atomic-cmdfile-task"}]`), 0o644); err != nil {
		t.Fatalf("complete the command file: %v", err)
	}

	// Assert: the COMPLETE request is what actually applies.
	roster := d.WatchRoster()
	got := awaitRoster(t, d, roster, "the completed file's task section", func(r *frontendv1.WorkspaceRoster) bool {
		return commandfileHasTaskTitled(r, "atomic-cmdfile-task")
	})
	if !commandfileHasTaskTitled(got, "atomic-cmdfile-task") {
		t.Fatalf("roster = %v, want the task created from the completed command file", got)
	}
}

// commandfileHasTaskTitled reports whether any task section's header carries
// the given title.
func commandfileHasTaskTitled(r *frontendv1.WorkspaceRoster, title string) bool {
	for _, s := range r.GetTask().GetSections() {
		if s.GetHeader().GetLabel().GetText() == title {
			return true
		}
	}
	return false
}
