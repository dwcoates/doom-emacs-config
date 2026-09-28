//go:build integration

package integration

import (
	"os"
	"path/filepath"
	"strings"
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
	t.Parallel()
	// Arrange
	repo := harness.NewRepo(t)
	d := harness.StartDaemon(t, harness.Opts{})
	// THE REPOSITORY IS REGISTERED FIRST, which is what the editor does when it
	// opens the tree: a create naming a repository the registry does not hold is
	// refused on `unknown_repository', through the command-file channel exactly
	// as through the rpc.
	harness.Register(t, d, repo.Dir)
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
	t.Parallel()
	// Arrange: a workspace with real layout facts, via CreateWorkspace (the
	// merge_test.go helper — same package, same conventions).
	repo := harness.NewRepo(t)
	d := harness.StartDaemon(t, harness.Opts{SelfRepo: repo.Dir})
	repoRef := mergeRepositoryRef(t, d, repo)
	f := mergeCreateChild(t, d, repoRef, "cmdfile-merge", "do the thing", nil)
	harness.CommitWork(t, f.ws.GetDir())
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
	t.Parallel()
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

func TestASendEntrySubmitsToTheNamedWorkspaceExactlyLikePrompt(t *testing.T) {
	t.Parallel()
	// Arrange: "send" is prompt's older spelling and behaves identically.
	f := newOpened(t, harness.Opts{})

	// Act
	commandfileWrite(t, f.d, "workspace_commands_send.json",
		`[{"type":"send","workspace":"`+f.ws.GetId()+`","prompt":"hello from a send entry"}]`)

	// Assert: delivered exactly like a prompt entry, including the origin.
	req := f.shim.ExpectStartTurn()
	if got := text(req.GetSaid()); got != "hello from a send entry" {
		t.Fatalf("StartTurn.said = %q, want the send entry's prompt", got)
	}
	if req.GetOrigin() != conversationv1.PromptOrigin_PROMPT_ORIGIN_LEGACY_HOST_PROMPT {
		t.Fatalf("StartTurn.origin = %v, want PROMPT_ORIGIN_LEGACY_HOST_PROMPT", req.GetOrigin())
	}
}

func TestACloseEntryClosesTheNamedWorkspaceOnTheRoster(t *testing.T) {
	t.Parallel()
	// Arrange: registered but never opened, so it is quiet and closable.
	d := harness.StartDaemon(t, harness.Opts{})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, d, repo.Dir)
	roster := d.WatchRoster()

	// Act
	commandfileWrite(t, d, "workspace_commands_close.json",
		`[{"type":"close","workspace":"`+ws.GetId()+`"}]`)

	// Assert: closed exactly like CloseWorkspace draws closed:true.
	got := awaitRoster(t, d, roster, "the command-file close reflected", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, ws.GetId())
		return row != nil && row.GetClosed().GetClosed()
	})
	if row := rosterRow(got, ws.GetId()); !row.GetClosed().GetClosed() {
		t.Fatalf("command-file-closed workspace roster row.closed = false, want true")
	}
}

func TestAnOpenEntrySpawnsTheShimForAClosedWorkspace(t *testing.T) {
	t.Parallel()
	// Arrange: registered but never opened, so nothing has spawned a shim yet.
	f := newRegistered(t, harness.Opts{})

	// Act
	commandfileWrite(t, f.d, "workspace_commands_open.json",
		`[{"type":"open","workspace":"`+f.ws.GetId()+`"}]`)

	// Assert: the fake shim's control socket comes up exactly like
	// OpenWorkspace would spawn it. Shim blocks on the connect, bounded by the
	// daemon's own context, so a shim that never spawns fails the test loudly
	// rather than passing silently.
	if shim := f.d.Shim(f.ws); shim == nil {
		t.Fatalf("f.d.Shim(f.ws) = nil, want a connected control client for the command-file-opened shim")
	}
}

func TestASwitchEntrySelectsTheNamedWorkspace(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{})
	roster := f.d.WatchRoster()

	// Act
	commandfileWrite(t, f.d, "workspace_commands_switch.json",
		`[{"type":"switch","workspace":"`+f.ws.GetId()+`"}]`)

	// Assert: stamped current exactly like SelectWorkspace.
	got := awaitRoster(t, f.d, roster, "the command-file switch stamped current", func(r *frontendv1.WorkspaceRoster) bool {
		return r.GetCurrent().GetWorkspace().GetId() == f.ws.GetId()
	})
	row := rosterRow(got, f.ws.GetId())
	if row == nil || !row.GetCurrent().GetCurrent() {
		t.Fatalf("command-file-switched workspace row.current = %v, want current=true", row.GetCurrent())
	}
}

func TestATaskToggleDoneEntryChecksTheTaskSection(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{})
	task := createTask(t, f, "toggle via commandfile")
	roster := f.d.WatchRoster()
	awaitRoster(t, f.d, roster, "the fresh task section", func(r *frontendv1.WorkspaceRoster) bool {
		return taskSection(r, task.GetId()) != nil
	})

	// Act
	commandfileWrite(t, f.d, "workspace_commands_task_toggle.json",
		`[{"type":"task-toggle-done","id":"`+task.GetId()+`","done":true}]`)

	// Assert: checked exactly like UpdateTask{set_done}.
	got := awaitRoster(t, f.d, roster, "the task section's done check", func(r *frontendv1.WorkspaceRoster) bool {
		s := taskSection(r, task.GetId())
		return s != nil && s.GetHeader().GetDone().GetDone()
	})
	if !taskSection(got, task.GetId()).GetHeader().GetDone().GetDone() {
		t.Fatalf("command-file-toggled task section done = false, want true")
	}
}

func TestATaskAddWorkspaceEntryAssignsTheNamedWorkspace(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{})
	task := createTask(t, f, "assign via commandfile")
	roster := f.d.WatchRoster()

	// Act
	commandfileWrite(t, f.d, "workspace_commands_task_assign.json",
		`[{"type":"task-add-workspace","id":"`+task.GetId()+`","workspace":"`+f.ws.GetId()+`"}]`)

	// Assert: grouped under the task exactly like AssignWorkspaceTask.
	got := awaitRoster(t, f.d, roster, "the workspace grouped under its task", func(r *frontendv1.WorkspaceRoster) bool {
		return rosterTaskRow(r, task.GetId(), f.ws.GetId()) != nil
	})
	if rosterTaskRow(got, task.GetId(), f.ws.GetId()) == nil {
		t.Fatalf("roster = %v, want %s grouped under task %s", got, f.ws.GetId(), task.GetId())
	}
}

func TestADirAddressedPromptEntryResolvesTheSameWorkspaceAsAnIdAddressedOne(t *testing.T) {
	t.Parallel()
	// Arrange: registered directly on the repo root, so its dir is exactly
	// repo.Dir — the shape the older dir-only producers write, resolved
	// through wsm.DB.WorkspaceByDir rather than the verbs' ref resolution.
	f := newOpened(t, harness.Opts{})

	// Act
	commandfileWrite(t, f.d, "workspace_commands_dir.json",
		`[{"type":"prompt","dir":"`+f.repo.Dir+`","prompt":"hello via dir addressing"}]`)

	// Assert: delivered to the SAME workspace a workspace-addressed entry
	// would reach, exactly like SubmitPrompt.
	req := f.shim.ExpectStartTurn()
	if got := text(req.GetSaid()); got != "hello via dir addressing" {
		t.Fatalf("StartTurn.said = %q, want the dir-addressed entry's prompt", got)
	}
}

func TestAOneShotCreateEntryDecoratesThePromptLikeCreateWorkspace(t *testing.T) {
	t.Parallel()
	// Arrange
	repo := harness.NewRepo(t)
	d := harness.StartDaemon(t, harness.Opts{})
	// THE REPOSITORY IS REGISTERED FIRST, which is what the editor does when it
	// opens the tree: a create naming a repository the registry does not hold is
	// refused on `unknown_repository', through the command-file channel exactly
	// as through the rpc.
	harness.Register(t, d, repo.Dir)
	roster := d.WatchRoster()

	// Act
	commandfileWrite(t, d, "workspace_commands_oneshot.json",
		`[{"type":"create","git_root":"`+repo.Dir+`","name":"cmdfile-oneshot","prompt":"add a health check endpoint","one_shot":true}]`)

	// Assert: the sent prompt is the user's text PLUS the success-suffix
	// brief spliced around it, never the bare text — exactly like a one-shot
	// CreateWorkspace.
	got := awaitRoster(t, d, roster, "the one-shot command-file workspace's row", func(r *frontendv1.WorkspaceRoster) bool {
		return commandfileRowByName(r, "cmdfile-oneshot") != nil
	})
	row := commandfileRowByName(got, "cmdfile-oneshot")
	ws := row.GetWorkspace().GetWorkspace()
	shim := d.Shim(ws)
	shim.ExpectStartSession()
	saidText := text(shim.ExpectStartTurn().GetSaid())
	if !strings.Contains(saidText, "add a health check endpoint") {
		t.Fatalf("the command-file one-shot's decorated prompt = %q, want the user's prompt spliced in", saidText)
	}
	if !strings.Contains(strings.ToLower(saidText), "invoke") {
		t.Fatalf("the command-file one-shot's decorated prompt = %q, want the success-suffix brief spliced in", saidText)
	}
}

func TestACreateEntryHonorsAnExplicitBaseRef(t *testing.T) {
	t.Parallel()
	// Arrange
	repo := harness.NewRepo(t)
	d := harness.StartDaemon(t, harness.Opts{})
	// THE REPOSITORY IS REGISTERED FIRST, which is what the editor does when it
	// opens the tree: a create naming a repository the registry does not hold is
	// refused on `unknown_repository', through the command-file channel exactly
	// as through the rpc.
	harness.Register(t, d, repo.Dir)
	repo.Branch("release")
	repo.Checkout(harness.DefaultBranch)
	roster := d.WatchRoster()

	// Act
	commandfileWrite(t, d, "workspace_commands_baseref.json",
		`[{"type":"create","git_root":"`+repo.Dir+`","name":"cmdfile-baseref","base_ref":"release"}]`)

	// Assert: the worktree was cut from the named base ref, not the default —
	// exactly like CreateWorkspace{base_ref}.
	awaitRoster(t, d, roster, "the command-file create's workspace row", func(r *frontendv1.WorkspaceRoster) bool {
		return commandfileRowByName(r, "cmdfile-baseref") != nil
	})
	found := false
	for _, c := range d.Git.Calls() {
		if createArgsContainAll(c.Args, "worktree", "add") && createArgsContain(c.Args, "release") {
			found = true
		}
	}
	if !found {
		t.Fatalf("git calls = %v, want a `worktree add` naming the base ref %q", d.Git.Calls(), "release")
	}
}

func TestAFileWithOneInvalidEntryAppliesNothing(t *testing.T) {
	t.Parallel()
	// Arrange: one valid entry (a task-create) followed by one invalid entry
	// (a prompt missing its required prompt text) in the SAME array. Every
	// entry is validated before any of them is applied, so this is one
	// request, and half of it is not a smaller request.
	d := harness.StartDaemon(t, harness.Opts{})
	roster := d.WatchRoster()
	awaitRoster(t, d, roster, "the daemon's initial roster", func(r *frontendv1.WorkspaceRoster) bool { return true })
	path := commandfileWrite(t, d, "workspace_commands_partial_invalid.json",
		`[{"type":"task-create","title":"should-never-appear"},{"type":"prompt","workspace":"whatever"}]`)

	// Act / Assert: the whole file is refused and quarantined, exactly like a
	// syntactically malformed one.
	d.AwaitFileGone(path)
	quarantined := filepath.Join(d.StateDir, "output", "quarantine", "workspace_commands_partial_invalid.json")
	d.AwaitFileExists(quarantined)
	d.AwaitRunLogOperation("daemon.commandfile.quarantine")
	d.ExpectWarnings("daemon.commandfile.quarantine")

	// Assert: the valid entry that preceded the invalid one applied NOTHING —
	// no roster push at all follows the quarantine (a task-create would push
	// a fresh task section).
	harness.ExpectNoPush(t, roster, harness.ProbeWindow, "a quarantined file applies nothing, not even its valid entries")
}

// TestAFileRouteMergeOnAnUnmergeableWorkspaceIsQuarantined pins down the
// audit's critique that a merge entry naming an unmergeable workspace is
// quarantined like any other command file the ingress cannot honor.
//
// Per ingress.go as read: ArmNoLayoutFacts surfaces only once the entry
// actually APPLIES (i.deps.Merge.Enqueue returns the refusal from inside
// ApplyFile's per-entry loop), which is AFTER the file is already claimed —
// unlike the parse-time refusals TestAMalformedCommandFileIsQuarantinedAndLoggedNeverIngested
// and TestAFileWithOneInvalidEntryAppliesNothing exercise, which quarantine
// before ever leaving the parse step. Only i.quarantine (called from the
// parse-failure branch) ever moves a file into QuarantineDir; an apply-time
// failure is instead joined into ApplyFile's returned error and logged at
// ERROR under daemon.commandfile.entry / daemon.commandfile.run, leaving the
// file sitting in ClaimedDir. This test asserts the CONTRACT the critique
// names; if the file never reaches quarantine, that is the gap to report.
func TestAFileRouteMergeOnAnUnmergeableWorkspaceIsQuarantined(t *testing.T) {
	t.Parallel()
	// Arrange: registered directly, never created, so it carries no creation
	// job — internal/merge's layoutFor refuses ANY merge of it
	// (ArmNoLayoutFacts), which is as unmergeable as a workspace gets.
	f := newRegistered(t, harness.Opts{})

	// Act
	path := commandfileWrite(t, f.d, "workspace_commands_unmergeable.json",
		`[{"type":"merge","workspace":"`+f.ws.GetId()+`"}]`)

	// Assert
	f.d.AwaitFileGone(path)
	quarantined := filepath.Join(f.d.StateDir, "output", "quarantine", "workspace_commands_unmergeable.json")
	f.d.AwaitFileExists(quarantined)
	f.d.AwaitRunLogOperation("daemon.commandfile.quarantine")
	// The refusal is RECORDED on the way to quarantine: the orchestrator's own
	// refusal, the entry that carried it, and the file's retirement.
	f.d.ExpectWarnings("daemon.commandfile.quarantine", "daemon.commandfile.entry",
		"daemon.merge.enqueue")
}

func TestAMalformedCommandFileIsQuarantinedAndLoggedNeverIngested(t *testing.T) {
	t.Parallel()
	// Arrange
	d := harness.StartDaemon(t, harness.Opts{})
	path := commandfileWrite(t, d, "workspace_commands_bad.json", `{not even an array`)

	// Act / Assert: the file leaves its original place...
	d.AwaitFileGone(path)

	// ...and lands in quarantine rather than being silently dropped or
	// ingested. The wait is for the file to ARRIVE there: leaving its own
	// place is the CLAIM's rename, and the quarantine is a second one.
	quarantined := filepath.Join(d.StateDir, "output", "quarantine", "workspace_commands_bad.json")
	d.AwaitFileExists(quarantined)

	// Assert: logged.
	d.AwaitRunLogOperation("daemon.commandfile.quarantine")
	d.ExpectWarnings("daemon.commandfile.quarantine")
}

func TestIngestionIsAtomicAHalfWrittenFileIsNotClaimedUntilComplete(t *testing.T) {
	t.Parallel()
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

func TestAForgetEntryRemovesTheWorkspaceFromTheRoster(t *testing.T) {
	t.Parallel()
	// Arrange: registered but never opened, so it is quiet and closable. The
	// close and the forget travel in ONE file, applied in order: forget is the
	// undo for a registration and it refuses an open workspace.
	d := harness.StartDaemon(t, harness.Opts{})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, d, repo.Dir)
	roster := d.WatchRoster()

	// Act
	commandfileWrite(t, d, "workspace_commands_forget.json",
		`[{"type":"close","workspace":"`+ws.GetId()+`"},`+
			`{"type":"forget","workspace":"`+ws.GetId()+`"}]`)

	// Assert: the row LEAVES the roster — nothing else ever removed a
	// registration, so a registered directory could only ever be closed.
	awaitRoster(t, d, roster, "the command-file forget's row removal", func(r *frontendv1.WorkspaceRoster) bool {
		return rosterRow(r, ws.GetId()) == nil
	})
}

func TestAForgetEntryTakesTheLastWorkspacesRepositoryWithIt(t *testing.T) {
	t.Parallel()
	// Arrange: one workspace, so its repository holds nothing else once it is
	// forgotten. Registering a directory mints BOTH records and nothing ever
	// deleted the repository one.
	d := harness.StartDaemon(t, harness.Opts{})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, d, repo.Dir)
	roster := d.WatchRoster()

	// Act
	commandfileWrite(t, d, "workspace_commands_forget_repo.json",
		`[{"type":"close","workspace":"`+ws.GetId()+`"},`+
			`{"type":"forget","workspace":"`+ws.GetId()+`"}]`)

	// Assert: the repository grouping has no section left naming a path the
	// registry no longer holds a workspace for.
	awaitRoster(t, d, roster, "the forgotten repository's section", func(r *frontendv1.WorkspaceRoster) bool {
		return len(r.GetRepository().GetSections()) == 0
	})
}

func TestAForgetEntryOnAnOpenWorkspaceIsQuarantined(t *testing.T) {
	t.Parallel()
	// Arrange: registered and never closed. Forget cannot tear editor state
	// down, and the close verb owns the quiet requirement it would otherwise
	// have to bypass, so the open workspace is refused.
	f := newRegistered(t, harness.Opts{})

	// Act
	path := commandfileWrite(t, f.d, "workspace_commands_forget_open.json",
		`[{"type":"forget","workspace":"`+f.ws.GetId()+`"}]`)

	// Assert
	f.d.AwaitFileGone(path)
	quarantined := filepath.Join(f.d.StateDir, "output", "quarantine", "workspace_commands_forget_open.json")
	f.d.AwaitFileExists(quarantined)
	f.d.AwaitRunLogOperation("daemon.commandfile.quarantine")
	f.d.ExpectWarnings("daemon.commandfile.quarantine", "daemon.commandfile.entry")
}

func TestAQuarantinedForgetLeavesTheWorkspaceOnTheRoster(t *testing.T) {
	t.Parallel()
	// Arrange: a refused forget destroys nothing, so the registration stands.
	f := newRegistered(t, harness.Opts{})
	roster := f.d.WatchRoster()
	awaitRoster(t, f.d, roster, "the registered workspace's row", func(r *frontendv1.WorkspaceRoster) bool {
		return rosterRow(r, f.ws.GetId()) != nil
	})

	// Act
	path := commandfileWrite(t, f.d, "workspace_commands_forget_refused.json",
		`[{"type":"forget","workspace":"`+f.ws.GetId()+`"}]`)
	f.d.AwaitFileGone(path)
	f.d.AwaitRunLogOperation("daemon.commandfile.quarantine")

	// Assert: the refusal republished nothing, so the row the roster already
	// carries is still the registry's answer.
	harness.ExpectNoPush(t, roster, harness.ProbeWindow, "a refused forget changes no registry fact")
	f.d.ExpectWarnings("daemon.commandfile.quarantine", "daemon.commandfile.entry")
}

// TestForgettingAWorkspaceWithALiveSessionRecordsNoFault covers the realtest
// harvest's own cleanup: the command file closes and then forgets a scratch
// workspace whose SHIM IS STILL UP, because a close is view-level and leaves
// the session alone. The forget stands that session down, and every side that
// then sees the departure — the liveness monitor, the redial ladder, the exit
// witness — must read a teardown this daemon ordered as an ordinary event.
//
// The assertion is the harness's own warning sweep, which fails this test on
// any WARN or ERROR record no ExpectWarnings declared. Before the stand-down
// latch was armed by the process kill, this run recorded
// `daemon.shimclient.exit` ERROR "adopted shim is gone" and two
// `daemon.shimclient.redial` WARNs (between-sweeps harvest, 2026-09-13).
func TestForgettingAWorkspaceWithALiveSessionRecordsNoFault(t *testing.T) {
	t.Parallel()
	// Arrange: a worktree workspace with a live spawned shim. The forget's
	// stand-down is the LAST verb that can address it, so this is the shape
	// the defect lived in.
	f := newOpenedWorktree(t, harness.Opts{}, "forget-live-session")
	roster := f.d.WatchRoster()

	// Act
	commandfileWrite(t, f.d, "workspace_commands_forget_live.json",
		`[{"type":"close","workspace":"`+f.ws.GetId()+`"},`+
			`{"type":"forget","workspace":"`+f.ws.GetId()+`"}]`)

	// Assert: the row leaves, and the cleanup sweep finds no fault behind it.
	awaitRoster(t, f.d, roster, "the forgotten live workspace's row removal", func(r *frontendv1.WorkspaceRoster) bool {
		return rosterRow(r, f.ws.GetId()) == nil
	})
}

// TestACreateEntryNamingAnUnregisteredRepositoryIsRefused pins the command-file
// half of the repository invariant. A workspace whose repository is
// unregistered must be impossible (owner ruling, 2026-09-13), so a create
// naming a directory the registry does not hold is refused on
// `unknown_repository' here exactly as it is on the rpc — and the file is
// quarantined like every other command file the ingress cannot honor.
func TestACreateEntryNamingAnUnregisteredRepositoryIsRefused(t *testing.T) {
	t.Parallel()
	// Arrange: a real repository on disk that nothing has registered.
	repo := harness.NewRepo(t)
	d := harness.StartDaemon(t, harness.Opts{})

	// Act
	path := commandfileWrite(t, d, "workspace_commands_unregistered.json",
		`[{"type":"create","git_root":"`+repo.Dir+`","name":"stray-ws","prompt":"build it"}]`)

	// Assert
	d.AwaitFileGone(path)
	d.AwaitFileExists(filepath.Join(d.StateDir, "output", "quarantine", "workspace_commands_unregistered.json"))
	d.AwaitRunLogOperation("daemon.commandfile.quarantine")
	d.ExpectWarnings("daemon.commandfile.quarantine", "daemon.commandfile.entry")
	if repo.HasBranch("stray-ws") {
		t.Fatalf("branches = %v, want nothing materialized for the refused create", repo.Branches())
	}
}
