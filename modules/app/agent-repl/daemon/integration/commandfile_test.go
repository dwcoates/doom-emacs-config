//go:build integration

package integration

import (
	"agentrepl/protohelpers/rosterwalk"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/integration/harness"
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

// commandfilePlace places one command file into DIR as every producer does
// (commandfile/write.go): the whole BODY is written under a dot name the
// ingress glob does not match, its mtime pinned to MTIME when one is given,
// and renamed into place as NAME (a rename keeps the mtime). The sweep never
// sees a partly written or not yet pinned file.
func commandfilePlace(t *testing.T, dir, name, body string, mtime *time.Time) string {
	t.Helper()
	staged := filepath.Join(dir, "."+name)
	if err := os.WriteFile(staged, []byte(body), 0o644); err != nil {
		t.Fatalf("stage %s: %v", staged, err)
	}
	if mtime != nil {
		if err := os.Chtimes(staged, *mtime, *mtime); err != nil {
			t.Fatalf("pin %s's mtime: %v", staged, err)
		}
	}
	path := filepath.Join(dir, name)
	if err := os.Rename(staged, path); err != nil {
		t.Fatalf("place %s: %v", path, err)
	}
	return path
}

// commandfileRowByName finds a roster row by its display name, anywhere in
// the repository grouping.
func commandfileRowByName(r *frontendv1.WorkspaceRoster, name string) *frontendv1.RosterRow {
	for _, open := range rosterwalk.OpenRows(r) {
		if open.GetRow().GetName().GetText() == name {
			return open.GetRow()
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

func TestIngestionIsAtomicAHalfWrittenFileIsNotClaimedUntilComplete(t *testing.T) {
	t.Parallel()
	// Arrange
	d := harness.StartDaemon(t, harness.Opts{})
	partial := `[{"type":"task-cre`
	// THE FILE'S YOUTH IS PINNED, NOT RACED. The ingress claims a file once it
	// is one poll interval old whether or not it parses, so a wall-clock probe
	// shorter than that interval raced the sweep: under a loaded run the
	// completing write landed after the partial had aged out and was
	// quarantined. A future mtime keeps the partial young for every sweep this
	// test can see, which is exactly the condition under test.
	//
	// THE PIN IS IN PLACE BEFORE THE SWEEP CAN SEE THE FILE: the partial is
	// written and pinned under a name the glob does not match, then renamed
	// into place (a rename keeps the mtime). Pinning it after writing it in
	// place raced the sweep the same way: a sweep between the write and the
	// pin claimed it by age and quarantined it, and the pin then found it gone.
	dir := filepath.Join(d.StateDir, "output")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("mkdir the command-file ingress dir: %v", err)
	}
	future := time.Now().Add(time.Hour)
	path := commandfilePlace(t, dir, "workspace_commands_atomic.json", partial, &future)

	// Act / Assert: a sweep judges the half-written file and leaves it alone —
	// never claimed on a truncated snapshot.
	d.AwaitLogRecord(d.RunLogPath(), "the sweep leaving the half-written file", func(r harness.LogRecord) bool {
		return r.PID == d.PID() && r.Operation == "daemon.commandfile.run" && r.Level == "debug" &&
			r.Message == "leaving a command file that is still being written" && r.Context["path"] == path
	})
	if body, err := os.ReadFile(path); err != nil || string(body) != partial {
		t.Fatalf("after a sweep left it, %s = (%q, %v), want it unchanged at %q", path, body, err, partial)
	}

	// Act: complete the write before the file ages out, ATOMICALLY, as every
	// producer does (commandfile/write.go, emit-workspace-commands.sh): the
	// whole document is written under a name the glob does not match and
	// renamed over the partial. An in-place os.WriteFile truncated the file
	// first, and a writer descheduled between the truncation and the write
	// left an EMPTY file whose fresh mtime aged past one poll interval on a
	// loaded run, which the ingress then rightly claimed by age and
	// quarantined: a race of this test's own making, not of the producers'.
	commandfilePlace(t, dir, "workspace_commands_atomic.json", `[{"type":"task-create","title":"atomic-cmdfile-task"}]`, nil)

	// Assert: the COMPLETE request is what actually applies.
	d.AwaitLogRecord(d.RunLogPath(), "the task the completed file creates", func(r harness.LogRecord) bool {
		return r.PID == d.PID() && r.Operation == "daemon.workspace.create_task" && r.Context["title"] == "atomic-cmdfile-task"
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

func TestCommandfilePlaceLandsTheWholeBodyUnderItsName(t *testing.T) {
	pinned := time.Unix(1_900_000_000, 0)
	tests := []struct {
		name  string
		mtime *time.Time
	}{
		{name: "unpinned", mtime: nil},
		{name: "pinned", mtime: &pinned},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			dir := t.TempDir()

			// Act.
			path := commandfilePlace(t, dir, "workspace_commands_x.json", "[]", tc.mtime)

			// Assert: only the placed file stands, whole, with its pin.
			entries, err := os.ReadDir(dir)
			if err != nil || len(entries) != 1 || entries[0].Name() != "workspace_commands_x.json" || path != filepath.Join(dir, "workspace_commands_x.json") {
				t.Fatalf("dir holds %v (%v), want only the placed file at %s", entries, err, path)
			}
			body, _ := os.ReadFile(path)
			info, _ := os.Stat(path)
			if string(body) != "[]" || (tc.mtime != nil && !info.ModTime().Equal(*tc.mtime)) {
				t.Fatalf("placed %q at %v, want [] pinned to %v", body, info.ModTime(), tc.mtime)
			}
		})
	}
}

func TestTheAtomicIngestionTestPlacesEveryFileThroughCommandfilePlace(t *testing.T) {
	// Assert: no hand-rolled stage-and-rename beside the helper.
	src, err := os.ReadFile("commandfile_test.go")
	if err != nil {
		t.Fatalf("read the suite: %v", err)
	}
	// The needle is split so this assertion is not itself a match.
	if n := strings.Count(string(src), "os."+"Rename("); n != 1 {
		t.Fatalf("the suite renames in %d places, want only commandfilePlace's", n)
	}
}
