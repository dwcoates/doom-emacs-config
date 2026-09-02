//go:build integration

package integration

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/integration/harness"

	"connectrpc.com/connect"
)

// ---- Standard create ----

func TestCreateWorkspaceStandardDerivesTheSlugCreatesTheBranchAndSubmitsTheInitialPromptWithWorkspaceCreatedOrigin(t *testing.T) {
	// Arrange
	d := newDaemon(t, harness.Opts{})
	repo := harness.NewRepo(t)
	repository := createRepositoryRef(t, d, repo)

	// Act
	resp, err := d.Client().CreateWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Repository: repository,
		Form: &agentreplv1.CreateWorkspaceRequest_Standard{Standard: &agentreplv1.CreateWorkspaceStandard{
			InitialPrompt: said("fix the flaky reconnect test"),
		}},
	}))

	// Assert: minted ref, worktree and branch created off the default branch.
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("CreateWorkspace(standard) = (%v, %v), want a success", resp, err)
	}
	ws := resp.Msg.GetSuccess().GetWorkspace()
	if ws.GetId() == "" || ws.GetDir() == "" {
		t.Fatalf("CreateWorkspace success = %v, want a minted workspace ref", resp.Msg)
	}
	if !repo.HasWorktree(ws.GetDir()) {
		t.Fatalf("the repository's worktrees = %v, want the new worktree %q", repo.Worktrees(), ws.GetDir())
	}
	if got := len(repo.Branches()); got < 2 {
		t.Fatalf("the repository's branches = %v, want a new branch cut off %q for the workspace", repo.Branches(), harness.DefaultBranch)
	}

	// Assert: a slug was derived.
	host := d.WatchHost(ws)
	naming := harness.AwaitView(t, d.Ctx(), host, "a derived slug", func(r *agentreplv1.WatchHostWorkspaceResponse) bool {
		return r.GetHost().GetNaming().GetSlug() != ""
	}).GetHost().GetNaming()
	if naming.GetSlug() == "" {
		t.Fatalf("the new workspace's naming.slug = %q, want a slug derived from the initial prompt", naming.GetSlug())
	}

	// Assert: the initial prompt was submitted, WORKSPACE_CREATED-attributed.
	shim := d.Shim(ws)
	fresh := shim.ExpectStartSession()
	if fresh.GetFresh() == nil {
		t.Fatalf("StartSession request = %v, want fresh for a brand-new workspace", fresh)
	}
	turn := shim.ExpectStartTurn()
	if text(turn.GetSaid()) != "fix the flaky reconnect test" {
		t.Fatalf("StartTurn.said = %q, want the initial prompt verbatim", text(turn.GetSaid()))
	}
	if turn.GetOrigin() != conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_CREATED {
		t.Fatalf("StartTurn.origin = %v, want WORKSPACE_CREATED", turn.GetOrigin())
	}
}

func TestCreateWorkspaceHonorsAnExplicitBaseRef(t *testing.T) {
	// Arrange
	d := newDaemon(t, harness.Opts{})
	repo := harness.NewRepo(t)
	repository := createRepositoryRef(t, d, repo)
	repo.Branch("release")
	repo.Checkout(harness.DefaultBranch)
	baseRef := "release"

	// Act
	resp, err := d.Client().CreateWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Repository: repository,
		Form: &agentreplv1.CreateWorkspaceRequest_Standard{Standard: &agentreplv1.CreateWorkspaceStandard{
			BaseRef: &baseRef,
		}},
	}))

	// Assert: the worktree was cut from the named base ref, not the default.
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("CreateWorkspace(base_ref=%q) = (%v, %v), want a success", baseRef, resp, err)
	}
	found := false
	for _, c := range d.Git.Calls() {
		if createArgsContainAll(c.Args, "worktree", "add") && createArgsContain(c.Args, baseRef) {
			found = true
		}
	}
	if !found {
		t.Fatalf("git calls = %v, want a `worktree add` naming the base ref %q", d.Git.Calls(), baseRef)
	}
}

func TestCreateWorkspaceWithABadBaseRefIsRefused(t *testing.T) {
	// Arrange
	d := newDaemon(t, harness.Opts{})
	// The sweep covers every test; the declared records are evidence of the bad base ref the test stages.
	d.ExpectWarnings("daemon.gitclient.resolve_ref")
	repo := harness.NewRepo(t)
	repository := createRepositoryRef(t, d, repo)
	bad := "does-not-exist"

	// Act
	resp, err := d.Client().CreateWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Repository: repository,
		Form: &agentreplv1.CreateWorkspaceRequest_Standard{Standard: &agentreplv1.CreateWorkspaceStandard{
			BaseRef: &bad,
		}},
	}))

	// Assert
	if err != nil {
		t.Fatalf("CreateWorkspace(bad base_ref) = error %v, want a typed base_ref_unresolved answer", err)
	}
	unresolved := resp.Msg.GetError().GetBaseRefUnresolved()
	if unresolved == nil || unresolved.GetRef() != bad {
		t.Fatalf("CreateWorkspace(bad base_ref) = %v, want error.base_ref_unresolved naming %q", resp.Msg, bad)
	}
}

func TestCreateWorkspaceWithASuppliedNameWinsOverTheDerivedSlug(t *testing.T) {
	// Arrange
	d := newDaemon(t, harness.Opts{})
	repo := harness.NewRepo(t)
	repository := createRepositoryRef(t, d, repo)
	name := "my-explicit-name"

	// Act
	resp, err := d.Client().CreateWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Repository: repository,
		Form: &agentreplv1.CreateWorkspaceRequest_Standard{Standard: &agentreplv1.CreateWorkspaceStandard{
			InitialPrompt: said("something entirely different"),
			Name:          &name,
		}},
	}))

	// Assert: the supplied name, not a prompt-derived slug, names the worktree.
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("CreateWorkspace(name=%q) = (%v, %v), want a success", name, resp, err)
	}
	ws := resp.Msg.GetSuccess().GetWorkspace()
	if !strings.Contains(ws.GetDir(), name) {
		t.Fatalf("the new workspace's dir = %q, want the supplied name %q to win over the derived slug", ws.GetDir(), name)
	}
}

func TestCreateWorkspaceWithAParentNestsTheChildAndTargetsTheParentsWorktreeOnMerge(t *testing.T) {
	// Arrange: a parent, top-level workspace.
	//
	// The daemon's own checkout IS this repository and a test-all script
	// stands in for the gate, because the emacs-repo method is the one that
	// runs `git merge` at all: in every other repository the landing belongs
	// to the merge prompts, and no git merge is issued for this assertion to
	// find.
	repo := harness.NewRepo(t)
	script := harness.NewTestAllScript(t, repo.Dir)
	script.SetExitCode(0)
	script.SetStdout("daemon: passed in 1s\n")
	d := newDaemon(t, harness.Opts{
		SelfRepo: repo.Dir,
		ExtraEnv: []string{"AGENT_REPL_TEST_ALL_SCRIPT=" + script.Path},
	})
	repository := createRepositoryRef(t, d, repo)
	parentResp, err := d.Client().CreateWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Repository: repository,
		Form:       &agentreplv1.CreateWorkspaceRequest_Standard{Standard: &agentreplv1.CreateWorkspaceStandard{}},
	}))
	if err != nil || parentResp.Msg.GetSuccess() == nil {
		t.Fatalf("CreateWorkspace(parent) = (%v, %v), want a success", parentResp, err)
	}
	parent := parentResp.Msg.GetSuccess().GetWorkspace()
	roster := d.WatchRoster()

	// Act: a child spawned from it.
	childResp, err := d.Client().CreateWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Repository: repository,
		Form:       &agentreplv1.CreateWorkspaceRequest_Standard{Standard: &agentreplv1.CreateWorkspaceStandard{}},
		Parent:     &agentreplv1.CreateWorkspaceParent{Workspace: parent},
	}))
	if err != nil || childResp.Msg.GetSuccess() == nil {
		t.Fatalf("CreateWorkspace(parent=%s) = (%v, %v), want a success", parent.GetId(), childResp, err)
	}
	child := childResp.Msg.GetSuccess().GetWorkspace()

	// Assert: the roster nests the child under the parent.
	awaitRoster(t, d, roster, "the child nested under its parent", func(r *frontendv1.WorkspaceRoster) bool {
		parentRow := rosterRepoRow(r, parent.GetId())
		if parentRow == nil {
			return false
		}
		for _, c := range parentRow.GetChildren() {
			if c.GetWorkspace().GetWorkspace().GetId() == child.GetId() {
				return true
			}
		}
		return false
	})

	// Assert: the child's merge lands into the parent's worktree, not main.
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: child})); err != nil {
		t.Fatalf("MergeWorkspace(child) = error %v, want the merge enqueued", err)
	}
	// The merge has to have REACHED ITS TERMINAL before the git calls say
	// anything: `merge_queued` is published before the run starts, so waiting
	// on it would read the trace of a merge that has not run yet.
	awaitRoster(t, d, roster, "the child's merge landed", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, child.GetId())
		return row != nil && row.GetMerged() != nil
	})
	// The git client selects its directory with `-C <dir>` and never with the
	// child process's cwd, so "inside the parent's worktree" is read off the
	// selection argument.
	targetedParent := false
	for _, c := range d.Git.Calls() {
		if createArgsContain(c.Args, "merge") && createGitDir(c.Args) == parent.GetDir() {
			targetedParent = true
		}
	}
	if !targetedParent {
		t.Fatalf("git calls = %v, want a merge run inside the parent's worktree %q", d.Git.Calls(), parent.GetDir())
	}
	// A clean landing into the parent's worktree (a passing test-all gate, no
	// conflict) reaches none of internal/merge's WARN sites, and the parent's
	// worktree is not this daemon's own checkout, so selfReload never fires
	// either.
}

func TestCreateWorkspaceForkPortsTheParentsTranscriptAndResumesIt(t *testing.T) {
	// Arrange: a parent with a minted vendor session and a fake transcript on
	// disk under its config root's project dir, exactly what a real vendor
	// session would have left behind.
	f := newOpened(t, harness.Opts{})
	req := f.shim.ExpectStartSession()
	if req.GetFresh() == nil {
		t.Fatalf("the parent's StartSession = %v, want fresh", req)
	}
	vendorID := f.shim.Info().VendorSessionID
	if vendorID == "" {
		t.Fatal("the fake shim reports no vendor session id after StartSession(fresh)")
	}
	parentProject := createProjectDir(f.d.DefaultConfigDir, f.ws.GetDir())
	if err := os.MkdirAll(parentProject, 0o755); err != nil {
		t.Fatalf("mkdir the parent's project dir: %v", err)
	}
	transcript := filepath.Join(parentProject, vendorID+".jsonl")
	if err := os.WriteFile(transcript, []byte(`{"type":"summary"}`+"\n"), 0o644); err != nil {
		t.Fatalf("seed the parent's transcript: %v", err)
	}
	repository := createRepositoryRef(t, f.d, f.repo)

	// Act
	resp, err := f.d.Client().CreateWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Repository: repository,
		Form:       &agentreplv1.CreateWorkspaceRequest_Standard{Standard: &agentreplv1.CreateWorkspaceStandard{}},
		Parent: &agentreplv1.CreateWorkspaceParent{
			Workspace: f.ws,
			Fork:      &agentreplv1.CreateWorkspaceFork{},
		},
	}))

	// Assert: the child resumes a conversation of its OWN. A vendor session id
	// is single-occupancy -- the shim takes session-<id>.lock inside
	// StartSession -- so a child resuming the live parent's id could never come
	// up; the daemon mints a fresh id and files the copy under it.
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("CreateWorkspace(fork) = (%v, %v), want a success", resp, err)
	}
	child := resp.Msg.GetSuccess().GetWorkspace()
	childShim := f.d.Shim(child)
	resume := childShim.ExpectStartSession().GetResume()
	if resume == nil {
		t.Fatalf("the forked child's StartSession = %v, want a resume", resume)
	}
	forked := resume.GetVendorSessionId()
	if forked == "" || forked == vendorID {
		t.Fatalf("the forked child resumes %q, want a fresh vendor session id and never the parent's %q", forked, vendorID)
	}

	// Assert: the parent's conversation is copied under the child's project
	// dir, filed under the child's own id, and the parent keeps its own.
	childProject := createProjectDir(f.d.DefaultConfigDir, child.GetDir())
	ported := filepath.Join(childProject, forked+".jsonl")
	if _, err := os.Stat(ported); err != nil {
		t.Fatalf("stat the ported transcript %s: %v, want the parent's conversation copied under the child's own id", ported, err)
	}
	if _, err := os.Stat(transcript); err != nil {
		t.Fatalf("stat the parent's transcript %s: %v, want it left in place", transcript, err)
	}
}

// ---- One-shot create ----

func TestCreateWorkspaceOneShotDecoratesThePromptFromPrompts(t *testing.T) {
	// Arrange
	d := newDaemon(t, harness.Opts{})
	repo := harness.NewRepo(t)
	repository := createRepositoryRef(t, d, repo)

	// Act
	resp, err := d.Client().CreateWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Repository: repository,
		Form: &agentreplv1.CreateWorkspaceRequest_OneShot{OneShot: &agentreplv1.CreateWorkspaceOneShot{
			Prompt: said("add a health check endpoint"),
			Finish: &agentreplv1.CreateWorkspaceOneShot_SelfMerge{SelfMerge: &agentreplv1.CreateWorkspaceOneShotSelfMerge{}},
		}},
	}))

	// Assert: the sent prompt is the user's text PLUS the success-suffix
	// brief spliced around it, never the bare text.
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("CreateWorkspace(one_shot) = (%v, %v), want a success", resp, err)
	}
	ws := resp.Msg.GetSuccess().GetWorkspace()
	shim := d.Shim(ws)
	shim.ExpectStartSession()
	got := text(shim.ExpectStartTurn().GetSaid())
	if !strings.Contains(got, "add a health check endpoint") {
		t.Fatalf("the one-shot's decorated prompt = %q, want the user's prompt spliced in", got)
	}
	if !strings.Contains(strings.ToLower(got), "invoke") {
		t.Fatalf("the one-shot's decorated prompt = %q, want the success-suffix brief spliced in", got)
	}
}

func TestCreateWorkspaceOneShotSelfMergeEnqueuesOnCompletion(t *testing.T) {
	// Arrange
	d := newDaemon(t, harness.Opts{})
	repo := harness.NewRepo(t)
	repository := createRepositoryRef(t, d, repo)
	resp, err := d.Client().CreateWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Repository: repository,
		Form: &agentreplv1.CreateWorkspaceRequest_OneShot{OneShot: &agentreplv1.CreateWorkspaceOneShot{
			Prompt: said("ship the fix"),
			Finish: &agentreplv1.CreateWorkspaceOneShot_SelfMerge{SelfMerge: &agentreplv1.CreateWorkspaceOneShotSelfMerge{}},
		}},
	}))
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("CreateWorkspace(one_shot, self_merge) = (%v, %v), want a success", resp, err)
	}
	ws := resp.Msg.GetSuccess().GetWorkspace()
	shim := d.Shim(ws)
	shim.ExpectStartSession()
	shim.ExpectStartTurn()
	roster := d.WatchRoster()

	// Act: the turn concludes with the ordinary success terminal.
	shim.PushAgentFrame(mainAgent, successFrame(mainAgent, nil))

	// Assert: the finish action fires — the workspace's merge is enqueued.
	awaitRoster(t, d, roster, "the one-shot's self-merge enqueued on completion", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, ws.GetId())
		return row != nil && (row.GetMergeQueued() != nil || row.GetMerging() != nil || row.GetMerged() != nil)
	})
	// The workspace was materialized through CreateWorkspace, so it carries
	// real layout facts and the enqueue succeeds without internal/merge's
	// no_layout_facts WARN; this repo is not the daemon's own checkout, so no
	// test gate and no selfReload run either.
}

func TestCreateWorkspaceOneShotOpenPrRunsThePrPostPrompt(t *testing.T) {
	// Arrange
	d := newDaemon(t, harness.Opts{})
	repo := harness.NewRepo(t)
	repository := createRepositoryRef(t, d, repo)
	resp, err := d.Client().CreateWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Repository: repository,
		Form: &agentreplv1.CreateWorkspaceRequest_OneShot{OneShot: &agentreplv1.CreateWorkspaceOneShot{
			Prompt: said("ship the fix"),
			Finish: &agentreplv1.CreateWorkspaceOneShot_OpenPr{OpenPr: &agentreplv1.CreateWorkspaceOneShotOpenPr{}},
		}},
	}))
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("CreateWorkspace(one_shot, open_pr) = (%v, %v), want a success", resp, err)
	}
	ws := resp.Msg.GetSuccess().GetWorkspace()
	shim := d.Shim(ws)
	shim.ExpectStartSession()
	shim.ExpectStartTurn()

	// Act: the turn concludes.
	shim.PushAgentFrame(mainAgent, successFrame(mainAgent, nil))

	// Assert: the finish action fires — a PR-opening post-prompt is submitted.
	got := text(shim.ExpectStartTurn().GetSaid())
	if !strings.Contains(got, "create-or-update-pr") {
		t.Fatalf("the one-shot's post-prompt = %q, want the PR post-prompt referencing create-or-update-pr", got)
	}
}

func TestCreateWorkspaceOneShotOpenPrPostPromptSplicesTheSelfCertifiedAndMergeQueueFlags(t *testing.T) {
	// Arrange: both request-carried flags asked for.
	d := newDaemon(t, harness.Opts{})
	repo := harness.NewRepo(t)
	repository := createRepositoryRef(t, d, repo)
	resp, err := d.Client().CreateWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Repository: repository,
		Form: &agentreplv1.CreateWorkspaceRequest_OneShot{OneShot: &agentreplv1.CreateWorkspaceOneShot{
			Prompt: said("ship the fix"),
			Finish: &agentreplv1.CreateWorkspaceOneShot_OpenPr{OpenPr: &agentreplv1.CreateWorkspaceOneShotOpenPr{
				SelfCertified:   true,
				AddToMergeQueue: true,
			}},
		}},
	}))
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("CreateWorkspace(one_shot, open_pr) = (%v, %v), want a success", resp, err)
	}
	ws := resp.Msg.GetSuccess().GetWorkspace()
	shim := d.Shim(ws)
	shim.ExpectStartSession()
	shim.ExpectStartTurn()

	// Act: the turn concludes, firing the CICD-gated post-prompt.
	shim.PushAgentFrame(mainAgent, successFrame(mainAgent, nil))

	// Assert: both flags are spliced into the pr command the post-prompt
	// TEXT names (internal/workspace/oneshot.go's createPrCommand, read back
	// through openPrFollowup's own splice).
	got := text(shim.ExpectStartTurn().GetSaid())
	if !strings.Contains(got, "--self-certified") {
		t.Fatalf("the one-shot's PR post-prompt = %q, want --self-certified spliced in", got)
	}
	if !strings.Contains(got, "--add-to-merge-queue") {
		t.Fatalf("the one-shot's PR post-prompt = %q, want --add-to-merge-queue spliced in", got)
	}
}

func TestOneShotSelfMergeFailureTerminalNeverEnqueuesTheMerge(t *testing.T) {
	// Arrange
	d := newDaemon(t, harness.Opts{})
	repo := harness.NewRepo(t)
	repository := createRepositoryRef(t, d, repo)
	resp, err := d.Client().CreateWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Repository: repository,
		Form: &agentreplv1.CreateWorkspaceRequest_OneShot{OneShot: &agentreplv1.CreateWorkspaceOneShot{
			Prompt: said("ship the fix"),
			Finish: &agentreplv1.CreateWorkspaceOneShot_SelfMerge{SelfMerge: &agentreplv1.CreateWorkspaceOneShotSelfMerge{}},
		}},
	}))
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("CreateWorkspace(one_shot, self_merge) = (%v, %v), want a success", resp, err)
	}
	ws := resp.Msg.GetSuccess().GetWorkspace()
	shim := d.Shim(ws)
	shim.ExpectStartSession()
	shim.ExpectStartTurn()

	// Act: the turn concludes with a FAILURE terminal, never the success
	// marker the finish hook gates on (internal/promptqueue/lifecycle.go's
	// runFinishHook: `how != wsm.CloseCompleted` skips the hook entirely).
	shim.PushAgentFrame(mainAgent, failureFrame(mainAgent, &conversationv1.AgentFailure{
		Failure: &conversationv1.AgentFailure_ApiRequestFailed{ApiRequestFailed: &conversationv1.ApiRequestFailed{
			Message: "boom",
			Kind:    &conversationv1.ApiRequestFailed_Internal{Internal: &conversationv1.ApiInternal{}},
		}},
	}))

	// Assert: the queue's own turn-ended handling has run to completion --
	// this log fires only AFTER runFinishHook's decision is already made, so
	// checking the merge's absence past this point is race-free rather than
	// a timing guess.
	d.AwaitWorkspaceLogOperation(ws.GetDir(), "daemon.promptqueue.turn_ended")

	// Assert: no merge was ever enqueued for this workspace -- proven with a
	// synchronous RPC read of current daemon state, not a probe window.
	evict, err := d.Client().UpdateMergeQueue(d.Ctx(), connect.NewRequest(&agentreplv1.UpdateMergeQueueRequest{
		Action: &agentreplv1.UpdateMergeQueueRequest_Evict{Evict: &agentreplv1.UpdateMergeQueueEvict{Workspace: ws}},
	}))
	if err != nil {
		t.Fatalf("UpdateMergeQueue(evict) = error %v, want a success carrying no_such_queued_merge", err)
	}
	if evict.Msg.GetError().GetNoSuchQueuedMerge() == nil {
		t.Fatalf("UpdateMergeQueue(evict) after a one-shot failure = %v, want no_such_queued_merge: the finish action must not fire on a failure terminal", evict.Msg)
	}
}

func TestOneShotOpenPrFinishWithTheFollowupBriefRemovedAnswersBriefMissing(t *testing.T) {
	// Arrange: the followup brief the finish hook reads at conclusion is
	// removed from this daemon's OWN prompts directory (a per-test copy, so
	// deleting from it touches nothing else).
	d := newDaemon(t, harness.Opts{})
	if err := os.Remove(filepath.Join(d.PromptsDir, "oneshot-create-pr-then-close-followup.md")); err != nil {
		t.Fatalf("remove the pr-followup brief: %v", err)
	}
	repo := harness.NewRepo(t)
	repository := createRepositoryRef(t, d, repo)
	resp, err := d.Client().CreateWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Repository: repository,
		Form: &agentreplv1.CreateWorkspaceRequest_OneShot{OneShot: &agentreplv1.CreateWorkspaceOneShot{
			Prompt: said("ship the fix"),
			Finish: &agentreplv1.CreateWorkspaceOneShot_OpenPr{OpenPr: &agentreplv1.CreateWorkspaceOneShotOpenPr{}},
		}},
	}))
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("CreateWorkspace(one_shot, open_pr) = (%v, %v), want a success", resp, err)
	}
	ws := resp.Msg.GetSuccess().GetWorkspace()
	d.WatchWorkspaceLogs(ws.GetDir())
	shim := d.Shim(ws)
	shim.ExpectStartSession()
	shim.ExpectStartTurn()

	// Act: the turn concludes with the success marker, driving the finish
	// hook straight into the now-missing followup brief.
	shim.PushAgentFrame(mainAgent, successFrame(mainAgent, nil))

	// Assert: `SubmitPromptError.brief_missing` is STILL an unlanded arm
	// (daemon/ERROR-ARMS.md's own row for exactly this hook and this exact
	// brief filename) -- the one-shot finish hook is not a live RPC a client
	// awaits, so the intended arm surfaces only in the daemon's own log
	// (internal/workspace/refusal.go's "daemon.refusal.typed") rather than as
	// a response the client could read.
	rec := d.AwaitWorkspaceLogOperation(ws.GetDir(), "daemon.refusal.typed")
	if got := rec.Context["arm"]; got != "brief_missing" {
		t.Fatalf("refusal arm = %v, want brief_missing", got)
	}
	if got := rec.Context["rpc"]; got != "SubmitPrompt" {
		t.Fatalf("refusal rpc = %v, want SubmitPrompt (the one-shot finish hook's own rpc name per ERROR-ARMS.md)", got)
	}
	if reason, _ := rec.Context["reason"].(string); !strings.Contains(reason, "oneshot-create-pr-then-close-followup.md") {
		t.Fatalf("refusal reason = %q, want it to name the missing brief file", reason)
	}
	d.ExpectWarnings("daemon.promptqueue.one_shot_finish")
}

// ---- Merge actions ----

func TestCreateWorkspaceMergeActionsAreRecordedAndReadBackByALaterMerge(t *testing.T) {
	// Arrange
	d := newDaemon(t, harness.Opts{})
	repo := harness.NewRepo(t)
	repository := createRepositoryRef(t, d, repo)
	resp, err := d.Client().CreateWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Repository: repository,
		Form: &agentreplv1.CreateWorkspaceRequest_Standard{Standard: &agentreplv1.CreateWorkspaceStandard{
			MergeActions: &agentreplv1.CreateWorkspaceMergeActions{
				BeforeWsMerge: said("run the pre-merge checklist"),
			},
		}},
	}))
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("CreateWorkspace(merge_actions) = (%v, %v), want a success", resp, err)
	}
	ws := resp.Msg.GetSuccess().GetWorkspace()
	shim := d.Shim(ws)
	shim.ExpectStartSession()

	// Act: a later merge reads the recorded action back.
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: ws})); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}

	// Assert
	pre := shim.ExpectStartTurn()
	if text(pre.GetSaid()) != "run the pre-merge checklist" {
		t.Fatalf("the merge's pre-prompt turn = %q, want the recorded before_ws_merge action read back", text(pre.GetSaid()))
	}
	if pre.GetOrigin() != conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_BEFORE_ACTION {
		t.Fatalf("the pre-prompt's origin = %v, want MERGE_BEFORE_ACTION", pre.GetOrigin())
	}
	// Created through CreateWorkspace (real layout facts), so the enqueue and
	// pre-prompt admission reach none of internal/merge's WARN sites.
}

// ---- NukeWorkspace: kill-before-destroy ordering, and a git failure ----

func TestNukeWorkspaceKillsTheLiveSessionBeforeAnyGitCommandRuns(t *testing.T) {
	// Arrange: an opened, LIVE workspace, its fake shim HUNG so KillSession's
	// arrival is observable before it is ever answered.
	//
	// The workspace is a WORKTREE of its repository, not the repository root
	// the shared fixture registers. Nuking a workspace whose directory IS the
	// repository root deletes the repository itself, so the `worktree prune`
	// that follows the removal has no repository left to run in and git fails
	// on the arrangement rather than on anything the daemon did; the subject
	// here is the ORDER of the kill against the git commands.
	f := newOpenedWorktree(t, harness.Opts{}, "nuke-order")
	// The sweep covers every test; the declared records are evidence of a session fault the test opens.
	f.d.ExpectWarnings("daemon.health.open_fault")
	f.shim.ExpectStartSession()
	f.shim.Hang()

	// Act: NukeWorkspace blocks on the hung KillSession
	// (internal/workspace/teardown.go's kill() awaits it before Nuke() ever
	// touches git), so it runs in its own goroutine.
	done := make(chan struct{})
	go func() {
		defer close(done)
		resp, err := f.d.Client().NukeWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.NukeWorkspaceRequest{Workspace: f.ws}))
		if err != nil || resp.Msg.GetSuccess() == nil {
			t.Errorf("NukeWorkspace = (%v, %v), want a success", resp, err)
		}
	}()

	// Assert: KillSession has reached the shim -- proof of ORDER, since the
	// fake's own log write happens on arrival, before it is gated on the
	// hang -- while the fake git's recorded calls carry no worktree removal
	// yet, because the daemon-side call is still blocked on the hung
	// KillSession and nothing past it has run.
	f.d.AwaitShimVerbOrder(f.ws.GetDir(), harness.RPCKillSession)
	for _, c := range f.d.Git.Calls() {
		if createArgsContainAll(c.Args, "worktree", "remove") {
			t.Fatalf("a worktree remove already ran while KillSession is still hung: %v, want the kill to finish first", c.Args)
		}
	}

	// Act: release the fake, letting KillSession answer and the nuke proceed.
	f.shim.Unhang()
	<-done

	// Assert: the worktree removal now follows the completed kill.
	found := false
	for _, c := range f.d.Git.Calls() {
		if createArgsContainAll(c.Args, "worktree", "remove") {
			found = true
		}
	}
	if !found {
		t.Fatalf("git calls = %v, want a worktree remove once the kill completed", f.d.Git.Calls())
	}
}

func TestNukeWorkspaceAGitFailureDuringTheWorktreeRemoveAnswersGitFailed(t *testing.T) {
	// Arrange: a registered (no live session) workspace whose worktree
	// removal is scripted to fail.
	d := newDaemon(t, harness.Opts{})
	repo := harness.NewRepo(t)
	dir := worktreeOf(t, repo, "nuke-fails")
	ws := harness.Register(t, d, dir)
	repo.ScriptFailure(repo.Dir, 1, "fatal: unable to remove worktree", "worktree", "remove")

	// Act
	resp, err := d.Client().NukeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.NukeWorkspaceRequest{Workspace: ws}))

	// Assert: NukeWorkspaceError.git_failed{detail} is the LANDED arm
	// endpoint_nuke_workspace.proto contracts for exactly this condition.
	// EXPECTED RED: grepping internal/, no production site wraps
	// internal/workspace/teardown.go's Nuke() git error into a
	// workspace.Refusal naming this arm (no "git_failed" producer anywhere
	// outside the proto and daemon/ERROR-ARMS.md's own commentary), so
	// server.answerRefusal's asRefusal falls through to a bare Connect
	// error today instead of this typed answer.
	if err != nil {
		t.Fatalf("NukeWorkspace with a scripted worktree-remove failure = transport error %v, want the git_failed arm", err)
	}
	failed := resp.Msg.GetError().GetGitFailed()
	if failed == nil {
		t.Fatalf("NukeWorkspace with a scripted worktree-remove failure = %v, want NukeWorkspaceError.git_failed", resp.Msg)
	}
	if !strings.Contains(failed.GetDetail(), "unable to remove worktree") {
		t.Fatalf("git_failed.detail = %q, want git's own account of the failure", failed.GetDetail())
	}
	// The git leaf records its own command failure at ERROR — that record IS
	// the evidence the refusal's detail is drawn from — and the verb answers
	// the refusal above it.
	d.ExpectWarnings("NukeWorkspace", "daemon.gitclient.remove_worktree")
}

// ---- CloseWorkspace: blocked by live detached work with no turn open ----

func TestCloseWorkspaceBlockedByLiveDetachedWorkWithNoTurnOpenAnswersBlocked(t *testing.T) {
	// Arrange: detached work announced with NO turn EVER opened -- distinct
	// from a turn-in-flight refusal (internal/workspace/open.go's
	// closeBlocker: `running.Turn != nil` is checked FIRST and answers
	// "turn_in_flight"; this test's own branch is reached only once that is
	// nil AND live work remains, answering "live_work" instead).
	f := newOpened(t, harness.Opts{})
	footer := f.d.WatchFooter(f.ws)
	f.shim.PushAgentFrame(mainAgent, detachedWorkFrame(mainAgent, detachedShell("work-close-1", "sleep 100")))
	awaitLiveWork(t, f, 1)

	// Act
	resp, err := f.d.Client().CloseWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.CloseWorkspaceRequest{Workspace: f.ws}))

	// Assert: the exact refusal arm the proto contracts.
	if err != nil {
		t.Fatalf("CloseWorkspace with live detached work and no open turn = transport error %v, want the blocked arm", err)
	}
	if resp.Msg.GetError().GetBlocked() == nil {
		t.Fatalf("CloseWorkspace with live detached work and no open turn = %v, want CloseWorkspaceError.blocked", resp.Msg)
	}

	// Assert: the footer's composed reason names the LIVE_WORK cause, not a
	// turn in flight (CloseWorkspaceBlocked itself carries no field per
	// ERROR-ARMS.md, so the evidence rides only the footer's own text).
	fv := awaitFooter(t, f, footer, "footer closing.blocked with the live-work reason", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetClosing().GetBlocked() != nil
	})
	line := fv.GetStrip().GetStatus().GetClosing().GetActivity().GetCloseBlocked().GetText()
	if !strings.Contains(line, "detached") {
		t.Fatalf("close-blocked activity text = %q, want it to name the live detached work, not a turn in flight", line)
	}
	f.d.ExpectWarnings("daemon.workspace.close")
}

// ---- create* helpers (prefixed create* so they cannot collide) ----

// createRepositoryRef registers a fresh workspace under a repository and
// reads back the daemon-minted RepositoryRef from the roster's repo section,
// since CreateWorkspace needs the echo token and no direct mint RPC exists.
func createRepositoryRef(t *testing.T, d *harness.Daemon, repo *harness.Repo) *workspacev1.RepositoryRef {
	t.Helper()
	ws := harness.Register(t, d, repo.Dir)
	roster := d.WatchRoster()
	got := harness.AwaitView(t, d.Ctx(), roster, "the repository section", func(r *frontendv1.WorkspaceRoster) bool {
		return rosterRepoRow(r, ws.GetId()) != nil
	})
	for _, s := range got.GetRepository().GetSections() {
		for _, row := range s.GetRows().GetRows() {
			if row.GetWorkspace().GetWorkspace().GetId() == ws.GetId() {
				return s.GetKey().GetRepository()
			}
		}
	}
	t.Fatalf("no repository section found for %s", repo.Dir)
	return nil
}

// createProjectDir mirrors the vendor CLI's project-directory convention: the
// absolute worktree path with every "/" replaced by "-".
func createProjectDir(configDir, workspaceDir string) string {
	return filepath.Join(configDir, "projects", strings.ReplaceAll(workspaceDir, "/", "-"))
}

// createArgsContain reports whether a git call's args contain an exact token.
// createGitDir answers the directory a recorded git call selected with -C.
func createGitDir(args []string) string {
	for i, a := range args {
		if a == "-C" && i+1 < len(args) {
			return args[i+1]
		}
	}
	return ""
}

func createArgsContain(args []string, want string) bool {
	for _, a := range args {
		if a == want {
			return true
		}
	}
	return false
}

// createArgsContainAll reports whether a git call's args contain every token,
// in any position.
func createArgsContainAll(args []string, want ...string) bool {
	for _, w := range want {
		if !createArgsContain(args, w) {
			return false
		}
	}
	return true
}
