//go:build integration

package integration

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/integration/harness"

	"connectrpc.com/connect"
)

// merge_test.go exercises MergeWorkspace, UpdateMergeQueue, AnswerHeldOffer
// and the merge orchestrator's visible effects (feed sub-feed, footer,
// roster, host composer) against a daemon whose own-checkout identity is
// injected via harness.Opts.SelfRepo, with a FAKE bin/test-all.sh
// (harness.NewTestAllScript) pointed at through AGENT_REPL_TEST_ALL_SCRIPT in
// harness.Opts.ExtraEnv, and the deploy script at d.Deploy. Conflicts are
// scripted with repo.ScriptConflict; there is no real git anywhere.
//
// A workspace registered by RegisterWorkspace alone carries no creation job,
// so every fixture here that needs layout facts goes through CreateWorkspace
// for real, exactly as the contract requires (internal/merge's layoutFor
// refuses a merge with no recorded creation job).

// ---------------------------------------------------------------------------
// Pre-state refusal
// ---------------------------------------------------------------------------

func TestMergeWorkspaceOnAWorkspaceWithoutLayoutFactsIsRefused(t *testing.T) {
	// Arrange: a workspace registered directly, never created, so it carries
	// no creation job.
	f := newRegistered(t, harness.Opts{})

	// Act
	resp, err := f.d.Client().MergeWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: f.ws}))

	// Assert
	if err != nil {
		t.Fatalf("MergeWorkspace = error %v, want a success carrying the no_layout_facts arm", err)
	}
	if resp.Msg.GetError().GetNoLayoutFacts() == nil {
		t.Fatalf("MergeWorkspace = %v, want MergeWorkspaceError.no_layout_facts", resp.Msg)
	}
}

// ---------------------------------------------------------------------------
// Enqueue: the queue tab, the footer's queued substatus, the roster arm, and
// a second workspace in the same repo queuing behind the first.
// ---------------------------------------------------------------------------

// mergeBlockedQueueFixture builds a self-repo with two child workspaces
// targeting it: the first is admitted to the front and immediately conflicts
// (parking forever, since nothing in the harness surface can clear a
// scripted conflict), which pins the queue so the second workspace stays
// genuinely, observably QUEUED for the test's duration.
func mergeBlockedQueueFixture(t *testing.T) (front, behind *fixture, repo *harness.Repo, d *harness.Daemon) {
	t.Helper()
	repo = harness.NewRepo(t)
	d = harness.StartDaemon(t, harness.Opts{SelfRepo: repo.Dir})
	repoRef := mergeRepositoryRef(t, d, repo)

	front = mergeCreateChild(t, d, repoRef, "front", "front work", nil)
	behind = mergeCreateChild(t, d, repoRef, "behind", "behind work", nil)

	frontBranch := mergeBranchOf(t, front.ws)
	// THE CONFLICT IS SCRIPTED WHERE THE MERGE RUNS: the merge is performed
	// in the TARGET worktree, and a top-level child targets the repository's
	// main worktree, never its own.
	repo.ScriptConflict(repo.Dir, frontBranch, "conflict.txt")

	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: front.ws})); err != nil {
		t.Fatalf("MergeWorkspace(front) = error %v, want the merge enqueued", err)
	}
	// Drain the front's conflict brief so its turn can conclude and the run
	// can actually reach the parked state, rather than sitting mid-turn.
	front.shim.ExpectStartTurn()
	front.shim.PushAgentFrame(mainAgent, successFrame(mainAgent, activityID("front-conflict-brief")))

	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: behind.ws})); err != nil {
		t.Fatalf("MergeWorkspace(behind) = error %v, want the merge enqueued", err)
	}
	return front, behind, repo, d
}

func TestASecondWorkspaceInTheSameRepoQueuesBehindTheFirstWithTheQueueTabFooterAndRoster(t *testing.T) {
	// Arrange / Act
	_, behind, _, _ := mergeBlockedQueueFixture(t)

	// Assert: roster arm.
	roster := behind.d.WatchRoster()
	got := awaitRoster(t, behind.d, roster, "the second workspace queued behind the first", func(r *frontendv1.WorkspaceRoster) bool {
		return rosterRow(r, behind.ws.GetId()).GetMergeQueued() != nil
	})
	if row := rosterRow(got, behind.ws.GetId()); row.GetMergeQueued() == nil {
		t.Fatalf("behind workspace roster status = %v, want merge_queued", row)
	}

	// Assert: footer queued{position, depth}.
	footer := behind.d.WatchFooter(behind.ws)
	fv := awaitFooter(t, behind, footer, "the footer's queued substatus", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetMerging().GetQueued() != nil
	})
	queued := fv.GetStrip().GetStatus().GetMerging().GetQueued()
	if queued.GetPosition() != 2 || queued.GetDepth() != 2 {
		t.Fatalf("footer queued = %+v, want position=2 depth=2", queued)
	}

	// Assert: the merge bubble's queue tab, on its own sub-feed.
	mergeRow := behind.awaitRowInFeed(nil, "the behind workspace's merge bubble", func(row *frontendv1.FeedRow) bool {
		return row.GetActivity().GetMerge() != nil
	})
	tabRow := behind.awaitRowInFeed(mergeRow.GetId(), "the queue tab", func(row *frontendv1.FeedRow) bool {
		return row.GetMergeTab().GetQueue() != nil
	})
	if tabRow.GetMergeTab().GetQueue().GetQueue().GetCurrent() == nil {
		t.Fatalf("queue tab = %v, want the workspace's own queue entry", tabRow.GetMergeTab())
	}
}

// ---------------------------------------------------------------------------
// UpdateMergeQueue: pause, resume, evict.
// ---------------------------------------------------------------------------

func TestUpdateMergeQueuePauseThenResumeToggleTheQueueStateAndRefuseNoOps(t *testing.T) {
	// Arrange
	_, _, repo, d := mergeBlockedQueueFixture(t)
	repoRef := mergeRepositoryRef(t, d, repo)

	// Act: pause.
	resp, err := d.Client().UpdateMergeQueue(d.Ctx(), connect.NewRequest(&agentreplv1.UpdateMergeQueueRequest{
		Action: &agentreplv1.UpdateMergeQueueRequest_Pause{Pause: &agentreplv1.UpdateMergeQueuePause{Repository: repoRef}},
	}))

	// Assert: pause succeeds.
	if err != nil || resp.Msg.GetError() != nil {
		t.Fatalf("UpdateMergeQueue(pause) = %v, %v, want a success", resp.Msg, err)
	}

	// Act: pause again.
	resp2, err := d.Client().UpdateMergeQueue(d.Ctx(), connect.NewRequest(&agentreplv1.UpdateMergeQueueRequest{
		Action: &agentreplv1.UpdateMergeQueueRequest_Pause{Pause: &agentreplv1.UpdateMergeQueuePause{Repository: repoRef}},
	}))
	if err != nil {
		t.Fatalf("UpdateMergeQueue(pause again) = error %v, want a success carrying already_paused", err)
	}
	if resp2.Msg.GetError().GetAlreadyPaused() == nil {
		t.Fatalf("UpdateMergeQueue(pause again) = %v, want UpdateMergeQueueError.already_paused", resp2.Msg)
	}

	// Act: resume.
	resp3, err := d.Client().UpdateMergeQueue(d.Ctx(), connect.NewRequest(&agentreplv1.UpdateMergeQueueRequest{
		Action: &agentreplv1.UpdateMergeQueueRequest_Resume{Resume: &agentreplv1.UpdateMergeQueueResume{Repository: repoRef}},
	}))
	if err != nil || resp3.Msg.GetError() != nil {
		t.Fatalf("UpdateMergeQueue(resume) = %v, %v, want a success", resp3.Msg, err)
	}

	// Act: resume again.
	resp4, err := d.Client().UpdateMergeQueue(d.Ctx(), connect.NewRequest(&agentreplv1.UpdateMergeQueueRequest{
		Action: &agentreplv1.UpdateMergeQueueRequest_Resume{Resume: &agentreplv1.UpdateMergeQueueResume{Repository: repoRef}},
	}))
	if err != nil {
		t.Fatalf("UpdateMergeQueue(resume again) = error %v, want a success carrying not_paused", err)
	}
	if resp4.Msg.GetError().GetNotPaused() == nil {
		t.Fatalf("UpdateMergeQueue(resume again) = %v, want UpdateMergeQueueError.not_paused", resp4.Msg)
	}
}

func TestUpdateMergeQueueEvictRemovesOneWorkspacesQueuedMerge(t *testing.T) {
	// Arrange
	_, behind, _, _ := mergeBlockedQueueFixture(t)
	roster := behind.d.WatchRoster()
	awaitRoster(t, behind.d, roster, "the behind workspace queued", func(r *frontendv1.WorkspaceRoster) bool {
		return rosterRow(r, behind.ws.GetId()).GetMergeQueued() != nil
	})

	// Act
	resp, err := behind.d.Client().UpdateMergeQueue(behind.d.Ctx(), connect.NewRequest(&agentreplv1.UpdateMergeQueueRequest{
		Action: &agentreplv1.UpdateMergeQueueRequest_Evict{Evict: &agentreplv1.UpdateMergeQueueEvict{Workspace: behind.ws}},
	}))
	if err != nil || resp.Msg.GetError() != nil {
		t.Fatalf("UpdateMergeQueue(evict) = %v, %v, want a success", resp.Msg, err)
	}

	// Assert: the roster no longer shows it queued.
	got := awaitRoster(t, behind.d, roster, "the evicted workspace off the queue", func(r *frontendv1.WorkspaceRoster) bool {
		return rosterRow(r, behind.ws.GetId()).GetMergeQueued() == nil
	})
	if row := rosterRow(got, behind.ws.GetId()); row.GetMergeQueued() != nil {
		t.Fatalf("evicted workspace roster status = %v, want no longer merge_queued", row)
	}

	// Act / Assert: evicting again finds nothing queued.
	resp2, err := behind.d.Client().UpdateMergeQueue(behind.d.Ctx(), connect.NewRequest(&agentreplv1.UpdateMergeQueueRequest{
		Action: &agentreplv1.UpdateMergeQueueRequest_Evict{Evict: &agentreplv1.UpdateMergeQueueEvict{Workspace: behind.ws}},
	}))
	if err != nil {
		t.Fatalf("UpdateMergeQueue(evict again) = error %v, want a success carrying no_such_queued_merge", err)
	}
	if resp2.Msg.GetError().GetNoSuchQueuedMerge() == nil {
		t.Fatalf("UpdateMergeQueue(evict again) = %v, want UpdateMergeQueueError.no_such_queued_merge", resp2.Msg)
	}
}

// ---------------------------------------------------------------------------
// Interrupt on a queued merge raises the dequeue HeldOffer; AnswerHeldOffer
// releases or keeps it.
// ---------------------------------------------------------------------------

func TestInterruptOnAQueuedWorkspaceRaisesTheDequeueHeldOffer(t *testing.T) {
	// Arrange
	_, behind, _, _ := mergeBlockedQueueFixture(t)
	roster := behind.d.WatchRoster()
	awaitRoster(t, behind.d, roster, "the behind workspace queued", func(r *frontendv1.WorkspaceRoster) bool {
		return rosterRow(r, behind.ws.GetId()).GetMergeQueued() != nil
	})
	holds := behind.d.WatchHolds(behind.ws)

	// Act
	resp, err := behind.d.Client().Interrupt(behind.d.Ctx(), connect.NewRequest(&agentreplv1.InterruptRequest{
		Workspace: behind.ws,
		Target:    &agentreplv1.InterruptRequest_Turn{Turn: &agentreplv1.InterruptTurn{}},
	}))

	// Assert: nothing was running.
	if err != nil {
		t.Fatalf("Interrupt(turn) on a queued, idle workspace = error %v, want a success", err)
	}
	if resp.Msg.GetSuccess().GetNothingRunning() == nil {
		t.Fatalf("Interrupt(turn) = %v, want InterruptSuccess.nothing_running", resp.Msg)
	}

	// Assert: the dequeue offer is raised.
	got := awaitView(t, behind, holds, "the merge-dequeue held offer", func(tray *frontendv1.DaemonHoldTray) bool {
		return mergeDequeueOffer(tray) != nil
	})
	if mergeDequeueOffer(got) == nil {
		t.Fatalf("held tray = %v, want a merge_dequeue offer", got)
	}
}

func TestAnswerHeldOfferReleaseEvictsTheQueuedMerge(t *testing.T) {
	// Arrange
	_, behind, _, _ := mergeBlockedQueueFixture(t)
	holds := behind.d.WatchHolds(behind.ws)
	if _, err := behind.d.Client().Interrupt(behind.d.Ctx(), connect.NewRequest(&agentreplv1.InterruptRequest{
		Workspace: behind.ws, Target: &agentreplv1.InterruptRequest_Turn{Turn: &agentreplv1.InterruptTurn{}},
	})); err != nil {
		t.Fatalf("Interrupt(turn) = error %v, want a success", err)
	}
	awaitView(t, behind, holds, "the merge-dequeue held offer", func(tray *frontendv1.DaemonHoldTray) bool {
		return mergeDequeueOffer(tray) != nil
	})

	// Act
	resp, err := behind.d.Client().AnswerHeldOffer(behind.d.Ctx(), connect.NewRequest(&agentreplv1.AnswerHeldOfferRequest{
		Workspace: behind.ws,
		Answer: &agentreplv1.AnswerHeldOfferRequest_MergeDequeue{MergeDequeue: &agentreplv1.AnswerHeldOfferMergeDequeue{
			Decision: &agentreplv1.AnswerHeldOfferMergeDequeue_Release{Release: &agentreplv1.AnswerHeldOfferRelease{}},
		}},
	}))

	// Assert
	if err != nil || resp.Msg.GetError() != nil {
		t.Fatalf("AnswerHeldOffer(release) = %v, %v, want a success", resp.Msg, err)
	}
	roster := behind.d.WatchRoster()
	got := awaitRoster(t, behind.d, roster, "the released workspace off the queue", func(r *frontendv1.WorkspaceRoster) bool {
		return rosterRow(r, behind.ws.GetId()).GetMergeQueued() == nil
	})
	if row := rosterRow(got, behind.ws.GetId()); row.GetMergeQueued() != nil {
		t.Fatalf("released workspace roster status = %v, want no longer merge_queued", row)
	}
}

func TestAnswerHeldOfferKeepKeepsTheQueuedMerge(t *testing.T) {
	// Arrange
	_, behind, _, _ := mergeBlockedQueueFixture(t)
	holds := behind.d.WatchHolds(behind.ws)
	if _, err := behind.d.Client().Interrupt(behind.d.Ctx(), connect.NewRequest(&agentreplv1.InterruptRequest{
		Workspace: behind.ws, Target: &agentreplv1.InterruptRequest_Turn{Turn: &agentreplv1.InterruptTurn{}},
	})); err != nil {
		t.Fatalf("Interrupt(turn) = error %v, want a success", err)
	}
	awaitView(t, behind, holds, "the merge-dequeue held offer", func(tray *frontendv1.DaemonHoldTray) bool {
		return mergeDequeueOffer(tray) != nil
	})

	// Act
	resp, err := behind.d.Client().AnswerHeldOffer(behind.d.Ctx(), connect.NewRequest(&agentreplv1.AnswerHeldOfferRequest{
		Workspace: behind.ws,
		Answer: &agentreplv1.AnswerHeldOfferRequest_MergeDequeue{MergeDequeue: &agentreplv1.AnswerHeldOfferMergeDequeue{
			Decision: &agentreplv1.AnswerHeldOfferMergeDequeue_Keep{Keep: &agentreplv1.AnswerHeldOfferKeep{}},
		}},
	}))

	// Assert
	if err != nil || resp.Msg.GetError() != nil {
		t.Fatalf("AnswerHeldOffer(keep) = %v, %v, want a success", resp.Msg, err)
	}
	holds2 := behind.d.WatchHolds(behind.ws)
	got := awaitView(t, behind, holds2, "the tray with the offer cleared", func(tray *frontendv1.DaemonHoldTray) bool {
		return mergeDequeueOffer(tray) == nil
	})
	if mergeDequeueOffer(got) != nil {
		t.Fatalf("held tray = %v, want the offer cleared after keep", got)
	}
	roster := behind.d.WatchRoster()
	rgot := awaitRoster(t, behind.d, roster, "the kept workspace still queued", func(r *frontendv1.WorkspaceRoster) bool {
		return rosterRow(r, behind.ws.GetId()) != nil
	})
	if row := rosterRow(rgot, behind.ws.GetId()); row.GetMergeQueued() == nil {
		t.Fatalf("kept workspace roster status = %v, want still merge_queued", row)
	}
}

// ---------------------------------------------------------------------------
// The Emacs-repo method: a conflicting branch opens the conflicts tab, briefs
// the agent once with the spliced brief, then parks.
// ---------------------------------------------------------------------------

func TestAConflictingBranchOpensTheConflictsTabAndPromptsWithTheSplicedBrief(t *testing.T) {
	// Arrange
	repo := harness.NewRepo(t)
	d := harness.StartDaemon(t, harness.Opts{SelfRepo: repo.Dir})
	repoRef := mergeRepositoryRef(t, d, repo)
	f := mergeCreateChild(t, d, repoRef, "feature", "do the feature", nil)
	branch := mergeBranchOf(t, f.ws)
	repo.ScriptConflict(repo.Dir, branch, "conflict.txt")

	// Act
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}
	req := f.shim.ExpectStartTurn()
	f.d.AwaitWorkspaceLogOperationCount(f.ws.GetDir(), harness.OpTurnOpened, 2)

	// Assert: the brief's placeholders were spliced.
	if req.GetOrigin() != mergeConflictRepairOrigin {
		t.Fatalf("conflict brief StartTurn.origin = %v, want PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR", req.GetOrigin())
	}
	said := text(req.GetSaid())
	if !strings.Contains(said, branch) {
		t.Fatalf("conflict brief = %q, want it to name the source branch %q", said, branch)
	}
	if !strings.Contains(said, repo.Dir) {
		t.Fatalf("conflict brief = %q, want it to name the target dir %q", said, repo.Dir)
	}

	// Act: conclude the brief's turn. The conflict is never cleared (no
	// harness surface exists to resolve it), so the run parks.
	f.shim.PushAgentFrame(mainAgent, successFrame(mainAgent, activityID("conflict-brief-done")))

	// Assert: footer, host composer.
	footer := f.d.WatchFooter(f.ws)
	fv := awaitFooter(t, f, footer, "the footer's parked substatus", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetMerging().GetParked() != nil
	})
	if fv.GetStrip().GetStatus().GetMerging().GetParked().GetLine() == "" {
		t.Fatalf("footer parked = %v, want a composed line", fv.GetStrip().GetStatus().GetMerging().GetParked())
	}
	host := f.d.WatchHost(f.ws)
	hv := awaitView(t, f, host, "the host composer parked on the merge", func(r *agentreplv1.WatchHostWorkspaceResponse) bool {
		return r.GetHost().GetExisting().GetLive().GetMergeParked() != nil
	})
	if hv.GetHost().GetExisting().GetLive().GetMergeParked() == nil {
		t.Fatalf("host composer = %v, want merge_parked", hv.GetHost())
	}

	// Assert: the roster shows the conflict.
	roster := f.d.WatchRoster()
	rgot := awaitRoster(t, f.d, roster, "the roster's merge_conflict arm", func(r *frontendv1.WorkspaceRoster) bool {
		return rosterRow(r, f.ws.GetId()).GetMergeConflict() != nil
	})
	if row := rosterRow(rgot, f.ws.GetId()); row.GetMergeConflict() == nil {
		t.Fatalf("parked workspace roster status = %v, want merge_conflict", row)
	}
}

func TestSubmitPromptWhileMergeParkedLandsInTheConflictsTabNotAsARefusal(t *testing.T) {
	// Arrange: park a merge on a scripted conflict.
	repo := harness.NewRepo(t)
	d := harness.StartDaemon(t, harness.Opts{SelfRepo: repo.Dir})
	repoRef := mergeRepositoryRef(t, d, repo)
	f := mergeCreateChild(t, d, repoRef, "feature", "do the feature", nil)
	branch := mergeBranchOf(t, f.ws)
	repo.ScriptConflict(repo.Dir, branch, "conflict.txt")
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}
	f.shim.ExpectStartTurn()
	f.d.AwaitWorkspaceLogOperationCount(f.ws.GetDir(), harness.OpTurnOpened, 2)
	f.shim.PushAgentFrame(mainAgent, successFrame(mainAgent, activityID("conflict-brief-done")))
	host := f.d.WatchHost(f.ws)
	awaitView(t, f, host, "the host composer parked on the merge", func(r *agentreplv1.WatchHostWorkspaceResponse) bool {
		return r.GetHost().GetExisting().GetLive().GetMergeParked() != nil
	})

	// Act: submit guidance while parked.
	err := f.submitExpectingError(&agentreplv1.SubmitPromptRequest{
		Workspace:      f.ws,
		Said:           said("please look again"),
		IdempotencyKey: "k-guidance",
		Origin:         origin,
	})

	// Assert: not refused.
	if err != nil {
		t.Fatalf("SubmitPrompt while merge-parked = error %v, want it delivered to the conflicts tab, not refused", err)
	}
	// Assert: delivery reaches the merge's resolution agent as a real turn.
	f.shim.ExpectStartTurn()
}

// ---------------------------------------------------------------------------
// The test gate: pass settles the run; failure opens the fixes tab with the
// brief and parks on escalation; no automatic re-run.
// ---------------------------------------------------------------------------

// mergeCleanRepo builds a self-repo with a scripted bin/test-all.sh and one
// child workspace, whose merge lands with no conflict (the merge tab always
// succeeds cleanly absent a scripted conflict).
func mergeCleanRepo(t *testing.T) (f *fixture, d *harness.Daemon, repo *harness.Repo, script *harness.Recorder) {
	t.Helper()
	repo = harness.NewRepo(t)
	script = harness.NewTestAllScript(t, repo.Dir)
	d = harness.StartDaemon(t, harness.Opts{SelfRepo: repo.Dir, ExtraEnv: []string{"AGENT_REPL_TEST_ALL_SCRIPT=" + script.Path}})
	repoRef := mergeRepositoryRef(t, d, repo)
	f = mergeCreateChild(t, d, repoRef, "clean", "do the clean thing", nil)
	return f, d, repo, script
}

func TestTheTestGatePassingSettlesTheTestsTab(t *testing.T) {
	// Arrange
	f, d, _, script := mergeCleanRepo(t)
	script.SetExitCode(0)
	script.SetStdout("daemon: passed in 1s\n")

	// Act
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}

	// Assert: the merge lands (no conflict, gate passes).
	root := f.watchRootFeed()
	mergeRow := awaitRow(t, f, root, "the merge's terminal push", func(row *frontendv1.FeedRow) bool {
		return row.GetActivity().GetMerge().GetSuccess() != nil || row.GetActivity().GetMerge().GetError() != nil
	})
	if mergeRow.GetActivity().GetMerge().GetError() != nil {
		t.Fatalf("merge terminal = %v, want success", mergeRow.GetActivity().GetMerge())
	}

	// Assert: the tests tab settled successfully at some point along the way.
	testsRow := f.awaitRowInFeed(mergeRow.GetId(), "the settled tests tab", func(row *frontendv1.FeedRow) bool {
		return row.GetMergeTab().GetTests().GetSettled() != nil
	})
	if testsRow.GetMergeTab().GetTests().GetSettled().GetSucceeded() == nil {
		t.Fatalf("tests tab settled = %v, want succeeded", testsRow.GetMergeTab().GetTests())
	}
}

func TestATestGateFailureOpensTheFixesTabWithTheBriefAndParksOnEscalation(t *testing.T) {
	// Arrange
	f, d, repo, script := mergeCleanRepo(t)
	script.SetExitCode(1)
	script.SetStdout("daemon failed after 1s with exit code 1\nsome failing output\n")

	// Act
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}
	req := f.shim.ExpectStartTurn()
	f.d.AwaitWorkspaceLogOperationCount(f.ws.GetDir(), harness.OpTurnOpened, 2)

	// Assert: the fixes brief was spliced.
	if req.GetOrigin() != mergeTestRepairOrigin {
		t.Fatalf("fixes brief StartTurn.origin = %v, want PROMPT_ORIGIN_MERGE_TEST_REPAIR", req.GetOrigin())
	}
	saidText := text(req.GetSaid())
	if !strings.Contains(saidText, "some failing output") {
		t.Fatalf("fixes brief = %q, want the failing tail spliced in", saidText)
	}
	if !strings.Contains(saidText, mergeEscalationFile) {
		t.Fatalf("fixes brief = %q, want the escalation file name spliced in", saidText)
	}

	// Act: the agent escalates rather than fixing it.
	if err := os.WriteFile(filepath.Join(repo.Dir, mergeEscalationFile),
		[]byte(mergeEscalationMarker+"\nthis needs a redesign\n"), 0o644); err != nil {
		t.Fatalf("write the escalation file: %v", err)
	}
	f.shim.PushAgentFrame(mainAgent, successFrame(mainAgent, activityID("fixes-brief-done")))

	// Assert: the run parks.
	footer := f.d.WatchFooter(f.ws)
	fv := awaitFooter(t, f, footer, "the footer's parked substatus", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetMerging().GetParked() != nil
	})
	if fv.GetStrip().GetStatus().GetMerging().GetParked() == nil {
		t.Fatalf("footer = %v, want merging.parked after the fixes escalation", fv.GetStrip().GetStatus())
	}
}

func TestATestGateFailureIsNeverAutomaticallyRerun(t *testing.T) {
	// Arrange
	f, d, _, script := mergeCleanRepo(t)
	script.SetExitCode(1)
	script.SetStdout("daemon failed after 1s with exit code 1\nboom\n")

	// Act
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}
	// The fixes tab opening and briefing the agent is proof the gate ran once
	// and reached the fixes step, without yet answering the brief.
	f.shim.ExpectStartTurn()

	// Assert: exactly one invocation so far — no flake re-run raced ahead of
	// the agent's own turn.
	if got := len(script.Invocations()); got != 1 {
		t.Fatalf("test-all.sh invocations = %d, want exactly 1 (no automatic re-run)", got)
	}
}

// ---------------------------------------------------------------------------
// Landed: success, footer, roster, worktree removal, and the self-repo
// rollout trigger.
// ---------------------------------------------------------------------------

func TestALandedMergeProducesSuccessFooterRosterAndRemovesTheWorktreeAfterTheTerminalPush(t *testing.T) {
	// Arrange
	f, d, repo, script := mergeCleanRepo(t)
	script.SetExitCode(0)
	script.SetStdout("daemon: passed in 1s\n")
	dir := f.ws.GetDir()

	// Act
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}

	// Assert: FeedMergeSuccess{commit}.
	root := f.watchRootFeed()
	mergeRow := awaitRow(t, f, root, "the merge's success", func(row *frontendv1.FeedRow) bool {
		return row.GetActivity().GetMerge().GetSuccess() != nil
	})
	if mergeRow.GetActivity().GetMerge().GetSuccess().GetCommit() == "" {
		t.Fatalf("FeedMergeSuccess = %v, want a landed commit", mergeRow.GetActivity().GetMerge().GetSuccess())
	}

	// Assert: the worktree is still present at the moment of the terminal
	// push (removal happens only AFTER it, never before).
	if !repo.HasWorktree(dir) {
		t.Fatalf("worktree %s is gone at the terminal push, want removal only after it", dir)
	}

	// Assert: footer merged, roster merged + recently_merged.
	footer := f.d.WatchFooter(f.ws)
	awaitFooter(t, f, footer, "the footer's merged substatus", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetMerging().GetMerged() != nil
	})
	roster := f.d.WatchRoster()
	got := awaitRoster(t, f.d, roster, "the merged workspace under recently_merged", func(r *frontendv1.WorkspaceRoster) bool {
		for _, row := range r.GetRecentlyMerged().GetRows().GetRows() {
			if row.GetWorkspace().GetWorkspace().GetId() == f.ws.GetId() {
				return true
			}
		}
		return false
	})
	if rosterRow(got, f.ws.GetId()).GetMerged() == nil {
		t.Fatalf("merged workspace roster status = %v, want merged", rosterRow(got, f.ws.GetId()))
	}

	// Assert: the worktree is removed once the terminal state has settled.
	d.AwaitFileGone(dir)
}

func TestLandingAMergeWhoseTargetIsTheSelfRepoTriggersTheRolloutDeploy(t *testing.T) {
	// Arrange
	f, d, repo, script := mergeCleanRepo(t)
	script.SetExitCode(0)
	script.SetStdout("daemon: passed in 1s\n")
	// THE BRANCH HAS TO CARRY A COMMIT. The self-reload fires off what the
	// merge LANDED -- the classifier reads the changed paths -- so a merge
	// that brought in nothing triggers nothing, correctly.
	writeCommit(t, repo, f.ws.GetDir(), "modules/app/agent-repl/daemon/cmd/claude-repld/main.go", "landed\n")

	// Act
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}
	f.awaitRowInFeed(nil, "the merge's success", func(row *frontendv1.FeedRow) bool {
		return row.GetActivity().GetMerge().GetSuccess() != nil
	})

	// Assert: the fake deploy script was invoked. The self-reload trigger
	// fires after the terminal push as part of teardown, so wait for its own
	// log record rather than racing the push.
	d.AwaitRunLogOperation("daemon.merge.self_reload")
	// The deploy chain runs BEYOND the trigger call, so its own record is the
	// synchronization point; the trigger's record only says it was asked for.
	d.AwaitRunLogOperation("daemon.rollout.deploy")
	if got := len(d.Deploy.Invocations()); got == 0 {
		t.Fatalf("deploy script invocations = %d, want at least one from the self-repo landing's rollout trigger", got)
	}
}

// ---------------------------------------------------------------------------
// The non-Emacs-repo method: only pre/post prompts, never the Emacs-only
// merge/conflicts/tests/fixes tabs.
// ---------------------------------------------------------------------------

func TestTheNonEmacsRepoMethodNeverDrawsTheEmacsOnlyTabs(t *testing.T) {
	// Arrange: a repo that is NOT the daemon's self repo.
	selfRepo := harness.NewRepo(t) // distinct identity; never used as a target.
	repo := harness.NewRepo(t)
	d := harness.StartDaemon(t, harness.Opts{SelfRepo: selfRepo.Dir})
	repoRef := mergeRepositoryRef(t, d, repo)
	f := mergeCreateChild(t, d, repoRef, "feature", "do the feature", nil)

	// Act
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}

	// Assert: the merge bubble reaches a terminal state.
	root := f.watchRootFeed()
	mergeRow := awaitRow(t, f, root, "the merge's terminal push", func(row *frontendv1.FeedRow) bool {
		return row.GetActivity().GetMerge().GetSuccess() != nil || row.GetActivity().GetMerge().GetError() != nil
	})

	// Assert: no Emacs-only tab ever rode the sub-feed.
	page, _ := f.openFeed(mergeRow.GetId())
	for _, row := range page.GetSuccess().GetRows() {
		tab := row.GetMergeTab()
		if tab.GetMerge() != nil || tab.GetConflicts() != nil || tab.GetTests() != nil || tab.GetFixes() != nil {
			t.Fatalf("non-Emacs-repo merge drew tab %v, want only queue/pre-prompt/post-prompt", tab)
		}
	}
}

// ---------------------------------------------------------------------------
// Boot recovery: a merge in flight across a restart is resumed or loudly
// failed, never left stuck.
// ---------------------------------------------------------------------------

func TestAMergeInFlightAcrossADaemonRestartIsResumedOrLoudlyFailedNeverStuck(t *testing.T) {
	// Arrange: park a merge on a scripted conflict, then crash the daemon.
	repo := harness.NewRepo(t)
	d := harness.StartDaemon(t, harness.Opts{SelfRepo: repo.Dir})
	repoRef := mergeRepositoryRef(t, d, repo)
	f := mergeCreateChild(t, d, repoRef, "feature", "do the feature", nil)
	branch := mergeBranchOf(t, f.ws)
	repo.ScriptConflict(repo.Dir, branch, "conflict.txt")
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}
	f.shim.ExpectStartTurn()
	f.d.AwaitWorkspaceLogOperationCount(f.ws.GetDir(), harness.OpTurnOpened, 2)
	f.shim.PushAgentFrame(mainAgent, successFrame(mainAgent, activityID("conflict-brief-done")))
	host := f.d.WatchHost(f.ws)
	awaitView(t, f, host, "the host composer parked on the merge", func(r *agentreplv1.WatchHostWorkspaceResponse) bool {
		return r.GetHost().GetExisting().GetLive().GetMergeParked() != nil
	})
	d.Kill()

	// Act: a fresh daemon on the same state root.
	d2 := harness.StartDaemon(t, harness.Opts{StateDir: d.StateDir, SelfRepo: repo.Dir})
	d2.AwaitRunLogOperation("daemon.merge.recover")

	// Assert: the workspace's merge status is a resolved merge arm, never an
	// unset or plain "none"/"inactive" status — a stuck lease would leave no
	// resolved account of it at all.
	roster := d2.WatchRoster()
	got := awaitRoster(t, d2, roster, "a resolved merge status after recovery", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, f.ws.GetId())
		return row != nil && (row.GetMergeEnqueuing() != nil || row.GetMerging() != nil ||
			row.GetMergeQueued() != nil || row.GetMergeConflict() != nil ||
			row.GetMergeFailed() != nil || row.GetMerged() != nil)
	})
	row := rosterRow(got, f.ws.GetId())
	if row.GetNone() != nil || row.GetInactive() != nil {
		t.Fatalf("recovered workspace roster status = %v, want a resolved merge arm, not none/inactive", row)
	}
}

// ---------------------------------------------------------------------------
// A missing brief file fails the step loudly.
// ---------------------------------------------------------------------------

func TestAMissingBriefFileFailsTheMergeStepLoudly(t *testing.T) {
	// Arrange: remove the conflict brief the conflicts step reads.
	repo := harness.NewRepo(t)
	d := harness.StartDaemon(t, harness.Opts{SelfRepo: repo.Dir})
	if err := os.Remove(filepath.Join(d.PromptsDir, "merge-conflict-resolve.md")); err != nil {
		t.Fatalf("remove the conflict brief: %v", err)
	}
	repoRef := mergeRepositoryRef(t, d, repo)
	f := mergeCreateChild(t, d, repoRef, "feature", "do the feature", nil)
	branch := mergeBranchOf(t, f.ws)
	repo.ScriptConflict(repo.Dir, branch, "conflict.txt")

	// Act
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}

	// Assert: the run fails loudly rather than silently parking or hanging.
	root := f.watchRootFeed()
	mergeRow := awaitRow(t, f, root, "the merge's loud failure", func(row *frontendv1.FeedRow) bool {
		return row.GetActivity().GetMerge().GetError() != nil
	})
	if mergeRow.GetActivity().GetMerge().GetError().GetFailed() == nil {
		t.Fatalf("merge error = %v, want the failed arm (a missing brief is a run failure, not an abandonment)", mergeRow.GetActivity().GetMerge().GetError())
	}
	f.d.ExpectWarnings(harness.AllowAllWarnings)
}

// ---------------------------------------------------------------------------
// merge-prefixed helpers (this suite's own; never shared).
// ---------------------------------------------------------------------------

// mergeConflictRepairOrigin and mergeTestRepairOrigin are the two merge
// briefing origins conversation/v1/prompt_origin.proto names.
const (
	mergeConflictRepairOrigin = 22 // PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR
	mergeTestRepairOrigin     = 23 // PROMPT_ORIGIN_MERGE_TEST_REPAIR
)

// mergeEscalationFile and mergeEscalationMarker mirror
// internal/merge.EscalationFile / EscalationMarker (daemon/internal/merge/api.go):
// not part of any proto or ARCHITECTURE.md text, so this suite pins the exact
// literal rather than importing the internal package.
const (
	mergeEscalationFile   = ".agent-repl-merge-escalation"
	mergeEscalationMarker = "MERGE ESCALATION: ARCHITECTURAL CHANGE REQUIRED"
)

// mergeRepositoryRef registers a repository's main worktree and reads back
// the daemon-minted RepositoryRef from the roster — the echo token
// CreateWorkspace takes back.
func mergeRepositoryRef(t *testing.T, d *harness.Daemon, repo *harness.Repo) *workspacev1.RepositoryRef {
	t.Helper()
	harness.Register(t, d, repo.Dir)
	roster := d.WatchRoster()
	got := awaitRoster(t, d, roster, "the repository's roster section", func(r *frontendv1.WorkspaceRoster) bool {
		return mergeFindRepoKey(r, repo.Dir) != nil
	})
	ref := mergeFindRepoKey(got, repo.Dir)
	if ref == nil {
		t.Fatalf("no roster repository section for %s", repo.Dir)
	}
	return ref
}

// mergeFindRepoKey finds a repository section by its worktree.
//
// A repository is keyed by its COMMON DIR -- registration derives it from git
// and it is `<worktree>/.git` for an ordinary checkout -- so a worktree's
// section is found under either spelling rather than the worktree's alone.
func mergeFindRepoKey(r *frontendv1.WorkspaceRoster, dir string) *workspacev1.RepositoryRef {
	for _, s := range r.GetRepository().GetSections() {
		switch s.GetKey().GetRepository().GetDir() {
		case dir, filepath.Join(dir, ".git"):
			return s.GetKey().GetRepository()
		}
	}
	return nil
}

// mergeCreateChild creates a top-level child workspace in a repository via
// CreateWorkspace (recording real layout facts), waits for its fake shim to
// spawn, and concludes its initial turn so the workspace starts idle.
func mergeCreateChild(t *testing.T, d *harness.Daemon, repoRef *workspacev1.RepositoryRef, name, prompt string, actions *agentreplv1.CreateWorkspaceMergeActions) *fixture {
	t.Helper()
	resp, err := d.Client().CreateWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Repository: repoRef,
		Form: &agentreplv1.CreateWorkspaceRequest_Standard{Standard: &agentreplv1.CreateWorkspaceStandard{
			InitialPrompt: said(prompt),
			Name:          strPtr(name),
			MergeActions:  actions,
		}},
	}))
	if err != nil {
		t.Fatalf("CreateWorkspace(%s) = error %v, want a success", name, err)
	}
	ws := resp.Msg.GetSuccess().GetWorkspace()
	if ws.GetId() == "" {
		t.Fatalf("CreateWorkspace(%s) = %v, want a success carrying a workspace ref", name, resp.Msg)
	}
	d.WatchWorkspaceLogs(ws.GetDir())
	shim := d.Shim(ws)
	shim.ExpectStartSession()
	shim.ExpectStartTurn()
	// The fake records StartTurn on ARRIVAL; the terminal frame must not be
	// pushed until the daemon has the turn OPEN, or the terminal names no turn
	// and everything waiting on that turn's end waits forever.
	d.AwaitWorkspaceLogOperationCount(ws.GetDir(), harness.OpTurnOpened, 1)
	shim.PushAgentFrame(mainAgent, successFrame(mainAgent, activityID(name+"-initial")))
	return &fixture{d: d, ws: ws, shim: shim, t: t}
}

// mergeBranchOf is the branch a mergeCreateChild workspace checked out —
// its directory's basename, since Name was passed as the branch name.
func mergeBranchOf(t *testing.T, ws *workspacev1.WorkspaceRef) string {
	t.Helper()
	return filepath.Base(ws.GetDir())
}

// mergeDequeueOffer finds the merge_dequeue held offer, if standing.
func mergeDequeueOffer(tray *frontendv1.DaemonHoldTray) *frontendv1.HeldOffer {
	for _, item := range tray.GetItems() {
		if offer := item.GetOffer(); offer.GetMergeDequeue() != nil {
			return offer
		}
	}
	return nil
}
