//go:build integration

package integration

import (
	"database/sql"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
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

	// Act: race a poller watching for the worktree's disappearance against
	// awaiting the terminal push, so the ordering is proven across two
	// independent timelines rather than sampled once right after the other —
	// a poll started only AFTER the await returns could simply be too late to
	// ever observe a too-early removal.
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}
	goneBeforePush := make(chan struct{})
	stopPolling := make(chan struct{})
	defer close(stopPolling)
	go func() {
		ticker := time.NewTicker(5 * time.Millisecond)
		defer ticker.Stop()
		for {
			select {
			case <-stopPolling:
				return
			case <-ticker.C:
				if !repo.HasWorktree(dir) {
					close(goneBeforePush)
					return
				}
			}
		}
	}()

	// Assert: FeedMergeSuccess{commit}.
	root := f.watchRootFeed()
	mergeRow := awaitRow(t, f, root, "the merge's success", func(row *frontendv1.FeedRow) bool {
		return row.GetActivity().GetMerge().GetSuccess() != nil
	})
	if mergeRow.GetActivity().GetMerge().GetSuccess().GetCommit() == "" {
		t.Fatalf("FeedMergeSuccess = %v, want a landed commit", mergeRow.GetActivity().GetMerge().GetSuccess())
	}

	// Assert: the poller never won the race — the worktree was not gone
	// before the terminal push arrived (removal happens only AFTER it).
	select {
	case <-goneBeforePush:
		t.Fatalf("the worktree %s was removed before the terminal push arrived, want removal only after it", dir)
	default:
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
	if got := len(d.Deploy.Invocations()); got != 1 {
		t.Fatalf("deploy script invocations = %d, want EXACTLY 1 from the self-repo landing's rollout trigger (no double-fire)", got)
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
	// Warning discipline: this run's own subject produces exactly two records —
	// "the merge conflicted" (daemon.merge.merge_tab, WARN, fired the instant the
	// no-ff merge conflicts, before any brief is ever read) and "a merge could
	// not continue" (daemon.merge.abort, ERROR, the missing-brief failure this
	// test is about) — read off internal/merge/phases.go's mergeTab Warn and
	// internal/merge/terminal.go's abort Error call sites.
	// The scripted conflict is stated by the git client too, and the aborted
	// run stops the admission pump: both are this failure, once each.
	f.d.ExpectWarnings("daemon.merge.merge_tab", "daemon.merge.abort",
		"daemon.gitclient.merge_no_ff", "daemon.merge.pump")
}

// ---------------------------------------------------------------------------
// The pre-prompt and post-prompt tabs: agentic, output-address parented, and
// the post-prompt's failure never fails the run.
// ---------------------------------------------------------------------------

func TestPrePromptTabRunsUnderTheLeaseAndParentsItsRowsToItsTabNotTheRoot(t *testing.T) {
	// Arrange: a configured before-merge prompt.
	repo := harness.NewRepo(t)
	d := harness.StartDaemon(t, harness.Opts{SelfRepo: repo.Dir})
	repoRef := mergeRepositoryRef(t, d, repo)
	f := mergeCreateChild(t, d, repoRef, "prepromptrepo", "do the feature", &agentreplv1.CreateWorkspaceMergeActions{
		BeforeWsMerge: said("run the setup script"),
	})

	// Act: enqueue and answer the pre-prompt's own turn.
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}
	req := f.shim.ExpectStartTurn()
	if req.GetOrigin() != mergeBeforeActionOrigin {
		t.Fatalf("pre-prompt StartTurn.origin = %v, want PROMPT_ORIGIN_MERGE_BEFORE_ACTION", req.GetOrigin())
	}

	mergeRow := f.awaitRowInFeed(nil, "the merge bubble's head", func(row *frontendv1.FeedRow) bool {
		return row.GetActivity().GetMerge() != nil
	})
	preTab := f.awaitRowInFeed(mergeRow.GetId(), "the pre_prompt tab, live", func(row *frontendv1.FeedRow) bool {
		return row.GetMergeTab().GetPrePrompt() != nil
	})

	// Act: conclude the pre-prompt's turn.
	f.shim.PushAgentFrame(mainAgent, successFrame(mainAgent, activityID("pre-prompt-done")))

	// Assert: the turn's own concluded row is parented to the pre_prompt tab —
	// the OUTPUT ADDRESS the lease's session was stamped with — never to root.
	// The tab's content is the merge sub-feed's rows PARENTED to the tab row
	// (feed.proto: "Content: sub-feed rows parented to this row") — a tab row
	// is not itself a feed, so the concluded row is looked up on the bubble's
	// sub-feed and its parent is what places it on the tab.
	concluded := f.awaitRowInFeed(mergeRow.GetId(), "the pre-prompt turn's concluded row", func(row *frontendv1.FeedRow) bool {
		return row.GetTurnEnded().GetConcluded() != nil
	})
	if concluded.GetParent().GetRow().GetValue() != preTab.GetId().GetValue() {
		t.Fatalf("the pre-prompt turn's concluded row parent = %v, want the pre_prompt tab %v",
			concluded.GetParent().GetRow(), preTab.GetId())
	}
	// Assert: THAT turn's terminal never rode the root feed. The workspace's
	// own creation turn ended on root before the merge was ever enqueued, so
	// the check is scoped to the pre-prompt turn's id rather than to every
	// turn_ended row.
	page, _ := f.openFeed(nil)
	for _, row := range page.GetSuccess().GetRows() {
		if row.GetTurnEnded() != nil && row.GetTurn().GetValue() == concluded.GetTurn().GetValue() {
			t.Fatalf("the pre-prompt turn's concluded row rode the ROOT feed: %v, want it parented to the pre_prompt tab only", row)
		}
	}

	// Assert: the tab itself settles.
	settled := f.awaitRowInFeed(mergeRow.GetId(), "the pre_prompt tab, settled", func(row *frontendv1.FeedRow) bool {
		return row.GetMergeTab().GetPrePrompt().GetSettled() != nil
	})
	if settled.GetMergeTab().GetPrePrompt().GetSettled().GetSucceeded() == nil {
		t.Fatalf("pre_prompt tab settled = %v, want succeeded", settled.GetMergeTab().GetPrePrompt())
	}
}

func TestAFailingPostPromptNeverFailsTheRunAndRidesTheTerminalSuccess(t *testing.T) {
	// Arrange: a clean landing with a configured after-merge prompt that fails.
	repo := harness.NewRepo(t)
	script := harness.NewTestAllScript(t, repo.Dir)
	script.SetExitCode(0)
	script.SetStdout("daemon: passed in 1s\n")
	d := harness.StartDaemon(t, harness.Opts{SelfRepo: repo.Dir, ExtraEnv: []string{"AGENT_REPL_TEST_ALL_SCRIPT=" + script.Path}})
	repoRef := mergeRepositoryRef(t, d, repo)
	f := mergeCreateChild(t, d, repoRef, "postpromptfail", "do the clean thing", &agentreplv1.CreateWorkspaceMergeActions{
		PostprocessingPrompt: said("clean up after landing"),
	})

	// Act: land, then answer the post-prompt's turn with a FAILURE.
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}
	req := f.shim.ExpectStartTurn()
	if req.GetOrigin() != mergeAfterActionOrigin {
		t.Fatalf("post-prompt StartTurn.origin = %v, want PROMPT_ORIGIN_MERGE_AFTER_ACTION", req.GetOrigin())
	}
	f.shim.PushAgentFrame(mainAgent, failureFrame(mainAgent, &conversationv1.AgentFailure{
		Failure: &conversationv1.AgentFailure_ApiRequestFailed{ApiRequestFailed: &conversationv1.ApiRequestFailed{
			Message: "boom",
			Kind:    &conversationv1.ApiRequestFailed_Internal{Internal: &conversationv1.ApiInternal{}},
		}},
	}))

	// Assert: the merge STILL lands as FeedMergeSuccess — the post-prompt's
	// failure rides the terminal rather than turning it into FeedMergeError.
	root := f.watchRootFeed()
	mergeRow := awaitRow(t, f, root, "the merge's terminal push", func(row *frontendv1.FeedRow) bool {
		return row.GetActivity().GetMerge().GetSuccess() != nil || row.GetActivity().GetMerge().GetError() != nil
	})
	if mergeRow.GetActivity().GetMerge().GetSuccess() == nil {
		t.Fatalf("merge terminal = %v, want FeedMergeSuccess even though the post-prompt failed", mergeRow.GetActivity().GetMerge())
	}

	// Warning discipline: the production path logs exactly one WARN for this
	// (internal/merge/run.go's method(), "the post-merge prompt failed; the
	// merge still landed", op daemon.merge.post_prompt).
	f.d.ExpectWarnings("daemon.merge.post_prompt")
}

// ---------------------------------------------------------------------------
// The displaced user turn: captured once at admission, resubmitted exactly
// once at release, never resubmitted across a restart mid-merge.
//
// UNEXPRESSIBLE WITHOUT A HARNESS/PRODUCTION HOOK — see this suite's report.
// Tracing internal/promptqueue/submit.go: a merge's own submission bypasses
// the LEASE refusal for merge origins (applyLeasePolicy's mergeOrigin
// special-case), but EVERY submission — the merge's own included, and
// resubmitDisplaced's own Queue.Submit at release — still falls through to
// the UNCONDITIONAL watcher.TurnInFlight() check just below it. So as long as
// the displaced turn's own terminal frame was never pushed (which is what
// keeps its DB row "open" for resubmitDisplaced's OpenTurns lookup to find),
// every later Queue.Submit on that workspace — the conflict/tests-repair
// brief, a configured pre/post prompt, AND resubmitDisplaced's own call — is
// HELD behind it instead of dispatched, because TurnInFlight() is gated
// unconditionally, with no merge-origin bypass. Pushing the turn's terminal
// frame to unblock that clears TurnInFlight() but ALSO closes the turn's DB
// row through the ordinary conclusion pipeline, removing it from OpenTurns —
// so resubmitDisplaced then finds nothing to resubmit. The only state that is
// simultaneously "open in the DB" (so OpenTurns finds it) and "not in flight"
// (so Queue.Submit does not hold on it) is what a daemon CRASH produces: the
// in-memory watcher is rebuilt empty on the new process while the turn's
// durable row is left open — exactly what resubmitDisplaced's own doc comment
// ("resubmitted EXACTLY ONCE... even across a daemon bounce") describes.
//
// Landing a crash deterministically inside the narrow window between
// CaptureDisplaced and either (a) the merge's own next Queue.Submit or (b) a
// CLEAN (no-conflict) run's near-instant finish() needs a pause point this
// suite has no hook for — there is no test knob to freeze a merge run
// mid-method, and a scripted conflict is the only park point this harness
// offers, but reaching it requires the conflict brief's OWN Queue.Submit to
// go through, which is exactly the call this scenario would leave held.
func TestADisplacedUserTurnIsResubmittedExactlyOnceAcrossADaemonBounce(t *testing.T) {
	t.Skip("unexpressible: no harness hook pauses a merge run between CaptureDisplaced " +
		"and its own next Queue.Submit, so a crash cannot be landed deterministically " +
		"in the window that leaves a turn open-and-displaced without also blocking the " +
		"merge's own submissions via internal/promptqueue/submit.go's unconditional " +
		"watcher.TurnInFlight() hold-check (see this test's own doc comment, and the " +
		"suite's report, for the full trace)")
}

// ---------------------------------------------------------------------------
// Conflicts: briefed exactly once per conflict commit; parked guidance lands
// on the conflicts tab, never as a root-feed row.
// ---------------------------------------------------------------------------

func TestAConflictedMergeBriefsTheAgentExactlyOnceEvenAfterItParks(t *testing.T) {
	// Arrange / Act: park a merge on a scripted conflict, exactly as the
	// spliced-brief test does.
	repo := harness.NewRepo(t)
	d := harness.StartDaemon(t, harness.Opts{SelfRepo: repo.Dir})
	repoRef := mergeRepositoryRef(t, d, repo)
	f := mergeCreateChild(t, d, repoRef, "onceconflict", "do the feature", nil)
	// THE CREATION ALREADY SENT ONE StartTurn — its initial prompt — so the
	// brief is counted as a DELTA over that, never as an absolute count.
	turnsBeforeTheMerge := f.shim.Count(harness.RPCStartTurn)
	branch := mergeBranchOf(t, f.ws)
	repo.ScriptConflict(repo.Dir, branch, "conflict.txt")
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}
	f.shim.ExpectStartTurn()
	f.d.AwaitWorkspaceLogOperationCount(f.ws.GetDir(), harness.OpTurnOpened, 2)
	f.shim.PushAgentFrame(mainAgent, successFrame(mainAgent, activityID("conflict-brief-done")))
	footer := f.d.WatchFooter(f.ws)
	awaitFooter(t, f, footer, "the footer's parked substatus", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetMerging().GetParked() != nil
	})

	// Assert: with the run settled on park, still exactly one StartTurn was
	// ever sent for this conflict commit — the ONE brief, never a repeat.
	if got := f.shim.Count(harness.RPCStartTurn) - turnsBeforeTheMerge; got != 1 {
		t.Fatalf("StartTurns since the merge began = %d, want exactly 1 (the conflict is briefed once, then parks)", got)
	}
}

func TestParkedGuidanceLandsAsAUserPromptRowOnTheConflictsTabNeverOnTheRootFeed(t *testing.T) {
	// Arrange: park a merge on a scripted conflict.
	repo := harness.NewRepo(t)
	d := harness.StartDaemon(t, harness.Opts{SelfRepo: repo.Dir})
	repoRef := mergeRepositoryRef(t, d, repo)
	f := mergeCreateChild(t, d, repoRef, "guidancetab", "do the feature", nil)
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
	rootMergeRow := f.awaitRowInFeed(nil, "the merge bubble's head", func(row *frontendv1.FeedRow) bool {
		return row.GetActivity().GetMerge() != nil
	})
	conflictsTab := f.awaitRowInFeed(rootMergeRow.GetId(), "the conflicts tab, parked", func(row *frontendv1.FeedRow) bool {
		return row.GetMergeTab().GetConflicts().GetParked() != nil
	})

	// Act: submit guidance while parked.
	guidanceText := "please look again"
	if err := f.submitExpectingError(&agentreplv1.SubmitPromptRequest{
		Workspace: f.ws, Said: said(guidanceText), IdempotencyKey: "k-guidance-tab", Origin: origin,
	}); err != nil {
		t.Fatalf("SubmitPrompt while merge-parked = error %v, want it delivered to the conflicts tab", err)
	}
	req := f.shim.ExpectStartTurn()

	// Assert: the redelivery carries the MERGE origin.
	if req.GetOrigin() != mergeConflictRepairOrigin {
		t.Fatalf("guidance StartTurn.origin = %v, want PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR", req.GetOrigin())
	}

	// Assert: the guidance's user_prompt row is parented to the conflicts tab.
	// The tab's content is the merge sub-feed's rows PARENTED to the tab row
	// (feed.proto: "Content: sub-feed rows parented to this row") — a tab row
	// is not itself a feed.
	guidanceRow := f.awaitRowInFeed(rootMergeRow.GetId(), "the guidance's user_prompt row on the conflicts tab", func(row *frontendv1.FeedRow) bool {
		return row.GetUserPrompt() != nil && promptText(row) == guidanceText
	})
	if guidanceRow.GetParent().GetRow().GetValue() != conflictsTab.GetId().GetValue() {
		t.Fatalf("the guidance's user_prompt row parent = %v, want the conflicts tab %v",
			guidanceRow.GetParent().GetRow(), conflictsTab.GetId())
	}

	// Assert: NO row for that guidance ever rode the ROOT feed.
	page, _ := f.openFeed(nil)
	for _, row := range page.GetSuccess().GetRows() {
		if row.GetUserPrompt() != nil && promptText(row) == guidanceText {
			t.Fatalf("the parked guidance %q landed on the ROOT feed: %v, want it confined to the conflicts tab", guidanceText, row)
		}
	}
}

// ---------------------------------------------------------------------------
// UpdateMergeQueue: an unset repository means every repository; an unknown
// one is refused.
// ---------------------------------------------------------------------------

func TestUpdateMergeQueuePauseWithNoRepositoryPausesEveryRepositoryWithAQueue(t *testing.T) {
	// Arrange: two independent repositories, each with a blocked queue, on
	// ONE daemon.
	d := harness.StartDaemon(t, harness.Opts{})
	_, _, _, repoRefA := mergeBlockedRepoOn(t, d, "repoa")
	_, _, _, repoRefB := mergeBlockedRepoOn(t, d, "repob")

	// Act: pause with NO repository named.
	resp, err := d.Client().UpdateMergeQueue(d.Ctx(), connect.NewRequest(&agentreplv1.UpdateMergeQueueRequest{
		Action: &agentreplv1.UpdateMergeQueueRequest_Pause{Pause: &agentreplv1.UpdateMergeQueuePause{}},
	}))
	if err != nil || resp.Msg.GetError() != nil {
		t.Fatalf("UpdateMergeQueue(pause, no repository) = %v, %v, want a success", resp.Msg, err)
	}

	// Assert: BOTH repositories' queues are now paused — proven by each one
	// individually refusing a further pause as already_paused.
	for _, ref := range []*workspacev1.RepositoryRef{repoRefA, repoRefB} {
		again, err := d.Client().UpdateMergeQueue(d.Ctx(), connect.NewRequest(&agentreplv1.UpdateMergeQueueRequest{
			Action: &agentreplv1.UpdateMergeQueueRequest_Pause{Pause: &agentreplv1.UpdateMergeQueuePause{Repository: ref}},
		}))
		if err != nil {
			t.Fatalf("UpdateMergeQueue(pause, %v) = error %v, want a success carrying already_paused", ref, err)
		}
		if again.Msg.GetError().GetAlreadyPaused() == nil {
			t.Fatalf("UpdateMergeQueue(pause, %v) = %v, want already_paused (the unset pause should have covered it)", ref, again.Msg)
		}
	}
}

func TestUpdateMergeQueueOnAnUnknownRepositoryIsRefused(t *testing.T) {
	// Arrange: a daemon that has never heard of this repository ref at all.
	d := harness.StartDaemon(t, harness.Opts{})
	bogus := &workspacev1.RepositoryRef{Dir: "/nowhere/this/repo/does/not/exist"}

	// Act
	resp, err := d.Client().UpdateMergeQueue(d.Ctx(), connect.NewRequest(&agentreplv1.UpdateMergeQueueRequest{
		Action: &agentreplv1.UpdateMergeQueueRequest_Pause{Pause: &agentreplv1.UpdateMergeQueuePause{Repository: bogus}},
	}))

	// Assert
	if err != nil {
		t.Fatalf("UpdateMergeQueue(pause, unknown repository) = error %v, want a success carrying unknown_repository", err)
	}
	if resp.Msg.GetError().GetUnknownRepository() == nil {
		t.Fatalf("UpdateMergeQueue(pause, unknown repository) = %v, want UpdateMergeQueueError.unknown_repository", resp.Msg)
	}
}

// ---------------------------------------------------------------------------
// The test gate: blast-radius suite selection and ANSI-painted output.
// ---------------------------------------------------------------------------

func TestTheTestGateNarrowsToTheBlastRadiusOfTheLandedChange(t *testing.T) {
	// Arrange: a landed commit touching ONLY the daemon module, whose blast
	// radius (internal/merge/suiteselect.go's own rule table) is exactly the
	// "daemon" suite.
	f, d, repo, script := mergeCleanRepo(t)
	script.SetExitCode(0)
	script.SetStdout("daemon: passed in 1s\n")
	writeCommit(t, repo, f.ws.GetDir(), "modules/app/agent-repl/daemon/internal/merge/blastradius.go", "touched\n")

	// Act
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}
	root := f.watchRootFeed()
	awaitRow(t, f, root, "the merge's success", func(row *frontendv1.FeedRow) bool {
		return row.GetActivity().GetMerge().GetSuccess() != nil
	})

	// Assert: the script was invoked with EXACTLY the narrowed selection, not
	// the full 17-suite roster.
	invocations := script.Invocations()
	if len(invocations) != 1 {
		t.Fatalf("test-all.sh invocations = %d, want exactly 1", len(invocations))
	}
	argv := invocations[0].Argv
	if !sameOrder(argv, []string{"--suites", "daemon"}) {
		t.Fatalf("test-all.sh argv = %v, want it to end with [--suites daemon] (the daemon-only blast radius)", argv)
	}
}

func TestTheTestsTabPaintsANSISpansFromTheScriptedOutput(t *testing.T) {
	// Arrange: the same daemon-only blast radius, with a green ANSI escape in
	// the scripted suite's own output.
	f, d, repo, script := mergeCleanRepo(t)
	script.SetExitCode(0)
	script.SetStdout("\x1b[32mall green\x1b[0m\ndaemon: passed in 1s\n")
	writeCommit(t, repo, f.ws.GetDir(), "modules/app/agent-repl/daemon/internal/merge/paint.go", "touched\n")

	// Act
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}
	root := f.watchRootFeed()
	mergeRow := awaitRow(t, f, root, "the merge's success", func(row *frontendv1.FeedRow) bool {
		return row.GetActivity().GetMerge().GetSuccess() != nil
	})

	// Assert: the settled tests tab carries the "daemon" suite's output as
	// DAEMON-PARSED paint spans — the client never sees the raw escape.
	testsRow := f.awaitRowInFeed(mergeRow.GetId(), "the settled tests tab", func(row *frontendv1.FeedRow) bool {
		return row.GetMergeTab().GetTests().GetSettled() != nil
	})
	suites := testsRow.GetMergeTab().GetTests().GetSuites()
	if len(suites) != 1 || suites[0].GetName() != "daemon" {
		t.Fatalf("tests tab suites = %v, want exactly one suite named %q", suites, "daemon")
	}
	var found bool
	for _, span := range suites[0].GetOutput() {
		if span.GetPaintClass() == "ansi-fg-green" && strings.Contains(span.GetText(), "all green") {
			found = true
		}
		if strings.ContainsAny(span.GetText(), "\x1b") {
			t.Fatalf("a raw ANSI escape reached the wire: span = %+v, want the daemon to have parsed it", span)
		}
	}
	if !found {
		t.Fatalf("tests tab suite %q output = %v, want a span {text: contains %q, paint_class: ansi-fg-green}", "daemon", suites[0].GetOutput(), "all green")
	}
}

// ---------------------------------------------------------------------------
// The merge tab's narration is asserted by content.
// ---------------------------------------------------------------------------

func TestTheMergeTabNarratesTheNoFFLandingByContent(t *testing.T) {
	// Arrange
	f, d, _, script := mergeCleanRepo(t)
	script.SetExitCode(0)
	script.SetStdout("daemon: passed in 1s\n")

	// Act
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}
	root := f.watchRootFeed()
	mergeRow := awaitRow(t, f, root, "the merge's success", func(row *frontendv1.FeedRow) bool {
		return row.GetActivity().GetMerge().GetSuccess() != nil
	})
	commit := mergeRow.GetActivity().GetMerge().GetSuccess().GetCommit()
	branch := mergeBranchOf(t, f.ws)

	// Assert: the merge tab settles with the exact two composed lines —
	// the opening narration and the landed line naming the short SHA.
	mergeTabRow := f.awaitRowInFeed(mergeRow.GetId(), "the settled merge tab", func(row *frontendv1.FeedRow) bool {
		return row.GetMergeTab().GetMerge().GetSettled() != nil
	})
	lines := mergeTabRow.GetMergeTab().GetMerge().GetLines()
	wantOpening := "merging " + branch + " into " + harness.DefaultBranch
	if len(lines) < 2 {
		t.Fatalf("merge tab lines = %v, want at least 2 narration lines", lines)
	}
	if lines[0].GetText() != wantOpening {
		t.Fatalf("merge tab's opening narration = %q, want %q", lines[0].GetText(), wantOpening)
	}
	wantShort := commit
	if len(wantShort) > 12 {
		wantShort = wantShort[:12]
	}
	wantClosing := "merged cleanly · " + wantShort
	if lines[len(lines)-1].GetText() != wantClosing {
		t.Fatalf("merge tab's closing narration = %q, want %q", lines[len(lines)-1].GetText(), wantClosing)
	}
}

// ---------------------------------------------------------------------------
// The merge ledger holds the tab intervals. NO rpc serves it (grepped
// modules/app/agent-repl/proto and internal/wsm/mergeledger.go); it is
// persisted only in the workspace state database's merge_tab_intervals table,
// so this test reads it directly with harness.WithDB, ONLY AFTER stopping the
// daemon, per that helper's own documented contract.
// ---------------------------------------------------------------------------

func TestALandedMergesLedgerRecordsEachTabsInterval(t *testing.T) {
	// Arrange
	f, d, _, script := mergeCleanRepo(t)
	script.SetExitCode(0)
	script.SetStdout("daemon: passed in 1s\n")

	// Act
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}
	root := f.watchRootFeed()
	awaitRow(t, f, root, "the merge's success", func(row *frontendv1.FeedRow) bool {
		return row.GetActivity().GetMerge().GetSuccess() != nil
	})
	d.Stop()

	// Assert: the ledger's merge_tab_intervals table carries one succeeded
	// interval each for "queue", "merge" and "tests" — the queue wait plus the
	// two phases this clean, no-configured-action landing opens.
	type interval struct {
		kind               string
		startedAt, endedAt int64
		outcome            string
	}
	var got []interval
	d.WithDB(func(db *sql.DB) {
		rows, err := db.Query(`SELECT kind, started_at, ended_at, outcome FROM merge_tab_intervals ORDER BY started_at`)
		if err != nil {
			t.Fatalf("query merge_tab_intervals: %v", err)
		}
		defer rows.Close()
		for rows.Next() {
			var iv interval
			if err := rows.Scan(&iv.kind, &iv.startedAt, &iv.endedAt, &iv.outcome); err != nil {
				t.Fatalf("scan merge_tab_intervals row: %v", err)
			}
			got = append(got, iv)
		}
		if err := rows.Err(); err != nil {
			t.Fatalf("iterate merge_tab_intervals: %v", err)
		}
	})

	byKind := map[string]interval{}
	for _, iv := range got {
		byKind[iv.kind] = iv
	}
	for _, kind := range []string{"queue", "merge", "tests"} {
		iv, ok := byKind[kind]
		if !ok {
			t.Fatalf("merge_tab_intervals holds no %q row, want one; got %+v", kind, got)
		}
		if iv.startedAt <= 0 || iv.endedAt <= 0 || iv.endedAt < iv.startedAt {
			t.Fatalf("merge_tab_intervals[%q] = %+v, want a well-formed [started_at, ended_at] interval", kind, iv)
		}
		if iv.outcome != "succeeded" {
			t.Fatalf("merge_tab_intervals[%q].outcome = %q, want %q", kind, iv.outcome, "succeeded")
		}
	}
}

// ---------------------------------------------------------------------------
// merge-prefixed helpers (this suite's own; never shared).
// ---------------------------------------------------------------------------

// mergeConflictRepairOrigin and mergeTestRepairOrigin are the two merge
// briefing origins conversation/v1/prompt_origin.proto names.
const (
	mergeConflictRepairOrigin  = 22 // PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR
	mergeTestRepairOrigin      = 23 // PROMPT_ORIGIN_MERGE_TEST_REPAIR
	mergeBeforeActionOrigin    = 24 // PROMPT_ORIGIN_MERGE_BEFORE_ACTION
	mergeAfterActionOrigin     = 25 // PROMPT_ORIGIN_MERGE_AFTER_ACTION
	mergeDisplacedResumeOrigin = 26 // PROMPT_ORIGIN_MERGE_DISPLACED_TURN_RESUME
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
	// A created workspace is an OPENED one, so all three connectivity hops are
	// up (daemon.md invariant 11): without the two client streams its footer
	// reads disconnected, which outranks every merge substatus.
	return &fixture{d: d, ws: ws, shim: shim, t: t,
		host: d.WatchHost(ws), web: d.WatchWeb(ws)}
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
