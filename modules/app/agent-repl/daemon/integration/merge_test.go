//go:build integration

package integration

import (
	"database/sql"
	"os"
	"path/filepath"
	"strings"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	shimv1 "agentrepl/proto/shim/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/integration/harness"

	"connectrpc.com/connect"
)

// merge_test.go exercises MergeWorkspace, UpdateMergeQueue, AnswerHeldOffer
// and the merge orchestrator's visible effects (feed sub-feed, footer,
// roster, host composer) against a daemon whose own-checkout identity is
// injected via harness.Opts.SelfRepo, with a FAKE bin/test-all.sh
// (harness.NewTestAllScript) pointed at through AGENT_REPL_TEST_ALL_SCRIPT in
// harness.Opts.ExtraEnv, and the deploy's fake build at d.Deploy. Conflicts are
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
	t.Parallel()
	// Arrange: a workspace registered directly, never created, so it carries
	// no creation job.
	f := newRegistered(t, harness.Opts{})
	// The sweep covers every test; the declared records are evidence of a refusal the test provokes.
	f.d.ExpectWarnings("daemon.merge.enqueue")

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
	pushConcludedTurn(front.shim, mainAgent, "front-conflict-brief")
	// The success frame and the merge worker are separate observers. Wait for
	// the worker's parked record so a test cannot finish and tear the daemon
	// down while its final conflicted-files subprocess is still running.
	front.d.AwaitWorkspaceLogOperation(front.ws.GetDir(), "daemon.merge.conflicts")

	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: behind.ws})); err != nil {
		t.Fatalf("MergeWorkspace(behind) = error %v, want the merge enqueued", err)
	}
	return front, behind, repo, d
}

func TestASecondWorkspaceInTheSameRepoQueuesBehindTheFirstWithTheQueueTabFooterAndRoster(t *testing.T) {
	t.Parallel()
	// Arrange / Act
	_, behind, _, _ := mergeBlockedQueueFixture(t)
	// The sweep covers every test; the declared records are evidence of the merge conflict the test stages.
	behind.d.ExpectWarnings("daemon.merge.conflicts", "daemon.gitclient.merge_no_ff", "daemon.merge.merge_tab")

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
	t.Parallel()
	// Arrange
	_, _, repo, d := mergeBlockedQueueFixture(t)
	// The sweep covers every test; the declared records are evidence of a refusal the test provokes, the merge conflict the test stages.
	d.ExpectWarnings("daemon.merge.conflicts", "daemon.gitclient.merge_no_ff", "daemon.merge.merge_tab", "daemon.merge.pause",
		"daemon.merge.unpause")
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
	t.Parallel()
	// Arrange
	_, behind, _, _ := mergeBlockedQueueFixture(t)
	// The sweep covers every test; the declared records are evidence of the merge conflict the test stages, the queued merge the test abandons.
	behind.d.ExpectWarnings("daemon.merge.conflicts", "daemon.gitclient.merge_no_ff", "daemon.merge.drop_queued",
		"daemon.merge.merge_tab")
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
	t.Parallel()
	// Arrange
	_, behind, _, _ := mergeBlockedQueueFixture(t)
	// The sweep covers every test; the declared records are evidence of the merge conflict the test stages.
	behind.d.ExpectWarnings("daemon.merge.conflicts", "daemon.gitclient.merge_no_ff", "daemon.merge.merge_tab")
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
	t.Parallel()
	// Arrange
	_, behind, _, _ := mergeBlockedQueueFixture(t)
	// The sweep covers every test; the declared records are evidence of the merge conflict the test stages, the queued merge the test abandons.
	behind.d.ExpectWarnings("daemon.merge.conflicts", "daemon.gitclient.merge_no_ff",
		"daemon.merge.drop_queued", "daemon.merge.merge_tab")
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
	t.Parallel()
	// Arrange
	_, behind, _, _ := mergeBlockedQueueFixture(t)
	// The sweep covers every test; the declared records are evidence of the merge conflict the test stages.
	behind.d.ExpectWarnings("daemon.gitclient.merge_no_ff", "daemon.merge.conflicts",
		"daemon.merge.merge_tab")
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
// The FOUR DISTINCT ENDS a queued merge can meet before it ever runs: the
// user's own evict, the user's release of the dequeue offer, the workspace
// being killed or nuked under it, and a restart that cannot put it back on its
// queue.
//
// frontend/v1/feed.proto's FeedMergeError carries only `failed` and
// `abandoned` -- no per-cause arm -- so the four are NOT distinguishable by
// arm. Landing 7's `FeedMergeAbandoned.summary` is where the cause lives
// instead: one resolved sentence per cause, composed in internal/merge from
// the AbandonCause the dropping site names, drawn as the collapsed line
// exactly as `FeedMergeFailed.summary` is. These tests assert the arm and the
// surfaces here, and the sentence itself where the cause is the point; the
// per-cause sentences are pinned as a set in internal/merge's own suite.
// ---------------------------------------------------------------------------

func TestAnEvictedQueuedMergeEndsAsFeedMergeAbandonedWithTheFooterAndRosterLeavingMerging(t *testing.T) {
	t.Parallel()
	// Arrange
	_, behind, _, _ := mergeBlockedQueueFixture(t)
	// The sweep covers every test; the declared records are evidence of the merge conflict the test stages, the queued merge the test abandons.
	behind.d.ExpectWarnings("daemon.merge.conflicts", "daemon.gitclient.merge_no_ff", "daemon.merge.drop_queued",
		"daemon.merge.merge_tab")
	root := behind.watchRootFeed()
	footer := behind.d.WatchFooter(behind.ws)

	// Act
	resp, err := behind.d.Client().UpdateMergeQueue(behind.d.Ctx(), connect.NewRequest(&agentreplv1.UpdateMergeQueueRequest{
		Action: &agentreplv1.UpdateMergeQueueRequest_Evict{Evict: &agentreplv1.UpdateMergeQueueEvict{Workspace: behind.ws}},
	}))
	if err != nil || resp.Msg.GetError() != nil {
		t.Fatalf("UpdateMergeQueue(evict) = %v, %v, want a success", resp.Msg, err)
	}

	// Assert: the bubble's terminal row. FeedMergeError.abandoned is the
	// ONLY arm this proto has for any queued merge dropped before it ran --
	// there is no distinct "evicted" arm to ask for instead.
	mergeRow := awaitRow(t, behind, root, "the evicted merge's abandoned terminal", func(row *frontendv1.FeedRow) bool {
		return row.GetActivity().GetMerge().GetError() != nil
	})
	if mergeRow.GetActivity().GetMerge().GetError().GetAbandoned() == nil {
		t.Fatalf("evicted merge terminal = %v, want FeedMergeError.abandoned (the only end this proto expresses for evict, dequeue AND a self-abandon alike)", mergeRow.GetActivity().GetMerge().GetError())
	}

	// Assert: the footer's merging status ENDS. FooterStatusMerging's oneof
	// (frontend/v1/footer.proto) has no dedicated evicted/abandoned
	// substatus, so all this proto can express is that "merging" no longer
	// stands.
	fv := awaitFooter(t, behind, footer, "the footer leaving merging after the evict", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetMerging() == nil
	})
	if fv.GetStrip().GetStatus().GetMerging() != nil {
		t.Fatalf("footer status = %v, want merging cleared after the evict", fv.GetStrip().GetStatus())
	}

	// Assert: the roster arm afterward carries none of the merge arms.
	roster := behind.d.WatchRoster()
	got := awaitRoster(t, behind.d, roster, "the evicted workspace off every merge arm", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, behind.ws.GetId())
		return row != nil && row.GetMergeQueued() == nil && row.GetMerging() == nil &&
			row.GetMergeConflict() == nil && row.GetMergeFailed() == nil && row.GetMerged() == nil
	})
	row := rosterRow(got, behind.ws.GetId())
	if row.GetMergeQueued() != nil || row.GetMerging() != nil || row.GetMergeConflict() != nil ||
		row.GetMergeFailed() != nil || row.GetMerged() != nil {
		t.Fatalf("evicted workspace roster status = %v, want no merge arm standing", row)
	}
}

func TestADequeuedQueuedMergeEndsAsFeedMergeAbandonedWithTheFooterAndRosterLeavingMerging(t *testing.T) {
	t.Parallel()
	// Arrange: the user's own answer to the interrupt offer, DISTINCT from an
	// operator's evict, though the proto cannot tell the two apart (see the
	// section comment above).
	_, behind, _, _ := mergeBlockedQueueFixture(t)
	// The sweep covers every test; the declared records are evidence of the merge conflict the test stages, the queued merge the test abandons.
	behind.d.ExpectWarnings("daemon.merge.conflicts", "daemon.gitclient.merge_no_ff",
		"daemon.merge.drop_queued", "daemon.merge.merge_tab")
	holds := behind.d.WatchHolds(behind.ws)
	if _, err := behind.d.Client().Interrupt(behind.d.Ctx(), connect.NewRequest(&agentreplv1.InterruptRequest{
		Workspace: behind.ws, Target: &agentreplv1.InterruptRequest_Turn{Turn: &agentreplv1.InterruptTurn{}},
	})); err != nil {
		t.Fatalf("Interrupt(turn) = error %v, want a success", err)
	}
	awaitView(t, behind, holds, "the merge-dequeue held offer", func(tray *frontendv1.DaemonHoldTray) bool {
		return mergeDequeueOffer(tray) != nil
	})
	root := behind.watchRootFeed()
	footer := behind.d.WatchFooter(behind.ws)

	// Act
	resp, err := behind.d.Client().AnswerHeldOffer(behind.d.Ctx(), connect.NewRequest(&agentreplv1.AnswerHeldOfferRequest{
		Workspace: behind.ws,
		Answer: &agentreplv1.AnswerHeldOfferRequest_MergeDequeue{MergeDequeue: &agentreplv1.AnswerHeldOfferMergeDequeue{
			Decision: &agentreplv1.AnswerHeldOfferMergeDequeue_Release{Release: &agentreplv1.AnswerHeldOfferRelease{}},
		}},
	}))
	if err != nil || resp.Msg.GetError() != nil {
		t.Fatalf("AnswerHeldOffer(release) = %v, %v, want a success", resp.Msg, err)
	}

	// Assert: the SAME FeedMergeError.abandoned arm as an evict -- the proto
	// has no way to tell "the user released the queue slot" apart from "the
	// operator evicted it" or a self give-up.
	mergeRow := awaitRow(t, behind, root, "the dequeued merge's abandoned terminal", func(row *frontendv1.FeedRow) bool {
		return row.GetActivity().GetMerge().GetError() != nil
	})
	if mergeRow.GetActivity().GetMerge().GetError().GetAbandoned() == nil {
		t.Fatalf("dequeued merge terminal = %v, want FeedMergeError.abandoned", mergeRow.GetActivity().GetMerge().GetError())
	}

	fv := awaitFooter(t, behind, footer, "the footer leaving merging after the dequeue", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetMerging() == nil
	})
	if fv.GetStrip().GetStatus().GetMerging() != nil {
		t.Fatalf("footer status = %v, want merging cleared after the dequeue", fv.GetStrip().GetStatus())
	}

	roster := behind.d.WatchRoster()
	got := awaitRoster(t, behind.d, roster, "the dequeued workspace off every merge arm", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, behind.ws.GetId())
		return row != nil && row.GetMergeQueued() == nil && row.GetMerging() == nil &&
			row.GetMergeConflict() == nil && row.GetMergeFailed() == nil && row.GetMerged() == nil
	})
	row := rosterRow(got, behind.ws.GetId())
	if row.GetMergeQueued() != nil || row.GetMerging() != nil || row.GetMergeConflict() != nil ||
		row.GetMergeFailed() != nil || row.GetMerged() != nil {
		t.Fatalf("dequeued workspace roster status = %v, want no merge arm standing", row)
	}
}

// TestKillingAWorkspaceAbandonsItsQueuedMergeWithTheCloseAsTheCause is the
// third end, reachable at last. CloseWorkspace refuses outright while a merge
// is queued, so KillWorkspace is the one door such a workspace leaves through,
// and the abandoned terminal it draws is the ONLY place the wire can say why:
// FeedMergeError has one `abandoned` arm for every end, so the cause lives in
// FeedMergeAbandoned.summary and nowhere else.
func TestKillingAWorkspaceAbandonsItsQueuedMergeWithTheCloseAsTheCause(t *testing.T) {
	t.Parallel()
	// Arrange
	_, behind, _, _ := mergeBlockedQueueFixture(t)
	// The sweep covers every test; the declared records are evidence of the merge conflict the test stages, the queued merge the kill abandons, a KillSession the fake shim answers by exiting, a session fault the kill opens, the shim death and severed link the kill drives.
	//
	// daemon.shimclient.redial belongs to that same kill: the fake answers
	// KillSession by EXITING, so its socket can break before the reaper has
	// decided the death, and the monitor then reports the break and its one
	// failed redial exactly as it should. Which side of that race a run lands
	// on is a matter of scheduling, so the record is declared here for the
	// same reason the sibling teardown tests declare it, not because it is
	// unimportant.
	behind.d.ExpectWarnings("daemon.merge.conflicts", "daemon.gitclient.merge_no_ff",
		"daemon.merge.drop_queued", "daemon.merge.merge_tab", "daemon.health.open_fault",
		"daemon.shimclient.exit", "daemon.shimclient.kill_session", "daemon.sessionwatcher.link_fault",
		"daemon.shimclient.redial",
		"daemon.workspace.kill", "daemon.sessionwatcher.watch_session", "daemon.sessionwatcher.watch_agent")
	root := behind.watchRootFeed()

	// Act
	if _, err := behind.d.Client().KillWorkspace(behind.d.Ctx(), connect.NewRequest(&agentreplv1.KillWorkspaceRequest{
		Workspace: behind.ws,
	})); err != nil {
		t.Fatalf("KillWorkspace = error %v, want a success", err)
	}

	// Assert
	mergeRow := awaitRow(t, behind, root, "the killed workspace's abandoned merge terminal", func(row *frontendv1.FeedRow) bool {
		return row.GetActivity().GetMerge().GetError().GetAbandoned() != nil
	})
	abandoned := mergeRow.GetActivity().GetMerge().GetError().GetAbandoned()
	want := "the workspace was closed while this merge was waiting in the queue"
	if abandoned.GetSummary() != want {
		t.Fatalf("the killed workspace's merge summary = %q, want %q", abandoned.GetSummary(), want)
	}
}

// ---------------------------------------------------------------------------
// The Emacs-repo method: a conflicting branch opens the conflicts tab, briefs
// the agent once with the spliced brief, then parks.
// ---------------------------------------------------------------------------

func TestAConflictingBranchOpensTheConflictsTabAndPromptsWithTheSplicedBrief(t *testing.T) {
	t.Parallel()
	// Arrange
	repo := harness.NewRepo(t)
	d := harness.StartDaemon(t, harness.Opts{SelfRepo: repo.Dir})
	// The sweep covers every test; the declared records are evidence of the merge conflict the test stages.
	d.ExpectWarnings("daemon.gitclient.merge_no_ff", "daemon.merge.conflicts",
		"daemon.merge.merge_tab")
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
	pushConcludedTurn(f.shim, mainAgent, "conflict-brief-done")

	// Assert: footer, host composer.
	footer := f.d.WatchFooter(f.ws)
	fv := awaitFooter(t, f, footer, "the footer's parked substatus", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetMergeConflict().GetParked() != nil
	})
	if fv.GetStrip().GetStatus().GetMergeConflict().GetParked().GetLine() == "" {
		t.Fatalf("footer parked = %v, want a composed line", fv.GetStrip().GetStatus().GetMergeConflict().GetParked())
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
	t.Parallel()
	// Arrange: park a merge on a scripted conflict.
	repo := harness.NewRepo(t)
	d := harness.StartDaemon(t, harness.Opts{SelfRepo: repo.Dir})
	// The sweep covers every test; the declared records are evidence of the merge conflict the test stages.
	d.ExpectWarnings("daemon.gitclient.merge_no_ff", "daemon.merge.conflicts",
		"daemon.merge.merge_tab")
	repoRef := mergeRepositoryRef(t, d, repo)
	f := mergeCreateChild(t, d, repoRef, "feature", "do the feature", nil)
	branch := mergeBranchOf(t, f.ws)
	repo.ScriptConflict(repo.Dir, branch, "conflict.txt")
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}
	f.shim.ExpectStartTurn()
	f.d.AwaitWorkspaceLogOperationCount(f.ws.GetDir(), harness.OpTurnOpened, 2)
	pushConcludedTurn(f.shim, mainAgent, "conflict-brief-done")
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
	t.Parallel()
	// Arrange
	f, d, _, script := mergeCleanRepo(t)
	script.SetExitCode(0)
	script.SetStdout("daemon: passed in 1s\n")

	// THE FEED IS SUBSCRIBED BEFORE THE MERGE IS ENQUEUED. A landing tears the
	// merged workspace's worktree down, and the feed resolves its log sink by
	// stat-ing that directory -- so a subscription opened after the enqueue
	// races the teardown and intermittently finds no such workspace to watch.
	// Opening first is the rendezvous the assertion actually needs.
	root := f.watchRootFeed()

	// Act
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}

	// Assert: the merge lands (no conflict, gate passes).
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
	t.Parallel()
	// Arrange
	f, d, repo, script := mergeCleanRepo(t)
	// The sweep covers every test; the declared records are evidence of the failing test gate the test stages, the fixes escalation the test stages.
	d.ExpectWarnings("daemon.merge.fixes", "daemon.merge.tests", "daemon.scriptrunner.run")
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
	pushConcludedTurn(f.shim, mainAgent, "fixes-brief-done")

	// Assert: the run parks.
	footer := f.d.WatchFooter(f.ws)
	fv := awaitFooter(t, f, footer, "the footer's parked substatus", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetMergeConflict().GetParked() != nil
	})
	if fv.GetStrip().GetStatus().GetMergeConflict().GetParked() == nil {
		t.Fatalf("footer = %v, want merging.parked after the fixes escalation", fv.GetStrip().GetStatus())
	}
}

func TestATestGateFailureIsNeverAutomaticallyRerun(t *testing.T) {
	t.Parallel()
	// Arrange
	f, d, _, script := mergeCleanRepo(t)
	// The sweep covers every test; the declared records are evidence of the failing test gate the test stages.
	d.ExpectWarnings("daemon.merge.tests", "daemon.scriptrunner.run")
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
// landing's one deploy.
// ---------------------------------------------------------------------------

func TestALandedMergeProducesSuccessFooterRosterAndRemovesTheWorktree(t *testing.T) {
	t.Parallel()
	// Arrange
	f, d, _, script := mergeCleanRepo(t)
	script.SetExitCode(0)
	script.SetStdout("daemon: passed in 1s\n")
	dir := f.ws.GetDir()
	// EVERY STREAM THIS TEST READS IS OPENED BEFORE THE MERGE IS ENQUEUED.
	// The feed's reason is below; the footer's and the roster's is that both
	// facts this test asserts are MOMENTS the landing passes through. The
	// footer's merged substatus is a momentary status the daemon's own
	// successor push retires after footer.DefaultMomentaryDwell, and the
	// merged roster row is closed out right behind it — so a watch opened
	// after the terminal feed row has already arrived is a watch that opens
	// on the state AFTER the one it is waiting for, and it then waits out its
	// whole bound for a push that has already happened.
	root := f.watchRootFeed()
	footer := f.d.WatchFooter(f.ws)
	roster := f.d.WatchRoster()

	// THE TEARDOWN'S ORDER IS NOT ASSERTED FROM HERE. Publishing the terminal
	// row and removing the worktree are both the daemon's, in that order, and
	// internal/merge's TestTerminalIsPublishedBeforeTheWorktreeIsRemoved holds
	// that ordering deterministically against the feed and git the run itself
	// drives. What a CLIENT sees is the row's arrival, which is a stream
	// delivery this test cannot order against a teardown that spawns git: a
	// poller racing the arrival measures how fast the push reached this
	// process, not what the daemon did first, and lost that race under load.

	// Act
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}

	// Assert: FeedMergeSuccess{commit}.
	mergeRow := awaitRow(t, f, root, "the merge's success", func(row *frontendv1.FeedRow) bool {
		return row.GetActivity().GetMerge().GetSuccess() != nil
	})
	if mergeRow.GetActivity().GetMerge().GetSuccess().GetCommit() == "" {
		t.Fatalf("FeedMergeSuccess = %v, want a landed commit", mergeRow.GetActivity().GetMerge().GetSuccess())
	}

	// Assert: footer merged, roster merged + recently_merged.
	awaitFooter(t, f, footer, "the footer's merged status", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetMerged() != nil
	})
	// THE STATUS IS PART OF THE PREDICATE, NOT A SECOND READ OF WHATEVER
	// SNAPSHOT THE FIRST ONE MATCHED. The row enters recently_merged as
	// MERGED and is closed out a moment later, so matching only on its
	// presence and then reading the status off that same view asserted
	// whichever of the two snapshots happened to arrive first — and read
	// `inactive` whenever the close had already landed.
	awaitRoster(t, f.d, roster, "the merged workspace under recently_merged, stated merged", func(r *frontendv1.WorkspaceRoster) bool {
		for _, row := range r.GetRecentlyMerged().GetRows().GetRows() {
			if row.GetWorkspace().GetWorkspace().GetId() == f.ws.GetId() {
				return row.GetMerged() != nil
			}
		}
		return false
	})

	// Assert: the worktree is removed once the terminal state has settled.
	d.AwaitFileGone(dir)
}

func TestLandingAMergeWhoseTargetIsTheSelfRepoDeploysOnce(t *testing.T) {
	t.Parallel()
	// Arrange: the deploy's build fails, which is the landing's loud answer.
	f, d, repo, script := mergeCleanRepo(t)
	d.StageDeployBuild(harness.DeployFails)
	script.SetExitCode(0)
	script.SetStdout("daemon: passed in 1s\n")
	// THE BRANCH HAS TO CARRY A COMMIT. The self-reload fires off what the
	// merge LANDED -- the classifier reads the changed paths -- so a merge
	// that brought in nothing triggers nothing, correctly.
	writeCommit(t, repo, f.ws.GetDir(), "modules/app/agent-repl/daemon/cmd/claude-repld/main.go", "landed\n")

	// THE FEED IS SUBSCRIBED BEFORE THE MERGE IS ENQUEUED. A landing tears the
	// merged workspace's worktree down, and the feed resolves its log sink by
	// stat-ing that directory -- so an open after the enqueue races the
	// teardown and intermittently finds no such workspace to watch.
	root := f.watchRootFeed()

	// Act
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}
	awaitRow(t, f, root, "the merge's success", func(row *frontendv1.FeedRow) bool {
		return row.GetActivity().GetMerge().GetSuccess() != nil
	})

	// Assert: the landing asked for ONE deploy, whose build the test fails,
	// so nothing was installed or restarted. The deploy runs off the merge, so
	// its global record is the synchronization point: the landing's own
	// workspace sink is reached through the worktree the teardown removed.
	d.ExpectWarnings("daemon.scriptrunner.run", "daemon.deploy.build", "daemon.deploy.run", "daemon.deploy.landing")
	d.AwaitLogRecord(d.RunLogPath(), "the landing's deploy failing its build", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.deploy.landing" && r.Message == "the landing's deploy failed"
	})
	invocations := d.Deploy.Invocations()
	if got := len(invocations); got != 1 {
		t.Fatalf("deploy builds = %d, want EXACTLY 1 for the one landing (never one per commit)", got)
	}
	if argv := invocations[0].Argv; len(argv) != 2 || argv[0] != "--out" {
		t.Fatalf("deploy build argv = %v, want --out <staging>", argv)
	}
	if got := len(d.Launchctl.Invocations()); got != 0 {
		t.Fatalf("launchctl invocations = %d, want none: a failed build restarts nothing", got)
	}
}

// ---------------------------------------------------------------------------
// Self-reload only for the self repo: internal/merge/run.go splits on TWO
// distinct booleans -- `emacsRepo` (SameRepo: same underlying repository,
// which is what selects between the Emacs method and the other-repo method)
// and `selfCheckout` (same && the literal directory IS this daemon's own
// checkout, "which is what SELECTS THE METHOD" per run.go's own comment on
// emacsRepo -- the self-reload's OWN gate additionally requires
// selfCheckout). The two tests below are the two ways of being "half right":
// the Emacs method running in the same repository but NOT the literal
// checkout (a sibling worktree), and the other-repo method entirely (a
// completely different repository). Neither half alone triggers the deploy.
// ---------------------------------------------------------------------------

func TestASiblingWorktreeOfTheSelfRepoRunsTheEmacsMethodButNeverTriggersTheDeploy(t *testing.T) {
	t.Parallel()
	// Arrange: a self-repo daemon, a top-level PARENT workspace (this
	// daemon's own checkout), and a CHILD nested under it whose merge lands
	// into the PARENT's worktree -- a sibling of the self checkout, same
	// underlying repository (emacsRepo=true: the Emacs method runs a real
	// `git merge`), but NOT the literal self-checkout directory
	// (selfCheckout=false: internal/merge/terminal.go's selfReload comment:
	// "A sibling worktree of the same repository is excluded").
	repo := harness.NewRepo(t)
	script := harness.NewTestAllScript(t, repo.Dir)
	script.SetExitCode(0)
	script.SetStdout("daemon: passed in 1s\n")
	d := harness.StartDaemon(t, harness.Opts{SelfRepo: repo.Dir, ExtraEnv: []string{"AGENT_REPL_TEST_ALL_SCRIPT=" + script.Path}})
	repoRef := mergeRepositoryRef(t, d, repo)
	parent := mergeCreateChild(t, d, repoRef, "sibling7a-parent", "the parent work", nil)

	childResp, err := d.Client().CreateWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Repository: repoRef,
		// The child carries an initial prompt: the arrangement below waits for
		// its StartTurn and terminates it, and a promptless create opens no
		// turn to wait for at all.
		Form: &agentreplv1.CreateWorkspaceRequest_Standard{Standard: &agentreplv1.CreateWorkspaceStandard{
			InitialPrompt: said("the child work"),
			Name:          strPtr("sibling7a-child"),
		}},
		Parent: &agentreplv1.CreateWorkspaceParent{Workspace: parent.ws},
	}))
	if err != nil || childResp.Msg.GetSuccess() == nil {
		t.Fatalf("CreateWorkspace(child) = (%v, %v), want a success", childResp, err)
	}
	child := childResp.Msg.GetSuccess().GetWorkspace()
	childShim := d.Shim(child)
	childShim.ExpectStartSession()
	childShim.ExpectStartTurn()
	pushConcludedTurn(childShim, mainAgent, "sibling7a-child-initial")
	// A commit that WOULD classify into this daemon's own subsystem, so
	// nothing but the literal-checkout gate is what keeps the deploy off.
	writeCommit(t, repo, child.GetDir(), "modules/app/agent-repl/daemon/cmd/claude-repld/main.go", "landed\n")

	// Act
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: child})); err != nil {
		t.Fatalf("MergeWorkspace(child) = error %v, want the merge enqueued", err)
	}
	roster := d.WatchRoster()
	awaitRoster(t, d, roster, "the child's merge landed", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, child.GetId())
		return row != nil && row.GetMerged() != nil
	})

	// Assert: the Emacs method DID run -- a real `git merge` targeted the
	// parent's worktree, never the daemon's own checkout directory.
	targetedParent := false
	for _, c := range d.Git.Calls() {
		if createArgsContain(c.Args, "merge") && createGitDir(c.Args) == parent.ws.GetDir() {
			targetedParent = true
		}
	}
	if !targetedParent {
		t.Fatalf("git calls = %v, want a merge run inside the parent's worktree %q", d.Git.Calls(), parent.ws.GetDir())
	}

	// Assert: the deploy never fires -- the target was a SIBLING worktree of
	// the self repo, not the daemon's own checkout.
	if got := len(d.Deploy.Invocations()); got != 0 {
		t.Fatalf("deploy builds = %d, want 0: a sibling worktree of the self repo is not the self checkout", got)
	}
}

func TestAOneShotMergeOnANonSelfRepoNeverTriggersTheDeploy(t *testing.T) {
	t.Parallel()
	// Arrange: a daemon whose self repo is a DISTINCT repository from the
	// one-shot's own -- the other-repo method entirely (emacsRepo=false),
	// the opposite half of the split from the sibling-worktree test above.
	selfRepo := harness.NewRepo(t) // distinct identity; never the merge target.
	repo := harness.NewRepo(t)
	d := harness.StartDaemon(t, harness.Opts{SelfRepo: selfRepo.Dir})
	repository := createRepositoryRef(t, d, repo)
	resp, err := d.Client().CreateWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Repository: repository,
		Form: &agentreplv1.CreateWorkspaceRequest_OneShot{OneShot: &agentreplv1.CreateWorkspaceOneShot{
			Prompt: said("ship the fix"),
		}},
	}))
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("CreateWorkspace(one_shot) = (%v, %v), want a success", resp, err)
	}
	ws := resp.Msg.GetSuccess().GetWorkspace()
	shim := d.Shim(ws)
	shim.ExpectStartSession()
	shim.ExpectStartTurn()
	roster := d.WatchRoster()

	// Act: the turn concludes, and the merge is asked for the way the agent
	// carrying the completion directive asks for it — the ordinary merge verb.
	shim.PushAgentFrame(mainAgent, successFrame(mainAgent, nil))
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: ws})); err != nil {
		t.Fatalf("MergeWorkspace(one_shot) = error %v, want the merge enqueued", err)
	}

	// Assert: the merge lands (the other-repo method has nothing between the
	// two configured prompts, so a clean run with neither reaches "merged"
	// straight away).
	awaitRoster(t, d, roster, "the one-shot's merge landed", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, ws.GetId())
		return row != nil && row.GetMerged() != nil
	})

	// Assert: the deploy never fires -- the merge target is not this
	// daemon's self repo at all.
	if got := len(d.Deploy.Invocations()); got != 0 {
		t.Fatalf("deploy builds = %d, want 0: the merge target is not this daemon's self repo", got)
	}
}

// ---------------------------------------------------------------------------
// The non-Emacs-repo method: only pre/post prompts, never the Emacs-only
// merge/conflicts/tests/fixes tabs.
// ---------------------------------------------------------------------------

func TestTheNonEmacsRepoMethodNeverDrawsTheEmacsOnlyTabs(t *testing.T) {
	t.Parallel()
	// Arrange: a repo that is NOT the daemon's self repo.
	selfRepo := harness.NewRepo(t) // distinct identity; never used as a target.
	repo := harness.NewRepo(t)
	d := harness.StartDaemon(t, harness.Opts{SelfRepo: selfRepo.Dir})
	repoRef := mergeRepositoryRef(t, d, repo)
	f := mergeCreateChild(t, d, repoRef, "feature", "do the feature", nil)

	// THE FEED IS SUBSCRIBED BEFORE THE MERGE IS ENQUEUED. A landing tears the
	// merged workspace's worktree down, and the feed resolves its log sink by
	// stat-ing that directory -- so a subscription opened after the enqueue
	// races the teardown and intermittently finds no such workspace to watch.
	// Opening first is the rendezvous the assertion actually needs.
	root := f.watchRootFeed()

	// Act
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}

	// Assert: the merge bubble reaches a terminal state.
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
	t.Parallel()
	// Arrange: park a merge on a scripted conflict, then crash the daemon.
	repo := harness.NewRepo(t)
	d := harness.StartDaemon(t, harness.Opts{SelfRepo: repo.Dir})
	// THE CONFLICT IS THE ARRANGEMENT, and the merge says so on the child
	// workspace's own sink. Those two records are the parked merge this test
	// then crashes the daemon across.
	d.ExpectWarnings("daemon.merge.merge_tab", "daemon.merge.conflicts", "daemon.gitclient.merge_no_ff")
	repoRef := mergeRepositoryRef(t, d, repo)
	f := mergeCreateChild(t, d, repoRef, "feature", "do the feature", nil)
	branch := mergeBranchOf(t, f.ws)
	repo.ScriptConflict(repo.Dir, branch, "conflict.txt")
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}
	f.shim.ExpectStartTurn()
	f.d.AwaitWorkspaceLogOperationCount(f.ws.GetDir(), harness.OpTurnOpened, 2)
	pushConcludedTurn(f.shim, mainAgent, "conflict-brief-done")
	host := f.d.WatchHost(f.ws)
	awaitView(t, f, host, "the host composer parked on the merge", func(r *agentreplv1.WatchHostWorkspaceResponse) bool {
		return r.GetHost().GetExisting().GetLive().GetMergeParked() != nil
	})
	d.Kill()

	// Act: a fresh daemon on the same state root.
	d2 := harness.StartDaemon(t, harness.Opts{StateDir: d.StateDir, SelfRepo: repo.Dir})
	// The sweep covers every test; the declared records are evidence of the unfinished merge a restart leaves.
	d2.ExpectWarnings("daemon.merge.recover")
	// A shim now genuinely SURVIVES this bounce: the successor probes the same
	// kernel-lock directory its predecessor named, so the surviving shim's
	// workspace lock reads held and the session is ADOPTED rather than
	// respawned. A bounce that wrote no intent manifest therefore has a live
	// session to account for, which the rollout reconciler states as a fault
	// by design.
	d2.ExpectWarnings("daemon.rollout.reconcile")
	// The recovered merge re-reaches the SAME scripted conflict, on this
	// daemon's own pid; the records are the recovery working, not a fault.
	d2.ExpectWarnings("daemon.merge.merge_tab", "daemon.merge.conflicts", "daemon.gitclient.merge_no_ff")
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
	t.Parallel()
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

	// THE FEED IS SUBSCRIBED BEFORE THE MERGE IS ENQUEUED. A landing tears the
	// merged workspace's worktree down, and the feed resolves its log sink by
	// stat-ing that directory -- so a subscription opened after the enqueue
	// races the teardown and intermittently finds no such workspace to watch.
	// Opening first is the rendezvous the assertion actually needs.
	root := f.watchRootFeed()

	// Act
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}

	// Assert: the run fails loudly rather than silently parking or hanging.
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
	t.Parallel()
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
	pushConcludedTurn(f.shim, mainAgent, "pre-prompt-done")

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
	t.Parallel()
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

	// THE FEED IS SUBSCRIBED BEFORE THE MERGE IS ENQUEUED. A landing tears the
	// merged workspace's worktree down, and the feed resolves its log sink by
	// stat-ing that directory -- so a subscription opened after the enqueue
	// races the teardown and intermittently finds no such workspace to watch.
	// Opening first is the rendezvous the assertion actually needs.
	root := f.watchRootFeed()

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
// SECOND FINDING, 2026-09-02: the missing hook is NOT the only thing standing
// in the way, and it is not the deeper one. THE BOUNCE-CROSSING RESUBMISSION
// HAS NO PRODUCER. `wsm.Turn.Displaced` is WRITTEN by
// internal/workspace/fleet_rollout.go's CaptureDisplaced and is read back by
// NOTHING: grep the tree and the only non-test readers of the column are
// wsm/turns.go's own scan and insert. internal/merge/recover.go re-enqueues an
// interrupted merge and runs it again, but the second run's CaptureDisplaced
// finds nothing in flight (the first run already KILLED the turn), so its
// `r.displaced` is nil and terminal.go's resubmitDisplaced returns
// immediately. A turn displaced by a merge that then crashes is marked
// displaced in the database forever and is never put back.
//
// So a pause hook alone would only make the gap OBSERVABLE. Un-skipping this
// test needs a PRODUCTION behavior first: a boot-time recovery that finds the
// turns still marked displaced on a workspace whose merge lease did not
// survive, resubmits each exactly once with
// PROMPT_ORIGIN_MERGE_DISPLACED_TURN_RESUME, and clears the mark in the same
// step so a second boot cannot double it. That is a behavior question for the
// project lead, not something a test hook can paper over.
//
// BOTH BLOCKERS ARE GONE (2026-09-02). The behavior exists:
// internal/merge/recover.go's recoverDisplaced sweeps every turn still marked
// displaced at boot and puts it back exactly once, claiming each record
// through wsm's conditional ClaimDisplacedTurn so the merge's own release and
// the sweep can never both submit one turn. The hook exists too:
// merge.Deps.PauseAfterCapture, nil in production, wired only from
// AGENT_REPL_MERGE_PAUSE_AFTER_CAPTURE, holds a run in exactly the window a
// crash has to land in.
func TestADisplacedUserTurnIsResubmittedExactlyOnceAcrossADaemonBounce(t *testing.T) {
	t.Parallel()
	// Arrange / Act: displace a turn, crash inside the capture window, and
	// bring a fresh daemon up on the same state root.
	f, _, resubmit := displacedTurnAcrossABounce(t)

	// Assert: the turn came back with the user's own words, under the resume
	// origin, on a turn id of its own.
	if got := text(resubmit.GetSaid()); got != displacedBounceText {
		t.Fatalf("the resubmitted turn's StartTurn.said = %q, want the displaced turn's own text %q", got, displacedBounceText)
	}
	if resubmit.GetOrigin() != mergeDisplacedResumeOrigin {
		t.Fatalf("the resubmitted turn's StartTurn.origin = %v, want PROMPT_ORIGIN_MERGE_DISPLACED_TURN_RESUME (%d)", resubmit.GetOrigin(), mergeDisplacedResumeOrigin)
	}
	// Assert: the mark is DOWN. A record still marked is one the next boot
	// would put back a second time.
	if n := f.d.DisplacedTurnCount(); n != 0 {
		t.Fatalf("turns still marked displaced after the recovery = %d, want none", n)
	}
}

// TestASecondDaemonBounceDoesNotResubmitTheDisplacedTurnAgain covers the
// exactly-once edge across TWO bounces: the claim that put the turn back is
// durable, so the boot after it finds nothing owed and submits nothing.
func TestASecondDaemonBounceDoesNotResubmitTheDisplacedTurnAgain(t *testing.T) {
	t.Parallel()
	// Arrange: the turn already recovered by the first bounce.
	f, d2, _ := displacedTurnAcrossABounce(t)
	afterFirstBounce := f.shim.Count(harness.RPCStartTurn)
	d2.Kill()

	// Act: a second fresh daemon on the same state root.
	d3 := harness.StartDaemon(t, harness.Opts{StateDir: d2.StateDir,
		ExtraEnv: []string{"AGENT_REPL_LOCK_DIR=" + d2.LockDir}})
	displacedBounceWarnings(d3)
	// The recovery's own record is the rendezvous: it is logged AFTER the
	// displaced sweep ran, so the count below is settled rather than probed.
	d3.AwaitRunLogOperation("daemon.merge.recover")

	// Assert.
	if got := f.shim.Count(harness.RPCStartTurn); got != afterFirstBounce {
		t.Fatalf("StartTurns after a second boot = %d, want %d (the displaced turn is put back once, ever)", got, afterFirstBounce)
	}
}

// displacedBounceWarnings declares the records every daemon in this scenario
// legitimately writes: see the call site in displacedTurnAcrossABounce.
func displacedBounceWarnings(d *harness.Daemon) {
	d.ExpectWarnings("daemon.rollout.reconcile", "daemon.merge.recover", "daemon.promptqueue.restore_holds")
}

// displacedBounceText is the user's own words, asserted end to end.
const displacedBounceText = "keep going"

// displacedTurnAcrossABounce stages the whole scenario: a user turn in flight
// when a merge admits, a daemon killed inside the window between the capture
// and everything that would close it, and a fresh daemon on the same state
// root. It answers the fixture (rebound to the new daemon), that daemon, and
// the resubmission's own StartTurn.
//
// THE MERGE MUST NOT COME BACK. An unclean merge target is not resumable, so
// the restart FAILS the interrupted merge instead of running it again — which
// is what leaves the displaced record to the boot sweep and keeps this test's
// StartTurn trace free of a second run's traffic.
func displacedTurnAcrossABounce(t *testing.T) (*fixture, *harness.Daemon, *shimv1.StartTurnRequest) {
	t.Helper()
	repo := harness.NewRepo(t)
	script := harness.NewTestAllScript(t, repo.Dir)
	script.SetExitCode(0)
	script.SetStdout("daemon: passed in 1s\n")
	rendezvous := filepath.Join(t.TempDir(), "capture.rendezvous")
	d := harness.StartDaemon(t, harness.Opts{SelfRepo: repo.Dir, ExtraEnv: []string{
		"AGENT_REPL_TEST_ALL_SCRIPT=" + script.Path,
		"AGENT_REPL_MERGE_PAUSE_AFTER_CAPTURE=" + rendezvous,
	}})
	// The sweep covers every test; these declared records are evidence of the
	// crash this test stages. A daemon killed mid-merge writes no stand-down
	// manifest (daemon.rollout.reconcile), the restart refuses to resume the
	// merge into an unclean target (daemon.merge.recover), and a turn left in
	// flight by a killed daemon is closed by the next boot
	// (daemon.promptqueue.restore_holds) -- which is what the RESUBMITTED turn
	// becomes when this test kills the daemon that received it.
	displacedBounceWarnings(d)
	repoRef := mergeRepositoryRef(t, d, repo)
	f := mergeCreateChild(t, d, repoRef, "displaced", "do the clean thing", nil)
	f.submit(displacedBounceText, "k-displace-bounce", origin)
	// No terminal frame is ever pushed for it: it is the workspace's in-flight
	// turn when the merge admits, which is what CaptureDisplaced reads.
	f.shim.ExpectStartTurn()
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}
	d.AwaitFileExists(rendezvous)
	repo.SetDirty(repo.Dir, true)
	d.Kill()

	d2 := harness.StartDaemon(t, harness.Opts{StateDir: d.StateDir, SelfRepo: repo.Dir, ExtraEnv: []string{
		"AGENT_REPL_TEST_ALL_SCRIPT=" + script.Path,
		"AGENT_REPL_LOCK_DIR=" + d.LockDir,
	}})
	displacedBounceWarnings(d2)
	f.d = d2
	// ExpectStartTurn BLOCKS for the next request, so this is the
	// resubmission's own arrival rather than a race against the boot.
	return f, d2, f.shim.ExpectStartTurn()
}

// ---------------------------------------------------------------------------
// Conflicts: briefed exactly once per conflict commit; parked guidance lands
// on the conflicts tab, never as a root-feed row.
// ---------------------------------------------------------------------------

func TestAConflictedMergeBriefsTheAgentExactlyOnceEvenAfterItParks(t *testing.T) {
	t.Parallel()
	// Arrange / Act: park a merge on a scripted conflict, exactly as the
	// spliced-brief test does.
	repo := harness.NewRepo(t)
	d := harness.StartDaemon(t, harness.Opts{SelfRepo: repo.Dir})
	// The sweep covers every test; the declared records are evidence of the merge conflict the test stages.
	d.ExpectWarnings("daemon.gitclient.merge_no_ff", "daemon.merge.conflicts",
		"daemon.merge.merge_tab")
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
	pushConcludedTurn(f.shim, mainAgent, "conflict-brief-done")
	footer := f.d.WatchFooter(f.ws)
	awaitFooter(t, f, footer, "the footer's parked substatus", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetMergeConflict().GetParked() != nil
	})

	// Assert: with the run settled on park, still exactly one StartTurn was
	// ever sent for this conflict commit — the ONE brief, never a repeat.
	if got := f.shim.Count(harness.RPCStartTurn) - turnsBeforeTheMerge; got != 1 {
		t.Fatalf("StartTurns since the merge began = %d, want exactly 1 (the conflict is briefed once, then parks)", got)
	}
}

func TestParkedGuidanceLandsAsAUserPromptRowOnTheConflictsTabNeverOnTheRootFeed(t *testing.T) {
	t.Parallel()
	// Arrange: park a merge on a scripted conflict.
	repo := harness.NewRepo(t)
	d := harness.StartDaemon(t, harness.Opts{SelfRepo: repo.Dir})
	// The sweep covers every test; the declared records are evidence of the merge conflict the test stages.
	d.ExpectWarnings("daemon.gitclient.merge_no_ff", "daemon.merge.conflicts",
		"daemon.merge.merge_tab")
	repoRef := mergeRepositoryRef(t, d, repo)
	f := mergeCreateChild(t, d, repoRef, "guidancetab", "do the feature", nil)
	branch := mergeBranchOf(t, f.ws)
	repo.ScriptConflict(repo.Dir, branch, "conflict.txt")
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}
	f.shim.ExpectStartTurn()
	f.d.AwaitWorkspaceLogOperationCount(f.ws.GetDir(), harness.OpTurnOpened, 2)
	pushConcludedTurn(f.shim, mainAgent, "conflict-brief-done")
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
	t.Parallel()
	// Arrange: two independent repositories, each with a blocked queue, on
	// ONE daemon.
	d := harness.StartDaemon(t, harness.Opts{})
	// The sweep covers every test; the declared records are evidence of a refusal the test provokes.
	d.ExpectWarnings("daemon.merge.pause")
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
	t.Parallel()
	// Arrange: a daemon that has never heard of this repository ref at all.
	d := harness.StartDaemon(t, harness.Opts{})
	// The sweep covers every test; the declared records are evidence of a refusal the test provokes.
	d.ExpectWarnings("daemon.merge.pause")
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
	t.Parallel()
	// Arrange: a landed commit touching ONLY the sandbox image under e2e/,
	// whose blast radius (internal/merge/suiteselect.go's own rule table) is
	// exactly the single "e2e-emacs" suite. daemon/ paths no longer make a
	// single-suite example: the rule table now widens a daemon change to
	// {daemon, e2e, e2e-emacs} (the cross-system suites run a real
	// claude-repld too), so a daemon-only test needs a genuinely narrow path
	// to stay a one-suite demonstration.
	f, d, repo, script := mergeCleanRepo(t)
	script.SetExitCode(0)
	script.SetStdout("e2e-emacs: passed in 1s\n")
	writeCommit(t, repo, f.ws.GetDir(), "modules/app/agent-repl/e2e/sandbox/blastradius.txt", "touched\n")

	// THE FEED IS SUBSCRIBED BEFORE THE MERGE IS ENQUEUED. A landing tears the
	// merged workspace's worktree down, and the feed resolves its log sink by
	// stat-ing that directory -- so a subscription opened after the enqueue
	// races the teardown and intermittently finds no such workspace to watch.
	// Opening first is the rendezvous the assertion actually needs.
	root := f.watchRootFeed()

	// Act
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}
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
	if !sameOrder(argv, []string{"--suites", "e2e-emacs"}) {
		t.Fatalf("test-all.sh argv = %v, want it to end with [--suites e2e-emacs] (the sandbox-only blast radius)", argv)
	}
}

func TestTheTestsTabPaintsANSISpansFromTheScriptedOutput(t *testing.T) {
	t.Parallel()
	// Arrange: a daemon change, with a green ANSI escape in the scripted
	// run's own output. Which suites the change's blast radius selects is
	// incidental to what this test asserts (ANSI parsing, not suite
	// selection) — internal/merge/suiteselect.go's rule table widening a
	// daemon path to more than one suite must not break it.
	f, d, repo, script := mergeCleanRepo(t)
	script.SetExitCode(0)
	script.SetStdout("\x1b[32mall green\x1b[0m\ndaemon: passed in 1s\n")
	writeCommit(t, repo, f.ws.GetDir(), "modules/app/agent-repl/daemon/internal/merge/paint.go", "touched\n")

	// THE FEED IS SUBSCRIBED BEFORE THE MERGE IS ENQUEUED. A landing tears the
	// merged workspace's worktree down, and the feed resolves its log sink by
	// stat-ing that directory -- so a subscription opened after the enqueue
	// races the teardown and intermittently finds no such workspace to watch.
	// Opening first is the rendezvous the assertion actually needs.
	root := f.watchRootFeed()

	// Act
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}
	mergeRow := awaitRow(t, f, root, "the merge's success", func(row *frontendv1.FeedRow) bool {
		return row.GetActivity().GetMerge().GetSuccess() != nil
	})

	// Assert: the settled tests tab carries the scripted run's output as
	// DAEMON-PARSED paint spans — the client never sees the raw escape. This
	// searches every selected suite's output rather than assuming a single
	// suite named "daemon": how many suites the blast radius selects is not
	// this test's concern, only that the ANSI escape was parsed into a span
	// wherever it landed, and that no raw escape reached the wire on any of
	// them.
	testsRow := f.awaitRowInFeed(mergeRow.GetId(), "the settled tests tab", func(row *frontendv1.FeedRow) bool {
		return row.GetMergeTab().GetTests().GetSettled() != nil
	})
	suites := testsRow.GetMergeTab().GetTests().GetSuites()
	var found bool
	for _, suite := range suites {
		for _, span := range suite.GetOutput() {
			if span.GetPaintClass() == "ansi-fg-green" && strings.Contains(span.GetText(), "all green") {
				found = true
			}
			if strings.ContainsAny(span.GetText(), "\x1b") {
				t.Fatalf("a raw ANSI escape reached the wire: span = %+v, want the daemon to have parsed it", span)
			}
		}
	}
	if !found {
		t.Fatalf("tests tab suites = %v, want some suite's output to carry a span {text: contains %q, paint_class: ansi-fg-green}", suites, "all green")
	}
}

// ---------------------------------------------------------------------------
// The merge tab's narration is asserted by content.
// ---------------------------------------------------------------------------

func TestTheMergeTabNarratesTheNoFFLandingByContent(t *testing.T) {
	t.Parallel()
	// Arrange
	f, d, _, script := mergeCleanRepo(t)
	script.SetExitCode(0)
	script.SetStdout("daemon: passed in 1s\n")

	// THE FEED IS SUBSCRIBED BEFORE THE MERGE IS ENQUEUED. A landing tears the
	// merged workspace's worktree down, and the feed resolves its log sink by
	// stat-ing that directory -- so a subscription opened after the enqueue
	// races the teardown and intermittently finds no such workspace to watch.
	// Opening first is the rendezvous the assertion actually needs.
	root := f.watchRootFeed()

	// Act
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}
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
	t.Parallel()
	// Arrange
	f, d, _, script := mergeCleanRepo(t)
	script.SetExitCode(0)
	script.SetStdout("daemon: passed in 1s\n")
	// THE FEED IS SUBSCRIBED BEFORE THE MERGE IS ENQUEUED. A landing tears the
	// merged workspace's worktree down, and the feed resolves its log sink by
	// stat-ing that directory -- so a subscription opened after the enqueue
	// races the teardown and intermittently finds no such workspace to watch.
	// Opening first is the rendezvous the assertion actually needs.
	root := f.watchRootFeed()

	// Act
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}
	awaitRow(t, f, root, "the merge's success", func(row *frontendv1.FeedRow) bool {
		return row.GetActivity().GetMerge().GetSuccess() != nil
	})
	// THE TERMINAL ROW IS NOT THE END OF THE MERGE. It is published partway
	// through `finish`, ahead of the landing's durable stamps and the whole of
	// the teardown, all of which run on the admission pump's goroutine and all
	// of which write to the state database. Stopping the daemon there yanks the
	// store out from under work still in flight, which is a shutdown artifact
	// of the test's own making -- not a fault the daemon owes a warning for.
	// The pump's idle record is the merge's real end: it is written after the
	// teardown returned, and `admitted` distinguishes the burst that ran this
	// merge from a boot-time pump that admitted nothing.
	idle := d.AwaitLogRecord(d.RunLogPath(), "the admission pump's idle record", func(r harness.LogRecord) bool {
		admitted, ok := r.Context["admitted"].(float64)
		return r.Operation == "daemon.merge.pump" && ok && admitted >= 1
	})
	if got := idle.Context["admitted"]; got != float64(1) {
		t.Fatalf("the admission pump's idle record reports admitted = %v, want 1", got)
	}
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
	shim := d.Shim(ws)
	shim.ExpectStartSession()
	shim.ExpectStartTurn()
	// The fake records StartTurn on ARRIVAL; the terminal frame must not be
	// pushed until the daemon has the turn OPEN, or the terminal names no turn
	// and everything waiting on that turn's end waits forever.
	d.AwaitWorkspaceLogOperationCount(ws.GetDir(), harness.OpTurnOpened, 1)
	pushConcludedTurn(shim, mainAgent, name+"-initial")
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

// TestAMergeOverATurnWithDetachedWorkEndsOnlyTheTurnAndWaitsForTheWork covers
// the rule that an interrupt ends only the synchronous turn, at the merge's
// admission. The displaced turn has a background subagent running: the merge
// ends the turn with an UNFORCED KillTurn, stops nothing detached, and waits
// for the workspace to fall free before it drives the session. Only once the
// subagent settles does the merge proceed, land, and put the displaced turn
// back exactly once.
func TestAMergeOverATurnWithDetachedWorkEndsOnlyTheTurnAndWaitsForTheWork(t *testing.T) {
	t.Parallel()
	// Arrange: a clean self-repo merge target whose running turn spawned a
	// background subagent that is still live.
	f, d, _, script := mergeCleanRepo(t)
	script.SetExitCode(0)
	script.SetStdout("daemon: passed in 1s\n")
	workspaceLog := harness.WorkspaceLogPath(f.ws.GetDir(), "daemon")
	f.submit("keep going", "k-displace-detached", origin)
	f.shim.ExpectStartTurn()
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, ftSubagentSpawn("toolu-1", "toolu-1", "sweep the tree")))
	f.shim.PushAgentFrame(mainAgent, detachedWorkFrame(mainAgent, movedSubagent("toolu-1")))
	d.AwaitLogRecord(workspaceLog, "the live subagent", func(r harness.LogRecord) bool {
		agents, ok := r.Context["agents"].(float64)
		return r.Operation == "daemon.sessionwatcher.live_work" && ok && agents == 1
	})
	beforeMerge := f.shim.Count(harness.RPCStartTurn)

	// Act: the merge admits over the running turn, and is seen waiting on the
	// live subagent before the subagent settles.
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}
	killReq := f.shim.ExpectKillTurn()
	d.AwaitLogRecord(workspaceLog, "the merge waiting for the workspace to fall free", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.merge.await_free" && strings.HasPrefix(r.Message, "the merge waits")
	})
	stopsWhileWaiting := f.shim.Count(harness.RPCStopBash) + f.shim.Count(harness.RPCUpdateAgent)
	startsWhileWaiting := f.shim.Count(harness.RPCStartTurn)
	f.shim.PushAgentFrame("toolu-1", activityFrame("toolu-1", ftSubagentSettled("sub-unit-9", "toolu-1")))
	resubmit, turns := f.shim.ExpectStartTurnWithCount()

	// Assert
	if killReq.GetForce() {
		t.Fatal("the displaced turn's KillTurn was forced, want an unforced kill that spares its detached work")
	}
	if stopsWhileWaiting != 0 {
		t.Fatalf("the merge issued %d stop(s) to detached work, want none", stopsWhileWaiting)
	}
	if startsWhileWaiting != beforeMerge {
		t.Fatalf("StartTurns while the merge waited = %d, want %d: nothing drives the session until it falls free", startsWhileWaiting, beforeMerge)
	}
	if turns != beforeMerge+1 {
		t.Fatalf("StartTurn count once the resubmission arrived = %d, want exactly %d", turns, beforeMerge+1)
	}
	if got := text(resubmit.GetSaid()); got != "keep going" {
		t.Fatalf("the resubmitted turn's StartTurn.said = %q, want the displaced turn's own text %q", got, "keep going")
	}
	if resubmit.GetOrigin() != mergeDisplacedResumeOrigin {
		t.Fatalf("the resubmitted turn's StartTurn.origin = %v, want PROMPT_ORIGIN_MERGE_DISPLACED_TURN_RESUME", resubmit.GetOrigin())
	}
}
