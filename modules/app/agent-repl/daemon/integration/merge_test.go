//go:build integration

package integration

import (
	"agentrepl/protohelpers/rosterwalk"
	"database/sql"
	"path/filepath"
	"strings"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	shimv1 "agentrepl/proto/shim/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/integration/harness"

	"connectrpc.com/connect"
)

// merge_test.go exercises FinishWorkspace, UpdateMergeQueue, AnswerMergeQueueOffer
// and the deploy queue's visible effects (feed sub-feed, footer, roster, host
// composer) against repositories whose `.agent-repl/deploy.yaml` the harness
// writes (Repo.DeploysAsAgentRepl for agent-repl's own), with a FAKE bin/test-all.sh
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
	harness.CommitWork(t, f.ws.GetDir())
	resp, err := f.d.Client().FinishWorkspace(f.d.Ctx(), connect.NewRequest(harness.FinishRequest(f.ws)))

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

// mergeBlockedQueueFixture builds a self-repo with three child workspaces
// targeting it: the first is admitted to the front and the second races it
// from right behind (internal/merge/race.go), each stopped in a configured
// before-merge prompt nobody answers, so their runs HOLD the queue's window
// and the third workspace stays genuinely, observably QUEUED for the test's
// duration. (A PARKED front no longer pins the queue: a parked merge yields
// its slot, owner ruling 2026-09-28.)
func mergeBlockedQueueFixture(t *testing.T) (front, racer, behind *fixture, repo *harness.Repo, d *harness.Daemon) {
	t.Helper()
	repo = harness.NewRepo(t)
	repo.DeploysAsAgentRepl()
	d = harness.StartDaemon(t, harness.Opts{DurableState: true})
	repoRef := mergeRepositoryRef(t, d, repo)

	front = mergeCreateChild(t, d, repoRef, "front", "front work",
		harness.PreDeployPrompts("hold the queue open"))
	racer = mergeCreateChild(t, d, repoRef, "racer", "racer work",
		harness.PreDeployPrompts("hold the race open"))
	behind = mergeCreateChild(t, d, repoRef, "behind", "behind work", nil)

	harness.CommitWork(t, front.ws.GetDir())
	if _, err := d.Client().FinishWorkspace(d.Ctx(), connect.NewRequest(harness.FinishRequest(front.ws))); err != nil {
		t.Fatalf("FinishWorkspace(front) = error %v, want the merge enqueued", err)
	}
	// The pre-prompt's turn is started and NEVER answered: the run sits in it
	// holding the repository's slot.
	front.shim.ExpectStartTurn()

	// THE MERGE RIGHT BEHIND THE HEAD RACES IT (internal/merge/race.go), so it
	// is held the same way, in its own pre-prompt: the merge behind it is the
	// first that truly waits in line, beyond the queue's window.
	harness.CommitWork(t, racer.ws.GetDir())
	if _, err := d.Client().FinishWorkspace(d.Ctx(), connect.NewRequest(harness.FinishRequest(racer.ws))); err != nil {
		t.Fatalf("FinishWorkspace(racer) = error %v, want the merge enqueued", err)
	}
	racer.shim.ExpectStartTurn()

	harness.CommitWork(t, behind.ws.GetDir())
	if _, err := d.Client().FinishWorkspace(d.Ctx(), connect.NewRequest(harness.FinishRequest(behind.ws))); err != nil {
		t.Fatalf("FinishWorkspace(behind) = error %v, want the merge enqueued", err)
	}
	return front, racer, behind, repo, d
}

// ---------------------------------------------------------------------------
// UpdateMergeQueue: pause, resume, evict.
// ---------------------------------------------------------------------------

func TestUpdateMergeQueuePauseThenResumeToggleTheQueueStateAndRefuseNoOps(t *testing.T) {
	t.Parallel()
	// Arrange
	_, _, _, repo, d := mergeBlockedQueueFixture(t)
	// The sweep covers every test; the declared records are evidence of a refusal the test provokes, the merge conflict the test stages.
	d.ExpectWarnings("daemon.gitclient.merge_no_ff", "daemon.merge.merge_tab", "daemon.merge.pause", "daemon.merge.unpause")
	repoRef := mergeRepositoryRef(t, d, repo)

	// Act: pause.
	resp, err := d.Client().UpdateMergeQueue(d.Ctx(), connect.NewRequest(&agentreplv1.UpdateMergeQueueRequest{
		Action: &agentreplv1.UpdateMergeQueueRequest_Pause{Pause: &agentreplv1.UpdateMergeQueuePause{Repository: repoRef}},
	}))

	// Assert: pause succeeds.
	requireRPCSuccess(t, "UpdateMergeQueue(pause)", resp, err)

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
	requireRPCSuccess(t, "UpdateMergeQueue(resume)", resp3, err)

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
	_, _, behind, _, _ := mergeBlockedQueueFixture(t)
	// The sweep covers every test; the declared records are evidence of the merge conflict the test stages, the queued merge the test abandons.
	behind.d.ExpectWarnings("daemon.gitclient.merge_no_ff", "daemon.merge.drop_queued", "daemon.merge.merge_tab")
	roster := behind.d.WatchRoster()
	awaitRoster(t, behind.d, roster, "the behind workspace queued", func(r *frontendv1.WorkspaceRoster) bool {
		return rosterRow(r, behind.ws.GetId()).GetMergeQueued() != nil
	})

	// Act
	resp, err := behind.d.Client().UpdateMergeQueue(behind.d.Ctx(), connect.NewRequest(&agentreplv1.UpdateMergeQueueRequest{
		Action: &agentreplv1.UpdateMergeQueueRequest_Evict{Evict: &agentreplv1.UpdateMergeQueueEvict{Workspace: behind.ws}},
	}))
	requireRPCSuccess(t, "UpdateMergeQueue(evict)", resp, err)

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
// Interrupt on a queued merge raises the merge-queue question in the pinned
// bubble; AnswerMergeQueueOffer evicts, prioritizes or cancels.
// ---------------------------------------------------------------------------

func TestInterruptOnAQueuedWorkspaceRaisesTheMergeQueueQuestion(t *testing.T) {
	t.Parallel()
	// Arrange
	_, _, behind, _, _ := mergeBlockedQueueFixture(t)
	// The sweep covers every test; the declared records are evidence of the merge conflict the test stages.
	behind.d.ExpectWarnings("daemon.gitclient.merge_no_ff", "daemon.merge.merge_tab")
	roster := behind.d.WatchRoster()
	awaitRoster(t, behind.d, roster, "the behind workspace queued", func(r *frontendv1.WorkspaceRoster) bool {
		return rosterRow(r, behind.ws.GetId()).GetMergeQueued() != nil
	})
	feed := behind.watchRootFeed()

	// Act
	resp, err := behind.d.Client().Interrupt(behind.d.Ctx(), connect.NewRequest(&agentreplv1.InterruptRequest{
		Workspace: behind.ws,
		Target:    &agentreplv1.InterruptRequest_Turn{Turn: &agentreplv1.InterruptTurn{}},
	}))

	// Assert: the stop is about the merge, and offers no rollback.
	if err != nil {
		t.Fatalf("Interrupt(turn) on a queued, idle workspace = error %v, want a success", err)
	}
	if resp.Msg.GetSuccess().GetMergeQueueOffered() == nil {
		t.Fatalf("Interrupt(turn) = %v, want InterruptSuccess.merge_queue_offered", resp.Msg)
	}

	// Assert: the question stands in the pinned bubble.
	row := awaitRow(t, behind, feed, "the merge head carrying the question", func(r *frontendv1.FeedRow) bool {
		return mergeQueueOffer(r) != nil
	})
	if row.GetOrder().Pinned == nil {
		t.Fatalf("the merge head carrying the question = %v, want it pinned", row)
	}
}

// durableQueueOrder stops D and answers every workspace on its merge queue,
// in queue order. The daemon holds the sole writing handle on its store while
// it runs, so the queue is read after its orderly exit; the queue is durable
// across it.
func durableQueueOrder(t *testing.T, d *harness.Daemon) []string {
	t.Helper()
	d.Stop()
	order := []string{}
	d.WithDB(func(db *sql.DB) {
		rows, err := db.Query(`SELECT workspace_id FROM merge_queue ORDER BY seq`)
		if err != nil {
			t.Fatalf("read the merge queue: %v", err)
		}
		defer rows.Close()
		for rows.Next() {
			var id string
			if err := rows.Scan(&id); err != nil {
				t.Fatalf("scan the merge queue: %v", err)
			}
			order = append(order, id)
		}
		if err := rows.Err(); err != nil {
			t.Fatalf("read the merge queue: %v", err)
		}
	})
	return order
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

// TestKillingAWorkspaceAbandonsItsQueuedMergeWithTheCloseAsTheCause is the
// third end, reachable at last. CloseWorkspace refuses outright while a merge
// is queued, so KillWorkspace is the one door such a workspace leaves through,
// and the abandoned terminal it draws is the ONLY place the wire can say why:
// FeedMergeError has one `abandoned` arm for every end, so the cause lives in
// FeedMergeAbandoned.summary and nowhere else.
func TestKillingAWorkspaceAbandonsItsQueuedMergeWithTheCloseAsTheCause(t *testing.T) {
	t.Parallel()
	// Arrange
	_, _, behind, _, _ := mergeBlockedQueueFixture(t)
	// The sweep covers every test; the declared records are evidence of the merge conflict the test stages, the queued merge the kill abandons, a KillSession the fake shim answers by exiting, a session fault the kill opens, the shim death and severed link the kill drives.
	//
	// daemon.shimclient.redial belongs to that same kill: the fake answers
	// KillSession by EXITING, so its socket can break before the reaper has
	// decided the death, and the monitor then reports the break and its one
	// failed redial exactly as it should. Which side of that race a run lands
	// on is a matter of scheduling, so the record is declared here for the
	// same reason the sibling teardown tests declare it, not because it is
	// unimportant.
	behind.d.ExpectWarnings("daemon.gitclient.merge_no_ff", "daemon.merge.drop_queued", "daemon.merge.merge_tab", "daemon.health.open_fault", "daemon.shimclient.exit", "daemon.shimclient.kill_session", "daemon.sessionwatcher.link_fault", "daemon.shimclient.redial", "daemon.workspace.kill", "daemon.sessionwatcher.watch_session", "daemon.sessionwatcher.watch_agent")
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
// The Emacs-repo method: a conflicting branch opens a conflict section on its commit, briefs
// the agent once with the spliced brief, then parks.
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// The test gate: pass settles the run; failure opens a fixing attempt with the
// brief and parks on escalation; no automatic re-run.
// ---------------------------------------------------------------------------

// mergeCleanRepo builds a self-repo with a scripted bin/test-all.sh and one
// child workspace, whose merge lands with no conflict (the merge tab always
// succeeds cleanly absent a scripted conflict).
func mergeCleanRepo(t *testing.T) (f *fixture, d *harness.Daemon, repo *harness.Repo, script *harness.Recorder) {
	t.Helper()
	repo = harness.NewRepo(t)
	script = harness.NewTestAllScript(t, repo.Dir)
	repo.DeploysAsAgentRepl()
	// A merge-spanning wait chains dozens of subprocesses; see harness.MergeChainTimeout.
	d = harness.StartDaemon(t, harness.Opts{DurableState: true, Timeout: harness.MergeChainTimeout, ExtraEnv: []string{harness.TestAllScriptEnv(script.Path)}})
	repoRef := mergeRepositoryRef(t, d, repo)
	f = mergeCreateChild(t, d, repoRef, "clean", "do the clean thing", nil)
	return f, d, repo, script
}

// ---------------------------------------------------------------------------
// Landed: success, footer, roster, worktree removal, and the self-repo
// landing's one deploy.
// ---------------------------------------------------------------------------

// TestALandedMergeIsSeenByEveryObserver lands one clean one-shot merge and
// reads it from every place a landing shows: the feed's success row, the
// footer, the roster, the desktop banners and the removed worktree.
//
// ONE ACT, SEVERAL OBSERVERS (AGENTS.md "A case may share an expensive
// fixture, never a slice another case reads"): each observation is its own
// named case, so one that fails reports without hiding the others. The waits
// bound to the daemon (the landing's deploy, the banner settling, the
// worktree's removal) run in the test itself, around the observers.
func TestALandedMergeIsSeenByEveryObserver(t *testing.T) {
	t.Parallel()
	// Arrange
	f, d, _, script := mergeCleanRepo(t)
	script.SetExitCode(0)
	script.SetStdout("daemon: passed in 1s\n")
	dir := f.ws.GetDir()
	// EVERY STREAM THIS TEST READS IS OPENED BEFORE THE MERGE IS ENQUEUED.
	// The footer's merged substatus is a momentary status the daemon's own
	// successor push retires after footer.DefaultMomentaryDwell, and the
	// merged roster row is closed out right behind it — so a watch opened
	// after the terminal feed row has already arrived is a watch that opens
	// on the state AFTER the one it is waiting for.
	//
	// THE TEARDOWN'S ORDER IS NOT ASSERTED FROM HERE. Publishing the terminal
	// row and removing the worktree are both the daemon's, in that order, and
	// internal/merge's TestTerminalIsPublishedBeforeTheWorktreeIsRemoved holds
	// that ordering deterministically.
	root := f.watchRootFeed()
	footer := f.d.WatchFooter(f.ws)
	roster := f.d.WatchRoster()

	// Act
	harness.CommitWork(t, f.ws.GetDir())
	if _, err := d.Client().FinishWorkspace(d.Ctx(), connect.NewRequest(harness.FinishRequest(f.ws))); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}
	mergeRow := awaitRow(t, f, root, "the merge's success", func(row *frontendv1.FeedRow) bool {
		return row.GetActivity().GetMerge().GetSuccess() != nil
	})
	d.AwaitLandingDeployed()
	d.AwaitLogRecord(d.RunLogPath(), "the landed merge's banner", func(r harness.LogRecord) bool {
		return bannerSettled(r) && r.Context["kind"] == "merge_resolved"
	})

	// Assert
	t.Run("the feed's success row carries the landed commit", func(t *testing.T) {
		if mergeRow.GetActivity().GetMerge().GetSuccess().GetCommit() == "" {
			t.Errorf("FeedMergeSuccess = %v, want a landed commit", mergeRow.GetActivity().GetMerge().GetSuccess())
		}
	})
	t.Run("the footer reads merged", func(t *testing.T) {
		awaitFooter(t, f, footer, "the footer's merged status", func(v *frontendv1.FooterView) bool {
			return v.GetStrip().GetStatus().GetMerged() != nil
		})
	})
	t.Run("the roster lists it under recently merged, stated merged", func(t *testing.T) {
		// THE STATUS IS PART OF THE PREDICATE: the row enters recently_merged
		// as MERGED and is closed out a moment later, so matching on presence
		// alone would read whichever snapshot arrived first.
		awaitRoster(t, f.d, roster, "the merged workspace under recently_merged, stated merged", func(r *frontendv1.WorkspaceRoster) bool {
			for _, row := range rosterwalk.MergedRows(r) {
				if row.GetWorkspace().GetWorkspace().GetId() == f.ws.GetId() {
					return row.GetMerged() != nil
				}
			}
			return false
		})
	})
	t.Run("only the queued and landed banners rose", func(t *testing.T) {
		var merges [][]string
		for _, inv := range bannerRuns(f) {
			argv := [][]string{inv.Argv}
			if bannerCarrying(argv, "merge queued") || bannerCarrying(argv, "merge landed") ||
				bannerCarrying(argv, "merge failed") || bannerCarrying(argv, "merge abandoned") {
				merges = append(merges, inv.Argv)
			}
		}
		if len(merges) != 2 || !bannerCarrying(merges[:1], "🔀", "merge queued") || !bannerCarrying(merges[1:], "🎉", "merge landed", "Merged into main") {
			t.Errorf("merge banners %q, want the queued banner then the landed one, and nothing else", merges)
		}
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
	writeCommit(t, repo, f.ws.GetDir(), "modules/app/agent-repl/daemon/cmd/agent-repld/main.go", "landed\n")

	// THE FEED IS SUBSCRIBED BEFORE THE MERGE IS ENQUEUED. A landing tears the
	// merged workspace's worktree down, and the feed resolves its log sink by
	// stat-ing that directory -- so an open after the enqueue races the
	// teardown and intermittently finds no such workspace to watch.
	root := f.watchRootFeed()

	// Act
	if _, err := d.Client().FinishWorkspace(d.Ctx(), connect.NewRequest(harness.FinishRequest(f.ws))); err != nil {
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
	if got := len(d.Launchctl.InvocationsExcept("print")); got != 0 {
		t.Fatalf("launchctl invocations = %d, want none: a failed build restarts nothing", got)
	}
}

// holdDeploys asks d to hold, or (release) to release, REPOSITORY's deploy
// stage.
func holdDeploys(t *testing.T, d *harness.Daemon, repository *workspacev1.RepositoryRef, release bool) *agentreplv1.UpdateDeployHoldResponse {
	t.Helper()
	req := &agentreplv1.UpdateDeployHoldRequest{Repository: repository,
		Action: &agentreplv1.UpdateDeployHoldRequest_Hold{Hold: &agentreplv1.UpdateDeployHoldHold{}}}
	if release {
		req.Action = &agentreplv1.UpdateDeployHoldRequest_Release{Release: &agentreplv1.UpdateDeployHoldRelease{}}
	}
	resp, err := d.Client().UpdateDeployHold(d.Ctx(), connect.NewRequest(req))
	if err != nil {
		t.Fatalf("UpdateDeployHold(%v) = error %v, want an answer", req.GetAction(), err)
	}
	return resp.Msg
}

// askMergeQueue sends d one UpdateMergeQueue request and answers its reply,
// failing the test on a transport error: every queue control is answered.
func askMergeQueue(t *testing.T, d *harness.Daemon, req *agentreplv1.UpdateMergeQueueRequest) *agentreplv1.UpdateMergeQueueResponse {
	t.Helper()
	resp, err := d.Client().UpdateMergeQueue(d.Ctx(), connect.NewRequest(req))
	if err != nil {
		t.Fatalf("UpdateMergeQueue(%v) = error %v, want an answer", req.GetAction(), err)
	}
	return resp.Msg
}

// TestAHoldTakenBeforeARestartKeepsTheSuccessorsLandingFromDeploying covers
// the hold end to end: taken on one daemon, it survives that daemon's stop,
// and the deploy the next daemon lands on main skips its deploy hook.
func TestAHoldTakenBeforeARestartKeepsTheSuccessorsLandingFromDeploying(t *testing.T) {
	t.Parallel()
	// Arrange, Act
	d2, _ := heldLanding(t)

	// Assert: the skipped deploy hook is recorded, and the deploy never built.
	d2.AwaitLogRecord(d2.RunLogPath(), "the held deploy", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.merge.deploy_stage" && r.Message == "the repository's deploy stage is held; the deploy hook is skipped"
	})
	if got := len(d2.Deploy.Invocations()); got != 0 {
		t.Fatalf("deploy builds = %d, want none while the repository is held", got)
	}
}

// TestReleasingTheHoldRunsTheSkippedDeployHook covers a release: the deploy
// hook the hold skipped runs once, and rolls out what landed during the hold.
func TestReleasingTheHoldRunsTheSkippedDeployHook(t *testing.T) {
	t.Parallel()
	// Arrange
	d2, repoRef := heldLanding(t)

	// Act
	resp := holdDeploys(t, d2, repoRef, true)

	// Assert
	released := resp.GetSuccess().GetReleased()
	if released.GetScriptRan().GetPath() != harness.DeployLandingScript || released.GetSkippedDeploys() != 1 {
		t.Fatalf("release = %v, want the deploy script run once for the one skipped deploy", resp)
	}
	d2.AwaitLandingDeployed()
	if got := len(d2.Deploy.Invocations()); got == 0 {
		t.Fatal("the release built nothing, want what landed during the hold deployed")
	}
}

// heldLanding holds the repository's deploys, stops the daemon that took the
// hold, and lands a deploy on main under its successor, answering the
// successor and the repository's ref.
func heldLanding(t *testing.T) (*harness.Daemon, *workspacev1.RepositoryRef) {
	t.Helper()
	repo := harness.NewRepo(t)
	script := harness.NewTestAllScript(t, repo.Dir)
	script.SetExitCode(0)
	script.SetStdout("daemon: passed in 1s\n")
	env := []string{harness.TestAllScriptEnv(script.Path)}
	repo.DeploysAsAgentRepl()
	d1 := harness.StartDaemon(t, harness.Opts{DurableState: true, Timeout: harness.MergeChainTimeout, ExtraEnv: env})
	if resp := holdDeploys(t, d1, mergeRepositoryRef(t, d1, repo), false); resp.GetSuccess().GetHeld() == nil {
		t.Fatalf("hold = %v, want held", resp)
	}
	d1.Stop()
	d2 := harness.StartDaemon(t, harness.Opts{StateDir: d1.StateDir, Timeout: harness.MergeChainTimeout,
		ExtraEnv: append(env, "AGENT_REPL_LOCK_DIR="+d1.LockDir)})
	inherited := d2.AwaitLogRecord(d2.RunLogPath(), "the inherited hold", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.merge.recover" && strings.HasPrefix(r.Message, "a repository's deploy stage is held")
	})
	if inherited.Level != "info" || inherited.Context["held_since"] == nil {
		t.Fatalf("the successor's boot record = %+v, want the inherited hold stated at INFO with its start", inherited)
	}
	repoRef := mergeRepositoryRef(t, d2, repo)
	f := mergeCreateChild(t, d2, repoRef, "held", "do the held thing", nil)
	writeCommit(t, repo, f.ws.GetDir(), "modules/app/agent-repl/daemon/cmd/agent-repld/main.go", "landed while held\n")
	root := f.watchRootFeed()
	if _, err := d2.Client().FinishWorkspace(d2.Ctx(), connect.NewRequest(harness.FinishRequest(f.ws))); err != nil {
		t.Fatalf("FinishWorkspace = error %v, want the deploy enqueued", err)
	}
	awaitRow(t, f, root, "the deploy's success", func(row *frontendv1.FeedRow) bool {
		return row.GetActivity().GetMerge().GetSuccess() != nil
	})
	return d2, repoRef
}

// ---------------------------------------------------------------------------
// The rollout runs only where the repository's deploy config asks for it:
// agent-repl's deploy hook runs for landings on main alone (runs_for: main),
// and a repository whose config names no deploy hook never rolls out. The two
// tests below are the two ways of being "half right": agent-repl's local merge
// landing in a parent workspace's worktree, and a deploy of hooks alone.
// ---------------------------------------------------------------------------

func TestALocalMergeIntoAParentWorktreeRunsNoDeployHook(t *testing.T) {
	t.Parallel()
	// Arrange: agent-repl's deploy config, a top-level PARENT workspace, and
	// a CHILD nested under it whose deploy lands into the PARENT's worktree:
	// the local merge runs a real `git merge` there, but the landing is not
	// on main, so the deploy hook (runs_for: main) does not run.
	repo := harness.NewRepo(t)
	script := harness.NewTestAllScript(t, repo.Dir)
	script.SetExitCode(0)
	script.SetStdout("daemon: passed in 1s\n")
	repo.DeploysAsAgentRepl()
	// A merge-spanning wait chains dozens of subprocesses; see harness.MergeChainTimeout.
	d := harness.StartDaemon(t, harness.Opts{Timeout: harness.MergeChainTimeout, ExtraEnv: []string{harness.TestAllScriptEnv(script.Path)}})
	repoRef := mergeRepositoryRef(t, d, repo)
	parent := mergeCreateChild(t, d, repoRef, "sibling7a-parent", "the parent work", nil)

	childResp, err := d.Client().CreateWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Repository:    repoRef,
		Requester:     harness.User(),
		ContextOrigin: harness.Fresh(),
		// The child carries an initial prompt: the arrangement below waits for
		// its StartTurn and terminates it, and a promptless create opens no
		// turn to wait for at all.
		Form: &agentreplv1.CreateWorkspaceRequest_Standard{Standard: &agentreplv1.CreateWorkspaceStandard{
			Topology:      harness.ChildOf(parent.ws, "the child's work"),
			InitialPrompt: said("the child work"),
			Name:          strPtr("sibling7a-child"),
		}},
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
	// nothing but the hook's scope is what keeps the deploy off.
	writeCommit(t, repo, child.GetDir(), "modules/app/agent-repl/daemon/cmd/agent-repld/main.go", "landed\n")

	// Act
	if _, err := d.Client().FinishWorkspace(d.Ctx(), connect.NewRequest(harness.FinishRequest(child))); err != nil {
		t.Fatalf("FinishWorkspace(child) = error %v, want the merge enqueued", err)
	}
	roster := d.WatchRoster()
	awaitRoster(t, d, roster, "the child's merge landed", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, child.GetId())
		return row != nil && row.GetMerged() != nil
	})

	// Assert: the local merge DID run -- a real `git merge` targeted the
	// parent's worktree, never the main checkout.
	targetedParent := false
	for _, c := range d.Git.Calls() {
		if createArgsContain(c.Args, "merge") && createGitDir(c.Args) == parent.ws.GetDir() {
			targetedParent = true
		}
	}
	if !targetedParent {
		t.Fatalf("git calls = %v, want a merge run inside the parent's worktree %q", d.Git.Calls(), parent.ws.GetDir())
	}

	// Assert: the rollout never fires -- the landing was not on main.
	if got := len(d.Deploy.Invocations()); got != 0 {
		t.Fatalf("deploy builds = %d, want 0: a landing in a parent's worktree runs no main-only deploy hook", got)
	}
}

func TestAHooksOnlyDeployNeverTriggersTheRollout(t *testing.T) {
	t.Parallel()
	// Arrange: a repository whose deploy config is hooks alone (the
	// fixture's default): no local merge and no deploy hook.
	repo := harness.NewRepo(t)
	d := harness.StartDaemon(t, harness.Opts{})
	repository := createRepositoryRef(t, d, repo)
	resp, err := d.Client().CreateWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Repository:    repository,
		Requester:     harness.User(),
		ContextOrigin: harness.Fresh(),
		Form: &agentreplv1.CreateWorkspaceRequest_OneShot{OneShot: &agentreplv1.CreateWorkspaceOneShot{
			Topology: harness.Primary(),
			Prompt:   said("ship the fix"),
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
	harness.CommitWork(t, ws.GetDir())
	if _, err := d.Client().FinishWorkspace(d.Ctx(), connect.NewRequest(harness.FinishRequest(ws))); err != nil {
		t.Fatalf("FinishWorkspace(one_shot) = error %v, want the merge enqueued", err)
	}

	// Assert: the deploy succeeds (a hooks-only config with no hooks has
	// nothing to run, so it reaches "merged" straight away).
	awaitRoster(t, d, roster, "the one-shot's merge landed", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, ws.GetId())
		return row != nil && row.GetMerged() != nil
	})

	// Assert: the rollout never fires -- the config names no deploy hook.
	if got := len(d.Deploy.Invocations()); got != 0 {
		t.Fatalf("deploy builds = %d, want 0: the repository's config names no deploy hook", got)
	}
}

// ---------------------------------------------------------------------------
// The non-Emacs-repo method: only pre/post prompts, never the Emacs-only
// rebasing and tests tabs.
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// Boot recovery: a merge in flight across a restart is resumed or loudly
// failed, never left stuck.
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// A missing brief file fails the step loudly.
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// The pre-prompt and post-prompt tabs: agentic, output-address parented, and
// the post-prompt's failure never fails the run.
// ---------------------------------------------------------------------------

func TestPrePromptTabRunsUnderTheLeaseAndParentsItsRowsToItsTabNotTheRoot(t *testing.T) {
	t.Parallel()
	// Arrange: a configured before-merge prompt. THE MERGE RUNS TO ITS END
	// INSIDE THE TEST: after the pre-prompt the merge goes on to its test gate,
	// so the gate is a scripted pass and the test awaits the landing below.
	// Ending at the pre-prompt left that work racing the test's teardown, and a
	// gate with no script to run logged its refusal whenever it won the race.
	repo := harness.NewRepo(t)
	script := harness.NewTestAllScript(t, repo.Dir)
	script.SetExitCode(0)
	script.SetStdout("daemon: passed in 1s\n")
	repo.DeploysAsAgentRepl()
	d := harness.StartDaemon(t, harness.Opts{Timeout: harness.MergeChainTimeout, ExtraEnv: []string{harness.TestAllScriptEnv(script.Path)}})
	repoRef := mergeRepositoryRef(t, d, repo)
	f := mergeCreateChild(t, d, repoRef, "prepromptrepo", "do the feature", harness.PreDeployPrompts("run the setup script"))
	root := f.watchRootFeed()

	// Act: enqueue and answer the pre-prompt's own turn.
	harness.CommitWork(t, f.ws.GetDir())
	if _, err := d.Client().FinishWorkspace(d.Ctx(), connect.NewRequest(harness.FinishRequest(f.ws))); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}
	req := f.shim.ExpectStartTurn()
	if req.GetOrigin() != mergeBeforeActionOrigin {
		t.Fatalf("pre-prompt StartTurn.origin = %v, want PROMPT_ORIGIN_MERGE_BEFORE_ACTION", req.GetOrigin())
	}

	mergeRow := f.awaitRowInFeed(nil, "the merge bubble's head", func(row *frontendv1.FeedRow) bool {
		return row.GetActivity().GetMerge() != nil
	})
	preTab := f.awaitRowInFeed(mergeRow.GetId(), "the pre-deploy hook tab, live", func(row *frontendv1.FeedRow) bool {
		return row.GetMergeTab().GetHookPrompt() != nil && row.GetMergeTab().GetLabel().GetText() == "pre-deploy"
	})

	// Act: conclude the pre-prompt's turn.
	pushConcludedTurn(f.shim, mainAgent, "pre-prompt-done")

	// Assert: the turn's own concluded row is parented to the pre-deploy hook tab —
	// the OUTPUT ADDRESS the lease's session was stamped with — never to root.
	// The tab's content is the merge sub-feed's rows PARENTED to the tab row
	// (feed.proto: "Content: sub-feed rows parented to this row") — a tab row
	// is not itself a feed, so the concluded row is looked up on the bubble's
	// sub-feed and its parent is what places it on the tab.
	concluded := f.awaitRowInFeed(mergeRow.GetId(), "the pre-prompt turn's concluded row", func(row *frontendv1.FeedRow) bool {
		return row.GetTurnEnded().GetConcluded() != nil
	})
	if concluded.GetParent().GetRow().GetValue() != preTab.GetId().GetValue() {
		t.Fatalf("the pre-prompt turn's concluded row parent = %v, want the pre-deploy hook tab %v",
			concluded.GetParent().GetRow(), preTab.GetId())
	}
	// Assert: THAT turn's terminal never rode the root feed. The workspace's
	// own creation turn ended on root before the merge was ever enqueued, so
	// the check is scoped to the pre-prompt turn's id rather than to every
	// turn_ended row.
	page, _ := f.openFeed(nil)
	for _, row := range page.GetSuccess().GetRows() {
		if row.GetTurnEnded() != nil && row.GetTurn().GetValue() == concluded.GetTurn().GetValue() {
			t.Fatalf("the pre-prompt turn's concluded row rode the ROOT feed: %v, want it parented to the pre-deploy hook tab only", row)
		}
	}

	// Assert: the tab itself settles.
	settled := f.awaitRowInFeed(mergeRow.GetId(), "the pre-deploy hook tab, settled", func(row *frontendv1.FeedRow) bool {
		return row.GetMergeTab().GetHookPrompt().GetSettled() != nil
	})
	if settled.GetMergeTab().GetHookPrompt().GetSettled().GetSucceeded() == nil {
		t.Fatalf("pre-deploy hook tab settled = %v, want succeeded", settled.GetMergeTab().GetHookPrompt())
	}

	// Assert: the merge lands, so none of its work outlives the test.
	awaitRow(t, f, root, "the merge's terminal push", isMergeTerminal)
	d.AwaitLandingDeployed()
}

func TestAFailingPostDeployHookFailsTheDeployAndKeepsTheLanding(t *testing.T) {
	t.Parallel()
	// Arrange: a clean landing with a post-deploy prompt hook that fails.
	repo := harness.NewRepo(t)
	script := harness.NewTestAllScript(t, repo.Dir)
	script.SetExitCode(0)
	script.SetStdout("daemon: passed in 1s\n")
	repo.DeploysAsAgentRepl()
	// A merge-spanning wait chains dozens of subprocesses; see harness.MergeChainTimeout.
	d := harness.StartDaemon(t, harness.Opts{Timeout: harness.MergeChainTimeout, ExtraEnv: []string{harness.TestAllScriptEnv(script.Path)}})
	repoRef := mergeRepositoryRef(t, d, repo)
	f := mergeCreateChild(t, d, repoRef, "postpromptfail", "do the clean thing", harness.PostDeployPrompts("clean up after landing"))

	// THE FEED IS SUBSCRIBED BEFORE THE MERGE IS ENQUEUED. A landing tears the
	// merged workspace's worktree down, and the feed resolves its log sink by
	// stat-ing that directory -- so a subscription opened after the enqueue
	// races the teardown and intermittently finds no such workspace to watch.
	// Opening first is the rendezvous the assertion actually needs.
	root := f.watchRootFeed()

	// Act: land, then answer the post-prompt's turn with a FAILURE.
	harness.CommitWork(t, f.ws.GetDir())
	if _, err := d.Client().FinishWorkspace(d.Ctx(), connect.NewRequest(harness.FinishRequest(f.ws))); err != nil {
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

	// Assert: the deploy FAILS, its account naming the landing that stands.
	mergeRow := awaitRow(t, f, root, "the deploy's terminal push", isMergeTerminal)
	failure := mergeRow.GetActivity().GetMerge().GetError()
	if failure == nil || !strings.Contains(failure.String(), "it landed as commit") {
		t.Fatalf("deploy terminal = %v, want FeedMergeError naming the landed commit", mergeRow.GetActivity().GetMerge())
	}
	if got := repo.BranchHead(harness.DefaultBranch); got == "" {
		t.Fatal("the default branch has no head, want the landing to stand")
	}
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

// displacedBounceWarnings declares the records every daemon in this scenario
// legitimately writes: see the call site in displacedTurnAcrossABounce.
func displacedBounceWarnings(d *harness.Daemon) {
	d.ExpectWarnings("daemon.merge.recover", "daemon.promptqueue.restore_holds")
}

// displacedBounceText is the user's own words, asserted end to end.
const displacedBounceText = "keep going"

// displacedTurnAcrossABounce stages the whole scenario: a user turn in flight
// when a merge admits, a daemon killed inside the window between the capture
// and everything that would close it, and a fresh daemon on the same state
// root. It answers the fixture (rebound to the new daemon), that daemon, and
// the resubmission's own StartTurn.
//
// THE CRASH LANDS BEFORE THE MERGE'S FIRST PROGRESS RECORD (the capture
// precedes it), so the restart has no step to resume the merge at: it runs
// again from the queue, and the displaced record -- which no resumed merge
// owns -- is the boot sweep's to put back.
func displacedTurnAcrossABounce(t *testing.T) (*fixture, *harness.Daemon, *shimv1.StartTurnRequest) {
	t.Helper()
	repo := harness.NewRepo(t)
	script := harness.NewTestAllScript(t, repo.Dir)
	script.SetExitCode(0)
	script.SetStdout("daemon: passed in 1s\n")
	rendezvous := filepath.Join(t.TempDir(), "capture.rendezvous")
	repo.DeploysAsAgentRepl()
	// A merge-spanning wait chains dozens of subprocesses; see harness.MergeChainTimeout.
	d := harness.StartDaemon(t, harness.Opts{DurableState: true, Timeout: harness.MergeChainTimeout, ExtraEnv: []string{
		harness.TestAllScriptEnv(script.Path),
		"AGENT_REPL_MERGE_PAUSE_AFTER_CAPTURE=" + rendezvous,
	}})
	// The sweep covers every test; these declared records are evidence of the
	// crash this test stages. A turn left in flight by a killed daemon is
	// closed by the next boot (daemon.promptqueue.restore_holds) -- which is
	// what the RESUBMITTED turn becomes when this test kills the daemon that
	// received it -- and a daemon killed mid-merge leaves a merge the next
	// boot recovers (daemon.merge.recover).
	displacedBounceWarnings(d)
	repoRef := mergeRepositoryRef(t, d, repo)
	f := mergeCreateChild(t, d, repoRef, "displaced", "do the clean thing", nil)
	f.submit(displacedBounceText, "k-displace-bounce", origin)
	// No terminal frame is ever pushed for it: it is the workspace's in-flight
	// turn when the merge admits, which is what CaptureDisplaced reads.
	f.shim.ExpectStartTurn()
	harness.CommitWork(t, f.ws.GetDir())
	if _, err := d.Client().FinishWorkspace(d.Ctx(), connect.NewRequest(harness.FinishRequest(f.ws))); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}
	d.AwaitFileExists(rendezvous)
	d.Kill()

	repo.DeploysAsAgentRepl()
	// A merge-spanning wait chains dozens of subprocesses; see harness.MergeChainTimeout.
	d2 := harness.StartDaemon(t, harness.Opts{StateDir: d.StateDir, Timeout: harness.MergeChainTimeout, ExtraEnv: []string{
		harness.TestAllScriptEnv(script.Path),
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
// on the conflicted commit's subtab, never as a root-feed row.
// ---------------------------------------------------------------------------

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
	requireRPCSuccess(t, "UpdateMergeQueue(pause, no repository)", resp, err)

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
	if _, err := d.Client().FinishWorkspace(d.Ctx(), connect.NewRequest(harness.FinishRequest(f.ws))); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}
	mergeRow := awaitRow(t, f, root, "the merge's success", func(row *frontendv1.FeedRow) bool {
		return row.GetActivity().GetMerge().GetSuccess() != nil
	})
	d.AwaitLandingDeployed()

	// Assert: the settled tests tab carries the scripted run's output as
	// DAEMON-PARSED paint spans — the client never sees the raw escape. This
	// searches every selected suite's output rather than assuming a single
	// suite named "daemon": how many suites the blast radius selects is not
	// this test's concern, only that the ANSI escape was parsed into a span
	// wherever it landed, and that no raw escape reached the wire on any of
	// them.
	runRow := f.awaitRowInFeed(mergeRow.GetId(), "the settled run", func(row *frontendv1.FeedRow) bool {
		return row.GetMergeTab().GetTestsRun().GetSettled() != nil
	})
	suites := runRow.GetMergeTab().GetTestsRun().GetOutput().GetSuites()
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

// ---------------------------------------------------------------------------
// The merge ledger holds the tab intervals. NO rpc serves it (grepped
// modules/app/agent-repl/proto and internal/wsm/mergeledger.go); it is
// persisted only in the workspace state database's merge_tab_intervals table,
// so this test reads it directly with harness.WithDB, ONLY AFTER stopping the
// daemon, per that helper's own documented contract.
// ---------------------------------------------------------------------------

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
func mergeCreateChild(t *testing.T, d *harness.Daemon, repoRef *workspacev1.RepositoryRef, name, prompt string, hooks *agentreplv1.CreateWorkspaceDeployHooks) *fixture {
	t.Helper()
	resp, err := d.Client().CreateWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Repository:    repoRef,
		Requester:     harness.User(),
		ContextOrigin: harness.Fresh(),
		Form: &agentreplv1.CreateWorkspaceRequest_Standard{Standard: &agentreplv1.CreateWorkspaceStandard{
			Topology:      harness.Primary(),
			InitialPrompt: said(prompt),
			Name:          strPtr(name),
			DeployHooks:   hooks,
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

// mergeQueueOffer answers the merge-queue question a merge head row carries,
// nil when the row is no merge head or carries none.
func mergeQueueOffer(row *frontendv1.FeedRow) *frontendv1.FeedMergeQueueOffer {
	return row.GetActivity().GetMerge().GetQueueOffer()
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
	harness.CommitWork(t, f.ws.GetDir())
	if _, err := d.Client().FinishWorkspace(d.Ctx(), connect.NewRequest(harness.FinishRequest(f.ws))); err != nil {
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

// ---------------------------------------------------------------------------
// The prompt-bubble-height sequence (2026-09-28, lease c8a3a664006f46c1): a
// gate that cannot run, the park it answers with at once, a user prompt the
// parked merge delivers and answers, the dequeue that releases everything,
// and the next merge in the repository landing. Before the fix the park
// swallowed the first prompt, left nothing to receive the second, and held
// the repository's queue until the daemon restarted.
// ---------------------------------------------------------------------------

// TestAQueuedMergeWhoseSessionParksAtItsColdGateLeavesTheQueue pins the owner's
// 2026-10-13 ruling: a queued merge whose session went cold is ejected from
// the queue, not kept waiting on a session that cannot take its prompts.
func TestAQueuedMergeWhoseSessionParksAtItsColdGateLeavesTheQueue(t *testing.T) {
	t.Parallel()
	// Arrange: behind waits in line; the daemon restarts, and behind's
	// resume is scripted cold.
	_, _, behind, _, d := mergeBlockedQueueFixture(t)
	// The sweep covers every test; the declared records are evidence of the restart under test.
	expectSessionKillRecords(d)
	// PAUSED, so nothing the restart cuts can let behind run before its
	// session comes up: a paused queue advances nothing.
	if resp, err := d.Client().UpdateMergeQueue(d.Ctx(), connect.NewRequest(&agentreplv1.UpdateMergeQueueRequest{
		Action: &agentreplv1.UpdateMergeQueueRequest_Pause{Pause: &agentreplv1.UpdateMergeQueuePause{}},
	})); err != nil || resp.Msg.GetError() != nil {
		t.Fatalf("UpdateMergeQueue(pause) = %v, %v, want the queue paused", resp, err)
	}
	if _, err := d.Client().UpdateShutdownSchedule(d.Ctx(), connect.NewRequest(&agentreplv1.UpdateShutdownScheduleRequest{
		Action: &agentreplv1.UpdateShutdownScheduleRequest_Now{Now: &agentreplv1.UpdateShutdownScheduleNow{
			Reason: drainReasonOperator("the restart under test"),
		}},
	})); err != nil {
		t.Fatalf("UpdateShutdownSchedule{now} = %v, want the immediate shutdown accepted", err)
	}
	d.AwaitExit()
	d.WriteShimProfile(behind.ws.GetDir(), harness.ShimProfile{ColdOnResume: &harness.ShimColdFacts{
		ContextTokens: 160210, LastRequestAtMS: 1_700_000_000_000, RequestedModel: "opus", CacheTTLMS: 300_000,
	}})

	// Act: the successor boots, and behind's session comes up parked at its
	// cold gate.
	d2 := harness.StartDaemon(t, d.SuccessorOpts())
	d2.DefaultConfigDir = d.DefaultConfigDir
	roster := d2.WatchRoster()

	// Assert: the merge left the queue, and the leaving is recorded at INFO.
	awaitRoster(t, d2, roster, "the cold workspace off the queue", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, behind.ws.GetId())
		return row != nil && row.GetMergeQueued() == nil
	})
	d2.AwaitRunLogRecordFromAnyProcess("the cold gate's ejection", func(r harness.LogRecord) bool {
		return r.Level == "info" && r.Operation == "daemon.merge.cold_gate" && r.Context["workspace"] == behind.ws.GetId()
	})
}
