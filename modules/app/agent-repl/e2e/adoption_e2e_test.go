// Adoption on restart (SPEC.md section C, "Adoption on restart", #47-50).
//
// Every test here drives a REAL daemon, a REAL shim-store, a REAL sidecar,
// and the REAL shim (its `--fake` scripted vendor), per world_test.go and
// main_test.go. Git is the harness's scripted fake-git world
// (claude-repld/integration/harness.NewRepo) — never a real git process —
// exactly like every other daemon/integration test; only the daemon, store,
// sidecar, and shim are real. No test seeds the store with hand-written
// rows: #47 and #48 use driveScenarioToCompletion (world_test.go) to drive a
// named, real fake-SDK scenario ("prose-streamed" — grounded, no manifest
// caveat) through a real session to completion before simulating a daemon
// bounce, per the binding ruling in
// docs/overhaul/reports/E2E-EVENT-INVENTORY.md's "PROJECT-LEAD RULINGS" #3.
//
//   - #47 ColdBootReadsReplayFromStore — docs/overhaul/store.md cursor/replay
//     semantics: a cold-started daemon (SIGKILL, then a fresh StartDaemon
//     against the same state root, kernel-lock dir, and store socket)
//     replays a workspace's history correctly from the store's durable rows,
//     written by a prior REAL session, never a live one it never started.
//   - #48 SessionStartedReAnnouncedOnEveryNewWatch —
//     docs/overhaul/PROTO-CHANGES.md "Landing 7": shim.v1 WatchSessionResponse
//     is `oneof frame { update = 1; session_started = 2 }`, "the original
//     SessionStarted re-announced ONCE per watch, right after the opening
//     diagnostics, on EVERY new watch, so an adopting daemon (crash boot,
//     handover) attaches purely." daemon/internal/sessionwatcher/watcher.go's
//     reannouncedLocked takes the facts up (logging "took the session facts
//     from the shim's re-announcement" at INFO) on a watcher's FIRST
//     application and logs "ignored a re-announced SessionStarted..." at
//     DEBUG on every later one within the SAME watcher's lifetime. A crash
//     boot's own sessionwatcher is fresh, so its first application is a
//     SECOND, freshly-counted "took the session facts" record against the
//     SAME still-running real shim — proving the cardinality is exactly one
//     PER WATCH, not per session.
//   - #49 HandoverTransfersAtFreeness — docs/overhaul/daemon.md "Rollout /
//     handover": "Blue-green self-rollout: old daemon spawns the rebuilt one
//     (joining mode), transfers workspaces one by one at FREENESS (no
//     in-flight turn, no live detached work)." A HEADLESS workspace (never
//     opened, so it has zero rendezvous participants) transfers the instant
//     the handover recognizes it as free, per the same section: "headless
//     workspaces transfer with zero rendezvous."
//   - #50 RefusalOrderingDuringHandover — same section: "Ordering by
//     REFUSAL: the new daemon refuses unowned workspaces (not_yet_adopted);
//     the old refuses with transferring_away{address}; lagging clients
//     self-heal." Exercised on an OPEN workspace whose host+web streams are
//     both live at the moment of announcement, so it is an EXPECTED
//     rendezvous participant on both sides and does not transfer until the
//     test itself, playing the lagging client, calls
//     AdoptHostWorkspace/AdoptWebWorkspace.
//
// #49 and #50 fire a REAL self-merge rollout (daemon.md's own blue-green
// mechanism: the incumbent re-execs itself with --joining) by registering the
// daemon's own fake-git checkout as its SelfRepo, landing a real (scripted,
// fake-git) commit through a real CreateWorkspace + MergeWorkspace round
// trip, and setting AGENT_REPL_TEST_ALL_SCRIPT so the merge gate passes —
// exactly the mechanism daemon/integration/drain_rollout_test.go's own
// TestHandoverTransfersAFreeWorkspaceThroughTheAdoptionRendezvous and
// TestAHeadlessWorkspaceTransfersWithoutAnyAdoptCall use, reimplemented here
// against a REAL shim's initial-turn completion (driven by a real
// "!prose-streamed" prompt, awaited on the feed) instead of that suite's
// fake-shim scripting, since e2e never fakes the shim.
package e2e

import (
	"context"
	"crypto/tls"
	"net"
	"net/http"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"connectrpc.com/connect"
	"golang.org/x/net/http2"

	"claude-repld/integration/harness"
)

// ===========================================================================
// #47 — ColdBootReadsReplayFromStore
// ===========================================================================

func TestColdBootReadsReplayFromStore(t *testing.T) {
	t.Parallel()
	// Arrange: drive a real "!prose-streamed" turn to completion so the
	// store durably holds REAL rows (driveScenarioToCompletion blocks on the
	// sidecar's own cursor advance) before any bounce is simulated — the
	// binding alternative to a hand-seeded storedAssistantEvent fixture
	// (E2E-EVENT-INVENTORY.md ruling 3).
	w := NewWorld(t, WorldOpts{})
	w.ExpectWarnings("daemon.rollout.reconcile")
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)

	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "prose-streamed")

	// Act: crash the daemon (SIGKILL) — the real shim, spawned into its own
	// process group by the daemon's own shim-spawn plumbing, survives — and
	// cold-boot a successor against the SAME state root, kernel-lock dir,
	// and store socket.
	successor := adColdBoot(t, w)

	// Assert: the cold-started successor's OpenFeed page replays the prior
	// REAL session's turn purely from the store's durable rows (store.md
	// cursor/replay semantics) — this daemon never started that session
	// itself.
	adAwaitReplayedFeedRow(t, successor, ws, "the prior REAL session's turn replayed from the store",
		endsTurn(turn))
}

// ===========================================================================
// #48 — SessionStartedReAnnouncedOnEveryNewWatch
// ===========================================================================

// adWatchSessionOp is the operation both the "took the session facts" and
// "ignored a re-announced SessionStarted" records use
// (daemon/internal/sessionwatcher/watcher.go's runSession/reannouncedLocked)
// — the two are told apart by MESSAGE, not by operation.
const adWatchSessionOp = "daemon.sessionwatcher.watch_session"

// adTookSessionFactsMessage is reannouncedLocked's INFO message on a
// watcher's FIRST application of the shim's re-announced SessionStarted.
const adTookSessionFactsMessage = "took the session facts from the shim's re-announcement"

// adIgnoredReannouncementMessage is reannouncedLocked's DEBUG message when a
// watcher that ALREADY holds the facts sees a later re-announcement on the
// SAME watch (an ordinary re-open after a link break, not a new watcher): it
// ignores the facts and reconciles only the live membership.
const adIgnoredReannouncementMessage = "ignored a re-announced SessionStarted's facts; they are already held"

// adCountLogMessage counts a workspace's daemon-log records matching an exact
// operation and a message substring, ACROSS EVERY DAEMON RUNTIME that wrote
// the sink. The canonical <workspace>/.claude/emacs/daemon.log symlink is NOT
// cumulative: internal/dlog/sink.go mints a fresh target per runtime ("A
// restart never trusts the previous run's destination") and re-points the link
// at it, so after a cold boot the link answers only the successor's own
// records. harness.ReadCumulativeWorkspaceLog reads every runtime's target.
func adCountLogMessage(t *testing.T, workspaceDir, op, substr string) int {
	t.Helper()
	n := 0
	for _, r := range harness.ReadCumulativeWorkspaceLog(t, workspaceDir, "daemon") {
		if r.Operation == op && strings.Contains(r.Message, substr) {
			n++
		}
	}
	return n
}

func TestSessionStartedReAnnouncedOnEveryNewWatch(t *testing.T) {
	t.Parallel()
	// Arrange
	w := NewWorld(t, WorldOpts{})
	w.ExpectWarnings("daemon.rollout.reconcile")
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)

	driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "prose-streamed")

	// THE ORIGINAL DAEMON TAKES NO FACTS FROM A RE-ANNOUNCEMENT, BUT DOES
	// SEE ONE. The shim re-announces on EVERY new watch (PROTO-CHANGES.md
	// landing 7) and re-announces only what a prior StartSession already
	// announced — reannounceStart returns undefined while `announcedStart`
	// is unset (agent-shim/claude/shim/src/engine/session.ts:2263-2267,
	// "UNSET before StartSession, which is the one state with nothing to
	// re-state").
	//
	// This daemon opens TWO distinct streams before any restart: the ready
	// probe first (which predates StartSession, so it draws no
	// re-announcement at all), then the session watch itself — established
	// AFTER StartSession set `announcedStart`, so it DOES draw one. That
	// watcher already holds the facts from StartSession's own answer, so
	// reannouncedLocked takes the ordinary ignore branch
	// (internal/sessionwatcher/watcher.go:804-808, "A watcher that ALREADY
	// HOLDS the facts ignores it ... It is the ORDINARY case"). One ignored
	// record, and no "took" record, is therefore the pre-crash truth — the
	// earlier expectation of zero ignored records assumed a single
	// pre-restart stream.
	if got := adCountLogMessage(t, repo.Dir, adWatchSessionOp, adTookSessionFactsMessage); got != 0 {
		t.Fatalf("the original daemon's watch_session log holds %d %q records, want 0 (it took the facts from StartSession's own answer)", got, adTookSessionFactsMessage)
	}
	if got := adCountLogMessage(t, repo.Dir, adWatchSessionOp, adIgnoredReannouncementMessage); got != 1 {
		t.Fatalf("the original daemon's watch_session log holds %d ignored re-announcements before any restart, want exactly 1 (its session watch, opened after StartSession, draws one)", got)
	}
	// Act: crash-adopt onto the SAME still-running real shim — PROTO-CHANGES.md
	// Landing 7: the shim re-announces SessionStarted once per watch "so an
	// adopting daemon (crash boot, handover) attaches purely."
	successor := adColdBoot(t, w)
	// WAIT FOR THE RECORD THIS TEST IS ABOUT. Waiting for one more record under
	// the OPERATION was satisfied by the successor's own "session watch
	// opened", which is written when the stream opens and before any
	// re-announcement has been read off it — so the assertions below ran
	// against a log the "took" record had not reached, and read 0 where they
	// want 1. Twice in eight in-container runs.
	successor.AwaitCumulativeWorkspaceLogMessageCount(repo.Dir, adWatchSessionOp, adTookSessionFactsMessage, 1)

	// Assert: the exact cardinality (SPEC.md #48), not merely presence. The
	// successor's fresh watcher — the first watch this session has seen
	// since StartSession set `announcedStart` — took the re-announced facts
	// up exactly once (the ONE "took" record across the two daemons'
	// combined, cumulative log), and the only ignored re-announcement in
	// that log is still the incumbent's own pre-crash one — a SECOND
	// successor-side ignore would mean the same WATCHER, not just the
	// process, survived the crash, the wrong shape for a cold boot.
	if got := adCountLogMessage(t, repo.Dir, adWatchSessionOp, adTookSessionFactsMessage); got != 1 {
		t.Fatalf("the cumulative watch_session log holds %d %q records across both daemons, want exactly 1 (only the successor's fresh watcher has no facts yet)", got, adTookSessionFactsMessage)
	}
	// CUMULATIVE, so the incumbent's own pre-crash ignore is still counted:
	// the log sink is per-workspace and survives the crash.
	if got := adCountLogMessage(t, repo.Dir, adWatchSessionOp, adIgnoredReannouncementMessage); got != 1 {
		t.Fatalf("the cumulative watch_session log holds %d ignored re-announcements, want exactly 1 (the incumbent's own session watch; the successor's fresh watcher took the facts instead)", got)
	}
}

// adAwaitReplayedFeedRow answers DAEMON's first feed row for WS matching PRED,
// waiting on the feed's OWN PINNED TAIL when the newest page does not carry it
// yet.
//
// A COLD BOOT'S REPLAY IS NOT FINISHED WHEN THE DAEMON ANSWERS, and reading
// only the opening page assumed it was. `harness.StartDaemon` returns once
// `daemon.addr` is written and the server is serving; the workspace's surviving
// shim is adopted, its watches are opened, and the store's durable rows are
// replayed into the feed AFTER that, on the watcher's own goroutines. So an
// `OpenFeed` issued on the first line after the boot legitimately serves an
// empty page, and every assertion about a replayed row read a page the replay
// had not reached. That is `TestSubagentBubbleFromAReplayIsStillAddressable`
// failing once in nine in-container runs, and it is latent in every other
// cold-boot page read.
//
// The wait is on the DAEMON'S OWN PUSH rather than on a clock: the replay
// upserts each row and publishes it, and the tail this page pinned delivers
// exactly those. Its bound is a `DefaultTimeout` child of the run's budget,
// never the run's budget itself.
func adAwaitReplayedFeedRow(t *testing.T, d *harness.Daemon, ws *workspacev1.WorkspaceRef, what string, pred func(*frontendv1.FeedRow) bool) *frontendv1.FeedRow {
	t.Helper()
	opened, err := d.Client().OpenFeed(d.Ctx(), connect.NewRequest(&agentreplv1.OpenFeedRequest{Workspace: ws}))
	if err != nil {
		t.Fatalf("OpenFeed on the cold-booted successor = error %v, want a success", err)
	}
	success := opened.Msg.GetSuccess()
	if success == nil {
		t.Fatalf("OpenFeed on the cold-booted successor = %v, want a success", opened.Msg)
	}
	page := success.GetPage().GetSuccess()
	if page == nil {
		t.Fatalf("OpenFeed on the cold-booted successor = %v, want a served page", opened.Msg)
	}
	for _, row := range page.GetRows() {
		if pred(row) {
			return row
		}
	}
	stream := d.WatchFeedOn(d.Client(), success.GetWatch())
	defer stream.Close()
	ctx, cancel := context.WithTimeout(d.Ctx(), DefaultTimeout)
	defer cancel()
	return harness.AwaitView(t, ctx, stream, what, pred)
}

// adColdBoot SIGKILLs w's daemon and boots a fresh one against the SAME
// state root, kernel-lock dir, and store socket — a crash-restart cold boot
// re-adopting whatever real shim survived, never a second spawn.
func adColdBoot(t *testing.T, w *World) *harness.Daemon {
	t.Helper()
	w.Kill()
	opts := w.SuccessorOpts(t)
	// A cold boot's own re-adoption of a still-running real shim chains a
	// SECOND real process's full boot onto this one test, exactly the shape
	// AdoptionChainTimeout documents (world_test.go) — reused verbatim rather
	// than the tighter single-boot DefaultTimeout.
	opts.Timeout = AdoptionChainTimeout
	successor := harness.StartDaemon(t, opts)
	successor.ExpectWarnings("daemon.rollout.reconcile")
	return successor
}

// ===========================================================================
// #49 — HandoverTransfersAtFreeness, #50 — RefusalOrderingDuringHandover
// ===========================================================================

// adSelfRepoWorld starts a World whose daemon's own checkout (SelfRepo) is a
// fresh fake-git repository with a passing merge gate, so a landed commit on
// it fires a real self-merge rollout (daemon.md "Rollout / handover"). The
// whole handover — the incumbent's own re-exec, the successor's boot, and
// its adoption of every workspace — runs on this ONE World's context, so it
// gets HandoverChainTimeout rather than the tighter single-boot default.
func adSelfRepoWorld(t *testing.T) (*harness.Repo, *World) {
	t.Helper()
	selfRepo := harness.NewRepo(t)
	script := harness.NewTestAllScript(t, selfRepo.Dir)
	script.SetExitCode(0)
	script.SetStdout("e2e: passed in 1s\n")
	w := NewWorld(t, WorldOpts{DaemonOpts: harness.Opts{
		SelfRepo: selfRepo.Dir,
		ExtraEnv: []string{"AGENT_REPL_TEST_ALL_SCRIPT=" + script.Path},
		Timeout:  HandoverChainTimeout,
	}})
	return selfRepo, w
}

// adFindRepoKey finds a repository's roster section by its worktree — keyed
// by the COMMON DIR, `<worktree>/.git` for an ordinary checkout, so either
// spelling is checked (mirrors daemon/integration/merge_test.go's
// mergeFindRepoKey).
func adFindRepoKey(r *frontendv1.WorkspaceRoster, dir string) *workspacev1.RepositoryRef {
	for _, s := range r.GetRepository().GetSections() {
		switch s.GetKey().GetRepository().GetDir() {
		case dir, filepath.Join(dir, ".git"):
			return s.GetKey().GetRepository()
		}
	}
	return nil
}

// adRepositoryRef registers a fake-git repository and answers its roster
// RepositoryRef, once the roster carries it.
func adRepositoryRef(t *testing.T, w *World, repo *harness.Repo) *workspacev1.RepositoryRef {
	t.Helper()
	harness.Register(t, w.Daemon, repo.Dir)
	roster := w.WatchRoster()
	defer roster.Close()
	got := harness.AwaitView(t, w.Ctx(), roster, "the repository's roster section", func(r *frontendv1.WorkspaceRoster) bool {
		return adFindRepoKey(r, repo.Dir) != nil
	})
	ref := adFindRepoKey(got, repo.Dir)
	if ref == nil {
		t.Fatalf("no roster repository section for %s", repo.Dir)
	}
	return ref
}

// adSaidText builds the plain UserSaid shape SubmitPrompt sends, for the
// CreateWorkspace/SubmitPrompt calls this file makes directly against a raw
// client rather than through world_test.go's own SubmitPrompt helper.
func adSaidText(text string) *conversationv1.UserSaid {
	return &conversationv1.UserSaid{Content: &conversationv1.UserContent{Blocks: []*conversationv1.UserContentBlock{
		{Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: text}}},
	}}}
}

func adStrPtr(s string) *string { return &s }

// adAwaitAnyTurnEnded waits for a workspace's feed to carry a FeedTurnEnded
// row, for a freshly CREATED workspace whose own initial-prompt turn id this
// caller does not track (CreateWorkspaceSuccess mints no TurnId of its own).
func adAwaitAnyTurnEnded(t *testing.T, w *World, ws *workspacev1.WorkspaceRef) {
	t.Helper()
	opened, err := w.Client().OpenFeed(w.Ctx(), connect.NewRequest(&agentreplv1.OpenFeedRequest{Workspace: ws}))
	if err != nil {
		t.Fatalf("OpenFeed(%s) = error %v, want a success", ws.GetId(), err)
	}
	success := opened.Msg.GetSuccess()
	if success == nil {
		t.Fatalf("OpenFeed(%s) = %v, want success", ws.GetId(), opened.Msg)
	}
	for _, row := range success.GetPage().GetSuccess().GetRows() {
		if row.GetTurnEnded() != nil {
			return
		}
	}
	stream := w.WatchFeedOn(w.Client(), success.GetWatch())
	defer stream.Close()
	harness.AwaitView(t, w.Ctx(), stream, "the created workspace's initial turn to end", func(row *frontendv1.FeedRow) bool {
		return row.GetTurnEnded() != nil
	})
}

// adCreateAndFinishChild creates a top-level child workspace via
// CreateWorkspace, driving its initial prompt through the REAL shim (never
// the fake-shim scripting daemon/integration's own mergeCreateChild uses),
// and waits for that turn to conclude so the workspace starts idle — the
// state MergeWorkspace requires.
func adCreateAndFinishChild(t *testing.T, w *World, repoRef *workspacev1.RepositoryRef, name string) *workspacev1.WorkspaceRef {
	t.Helper()
	resp, err := w.Client().CreateWorkspace(w.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Repository: repoRef,
		Form: &agentreplv1.CreateWorkspaceRequest_Standard{Standard: &agentreplv1.CreateWorkspaceStandard{
			InitialPrompt: adSaidText("!prose-streamed"),
			Name:          adStrPtr(name),
		}},
	}))
	if err != nil {
		t.Fatalf("CreateWorkspace(%s) = error %v, want a success", name, err)
	}
	ws := resp.Msg.GetSuccess().GetWorkspace()
	if ws.GetId() == "" {
		t.Fatalf("CreateWorkspace(%s) = %v, want a success carrying a workspace ref", name, resp.Msg)
	}
	adAwaitAnyTurnEnded(t, w, ws)
	return ws
}

// adTriggerDeploy stages a deploy build that changes one component, then
// lands one fake-git-scripted commit on selfRepo through a real created child
// workspace and a real MergeWorkspace call: the landing runs the daemon's ONE
// deploy for it, which finds that component out of date (mirrors
// daemon/integration/drain_rollout_test.go's drainTriggerDeploy).
func adTriggerDeploy(t *testing.T, w *World, selfRepo *harness.Repo, build harness.DeployBuild) {
	t.Helper()
	w.StageDeployBuild(build)
	path := adSelfMergeTriggerPath
	// THE TRIGGER WORKSPACE'S SHIM IS STOOD DOWN BY THE MERGE before its
	// worktree is removed, and its link severing is the evidence of that stop
	// rather than a fault. The trigger is gone before the handover begins, so
	// the handover cannot transfer it: the successor could not resolve a log
	// sink for a directory that is gone. A workspace this daemon serves and does
	// not hand over is stood down before the exit, because nothing after this
	// daemon would know its process existed
	// (daemon/internal/rollout/handover.go, standDownTheUntransferred).
	w.ExpectWarnings("daemon.shimclient.redial", "daemon.sessionwatcher.link_fault")
	repoRef := adRepositoryRef(t, w, selfRepo)
	trigger := adCreateAndFinishChild(t, w, repoRef, "trigger")
	sha := selfRepo.CommitIn(trigger.GetDir(), path, "trigger\n")
	selfRepo.SetPaths(sha, path)
	if _, err := w.Client().MergeWorkspace(w.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{
		Workspace: trigger,
		Source:    harness.OwnBranch(false),
	})); err != nil {
		t.Fatalf("MergeWorkspace(trigger) = error %v, want the merge enqueued and landed", err)
	}
}

// adOpenWorkspace opens a registered workspace (spawns its real shim) so its
// host/web streams reflect a genuinely live session at handover time.
func adOpenWorkspace(t *testing.T, w *World, ws *workspacev1.WorkspaceRef) {
	t.Helper()
	resp, err := w.Client().OpenWorkspace(w.Ctx(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: ws}))
	if err != nil {
		t.Fatalf("OpenWorkspace(%s) = error %v, want a success", ws.GetId(), err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("OpenWorkspace(%s) = %v, want a success", ws.GetId(), resp.Msg)
	}
}

// adDial builds a raw Connect client against an arbitrary address, for
// reaching a handover's successor before it is discoverable any other way
// (its address rides the shutdown_announced push, never a harness field).
// Mirrors daemon/integration/drain_rollout_test.go's drainDial.
func adDial(addr string) agentreplv1connect.AgentReplClient {
	client := &http.Client{
		Transport: &http2.Transport{
			AllowHTTP: true,
			DialTLSContext: func(ctx context.Context, network, a string, _ *tls.Config) (net.Conn, error) {
				var dialer net.Dialer
				return dialer.DialContext(ctx, network, a)
			},
		},
	}
	return agentreplv1connect.NewAgentReplClient(client, "http://"+addr)
}

// adAwaitAddrFileChange polls daemon.addr until it holds exactly `want`,
// bounded by the daemon's own context — never a fixed sleep. Mirrors
// daemon/integration/drain_rollout_test.go's drainAwaitAddrFileChange.
func adAwaitAddrFileChange(t *testing.T, d *harness.Daemon, want string) {
	t.Helper()
	ticker := time.NewTicker(pollInterval)
	defer ticker.Stop()
	for {
		if body, err := os.ReadFile(d.AddrFile()); err == nil && harness.AddrLine(string(body)) == want {
			return
		}
		select {
		case <-ticker.C:
		case <-d.Ctx().Done():
			t.Fatalf("daemon.addr never became %q after the handover: %v", want, d.Ctx().Err())
		}
	}
}

// adSelfMergeTriggerPath is the path a trigger commit touches on the daemon's
// own checkout, the same one daemon/integration/drain_rollout_test.go's
// drainTriggerDeploy commits. What the landing's deploy changes is the staged
// build's, never the path's.
const adSelfMergeTriggerPath = "modules/app/agent-repl/daemon/cmd/claude-repld/main.go"

func TestHandoverTransfersAtFreeness(t *testing.T) {
	t.Parallel()
	// Arrange: a HEADLESS workspace — registered, never opened, so it has
	// ZERO rendezvous participants and, per daemon.md ("headless workspaces
	// transfer with zero rendezvous"), transfers the instant the handover
	// recognizes it as free (no in-flight turn, no live detached work).
	selfRepo, w := adSelfRepoWorld(t)
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)
	roster := w.WatchRoster()
	defer roster.Close()
	harness.AwaitNext(t, w.Ctx(), roster, "the roster with the headless row")
	daemonStream := w.WatchDaemonStream()
	defer daemonStream.Close()

	// Act: land a real, fake-git-scripted commit on the daemon's own
	// checkout, driven through a real created child workspace and a real
	// MergeWorkspace call.
	adTriggerDeploy(t, w, selfRepo, harness.DeployStaleDaemon)

	announced := harness.AwaitView(t, w.Ctx(), daemonStream, "shutdown_announced", func(r *agentreplv1.WatchDaemonResponse) bool {
		return r.GetShutdownAnnounced() != nil
	}).GetShutdownAnnounced()
	if announced.GetCause().GetSelfMergeRollout() == nil {
		t.Fatalf("shutdown_announced.cause = %v, want self_merge_rollout", announced.GetCause())
	}
	if announced.GetExpectedOutageMs() <= 0 {
		t.Fatalf("shutdown_announced.expected_outage_ms = %d, want a stated positive bounded outage", announced.GetExpectedOutageMs())
	}
	addr := announced.GetAddress()
	if addr == "" {
		t.Fatal("shutdown_announced.address is unset, want the successor's address for a handover")
	}

	// Assert: the incumbent exits once the headless (free, zero-participant)
	// workspace has transferred, having needed no AdoptHostWorkspace/
	// AdoptWebWorkspace rendezvous call for it at all.
	if code := w.AwaitExit(); code != 0 {
		t.Fatalf("the incumbent's exit code = %d, want an orderly 0 after the handover", code)
	}
	adAwaitAddrFileChange(t, w.Daemon, addr)

	// Assert: the workspace is immediately usable on the successor.
	successor := adDial(addr)
	if _, err := successor.SelectWorkspace(w.Ctx(), connect.NewRequest(&agentreplv1.SelectWorkspaceRequest{Workspace: ws})); err != nil {
		t.Fatalf("SelectWorkspace on the successor for the transferred headless workspace = error %v, want a success", err)
	}
}

func TestRefusalOrderingDuringHandover(t *testing.T) {
	t.Parallel()
	// Arrange: an ordinary, idle (free) workspace whose host+web streams are
	// BOTH open at the moment of announcement, so it is an EXPECTED
	// rendezvous participant and does not transfer until AdoptHostWorkspace/
	// AdoptWebWorkspace are explicitly called.
	selfRepo, w := adSelfRepoWorld(t)
	// The sweep covers every test; the declared record is evidence of the
	// refusal this test deliberately provokes (mirrors
	// TestHandoverTransfersAFreeWorkspaceThroughTheAdoptionRendezvous).
	w.ExpectWarnings("daemon.refusal.unlanded_arm.standing")

	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)
	adOpenWorkspace(t, w, ws)
	host := w.WatchHost(ws)
	defer host.Close()
	web := w.WatchWeb(ws)
	defer web.Close()
	harness.AwaitNext(t, w.Ctx(), host, "the fresh host push")
	daemonStream := w.WatchDaemonStream()
	defer daemonStream.Close()

	// Act
	adTriggerDeploy(t, w, selfRepo, harness.DeployStaleDaemon)
	announced := harness.AwaitView(t, w.Ctx(), daemonStream, "shutdown_announced", func(r *agentreplv1.WatchDaemonResponse) bool {
		return r.GetShutdownAnnounced() != nil
	}).GetShutdownAnnounced()
	addr := announced.GetAddress()
	if addr == "" {
		t.Fatal("shutdown_announced.address is unset, want the successor's address for a handover")
	}
	successor := adDial(addr)

	// Assert: the NEW daemon refuses the same workspace before adoption.
	// This one holds from the announcement onward and so is asserted first.
	newResp, err := successor.SubmitPrompt(w.Ctx(), connect.NewRequest(&agentreplv1.SubmitPromptRequest{
		Workspace: ws, Said: adSaidText("hello"), IdempotencyKey: newIdempotencyKey(t), Origin: e2ePromptOrigin,
	}))
	if err != nil {
		t.Fatalf("SubmitPrompt on the new daemon before adoption = error %v, want a typed not_yet_adopted answer", err)
	}
	if newResp.Msg.GetError().GetNotYetAdopted() == nil {
		t.Fatalf("SubmitPrompt on the new daemon before adoption = %v, want error.not_yet_adopted", newResp.Msg)
	}

	// THE OLD DAEMON'S REFUSAL IS ASSERTED BEFORE THE ADOPTS. It exits the
	// moment the successor serves every workspace (rollout/handover.go's
	// completeHandover: the rendezvous closing ends the adoption window, and
	// the exit follows at once, by design), so asked after the adopts it was
	// sometimes already gone — `connection refused` one run in three. Until the
	// rendezvous closes, timeAdoption holds it alive, still answering.
	//
	// Assert: both watchers see the transfer, naming the successor.
	//
	// THE TRANSFER PUSH IS THE BARRIER FOR THE OLD DAEMON'S REFUSAL. The old
	// daemon only starts answering transferring_away once recordTransfer has
	// run, and transfer() runs it AFTER awaitFreeForever and Quiesce, one
	// line before PushTransferred (daemon/internal/rollout/handover.go:129-
	// 130). Asserting the refusal off the shutdown announcement alone races
	// that whole sequence; awaiting the push is the event-driven signal that
	// the record is set.
	harness.AwaitView(t, w.Ctx(), host, "transferred", func(r *agentreplv1.WatchHostWorkspaceResponse) bool {
		return r.GetTransferred() != nil
	})
	webTransfer := harness.AwaitView(t, w.Ctx(), web, "transferred", func(r *agentreplv1.WatchWebWorkspaceResponse) bool {
		return r.GetTransferred() != nil
	}).GetTransferred()
	if webTransfer.GetAddress() != addr {
		t.Fatalf("WatchWebWorkspace transferred.address = %q, want the announced %q", webTransfer.GetAddress(), addr)
	}

	// Assert: the OLD daemon refuses further intake for the departing
	// workspace, naming the successor.
	oldResp, err := w.Client().SubmitPrompt(w.Ctx(), connect.NewRequest(&agentreplv1.SubmitPromptRequest{
		Workspace: ws, Said: adSaidText("hello"), IdempotencyKey: newIdempotencyKey(t), Origin: e2ePromptOrigin,
	}))
	if err != nil {
		t.Fatalf("SubmitPrompt on the old daemon after the transfer = error %v, want a typed transferring_away answer", err)
	}
	if away := oldResp.Msg.GetError().GetTransferringAway(); away == nil || away.GetAddress() != addr {
		t.Fatalf("SubmitPrompt on the old daemon = %v, want error.transferring_away naming %q", oldResp.Msg, addr)
	}

	// Act: the lagging client self-heals by completing the rendezvous.
	//
	// THE TWO ADOPTS MUST BE CONCURRENT. "EVERY EXPECTED PARTICIPANT
	// SUCCEEDS TOGETHER. The callers arrive concurrently and the one that
	// arrives first has not failed: it waits for the one that completes the
	// rendezvous" (daemon/internal/rollout/adopt.go:222-251). Issuing them
	// one after the other blocks the host call inside the rendezvous until
	// its own context expires — the web call is never made, and the caller
	// gets ErrNotYetAdopted — so this test issued them concurrently and
	// joins both.
	adopts := make(chan error, 2)
	go func() {
		_, err := successor.AdoptHostWorkspace(w.Ctx(), connect.NewRequest(&agentreplv1.AdoptHostWorkspaceRequest{Workspace: ws}))
		adopts <- err
	}()
	go func() {
		_, err := successor.AdoptWebWorkspace(w.Ctx(), connect.NewRequest(&agentreplv1.AdoptWebWorkspaceRequest{Workspace: ws}))
		adopts <- err
	}()
	for i := 0; i < 2; i++ {
		if err := <-adopts; err != nil {
			t.Fatalf("adopting the workspace on the successor = error %v, want a success (both adopts succeed together)", err)
		}
	}

	// Assert: the self-heal is complete — the workspace now answers
	// ordinarily on the successor.
	healed, err := successor.SubmitPrompt(w.Ctx(), connect.NewRequest(&agentreplv1.SubmitPromptRequest{
		Workspace: ws, Said: adSaidText("hello"), IdempotencyKey: newIdempotencyKey(t), Origin: e2ePromptOrigin,
	}))
	if err != nil || healed.Msg.GetSuccess() == nil {
		t.Fatalf("SubmitPrompt on the successor after self-heal = (%v, %v), want a success", healed.Msg, err)
	}
}

// ===========================================================================
// A handed-over workspace with no session draws its feed with no prompt
// ===========================================================================

// adLeaveSessionDown kills a workspace's shim twice on the incumbent: the
// first death is brought back by the incumbent itself (reviveAfterDeath), and
// the second, before any turn has ended, is left down by the revival's loop
// guard. It returns once the incumbent has said it leaves the session down,
// so the workspace is SESSION-LESS on a daemon that still serves it. Each
// step waits on the incumbent's own record of it, never on the killed pid,
// which the live incumbent reaps on its own schedule.
func adLeaveSessionDown(t *testing.T, w *World, ws *workspacev1.WorkspaceRef) {
	t.Helper()
	first := killShimProcesses(t, w.Daemon)
	w.Daemon.AwaitWorkspaceLogRecord(ws.GetDir(), "the incumbent's revival of the killed shim", func(r harness.LogRecord) bool {
		pid, ok := r.Context["shim_pid"].(float64)
		return r.Operation == "daemon.workspace.bring_up" && r.Message == "the session is up" &&
			ok && !slices.Contains(first, int(pid))
	})
	killShimProcesses(t, w.Daemon)
	w.Daemon.AwaitWorkspaceLogRecord(ws.GetDir(), "the loop guard leaving the session down", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.promptqueue.revive" && strings.Contains(r.Message, "left down until the next prompt")
	})
}

// TestASessionlessWorkspaceHandedOverDrawsItsFeedWithNoPrompt — owner ruling
// 2026-10-02: "The feed (or rather, the most recent page of the feed) should
// be automatically rendered when the workspace bounces or its backend(s)
// bounce, always." A workspace the incumbent serves with NO session is handed
// over with a free lock; the successor used to adopt it on its WSM facts alone
// and serve an empty feed until the user's next prompt revived it (ship-gns,
// 2026-10-01 17:55). The successor now starts its session as it adopts it, so
// the conversation's turn is drawn on the successor's feed with no prompt.
func TestASessionlessWorkspaceHandedOverDrawsItsFeedWithNoPrompt(t *testing.T) {
	t.Parallel()
	// Arrange: a finished real conversation whose session the incumbent has
	// left down. The shim deaths are what this test provokes: the link
	// records, the redials, the fault and the loop guard's own WARN are their
	// evidence, not defects.
	//
	// THE SECOND KILL LANDS THE MOMENT THE REVIVED SESSION IS UP, which is
	// exactly when the bring-up's follow-on calls are on the wire: its title
	// digest and its agent and session watches. Whichever of them the SIGKILL
	// catches in flight fails on the shim client with an EOF, at ERROR because
	// nobody in the daemon ordered that death. Which ones it catches is the
	// schedule's choice, so they are declared with the rest of the trail.
	w := dpStaleDaemonWorld(t)
	w.ExpectWarnings("daemon.promptqueue.revive", "daemon.sessionwatcher.link_fault",
		"daemon.shimclient.redial", "daemon.health.open_fault", "daemon.shimclient.exit",
		"daemon.sessionwatcher.watch_agent", "daemon.sessionwatcher.watch_session",
		"daemon.shimclient.gather_title_digest", "daemon.shimclient.watch_agent",
		"daemon.shimclient.watch_session")
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "prose-streamed")
	adLeaveSessionDown(t, w, ws)
	daemonStream := w.WatchDaemonStream()
	defer daemonStream.Close()

	// Act: an unforced deploy hands the session-less workspace over.
	resp, err := w.Client().Deploy(w.Ctx(), dpUnforced())
	if err != nil {
		t.Fatalf("Deploy: %v", err)
	}
	dpHandingOver(t, resp)
	addr := dpAwaitAnnounced(t, w, daemonStream).GetAddress()
	if addr == "" {
		t.Fatal("shutdown_announced.address is unset, want the successor's address")
	}
	if code := w.AwaitExit(); code != 0 {
		t.Fatalf("the incumbent's exit code = %d, want an orderly 0 after the handover", code)
	}
	adAwaitAddrFileChange(t, w.Daemon, addr)

	// Assert: with no prompt sent anywhere, the successor's feed draws the
	// conversation's turn.
	awaitFeedRowOn(t, w, adDial(addr), ws, "turn "+turn.GetValue()+" drawn on the successor with no prompt", func(r *frontendv1.FeedRow) bool {
		return r.GetTurn().GetValue() == turn.GetValue()
	})
}
