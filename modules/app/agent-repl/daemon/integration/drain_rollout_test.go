//go:build integration

package integration

import (
	"context"
	"crypto/tls"
	"net"
	"net/http"
	"os"
	"strings"
	"sync"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/integration/harness"
	"claude-repld/internal/rollout"

	"connectrpc.com/connect"
	"golang.org/x/net/http2"
)

// ---- UpdateShutdownSchedule ----

func TestUpdateShutdownScheduleSchedulePushesDrainScheduledToEverySubscriber(t *testing.T) {
	t.Parallel()
	// Arrange: two subscribers, standing in for Emacs and a webview.
	d := newDaemon(t, harness.Opts{})
	emacs := d.WatchDaemonStream()
	webview := d.WatchDaemonStreamOn(d.Dial())

	// Act
	resp, err := d.Client().UpdateShutdownSchedule(d.Ctx(), connect.NewRequest(&agentreplv1.UpdateShutdownScheduleRequest{
		Action: &agentreplv1.UpdateShutdownScheduleRequest_Schedule{Schedule: &agentreplv1.UpdateShutdownScheduleSchedule{
			AtMs:   time.Now().Add(time.Hour).UnixMilli(),
			Reason: drainReasonDeploy(),
		}},
	}))

	// Assert
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("UpdateShutdownSchedule{schedule} = (%v, %v), want a success", resp, err)
	}
	for _, s := range []*harness.Stream[*agentreplv1.WatchDaemonResponse]{emacs, webview} {
		push := harness.AwaitView(t, d.Ctx(), s, "drain_scheduled", func(r *agentreplv1.WatchDaemonResponse) bool {
			return r.GetDrainScheduled() != nil
		})
		if push.GetDrainScheduled().GetReason().GetDeploy() == nil {
			t.Fatalf("drain_scheduled.reason = %v, want the deploy reason echoed", push.GetDrainScheduled().GetReason())
		}
	}
}

func TestUpdateShutdownScheduleCancelPushesDrainCancelled(t *testing.T) {
	t.Parallel()
	// Arrange
	d := newDaemon(t, harness.Opts{})
	stream := d.WatchDaemonStream()
	if _, err := d.Client().UpdateShutdownSchedule(d.Ctx(), connect.NewRequest(&agentreplv1.UpdateShutdownScheduleRequest{
		Action: &agentreplv1.UpdateShutdownScheduleRequest_Schedule{Schedule: &agentreplv1.UpdateShutdownScheduleSchedule{
			AtMs: time.Now().Add(time.Hour).UnixMilli(), Reason: drainReasonDeploy(),
		}},
	})); err != nil {
		t.Fatalf("UpdateShutdownSchedule{schedule} = error %v, want a success", err)
	}
	harness.AwaitView(t, d.Ctx(), stream, "drain_scheduled", func(r *agentreplv1.WatchDaemonResponse) bool {
		return r.GetDrainScheduled() != nil
	})

	// Act
	resp, err := d.Client().UpdateShutdownSchedule(d.Ctx(), connect.NewRequest(&agentreplv1.UpdateShutdownScheduleRequest{
		Action: &agentreplv1.UpdateShutdownScheduleRequest_Cancel{Cancel: &agentreplv1.UpdateShutdownScheduleCancel{}},
	}))

	// Assert
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("UpdateShutdownSchedule{cancel} = (%v, %v), want a success", resp, err)
	}
	harness.AwaitView(t, d.Ctx(), stream, "drain_cancelled", func(r *agentreplv1.WatchDaemonResponse) bool {
		return r.GetDrainCancelled() != nil
	})
}

func TestUpdateShutdownScheduleCancelWithNothingScheduledIsRefused(t *testing.T) {
	t.Parallel()
	// Arrange
	d := newDaemon(t, harness.Opts{})
	// The sweep covers every test; the declared records are evidence of a refusal the test provokes.
	d.ExpectWarnings("daemon.drain.cancel")

	// Act
	resp, err := d.Client().UpdateShutdownSchedule(d.Ctx(), connect.NewRequest(&agentreplv1.UpdateShutdownScheduleRequest{
		Action: &agentreplv1.UpdateShutdownScheduleRequest_Cancel{Cancel: &agentreplv1.UpdateShutdownScheduleCancel{}},
	}))

	// Assert
	if err != nil {
		t.Fatalf("UpdateShutdownSchedule{cancel} = error %v, want a typed nothing_scheduled error", err)
	}
	if resp.Msg.GetError().GetNothingScheduled() == nil {
		t.Fatalf("UpdateShutdownSchedule{cancel} with nothing scheduled = %v, want error.nothing_scheduled", resp.Msg)
	}
}

func TestUpdateShutdownScheduleWithABlankOperatorNoteIsRefused(t *testing.T) {
	t.Parallel()
	// Arrange
	d := newDaemon(t, harness.Opts{})

	// Act
	_, err := d.Client().UpdateShutdownSchedule(d.Ctx(), connect.NewRequest(&agentreplv1.UpdateShutdownScheduleRequest{
		Action: &agentreplv1.UpdateShutdownScheduleRequest_Schedule{Schedule: &agentreplv1.UpdateShutdownScheduleSchedule{
			AtMs:   time.Now().Add(time.Hour).UnixMilli(),
			Reason: &agentreplv1.DrainReason{Kind: &agentreplv1.DrainReason_Operator{Operator: &agentreplv1.DrainReasonOperator{Note: "   "}}},
		}},
	}))

	// Assert
	if err == nil {
		t.Fatal("UpdateShutdownSchedule{schedule} with a blank operator note = success, want a refusal")
	}
	if connectCode(err) != connect.CodeInvalidArgument || !containsField(err, "note") {
		t.Fatalf("UpdateShutdownSchedule refusal = %v, want InvalidArgument naming the blank note", err)
	}
}

func TestUpdateShutdownScheduleNowAnnouncesImmediateShutdownWithNoAddress(t *testing.T) {
	t.Parallel()
	// Arrange
	d := newDaemon(t, harness.Opts{})
	stream := d.WatchDaemonStream()

	// Act
	before := time.Now()
	resp, err := d.Client().UpdateShutdownSchedule(d.Ctx(), connect.NewRequest(&agentreplv1.UpdateShutdownScheduleRequest{
		Action: &agentreplv1.UpdateShutdownScheduleRequest_Now{Now: &agentreplv1.UpdateShutdownScheduleNow{Reason: drainReasonOperator("operator maintenance")}},
	}))

	// Assert
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("UpdateShutdownSchedule{now} = (%v, %v), want a success", resp, err)
	}
	announced := harness.AwaitView(t, d.Ctx(), stream, "shutdown_announced", func(r *agentreplv1.WatchDaemonResponse) bool {
		return r.GetShutdownAnnounced() != nil
	}).GetShutdownAnnounced()
	after := time.Now()
	if announced.GetCause().GetImmediate() == nil {
		t.Fatalf("shutdown_announced.cause = %v, want immediate", announced.GetCause())
	}
	if got := announced.GetCause().GetImmediate().GetReason().GetOperator().GetNote(); got != "operator maintenance" {
		t.Fatalf("shutdown_announced.cause.immediate.reason.operator.note = %q, want the reason echoed verbatim", got)
	}
	if announced.Address != nil {
		t.Fatalf("shutdown_announced.address = %q, want unset (a plain bounce, no successor)", announced.GetAddress())
	}
	// Enrichment (critique 15): minted_at_ms is this announcement's own mint
	// instant (internal/drain/controller.go ShutdownNow: deps.Clock.Now()), so
	// it falls inside the wall-clock window the rpc call bracketed.
	// The comparison is in MILLISECONDS on both sides: minted_at_ms is
	// truncated to the millisecond, and comparing it against a wall-clock
	// instant carrying sub-millisecond precision fails whenever the bracket's
	// own remainder happens to be non-zero.
	mintedAt := time.UnixMilli(announced.GetMintedAtMs())
	if mintedAt.UnixMilli() < before.UnixMilli() || mintedAt.UnixMilli() > after.UnixMilli() {
		t.Fatalf("shutdown_announced.minted_at_ms = %d, want between %d and %d", announced.GetMintedAtMs(), before.UnixMilli(), after.UnixMilli())
	}
	// An immediate operator shutdown states no bounded outage: unlike the
	// self-merge rollout's handover.go (which states rollout.DefaultExpectedOutage),
	// internal/drain/controller.go's ShutdownNow never sets ExpectedOutageMs.
	if got := announced.GetExpectedOutageMs(); got != 0 {
		t.Fatalf("shutdown_announced.expected_outage_ms for an immediate shutdown = %d, want 0 (the drain controller states no outage)", got)
	}
	// An immediate shutdown with a valid reason logs only at INFO
	// (internal/drain/controller.go opNow): no WARN/ERROR is reached.
}

// TestUpdateShutdownScheduleNowLeavesNoShimBehindEvenAtAPermissionGate is the
// process-tree half of the host's stop, and the defect it pins was MEASURED:
// over one 24-scenario Emacs e2e run the reaper found 17 leaked `claude-repld`
// processes, 24 leaked shims and 48 leaked `shim-lock` holders, and the
// container's memory climbed from 0.5 GiB to 3.1 GiB across a full run.
//
// The workspace here is parked exactly where the leak was worst: a turn is in
// flight and a permission ask is standing unanswered, so the workspace never
// falls free on its own. A stop that waited for freeness would wait forever;
// `now` waits for none.
func TestUpdateShutdownScheduleNowLeavesNoShimBehindEvenAtAPermissionGate(t *testing.T) {
	t.Parallel()
	// Arrange: a live session parked at a permission gate.
	f := newOpened(t, harness.Opts{})
	expectSessionKillRecords(f.d)
	// Standing the session down on purpose opens the shim_died and
	// link_severed faults and, because the fake shim exits on the forced kill
	// rather than answering it, the "session kill did not answer" WARN.
	f.d.ExpectWarnings("daemon.health.open_fault", "daemon.workspace.bring_up")
	f.shim.ExpectStartSession()
	feed := f.watchRootFeed()
	f.submit("do the thing", "k-stop-at-a-gate", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	f.shim.PushAgentFrame(mainAgent, updateFrame(mainAgent, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_Permission{Permission: openPermission("perm-stop", "act-stop")},
	}))
	awaitRow(t, f, feed, "the standing permission card", func(r *frontendv1.FeedRow) bool {
		return r.GetPermission() != nil
	})
	if before := f.d.StrayPIDs(); len(before) == 0 {
		t.Fatal("no process names this run's state directory before the stop, so this test could pass without reclaiming anything")
	}

	// Act
	resp, err := f.d.Client().UpdateShutdownSchedule(f.d.Ctx(), connect.NewRequest(&agentreplv1.UpdateShutdownScheduleRequest{
		Action: &agentreplv1.UpdateShutdownScheduleRequest_Now{Now: &agentreplv1.UpdateShutdownScheduleNow{
			Reason: drainReasonOperator("emacs"),
		}},
	}))

	// Assert: the proto's `now` has exactly one outcome arm, success, and the
	// daemon and its whole tree go with it.
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("UpdateShutdownSchedule{now} at a permission gate = (%v, %v), want a success", resp, err)
	}
	f.d.AwaitExit()
	// The stand-down SIGKILLs each shim's whole process group and waits for
	// the reap before it returns, so nothing is left the moment the daemon has
	// gone. The bound is for the kernel's own bookkeeping catching up in `ps`,
	// not for a process still standing down.
	if left := awaitNoStrays(t, f.d, strayReclaimBound); len(left) > 0 {
		t.Fatalf("the host's stop left %d process(es) alive: %v — a shim outliving its daemon holds the workspace lock that refuses the next session", len(left), left)
	}
}

// TestUpdateShutdownScheduleNowAfterACompletedTurnExitsWellInsideItsOwnBound
// pins the STOP'S OWN COST after the shape every playbook ends in: a turn that
// ran and concluded, with the session's watches still standing.
//
// It exists because a headless run of the real editor reported `emacs phase
// daemon-exit took 6.04s (bound 6s)` on every scenario that ran a turn, which
// reads as a daemon that does not exit on the stop it acked. It is not: the
// bound belonged to the e2e Emacs layer's own stray finder, which counted the
// SCENARIO'S OWN Xvfb and sidecar and so could never come up empty. This is
// the assertion that says so from the daemon's side, and the one that would
// fail first if the exit ever did start riding a bound.
func TestUpdateShutdownScheduleNowAfterACompletedTurnExitsWellInsideItsOwnBound(t *testing.T) {
	t.Parallel()
	// Arrange: a live session that has run one turn to its terminal, with the
	// feed watch and the session's own watches still standing.
	f := newOpened(t, harness.Opts{})
	expectSessionKillRecords(f.d)
	// Standing a live session down on purpose opens the shim_died and
	// link_severed faults, and the fake shim exits on the forced kill rather
	// than answering it, which is the "session kill did not answer" WARN.
	f.d.ExpectWarnings("daemon.health.open_fault", "daemon.workspace.bring_up")
	f.shim.ExpectStartSession()
	feed := f.watchRootFeed()
	f.submit("do the thing", "k-stop-after-a-turn", origin)
	f.shim.ExpectStartTurn()
	// The DAEMON's opening of the turn, not the shim's receipt of it: a
	// terminal pushed on the request races the answer that names the main
	// agent, and one that wins is withheld rather than attributed.
	f.d.AwaitWorkspaceLogOperationCount(f.ws.GetDir(), harness.OpTurnOpened, 1)
	f.shim.PushAgentFrame(mainAgent, successFrame(mainAgent, nil))
	awaitRow(t, f, feed, "the turn's terminal row", func(r *frontendv1.FeedRow) bool {
		return r.GetTurnEnded() != nil
	})

	// Act: exactly what Emacs sends.
	asked := time.Now()
	resp, err := f.d.Client().UpdateShutdownSchedule(f.d.Ctx(), connect.NewRequest(&agentreplv1.UpdateShutdownScheduleRequest{
		Action: &agentreplv1.UpdateShutdownScheduleRequest_Now{Now: &agentreplv1.UpdateShutdownScheduleNow{
			Reason: drainReasonOperator("emacs"),
		}},
	}))

	// Assert
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("UpdateShutdownSchedule{now} after a completed turn = (%v, %v), want a success", resp, err)
	}
	f.d.AwaitExit()
	took := time.Since(asked)
	t.Logf("the stop after a completed turn was asked and the process was gone %s later", took.Round(time.Millisecond))
	if took > stopAfterATurnBound {
		t.Fatalf("the stop after a completed turn took %s to leave no process (bound %s); the daemon acked a stop it then rode a bound to perform",
			took.Round(time.Millisecond), stopAfterATurnBound)
	}
}

// stopAfterATurnBound is how long the whole stop above may take, from the
// request leaving the client to the daemon's process being reaped.
//
// MEASURED rather than chosen: 7ms at the median and 8ms at the worst across
// a -count=10 run of this test, and 5ms for the same stop against the REAL
// quartet (e2e/daemonstop_e2e_test.go). It covers the WHOLE orderly exit --
// the announcement, the forced stand-down of the one live session, the
// in-flight write grace, the merge drain, the watchers and the background
// loops -- so anything approaching it means a step of that exit has started
// riding its own bound instead of ending on an event. 250ms is ~31x the
// observed worst case, which is headroom for a loaded box and still an order
// of magnitude under the smallest bound the exit itself states
// (`writesQuietBound`, 250ms, is the only one this small, and every other
// step is measured in seconds).
const stopAfterATurnBound = 250 * time.Millisecond

// strayReclaimBound is how long the kernel is given to finish reaping a
// process group the daemon already SIGKILLed and already waited on.
//
// MEASURED: with the stand-down in place, the stray set is empty on the first
// read every time (observed over the -count=10 run of this test). This is a
// hundred times that, and anything still present after it is a leak rather
// than a slow reap.
const strayReclaimBound = 2 * time.Second

// awaitNoStrays polls until nothing names the daemon's state directory any
// more, answering whatever is left when the bound expires.
func awaitNoStrays(t *testing.T, d *harness.Daemon, bound time.Duration) []int {
	t.Helper()
	deadline := time.Now().Add(bound)
	ticker := time.NewTicker(10 * time.Millisecond)
	defer ticker.Stop()
	for {
		left := d.StrayPIDs()
		if len(left) == 0 || time.Now().After(deadline) {
			return left
		}
		<-ticker.C
	}
}

// TestUpdateShutdownScheduleNowLeavesNoShimBehindThatIsStillBringingUp is the
// leak the test above CANNOT reach: its workspace has a registered session, so
// the walk over the workspaces finds it. A shim that has been spawned and has
// not finished coming up is in no session map at all — Fleet.Stop answers nil
// for such a workspace — so the walk steps past it and the process outlives
// the daemon holding the workspace lock and ~95 MiB. Measured before the fix:
// a shim spawned at 18:02:54.268 outlived a daemon whose serving lifetime
// ended 21 ms later, and was still there when a 10 second grace expired.
//
// The bring-up is held open by withholding the fake's opening diagnostics,
// which is exactly the window the real spawn spends dialing its shim.
func TestUpdateShutdownScheduleNowLeavesNoShimBehindThatIsStillBringingUp(t *testing.T) {
	t.Parallel()
	// Arrange: a shim that withholds its opening diagnostics, so OpenWorkspace
	// blocks inside bring-up and the process reaches no session map.
	f := newRegistered(t, harness.Opts{})
	f.d.WriteShimProfile(f.repo.Dir, harness.ShimProfile{DelayDiagnostics: true})
	// The daemon is stood down mid-bring-up on purpose; these are that act's
	// own trail, on both sides of the abandoned spawn.
	f.d.ExpectWarnings(
		"daemon.shimclient.standdown", "daemon.shimclient.spawn",
		"daemon.shimclient.exit", "daemon.shimclient.redial",
		"daemon.workspace.bring_up", "daemon.workspace.open",
		"daemon.health.session", "daemon.health.open_fault",
	)

	opened := make(chan *agentreplv1.OpenWorkspaceResponse, 1)
	openFailed := make(chan error, 1)
	go func() {
		msg, err := f.openRaw()
		opened <- msg
		openFailed <- err
	}()
	// The fake binds its control listener at startup, so this returns as soon
	// as the PROCESS is up — long before any diagnostics it is withholding.
	shimPID := f.d.Shim(f.ws).Info().PID
	if shimPID == 0 {
		t.Fatal("the fake shim reported no pid; this test has no process to observe")
	}

	// Act
	resp, err := f.d.Client().UpdateShutdownSchedule(f.d.Ctx(), connect.NewRequest(&agentreplv1.UpdateShutdownScheduleRequest{
		Action: &agentreplv1.UpdateShutdownScheduleRequest_Now{Now: &agentreplv1.UpdateShutdownScheduleNow{
			Reason: drainReasonOperator("emacs"),
		}},
	}))

	// Assert
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("UpdateShutdownSchedule{now} with a spawn in flight = (%v, %v), want a success", resp, err)
	}
	harness.AwaitProcessGone(t, f.d.Ctx(), shimPID)
	f.d.AwaitExit()
	if left := awaitNoStrays(t, f.d, strayReclaimBound); len(left) > 0 {
		t.Fatalf("the host's stop left %d process(es) alive: %v — a shim spawned and not yet registered is nothing but the supervisor's to stand down", len(left), left)
	}
	// The bring-up its process was stood down under does not succeed, and the
	// caller is TOLD — through the verb's own refusal arm, which is how this
	// daemon says no. A transport error is what it must NOT be: that would be
	// the answer being cut off by the exit rather than produced by it, and the
	// caller would have no idea whether a session came up.
	answer := <-opened
	if err := <-openFailed; err != nil {
		t.Fatalf("OpenWorkspace during the stand-down = transport error %v, want the daemon's own refusal", err)
	}
	if answer.GetError().GetSpawnFailed() == nil {
		t.Fatalf("OpenWorkspace after its shim was stood down mid-bring-up = %v, want OpenWorkspaceError.spawn_failed", answer)
	}
}

// TestScheduledDrainFiresAndAnnouncesShutdownWithTheScheduledDrainCause is
// critique 15: strengthens the shutdown-announcement assertions onto the ONE
// cause no other test in this file reaches — a schedule that actually fires
// (every other shutdown_announced assertion here is either `now` or a
// self-merge handover). A headless daemon has nothing to wait free, so the
// drain loop (internal/drain/sweep.go Run, which caps its wait at the
// schedule's own deadline regardless of the 5-minute sweep cadence) fires the
// instant the deadline passes.
func TestScheduledDrainFiresAndAnnouncesShutdownWithTheScheduledDrainCause(t *testing.T) {
	t.Parallel()
	// Arrange
	d := newDaemon(t, harness.Opts{})
	stream := d.WatchDaemonStream()
	deadline := time.Now().Add(50 * time.Millisecond)
	if _, err := d.Client().UpdateShutdownSchedule(d.Ctx(), connect.NewRequest(&agentreplv1.UpdateShutdownScheduleRequest{
		Action: &agentreplv1.UpdateShutdownScheduleRequest_Schedule{Schedule: &agentreplv1.UpdateShutdownScheduleSchedule{
			AtMs: deadline.UnixMilli(), Reason: drainReasonDeploy(),
		}},
	})); err != nil {
		t.Fatalf("UpdateShutdownSchedule{schedule} = error %v, want a success", err)
	}
	harness.AwaitView(t, d.Ctx(), stream, "drain_scheduled", func(r *agentreplv1.WatchDaemonResponse) bool {
		return r.GetDrainScheduled() != nil
	})

	// Act: wait out the deadline.
	announced := harness.AwaitView(t, d.Ctx(), stream, "shutdown_announced", func(r *agentreplv1.WatchDaemonResponse) bool {
		return r.GetShutdownAnnounced() != nil
	}).GetShutdownAnnounced()
	after := time.Now()

	// Assert: the cause is the scheduled drain, carrying the SAME reason that
	// was scheduled (internal/drain/controller.go fire() decodes the persisted
	// row rather than re-deriving it).
	if announced.GetCause().GetScheduledDrain().GetReason().GetDeploy() == nil {
		t.Fatalf("shutdown_announced.cause = %v, want scheduled_drain with the deploy reason echoed", announced.GetCause())
	}
	// Enrichment (critique 15): minted_at_ms is minted only once fire() runs,
	// which cannot happen before the deadline it is waiting on, and this
	// assertion's own wall clock bounds it from above.
	// The wire stamp is MILLISECONDS, so both bounds are compared in
	// milliseconds too: a nanosecond-precision `deadline` is otherwise
	// strictly after its own truncated stamp whenever they share a
	// millisecond, which fails a run that is in fact exactly on time.
	mintedAt := time.UnixMilli(announced.GetMintedAtMs())
	if mintedAt.Before(deadline.Truncate(time.Millisecond)) || mintedAt.After(after) {
		t.Fatalf("shutdown_announced.minted_at_ms = %d, want between the deadline %d and now %d", announced.GetMintedAtMs(), deadline.UnixMilli(), after.UnixMilli())
	}
	// A scheduled drain states no bounded outage either (see the immediate-
	// shutdown test above for the same gap against the self-merge rollout).
	if got := announced.GetExpectedOutageMs(); got != 0 {
		t.Fatalf("shutdown_announced.expected_outage_ms for a scheduled drain = %d, want 0 (the drain controller states no outage)", got)
	}
	if announced.Address != nil {
		t.Fatalf("shutdown_announced.address = %q, want unset (a scheduled drain has no successor)", announced.GetAddress())
	}

	// Assert: the orderly exit that closes fire() actually ran.
	if code := d.AwaitExit(); code != 0 {
		t.Fatalf("the daemon's exit code after a fired scheduled drain = %d, want an orderly 0", code)
	}
}

// A SETTLED DETACHED RUN IS NOT SOMETHING TO WAIT FOR. The drain waits for
// every workspace to fall free without interrupting anything, and freeness is
// no turn in flight AND an empty live-work set. A detached subagent's own
// stream carries no agent terminal, so before the live set retired the run at
// its UNIT's terminal a workspace whose background agent had finished never
// fell free again and the drain waited on it until its context died.
func TestAScheduledDrainDoesNotWaitOnADetachedSubagentThatHasSettled(t *testing.T) {
	t.Parallel()
	// Arrange: a live session with one detached subagent, settled.
	f := newOpened(t, harness.Opts{})
	expectSessionKillRecords(f.d)
	f.d.ExpectWarnings("daemon.health.open_fault", "daemon.workspace.bring_up")
	f.shim.ExpectStartSession()
	footer := f.d.WatchFooter(f.ws)
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, ftSubagentSpawn("toolu-1", "toolu-1", "sweep the tree")))
	f.shim.PushAgentFrame(mainAgent, detachedWorkFrame(mainAgent, movedSubagent("toolu-1")))
	awaitFooter(t, f, footer, "the agents chip counting the detached run", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetLiveWork().GetAgents().GetCount() == 1
	})
	f.shim.PushAgentFrame("toolu-1", activityFrame("toolu-1", ftSubagentSettled("sub-unit-9", "toolu-1")))
	awaitFooter(t, f, footer, "the agents chip retired at the run's terminal", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetLiveWork().GetAgents() == nil
	})
	stream := f.d.WatchDaemonStream()

	// Act: schedule a drain and wait out its deadline.
	if _, err := f.d.Client().UpdateShutdownSchedule(f.d.Ctx(), connect.NewRequest(&agentreplv1.UpdateShutdownScheduleRequest{
		Action: &agentreplv1.UpdateShutdownScheduleRequest_Schedule{Schedule: &agentreplv1.UpdateShutdownScheduleSchedule{
			AtMs: time.Now().Add(50 * time.Millisecond).UnixMilli(), Reason: drainReasonDeploy(),
		}},
	})); err != nil {
		t.Fatalf("UpdateShutdownSchedule{schedule} = error %v, want a success", err)
	}

	// Assert: the drain reaches its announcement rather than standing on the
	// settled run, and the orderly exit that closes it runs.
	harness.AwaitView(t, f.d.Ctx(), stream, "shutdown_announced", func(r *agentreplv1.WatchDaemonResponse) bool {
		return r.GetShutdownAnnounced().GetCause().GetScheduledDrain() != nil
	})
	if code := f.d.AwaitExit(); code != 0 {
		t.Fatalf("the daemon's exit code after a drain over a settled detached run = %d, want an orderly 0", code)
	}
}

// ---- Drain intake and exit ----

func TestDuringADrainNewPromptsAreHeldWithTheShutdownHold(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	tray := f.d.WatchHolds(f.ws)
	harness.AwaitNext(t, f.d.Ctx(), tray, "the empty tray")
	if _, err := f.d.Client().UpdateShutdownSchedule(f.d.Ctx(), connect.NewRequest(&agentreplv1.UpdateShutdownScheduleRequest{
		Action: &agentreplv1.UpdateShutdownScheduleRequest_Schedule{Schedule: &agentreplv1.UpdateShutdownScheduleSchedule{
			AtMs: time.Now().Add(time.Hour).UnixMilli(), Reason: drainReasonDeploy(),
		}},
	})); err != nil {
		t.Fatalf("UpdateShutdownSchedule{schedule} = error %v, want a success", err)
	}

	// Act
	resp := f.submit("fix the flaky test", "k-drain-hold", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	if resp.GetSuccess().GetTurn().GetTurn().GetValue() == "" {
		t.Fatalf("SubmitPrompt during a drain = %v, want a minted TurnId (held, not refused)", resp)
	}

	// Assert
	got := harness.AwaitView(t, f.d.Ctx(), tray, "the shutdown-held prompt", func(tr *frontendv1.DaemonHoldTray) bool {
		for _, item := range tr.GetItems() {
			if item.GetPrompt().GetShutdown() != nil {
				return true
			}
		}
		return false
	})
	var held *frontendv1.HeldPrompt
	for _, item := range got.GetItems() {
		if item.GetPrompt().GetShutdown() != nil {
			held = item.GetPrompt()
		}
	}
	if held.GetShutdown().GetScheduleId() == "" {
		t.Fatalf("held prompt's shutdown hold = %v, want a schedule id", held.GetShutdown())
	}
	// The one submission held under the drain lease is the FIRST refusal the
	// controller ever notes, which always fires its WARN immediately
	// (internal/drain/refusals.go NoteRefusal: openedAt is zero).
	f.d.ExpectWarnings("daemon.drain.refusal")
}

// TestDrainRefusalLogsAreRateLimitedWithSuppressedAndTotalCounts is critique
// 11's second half: repeated refusals under the drain lease collapse to ONE
// WARN per rate-limit window, and the suppressed ones still count.
//
// internal/drain/refusals.go's NoteRefusal fires its WARN the instant the
// window opens (openedAt is zero), so the FIRST of a burst always emits it
// with suppressed=0; every later refusal inside DefaultRefusalWindow (one
// minute, internal/drain/controller.go — nothing wires a flag or env to
// compress it) logs the running counts at DEBUG instead, never a second WARN.
func TestDrainRefusalLogsAreRateLimitedWithSuppressedAndTotalCounts(t *testing.T) {
	t.Parallel()
	// Arrange: a drain in force, so every submission is held under the
	// shutdown lease and reported to the rate limiter.
	f := newOpened(t, harness.Opts{})
	f.d.ExpectWarnings("daemon.drain.refusal")
	tray := f.d.WatchHolds(f.ws)
	harness.AwaitNext(t, f.d.Ctx(), tray, "the empty tray")
	if _, err := f.d.Client().UpdateShutdownSchedule(f.d.Ctx(), connect.NewRequest(&agentreplv1.UpdateShutdownScheduleRequest{
		Action: &agentreplv1.UpdateShutdownScheduleRequest_Schedule{Schedule: &agentreplv1.UpdateShutdownScheduleSchedule{
			AtMs: time.Now().Add(time.Hour).UnixMilli(), Reason: drainReasonDeploy(),
		}},
	})); err != nil {
		t.Fatalf("UpdateShutdownSchedule{schedule} = error %v, want a success", err)
	}

	// Act: three submissions held under the same drain lease, all inside the
	// controller's one rate-limit window.
	for _, key := range []string{"k-rate-1", "k-rate-2", "k-rate-3"} {
		resp := f.submit("drain refusal "+key, key, conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
		if resp.GetSuccess().GetTurn().GetTurn().GetValue() == "" {
			t.Fatalf("SubmitPrompt(%s) during a drain = %v, want a minted TurnId (held, not refused)", key, resp)
		}
	}
	// Wait for the tray to carry all three held prompts, so every NoteRefusal
	// call has already landed on the run log by the time it is read below.
	harness.AwaitView(t, f.d.Ctx(), tray, "three shutdown-held prompts", func(tr *frontendv1.DaemonHoldTray) bool {
		n := 0
		for _, item := range tr.GetItems() {
			if item.GetPrompt().GetShutdown() != nil {
				n++
			}
		}
		return n == 3
	})

	// Assert: exactly one WARN, carrying the first refusal's counts.
	warns := drainRunLogRecordsAt(t, f.d, "daemon.drain.refusal", "warn")
	if len(warns) != 1 {
		t.Fatalf("daemon.drain.refusal WARN records = %d, want exactly 1 (repeated refusals inside the window collapse to DEBUG)", len(warns))
	}
	if suppressed, total := drainRefusalCounts(t, warns[0]); suppressed != 0 || total != 1 {
		t.Fatalf("the WARN record's (suppressed, total) = (%d, %d), want (0, 1): it fires on the very first refusal", suppressed, total)
	}

	// Assert: the second and third refusals are exact DEBUG records carrying
	// the running counts, never a second WARN.
	debugs := drainRunLogRecordsAt(t, f.d, "daemon.drain.refusal", "debug")
	if len(debugs) != 2 {
		t.Fatalf("daemon.drain.refusal DEBUG records = %d, want exactly 2 (the second and third refusals)", len(debugs))
	}
	if suppressed, total := drainRefusalCounts(t, debugs[0]); suppressed != 1 || total != 2 {
		t.Fatalf("the first suppressed record's (suppressed, total) = (%d, %d), want (1, 2)", suppressed, total)
	}
	if suppressed, total := drainRefusalCounts(t, debugs[1]); suppressed != 2 || total != 3 {
		t.Fatalf("the second suppressed record's (suppressed, total) = (%d, %d), want (2, 3)", suppressed, total)
	}
}

// TestTheDaemonExitsAfterTheInFlightTurnEndsDuringADrainAndNeverInterruptsTheVendor
// is the SCHEDULED drain's bargain, stated by the proto:
// UpdateShutdownScheduleSchedule is "drain then exit at/after this instant:
// finish in-flight turns, hold new prompts, exit when quiet". Waiting the turn
// out is what "finish in-flight turns" means, and the wait is the whole
// mechanism — the drain never interrupts the vendor to get there.
//
// IT DRIVES A SCHEDULE, NOT `now`. It used to send
// UpdateShutdownSchedule{now} and then assert these same schedule properties,
// which contradicts that arm's own proto sentence ("Exit now: stop accepting
// work, flush in-flight writes, go") and the drain controller's own
// ShutdownNow ("takes no lease and waits for no freeness"). It passed only
// because harness.Daemon.Exited() read the daemon's unreaped ZOMBIE as still
// running: the daemon it asserted was "still up" had already exited. `now`'s
// contract is covered by the two tests above it.
func TestTheDaemonExitsAfterTheInFlightTurnEndsDuringADrainAndNeverInterruptsTheVendor(t *testing.T) {
	t.Parallel()
	// Arrange: a turn in flight when the drain's deadline passes.
	f := newOpened(t, harness.Opts{})
	resp := f.submit("do the thing", "k-drain-now", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	if resp.GetSuccess().GetTurn().GetTurn().GetValue() == "" {
		t.Fatalf("SubmitPrompt = %v, want a minted TurnId", resp)
	}
	f.shim.ExpectStartTurn()

	// Act
	if _, err := f.d.Client().UpdateShutdownSchedule(f.d.Ctx(), connect.NewRequest(&agentreplv1.UpdateShutdownScheduleRequest{
		Action: &agentreplv1.UpdateShutdownScheduleRequest_Schedule{Schedule: &agentreplv1.UpdateShutdownScheduleSchedule{
			AtMs:   time.Now().UnixMilli(),
			Reason: drainReasonOperator("draining now"),
		}},
	})); err != nil {
		t.Fatalf("UpdateShutdownSchedule{schedule} = error %v, want a success", err)
	}

	// Assert: still up, and the vendor is never interrupted for a drain.
	if f.d.Exited() {
		t.Fatal("the daemon exited before its in-flight turn ended, want it to wait out the turn")
	}
	if got := f.shim.Count(harness.RPCKillTurn); got != 0 {
		t.Fatalf("KillTurn was called %d times during a drain, want 0: the vendor is never interrupted", got)
	}

	// Act: let the turn conclude.
	f.shim.PushAgentFrame(mainAgent, successFrame(mainAgent, nil))

	// Assert: the daemon exits once quiet.
	if code := f.d.AwaitExit(); code != 0 {
		t.Fatalf("the daemon's exit code after a drained shutdown = %d, want an orderly 0", code)
	}
	// No further submission is made once the drain fires (the turn's own
	// terminal frame is pushed, never submitted), so NoteRefusal is never
	// called and the orderly exit itself logs nothing above DEBUG.
}

// ---- Reload webapp (a deploy that finds only the webapp out of date) ----

func TestReloadWebappTriggerPushesWithNoAddress(t *testing.T) {
	t.Parallel()
	// Arrange: the merge target is the daemon's own checkout, and the deploy
	// its landing runs finds only the webapp out of date.
	selfRepo, d := drainSelfRepoDaemon(t)
	f := drainOpenWorkspace(t, d)
	host := d.WatchHost(f.ws)
	// The reload is pushed to a workspace whose WEBVIEW is open: a workspace
	// with no page has nothing to reload. The arm rides the HOST stream, which
	// is what Emacs listens on to reload the xwidget.
	web := d.WatchWeb(f.ws)
	defer web.Close()
	harness.AwaitNext(t, d.Ctx(), host, "the fresh host push")

	// Act
	drainTriggerDeploy(t, d, selfRepo, harness.DeployStaleWebapp)

	// Assert
	harness.AwaitView(t, d.Ctx(), host, "reload_webapp", func(r *agentreplv1.WatchHostWorkspaceResponse) bool {
		return r.GetReloadWebapp() != nil
	})
	// The staged build succeeds and every other component is current, so the
	// deploy records nothing above INFO.
}

// ---- Handover ----

func TestHandoverTransfersAFreeWorkspaceThroughTheAdoptionRendezvous(t *testing.T) {
	t.Parallel()
	// Arrange: an ordinary, idle workspace whose host+web streams are open at
	// the moment the handover is announced, so it is an expected participant.
	selfRepo, d := drainSelfRepoDaemon(t)
	// The sweep covers every test; the declared records are evidence of a refusal the test provokes.
	d.ExpectWarnings("daemon.refusal.unlanded_arm.standing")
	f := drainOpenWorkspace(t, d)
	f.shim.ExpectStartSession()
	f.shim.ExpectWatchSession()
	host := d.WatchHost(f.ws)
	web := d.WatchWeb(f.ws)
	harness.AwaitNext(t, d.Ctx(), host, "the fresh host push")
	daemonStream := d.WatchDaemonStream()

	// Act: land a commit on the daemon's own checkout to fire the self-merge
	// rollout.
	drainTriggerDeploy(t, d, selfRepo, harness.DeployStaleDaemon)

	// Assert: the announcement names the successor.
	announced := harness.AwaitView(t, d.Ctx(), daemonStream, "shutdown_announced", func(r *agentreplv1.WatchDaemonResponse) bool {
		return r.GetShutdownAnnounced() != nil
	}).GetShutdownAnnounced()
	if announced.GetCause().GetSelfMergeRollout() == nil {
		t.Fatalf("shutdown_announced.cause = %v, want self_merge_rollout", announced.GetCause())
	}
	// Enrichment (critique 15): a handover states the ONE bounded outage the
	// codebase knows exactly (internal/rollout/controller.go
	// DefaultExpectedOutage, wired as rollout.Deps.ExpectedOutage in
	// internal/rollout/handover.go) — unlike drain/controller.go's fire() and
	// ShutdownNow(), which never set it.
	if got, want := announced.GetExpectedOutageMs(), int64(rollout.DefaultExpectedOutage/time.Millisecond); got != want {
		t.Fatalf("shutdown_announced.expected_outage_ms for a handover = %d, want the stated %d", got, want)
	}
	if announced.GetMintedAtMs() <= 0 {
		t.Fatalf("shutdown_announced.minted_at_ms = %d, want a positive mint instant", announced.GetMintedAtMs())
	}
	addr := announced.GetAddress()
	if addr == "" {
		t.Fatal("shutdown_announced.address is unset, want the successor's address for a handover")
	}
	successor := drainDial(addr)

	// Assert: the free workspace transfers, and its watchers see the new
	// address.
	harness.AwaitView(t, d.Ctx(), host, "transferred", func(r *agentreplv1.WatchHostWorkspaceResponse) bool {
		return r.GetTransferred() != nil
	})
	webTransfer := harness.AwaitView(t, d.Ctx(), web, "transferred", func(r *agentreplv1.WatchWebWorkspaceResponse) bool {
		return r.GetTransferred() != nil
	}).GetTransferred()
	if webTransfer.GetAddress() != addr {
		t.Fatalf("WatchWebWorkspace transferred.address = %q, want the announced %q", webTransfer.GetAddress(), addr)
	}

	// Assert: a per-workspace rpc on the OLD daemon now answers
	// transferring_away naming the successor.
	oldResp, err := d.Client().SubmitPrompt(d.Ctx(), connect.NewRequest(&agentreplv1.SubmitPromptRequest{
		Workspace: f.ws, Said: said("hello"), IdempotencyKey: "k-old", Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT,
	}))
	if err != nil {
		t.Fatalf("SubmitPrompt on the old daemon after transfer = error %v, want a typed transferring_away answer", err)
	}
	if away := oldResp.Msg.GetError().GetTransferringAway(); away == nil || away.GetAddress() != addr {
		t.Fatalf("SubmitPrompt on the old daemon = %v, want error.transferring_away naming %q", oldResp.Msg, addr)
	}

	// Assert: the same rpc on the NEW daemon, before adoption, answers
	// not_yet_adopted.
	newResp, err := successor.SubmitPrompt(d.Ctx(), connect.NewRequest(&agentreplv1.SubmitPromptRequest{
		Workspace: f.ws, Said: said("hello"), IdempotencyKey: "k-new-early", Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT,
	}))
	if err != nil {
		t.Fatalf("SubmitPrompt on the new daemon before adoption = error %v, want a typed not_yet_adopted answer", err)
	}
	if newResp.Msg.GetError().GetNotYetAdopted() == nil {
		t.Fatalf("SubmitPrompt on the new daemon before adoption = %v, want error.not_yet_adopted", newResp.Msg)
	}

	// Act: both participants adopt, together.
	var wg sync.WaitGroup
	var hostAdopt *connect.Response[agentreplv1.AdoptHostWorkspaceResponse]
	var webAdopt *connect.Response[agentreplv1.AdoptWebWorkspaceResponse]
	var hostErr, webErr error
	wg.Add(2)
	go func() {
		defer wg.Done()
		hostAdopt, hostErr = successor.AdoptHostWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.AdoptHostWorkspaceRequest{Workspace: f.ws}))
	}()
	go func() {
		defer wg.Done()
		webAdopt, webErr = successor.AdoptWebWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.AdoptWebWorkspaceRequest{Workspace: f.ws}))
	}()
	wg.Wait()

	// Assert: both succeed together.
	if hostErr != nil || hostAdopt.Msg.GetSuccess() == nil {
		t.Fatalf("AdoptHostWorkspace = (%v, %v), want a success", hostAdopt, hostErr)
	}
	if webErr != nil || webAdopt.Msg.GetSuccess() == nil {
		t.Fatalf("AdoptWebWorkspace = (%v, %v), want a success", webAdopt, webErr)
	}

	// Assert: the new daemon adopted the running fake shim rather than
	// spawning a second one.
	f.shim.ExpectWatchSession()
	if got := f.shim.Count(harness.RPCStartSession); got != 1 {
		t.Fatalf("StartSession was called %d times across the handover, want exactly the original 1 (adoption dials the running shim, it never re-spawns)", got)
	}

	// Assert: the incumbent exits once every workspace has transferred, and
	// the successor claims the address file.
	if code := d.AwaitExit(); code != 0 {
		t.Fatalf("the incumbent's exit code = %d, want an orderly 0 after the last transfer", code)
	}
	drainAwaitAddrFileChange(t, d, addr)
}

func TestABusyWorkspaceIsNotTransferredUntilItsTurnEndsThenItsHeldIntakeDrainsInOrder(t *testing.T) {
	t.Parallel()
	// Arrange
	selfRepo, d := drainSelfRepoDaemon(t)
	f := drainOpenWorkspace(t, d)
	f.shim.ExpectStartSession()
	f.shim.ExpectWatchSession()
	host := d.WatchHost(f.ws)
	harness.AwaitNext(t, d.Ctx(), host, "the fresh host push")

	first := f.submit("first", "k-busy-1", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	if first.GetSuccess().GetTurn().GetTurn().GetValue() == "" {
		t.Fatalf("SubmitPrompt(first) = %v, want a minted TurnId", first)
	}
	f.shim.ExpectStartTurn()
	second := f.submit("second", "k-busy-2", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	if second.GetSuccess().GetTurn().GetTurn().GetValue() == "" {
		t.Fatalf("SubmitPrompt(second) = %v, want a minted TurnId (held for the turn's end)", second)
	}
	third := f.submit("third", "k-busy-3", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	if third.GetSuccess().GetTurn().GetTurn().GetValue() == "" {
		t.Fatalf("SubmitPrompt(third) = %v, want a minted TurnId (held for the turn's end)", third)
	}

	daemonStream := d.WatchDaemonStream()

	// Act: fire the handover while the turn is still running.
	drainTriggerDeploy(t, d, selfRepo, harness.DeployStaleDaemon)
	announced := harness.AwaitView(t, d.Ctx(), daemonStream, "shutdown_announced", func(r *agentreplv1.WatchDaemonResponse) bool {
		return r.GetShutdownAnnounced() != nil
	}).GetShutdownAnnounced()
	addr := announced.GetAddress()

	// Assert: the busy workspace is not yet transferred.
	harness.ExpectNoPush(t, host, harness.ProbeWindow, "a busy workspace transferring before its turn ends")

	// Act: let the turn conclude, freeing the workspace to transfer.
	f.shim.PushAgentFrame(mainAgent, successFrame(mainAgent, nil))

	// Assert: NOW it transfers.
	harness.AwaitView(t, d.Ctx(), host, "transferred", func(r *agentreplv1.WatchHostWorkspaceResponse) bool {
		return r.GetTransferred() != nil
	})

	// Act: adopt on the successor.
	successor := drainDial(addr)
	if _, err := successor.AdoptHostWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.AdoptHostWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("AdoptHostWorkspace = error %v, want a success", err)
	}
	if _, err := successor.AdoptWebWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.AdoptWebWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("AdoptWebWorkspace = error %v, want a success", err)
	}

	// Assert: the two held prompts drain FIFO onto the adopted (running) shim.
	req1 := f.shim.ExpectStartTurn()
	// ONE TURN AT A TIME: the third prompt was held for the running turn's
	// end, so it waits for the second's end on the successor exactly as it
	// would have on the incumbent.
	if got := f.shim.Count(harness.RPCStartTurn); got != 2 {
		t.Fatalf("StartTurn count = %d while the second prompt's turn runs, want 2 (the third waits for it)", got)
	}
	f.shim.PushAgentFrame(mainAgent, successFrame(mainAgent, nil))
	req2 := f.shim.ExpectStartTurn()
	if text(req1.GetSaid()) != "second" || text(req2.GetSaid()) != "third" {
		t.Fatalf("held intake drained as (%q, %q), want (\"second\", \"third\") in order", text(req1.GetSaid()), text(req2.GetSaid()))
	}
}

// TestARestartJoiningABusyWorkspacesTransferStillTransfersIt pins the
// 2026-09-27 regression at the daemon's own surface: a handover's transfer was
// registered behind a busy workspace, the restart verb joined that registered
// bounce, and the newest action won -- the restart ran on the outgoing daemon
// and the workspace was never sent its transfer notice, so its host stream died
// with the daemon. Both now run when the turn ends: the restart, then the
// transfer, whose notice reaches the host stream.
//
// THE RESTART IS GRACEFUL ON PURPOSE. A forced one interrupts the turn first,
// and whether that turn's end reaches the registry before the restart's own
// request does is a race: the transfer can start alone and the restart then
// joins a RUNNING move. A graceful restart registers beside the transfer while
// the turn still runs, which is exactly the coalescing under test.
func TestARestartJoiningABusyWorkspacesTransferStillTransfersIt(t *testing.T) {
	t.Parallel()
	// Arrange: a busy workspace whose transfer is registered behind its turn.
	selfRepo, d := drainSelfRepoDaemon(t)
	// The restart's trail: the stand-down the fake shim ends by exiting, and
	// the shim link that dies with it.
	d.ExpectWarnings("daemon.shimclient.redial", "daemon.sessionwatcher.reopen", "daemon.health.open_fault", "daemon.sessionwatcher.watch_session",
		"daemon.rollout.relaunch", "daemon.sessionwatcher.link_fault",
		"daemon.sessionwatcher.watch_agent", "daemon.shimclient.exit", "daemon.shimclient.kill_session")
	f := drainOpenWorkspace(t, d)
	f.shim.ExpectStartSession()
	f.shim.ExpectWatchSession()
	host := d.WatchHost(f.ws)
	harness.AwaitNext(t, d.Ctx(), host, "the fresh host push")
	f.submit("long running work", "k-coalesced-busy", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	f.shim.ExpectStartTurn()
	daemonStream := d.WatchDaemonStream()
	drainTriggerDeploy(t, d, selfRepo, harness.DeployStaleDaemon)
	harness.AwaitView(t, d.Ctx(), daemonStream, "shutdown_announced", func(r *agentreplv1.WatchDaemonResponse) bool {
		return r.GetShutdownAnnounced() != nil
	})
	registered := func(reason, what string) {
		d.AwaitWorkspaceLogRecord(f.ws.GetDir(), what, func(r harness.LogRecord) bool {
			return r.PID == d.PID() && r.Operation == "daemon.promptqueue.bounce" &&
				r.Message == "the workspace has work in flight; registered the bounce for when it ends" &&
				r.Context["reason"] == reason
		})
	}
	registered("handover_transfer", "the transfer registered behind the turn")
	resp, err := d.Client().RestartWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.RestartWorkspaceRequest{Workspace: f.ws, Force: false}))
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("RestartWorkspace{force:false} = (%v, %v), want a success", resp, err)
	}
	registered("restart_verb", "the restart registered beside the transfer")

	// Act: the turn ends, freeing the workspace for the coalesced bounce.
	f.shim.PushAgentFrame(mainAgent, successFrame(mainAgent, nil))

	// Assert: the transfer notice still came, after the restart ran as the
	// bounce's first stage.
	harness.AwaitView(t, d.Ctx(), host, "transferred", func(r *agentreplv1.WatchHostWorkspaceResponse) bool {
		return r.GetTransferred() != nil
	})
	d.AwaitWorkspaceLogRecord(f.ws.GetDir(), "the restart stage finishing ahead of the transfer", func(r harness.LogRecord) bool {
		return r.PID == d.PID() && r.Operation == "daemon.promptqueue.bounce" &&
			r.Message == "a stage of the bounce finished; the workspace stays draining for the stage after it"
	})
}

func TestASuccessorDoesNotAdoptABusyWorkspaceBeforeTheIncumbentTransfersIt(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name   string
		prompt string
		key    string
	}{
		{name: "an adoption call arriving during a turn waits for transfer", prompt: "still running", key: "k-adopt-before-free"},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			t.Parallel()
			// Arrange: one host participant calls the successor while the
			// workspace's turn still holds the incumbent behind its freeness
			// gate.
			selfRepo, d := drainSelfRepoDaemon(t)
			f := drainOpenWorkspace(t, d)
			f.shim.ExpectStartSession()
			f.shim.ExpectWatchSession()
			watchesBefore := f.shim.Count(harness.RPCWatchSession)
			host := d.WatchHost(f.ws)
			harness.AwaitNext(t, d.Ctx(), host, "the fresh host push")
			if got := f.submit(test.prompt, test.key, conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT); got.GetSuccess() == nil {
				t.Fatalf("SubmitPrompt = %v, want the turn accepted", got)
			}
			f.shim.ExpectStartTurn()
			daemonStream := d.WatchDaemonStream()
			drainTriggerDeploy(t, d, selfRepo, harness.DeployStaleDaemon)
			announced := harness.AwaitView(t, d.Ctx(), daemonStream, "shutdown_announced", func(r *agentreplv1.WatchDaemonResponse) bool {
				return r.GetShutdownAnnounced() != nil
			}).GetShutdownAnnounced()
			successor := drainDial(announced.GetAddress())

			// Act: this is the early call the webapp-layer handover driver
			// makes as soon as the successor address is announced.
			adopted := make(chan *connect.Response[agentreplv1.AdoptHostWorkspaceResponse], 1)
			adoptFailed := make(chan error, 1)
			go func() {
				resp, err := successor.AdoptHostWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.AdoptHostWorkspaceRequest{Workspace: f.ws}))
				adopted <- resp
				adoptFailed <- err
			}()

			// Assert: elapsed time cannot start successor ownership while the
			// incumbent is still serving. A new WatchSession would prove the
			// successor dialed and adopted the live shim too early.
			expectRPCCount(t, f.shim, harness.RPCWatchSession, watchesBefore, harness.ProbeWindow)

			// Act: the turn terminal releases freeness; the incumbent
			// quiesces, detaches and clears serving ownership before the
			// successor may proceed.
			f.shim.PushAgentFrame(mainAgent, successFrame(mainAgent, nil))
			harness.AwaitView(t, d.Ctx(), host, "transferred", func(r *agentreplv1.WatchHostWorkspaceResponse) bool {
				return r.GetTransferred() != nil
			})

			// Assert: the pending adoption now succeeds and opens exactly the
			// successor's session watch.
			resp := <-adopted
			if err := <-adoptFailed; err != nil {
				t.Fatalf("AdoptHostWorkspace after incumbent transfer = error %v, want success", err)
			}
			if resp.Msg.GetSuccess() == nil {
				t.Fatalf("AdoptHostWorkspace after incumbent transfer = %v, want success", resp.Msg)
			}
			ftAwaitTrue(t, d.Ctx(), func() bool {
				return f.shim.Count(harness.RPCWatchSession) > watchesBefore
			}, "the successor's WatchSession after serving ownership was released")
		})
	}
}

// TestANeverFreeHandoverEmitsAPeriodicWarningNamingTheHoldout is critique 11's
// first half: a workspace that never falls free leaves both daemons up
// forever, naming the holdout in a periodic WARN
// (internal/rollout/handover.go awaitFreeForever, operation
// daemon.rollout.transfer). The production cadence is ten minutes; the
// AGENT_REPL_HOLDOUT_WARN_EVERY test knob compresses it so the warning is
// observable inside a bounded window.
func TestANeverFreeHandoverEmitsAPeriodicWarningNamingTheHoldout(t *testing.T) {
	t.Parallel()
	// Arrange: a daemon whose holdout cadence is milliseconds, with a
	// workspace held busy by a turn that never concludes.
	selfRepo := harness.NewRepo(t)
	script := harness.NewTestAllScript(t, selfRepo.Dir)
	script.SetExitCode(0)
	script.SetStdout("daemon: passed in 1s\n")
	d := harness.StartDaemon(t, harness.Opts{
		SelfRepo: selfRepo.Dir,
		ExtraEnv: []string{
			"AGENT_REPL_TEST_ALL_SCRIPT=" + script.Path,
			"AGENT_REPL_HOLDOUT_WARN_EVERY=25ms",
		},
		// This test's own drainTriggerDeploy call boots a real successor
		// within THIS daemon's one context (see drainSelfRepoDaemon), so it
		// needs the same longer, justified bound.
		Timeout: harness.HandoverChainTimeout,
	})
	f := drainOpenWorkspace(t, d)
	f.shim.ExpectStartSession()
	f.shim.ExpectWatchSession()
	host := d.WatchHost(f.ws)
	harness.AwaitNext(t, d.Ctx(), host, "the fresh host push")
	if got := f.submit("forever", "k-holdout-1", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT); got.GetSuccess() == nil {
		t.Fatalf("SubmitPrompt = %v, want the turn accepted", got)
	}
	f.shim.ExpectStartTurn()
	// THE INCUMBENT'S OWN RUN LOG. Successor and incumbent append to the same
	// size-rotated file, so the predicate pins the incumbent's pid.
	incumbentLog := d.RunLogPath()

	// Act: hand over while the turn is still in flight, and never end it.
	drainTriggerDeploy(t, d, selfRepo, harness.DeployStaleDaemon)

	// Assert: the holdout is named in a periodic warning that repeats.
	second := d.AwaitLogRecord(incumbentLog, "the second holdout warning", func(r harness.LogRecord) bool {
		return r.PID == d.PID() && r.Operation == "daemon.rollout.handover" && strings.ToLower(r.Level) == "warn" &&
			namesHoldout(r.Context["holdouts"], f.ws.GetId()) && numeric(r.Context["warnings"]) >= 2
	})
	if got := second.Context["cadence"]; got != "25ms" {
		t.Fatalf("the holdout warning's cadence = %v, want the 25ms the knob set", got)
	}
	d.ExpectWarnings("daemon.rollout.handover")
}

// namesHoldout reports whether a JSON-decoded holdout list names ws.
func namesHoldout(v any, ws string) bool {
	list, _ := v.([]any)
	for _, item := range list {
		if item == ws {
			return true
		}
	}
	return false
}

// numeric reads a JSON-decoded log context number, which arrives as float64.
func numeric(v any) float64 {
	f, _ := v.(float64)
	return f
}

func TestAHeadlessWorkspaceTransfersWithoutAnyAdoptCall(t *testing.T) {
	t.Parallel()
	// Arrange: registered, never opened — no host or web stream ever existed
	// for it, so it has zero rendezvous participants.
	selfRepo, d := drainSelfRepoDaemon(t)
	repo := harness.NewRepo(t)
	ws := harness.Register(t, d, repo.Dir)
	roster := d.WatchRoster()
	harness.AwaitNext(t, d.Ctx(), roster, "the roster with the headless row")
	daemonStream := d.WatchDaemonStream()

	// Act
	drainTriggerDeploy(t, d, selfRepo, harness.DeployStaleDaemon)
	announced := harness.AwaitView(t, d.Ctx(), daemonStream, "shutdown_announced", func(r *agentreplv1.WatchDaemonResponse) bool {
		return r.GetShutdownAnnounced() != nil
	}).GetShutdownAnnounced()
	addr := announced.GetAddress()

	// Assert: the incumbent exits once the handover completes, having
	// transferred the headless workspace with no adopt call for it.
	if code := d.AwaitExit(); code != 0 {
		t.Fatalf("the incumbent's exit code = %d, want an orderly 0", code)
	}
	drainAwaitAddrFileChange(t, d, addr)

	// Assert: the workspace is usable on the successor without ever having
	// been the subject of AdoptHostWorkspace/AdoptWebWorkspace.
	successor := drainDial(addr)
	if _, err := successor.SelectWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.SelectWorkspaceRequest{Workspace: ws})); err != nil {
		t.Fatalf("SelectWorkspace on the successor for a headless workspace = error %v, want a success", err)
	}
}

// TestAdoptWebWorkspaceRefusesParticipantNotExpectedForAClientNotOpenAtAnnouncement
// is critique 14's first arm: a client whose stream was NOT open at the
// instant the handover was announced is refused with participant_not_expected
// when it tries to join the rendezvous — verified against
// AdoptWebWorkspaceParticipantNotExpected
// (proto/src/agentrepl/v1/endpoint_adopt_web_workspace.proto) and
// rollout.ErrParticipantNotExpected (internal/rollout/adopt.go).
func TestAdoptWebWorkspaceRefusesParticipantNotExpectedForAClientNotOpenAtAnnouncement(t *testing.T) {
	t.Parallel()
	// Arrange: only the HOST stream is open when the handover fires, so the
	// manifest's ExpectedWeb is false (internal/rollout/handover.go: the
	// snapshot is taken at announcement) — no web participant was ever
	// expected for this workspace.
	selfRepo, d := drainSelfRepoDaemon(t)
	// The sweep covers every test; the declared records are evidence of the adoption refusal the test provokes.
	d.ExpectWarnings("daemon.rollout.adopt_web")
	f := drainOpenWorkspace(t, d)
	f.shim.ExpectStartSession()
	f.shim.ExpectWatchSession()
	host := d.WatchHost(f.ws)
	harness.AwaitNext(t, d.Ctx(), host, "the fresh host push")

	// THE DAEMON STREAM IS SUBSCRIBED BEFORE THE HANDOVER IS FIRED. The
	// daemon-level push topic replays only its own process's latest value to
	// new subscribers, and the incumbent tears that process down as the
	// handover completes -- so a subscription opened after the trigger races
	// the announcement it is waiting for and, under load, opens too late to
	// ever see it. Subscribing first is the rendezvous, exactly as the
	// headless-transfer test above does it.
	daemonStream := d.WatchDaemonStream()

	// Act: fire the handover with no web stream ever opened.
	drainTriggerDeploy(t, d, selfRepo, harness.DeployStaleDaemon)
	announced := harness.AwaitView(t, d.Ctx(), daemonStream, "shutdown_announced", func(r *agentreplv1.WatchDaemonResponse) bool {
		return r.GetShutdownAnnounced() != nil
	}).GetShutdownAnnounced()
	// THE ANNOUNCEMENT PRECEDES THE MANIFEST. Handover spawns, announces,
	// snapshots the participants and only THEN writes the intent manifest
	// (internal/rollout/handover.go), and the successor arms its rendezvous
	// from that manifest — so an adopt call made the instant shutdown_announced
	// lands reaches a successor with nothing to arm from and is refused
	// no_transfer_announced, which is the OTHER arm of this same endpoint.
	//
	// THE EDGE IS THE SUCCESSOR'S ARMING, NEVER THE MANIFEST'S EXISTENCE. The
	// successor polls for the manifest, arms from it and at once RETIRES it
	// (internal/rollout/adopt.go joinFromManifest): a failing run measured the
	// file installed at 36.376 and removed at 36.379, a 3ms lifetime a 5ms
	// existence poll could miss entirely and then wait out its whole bound for
	// a file that would never come back. The arming record is written by the
	// successor into the run log its own boot opened, so the incumbent's
	// rotated-inode hazard does not reach it, and it is written only once the
	// rendezvous this call needs is armed.
	d.AwaitRunLogRecordFromAnyProcess("the successor's rendezvous armed from the intent manifest", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.rollout.join" && strings.HasPrefix(r.Message, "armed the adopt rendezvous from ")
	})
	successor := drainDial(announced.GetAddress())

	// Act: the never-open web client attempts to join anyway.
	resp, err := successor.AdoptWebWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.AdoptWebWorkspaceRequest{Workspace: f.ws}))

	// Assert
	if err != nil {
		t.Fatalf("AdoptWebWorkspace for a client not open at announcement = error %v, want a typed participant_not_expected answer", err)
	}
	if resp.Msg.GetError().GetParticipantNotExpected() == nil {
		t.Fatalf("AdoptWebWorkspace for a client not open at announcement = %v, want error.participant_not_expected", resp.Msg)
	}
}

// TestAdoptWebWorkspaceRefusesNoTransferAnnouncedOnAPlainBootLoggedAtInfo is
// critique 14's second arm: AdoptWebWorkspace on a plain boot — no handover
// ever announced — is refused with no_transfer_announced, and
// internal/server/adopt.go marks this refusal Info (the ORDINARY case on
// every non-handover page boot), never WARN.
func TestAdoptWebWorkspaceRefusesNoTransferAnnouncedOnAPlainBootLoggedAtInfo(t *testing.T) {
	t.Parallel()
	// Arrange: an ordinary opened workspace; no handover was ever announced on
	// this daemon.
	f := newOpened(t, harness.Opts{})

	// Act
	resp, err := f.d.Client().AdoptWebWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.AdoptWebWorkspaceRequest{Workspace: f.ws}))

	// Assert: the typed refusal.
	if err != nil {
		t.Fatalf("AdoptWebWorkspace on a plain boot = error %v, want a typed no_transfer_announced answer", err)
	}
	if resp.Msg.GetError().GetNoTransferAnnounced() == nil {
		t.Fatalf("AdoptWebWorkspace on a plain boot = %v, want error.no_transfer_announced", resp.Msg)
	}

	// Assert: logged at INFO, naming the arm, never WARN. AdoptWebWorkspace is
	// a PER-WORKSPACE rpc, so its refusal record goes to that workspace's own
	// sink — a record about one workspace in the global run log is the
	// invariant violation the logging contract names.
	rec := f.d.AwaitWorkspaceLogOperation(f.ws.GetDir(), "AdoptWebWorkspace")
	if strings.ToLower(rec.Level) != "info" {
		t.Fatalf("AdoptWebWorkspace's no_transfer_announced refusal logged at %q, want info", rec.Level)
	}
	if got := rec.Context["arm"]; got != "no_transfer_announced" {
		t.Fatalf("AdoptWebWorkspace refusal record's arm = %v, want no_transfer_announced", got)
	}
}

// ---- Drain schedule durability ----

// TestDrainScheduleSurvivesARestartAndTheStandingBannerReappears is
// critique 11: the CONTRACT is that the standing drain banner (drain_scheduled)
// reappears on a fresh WatchDaemon subscription after a restart on the same
// state root — a client that reconnects after the daemon bounces must not
// silently lose a schedule that is still in force.
//
// THIS TEST IS EXPECTED TO BE RED. wsm.DB persists the schedule
// (PutDrainSchedule/DrainSchedule, internal/drain/api.go), and
// internal/drain/sweep.go's Run DOES read DB.DrainSchedule(ctx) on every loop
// iteration including the very first one after boot — but it only ACTS on a
// schedule whose deadline has already passed (calling fire, which announces).
// A schedule still in the future is silently absorbed into Run's wait
// calculation and never handed to Announcer.DrainScheduled. The daemon-level
// push topic (internal/server/api.go daemonTopic, a publish.Topic that replays
// only its own process's latest value to new subscribers) is therefore EMPTY
// on a fresh process until something re-announces — and nothing does: there is
// no boot caller of DrainSchedule()/Current() anywhere in
// cmd/claude-repld/graph.go (where drainController is built, ~line 385) or
// internal/boot/sequence.go (which never mentions drain at all). The missing
// call site is exactly there: after drain.New in graph.go, nothing reads the
// persisted schedule and forwards it to the Announcer before the server starts
// serving.
func TestDrainScheduleSurvivesARestartAndTheStandingBannerReappears(t *testing.T) {
	t.Parallel()
	// Arrange: schedule a drain far enough out that it never fires during this
	// test, and confirm it is standing before the restart.
	d1 := newDaemon(t, harness.Opts{})
	stream1 := d1.WatchDaemonStream()
	if _, err := d1.Client().UpdateShutdownSchedule(d1.Ctx(), connect.NewRequest(&agentreplv1.UpdateShutdownScheduleRequest{
		Action: &agentreplv1.UpdateShutdownScheduleRequest_Schedule{Schedule: &agentreplv1.UpdateShutdownScheduleSchedule{
			AtMs: time.Now().Add(time.Hour).UnixMilli(), Reason: drainReasonDeploy(),
		}},
	})); err != nil {
		t.Fatalf("UpdateShutdownSchedule{schedule} = error %v, want a success", err)
	}
	harness.AwaitView(t, d1.Ctx(), stream1, "drain_scheduled", func(r *agentreplv1.WatchDaemonResponse) bool {
		return r.GetDrainScheduled() != nil
	})
	d1.Stop()

	// Act: restart on the same state root, then open a BRAND NEW subscription
	// (standing in for a reconnecting Emacs or webview after the bounce).
	d2 := harness.StartDaemon(t, harness.Opts{StateDir: d1.StateDir})
	stream2 := d2.WatchDaemonStream()

	// Assert: the contract — the standing banner reappears. Bounded to a short
	// probe rather than the harness's full DefaultTimeout, since this is
	// expected to time out rather than succeed.
	probeCtx, cancel := context.WithTimeout(d2.Ctx(), 2*time.Second)
	defer cancel()
	harness.AwaitView(t, probeCtx, stream2, "drain_scheduled reappearing after a restart", func(r *agentreplv1.WatchDaemonResponse) bool {
		return r.GetDrainScheduled() != nil
	})
}

// ---- Asset origin ----

func TestAssetOriginServesIndexWithNoStoreCacheControl(t *testing.T) {
	t.Parallel()
	// Arrange
	d := newDaemon(t, harness.Opts{})

	// Act
	resp, err := d.HTTP().Get("http://" + d.Addr + "/")

	// Assert
	if err != nil {
		t.Fatalf("GET / = error %v, want the served index.html", err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("GET / = %d, want 200", resp.StatusCode)
	}
	if got := resp.Header.Get("Cache-Control"); got != "no-store" {
		t.Fatalf("GET / Cache-Control = %q, want %q", got, "no-store")
	}
}

func TestRewritingIndexHtmlOnDiskIsServedOnTheNextRequestWithoutRestart(t *testing.T) {
	t.Parallel()
	// Arrange
	d := newDaemon(t, harness.Opts{})
	indexPath := d.WebappDir + "/index.html"

	// Act
	if err := os.WriteFile(indexPath, []byte("<!doctype html><title>rewritten</title>"), 0o644); err != nil {
		t.Fatalf("rewrite index.html: %v", err)
	}
	resp, err := d.HTTP().Get("http://" + d.Addr + "/")

	// Assert
	if err != nil {
		t.Fatalf("GET / after rewriting index.html = error %v", err)
	}
	defer resp.Body.Close()
	body := make([]byte, 4096)
	n, _ := resp.Body.Read(body)
	if !strings.Contains(string(body[:n]), "rewritten") {
		t.Fatalf("GET / body = %q, want the rewritten index.html served without a daemon restart", body[:n])
	}
}

func TestAssetsAreServedWithoutNoStore(t *testing.T) {
	t.Parallel()
	// Arrange
	d := newDaemon(t, harness.Opts{})

	// Act
	resp, err := d.HTTP().Get("http://" + d.Addr + "/assets/app.js")

	// Assert
	if err != nil {
		t.Fatalf("GET /assets/app.js = error %v, want the served asset", err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("GET /assets/app.js = %d, want 200", resp.StatusCode)
	}
	if got := resp.Header.Get("Cache-Control"); got == "no-store" {
		t.Fatalf("GET /assets/app.js Cache-Control = %q, want the entry point's no-store NOT applied to a hashed asset", got)
	}
}

// ---- drain/rollout helpers (prefixed drain* so they cannot collide) ----

// drainReasonDeploy builds the deploy-tooling drain reason.
func drainReasonDeploy() *agentreplv1.DrainReason {
	return &agentreplv1.DrainReason{Kind: &agentreplv1.DrainReason_Deploy{Deploy: &agentreplv1.DrainReasonDeploy{}}}
}

// drainReasonOperator builds an operator-supplied drain reason.
func drainReasonOperator(note string) *agentreplv1.DrainReason {
	return &agentreplv1.DrainReason{Kind: &agentreplv1.DrainReason_Operator{Operator: &agentreplv1.DrainReasonOperator{Note: note}}}
}

// drainOpenWorkspace registers and opens a fresh repository's workspace on an
// already-running daemon, mirroring newOpened without minting a new daemon.
func drainOpenWorkspace(t *testing.T, d *harness.Daemon) *fixture {
	t.Helper()
	repo := harness.NewRepo(t)
	ws := harness.Register(t, d, repo.Dir)
	f := &fixture{d: d, repo: repo, ws: ws, t: t}
	f.open()
	return f
}

// drainSelfRepoDaemon starts a daemon whose OWN checkout is a fresh fake
// repository, with a PASSING test gate.
//
// The gate matters: the deploy runs only off a merge that LANDED, and a merge
// whose gate fails opens the fixes tab instead. Without the override the gate
// runs the repository's own bin/test-all.sh, which a fake repository does not
// have, and every landing died at exit 127.
func drainSelfRepoDaemon(t *testing.T) (*harness.Repo, *harness.Daemon) {
	t.Helper()
	selfRepo := harness.NewRepo(t)
	script := harness.NewTestAllScript(t, selfRepo.Dir)
	script.SetExitCode(0)
	script.SetStdout("daemon: passed in 1s\n")
	d := harness.StartDaemon(t, harness.Opts{
		SelfRepo: selfRepo.Dir,
		ExtraEnv: []string{"AGENT_REPL_TEST_ALL_SCRIPT=" + script.Path},
		// Every caller of this fixture drives a real self-reload handover
		// (drainTriggerDeploy): a second real claude-repld boots and adopts
		// every workspace within THIS daemon's one context, never a fresh one
		// of its own, so it gets HandoverChainTimeout rather than the tighter
		// single-boot default.
		Timeout: harness.HandoverChainTimeout,
	})
	return selfRepo, d
}

// drainTriggerDeploy stages a deploy build that changes one component, then
// lands one commit on the daemon's own checkout (selfRepo): the landing runs
// the daemon's ONE deploy for it, which finds that component out of date.
func drainTriggerDeploy(t *testing.T, d *harness.Daemon, selfRepo *harness.Repo, build harness.DeployBuild) {
	t.Helper()
	d.StageDeployBuild(build)
	path := "modules/app/agent-repl/daemon/cmd/claude-repld/main.go"
	// The trigger workspace is CREATED, never merely registered: a merge runs
	// off the creation job's recorded geometry, and a registered worktree has
	// none, so a registered one is refused with no_layout_facts and no merge
	// ever lands to run the deploy.
	repoRef := mergeRepositoryRef(t, d, selfRepo)
	f := mergeCreateChild(t, d, repoRef, "trigger", "trigger work", nil)
	sha := writeCommit(t, selfRepo, f.ws.GetDir(), path, "trigger\n")
	selfRepo.SetPaths(sha, path)
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("MergeWorkspace(trigger) = error %v, want the merge enqueued and landed", err)
	}
}

// drainAwaitAddrFileChange polls daemon.addr until it holds exactly `want`,
// bounded by the daemon's own context — never a fixed sleep.
func drainAwaitAddrFileChange(t *testing.T, d *harness.Daemon, want string) {
	t.Helper()
	ticker := time.NewTicker(5 * time.Millisecond)
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

// drainDial builds a raw Connect client against an arbitrary address, for the
// tests that must reach a handover successor before it is discoverable any
// other way (its address rides the announcement, never a harness field).
func drainDial(addr string) agentreplv1connect.AgentReplClient {
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
