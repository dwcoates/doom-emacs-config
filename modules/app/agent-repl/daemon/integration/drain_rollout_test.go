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

// TestScheduledDrainFiresAndAnnouncesShutdownWithTheScheduledDrainCause is
// critique 15: strengthens the shutdown-announcement assertions onto the ONE
// cause no other test in this file reaches — a schedule that actually fires
// (every other shutdown_announced assertion here is either `now` or a
// self-merge handover). A headless daemon has nothing to wait free, so the
// drain loop (internal/drain/sweep.go Run, which caps its wait at the
// schedule's own deadline regardless of the 5-minute sweep cadence) fires the
// instant the deadline passes.
func TestScheduledDrainFiresAndAnnouncesShutdownWithTheScheduledDrainCause(t *testing.T) {
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

// ---- Drain intake and exit ----

func TestDuringADrainNewPromptsAreHeldWithTheShutdownHold(t *testing.T) {
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

func TestTheDaemonExitsAfterTheInFlightTurnEndsDuringADrainAndNeverInterruptsTheVendor(t *testing.T) {
	// Arrange: a turn in flight when the drain fires now.
	f := newOpened(t, harness.Opts{})
	resp := f.submit("do the thing", "k-drain-now", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	if resp.GetSuccess().GetTurn().GetTurn().GetValue() == "" {
		t.Fatalf("SubmitPrompt = %v, want a minted TurnId", resp)
	}
	f.shim.ExpectStartTurn()

	// Act
	if _, err := f.d.Client().UpdateShutdownSchedule(f.d.Ctx(), connect.NewRequest(&agentreplv1.UpdateShutdownScheduleRequest{
		Action: &agentreplv1.UpdateShutdownScheduleRequest_Now{Now: &agentreplv1.UpdateShutdownScheduleNow{Reason: drainReasonOperator("draining now")}},
	})); err != nil {
		t.Fatalf("UpdateShutdownSchedule{now} = error %v, want a success", err)
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

// ---- Reload webapp (webapp-only rollout) ----

func TestReloadWebappTriggerPushesWithNoAddress(t *testing.T) {
	// Arrange: the merge target is the daemon's own checkout; a landed
	// commit touching only the webapp subsystem classifies as webapp-only.
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
	drainTriggerRollout(t, d, selfRepo, "modules/app/agent-repl/webapp/src/App.tsx")

	// Assert
	harness.AwaitView(t, d.Ctx(), host, "reload_webapp", func(r *agentreplv1.WatchHostWorkspaceResponse) bool {
		return r.GetReloadWebapp() != nil
	})
	// A single-subsystem (webapp-only) landed range classifies cleanly and the
	// harness's own fake deploy script succeeds, so rollout.Trigger
	// (internal/rollout/trigger.go) never reaches its opClassify/opDeploy WARNs.
}

// ---- Handover ----

func TestHandoverTransfersAFreeWorkspaceThroughTheAdoptionRendezvous(t *testing.T) {
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
	drainTriggerRollout(t, d, selfRepo, "modules/app/agent-repl/daemon/cmd/claude-repld/main.go")

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
	drainTriggerRollout(t, d, selfRepo, "modules/app/agent-repl/daemon/cmd/claude-repld/main.go")
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
	req2 := f.shim.ExpectStartTurn()
	if text(req1.GetSaid()) != "second" || text(req2.GetSaid()) != "third" {
		t.Fatalf("held intake drained as (%q, %q), want (\"second\", \"third\") in order", text(req1.GetSaid()), text(req2.GetSaid()))
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
		// This test's own drainTriggerRollout call boots a real successor
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
	// THE INCUMBENT'S OWN RUN LOG. The run log is restart-scoped: the
	// successor's boot rotates the incumbent's file into slot 1 while the
	// incumbent goes on writing through the same descriptor, so the holdout
	// warnings land in daemon.run.log.1 rather than the canonical path.
	incumbentLog := d.RunLogPath() + ".1"

	// Act: hand over while the turn is still in flight, and never end it.
	drainTriggerRollout(t, d, selfRepo, "modules/app/agent-repl/daemon/cmd/claude-repld/main.go")

	// Assert: the holdout is named in a periodic warning that repeats.
	second := d.AwaitLogRecord(incumbentLog, "the second holdout warning", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.rollout.transfer" && strings.ToLower(r.Level) == "warn" &&
			r.Context["workspace"] == f.ws.GetId() && numeric(r.Context["warnings"]) >= 2
	})
	if got := second.Context["cadence"]; got != "25ms" {
		t.Fatalf("the holdout warning's cadence = %v, want the 25ms the knob set", got)
	}
	d.ExpectWarnings("daemon.rollout.transfer")
}

// numeric reads a JSON-decoded log context number, which arrives as float64.
func numeric(v any) float64 {
	f, _ := v.(float64)
	return f
}

func TestAHeadlessWorkspaceTransfersWithoutAnyAdoptCall(t *testing.T) {
	// Arrange: registered, never opened — no host or web stream ever existed
	// for it, so it has zero rendezvous participants.
	selfRepo, d := drainSelfRepoDaemon(t)
	repo := harness.NewRepo(t)
	ws := harness.Register(t, d, repo.Dir)
	roster := d.WatchRoster()
	harness.AwaitNext(t, d.Ctx(), roster, "the roster with the headless row")
	daemonStream := d.WatchDaemonStream()

	// Act
	drainTriggerRollout(t, d, selfRepo, "modules/app/agent-repl/daemon/cmd/claude-repld/main.go")
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
	drainTriggerRollout(t, d, selfRepo, "modules/app/agent-repl/daemon/cmd/claude-repld/main.go")
	announced := harness.AwaitView(t, d.Ctx(), daemonStream, "shutdown_announced", func(r *agentreplv1.WatchDaemonResponse) bool {
		return r.GetShutdownAnnounced() != nil
	}).GetShutdownAnnounced()
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
	d.WatchWorkspaceLogs(repo.Dir)
	f := &fixture{d: d, repo: repo, ws: ws, t: t}
	f.open()
	return f
}

// drainTriggerRollout lands one commit touching the given path on the
// daemon's own checkout (selfRepo), which fires rollout.Trigger classified
// by that path's subsystem prefix.
// drainSelfRepoDaemon starts a daemon whose OWN checkout is a fresh fake
// repository, with a PASSING test gate.
//
// The gate matters: the rollout fires only off a merge that LANDED, and a
// merge whose gate fails opens the fixes tab instead. Without the override the
// gate runs the repository's own bin/test-all.sh, which a fake repository does
// not have, and every rollout trigger died at exit 127.
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
		// (drainTriggerRollout): a second real claude-repld boots and adopts
		// every workspace within THIS daemon's one context, never a fresh one
		// of its own, so it gets HandoverChainTimeout rather than the tighter
		// single-boot default.
		Timeout: harness.HandoverChainTimeout,
	})
	return selfRepo, d
}

func drainTriggerRollout(t *testing.T, d *harness.Daemon, selfRepo *harness.Repo, path string) {
	t.Helper()
	// The trigger workspace is CREATED, never merely registered: a merge runs
	// off the creation job's recorded geometry, and a registered worktree has
	// none, so a registered one is refused with no_layout_facts and no merge
	// ever lands to trigger the rollout.
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
		if body, err := os.ReadFile(d.AddrFile()); err == nil && strings.TrimSuffix(string(body), "\n") == want {
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
