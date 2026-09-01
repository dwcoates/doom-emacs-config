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
	if announced.GetCause().GetImmediate() == nil {
		t.Fatalf("shutdown_announced.cause = %v, want immediate", announced.GetCause())
	}
	if announced.Address != nil {
		t.Fatalf("shutdown_announced.address = %q, want unset (a plain bounce, no successor)", announced.GetAddress())
	}
	d.ExpectWarnings(harness.AllowAllWarnings)
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
	f.d.ExpectWarnings(harness.AllowAllWarnings)
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
	f.d.ExpectWarnings(harness.AllowAllWarnings)
}

// ---- Reload webapp (webapp-only rollout) ----

func TestReloadWebappTriggerPushesWithNoAddress(t *testing.T) {
	// Arrange: the merge target is the daemon's own checkout; a landed
	// commit touching only the webapp subsystem classifies as webapp-only.
	selfRepo := harness.NewRepo(t)
	d := harness.StartDaemon(t, harness.Opts{SelfRepo: selfRepo.Dir})
	f := drainOpenWorkspace(t, d)
	host := d.WatchHost(f.ws)
	harness.AwaitNext(t, d.Ctx(), host, "the fresh host push")

	// Act
	drainTriggerRollout(t, d, selfRepo, "modules/app/agent-repl/webapp/src/App.tsx")

	// Assert
	harness.AwaitView(t, d.Ctx(), host, "reload_webapp", func(r *agentreplv1.WatchHostWorkspaceResponse) bool {
		return r.GetReloadWebapp() != nil
	})
	d.ExpectWarnings(harness.AllowAllWarnings)
}

// ---- Handover ----

func TestHandoverTransfersAFreeWorkspaceThroughTheAdoptionRendezvous(t *testing.T) {
	// Arrange: an ordinary, idle workspace whose host+web streams are open at
	// the moment the handover is announced, so it is an expected participant.
	selfRepo := harness.NewRepo(t)
	d := harness.StartDaemon(t, harness.Opts{SelfRepo: selfRepo.Dir})
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
	selfRepo := harness.NewRepo(t)
	d := harness.StartDaemon(t, harness.Opts{SelfRepo: selfRepo.Dir})
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

func TestAHeadlessWorkspaceTransfersWithoutAnyAdoptCall(t *testing.T) {
	// Arrange: registered, never opened — no host or web stream ever existed
	// for it, so it has zero rendezvous participants.
	selfRepo := harness.NewRepo(t)
	d := harness.StartDaemon(t, harness.Opts{SelfRepo: selfRepo.Dir})
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
func drainTriggerRollout(t *testing.T, d *harness.Daemon, selfRepo *harness.Repo, path string) {
	t.Helper()
	source := worktreeOfRepo(t, selfRepo, "trigger")
	ws := harness.Register(t, d, source)
	sha := writeCommit(t, selfRepo, source, path, "trigger\n")
	selfRepo.SetPaths(sha, path)
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: ws})); err != nil {
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
