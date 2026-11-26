//go:build integration

package integration

import (
	"syscall"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/integration/harness"

	"connectrpc.com/connect"
)

// dead_shim_recovery_test.go: a shim that dies ON ITS OWN, outside any
// stand-down, is brought back without the user doing anything -- live, by the
// departure's own revival, and at the next boot, by the boot's bring-up -- on
// the same vendor session, with no fault left on the footer and
// a prompt sent into the gap delivered.

// deadShimRecords are the WARN-or-worse records a shim's own death writes on
// the way to its recovery, and they are REQUIRED (RequireWarnings), so the
// list cannot outlive the records. ONE DEATH IS ONE ERROR: the exit is decoded
// and recorded (exit), and the session's health opens its fault (open_fault).
// Everything else the death takes down -- the liveness stream and its redial,
// the standing watches, the link fault, a call in flight -- attributes to it at
// INFO (2026-10-06).
var deadShimRecords = []string{"daemon.shimclient.exit", "daemon.health.open_fault"}

// killShim SIGKILLs a fake shim outside every daemon verb and waits for the
// kernel to confirm it is gone.
func killShim(t *testing.T, f *fixture, shim *harness.ShimControl) {
	t.Helper()
	pid := shim.Info().PID
	if err := syscall.Kill(pid, syscall.SIGKILL); err != nil {
		t.Fatalf("kill the fake shim %d: %v", pid, err)
	}
	f.d.AwaitProcessExit(pid)
}

// concludeFirstTurn runs one turn to its end, so the session has a
// conversation to resume.
func concludeFirstTurn(t *testing.T, f *fixture) {
	t.Helper()
	f.submit("do the thing", "k-dead-shim-first", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	f.shim.ExpectStartTurn()
	f.d.AwaitWorkspaceLogOperationCount(f.repo.Dir, harness.OpTurnOpened, 1)
	f.shim.PushAgentFrame(mainAgent, successFrame(mainAgent, nil))
	// THE DURABLE CLOSE, not the routing: the session watcher's `turn_ended`
	// is written before the prompt queue stamps the row, and a SIGKILL landing
	// between the two left the turn open for the next boot to close as an
	// orphan at WARN (`closed the in-flight turns of a workspace with no
	// session`).
	f.d.AwaitWorkspaceLogRecord(f.repo.Dir, "the first turn's durable close", func(r harness.LogRecord) bool {
		return r.Operation == harness.OpTurnEnded && r.Message == harness.TurnCloseStamped
	})
}

// footerSettled is a strip that is serving and idle with NOTHING STANDING on
// its activity cell: no salient line. The death's own warning may still be a
// live transient — it announces an event and stands on nothing — so the cell
// is judged by its tier, not by what its text mentions.
func footerSettled(v *frontendv1.FooterView) bool {
	idle := v.GetStrip().GetStatus().GetIdle()
	return idle != nil && idle.GetActivity().GetSalient() == nil
}

// footerDisconnected is a strip drawing a lost link.
func footerDisconnected(v *frontendv1.FooterView) bool {
	return v.GetStrip().GetStatus().GetAgentReplFault() != nil
}

// rosterLost is the workspace's roster row in a link-loss arm.
func rosterLost(f *fixture) func(*frontendv1.RosterPush) bool {
	return func(r *frontendv1.RosterPush) bool {
		row := rosterRow(r, f.ws.GetId())
		return row != nil && (row.GetDead() != nil || row.GetSevered() != nil)
	}
}

// rosterServing is the workspace's roster row out of every link-loss arm.
func rosterServing(f *fixture) func(*frontendv1.RosterPush) bool {
	return func(r *frontendv1.RosterPush) bool {
		row := rosterRow(r, f.ws.GetId())
		return row != nil && row.GetDead() == nil && row.GetSevered() == nil && row.GetInit() == nil
	}
}

// expectSessionHealthy asserts SessionHealth answers healthy: every link
// fault the death opened was closed by the healthy attach that followed it.
func expectSessionHealthy(t *testing.T, d *harness.Daemon, f *fixture) {
	t.Helper()
	resp, err := d.Client().SessionHealth(d.Ctx(), connect.NewRequest(&agentreplv1.SessionHealthRequest{Workspace: f.ws}))
	if err != nil {
		t.Fatalf("SessionHealth = error %v, want a success", err)
	}
	if resp.Msg.GetSuccess().GetHealthy() == nil {
		t.Fatalf("SessionHealth after the recovery = %v, want healthy", resp.Msg)
	}
}

func TestAShimThatDiesOnItsOwnIsBroughtBackUnasked(t *testing.T) {
	t.Parallel()
	// Arrange: a session with one finished turn.
	f := newCanonical(t)
	f.d.RequireWarnings(deadShimRecords...)
	start := f.shim.ExpectStartSession()
	concludeFirstTurn(t, f)
	vendor := f.shim.Info().VendorSessionID
	footer := f.d.WatchFooter(f.ws)
	roster := f.d.WatchRoster()

	// Act: the shim dies on its own, and nobody does anything.
	killShim(t, f, f.shim)

	// Assert: a new shim resumes the same conversation, unasked.
	revived := f.d.ShimAt(f.d.SocketPath(f.ws) + ".ctl")
	// THE REVIVAL IS TAKEN ON THE DEATH ITSELF, not on a later prompt or a
	// periodic pass: the record is the decision, written as the departure is
	// handled. A wall-clock bound from the kill to the settled footer stood in
	// for this and failed whenever the host was saturated.
	f.d.AwaitWorkspaceLogRecord(f.repo.Dir, "the revival taken on the death", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.promptqueue.revive" && r.Message == "the shim died on its own; its session is brought back now"
	})
	resume := revived.ExpectStartSession()
	if got := resume.GetResume().GetVendorSessionId(); got != vendor || start.GetFresh() == nil {
		t.Fatalf("the revival's StartSession = %v, want a resume of the original vendor session %q", resume, vendor)
	}
	// Assert: the revived watcher RESUMES the feed it had, never replays it.
	f.d.AwaitWorkspaceLogRecord(f.repo.Dir, "the revival's resumed opening", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.workspace.bring_up" && r.Context["opening"] == "resumed" && r.Context["replays_history"] == false
	})
	// Assert: every surface settles with nothing standing on it, once the
	// death itself has been drawn.
	awaitFooter(t, f, footer, "the footer drawing the death", footerDisconnected)
	awaitRoster(t, f.d, roster, "the roster row drawing the death", rosterLost(f))
	awaitFooter(t, f, footer, "the footer idle with no fault", footerSettled)
	awaitRoster(t, f.d, roster, "the roster row serving again", rosterServing(f))
	expectSessionHealthy(t, f.d, f)
}

// bootAfterADeadShim kills a session's daemon AND its shim, then boots a
// successor on the same machine -- state root, account root, profiles and
// locks -- with the host and web streams a running Emacs and page hold.
// EXTRAENV is appended to the successor's environment only.
func bootAfterADeadShim(t *testing.T, extraEnv ...string) (*fixture, *harness.Daemon, string) {
	t.Helper()
	f := newOpened(t, harness.Opts{DurableState: true})
	f.shim.ExpectStartSession()
	concludeFirstTurn(t, f)
	vendor := f.shim.Info().VendorSessionID
	pid := f.shim.Info().PID
	f.d.Kill()
	if err := syscall.Kill(pid, syscall.SIGKILL); err != nil {
		t.Fatalf("kill the fake shim %d: %v", pid, err)
	}
	f.d.AwaitProcessExit(pid)
	opts := f.d.SuccessorOpts()
	opts.ExtraEnv = extraEnv
	successor := harness.StartDaemon(t, opts)
	f.d = successor
	f.host = successor.WatchHost(f.ws)
	f.web = successor.WatchWeb(f.ws)
	return f, successor, vendor
}

func TestAShimThatDiedBeforeABootIsBroughtBackAtBoot(t *testing.T) {
	t.Parallel()
	// Arrange, Act
	f, d, vendor := bootAfterADeadShim(t)
	footer := d.WatchFooter(f.ws)
	roster := d.WatchRoster()

	// Assert: the boot resumes the same conversation, unasked.
	shim := d.ShimAt(d.SocketPath(f.ws) + ".ctl")
	if got := shim.ExpectStartSession().GetResume().GetVendorSessionId(); got != vendor {
		t.Fatalf("the boot's StartSession resumed %q, want the original vendor session %q", got, vendor)
	}
	d.AwaitWorkspaceLogRecordInState("the boot's session up", func(r harness.LogRecord) bool {
		return r.PID == d.PID() && r.Operation == "daemon.workspace.bring_up" && r.Message == "the session is up"
	})
	awaitFooter(t, f, footer, "the footer idle with no fault", footerSettled)
	awaitRoster(t, d, roster, "the roster row serving", rosterServing(f))
	expectSessionHealthy(t, d, f)
}

func TestAPromptSentRightAfterABootOverADeadShimIsDelivered(t *testing.T) {
	t.Parallel()
	// Arrange
	f, d, _ := bootAfterADeadShim(t)

	// Act: the prompt races the boot's own bring-up.
	resp := f.submit("the prompt sent at boot", "k-dead-shim-boot", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)

	// Assert
	if resp.GetSuccess() == nil {
		t.Fatalf("SubmitPrompt at boot = %v, want accepted", resp)
	}
	shim := d.ShimAt(d.SocketPath(f.ws) + ".ctl")
	if got := text(shim.ExpectStartTurn().GetSaid()); got != "the prompt sent at boot" {
		t.Fatalf("the delivered StartTurn said %q, want the prompt sent at boot", got)
	}
	expectRPCCount(t, shim, harness.RPCStartTurn, 1)
}

// TestATurnCutBeforeABootReplaysAsEnded covers the replay half of the one
// door: a turn running when the daemon and its shim both died is closed by the
// successor's boot while no feed has seen it, and the successor's replay of
// the conversation draws its ending from that recorded close, so it never
// replays as running. A crash cut the turn, so the ending is "interrupted"
// (owner ruling, 2026-10-30), never a failure.
func TestATurnCutBeforeABootReplaysAsEnded(t *testing.T) {
	t.Parallel()
	// Arrange: a turn running when the daemon and its shim both die.
	f := newOpened(t, harness.Opts{DurableState: true})
	f.shim.ExpectStartSession()
	const cutText = "the turn nobody saw end"
	resp := f.submit(cutText, "k-dead-shim-replay", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	cut := resp.GetSuccess().GetTurn().GetTurn().GetValue()
	f.shim.ExpectStartTurn()
	f.d.AwaitWorkspaceLogOperationCount(f.repo.Dir, harness.OpTurnOpened, 1)
	pid := f.shim.Info().PID
	f.d.Kill()
	if err := syscall.Kill(pid, syscall.SIGKILL); err != nil {
		t.Fatalf("kill the fake shim %d: %v", pid, err)
	}
	f.d.AwaitProcessExit(pid)
	// The store's book as the resumed shim serves it: the cut turn's prompt,
	// and no terminal, because none was ever written. The store holds it
	// before the successor boots, as the real store does.
	resumed := harness.ShimProfile{
		ResumeHistory: harness.EncodeHistory(t, &conversationv1.HistoryEntry{
			Entry: &conversationv1.HistoryEntry_UserPrompt{UserPrompt: &conversationv1.AgentPrompt{
				Id:     &conversationv1.TurnId{Value: cut},
				Agent:  &conversationv1.AgentId{Value: mainAgent},
				Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT,
				Said:   said(cutText),
			}},
		}),
	}
	f.d.WriteShimProfile(f.repo.Dir, resumed)
	f.d.KeepResumedBook(f.ws.GetId(), resumed)

	// Act: a successor boots on the same machine.
	opts := f.d.SuccessorOpts()
	// The boot closing a turn the crash cut is loud by design.
	opts.ExpectWarnings = []string{"daemon.promptqueue.restore_holds"}
	successor := harness.StartDaemon(t, opts)
	f.d = successor
	f.host = successor.WatchHost(f.ws)
	f.web = successor.WatchWeb(f.ws)

	// Assert: the replayed feed ends the cut turn as interrupted.
	f.openFeedOnceCarrying("the cut turn's replayed ending", func(p *frontendv1.FeedPage) bool {
		for _, row := range p.GetSuccess().GetRows() {
			if row.GetTurn().GetValue() == cut && row.GetTurnEnded().GetInterrupted() != nil {
				return true
			}
		}
		return false
	})
}
