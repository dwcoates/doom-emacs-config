//go:build integration

package integration

import (
	"syscall"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/integration/harness"

	"connectrpc.com/connect"
)

// dead_shim_recovery_test.go: a shim that dies ON ITS OWN, outside any
// stand-down, is brought back without the user doing anything -- live, by the
// departure's own revival, and at the next boot, by the boot's bring-up -- on
// the same vendor session, with no fault left on the footer or the topbar and
// a prompt sent into the gap delivered.

// deadShimRecords are the records a shim's own death writes on the way to its
// recovery. Each is evidence of the kill the test performs: the streams break
// (redial, reopen, the watches), the link loss is recorded as the session's
// fault (link_fault, open_fault) and the exit is decoded (exit). A title
// gather in flight at the kill answers `unexpected EOF` at ERROR.
var deadShimRecords = []string{
	"daemon.shimclient.redial", "daemon.sessionwatcher.reopen", "daemon.health.open_fault",
	"daemon.sessionwatcher.link_fault", "daemon.sessionwatcher.watch_agent", "daemon.sessionwatcher.watch_session",
	"daemon.shimclient.exit", "daemon.shimclient.gather_title_digest",
}

// deadShimRecoveryBound is how long a live death may take, from the SIGKILL to
// a footer serving idle with no fault. MEASURED: 122-196ms across eight runs
// alone and 120-147ms in four full-suite runs at -parallel 8; the bound is ~5x
// the slowest, and well inside harness.DefaultTimeout.
const deadShimRecoveryBound = time.Second

// killShim SIGKILLs a fake shim outside every daemon verb and waits for the
// kernel to confirm it is gone, answering the instant it was killed.
func killShim(t *testing.T, f *fixture, shim *harness.ShimControl) time.Time {
	t.Helper()
	pid := shim.Info().PID
	killedAt := time.Now()
	if err := syscall.Kill(pid, syscall.SIGKILL); err != nil {
		t.Fatalf("kill the fake shim %d: %v", pid, err)
	}
	harness.AwaitProcessGone(t, f.d.Ctx(), pid)
	return killedAt
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
		return r.Operation == "daemon.promptqueue.turn_ended" && r.Message == "stamped the turn's close"
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
func rosterLost(f *fixture) func(*frontendv1.WorkspaceRoster) bool {
	return func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, f.ws.GetId())
		return row != nil && (row.GetDead() != nil || row.GetSevered() != nil)
	}
}

// rosterServing is the workspace's roster row out of every link-loss arm.
func rosterServing(f *fixture) func(*frontendv1.WorkspaceRoster) bool {
	return func(r *frontendv1.WorkspaceRoster) bool {
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
	f := newOpened(t, harness.Opts{})
	f.d.ExpectWarnings(deadShimRecords...)
	start := f.shim.ExpectStartSession()
	concludeFirstTurn(t, f)
	vendor := f.shim.Info().VendorSessionID
	footer := f.d.WatchFooter(f.ws)
	topbar := f.d.WatchTopbar(f.ws)
	roster := f.d.WatchRoster()

	// Act: the shim dies on its own, and nobody does anything.
	killedAt := killShim(t, f, f.shim)

	// Assert: a new shim resumes the same conversation, unasked.
	revived := f.d.ShimAt(f.d.SocketPath(f.ws) + ".ctl")
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
	settledAt := time.Now()
	awaitTopbar(t, f, topbar, "the topbar with no warning", func(v *frontendv1.TopbarView) bool {
		return len(v.GetWarnings().GetWarnings()) == 0
	})
	awaitRoster(t, f.d, roster, "the roster row serving again", rosterServing(f))
	expectSessionHealthy(t, f.d, f)
	if took := settledAt.Sub(killedAt); took > deadShimRecoveryBound {
		t.Fatalf("death to a settled footer took %s, want within %s", took, deadShimRecoveryBound)
	} else {
		t.Logf("death to a settled footer: %s", took)
	}
}

func TestAPromptSentRightAfterAShimDiesIsDelivered(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.d.ExpectWarnings(deadShimRecords...)
	f.shim.ExpectStartSession()
	concludeFirstTurn(t, f)
	killShim(t, f, f.shim)

	// Act: the prompt races the revival the death started.
	resp := f.submit("the prompt sent into the gap", "k-dead-shim-gap", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)

	// Assert: accepted, then delivered once to the revived shim.
	if resp.GetSuccess() == nil {
		t.Fatalf("SubmitPrompt into the gap = %v, want accepted", resp)
	}
	revived := f.d.ShimAt(f.d.SocketPath(f.ws) + ".ctl")
	revived.ExpectStartSession()
	turn := revived.ExpectStartTurn()
	if got := text(turn.GetSaid()); got != "the prompt sent into the gap" {
		t.Fatalf("the delivered StartTurn said %q, want the prompt sent into the gap", got)
	}
	expectRPCCount(t, revived, harness.RPCStartTurn, 1, harness.ProbeWindow)
}

func TestAShimThatDiesMidTurnEndsThatTurnTruthfully(t *testing.T) {
	t.Parallel()
	// Arrange: a turn running when the shim dies.
	f := newOpened(t, harness.Opts{})
	f.d.ExpectWarnings(deadShimRecords...)
	f.shim.ExpectStartSession()
	resp := f.submit("the turn the death cuts", "k-dead-shim-cut", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	cut := resp.GetSuccess().GetTurn().GetTurn().GetValue()
	f.shim.ExpectStartTurn()
	f.d.AwaitWorkspaceLogOperationCount(f.repo.Dir, harness.OpTurnOpened, 1)
	feed := f.watchRootFeed()
	footer := f.d.WatchFooter(f.ws)

	// Act
	killShim(t, f, f.shim)

	// Assert: the feed ends the cut turn as the agent process's death.
	awaitRow(t, f, feed, "the cut turn's agent-process-death ending", func(r *frontendv1.FeedRow) bool {
		return r.GetTurn().GetValue() == cut && r.GetTurnEnded().GetErrored().GetAgentProcessDied() != nil
	})
	// Assert: its durable row is closed, as the agent dying, by the queue.
	f.d.AwaitWorkspaceLogRecord(f.repo.Dir, "the cut turn's agent-died close", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.promptqueue.turn_ended" && r.Context["turn"] == cut && r.Context["close"] == "agent_died"
	})
	// Assert: the revived session is drawn idle, not as the cut turn thinking.
	awaitFooter(t, f, footer, "the footer drawing the death", footerDisconnected)
	f.d.ShimAt(f.d.SocketPath(f.ws) + ".ctl").ExpectStartSession()
	awaitFooter(t, f, footer, "the footer idle with no fault", footerSettled)
}

// TestAShimThatDiesMidTurnLeavesTheRevivedRowOnAnUnreadTurnFailed covers
// RosterRowStatusTurnFailed: a turn the agent process cut by dying is a FAILED
// turn end (owner ruling, 2026-09-28), so once the revived session is up the
// row reports that failure, blue, rather than a green done.
func TestAShimThatDiesMidTurnLeavesTheRevivedRowOnAnUnreadTurnFailed(t *testing.T) {
	t.Parallel()
	// Arrange: a turn running when the shim dies.
	f := newOpened(t, harness.Opts{})
	f.d.ExpectWarnings(deadShimRecords...)
	f.shim.ExpectStartSession()
	f.submit("the turn the death cuts", "k-dead-shim-failed", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	f.shim.ExpectStartTurn()
	f.d.AwaitWorkspaceLogOperationCount(f.repo.Dir, harness.OpTurnOpened, 1)
	roster := f.d.WatchRoster()

	// Act
	killShim(t, f, f.shim)
	f.d.ShimAt(f.d.SocketPath(f.ws) + ".ctl").ExpectStartSession()

	// Assert
	got := awaitRoster(t, f.d, roster, "turn_failed once the revived session is up", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, f.ws.GetId())
		return row != nil && row.GetTurnFailed() != nil
	})
	if row := rosterRow(got, f.ws.GetId()); row.GetViewed() != nil {
		t.Fatal("the turn_failed row carries the viewed marker, want its result unread")
	}
}

func TestAShimThatDiesAgainBeforeAnyTurnEndsIsLeftDownUntilTheNextPrompt(t *testing.T) {
	t.Parallel()
	// Arrange: the first death was brought back, and nothing has run since.
	f := newOpened(t, harness.Opts{})
	// The second death is a shim that cannot hold a session: loud by design.
	f.d.ExpectWarnings(append([]string{"daemon.promptqueue.revive"}, deadShimRecords...)...)
	f.shim.ExpectStartSession()
	concludeFirstTurn(t, f)
	killShim(t, f, f.shim)
	revived := f.d.ShimAt(f.d.SocketPath(f.ws) + ".ctl")
	revived.ExpectStartSession()
	f.d.AwaitWorkspaceLogRecord(f.repo.Dir, "the first revival's resumed opening", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.workspace.bring_up" && r.Context["opening"] == "resumed"
	})
	roster := f.d.WatchRoster()

	// Act
	killShim(t, f, revived)

	// Assert: left down, and said so.
	f.d.AwaitWorkspaceLogRecord(f.repo.Dir, "the WARN leaving the looping shim down", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.promptqueue.revive" && r.Level == "warn"
	})
	awaitRoster(t, f.d, roster, "the roster row dead", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, f.ws.GetId())
		return row != nil && row.GetDead() != nil
	})
	// Assert: the next prompt still brings it back and is delivered.
	f.submit("the next prompt", "k-dead-shim-loop", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	third := f.d.ShimAt(f.d.SocketPath(f.ws) + ".ctl")
	third.ExpectStartSession()
	if got := text(third.ExpectStartTurn().GetSaid()); got != "the next prompt" {
		t.Fatalf("the delivered StartTurn said %q, want the next prompt", got)
	}
}

// bootAfterADeadShim kills a session's daemon AND its shim, then boots a
// successor on the same machine -- state root, account root, profiles and
// locks -- with the host and web streams a running Emacs and page hold.
func bootAfterADeadShim(t *testing.T) (*fixture, *harness.Daemon, string) {
	t.Helper()
	f := newOpened(t, harness.Opts{})
	f.shim.ExpectStartSession()
	concludeFirstTurn(t, f)
	vendor := f.shim.Info().VendorSessionID
	pid := f.shim.Info().PID
	f.d.Kill()
	if err := syscall.Kill(pid, syscall.SIGKILL); err != nil {
		t.Fatalf("kill the fake shim %d: %v", pid, err)
	}
	harness.AwaitProcessGone(t, f.d.Ctx(), pid)
	successor := harness.StartDaemon(t, harness.Opts{
		StateDir: f.d.StateDir, ProfileDir: f.d.ProfileDir,
		ExtraArgs: []string{"--default-config-dir", f.d.DefaultConfigDir},
		ExtraEnv:  []string{"AGENT_REPL_LOCK_DIR=" + f.d.LockDir},
	})
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
	topbar := d.WatchTopbar(f.ws)
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
	awaitTopbar(t, f, topbar, "the topbar with no warning", func(v *frontendv1.TopbarView) bool {
		return len(v.GetWarnings().GetWarnings()) == 0
	})
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
	expectRPCCount(t, shim, harness.RPCStartTurn, 1, harness.ProbeWindow)
}

// TestATurnCutBeforeABootReplaysAsEnded covers the replay half of the one
// door: a turn running when the daemon and its shim both died is closed by the
// successor's boot while no feed has seen it, and the successor's replay of
// the conversation draws its ending from that recorded close, so it never
// replays as running.
func TestATurnCutBeforeABootReplaysAsEnded(t *testing.T) {
	t.Parallel()
	// Arrange: a turn running when the daemon and its shim both die.
	f := newOpened(t, harness.Opts{})
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
	harness.AwaitProcessGone(t, f.d.Ctx(), pid)
	// The store's book as the resumed shim serves it: the cut turn's prompt,
	// and no terminal, because none was ever written.
	f.d.WriteShimProfile(f.repo.Dir, harness.ShimProfile{
		ResumeHistory: harness.EncodeHistory(t, &conversationv1.HistoryEntry{
			Entry: &conversationv1.HistoryEntry_UserPrompt{UserPrompt: &conversationv1.AgentPrompt{
				Id:     &conversationv1.TurnId{Value: cut},
				Agent:  &conversationv1.AgentId{Value: mainAgent},
				Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT,
				Said:   said(cutText),
			}},
		}),
	})

	// Act: a successor boots on the same machine.
	successor := harness.StartDaemon(t, harness.Opts{
		StateDir: f.d.StateDir, ProfileDir: f.d.ProfileDir,
		ExtraArgs: []string{"--default-config-dir", f.d.DefaultConfigDir},
		ExtraEnv:  []string{"AGENT_REPL_LOCK_DIR=" + f.d.LockDir},
	})
	// The boot closing a turn the crash cut is loud by design.
	successor.ExpectWarnings("daemon.promptqueue.restore_holds")
	f.d = successor
	f.host = successor.WatchHost(f.ws)
	f.web = successor.WatchWeb(f.ws)

	// Assert: the replayed feed ends the cut turn as dropped.
	f.openFeedOnceCarrying("the cut turn's replayed ending", func(p *frontendv1.FeedPage) bool {
		for _, row := range p.GetSuccess().GetRows() {
			if row.GetTurn().GetValue() == cut && row.GetTurnEnded().GetErrored().GetTurnFailed().GetStopReason() == "closed:orphaned" {
				return true
			}
		}
		return false
	})
}

// THE DAEMON'S OWN WARNING ABOUT A WORKSPACE REACHES ITS STRIP, through the one
// record tee at dlog's workspace-logger emit point: the watcher's warning that
// the shim is gone is taken by the footer as the `daemon_warning` transient.
//
// WHICH TRANSIENT IS ON SCREEN AFTERWARDS IS NOT THIS TEST'S TO PIN. A kill
// writes several daemon records from different goroutines -- the exit's
// error, the link warning, a standing stream's error -- and the newest
// transient wins (owner ruling, 2026-09-28). The link warning was the one
// drawn only when it happened to be written last; 3 runs in 30 it was not,
// and this test failed waiting for a strip that had already moved on
// (2026-10-06). So the tee is asserted where it lands (the footer's own
// record of taking the warning), and the strip is asserted to announce the
// kill's daemon records at all.
func TestADaemonWarningAboutTheWorkspaceIsAnnouncedOnItsFooter(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.d.ExpectWarnings(deadShimRecords...)
	f.shim.ExpectStartSession()
	concludeFirstTurn(t, f)
	footer := f.d.WatchFooter(f.ws)

	// Act
	killShim(t, f, f.shim)

	// Assert: the footer took the link warning as a daemon warning.
	f.d.AwaitWorkspaceLogRecord(f.repo.Dir, "the footer taking the link warning", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.footer.daemon_record" &&
			r.Context["operation"] == "daemon.sessionwatcher.link_fault" && r.Context["level"] == "warn"
	})
	// Assert: the strip announces the kill's daemon records.
	awaitFooter(t, f, footer, "a daemon record about the kill announced", func(v *frontendv1.FooterView) bool {
		if warning := unpinnedWarning(v); warning.GetMessage() != "" {
			return true
		}
		return unpinnedError(v).GetMessage() != ""
	})
	f.d.ShimAt(f.d.SocketPath(f.ws) + ".ctl").ExpectStartSession()
}

// unpinnedError is the daemon_error transient whatever status carries it.
func unpinnedError(v *frontendv1.FooterView) *frontendv1.FooterActivityTransientDaemonError {
	status := v.GetStrip().GetStatus()
	for _, cell := range []*frontendv1.FooterActivityTransientOverEnduring{
		status.GetIdle().GetActivity().GetUnpinned(),
		status.GetAgentReplFault().GetActivity().GetUnpinned(),
	} {
		if e := cell.GetTransient().GetDaemonError(); e != nil {
			return e
		}
	}
	return nil
}

// unpinnedWarning is the daemon_warning transient whatever status carries it.
func unpinnedWarning(v *frontendv1.FooterView) *frontendv1.FooterActivityTransientDaemonWarning {
	status := v.GetStrip().GetStatus()
	for _, cell := range []*frontendv1.FooterActivityTransientOverEnduring{
		status.GetIdle().GetActivity().GetUnpinned(),
		status.GetAgentReplFault().GetActivity().GetUnpinned(),
	} {
		if w := cell.GetTransient().GetDaemonWarning(); w != nil {
			return w
		}
	}
	return nil
}
