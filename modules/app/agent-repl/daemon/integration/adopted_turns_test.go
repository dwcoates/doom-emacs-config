//go:build integration

package integration

import (
	"sync"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/integration/harness"
	"claude-repld/internal/ids"
	"claude-repld/internal/rollout"

	"connectrpc.com/connect"
)

// adopted_turns_test.go: at EVERY adoption the daemon compares the
// workspace's open turn rows with the adopted shim's own turn_in_flight. A row
// the shim no longer runs ended while no daemon was watching and is closed at
// INFO; the row the shim still runs stays open until its own terminal.

// adoptedTurnCase is one side of the comparison.
type adoptedTurnCase struct {
	name string
	// finishUnwatched ends the turn on the shim while no daemon watches it.
	finishUnwatched bool
}

var adoptedTurnCases = []adoptedTurnCase{
	{name: "a turn that finished while no daemon watched is closed", finishUnwatched: true},
	{name: "a turn the shim still runs stays open"},
}

// openTurnOn submits one prompt and waits for the daemon to open its turn,
// answering the turn's id.
func openTurnOn(t *testing.T, f *fixture, key string) ids.TurnID {
	t.Helper()
	resp := f.submit("work across the adoption", key, conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	turn := resp.GetSuccess().GetTurn().GetTurn().GetValue()
	if turn == "" {
		t.Fatalf("SubmitPrompt = %v, want a minted TurnId", resp)
	}
	f.shim.ExpectStartTurn()
	f.d.AwaitWorkspaceLogOperationCount(f.repo.Dir, harness.OpTurnOpened, 1)
	return ids.TurnID(turn)
}

// awaitAdoptedTurnOutcome asserts the adoption's decision about one turn from
// the workspace sink, which spans every daemon that served the workspace, and
// for a kept turn ends it on the shim and asserts its ordinary close.
func awaitAdoptedTurnOutcome(t *testing.T, d *harness.Daemon, f *fixture, turn ids.TurnID, tc adoptedTurnCase) {
	t.Helper()
	if tc.finishUnwatched {
		d.AwaitWorkspaceLogRecord(f.repo.Dir, "the queue's INFO close of the unobserved turn", func(r harness.LogRecord) bool {
			return r.Operation == "daemon.promptqueue.turn_ended" && r.Level == "info" &&
				r.Context["turn"] == string(turn) && r.Context["close"] == "orphaned"
		})
		return
	}
	kept := d.AwaitWorkspaceLogRecord(f.repo.Dir, "the INFO record keeping the running turn open", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.sessionwatcher.turn_open_at_attach" && r.Level == "info" && r.Context["turn_id"] == string(turn)
	})
	// THE ADOPTER'S MAIN WATCH MUST BE SUBSCRIBED BEFORE THE TURN ENDS. The
	// keep record is written as the adopter takes the session facts, and the
	// main agent watch is dialed only after it; the fake shim delivers a
	// pushed frame to the streams subscribed at that moment and replays
	// nothing. It subscribes before it sends a stream's opening page, so the
	// page's record from the same daemon proves the end will be delivered.
	d.AwaitWorkspaceLogRecord(f.repo.Dir, "the adopter's main watch opening page", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.sessionwatcher.history_page" && r.PID == kept.PID
	})
	f.shim.PushAgentFrame(mainAgent, successFrame(mainAgent, nil))
	d.AwaitWorkspaceLogRecord(f.repo.Dir, "the kept turn's own completed close", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.promptqueue.turn_ended" && r.Context["turn"] == string(turn) && r.Context["close"] == "completed"
	})
	for _, r := range harness.ReadCumulativeWorkspaceLog(t, f.repo.Dir, "daemon") {
		if r.Operation == "daemon.sessionwatcher.turn_ended_unobserved" && r.Context["turn_id"] == string(turn) {
			t.Fatalf("the running turn %s was closed as unobserved: %v", turn, r)
		}
	}
}

// expectNoWorkspaceWarnings fails on any WARN or ERROR any daemon wrote to
// the workspace's own sink. It covers a successor the harness did not start,
// whose run log no sweep reads.
func expectNoWorkspaceWarnings(t *testing.T, f *fixture) {
	t.Helper()
	for _, r := range harness.ReadCumulativeWorkspaceLog(t, f.repo.Dir, "daemon") {
		if r.Level == "warn" || r.Level == "error" {
			t.Fatalf("the workspace sink carries %s %s: %s %v", r.Level, r.Operation, r.Message, r.Context)
		}
	}
}

func TestTheBootAdoptionOfASurvivorReconcilesItsOpenTurn(t *testing.T) {
	t.Parallel()
	for _, tc := range adoptedTurnCases {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			// Arrange: a turn in flight, then the daemon dies and the shim
			// survives it, as a crash leaves them.
			f := newOpened(t, harness.Opts{})
			f.shim.ExpectStartSession()
			turn := openTurnOn(t, f, "k-boot-adopt")
			info := f.shim.Info()
			f.d.Kill()
			if tc.finishUnwatched {
				f.shim.PushAgentFrame(mainAgent, successFrame(mainAgent, nil))
			}
			// The outgoing daemon meant to preserve the session, so the
			// reconciliation records it PRESERVED rather than unaccounted for.
			writeIntentManifest(t, f.d, rollout.ManifestSession{
				Workspace: ids.WorkspaceID(f.ws.GetId()), Dir: f.repo.Dir, ShimPID: info.PID,
				VendorSessionID: info.VendorSessionID, Intent: rollout.IntentPreserve,
			})

			// Act: the successor boots and adopts the survivor.
			successor := harness.StartDaemon(t, harness.Opts{
				StateDir: f.d.StateDir, ProfileDir: f.d.ProfileDir,
				ExtraArgs: []string{"--default-config-dir", f.d.DefaultConfigDir},
				ExtraEnv:  []string{"AGENT_REPL_LOCK_DIR=" + f.d.LockDir},
			})

			// Assert
			awaitAdoptedTurnOutcome(t, successor, f, turn, tc)
			expectNoWorkspaceWarnings(t, f)
		})
	}
}

func TestTheTakeoversOrphanRecoveryReconcilesItsOpenTurn(t *testing.T) {
	t.Parallel()
	for _, tc := range adoptedTurnCases {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			// Arrange: a turn in flight on an incumbent a successor joined.
			f := newOpened(t, harness.Opts{})
			f.shim.ExpectStartSession()
			turn := openTurnOn(t, f, "k-takeover")
			successor := harness.StartDaemon(t, harness.Opts{
				StateDir: f.d.StateDir, Joining: f.d.Addr,
				ExtraEnv: []string{"AGENT_REPL_LOCK_DIR=" + f.d.LockDir},
			})
			// The incumbent is FROZEN before the turn ends, so the end is
			// written to a daemon that never reads it: it ends unwatched.
			if tc.finishUnwatched {
				f.d.Freeze()
				f.shim.PushAgentFrame(mainAgent, successFrame(mainAgent, nil))
			}

			// Act: the incumbent dies having handed nothing over.
			f.d.Kill()

			// Assert
			successor.AwaitLogRecord(successor.RunLogPath(), "the takeover's recovery of the unserved shim", func(r harness.LogRecord) bool {
				return r.Operation == "daemon.rollout.orphans" && r.Message == "adopted a shim a gone daemon left running"
			})
			awaitAdoptedTurnOutcome(t, successor, f, turn, tc)
			expectNoWorkspaceWarnings(t, f)
		})
	}
}

func TestAForcedHandoversAdoptionReconcilesItsOpenTurn(t *testing.T) {
	t.Parallel()
	for _, tc := range adoptedTurnCases {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			// Arrange: a turn in flight, with both participants open so the
			// successor adopts only when they ask it to.
			d := harness.StartDaemon(t, harness.Opts{Timeout: harness.HandoverChainTimeout})
			f := drainOpenWorkspace(t, d)
			f.shim.ExpectStartSession()
			host := d.WatchHost(f.ws)
			f.web = d.WatchWeb(f.ws)
			harness.AwaitNext(t, d.Ctx(), host, "the fresh host push")
			turn := openTurnOn(t, f, "k-handover")
			daemonStream := d.WatchDaemonStream()

			// Act: a FORCED handover transfers the busy workspace now, the
			// shim detached and still running its turn.
			d.StageDeployBuild(harness.DeployStaleDaemon)
			if _, err := d.Client().Deploy(d.Ctx(), connect.NewRequest(&agentreplv1.DeployRequest{Force: true})); err != nil {
				t.Fatalf("Deploy{force} = error %v, want the handover accepted", err)
			}
			announced := harness.AwaitView(t, d.Ctx(), daemonStream, "shutdown_announced", func(r *agentreplv1.WatchDaemonResponse) bool {
				return r.GetShutdownAnnounced() != nil
			}).GetShutdownAnnounced()
			harness.AwaitView(t, d.Ctx(), host, "transferred", func(r *agentreplv1.WatchHostWorkspaceResponse) bool {
				return r.GetTransferred() != nil
			})
			// The incumbent's watches outlive the transfer notice, so it is
			// FROZEN before the turn ends: between the transfer and the
			// adoption no daemon watches the turn end.
			if tc.finishUnwatched {
				d.Freeze()
				f.shim.PushAgentFrame(mainAgent, successFrame(mainAgent, nil))
			}
			// Both participants adopt together: the rendezvous waits for both.
			successor := drainDial(announced.GetAddress())
			var wg sync.WaitGroup
			var hostErr, webErr error
			wg.Add(2)
			go func() {
				defer wg.Done()
				_, hostErr = successor.AdoptHostWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.AdoptHostWorkspaceRequest{Workspace: f.ws}))
			}()
			go func() {
				defer wg.Done()
				_, webErr = successor.AdoptWebWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.AdoptWebWorkspaceRequest{Workspace: f.ws}))
			}()
			wg.Wait()
			if hostErr != nil || webErr != nil {
				t.Fatalf("AdoptHostWorkspace = %v, AdoptWebWorkspace = %v; want both to succeed", hostErr, webErr)
			}

			// Assert
			awaitAdoptedTurnOutcome(t, d, f, turn, tc)
			expectNoWorkspaceWarnings(t, f)
		})
	}
}

// A TURN WAITING BEHIND A VENDOR-STARTED TURN SURVIVES A RE-ATTACH. The
// daemon's own turn was accepted, the vendor then started a turn of its own
// that runs ahead of it, and the daemon died with both held by the shim. The
// successor learns the running turn from turn_in_flight and the waiting one
// from turns_waiting: it keeps both open, the waiting turn stands in flight
// when the vendor's turn ends, and each is closed by its own terminal. Told
// only of the running turn, it closed the waiting one as ended unobserved.
func TestTheBootAdoptionKeepsATurnWaitingBehindAVendorStartedTurn(t *testing.T) {
	t.Parallel()
	// Arrange: the daemon's turn is accepted, the vendor starts one ahead of
	// it, and the daemon dies with the shim running both.
	f := newOpened(t, harness.Opts{})
	f.shim.ExpectStartSession()
	waiting := openTurnOn(t, f, "k-waiting-behind-vendor")
	const vendorTurn = "turn-vendor-ahead"
	f.shim.PushUserPrompt(mainAgent, &conversationv1.AgentPrompt{
		Id:     &conversationv1.TurnId{Value: vendorTurn},
		Agent:  &conversationv1.AgentId{Value: mainAgent},
		Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_VENDOR_STARTED,
	})
	f.d.AwaitWorkspaceLogRecord(f.repo.Dir, "the queue's INFO record of the adopted turn", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.promptqueue.turn_adopted" && r.Level == "info" && r.Context["turn"] == vendorTurn
	})
	info := f.shim.Info()
	f.d.Kill()
	writeIntentManifest(t, f.d, rollout.ManifestSession{
		Workspace: ids.WorkspaceID(f.ws.GetId()), Dir: f.repo.Dir, ShimPID: info.PID,
		VendorSessionID: info.VendorSessionID, Intent: rollout.IntentPreserve,
	})

	// Act: the successor boots and adopts the survivor, then the vendor's
	// turn ends and the waiting turn after it.
	successor := harness.StartDaemon(t, harness.Opts{
		StateDir: f.d.StateDir, ProfileDir: f.d.ProfileDir,
		ExtraArgs: []string{"--default-config-dir", f.d.DefaultConfigDir},
		ExtraEnv:  []string{"AGENT_REPL_LOCK_DIR=" + f.d.LockDir},
	})
	successor.AwaitWorkspaceLogRecord(f.repo.Dir, "the INFO record keeping the waiting turn open", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.sessionwatcher.turn_open_at_attach" && r.Level == "info" && r.Context["turn_id"] == string(waiting)
	})
	f.shim.PushAgentFrameIn(mainAgent, vendorTurn, successFrame(mainAgent, nil))
	successor.AwaitWorkspaceLogRecord(f.repo.Dir, "the waiting turn standing in flight at the vendor turn's end", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.sessionwatcher.turn_resumed" && r.Level == "info" && r.Context["turn_id"] == string(waiting)
	})
	f.shim.PushAgentFrameIn(mainAgent, string(waiting), successFrame(mainAgent, nil))

	// Assert
	successor.AwaitWorkspaceLogRecord(f.repo.Dir, "the waiting turn's own completed close", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.promptqueue.turn_ended" && r.Context["turn"] == string(waiting) && r.Context["close"] == "completed"
	})
	for _, r := range harness.ReadCumulativeWorkspaceLog(t, f.repo.Dir, "daemon") {
		if r.Operation == "daemon.sessionwatcher.turn_ended_unobserved" && r.Context["turn_id"] == string(waiting) {
			t.Fatalf("the waiting turn %s was closed as unobserved: %v", waiting, r)
		}
	}
	expectNoWorkspaceWarnings(t, f)
}
