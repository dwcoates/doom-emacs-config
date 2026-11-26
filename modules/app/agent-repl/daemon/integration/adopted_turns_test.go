//go:build integration

package integration

import (
	"sync"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/integration/harness"
	"agent-repld/internal/ids"
	"agent-repld/internal/rollout"

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

var (
	adoptedTurnFinishedUnwatched = adoptedTurnCase{name: "a turn that finished while no daemon watched is closed", finishUnwatched: true}
	adoptedTurnStillRuns         = adoptedTurnCase{name: "a turn the shim still runs stays open"}
	adoptedTurnCases             = []adoptedTurnCase{adoptedTurnFinishedUnwatched, adoptedTurnStillRuns}
)

// adoptedTurnStillRunsOnly is the one case a boot adoption and a forced
// handover each run. Whichever route adopts the survivor, its open turn is
// reconciled by one shared path (sessionwatcher's reconcileOpenAtAttachLocked,
// then the prompt queue's lifecycle), unit-pinned for both outcomes in
// sessionwatcher/watcher_test.go and promptqueue/lifecycle_test.go. What each
// route adds is its own hand-over of the open turns, which the still-running
// case exercises and the restored turn's later close completes. A turn that
// finished while no daemon watched reaches that shared path the same way on
// every route, so the takeover alone keeps it as the integration witness.
var adoptedTurnStillRunsOnly = []adoptedTurnCase{adoptedTurnStillRuns}

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
			return r.Operation == harness.OpTurnEnded && r.Level == "info" &&
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
		return r.Operation == harness.OpTurnEnded && r.Context["turn"] == string(turn) && r.Context["close"] == "completed"
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
	for _, tc := range adoptedTurnStillRunsOnly {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			// Arrange: a turn in flight, then the daemon dies and the shim
			// survives it, as a crash leaves them.
			f := newOpened(t, harness.Opts{DurableState: true})
			f.shim.ExpectStartSession()
			turn := openTurnOn(t, f, "k-boot-adopt")
			info := f.shim.Info()
			f.d.Kill()
			// The outgoing daemon meant to preserve the session, so the
			// reconciliation records it PRESERVED rather than unaccounted for.
			writeIntentManifest(t, f.d, rollout.ManifestSession{
				Workspace: ids.WorkspaceID(f.ws.GetId()), Dir: f.repo.Dir, ShimPID: info.PID,
				VendorSessionID: info.VendorSessionID, Intent: rollout.IntentPreserve,
			})

			// Act: the successor boots and adopts the survivor.
			successor := harness.StartDaemon(t, f.d.SuccessorOpts())

			// Assert
			awaitAdoptedTurnOutcome(t, successor, f, turn, tc)
			expectNoWorkspaceWarnings(t, f)
		})
	}
}

// A GATE OPEN ACROSS AN ADOPTION STANDS ON THE SUCCESSOR. The daemon died
// while the survivor's turn waited on the user; the successor's watches open
// at the tail and never see the ask's start, so only the survivor's
// re-announcement (SessionStarted.open_asks) can tell it. Told only the turn
// in flight, it drew the footer working and the row thinking, red, until the
// answer (owner report, 2026-10-08).
func TestTheBootAdoptionStandsTheGateTheSurvivorHoldsOpen(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name   string
		update *conversationv1.AgentUpdate
		footer func(*frontendv1.FooterStatus) bool
		roster func(*frontendv1.RosterRow) bool
	}{
		{
			name:   "a permission gate",
			update: &conversationv1.AgentUpdate{Update: &conversationv1.AgentUpdate_Permission{Permission: openPermission("perm-1", "gated-1")}},
			footer: func(s *frontendv1.FooterStatus) bool { return s.GetPermission() != nil },
			roster: func(r *frontendv1.RosterRow) bool { return r.GetPermission() != nil },
		},
		{
			name: "a question gate",
			update: &conversationv1.AgentUpdate{Update: &conversationv1.AgentUpdate_Question{Question: &conversationv1.AgentQuestion{
				Id: &conversationv1.AgentQuestionId{Value: "q-1"},
				Result: &conversationv1.AgentQuestion_Start{Start: &conversationv1.AgentQuestionStart{
					Batch: &conversationv1.AgentQuestionBatch{Questions: []*conversationv1.AgentQuestionAsked{{
						Question: &conversationv1.AgentQuestionText{Text: "Which approach?"},
						// The set arm is the question's mode (conversation.v1
						// AgentQuestionAsked.choices): a question with none is malformed.
						Choices: &conversationv1.AgentQuestionAsked_SingleSelect{SingleSelect: &conversationv1.AgentQuestionSingleSelect{
							Options: []*conversationv1.AgentQuestionOption{{Label: &conversationv1.AgentQuestionOptionLabel{Label: "the first"}}},
						}},
					}}},
					StartedAt: startedAt(1),
				}},
			}}},
			footer: func(s *frontendv1.FooterStatus) bool { return s.GetQuestion() != nil },
			roster: func(r *frontendv1.RosterRow) bool { return r.GetQuestion() != nil },
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			// Arrange: a turn in flight waits on the user, then the daemon
			// dies and the shim survives it.
			f := newOpened(t, harness.Opts{DurableState: true})
			f.shim.ExpectStartSession()
			openTurnOn(t, f, "k-gate-adopt")
			footer := f.d.WatchFooter(f.ws)
			f.shim.PushAgentFrame(mainAgent, updateFrame(mainAgent, tt.update))
			awaitFooter(t, f, footer, "the gate on the incumbent's strip", func(v *frontendv1.FooterView) bool {
				return tt.footer(v.GetStrip().GetStatus())
			})
			info := f.shim.Info()
			f.d.Kill()
			writeIntentManifest(t, f.d, rollout.ManifestSession{
				Workspace: ids.WorkspaceID(f.ws.GetId()), Dir: f.repo.Dir, ShimPID: info.PID,
				VendorSessionID: info.VendorSessionID, Intent: rollout.IntentPreserve,
			})

			// Act: the successor boots and adopts the survivor.
			successor := harness.StartDaemon(t, f.d.SuccessorOpts())

			// Assert: the successor's strip and row name the gate.
			ctx, cancel := successor.WaitCtx()
			defer cancel()
			harness.AwaitView(t, ctx, successor.WatchFooter(f.ws), "the gate on the successor's strip", func(v *frontendv1.FooterView) bool {
				return tt.footer(v.GetStrip().GetStatus())
			})
			awaitRoster(t, successor, successor.WatchRoster(), "the gate on the successor's row", func(r *frontendv1.RosterPush) bool {
				row := rosterRow(r, f.ws.GetId())
				return row != nil && tt.roster(row)
			})
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
			f := newOpened(t, harness.Opts{DurableState: true})
			f.shim.ExpectStartSession()
			turn := openTurnOn(t, f, "k-takeover")
			successor := harness.StartDaemon(t, harness.Opts{
				StateDir: f.d.StateDir, Joining: f.d.Addr,
				ExtraEnv: []string{"AGENT_REPL_LOCK_DIR=" + f.d.LockDir},
			})
			// The turn ends on the shim with its end delivered to no
			// stream, so no daemon ever reads it: it ends unwatched.
			if tc.finishUnwatched {
				f.shim.EndTurnUnwatched(mainAgent, successFrame(mainAgent, nil))
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
	for _, tc := range adoptedTurnStillRunsOnly {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			// Arrange: a turn in flight, with both participants open so the
			// successor adopts only when they ask it to.
			d := harness.StartDaemon(t, harness.Opts{DurableState: true, Timeout: harness.HandoverChainTimeout})
			f := drainOpenWorkspace(t, d)
			f.shim.ExpectStartSession()
			host := d.WatchHost(f.ws)
			f.web = d.WatchWeb(f.ws)
			harness.AwaitHostView(t, d.Ctx(), host, "the fresh host push")
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
