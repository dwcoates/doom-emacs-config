//go:build integration

// Package integration: the audit-3 SUBSCRIPTION INVARIANT and FLUSH-ON-ACCEPT
// family tests, table-driven across harness.WatchKinds(), plus the
// push-cadence no-change probes and the per-workspace transport-closed
// refusal family. See daemon/ARCHITECTURE.md "Standing-stream mechanics" and
// docs/overhaul/daemon.md invariant 13.
package integration

import (
	"fmt"
	"path/filepath"
	"strings"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/internal/workspace"

	"claude-repld/integration/harness"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"
)

// ---------------------------------------------------------------------------
// Critique 1: the SUBSCRIPTION INVARIANT, table-driven across
// harness.WatchKinds() — a late subscriber's first push is the
// last-published view, and everything after arrives in order.
// ---------------------------------------------------------------------------

// subscriptionScenario drives one WatchKind through two distinguishable
// states so the invariant can be checked against a witness stream opened
// BEFORE either change.
type subscriptionScenario struct {
	// drive1 performs the action whose resulting view a LATE subscriber must
	// receive as its first push.
	drive1 func(t *testing.T, f *fixture)
	// isFirst identifies drive1's resulting view on the wire.
	isFirst func(msg proto.Message) bool
	// drive2 performs a FURTHER action after the late subscriber is already
	// attached.
	drive2 func(t *testing.T, f *fixture)
	// isSecond identifies drive2's resulting view on the wire.
	isSecond func(msg proto.Message) bool
	// ofTopic identifies the frames of the DRIVEN topic, for a stream that
	// merges several standing topics onto one wire (an Emacs WatchDaemon is
	// also told the loud faults and the persistent-wifi standing). The
	// invariant is per topic, and the replay order BETWEEN topics is not part
	// of it. Nil is a single-topic stream: every frame is the driven topic's.
	ofTopic func(msg proto.Message) bool
}

// subscriptionScenarioFor answers the lightweight two-step driver for one
// WatchKind, or false when none exists in this suite.
//
// WatchWebWorkspace has none: its only push arm is `transferred`, fired
// exactly once per handover, and by the time it fires the workspace's
// standing on THIS daemon has already become transferring_away —
// resolveStreamRef (internal/server/refuse.go) refuses every FURTHER
// per-workspace open on this daemon before it ever reaches the topic. There
// is no window in which "subscribe late on the daemon holding the published
// transferred view" is even reachable, so the late-subscribe half of the
// invariant cannot be demonstrated for it without contradicting that refusal
// contract. See the report for this note restated.
func subscriptionScenarioFor(name string) (subscriptionScenario, bool) {
	switch name {
	case "WatchFooter":
		return subscriptionScenario{
			drive1: func(t *testing.T, f *fixture) {
				f.submit("start the work", "k-sub-inv-footer-1", origin)
				f.shim.ExpectStartTurn()
			},
			// The turn's LAST view of the drive: accepted (working), then
			// delivered, which stands the quiet-stretch line.
			isFirst: func(msg proto.Message) bool {
				return msg.(*frontendv1.FooterView).GetStrip().GetStatus().GetWorking().GetActivity().GetUnpinned().GetQuietStretch() != nil
			},
			drive2: func(t *testing.T, f *fixture) {
				f.shim.PushAgentFrame(mainAgent, successFrame(mainAgent, nil))
			},
			isSecond: func(msg proto.Message) bool {
				return msg.(*frontendv1.FooterView).GetStrip().GetStatus().GetIdle() != nil
			},
		}, true

	case "WatchTopbar":
		return subscriptionScenario{
			drive1: func(t *testing.T, f *fixture) {
				f.shim.PushSessionUpdate(&conversationv1.SessionUpdate{
					Update: &conversationv1.SessionUpdate_ContextUsage{ContextUsage: &conversationv1.SessionContextUsage{
						TotalTokens: 10_000, MaxTokens: 200_000, Percentage: 5, Model: "claude-opus-5",
						Categories: []*conversationv1.SessionContextCategory{{Label: "system prompt", Tokens: 1_000, Color: "blue"}},
					}},
				})
			},
			// figures.Tokens(10_000) == "10k" (internal/figures/tokens.go).
			isFirst: func(msg proto.Message) bool {
				return msg.(*frontendv1.TopbarView).GetContext().GetText() == "10k"
			},
			drive2: func(t *testing.T, f *fixture) {
				f.shim.PushSessionUpdate(&conversationv1.SessionUpdate{
					Update: &conversationv1.SessionUpdate_ContextUsage{ContextUsage: &conversationv1.SessionContextUsage{
						TotalTokens: 20_000, MaxTokens: 200_000, Percentage: 10, Model: "claude-opus-5",
						Categories: []*conversationv1.SessionContextCategory{{Label: "system prompt", Tokens: 1_000, Color: "blue"}},
					}},
				})
			},
			isSecond: func(msg proto.Message) bool {
				return msg.(*frontendv1.TopbarView).GetContext().GetText() == "20k"
			},
		}, true

	case "WatchWorkspaceRoster":
		var ws2, ws3 *workspacev1.WorkspaceRef
		return subscriptionScenario{
			drive1: func(t *testing.T, f *fixture) {
				repo2 := harness.NewRepo(t)
				ws2 = harness.Register(t, f.d, repo2.Dir)
			},
			isFirst: func(msg proto.Message) bool {
				return ws2 != nil && rosterRow(msg.(*frontendv1.WorkspaceRoster), ws2.GetId()) != nil
			},
			drive2: func(t *testing.T, f *fixture) {
				repo3 := harness.NewRepo(t)
				ws3 = harness.Register(t, f.d, repo3.Dir)
			},
			isSecond: func(msg proto.Message) bool {
				return ws3 != nil && rosterRow(msg.(*frontendv1.WorkspaceRoster), ws3.GetId()) != nil
			},
		}, true

	case "WatchDaemon":
		return subscriptionScenario{
			drive1: func(t *testing.T, f *fixture) {
				_, err := f.d.Client().UpdateShutdownSchedule(f.d.Ctx(), connect.NewRequest(&agentreplv1.UpdateShutdownScheduleRequest{
					Action: &agentreplv1.UpdateShutdownScheduleRequest_Schedule{Schedule: &agentreplv1.UpdateShutdownScheduleSchedule{
						AtMs:   time.Now().Add(time.Hour).UnixMilli(),
						Reason: drainReasonDeploy(),
					}},
				}))
				if err != nil {
					t.Fatalf("UpdateShutdownSchedule{schedule} = error %v, want a success", err)
				}
			},
			isFirst: func(msg proto.Message) bool {
				return msg.(*agentreplv1.WatchDaemonResponse).GetDrainScheduled() != nil
			},
			drive2: func(t *testing.T, f *fixture) {
				_, err := f.d.Client().UpdateShutdownSchedule(f.d.Ctx(), connect.NewRequest(&agentreplv1.UpdateShutdownScheduleRequest{
					Action: &agentreplv1.UpdateShutdownScheduleRequest_Cancel{Cancel: &agentreplv1.UpdateShutdownScheduleCancel{}},
				}))
				if err != nil {
					t.Fatalf("UpdateShutdownSchedule{cancel} = error %v, want a success", err)
				}
			},
			isSecond: func(msg proto.Message) bool {
				return msg.(*agentreplv1.WatchDaemonResponse).GetDrainCancelled() != nil
			},
			ofTopic: func(msg proto.Message) bool {
				r := msg.(*agentreplv1.WatchDaemonResponse)
				return r.GetDrainScheduled() != nil || r.GetDrainCancelled() != nil || r.GetShutdownAnnounced() != nil
			},
		}, true

	case "WatchDaemonHolds":
		var turn2, turn3 *conversationv1.TurnId
		return subscriptionScenario{
			drive1: func(t *testing.T, f *fixture) {
				f.submit("start the work", "k-sub-inv-holds-run", origin)
				f.shim.ExpectStartTurn()
				resp := f.submit("a follow-up while it runs", "k-sub-inv-holds-1", origin)
				turn2 = resp.GetSuccess().GetTurn().GetTurn()
				if turn2.GetValue() == "" {
					t.Fatalf("SubmitPrompt while a turn runs = %v, want a minted TurnId (it is HELD)", resp)
				}
			},
			isFirst: func(msg proto.Message) bool {
				e := promptHeldEntry(msg.(*frontendv1.DaemonHoldTray), turn2)
				return e != nil && e.GetHoldForTurnEnd() != nil
			},
			drive2: func(t *testing.T, f *fixture) {
				resp := f.submit("a second follow-up while it still runs", "k-sub-inv-holds-2", origin)
				turn3 = resp.GetSuccess().GetTurn().GetTurn()
				if turn3.GetValue() == "" {
					t.Fatalf("SubmitPrompt while a turn runs = %v, want a minted TurnId (it is HELD)", resp)
				}
			},
			isSecond: func(msg proto.Message) bool {
				tray := msg.(*frontendv1.DaemonHoldTray)
				e2 := promptHeldEntry(tray, turn2)
				e3 := promptHeldEntry(tray, turn3)
				return e2 != nil && e2.GetHoldForTurnEnd() != nil && e3 != nil && e3.GetHoldForTurnEnd() != nil
			},
		}, true

	// WatchHostWorkspace has NO scenario here on purpose. Its only lightweight
	// two-step driver in this suite is OpenInEditor, and that is a one-shot
	// host RELAY — a command sent once to whoever is listening — not a
	// retained view. The invariant under test is about views: what a LATE
	// subscriber is replayed. A relay is replayed to nobody, by design, so
	// driving one here would assert the opposite of the contract.

	default:
		return subscriptionScenario{}, false
	}
}

func TestSubscriptionInvariantAcrossWatchKinds(t *testing.T) {
	t.Parallel()
	for _, k := range harness.WatchKinds() {
		k := k
		t.Run(k.Name, func(t *testing.T) {
			t.Parallel()
			scenario, ok := subscriptionScenarioFor(k.Name)
			if !ok {
				t.Skipf("%s has no lightweight two-step driver in this suite (see subscriptionScenarioFor's doc comment)", k.Name)
				return
			}

			// Arrange: a workspace already opened, a witness attached before
			// anything is driven.
			f := newOpened(t, harness.Opts{})
			var ws *workspacev1.WorkspaceRef
			if k.PerWorkspace {
				ws = f.ws
			}
			witness := k.Open(f.d, ws)

			// Act: drive the first change, and confirm it landed on the
			// witness before subscribing late — "a workspace already opened
			// and its view already published".
			scenario.drive1(t, f)
			first := harness.AwaitView(t, f.d.Ctx(), witness, k.Name+": the driven first view", scenario.isFirst)

			// Act: subscribe LATE.
			late := k.Open(f.d, ws)

			// Assert (a): the first push a late subscriber receives is the
			// last-published view.
			ofTopic := scenario.ofTopic
			if ofTopic == nil {
				ofTopic = func(proto.Message) bool { return true }
			}
			gotFirst := harness.AwaitView(t, f.d.Ctx(), late, k.Name+": the late subscriber's first push", ofTopic)
			if !proto.Equal(gotFirst, first) {
				t.Fatalf("%s: late subscriber's first push = %v, want the last-published view %v", k.Name, gotFirst, first)
			}

			// Act: drive a further change.
			scenario.drive2(t, f)
			second := harness.AwaitView(t, f.d.Ctx(), witness, k.Name+": the driven second view", scenario.isSecond)

			// Assert (b): the further change reaches the late subscriber too.
			// It is AWAITED, not demanded of the very next frame: a driver may
			// publish INTERMEDIATE views on its way to the settled one (the
			// hold tray's `classifying` before its verdict), and every one of
			// them is a real push the late subscriber is entitled to.
			gotSecond := harness.AwaitView(t, f.d.Ctx(), late, k.Name+": the late subscriber's further view", scenario.isSecond)
			if !proto.Equal(gotSecond, second) {
				t.Fatalf("%s: late subscriber's further view = %v, want the further-published view %v", k.Name, gotSecond, second)
			}
		})
	}
}

// ---------------------------------------------------------------------------
// Critique 2: FLUSH-ON-ACCEPT, table-driven across harness.WatchKinds().
// ---------------------------------------------------------------------------

// TestFlushOnAcceptAcrossWatchKinds asserts that every Watch* stream's
// response headers reach the client at accept time, before any push. Three
// kinds — WatchDaemonHolds, WatchHostWorkspace and WatchWebWorkspace — turn
// out to ALWAYS have a view to send even on a workspace that was only just
// registered (holds publishes the empty tray at SetWorkspaceDir/registration
// time; the two per-workspace link streams compose and publish their state
// synchronously before every subscribe — "COMPOSE BEFORE SUBSCRIBING",
// internal/server/streams.go). For those three the no-view flush path this
// test exercises for every other kind never actually fires, so the
// assertion here is deliberately per-kind rather than one blanket
// ExpectNoPush, per the sub-brief's own instruction.
func TestFlushOnAcceptAcrossWatchKinds(t *testing.T) {
	t.Parallel()
	for _, k := range harness.WatchKinds() {
		k := k
		t.Run(k.Name, func(t *testing.T) {
			t.Parallel()
			var d *harness.Daemon
			var ws *workspacev1.WorkspaceRef
			if k.PerWorkspace {
				f := newRegistered(t, harness.Opts{})
				d, ws = f.d, f.ws
			} else {
				d = newDaemon(t, harness.Opts{})
			}

			s := k.Open(d, ws)
			hdrs := s.AwaitHeaders(t, d.Ctx(), k.Name)
			if hdrs == nil {
				t.Fatalf("%s: AwaitHeaders returned no header set", k.Name)
			}

			switch k.Name {
			case "WatchDaemonHolds":
				tray := harness.AwaitNext(t, d.Ctx(), s, k.Name+": the tray a fresh registration opens with")
				if got := len(tray.(*frontendv1.DaemonHoldTray).GetItems()); got != 0 {
					t.Fatalf("%s on a fresh registration = %d items, want the empty tray (it is a complete answer, published at registration)", k.Name, got)
				}
			case "WatchHostWorkspace":
				push := harness.AwaitNext(t, d.Ctx(), s, k.Name+": the host push a registered-but-unopened workspace opens with")
				if push.(*agentreplv1.WatchHostWorkspaceResponse).GetHost().GetNone() == nil {
					t.Fatalf("%s on a registered-but-unopened workspace = %v, want host.none (composed before every subscribe)", k.Name, push)
				}
			case "WatchWebWorkspace":
				// LANDING 15 PUT A STATE ARM ON THIS STREAM TOO. The page binds
				// its log context from `session_identity`, so a fresh stream
				// that carried none would leave every record of that page's
				// life unattributed; the identity is therefore composed before
				// every subscribe, exactly as the host view is. A
				// registered-but-unopened workspace has no session, and the
				// EMPTY identity is the honest statement of that.
				push := harness.AwaitNext(t, d.Ctx(), s, k.Name+": the identity a registered-but-unopened workspace opens with")
				identity := push.(*agentreplv1.WatchWebWorkspaceResponse).GetSessionIdentity()
				if identity == nil {
					t.Fatalf("%s on a registered-but-unopened workspace = %v, want session_identity (composed before every subscribe)", k.Name, push)
				}
				if identity.GetAgentReplSessionId() != "" {
					t.Fatalf("%s on a workspace with no session = agent_repl_session_id %q, want empty", k.Name, identity.GetAgentReplSessionId())
				}
			case "WatchFooter":
				// THE STRIP IS ALWAYS DRAWN (footer.proto: "The one-line strip at
				// the bottom of the workspace view. Always drawn."), and a
				// registration PRIMES it: workspace/register.go calls
				// Footer.Prime, whose whole purpose is that "a subscriber that
				// arrives early receives the first view ever published rather than
				// an empty one". So a registered-but-unopened workspace has a
				// footer to send, and the no-view flush path never fires here --
				// the same reason WatchTopbar below gets its own case.
				//
				// The render bottoms out rather than being withheld: the status
				// falls through to idle, and every panel is populated, because a
				// workspace with no session fact yet still has a complete footer.
				push := harness.AwaitNext(t, d.Ctx(), s, k.Name+": the primed strip a registered workspace opens with")
				view := push.(*frontendv1.FooterView)
				if view.GetStrip() == nil {
					t.Fatalf("%s on a registered-but-unopened workspace = %v, want the always-drawn strip", k.Name, view)
				}
				if view.GetStrip().GetStatus().GetIdle() == nil {
					t.Fatalf("%s on a workspace with no session = status %v, want idle (the status the render bottoms out at)", k.Name, view.GetStrip().GetStatus())
				}
			case "WatchTopbar":
				// THE STRIP HAS ONE SHAPE AND IS NEVER WITHHELD (topbar.proto,
				// FIXED SCHEMA AND ORGANIZATION). Its readiness gate is the
				// workspace's own two facts — the naming and the account —
				// both of which a registration installs, so a
				// registered-but-unopened workspace opens with the session-less
				// strip: every cell drawn, the three session-scoped controls
				// absent so the client draws their dashes.
				push := harness.AwaitNext(t, d.Ctx(), s, k.Name+": the session-less strip a registered workspace opens with")
				view := push.(*frontendv1.TopbarView)
				if view.GetTitle() == nil || view.GetContext() == nil || view.GetWarnings() == nil {
					t.Fatalf("%s on a registered-but-unopened workspace = %v, want the always-drawn cells", k.Name, view)
				}
				if view.GetModelSelector() != nil {
					t.Fatalf("%s on a workspace with no session = model_selector %v, want absent", k.Name, view.GetModelSelector())
				}
			case "WatchDaemon":
				// THE BOOT READS THE PERSISTENT-WIFI STANDING: cmd/claude-repld's
				// Prime step refreshes it before anything is served, and an
				// Emacs stream (which this one is) is told it as standing state,
				// so even a daemon that never scheduled a drain opens with it.
				push := harness.AwaitNext(t, d.Ctx(), s, k.Name+": the persistent-wifi standing the boot read")
				if push.(*agentreplv1.WatchDaemonResponse).GetPersistentWifi() == nil {
					t.Fatalf("%s first frame = %v, want persistent_wifi (read before anything is served)", k.Name, push)
				}
			case "WatchWorkspaceRoster":
				// The BOOT publishes the roster: cmd/claude-repld's Prime step
				// runs verbs.PublishRegistry after the push surface is bound
				// and before anything is served, so every subscriber — first or
				// late — always has a complete roster to receive.
				harness.AwaitNext(t, d.Ctx(), s, k.Name+": the roster the boot primed")
			default:
				harness.ExpectNoPush(t, s, harness.ProbeWindow, k.Name+" carries no frame before anything is ever published")
			}
		})
	}
}

// TestWatchDaemonFlushesHeadersBeforeAnyFrameWhenNoDrainWasEverScheduled is
// the sub-brief's specific "WatchDaemon with no view yet" case: the
// daemon-wide announcement topic is published only by a drain schedule or
// cancellation, never at boot. A WEBVIEW's stream subscribes to nothing else,
// so a fresh daemon's webview WatchDaemon has genuinely nothing to send (an
// Emacs stream is also told the boot-read persistent-wifi standing).
func TestWatchDaemonFlushesHeadersBeforeAnyFrameWhenNoDrainWasEverScheduled(t *testing.T) {
	t.Parallel()
	// Arrange
	d := newDaemon(t, harness.Opts{})

	// Act
	s := d.WatchWebviewDaemonStream()

	// Assert: headers arrive even though nothing was ever published, and no
	// frame follows.
	s.AwaitHeaders(t, d.Ctx(), "WatchDaemon")
	harness.ExpectNoPush(t, s, harness.ProbeWindow, "WatchDaemon with no drain ever scheduled carries no frame")
}

// TestWatchDaemonHoldsFlushesHeadersThenPushesTheAlreadyEmptyTray is the
// sub-brief's specific "WatchDaemonHolds with an empty tray" case. Per
// internal/resolve/holds/resolver.go ("THE EMPTY TRAY IS A COMPLETE ANSWER,
// and binding is when it can first be given"), SetWorkspaceDir — called at
// REGISTRATION — already publishes the empty tray, so a subscriber opening
// on a merely-registered workspace does NOT find "no view due": it gets the
// empty tray as an ordinary first push. Asserting a blanket ExpectNoPush here
// would be asserting a claim the source contradicts.
func TestWatchDaemonHoldsFlushesHeadersThenPushesTheAlreadyEmptyTray(t *testing.T) {
	t.Parallel()
	// Arrange: registered, never opened.
	f := newRegistered(t, harness.Opts{})

	// Act
	holds := f.d.WatchHolds(f.ws)

	// Assert: headers arrive, and the already-published empty tray follows.
	holds.AwaitHeaders(t, f.d.Ctx(), "WatchDaemonHolds")
	tray := harness.AwaitNext(t, f.d.Ctx(), holds, "the empty tray a fresh registration already published")
	if got := len(tray.GetItems()); got != 0 {
		t.Fatalf("WatchDaemonHolds on a fresh registration = %d items, want the empty tray", got)
	}
}

// TestWatchHostWorkspaceFlushesHeadersThenPushesHostNoneWhenRegisteredButNotOpened
// is the sub-brief's specific "WatchHostWorkspace on a workspace that is
// registered but not opened" case. Per internal/server/streams.go's
// WatchHostWorkspace handler ("COMPOSE BEFORE SUBSCRIBING... publishing here
// is what gives every fresh subscription its opening host push -- including
// the first one, before any session edge has ever fired") and
// composeHostWorkspace's `!hasSession` arm ("Registered, and no session was
// ever created for it. This is the one session arm that needs no live facts
// at all."), this open ALWAYS has a view to send: host.none. As with holds
// above, a blanket "no push is due" assertion would not match the source.
func TestWatchHostWorkspaceFlushesHeadersThenPushesHostNoneWhenRegisteredButNotOpened(t *testing.T) {
	t.Parallel()
	// Arrange: registered, never opened -- no session exists for it at all.
	f := newRegistered(t, harness.Opts{})

	// Act
	host := f.d.WatchHost(f.ws)

	// Assert: headers arrive, and the composed host.none push follows.
	host.AwaitHeaders(t, f.d.Ctx(), "WatchHostWorkspace")
	push := harness.AwaitNext(t, f.d.Ctx(), host, "the host.none push a registered-but-unopened workspace already has")
	if push.GetHost().GetNone() == nil {
		t.Fatalf("WatchHostWorkspace on a registered-but-unopened workspace = %v, want host.none", push)
	}
}

// ---------------------------------------------------------------------------
// Critique 3: push-cadence no-change probes.
// ---------------------------------------------------------------------------

// TestTopbarIdenticalContextUsagePushProducesNoSecondPush re-pushes the exact
// same context_usage session update and asserts no second topbar push
// follows — the topbar resolver's whole-view dedup (internal/publish's
// Topic.Publish, proto.Equal) applies here exactly as it does to the footer
// (TestFooterPushesAreWholeViewsDeduplicated).
func TestTopbarIdenticalContextUsagePushProducesNoSecondPush(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	topbar := f.d.WatchTopbar(f.ws)
	usage := &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_ContextUsage{ContextUsage: &conversationv1.SessionContextUsage{
			TotalTokens: 10_000, MaxTokens: 200_000, Percentage: 5, Model: "claude-opus-5",
			Categories: []*conversationv1.SessionContextCategory{{Label: "system prompt", Tokens: 1_000, Color: "blue"}},
		}},
	}
	f.shim.PushSessionUpdate(usage)
	awaitTopbar(t, f, topbar, "the context chip after the first context_usage push", func(v *frontendv1.TopbarView) bool {
		return v.GetContext().GetText() == "10k"
	})

	// Act: push the IDENTICAL context_usage fact again.
	f.shim.PushSessionUpdate(usage)

	// Assert
	harness.ExpectNoPush(t, topbar, harness.ProbeWindow, "an identical consecutive context_usage push is not sent")
}

// TestHoldsIdenticalSecondAcceptProducesNoSecondPush accepts the same
// hold_for_turn_end verdict twice. internal/promptqueue/holdactions.go's
// Accept only checks that the classification is STILL hold_for_turn_end, not
// whether it was already accepted, so the second call succeeds as a no-op —
// and the tray's whole-view dedup means it produces no second push.
func TestHoldsIdenticalSecondAcceptProducesNoSecondPush(t *testing.T) {
	t.Parallel()
	// Arrange: a turn running, and a follow-up held with the hold_for_turn_end
	// verdict.
	f := newOpened(t, harness.Opts{})
	f.submit("start the work", "k-accept-run", origin)
	f.shim.ExpectStartTurn()
	resp := f.submit("a follow-up while it runs", "k-accept-held", origin)
	turn := resp.GetSuccess().GetTurn().GetTurn()
	holds := f.d.WatchHolds(f.ws)
	awaitView(t, f, holds, "the hold_for_turn_end verdict", func(tray *frontendv1.DaemonHoldTray) bool {
		e := promptHeldEntry(tray, turn)
		return e != nil && e.GetHoldForTurnEnd() != nil
	})

	// Act: accept once.
	accept1, err := f.d.Client().UpdateHeldPrompt(f.d.Ctx(), connect.NewRequest(&agentreplv1.UpdateHeldPromptRequest{
		Workspace: f.ws, Turn: turn,
		Action: &agentreplv1.UpdateHeldPromptRequest_Accept{Accept: &agentreplv1.UpdateHeldPromptAccept{}},
	}))
	if err != nil || accept1.Msg.GetSuccess() == nil {
		t.Fatalf("UpdateHeldPrompt{accept} = (%v, %v), want a success", accept1, err)
	}
	awaitView(t, f, holds, "the tray after the first accept", func(tray *frontendv1.DaemonHoldTray) bool {
		return promptHeldEntry(tray, turn).GetHoldForTurnEnd().GetAccepted().GetAccepted()
	})

	// Act: the SAME accept again — a no-op.
	accept2, err := f.d.Client().UpdateHeldPrompt(f.d.Ctx(), connect.NewRequest(&agentreplv1.UpdateHeldPromptRequest{
		Workspace: f.ws, Turn: turn,
		Action: &agentreplv1.UpdateHeldPromptRequest_Accept{Accept: &agentreplv1.UpdateHeldPromptAccept{}},
	}))
	if err != nil || accept2.Msg.GetSuccess() == nil {
		t.Fatalf("UpdateHeldPrompt{accept} (second, no-op) = (%v, %v), want a success", accept2, err)
	}

	// Assert: the unchanged tray is never re-pushed.
	harness.ExpectNoPush(t, holds, harness.ProbeWindow, "an already-accepted hold_for_turn_end re-accepted produces no second tray push")
}

// TestHostStreamIdenticalRepublishedViewProducesNoSecondPush opens a SECOND
// WatchHostWorkspace on the same, unchanged workspace: the handler composes
// and publishes the host view again on every open ("COMPOSE BEFORE
// SUBSCRIBING", internal/server/streams.go), and since nothing about the
// workspace changed the republished view is identical, so an already-caught-up
// witness receives no further push.
func TestHostStreamIdenticalRepublishedViewProducesNoSecondPush(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	witness := f.d.WatchHost(f.ws)
	harness.AwaitNext(t, f.d.Ctx(), witness, "the witness's initial host push")

	// Act: a second subscription re-triggers PublishHostWorkspace with an
	// unchanged, hence identical, composed view.
	second := f.d.WatchHost(f.ws)
	harness.AwaitNext(t, f.d.Ctx(), second, "the second subscriber's own initial push")

	// Assert: the witness, already caught up, gets nothing further.
	harness.ExpectNoPush(t, witness, harness.ProbeWindow, "a re-published identical host view produces no second push")
}

// ---------------------------------------------------------------------------
// Critique 12: per-workspace Watch* transport-closed refusals.
// ---------------------------------------------------------------------------

// firstReceiveErr answers a streaming rpc's refusal, whether it surfaced at
// the open call itself or (per connect-go's stream semantics) only at the
// first Receive.
func firstReceiveErr[T any](stream *connect.ServerStreamForClient[T], err error) error {
	if err != nil {
		return err
	}
	if stream.Receive() {
		return fmt.Errorf("delivered a frame %v, want a transport refusal", stream.Msg())
	}
	return stream.Err()
}

// transportClosedRPC is one per-workspace Watch* rpc, opened DIRECTLY via
// d.Client() (never harness.Stream, which fatals on an open error).
type transportClosedRPC struct {
	name string
	open func(d *harness.Daemon, ws *workspacev1.WorkspaceRef) error
}

func transportClosedRPCs() []transportClosedRPC {
	return []transportClosedRPC{
		{"WatchFooter", func(d *harness.Daemon, ws *workspacev1.WorkspaceRef) error {
			s, err := d.Client().WatchFooter(d.Ctx(), connect.NewRequest(&agentreplv1.WatchFooterRequest{Workspace: ws}))
			return firstReceiveErr(s, err)
		}},
		{"WatchTopbar", func(d *harness.Daemon, ws *workspacev1.WorkspaceRef) error {
			s, err := d.Client().WatchTopbar(d.Ctx(), connect.NewRequest(&agentreplv1.WatchTopbarRequest{Workspace: ws}))
			return firstReceiveErr(s, err)
		}},
		{"WatchDaemonHolds", func(d *harness.Daemon, ws *workspacev1.WorkspaceRef) error {
			s, err := d.Client().WatchDaemonHolds(d.Ctx(), connect.NewRequest(&agentreplv1.WatchDaemonHoldsRequest{Workspace: ws}))
			return firstReceiveErr(s, err)
		}},
		{"WatchHostWorkspace", func(d *harness.Daemon, ws *workspacev1.WorkspaceRef) error {
			s, err := d.Client().WatchHostWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.WatchHostWorkspaceRequest{Workspace: ws}))
			return firstReceiveErr(s, err)
		}},
		{"WatchWebWorkspace", func(d *harness.Daemon, ws *workspacev1.WorkspaceRef) error {
			s, err := d.Client().WatchWebWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.WatchWebWorkspaceRequest{Workspace: ws, WebappBuild: harness.FakeWebappEntry}))
			return firstReceiveErr(s, err)
		}},
		{"WatchLoginTerminal", func(d *harness.Daemon, ws *workspacev1.WorkspaceRef) error {
			s, err := d.Client().WatchLoginTerminal(d.Ctx(), connect.NewRequest(&agentreplv1.WatchLoginTerminalRequest{Workspace: ws}))
			return firstReceiveErr(s, err)
		}},
	}
}

// assertTransportClosed checks the shared shape of every case below: a
// refusal at connect time, recorded once at INFO under
// daemon.refusal.transport_closed, and never warned.
func assertTransportClosed(t *testing.T, d *harness.Daemon, rpcName, wantCause string) {
	t.Helper()
	rec := d.AwaitRunLogOperation("daemon.refusal.transport_closed")
	if !strings.EqualFold(rec.Level, "info") {
		t.Fatalf("the transport-closed record = level %q, want INFO", rec.Level)
	}
	if rec.Context["rpc"] != rpcName || rec.Context["cause"] != wantCause {
		t.Fatalf("the transport-closed record's context = %v, want rpc %q and cause %q", rec.Context, rpcName, wantCause)
	}
}

// TestPerWorkspaceWatchOpensWithABogusWorkspaceAreTransportClosed covers an
// id the registry does not hold, for every per-workspace Watch* rpc.
func TestPerWorkspaceWatchOpensWithABogusWorkspaceAreTransportClosed(t *testing.T) {
	t.Parallel()
	for _, rpc := range transportClosedRPCs() {
		rpc := rpc
		t.Run(rpc.name, func(t *testing.T) {
			t.Parallel()
			// Arrange
			d := newDaemon(t, harness.Opts{})
			bogus := &workspacev1.WorkspaceRef{Id: "bogus-unregistered-workspace-id"}

			// Act
			err := rpc.open(d, bogus)

			// Assert
			if err == nil {
				t.Fatalf("%s(unknown workspace) = success, want a transport-level refusal", rpc.name)
			}
			assertTransportClosed(t, d, rpc.name, workspace.ArmUnknownWorkspace)
		})
	}
}

// TestPerWorkspaceWatchOpensWithAMismatchedDirAreTransportClosed covers a ref
// whose `dir` disagrees with the registry, for every per-workspace Watch*
// rpc.
func TestPerWorkspaceWatchOpensWithAMismatchedDirAreTransportClosed(t *testing.T) {
	t.Parallel()
	for _, rpc := range transportClosedRPCs() {
		rpc := rpc
		t.Run(rpc.name, func(t *testing.T) {
			t.Parallel()
			// Arrange
			f := newRegistered(t, harness.Opts{})
			mismatched := &workspacev1.WorkspaceRef{Id: f.ws.GetId(), Dir: f.ws.GetDir() + "-mismatched"}

			// Act
			err := rpc.open(f.d, mismatched)

			// Assert
			if err == nil {
				t.Fatalf("%s(mismatched dir) = success, want a transport-level refusal", rpc.name)
			}
			assertTransportClosed(t, f.d, rpc.name, workspace.ArmWorkspaceRefMismatch)
		})
	}
}

// ---------------------------------------------------------------------------
// The orderly exit's QUIETNESS: a cancelled watch is the client leaving.
// ---------------------------------------------------------------------------

// TestAnOrderlyExitWithStandingWatchesRecordsNoServingErrors pins that SIGTERM
// under standing streams is not a fault. The exit cancels every standing
// request context, and a publish or a resolve still in flight then reads
// through a cancelled context — which used to be recorded as
// "daemon.wsm.workspace: refused the read" and
// "daemon.server.publish_host_workspace: could not resolve the workspace's log
// sink" at ERROR on EVERY shutdown. The stream ends quietly instead, and this
// test needs no ExpectWarnings declaration to pass.
func TestAnOrderlyExitWithStandingWatchesRecordsNoServingErrors(t *testing.T) {
	t.Parallel()
	// Arrange: an opened workspace already holds a standing host watch and a
	// standing web watch; a footer watch is the second family under audit.
	f := newOpened(t, harness.Opts{})
	footer := f.d.WatchFooter(f.ws)
	awaitFooter(t, f, footer, "the footer's opening view", func(v *frontendv1.FooterView) bool {
		return v.GetStrip() != nil
	})

	// Act: the orderly exit, with both watches still standing.
	f.d.Stop()
	f.d.AwaitExit()

	// Assert: nothing the serving surface or the state store recorded on the
	// way out is an error.
	for _, record := range shutdownErrorRecords(t, f.d) {
		if strings.HasPrefix(record.Operation, "daemon.server.") ||
			strings.HasPrefix(record.Operation, "daemon.wsm.") {
			t.Errorf("an orderly exit under standing watches recorded an error: %s: %s %v",
				record.Operation, record.Message, record.Context)
		}
	}
}

// shutdownErrorRecords is every ERROR record in every sink under the state
// root's logs directory, which is where both the run log and the
// per-workspace sink land.
func shutdownErrorRecords(t *testing.T, d *harness.Daemon) []harness.LogRecord {
	t.Helper()
	sinks, err := filepath.Glob(filepath.Join(d.StateDir, "logs", "*.log"))
	if err != nil {
		t.Fatalf("globbing the daemon's log sinks: %v", err)
	}
	var out []harness.LogRecord
	for _, sink := range sinks {
		for _, record := range harness.ReadLog(t, sink) {
			if record.Level == "error" || record.Level == "fatal" {
				out = append(out, record)
			}
		}
	}
	return out
}
