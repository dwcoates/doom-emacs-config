//go:build integration

package integration

import (
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/integration/harness"
	"agent-repld/internal/ratelimittest"

	"connectrpc.com/connect"
)

// ---------------------------------------------------------------------------
// Footer: the status tree
// ---------------------------------------------------------------------------

func TestFooterStatusTreeFollowsIdleWorkingDone(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newCanonical(t)
	footer := f.d.WatchFooter(f.ws)
	awaitFooter(t, f, footer, "idle.ready before any turn", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetIdle().GetReady() != nil
	})

	// Act: the fake does not answer StartTurn until the footer has been seen
	// submitting, because the daemon moves it on to thinking the moment the
	// answer lands and a reader that is slow to take pushes would never see
	// the submitting view between them. The submission waits on that answer,
	// so it is sent from a goroutine.
	f.shim.Hang()
	submitted := make(chan error, 1)
	go func() {
		_, err := f.d.Client().SubmitPrompt(f.d.Ctx(), connect.NewRequest(&agentreplv1.SubmitPromptRequest{
			Workspace:      f.ws,
			Said:           said("do it"),
			IdempotencyKey: "k-tree",
			Origin:         conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT,
		}))
		submitted <- err
	}()

	// Assert: submitting, then thinking, then done once the turn concludes.
	awaitFooter(t, f, footer, "working.submitting on StartTurn", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetWorking().GetSubmitting() != nil
	})
	f.shim.Unhang()
	if err := <-submitted; err != nil {
		t.Fatalf("SubmitPrompt(\"do it\") = error %v, want an answer", err)
	}
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("think-1"),
		Item:       &conversationv1.AgentActivity_Thinking{Thinking: &conversationv1.AgentThinking{Result: &conversationv1.AgentThinking_Start{Start: &conversationv1.AgentThinkingStart{}}}},
	}))
	awaitFooter(t, f, footer, "the working status while reasoning runs", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetWorking() != nil
	})
	f.shim.PushAgentFrame(mainAgent, successFrame(mainAgent, nil))
	awaitFooter(t, f, footer, "idle.done after the turn concludes", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetIdle().GetDone() != nil
	})
}

// ftDwell is the momentary-status dwell the retirement test runs the
// daemon with, in place of footer.DefaultMomentaryDwell's 1.5s.
//
// MEASURED BASIS: a push that IS coming arrives on an open stream at a p90 of
// 5.8ms and a p50 of 0.4ms over this suite at -parallel 8, so 150ms is ~25x the
// p90 — wide enough that the momentary status and its successor remain two
// distinct, separately observed pushes rather than a coalesced one, and short
// enough that the test does not spend its wall time waiting on a window sized for a
// reader.
const ftDwell = 150 * time.Millisecond

// ---------------------------------------------------------------------------
// Footer: the tokens cell
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// Footer: live-work chips
// ---------------------------------------------------------------------------

// A MONITOR ROW JUMPS TO ITS CALL'S TOOL-CALL CARD: the feed draws the card
// and announces it, and the footer row names exactly that FeedId.
func TestFooterMonitorRowJumpsToTheMonitorsToolCallCard(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newCanonical(t)
	feed := f.watchRootFeed()
	footer := f.d.WatchFooter(f.ws)

	// Act
	pushMonitor(f.shim, "mon-1", "watching the build log")

	// Assert
	card := awaitRow(t, f, feed, "the monitor's tool-call card", func(r *frontendv1.FeedRow) bool {
		return r.GetActivity().GetSimpleToolCall().GetName().GetText() == "Monitor"
	})
	awaitFooter(t, f, footer, "the monitor row naming the card", func(v *frontendv1.FooterView) bool {
		rows := v.GetExpanded().GetMonitors().GetRows()
		return len(rows) == 1 && rows[0].GetJump().GetEntry().GetValue() == card.GetId().GetValue()
	})
}

// movedSubagent announces that an in-turn SPAWN's work left for the
// background. The handle IS the spawning call's own id, so one identity
// addresses the run, the unit it moved out of, and the book it writes.
func movedSubagent(unit string) *conversationv1.AgentDetachedWork {
	return &conversationv1.AgentDetachedWork{
		Work: &conversationv1.DetachedWorkId{Value: unit},
		// The spawn's own id IS its created agent (the minting rule).
		Kind: subagentKind(unit),
		Origin: &conversationv1.AgentDetachedWork_Detached{Detached: &conversationv1.DetachedWorkDetached{
			DetachedFromId: activityID(unit),
			Cause:          &conversationv1.DetachedWorkDetached_Requested{Requested: &conversationv1.DetachedCauseRequested{}},
		}},
	}
}

// ftSubagentSpawn is a spawn on the CALLER's own activity stream.
func ftSubagentSpawn(unit, created, label string) *conversationv1.AgentActivity {
	return &conversationv1.AgentActivity{
		ActivityId: activityID(unit),
		Item: &conversationv1.AgentActivity_Subagent{Subagent: &conversationv1.AgentSubagent{
			Result: &conversationv1.AgentSubagent_Start{Start: &conversationv1.AgentSubagentStart{
				CreatedAgentId: &conversationv1.AgentId{Value: created},
				Prompt:         &conversationv1.AgentSubagentPrompt{Text: label},
				StartedAt:      startedAt(1_700_000_000_000),
			}},
		}},
	}
}

// ftSubagentSettled is a spawn's terminal, whichever unit id carries it. The
// terminal names the created agent so the fixture does not also manufacture a
// producer-fault warning unrelated to the chip lifecycle under test.
func ftSubagentSettled(unit, created string) *conversationv1.AgentActivity {
	return &conversationv1.AgentActivity{
		ActivityId: activityID(unit),
		Item: &conversationv1.AgentActivity_Subagent{Subagent: &conversationv1.AgentSubagent{
			Result: &conversationv1.AgentSubagent_Success{Success: &conversationv1.AgentSubagentSuccess{
				CreatedAgentId: &conversationv1.AgentId{Value: created},
			}},
		}},
	}
}

// A DETACHED RUN'S TERMINAL ARRIVES ON ITS OWN BOOK, under an activity id of
// that book's own minting -- nothing joins it back to the spawn unit the chip
// row is keyed by. The row outlived the run for the rest of the session, which
// is what the G50 playbook read: two settled placements, one live, chip of 3.
func TestFooterAgentsChipRetiresADetachedRunOnItsOwnStream(t *testing.T) {
	t.Parallel()
	// Arrange: one spawn, detached under its own handle.
	f := newCanonical(t)
	footer := f.d.WatchFooter(f.ws)
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, ftSubagentSpawn("toolu-1", "toolu-1", "sweep the tree")))
	f.shim.PushAgentFrame(mainAgent, detachedWorkFrame(mainAgent, movedSubagent("toolu-1")))
	awaitDetachedRunLive(t, f, footer)

	// Act: the run settles on ITS OWN stream, under that book's own unit id.
	f.shim.PushAgentFrame("toolu-1", activityFrame("toolu-1", ftSubagentSettled("sub-unit-9", "toolu-1")))

	// Assert
	awaitDetachedRunsSettled(t, f, footer)
}

// AND THE OTHER DELIVERY, which is the one the vendor's task notification
// takes: the same run's terminal settled on the SPAWNING agent's book, under
// the spawn unit. Both are the handle's terminal and both must retire the chip.
func TestFooterAgentsChipRetiresADetachedRunSettledOnTheCallersStream(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newCanonical(t)
	footer := f.d.WatchFooter(f.ws)
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, ftSubagentSpawn("toolu-1", "toolu-1", "sweep the tree")))
	f.shim.PushAgentFrame(mainAgent, detachedWorkFrame(mainAgent, movedSubagent("toolu-1")))
	awaitDetachedRunLive(t, f, footer)

	// Act
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, ftSubagentSettled("toolu-1", "toolu-1")))

	// Assert
	awaitDetachedRunsSettled(t, f, footer)
}

// ---------------------------------------------------------------------------
// Footer: the allowance cell — drawn from the vendor's rate-limit event.
// ---------------------------------------------------------------------------

// A SEVEN-DAY WINDOW'S WARNING FEEDS THE WEEKLY ALLOWANCE, its verdict and its
// figure, on the enduring usage line alone: the event is never a salient line
// (owner ruling, 2026-10-01). Both hops are asserted, the drawn allowance and
// the session-arm record behind it, because the allowance alone would not say
// the event's own window reached the footer's session-update path.
func TestFooterWeeklyAllowanceDrawnFromASevenDayWarningCarriesItsVerdictAndFigureAndNoSalientLine(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newCanonical(t)
	footer := f.d.WatchFooter(f.ws)
	awaitFooter(t, f, footer, "idle before any rate-limit event", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetIdle() != nil
	})
	resetsAtMs := time.Now().Add(5 * time.Hour).UnixMilli()
	status := ratelimittest.Figured(ratelimittest.AllowedWarning(ratelimittest.SevenDay()), 91, resetsAtMs)

	// Act
	f.shim.PushSessionUpdate(ratelimittest.Update(status))

	// Assert
	got := awaitFooter(t, f, footer, "the weekly allowance's warning at the event's figure", func(v *frontendv1.FooterView) bool {
		allowance := v.GetStrip().GetStatus().GetIdle().GetActivity().GetUnpinned().GetEnduring().GetUsage().GetWeekly().GetOverall()
		return allowance.GetAllowedWarning() != nil
	})
	allowance := got.GetStrip().GetStatus().GetIdle().GetActivity().GetUnpinned().GetEnduring().GetUsage().GetWeekly().GetOverall()
	if utilization := allowance.GetUtilization(); utilization < 0.905 || utilization > 0.915 {
		t.Fatalf("the weekly allowance's utilization = %v, want the event's 0.91 as a 0..1 fraction", utilization)
	}
	if allowance.GetResetsAtS() != resetsAtMs/1000 {
		t.Fatalf("the weekly allowance's resets_at_s = %d, want the event's reset in epoch seconds %d", allowance.GetResetsAtS(), resetsAtMs/1000)
	}
	if salient := got.GetStrip().GetStatus().GetIdle().GetActivity().GetSalient(); salient != nil {
		t.Fatalf("the footer stands salient line %v, want none: a rate-limit event feeds the enduring usage line only", salient)
	}
	f.d.AwaitWorkspaceLogRecord(f.ws.GetDir(), "the footer's session-update record for the rate-limit arm", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.footer.on_session_update" && r.Context["arm"] == "rate_limit_status"
	})
}

// ---------------------------------------------------------------------------
// Deny-and-continue.
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// Flush-on-accept.
// ---------------------------------------------------------------------------

func TestWatchWebWorkspaceOpensWithTheOpenedWorkspacesSessionIdentity(t *testing.T) {
	t.Parallel()
	// Arrange / Act: this test used to assert the OPPOSITE -- that a fresh
	// WatchWebWorkspace carries no frame at all, because the stream had only
	// the `transferred` event and no state of its own. Landing 15 gave it
	// one: the page binds its log context from `session_identity`, so the
	// daemon composes the identity before every subscribe and the topic
	// replays it to each new subscriber. The genuine no-view flush-on-accept
	// case now lives on WatchDaemon
	// (TestWatchDaemonFlushesHeadersBeforeAnyFrameWhenNoDrainWasEverScheduled),
	// and this stream's header flush is covered per kind by
	// TestFlushOnAcceptAcrossWatchKinds.
	//
	// What this asserts instead is the fact only an OPENED workspace can
	// show, and the one the whole landing exists for: the identity the page
	// stamps its forwarded log records with is the session the daemon is
	// actually operating, not an empty placeholder.
	f := newCanonical(t)
	web := f.d.WatchWeb(f.ws)

	// Assert.
	push := harness.AwaitView(t, f.d.Ctx(), web, "WatchWebWorkspace: the opened workspace's session identity",
		func(r *agentreplv1.WatchWebWorkspaceResponse) bool {
			return r.GetSessionIdentity().GetAgentReplSessionId() != ""
		})
	if got := push.GetSessionIdentity().GetAgentReplSessionId(); got == "" {
		t.Fatalf("WatchWebWorkspace on an opened workspace = agent_repl_session_id %q, want the operated session's", got)
	}
}

// ---------------------------------------------------------------------------
// Footer: wakeup fallback and precedence
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// Footer: link death
// ---------------------------------------------------------------------------

func TestFooterLinkDeathFlipsToSeveredAndTheDaemonRedials(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newCanonical(t)
	// The sweep covers every test; the declared records are evidence of a session fault the test opens, the shim link the test severs.
	f.d.ExpectWarnings("daemon.sessionwatcher.reopen", "daemon.health.open_fault", "daemon.sessionwatcher.link_fault",
		"daemon.sessionwatcher.watch_session", "daemon.shimclient.redial")
	footer := f.d.WatchFooter(f.ws)
	roster := f.d.WatchRoster()
	awaitFooter(t, f, footer, "idle before the link dies", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetIdle() != nil
	})
	watchesBefore := f.shim.Count(harness.RPCWatchSession)
	// THE SEVERING LASTS UNTIL IT IS SEEN: the daemon redials at once, and a
	// footer reader slower than the redial saw only the link restored.
	f.shim.HoldSessionReopen()

	// Act: sever the session stream.
	f.shim.DropStream(harness.StreamSession)

	// Assert: the footer flips to disconnected.severed and the roster agrees.
	awaitFooter(t, f, footer, "disconnected.severed after the link dies", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetAgentReplFault().GetSevered() != nil
	})
	awaitRoster(t, f.d, roster, "the roster's severed status", func(r *frontendv1.RosterPush) bool {
		row := rosterRow(r, f.ws.GetId())
		return row != nil && row.GetSevered() != nil
	})
	f.shim.ReleaseSessionReopen()

	// Assert: the daemon redials — a fresh WatchSession is opened without any
	// client action.
	f.shim.AwaitCount(harness.RPCWatchSession, watchesBefore+1)
}

func TestFooterShimExitFlipsToDeadAndStopsRedials(t *testing.T) {
	t.Parallel()
	// Arrange
	// THE REVIVAL'S HELD START IS BOUNDED SHORT, so the daemon settles on its
	// failure and the test barrier can say nothing more is coming (the
	// suite's default bound is the production minute).
	f := newOpened(t, harness.Opts{ExtraEnv: []string{"AGENT_REPL_START_SESSION_BOUND=300ms"}})
	// The exit was not attributed to a daemon-requested kill, so
	// publishExit's ELSE branch fires: daemon.shimclient.exit at ERROR
	// ("shim died"). Nothing else observes this exit (no query_died update
	// was pushed -- the process simply exited).
	// The shim's own standing streams end WITH IT, and the session never
	// ended, which is exactly what the session watcher records at ERROR for
	// each of the two. They are the same crash the exit record names.
	// The lost link is also RECORDED as the session's own fault, which is what
	// SessionHealth answers with: the watcher says so and the reporter opens
	// it.
	// daemon.shimclient.redial is the adopted-death witness doing its job: the
	// monitor can see the stream break before the exit is decoded, so it says
	// "shim link broke; redialing" and then, the moment the death is evidence,
	// "redial stopped" -- which is precisely the stop this test asserts.
	// Before the witness was wired the redials looped forever instead; both
	// records are failure-path evidence and stay loud.
	f.d.ExpectWarnings("daemon.sessionwatcher.reopen", "daemon.shimclient.exit", "daemon.shimclient.redial",
		"daemon.sessionwatcher.watch_session", "daemon.sessionwatcher.watch_agent",
		"daemon.sessionwatcher.link_fault", "daemon.health.open_fault",
		// The revival's start, held until its bound fails it.
		"daemon.workspace.bring_up", "daemon.promptqueue.revive", "daemon.promptqueue.submit",
		"daemon.shimclient.start_session")
	footer := f.d.WatchFooter(f.ws)
	roster := f.d.WatchRoster()
	awaitFooter(t, f, footer, "idle before the shim exits", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetIdle() != nil
	})

	// The daemon brings a shim that died on its own straight back; the
	// revived shim HOLDS its StartSession, so the dead state this test is
	// about stands for as long as the test looks at it.
	f.d.WriteShimProfile(f.repo.Dir, harness.ShimProfile{HangStartSession: true})

	// Act: the fake shim process exits outright.
	f.shim.Exit(1, "simulated crash")

	// Assert: the footer flips to disconnected.dead and the roster agrees.
	awaitFooter(t, f, footer, "disconnected.dead after the shim exits", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetAgentReplFault().GetDead() != nil
	})
	awaitRoster(t, f.d, roster, "the roster's dead status", func(r *frontendv1.RosterPush) bool {
		row := rosterRow(r, f.ws.GetId())
		return row != nil && row.GetDead() != nil
	})

	// Assert: no further churn — a dead shim gets no more redial attempts.
	// The revival the daemon starts for it may retract the death's fault line
	// as its new shim attaches (held at StartSession above, until the start's
	// bound fails it), so pushes may come; once the daemon is idle, none of
	// them is a redial's severed step.
	harness.ExpectNoPushMatching(t, footer,
		"no redial once the shim is dead: the footer never steps to severed",
		func(v *frontendv1.FooterView) bool {
			return v.GetStrip().GetStatus().GetAgentReplFault().GetSevered() != nil
		})
}

// ---------------------------------------------------------------------------
// Topbar
// ---------------------------------------------------------------------------

func TestTopbarModelSelectorReflectsTheCatalogAndTheEffectiveModel(t *testing.T) {
	t.Parallel()
	// Arrange / Act
	f := newCanonical(t)
	topbar := f.d.WatchTopbar(f.ws)

	// Assert: the fake's default StartSession answer serves the fixed
	// [opus, sonnet, haiku] catalog with its display names verbatim, and the
	// selector's `selected` is exactly the fake's effective model, "opus".
	got := awaitTopbar(t, f, topbar, "the model selector resolved from the catalog", func(v *frontendv1.TopbarView) bool {
		return len(v.GetModelSelector().GetOptions()) > 0 && v.GetModelSelector().GetSelected() != nil
	})
	if got.GetModelSelector().GetSelected().GetModel().GetName() != "opus" {
		t.Fatalf("model selector selected = %q, want the fake's effective model \"opus\"", got.GetModelSelector().GetSelected().GetModel().GetName())
	}
	wantOptions := []struct{ name, display string }{
		{"opus", "Opus"}, {"sonnet", "Sonnet"}, {"haiku", "Haiku"},
	}
	options := got.GetModelSelector().GetOptions()
	if len(options) != len(wantOptions) {
		t.Fatalf("model selector options = %v (%d), want exactly %d: opus, sonnet, haiku", options, len(options), len(wantOptions))
	}
	for i, want := range wantOptions {
		if options[i].GetModel().GetName() != want.name || options[i].GetDisplayName() != want.display {
			t.Fatalf("model selector options[%d] = {name:%q, display:%q}, want {name:%q, display:%q}",
				i, options[i].GetModel().GetName(), options[i].GetDisplayName(), want.name, want.display)
		}
	}
}

// ---------------------------------------------------------------------------
// helpers — prefixed ft* so they cannot collide with another suite's file.
// ---------------------------------------------------------------------------

// ftMonitorActivity builds a persistent monitor's arming unit.
func ftMonitorActivity(id, description string) *conversationv1.AgentActivity {
	return &conversationv1.AgentActivity{
		ActivityId: activityID(id),
		Item: &conversationv1.AgentActivity_Monitor{Monitor: &conversationv1.AgentMonitor{
			Result: &conversationv1.AgentMonitor_Start{Start: &conversationv1.AgentMonitorStart{
				Description: description,
				Lifetime:    &conversationv1.AgentMonitorStart_Persistent{Persistent: &conversationv1.AgentMonitorPersistent{}},
				StartedAtMs: 1_700_000_000_000,
			}},
		}},
	}
}

// pushMonitor pushes a main-agent persistent monitor the way a producer
// states one: its arming unit (the call's card), then the announcement that
// the work is live. The watcher lists a monitor only from its announcement,
// and the footer draws only what the watcher lists.
func pushMonitor(shim *harness.ShimControl, id, description string) {
	arming := ftMonitorActivity(id, description)
	shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, arming))
	shim.PushAgentFrame(mainAgent, detachedWorkFrame(mainAgent, &conversationv1.AgentDetachedWork{
		Work:  &conversationv1.DetachedWorkId{Value: id},
		Owner: &conversationv1.AgentId{Value: mainAgent},
		Kind:  &conversationv1.DetachedWorkKind{Kind: &conversationv1.DetachedWorkKind_Monitor{Monitor: &conversationv1.DetachedWorkKindMonitor{}}},
		Origin: &conversationv1.AgentDetachedWork_Created{Created: &conversationv1.DetachedWorkCreated{
			WorkCreated: &conversationv1.DetachableWork{Work: &conversationv1.DetachableWork_Monitor{Monitor: arming.GetMonitor()}},
		}},
	}))
}

// ---------------------------------------------------------------------------
// Connectivity truth per hop (daemon.md invariant 11): a workspace is
// CONNECTED only while its shim.v1 WatchSession, its WatchHostWorkspace and its
// web hop are all live. The web hop is down only for a page that CONNECTED and
// went away unannounced; a page not yet connected, or one Emacs unloaded on
// purpose (ReportPageUnloading), is awaited and never drawn as a fault (owner
// ruling, 2026-10-20).
// ---------------------------------------------------------------------------

func TestFooterReturnsToNotConnectedWhenTheWebHopGoesAway(t *testing.T) {
	t.Parallel()
	// Arrange: every hop up.
	f := newCanonical(t)
	footer := f.d.WatchFooter(f.ws)
	awaitFooter(t, f, footer, "idle with every hop live", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetIdle() != nil
	})

	// Act: the page goes away.
	f.web.Close()

	// Assert
	awaitFooter(t, f, footer, "disconnected once the web hop is cancelled", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetAgentReplFault() != nil
	})
}

// ---------------------------------------------------------------------------
// Footer: the activity cell's tiers
// ---------------------------------------------------------------------------

// ftFeedItemDerivedLine matches the wording of a line composed from a feed item
// that landed ("✅ Read finished — handling result...", "❌ Bash failed",
// "✅ Prompt delivered — awaiting response...", "✅ Moved to background —
// continuing..."): the retired quiet tier's lines (owner ruling, 2026-10-01).

func TestFooterLoadingStatusIsRetiredByADaemonSideDwell(t *testing.T) {
	t.Parallel()
	// Arrange: the dwell is compressed to ftDwell, for the reason its comment
	// states.
	f := newOpened(t, harness.Opts{FooterMomentaryDwell: ftDwell})
	footer := f.d.WatchFooter(f.ws)
	f.submit("do it", "k-loading", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	awaitFooter(t, f, footer, "thinking before the injection", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetWorking() != nil
	})

	// Act: a memory file is silently injected into context.
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("inject-1"),
		Item: &conversationv1.AgentActivity_ContextInjected{ContextInjected: &conversationv1.AgentContextInjected{
			Injected: &conversationv1.AgentContextInjected_Memory{Memory: &conversationv1.AgentInjectedMemory{Path: "CLAUDE.md", Content: "be terse\n"}},
		}},
	}))

	// Assert: MOMENTARY loading, then a daemon-side dwell falls back to the
	// standing status (thinking) with no client action.
	awaitFooter(t, f, footer, "the momentary loading status", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetLoading() != nil
	})
	awaitFooter(t, f, footer, "the dwell's successor push retiring loading", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetLoading() == nil
	})
}
