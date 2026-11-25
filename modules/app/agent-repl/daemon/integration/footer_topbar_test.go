//go:build integration

package integration

import (
	"regexp"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/integration/harness"
	"agent-repld/internal/ratelimittest"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/encoding/prototext"
)

func TestFooterExpandedPanelsArrivePopulatedOnEveryPush(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newCanonical(t)
	footer := f.d.WatchFooter(f.ws)

	// Act: force a push by starting a turn.
	f.submit("go", "k-panels", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)

	// Assert: every panel field is present (non-nil), whether or not it holds
	// rows — ALL panels arrive populated on every push, per the contract.
	got := awaitFooter(t, f, footer, "the footer after StartTurn", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetWorking() != nil
	})
	exp := got.GetExpanded()
	if exp == nil {
		t.Fatal("FooterView.expanded = nil, want the expanded section always resolved")
	}
	if exp.GetTokens() == nil || exp.GetAgents() == nil || exp.GetTasks() == nil ||
		exp.GetShells() == nil || exp.GetMonitors() == nil || exp.GetCrons() == nil {
		t.Fatalf("FooterExpanded = %v, want every panel (tokens/agents/tasks/shells/monitors/crons) resolved", exp)
	}
}

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

// ftPanelInput is the tokens panel's summed uncached-input line: the spend
// across every agent. The strip's cell is the main agent's context growth, so
// the uncached-spend contract is asserted here, on the panel.
func ftPanelInput(v *frontendv1.FooterView) string {
	return v.GetExpanded().GetTokens().GetInput().GetValue()
}

func TestFooterTokensPanelUsageIsNotDoubleCountedAcrossAResponsesUnits(t *testing.T) {
	t.Parallel()
	// Arrange: one API response whose usage is stamped on the FIRST unit
	// only, per the envelope contract.
	f := newCanonical(t)
	footer := f.d.WatchFooter(f.ws)
	f.submit("go", "k-tok-dup", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)

	// Act: the response's first unit carries usage — 1000 input tokens (all
	// misses), so the canonical formatter's line is exactly "1k".
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, ftUsageActivity("resp-unit-1", ftUsage(1000, 0, 0))))
	// The line is UNSET until usage lands, so the usage-carrying push is the
	// first one with a figure.
	firstUnit := awaitFooter(t, f, footer, "the tokens panel after the usage-carrying unit", func(v *frontendv1.FooterView) bool {
		return ftPanelInput(v) != ""
	})
	if ftPanelInput(firstUnit) != "1k" {
		t.Fatalf("panel input = %q for 1000 input tokens, want the canonical formatter's exact \"1k\"", ftPanelInput(firstUnit))
	}

	// Act: the SAME response's second unit (a tool call in the same
	// assistant message) carries NO usage.
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("resp-unit-2"),
		Item: &conversationv1.AgentActivity_Read{Read: &conversationv1.AgentRead{
			Result: &conversationv1.AgentRead_Start{Start: &conversationv1.AgentReadStart{Path: &conversationv1.ReadPath{Path: "a.go"}}},
		}},
	}))

	// Act: end the turn. A whole view identical to the last one is never
	// pushed, so the turn's terminal is what makes the post-second-unit line
	// observable at all.
	f.shim.PushAgentFrame(mainAgent, successFrame(mainAgent, nil))

	// Assert: the line is unchanged — a second unit of the SAME response
	// leaving usage unset must not add a second charge.
	stillOne := awaitFooter(t, f, footer, "the footer after the unstamped second unit", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetIdle().GetDone() != nil
	})
	if ftPanelInput(stillOne) != ftPanelInput(firstUnit) {
		t.Fatalf("panel input = %q after the second unit, want it unchanged at %q: usage rides exactly one unit per response",
			ftPanelInput(stillOne), ftPanelInput(firstUnit))
	}
}

// ---------------------------------------------------------------------------
// Footer: live-work chips
// ---------------------------------------------------------------------------

// A CREATED CRON JOB STANDS ON THE FOOTER'S CRONS CHIP AFTER ITS TURN ENDS. The
// job is left standing by the call that made it, so the chip is a state every
// footer reader is handed, however far behind: a footer opened only after the
// turn concluded still counts the one job.
func TestFooterCronsChipCountsACreatedJobForAReaderOpenedAfterTheTurnEnded(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newCanonical(t)
	f.submit("go", "k-cron-create", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	feed := f.watchRootFeed()

	// Act: the agent creates one job, then the turn concludes.
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("cron-create-1"),
		Item: &conversationv1.AgentActivity_Cron{Cron: &conversationv1.AgentCron{State: &conversationv1.AgentCron_Start{Start: &conversationv1.AgentCronStart{
			Act:       &conversationv1.AgentCronStart_Create{Create: &conversationv1.AgentCronCreate{Cron: "*/5 * * * *", Prompt: "check the build", Recurring: true}},
			StartedAt: startedAt(1),
		}}}},
	}))
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("cron-create-1"),
		Item: &conversationv1.AgentActivity_Cron{Cron: &conversationv1.AgentCron{State: &conversationv1.AgentCron_Success{Success: &conversationv1.AgentCronSuccess{
			Act:       &conversationv1.AgentCronSuccess_Created{Created: &conversationv1.AgentCronCreated{JobId: "job-1", HumanSchedule: "every 5 minutes", Recurring: true}},
			SettledAt: settledAt(2),
		}}}},
	}))
	pushConcludedTurn(f.shim, mainAgent, "resp-cron")
	awaitRow(t, f, feed, "the turn's conclusion", func(r *frontendv1.FeedRow) bool { return r.GetTurnEnded().GetConcluded() != nil })
	footer := f.d.WatchFooter(f.ws)

	// Assert: the one standing job is on the chip.
	got := awaitFooter(t, f, footer, "the crons chip for the standing job", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetLiveWork().GetCrons() != nil
	})
	if count := got.GetStrip().GetLiveWork().GetCrons().GetCount(); count != 1 {
		t.Fatalf("the footer's crons chip counts %d jobs, want the 1 standing job", count)
	}
}

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

func TestDenyAndContinueKeepsTheFooterThinkingUntilTheFakesOwnTerminal(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newCanonical(t)
	f.submit("go", "k-deny-continue", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()
	footer := f.d.WatchFooter(f.ws)
	awaitFooter(t, f, footer, "thinking before the permission ask", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetWorking() != nil
	})
	f.shim.PushAgentFrame(mainAgent, updateFrame(mainAgent, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_Permission{Permission: openPermission("perm-deny-cont", "act-deny-cont")},
	}))
	row := awaitRow(t, f, tail, "the open permission card", func(r *frontendv1.FeedRow) bool { return r.GetPermission().GetOpen() != nil })

	// Act: deny the permission.
	resp, err := f.d.Client().AnswerPermission(f.d.Ctx(), connect.NewRequest(&agentreplv1.AnswerPermissionRequest{
		Workspace: f.ws, Permission: row.GetId(),
		Answer: &agentreplv1.AnswerPermissionRequest_Deny{Deny: &agentreplv1.AnswerPermissionDeny{
			Reason: &agentreplv1.AnswerPermissionDenyReason{Text: "no"},
		}},
	}))
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("AnswerPermission{deny} = %v, %v, want a success", resp, err)
	}
	// The card itself re-pushes answered -- consuming that push first keeps
	// the ExpectNoPush below honest about what comes AFTER the deny.
	awaitRow(t, f, tail, "the answered (denied) permission card", func(r *frontendv1.FeedRow) bool {
		return r.GetId().GetValue() == row.GetId().GetValue() && r.GetPermission().GetAnswered() != nil
	})
	// The settled ask resumes the turn's prompt: it re-pushes still working and
	// no longer awaiting the user, and is consumed here for the same reason.
	awaitRow(t, f, tail, "the prompt resumed after the deny", func(r *frontendv1.FeedRow) bool {
		return r.GetUserPrompt().GetWorking() && !r.GetUserPrompt().GetAwaitingUser()
	})

	// Assert: the footer stays thinking (a deny does not end the turn) and no
	// turn_ended row is drawn until the fake pushes the turn's own terminal.
	got := awaitFooter(t, f, footer, "thinking still standing after the deny", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetWorking() != nil
	})
	if got.GetStrip().GetStatus().GetIdle() != nil {
		t.Fatalf("footer status = %v after a permission deny, want the turn still in flight", got.GetStrip().GetStatus())
	}
	harness.ExpectNoPush(t, tail, "no turn_ended row until the fake's own terminal")

	// Act: the fake concludes the turn on its own.
	f.shim.PushAgentFrame(mainAgent, successFrame(mainAgent, nil))

	// Assert: now, and only now, the terminal lands.
	awaitRow(t, f, tail, "the turn's terminal row after the fake's own conclusion", func(r *frontendv1.FeedRow) bool {
		return r.GetTurnEnded() != nil
	})
}

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

func TestFooterWakeupShowsOnlyWhenNothingElseStands(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newCanonical(t)
	footer := f.d.WatchFooter(f.ws)
	awaitFooter(t, f, footer, "idle before scheduling a wakeup", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetIdle() != nil
	})

	// Act: the agent self-schedules a wakeup while idle, for the scheduled
	// instant 1_700_000_060_000 (ftWakeupScheduleActivity's fixed WakeAtMs).
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, ftWakeupScheduleActivity("wake-1", 60)))

	// Assert: the wakeup fallback stands because nothing else does, and its
	// activity carries the EXACT scheduled instant, not merely a non-nil arm.
	got := awaitFooter(t, f, footer, "waiting.wakeup with nothing else standing", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetWaiting().GetWakeup() != nil
	})
	if got.GetStrip().GetStatus().GetWaiting().GetActivity().GetSalient().GetWakeup().GetWakeAtMs() != 1_700_000_060_000 {
		t.Fatalf("waiting.activity.wakeup.wake_at_ms = %d, want exactly 1_700_000_060_000 (the scheduled instant)",
			got.GetStrip().GetStatus().GetWaiting().GetActivity().GetSalient().GetWakeup().GetWakeAtMs())
	}
}

func TestFooterApiErrorMidTurnDrawsRetryingEvidenceWithoutEndingTheTurn(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newCanonical(t)
	footer := f.d.WatchFooter(f.ws)
	f.submit("go", "k-api-error", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	awaitFooter(t, f, footer, "thinking before the mid-turn error", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetWorking() != nil
	})

	// Act: a 429 mid-turn, recorded but recovered from.
	f.shim.PushAgentFrame(mainAgent, updateFrame(mainAgent, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_ApiError{ApiError: &conversationv1.ApiRequestFailed{
			Message: "rate limited",
			Kind:    &conversationv1.ApiRequestFailed_RateLimited{RateLimited: &conversationv1.ApiRateLimited{}},
		}},
	}))

	// Assert: the turn is not ended; it is blocked · api_retrying with the
	// retry line, the status a turn whose call the vendor is retrying stands
	// in (owner ruling, 2026-10-01: the turn cannot advance until the retried
	// call is answered).
	got := awaitFooter(t, f, footer, "blocked.api_retrying with the retry line mid-turn", func(v *frontendv1.FooterView) bool {
		blocked := v.GetStrip().GetStatus().GetVendorFault()
		return blocked.GetApiRetrying() != nil && blocked.GetActivity().GetSalient().GetRetrying() != nil
	})
	if got.GetStrip().GetStatus().GetIdle() != nil {
		t.Fatalf("footer status = %v after a recovered mid-turn api_error, want the turn still in flight", got.GetStrip().GetStatus())
	}

	// Act: the turn concludes normally — the frame-level terminal is
	// authoritative, not the mid-turn evidence.
	f.shim.PushAgentFrame(mainAgent, successFrame(mainAgent, nil))
	awaitFooter(t, f, footer, "idle.done once the turn's own terminal lands", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetIdle().GetDone() != nil
	})
}

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

func TestTopbarContextBreakdownRowsCarrySharePermilleAndEmphasized(t *testing.T) {
	t.Parallel()
	// Arrange: a turn whose single usage-carrying unit fixes the session
	// breakdown's basis at a round 1000 tokens (100 uncached input + 900
	// cache read), so each row's share_permille is an exact, checkable figure.
	f := newCanonical(t)
	topbar := f.d.WatchTopbar(f.ws)
	f.submit("go", "k-breakdown-shares", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)

	// Act
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, ftUsageActivity("resp-shares", ftUsage(100, 0, 900))))

	// Assert: the session section's headline rows carry the section's own
	// precomputed share (permille of its own basis) and are drawn emphasized;
	// the nested detail row carries neither.
	// Every section ships every row from the first view on, at zero, so the
	// wait is for the FIGURES to arrive rather than for the row to exist.
	got := awaitTopbar(t, f, topbar, "the context chip's breakdown after the usage-carrying unit", func(v *frontendv1.TopbarView) bool {
		return ftFindBreakdownRow(v.GetContext().GetBreakdown(), "uncached input").GetTokens() > 0
	})
	breakdown := got.GetContext().GetBreakdown()
	uncached := ftFindBreakdownRow(breakdown, "uncached input")
	if uncached.GetTokens() != 100 || uncached.GetSharePermille() != 100 || !uncached.GetEmphasized() {
		t.Fatalf("\"uncached input\" row = %+v, want tokens=100, share_permille=100, emphasized=true", uncached)
	}
	cacheRead := ftFindBreakdownRow(breakdown, "cache read")
	if cacheRead.GetTokens() != 900 || cacheRead.GetSharePermille() != 900 || !cacheRead.GetEmphasized() {
		t.Fatalf("\"cache read\" row = %+v, want tokens=900, share_permille=900, emphasized=true", cacheRead)
	}
	freshInput := ftFindBreakdownRow(breakdown, "fresh input")
	if freshInput.GetSharePermille() != 0 || freshInput.GetEmphasized() {
		t.Fatalf("\"fresh input\" detail row = %+v, want no share_permille and not emphasized (it is a partition of the headline above it)", freshInput)
	}
}

// ---------------------------------------------------------------------------
// helpers — prefixed ft* so they cannot collide with another suite's file.
// ---------------------------------------------------------------------------

// ftUsage builds a TokenUsage from its three input buckets.
func ftUsage(unwritten, written, cacheRead uint64) *conversationv1.TokenUsage {
	return &conversationv1.TokenUsage{
		InputHits:   &conversationv1.TokenCacheHits{Read: cacheRead},
		InputMisses: &conversationv1.TokenCacheMisses{Written: written, Unwritten: unwritten},
	}
}

// ftUsageActivity wraps a usage figure on the envelope of a plain response
// unit, as the first unit of an API response would carry it.
func ftUsageActivity(id string, usage *conversationv1.TokenUsage) *conversationv1.AgentActivity {
	return &conversationv1.AgentActivity{
		ActivityId: activityID(id),
		Usage:      usage,
		Item: &conversationv1.AgentActivity_Response{Response: &conversationv1.AgentResponse{
			Result: &conversationv1.AgentResponse_Start{Start: &conversationv1.AgentResponseStart{}},
		}},
	}
}

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

// ftWakeupScheduleActivity builds a self-scheduled wakeup's start-then-answer
// pair as one settled unit (the daemon needs the success to know a wakeup is
// really pending).
func ftWakeupScheduleActivity(id string, delaySeconds uint32) *conversationv1.AgentActivity {
	return &conversationv1.AgentActivity{
		ActivityId: activityID(id),
		Item: &conversationv1.AgentActivity_ScheduleWakeup{ScheduleWakeup: &conversationv1.AgentScheduleWakeup{
			Result: &conversationv1.AgentScheduleWakeup_Success{Success: &conversationv1.AgentScheduleWakeupSuccess{
				Outcome: &conversationv1.AgentScheduleWakeupSuccess_Scheduled{Scheduled: &conversationv1.AgentScheduleWakeupScheduled{
					WakeAtMs: 1_700_000_060_000,
				}},
			}},
		}},
	}
}

// ftFindBreakdownRow searches every section of a token-breakdown menu for a
// row with the given label.
func ftFindBreakdownRow(v *frontendv1.TokenBreakdownView, label string) *frontendv1.TokenBreakdownRow {
	for _, section := range v.GetSections() {
		for _, row := range section.GetRows() {
			if row.GetLabel() == label {
				return row
			}
		}
	}
	return nil
}

// ---------------------------------------------------------------------------
// Connectivity truth per hop (daemon.md invariant 11): a workspace is
// CONNECTED only while its shim.v1 WatchSession, its WatchHostWorkspace and its
// web hop are all live. The web hop is down only for a page that CONNECTED and
// went away unannounced; a page not yet connected, or one Emacs unloaded on
// purpose (ReportPageUnloading), is awaited and never drawn as a fault (owner
// ruling, 2026-10-20).
// ---------------------------------------------------------------------------

// ftBreakTheWebHop connects the workspace's page and lets it go away
// unannounced, which is what breaks the web hop.
func ftBreakTheWebHop(f *fixture) {
	f.web = f.d.WatchWeb(f.ws)
	f.web.Close()
}

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

func TestTopbarIsNotConnectedWhileTheWebHopIsDown(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newCanonicalRegistered(t)
	f.open()
	f.host = f.d.WatchHost(f.ws)
	topbar := f.d.WatchTopbar(f.ws)

	// Act
	ftBreakTheWebHop(f)

	// Assert: the connectivity indicator leaves the serving glyph while a
	// client hop is down.
	awaitTopbar(t, f, topbar, "the not-connected indicator", func(v *frontendv1.TopbarView) bool {
		return v.GetConnectivity() != nil && v.GetConnectivity().GetTitle() != "connected to the session"
	})
}

func TestTopbarBecomesConnectedWhenTheWebHopComesUp(t *testing.T) {
	t.Parallel()
	// Arrange: a page that connected and went away.
	f := newCanonicalRegistered(t)
	f.open()
	f.host = f.d.WatchHost(f.ws)
	topbar := f.d.WatchTopbar(f.ws)
	ftBreakTheWebHop(f)
	awaitTopbar(t, f, topbar, "the not-connected indicator", func(v *frontendv1.TopbarView) bool {
		return v.GetConnectivity() != nil && v.GetConnectivity().GetTitle() != "connected to the session"
	})

	// Act
	f.web = f.d.WatchWeb(f.ws)

	// Assert
	awaitTopbar(t, f, topbar, "the connected indicator", func(v *frontendv1.TopbarView) bool {
		return v.GetConnectivity().GetTitle() == "connected to the session"
	})
}

// ---------------------------------------------------------------------------
// Footer: the activity cell's tiers
// ---------------------------------------------------------------------------

// ftTransient is the idle cell's live transient, nil when none is live.
func ftTransient(v *frontendv1.FooterView) *frontendv1.FooterActivityTransient {
	return v.GetStrip().GetStatus().GetIdle().GetActivity().GetUnpinned().GetTransient()
}

// ftHookStart is a hook named NAME starting, as the main agent's activity.
func ftHookStart(id, name string) *conversationv1.AgentFrame {
	return activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID(id),
		Item: &conversationv1.AgentActivity_Hook{Hook: &conversationv1.AgentHook{Result: &conversationv1.AgentHook_Start{
			Start: &conversationv1.AgentHookStart{HookName: name, Event: conversationv1.AgentHookEvent_AGENT_HOOK_EVENT_PRE_TOOL_USE, StartedAt: startedAt(1)},
		}}},
	})
}

// THE PUSH NOTIFICATION IS SALIENT (owner ruling, 2026-09-30): it stands over
// a live transient until the next prompt.
func TestFooterAPushNotificationStandsOverALiveTransient(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newCanonical(t)
	footer := f.d.WatchFooter(f.ws)
	f.shim.PushAgentFrame(mainAgent, ftHookStart("hook-1", "a-hook"))
	awaitFooter(t, f, footer, "the hook's transient", func(v *frontendv1.FooterView) bool {
		return ftTransient(v).GetHook() != nil
	})

	// Act
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("notify-1"),
		Item: &conversationv1.AgentActivity_PushNotification{PushNotification: &conversationv1.AgentPushNotification{
			State: &conversationv1.AgentPushNotification_Start{Start: &conversationv1.AgentPushNotificationStart{
				Message:   "the branch is ready for review",
				StartedAt: startedAt(1_700_000_000_000),
			}},
		}},
	}))

	// Assert
	awaitFooter(t, f, footer, "the notification standing salient", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetIdle().GetActivity().GetSalient().GetNotification().GetText() == "the branch is ready for review"
	})
}

// ftFeedItemDerivedLine matches the wording of a line composed from a feed item
// that landed ("✅ Read finished — handling result...", "❌ Bash failed",
// "✅ Prompt delivered — awaiting response...", "✅ Moved to background —
// continuing..."): the retired quiet tier's lines (owner ruling, 2026-10-01).
var ftFeedItemDerivedLine = regexp.MustCompile(`[✅❌] [^"]* (finished|failed|cancelled|started|delivered)|Moved to background`)

// THE QUIET TIER IS RETIRED: between two feed items the footer composes no line
// from the item that landed. A turn draws two feed items, a Read call and the
// closing response, and every footer push across the turn is swept for a line
// worded from a landed item. The pushes are read on their own goroutine as
// they arrive, so the stream's buffer never decides what the sweep sees.
func TestFooterComposesNoLineFromAFeedItemThatLanded(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newCanonical(t)
	footer := f.d.WatchFooter(f.ws)
	type swept struct {
		working    int
		violations []string
	}
	done := make(chan swept, 1)
	go func() {
		var out swept
		for v := range footer.C {
			status := v.GetStrip().GetStatus()
			if status.GetWorking() != nil {
				out.working++
			}
			if text := prototext.Format(v); ftFeedItemDerivedLine.MatchString(text) {
				out.violations = append(out.violations, text)
			}
			if out.working > 0 && status.GetIdle() != nil {
				break
			}
		}
		done <- out
	}()

	// Act: a turn that reads a file, then answers.
	f.submit("go", "k-quiet-tier", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	f.shim.ExpectStartTurn()
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("read-1"),
		Item: &conversationv1.AgentActivity_Read{Read: &conversationv1.AgentRead{Result: &conversationv1.AgentRead_Start{
			Start: &conversationv1.AgentReadStart{Path: &conversationv1.ReadPath{Path: "a.go"}, StartedAt: startedAt(1)},
		}}},
	}))
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("read-1"),
		Item: &conversationv1.AgentActivity_Read{Read: &conversationv1.AgentRead{Result: &conversationv1.AgentRead_Success{
			Success: &conversationv1.AgentReadSuccess{
				Path:      &conversationv1.ReadPath{Path: "a.go"},
				Extent:    &conversationv1.AgentReadSuccess_Whole{Whole: &conversationv1.AgentReadWhole{Contents: "package a\n"}},
				SettledAt: settledAt(2),
			},
		}}},
	}))
	pushConcludedTurn(f.shim, mainAgent, "resp-1")
	ctx, cancel := f.d.WaitCtx()
	defer cancel()
	var got swept
	select {
	case got = <-done:
	case <-ctx.Done():
		t.Fatalf("waiting for the footer to return to idle after the turn: %v", ctx.Err())
	}

	// Assert
	if got.working == 0 {
		t.Fatal("no working push observed: the sweep saw none of the turn")
	}
	if len(got.violations) != 0 {
		t.Fatalf("footer pushes carried %d feed-item-derived lines, want none:\n%v", len(got.violations), got.violations)
	}
}

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
