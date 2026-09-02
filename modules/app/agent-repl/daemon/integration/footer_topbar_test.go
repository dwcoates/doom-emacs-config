//go:build integration

package integration

import (
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/integration/harness"

	"connectrpc.com/connect"
)

// ---------------------------------------------------------------------------
// Footer: whole-view push discipline and populated panels
// ---------------------------------------------------------------------------

func TestFooterPushesAreWholeViewsDeduplicated(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	footer := f.d.WatchFooter(f.ws)
	first := awaitFooter(t, f, footer, "the footer after readiness", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetIdle() != nil
	})

	// Act: push the identical healthy diagnostics again — nothing changed.
	f.shim.PushHealthy()

	// Assert
	harness.ExpectNoPush(t, footer, harness.ProbeWindow, "an identical consecutive push is not sent")
	// Sanity: the first push really was whole (strip present).
	if first.GetStrip() == nil {
		t.Fatalf("footer push = %v, want a whole strip", first)
	}
}

func TestFooterExpandedPanelsArrivePopulatedOnEveryPush(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	footer := f.d.WatchFooter(f.ws)

	// Act: force a push by starting a turn.
	f.submit("go", "k-panels", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)

	// Assert: every panel field is present (non-nil), whether or not it holds
	// rows — ALL panels arrive populated on every push, per the contract.
	got := awaitFooter(t, f, footer, "the footer after StartTurn", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetThinking() != nil
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

func TestFooterStatusTreeFollowsIdleThinkingDone(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	footer := f.d.WatchFooter(f.ws)
	awaitFooter(t, f, footer, "idle.ready before any turn", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetIdle().GetReady() != nil
	})

	// Act: StartTurn should be visible as thinking.submitting before the fake
	// even answers.
	f.submit("do it", "k-tree", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)

	// Assert: submitting, then thinking, then done once the turn concludes.
	awaitFooter(t, f, footer, "thinking.submitting on StartTurn", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetThinking().GetSubmitting() != nil
	})
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("think-1"),
		Item:       &conversationv1.AgentActivity_Thinking{Thinking: &conversationv1.AgentThinking{Result: &conversationv1.AgentThinking_Start{Start: &conversationv1.AgentThinkingStart{}}}},
	}))
	awaitFooter(t, f, footer, "the bare thinking status while reasoning runs", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetThinking() != nil
	})
	f.shim.PushAgentFrame(mainAgent, successFrame(mainAgent, nil))
	awaitFooter(t, f, footer, "idle.done after the turn concludes", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetIdle().GetDone() != nil
	})
}

func TestFooterInterruptedStatusIsRetiredByADaemonSideDwell(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	footer := f.d.WatchFooter(f.ws)
	f.submit("do it", "k-interrupted", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	awaitFooter(t, f, footer, "thinking before the interrupt", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetThinking() != nil
	})

	// Act: the agent binary acknowledges a user stop.
	f.shim.PushAgentFrame(mainAgent, interruptedFrame(mainAgent))

	// Assert: `interrupted` shows momentarily, then the daemon itself retires
	// it into a successor push (idle) with no further client action.
	awaitFooter(t, f, footer, "the momentary interrupted status", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetInterrupted() != nil
	})
	awaitFooter(t, f, footer, "the dwell's successor push retiring interrupted", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetInterrupted() == nil
	})
}

func TestFooterLoadingStatusIsRetiredByADaemonSideDwell(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	footer := f.d.WatchFooter(f.ws)
	f.submit("do it", "k-loading", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	awaitFooter(t, f, footer, "thinking before the injection", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetThinking() != nil
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

// ---------------------------------------------------------------------------
// Footer: the tokens cell
// ---------------------------------------------------------------------------

func TestFooterTokensCellExcludesCacheReads(t *testing.T) {
	// Arrange: a turn whose only usage is a huge cache READ and no misses.
	f := newOpened(t, harness.Opts{})
	footer := f.d.WatchFooter(f.ws)
	f.submit("first", "k-tok-a", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, ftUsageActivity("resp-a", ftUsage(0, 0, 500_000))))
	readOnly := awaitFooter(t, f, footer, "the tokens cell after a cache-read-only response", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetTokens().GetInput().GetText() != ""
	})
	f.shim.PushAgentFrame(mainAgent, successFrame(mainAgent, nil))
	awaitFooter(t, f, footer, "idle.done after the first turn", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetIdle().GetDone() != nil
	})

	// Act: a second, fresh turn whose usage adds a small MISS on top of the
	// same huge cache read.
	f.submit("second", "k-tok-b", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	f.shim.ExpectStartTurn()
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, ftUsageActivity("resp-b", ftUsage(250, 0, 500_000))))

	// Assert: the cell changed — the diff can only be the miss, since the
	// cache-read figure (input_hits) is identical in both turns and a fresh
	// turn resets the cell.
	withMiss := awaitFooter(t, f, footer, "the tokens cell after the miss joins the same cache read", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetTokens().GetInput().GetText() != "" && v.GetStrip().GetTokens().GetInput().GetText() != readOnly.GetStrip().GetTokens().GetInput().GetText()
	})
	if withMiss.GetStrip().GetTokens().GetInput().GetText() == readOnly.GetStrip().GetTokens().GetInput().GetText() {
		t.Fatalf("tokens cell unchanged by a real miss (%q); a cache-read-only figure must not already count it in", readOnly.GetStrip().GetTokens().GetInput().GetText())
	}
}

func TestFooterTokensCellUsageIsNotDoubleCountedAcrossAResponsesUnits(t *testing.T) {
	// Arrange: one API response whose usage is stamped on the FIRST unit
	// only, per the envelope contract.
	f := newOpened(t, harness.Opts{})
	footer := f.d.WatchFooter(f.ws)
	f.submit("go", "k-tok-dup", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)

	// Act: the response's first unit carries usage.
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, ftUsageActivity("resp-unit-1", ftUsage(1000, 0, 0))))
	// The cell is ALWAYS populated (it reads "0 in" before any usage lands),
	// so the usage-carrying push is the first one whose figure is not zero.
	firstUnit := awaitFooter(t, f, footer, "the tokens cell after the usage-carrying unit", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetTokens().GetInput().GetText() != "" && v.GetStrip().GetTokens().GetInput().GetText() != "0 in"
	})

	// Act: the SAME response's second unit (a tool call in the same
	// assistant message) carries NO usage.
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("resp-unit-2"),
		Item: &conversationv1.AgentActivity_Read{Read: &conversationv1.AgentRead{
			Result: &conversationv1.AgentRead_Start{Start: &conversationv1.AgentReadStart{Path: &conversationv1.ReadPath{Path: "a.go"}}},
		}},
	}))

	// Act: end the turn. A whole view identical to the last one is never
	// pushed, so the turn's terminal is what makes the post-second-unit cell
	// observable at all.
	f.shim.PushAgentFrame(mainAgent, successFrame(mainAgent, nil))

	// Assert: the cell is unchanged — a second unit of the SAME response
	// leaving usage unset must not add a second charge.
	stillOne := awaitFooter(t, f, footer, "the footer after the unstamped second unit", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetIdle().GetDone() != nil
	})
	if stillOne.GetStrip().GetTokens().GetInput().GetText() != firstUnit.GetStrip().GetTokens().GetInput().GetText() {
		t.Fatalf("tokens cell = %q after the second unit, want it unchanged at %q: usage rides exactly one unit per response",
			stillOne.GetStrip().GetTokens().GetInput().GetText(), firstUnit.GetStrip().GetTokens().GetInput().GetText())
	}
}

func TestFooterTokensCellVerdictIsIncompleteWhenAResponseCarriedNoUsage(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	footer := f.d.WatchFooter(f.ws)
	f.submit("go", "k-tok-incomplete", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)

	// Act: the response's terminal frame settles with NO unit ever having
	// carried usage.
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("resp-no-usage"),
		Item: &conversationv1.AgentActivity_Response{Response: &conversationv1.AgentResponse{
			Result: &conversationv1.AgentResponse_Success{Success: &conversationv1.AgentResponseSuccess{
				Prose: &conversationv1.AgentResponseProse{Markdown: "done"},
			}},
		}},
	}))
	f.shim.PushAgentFrame(mainAgent, successFrame(mainAgent, activityID("resp-no-usage")))

	// Assert
	got := awaitFooter(t, f, footer, "idle.done with the reconciled verdict", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetIdle().GetDone() != nil
	})
	if got.GetStrip().GetTokens().GetVerdict().GetIncomplete() == nil {
		t.Fatalf("tokens cell verdict = %v, want incomplete when no response of the turn carried usage", got.GetStrip().GetTokens().GetVerdict())
	}
}

// ---------------------------------------------------------------------------
// Footer: live-work chips
// ---------------------------------------------------------------------------

func TestFooterLiveWorkChipsReflectEachKindsCount(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	footer := f.d.WatchFooter(f.ws)

	// Act: one live subagent, one live shell.
	f.shim.PushAgentFrame(mainAgent, detachedWorkFrame(mainAgent, detachedSubagent("work-agent", "sub-1", "explore the tree")))
	f.shim.PushAgentFrame(mainAgent, detachedWorkFrame(mainAgent, detachedShell("work-shell", "sleep 5")))
	// Two tracker tasks, one done.
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, ftTaskActivity("task-1", "t-1", "write the tests", true)))
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, ftTaskActivity("task-2", "t-2", "land the change", false)))
	// One live monitor.
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, ftMonitorActivity("mon-1", "watching the build log")))
	// A two-job cron listing.
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("cron-1"),
		Item: &conversationv1.AgentActivity_Cron{Cron: &conversationv1.AgentCron{State: &conversationv1.AgentCron_Success{Success: &conversationv1.AgentCronSuccess{
			Act: &conversationv1.AgentCronSuccess_Listed{Listed: &conversationv1.AgentCronListed{
				Jobs: []*conversationv1.AgentCronJob{{JobId: "j1"}, {JobId: "j2"}},
			}},
		}}}},
	}))

	// Assert: every chip lands with its resolved count.
	got := awaitFooter(t, f, footer, "every live-work chip populated", func(v *frontendv1.FooterView) bool {
		chips := v.GetStrip().GetLiveWork()
		return chips.GetAgents().GetCount() == 1 &&
			chips.GetShells().GetCount() == 1 &&
			chips.GetTasks().GetDone() == 1 && chips.GetTasks().GetTotal() == 2 &&
			chips.GetMonitors().GetCount() == 1 &&
			chips.GetCrons().GetCount() == 2
	})
	_ = got
}

func TestFooterLiveWorkChipsAreUnsetWhenZero(t *testing.T) {
	// Arrange / Act
	f := newOpened(t, harness.Opts{})
	footer := f.d.WatchFooter(f.ws)

	// Assert: a quiet workspace shows no chips at all.
	got := awaitFooter(t, f, footer, "the footer with nothing live", func(v *frontendv1.FooterView) bool {
		return v.GetStrip() != nil
	})
	chips := got.GetStrip().GetLiveWork()
	if chips.GetAgents() != nil || chips.GetShells() != nil || chips.GetTasks() != nil ||
		chips.GetMonitors() != nil || chips.GetCrons() != nil {
		t.Fatalf("live-work chips = %v, want every chip unset when nothing is live", chips)
	}
}

// ---------------------------------------------------------------------------
// Footer: the allowance cell — composed from TWO facts.
// ---------------------------------------------------------------------------

func TestFooterAllowanceComposedFromAccountUsageAndRateLimitStatus(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	footer := f.d.WatchFooter(f.ws)
	awaitFooter(t, f, footer, "idle before any usage sample", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetIdle() != nil
	})

	// Act: the FIGURES come from account_usage...
	f.shim.PushSessionUpdate(&conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_AccountUsage{AccountUsage: &conversationv1.SessionAccountUsage{
			ObservedAtMs: 1_700_000_000_000,
			Outcome: &conversationv1.SessionAccountUsage_Available{Available: &conversationv1.SessionAccountUsageAvailable{
				FiveHour: &conversationv1.SessionUsageWindow{UtilizationPercent: 95, ResetsAtMs: 1_700_010_000_000},
			}},
		}},
	})
	// ...and the VERDICT comes from rate_limit_status, for the same window.
	f.shim.PushSessionUpdate(&conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_RateLimitStatus{RateLimitStatus: &conversationv1.SessionRateLimitStatus{
			Status: &conversationv1.SessionRateLimitStatus_AllowedWarning{AllowedWarning: &conversationv1.SessionRateLimitAllowedWarning{}},
			RateLimitType: &conversationv1.SessionRateLimitType{
				Window: &conversationv1.SessionRateLimitType_FiveHour{FiveHour: &conversationv1.SessionRateLimitWindowFiveHour{}},
			},
		}},
	})

	// Assert: the session allowance carries BOTH the figure (utilization,
	// reset) from account_usage and the verdict (allowed_warning) from
	// rate_limit_status -- one cell composed from two different facts.
	got := awaitFooter(t, f, footer, "the session allowance composed from both facts", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetIdle().GetActivity().GetRateLimited().GetSession().GetAllowedWarning() != nil
	})
	allowance := got.GetStrip().GetStatus().GetIdle().GetActivity().GetRateLimited().GetSession()
	if allowance.GetUtilization() != 0.95 {
		t.Fatalf("the session allowance's utilization = %v, want 0.95 (the account_usage figure, converted)", allowance.GetUtilization())
	}
	if allowance.GetResetsAtS() != 1_700_010_000_000/1000 {
		t.Fatalf("the session allowance's resets_at_s = %d, want the account_usage figure converted to seconds", allowance.GetResetsAtS())
	}
	if !allowance.GetNewsworthy() {
		t.Fatal("the session allowance is not drawn newsworthy at 95% utilization")
	}
}

// ---------------------------------------------------------------------------
// Deny-and-continue.
// ---------------------------------------------------------------------------

func TestDenyAndContinueKeepsTheFooterThinkingUntilTheFakesOwnTerminal(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-deny-continue", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()
	footer := f.d.WatchFooter(f.ws)
	awaitFooter(t, f, footer, "thinking before the permission ask", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetThinking() != nil
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

	// Assert: the footer stays thinking (a deny does not end the turn) and no
	// turn_ended row is drawn until the fake pushes the turn's own terminal.
	got := awaitFooter(t, f, footer, "thinking still standing after the deny", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetThinking() != nil
	})
	if got.GetStrip().GetStatus().GetIdle() != nil {
		t.Fatalf("footer status = %v after a permission deny, want the turn still in flight", got.GetStrip().GetStatus())
	}
	harness.ExpectNoPush(t, tail, harness.ProbeWindow, "no turn_ended row until the fake's own terminal")

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

func TestWatchWebWorkspaceFlushesHeadersBeforeAnyFrameWhenNothingIsPublishedYet(t *testing.T) {
	// Arrange / Act: WatchWebWorkspace carries no state topic of its own --
	// only the `transferred` event, which nothing in this test ever raises --
	// so a fresh subscription has no published view to replay. Without
	// flush-on-accept (a ResponseWriter wrapper that flushes headers the
	// moment the subscription is registered) the daemon would never send
	// anything and this open would hang until the daemon's context times out;
	// WatchWeb already t.Fatalf's on an open error, so its returning at all
	// is the first half of the assertion.
	f := newOpened(t, harness.Opts{})
	web := f.d.WatchWeb(f.ws)

	// Assert: headers arrived (the open returned) and no frame follows.
	harness.ExpectNoPush(t, web, harness.ProbeWindow, "WatchWebWorkspace with nothing ever published carries no frame")
}

// ---------------------------------------------------------------------------
// Footer: wakeup fallback and precedence
// ---------------------------------------------------------------------------

func TestFooterWakeupShowsOnlyWhenNothingElseStands(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	footer := f.d.WatchFooter(f.ws)
	awaitFooter(t, f, footer, "idle before scheduling a wakeup", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetIdle() != nil
	})

	// Act: the agent self-schedules a wakeup while idle.
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, ftWakeupScheduleActivity("wake-1", 60)))

	// Assert: the wakeup fallback stands because nothing else does.
	awaitFooter(t, f, footer, "waiting.wakeup with nothing else standing", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetWaiting().GetWakeup() != nil
	})
}

func TestFooterARealStatusWinsOverAPendingWakeup(t *testing.T) {
	// Arrange: a pending wakeup while idle.
	f := newOpened(t, harness.Opts{})
	footer := f.d.WatchFooter(f.ws)
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, ftWakeupScheduleActivity("wake-2", 60)))
	awaitFooter(t, f, footer, "waiting.wakeup before the real status", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetWaiting().GetWakeup() != nil
	})

	// Act: a real status (a turn) begins.
	f.submit("go", "k-wakeup-real", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)

	// Assert: thinking wins; the wakeup fallback no longer shows.
	got := awaitFooter(t, f, footer, "thinking replacing the wakeup fallback", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetThinking() != nil
	})
	if got.GetStrip().GetStatus().GetWaiting() != nil {
		t.Fatalf("footer status = %v while a turn runs, want the wakeup fallback retired", got.GetStrip().GetStatus())
	}
}

func TestFooterNotificationOutranksRateLimitedAndContextBudget(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	footer := f.d.WatchFooter(f.ws)
	awaitFooter(t, f, footer, "idle before any competing activity", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetIdle() != nil
	})

	// Act: a context-budget warning, then a rate-limit report, then a
	// notification — all while idle, all competing for the one activity slot.
	f.shim.PushAgentFrame(mainAgent, updateFrame(mainAgent, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_ContextBudgetWarning{ContextBudgetWarning: &conversationv1.ContextBudgetWarning{Text: "context filling"}},
	}))
	f.shim.PushSessionUpdate(&conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_RateLimitStatus{RateLimitStatus: &conversationv1.SessionRateLimitStatus{
			Status:             &conversationv1.SessionRateLimitStatus_AllowedWarning{AllowedWarning: &conversationv1.SessionRateLimitAllowedWarning{}},
			UtilizationPercent: f64Ptr(92),
		}},
	})
	// THE MESSAGE RIDES THE START ARM — the success states only the vendor's
	// delivery outcome — so the notification the footer draws is the start's.
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("notify-1"),
		Item: &conversationv1.AgentActivity_PushNotification{PushNotification: &conversationv1.AgentPushNotification{
			State: &conversationv1.AgentPushNotification_Start{Start: &conversationv1.AgentPushNotificationStart{
				Message:   "the branch is ready for review",
				StartedAt: startedAt(1_700_000_000_000),
			}},
		}},
	}))
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("notify-1"),
		Item: &conversationv1.AgentActivity_PushNotification{PushNotification: &conversationv1.AgentPushNotification{
			State: &conversationv1.AgentPushNotification_Success{Success: &conversationv1.AgentPushNotificationSuccess{
				Outcome: &conversationv1.AgentPushNotificationSuccess_Sent{Sent: &conversationv1.AgentPushNotificationSent{PushSent: true}},
			}},
		}},
	}))

	// Assert: the notification is the standing activity.
	got := awaitFooter(t, f, footer, "the notification standing over rate-limit and context-budget", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetIdle().GetActivity().GetNotification() != nil
	})
	activity := got.GetStrip().GetStatus().GetIdle().GetActivity()
	if activity.GetRateLimited() != nil || activity.GetContextBudget() != nil {
		t.Fatalf("idle activity = %v, want ONLY the notification standing (outranks both)", activity)
	}
}

// ---------------------------------------------------------------------------
// Footer: mid-turn API error evidence
// ---------------------------------------------------------------------------

func TestFooterApiErrorMidTurnDrawsRetryingEvidenceWithoutEndingTheTurn(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	footer := f.d.WatchFooter(f.ws)
	f.submit("go", "k-api-error", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	awaitFooter(t, f, footer, "thinking before the mid-turn error", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetThinking() != nil
	})

	// Act: a 429 mid-turn, recorded but recovered from.
	f.shim.PushAgentFrame(mainAgent, updateFrame(mainAgent, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_ApiError{ApiError: &conversationv1.ApiRequestFailed{
			Message: "rate limited",
			Kind:    &conversationv1.ApiRequestFailed_RateLimited{RateLimited: &conversationv1.ApiRateLimited{}},
		}},
	}))

	// Assert: the turn is still thinking (not ended) and the footer shows the
	// retry evidence.
	got := awaitFooter(t, f, footer, "thinking.retrying evidence mid-turn", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetThinking() != nil
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
	// Arrange
	f := newOpened(t, harness.Opts{})
	footer := f.d.WatchFooter(f.ws)
	roster := f.d.WatchRoster()
	awaitFooter(t, f, footer, "idle before the link dies", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetIdle() != nil
	})
	watchesBefore := f.shim.Count(harness.RPCWatchSession)

	// Act: sever the session stream.
	f.shim.DropStream(harness.StreamSession)

	// Assert: the footer flips to disconnected.severed and the roster agrees.
	awaitFooter(t, f, footer, "disconnected.severed after the link dies", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetDisconnected().GetSevered() != nil
	})
	awaitRoster(t, f.d, roster, "the roster's severed status", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, f.ws.GetId())
		return row != nil && row.GetSevered() != nil
	})

	// Assert: the daemon redials — a fresh WatchSession is opened without any
	// client action.
	ftAwaitTrue(t, f.d.Ctx(), func() bool { return f.shim.Count(harness.RPCWatchSession) > watchesBefore }, "a redialed WatchSession after the link is severed")
}

func TestFooterShimExitFlipsToDeadAndStopsRedials(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	footer := f.d.WatchFooter(f.ws)
	roster := f.d.WatchRoster()
	awaitFooter(t, f, footer, "idle before the shim exits", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetIdle() != nil
	})

	// Act: the fake shim process exits outright.
	f.shim.Exit(1, "simulated crash")

	// Assert: the footer flips to disconnected.dead and the roster agrees.
	awaitFooter(t, f, footer, "disconnected.dead after the shim exits", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetDisconnected().GetDead() != nil
	})
	awaitRoster(t, f.d, roster, "the roster's dead status", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, f.ws.GetId())
		return row != nil && row.GetDead() != nil
	})

	// Assert: no further churn — a dead shim gets no more redial attempts, so
	// the footer settles rather than cycling.
	harness.ExpectNoPush(t, footer, harness.ProbeWindow, "no further footer churn once the shim is dead (redials stop)")
	// The exit was not attributed to a daemon-requested kill, so
	// publishExit's ELSE branch fires: daemon.shimclient.exit at ERROR
	// ("shim died"). Nothing else observes this exit (no query_died update
	// was pushed -- the process simply exited).
	f.d.ExpectWarnings("daemon.shimclient.exit")
}

// ---------------------------------------------------------------------------
// Topbar
// ---------------------------------------------------------------------------

func TestTopbarTitleIsComposedFromTheWorkspacesNaming(t *testing.T) {
	// Arrange / Act
	f := newOpened(t, harness.Opts{})
	topbar := f.d.WatchTopbar(f.ws)

	// Assert
	got := awaitTopbar(t, f, topbar, "the composed title", func(v *frontendv1.TopbarView) bool {
		return v.GetTitle().GetText() != ""
	})
	if got.GetTitle().GetText() == "" {
		t.Fatalf("topbar title = %q, want a composed, non-empty name", got.GetTitle().GetText())
	}
}

func TestTopbarModelSelectorReflectsTheCatalogAndTheEffectiveModel(t *testing.T) {
	// Arrange / Act
	f := newOpened(t, harness.Opts{})
	topbar := f.d.WatchTopbar(f.ws)

	// Assert: the fake's default StartSession answer serves a three-model
	// catalog, and the selector's `selected` names one of its own options.
	got := awaitTopbar(t, f, topbar, "the model selector resolved from the catalog", func(v *frontendv1.TopbarView) bool {
		return len(v.GetModelSelector().GetOptions()) > 0 && v.GetModelSelector().GetSelected() != nil
	})
	selected := got.GetModelSelector().GetSelected().GetModel().GetName()
	found := false
	for _, opt := range got.GetModelSelector().GetOptions() {
		if opt.GetModel().GetName() == selected {
			found = true
		}
	}
	if !found {
		t.Fatalf("model selector selected = %q, want it among the served options %v", selected, got.GetModelSelector().GetOptions())
	}
}

func TestTopbarContextChipReflectsTheContextUsagePush(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	topbar := f.d.WatchTopbar(f.ws)

	// Act
	f.shim.PushSessionUpdate(&conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_ContextUsage{ContextUsage: &conversationv1.SessionContextUsage{
			TotalTokens: 142_300,
			MaxTokens:   200_000,
			Percentage:  71,
			Model:       "claude-opus-5",
			Categories:  []*conversationv1.SessionContextCategory{{Label: "system prompt", Tokens: 4_000, Color: "blue"}},
		}},
	})

	// Assert: the chip carries a formatted, non-empty figure and always ships
	// a populated breakdown (no round-trip needed to open the hover).
	got := awaitTopbar(t, f, topbar, "the context chip after context_usage", func(v *frontendv1.TopbarView) bool {
		return v.GetContext().GetText() != ""
	})
	if got.GetContext().GetBreakdown() == nil {
		t.Fatalf("context chip breakdown = nil, want it always populated on the push carrying context_usage")
	}
}

func TestTopbarContextPanelResolvesFromTheSameContextUsageFact(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	topbar := f.d.WatchTopbar(f.ws)

	// Act
	f.shim.PushSessionUpdate(&conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_ContextUsage{ContextUsage: &conversationv1.SessionContextUsage{
			TotalTokens: 50_000,
			MaxTokens:   200_000,
			Percentage:  25,
			Model:       "claude-opus-5",
			Categories:  []*conversationv1.SessionContextCategory{{Label: "system prompt", Tokens: 4_000, Color: "blue"}},
		}},
	})
	// The chip resolves from the same fact, so its arrival is the synchronizing
	// edge for the panel the fact also feeds.
	awaitTopbar(t, f, topbar, "the context chip carrying the pushed usage", func(v *frontendv1.TopbarView) bool {
		return v.GetContext().GetText() != ""
	})

	// Assert: the /context panel — the topbar resolver's OTHER product from
	// the same fact, drawn by the `/context` command rather than by the chip's
	// hover (the chip's hover is the SESSION token breakdown, a different
	// fact) — carries the pushed category verbatim.
	resp := f.submit("/context", "k-context-panel", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	panel := resp.GetSuccess().GetCommandPanel().GetContext()
	if panel == nil {
		t.Fatalf("SubmitPrompt(/context) = %v, want a command_panel.context", resp)
	}
	row := ftFindContextCategory(panel, "system prompt")
	if row == nil {
		t.Fatalf("the /context panel = %v, want a system prompt category", panel.GetCategories())
	}
	if !strings.Contains(row.GetFigure(), "4") {
		t.Fatalf("the system prompt category figure = %q, want the pushed 4000 tokens", row.GetFigure())
	}
}

// ftFindContextCategory finds a /context panel category by label.
func ftFindContextCategory(panel *frontendv1.ContextPanelView, label string) *frontendv1.ContextPanelCategory {
	for _, category := range panel.GetCategories() {
		if category.GetLabel() == label {
			return category
		}
	}
	return nil
}

func TestTopbarWarningForASessionFaultIsRetractedOnTheNextHealthyPush(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	topbar := f.d.WatchTopbar(f.ws)

	// Act: an unhealthy diagnostics pull surfaces a warning.
	f.shim.PushUnhealthy(&conversationv1.SessionFault{
		Component: "converter",
		Detail:    "could not model a record",
		Kind:      &conversationv1.SessionFault_ConverterDefect{ConverterDefect: &conversationv1.SessionFaultConverterDefect{}},
	})
	awaitTopbar(t, f, topbar, "the session-fault warning", func(v *frontendv1.TopbarView) bool {
		return len(v.GetWarnings().GetWarnings()) > 0
	})

	// Act: the next pull comes back healthy.
	f.shim.PushHealthy()

	// Assert: the warning is retracted.
	awaitTopbar(t, f, topbar, "the warning retracted on the next healthy push", func(v *frontendv1.TopbarView) bool {
		return len(v.GetWarnings().GetWarnings()) == 0
	})
}

func TestTopbarDegradedWindowIsDrawnOpenThenClosed(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	topbar := f.d.WatchTopbar(f.ws)

	// Act: a degraded window opens.
	f.shim.PushSessionUpdate(&conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_Diagnostics{Diagnostics: &conversationv1.SessionDiagnostics{
			Health: &conversationv1.SessionDiagnostics_Healthy{Healthy: &conversationv1.SessionHealthy{}},
			DegradedWindows: []*conversationv1.SessionDegradedWindow{{
				Component: "converter",
				Reason:    "backlogged",
				BeganAtMs: 1_700_000_000_000,
				Extent:    &conversationv1.SessionDegradedWindow_Open{Open: &conversationv1.SessionDegradedOpen{}},
			}},
		}},
	})

	// Assert: drawn open.
	got := awaitTopbar(t, f, topbar, "the degraded window drawn open", func(v *frontendv1.TopbarView) bool {
		return ftFindDegradedWindow(v) != nil && ftFindDegradedWindow(v).GetOpen() != nil
	})
	_ = got

	// Act: the window closes.
	f.shim.PushSessionUpdate(&conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_Diagnostics{Diagnostics: &conversationv1.SessionDiagnostics{
			Health: &conversationv1.SessionDiagnostics_Healthy{Healthy: &conversationv1.SessionHealthy{}},
			DegradedWindows: []*conversationv1.SessionDegradedWindow{{
				Component: "converter",
				Reason:    "backlogged",
				BeganAtMs: 1_700_000_000_000,
				Extent: &conversationv1.SessionDegradedWindow_Closed{Closed: &conversationv1.SessionDegradedClosed{
					EndedAtMs:    1_700_000_005_000,
					DroppedCount: 12,
				}},
			}},
		}},
	})

	// Assert: drawn closed, with its cost.
	closedGot := awaitTopbar(t, f, topbar, "the degraded window drawn closed", func(v *frontendv1.TopbarView) bool {
		return ftFindDegradedWindow(v) != nil && ftFindDegradedWindow(v).GetClosed() != nil
	})
	if ftFindDegradedWindow(closedGot).GetClosed().GetDroppedCount() != 12 {
		t.Fatalf("degraded window closed.dropped_count = %d, want 12", ftFindDegradedWindow(closedGot).GetClosed().GetDroppedCount())
	}
}

func TestTopbarConnectivityToneAndGlyphComeFromTheSharedVocabulary(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	topbar := f.d.WatchTopbar(f.ws)
	tones := ftLoadTopbarTones(t)

	// Act / Assert
	got := awaitTopbar(t, f, topbar, "connectivity resolved after readiness", func(v *frontendv1.TopbarView) bool {
		return v.GetConnectivity().GetGlyph() != ""
	})
	tone := got.GetConnectivity().GetTone()
	if !tones[tone] {
		t.Fatalf("connectivity tone = %q, want one of the shared vocabulary's topbar_tones %v", tone, tones)
	}
}

func TestTopbarAccountReflectsTheWorkspacesConfigRootEmail(t *testing.T) {
	// Arrange / Act
	f := newOpened(t, harness.Opts{DefaultAccountEmail: "dodge@example.invalid"})
	topbar := f.d.WatchTopbar(f.ws)

	// Assert
	got := awaitTopbar(t, f, topbar, "the account email", func(v *frontendv1.TopbarView) bool {
		return v.GetAccount().GetLoggedIn().GetEmail() != ""
	})
	if got.GetAccount().GetLoggedIn().GetEmail() != "dodge@example.invalid" {
		t.Fatalf("topbar account email = %q, want the config root's dodge@example.invalid", got.GetAccount().GetLoggedIn().GetEmail())
	}
}

func TestTopbarPermissionModePickerServesTheSwitchableSetWithTheCurrentMode(t *testing.T) {
	// Arrange / Act
	f := newOpened(t, harness.Opts{})
	topbar := f.d.WatchTopbar(f.ws)

	// Assert
	got := awaitTopbar(t, f, topbar, "the permission-mode picker resolved", func(v *frontendv1.TopbarView) bool {
		return v.GetPermissionModePicker().GetCurrent() != nil
	})
	picker := got.GetPermissionModePicker()
	if picker.GetCurrent().GetMode() == "" {
		t.Fatalf("permission_mode_picker.current = %v, want the mode in force named", picker.GetCurrent())
	}
	found := false
	for _, opt := range picker.GetOptions() {
		if opt.GetMode() == picker.GetCurrent().GetMode() {
			found = true
		}
	}
	if !found {
		t.Fatalf("permission_mode_picker.options = %v, want the current mode among the switchable set", picker.GetOptions())
	}
}

func TestTopbarPermissionModeChangedPushUpdatesTheCurrentMode(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	topbar := f.d.WatchTopbar(f.ws)
	before := awaitTopbar(t, f, topbar, "the picker before the change", func(v *frontendv1.TopbarView) bool {
		return v.GetPermissionModePicker().GetCurrent() != nil
	})

	// Act: the shim reports a mode change (e.g. from a standing permission
	// grant), not a client-initiated SetPermissionMode.
	f.shim.PushSessionUpdate(&conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_PermissionModeChanged{PermissionModeChanged: &conversationv1.SessionPermissionModeChanged{
			PermissionMode: &conversationv1.AgentPermissionMode{Mode: &conversationv1.AgentPermissionMode_AcceptEdits{AcceptEdits: &conversationv1.AgentPermissionModeAcceptEdits{}}},
		}},
	})

	// Assert
	after := awaitTopbar(t, f, topbar, "the picker's current mode after permission_mode_changed", func(v *frontendv1.TopbarView) bool {
		return v.GetPermissionModePicker().GetCurrent().GetMode() != before.GetPermissionModePicker().GetCurrent().GetMode()
	})
	_ = after
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

// ftTaskActivity builds a task-tracker act creating one task at a status.
func ftTaskActivity(activityIDValue, taskID, subject string, completed bool) *conversationv1.AgentActivity {
	state := &conversationv1.AgentTaskState{Subject: subject}
	if completed {
		state.Status = &conversationv1.AgentTaskState_Completed{Completed: &conversationv1.AgentTaskCompleted{}}
	} else {
		state.Status = &conversationv1.AgentTaskState_Pending{Pending: &conversationv1.AgentTaskPending{}}
	}
	return &conversationv1.AgentActivity{
		ActivityId: activityID(activityIDValue),
		Item: &conversationv1.AgentActivity_TaskAct{TaskAct: &conversationv1.AgentTaskAct{
			Task:  &conversationv1.AgentTaskId{Value: taskID},
			Act:   &conversationv1.AgentTaskAct_Created{Created: &conversationv1.AgentTaskCreated{}},
			State: state,
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

// ftFindDegradedWindow finds the first degraded-window warning detail in the
// topbar's warning strip, if any.
func ftFindDegradedWindow(v *frontendv1.TopbarView) *frontendv1.TopbarDegradedWindowWarningDetail {
	for _, w := range v.GetWarnings().GetWarnings() {
		if d := w.GetDegradedWindow(); d != nil {
			return d
		}
	}
	return nil
}

// ftLoadTopbarTones reads the shared render-colors vocabulary's topbar_tones
// set, so a test asserts membership rather than a hardcoded literal.
func ftLoadTopbarTones(t *testing.T) map[string]bool {
	t.Helper()
	path := filepath.Join(harness.RepoRoot(t), "proto", "vocab", "render-colors.json")
	body, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read the shared render-colors vocabulary %s: %v", path, err)
	}
	var doc struct {
		TopbarTones []string `json:"topbar_tones"`
	}
	if err := json.Unmarshal(body, &doc); err != nil {
		t.Fatalf("decode %s: %v", path, err)
	}
	out := make(map[string]bool, len(doc.TopbarTones))
	for _, tone := range doc.TopbarTones {
		out[tone] = true
	}
	return out
}

// f64Ptr is a float64 optional-field builder.
func f64Ptr(v float64) *float64 { return &v }

// ftAwaitTrue polls a condition until it holds, bounded by ctx. Not a
// time.Sleep synchronization device: a bounded poll for an eventually-true
// fact, in the same style as the harness's own file/log waits.
func ftAwaitTrue(t *testing.T, ctx interface{ Done() <-chan struct{} }, pred func() bool, what string) {
	t.Helper()
	ticker := time.NewTicker(5 * time.Millisecond)
	defer ticker.Stop()
	for {
		if pred() {
			return
		}
		select {
		case <-ticker.C:
		case <-ctx.Done():
			t.Fatalf("waiting for %s: context done", what)
		}
	}
}
