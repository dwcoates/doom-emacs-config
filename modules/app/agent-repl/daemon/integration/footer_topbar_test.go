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
	t.Parallel()
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
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
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
	f := newOpened(t, harness.Opts{})
	footer := f.d.WatchFooter(f.ws)
	awaitFooter(t, f, footer, "idle.ready before any turn", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetIdle().GetReady() != nil
	})

	// Act: StartTurn should be visible as working.submitting before the fake
	// even answers.
	f.submit("do it", "k-tree", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)

	// Assert: submitting, then thinking, then done once the turn concludes.
	awaitFooter(t, f, footer, "working.submitting on StartTurn", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetWorking().GetSubmitting() != nil
	})
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

// TestFooterNamesTheRunningCallAndTheQuietStretch pins the working step and
// the quiet-stretch line through the real watcher: the main agent's running
// shell command is the `executing` step, and once it lands the activity line
// says so until the next feed item surfaces.
func TestFooterNamesTheRunningCallAndTheQuietStretch(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	footer := f.d.WatchFooter(f.ws)
	f.submit("do it", "k-quiet", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	bash := func(result *conversationv1.AgentBash) *conversationv1.AgentFrame {
		return activityFrame(mainAgent, &conversationv1.AgentActivity{
			ActivityId: activityID("bash-quiet"),
			Item:       &conversationv1.AgentActivity_Bash{Bash: result},
		})
	}

	// Act: the shell command surfaces.
	f.shim.PushAgentFrame(mainAgent, bash(&conversationv1.AgentBash{
		Result: &conversationv1.AgentBash_Start{Start: &conversationv1.AgentBashStart{}}}))

	// Assert
	awaitFooter(t, f, footer, "working.executing while the shell command runs", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetWorking().GetExecuting() != nil
	})

	// Act: it lands.
	f.shim.PushAgentFrame(mainAgent, bash(&conversationv1.AgentBash{
		Result: &conversationv1.AgentBash_Success{Success: &conversationv1.AgentBashSuccess{}}}))

	// Assert
	awaitFooter(t, f, footer, "the quiet-stretch line once the shell command lands", func(v *frontendv1.FooterView) bool {
		working := v.GetStrip().GetStatus().GetWorking()
		return working.GetThinking() != nil &&
			working.GetActivity().GetUnpinned().GetQuietStretch().GetText() == "✅ Bash finished — handling result..."
	})

	// Act: the response's first frame surfaces.
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("resp-quiet"),
		Item: &conversationv1.AgentActivity_Response{Response: &conversationv1.AgentResponse{
			Result: &conversationv1.AgentResponse_Start{Start: &conversationv1.AgentResponseStart{}}}},
	}))

	// Assert
	awaitFooter(t, f, footer, "the quiet-stretch line cleared by the next surfacing", func(v *frontendv1.FooterView) bool {
		working := v.GetStrip().GetStatus().GetWorking()
		return working != nil && working.GetActivity().GetUnpinned().GetQuietStretch() == nil
	})
}

// ftDwell is the momentary-status dwell the two retirement tests run the
// daemon with, in place of footer.DefaultMomentaryDwell's 1.5s.
//
// MEASURED BASIS: a push that IS coming arrives on an open stream at a p90 of
// 5.8ms and a p50 of 0.4ms over this suite at -parallel 8, so 150ms is ~25x the
// p90 — wide enough that the momentary status and its successor remain two
// distinct, separately observed pushes rather than a coalesced one, and short
// enough that neither test spends its wall time waiting on a window sized for a
// reader.
const ftDwell = 150 * time.Millisecond

func TestFooterInterruptedStatusIsRetiredByADaemonSideDwell(t *testing.T) {
	t.Parallel()
	// Arrange: the dwell is compressed to ftDwell. THE SUBJECT IS THE
	// RETIREMENT, not the window's length — nothing below reads the clock —
	// and the product's 1.5s is sized for a person's eyes, not for a stream
	// this test already holds open.
	f := newOpened(t, harness.Opts{FooterMomentaryDwell: ftDwell})
	footer := f.d.WatchFooter(f.ws)
	f.submit("do it", "k-interrupted", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	awaitFooter(t, f, footer, "thinking before the interrupt", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetWorking() != nil
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
	t.Parallel()
	// Arrange: the dwell is compressed to ftDwell, for the same reason as the
	// interrupted case above.
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

// ---------------------------------------------------------------------------
// Footer: the tokens cell
// ---------------------------------------------------------------------------

// ftPanelInput is the tokens panel's summed uncached-input line: the spend
// across every agent. The strip's cell is the main agent's context growth, so
// the uncached-spend contract is asserted here, on the panel.
func ftPanelInput(v *frontendv1.FooterView) string {
	return v.GetExpanded().GetTokens().GetInput().GetValue()
}

func TestFooterTokensPanelExcludesCacheReads(t *testing.T) {
	t.Parallel()
	// Arrange: a turn whose only usage is a huge cache READ and no misses.
	f := newOpened(t, harness.Opts{})
	footer := f.d.WatchFooter(f.ws)
	f.submit("first", "k-tok-a", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, ftUsageActivity("resp-a", ftUsage(0, 0, 500_000))))
	readOnly := awaitFooter(t, f, footer, "the tokens panel after a cache-read-only response", func(v *frontendv1.FooterView) bool {
		return ftPanelInput(v) != ""
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

	// Assert: the line changed — the diff can only be the miss, since the
	// cache-read figure (input_hits) is identical in both turns and a fresh
	// turn resets the accounting.
	withMiss := awaitFooter(t, f, footer, "the tokens panel after the miss joins the same cache read", func(v *frontendv1.FooterView) bool {
		return ftPanelInput(v) != "" && ftPanelInput(v) != ftPanelInput(readOnly)
	})
	if ftPanelInput(withMiss) == ftPanelInput(readOnly) {
		t.Fatalf("panel input unchanged by a real miss (%q); a cache-read-only figure must not already count it in", ftPanelInput(readOnly))
	}
}

func TestFooterTokensPanelUsageIsNotDoubleCountedAcrossAResponsesUnits(t *testing.T) {
	t.Parallel()
	// Arrange: one API response whose usage is stamped on the FIRST unit
	// only, per the envelope contract.
	f := newOpened(t, harness.Opts{})
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

// TestFooterTokensCellIsNotDerivedFromTheTopbarContextChip pins the two as
// different facts (AGENTS.md, "Fresh input is the one token quantity every
// spend figure counts"): the chip is the context window's size, the cell the
// main agent's fresh input for the turn. A mid-turn context_usage push moves
// the chip and adds nothing to the cell.
func TestFooterTokensCellIsNotDerivedFromTheTopbarContextChip(t *testing.T) {
	t.Parallel()
	// Arrange: the context held before the turn is 100k; then a turn opens.
	f := newOpened(t, harness.Opts{})
	footer := f.d.WatchFooter(f.ws)
	topbar := f.d.WatchTopbar(f.ws)
	f.shim.PushSessionUpdate(ftContextUsage(100_000))
	awaitTopbar(t, f, topbar, "the context chip at the pre-turn 100k", func(v *frontendv1.TopbarView) bool {
		return v.GetContext().GetText() == "100k"
	})
	f.submit("go", "k-tok-context", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	f.shim.ExpectStartTurn()
	awaitFooter(t, f, footer, "the tokens cell at the turn's open", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetTokens().GetInput().GetText() == "0 in"
	})

	// Act: ONE mid-turn context_usage push growing the context by 18.2k, then
	// the main agent's API response with 5k fresh input.
	f.shim.PushSessionUpdate(ftContextUsage(118_200))
	chip := awaitTopbar(t, f, topbar, "the context chip after the mid-turn push", func(v *frontendv1.TopbarView) bool {
		return v.GetContext().GetText() != "100k"
	})
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, ftUsageActivity("resp-ctx", ftUsage(1_000, 4_000, 90_000))))

	// Assert: the chip states the context held; the cell states the fresh
	// input alone, with none of the context's growth in it.
	if chip.GetContext().GetText() != "118.2k" {
		t.Fatalf("context chip = %q, want 118.2k", chip.GetContext().GetText())
	}
	cell := awaitFooter(t, f, footer, "the tokens cell after the usage-carrying response", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetTokens().GetInput().GetText() != "0 in"
	})
	if cell.GetStrip().GetTokens().GetInput().GetText() != "5k in" {
		t.Fatalf("tokens cell = %q, want 5k in: the response's fresh input, none of the context's 18.2k growth",
			cell.GetStrip().GetTokens().GetInput().GetText())
	}
}

// ftContextUsage is one context_usage push stating the context held.
func ftContextUsage(total int64) *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_ContextUsage{ContextUsage: &conversationv1.SessionContextUsage{
			TotalTokens: total,
			MaxTokens:   200_000,
			Model:       "claude-opus-5",
		}},
	}
}

func TestFooterTokensCellVerdictIsIncompleteWhenAResponseCarriedNoUsage(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	footer := f.d.WatchFooter(f.ws)
	f.submit("go", "k-tok-incomplete", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	// The turn's frames follow the shim being ASKED to start it, as they must:
	// a shim cannot stream a turn it has not been handed.
	f.shim.ExpectStartTurn()

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

func TestFooterTokensCellVerdictIsCompleteWhenTheUsageRodeAnEarlierUnitOfTheSameResponse(t *testing.T) {
	t.Parallel()
	// Arrange: the ordinary prose turn. One API response is written as several
	// units and its usage rides the FIRST — here the reasoning block that
	// opened it — so the response unit that settles carries none of its own.
	// Absent usage means "not the carrying unit", never "free", and this turn
	// reconciles CLEANLY.
	f := newOpened(t, harness.Opts{})
	footer := f.d.WatchFooter(f.ws)
	f.submit("go", "k-tok-complete", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	// THE TURN'S FRAMES FOLLOW THE SHIM BEING ASKED TO START IT. A fake that
	// pushes them before StartTurn has reached it is doing what no shim can,
	// and on the revival path that let them land before the turn was even
	// accepted, where the acceptance's reset wiped the usage they carried.
	f.shim.ExpectStartTurn()

	// Act
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("resp-block-0"),
		Usage:      ftUsage(0, 1000, 0),
		Item: &conversationv1.AgentActivity_Thinking{Thinking: &conversationv1.AgentThinking{
			Result: &conversationv1.AgentThinking_Success{Success: &conversationv1.AgentThinkingSuccess{
				Reasoning: &conversationv1.AgentThinkingSuccess_Withheld{Withheld: &conversationv1.AgentThinkingWithheld{}},
			}},
		}},
	}))
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("resp-block-1"),
		Item: &conversationv1.AgentActivity_Response{Response: &conversationv1.AgentResponse{
			Result: &conversationv1.AgentResponse_Success{Success: &conversationv1.AgentResponseSuccess{
				Prose: &conversationv1.AgentResponseProse{Markdown: "done"},
			}},
		}},
	}))
	f.shim.PushAgentFrame(mainAgent, successFrame(mainAgent, activityID("resp-block-1")))

	// Assert
	got := awaitFooter(t, f, footer, "idle.done with the reconciled verdict", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetIdle().GetDone() != nil
	})
	if got.GetStrip().GetTokens().GetVerdict().GetComplete() == nil {
		t.Fatalf("tokens cell verdict = %v, want complete: the response's usage rode the unit that opened it",
			got.GetStrip().GetTokens().GetVerdict())
	}
}

// ---------------------------------------------------------------------------
// Footer: live-work chips
// ---------------------------------------------------------------------------

func TestFooterLiveWorkChipsReflectEachKindsCount(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	footer := f.d.WatchFooter(f.ws)

	// Act: one live subagent, one live shell.
	f.shim.PushAgentFrame(mainAgent, detachedWorkFrame(mainAgent, detachedSubagent("work-agent", "sub-1", "explore the tree")))
	pushDetachedShell(f.shim, "work-shell", "sleep 5")
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

// A MONITOR ROW JUMPS TO ITS CALL'S TOOL-CALL CARD: the feed draws the card
// and announces it, and the footer row names exactly that FeedId.
func TestFooterMonitorRowJumpsToTheMonitorsToolCallCard(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	feed := f.watchRootFeed()
	footer := f.d.WatchFooter(f.ws)

	// Act
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, ftMonitorActivity("mon-1", "watching the build log")))

	// Assert
	card := awaitRow(t, f, feed, "the monitor's tool-call card", func(r *frontendv1.FeedRow) bool {
		return r.GetActivity().GetSimpleToolCall().GetName().GetText() == "Monitor"
	})
	awaitFooter(t, f, footer, "the monitor row naming the card", func(v *frontendv1.FooterView) bool {
		rows := v.GetExpanded().GetMonitors().GetRows()
		return len(rows) == 1 && rows[0].GetJump().GetEntry().GetValue() == card.GetId().GetValue()
	})
}

// A STATUS-ONLY UPDATE MUST NOT BLANK THE CHECKLIST. `TaskUpdate` carries no
// subject when it names only a status, so the act's state leaves the field
// UNSET -- and applying that over the create's own subject drew every checklist
// row as a bare glyph with no words beside it, which is what the G52 playbook
// photographed. The same act states no STATUS either when the tracker has not
// answered it, and reading that as `pending` knocked a running task back to
// unstarted.
func TestFooterChecklistKeepsWhatAnUpdateDidNotState(t *testing.T) {
	t.Parallel()
	// Arrange: a created task with a subject, moved to running.
	f := newOpened(t, harness.Opts{})
	footer := f.d.WatchFooter(f.ws)
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, ftTaskActivity("task-1", "t-1", "Land the converter", false)))
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, ftTaskRunning("task-2", "t-1")))

	// Act: an update that names neither a subject nor a status -- the shape an
	// announcement the tracker has not answered yet produces.
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, ftTaskUnstated("task-3", "t-1")))

	// Assert: the row still says what it is and where it stands.
	awaitFooter(t, f, footer, "the checklist row keeping its subject and its status", func(v *frontendv1.FooterView) bool {
		rows := v.GetExpanded().GetTasks().GetRows()
		if len(rows) != 1 {
			return false
		}
		return rows[0].GetSubject().GetText() == "Land the converter" &&
			rows[0].GetStatus().GetRunning() != nil
	})
}

// A REFUSED UPDATE ADDS NOTHING. `AgentTaskRejected` states it outright, and
// the checklist gained a phantom row anyway -- a bare glyph with no words
// beside it, counted in the chip's denominator, for a task the tracker had just
// said it does not hold. Photographed by the G52 playbook.
func TestFooterChecklistGainsNoRowForATaskNobodyNamed(t *testing.T) {
	t.Parallel()
	// Arrange: one real task.
	f := newOpened(t, harness.Opts{})
	footer := f.d.WatchFooter(f.ws)
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, ftTaskActivity("task-1", "t-1", "Land the converter", false)))
	awaitFooter(t, f, footer, "the checklist with its one named task", func(v *frontendv1.FooterView) bool {
		return len(v.GetExpanded().GetTasks().GetRows()) == 1
	})

	// Act: a refused update's own shape -- an act naming a task and no subject.
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, ftTaskUnstated("task-2", "t-9")))
	// A second, ordinary act gives the assertion something to wait FOR, so it
	// is not a wait on an absence that passes before the frame arrives.
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, ftTaskActivity("task-3", "t-2", "Land the store writer", false)))

	// Assert
	awaitFooter(t, f, footer, "the checklist holding only the tasks that were named", func(v *frontendv1.FooterView) bool {
		rows := v.GetExpanded().GetTasks().GetRows()
		if len(rows) != 2 {
			return false
		}
		return rows[0].GetSubject().GetText() == "Land the converter" &&
			rows[1].GetSubject().GetText() == "Land the store writer"
	})
}

// A SUBJECT STATED EMPTY IS STILL A SUBJECT, and presence is the whole reason
// the daemon can tell it from one an act never named. The producers state it
// this way end to end, so the wire's own distinction is asserted here rather
// than only in the resolver's unit tests.
func TestFooterChecklistTakesASubjectAnActStatesEmpty(t *testing.T) {
	t.Parallel()
	// Arrange: a created task with a subject.
	f := newOpened(t, harness.Opts{})
	footer := f.d.WatchFooter(f.ws)
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, ftTaskActivity("task-1", "t-1", "Land the converter", false)))
	awaitFooter(t, f, footer, "the checklist with its one named task", func(v *frontendv1.FooterView) bool {
		return len(v.GetExpanded().GetTasks().GetRows()) == 1
	})

	// Act: an act that STATES an empty subject.
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, ftTaskActivity("task-2", "t-1", "", false)))

	// Assert
	awaitFooter(t, f, footer, "the checklist row taking the stated empty subject", func(v *frontendv1.FooterView) bool {
		rows := v.GetExpanded().GetTasks().GetRows()
		return len(rows) == 1 && rows[0].GetSubject().GetText() == ""
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
	f := newOpened(t, harness.Opts{})
	footer := f.d.WatchFooter(f.ws)
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, ftSubagentSpawn("toolu-1", "toolu-1", "sweep the tree")))
	f.shim.PushAgentFrame(mainAgent, detachedWorkFrame(mainAgent, movedSubagent("toolu-1")))
	awaitFooter(t, f, footer, "the agents chip counting the detached run", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetLiveWork().GetAgents().GetCount() == 1
	})

	// Act: the run settles on ITS OWN stream, under that book's own unit id.
	f.shim.PushAgentFrame("toolu-1", activityFrame("toolu-1", ftSubagentSettled("sub-unit-9", "toolu-1")))

	// Assert
	awaitFooter(t, f, footer, "the agents chip retired at the run's terminal", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetLiveWork().GetAgents() == nil
	})
}

// AND THE OTHER DELIVERY, which is the one the vendor's task notification
// takes: the same run's terminal settled on the SPAWNING agent's book, under
// the spawn unit. Both are the handle's terminal and both must retire the chip.
func TestFooterAgentsChipRetiresADetachedRunSettledOnTheCallersStream(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	footer := f.d.WatchFooter(f.ws)
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, ftSubagentSpawn("toolu-1", "toolu-1", "sweep the tree")))
	f.shim.PushAgentFrame(mainAgent, detachedWorkFrame(mainAgent, movedSubagent("toolu-1")))
	awaitFooter(t, f, footer, "the agents chip counting the detached run", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetLiveWork().GetAgents().GetCount() == 1
	})

	// Act
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, ftSubagentSettled("toolu-1", "toolu-1")))

	// Assert
	awaitFooter(t, f, footer, "the agents chip retired at the run's terminal", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetLiveWork().GetAgents() == nil
	})
}

func TestFooterLiveWorkChipsAreUnsetWhenZero(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
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
	// ...and the VERDICT comes from rate_limit_status, for the same window. An
	// ALLOWED verdict, because a refusal blocks the session and moves the
	// enduring line out of the idle cell.
	f.shim.PushSessionUpdate(&conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_RateLimitStatus{RateLimitStatus: &conversationv1.SessionRateLimitStatus{
			Status: &conversationv1.SessionRateLimitStatus_Allowed{Allowed: &conversationv1.SessionRateLimitAllowed{}},
			RateLimitType: &conversationv1.SessionRateLimitType{
				Window: &conversationv1.SessionRateLimitType_FiveHour{FiveHour: &conversationv1.SessionRateLimitWindowFiveHour{}},
			},
		}},
	})

	// Assert: the session allowance carries BOTH the figure (utilization,
	// reset) from account_usage and the verdict (allowed) from
	// rate_limit_status -- one cell composed from two different facts.
	got := awaitFooter(t, f, footer, "the session allowance composed from both facts", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetIdle().GetActivity().GetUnpinned().GetEnduring().GetUsage().GetSession().GetAllowed() != nil
	})
	allowance := got.GetStrip().GetStatus().GetIdle().GetActivity().GetUnpinned().GetEnduring().GetUsage().GetSession()
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
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
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

	// Assert: the footer stays thinking (a deny does not end the turn) and no
	// turn_ended row is drawn until the fake pushes the turn's own terminal.
	got := awaitFooter(t, f, footer, "thinking still standing after the deny", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetWorking() != nil
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
	f := newOpened(t, harness.Opts{})
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
	f := newOpened(t, harness.Opts{})
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

func TestFooterARealStatusWinsOverAPendingWakeup(t *testing.T) {
	t.Parallel()
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
		return v.GetStrip().GetStatus().GetWorking() != nil
	})
	if got.GetStrip().GetStatus().GetWaiting() != nil {
		t.Fatalf("footer status = %v while a turn runs, want the wakeup fallback retired", got.GetStrip().GetStatus())
	}
}

func TestFooterApiErrorMidTurnDrawsRetryingEvidenceWithoutEndingTheTurn(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	// The sweep covers every test; the declared record is the vendor failure the test feeds, stated once by its owner.
	f.d.ExpectWarnings("daemon.sessionwatcher.api_error")
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

	// Assert: the turn is still thinking (not ended) and the footer shows the
	// retry evidence.
	got := awaitFooter(t, f, footer, "thinking.retrying evidence mid-turn", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetWorking() != nil
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

func TestFooterScheduledApiRetryCountsLikeTheVendorAndItsResponseAnnouncesTheRestoredAPI(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	// The sweep covers every test; the declared record is the vendor failure the test feeds, stated once by its owner.
	f.d.ExpectWarnings("daemon.sessionwatcher.api_error")
	footer := f.d.WatchFooter(f.ws)
	f.submit("go", "k-api-retry", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	awaitFooter(t, f, footer, "thinking before the mid-turn error", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetWorking() != nil
	})
	const nextAt = int64(1_790_000_032_000)

	// Act: the vendor's eighth retry is scheduled, of ten it allows.
	f.shim.PushAgentFrame(mainAgent, updateFrame(mainAgent, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_ApiError{ApiError: &conversationv1.ApiRequestFailed{
			Message: "connection refused",
			Kind:    &conversationv1.ApiRequestFailed_RateLimited{RateLimited: &conversationv1.ApiRateLimited{}},
			Retry:   &conversationv1.ApiRetry{Attempt: 8, MaxRetries: 10, NextAttemptAtMs: nextAt},
		}},
	}))

	// Assert: the retrying line counts attempts as the vendor does and carries
	// its schedule.
	got := awaitFooter(t, f, footer, "the retrying line with the vendor's schedule", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetWorking().GetActivity().GetSalient().GetRetrying() != nil
	})
	retry := got.GetStrip().GetStatus().GetWorking().GetActivity().GetSalient().GetRetrying()
	if retry.GetAttempt() != 9 || retry.GetMaxAttempt() != 11 || retry.GetNextAttempt().GetAtMs() != nextAt {
		t.Fatalf("retrying = %v, want attempt 9 of 11 next at %d", retry, nextAt)
	}

	// Act: the retried call answers.
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("think-restored"),
		Item:       &conversationv1.AgentActivity_Thinking{Thinking: &conversationv1.AgentThinking{Result: &conversationv1.AgentThinking_Start{Start: &conversationv1.AgentThinkingStart{}}}},
	}))

	// Assert: the retrying line ends and the restored API is announced.
	got = awaitFooter(t, f, footer, "the api_restored transient", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetWorking().GetActivity().GetUnpinned().GetTransient().GetApiRestored() != nil
	})
	restored := got.GetStrip().GetStatus().GetWorking().GetActivity().GetUnpinned().GetTransient().GetApiRestored()
	if restored.GetFailedAttempts() != 8 {
		t.Fatalf("api_restored = %v, want 8 failed attempts", restored)
	}
}

// ---------------------------------------------------------------------------
// Footer: link death
// ---------------------------------------------------------------------------

func TestFooterLinkDeathFlipsToSeveredAndTheDaemonRedials(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	// The sweep covers every test; the declared records are evidence of a session fault the test opens, the shim link the test severs.
	f.d.ExpectWarnings("daemon.sessionwatcher.reopen", "daemon.health.open_fault", "daemon.sessionwatcher.link_fault",
		"daemon.sessionwatcher.watch_session", "daemon.shimclient.redial")
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
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
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
		return v.GetStrip().GetStatus().GetDisconnected().GetDead() != nil
	})
	awaitRoster(t, f.d, roster, "the roster's dead status", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, f.ws.GetId())
		return row != nil && row.GetDead() != nil
	})

	// Assert: no further churn — a dead shim gets no more redial attempts, so
	// the footer settles on dead rather than cycling. The revival the daemon
	// starts for it may retract the death's fault line as its new shim
	// attaches (held at StartSession above), so a push may come; none of
	// them leaves dead for a redial's severed or dialing step.
	probe := time.NewTimer(harness.ProbeWindow)
	defer probe.Stop()
	for waiting := true; waiting; {
		select {
		case v, ok := <-footer.C:
			if ok && v.GetStrip().GetStatus().GetDisconnected().GetDead() == nil {
				t.Fatalf("footer push %v, want it still disconnected.dead: no further churn once the shim is dead (redials stop)", v.GetStrip().GetStatus())
			}
		case <-probe.C:
			waiting = false
		}
	}
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
		"daemon.sessionwatcher.link_fault", "daemon.health.open_fault")
}

// ---------------------------------------------------------------------------
// Topbar
// ---------------------------------------------------------------------------

func TestTopbarTitleIsComposedFromTheWorkspacesNaming(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
	// Arrange / Act
	f := newOpened(t, harness.Opts{})
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

func TestTopbarContextChipReflectsTheContextUsagePush(t *testing.T) {
	t.Parallel()
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

	// Assert: the chip's figure is EXACTLY the canonical formatter's output
	// for 142_300 tokens (figures.Tokens, internal/figures/tokens.go), and the
	// chip always ships a populated breakdown (no round-trip needed to open
	// the hover).
	// The fake's OPENING context_usage push already fills the chip
	// (fakeshim.DefaultContextUsage, 1000 tokens), so the wait is for the
	// chip to move off that opening figure -- waiting merely for a non-empty
	// one would read the opening view and assert against it.
	got := awaitTopbar(t, f, topbar, "the context chip after context_usage", func(v *frontendv1.TopbarView) bool {
		text := v.GetContext().GetText()
		return text != "" && text != "1k"
	})
	if got.GetContext().GetText() != "142.3k" {
		t.Fatalf("context chip text = %q, want the canonical formatter's \"142.3k\" for 142_300 tokens", got.GetContext().GetText())
	}
	if got.GetContext().GetBreakdown() == nil {
		t.Fatalf("context chip breakdown = nil, want it always populated on the push carrying context_usage")
	}
}

func TestTopbarContextBreakdownRowsCarrySharePermilleAndEmphasized(t *testing.T) {
	t.Parallel()
	// Arrange: a turn whose single usage-carrying unit fixes the session
	// breakdown's basis at a round 1000 tokens (100 uncached input + 900
	// cache read), so each row's share_permille is an exact, checkable figure.
	f := newOpened(t, harness.Opts{})
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

func TestTopbarContextPanelResolvesFromTheSameContextUsageFact(t *testing.T) {
	t.Parallel()
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
	// edge for the panel the fact also feeds. THE EDGE NAMES THIS FACT'S OWN
	// FIGURE: the fake states an opening context usage of its own alongside
	// readiness (fakeshim.DefaultContextUsage, 1000 tokens and no categories),
	// so a merely non-empty chip is already true before this push lands and
	// would let the panel be read from that opening fact instead.
	awaitTopbar(t, f, topbar, "the context chip carrying the pushed 50,000 tokens", func(v *frontendv1.TopbarView) bool {
		return v.GetContext().GetText() == "50k"
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
		t.Fatalf("the /context panel = %v, want a system prompt category", panel.GetSections())
	}
	if !strings.Contains(row.GetFigure(), "4") {
		t.Fatalf("the system prompt category figure = %q, want the pushed 4000 tokens", row.GetFigure())
	}
}

// ftFindContextCategory finds a /context panel top-level section by label. The
// panel is a SECTION TREE: a vendor category is a top-level section row.
func ftFindContextCategory(panel *frontendv1.ContextPanelView, label string) *frontendv1.ContextPanelSection {
	for _, section := range panel.GetSections() {
		if section.GetLabel() == label {
			return section
		}
	}
	return nil
}

func TestTopbarWarningForASessionFaultIsRetractedOnTheNextHealthyPush(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	// The sweep covers every test; the declared records are evidence of a session fault the test opens.
	f.d.ExpectWarnings("daemon.health.open_fault")
	topbar := f.d.WatchTopbar(f.ws)

	// Act: an unhealthy diagnostics pull surfaces a warning.
	f.shim.PushUnhealthy(&conversationv1.SessionFault{
		Component: "converter",
		Detail:    "could not model a record",
		Kind:      &conversationv1.SessionFault_ConverterDefect{ConverterDefect: &conversationv1.SessionFaultConverterDefect{}},
	})
	got := awaitTopbar(t, f, topbar, "the session-fault warning", func(v *frontendv1.TopbarView) bool {
		return len(v.GetWarnings().GetWarnings()) > 0
	})

	// Assert: exactly one warning is drawn for the one fault, carrying the
	// pushed component and detail verbatim, with a non-empty list line.
	warnings := got.GetWarnings().GetWarnings()
	if len(warnings) != 1 {
		t.Fatalf("warnings = %v (%d), want exactly 1 for the one pushed fault", warnings, len(warnings))
	}
	fault := warnings[0].GetSessionFault()
	if fault.GetComponent().GetText() != "converter" {
		t.Fatalf("session_fault.component.text = %q, want the pushed \"converter\"", fault.GetComponent().GetText())
	}
	if fault.GetDetail().GetText() != "could not model a record" {
		t.Fatalf("session_fault.detail.text = %q, want the pushed \"could not model a record\"", fault.GetDetail().GetText())
	}
	if warnings[0].GetLine().GetText() == "" {
		t.Fatal("warning.line.text is empty, want a non-empty dropdown-row sentence")
	}

	// Act: the next pull comes back healthy.
	f.shim.PushHealthy()

	// Assert: the warning is retracted.
	awaitTopbar(t, f, topbar, "the warning retracted on the next healthy push", func(v *frontendv1.TopbarView) bool {
		return len(v.GetWarnings().GetWarnings()) == 0
	})
}

func TestTopbarTwoSessionFaultsDrawTwoWarningsNewestFirst(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	// The sweep covers every test; the declared record is evidence of the session faults the test pushes.
	f.d.ExpectWarnings("daemon.health.open_fault")
	topbar := f.d.WatchTopbar(f.ws)

	// Act: one unhealthy pull naming two distinct faults, in this order.
	f.shim.PushUnhealthy(
		&conversationv1.SessionFault{
			Component: "store",
			Detail:    "writes are buffering",
			Kind:      &conversationv1.SessionFault_StoreUnreachable{StoreUnreachable: &conversationv1.SessionFaultStoreUnreachable{}},
		},
		&conversationv1.SessionFault{
			Component: "converter",
			Detail:    "could not model a record",
			Kind:      &conversationv1.SessionFault_ConverterDefect{ConverterDefect: &conversationv1.SessionFaultConverterDefect{}},
		},
	)

	// Assert: two warnings are drawn, the LATER-named fault (converter) first.
	got := awaitTopbar(t, f, topbar, "both session-fault warnings", func(v *frontendv1.TopbarView) bool {
		return len(v.GetWarnings().GetWarnings()) == 2
	})
	warnings := got.GetWarnings().GetWarnings()
	if warnings[0].GetSessionFault().GetComponent().GetText() != "converter" {
		t.Fatalf("warnings[0].session_fault.component.text = %q, want the newest fault (\"converter\") first", warnings[0].GetSessionFault().GetComponent().GetText())
	}
	if warnings[1].GetSessionFault().GetComponent().GetText() != "store" {
		t.Fatalf("warnings[1].session_fault.component.text = %q, want the older fault (\"store\") second", warnings[1].GetSessionFault().GetComponent().GetText())
	}
}

func TestTopbarDegradedWindowIsDrawnOpenThenClosed(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
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
	t.Parallel()
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
	t.Parallel()
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
	t.Parallel()
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

	// Assert: current.mode is exactly the wire spelling "accept_edits" — the
	// session facts' own vocabulary, which SetPermissionMode echoes unchanged.
	after := awaitTopbar(t, f, topbar, "the picker's current mode after permission_mode_changed", func(v *frontendv1.TopbarView) bool {
		return v.GetPermissionModePicker().GetCurrent().GetMode() != before.GetPermissionModePicker().GetCurrent().GetMode()
	})
	if after.GetPermissionModePicker().GetCurrent().GetMode() != "accept_edits" {
		t.Fatalf("permission_mode_picker.current.mode = %q after permission_mode_changed{accept_edits}, want exactly \"accept_edits\"",
			after.GetPermissionModePicker().GetCurrent().GetMode())
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

// ftTaskActivity builds a task-tracker act creating one task at a status.
func ftTaskActivity(activityIDValue, taskID, subject string, completed bool) *conversationv1.AgentActivity {
	state := &conversationv1.AgentTaskState{Subject: &subject}
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

// ftTaskRunning moves a task to running, naming NO subject -- which is what a
// `TaskUpdate(status)` carries, the tracker echoing no subject of its own. The
// field is UNSET rather than empty, which is what presence is for.
func ftTaskRunning(activityIDValue, taskID string) *conversationv1.AgentActivity {
	return &conversationv1.AgentActivity{
		ActivityId: activityID(activityIDValue),
		Item: &conversationv1.AgentActivity_TaskAct{TaskAct: &conversationv1.AgentTaskAct{
			Task: &conversationv1.AgentTaskId{Value: taskID},
			Act:  &conversationv1.AgentTaskAct_Changed{Changed: &conversationv1.AgentTaskChanged{}},
			State: &conversationv1.AgentTaskState{
				Status: &conversationv1.AgentTaskState_Running{Running: &conversationv1.AgentTaskRunning{}},
			},
		}},
	}
}

// ftTaskUnstated is an act that says nothing about the task at all: the shape
// an announcement the tracker has not answered yet produces.
func ftTaskUnstated(activityIDValue, taskID string) *conversationv1.AgentActivity {
	return &conversationv1.AgentActivity{
		ActivityId: activityID(activityIDValue),
		Item: &conversationv1.AgentActivity_TaskAct{TaskAct: &conversationv1.AgentTaskAct{
			Task:  &conversationv1.AgentTaskId{Value: taskID},
			Act:   &conversationv1.AgentTaskAct_Changed{Changed: &conversationv1.AgentTaskChanged{}},
			State: &conversationv1.AgentTaskState{},
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

// ---------------------------------------------------------------------------
// Connectivity truth per hop (daemon.md invariant 11): a workspace is
// CONNECTED only while its shim.v1 WatchSession, its WatchHostWorkspace and its
// WatchWebWorkspace are all live. Any one down is not connected.
// ---------------------------------------------------------------------------

func TestFooterIsNotConnectedWhileTheWebHopIsDown(t *testing.T) {
	t.Parallel()
	// Arrange: a workspace opened with only the HOST hop held, so the shim
	// link serves but the web hop does not.
	f := newRegistered(t, harness.Opts{})
	f.open()
	f.host = f.d.WatchHost(f.ws)
	footer := f.d.WatchFooter(f.ws)

	// Act, Assert: the footer draws disconnected.
	awaitFooter(t, f, footer, "disconnected while the web hop is down", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetDisconnected() != nil
	})
}

func TestFooterBecomesConnectedWhenTheWebHopComesUp(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{})
	f.open()
	f.host = f.d.WatchHost(f.ws)
	footer := f.d.WatchFooter(f.ws)
	awaitFooter(t, f, footer, "disconnected while the web hop is down", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetDisconnected() != nil
	})

	// Act: the page opens its stream, putting the last hop up.
	f.web = f.d.WatchWeb(f.ws)

	// Assert
	awaitFooter(t, f, footer, "idle once every hop is live", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetIdle() != nil
	})
}

func TestFooterReturnsToNotConnectedWhenTheWebHopGoesAway(t *testing.T) {
	t.Parallel()
	// Arrange: every hop up.
	f := newOpened(t, harness.Opts{})
	footer := f.d.WatchFooter(f.ws)
	awaitFooter(t, f, footer, "idle with every hop live", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetIdle() != nil
	})

	// Act: the page goes away.
	f.web.Close()

	// Assert
	awaitFooter(t, f, footer, "disconnected once the web hop is cancelled", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetDisconnected() != nil
	})
}

func TestTopbarIsNotConnectedWhileTheWebHopIsDown(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{})
	f.open()
	f.host = f.d.WatchHost(f.ws)
	topbar := f.d.WatchTopbar(f.ws)

	// Act, Assert: the connectivity indicator never reads the serving glyph
	// while a client hop is down.
	got := awaitTopbar(t, f, topbar, "a resolved topbar", func(v *frontendv1.TopbarView) bool {
		return v.GetConnectivity() != nil
	})
	if got.GetConnectivity().GetTitle() == "connected to the session" {
		t.Fatalf("topbar connectivity = %+v, want not-connected while the web hop is down", got.GetConnectivity())
	}
}

func TestTopbarBecomesConnectedWhenTheWebHopComesUp(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{})
	f.open()
	f.host = f.d.WatchHost(f.ws)
	topbar := f.d.WatchTopbar(f.ws)
	awaitTopbar(t, f, topbar, "a resolved topbar", func(v *frontendv1.TopbarView) bool {
		return v.GetConnectivity() != nil
	})

	// Act
	f.web = f.d.WatchWeb(f.ws)

	// Assert
	awaitTopbar(t, f, topbar, "the connected indicator", func(v *frontendv1.TopbarView) bool {
		return v.GetConnectivity().GetTitle() == "connected to the session"
	})
}

func TestTopbarReturnsToNotConnectedWhenTheWebHopGoesAway(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	topbar := f.d.WatchTopbar(f.ws)
	awaitTopbar(t, f, topbar, "the connected indicator", func(v *frontendv1.TopbarView) bool {
		return v.GetConnectivity().GetTitle() == "connected to the session"
	})

	// Act
	f.web.Close()

	// Assert
	awaitTopbar(t, f, topbar, "the not-connected indicator", func(v *frontendv1.TopbarView) bool {
		return v.GetConnectivity().GetTitle() != "connected to the session"
	})
}

// TestMcpPanelListsEveryServerTheSessionStatedAHealthFor pins the /mcp
// producer end to end: the shim's per-server mcp_server updates are retained
// by the topbar resolver and drawn as the panel's rows, one per server, each
// carrying the health the shim stated.
func TestMcpPanelListsEveryServerTheSessionStatedAHealthFor(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	topbar := f.d.WatchTopbar(f.ws)

	// Act
	f.shim.PushSessionUpdate(&conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_McpServer{McpServer: &conversationv1.SessionMcpServer{
			Name:   "github",
			Health: &conversationv1.SessionMcpServer_Connected{Connected: &conversationv1.SessionMcpServerConnected{}},
		}},
	})
	f.shim.PushSessionUpdate(&conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_McpServer{McpServer: &conversationv1.SessionMcpServer{
			Name: "linear",
			Health: &conversationv1.SessionMcpServer_Failed{Failed: &conversationv1.SessionMcpServerFailed{
				Error: "connection refused",
			}},
		}},
	})
	// The two updates carry nothing the topbar draws, so a LATER update on the
	// SAME stream is the synchronizing edge: the stream is ordered, so a chip
	// carrying this usage proves both mcp_server frames were already applied.
	f.shim.PushSessionUpdate(&conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_ContextUsage{ContextUsage: &conversationv1.SessionContextUsage{
			TotalTokens: 50_000, MaxTokens: 200_000, Percentage: 25, Model: "claude-opus-5",
		}},
	})
	// The edge names THIS push's own figure: the fake's opening context usage
	// (1000 tokens) already draws a non-empty chip, so "non-empty" would be
	// satisfied before this update — and therefore before the two mcp_server
	// frames ahead of it — had been applied.
	awaitTopbar(t, f, topbar, "the context chip that follows the mcp_server updates", func(v *frontendv1.TopbarView) bool {
		return v.GetContext().GetText() == "50k"
	})

	// Assert
	resp := f.submit("/mcp", "k-mcp-panel", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	panel := resp.GetSuccess().GetCommandPanel().GetMcp()
	if panel == nil {
		t.Fatalf("SubmitPrompt(/mcp) = %v, want a command_panel.mcp", resp)
	}
	rows := panel.GetRows()
	if len(rows) != 2 {
		t.Fatalf("the /mcp panel rows = %v, want one row per stated server", rows)
	}
	if rows[0].GetName() != "github" || rows[0].GetConnected() == nil {
		t.Fatalf("row 0 = %v, want github connected", rows[0])
	}
	if rows[1].GetName() != "linear" || rows[1].GetFailed().GetDetail().GetText() != "connection refused" {
		t.Fatalf("row 1 = %v, want linear failed with the stated error", rows[1])
	}
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

// A NEWER TRANSIENT REPLACES AN OLDER ONE: the transient tier is ordered by
// recency alone (owner ruling, 2026-09-28).
func TestFooterANewerTransientReplacesAnOlderOne(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	footer := f.d.WatchFooter(f.ws)
	f.shim.PushAgentFrame(mainAgent, ftHookStart("hook-1", "first-hook"))
	awaitFooter(t, f, footer, "the first hook's transient", func(v *frontendv1.FooterView) bool {
		return ftTransient(v).GetHook().GetName() == "first-hook"
	})

	// Act
	f.shim.PushAgentFrame(mainAgent, ftHookStart("hook-2", "second-hook"))

	// Assert
	got := awaitFooter(t, f, footer, "the newer hook replacing the first", func(v *frontendv1.FooterView) bool {
		return ftTransient(v).GetHook().GetName() == "second-hook"
	})
	transient := ftTransient(got)
	if transient.GetExpiry().GetExpiresAtMs() <= transient.GetAt().GetAtMs() {
		t.Fatalf("transient expiry = %d at %d, want an expiry after the event", transient.GetExpiry().GetExpiresAtMs(), transient.GetAt().GetAtMs())
	}
}

// THE PUSH NOTIFICATION IS SALIENT (owner ruling, 2026-09-30): it stands over
// a live transient until the next prompt.
func TestFooterAPushNotificationStandsOverALiveTransient(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
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

// A CONTEXT-BUDGET WARNING IS SALIENT (owner ruling, 2026-09-30), standing
// until a cut shrinks the context.
func TestFooterAContextBudgetWarningStandsUntilACut(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	footer := f.d.WatchFooter(f.ws)
	f.shim.PushAgentFrame(mainAgent, updateFrame(mainAgent, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_ContextBudgetWarning{ContextBudgetWarning: &conversationv1.ContextBudgetWarning{Text: "context filling"}},
	}))
	awaitFooter(t, f, footer, "the salient context-budget line", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetIdle().GetActivity().GetSalient().GetContextBudget().GetText() == "context filling"
	})

	// Act
	f.shim.PushSessionUpdate(&conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_CompactionProgress{
		CompactionProgress: &conversationv1.SessionCompactionProgress{
			Phase:        conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_STARTED,
			TokensBefore: 180_000,
			TokensAfter:  20_000,
		}}})

	// Assert
	awaitFooter(t, f, footer, "the budget line ended by the compaction", func(v *frontendv1.FooterView) bool {
		activity := v.GetStrip().GetStatus().GetIdle().GetActivity()
		return activity.GetSalient().GetContextBudget() == nil && activity.GetUnpinned().GetTransient().GetCompactionConcluded() != nil
	})
}

// THE ENDURING LINE IS ALWAYS DRAWN: an idle session with no transient still
// states how close the account is and how full the context window is, however
// unremarkable the figures.
func TestFooterTheEnduringLineStandsWithNoTransient(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	footer := f.d.WatchFooter(f.ws)

	// Act: an unremarkable sample.
	f.shim.PushSessionUpdate(&conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_AccountUsage{AccountUsage: &conversationv1.SessionAccountUsage{
			ObservedAtMs: 1_700_000_000_000,
			Outcome: &conversationv1.SessionAccountUsage_Available{Available: &conversationv1.SessionAccountUsageAvailable{
				FiveHour: &conversationv1.SessionUsageWindow{UtilizationPercent: 12, ResetsAtMs: 1_700_010_000_000},
			}},
		}},
	})

	// Assert
	got := awaitFooter(t, f, footer, "the enduring usage line", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetIdle().GetActivity().GetUnpinned().GetEnduring().GetUsage().GetSession() != nil
	})
	enduring := got.GetStrip().GetStatus().GetIdle().GetActivity().GetUnpinned().GetEnduring()
	if session := enduring.GetUsage().GetSession(); session.GetUtilization() != 0.12 || session.GetNewsworthy() {
		t.Fatalf("session allowance = %v, want 0.12 drawn and not newsworthy", session)
	}
}

// A BACKGROUND SUBAGENT WAITING FOR THE API KEEPS ITS ROW (visibility only,
// owner ruling 2026-09-28): the failed run's row is drawn waiting_for_api, the
// agents chip counts it with the waiting glyph, and each edge of the wait is a
// transient.
func TestFooterANetworkResumeWaitKeepsTheAgentsRowAndMarksTheChip(t *testing.T) {
	t.Parallel()
	// Arrange: a detached run that has ended.
	f := newOpened(t, harness.Opts{})
	footer := f.d.WatchFooter(f.ws)
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, ftSubagentSpawn("toolu-1", "toolu-1", "sweep the tree")))
	f.shim.PushAgentFrame(mainAgent, detachedWorkFrame(mainAgent, movedSubagent("toolu-1")))
	awaitFooter(t, f, footer, "the agents chip counting the detached run", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetLiveWork().GetAgents().GetCount() == 1
	})
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, ftSubagentSettled("toolu-1", "toolu-1")))
	awaitFooter(t, f, footer, "the agents chip retired at the run's terminal", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetLiveWork().GetAgents() == nil
	})

	// Act: the shim opens a wait for the failed run.
	f.shim.PushSessionUpdate(&conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_NetworkResumeWaits{
		NetworkResumeWaits: &conversationv1.SessionNetworkResumeWaits{Waits: []*conversationv1.SessionNetworkResumeWait{{
			Work:        &conversationv1.DetachedWorkId{Value: "toolu-1"},
			FailedAtMs:  1_700_000_000_000,
			GivesUpAtMs: 1_700_001_800_000,
		}}},
	}})

	// Assert
	got := awaitFooter(t, f, footer, "the waiting row and chip glyph", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetLiveWork().GetAgents().GetWaitingForApi().GetCount() == 1
	})
	if chip := got.GetStrip().GetLiveWork().GetAgents(); chip.GetCount() != 1 {
		t.Fatalf("agents chip = %v, want the waiting row counted", chip)
	}
	rows := got.GetExpanded().GetAgents().GetRows()
	if len(rows) != 1 || rows[0].GetWaitingForApi().GetGivesUpAtMs() != 1_700_001_800_000 {
		t.Fatalf("agent rows = %v, want the one row waiting with its give-up instant", rows)
	}
	if edge := ftTransient(got).GetNetworkResume().GetWaiting(); edge == nil {
		t.Fatalf("transient = %v, want the waiting edge", ftTransient(got))
	}
	if got.GetStrip().GetStatus().GetIdle() == nil {
		t.Fatalf("status = %v, want idle: a wait is not background work", got.GetStrip().GetStatus())
	}

	// Act: the wait ends, resumed.
	f.shim.PushSessionUpdate(&conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_NetworkResumeOutcome{
		NetworkResumeOutcome: &conversationv1.SessionNetworkResumeOutcome{
			Work:    &conversationv1.DetachedWorkId{Value: "toolu-1"},
			Outcome: &conversationv1.SessionNetworkResumeOutcome_Resumed{Resumed: &conversationv1.SessionNetworkResumeResumed{}},
		},
	}})
	f.shim.PushSessionUpdate(&conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_NetworkResumeWaits{
		NetworkResumeWaits: &conversationv1.SessionNetworkResumeWaits{},
	}})

	// Assert
	awaitFooter(t, f, footer, "the resumed edge with the row gone", func(v *frontendv1.FooterView) bool {
		return ftTransient(v).GetNetworkResume().GetResumed() != nil && v.GetStrip().GetLiveWork().GetAgents() == nil
	})
}
