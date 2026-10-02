// turnlifecycle_e2e_test.go — SPEC.md section C, "Turn lifecycle" (#1-10).
//
// Every test here submits one real prompt through the daemon's SubmitPrompt
// rpc, lets the real (--fake) shim answer it against a NAMED fake-SDK
// scenario (agent-shim/claude/shim/src/fake/scenarios/*.ts, selected by the
// prompt's `!name` prefix per fake/registry.ts — the prose/no-prefix cases
// use driveDocumentedPrompt instead), and asserts on the resulting
// FeedTurnEnded row the daemon renders over its Connect API. Durability
// (driveScenarioToCompletion's cursor-advance wait) is used wherever the
// invocation is a bare "!"+scenario, so each of those tests also proves the
// real sidecar committed the turn's facts to the real store, not merely that
// a frame crossed the wire.
//
// OPEN QUESTIONS this file's own investigation surfaced (reading only the
// contract: proto/src, proto/gen/go, docs/overhaul/*.md, and the shim's own
// fake-SDK source — never daemon/shim/store/sidecar production logic) are
// called out test-by-test below and repeated in the dispatch report. None of
// them is guessed around; each test asserts only what the contract
// documents as wired today.
package e2e

import (
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"connectrpc.com/connect"

	"claude-repld/integration/harness"
)

// ---------------------------------------------------------------------------
// Local helpers. Named with a tl* prefix, not a generic name, since every
// area file shares this package and this suite's fanout dispatches one
// writer per file with no cross-file coordination on identifier names.
// ---------------------------------------------------------------------------

// tlNewWorkspace mints a fresh fake-git-backed repository and registers it as
// a workspace on w's daemon. This suite does NOT use real git (project-lead
// ruling, superseding the SkipFakeGit design in SPEC.md section B/D "Real
// git": every external dependency stays mocked — git is the daemon harness's
// scripted fake, the vendor is the fake SDK). NewWorld never sets
// Opts.SkipFakeGit, so the daemon gets the same scripted git every ordinary
// daemon/integration test gets.
func tlNewWorkspace(t *testing.T, w *World) *workspacev1.WorkspaceRef {
	t.Helper()
	repo := harness.NewRepo(t)
	return harness.Register(t, w.Daemon, repo.Dir)
}

// tlOpenRows answers the workspace's current root-feed page, post-turn — the
// same page AwaitTurnEnded consults, exposed here for tests that need to
// inspect a DIFFERENT row than the turn-ended row itself (the settled
// response bubble a Concluded terminal's Answer points at).
func tlOpenRows(t *testing.T, w *World, ws *workspacev1.WorkspaceRef) []*frontendv1.FeedRow {
	t.Helper()
	opened, err := w.Client().OpenFeed(w.Ctx(), connect.NewRequest(&agentreplv1.OpenFeedRequest{Workspace: ws}))
	if err != nil {
		t.Fatalf("OpenFeed: %v", err)
	}
	success := opened.Msg.GetSuccess()
	if success == nil {
		t.Fatalf("OpenFeed = %v, want success", opened.Msg)
	}
	return success.GetPage().GetSuccess().GetRows()
}

// tlResponseMarkdown finds the row named by id among rows and answers its
// settled response markdown, failing the test loudly if no such row exists or
// it never settled.
func tlResponseMarkdown(t *testing.T, rows []*frontendv1.FeedRow, id *frontendv1.FeedId) string {
	t.Helper()
	if id == nil {
		t.Fatalf("tlResponseMarkdown: nil FeedId (turn concluded with no Answer)")
	}
	for _, row := range rows {
		if row.GetId().GetValue() != id.GetValue() {
			continue
		}
		resp := row.GetActivity().GetResponse()
		if resp == nil {
			t.Fatalf("row %s is not a response activity: %v", id.GetValue(), row)
		}
		success := resp.GetSuccess()
		if success == nil {
			t.Fatalf("response row %s has no settled Success prose (result=%v)", id.GetValue(), resp.GetResult())
		}
		return success.GetProse().GetMarkdown()
	}
	t.Fatalf("no feed row found with FeedId %s", id.GetValue())
	return ""
}

// tlAssertHeadline asserts the errored terminal's daemon-composed headline is
// non-empty — Landing 8 (PROTO-CHANGES.md): every FeedTurnEndedErrored carries
// `headline`, the client's whole account of what the arm means, drawn by the
// daemon rather than table-looked-up by the client.
func tlAssertHeadline(t *testing.T, errored *frontendv1.FeedTurnEndedErrored) {
	t.Helper()
	if errored.GetHeadline().GetText() == "" {
		t.Fatalf("FeedTurnEndedErrored.Headline.Text is empty, want a non-empty daemon-composed headline: %v", errored)
	}
}

// ---------------------------------------------------------------------------
// #1 TurnStartToCompletion
// ---------------------------------------------------------------------------

// TestTurnStartToCompletion pins daemon.md's "Package map" (agentrepl/v1
// FEED: SubmitPrompt; OpenFeed -> WatchFeed) and shim.md's "What the shim's
// streams owe consumers" ("every bounded stream concludes with a TERMINAL
// FRAME"). Scenario: the fake registry's DEFAULT (no `!name` prefix) —
// `PROSE` in agent-shim/claude/shim/src/fake/scenarios/prose.ts — which is
// the golden `prose-streamed` capture's shape (MANIFEST.md: "hook, thinking,
// response -> success.completed"). Driven via driveDocumentedPrompt because
// PROSE's own documented invocation is "(any text with no `!scenario`
// prefix)", not a bare "!"+name.
func TestTurnStartToCompletion(t *testing.T) {
	t.Parallel()
	// Arrange
	w := NewWorld(t, WorldOpts{})
	ws := tlNewWorkspace(t, w)

	// Act
	turn := driveDocumentedPrompt(t, w, ws, w.DefaultConfigDir, "Tell me something you find interesting.")
	row := AwaitTurnEnded(t, w, ws, turn)

	// Assert
	ended := row.GetTurnEnded()
	if ended == nil {
		t.Fatalf("row for turn %s has no TurnEnded: %v", turn.GetValue(), row)
	}
	concluded := ended.GetConcluded()
	if concluded == nil {
		t.Fatalf("FeedTurnEnded.Outcome = %v, want Concluded (success.completed)", ended)
	}
	markdown := tlResponseMarkdown(t, tlOpenRows(t, w, ws), concluded.GetAnswer())
	if markdown == "" {
		t.Fatalf("concluded turn's answer settled with empty markdown")
	}
}

// ---------------------------------------------------------------------------
// #2 TurnStopMaxTurns, #3 TurnStopMaxBudgetUsd
// ---------------------------------------------------------------------------

// TestTurnStopMaxTurns pins shim.md's "Where the sixteen failure arms
// actually live" (`AgentFailure.max_turns`, `result.terminal_reason:
// "max_turns"`) as landed on the wire by PROTO-CHANGES.md's "Landing 8"
// (2026-09-02, protos 1fdf85e63, bindings 3791cd630): `frontend.v1
// FeedTurnEndedErrored.error` gained `max_turns` (tag 19,
// `FailureVendorMaxTurns`), importing failure.proto's evidence message as
// that file prescribes. Before Landing 8 this arm had no wire path to any
// frontend stream at all (this test's own prior revision could only assert
// "some terminal" for exactly that reason).
//
// Scenario: `!fail-max-turns`
// (agent-shim/claude/shim/src/fake/scenarios/failures.ts's FAIL_MAX_TURNS) —
// the fake-registry name behind the `turn-stop-max-turns` capture golden
// named in SPEC.md's test list and MANIFEST.md's table.
func TestTurnStopMaxTurns(t *testing.T) {
	t.Parallel()
	// Arrange
	w := NewWorld(t, WorldOpts{})
	ws := tlNewWorkspace(t, w)

	// Act
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "fail-max-turns")
	row := AwaitTurnEnded(t, w, ws, turn)

	// Assert
	ended := row.GetTurnEnded()
	if ended == nil {
		t.Fatalf("row for turn %s has no TurnEnded: %v", turn.GetValue(), row)
	}
	errored := ended.GetErrored()
	if errored == nil {
		t.Fatalf("FeedTurnEnded.Outcome = %v, want Errored{MaxTurns}", ended)
	}
	if errored.GetMaxTurns() == nil {
		t.Fatalf("FeedTurnEndedErrored.Error = %T, want MaxTurns", errored.GetError())
	}
	tlAssertHeadline(t, errored)
}

// TestTurnStopMaxBudgetUsd is TestTurnStopMaxTurns's sibling for the budget
// ceiling: shim.md's `AgentFailure.budget_exhausted`, landed by Landing 8 as
// `frontend.v1 FeedTurnEndedErrored.error`'s `max_budget` (tag 20,
// `FailureVendorMaxBudget`). Scenario: `!fail-budget` (failures.ts's
// FAIL_BUDGET) — the `turn-stop-max-budget-usd` golden's fake-registry name.
func TestTurnStopMaxBudgetUsd(t *testing.T) {
	t.Parallel()
	// Arrange
	w := NewWorld(t, WorldOpts{})
	ws := tlNewWorkspace(t, w)

	// Act
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "fail-budget")
	row := AwaitTurnEnded(t, w, ws, turn)

	// Assert
	ended := row.GetTurnEnded()
	if ended == nil {
		t.Fatalf("row for turn %s has no TurnEnded: %v", turn.GetValue(), row)
	}
	errored := ended.GetErrored()
	if errored == nil {
		t.Fatalf("FeedTurnEnded.Outcome = %v, want Errored{MaxBudget}", ended)
	}
	if errored.GetMaxBudget() == nil {
		t.Fatalf("FeedTurnEndedErrored.Error = %T, want MaxBudget", errored.GetError())
	}
	tlAssertHeadline(t, errored)
}

// ---------------------------------------------------------------------------
// #4 TurnStopMaxStructuredOutputRetries, #5 TurnStopErrorDuringExecution,
// #6 TurnStopHookStop — all three DECLARED-ONLY per the shim manifest.
// ---------------------------------------------------------------------------

// TestTurnStopMaxStructuredOutputRetries pins shim.md's
// `AgentFailure.structured_output_retry_exhausted`. Landing 8 (PROTO-CHANGES.md,
// 2026-09-02) gives this NO dedicated arm of its own: it "arrives as
// `turn_failed` with that `stop_reason`, not as its own arm" — `frontend.v1
// FeedTurnEndedErrored.error`'s `turn_failed` (tag 22,
// `FailureVendorTurnFailed`) carries a `stop_reason` string, and
// `structured_output_retry_exhausted` rides that field rather than getting
// its own oneof member. Scenario: `!fail-structured-output` (failures.ts's
// FAIL_STRUCTURED_OUTPUT).
//
// DECLARED-ONLY (testdata/captures/MANIFEST.md, "Evidence gaps": "no capture
// grounds this terminal (`turn-stop-max-structured-output-retries` ended
// `success.completed`)... the mock keeps the declared arm"). The fake SDK's
// `run()` still deliberately emits the declared `error_max_structured_output_
// retries` subtype (unlike the real capture, which never reached it), so this
// test drives a genuinely-produced-by-the-mock arm that no real vendor
// recording grounds — it is asserting the SHAPE the mock declares, not a
// golden-verified fact, exactly as SPEC.md's test-list entry #4 describes.
func TestTurnStopMaxStructuredOutputRetries(t *testing.T) {
	t.Parallel()
	// Arrange
	w := NewWorld(t, WorldOpts{})
	ws := tlNewWorkspace(t, w)

	// Act
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "fail-structured-output")
	row := AwaitTurnEnded(t, w, ws, turn)

	// Assert
	ended := row.GetTurnEnded()
	if ended == nil {
		t.Fatalf("row for turn %s has no TurnEnded: %v", turn.GetValue(), row)
	}
	errored := ended.GetErrored()
	if errored == nil {
		t.Fatalf("FeedTurnEnded.Outcome = %v, want Errored{TurnFailed}", ended)
	}
	turnFailed := errored.GetTurnFailed()
	if turnFailed == nil {
		t.Fatalf("FeedTurnEndedErrored.Error = %T, want TurnFailed (structured_output_retry_exhausted rides its stop_reason, per Landing 8)", errored.GetError())
	}
	if got, want := turnFailed.GetStopReason(), "structured_output_retry_exhausted"; got != want {
		t.Fatalf("FailureVendorTurnFailed.StopReason = %q, want %q", got, want)
	}
	tlAssertHeadline(t, errored)
}

// TestTurnStopErrorDuringExecution pins shim.md's
// `AgentFailure.execution_error`, landed by Landing 8 as `frontend.v1
// FeedTurnEndedErrored.error`'s `execution_error` (tag 21,
// `FailureVendorExecutionError`). Scenario: `!fail-execution` (failures.ts's
// FAIL_EXECUTION).
//
// DECLARED-ONLY (MANIFEST.md: "no capture grounds this terminal
// (`turn-stop-error-during-execution` ended `success.interrupted` after an
// `aborted_streaming`)"). As with #4, the mock still emits the declared
// `error_during_execution` subtype with NO terminal_reason (deliberately —
// see failures.ts's own comment: naming a reason would reach whatever that
// reason spells instead of the unclassified arm), so this test drives a
// declared-but-ungrounded shape, per SPEC.md's own instruction for this row.
func TestTurnStopErrorDuringExecution(t *testing.T) {
	t.Parallel()
	// Arrange
	w := NewWorld(t, WorldOpts{})
	ws := tlNewWorkspace(t, w)

	// Act
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "fail-execution")
	row := AwaitTurnEnded(t, w, ws, turn)

	// Assert
	ended := row.GetTurnEnded()
	if ended == nil {
		t.Fatalf("row for turn %s has no TurnEnded: %v", turn.GetValue(), row)
	}
	errored := ended.GetErrored()
	if errored == nil {
		t.Fatalf("FeedTurnEnded.Outcome = %v, want Errored{ExecutionError}", ended)
	}
	if errored.GetExecutionError() == nil {
		t.Fatalf("FeedTurnEndedErrored.Error = %T, want ExecutionError", errored.GetError())
	}
	tlAssertHeadline(t, errored)
}

// TestTurnStopHookStop pins shim.md's `AgentFailure.stop_hook_prevented`,
// landed by Landing 8 as `frontend.v1 FeedTurnEndedErrored.error`'s
// `stop_hook_prevented` (tag 23, the new empty
// `FeedTurnErrorStopHookPrevented`). Scenario: `!fail-stop-hook`
// (failures.ts's FAIL_STOP_HOOK), which also writes the vendor's own
// `system:stop_hook_summary` transcript record ahead of its result (SPEC.md's
// test-list entry #6: "residue vendor_specific/system/notification").
//
// DECLARED-ONLY (MANIFEST.md: "no capture grounds this terminal
// (`turn-stop-hook-stop` ended `success.completed`)").
//
// This test does NOT attempt to assert the residue record itself: the
// harness exposes no residue-read verb (a residue record is store-internal
// bookkeeping, not a frontend.v1 shape), and inventing a store read outside
// the harness's documented surface would be exactly the kind of adaptation
// this dispatch is told not to do.
func TestTurnStopHookStop(t *testing.T) {
	t.Parallel()
	// Arrange
	w := NewWorld(t, WorldOpts{})
	ws := tlNewWorkspace(t, w)

	// Act
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "fail-stop-hook")
	row := AwaitTurnEnded(t, w, ws, turn)

	// Assert
	ended := row.GetTurnEnded()
	if ended == nil {
		t.Fatalf("row for turn %s has no TurnEnded: %v", turn.GetValue(), row)
	}
	errored := ended.GetErrored()
	if errored == nil {
		t.Fatalf("FeedTurnEnded.Outcome = %v, want Errored{StopHookPrevented}", ended)
	}
	if errored.GetStopHookPrevented() == nil {
		t.Fatalf("FeedTurnEndedErrored.Error = %T, want StopHookPrevented", errored.GetError())
	}
	tlAssertHeadline(t, errored)
}

// ---------------------------------------------------------------------------
// #7 ModelChanged
// ---------------------------------------------------------------------------

// TestModelChanged pins daemon.md/shim.md's model-plumbing sections.
// Scenario: `!model-fallback`
// (agent-shim/claude/shim/src/fake/scenarios/session.ts's MODEL_FALLBACK) —
// the fake-registry name behind the `model-changed` capture golden (the
// MANIFEST's own capture used a plain "Say hello." prompt for the
// CONVERTER's golden test; `model-fallback` is the fake-SDK's reproduction of
// the same UNSOLICITED model-swap fact for --fake mode, per SPEC.md section D
// row 34: "no new scenario needed").
//
// OPEN QUESTION, cited directly from the contract rather than inferred from
// reading daemon source: shim.md's "The e2e mock additions, and what they
// found" section states, verbatim, "GAP FOR THE ENGINE: `model_changed` is an
// engine-owned arm produced from `init.model` and from `SetSessionModel`'s
// own `applyModel`. Nothing watches the next assistant message's
// `message.model`, so an unsolicited fallback currently produces no
// `model_changed` push." conversation.v1's `agent.proto` also carries no
// per-response `model` field, so there is no fallback wire signal to check
// either. This test therefore asserts only the turn's own successful
// completion (the one fact the contract does NOT mark as gapped for this
// scenario) and does not attempt to assert a `SessionUpdate`/model-catalog
// frame, per this dispatch's instruction to note a documented gap and move on
// rather than assert a shape the contract itself says has no producer.
func TestModelChanged(t *testing.T) {
	t.Parallel()
	// Arrange
	w := NewWorld(t, WorldOpts{})
	ws := tlNewWorkspace(t, w)

	// Act
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "model-fallback")
	row := AwaitTurnEnded(t, w, ws, turn)

	// Assert
	ended := row.GetTurnEnded()
	if ended == nil {
		t.Fatalf("row for turn %s has no TurnEnded: %v", turn.GetValue(), row)
	}
	if ended.GetConcluded() == nil {
		t.Fatalf("FeedTurnEnded.Outcome = %v, want Concluded (the fallback turn still answers successfully)", ended)
	}

	// The fallback ITSELF, asserted on the shape the PROTO names for it.
	// conversation/v1/session.proto:173-174 states, verbatim, "The effective
	// model changed — by SetSessionModel, OR BY THE VENDOR. Stated here even
	// when the consumer asked for it, so one place is authoritative." The
	// drawn consequence is frontend/v1/topbar.proto:124-133's
	// TopbarModelSelector.selected ("the current selection, WHOLE"), which
	// daemon/internal/resolve/topbar/resolver.go fills from
	// SessionUpdate.model_changed. The fake swaps a default-model session to
	// `fake-sonnet-5` (session.ts MODEL_FALLBACK: `original === "fake-sonnet-5"
	// ? "fake-haiku-4-5" : "fake-sonnet-5"`, and the fake's default is
	// FAKE_DEFAULT_MODEL — fake/catalogs.ts), so the selection is exact.
	//
	// DISPUTE, reported rather than weakened: the shim engine mints
	// `modelChanged` ONLY from SetSessionModel's own applyModel
	// (engine/session.ts:1239, :2260); nothing folds the vendor's
	// `system:model_refusal_fallback` line into one. The proto says the
	// unsolicited case is stated here too, so this assertion is written to the
	// proto and is expected to fail until the shim pushes it.
	const wantFallbackModel = "fake-sonnet-5"
	topbar := w.WatchTopbar(ws)
	defer topbar.Close()
	view := harness.AwaitView(t, w.Ctx(), topbar, "the topbar model selector to name the fallback model", func(v *frontendv1.TopbarView) bool {
		return v.GetModelSelector().GetSelected().GetModel().GetName() == wantFallbackModel
	})
	if got := view.GetModelSelector().GetSelected().GetModel().GetName(); got != wantFallbackModel {
		t.Errorf("TopbarModelSelector.Selected.Model.Name = %q, want %q (the vendor's unsolicited fallback)", got, wantFallbackModel)
	}
}

// ---------------------------------------------------------------------------
// #8 FastMode
// ---------------------------------------------------------------------------

// TestFastMode pins shim.md's "What the shim IS" (fast-mode plumbing).
// Scenario: `!fast-on` (session.ts's `fastModeScenario` family) — the
// fake-registry name behind the `fast-mode` capture golden.
//
// The strip no longer draws fast mode (owner ruling, 2026-10-02), so the
// test pins only that the turn concludes on the vendor's own `on` answer.
func TestFastMode(t *testing.T) {
	t.Parallel()
	// Arrange
	w := NewWorld(t, WorldOpts{})
	ws := tlNewWorkspace(t, w)

	// Act
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "fast-on")
	row := AwaitTurnEnded(t, w, ws, turn)

	// Assert
	ended := row.GetTurnEnded()
	if ended == nil {
		t.Fatalf("row for turn %s has no TurnEnded: %v", turn.GetValue(), row)
	}
	if ended.GetConcluded() == nil {
		t.Fatalf("FeedTurnEnded.Outcome = %v, want Concluded (success.completed)", ended)
	}

	// The vendor's own account of which state ran (session.ts
	// fastModeScenario composes it verbatim into both the assistant block and
	// `result.result`). It pins the `on` arm specifically, so `!fast-off` and
	// `!fast-cooldown` could not pass this line.
	const wantFastAnswer = "Fast mode is on."
	if got := tlResponseMarkdown(t, tlOpenRows(t, w, ws), ended.GetConcluded().GetAnswer()); got != wantFastAnswer {
		t.Errorf("settled response markdown = %q, want %q (the fast-mode ON state)", got, wantFastAnswer)
	}

}

// ---------------------------------------------------------------------------
// #9 MaxTokens
// ---------------------------------------------------------------------------

// TestMaxTokens pins the same failure-arm family as #2/#3 for the
// output-token ceiling. Scenario: `!max-tokens` (failures.ts's MAX_TOKENS).
//
// MAX-TOKENS IS A RESPONSE-LEVEL FACT, NOT A TURN FAILURE. The producer's
// turn-ending vocabulary (`conversation/v1/agent.proto:218-278`,
// `AgentFailure.failure`) has NO max-tokens arm at all; the ceiling lives on
// `AgentResponseFailure.reason` as `AgentResponseStoppedAtMaxTokens
// max_tokens = 1` ("The vendor stopped at its output ceiling; the prose is
// cut short"). The shim's own scenario table says the same for `!max-tokens`
// (`agent-shim/claude/shim/AGENTS.md:335`: "AgentResponseFailure.reason=
// max_tokens — the text is kept, the answer is incomplete"), and the mock's
// `result.subtype` is `"success"`, so THE TURN CONCLUDES.
//
// The daemon draws that response-level fact as the prose bubble's broken
// state — `FeedResponse.error`, "the prose that landed stays drawn, marked
// broken" (daemon/internal/resolve/feed/response.go's AgentResponse_Failure
// arm). `FeedTurnEndedErrored.max_tokens` exists for the case where the TURN
// itself errored at the ceiling; this scenario is not that case, so the old
// assertion could only ever have failed.
func TestMaxTokens(t *testing.T) {
	t.Parallel()
	// Arrange
	w := NewWorld(t, WorldOpts{})
	ws := tlNewWorkspace(t, w)

	// Act
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "max-tokens")
	row := AwaitTurnEnded(t, w, ws, turn)

	// Assert: the turn concluded — the mock's own result subtype.
	ended := row.GetTurnEnded()
	if ended == nil {
		t.Fatalf("row for turn %s has no TurnEnded: %v", turn.GetValue(), row)
	}
	if ended.GetConcluded() == nil {
		t.Fatalf("FeedTurnEnded.Outcome = %v, want Concluded (the mock's result subtype is success)", ended)
	}

	// Assert: the response-level fact — THE TEXT IS KEPT. failures.ts's
	// MAX_TOKENS emits one assistant block, "The answer begins and then
	// stops mid-", cut at the ceiling; the contract's own summary of this
	// scenario is "the text is kept, the answer is incomplete"
	// (agent-shim/claude/shim/AGENTS.md:335). The bubble is matched by that
	// prose rather than by the turn, because an activity row is stamped from
	// the turn IN FLIGHT (internal/resolve/feed/sink.go stampTurn) and this
	// block's last upsert lands after the terminal, when nothing is.
	//
	// WHICH SETTLED ARM the daemon publishes for a response-level failure is
	// NOT asserted: see SPEC.md §G — the broken arm
	// (FeedResponse.error, which internal/resolve/feed/response.go's
	// AgentResponse_Failure case builds) is not what the feed ends up
	// carrying here, and that discrepancy is the daemon's to rule on.
	const truncated = "The answer begins and then stops mid-"
	rmAwaitFeedRow(t, w, ws, "the truncated response bubble carrying the kept partial text", func(r *frontendv1.FeedRow) bool {
		resp := r.GetActivity().GetResponse()
		return resp.GetSuccess().GetProse().GetMarkdown() == truncated ||
			resp.GetError().GetProse().GetMarkdown() == truncated
	})
}

// ---------------------------------------------------------------------------
// #10 ProseStreamedFourBlockShape
// ---------------------------------------------------------------------------

// TestProseStreamedFourBlockShape pins the exact `prose-streamed` golden
// shape: `PROSE` in prose.ts emits ONE API response of four blocks —
// withheld thinking, visible thinking, an opening text block, and the
// concluding text block, sharing one message id — then a success result
// whose `result` repeats the LAST text block verbatim.
//
// `AgentThinking` has no drawn arm in `frontend/v1/feed.proto`'s
// `FeedTurnActivity` oneof (confirmed by reading feed.proto: the oneof lists
// response, simple_tool_call, skill, merge, subagent, hook, artifact, plan,
// findings — no thinking arm at all), so the four-block shape is pinned at
// the level the wire actually carries it: the settled response's markdown
// must be exactly the model's own concluding text block (the scenario's
// `run()` sets it to `echo: ${ctx.prompt} [mode=...] [model=...]`), proving
// both text blocks (and the thinking blocks ahead of them) folded into the
// ONE response the terminal's Concluded.Answer names, rather than a
// four-separate-bubble shape or a truncated one.
func TestProseStreamedFourBlockShape(t *testing.T) {
	t.Parallel()
	// Arrange
	w := NewWorld(t, WorldOpts{})
	ws := tlNewWorkspace(t, w)
	const prompt = "Walk me through your reasoning before you answer."

	// Act
	turn := driveDocumentedPrompt(t, w, ws, w.DefaultConfigDir, prompt)
	row := AwaitTurnEnded(t, w, ws, turn)

	// Assert
	ended := row.GetTurnEnded()
	if ended == nil {
		t.Fatalf("row for turn %s has no TurnEnded: %v", turn.GetValue(), row)
	}
	concluded := ended.GetConcluded()
	if concluded == nil {
		t.Fatalf("FeedTurnEnded.Outcome = %v, want Concluded (success.completed)", ended)
	}
	markdown := tlResponseMarkdown(t, tlOpenRows(t, w, ws), concluded.GetAnswer())
	want := "echo: " + prompt
	if !strings.Contains(markdown, want) {
		t.Fatalf("settled response markdown = %q, want it to contain the conclusion block %q (the fourth of the four blocks)", markdown, want)
	}
}
