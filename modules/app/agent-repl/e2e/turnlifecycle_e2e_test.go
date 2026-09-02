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

// tlLogTerminal records the actual FeedTurnEnded outcome arm this run
// observed, for the tests below whose SPEC.md-described terminal shape has no
// currently-wired proto path (see each such test's own header comment) — a
// human reading `go test -v` output can see the real shape without this file
// asserting one it cannot ground in the contract.
func tlLogTerminal(t *testing.T, ended *frontendv1.FeedTurnEnded) {
	t.Helper()
	switch {
	case ended.GetConcluded() != nil:
		t.Logf("turn ended: Concluded (answer=%v)", ended.GetConcluded().GetAnswer())
	case ended.GetErrored() != nil:
		t.Logf("turn ended: Errored (arm=%T, headline=%q)", ended.GetErrored().GetError(), ended.GetErrored().GetHeadline().GetText())
	case ended.GetInterrupted() != nil:
		t.Logf("turn ended: Interrupted")
	default:
		t.Fatalf("FeedTurnEnded has no outcome set at all: %v", ended)
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
// "max_turns"`). Scenario: `!fail-max-turns`
// (agent-shim/claude/shim/src/fake/scenarios/failures.ts's FAIL_MAX_TURNS) —
// the fake-registry name behind the `turn-stop-max-turns` capture golden
// named in SPEC.md's test list and MANIFEST.md's table (confirmed by reading
// both scenario source and the manifest: the capture's own recorded prompt
// was free prose, but the fake SDK reproduces the SAME arm pairing under this
// `!name`).
//
// OPEN QUESTION (not guessed around — reading only proto/src): this test does
// NOT assert a `FeedTurnEndedErrored` arm for max_turns. Confirmed by
// grep: `frontend/v1/feed.proto`'s `FeedTurnEndedErrored.error` oneof carries
// only the twelve `api_request_failed` sub-arms (rate_limited ..
// max_output_tokens); `frontend/v1/failure.proto` separately declares
// `FailureVendorMaxTurns` with a header comment stating it is meant to be
// "that entry's OWN `error` arm in feed.proto, which imports the evidence
// message directly" — but feed.proto does not reference
// `FailureVendorMaxTurns` anywhere (confirmed by grep), nor is it wired into
// `FailureKind`'s oneof. So no proto-typed path currently carries this fact
// to a frontend watch stream. This test therefore asserts only the
// structurally-guaranteed fact — the turn reaches SOME terminal, per shim.md
// "every bounded stream concludes with a TERMINAL FRAME" — and logs the
// observed outcome arm for whoever resolves this gap.
func TestTurnStopMaxTurns(t *testing.T) {
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
	tlLogTerminal(t, ended)
	if ended.GetInterrupted() != nil {
		t.Fatalf("FeedTurnEnded.Outcome = Interrupted, want a vendor-stop terminal (max_turns is a limit reached, never a user interrupt)")
	}
}

// TestTurnStopMaxBudgetUsd is TestTurnStopMaxTurns's sibling for the budget
// ceiling: shim.md's `AgentFailure.budget_exhausted`,
// `terminal_reason: "budget_exhausted"`. Scenario: `!fail-budget`
// (failures.ts's FAIL_BUDGET) — the `turn-stop-max-budget-usd` golden's
// fake-registry name. Same OPEN QUESTION as TestTurnStopMaxTurns applies
// verbatim: `FailureVendorMaxBudget` is declared in failure.proto for exactly
// this fact and is not wired into any oneof a frontend watch stream carries.
func TestTurnStopMaxBudgetUsd(t *testing.T) {
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
	tlLogTerminal(t, ended)
	if ended.GetInterrupted() != nil {
		t.Fatalf("FeedTurnEnded.Outcome = Interrupted, want a vendor-stop terminal (budget_exhausted is a limit reached, never a user interrupt)")
	}
}

// ---------------------------------------------------------------------------
// #4 TurnStopMaxStructuredOutputRetries, #5 TurnStopErrorDuringExecution,
// #6 TurnStopHookStop — all three DECLARED-ONLY per the shim manifest.
// ---------------------------------------------------------------------------

// TestTurnStopMaxStructuredOutputRetries pins shim.md's
// `AgentFailure.structured_output_retry_exhausted`. Scenario:
// `!fail-structured-output` (failures.ts's FAIL_STRUCTURED_OUTPUT).
//
// DECLARED-ONLY (testdata/captures/MANIFEST.md, "Evidence gaps": "no capture
// grounds this terminal (`turn-stop-max-structured-output-retries` ended
// `success.completed`)... the mock keeps the declared arm"). The fake SDK's
// `run()` still deliberately emits the declared `error_max_structured_output_
// retries` subtype (unlike the real capture, which never reached it), so
// this test drives a genuinely-produced-by-the-mock arm that no real vendor
// recording grounds — it is asserting the SHAPE the mock declares, not a
// golden-verified fact, exactly as SPEC.md's test-list entry #4 describes.
//
// Same OPEN QUESTION as the max-turns/budget pair: no `FeedTurnEndedErrored`
// arm or wired `FailureKind` arm corresponds to
// `structured_output_retry_exhausted` either, so this test asserts only the
// structural terminal fact and logs the observed arm.
func TestTurnStopMaxStructuredOutputRetries(t *testing.T) {
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
	tlLogTerminal(t, ended)
	if ended.GetInterrupted() != nil {
		t.Fatalf("FeedTurnEnded.Outcome = Interrupted, want a vendor-stop terminal")
	}
}

// TestTurnStopErrorDuringExecution pins shim.md's
// `AgentFailure.execution_error`. Scenario: `!fail-execution`
// (failures.ts's FAIL_EXECUTION).
//
// DECLARED-ONLY (MANIFEST.md: "no capture grounds this terminal
// (`turn-stop-error-during-execution` ended `success.interrupted` after an
// `aborted_streaming`)"). As with #4, the mock still emits the declared
// `error_during_execution` subtype with NO terminal_reason (deliberately —
// see failures.ts's own comment: naming a reason would reach whatever that
// reason spells instead of the unclassified arm), so this test drives a
// declared-but-ungrounded shape, per SPEC.md's own instruction for this row.
//
// Same OPEN QUESTION: `FailureVendorExecutionError` is declared in
// failure.proto for exactly this fact and is unwired, same as MaxTurns/
// MaxBudget above.
func TestTurnStopErrorDuringExecution(t *testing.T) {
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
	tlLogTerminal(t, ended)
	if ended.GetInterrupted() != nil {
		t.Fatalf("FeedTurnEnded.Outcome = Interrupted, want a vendor-stop terminal")
	}
}

// TestTurnStopHookStop pins shim.md's `AgentFailure.stop_hook_prevented`.
// Scenario: `!fail-stop-hook` (failures.ts's FAIL_STOP_HOOK), which also
// writes the vendor's own `system:stop_hook_summary` transcript record ahead
// of its result (SPEC.md's test-list entry #6: "residue
// vendor_specific/system/notification").
//
// DECLARED-ONLY (MANIFEST.md: "no capture grounds this terminal
// (`turn-stop-hook-stop` ended `success.completed`)").
//
// This test asserts only the terminal fact (same OPEN QUESTION as #2-#5:
// `FailureVendorTurnFailed`/no wired arm covers `stop_hook_prevented`
// either). It does NOT attempt to assert the residue record itself: the
// harness exposes no residue-read verb (a residue record is store-internal
// bookkeeping, not a frontend.v1 shape), and inventing a store read outside
// the harness's documented surface would be exactly the kind of adaptation
// this dispatch is told not to do.
func TestTurnStopHookStop(t *testing.T) {
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
	tlLogTerminal(t, ended)
	if ended.GetInterrupted() != nil {
		t.Fatalf("FeedTurnEnded.Outcome = Interrupted, want a vendor-stop terminal")
	}
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
}

// ---------------------------------------------------------------------------
// #8 FastMode
// ---------------------------------------------------------------------------

// TestFastMode pins shim.md's "What the shim IS" (fast-mode plumbing).
// Scenario: `!fast-on` (session.ts's `fastModeScenario` family) — the
// fake-registry name behind the `fast-mode` capture golden.
//
// OPEN QUESTION, again cited directly from the contract: shim.md's same "The
// e2e mock additions" section states, verbatim, "GAP FOR THE ENGINE:
// `fastMode` is in the engine's OWNED_ARMS, so the fold-produced `fast_mode`
// update from `init` is DROPPED, and nothing in the engine pushes one.
// WatchSession's `fast_mode` arm has no producer yet." `fast_mode` lives only
// in conversation.v1's `session.proto` (`SessionUpdate.fast_mode`), which the
// daemon consumes over shim.v1 but is not itself a frontend.v1 shape; with no
// SessionUpdate push, there is no discoverable frontend surface for this
// fact today. This test therefore asserts only the turn's own successful
// completion (SPEC.md's own description: "turn terminal success.completed"),
// and does not attempt the "fast-mode marker on the turn's record" half of
// SPEC.md's test-list entry #8, per the same documented-gap rule as #7.
func TestFastMode(t *testing.T) {
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
}

// ---------------------------------------------------------------------------
// #9 MaxTokens
// ---------------------------------------------------------------------------

// TestMaxTokens pins the same failure-arm family as #2/#3 for the
// output-token ceiling. Scenario: `!max-tokens` (failures.ts's MAX_TOKENS).
//
// Unlike max-turns/budget/structured-output/execution-error, this one HAS a
// grounded, wired path: `frontend/v1/feed.proto`'s `FeedTurnEndedErrored.
// error` oneof declares `FeedTurnErrorMaxTokens max_tokens = 11` with the
// doc comment "The vendor stopped at its output ceiling; whatever prose
// landed may be cut short" — matching the scenario's own `result` exactly
// (`stopReason: "max_tokens"`, partial text kept), even though the mock's
// own `result.subtype` is `"success"` (max-tokens has no dedicated
// `sdk.d.ts` error subtype; the ceiling is carried entirely by `stop_reason`,
// per failures.ts's own comment on this scenario). So this test asserts the
// specific Errored{MaxTokens} arm, not just the structural terminal fact.
func TestMaxTokens(t *testing.T) {
	// Arrange
	w := NewWorld(t, WorldOpts{})
	ws := tlNewWorkspace(t, w)

	// Act
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "max-tokens")
	row := AwaitTurnEnded(t, w, ws, turn)

	// Assert
	ended := row.GetTurnEnded()
	if ended == nil {
		t.Fatalf("row for turn %s has no TurnEnded: %v", turn.GetValue(), row)
	}
	errored := ended.GetErrored()
	if errored == nil {
		t.Fatalf("FeedTurnEnded.Outcome = %v, want Errored{MaxTokens}", ended)
	}
	if errored.GetMaxTokens() == nil {
		t.Fatalf("FeedTurnEndedErrored.Error = %T, want MaxTokens", errored.GetError())
	}
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
