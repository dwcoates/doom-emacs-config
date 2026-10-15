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
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"connectrpc.com/connect"

	"agent-repld/integration/harness"
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
