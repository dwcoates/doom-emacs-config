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
)

// ---------------------------------------------------------------------------
// Local helpers. Named with a tl* prefix, not a generic name, since every
// area file shares this package and this suite's fanout dispatches one
// writer per file with no cross-file coordination on identifier names.
// ---------------------------------------------------------------------------

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
//
// Seam: prompt-round-trip
func TestTurnStartToCompletion(t *testing.T) {
	t.Parallel()
	// Arrange
	w := NewWorld(t, WorldOpts{})
	ws := w.NewWorkspace(t)

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
