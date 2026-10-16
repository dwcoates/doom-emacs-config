// filetools_e2e_test.go — File tools area (SPEC.md §C "File tools", #66-71).
//
// CONTRACT GROUNDING:
//   - proto/src/conversation/v1/agent_activity.proto: AgentRead/AgentReadSuccess
//     (extent oneof whole/head/range — "the set arm IS whether more exists, so
//     a whole read carries no truncation vocabulary"), AgentWrite/AgentWriteSuccess
//     (outcome oneof created/updated, "the producer diffs after the fact...
//     For a creation every line is an addition"), AgentEdit/AgentEditSuccess,
//     AgentGrep/AgentGrepSuccess (matches oneof content/files/count),
//     AgentGlob/AgentGlobSuccess (extent oneof all/partial), and the shared
//     AgentDiagnosticsReport arm riding a write/edit's own oneof as a
//     post-terminal consequence.
//   - proto/src/frontend/v1/feed.proto: FeedSimpleToolCall / FeedToolCallReturned
//     — the ONE rendered shape every file tool answers through (form oneof
//     text/code/diff/lines/links/none; "Always a HEAD of the file, never a
//     middle slice" for FeedToolCallCodeOutput.omitted; "a count, a bare
//     summary" as FeedToolCallTextOutput's own worked example).
//   - agent-shim/claude/shim/src/fake/scenarios/files.ts (read directly in
//     this worktree, per SPEC.md's grounding discipline): the exact scenario
//     names and fixture shapes these tests drive.
//
// SPEC.md's D-table golden names for this area ("read-whole-head-range",
// "write-created-and-updated", "grep-content-files-count") are the MANIFEST's
// CAPTURE names, not `!name` prompt selectors — files.ts's own scenario names
// are split narrower, by extent/outcome/mode: read/read-head/read-range,
// write-create/write-update, grep-content/grep-files/grep-count. Each test
// below drives every fake scenario its golden capture bundles, as named
// table rows, rather than inventing a single nonexistent combined `!name`.
// None of the twelve scenarios this file drives (edit, ide-diagnostics,
// ide-diagnostics-write, glob,
// read, read-head, read-range, write-create, write-update, grep-content,
// grep-files, grep-count) carry an UNGROUNDED/INVENTED/DECLARED-ONLY mark in
// the shim's own MANIFEST.md — every fixture here is corpus-grounded or, for
// grep/glob (which have no corpus fixture), grounded in the DECLARED
// `GrepOutput`/`GlobOutput` shapes files.ts itself cites.
//
// This suite uses the SCRIPTED FAKE git the daemon harness installs
// (harness.NewRepo) and the fake-SDK vendor riding the real shim's --fake
// mode — no real git, no real vendor binary, no network. Every fact a test
// asserts comes from a named fake-SDK scenario driven through the real shim,
// per this package's grep gate.
package e2e

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
)

// newFileToolsWorkspace builds one World and registers a fresh fake
// repository (harness.NewRepo — the scripted fake git, never the real
// binary) as its workspace.
func newFileToolsWorkspace(t *testing.T) (*World, *workspacev1.WorkspaceRef) {
	t.Helper()
	w := NewWorld(t, WorldOpts{})
	ws := w.NewWorkspace(t)
	return w, ws
}

// toolCallSettled matches a SimpleToolCall row for the given turn and tool
// name whose outcome has reached Returned (succeeded or failed).
func toolCallSettled(turn *conversationv1.TurnId, toolName string) func(*frontendv1.FeedRow) bool {
	return func(row *frontendv1.FeedRow) bool {
		call := row.GetActivity().GetSimpleToolCall()
		return row.GetTurn().GetValue() == turn.GetValue() &&
			call.GetName().GetText() == toolName &&
			call.GetReturned() != nil
	}
}

// requireSucceeded fails the test if the row's tool call did not settle with
// the succeeded verdict.
func requireSucceeded(t *testing.T, row *frontendv1.FeedRow, toolName string) *frontendv1.FeedToolCallReturned {
	t.Helper()
	returned := row.GetActivity().GetSimpleToolCall().GetReturned()
	if returned.GetSucceeded() == nil {
		t.Fatalf("%s tool call returned = %v, want the succeeded verdict", toolName, returned)
	}
	if returned.GetFailed() != nil {
		t.Fatalf("%s tool call returned = %v, want no failed verdict alongside succeeded", toolName, returned)
	}
	return returned
}

// TestIdeDiagnosticsAfterWrite drives the `ide-diagnostics-write` fake
// scenario (files.ts IDE_DIAGNOSTICS_WRITE) — the WRITE sibling of
// TestIdeDiagnosticsAfterEdit above.
//
// It exists because the adjacency join has to know WHICH KIND of change it
// is joining to: a diagnostics attachment following a Write once landed on
// the edit arm, because the fold remembered that a change had happened and
// not that it was a write. That defect is unit-tested in the shim's fold and
// the daemon's resolver; this is the end-to-end statement of it, asserting
// the diagnostics hang off the tool card NAMED "Write" at the feed.
//
// GROUNDING: the scenario composes the real `Write` `type: "create"` result
// from testdata/captures/write-created-and-updated with the real harvested
// attachment in testdata/corpus/attachments/diagnostics.jsonl — the same two
// grades of evidence its edit sibling composes, and the same as every other
// scenario this file drives. No capture grounds a diagnostics attachment on
// any host (it requires a connected editor integration), which the shim's
// testdata/captures/MANIFEST.md states once for both siblings.
func TestIdeDiagnosticsAfterWrite(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws := newFileToolsWorkspace(t)
	// As above, transcript diagnostics and the live Write card may arrive in
	// either order; the resolver retains the early attachment for this card.

	// Act
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "ide-diagnostics-write")

	// Assert
	row := awaitFeedRow(t, w, ws, "the write's diagnostics-carrying tool card", func(r *frontendv1.FeedRow) bool {
		call := r.GetActivity().GetSimpleToolCall()
		return r.GetTurn().GetValue() == turn.GetValue() &&
			call.GetName().GetText() == "Write" &&
			call.GetReturned().GetDiagnostics() != nil
	})
	returned := requireSucceeded(t, row, "Write")
	diagnostics := returned.GetDiagnostics()
	if len(diagnostics.GetLines()) == 0 {
		t.Fatal("the write's diagnostics carry no composed lines, want the vendor's typescript error")
	}
}
