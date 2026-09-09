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

	"claude-repld/integration/harness"
)

// newFileToolsWorkspace builds one World and registers a fresh fake
// repository (harness.NewRepo — the scripted fake git, never the real
// binary) as its workspace.
func newFileToolsWorkspace(t *testing.T) (*World, *workspacev1.WorkspaceRef) {
	t.Helper()
	w := NewWorld(t, WorldOpts{})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)
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

// TestEdit drives the `edit` fake scenario (files.ts EDIT) — golden #18
// "edit" (SPEC.md §C #66). Zero support in the old fake-query.ts per the
// shim coverage report; this is the round trip's first real e2e coverage.
func TestEdit(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws := newFileToolsWorkspace(t)

	// Act
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "edit")

	// Assert
	row := awaitFeedRow(t, w, ws, "the edit's settled tool card", toolCallSettled(turn, "Edit"))
	returned := requireSucceeded(t, row, "Edit")
	diff := returned.GetDiff()
	if diff == nil {
		t.Fatalf("Edit tool call returned = %v, want a diff output form", returned)
	}
	if len(diff.GetLines()) == 0 {
		t.Fatal("Edit tool call's diff output carries no lines, want the change's hunk")
	}
}

// TestGlob drives the `glob` fake scenario (files.ts GLOB, golden #21) —
// SPEC.md §C #67. The fixture's totalMatches (7) exceeds its returned paths
// (2) with countIsComplete=true, so AgentGlobSuccess carries the EXACT
// omitted arm (AgentGlobOmittedExact) — this asserts that arm reaches the
// frontend's composed FeedToolCallLinesOutput.omitted.
func TestGlob(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws := newFileToolsWorkspace(t)

	// Act
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "glob")

	// Assert
	row := awaitFeedRow(t, w, ws, "the glob's settled tool card", toolCallSettled(turn, "Glob"))
	returned := requireSucceeded(t, row, "Glob")
	lines := returned.GetLines()
	if lines == nil {
		t.Fatalf("Glob tool call returned = %v, want a lines output form", returned)
	}
	if got := len(lines.GetLines()); got != 2 {
		t.Fatalf("Glob tool call's lines output has %d lines, want the two matched paths", got)
	}
	if lines.GetOmitted() == nil {
		t.Fatal("Glob tool call's lines output carries no omitted floor, want one composed (7 total, 2 shown)")
	}
}

// TestGrepContentFilesCount drives the three grep-mode fake scenarios
// (files.ts GREP_CONTENT/GREP_FILES/GREP_COUNT, goldens #22 in D-table's
// bundled sense) as one table-driven test — SPEC.md §C #68
// GrepContentFilesCount. AgentGrepSuccess.matches is a three-way oneof
// (content/files/count); each row below drives one arm.
func TestGrepContentFilesCount(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws := newFileToolsWorkspace(t)

	cases := []struct {
		name         string
		scenario     string
		wantOmitted  bool // AgentGrepContentPartial/AgentGrepFilesPartial present
		wantLines    bool // rendered as FeedToolCallLinesOutput
		wantText     bool // rendered as FeedToolCallTextOutput ("a count, a bare summary")
		minLineCount int
	}{
		// grep-content: 2 lines returned, totalLines 5 > numLines 2 — partial.
		{name: "content", scenario: "grep-content", wantOmitted: true, wantLines: true, minLineCount: 1},
		// grep-files: 1 file returned, totalFiles 1 == numFiles 1 — all, no omission.
		{name: "files", scenario: "grep-files", wantOmitted: false, wantLines: true, minLineCount: 1},
		// grep-count: no line/file list at all, only a total — the frontend's
		// own worked example for its plain-text form ("a count, a bare
		// summary" — feed.proto's FeedToolCallTextOutput doc comment).
		{name: "count", scenario: "grep-count", wantText: true},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, tc.scenario)

			// Assert
			row := awaitFeedRow(t, w, ws, "the grep ("+tc.name+") settled tool card", toolCallSettled(turn, "Grep"))
			returned := requireSucceeded(t, row, "Grep")

			if tc.wantLines {
				lines := returned.GetLines()
				if lines == nil {
					t.Fatalf("grep-%s tool call returned = %v, want a lines output form", tc.name, returned)
				}
				if len(lines.GetLines()) < tc.minLineCount {
					t.Fatalf("grep-%s tool call's lines output has %d lines, want at least %d", tc.name, len(lines.GetLines()), tc.minLineCount)
				}
				if tc.wantOmitted && lines.GetOmitted() == nil {
					t.Fatalf("grep-%s tool call's lines output carries no omitted floor, want one composed", tc.name)
				}
				if !tc.wantOmitted && lines.GetOmitted() != nil {
					t.Fatalf("grep-%s tool call's lines output carries an omitted floor %v, want none (every match is present)", tc.name, lines.GetOmitted())
				}
			}
			if tc.wantText {
				text := returned.GetText()
				if text == nil {
					t.Fatalf("grep-%s tool call returned = %v, want a text output form", tc.name, returned)
				}
				if text.GetText() == "" {
					t.Fatalf("grep-%s tool call's text output is empty, want the composed count", tc.name)
				}
			}
		})
	}
}

// TestReadWholeHeadRange drives the three read-extent fake scenarios
// (files.ts READ_WHOLE/READ_HEAD/READ_RANGE) as one table-driven test —
// SPEC.md §C #69 ReadWholeHeadRange. AgentReadSuccess.extent is a three-way
// oneof (whole/head/range); each row below drives one arm.
//
// FeedToolCallCodeOutput.omitted is present "iff truncated", so only a
// WHOLE read carries none: it is the one extent that says "nothing further
// can be fetched" (agent_activity.proto, AgentReadWhole). Both a head and a
// range are short of the file, and both must say so. For the range,
// agent_activity.proto pins the sentence and its purpose on
// AgentReadRange.total_lines: "What the whole file holds, so a consumer
// states \"lines 400-499 of 4,312\" without arithmetic and without a second
// request" — the omitted line is the only element of
// FeedToolCallCodeOutput that can carry that statement, so a range read
// carries one. feed.proto's "always a HEAD of the file, never a middle
// slice" qualifies the CODE the spans paint (a highlighter needs context
// from offset zero, the same rationale AgentReadHead.contents gives) and
// predates the range arm, which was added at tag 10 after the extents at
// tags 2-8; it does not license dropping a middle slice's extent line.
func TestReadWholeHeadRange(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws := newFileToolsWorkspace(t)

	cases := []struct {
		name        string
		scenario    string
		wantOmitted bool
	}{
		{name: "whole", scenario: "read", wantOmitted: false},
		{name: "head", scenario: "read-head", wantOmitted: true},
		{name: "range", scenario: "read-range", wantOmitted: true},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, tc.scenario)

			// Assert
			row := awaitFeedRow(t, w, ws, "the read ("+tc.name+") settled tool card", toolCallSettled(turn, "Read"))
			returned := requireSucceeded(t, row, "Read")
			code := returned.GetCode()
			if code == nil {
				t.Fatalf("read-%s tool call returned = %v, want a code output form", tc.name, returned)
			}
			if len(code.GetSpans()) == 0 {
				t.Fatalf("read-%s tool call's code output carries no paint spans, want at least one", tc.name)
			}
			if tc.wantOmitted && code.GetOmitted() == nil {
				t.Fatalf("read-%s tool call's code output carries no omitted line, want one composed for the cut", tc.name)
			}
			if !tc.wantOmitted && code.GetOmitted() != nil {
				t.Fatalf("read-%s tool call's code output carries an omitted line %v, want none", tc.name, code.GetOmitted())
			}
		})
	}
}

// TestWriteCreatedAndUpdated drives the two write-outcome fake scenarios
// (files.ts WRITE_CREATE/WRITE_UPDATE) as one table-driven test — SPEC.md
// §C #70 WriteCreatedAndUpdated. AgentWriteSuccess.outcome is a two-way
// oneof (created/updated); each row below drives one arm. The frontend's
// FeedToolCallDiffOutput carries no typed created-vs-updated arm of its own
// (that distinction lives only in the daemon's composed input-line
// phrasing, which this test does not pin) — what this test proves is that
// BOTH oneof arms reach the daemon and render as a non-empty diff, per
// agent_activity.proto's comment on AgentWriteSuccess.patch: "For a
// creation every line is an addition. The producer diffs after the fact
// precisely so a card can show the CHANGE" — i.e. a created file's patch is
// populated too, never empty, even though the WRITE_CREATE fixture's own
// vendor-reported structuredPatch is [].
func TestWriteCreatedAndUpdated(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws := newFileToolsWorkspace(t)

	cases := []struct {
		name     string
		scenario string
	}{
		{name: "created", scenario: "write-create"},
		{name: "updated", scenario: "write-update"},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, tc.scenario)

			// Assert
			row := awaitFeedRow(t, w, ws, "the write ("+tc.name+") settled tool card", toolCallSettled(turn, "Write"))
			returned := requireSucceeded(t, row, "Write")
			diff := returned.GetDiff()
			if diff == nil {
				t.Fatalf("write-%s tool call returned = %v, want a diff output form", tc.name, returned)
			}
			if len(diff.GetLines()) == 0 {
				t.Fatalf("write-%s tool call's diff output carries no lines, want the write's hunk", tc.name)
			}
		})
	}
}

// TestIdeDiagnosticsAfterEdit drives the `ide-diagnostics` fake scenario
// (files.ts IDE_DIAGNOSTICS) — golden "ide-diagnostics-after-edit" (SPEC.md
// §C #71). The vendor's diagnostics attachment carries no tool-call id and
// joins to the preceding edit by ADJACENCY (agent_activity.proto's producer
// note on AgentDiagnosticsReport); it arrives as a SECOND frame on the same
// edit unit, after the edit's own result, so this asserts the settled row's
// diagnostics arm specifically rather than merely the edit's own success.
func TestIdeDiagnosticsAfterEdit(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws := newFileToolsWorkspace(t)

	// Act
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "ide-diagnostics")

	// Assert
	row := awaitFeedRow(t, w, ws, "the edit's diagnostics-carrying tool card", func(r *frontendv1.FeedRow) bool {
		call := r.GetActivity().GetSimpleToolCall()
		return r.GetTurn().GetValue() == turn.GetValue() &&
			call.GetName().GetText() == "Edit" &&
			call.GetReturned().GetDiagnostics() != nil
	})
	returned := requireSucceeded(t, row, "Edit")
	if returned.GetDiff() == nil {
		t.Fatal("Edit tool call returned = want a diff output form alongside its diagnostics")
	}
	diagnostics := returned.GetDiagnostics()
	if len(diagnostics.GetLines()) == 0 {
		t.Fatal("the edit's diagnostics carry no composed lines, want the vendor's typescript error")
	}
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

// TestReadTruncatedStatesTheCut drives the `read-truncated` fake scenario
// (files.ts READ_TRUNCATED) — the read that came back short of the file.
//
// The DRAWN contract is FeedToolCallCodeOutput.omitted, "present iff
// truncated" (feed.proto), composed by the daemon as "showing N of M lines"
// (daemon/internal/resolve/feed/format.go formatShowingOf) from
// AgentReadHead.total_lines, which agent_activity.proto exists for: "What
// the whole file holds, so a consumer states 'showing 200 of 4,312' without
// arithmetic and without a second request." The fixture returns 2 of the
// file's 4 lines, so the composed line is pinned exactly.
//
// THE CUT ARM, resolved: the scenario declares "AgentReadSuccess.cut=
// token_cap", and the shim's converter reaches AgentReadCutAtTokenCap only
// when the vendor's toolUseResult sets `truncatedByTokenCap`
// (convert/tools/read.ts textExtent). The fixture now sets it, so the
// declared arm is the one produced. NOTHING IS ASSERTED ABOUT THE CUT HERE
// because the two cuts are indistinguishable downstream:
// FeedToolCallCodeOutput carries the composed omitted line and no cut arm of
// its own, so a token-capped read draws exactly what this test asserts. The
// cut itself is pinned at the converter (shim test/convert/tools/read.ts).
func TestReadTruncatedStatesTheCut(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws := newFileToolsWorkspace(t)

	// Act
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "read-truncated")

	// Assert
	row := awaitFeedRow(t, w, ws, "the truncated read's settled tool card", toolCallSettled(turn, "Read"))
	returned := requireSucceeded(t, row, "Read")
	code := returned.GetCode()
	if code == nil {
		t.Fatalf("read-truncated tool call returned = %v, want a code output form", returned)
	}
	if len(code.GetSpans()) == 0 {
		t.Fatal("read-truncated tool call's code output carries no paint spans, want the lines that did come back")
	}
	const want = "showing 2 of 4 lines"
	if got := code.GetOmitted().GetText(); got != want {
		t.Errorf("read-truncated tool call's omitted line = %q, want %q composed from the head's total_lines", got, want)
	}
}

// TestReadImageSettlesWithNoOutput drives the `read-image` fake scenario
// (files.ts READ_IMAGE: a Read of a png answered with an image content block
// and an image `toolUseResult` carrying dimensions).
//
// THE RULING: the contract has no arm for HOW MUCH of an image came back —
// agent_activity.proto's AgentReadSuccess.extent oneof carries whole/head/
// range and records that "Tags 4-8 are RETIRED: the image, pdf, notebook,
// split-to-directory and unchanged extents are deferred" — but it does have
// an arm for the read HAVING SETTLED. An AgentReadSuccess whose extent oneof
// is UNSET states the path and the settle instant and claims nothing about
// the contents, and the daemon already answers exactly that: resolve/feed/
// toolcall.go readForm returns no output form for an unset extent ("A read
// that came back with no extent has nothing to draw; the card still says it
// returned"), which applyReturnedForm renders as feed.proto's
// FeedToolCallNoOutput — "The call returned NOTHING to draw: the output
// section is omitted. An arm, never an empty text — presence, not a
// sentinel."
//
// So the drawn shape is a SETTLED card with the succeeded verdict and the
// `none` output form. A card that never settles would be the defect: the
// call did finish, and a perpetually-running card says otherwise.
func TestReadImageSettlesWithNoOutput(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws := newFileToolsWorkspace(t)

	// Act
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "read-image")

	// Assert
	row := awaitFeedRow(t, w, ws, "the image read's settled tool card", toolCallSettled(turn, "Read"))
	returned := requireSucceeded(t, row, "Read")
	if returned.GetNone() == nil {
		t.Errorf("the image read's output form = %v, want the `none` arm: AgentReadSuccess has no image extent this wave, so nothing is drawn below the divider",
			returned.GetForm())
	}
}
