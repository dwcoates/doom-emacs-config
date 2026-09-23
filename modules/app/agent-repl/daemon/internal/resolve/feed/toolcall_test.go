package feed

import (
	"fmt"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

// ONE SHARED SHELL, per-tool specifics composed by the DAEMON: the client holds
// no per-tool knowledge, so every phrasing rule gets a case here.

// card returns the only tool card on the root feed.
func (h *harness) card() *frontendv1.FeedSimpleToolCall {
	h.t.Helper()
	return h.only(rootFeed()).GetActivity().GetSimpleToolCall()
}

// activityOf wraps one tool item as an activity.
func activityOf(unit string, item any) *conversationv1.AgentActivity {
	act := &conversationv1.AgentActivity{ActivityId: &conversationv1.AgentActivityId{Value: unit}}
	switch i := item.(type) {
	case *conversationv1.AgentRead:
		act.Item = &conversationv1.AgentActivity_Read{Read: i}
	case *conversationv1.AgentWrite:
		act.Item = &conversationv1.AgentActivity_Write{Write: i}
	case *conversationv1.AgentEdit:
		act.Item = &conversationv1.AgentActivity_Edit{Edit: i}
	case *conversationv1.AgentGrep:
		act.Item = &conversationv1.AgentActivity_Grep{Grep: i}
	case *conversationv1.AgentGlob:
		act.Item = &conversationv1.AgentActivity_Glob{Glob: i}
	case *conversationv1.AgentBash:
		act.Item = &conversationv1.AgentActivity_Bash{Bash: i}
	case *conversationv1.AgentWebFetch:
		act.Item = &conversationv1.AgentActivity_WebFetch{WebFetch: i}
	case *conversationv1.AgentWebSearch:
		act.Item = &conversationv1.AgentActivity_WebSearch{WebSearch: i}
	case *conversationv1.AgentUnmodeled:
		act.Item = &conversationv1.AgentActivity_Unmodeled{Unmodeled: i}
	}
	return act
}

// send pushes one activity through the sink.
func (h *harness) send(act *conversationv1.AgentActivity) {
	h.t.Helper()
	h.resolver.OnActivity(testWorkspace, mainAgent(), act, noAddress())
}

// ---- READ ----

func TestReadStartDrawsTheRunningCardWithThePathAsItsInput(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.send(activityOf("unit-1", &conversationv1.AgentRead{
		Result: &conversationv1.AgentRead_Start{Start: &conversationv1.AgentReadStart{
			Path:      &conversationv1.ReadPath{Path: "internal/feed/row.go"},
			StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
		}},
	}))

	// Assert.
	card := h.card()
	if card.GetName().GetText() != "Read" {
		t.Fatalf("name = %q, want Read", card.GetName().GetText())
	}
	if card.GetInput().GetText() != "internal/feed/row.go" {
		t.Fatalf("input = %q, want the path", card.GetInput().GetText())
	}
	if card.GetInput().GetPath() == nil {
		t.Fatalf("input form = %T, want the path form", card.GetInput().GetForm())
	}
	if card.GetRunning() == nil {
		t.Fatalf("outcome = %T, want running", card.GetOutcome())
	}
}

func TestAProgressBeatBecomesTheRunningCardsLastProgress(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.send(activityOf("unit-1", &conversationv1.AgentRead{
		Result: &conversationv1.AgentRead_Start{Start: &conversationv1.AgentReadStart{
			Path:      &conversationv1.ReadPath{Path: "a.go"},
			StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
		}},
	}))

	// Act.
	h.send(activityOf("unit-1", &conversationv1.AgentRead{
		Result: &conversationv1.AgentRead_Progress{
			Progress: &conversationv1.AgentToolCallProgress{LastProgressAtMs: 4_200},
		},
	}))

	// Assert: the daemon relays the observed beat; the client ticks locally.
	if got := h.card().GetRunning().GetLastProgress().GetAtMs(); got != 4_200 {
		t.Fatalf("last_progress = %d, want 4200", got)
	}
}

func TestAWholeReadDrawsHighlightedCodeWithNoOmittedLine(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.send(activityOf("unit-1", &conversationv1.AgentRead{
		Result: &conversationv1.AgentRead_Success{Success: &conversationv1.AgentReadSuccess{
			Path:   &conversationv1.ReadPath{Path: "row.go"},
			Extent: &conversationv1.AgentReadSuccess_Whole{Whole: &conversationv1.AgentReadWhole{Contents: "package feed\n"}},
		}},
	}))

	// Assert: painted spans, the grammar chosen from the path, no truncation.
	code := h.card().GetReturned().GetCode()
	if code == nil || len(code.GetSpans()) != 1 || code.GetSpans()[0].GetPaintClass() != "keyword" {
		t.Fatalf("code = %+v, want the painter's spans", code)
	}
	if h.painter.lastLanguage != "go" {
		t.Fatalf("language = %q, want go", h.painter.lastLanguage)
	}
	if code.GetOmitted() != nil {
		t.Fatalf("omitted = %+v, want unset for a whole read", code.GetOmitted())
	}
}

func TestAHeadReadStatesTheCut(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.send(activityOf("unit-1", &conversationv1.AgentRead{
		Result: &conversationv1.AgentRead_Success{Success: &conversationv1.AgentReadSuccess{
			Path: &conversationv1.ReadPath{Path: "row.go"},
			Extent: &conversationv1.AgentReadSuccess_Head{Head: &conversationv1.AgentReadHead{
				Contents:   "a\nb\n",
				TotalLines: 4_312,
				Cut: &conversationv1.AgentReadHead_LineCap{
					LineCap: &conversationv1.AgentReadCutAtLineCap{},
				},
			}},
		}},
	}))

	// Assert.
	if got := h.card().GetReturned().GetCode().GetOmitted().GetText(); got != "showing 2 of 4,312 lines" {
		t.Fatalf("omitted = %q", got)
	}
}

func TestAnOffsetReadStatesTheSliceItDrew(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.send(activityOf("unit-1", &conversationv1.AgentRead{
		Result: &conversationv1.AgentRead_Success{Success: &conversationv1.AgentReadSuccess{
			Path: &conversationv1.ReadPath{Path: "row.go"},
			Extent: &conversationv1.AgentReadSuccess_Range{Range: &conversationv1.AgentReadRange{
				Contents: "x\n", FirstLine: 400, LineCount: 100, TotalLines: 4_312,
			}},
		}},
	}))

	// Assert.
	if got := h.card().GetReturned().GetCode().GetOmitted().GetText(); got != "lines 400-499 of 4,312" {
		t.Fatalf("omitted = %q", got)
	}
}

func TestAPainterRefusalDrawsThePlainCodeAndWarns(t *testing.T) {
	// Arrange: the file the agent read is worth more than its coloring.
	h := newHarness(t)
	h.painter.err = fmt.Errorf("no such grammar")

	// Act.
	h.send(activityOf("unit-1", &conversationv1.AgentRead{
		Result: &conversationv1.AgentRead_Success{Success: &conversationv1.AgentReadSuccess{
			Path:   &conversationv1.ReadPath{Path: "row.go"},
			Extent: &conversationv1.AgentReadSuccess_Whole{Whole: &conversationv1.AgentReadWhole{Contents: "package feed"}},
		}},
	}))

	// Assert.
	spans := h.card().GetReturned().GetCode().GetSpans()
	if len(spans) != 1 || spans[0].GetText() != "package feed" || spans[0].GetPaintClass() != "" {
		t.Fatalf("spans = %+v, want one plain span", spans)
	}
	if !h.hasRecord("warn", "daemon.feed.highlight_failed") {
		t.Fatalf("records = %+v, want a WARN daemon.feed.highlight_failed", h.records())
	}
}

func TestAFailedReadDrawsItsErrorTextWithTheFailedBadge(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.send(activityOf("unit-1", &conversationv1.AgentRead{
		Result: &conversationv1.AgentRead_Failure{Failure: &conversationv1.AgentReadFailure{
			// A settled frame restates what its call named (the contract);
			// without it the frame is a producer fault and draws no row.
			Path: &conversationv1.ReadPath{Path: "/tmp/missing"},
			Error: &conversationv1.AgentToolFailure{
				Content: &conversationv1.ToolResultContent{Blocks: []*conversationv1.ToolResultContentBlock{{
					Block: &conversationv1.ToolResultContentBlock_Text{
						Text: &conversationv1.TextBlock{Text: "no such file"},
					},
				}}},
				SettledAt: &conversationv1.AgentActivitySettledAt{AtMs: 2_000},
			},
		}},
	}))

	// Assert.
	returned := h.card().GetReturned()
	if returned.GetFailed() == nil {
		t.Fatalf("verdict = %T, want failed", returned.GetVerdict())
	}
	if returned.GetText().GetText() != "no such file" {
		t.Fatalf("output = %q, want the error text", returned.GetText().GetText())
	}
}

// failedReadWith is a failed Read whose account is the given blocks.
func failedReadWith(blocks ...*conversationv1.ToolResultContentBlock) *conversationv1.AgentActivity {
	return activityOf("unit-1", &conversationv1.AgentRead{
		Result: &conversationv1.AgentRead_Failure{Failure: &conversationv1.AgentReadFailure{
			// A settled frame restates what its call named (the contract);
			// without it the frame is a producer fault and draws no row.
			Path: &conversationv1.ReadPath{Path: "/tmp/missing"},
			Error: &conversationv1.AgentToolFailure{
				Content:   &conversationv1.ToolResultContent{Blocks: blocks},
				SettledAt: &conversationv1.AgentActivitySettledAt{AtMs: 2_000},
			},
		}},
	})
}

// imageResultBlock is an image a tool returned.
func imageResultBlock() *conversationv1.ToolResultContentBlock {
	return &conversationv1.ToolResultContentBlock{
		Block: &conversationv1.ToolResultContentBlock_Image{Image: &conversationv1.ImageBlock{
			Location: &conversationv1.ImageBlock_Url{Url: &conversationv1.ImageBlockUrl{
				Url: "data:image/png;base64,aGk=",
			}},
			MediaType: "image/png",
		}},
	}
}

// textResultBlock is a word a tool returned.
func textResultBlock(text string) *conversationv1.ToolResultContentBlock {
	return &conversationv1.ToolResultContentBlock{
		Block: &conversationv1.ToolResultContentBlock_Text{Text: &conversationv1.TextBlock{Text: text}},
	}
}

func TestAWordlessToolFailureDrawsTheImageItAnsweredWith(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.send(failedReadWith(imageResultBlock()))

	// Assert: the image reaches the card through the feed's shared block.
	returned := h.card().GetReturned()
	if returned.GetImage().GetSrc() != "https://host/img" {
		t.Fatalf("form = %T, want the image the failure answered with", returned.GetForm())
	}
}

func TestAToolFailureWithWordsDrawsTheAccountRatherThanTheImage(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.send(failedReadWith(textResultBlock("no such file"), imageResultBlock()))

	// Assert: the sentence that explains the failure is what the reader came
	// for, and `form` holds one arm.
	if got := h.card().GetReturned().GetText().GetText(); got != "no such file" {
		t.Fatalf("output = %q, want the error text over the image", got)
	}
}

func TestAnUnresolvableToolFailureImageDrawsNoBodyAndIsWarned(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.resolver.deps.ResolveImage = func(*conversationv1.ImageBlock) (string, string, error) {
		return "", "", fmt.Errorf("the reference names nothing servable")
	}

	// Act.
	h.send(failedReadWith(imageResultBlock()))

	// Assert: no invented src, and the loss is loud.
	if h.card().GetReturned().GetNone() == nil {
		t.Fatalf("form = %T, want the none arm for an unresolvable image",
			h.card().GetReturned().GetForm())
	}
	if !h.hasRecord("warn", "daemon.feed.tool_failure_image_unresolved") {
		t.Fatalf("records = %+v, want a WARN daemon.feed.tool_failure_image_unresolved", h.records())
	}
}

// ---- WRITE and EDIT ----

func TestAWriteDrawsTheBarePathAsItsInputLine(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.send(activityOf("unit-1", &conversationv1.AgentWrite{
		Result: &conversationv1.AgentWrite_Success{Success: &conversationv1.AgentWriteSuccess{
			Path:    &conversationv1.ReadPath{Path: "new.go"},
			Outcome: &conversationv1.AgentWriteSuccess_Created{Created: &conversationv1.AgentWriteCreated{}},
			Patch:   []*conversationv1.FilePatchHunk{{Lines: []string{"+package feed"}}},
		}},
	}))

	// Assert.
	if got := h.card().GetInput().GetText(); got != "new.go" {
		t.Fatalf("input = %q, want the bare path", got)
	}
}

func TestDiffLinesCarryTheirKindAsTheArmAndNotAsAPrefix(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.send(activityOf("unit-1", &conversationv1.AgentEdit{
		Result: &conversationv1.AgentEdit_Success{Success: &conversationv1.AgentEditSuccess{
			Path: &conversationv1.ReadPath{Path: "row.go"},
			Patch: []*conversationv1.FilePatchHunk{{
				OldRange: &conversationv1.FilePatchHunkRange{Start: 3, Lines: 7},
				NewRange: &conversationv1.FilePatchHunkRange{Start: 3, Lines: 9},
				Lines:    []string{" context", "-old", "+new"},
			}},
		}},
	}))

	// Assert: a header composed from the ranges, then the three kinds, each
	// with its marker stripped.
	lines := h.card().GetReturned().GetDiff().GetLines()
	if len(lines) != 4 {
		t.Fatalf("diff lines = %d, want 4", len(lines))
	}
	if lines[0].GetHeader() == nil || lines[0].GetText() != "@@ -3,7 +3,9 @@" {
		t.Fatalf("header = %+v", lines[0])
	}
	if lines[1].GetContext() == nil || lines[1].GetText() != "context" {
		t.Fatalf("context line = %+v", lines[1])
	}
	if lines[2].GetRemoved() == nil || lines[2].GetText() != "old" {
		t.Fatalf("removed line = %+v", lines[2])
	}
	if lines[3].GetAdded() == nil || lines[3].GetText() != "new" {
		t.Fatalf("added line = %+v", lines[3])
	}
}

func TestDiagnosticsAmendTheSettledCardTheyFollow(t *testing.T) {
	// Arrange: a settled edit.
	h := newHarness(t)
	h.send(activityOf("unit-1", &conversationv1.AgentEdit{
		Result: &conversationv1.AgentEdit_Success{Success: &conversationv1.AgentEditSuccess{
			Path:  &conversationv1.ReadPath{Path: "render.ts"},
			Patch: []*conversationv1.FilePatchHunk{{Lines: []string{"+const x = 1"}}},
		}},
	}))

	// Act: the report arrives AFTER the terminal, by adjacency.
	h.send(activityOf("unit-1", &conversationv1.AgentEdit{
		Result: &conversationv1.AgentEdit_Diagnostics{Diagnostics: &conversationv1.AgentDiagnosticsReport{
			Files: []*conversationv1.AgentDiagnosticsFile{{
				Path: "render.ts",
				Diagnostics: []*conversationv1.AgentDiagnostic{{
					Severity:  conversationv1.AgentDiagnosticSeverity_AGENT_DIAGNOSTIC_SEVERITY_ERROR,
					Message:   "'x' is never used",
					StartLine: 213,
				}},
			}},
		}},
	}))

	// Assert: one card, amended — the vendor's zero-based line drawn one-based.
	lines := h.card().GetReturned().GetDiagnostics().GetLines()
	if len(lines) != 1 || lines[0] != "render.ts:214 · error · 'x' is never used" {
		t.Fatalf("diagnostics = %v", lines)
	}
}

func TestDiagnosticsBeforeItsCardAreRecordedAtDebug(t *testing.T) {
	// Arrange: no card has settled.
	h := newHarness(t)

	// Act.
	h.send(activityOf("unit-1", &conversationv1.AgentWrite{
		Result: &conversationv1.AgentWrite_Diagnostics{Diagnostics: &conversationv1.AgentDiagnosticsReport{}},
	}))

	// Assert.
	if rows := h.rows(rootFeed()); len(rows) != 0 {
		t.Fatalf("rows = %d, want none", len(rows))
	}
	if !h.hasRecord("debug", "daemon.feed.diagnostics_without_card") {
		t.Fatalf("records = %+v, want a DEBUG daemon.feed.diagnostics_without_card", h.records())
	}
	if h.hasRecord("warn", "daemon.feed.diagnostics_without_card") {
		t.Fatalf("records = %+v, want no WARN daemon.feed.diagnostics_without_card", h.records())
	}
}

// ---- GREP and GLOB ----

func TestGrepContentDrawsItsMatchingLinesAndStatesTheOmitted(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.send(activityOf("unit-1", &conversationv1.AgentGrep{
		Result: &conversationv1.AgentGrep_Success{Success: &conversationv1.AgentGrepSuccess{
			Query: &conversationv1.AgentGrepQuery{Pattern: "FeedRow"},
			Matches: &conversationv1.AgentGrepSuccess_Content{Content: &conversationv1.AgentGrepContent{
				Content: "row.go:1:FeedRow\nrow.go:2:FeedRow\n",
				Extent: &conversationv1.AgentGrepContent_Partial{
					Partial: &conversationv1.AgentGrepContentPartial{LinesReturned: 2, LinesOmitted: 42},
				},
			}},
		}},
	}))

	// Assert.
	card := h.card()
	if got := card.GetInput().GetText(); got != "FeedRow" {
		t.Fatalf("input = %q", got)
	}
	if card.GetInput().GetQuery() == nil {
		t.Fatalf("input form = %T, want the query form", card.GetInput().GetForm())
	}
	lines := card.GetReturned().GetLines()
	if len(lines.GetLines()) != 2 {
		t.Fatalf("lines = %v, want the two matches", lines.GetLines())
	}
	if lines.GetOmitted().GetText() != "42 more lines not shown" {
		t.Fatalf("omitted = %q", lines.GetOmitted().GetText())
	}
}

func TestGrepMatchingNothingIsASuccessWithNothingToDraw(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.send(activityOf("unit-1", &conversationv1.AgentGrep{
		Result: &conversationv1.AgentGrep_Success{Success: &conversationv1.AgentGrepSuccess{
			Query: &conversationv1.AgentGrepQuery{Pattern: "nothing"},
			Matches: &conversationv1.AgentGrepSuccess_Files{Files: &conversationv1.AgentGrepFiles{
				Extent: &conversationv1.AgentGrepFiles_All{All: &conversationv1.AgentGrepFilesAll{}},
			}},
		}},
	}))

	// Assert: the caller asked a question and got one — a success whose form is
	// `none`, never an empty text output.
	returned := h.card().GetReturned()
	if returned.GetSucceeded() == nil {
		t.Fatalf("verdict = %T, want succeeded", returned.GetVerdict())
	}
	if returned.GetNone() == nil {
		t.Fatalf("form = %T, want none", returned.GetForm())
	}
}

func TestGrepCountDrawsTheFigureAsText(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.send(activityOf("unit-1", &conversationv1.AgentGrep{
		Result: &conversationv1.AgentGrep_Success{Success: &conversationv1.AgentGrepSuccess{
			Query: &conversationv1.AgentGrepQuery{Pattern: "x"},
			Matches: &conversationv1.AgentGrepSuccess_Count{
				Count: &conversationv1.AgentGrepCount{Matches: 1_204},
			},
		}},
	}))

	// Assert.
	if got := h.card().GetReturned().GetText().GetText(); got != "1,204 matches" {
		t.Fatalf("output = %q", got)
	}
}

func TestGlobDistinguishesAnExactRemainderFromAFloor(t *testing.T) {
	tests := []struct {
		name    string
		omitted any
		want    string
	}{
		{
			name:    "exact",
			omitted: &conversationv1.AgentGlobOmittedExact{FilesOmitted: 42},
			want:    "42 more paths not shown",
		},
		{
			name:    "a floor is a different claim",
			omitted: &conversationv1.AgentGlobOmittedAtLeast{FilesOmittedAtLeast: 42},
			want:    "at least 42 more paths not shown",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			partial := &conversationv1.AgentGlobPartial{FilesReturned: 3}
			switch o := tc.omitted.(type) {
			case *conversationv1.AgentGlobOmittedExact:
				partial.Omitted = &conversationv1.AgentGlobPartial_Exact{Exact: o}
			case *conversationv1.AgentGlobOmittedAtLeast:
				partial.Omitted = &conversationv1.AgentGlobPartial_AtLeast{AtLeast: o}
			}

			// Act.
			h.send(activityOf("unit-1", &conversationv1.AgentGlob{
				Result: &conversationv1.AgentGlob_Success{Success: &conversationv1.AgentGlobSuccess{
					Query:  &conversationv1.AgentGlobQuery{Pattern: "**/*.go"},
					Paths:  []string{"a.go", "b.go", "c.go"},
					Extent: &conversationv1.AgentGlobSuccess_Partial{Partial: partial},
				}},
			}))

			// Assert.
			got := h.card().GetReturned().GetLines().GetOmitted().GetText()
			if got != tc.want {
				t.Fatalf("omitted = %q, want %q", got, tc.want)
			}
		})
	}
}

// ---- BASH, in the FOREGROUND ----

func TestAForegroundShellsInputLineIsTheBareCommandInTheCommandForm(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.send(activityOf("unit-1", &conversationv1.AgentBash{
		Result: &conversationv1.AgentBash_Start{Start: &conversationv1.AgentBashStart{
			Command:   &conversationv1.AgentBashCommand{Line: "go test ./..."},
			StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
		}},
	}))

	// Assert.
	card := h.card()
	if got := card.GetInput().GetText(); got != "go test ./..." {
		t.Fatalf("input = %q", got)
	}
	if card.GetInput().GetCommand() == nil {
		t.Fatalf("input form = %T, want the command form", card.GetInput().GetForm())
	}
}

func TestTheInputLineCarriesNoChromeForAnyForm(t *testing.T) {
	tests := []struct {
		name     string
		item     any
		wantText string
		wantForm string
	}{
		{
			name: "a command line is the vendor's line, byte for byte",
			item: &conversationv1.AgentBash{
				Result: &conversationv1.AgentBash_Start{Start: &conversationv1.AgentBashStart{
					Command:   &conversationv1.AgentBashCommand{Line: "go test ./..."},
					StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
				}},
			},
			wantText: "go test ./...",
			wantForm: "command",
		},
		{
			name: "a path is the path, with no verb",
			item: &conversationv1.AgentRead{
				Result: &conversationv1.AgentRead_Start{Start: &conversationv1.AgentReadStart{
					Path:      &conversationv1.ReadPath{Path: "internal/feed/row.go"},
					StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
				}},
			},
			wantText: "internal/feed/row.go",
			wantForm: "path",
		},
		{
			name: "a query is the terms, with no label",
			item: &conversationv1.AgentGrep{
				Result: &conversationv1.AgentGrep_Start{Start: &conversationv1.AgentGrepStart{
					Query:     &conversationv1.AgentGrepQuery{Pattern: "FeedRow"},
					StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
				}},
			},
			wantText: "FeedRow",
			wantForm: "query",
		},
		{
			name: "a glob pattern is the pattern",
			item: &conversationv1.AgentGlob{
				Result: &conversationv1.AgentGlob_Start{Start: &conversationv1.AgentGlobStart{
					Query:     &conversationv1.AgentGlobQuery{Pattern: "**/*.go"},
					StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
				}},
			},
			wantText: "**/*.go",
			wantForm: "query",
		},
		{
			name: "search terms are the terms",
			item: &conversationv1.AgentWebSearch{
				Result: &conversationv1.AgentWebSearch_Start{Start: &conversationv1.AgentWebSearchStart{
					Query:       &conversationv1.AgentWebSearchQuery{Terms: "connect-go streaming"},
					StartedAtMs: 1_000,
				}},
			},
			wantText: "connect-go streaming",
			wantForm: "query",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			h.send(activityOf("unit-1", tc.item))

			// Assert: the DAEMON states the form and the CLIENT draws its
			// chrome, so the wire text is never decorated.
			input := h.card().GetInput()
			if input.GetText() != tc.wantText {
				t.Fatalf("input text = %q, want the bare %q", input.GetText(), tc.wantText)
			}
			if got := inputFormWord(input); got != tc.wantForm {
				t.Fatalf("input form = %q, want %q", got, tc.wantForm)
			}
		})
	}
}

// inputFormWord names the form arm an input line carries.
func inputFormWord(input *frontendv1.FeedToolCallInput) string {
	switch input.GetForm().(type) {
	case *frontendv1.FeedToolCallInput_Command:
		return "command"
	case *frontendv1.FeedToolCallInput_Path:
		return "path"
	case *frontendv1.FeedToolCallInput_Query:
		return "query"
	}
	return "none"
}

func TestANonZeroExitStillCompletedTheCall(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.send(activityOf("unit-1", &conversationv1.AgentBash{
		Result: &conversationv1.AgentBash_Success{Success: &conversationv1.AgentBashSuccess{
			Command: &conversationv1.AgentBashCommand{Line: "false"},
			Outcome: &conversationv1.AgentBashSuccess_Completed{Completed: &conversationv1.AgentBashCompleted{
				Output: &conversationv1.AgentBashOutput{
					Form: &conversationv1.AgentBashOutput_Text{Text: &conversationv1.AgentBashOutputText{
						Stdout: "boom",
						Extent: &conversationv1.AgentBashOutputText_Whole{Whole: &conversationv1.AgentBashOutputWhole{}},
					}},
				},
			}},
			SettledAt: &conversationv1.AgentActivitySettledAt{AtMs: 5_200},
		}},
	}))

	// Assert: the exit code is the command's verdict on itself, not the call's.
	if h.card().GetReturned().GetSucceeded() == nil {
		t.Fatalf("verdict = %T, want succeeded for a completed call", h.card().GetReturned().GetVerdict())
	}
}

func TestATimedOutShellSaysSoAboveItsLastLines(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.send(activityOf("unit-1", &conversationv1.AgentBash{
		Result: &conversationv1.AgentBash_Success{Success: &conversationv1.AgentBashSuccess{
			Command: &conversationv1.AgentBashCommand{Line: "sleep 999"},
			Outcome: &conversationv1.AgentBashSuccess_Interrupted{Interrupted: &conversationv1.AgentBashInterrupted{
				Output: &conversationv1.AgentBashOutput{
					Form: &conversationv1.AgentBashOutput_Text{Text: &conversationv1.AgentBashOutputText{
						Stdout: "still going",
						Extent: &conversationv1.AgentBashOutputText_Whole{Whole: &conversationv1.AgentBashOutputWhole{}},
					}},
				},
				Cause: &conversationv1.AgentBashInterrupted_TimedOut{
					TimedOut: &conversationv1.AgentBashInterruptedByTimeout{TimeoutMs: 120_000},
				},
			}},
		}},
	}))

	// Assert: THE PROTO ALWAYS WINS — AgentBashInterrupted nests inside
	// AgentBashSuccess, so the badge reads succeeded and the text says how the
	// call was cut, above its last lines.
	returned := h.card().GetReturned()
	if returned.GetSucceeded() == nil {
		t.Fatalf("verdict = %T, want succeeded for an interrupted call", returned.GetVerdict())
	}
	text := returned.GetText().GetText()
	if !contains(text, "timed out after 2m 0s") || !contains(text, "still going") {
		t.Fatalf("output = %q, want the cause above the last lines", text)
	}
}

func TestAUserInterruptedShellNamesThePerson(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.send(activityOf("unit-1", &conversationv1.AgentBash{
		Result: &conversationv1.AgentBash_Success{Success: &conversationv1.AgentBashSuccess{
			Command: &conversationv1.AgentBashCommand{Line: "sleep 999"},
			Outcome: &conversationv1.AgentBashSuccess_Interrupted{Interrupted: &conversationv1.AgentBashInterrupted{
				Output: &conversationv1.AgentBashOutput{},
				Cause: &conversationv1.AgentBashInterrupted_ByUser{
					ByUser: &conversationv1.AgentBashInterruptedByUser{},
				},
			}},
		}},
	}))

	// Assert.
	if got := h.card().GetReturned().GetText().GetText(); got != "interrupted by the user" {
		t.Fatalf("output = %q", got)
	}
}

// A user interrupt is a SUCCESS arm in conversation.v1, so its verdict is
// succeeded — the edge this test pins apart from its text.
func TestAUserInterruptedShellStillReturnedSucceeded(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.send(activityOf("unit-1", &conversationv1.AgentBash{
		Result: &conversationv1.AgentBash_Success{Success: &conversationv1.AgentBashSuccess{
			Command: &conversationv1.AgentBashCommand{Line: "sleep 999"},
			Outcome: &conversationv1.AgentBashSuccess_Interrupted{Interrupted: &conversationv1.AgentBashInterrupted{
				Output: &conversationv1.AgentBashOutput{},
				Cause: &conversationv1.AgentBashInterrupted_ByUser{
					ByUser: &conversationv1.AgentBashInterruptedByUser{},
				},
			}},
		}},
	}))

	// Assert.
	returned := h.card().GetReturned()
	if returned.GetSucceeded() == nil {
		t.Fatalf("verdict = %T, want succeeded for a user-interrupted call", returned.GetVerdict())
	}
}

// THE CALL ITSELF BREAKING is what still draws `failed`: AgentBash_Failure,
// the arm that is not nested inside a success.
func TestAFailedShellCallReturnedFailed(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.send(activityOf("unit-1", &conversationv1.AgentBash{
		Result: &conversationv1.AgentBash_Failure{Failure: &conversationv1.AgentBashFailure{
			// A settled frame restates what its call named (the contract).
			Command: &conversationv1.AgentBashCommand{Line: "make"},
			Error: &conversationv1.AgentToolFailure{
				Content: &conversationv1.ToolResultContent{Blocks: []*conversationv1.ToolResultContentBlock{{
					Block: &conversationv1.ToolResultContentBlock_Text{
						Text: &conversationv1.TextBlock{Text: "the shell would not spawn"},
					},
				}}},
			},
		}},
	}))

	// Assert.
	returned := h.card().GetReturned()
	if returned.GetFailed() == nil {
		t.Fatalf("verdict = %T, want failed for a broken Bash call", returned.GetVerdict())
	}
	if got := returned.GetText().GetText(); got != "the shell would not spawn" {
		t.Fatalf("output = %q, want the failure's own account", got)
	}
}

func TestASettledCardStatesItsRuntime(t *testing.T) {
	// Arrange: a start instant, then a settle.
	h := newHarness(t)
	h.send(activityOf("unit-1", &conversationv1.AgentRead{
		Result: &conversationv1.AgentRead_Start{Start: &conversationv1.AgentReadStart{
			Path:      &conversationv1.ReadPath{Path: "a.go"},
			StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
		}},
	}))

	// Act.
	h.send(activityOf("unit-1", &conversationv1.AgentRead{
		Result: &conversationv1.AgentRead_Success{Success: &conversationv1.AgentReadSuccess{
			Path:      &conversationv1.ReadPath{Path: "a.go"},
			Extent:    &conversationv1.AgentReadSuccess_Whole{Whole: &conversationv1.AgentReadWhole{Contents: "x"}},
			SettledAt: &conversationv1.AgentActivitySettledAt{AtMs: 5_200},
		}},
	}))

	// Assert.
	if got := h.card().GetReturned().GetRuntime().GetText(); got != "ran 4.2 s" {
		t.Fatalf("runtime = %q", got)
	}
}

func TestASettleWithNoStartInstantShowsNoElapsedFigure(t *testing.T) {
	// Arrange, Act: a settled frame with no announcement before it.
	h := newHarness(t)
	h.send(activityOf("unit-1", &conversationv1.AgentRead{
		Result: &conversationv1.AgentRead_Success{Success: &conversationv1.AgentReadSuccess{
			Path:      &conversationv1.ReadPath{Path: "a.go"},
			Extent:    &conversationv1.AgentReadSuccess_Whole{Whole: &conversationv1.AgentReadWhole{Contents: "x"}},
			SettledAt: &conversationv1.AgentActivitySettledAt{AtMs: 5_200},
		}},
	}))

	// Assert: no ticking and no invented figure.
	if h.card().GetReturned().GetRuntime() != nil {
		t.Fatalf("runtime = %+v, want unset", h.card().GetReturned().GetRuntime())
	}
}

// ---- WEB FETCH and WEB SEARCH ----

func TestAWebFetchLinksItsInputLineToThePage(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.send(activityOf("unit-1", &conversationv1.AgentWebFetch{
		Result: &conversationv1.AgentWebFetch_Start{Start: &conversationv1.AgentWebFetchStart{
			Target:      &conversationv1.AgentWebFetchTarget{Url: "https://example.com/doc"},
			StartedAtMs: 1_000,
		}},
	}))

	// Assert.
	if got := h.card().GetInput().GetLink().GetUrl(); got != "https://example.com/doc" {
		t.Fatalf("link = %q", got)
	}
}

func TestAnHttpErrorPageIsAServedAnswerAndBadgedFailed(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.send(activityOf("unit-1", &conversationv1.AgentWebFetch{
		Result: &conversationv1.AgentWebFetch_Success{Success: &conversationv1.AgentWebFetchSuccess{
			Target: &conversationv1.AgentWebFetchTarget{Url: "https://example.com/missing"},
			Status: &conversationv1.AgentWebFetchHttpStatus{Code: 404, Text: "Not Found"},
			Result: "the page is gone",
		}},
	}))

	// Assert: the status says how the server answered, and it is drawn.
	returned := h.card().GetReturned()
	if returned.GetFailed() == nil {
		t.Fatalf("verdict = %T, want failed for a 404", returned.GetVerdict())
	}
	if !contains(returned.GetText().GetText(), "404 Not Found") {
		t.Fatalf("output = %q, want the status drawn", returned.GetText().GetText())
	}
}

func TestAWebSearchDrawsLinksAndNarrationInTheServedOrder(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.send(activityOf("unit-1", &conversationv1.AgentWebSearch{
		Result: &conversationv1.AgentWebSearch_Success{Success: &conversationv1.AgentWebSearchSuccess{
			Query: &conversationv1.AgentWebSearchQuery{Terms: "connect-go streaming"},
			Results: []*conversationv1.AgentWebSearchResult{
				{Entry: &conversationv1.AgentWebSearchResult_Note{
					Note: &conversationv1.AgentWebSearchNote{Text: "searching the web"},
				}},
				{Entry: &conversationv1.AgentWebSearchResult_Link{
					Link: &conversationv1.AgentWebSearchLink{Title: "Connect docs", Url: "https://connectrpc.com"},
				}},
			},
		}},
	}))

	// Assert: a narration row is not clickable; a link row is.
	links := h.card().GetReturned().GetLinks().GetLinks()
	if len(links) != 2 {
		t.Fatalf("links = %d, want 2", len(links))
	}
	if links[0].GetUrl() != nil {
		t.Fatalf("narration row = %+v, want no url", links[0])
	}
	if links[1].GetUrl().GetUrl() != "https://connectrpc.com" {
		t.Fatalf("link row = %+v", links[1])
	}
}

// ---- the kinds that draw NOTHING ----

func TestAnUnmodeledToolDrawsNoRow(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.send(activityOf("unit-1", &conversationv1.AgentUnmodeled{
		Result: &conversationv1.AgentUnmodeled_Start{Start: &conversationv1.AgentUnmodeledStart{
			ToolName:  "mcp__thing__do",
			StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
		}},
	}))

	// Assert: its home is the topbar's warning dropdown, never a feed row.
	if rows := h.rows(rootFeed()); len(rows) != 0 {
		t.Fatalf("rows = %d, want 0 for an unmodeled tool", len(rows))
	}
	if !h.hasRecord("debug", "daemon.feed.activity_draws_nothing") {
		t.Fatalf("records = %+v, want the not-a-row branch recorded", h.records())
	}
}

func TestAnActivityWithNoUnitIdentityIsRefusedLoudly(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.resolver.OnActivity(testWorkspace, mainAgent(), &conversationv1.AgentActivity{
		Item: &conversationv1.AgentActivity_Response{Response: &conversationv1.AgentResponse{}},
	}, noAddress())

	// Assert.
	if !h.hasRecord("error", "daemon.feed.activity_without_identity") {
		t.Fatalf("records = %+v, want an ERROR daemon.feed.activity_without_identity", h.records())
	}
}

// unusedFrontend keeps the frontend import honest.
var _ = (*frontendv1.FeedRow)(nil)

// TestAReadWithNoExtentDrawsTheNoneArm pins the retired-extent read: a read
// that settled with its extent oneof unset (an image this wave) has nothing to
// draw below the divider, and feed.proto's FeedToolCallReturned.output is a
// PRESENCE contract — the `none` arm, never an unset oneof and never an empty
// text.
func TestAReadWithNoExtentDrawsTheNoneArm(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.send(activityOf("unit-1", &conversationv1.AgentRead{
		Result: &conversationv1.AgentRead_Success{Success: &conversationv1.AgentReadSuccess{
			Path: &conversationv1.ReadPath{Path: "shot.png"},
		}},
	}))

	// Assert.
	returned := h.card().GetReturned()
	if returned.GetSucceeded() == nil {
		t.Fatalf("returned = %v, want the succeeded verdict", returned)
	}
	if returned.GetNone() == nil {
		t.Fatalf("output form = %v, want the `none` arm", returned.GetForm())
	}
}

// ---- BASH: the image arm, and the exit chip (landing 16) ----

// bashImageActivity is a settled foreground shell whose output was image data.
func bashImageActivity(mediaType string, data []byte) *conversationv1.AgentActivity {
	return activityOf("unit-1", &conversationv1.AgentBash{
		Result: &conversationv1.AgentBash_Success{Success: &conversationv1.AgentBashSuccess{
			Command: &conversationv1.AgentBashCommand{Line: "screencapture -x -"},
			Outcome: &conversationv1.AgentBashSuccess_Completed{Completed: &conversationv1.AgentBashCompleted{
				Output: &conversationv1.AgentBashOutput{
					Form: &conversationv1.AgentBashOutput_Image{Image: &conversationv1.AgentBashOutputImage{
						Data:      data,
						MediaType: mediaType,
					}},
				},
			}},
			SettledAt: &conversationv1.AgentActivitySettledAt{AtMs: 5_200},
		}},
	})
}

func TestAnImageProducingShellDrawsTheImageArm(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.send(bashImageActivity("image/png", []byte{0x89, 'P', 'N', 'G'}))

	// Assert: the shared image block, with the daemon's resolved src.
	image := h.card().GetReturned().GetImage()
	if image == nil {
		t.Fatalf("form = %T, want the image arm", h.card().GetReturned().GetForm())
	}
	if want := "data:image/png;base64,iVBORw=="; image.GetSrc() != want {
		t.Fatalf("src = %q, want %q", image.GetSrc(), want)
	}
}

func TestAnImageProducingShellCaptionsTheImageWithItsCommand(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.send(bashImageActivity("image/png", []byte{0x89}))

	// Assert: the command line is the only caption the record affords.
	if got := h.card().GetReturned().GetImage().GetAlt(); got != "screencapture -x -" {
		t.Fatalf("alt = %q, want the command line", got)
	}
}

func TestAnImageWithNoMediaTypeDrawsNoOutputBody(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.send(bashImageActivity("", []byte{0x89, 'P'}))

	// Assert: a src the daemon cannot compose is not invented; the card falls
	// back to the `none` arm rather than a broken image.
	if h.card().GetReturned().GetNone() == nil {
		t.Fatalf("form = %T, want none when no src can be composed", h.card().GetReturned().GetForm())
	}
}

func TestAnImageWithNoBytesRecordsTheGap(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.send(bashImageActivity("image/png", nil))

	// Assert: the gap is legible in the daemon's own narrative, not only in
	// the drawn absence.
	var found bool
	for _, r := range h.log.Records() {
		if r.Operation == "daemon.feed.bash_image_unresolved" {
			found = true
		}
	}
	if !found {
		t.Fatalf("records = %v, want one under daemon.feed.bash_image_unresolved", h.log.Records())
	}
}

// bashExitActivity is a settled foreground shell with the given termination.
func bashExitActivity(termination *conversationv1.AgentBashTermination) *conversationv1.AgentActivity {
	return activityOf("unit-1", &conversationv1.AgentBash{
		Result: &conversationv1.AgentBash_Success{Success: &conversationv1.AgentBashSuccess{
			Command: &conversationv1.AgentBashCommand{Line: "exit 3"},
			Outcome: &conversationv1.AgentBashSuccess_Completed{Completed: &conversationv1.AgentBashCompleted{
				Output: &conversationv1.AgentBashOutput{
					Form: &conversationv1.AgentBashOutput_Text{Text: &conversationv1.AgentBashOutputText{
						Stderr: "boom",
						Extent: &conversationv1.AgentBashOutputText_Whole{Whole: &conversationv1.AgentBashOutputWhole{}},
					}},
				},
				Termination: termination,
			}},
			SettledAt: &conversationv1.AgentActivitySettledAt{AtMs: 5_200},
		}},
	})
}

func TestAStatedExitCodeReachesTheForegroundCard(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.send(bashExitActivity(&conversationv1.AgentBashTermination{
		How: &conversationv1.AgentBashTermination_Exited{Exited: &conversationv1.AgentBashExited{Code: 3}},
	}))

	// Assert: the SAME element the detached shell's settled shape carries.
	exit := h.card().GetReturned().GetExit()
	if exit == nil {
		t.Fatal("exit = nil, want the code the command reported")
	}
	if exit.GetCode() != 3 {
		t.Fatalf("exit code = %d, want 3", exit.GetCode())
	}
}

func TestAShellThatStatedNoTerminationDrawsNoExitChip(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.send(bashExitActivity(nil))

	// Assert: absence draws no chip, NEVER a zero.
	if exit := h.card().GetReturned().GetExit(); exit != nil {
		t.Fatalf("exit = %v, want unset when the producer stated no termination", exit)
	}
}

func TestAKilledShellDrawsNoExitChip(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.send(bashExitActivity(&conversationv1.AgentBashTermination{
		How: &conversationv1.AgentBashTermination_Killed{Killed: &conversationv1.AgentBashKilled{}},
	}))

	// Assert: an exit code and a kill are different endings, and only one of
	// them has a number.
	if exit := h.card().GetReturned().GetExit(); exit != nil {
		t.Fatalf("exit = %v, want unset for a killed command", exit)
	}
}

func TestATextOutputShellDrawsNoExitChipWhenNoneWasStated(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.send(activityOf("unit-1", &conversationv1.AgentBash{
		Result: &conversationv1.AgentBash_Success{Success: &conversationv1.AgentBashSuccess{
			Command: &conversationv1.AgentBashCommand{Line: "sleep 999"},
			Outcome: &conversationv1.AgentBashSuccess_Interrupted{Interrupted: &conversationv1.AgentBashInterrupted{
				Output: &conversationv1.AgentBashOutput{
					Form: &conversationv1.AgentBashOutput_Text{Text: &conversationv1.AgentBashOutputText{
						Stdout: "still going",
						Extent: &conversationv1.AgentBashOutputText_Whole{Whole: &conversationv1.AgentBashOutputWhole{}},
					}},
				},
				Cause: &conversationv1.AgentBashInterrupted_ByUser{ByUser: &conversationv1.AgentBashInterruptedByUser{}},
			}},
		}},
	}))

	// Assert: an interrupted command reported no status, so the card shows none.
	if exit := h.card().GetReturned().GetExit(); exit != nil {
		t.Fatalf("exit = %v, want unset for an interrupted command", exit)
	}
}

// ---- BASH, WHOSE WORK MOVED TO THE BACKGROUND ----
//
// A BACKGROUNDED COMMAND DID NOT END, IT MOVED, and the card is not where it
// finishes. Ruled 2026-09-14: the detached shell is ALWAYS a canonical bubble
// and there is NO top-level shell row, so a foreground→detached transition
// RETIRES the running tool card and redraws the shell HEAD bubble
// (KindShellHead) in its place. The run reports on the head from then on, and
// every later frame of the retired unit draws nothing.

// startForegroundBash draws the running card a detachment later moves.
func startForegroundBash(h *harness, unit, line string) {
	h.t.Helper()
	h.send(activityOf(unit, &conversationv1.AgentBash{
		Result: &conversationv1.AgentBash_Start{Start: &conversationv1.AgentBashStart{
			Command:   &conversationv1.AgentBashCommand{Line: line},
			StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
		}},
	}))
}

func TestACommandWhoseWorkMovedToTheBackgroundRetiresTheRunningCard(t *testing.T) {
	// Arrange: a foreground shell, drawn running.
	h := newHarness(t)
	startForegroundBash(h, "unit-1", "sleep 600")
	if !h.hasActivityRow("unit-1") {
		t.Fatalf("no running card before the move, want one")
	}

	// Act: the work leaves for the background.
	h.detachWork("unit-1", "unit-1")

	// Assert: the running card is gone and the shell HEAD bubble stands in its
	// place, carrying the command.
	if h.hasActivityRow("unit-1") {
		t.Fatalf("the running card survives the move, want it retired")
	}
	if got := h.shellHead().GetCommand().GetText(); got != "sleep 600" {
		t.Fatalf("head command = %q, want the command carried onto the bubble", got)
	}
}

func TestTheDetachedRunsOwnSettleSettlesTheHeadNotAMovedCard(t *testing.T) {
	// Arrange: the work moved, and the detached shell bubble is now where it
	// reports.
	h := newHarness(t)
	startForegroundBash(h, "unit-1", "sleep 600")
	h.detachWork("unit-1", "unit-1")

	// Act: the run ends, on the shell's own stream.
	h.bash("unit-1", &conversationv1.AgentBashSuccess{
		Command: &conversationv1.AgentBashCommand{Line: "sleep 600"},
		Outcome: &conversationv1.AgentBashSuccess_Completed{Completed: &conversationv1.AgentBashCompleted{
			Output: &conversationv1.AgentBashOutput{},
			Termination: &conversationv1.AgentBashTermination{
				How: &conversationv1.AgentBashTermination_Exited{Exited: &conversationv1.AgentBashExited{Code: 0}},
			},
		}},
		SettledAt: &conversationv1.AgentActivitySettledAt{AtMs: 9_000},
	})

	// Assert: the ending settles the shell HEAD bubble, and no stale card
	// reappears beside it.
	if h.shellHead().GetSettled() == nil {
		t.Fatalf("state = %T, want the head bubble settled by the run's terminal", h.shellHead().GetState())
	}
	if h.hasActivityRow("unit-1") {
		t.Fatalf("a tool card reappeared beside the bubble, want none")
	}
}

func TestAForegroundCommandThatEndedWhereItRanStillDrawsReturned(t *testing.T) {
	// Arrange: an ordinary foreground shell. The silence above must not swallow
	// the verdict every command that ACTUALLY ended still owes.
	h := newHarness(t)
	startForegroundBash(h, "unit-1", "go test ./...")

	// Act
	h.send(activityOf("unit-1", &conversationv1.AgentBash{
		Result: &conversationv1.AgentBash_Success{Success: &conversationv1.AgentBashSuccess{
			Command: &conversationv1.AgentBashCommand{Line: "go test ./..."},
			Outcome: &conversationv1.AgentBashSuccess_Completed{Completed: &conversationv1.AgentBashCompleted{
				Output: &conversationv1.AgentBashOutput{},
				Termination: &conversationv1.AgentBashTermination{
					How: &conversationv1.AgentBashTermination_Exited{Exited: &conversationv1.AgentBashExited{Code: 0}},
				},
			}},
			SettledAt: &conversationv1.AgentActivitySettledAt{AtMs: 9_000},
		}},
	}))

	// Assert
	if h.card().GetReturned().GetSucceeded() == nil {
		t.Fatalf("outcome = %T, want the returned arm's succeeded verdict", h.card().GetOutcome())
	}
}

func TestAReplayedUnitFrameAfterTheMoveDrawsNoCard(t *testing.T) {
	// Arrange: the work moved. The producers go on restating this unit's own
	// frames afterwards -- the vendor's receipt for the launch, replayed by the
	// other plane -- and none of them says the work left.
	h := newHarness(t)
	startForegroundBash(h, "unit-1", "sleep 600")
	h.detachWork("unit-1", "unit-1")

	// Act
	h.send(activityOf("unit-1", &conversationv1.AgentBash{
		Result: &conversationv1.AgentBash_Start{Start: &conversationv1.AgentBashStart{
			Command:   &conversationv1.AgentBashCommand{Line: "sleep 600"},
			StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
		}},
	}))

	// Assert: a moved unit draws nothing, so the retired card never returns.
	if h.hasActivityRow("unit-1") {
		t.Fatalf("a replayed unit frame redrew a tool card, want none after the move")
	}
}

// A MOVED CALL'S OWN TERMINAL IS ITS WORK'S ENDING. The backgrounding receipt
// settles nothing (the shim answers no terminal for it), so a terminal on a
// moved unit is the work itself ending, and the head the card became settles on
// it rather than drawing running forever.

// exitedSuccess is a foreground call's own completed result, exit code given.
func exitedSuccess(line string, code int32, atMs int64) *conversationv1.AgentBash {
	return &conversationv1.AgentBash{
		Result: &conversationv1.AgentBash_Success{Success: &conversationv1.AgentBashSuccess{
			Command: &conversationv1.AgentBashCommand{Line: line},
			Outcome: &conversationv1.AgentBashSuccess_Completed{Completed: &conversationv1.AgentBashCompleted{
				Output: &conversationv1.AgentBashOutput{},
				Termination: &conversationv1.AgentBashTermination{
					How: &conversationv1.AgentBashTermination_Exited{Exited: &conversationv1.AgentBashExited{Code: code}},
				},
			}},
			SettledAt: &conversationv1.AgentActivitySettledAt{AtMs: atMs},
		}},
	}
}

// failedCall is a foreground call's own failure result.
func failedCall(atMs int64) *conversationv1.AgentBash {
	return &conversationv1.AgentBash{
		Result: &conversationv1.AgentBash_Failure{Failure: &conversationv1.AgentBashFailure{
			Error: &conversationv1.AgentToolFailure{
				SettledAt: &conversationv1.AgentActivitySettledAt{AtMs: atMs},
			},
		}},
	}
}

func TestAMovedCallsOwnSuccessSettlesTheHeadItBecame(t *testing.T) {
	// Arrange: the call's work moved and the head stands in the card's place.
	h := newHarness(t)
	startForegroundBash(h, "unit-1", "go test ./...")
	h.detachWork("work-1", "unit-1")

	// Act: the call's own result arrives on the unit.
	h.send(activityOf("unit-1", exitedSuccess("go test ./...", 3, 9_000)))

	// Assert: the head is settled, completed, with the call's exit code.
	settled := h.shellHead().GetSettled()
	if settled.GetCompleted() == nil || settled.GetExit().GetCode() != 3 || settled.GetEndedAtMs() != 9_000 {
		t.Fatalf("settled = %+v, want completed exit 3 at 9000", settled)
	}
}

func TestAMovedCallsOwnFailureSettlesTheHeadAsCancelled(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	startForegroundBash(h, "unit-1", "go test ./...")
	h.detachWork("work-1", "unit-1")

	// Act.
	h.send(activityOf("unit-1", failedCall(9_000)))

	// Assert: the same rendering a detached run's own failure gets.
	settled := h.shellHead().GetSettled()
	if settled.GetCancelled() == nil || settled.GetEndedAtMs() != 9_000 {
		t.Fatalf("settled = %+v, want cancelled at 9000", settled)
	}
}

func TestAMovedCallsOwnTerminalDrawsNoCardBesideTheHead(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	startForegroundBash(h, "unit-1", "go test ./...")
	h.detachWork("work-1", "unit-1")

	// Act.
	h.send(activityOf("unit-1", exitedSuccess("go test ./...", 0, 9_000)))

	// Assert.
	if h.hasActivityRow("unit-1") {
		t.Fatalf("the call's terminal redrew a tool card, want only the settled head")
	}
}

func TestAMovedCallsOwnTerminalIsRecordedAsSettlingTheHead(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	startForegroundBash(h, "unit-1", "go test ./...")
	h.detachWork("work-1", "unit-1")

	// Act.
	h.send(activityOf("unit-1", exitedSuccess("go test ./...", 0, 9_000)))

	// Assert.
	if !h.hasRecord("debug", "daemon.feed.detached_shell_settled_by_call") {
		t.Fatalf("records = %+v, want the settle recorded", h.records())
	}
}

func TestAMovedCallsNonTerminalFrameLeavesTheHeadLive(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	startForegroundBash(h, "unit-1", "go test ./...")
	h.detachWork("work-1", "unit-1")

	// Act: a restated start is not an ending.
	h.send(activityOf("unit-1", &conversationv1.AgentBash{
		Result: &conversationv1.AgentBash_Start{Start: &conversationv1.AgentBashStart{
			Command:   &conversationv1.AgentBashCommand{Line: "go test ./..."},
			StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
		}},
	}))

	// Assert.
	if h.shellHead().GetLive() == nil {
		t.Fatalf("state = %T, want the head still live", h.shellHead().GetState())
	}
}

// ---- CROSS-PLANE ORDER ----

func TestAStartAfterTheReturnKeepsTheCardReturned(t *testing.T) {
	// Arrange: the stream plane's return lands before the file plane's start
	// for the same unit.
	h := newHarness(t)
	h.send(activityOf("unit-1", &conversationv1.AgentGrep{
		Result: &conversationv1.AgentGrep_Success{Success: &conversationv1.AgentGrepSuccess{
			Query: &conversationv1.AgentGrepQuery{Pattern: "FeedRow"},
			Matches: &conversationv1.AgentGrepSuccess_Files{Files: &conversationv1.AgentGrepFiles{
				Extent: &conversationv1.AgentGrepFiles_All{All: &conversationv1.AgentGrepFilesAll{}},
			}},
		}},
	}))

	// Act.
	h.send(activityOf("unit-1", &conversationv1.AgentGrep{
		Result: &conversationv1.AgentGrep_Start{Start: &conversationv1.AgentGrepStart{
			Query:     &conversationv1.AgentGrepQuery{Pattern: "FeedRow"},
			StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
		}},
	}))

	// Assert.
	if h.card().GetReturned() == nil {
		t.Fatalf("outcome = %T, want the returned card to stand", h.card().GetOutcome())
	}
}

func TestAStartAfterTheReturnStillStatesItsInput(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.send(activityOf("unit-1", &conversationv1.AgentGrep{
		Result: &conversationv1.AgentGrep_Success{Success: &conversationv1.AgentGrepSuccess{
			Query: &conversationv1.AgentGrepQuery{Pattern: "FeedRow"},
			Matches: &conversationv1.AgentGrepSuccess_Files{Files: &conversationv1.AgentGrepFiles{
				Extent: &conversationv1.AgentGrepFiles_All{All: &conversationv1.AgentGrepFilesAll{}},
			}},
		}},
	}))

	// Act.
	h.send(activityOf("unit-1", &conversationv1.AgentGrep{
		Result: &conversationv1.AgentGrep_Start{Start: &conversationv1.AgentGrepStart{
			Query:     &conversationv1.AgentGrepQuery{Pattern: "FeedRow"},
			StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
		}},
	}))

	// Assert.
	if got := h.card().GetInput().GetText(); got != "FeedRow" {
		t.Fatalf("input = %q, want the start's query", got)
	}
}

// replayedFailures is each tool family's failure arm restating `input`, or
// restating nothing when `input` is empty, with no start ever delivered.
func replayedFailures(input string) []struct {
	name string
	act  *conversationv1.AgentActivity
} {
	failure := &conversationv1.AgentToolFailure{
		Content: &conversationv1.ToolResultContent{Blocks: []*conversationv1.ToolResultContentBlock{
			textResultBlock("it did not work"),
		}},
	}
	var (
		path    *conversationv1.ReadPath
		grep    *conversationv1.AgentGrepQuery
		glob    *conversationv1.AgentGlobQuery
		command *conversationv1.AgentBashCommand
		search  *conversationv1.AgentWebSearchQuery
	)
	if input != "" {
		path = &conversationv1.ReadPath{Path: input}
		grep = &conversationv1.AgentGrepQuery{Pattern: input}
		glob = &conversationv1.AgentGlobQuery{Pattern: input}
		command = &conversationv1.AgentBashCommand{Line: input}
		search = &conversationv1.AgentWebSearchQuery{Terms: input}
	}
	return []struct {
		name string
		act  *conversationv1.AgentActivity
	}{
		{"read", activityOf("unit-1", &conversationv1.AgentRead{Result: &conversationv1.AgentRead_Failure{
			Failure: &conversationv1.AgentReadFailure{Error: failure, Path: path}}})},
		{"write", activityOf("unit-1", &conversationv1.AgentWrite{Result: &conversationv1.AgentWrite_Failure{
			Failure: &conversationv1.AgentWriteFailure{Error: failure, Path: path}}})},
		{"edit", activityOf("unit-1", &conversationv1.AgentEdit{Result: &conversationv1.AgentEdit_Failure{
			Failure: &conversationv1.AgentEditFailure{Error: failure, Path: path}}})},
		{"grep", activityOf("unit-1", &conversationv1.AgentGrep{Result: &conversationv1.AgentGrep_Failure{
			Failure: &conversationv1.AgentGrepFailure{Error: failure, Query: grep}}})},
		{"glob", activityOf("unit-1", &conversationv1.AgentGlob{Result: &conversationv1.AgentGlob_Failure{
			Failure: &conversationv1.AgentGlobFailure{Error: failure, Query: glob}}})},
		{"bash", activityOf("unit-1", &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Failure{
			Failure: &conversationv1.AgentBashFailure{Error: failure, Command: command}}})},
		{"web search", activityOf("unit-1", &conversationv1.AgentWebSearch{Result: &conversationv1.AgentWebSearch_Failure{
			Failure: &conversationv1.AgentWebSearchFailure{Failure: failure, Query: search}}})},
	}
}

func TestAReplayedFailureDrawsTheInputItRestated(t *testing.T) {
	for _, tc := range replayedFailures("internal/feed/row.go") {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act: the settle alone, as a replay serves it.
			h.send(tc.act)

			// Assert.
			if got := h.card().GetInput().GetText(); got != "internal/feed/row.go" {
				t.Fatalf("input = %q, want the restated input", got)
			}
		})
	}
}

func TestAReplayedFailureRestatingNothingDrawsNoCard(t *testing.T) {
	for _, tc := range replayedFailures("") {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			h.send(tc.act)

			// Assert: no card with an empty input line is drawn.
			if rows := h.rows(rootFeed()); len(rows) != 0 {
				t.Fatalf("rows = %d, want 0: an unrestated failure must not draw an empty card", len(rows))
			}
		})
	}
}

func TestAReplayedFailureRestatingNothingIsRecordedAtError(t *testing.T) {
	for _, tc := range replayedFailures("") {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			h.send(tc.act)

			// Assert.
			if !h.hasRecord("error", "daemon.feed.activity_undrawable") {
				t.Fatalf("records = %+v, want an ERROR daemon.feed.activity_undrawable", h.records())
			}
		})
	}
}

func TestAFailureRestatingNothingIsDrawnFromTheStartHeld(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.send(activityOf("unit-1", &conversationv1.AgentRead{
		Result: &conversationv1.AgentRead_Start{Start: &conversationv1.AgentReadStart{
			Path: &conversationv1.ReadPath{Path: "internal/feed/row.go"},
		}},
	}))

	// Act.
	h.send(replayedFailures("")[0].act)

	// Assert.
	if got := h.card().GetInput().GetText(); got != "internal/feed/row.go" {
		t.Fatalf("input = %q, want the held start's path", got)
	}
}

func TestAFailureRestatingNothingIsRecordedAtErrorWithTheStartHeld(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.send(activityOf("unit-1", &conversationv1.AgentRead{
		Result: &conversationv1.AgentRead_Start{Start: &conversationv1.AgentReadStart{
			Path: &conversationv1.ReadPath{Path: "internal/feed/row.go"},
		}},
	}))

	// Act.
	h.send(replayedFailures("")[0].act)

	// Assert.
	if !h.hasRecord("error", "daemon.feed.settle_not_restated") {
		t.Fatalf("records = %+v, want an ERROR daemon.feed.settle_not_restated", h.records())
	}
}
