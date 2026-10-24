package prose

import (
	"testing"

	"google.golang.org/protobuf/proto"

	"agent-repld/internal/dlog"

	frontendv1 "agentrepl/proto/frontend/v1"
)

// entryBlock is the entry block answering LABEL with TEXT.
func entryBlock(label, text string) *frontendv1.ProseBlock {
	return &frontendv1.ProseBlock{Block: &frontendv1.ProseBlock_Entry{Entry: &frontendv1.ProseEntry{
		Label: &frontendv1.ProseEntryLabel{Text: label},
		Text:  &frontendv1.ProseEntryText{Markdown: text},
	}}}
}

// plainBlock is the plain block of MARKDOWN.
func plainBlock(markdown string) *frontendv1.ProseBlock {
	return &frontendv1.ProseBlock{Block: &frontendv1.ProseBlock_Plain{Plain: &frontendv1.ProsePlain{Markdown: markdown}}}
}

func TestParsePromptReadsEntries(t *testing.T) {
	tests := []struct {
		name string
		text string
		want []*frontendv1.ProseBlock
	}{
		{name: "a label and a dash", text: "3.1.2 - my answer", want: []*frontendv1.ProseBlock{entryBlock("3.1.2", "my answer")}},
		{name: "a label and a colon", text: "3.1: my answer", want: []*frontendv1.ProseBlock{entryBlock("3.1", "my answer")}},
		{name: "a label and an em dash", text: "2 — my answer", want: []*frontendv1.ProseBlock{entryBlock("2", "my answer")}},
		{name: "a dotted label alone, then words", text: "3.1.2 my answer", want: []*frontendv1.ProseBlock{entryBlock("3.1.2", "my answer")}},
		{name: "a label on its own line", text: "3.1.2\nmy answer", want: []*frontendv1.ProseBlock{entryBlock("3.1.2", "my answer")}},
		{name: "a label with nothing after it", text: "3.1.2 -", want: []*frontendv1.ProseBlock{entryBlock("3.1.2", "")}},
		{
			name: "an entry running to several lines",
			text: "3.1 - first\nmore of it\n",
			want: []*frontendv1.ProseBlock{entryBlock("3.1", "first\nmore of it")},
		},
		{
			name: "two entries, a blank line apart",
			text: "3.1 - first\n\n3.2 - second",
			want: []*frontendv1.ProseBlock{entryBlock("3.1", "first"), entryBlock("3.2", "second")},
		},
		{
			name: "words before the first entry",
			text: "thanks, a few notes\n3.1 - first",
			want: []*frontendv1.ProseBlock{plainBlock("thanks, a few notes"), entryBlock("3.1", "first")},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := ParsePrompt(dlog.NewTestLogger(), tc.text)

			// Assert
			if !proto.Equal(&frontendv1.ParsedProse{Blocks: got.GetBlocks()}, &frontendv1.ParsedProse{Blocks: tc.want}) {
				t.Fatalf("blocks of %q = %v, want %v", tc.text, got.GetBlocks(), tc.want)
			}
		})
	}
}

func TestParsePromptLeavesTextWithNoEntryAsParseReadsIt(t *testing.T) {
	tests := []struct {
		name string
		text string
	}{
		{name: "plain words", text: "please fix the footer"},
		{name: "a bare number opening a line", text: "10 items are left"},
		{name: "a markdown ordered list", text: "1. First\n2. Second"},
		{name: "a pasted tree, whose roots open on a marker", text: "1 🔧 Fixed it\n└── 1.1 a detail"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := ParsePrompt(dlog.NewTestLogger(), tc.text)

			// Assert
			if want := parse(tc.text, Settled).GetBlocks(); !proto.Equal(&frontendv1.ParsedProse{Blocks: got.GetBlocks()}, &frontendv1.ParsedProse{Blocks: want}) {
				t.Fatalf("blocks of %q = %v, want Parse's %v", tc.text, got.GetBlocks(), want)
			}
		})
	}
}

func TestParsePromptKeepsTheTextAsWrittenForItsSource(t *testing.T) {
	tests := []struct {
		name string
		text string
	}{
		{name: "entries", text: "3.2 - second\n3.1 - first"},
		{name: "a pasted tree whose labels Parse would renumber", text: "1 🔧 Fixed it\n└── 1.4 a detail"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := ParsePrompt(dlog.NewTestLogger(), tc.text)

			// Assert
			if src := got.GetSource().GetMarkdown(); src != tc.text {
				t.Fatalf("source = %q, want the text as written %q", src, tc.text)
			}
		})
	}
}

func TestParsePromptDrawsAPastedTreeAfterAnEntryAsATree(t *testing.T) {
	// Arrange — an entry, then a pasted tree's root and its branch.
	text := "3.1 - see this\n1 🔧 Fixed it\n└── 1.1 a detail"

	// Act
	got := ParsePrompt(dlog.NewTestLogger(), text).GetBlocks()

	// Assert
	if len(got) != 2 || got[0].GetEntry().GetLabel().GetText() != "3.1" || got[1].GetTree() == nil {
		t.Fatalf("blocks = %v, want the entry and then the tree", got)
	}
}

func TestParsePromptRecordsAFailureOnceAndPanics(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	defer func(saved func(string) *frontendv1.ParsedProse) { parsePromptText = saved }(parsePromptText)
	parsePromptText = func(string) *frontendv1.ParsedProse { panic("prose: a broken prompt parse") }

	// Act
	recovered := func() (r any) {
		defer func() { r = recover() }()
		ParsePrompt(log, "3.1 - x")
		return nil
	}()

	// Assert
	if recovered != "prose: a broken prompt parse" {
		t.Fatalf("ParsePrompt recovered %v, want the parse's own panic re-raised", recovered)
	}
	errors := dlog.FilterRecords(log.Records(), dlog.RecordQuery{Level: "error", Operation: invariantOperation})
	if len(errors) != 1 {
		t.Fatalf("ParsePrompt recorded %d errors under %s, want 1: %+v", len(errors), invariantOperation, log.Records())
	}
	if ctx := errors[0].Context; ctx["function"] != "ParsePrompt" || ctx["length"] != len("3.1 - x") || ctx["cause"] != "prose: a broken prompt parse" {
		t.Errorf("the record's context = %v, want the function, the text's length and the cause", ctx)
	}
}

func TestReplaceEntrySwapsOneEntrysWords(t *testing.T) {
	tests := []struct {
		name  string
		text  string
		label string
		says  string
		want  string
	}{
		{name: "after a dash", text: "3.1 - old words", label: "3.1", says: "new words", want: "3.1 - new words"},
		{name: "after a colon", text: "3.1: old", label: "3.1", says: "new", want: "3.1: new"},
		{name: "after a dotted label alone", text: "3.1 old", label: "3.1", says: "new", want: "3.1 new"},
		{name: "on the line after its label", text: "3.1\nold", label: "3.1", says: "new", want: "3.1\nnew"},
		{name: "after a label with nothing after it", text: "3.1 -", label: "3.1", says: "new", want: "3.1 - new"},
		{name: "running to several lines", text: "3.1 - old\nmore old\n", label: "3.1", says: "new", want: "3.1 - new\n"},
		{
			name: "among others, each kept",
			text: "thanks\n3.1 - first\n\n3.2 - second\n3.3 - third", label: "3.2", says: "changed",
			want: "thanks\n3.1 - first\n\n3.2 - changed\n3.3 - third",
		},
		{name: "the first of two answering one label", text: "3.1 - a\n3.1 - b", label: "3.1", says: "z", want: "3.1 - z\n3.1 - b"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, ok := ReplaceEntry(tc.text, tc.label, tc.says)

			// Assert
			if !ok || got != tc.want {
				t.Fatalf("ReplaceEntry(%q, %q, %q) = %q, %v; want %q, true", tc.text, tc.label, tc.says, got, ok, tc.want)
			}
		})
	}
}

func TestReplaceEntryLeavesATextWithNoSuchEntryAlone(t *testing.T) {
	tests := []struct {
		name string
		text string
	}{
		{name: "no entry at all", text: "please fix it"},
		{name: "entries answering other labels", text: "3.1 - a\n3.2 - b"},
		{name: "a pasted tree's root with the label", text: "9 🔧 Fixed\n└── 9.1 x"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, ok := ReplaceEntry(tc.text, "9", "new")

			// Assert
			if ok || got != tc.text {
				t.Fatalf("ReplaceEntry(%q) = %q, %v; want the text unchanged and false", tc.text, got, ok)
			}
		})
	}
}
