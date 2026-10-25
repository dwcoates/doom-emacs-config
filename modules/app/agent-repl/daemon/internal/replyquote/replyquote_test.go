package replyquote

import (
	"errors"
	"testing"

	"google.golang.org/protobuf/proto"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/sourcescan"
)

// text is a text block.
func text(s string) *conversationv1.UserContentBlock {
	return &conversationv1.UserContentBlock{Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: s}}}
}

// image is an image block by path.
func image(path string) *conversationv1.UserContentBlock {
	return &conversationv1.UserContentBlock{Block: &conversationv1.UserContentBlock_Image{Image: &conversationv1.ImageBlock{
		Location: &conversationv1.ImageBlock_Path{Path: &conversationv1.ImageBlockPath{Path: path}},
	}}}
}

// quote is a quote block with the given text.
func quote(s string) *conversationv1.UserContentBlock {
	return &conversationv1.UserContentBlock{Block: &conversationv1.UserContentBlock_Quote{Quote: &conversationv1.UserQuoteBlock{Text: s}}}
}

// said is a prompt of BLOCKS.
func said(blocks ...*conversationv1.UserContentBlock) *conversationv1.UserSaid {
	return &conversationv1.UserSaid{Content: &conversationv1.UserContent{Blocks: blocks}}
}

func TestFence(t *testing.T) {
	cases := []struct {
		name   string
		quoted string
		want   string
	}{
		{"no backticks takes the shortest fence", "plain prose", "```"},
		{"inline code below three keeps three", "run `go test` now", "```"},
		{"a triple fence inside takes four", "```go\nx := 1\n```", "````"},
		{"the longest run decides, wherever it is", "`a` then ````` then ``", "``````"},
		{"a run at the very end counts", "ends with ````", "`````"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := Fence(tc.quoted)

			// Assert.
			if got != tc.want {
				t.Fatalf("Fence(%q) = %q, want %q", tc.quoted, got, tc.want)
			}
		})
	}
}

func TestFenced(t *testing.T) {
	tests := []struct {
		name string
		text string
		want string
	}{
		{name: "plain text sits inside the shortest fence", text: "boom", want: "```\nboom\n```"},
		{name: "a fence inside the text gets a longer one around it", text: "```go\nx\n```", want: "````\n```go\nx\n```\n````"},
		{name: "empty text is still fenced", text: "", want: "```\n\n```"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the cases.

			// Act
			got := Fenced(tt.text)

			// Assert
			if got != tt.want {
				t.Fatalf("Fenced(%q) = %q, want %q", tt.text, got, tt.want)
			}
		})
	}
}

// TestEveryFenceInThePackageIsComposedByFenced pins that the package's quote
// composes its fence through Fenced: a second hand-built fence-text-fence
// would drift from it (another separator, a fence one short).
func TestEveryFenceInThePackageIsComposedByFenced(t *testing.T) {
	// Arrange / Act
	calls := sourcescan.Count(t, "Fence(")

	// Assert: Fence's own definition and Fenced's one call of it.
	if calls != 2 {
		t.Fatalf("Fence( occurs %d times in the package, want 2: its definition and Fenced's one call", calls)
	}
}

func TestBlock(t *testing.T) {
	cases := []struct {
		name     string
		markdown string
		prompt   bool
		want     string
	}{
		{
			name:     "a response is quoted under the response preamble, fenced",
			markdown: "The capital is Paris.",
			want: "⟢ Replying to an earlier response of yours:\n\n" +
				"```\nThe capital is Paris.\n```" +
				"\n\n⟢ My message:\n",
		},
		{
			name:     "a prompt is quoted under the prompt preamble, fenced",
			markdown: "text of p2",
			prompt:   true,
			want: "⟢ Replying to an earlier prompt in this conversation:\n\n" +
				"```\ntext of p2\n```" +
				"\n\n⟢ My message:\n",
		},
		{
			name:     "a quoted code fence is wrapped in a longer one",
			markdown: "```sh\nmake test\n```",
			want: "⟢ Replying to an earlier response of yours:\n\n" +
				"````\n```sh\nmake test\n```\n````" +
				"\n\n⟢ My message:\n",
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := Block(tc.markdown, tc.prompt, "")

			// Assert.
			if got.GetQuote().GetText() != tc.want {
				t.Fatalf("quote text =\n%q\nwant\n%q", got.GetQuote().GetText(), tc.want)
			}
		})
	}
}

func TestBlockKeepsTheQuotedRow(t *testing.T) {
	// Act.
	got := Block("Paris.", false, "row-p1").GetQuote()

	// Assert.
	if got.GetQuotedRow() != "row-p1" {
		t.Fatalf("quoted_row = %q, want the quoted bubble's row", got.GetQuotedRow())
	}
}

func TestQuote(t *testing.T) {
	cases := []struct {
		name string
		said *conversationv1.UserSaid
		want *conversationv1.UserSaid
	}{
		{
			name: "the quote leads the person's words",
			said: said(text("And its population?")),
			want: said(Block("Paris.", false, "row-p1"), text("And its population?")),
		},
		{
			name: "every composed block is kept, in order, unflattened",
			said: said(text("one"), image("/tmp/a.png"), text("two")),
			want: said(Block("Paris.", false, "row-p1"), text("one"), image("/tmp/a.png"), text("two")),
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			before := proto.Clone(tc.said)

			// Act.
			got := Quote(tc.said, "Paris.", false, "row-p1")

			// Assert.
			if !proto.Equal(got, tc.want) {
				t.Fatalf("Quote = %v, want %v", got, tc.want)
			}
			if !proto.Equal(tc.said, before) {
				t.Fatalf("Quote modified its input: %v, was %v", tc.said, before)
			}
		})
	}
}

func TestWords(t *testing.T) {
	cases := []struct {
		name string
		said *conversationv1.UserSaid
		want *conversationv1.UserSaid
	}{
		{"a prompt with no quote is unchanged", said(text("hi"), image("/a.png")), said(text("hi"), image("/a.png"))},
		{"a leading quote is left out", said(quote("q"), text("hi")), said(text("hi"))},
		{"every quote is left out, wherever it stands", said(quote("q1"), text("a"), quote("q2"), text("b")), said(text("a"), text("b"))},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := Words(tc.said)

			// Assert.
			if !proto.Equal(got, tc.want) {
				t.Fatalf("Words = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestRequote(t *testing.T) {
	cases := []struct {
		name     string
		original *conversationv1.UserSaid
		edited   *conversationv1.UserSaid
		want     *conversationv1.UserSaid
	}{
		{
			name:     "an unquoted prompt's edit is the edit",
			original: said(text("old")),
			edited:   said(text("new")),
			want:     said(text("new")),
		},
		{
			name:     "the original's quote leads the edited words",
			original: said(quote("q"), text("old")),
			edited:   said(text("new"), image("/a.png")),
			want:     said(quote("q"), text("new"), image("/a.png")),
		},
		{
			name:     "every original quote is kept, in order, ahead of the words",
			original: said(quote("q1"), text("a"), quote("q2"), text("b")),
			edited:   said(text("a and b")),
			want:     said(quote("q1"), quote("q2"), text("a and b")),
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got, err := Requote(tc.original, tc.edited)

			// Assert.
			if err != nil {
				t.Fatalf("Requote: %v", err)
			}
			if !proto.Equal(got, tc.want) {
				t.Fatalf("Requote = %v, want %v", got, tc.want)
			}
		})
	}
}

// TestRequoteRefusesAnEditCarryingAQuote pins that a quote in the EDITED
// content is refused rather than kept or dropped: an editor is never handed
// one, so its arrival is the editor's defect.
func TestRequoteRefusesAnEditCarryingAQuote(t *testing.T) {
	// Arrange.
	original := said(quote("q"), text("old"))
	edited := said(quote("forged"), text("new"))

	// Act.
	got, err := Requote(original, edited)

	// Assert.
	if !errors.Is(err, ErrEditedQuote) {
		t.Fatalf("err = %v, want ErrEditedQuote", err)
	}
	if got != nil {
		t.Fatalf("Requote answered %v alongside its refusal", got)
	}
}

func TestSplit(t *testing.T) {
	cases := []struct {
		name     string
		markdown string
		prompt   bool
		want     Parts
	}{
		{
			name:     "a quoted response splits into its heading, its markdown and the closing marker",
			markdown: "The capital is Paris.",
			want:     Parts{Heading: LabelResponse, Quoted: "The capital is Paris.", Closing: ClosingMarker},
		},
		{
			name:     "a quoted prompt splits under the prompt heading",
			markdown: "text of p2",
			prompt:   true,
			want:     Parts{Heading: HeadingPrompt, Quoted: "text of p2", Closing: ClosingMarker},
		},
		{
			name:     "a quoted code fence comes back without the longer fence around it",
			markdown: "```sh\nmake test\n```",
			want:     Parts{Heading: LabelResponse, Quoted: "```sh\nmake test\n```", Closing: ClosingMarker},
		},
		{
			name:     "an empty quote splits to empty markdown",
			markdown: "",
			want:     Parts{Heading: LabelResponse, Quoted: "", Closing: ClosingMarker},
		},
		{
			name:     "a multi-line quote keeps every line",
			markdown: "1 root\n├── 1.1 a\n└── 1.2 b",
			want:     Parts{Heading: LabelResponse, Quoted: "1 root\n├── 1.1 a\n└── 1.2 b", Closing: ClosingMarker},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			composed := Block(tc.markdown, tc.prompt, "").GetQuote().GetText()

			// Act.
			got, err := Split(composed)

			// Assert.
			if err != nil {
				t.Fatalf("Split(%q) error = %v, want none", composed, err)
			}
			if got != tc.want {
				t.Fatalf("Split(%q) = %+v, want %+v", composed, got, tc.want)
			}
		})
	}
}

func TestSplitRefusesTextBlockDidNotCompose(t *testing.T) {
	cases := []struct {
		name string
		text string
	}{
		{name: "no heading", text: "the quote"},
		{name: "a heading with no closing marker", text: "⟢ Replying to an earlier response of yours:\n\n```\nParis.\n```"},
		{name: "an unfenced quote", text: "⟢ Replying to an earlier response of yours:\n\nParis.\n\n⟢ My message:\n"},
		{name: "a fence that does not close", text: "⟢ Replying to an earlier response of yours:\n\n```\nParis.\n\n⟢ My message:\n"},
		{name: "a fence too short for the quote", text: "⟢ Replying to an earlier response of yours:\n\n```\n```go\nx\n```\n```\n\n⟢ My message:\n"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got, err := Split(tc.text)

			// Assert.
			if !errors.Is(err, ErrNotComposed) {
				t.Fatalf("Split(%q) error = %v, want ErrNotComposed", tc.text, err)
			}
			if got != (Parts{}) {
				t.Fatalf("Split(%q) = %+v, want the zero Parts", tc.text, got)
			}
		})
	}
}

func TestHighlighted(t *testing.T) {
	cases := []struct {
		name  string
		said  *conversationv1.UserSaid
		words string
		want  *conversationv1.UserSaid
	}{
		{
			name:  "the words' quote leads the person's blocks",
			said:  said(text("why?")),
			words: "the sky is blue",
			want: said(quote("⟢ Asking about these words from an earlier response of yours:\n\n```\nthe sky is blue\n```\n\n⟢ My message:\n"),
				text("why?")),
		},
		{
			name:  "words carrying a fence get a longer one",
			said:  said(text("why?"), image("/a.png")),
			words: "```x```",
			want: said(quote("⟢ Asking about these words from an earlier response of yours:\n\n````\n```x```\n````\n\n⟢ My message:\n"),
				text("why?"), image("/a.png")),
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			before := proto.Clone(tc.said)

			// Act
			got := Highlighted(tc.said, tc.words)

			// Assert
			if !proto.Equal(got, tc.want) {
				t.Fatalf("Highlighted = %v, want %v", got, tc.want)
			}
			if !proto.Equal(tc.said, before) {
				t.Fatal("Highlighted modified the said it was given")
			}
		})
	}
}

func TestSplitTakesAHighlightApart(t *testing.T) {
	// Arrange
	composed := Highlighted(said(text("why?")), "the sky is blue").GetContent().GetBlocks()[0].GetQuote().GetText()

	// Act
	got, err := Split(composed)

	// Assert
	if err != nil {
		t.Fatalf("Split: %v", err)
	}
	if want := (Parts{Heading: HeadingHighlight, Quoted: "the sky is blue", Closing: ClosingMarker}); got != want {
		t.Fatalf("Split = %+v, want %+v", got, want)
	}
}
