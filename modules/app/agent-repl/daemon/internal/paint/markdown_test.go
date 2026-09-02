package paint

import "testing"

func TestHighlightMarkdownConstructs(t *testing.T) {
	tests := []struct {
		name  string
		code  string
		token string
		want  string
	}{
		{name: "heading", code: "# Title\n", token: "# Title", want: "heading"},
		{name: "indented heading", code: "  ## Sub\n", token: "## Sub", want: "heading"},
		{name: "blockquote", code: "> quoted\n", token: "> quoted", want: "comment"},
		{name: "bullet marker", code: "- item\n", token: "- ", want: "punctuation"},
		{name: "ordered marker", code: "1. item\n", token: "1. ", want: "punctuation"},
		{name: "code span", code: "a `x` b\n", token: "`x`", want: "string"},
		{name: "strong with asterisks", code: "a **x** b\n", token: "**x**", want: "strong"},
		{name: "strong with underscores", code: "a __x__ b\n", token: "__x__", want: "strong"},
		{name: "emphasis with asterisk", code: "a *x* b\n", token: "*x*", want: "emphasis"},
		{name: "emphasis with underscore", code: "a _x_ b\n", token: "_x_", want: "emphasis"},
		{name: "link", code: "a [text](url) b\n", token: "[text](url)", want: "link"},
		{name: "fence", code: "```go\nx\n```\n", token: "```go", want: "punctuation"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			spans := highlight(t, "markdown", tc.code)

			// Assert.
			if got := classOf(t, spans, tc.token); got != tc.want {
				t.Fatalf("class of %q = %q, want %q", tc.token, got, tc.want)
			}
		})
	}
}

func TestHighlightMarkdownLeavesFencedContentPlain(t *testing.T) {
	// Arrange: a heading marker inside a fence is content, not a heading.
	code := "```\n# not a heading\n```\n"

	// Act.
	spans := highlight(t, "markdown", code)

	// Assert.
	if got := classOf(t, spans, "\n# not a heading\n"); got != "" {
		t.Fatalf("class = %q, want plain", got)
	}
}

func TestHighlightMarkdownKeepsAnUnclosedEmphasisPlain(t *testing.T) {
	// Arrange.
	code := "a *unclosed\n"

	// Act.
	spans := highlight(t, "markdown", code)

	// Assert.
	if len(spans) != 1 || spans[0].Class != "" {
		t.Fatalf("spans = %+v, want one plain span", spans)
	}
}

func TestHighlightMarkdownKeepsTextVerbatim(t *testing.T) {
	// Arrange.
	code := "# H\n\n- a **b** _c_ `d` [e](f)\n\n> q\n\n```sh\nls\n```\n\ntrailing without newline"

	// Act.
	spans := highlight(t, "md", code)

	// Assert: highlight already asserts the round trip; the alias is what this
	// subject pins.
	if len(spans) == 0 {
		t.Fatal("no spans emitted")
	}
}

func TestHighlightMarkdownLinkWithoutTargetStaysPlain(t *testing.T) {
	// Arrange.
	code := "see [text] here\n"

	// Act.
	spans := highlight(t, "markdown", code)

	// Assert.
	for _, span := range spans {
		if span.Class == "link" {
			t.Fatalf("a bracket with no target painted as a link: %+v", spans)
		}
	}
}
