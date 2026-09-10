package prompts

import "testing"

func TestStripSentinelsRemovesTheSpanAndTheWhitespaceItLeaves(t *testing.T) {
	tests := []struct {
		name string
		in   string
		want string
	}{
		{
			name: "no sentinels",
			in:   "plain text",
			want: "plain text",
		},
		{
			name: "span alone leaves nothing",
			in:   Wrap("injected"),
			want: "",
		},
		{
			name: "span on its own line leaves no blank line",
			in:   "before\n" + Wrap("injected") + "\nafter",
			want: "before\nafter",
		},
		{
			name: "span inline collapses to one space",
			in:   "before " + Wrap("injected") + " after",
			want: "before after",
		},
		{
			name: "span at the start takes its trailing whitespace",
			in:   Wrap("injected") + "\n\nafter",
			want: "after",
		},
		{
			name: "span at the end takes its leading whitespace",
			in:   "before\n\n" + Wrap("injected"),
			want: "before",
		},
		{
			name: "span with no surrounding whitespace closes up",
			in:   "a" + Wrap("injected") + "b",
			want: "ab",
		},
		{
			name: "two spans",
			in:   Wrap("one") + "\nkept\n" + Wrap("two") + "\nalso kept",
			want: "kept\nalso kept",
		},
		{
			name: "empty span",
			in:   "before\n" + Wrap("") + "\nafter",
			want: "before\nafter",
		},
		{
			name: "span carrying a newline",
			in:   "before\n" + Wrap("line one\nline two") + "\nafter",
			want: "before\nafter",
		},
		{
			name: "blank line preserved where no span was removed",
			in:   "a\n\nb",
			want: "a\n\nb",
		},
		{
			name: "empty input",
			in:   "",
			want: "",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got, err := StripSentinels(tc.in)

			// Assert.
			if err != nil {
				t.Fatalf("StripSentinels: %v", err)
			}
			if got != tc.want {
				t.Fatalf("StripSentinels = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestStripSentinelsFailsOnAnUnbalancedMarker(t *testing.T) {
	tests := []struct {
		name string
		in   string
	}{
		{name: "open never closes", in: "a" + MetaOpen + "b"},
		{name: "close never opened", in: "a" + MetaClose + "b"},
		{name: "stray close before a balanced span", in: "a" + MetaClose + "b" + Wrap("c")},
		{name: "second open never closes", in: Wrap("a") + MetaOpen + "b"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			_, err := StripSentinels(tc.in)

			// Assert.
			if err == nil {
				t.Fatal("StripSentinels accepted an unbalanced marker")
			}
		})
	}
}

func TestWrapProducesAStrippableSpan(t *testing.T) {
	// Arrange: the daemon wraps its own injected spans with the same markers.
	text := "a " + Wrap("daemon note") + " b"

	// Act.
	got, err := StripSentinels(text)

	// Assert.
	if err != nil {
		t.Fatalf("StripSentinels: %v", err)
	}
	if got != "a b" {
		t.Fatalf("StripSentinels = %q, want %q", got, "a b")
	}
}

func TestStripSentinelsKeepsTheUserTextOfAMixedPrompt(t *testing.T) {
	// Arrange: an Emacs metaprompt injection around a user's own turn.
	text := Wrap("read the metaprompt") + "\n\nfix the bug in status.el\n\n" + Wrap("respond in full")

	// Act.
	got, err := StripSentinels(text)

	// Assert.
	if err != nil {
		t.Fatalf("StripSentinels: %v", err)
	}
	if got != "fix the bug in status.el" {
		t.Fatalf("StripSentinels = %q, want the user text alone", got)
	}
}
