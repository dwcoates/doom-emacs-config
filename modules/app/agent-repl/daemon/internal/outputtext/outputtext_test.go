package outputtext

import "testing"

func TestLastLine(t *testing.T) {
	tests := []struct {
		name string
		text string
		want string
	}{
		{name: "one line", text: "done", want: "done"},
		{name: "last of several", text: "a\nb\nc", want: "c"},
		{name: "skips trailing blank lines", text: "a\nb\n\n  \n", want: "b"},
		{name: "trims the line", text: "a\n  npm error E401  \n", want: "npm error E401"},
		{name: "empty", text: "", want: ""},
		{name: "only blank lines", text: "\n \n", want: ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := LastLine(tt.text)

			// Assert.
			if got != tt.want {
				t.Fatalf("LastLine(%q) = %q, want %q", tt.text, got, tt.want)
			}
		})
	}
}

func TestTailLines(t *testing.T) {
	tests := []struct {
		name string
		text string
		n    int
		want string
	}{
		{name: "fewer lines than n", text: "a\nb", n: 5, want: "a\nb"},
		{name: "exactly n", text: "a\nb", n: 2, want: "a\nb"},
		{name: "keeps the last n", text: "a\nb\nc\nd", n: 2, want: "c\nd"},
		{name: "drops trailing newlines", text: "a\nb\n\n", n: 1, want: "b"},
		{name: "empty", text: "", n: 3, want: ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := TailLines(tt.text, tt.n)

			// Assert.
			if got != tt.want {
				t.Fatalf("TailLines(%q, %d) = %q, want %q", tt.text, tt.n, got, tt.want)
			}
		})
	}
}
