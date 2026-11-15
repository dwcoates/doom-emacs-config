package mdfence

import "testing"

func TestParse(t *testing.T) {
	tests := []struct {
		name string
		line string
		want Delimiter
	}{
		{name: "a bare backtick run", line: "```", want: Delimiter{Char: '`', Run: 3, Bare: true}},
		{name: "a longer bare run", line: "`````", want: Delimiter{Char: '`', Run: 5, Bare: true}},
		{name: "a run with trailing whitespace is bare", line: "````  \t", want: Delimiter{Char: '`', Run: 4, Bare: true}},
		{name: "a run carrying a language", line: "```go", want: Delimiter{Char: '`', Run: 3}},
		{name: "a run with a spaced info string", line: "```` markdown x", want: Delimiter{Char: '`', Run: 4}},
		{name: "a tilde run", line: "~~~", want: Delimiter{Char: '~', Run: 3, Bare: true}},
		{name: "a short run", line: "``", want: Delimiter{Char: '`', Run: 2, Bare: true}},
		{name: "a run of one character only", line: "```~~~", want: Delimiter{Char: '`', Run: 3}},
		{name: "prose", line: "x := 1", want: Delimiter{}},
		{name: "an indent the caller left on", line: "  ```", want: Delimiter{}},
		{name: "an empty line", line: "", want: Delimiter{Bare: true}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := Parse(tt.line)

			// Assert.
			if got != tt.want {
				t.Errorf("Parse(%q) = %+v, want %+v", tt.line, got, tt.want)
			}
		})
	}
}

func TestDelimiterOpens(t *testing.T) {
	tests := []struct {
		name string
		d    Delimiter
		want bool
	}{
		{name: "three backticks open", d: Delimiter{Char: '`', Run: 3, Bare: true}, want: true},
		{name: "three tildes open", d: Delimiter{Char: '~', Run: 3, Bare: true}, want: true},
		{name: "an info string does not stop an open", d: Delimiter{Char: '`', Run: 4}, want: true},
		{name: "two backticks never open", d: Delimiter{Char: '`', Run: 2, Bare: true}, want: false},
		{name: "no run never opens", d: Delimiter{Bare: true}, want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := tt.d.Opens()

			// Assert.
			if got != tt.want {
				t.Errorf("%+v.Opens() = %v, want %v", tt.d, got, tt.want)
			}
		})
	}
}

func TestDelimiterCloses(t *testing.T) {
	backticks := func(run int) Delimiter { return Delimiter{Char: '`', Run: run, Bare: true} }
	tests := []struct {
		name string
		d    Delimiter
		open Delimiter
		want bool
	}{
		{name: "an equal bare run closes", d: backticks(3), open: backticks(3), want: true},
		{name: "a longer bare run closes", d: backticks(5), open: Delimiter{Char: '`', Run: 4}, want: true},
		{name: "a shorter bare run never closes", d: backticks(3), open: backticks(4), want: false},
		{name: "a run carrying a language never closes", d: Delimiter{Char: '`', Run: 4}, open: backticks(3), want: false},
		{name: "a run of the other character never closes", d: Delimiter{Char: '~', Run: 3, Bare: true}, open: backticks(3), want: false},
		{name: "a blank line never closes", d: Delimiter{Bare: true}, open: Delimiter{Bare: true}, want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := tt.d.Closes(tt.open)

			// Assert.
			if got != tt.want {
				t.Errorf("%+v.Closes(%+v) = %v, want %v", tt.d, tt.open, got, tt.want)
			}
		})
	}
}
