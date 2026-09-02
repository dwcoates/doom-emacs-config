package feed

import "testing"

// Every figure a client draws is composed here, so every rule these helpers
// encode gets its own case: a second authority on a number eventually
// disagrees with the first. The TOKEN figure itself is daemon-wide and is
// pinned once, in internal/figures.

func TestFormatRuntime(t *testing.T) {
	tests := []struct {
		name   string
		start  int64
		end    int64
		want   string
		wantOK bool
	}{
		{name: "sub-second reads in milliseconds", start: 1_000, end: 1_340, want: "ran 340 ms", wantOK: true},
		{name: "seconds carry one digit", start: 1_000, end: 5_200, want: "ran 4.2 s", wantOK: true},
		{name: "a whole second drops the digit", start: 1_000, end: 3_000, want: "ran 2 s", wantOK: true},
		{name: "minutes split from seconds", start: 1_000, end: 135_000, want: "ran 2m 14s", wantOK: true},
		{name: "a missing settle draws no figure", start: 1_000, end: 0, wantOK: false},
		{name: "a missing start draws no figure", start: 0, end: 1_000, wantOK: false},
		{name: "a settle before the start is undrawable", start: 5_000, end: 1_000, wantOK: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			got, ok := formatRuntime(tc.start, tc.end)

			// Assert.
			if ok != tc.wantOK {
				t.Fatalf("formatRuntime(%d, %d) ok = %v, want %v", tc.start, tc.end, ok, tc.wantOK)
			}
			if ok && got != tc.want {
				t.Fatalf("formatRuntime(%d, %d) = %q, want %q", tc.start, tc.end, got, tc.want)
			}
		})
	}
}

func TestFormatCount(t *testing.T) {
	tests := []struct {
		name string
		n    uint64
		want string
	}{
		{name: "under a thousand is bare", n: 312, want: "312"},
		{name: "exactly a thousand is grouped", n: 1_000, want: "1,000"},
		{name: "four digits group once", n: 4_312, want: "4,312"},
		{name: "seven digits group twice", n: 1_204_000, want: "1,204,000"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			got := formatCount(tc.n)

			// Assert.
			if got != tc.want {
				t.Fatalf("formatCount(%d) = %q, want %q", tc.n, got, tc.want)
			}
		})
	}
}

func TestOmittedLinesDistinguishAnExactRemainderFromAFloor(t *testing.T) {
	// Arrange, Act: the same figure, two different CLAIMS.
	exact := formatOmittedExact(42, "paths")
	floor := formatOmittedAtLeast(42, "paths")

	// Assert: only one of them is safe to read as a total.
	if exact != "42 more paths not shown" {
		t.Fatalf("exact = %q", exact)
	}
	if floor != "at least 42 more paths not shown" {
		t.Fatalf("floor = %q", floor)
	}
}

func TestFormatShowingOfStatesTheHeadCut(t *testing.T) {
	// Arrange, Act.
	got := formatShowingOf(200, 4_312)

	// Assert.
	if got != "showing 200 of 4,312 lines" {
		t.Fatalf("formatShowingOf = %q", got)
	}
}

func TestFormatLineRangeStatesAnOffsetRead(t *testing.T) {
	// Arrange, Act.
	got := formatLineRange(400, 100, 4_312)

	// Assert: the range is inclusive on both ends.
	if got != "lines 400-499 of 4,312" {
		t.Fatalf("formatLineRange = %q", got)
	}
}

func TestFormatEarlierLinesStatesACappedSpool(t *testing.T) {
	// Arrange, Act.
	got := formatEarlierLines(1_204)

	// Assert.
	if got != "1,204 earlier lines not shown" {
		t.Fatalf("formatEarlierLines = %q", got)
	}
}

func TestLangFromPath(t *testing.T) {
	tests := []struct {
		name string
		path string
		want string
	}{
		{name: "go", path: "internal/feed/row.go", want: "go"},
		{name: "typescript", path: "webapp/src/render.tsx", want: "typescript"},
		{name: "elisp", path: "lisp/status.el", want: "elisp"},
		{name: "proto", path: "proto/src/frontend/v1/feed.proto", want: "proto"},
		{name: "an unknown extension picks no grammar", path: "notes.xyz", want: ""},
		{name: "no extension picks no grammar", path: "Makefile", want: ""},
		{name: "the extension is matched case-insensitively", path: "READ.MD", want: "markdown"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			got := langFromPath(tc.path)

			// Assert.
			if got != tc.want {
				t.Fatalf("langFromPath(%q) = %q, want %q", tc.path, got, tc.want)
			}
		})
	}
}

func TestCountLines(t *testing.T) {
	tests := []struct {
		name string
		text string
		want uint64
	}{
		{name: "empty is no lines", text: "", want: 0},
		{name: "one unterminated line counts once", text: "a", want: 1},
		{name: "a trailing newline ends the last line", text: "a\n", want: 1},
		{name: "two terminated lines", text: "a\nb\n", want: 2},
		{name: "a final unterminated line still counts", text: "a\nb", want: 2},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			got := countLines(tc.text)

			// Assert.
			if got != tc.want {
				t.Fatalf("countLines(%q) = %d, want %d", tc.text, got, tc.want)
			}
		})
	}
}
