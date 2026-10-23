package prose

import (
	"strings"
	"testing"

	ts "github.com/tree-sitter/go-tree-sitter"
)

func TestOpensMarker(t *testing.T) {
	tests := []struct {
		name string
		text string
		want bool
	}{
		{"an emoji at or above the floor", "🔧 text", true},
		{"a symbol at the floor", "← text", true},
		{"an emoji below the floor with the presentation selector", "ℹ️ text", true},
		{"a keycap", "1️⃣ text", true},
		{"a symbol below the floor with no presentation selector", "© text", false},
		{"ASCII prose", "items remain", false},
		{"nothing", "", false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := opensMarker(tc.text)

			// Assert
			if got != tc.want {
				t.Fatalf("opensMarker(%q) = %v, want %v", tc.text, got, tc.want)
			}
		})
	}
}

func TestSplitMarkerKeepsAMarkersContinuation(t *testing.T) {
	tests := []struct {
		name, body, marker, text string
	}{
		{"a presentation selector", "ℹ️ What", "ℹ️", "What"},
		{"a keycap", "1️⃣ First", "1️⃣", "First"},
		{"a zero-width-joined emoji", "🧑‍💻 Coded", "🧑‍💻", "Coded"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			marker, text := splitMarker(tc.body)

			// Assert
			if marker != tc.marker || text != tc.text {
				t.Fatalf("splitMarker(%q) = (%q, %q), want (%q, %q)", tc.body, marker, text, tc.marker, tc.text)
			}
		})
	}
}

func TestSplitMarkerPanicsOnABodyWithNoMarker(t *testing.T) {
	// Arrange
	defer func() {
		if recover() == nil {
			t.Fatal("splitMarker did not panic on a body with no marker")
		}
	}()

	// Act
	splitMarker("© text")
}

// promptMarker is the marker the prompt grammar reads on TEXT's first
// heading, or "" when it reads no heading with one.
func promptMarker(t *testing.T, text string) string {
	t.Helper()
	p := promptParsers.Get().(*ts.Parser)
	defer promptParsers.Put(p)
	src := []byte(text)
	tree := p.Parse(src, nil)
	defer tree.Close()
	root := tree.RootNode()
	for i := uint(0); i < root.NamedChildCount(); i++ {
		if section := root.NamedChild(i); section.Kind() == "section" {
			if marker := childOfKind(childOfKind(section, "heading"), "marker"); marker != nil {
				return strings.TrimSpace(text[marker.StartByte():marker.EndByte()])
			}
		}
	}
	return ""
}

// The response grammar (tldr, through Parse) and the prompt grammar
// (tldr_prompt) read a marker by one rule: the same line opens a root on the
// same marker in both, or in neither.
func TestBothGrammarsReadAMarkerAlike(t *testing.T) {
	tests := []struct {
		name   string
		marker string
		want   string
	}{
		{"an emoji at or above the floor", "🔧", "🔧"},
		{"an emoji below the floor with the presentation selector", "ℹ️", "ℹ️"},
		{"a keycap", "1️⃣", "1️⃣"},
		{"a symbol below the floor with no presentation selector", "©", ""},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			line := "1 " + tc.marker + " Title"

			// Act
			nodes := ParseForTest(line + "\n└── 1.1 a").GetBlocks()[0].GetTree().GetNodes()
			response := ""
			if len(nodes) > 0 {
				response = nodes[0].GetRoot().GetMarker().GetText()
			}
			prompt := promptMarker(t, line)

			// Assert
			if response != tc.want || prompt != tc.want {
				t.Fatalf("%q: the response grammar read marker %q and the prompt grammar %q, want %q in both", line, response, prompt, tc.want)
			}
		})
	}
}
