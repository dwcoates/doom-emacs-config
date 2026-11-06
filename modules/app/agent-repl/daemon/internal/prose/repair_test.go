package prose

import (
	"go/ast"
	"go/parser"
	"go/token"
	"path/filepath"
	"strings"
	"testing"
)

func TestRepairFencedResponseTree(t *testing.T) {
	const header = "Response (👀 no changes made)"
	tree := lines("1 🔧 A", "├── 1.1 a", "└── 1.2 b")
	tests := []struct {
		name  string
		text  string
		stage Stage
		want  string
	}{
		{
			name: "a text fence after the header is dropped",
			text: lines(header, "", "```text", tree, "```"),
			want: lines(header, "", tree, ""),
		},
		{
			name: "a language-less fence after the header is dropped",
			text: lines(header, "", "```", tree, "```", ""),
			want: lines(header, "", tree, ""),
		},
		{
			name: "railed and indented fences inside never close the wrapper",
			text: lines(header, "```text", "1 🔧 A", "├── 1.1 a", "│   ```bash", "│   ls", "│   ```", "├── 1.2 c", "├── 1.3 d", "├── 1.4 e", "├── 1.5 f", "├── 1.6 g", "├── 1.7 h", "├── 1.8 i", "├── 1.9 j", "└── 1.10 b", "    ```bash", "    pwd", "    ```", "```"),
			want: lines(header, "1 🔧 A", "├── 1.1 a", "│   ```bash", "│   ls", "│   ```", "├── 1.2 c", "├── 1.3 d", "├── 1.4 e", "├── 1.5 f", "├── 1.6 g", "├── 1.7 h", "├── 1.8 i", "├── 1.9 j", "└── 1.10 b", "    ```bash", "    pwd", "    ```", ""),
		},
		{
			name: "a column-0 fence with a language inside is matched past",
			text: lines(header, "```text", "1 🔧 A", "├── 1.1 a", "```bash", "ls", "```", "├── 1.2 c", "├── 1.3 d", "└── 1.4 b", "```"),
			want: lines(header, "1 🔧 A", "├── 1.1 a", "```bash", "ls", "```", "├── 1.2 c", "├── 1.3 d", "└── 1.4 b", ""),
		},
		{
			name: "prose after the wrapper stays",
			text: lines(header, "```text", tree, "```", "after"),
			want: lines(header, tree, "after"),
		},
		{
			name: "an unclosed wrapper loses only its open line",
			text: lines(header, "```text", tree),
			want: lines(header, tree),
		},
		{
			name:  "while arriving, an unterminated last delimiter is not the close",
			text:  lines(header, "```text", tree, "```"),
			stage: Arriving,
			want:  lines(header, tree, "```"),
		},
		{
			name:  "while arriving, a terminated close is the close",
			text:  lines(header, "```text", tree, "```", ""),
			stage: Arriving,
			want:  lines(header, tree, ""),
		},
		{
			name: "a fence with no header before it stays",
			text: lines("```text", tree, "```"),
			want: lines("```text", tree, "```"),
		},
		{
			name: "a fence after prose under the header stays",
			text: lines(header, "Here is the format:", "```text", tree, "```"),
			want: lines(header, "Here is the format:", "```text", tree, "```"),
		},
		{
			name: "a fence after the header holding code stays",
			text: lines(header, "```go", "x := 1", "y := 2", "```"),
			want: lines(header, "```go", "x := 1", "y := 2", "```"),
		},
		{
			name: "a header with no fence after it stays",
			text: lines(header, tree),
			want: lines(header, tree),
		},
		{
			name: "a lone header stays",
			text: header,
			want: header,
		},
		{
			name: "an empty text stays",
			text: "",
			want: "",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := apply(tt.text, repairFencedResponseTree(tt.text, tt.stage))

			// Assert.
			if got != tt.want {
				t.Errorf("repairFencedResponseTree(%q) gives\n%q\nwant\n%q", tt.text, got, tt.want)
			}
		})
	}
}

func TestHoldsTree(t *testing.T) {
	tests := []struct {
		name string
		body string
		want bool
	}{
		{name: "a tree", body: lines("1 🔧 A", "└── 1.1 a"), want: true},
		{name: "a tree with code under a branch", body: lines("1 🔧 A", "├── 1.1 a", "│   ```go", "│   x := 1", "│   ```", "├── 1.2 c", "├── 1.3 d", "└── 1.4 b"), want: true},
		{name: "code", body: lines("x := 1", "y := 2"), want: false},
		{name: "a lone root", body: "1 🔧 A", want: false},
		{name: "nothing", body: "", want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := holdsTree(tt.body)

			// Assert.
			if got != tt.want {
				t.Errorf("holdsTree(%q) = %v, want %v", tt.body, got, tt.want)
			}
		})
	}
}

func TestRawLines(t *testing.T) {
	tests := []struct {
		name string
		text string
		want []rawLine
	}{
		{name: "no text", text: "", want: nil},
		{name: "one unterminated line", text: "a", want: []rawLine{{0, 1, "a"}}},
		{name: "a terminated line", text: "a\n", want: []rawLine{{0, 2, "a"}}},
		{name: "a blank line between", text: "a\n\nb", want: []rawLine{{0, 2, "a"}, {2, 3, ""}, {3, 4, "b"}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := rawLines(tt.text)

			// Assert.
			if len(got) != len(tt.want) {
				t.Fatalf("rawLines(%q) = %v, want %v", tt.text, got, tt.want)
			}
			for i := range got {
				if got[i] != tt.want[i] {
					t.Errorf("rawLines(%q)[%d] = %v, want %v", tt.text, i, got[i], tt.want[i])
				}
			}
		})
	}
}

// TestEveryFencedTreeTestGoesThroughHoldsTree holds the package to one test of
// whether a fence wraps a tree: both places that ask call holdsTree, and only
// holdsTree reads a tree region for isMetapromptTree.
func TestEveryFencedTreeTestGoesThroughHoldsTree(t *testing.T) {
	// Arrange.
	fset := token.NewFileSet()
	calls := map[string]map[string]bool{}
	files, err := filepath.Glob("*.go")
	if err != nil {
		t.Fatalf("glob the package's files: %v", err)
	}
	for _, file := range files {
		if strings.HasSuffix(file, "_test.go") {
			continue
		}
		f, err := parser.ParseFile(fset, file, nil, 0)
		if err != nil {
			t.Fatalf("parse %s: %v", file, err)
		}
		for _, decl := range f.Decls {
			fn, ok := decl.(*ast.FuncDecl)
			if !ok || fn.Body == nil {
				continue
			}
			ast.Inspect(fn.Body, func(n ast.Node) bool {
				if call, ok := n.(*ast.CallExpr); ok {
					if id, ok := call.Fun.(*ast.Ident); ok {
						if calls[id.Name] == nil {
							calls[id.Name] = map[string]bool{}
						}
						calls[id.Name][fn.Name.Name] = true
					}
				}
				return true
			})
		}
	}

	// Assert.
	for _, caller := range []string{"plainBlocks", "repairFencedResponseTree"} {
		if !calls["holdsTree"][caller] {
			t.Errorf("%s does not call holdsTree", caller)
		}
	}
	for caller := range calls["isMetapromptTree"] {
		if caller != "holdsTree" {
			t.Errorf("%s calls isMetapromptTree itself instead of holdsTree", caller)
		}
	}
}
