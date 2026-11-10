package textline

import (
	"go/ast"
	"go/parser"
	"go/token"
	"io/fs"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
)

func TestLines(t *testing.T) {
	tests := []struct {
		name string
		text string
		want []string
	}{
		{name: "the empty text has no lines", text: "", want: nil},
		{name: "a final newline ends the last line", text: "a\nb\n", want: []string{"a", "b"}},
		{name: "an unterminated last line is still a line", text: "a\nb", want: []string{"a", "b"}},
		{name: "a lone newline is one empty line", text: "\n", want: []string{""}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := Lines(tt.text)

			// Assert.
			if !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("Lines(%q) = %q, want %q", tt.text, got, tt.want)
			}
		})
	}
}

func TestFirstNonBlank(t *testing.T) {
	tests := []struct {
		name string
		text string
		want string
	}{
		{name: "one line", text: "hello", want: "hello"},
		{name: "leading blank lines are skipped", text: "\n \n  hello  \nworld", want: "hello"},
		{name: "later lines are dropped", text: "first\nsecond", want: "first"},
		{name: "nothing but blanks", text: " \n\t\n", want: ""},
		{name: "empty text", text: "", want: ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := FirstNonBlank(tt.text)

			// Assert
			if got != tt.want {
				t.Fatalf("FirstNonBlank(%q) = %q, want %q", tt.text, got, tt.want)
			}
		})
	}
}

// divergentFirstLines are the packages whose own firstLine keeps a DIFFERENT
// rule than FirstNonBlank, so they are not this helper's call sites: the
// sidebar keeps a prompt's literal first line, and the merge narration names
// a fallback reason for an empty one.
var divergentFirstLines = map[string]bool{
	filepath.Join("..", "resolve", "sidebar", "rows.go"): true,
	filepath.Join("..", "merge", "process.go"):           true,
}

// TestNoPackageHandRollsItsOwnFirstLine pins that the daemon's packages take
// the first non-blank line through this package rather than a firstLine of
// their own.
func TestNoPackageHandRollsItsOwnFirstLine(t *testing.T) {
	// Arrange
	var found []string
	files := token.NewFileSet()

	// Act
	err := filepath.WalkDir("..", func(path string, entry fs.DirEntry, walkErr error) error {
		if walkErr != nil {
			return walkErr
		}
		if entry.IsDir() || filepath.Ext(path) != ".go" || strings.HasSuffix(path, "_test.go") || divergentFirstLines[path] {
			return nil
		}
		parsed, err := parser.ParseFile(files, path, nil, 0)
		if err != nil {
			return err
		}
		for _, decl := range parsed.Decls {
			if fn, ok := decl.(*ast.FuncDecl); ok && (fn.Name.Name == "firstLine" || fn.Name.Name == "firstNonBlankLine") {
				found = append(found, path)
			}
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("scan: %v", err)
	}
	if len(found) > 0 {
		t.Fatalf("hand-rolled firstLine in %v, want textline.FirstNonBlank", found)
	}
}

func TestNoPackageHandRollsItsOwnLineSplit(t *testing.T) {
	// Arrange
	var found []string

	// Act
	err := filepath.WalkDir("..", func(path string, entry fs.DirEntry, walkErr error) error {
		if walkErr != nil {
			return walkErr
		}
		if entry.IsDir() || filepath.Ext(path) != ".go" || strings.HasSuffix(path, "_test.go") || filepath.Base(filepath.Dir(path)) == "textline" {
			return nil
		}
		source, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if strings.Contains(string(source), `strings.Split(strings.TrimSuffix(`) {
			found = append(found, path)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("scan: %v", err)
	}
	if len(found) > 0 {
		t.Fatalf("hand-rolled line split in %v, want textline.Lines", found)
	}
}

func TestOneSentenceAcceptsOneLineOfOneSentence(t *testing.T) {
	tests := []struct {
		name string
		text string
	}{
		{name: "at the fewest words", text: "Design doc"},
		{name: "at the most words", text: "one two three four"},
		{name: "a closing period", text: "The design doc."},
		{name: "a dotted word inside", text: "The v1.2 notes"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange, Act
			err := OneSentence("label", tt.text, 2, 4)

			// Assert
			if err != nil {
				t.Fatalf("OneSentence(%q) = %v, want nil", tt.text, err)
			}
		})
	}
}

func TestOneSentenceNamesWhyATextIsRefused(t *testing.T) {
	tests := []struct {
		name string
		text string
		want string
	}{
		{name: "too few words", text: "Doc", want: "a label is 2 to 4 words, not 1"},
		{name: "too many words", text: "one two three four five", want: "a label is 2 to 4 words, not 5"},
		{name: "a line break", text: "Design\ndoc", want: "a label is one line"},
		{name: "a carriage return", text: "Design\rdoc", want: "a label is one line"},
		{name: "surrounding whitespace", text: "Design doc ", want: "a label carries no whitespace at either end"},
		{name: "a second sentence", text: "Why? Because", want: `a label is one sentence, and "Why?" ends one before its last word`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange, Act
			err := OneSentence("label", tt.text, 2, 4)

			// Assert
			if err == nil || err.Error() != tt.want {
				t.Fatalf("OneSentence(%q) = %v, want %q", tt.text, err, tt.want)
			}
		})
	}
}
