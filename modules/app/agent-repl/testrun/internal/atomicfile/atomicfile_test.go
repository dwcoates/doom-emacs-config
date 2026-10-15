package atomicfile

import (
	"go/ast"
	"go/parser"
	"go/token"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestReplaceWritesTheWholeFile(t *testing.T) {
	tests := []struct {
		name  string
		prior *string
	}{
		{"a new file", nil},
		{"an existing file", ptr("old contents that are longer")},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			dir := t.TempDir()
			path := filepath.Join(dir, "f.json")
			if tt.prior != nil {
				if err := os.WriteFile(path, []byte(*tt.prior), 0o644); err != nil {
					t.Fatal(err)
				}
			}

			// Act
			err := Replace(path, ".f-*", []byte("new"))

			// Assert
			if err != nil {
				t.Fatal(err)
			}
			got, err := os.ReadFile(path)
			if err != nil || string(got) != "new" {
				t.Fatalf("contents = %q, %v; want %q", got, err, "new")
			}
			assertNoTemp(t, dir)
		})
	}
}

func TestReplaceIntoAMissingDirectoryFails(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "absent", "f.json")

	// Act
	err := Replace(path, ".f-*", []byte("x"))

	// Assert
	if err == nil || !strings.Contains(err.Error(), "create a temp file beside "+path) {
		t.Fatalf("err = %v, want the temp file's creation named", err)
	}
}

func TestAFailedRenameLeavesTheOldFileAndNoTemp(t *testing.T) {
	// Arrange: the target is a non-empty directory, which no rename replaces.
	dir := t.TempDir()
	path := filepath.Join(dir, "target")
	if err := os.MkdirAll(filepath.Join(path, "inside"), 0o755); err != nil {
		t.Fatal(err)
	}

	// Act
	err := Replace(path, ".f-*", []byte("x"))

	// Assert
	if err == nil || !strings.Contains(err.Error(), "replace "+path) {
		t.Fatalf("err = %v, want the rename named", err)
	}
	if info, statErr := os.Stat(path); statErr != nil || !info.IsDir() {
		t.Fatalf("the target changed: %v, %v", info, statErr)
	}
	assertNoTemp(t, dir)
}

// TestTheRunnerReplacesFilesOnlyThroughReplace fails a function of the runner
// that creates a temp file and renames it into place by hand: every such site shares
// Replace, so none drifts into leaving a temp file or a half-written one.
func TestTheRunnerReplacesFilesOnlyThroughReplace(t *testing.T) {
	// Arrange
	root := filepath.Join("..", "..")
	var offenders []string

	// Act
	err := filepath.WalkDir(root, func(p string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(p, ".go") || strings.HasSuffix(p, "_test.go") || strings.Contains(p, "atomicfile") {
			return nil
		}
		f, err := parser.ParseFile(token.NewFileSet(), p, nil, 0)
		if err != nil {
			return err
		}
		for _, decl := range f.Decls {
			fn, ok := decl.(*ast.FuncDecl)
			if !ok || fn.Body == nil {
				continue
			}
			calls := map[string]bool{}
			ast.Inspect(fn.Body, func(n ast.Node) bool {
				if call, ok := n.(*ast.CallExpr); ok {
					if sel, ok := call.Fun.(*ast.SelectorExpr); ok {
						if pkg, ok := sel.X.(*ast.Ident); ok && pkg.Name == "os" {
							calls[sel.Sel.Name] = true
						}
					}
				}
				return true
			})
			if calls["CreateTemp"] && calls["Rename"] {
				offenders = append(offenders, p+": "+fn.Name.Name)
			}
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if len(offenders) > 0 {
		t.Fatalf("these functions rename a temp file into place by hand; replace a file through atomicfile.Replace: %v", offenders)
	}
}

func assertNoTemp(t *testing.T, dir string) {
	t.Helper()
	entries, err := os.ReadDir(dir)
	if err != nil {
		t.Fatal(err)
	}
	for _, e := range entries {
		if strings.HasPrefix(e.Name(), ".f-") {
			t.Fatalf("a temp file outlived Replace: %s", e.Name())
		}
	}
}

func ptr(s string) *string { return &s }
