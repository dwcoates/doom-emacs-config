package reqmanifest

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestIncludesAnswersTheRootManifestsIncludes(t *testing.T) {
	tests := []struct {
		name    string
		body    *string
		want    []string
		wantErr bool
	}{
		{name: "the includes are answered in order", body: ptr(`{"includes": ["a/requirements.json", "b/requirements.json"]}`), want: []string{"a/requirements.json", "b/requirements.json"}},
		{name: "a manifest including nothing answers none", body: ptr(`{"requirements": []}`)},
		{name: "a manifest that is no JSON is an error", body: ptr(`{`), wantErr: true},
		{name: "no manifest is an error", wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			path := filepath.Join(t.TempDir(), "requirements.json")
			if tt.body != nil {
				if err := os.WriteFile(path, []byte(*tt.body), 0o600); err != nil {
					t.Fatal(err)
				}
			}

			// Act
			got, err := Includes(path)

			// Assert
			if (err != nil) != tt.wantErr || fmt.Sprint(got) != fmt.Sprint(tt.want) {
				t.Fatalf("Includes = (%v, %v), want %v (error %v)", got, err, tt.want, tt.wantErr)
			}
		})
	}
}

func ptr(s string) *string { return &s }

// TestEveryReaderOfTheIncludesCallsIncludes pins the one parse of the root
// manifest's includes: the holder ensure and the harness call Includes, and
// neither declares an `includes` field of its own.
func TestEveryReaderOfTheIncludesCallsIncludes(t *testing.T) {
	tests := []struct {
		name string
		dir  string
	}{
		{name: "the holder ensure", dir: "../holderensure"},
		{name: "the integration harness", dir: "../../integration/harness"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			entries, err := os.ReadDir(tt.dir)
			if err != nil {
				t.Fatal(err)
			}
			fset := token.NewFileSet()
			calls, ownField := 0, ""

			// Act
			for _, e := range entries {
				if !strings.HasSuffix(e.Name(), ".go") || strings.HasSuffix(e.Name(), "_test.go") {
					continue
				}
				f, err := parser.ParseFile(fset, filepath.Join(tt.dir, e.Name()), nil, 0)
				if err != nil {
					t.Fatal(err)
				}
				ast.Inspect(f, func(n ast.Node) bool {
					switch n := n.(type) {
					case *ast.Field:
						if n.Tag != nil && strings.Contains(n.Tag.Value, `json:"includes"`) {
							ownField = fset.Position(n.Pos()).String()
						}
					case *ast.SelectorExpr:
						if id, ok := n.X.(*ast.Ident); ok && id.Name == "reqmanifest" && n.Sel.Name == "Includes" {
							calls++
						}
					}
					return true
				})
			}

			// Assert
			if ownField != "" || calls == 0 {
				t.Fatalf("own includes field at %q, %d reqmanifest.Includes calls; want none of its own and at least one call", ownField, calls)
			}
		})
	}
}
