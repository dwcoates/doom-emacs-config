package reqmanifest

import (
	"agent-repld/internal/sourcescan"
	"fmt"
	"go/ast"
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
// manifest's includes: the holder ensure calls Includes, and declares an `includes` field of its own.
func TestEveryReaderOfTheIncludesCallsIncludes(t *testing.T) {
	tests := []struct {
		name string
		dir  string
	}{
		{name: "the holder ensure", dir: "../holderensure"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			calls, ownField := 0, ""

			// Act
			sourcescan.EachSourceIn(t, tt.dir, false, func(fset *token.FileSet, f *ast.File) {
				ast.Inspect(f, func(n ast.Node) bool {
					if field, ok := n.(*ast.Field); ok && field.Tag != nil && strings.Contains(field.Tag.Value, `json:"includes"`) {
						ownField = fset.Position(field.Pos()).String()
					}
					if sourcescan.IsSelector(n, "reqmanifest", "Includes") {
						calls++
					}
					return true
				})
			})

			// Assert
			if ownField != "" || calls == 0 {
				t.Fatalf("own includes field at %q, %d reqmanifest.Includes calls; want none of its own and at least one call", ownField, calls)
			}
		})
	}
}
