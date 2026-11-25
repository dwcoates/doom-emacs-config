package daemonrules

import (
	"go/ast"
	"go/token"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalReqmanifestDir is the package these rules hold, under the daemon module's root.
const internalReqmanifestDir = "internal/reqmanifest"

// TestEveryReaderOfTheIncludesCallsIncludes pins the one parse of the root
// manifest's includes: the holder ensure calls Includes, and declares an `includes` field of its own.
func TestEveryReaderOfTheIncludesCallsIncludes(t *testing.T) {
	sourcescan.InPackage(t, internalReqmanifestDir)
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
