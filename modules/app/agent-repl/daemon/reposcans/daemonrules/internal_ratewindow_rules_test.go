package daemonrules

import (
	"go/ast"
	"go/token"
	"path/filepath"
	"slices"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalRatewindowDir is the package these rules hold, under the daemon module's root.
const internalRatewindowDir = "internal/ratewindow"

// TestNoRecordIsRateLimitedByHand pins the one home: no daemon source outside
// this package counts the records it turned away by hand (an increment of a
// counter named suppressed), which is the shape a hand-rolled window takes.
func TestNoRecordIsRateLimitedByHand(t *testing.T) {
	sourcescan.InPackage(t, internalRatewindowDir)
	// Arrange.
	var strays []string

	// Act.
	sourcescan.EachDaemonSource(t, sourcescan.DaemonRoot(t), []string{"internal/ratewindow"}, func(fset *token.FileSet, file *ast.File) {
		ast.Inspect(file, func(n ast.Node) bool {
			inc, ok := n.(*ast.IncDecStmt)
			if !ok || inc.Tok != token.INC {
				return true
			}
			var name string
			switch x := inc.X.(type) {
			case *ast.Ident:
				name = x.Name
			case *ast.SelectorExpr:
				name = x.Sel.Name
			}
			if strings.Contains(strings.ToLower(name), "suppressed") {
				strays = append(strays, fset.Position(inc.Pos()).String())
			}
			return true
		})
	})

	// Assert.
	if len(strays) > 0 {
		t.Fatalf("hand-rolled rate windows at %v; admit through ratewindow.Window", strays)
	}
}

// TestTheRateLimitedRecordsShareTheWindow pins the call sites that were
// folded into Window: each still declares one.
func TestTheRateLimitedRecordsShareTheWindow(t *testing.T) {
	sourcescan.InPackage(t, internalRatewindowDir)
	// Arrange.
	want := []string{"internal/drain/refusals.go", "internal/resolve/feed/heldmutex.go", "internal/wsm/conn.go"}
	root := sourcescan.DaemonRoot(t)
	var got []string

	// Act.
	sourcescan.EachDaemonSource(t, root, []string{"internal/ratewindow"}, func(fset *token.FileSet, file *ast.File) {
		found := false
		ast.Inspect(file, func(n ast.Node) bool {
			if sourcescan.IsSelector(n, "ratewindow", "Window") {
				found = true
			}
			return !found
		})
		if found {
			rel, err := filepath.Rel(root, fset.Position(file.Pos()).Filename)
			if err != nil {
				t.Fatalf("relative path: %v", err)
			}
			got = append(got, rel)
		}
	})

	// Assert.
	slices.Sort(got)
	for _, site := range want {
		if !slices.Contains(got, site) {
			t.Fatalf("files declaring a ratewindow.Window = %v, want %s among them", got, site)
		}
	}
}
