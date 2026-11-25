package daemonrules

import (
	"go/ast"
	"go/token"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalResolveSidebarDir is the package these rules hold, under the daemon module's root.
const internalResolveSidebarDir = "internal/resolve/sidebar"

// TestNoSiteScansTheRegistryForOneRecord holds every lookup of one
// workspace's record to rosterState.record: a loop over the registry's
// workspaces that leaves the loop (a return, a break) once a workspace's id
// equals the one it looks for is a scan for one record the index replaced,
// and one per row per render was what made a render cost the whole registry.
func TestNoSiteScansTheRegistryForOneRecord(t *testing.T) {
	sourcescan.InPackage(t, internalResolveSidebarDir)
	// Arrange
	var scans []string

	// Act
	sourcescan.EachSourceIn(t, ".", false, func(fset *token.FileSet, file *ast.File) {
		ast.Inspect(file, func(n ast.Node) bool {
			loop, ok := n.(*ast.RangeStmt)
			if !ok || !strings.HasSuffix(exprText(loop.X), "reg.Workspaces") {
				return true
			}
			ids := map[string]bool{exprText(loop.X) + "[].ID": true}
			if v, ok := loop.Value.(*ast.Ident); ok {
				ids[v.Name+".ID"] = true
			}
			ast.Inspect(loop.Body, func(n ast.Node) bool {
				found, ok := n.(*ast.IfStmt)
				if !ok || !comparesID(found.Cond, ids) || !leavesLoop(found.Body) {
					return true
				}
				scans = append(scans, fset.Position(loop.Pos()).String())
				return false
			})
			return true
		})
	})

	// Assert
	if len(scans) > 0 {
		t.Fatalf("these sites scan the registry for one workspace's record, want rosterState.record: %v", scans)
	}
}

// comparesID reports whether COND is an equality of one of IDS with anything.
func comparesID(cond ast.Expr, ids map[string]bool) bool {
	cmp, ok := cond.(*ast.BinaryExpr)
	return ok && cmp.Op == token.EQL && (ids[exprText(cmp.X)] || ids[exprText(cmp.Y)])
}

// leavesLoop reports whether BODY ends by leaving its loop: a return or a
// break.
func leavesLoop(body *ast.BlockStmt) bool {
	if len(body.List) == 0 {
		return false
	}
	switch last := body.List[len(body.List)-1].(type) {
	case *ast.ReturnStmt:
		return true
	case *ast.BranchStmt:
		return last.Tok == token.BREAK
	}
	return false
}

// exprText spells a selector chain (r.state.reg.Workspaces), empty for any
// other expression.
func exprText(e ast.Expr) string {
	switch e := e.(type) {
	case *ast.Ident:
		return e.Name
	case *ast.SelectorExpr:
		return exprText(e.X) + "." + e.Sel.Name
	case *ast.IndexExpr:
		return exprText(e.X) + "[]"
	default:
		return ""
	}
}

// TestTheSidebarFilesFamiliesThroughTheSharedWalk pins the sidebar to wsm's
// family walk: a copy of its own would let the sections drift from a
// repository fold's membership (wsm.RepositoryMembers).
func TestTheSidebarFilesFamiliesThroughTheSharedWalk(t *testing.T) {
	sourcescan.InPackage(t, internalResolveSidebarDir)
	// Act
	shared := sourcescan.Count(t, internalResolveSidebarDir, "wsm.SectionRepos(") + sourcescan.Count(t, internalResolveSidebarDir, "wsm.FamilyRoots(") + sourcescan.Count(t, internalResolveSidebarDir, "wsm.ParentsIn(")
	byHand := sourcescan.Count(t, internalResolveSidebarDir, "func familyRoots(") + sourcescan.Count(t, internalResolveSidebarDir, "func sectionRepos(") + sourcescan.Count(t, internalResolveSidebarDir, "func parentsIn(") + sourcescan.Count(t, internalResolveSidebarDir, "func cycles(")

	// Assert
	if shared == 0 || byHand != 0 {
		t.Fatalf("shared family walks = %d, hand-rolled = %d; want some and none", shared, byHand)
	}
}
