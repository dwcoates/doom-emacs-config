package daemonrules

import (
	"go/ast"
	"go/token"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalGitlayoutDir is the package these rules hold, under the daemon module's root.
const internalGitlayoutDir = "internal/gitlayout"

// TestNoOtherDaemonSourceReadsAWorktreesGitFile pins the one home of the
// linked-worktree read: no production source outside this package spells
// git's "gitdir: " line, so no second reading of it can drift.
func TestNoOtherDaemonSourceReadsAWorktreesGitFile(t *testing.T) {
	sourcescan.InPackage(t, internalGitlayoutDir)
	// Act
	var sites []string
	sourcescan.EachDaemonSource(t, sourcescan.DaemonRoot(t), []string{"internal/gitlayout"}, func(fset *token.FileSet, file *ast.File) {
		ast.Inspect(file, func(n ast.Node) bool {
			if lit, ok := n.(*ast.BasicLit); ok && lit.Kind == token.STRING && strings.Contains(lit.Value, "gitdir: ") {
				sites = append(sites, fset.Position(lit.Pos()).String())
			}
			return true
		})
	})

	// Assert
	if len(sites) != 0 {
		t.Fatalf("production sources read a worktree's .git file outside gitlayout: %v", sites)
	}
}
