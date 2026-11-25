package daemonrules

import (
	"go/ast"
	"go/token"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalChildprocDir is the package these rules hold, under the daemon module's root.
const internalChildprocDir = "internal/childproc"

// TestEveryCapturedChildWritesToFiles holds every daemon source outside this
// package to CaptureOutput: a child whose output is an in-memory buffer gets a
// pipe, and Wait then waits on every process that inherited it.
func TestEveryCapturedChildWritesToFiles(t *testing.T) {
	sourcescan.InPackage(t, internalChildprocDir)
	// Arrange
	var offenders []string

	// Act
	sourcescan.EachDaemonSource(t, sourcescan.DaemonRoot(t), []string{"internal/childproc"}, func(fset *token.FileSet, file *ast.File) {
		// The commands this file builds, by the name each is bound to: only
		// their Output and CombinedOutput are exec's, which pipe the output.
		commands := map[string]bool{}
		ast.Inspect(file, func(n ast.Node) bool {
			if assign, ok := n.(*ast.AssignStmt); ok && len(assign.Lhs) == len(assign.Rhs) {
				for i, rhs := range assign.Rhs {
					if builtHere(rhs) {
						if id, ok := assign.Lhs[i].(*ast.Ident); ok {
							commands[id.Name] = true
						}
					}
				}
			}
			return true
		})
		ast.Inspect(file, func(n ast.Node) bool {
			switch n := n.(type) {
			case *ast.AssignStmt:
				for i, lhs := range n.Lhs {
					sel, ok := lhs.(*ast.SelectorExpr)
					if !ok || (sel.Sel.Name != "Stdout" && sel.Sel.Name != "Stderr") || i >= len(n.Rhs) {
						continue
					}
					if unary, ok := n.Rhs[i].(*ast.UnaryExpr); ok && unary.Op == token.AND {
						offenders = append(offenders, fset.Position(n.Pos()).String())
					}
				}
			case *ast.CallExpr:
				sel, ok := n.Fun.(*ast.SelectorExpr)
				if !ok || (sel.Sel.Name != "Output" && sel.Sel.Name != "CombinedOutput") {
					break
				}
				if id, ok := sel.X.(*ast.Ident); (ok && commands[id.Name]) || builtHere(sel.X) {
					offenders = append(offenders, fset.Position(n.Pos()).String())
				}
			}
			return true
		})
	})

	// Assert
	if len(offenders) > 0 {
		t.Fatalf("these sites capture a child's output in memory; use childproc.CaptureOutput:\n%s", strings.Join(offenders, "\n"))
	}
}

// builtHere reports whether expr is a call to this package's command builders.
func builtHere(expr ast.Expr) bool {
	call, ok := expr.(*ast.CallExpr)
	if !ok {
		return false
	}
	sel, ok := call.Fun.(*ast.SelectorExpr)
	if !ok {
		return false
	}
	pkg, ok := sel.X.(*ast.Ident)
	return ok && pkg.Name == "childproc" && (sel.Sel.Name == "Command" || sel.Sel.Name == "CommandContext")
}

// TestEveryChildIsBuiltHere holds every daemon source outside this package to
// Command and CommandContext: a site calling os/exec's builders directly
// would start its child wherever the daemon stands.
func TestEveryChildIsBuiltHere(t *testing.T) {
	sourcescan.InPackage(t, internalChildprocDir)
	// Arrange
	var offenders []string

	// Act
	sourcescan.EachDaemonSource(t, sourcescan.DaemonRoot(t), []string{"internal/childproc"}, func(fset *token.FileSet, file *ast.File) {
		ast.Inspect(file, func(n ast.Node) bool {
			sel, ok := n.(*ast.SelectorExpr)
			if !ok {
				return true
			}
			if pkg, ok := sel.X.(*ast.Ident); ok && pkg.Name == "exec" && (sel.Sel.Name == "Command" || sel.Sel.Name == "CommandContext") {
				offenders = append(offenders, fset.Position(sel.Pos()).String())
			}
			return true
		})
	})

	// Assert
	if len(offenders) > 0 {
		t.Fatalf("these sites build a child with os/exec directly; use childproc.Command or childproc.CommandContext:\n%s", strings.Join(offenders, "\n"))
	}
}

// capturedRunSites are the daemon sources that call CaptureOutput themselves
// rather than Run: gitclient records the child's pid and its duration between
// the start and the wait, which Run does not answer.
var capturedRunSites = map[string]bool{"internal/gitclient/run.go": true}

// TestEveryCapturedRunGoesThroughRun holds every daemon source outside this
// package to Run: a hand-rolled capture, start, wait and read drifts from the
// others in how it reports a child that never started.
func TestEveryCapturedRunGoesThroughRun(t *testing.T) {
	sourcescan.InPackage(t, internalChildprocDir)
	// Arrange
	root := sourcescan.DaemonRoot(t)
	var offenders []string

	// Act
	sourcescan.EachDaemonSource(t, root, []string{"internal/childproc"}, func(fset *token.FileSet, file *ast.File) {
		ast.Inspect(file, func(n ast.Node) bool {
			call, ok := n.(*ast.CallExpr)
			if !ok {
				return true
			}
			sel, ok := call.Fun.(*ast.SelectorExpr)
			if !ok || sel.Sel.Name != "CaptureOutput" {
				return true
			}
			if pkg, isIdent := sel.X.(*ast.Ident); !isIdent || pkg.Name != "childproc" {
				return true
			}
			pos := fset.Position(call.Pos())
			rel, err := filepath.Rel(root, pos.Filename)
			if err != nil {
				t.Fatalf("relative path of %s: %v", pos.Filename, err)
			}
			if !capturedRunSites[filepath.ToSlash(rel)] && !strings.HasSuffix(rel, "_test.go") {
				offenders = append(offenders, pos.String())
			}
			return true
		})
	})

	// Assert
	if len(offenders) > 0 {
		t.Fatalf("these sites capture, start and wait on a child by hand; use childproc.Run:\n%s", strings.Join(offenders, "\n"))
	}
}

// TestTheSourceScansShareOneWalk holds this package's scans to
// sourcescan.EachDaemonSource: a scan with a walk of its own would drift in
// what it skips.
func TestTheSourceScansShareOneWalk(t *testing.T) {
	sourcescan.InPackage(t, internalChildprocDir)
	// Arrange
	matches, err := filepath.Glob("*_test.go")
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	var offenders []string

	// Act
	for _, name := range matches {
		// This file names the patterns it looks for.
		if name == "sources_test.go" {
			continue
		}
		raw, err := os.ReadFile(name)
		if err != nil {
			t.Fatalf("read %s: %v", name, err)
		}
		if strings.Contains(string(raw), "filepath.WalkDir(") || strings.Contains(string(raw), "parser.ParseFile(") {
			offenders = append(offenders, name)
		}
	}

	// Assert
	if len(offenders) > 0 {
		t.Fatalf("these files walk the daemon sources themselves; use sourcescan.EachDaemonSource: %v", offenders)
	}
}
