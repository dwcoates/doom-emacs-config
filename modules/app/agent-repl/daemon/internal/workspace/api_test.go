package workspace

import (
	"go/ast"
	"go/parser"
	"go/token"
	"io/fs"
	"path/filepath"
	"sort"
	"strings"
	"testing"

	"agent-repld/internal/lifecycleconfig"
)

func TestNewCreateSpecStatesItsOriginator(t *testing.T) {
	for _, originator := range lifecycleconfig.Originators {
		t.Run(originator.String(), func(t *testing.T) {
			// Act
			spec := NewCreateSpec(originator)

			// Assert
			if got := spec.Originator(); got != originator {
				t.Fatalf("Originator() = %v, want %v", got, originator)
			}
		})
	}
}

func TestABareCreateSpecStatesNoOriginator(t *testing.T) {
	// Act
	got := CreateSpec{}.Originator()

	// Assert
	if got != 0 {
		t.Fatalf("Originator() = %v, want the zero value validateCreate refuses", got)
	}
}

func TestACreateSpecFromNewCreateSpecPassesValidation(t *testing.T) {
	// Arrange
	f := newFixture(t)
	spec := NewCreateSpec(lifecycleconfig.OriginatorUserOrWorkspace)
	spec.RepoDir, spec.InitialPrompt = mainWorktree(t, f), "fix the login bug"

	// Act
	err := f.verbs.(*verbs).validateCreate(f.log.logger, spec)

	// Assert
	if err != nil {
		t.Fatalf("validateCreate = %v, want the spec accepted", err)
	}
}

// TestEveryCreateSpecBuilderGoesThroughNewCreateSpec pins the one way a
// production spec is built: no source of the daemon composes a CreateSpec
// literal with fields (an empty one is only a zero value beside an error),
// and every known builder calls NewCreateSpec.
func TestEveryCreateSpecBuilderGoesThroughNewCreateSpec(t *testing.T) {
	// Arrange
	want := []string{
		"internal/server/workspaces.go:CreateWorkspace",
		"internal/workspace/commandsupport.go:RequestCommandSupport",
		"internal/workspace/create.go:CreateMergeWorkspace",
		"internal/workspace/deploychecks.go:deployMediatorSpec",
		"internal/workspace/intake.go:CreateIntake",
		"internal/workspace/pendingcreates.go:decodeHeldSpec",
	}

	// Act
	literals, callers := createSpecBuilders(t)

	// Assert
	if len(literals) > 0 {
		t.Fatalf("CreateSpec literals with fields outside NewCreateSpec: %v", literals)
	}
	if strings.Join(callers, "\n") != strings.Join(want, "\n") {
		t.Fatalf("NewCreateSpec callers = %v, want %v", callers, want)
	}
}

// createSpecBuilders scans every production source of the daemon for
// CreateSpec literals with fields outside NewCreateSpec, and for the
// functions that call NewCreateSpec, each as path:function.
func createSpecBuilders(t *testing.T) (literals, callers []string) {
	t.Helper()
	root := filepath.Join("..", "..")
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() {
			if d.Name() == "testdata" || d.Name() == "node_modules" {
				return filepath.SkipDir
			}
			return nil
		}
		if !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		file, err := parser.ParseFile(token.NewFileSet(), path, nil, 0)
		if err != nil {
			return err
		}
		rel, err := filepath.Rel(root, path)
		if err != nil {
			return err
		}
		for _, decl := range file.Decls {
			fn, ok := decl.(*ast.FuncDecl)
			if !ok || fn.Body == nil {
				continue
			}
			site := filepath.ToSlash(rel) + ":" + fn.Name.Name
			calls := false
			ast.Inspect(fn.Body, func(n ast.Node) bool {
				switch n := n.(type) {
				case *ast.CompositeLit:
					if isCreateSpecName(n.Type) && len(n.Elts) > 0 && fn.Name.Name != "NewCreateSpec" {
						literals = append(literals, site)
					}
				case *ast.CallExpr:
					if isNewCreateSpecName(n.Fun) {
						calls = true
					}
				}
				return true
			})
			if calls {
				callers = append(callers, site)
			}
		}
		return nil
	})
	if err != nil {
		t.Fatalf("scan the daemon's sources: %v", err)
	}
	sort.Strings(callers)
	return literals, callers
}

// isCreateSpecName reports whether EXPR names CreateSpec, bare or qualified.
func isCreateSpecName(expr ast.Expr) bool {
	return lastName(expr) == "CreateSpec"
}

// isNewCreateSpecName reports whether EXPR names NewCreateSpec, bare or
// qualified.
func isNewCreateSpecName(expr ast.Expr) bool {
	return lastName(expr) == "NewCreateSpec"
}

func lastName(expr ast.Expr) string {
	switch e := expr.(type) {
	case *ast.Ident:
		return e.Name
	case *ast.SelectorExpr:
		return e.Sel.Name
	}
	return ""
}
