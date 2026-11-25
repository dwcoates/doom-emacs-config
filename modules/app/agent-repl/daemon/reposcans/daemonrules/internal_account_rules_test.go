package daemonrules

import (
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalAccountDir is the package these rules hold, under the daemon module's root.
const internalAccountDir = "internal/account"

// TestNoOtherSelectedConfigDirPrecedence fails on a daemon source outside
// this file that decides the chosen-over-routed precedence itself
// (`SelectedConfigDir != ""`) instead of calling RootFor.
func TestNoOtherSelectedConfigDirPrecedence(t *testing.T) {
	sourcescan.InPackage(t, internalAccountDir)
	root := filepath.Join("..", "..")
	err := filepath.WalkDir(root, func(path string, d os.DirEntry, err error) error {
		if err != nil || d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return err
		}
		if filepath.Base(path) == "workspaceroots.go" && filepath.Base(filepath.Dir(path)) == "account" {
			return nil
		}
		src, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if strings.Contains(string(src), `SelectedConfigDir != ""`) {
			t.Errorf("%s decides the chosen-over-routed precedence itself; call account.RootFor", path)
		}
		return nil
	})
	if err != nil {
		t.Fatal(err)
	}
}

// TestNoOtherWorkspaceConfigDirsInterface fails on a daemon package that
// declares its own interface with WorkspaceConfigDirs' method instead of
// taking this one.
func TestNoOtherWorkspaceConfigDirsInterface(t *testing.T) {
	sourcescan.InPackage(t, internalAccountDir)
	root := filepath.Join("..", "..")
	fset := token.NewFileSet()
	err := filepath.WalkDir(root, func(path string, d os.DirEntry, err error) error {
		if err != nil || d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return err
		}
		if filepath.Base(path) == "workspaceroots.go" && filepath.Base(filepath.Dir(path)) == "account" {
			return nil
		}
		file, err := parser.ParseFile(fset, path, nil, 0)
		if err != nil {
			return err
		}
		ast.Inspect(file, func(n ast.Node) bool {
			spec, ok := n.(*ast.TypeSpec)
			if !ok {
				return true
			}
			iface, ok := spec.Type.(*ast.InterfaceType)
			if !ok {
				return true
			}
			for _, m := range iface.Methods.List {
				fn, ok := m.Type.(*ast.FuncType)
				if ok && len(m.Names) == 1 && m.Names[0].Name == "ConfigDirFor" && len(fn.Params.List) == 2 {
					t.Errorf("%s declares interface %s with ConfigDirFor(ctx, ws); take account.WorkspaceConfigDirs", path, spec.Name.Name)
				}
			}
			return true
		})
		return nil
	})
	if err != nil {
		t.Fatal(err)
	}
}
