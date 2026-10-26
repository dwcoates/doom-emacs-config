package account

import (
	"context"
	"errors"
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agent-repld/internal/ids"
)

// routeAll is a Resolver that routes every directory to one root.
type routeAll struct {
	Resolver
	root string
}

func (r routeAll) ConfigDirFor(string) string { return r.root }

func TestWorkspaceRootsAnswersTheWorkspacesAccountRoot(t *testing.T) {
	// Arrange.
	roots := WorkspaceRoots{
		Dir:      func(context.Context, ids.WorkspaceID) (string, error) { return "/tree/w1", nil },
		Accounts: routeAll{root: "/acct"},
	}

	// Act.
	got, err := roots.ConfigDirFor(context.Background(), "w1")

	// Assert.
	if err != nil || got != "/acct" {
		t.Fatalf("ConfigDirFor = %q, %v; want /acct", got, err)
	}
}

func TestWorkspaceRootsReadsOnTheCallersContext(t *testing.T) {
	// Arrange: the lookup answers the context it was handed.
	roots := WorkspaceRoots{
		Dir:      func(ctx context.Context, _ ids.WorkspaceID) (string, error) { return "", ctx.Err() },
		Accounts: routeAll{root: "/acct"},
	}
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	_, err := roots.ConfigDirFor(ctx, "w1")

	// Assert.
	if !errors.Is(err, context.Canceled) || !strings.Contains(err.Error(), `"w1"`) {
		t.Fatalf("err = %v, want the caller's ended context, naming the workspace", err)
	}
}

// TestNoOtherWorkspaceConfigDirsInterface fails on a daemon package that
// declares its own interface with WorkspaceConfigDirs' method instead of
// taking this one.
func TestNoOtherWorkspaceConfigDirsInterface(t *testing.T) {
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
