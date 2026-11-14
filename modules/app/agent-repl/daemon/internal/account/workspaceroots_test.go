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
		Read: func(context.Context, ids.WorkspaceID) (WorkspaceAccount, error) {
			return WorkspaceAccount{Dir: "/tree/w1"}, nil
		},
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
		Read: func(ctx context.Context, _ ids.WorkspaceID) (WorkspaceAccount, error) {
			return WorkspaceAccount{}, ctx.Err()
		},
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

func TestWorkspaceRootsBillsTheChosenAccountOverTheRoutedOne(t *testing.T) {
	// Arrange: the reader picked another account for the workspace.
	roots := WorkspaceRoots{
		Read: func(context.Context, ids.WorkspaceID) (WorkspaceAccount, error) {
			return WorkspaceAccount{Dir: "/tree/w1", SelectedConfigDir: "/acct-chosen"}, nil
		},
		Accounts: routeAll{root: "/acct-routed"},
	}

	// Act.
	got, err := roots.ConfigDirFor(context.Background(), "w1")

	// Assert.
	if err != nil || got != "/acct-chosen" {
		t.Fatalf("ConfigDirFor = %q, %v; want the chosen /acct-chosen", got, err)
	}
}

func TestRootFor(t *testing.T) {
	tests := []struct {
		name     string
		selected string
		want     string
	}{
		{name: "a choice outranks the routing", selected: "/acct-chosen", want: "/acct-chosen"},
		{name: "no choice follows the routing", selected: "", want: "/acct-routed"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			accounts := routeAll{root: "/acct-routed"}

			// Act.
			got := RootFor(accounts, "/tree/w1", tt.selected)

			// Assert.
			if got != tt.want {
				t.Fatalf("RootFor = %q, want %q", got, tt.want)
			}
		})
	}
}

// TestNoOtherSelectedConfigDirPrecedence fails on a daemon source outside
// this file that decides the chosen-over-routed precedence itself
// (`SelectedConfigDir != ""`) instead of calling RootFor.
func TestNoOtherSelectedConfigDirPrecedence(t *testing.T) {
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
