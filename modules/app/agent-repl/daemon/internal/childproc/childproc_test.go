package childproc

import (
	"context"
	"go/ast"
	"go/parser"
	"go/token"
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

func TestBuildersRunInTheNamedDirectory(t *testing.T) {
	builders := map[string]func(dir string) *exec.Cmd{
		"Command":        func(dir string) *exec.Cmd { return Command(dir, "true") },
		"CommandContext": func(dir string) *exec.Cmd { return CommandContext(context.Background(), dir, "true") },
	}
	cases := []struct {
		name    string
		dir     string
		wantErr string
	}{
		{name: "an absolute directory is the child's", dir: "/tmp"},
		{name: "the stable directory is the child's", dir: Stable},
		{name: "an empty directory is refused", dir: "", wantErr: `not ""`},
		{name: "a relative directory is refused", dir: "worktree", wantErr: `not "worktree"`},
	}
	for builder, build := range builders {
		for _, tc := range cases {
			t.Run(builder+"/"+tc.name, func(t *testing.T) {
				// Act
				cmd := build(tc.dir)

				// Assert
				if tc.wantErr == "" {
					if cmd.Err != nil || cmd.Dir != tc.dir {
						t.Fatalf("cmd.Dir = %q, cmd.Err = %v; want %q and no error", cmd.Dir, cmd.Err, tc.dir)
					}
					return
				}
				if cmd.Dir != "" || cmd.Err == nil || !strings.Contains(cmd.Err.Error(), tc.wantErr) {
					t.Fatalf("cmd.Dir = %q, cmd.Err = %v; want no directory and an error containing %q", cmd.Dir, cmd.Err, tc.wantErr)
				}
			})
		}
	}
}

func TestARefusedDirectoryNeverStarts(t *testing.T) {
	// Arrange
	cmd := Command("", "true")

	// Act
	err := cmd.Start()

	// Assert
	if err == nil || cmd.Process != nil {
		t.Fatalf("Start = %v with process %v; want the refusal and no process", err, cmd.Process)
	}
}

func TestAChildRunsInItsDirectoryWhenTheDaemonsIsDeleted(t *testing.T) {
	// Arrange: the test process stands in a directory that is then removed,
	// as the daemon stood in a landed workspace's worktree.
	gone := t.TempDir()
	t.Chdir(gone)
	if err := os.Remove(gone); err != nil {
		t.Fatalf("remove the working directory: %v", err)
	}

	// Act
	out, err := Command(Stable, "pwd").Output()

	// Assert
	if err != nil || strings.TrimSpace(string(out)) != Stable {
		t.Fatalf("pwd = %q, %v; want %q", out, err, Stable)
	}
}

// TestEveryChildIsBuiltHere holds every daemon source outside this package to
// Command and CommandContext: a site calling os/exec's builders directly
// would start its child wherever the daemon stands.
func TestEveryChildIsBuiltHere(t *testing.T) {
	// Arrange
	root, err := filepath.Abs("../..")
	if err != nil {
		t.Fatalf("resolve the daemon root: %v", err)
	}
	var offenders []string
	fset := token.NewFileSet()

	// Act
	err = filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		rel, _ := filepath.Rel(root, path)
		if d.IsDir() {
			// The integration and e2e harnesses start the daemon under test,
			// not a child of a serving daemon.
			if rel == "integration" || rel == "e2e" || rel == "internal/childproc" || strings.HasPrefix(d.Name(), ".") {
				return filepath.SkipDir
			}
			return nil
		}
		if !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		file, err := parser.ParseFile(fset, path, nil, parser.SkipObjectResolution)
		if err != nil {
			return err
		}
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
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("walk the daemon sources: %v", err)
	}
	if len(offenders) > 0 {
		t.Fatalf("these sites build a child with os/exec directly; use childproc.Command or childproc.CommandContext:\n%s", strings.Join(offenders, "\n"))
	}
}
