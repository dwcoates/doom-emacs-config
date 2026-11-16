package stack

import (
	"context"
	"go/ast"
	"go/parser"
	"go/token"
	"io/fs"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"agent-repld/integration/grantedclock"
)

func TestAwaitPIDsGone(t *testing.T) {
	cases := []struct {
		name string
		// exit makes each process exit before the wait.
		exit bool
		// bound is the wait's bound: the fallback for an exit, and the
		// subject itself for processes that never exit.
		bound   time.Duration
		wantErr string
	}{
		{name: "processes that exit are reported gone", exit: true, bound: DefaultTimeout},
		{name: "processes still running at the bound are named", bound: 200 * time.Millisecond, wantErr: "still running"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: two processes this wait does not reap.
			var pids []int
			for range 2 {
				cmd := exec.Command("/bin/sh", "-c", "read _")
				stdin, err := cmd.StdinPipe()
				if err != nil {
					t.Fatalf("stdin: %v", err)
				}
				if err := cmd.Start(); err != nil {
					t.Fatalf("start: %v", err)
				}
				reaped := make(chan struct{})
				go func() { _ = cmd.Wait(); close(reaped) }()
				t.Cleanup(func() { _ = stdin.Close(); <-reaped })
				if tc.exit {
					_ = stdin.Close()
				}
				pids = append(pids, cmd.Process.Pid)
			}
			ctx, cancel := grantedclock.WithBound(context.Background(), tc.bound)
			defer cancel()

			// Act
			err := AwaitPIDsGone(ctx, pids)

			// Assert
			switch {
			case tc.wantErr == "" && err != nil:
				t.Fatalf("AwaitPIDsGone = %v, want gone", err)
			case tc.wantErr != "" && (err == nil || !strings.Contains(err.Error(), tc.wantErr)):
				t.Fatalf("AwaitPIDsGone = %v, want an error naming %q", err, tc.wantErr)
			}
		})
	}
}

// No e2e source polls a pid with kill(pid, 0) in a loop: a wait on a process
// is its exit event (AwaitPIDsGone).
func TestNoE2ESourcePollsAPIDInALoop(t *testing.T) {
	// Arrange
	root, err := filepath.Abs(filepath.Join("..", ".."))
	if err != nil {
		t.Fatalf("the e2e root: %v", err)
	}
	fset := token.NewFileSet()

	// Act
	var polls []string
	walkErr := filepath.WalkDir(root, func(path string, entry fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if entry.IsDir() || !strings.HasSuffix(path, ".go") {
			return nil
		}
		file, err := parser.ParseFile(fset, path, nil, 0)
		if err != nil {
			return err
		}
		ast.Inspect(file, func(n ast.Node) bool {
			loop, ok := n.(*ast.ForStmt)
			if !ok {
				return true
			}
			ast.Inspect(loop.Body, func(n ast.Node) bool {
				if isKillZero(n) {
					polls = append(polls, fset.Position(n.Pos()).String())
				}
				return true
			})
			return false
		})
		return nil
	})

	// Assert
	if walkErr != nil {
		t.Fatalf("walk %s: %v", root, walkErr)
	}
	if len(polls) != 0 {
		t.Fatalf("kill(pid, 0) polled in a loop at %v; wait on the exit with AwaitPIDsGone", polls)
	}
}

// isKillZero reports whether N is a `syscall.Kill(X, 0)` call.
func isKillZero(n ast.Node) bool {
	call, ok := n.(*ast.CallExpr)
	if !ok || len(call.Args) != 2 {
		return false
	}
	sel, ok := call.Fun.(*ast.SelectorExpr)
	if !ok || sel.Sel.Name != "Kill" {
		return false
	}
	if pkg, ok := sel.X.(*ast.Ident); !ok || pkg.Name != "syscall" {
		return false
	}
	lit, ok := call.Args[1].(*ast.BasicLit)
	return ok && lit.Value == "0"
}
