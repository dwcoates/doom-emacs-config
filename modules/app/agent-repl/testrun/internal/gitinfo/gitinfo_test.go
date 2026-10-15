package gitinfo

import (
	"go/ast"
	"go/parser"
	"go/token"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// fakeGit points the package at a shell script answering each git command
// with canned output, and recording every argv it was given.
func fakeGit(t *testing.T, script string) string {
	t.Helper()
	dir := t.TempDir()
	log := filepath.Join(dir, "calls")
	body := "#!/bin/sh\necho \"$*\" >> " + log + "\n" + script
	p := filepath.Join(dir, "git")
	if err := os.WriteFile(p, []byte(body), 0o755); err != nil {
		t.Fatal(err)
	}
	old := gitBinary
	gitBinary = p
	t.Cleanup(func() { gitBinary = old })
	return log
}

const answers = `case "$3" in
  branch) echo feature ;;
  rev-parse) echo c0ffee ;;
  status) printf "%s" "$STATUS" ;;
esac
`

func TestHead(t *testing.T) {
	// Arrange
	log := fakeGit(t, answers)

	// Act
	branch, commit, err := Head("/repo")

	// Assert
	if err != nil || branch != "feature" || commit != "c0ffee" {
		t.Fatalf("Head = %q, %q, %v", branch, commit, err)
	}
	calls, _ := os.ReadFile(log)
	if want := "-C /repo branch --show-current\n-C /repo rev-parse HEAD\n"; string(calls) != want {
		t.Fatalf("calls = %q, want %q", calls, want)
	}
}

func TestDirty(t *testing.T) {
	tests := []struct {
		name   string
		status string
		want   bool
	}{
		{"a clean checkout", "", false},
		{"a changed file", " M a.go\n", true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			fakeGit(t, answers)
			t.Setenv("STATUS", tt.status)

			// Act
			got, err := Dirty("/repo")

			// Assert
			if err != nil || got != tt.want {
				t.Fatalf("Dirty = %v, %v; want %v", got, err, tt.want)
			}
		})
	}
}

func TestAFailingGitIsNamedWithWhatItPrinted(t *testing.T) {
	// Arrange
	fakeGit(t, "echo 'fatal: not a git repository' >&2\nexit 128\n")

	// Act
	_, _, err := Head("/repo")

	// Assert
	if err == nil || !strings.Contains(err.Error(), "git branch --show-current") || !strings.Contains(err.Error(), "not a git repository") {
		t.Fatalf("err = %v, want the command and git's message", err)
	}
}

// TestTheRunnerAsksGitOnlyThroughGitinfo fails a runner file that starts git
// itself: every question shares Output's error shape.
func TestTheRunnerAsksGitOnlyThroughGitinfo(t *testing.T) {
	// Arrange
	root := filepath.Join("..", "..")
	var offenders []string

	// Act
	err := filepath.WalkDir(root, func(p string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(p, ".go") || strings.HasSuffix(p, "_test.go") || strings.Contains(p, "gitinfo") {
			return nil
		}
		f, err := parser.ParseFile(token.NewFileSet(), p, nil, 0)
		if err != nil {
			return err
		}
		ast.Inspect(f, func(n ast.Node) bool {
			call, ok := n.(*ast.CallExpr)
			if !ok || len(call.Args) == 0 {
				return true
			}
			sel, ok := call.Fun.(*ast.SelectorExpr)
			if !ok || sel.Sel.Name != "Command" {
				return true
			}
			if lit, ok := call.Args[0].(*ast.BasicLit); ok && lit.Value == `"git"` {
				offenders = append(offenders, p)
			}
			return true
		})
		return nil
	})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if len(offenders) > 0 {
		t.Fatalf("these files run git themselves; ask through gitinfo: %v", offenders)
	}
}
