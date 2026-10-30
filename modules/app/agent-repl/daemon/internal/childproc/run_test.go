package childproc

import (
	"go/ast"
	"go/token"
	"path/filepath"
	"strings"
	"testing"

	"agent-repld/internal/sourcescan"
)

func TestRun(t *testing.T) {
	tests := []struct {
		name        string
		script      string
		wantStdout  string
		wantStderr  string
		wantRunErr  bool
		wantStarted int
	}{
		{name: "a child that succeeds answers both streams", script: `printf out; printf err >&2`, wantStdout: "out", wantStderr: "err", wantStarted: 1},
		{name: "a child that fails answers both streams and its exit", script: `printf out; printf boom >&2; exit 3`, wantStdout: "out", wantStderr: "boom", wantRunErr: true, wantStarted: 1},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			cmd := Command(Stable, "/bin/sh", "-c", tt.script)
			started := 0

			// Act
			stdout, stderr, runErr, err := Run(cmd, func() { started++ })

			// Assert
			if err != nil || stdout != tt.wantStdout || stderr != tt.wantStderr || (runErr != nil) != tt.wantRunErr || started != tt.wantStarted {
				t.Fatalf("Run = %q, %q, %v, %v (started %d); want %q, %q, run error %v, started %d",
					stdout, stderr, runErr, err, started, tt.wantStdout, tt.wantStderr, tt.wantRunErr, tt.wantStarted)
			}
		})
	}
}

func TestRunOfAChildThatNeverStartsAnswersItsStartError(t *testing.T) {
	// Arrange
	cmd := Command(Stable, filepath.Join(t.TempDir(), "absent"))
	started := 0

	// Act
	stdout, stderr, runErr, err := Run(cmd, func() { started++ })

	// Assert
	if err != nil || runErr == nil || cmd.Process != nil || stdout != "" || stderr != "" || started != 0 {
		t.Fatalf("Run = %q, %q, %v, %v (process %v, started %d); want the start error alone and started never called",
			stdout, stderr, runErr, err, cmd.Process, started)
	}
}

func TestRunAcceptsNoStartedCallback(t *testing.T) {
	// Arrange
	cmd := Command(Stable, "/bin/sh", "-c", "printf ok")

	// Act
	stdout, _, runErr, err := Run(cmd, nil)

	// Assert
	if err != nil || runErr != nil || stdout != "ok" {
		t.Fatalf("Run = %q, %v, %v; want %q", stdout, runErr, err, "ok")
	}
}

func TestRunRefusesAnOutputThatCannotBeCaptured(t *testing.T) {
	// Arrange: no temporary directory exists to hold the output files.
	t.Setenv("TMPDIR", filepath.Join(t.TempDir(), "absent"))
	cmd := Command(Stable, "/bin/sh", "-c", "printf ok")

	// Act
	_, _, runErr, err := Run(cmd, nil)

	// Assert
	if err == nil || runErr != nil || cmd.Process != nil {
		t.Fatalf("Run = %v, %v (process %v); want the capture refused and nothing started", runErr, err, cmd.Process)
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
