package reposcans

import (
	"os"
	"os/exec"
	"path/filepath"
	"slices"
	"strings"
	"testing"

	"agent-repld/internal/merge"
	"agentrepl/testrun/roster"
)

// shellcheckOutput runs bin/shellcheck.sh with arg and answers its lines.
func shellcheckOutput(t *testing.T, arg string) []string {
	t.Helper()
	cmd := exec.Command(filepath.Join(moduleRootDir(t), "bin", "shellcheck.sh"), arg)
	out, err := cmd.Output()
	if err != nil {
		t.Fatalf("bin/shellcheck.sh %s: %v", arg, err)
	}
	return strings.Fields(string(out))
}

// TestTheShellcheckSuiteDeclaresTheRootsItsScriptWalks holds the roster's
// reuse declaration for the shellcheck suite to the roots its script reads: a
// root the script walks but the roster does not declare would be refused by
// the unit's confinement (an empty listing), and one the roster declares but
// the script never walks would rerun the suite for nothing.
func TestTheShellcheckSuiteDeclaresTheRootsItsScriptWalks(t *testing.T) {
	// Arrange.
	suite, ok := roster.Lookup("shellcheck")
	if !ok || suite.Reuse == nil {
		t.Fatal("the roster has no shellcheck suite declaring its inputs")
	}

	// Act.
	roots := shellcheckOutput(t, "--roots")

	// Assert.
	if !slices.Equal(roots, roster.ShellcheckRoots) || !slices.Equal(suite.Reuse.Inputs, roster.ShellcheckRoots) {
		t.Errorf("bin/shellcheck.sh walks %v; roster.ShellcheckRoots is %v and the suite declares %v", roots, roster.ShellcheckRoots, suite.Reuse.Inputs)
	}
}

// TestEveryShellScriptSelectsTheShellcheckSuite checks a change to any script
// the shellcheck gate lints selects that gate, so no script's warning can land
// through a merge whose gate never ran it.
func TestEveryShellScriptSelectsTheShellcheckSuite(t *testing.T) {
	// Arrange.
	files := shellcheckOutput(t, "--files")
	if len(files) == 0 {
		t.Fatal("bin/shellcheck.sh --files listed no script")
	}

	for _, file := range files {
		// Act.
		got := merge.SelectSuites([]string{file})

		// Assert.
		if !got.Full && !slices.Contains(got.Suites, "shellcheck") {
			t.Errorf("a change to %s selects %v, which leaves out shellcheck", file, got.Suites)
		}
	}
}

// TestEveryByteCompiledSourceSelectsTheByteCompileSuite checks a change to any
// source the byte-compile gate compiles, or to the gate and its prelude,
// selects that gate.
func TestEveryByteCompiledSourceSelectsTheByteCompileSuite(t *testing.T) {
	// Arrange.
	root := moduleRootDir(t)
	sources, err := filepath.Glob(filepath.Join(root, "lisp", "*.el"))
	if err != nil {
		t.Fatal(err)
	}
	paths := []string{moduleRoot + "bin/byte-compile.sh", moduleRoot + "bin/compile-prelude.el"}
	for _, src := range sources {
		if !strings.HasPrefix(filepath.Base(src), "test-") {
			paths = append(paths, moduleRoot+"lisp/"+filepath.Base(src))
		}
	}
	if len(paths) == 2 {
		t.Fatal("lisp/ holds no source; the glob read nothing")
	}
	if _, err := os.Stat(filepath.Join(root, "bin", "byte-compile.sh")); err != nil {
		t.Fatalf("the byte-compile gate is gone: %v", err)
	}

	for _, path := range paths {
		// Act.
		got := merge.SelectSuites([]string{path})

		// Assert.
		if !got.Full && !slices.Contains(got.Suites, "byte-compile") {
			t.Errorf("a change to %s selects %v, which leaves out byte-compile", path, got.Suites)
		}
	}
}
