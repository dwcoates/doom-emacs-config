package sandboxscripts

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// stampTree lays out a module the stamp can be computed over: the real
// sandbox-image.sh at its real relative path, and one of every input kind.
func stampTree(t *testing.T) (module string) {
	t.Helper()
	module = filepath.Join(t.TempDir(), "modules", "app", "agent-repl")
	lib, err := os.ReadFile(filepath.Join(filepath.Dir(sandboxScript(t)), "sandbox-image.sh"))
	if err != nil {
		t.Fatal(err)
	}
	files := map[string]string{
		"e2e/sandbox/bin/sandbox-image.sh":         string(lib),
		"e2e/sandbox/Dockerfile":                   "FROM scratch\n",
		"agent-shim/claude/shim/package.json":      `{"name":"shim"}`,
		"agent-shim/claude/shim/package-lock.json": `{"lockfileVersion":3}`,
		"webapp/package.json":                      `{"name":"webapp"}`,
		"webapp/package-lock.json":                 `{"lockfileVersion":3}`,
		"packages.el":                              ";; no packages\n",
		"daemon/go.mod":                            "module agent-repld\n",
		"daemon/go.sum":                            "",
		"daemon/main.go":                           "package main\n",
		"lisp/core.el":                             ";; core\n",
		"webapp/node_modules/x/go.mod":             "module x\n",
	}
	for rel, body := range files {
		writeFile(t, filepath.Join(module, rel), body)
	}
	return module
}

func writeFile(t *testing.T, path, body string) {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, []byte(body), 0o644); err != nil {
		t.Fatal(err)
	}
}

// stampOf sources the tree's sandbox-image.sh and answers its sandbox_stamp.
func stampOf(t *testing.T, module string) (string, error) {
	t.Helper()
	lib := filepath.Join(module, "e2e", "sandbox", "bin", "sandbox-image.sh")
	out, err := exec.Command("bash", "-c", fmt.Sprintf(`set -euo pipefail; . %q; sandbox_stamp`, lib)).CombinedOutput()
	return strings.TrimSpace(string(out)), err
}

func TestSandboxStamp(t *testing.T) {
	tests := []struct {
		name     string
		edit     string
		wantSame bool
	}{
		{name: "a shim lockfile change names a new image", edit: "agent-shim/claude/shim/package-lock.json"},
		{name: "a webapp manifest change names a new image", edit: "webapp/package.json"},
		{name: "a go.sum change names a new image", edit: "daemon/go.sum"},
		{name: "a packages.el change names a new image", edit: "packages.el"},
		{name: "a sandbox source change names a new image", edit: "e2e/sandbox/Dockerfile"},
		{name: "a Go source change keeps the image", edit: "daemon/main.go", wantSame: true},
		{name: "an elisp change keeps the image", edit: "lisp/core.el", wantSame: true},
		{name: "a go.mod under node_modules keeps the image", edit: "webapp/node_modules/x/go.mod", wantSame: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			module := stampTree(t)
			before, err := stampOf(t, module)
			if err != nil {
				t.Fatalf("stamp: %v: %s", err, before)
			}

			// Act
			writeFile(t, filepath.Join(module, tt.edit), "changed\n")
			after, err := stampOf(t, module)
			if err != nil {
				t.Fatalf("stamp: %v: %s", err, after)
			}

			// Assert
			if (before == after) != tt.wantSame {
				t.Fatalf("stamp %s -> %s after editing %s; want same=%v", before, after, tt.edit, tt.wantSame)
			}
		})
	}
}

func TestSandboxStampIsTheSameForTheSameInputs(t *testing.T) {
	// Arrange
	first, second := stampTree(t), stampTree(t)

	// Act
	a, errA := stampOf(t, first)
	b, errB := stampOf(t, second)

	// Assert
	if errA != nil || errB != nil || a != b || !strings.HasPrefix(a, "inputs-") {
		t.Fatalf("stamps %q (%v) and %q (%v), want one equal inputs- digest for two identical trees", a, errA, b, errB)
	}
}

func TestSandboxStampRefusesAMissingInput(t *testing.T) {
	// Arrange
	module := stampTree(t)
	if err := os.Remove(filepath.Join(module, "webapp", "package-lock.json")); err != nil {
		t.Fatal(err)
	}

	// Act
	out, err := stampOf(t, module)

	// Assert
	if err == nil || !strings.Contains(out, "the stamp input webapp/package-lock.json is missing") {
		t.Fatalf("stamp = %q, %v; want a refusal naming the missing lockfile", out, err)
	}
}
