package sandboxscripts

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
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

// goModuleFilesOf sources the tree's sandbox-image.sh and answers the
// go.mod and go.sum files its stamp walk lists, sorted.
func goModuleFilesOf(t *testing.T, module string) []string {
	t.Helper()
	lib := filepath.Join(module, "e2e", "sandbox", "bin", "sandbox-image.sh")
	cmd := exec.Command("bash", "-c", fmt.Sprintf(`set -euo pipefail; . %q; sandbox_go_module_files`, lib))
	cmd.Dir = module
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("sandbox_go_module_files: %v: %s", err, out)
	}
	var files []string
	for f := range strings.SplitSeq(strings.TrimRight(string(out), "\x00"), "\x00") {
		if f != "" {
			files = append(files, f)
		}
	}
	sort.Strings(files)
	return files
}

func TestSandboxGoModuleWalk(t *testing.T) {
	tests := []struct {
		name  string
		setup func(t *testing.T, module string)
		want  []string
	}{
		{
			name: "lists every go.mod and go.sum outside node_modules",
			want: []string{"./daemon/go.mod", "./daemon/go.sum"},
		},
		{
			name: "lists a module under a dot directory",
			setup: func(t *testing.T, module string) {
				writeFile(t, filepath.Join(module, ".tools", "go.mod"), "module tools\n")
			},
			want: []string{"./.tools/go.mod", "./daemon/go.mod", "./daemon/go.sum"},
		},
		{
			name: "does not follow a symlinked directory",
			setup: func(t *testing.T, module string) {
				if err := os.Symlink(filepath.Join(module, "daemon"), filepath.Join(module, "daemon-link")); err != nil {
					t.Fatal(err)
				}
			},
			want: []string{"./daemon/go.mod", "./daemon/go.sum"},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			module := stampTree(t)
			if tt.setup != nil {
				tt.setup(t, module)
			}

			// Act
			got := goModuleFilesOf(t, module)

			// Assert
			if strings.Join(got, "\n") != strings.Join(tt.want, "\n") {
				t.Fatalf("walk listed %q, want %q", got, tt.want)
			}
		})
	}
}

// TestSandboxStampNeverRunsFind pins the stamp's walk off find: the stamp runs
// on the live checkout while other suites of the same run write into it, and
// find fails the whole walk when a listed entry vanishes before it is
// stat'ed (npm's .node_modules.lock, 2026-10-16). It also pins the walk off
// array pops, which killed a gate's stamp with a bus error the same day.
func TestSandboxStampNeverRunsFind(t *testing.T) {
	// Arrange
	src, err := os.ReadFile(filepath.Join(filepath.Dir(sandboxScript(t)), "sandbox-image.sh"))
	if err != nil {
		t.Fatal(err)
	}

	// Act
	var calls []string
	for i, line := range strings.Split(string(src), "\n") {
		code := strings.TrimSpace(line)
		if strings.HasPrefix(code, "#") {
			continue
		}
		if strings.HasPrefix(code, "find ") || strings.Contains(code, "| find ") || strings.Contains(code, "$(find ") {
			calls = append(calls, fmt.Sprintf("%d: %s", i+1, code))
		}
		if strings.HasPrefix(code, "unset '") {
			calls = append(calls, fmt.Sprintf("%d: %s (an array pop, which killed a gate's stamp with a bus error)", i+1, code))
		}
	}

	// Assert
	if len(calls) != 0 {
		t.Fatalf("sandbox-image.sh walks in a way a gate can fail:\n%s", strings.Join(calls, "\n"))
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
