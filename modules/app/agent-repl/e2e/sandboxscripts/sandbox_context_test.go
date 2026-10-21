package sandboxscripts

import (
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"slices"
	"strings"
	"testing"
)

// The BUILD context (stage_context in sandbox/bin/e2e-sandbox.sh) is what the
// image's layers are keyed by. A file staged into it that no layer needs
// still rebuilds that layer, and every layer after it, whenever it changes.

// stagedContext runs the script's own stage_context into a fresh directory,
// against this checkout, and returns that directory.
func stagedContext(t *testing.T) string {
	t.Helper()
	f := newFakeDocker(t)
	ctx := t.TempDir()
	cmd := exec.Command("bash", "-c", `set -euo pipefail; script=$1; ctx=$2; . "$script"; stage_context "$ctx"`,
		"bash", sandboxScript(t), ctx)
	cmd.Env = sandboxEnv(f, filepath.Join(t.TempDir(), "build.lock"))
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("stage_context = %v\n%s", err, out)
	}
	return ctx
}

// filesUnder lists every regular file under ROOT, relative to it.
func filesUnder(t *testing.T, root string) []string {
	t.Helper()
	var files []string
	err := filepath.WalkDir(root, func(p string, d fs.DirEntry, err error) error {
		if err != nil || d.IsDir() {
			return err
		}
		rel, err := filepath.Rel(root, p)
		files = append(files, rel)
		return err
	})
	if err != nil {
		t.Fatalf("walk %s: %v", root, err)
	}
	return files
}

// TestTheBuildContextStagesOnlyPackagesElOfTheModule: the `doom sync --aot`
// layer is keyed by module-loader-files, and the node and Go layers sit after
// it. Staging the module's elisp there rebuilt all of them on every elisp
// edit, for files a run never reads (it links the checkout's module in).
func TestTheBuildContextStagesOnlyPackagesElOfTheModule(t *testing.T) {
	t.Parallel()
	// Arrange
	ctx := stagedContext(t)

	// Act
	got := filesUnder(t, filepath.Join(ctx, "module-loader-files"))

	// Assert
	if !slices.Equal(got, []string{"packages.el"}) {
		t.Fatalf("module-loader-files = %q, want only packages.el", got)
	}
}

// TestSlotBudgetIsOneWhenTheVMMemoryIsUnknown: a gate that could not read the
// VM's memory opens to one container, never wide.
func TestSlotBudgetIsOneWhenTheVMMemoryIsUnknown(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name     string
		memTotal string
	}{
		{name: "no answer", memTotal: ""},
		{name: "a zero answer", memTotal: "0"},
		{name: "a non-numeric answer", memTotal: "<no value>"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			// Arrange
			f := newFakeDocker(t)
			if err := os.WriteFile(filepath.Join(f.dir, "info"), []byte(tc.memTotal+"\n"), 0o644); err != nil {
				t.Fatalf("write info: %v", err)
			}
			cmd := exec.Command("bash", "-c", `set -euo pipefail; script=$1; . "$script"; slot_budget docker`, "bash", sandboxScript(t))
			cmd.Env = sandboxEnv(f, filepath.Join(t.TempDir(), "build.lock"))

			// Act
			out, err := cmd.Output()

			// Assert
			if err != nil {
				t.Fatalf("slot_budget = %v", err)
			}
			if got := strings.TrimSpace(string(out)); got != "1" {
				t.Fatalf("slot_budget = %q, want 1", got)
			}
		})
	}
}
