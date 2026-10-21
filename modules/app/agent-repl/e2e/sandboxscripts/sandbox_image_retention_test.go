package sandboxscripts

import (
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"slices"
	"strings"
	"testing"
)

// IMAGE RETENTION. Every sandbox stamp names its own image and nothing ever
// removed one, until 226 GB of them filled the disk. A successful build now
// prunes all but the newest AGENT_REPL_SANDBOX_KEEP_IMAGES, never an image a
// live run holds or a container uses. These cases drive prune_images and
// hold_image against the fake runtime.

// Three images, newest first by build time; the listing order is deliberately
// not the age order, so the prune must sort.
var threeImages = []string{
	"sha256:bbb 2026-10-19T00:00:00Z",
	"sha256:ccc 2026-10-20T00:00:00Z",
	"sha256:aaa 2026-10-18T00:00:00Z",
}

// retentionEnv is sandboxEnv with the image lock and the holds in a private
// directory, which it returns with the environment.
func retentionEnv(t *testing.T, f *fakeDocker, extra ...string) ([]string, string) {
	t.Helper()
	dir := t.TempDir()
	return sandboxEnv(f, filepath.Join(dir, "build.lock"), extra...), dir
}

// pruneImages runs prune_images docker BUILT against the sourced script.
func pruneImages(t *testing.T, env []string, built string) *exec.Cmd {
	t.Helper()
	cmd := exec.Command("bash", "-c", `set -euo pipefail; script=$1; built=$2; . "$script"; prune_images docker "$built"`,
		"bash", sandboxScript(t), built)
	cmd.Env = env
	return cmd
}

// writeHold leaves the hold a run with PID makes on image ID.
func writeHold(t *testing.T, dir string, pid int, id string) string {
	t.Helper()
	holds := filepath.Join(dir, "holds")
	if err := os.MkdirAll(holds, 0o755); err != nil {
		t.Fatalf("mkdir holds: %v", err)
	}
	p := filepath.Join(holds, fmt.Sprintf("%d.%s", pid, id))
	if err := os.WriteFile(p, nil, 0o644); err != nil {
		t.Fatalf("write hold: %v", err)
	}
	return p
}

// TestPruneKeepsTheNewestImages: the newest KEEP images stay, the image just
// built stays whatever its age, and everything else is removed.
func TestPruneKeepsTheNewestImages(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name  string
		keep  string // AGENT_REPL_SANDBOX_KEEP_IMAGES; "" leaves the default
		built string
		want  []string
	}{
		{name: "the default keeps three", keep: "", built: "ccc", want: []string{"sha256:bbb", "sha256:ccc", "sha256:aaa"}},
		{name: "keep one removes the rest", keep: "1", built: "ccc", want: []string{"sha256:ccc"}},
		{name: "keep two removes the oldest", keep: "2", built: "ccc", want: []string{"sha256:bbb", "sha256:ccc"}},
		{name: "the image just built stays though older", keep: "1", built: "aaa", want: []string{"sha256:ccc", "sha256:aaa"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			// Arrange
			f := newFakeDocker(t)
			f.setImages(t, threeImages...)
			var extra []string
			if tc.keep != "" {
				extra = append(extra, "AGENT_REPL_SANDBOX_KEEP_IMAGES="+tc.keep)
			}
			env, _ := retentionEnv(t, f, extra...)

			// Act
			out, err := pruneImages(t, env, tc.built).CombinedOutput()

			// Assert
			if err != nil {
				t.Fatalf("prune_images = %v\n%s", err, out)
			}
			if got := f.images(t); !slices.Equal(got, tc.want) {
				t.Fatalf("images left = %q, want %q\n%s", got, tc.want, out)
			}
		})
	}
}

// TestPruneLogsEachRemovalAndTheTally: the shared log says which image went,
// when it was built, and how many were kept and removed.
func TestPruneLogsEachRemovalAndTheTally(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newFakeDocker(t)
	f.setImages(t, threeImages...)
	env, _ := retentionEnv(t, f, "AGENT_REPL_SANDBOX_KEEP_IMAGES=2")

	// Act
	out, err := pruneImages(t, env, "ccc").CombinedOutput()

	// Assert
	if err != nil {
		t.Fatalf("prune_images = %v\n%s", err, out)
	}
	for _, want := range []string{
		"e2e-sandbox: removing sandbox image aaa (built 2026-10-18T00:00:00Z)",
		"e2e-sandbox: sandbox images: kept 2, removed 1 (AGENT_REPL_SANDBOX_KEEP_IMAGES=2)",
	} {
		if !strings.Contains(string(out), want) {
			t.Fatalf("output lacks %q:\n%s", want, out)
		}
	}
}

// TestPruneKeepsWhatIsInUse: an image a live run holds, or a container still
// uses, is never removed, whatever its age.
func TestPruneKeepsWhatIsInUse(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name    string
		arrange func(t *testing.T, f *fakeDocker, dir string)
		wantLog string
	}{
		{
			name:    "an image a live run holds",
			arrange: func(t *testing.T, f *fakeDocker, dir string) { writeHold(t, dir, os.Getpid(), "aaa") },
			wantLog: "keeping sandbox image aaa (built 2026-10-18T00:00:00Z): a live run holds it",
		},
		{
			name:    "an image a container uses",
			arrange: func(t *testing.T, f *fakeDocker, dir string) { f.setUsers(t, "aaa", "c0ffee") },
			wantLog: "keeping sandbox image aaa (built 2026-10-18T00:00:00Z): container(s) c0ffee still use it",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			// Arrange
			f := newFakeDocker(t)
			f.setImages(t, threeImages...)
			env, dir := retentionEnv(t, f, "AGENT_REPL_SANDBOX_KEEP_IMAGES=1")
			tc.arrange(t, f, dir)

			// Act
			out, err := pruneImages(t, env, "ccc").CombinedOutput()

			// Assert: only the unused older image went.
			if err != nil {
				t.Fatalf("prune_images = %v\n%s", err, out)
			}
			if got, want := f.images(t), []string{"sha256:ccc", "sha256:aaa"}; !slices.Equal(got, want) {
				t.Fatalf("images left = %q, want %q\n%s", got, want, out)
			}
			if !strings.Contains(string(out), tc.wantLog) {
				t.Fatalf("output lacks %q:\n%s", tc.wantLog, out)
			}
		})
	}
}

// TestPruneReapsADeadRunsHold: a hold whose process is gone protects nothing;
// it is removed, and so is the image it named.
func TestPruneReapsADeadRunsHold(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newFakeDocker(t)
	f.setImages(t, threeImages...)
	env, dir := retentionEnv(t, f, "AGENT_REPL_SANDBOX_KEEP_IMAGES=2")
	hold := writeHold(t, dir, reapedPid(t), "aaa")

	// Act
	out, err := pruneImages(t, env, "ccc").CombinedOutput()

	// Assert
	if err != nil {
		t.Fatalf("prune_images = %v\n%s", err, out)
	}
	if _, statErr := os.Stat(hold); !os.IsNotExist(statErr) {
		t.Fatalf("the dead run's hold %s survived the prune", hold)
	}
	if slices.Contains(f.images(t), "sha256:aaa") {
		t.Fatalf("the image only a dead run held survived:\n%s", out)
	}
	if !strings.Contains(string(out), "reaping the image hold") {
		t.Fatalf("the reap was not logged:\n%s", out)
	}
}

// TestPruneFailsLoudly: a refused removal and a nonsense keep count each fail
// the build that pruned, with the cause in the shared log, and the build's
// exit leaves the image lock free.
func TestPruneFailsLoudly(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name    string
		arrange func(t *testing.T, f *fakeDocker)
		keep    string
		wantLog string
	}{
		{
			name: "the runtime refuses a removal",
			arrange: func(t *testing.T, f *fakeDocker) {
				if err := os.WriteFile(filepath.Join(f.dir, "rm-fails"), nil, 0o644); err != nil {
					t.Fatalf("write rm-fails: %v", err)
				}
			},
			keep:    "2",
			wantLog: "e2e-sandbox: removing sandbox image aaa FAILED",
		},
		{
			name:    "the keep count is not a positive whole number",
			arrange: func(t *testing.T, f *fakeDocker) {},
			keep:    "0",
			wantLog: "e2e-sandbox: AGENT_REPL_SANDBOX_KEEP_IMAGES must be a positive whole number of images, got '0'",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			// Arrange
			f := newFakeDocker(t)
			f.setImages(t, threeImages...)
			tc.arrange(t, f)
			env, dir := retentionEnv(t, f, "AGENT_REPL_SANDBOX_KEEP_IMAGES="+tc.keep)
			cmd := sandboxBuildUnstaged(t)
			cmd.Env = env

			// Act
			out, err := cmd.CombinedOutput()

			// Assert
			if err == nil {
				t.Fatalf("the build succeeded, want its prune to fail it:\n%s", out)
			}
			if !strings.Contains(string(out), tc.wantLog) {
				t.Fatalf("output lacks %q:\n%s", tc.wantLog, out)
			}
			if got := f.images(t); !slices.Contains(got, "sha256:aaa") {
				t.Fatalf("images left = %q, want the image whose removal failed still listed", got)
			}
			if _, statErr := os.Stat(filepath.Join(dir, "image.lock")); !os.IsNotExist(statErr) {
				t.Fatalf("the failed build left the image lock held")
			}
		})
	}
}

// TestBuildPrunesAfterBuilding: a build that produced an image prunes, and a
// build that found its image already built does not.
func TestBuildPrunesAfterBuilding(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name      string
		upToDate  bool
		wantPrune bool
	}{
		{name: "a build prunes", upToDate: false, wantPrune: true},
		{name: "an up-to-date image prunes nothing", upToDate: true, wantPrune: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			// Arrange
			f := newFakeDocker(t)
			if tc.upToDate {
				f.setLabel(t, currentSandboxStamp(t, f))
			}
			env, _ := retentionEnv(t, f)
			cmd := sandboxBuildUnstaged(t)
			cmd.Env = env

			// Act
			out, err := cmd.CombinedOutput()

			// Assert
			if err != nil {
				t.Fatalf("build = %v\n%s", err, out)
			}
			if got := strings.Contains(string(out), "sandbox images: kept"); got != tc.wantPrune {
				t.Fatalf("pruned = %v, want %v:\n%s", got, tc.wantPrune, out)
			}
		})
	}
}

// TestPruneAndHoldWaitForTheImageLock: the prune and a run's hold take the
// same lock, so neither proceeds while the other decides.
func TestPruneAndHoldWaitForTheImageLock(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name string
		call string
	}{
		{name: "the prune", call: `prune_images docker ccc`},
		{name: "a run's hold", call: `hold_image docker`},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			// Arrange: this test process holds the image lock.
			f := newFakeDocker(t)
			f.setImages(t, threeImages...)
			f.setLabel(t, currentSandboxStamp(t, f))
			env, dir := retentionEnv(t, f, "AGENT_REPL_SANDBOX_KEEP_IMAGES=1")
			lock := filepath.Join(dir, "image.lock")
			holdLock(t, lock)
			cmd := exec.Command("bash", "-c", `set -euo pipefail; script=$1; . "$script"; `+tc.call, "bash", sandboxScript(t))
			cmd.Env = env

			// Act
			out, err := waitBehindLock(t, cmd, lock, "WAITING: a sandbox run or prune holds the host image lock", func() {
				// Assert: nothing was removed or held while it waited.
				if got := f.images(t); len(got) != 3 {
					t.Fatalf("images changed while waiting: %q", got)
				}
				if _, statErr := os.Stat(filepath.Join(dir, "holds")); !os.IsNotExist(statErr) {
					t.Fatalf("a hold was made while waiting")
				}
			})

			// Assert
			if err != nil {
				t.Fatalf("%s = %v\n%s", tc.call, err, out)
			}
		})
	}
}

// TestRunStartsFromTheHeldImageAndDropsTheHold: the container starts from the
// held image ID, not the tag, and the hold is gone once the run ends.
func TestRunStartsFromTheHeldImageAndDropsTheHold(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newFakeDocker(t)
	f.setLabel(t, currentSandboxStamp(t, f))
	env, dir := retentionEnv(t, f, "AGENT_REPL_SANDBOX_SLOT_DIR="+t.TempDir(), "AGENT_REPL_SANDBOX_NO_WEBAPP_BUILD=1")
	cmd := exec.Command("bash", sandboxScript(t), "run", "true")
	cmd.Env = env

	// Act
	out, err := cmd.CombinedOutput()

	// Assert
	if err != nil {
		t.Fatalf("run = %v\n%s", err, out)
	}
	idx := slices.IndexFunc(f.calls(t), func(c string) bool { return strings.HasPrefix(c, "run ") })
	if idx < 0 || !strings.HasSuffix(f.calls(t)[idx], " fakeimage true") {
		t.Fatalf("calls = %q, want the container started from the held ID fakeimage", f.calls(t))
	}
	if holds := filesUnder(t, filepath.Join(dir, "holds")); len(holds) != 0 {
		t.Fatalf("holds left after the run: %q", holds)
	}
}

// TestRunRefusingAStaleImageHoldsNothing: a refused run leaves no hold and
// the image lock free, and its refusal is in the log.
func TestRunRefusingAStaleImageHoldsNothing(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newFakeDocker(t)
	f.setLabel(t, "inputs-stale")
	env, dir := retentionEnv(t, f, "AGENT_REPL_SANDBOX_SLOT_DIR="+t.TempDir(), "AGENT_REPL_SANDBOX_NO_WEBAPP_BUILD=1")
	cmd := exec.Command("bash", sandboxScript(t), "run", "true")
	cmd.Env = env

	// Act
	out, err := cmd.CombinedOutput()

	// Assert
	var exitErr *exec.ExitError
	if !errors.As(err, &exitErr) || exitErr.ExitCode() != 3 {
		t.Fatalf("run = %v, want the stale-image refusal (exit 3)\n%s", err, out)
	}
	if !strings.Contains(string(out), "REFUSING TO RUN") {
		t.Fatalf("the refusal was not logged:\n%s", out)
	}
	if _, statErr := os.Stat(filepath.Join(dir, "holds")); !os.IsNotExist(statErr) {
		if holds := filesUnder(t, filepath.Join(dir, "holds")); len(holds) != 0 {
			t.Fatalf("a refused run left holds: %q", holds)
		}
	}
	if _, statErr := os.Stat(filepath.Join(dir, "image.lock")); !os.IsNotExist(statErr) {
		t.Fatalf("a refused run left the image lock held")
	}
}
