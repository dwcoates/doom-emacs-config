package sandboxscripts

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
	"testing"
)

// claim_host_lock is the one host-wide mkdir lock of e2e-sandbox.sh: the build
// lock and the image lock both claim through it. These cases drive it directly,
// sourced, with the fake runtime and the fake git on PATH.

// claimHostLock runs claim_host_lock DIR against the sourced script, naming the
// lock "test lock", its holder "another test" and its purpose "Tests share it.",
// then prints the pid file it left.
func claimHostLock(t *testing.T, f *fakeDocker, dir string) *exec.Cmd {
	t.Helper()
	cmd := exec.Command("bash", "-c",
		`set -euo pipefail; script=$1; dir=$2; . "$script"; `+
			`claim_host_lock "$dir" "test lock" "another test" "Tests share it."; cat "$dir/pid"; echo "self $$"`,
		"bash", sandboxScript(t), dir)
	cmd.Env = sandboxEnv(f, filepath.Join(t.TempDir(), "build.lock"))
	return cmd
}

// TestClaimHostLockClaims covers the two claims that do not wait: a free lock,
// and a lock whose recorded holder is dead.
func TestClaimHostLockClaims(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name     string
		heldBy   int // 0: no lock exists; otherwise the pid recorded in it
		wantLogs []string
	}{
		{name: "a free lock is claimed", heldBy: 0},
		{
			name:     "a dead holder's lock is taken over",
			heldBy:   reapedPid(t),
			wantLogs: []string{"reclaiming the test lock from dead pid"},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			// Arrange
			f := newFakeDocker(t)
			dir := filepath.Join(t.TempDir(), "host.lock")
			if tc.heldBy != 0 {
				if err := os.MkdirAll(dir, 0o755); err != nil {
					t.Fatalf("mkdir lock: %v", err)
				}
				if err := os.WriteFile(filepath.Join(dir, "pid"), fmt.Appendf(nil, "%d\n", tc.heldBy), 0o644); err != nil {
					t.Fatalf("write pid: %v", err)
				}
			}

			// Act
			out, err := claimHostLock(t, f, dir).CombinedOutput()

			// Assert: the lock records the claimer's own pid.
			if err != nil {
				t.Fatalf("claim_host_lock = %v\n%s", err, out)
			}
			m := regexp.MustCompile(`(?m)^(\d+)\nself (\d+)$`).FindStringSubmatch(string(out))
			if m == nil || m[1] != m[2] {
				t.Fatalf("the lock does not record the claimer's pid:\n%s", out)
			}
			for _, want := range tc.wantLogs {
				if !strings.Contains(string(out), want) {
					t.Fatalf("output lacks %q:\n%s", want, out)
				}
			}
		})
	}
}

// TestClaimHostLockWaitsForALiveHolder: a live holder blocks the claim, the
// wait names the lock, its holder and its purpose, and the claim goes through
// once the holder lets go. Observed through the output, never a timed sleep.
func TestClaimHostLockWaitsForALiveHolder(t *testing.T) {
	t.Parallel()
	// Arrange: this test process holds the lock.
	f := newFakeDocker(t)
	dir := filepath.Join(t.TempDir(), "host.lock")
	holdLock(t, dir)

	// Act: the claim waits, and the holder lets go.
	out, err := waitBehindLock(t, claimHostLock(t, f, dir), dir, "Tests share it.", func() {})

	// Assert: the wait named the lock and its holder, and the claim went through.
	if err != nil {
		t.Fatalf("claim_host_lock = %v\n%s", err, out)
	}
	if want := fmt.Sprintf("WAITING: another test holds the host test lock (pid %d).", os.Getpid()); !strings.Contains(out, want) {
		t.Fatalf("output lacks %q:\n%s", want, out)
	}
}

// TestEveryHostLockClaimsThroughClaimHostLock: a lock claimed by a hand-rolled
// `mkdir "$..._LOCK_DIR"` would skip the dead-holder takeover and the
// announced wait, so the script names no lock directory in a mkdir claim of
// its own.
func TestEveryHostLockClaimsThroughClaimHostLock(t *testing.T) {
	t.Parallel()
	// Arrange
	body, err := os.ReadFile(sandboxScript(t))
	if err != nil {
		t.Fatalf("read script: %v", err)
	}

	// Act
	claims := regexp.MustCompile(`mkdir "\$\{?[A-Z_]*LOCK[A-Z_]*`).FindAllString(string(body), -1)

	// Assert
	if len(claims) != 0 {
		t.Fatalf("e2e-sandbox.sh claims a lock outside claim_host_lock: %q", claims)
	}
	if !strings.Contains(string(body), `claim_host_lock "$BUILD_LOCK_DIR"`) {
		t.Fatalf("acquire_build_lock does not claim through claim_host_lock")
	}
}

// TestReclaimDeadHolder: a lock or slot directory is taken over only when the
// pid it records is gone; a live holder and a claim still writing its pid keep it.
func TestReclaimDeadHolder(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name        string
		pid         string // the recorded pid; "" writes no pid file
		wantReclaim bool
	}{
		{name: "a dead holder's directory is reclaimed", pid: strconv.Itoa(reapedPid(t)), wantReclaim: true},
		{name: "a live holder's directory is kept", pid: strconv.Itoa(os.Getpid()), wantReclaim: false},
		{name: "a claim with no pid yet is kept", pid: "", wantReclaim: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			// Arrange
			f := newFakeDocker(t)
			dir := filepath.Join(t.TempDir(), "held")
			if err := os.MkdirAll(dir, 0o755); err != nil {
				t.Fatalf("mkdir: %v", err)
			}
			if tc.pid != "" {
				if err := os.WriteFile(filepath.Join(dir, "pid"), []byte(tc.pid+"\n"), 0o644); err != nil {
					t.Fatalf("write pid: %v", err)
				}
			}
			cmd := exec.Command("bash", "-c", `set -euo pipefail; script=$1; dir=$2; . "$script"; reclaim_dead_holder "$dir" "the test lock"`,
				"bash", sandboxScript(t), dir)
			cmd.Env = sandboxEnv(f, filepath.Join(t.TempDir(), "build.lock"))

			// Act
			out, err := cmd.CombinedOutput()

			// Assert
			if got := err == nil; got != tc.wantReclaim {
				t.Fatalf("reclaimed = %v, want %v\n%s", got, tc.wantReclaim, out)
			}
			_, statErr := os.Stat(dir)
			if gone := os.IsNotExist(statErr); gone != tc.wantReclaim {
				t.Fatalf("directory gone = %v, want %v", gone, tc.wantReclaim)
			}
			if logged := strings.Contains(string(out), "reclaiming the test lock from dead pid "+tc.pid); logged != tc.wantReclaim {
				t.Fatalf("reclaim logged = %v, want %v:\n%s", logged, tc.wantReclaim, out)
			}
		})
	}
}

// TestEveryTakeoverGoesThroughReclaimDeadHolder: the move-aside that frees a
// dead holder's directory exists once, in reclaim_dead_holder, so the build
// lock, the image lock and the slots cannot drift apart in how they reclaim.
func TestEveryTakeoverGoesThroughReclaimDeadHolder(t *testing.T) {
	t.Parallel()
	// Arrange
	body, err := os.ReadFile(sandboxScript(t))
	if err != nil {
		t.Fatalf("read script: %v", err)
	}

	// Act
	moves := strings.Count(string(body), `.dead.$$"`)

	// Assert: one `mv ... .dead.$$` and its `rm -rf ... .dead.$$`.
	if moves != 2 {
		t.Fatalf("found %d .dead.$$ references, want the 2 of reclaim_dead_holder's one move-aside", moves)
	}
}
