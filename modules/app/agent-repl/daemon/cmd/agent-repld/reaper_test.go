package main

import (
	"context"
	"os"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/gitclient"
	"agent-repld/internal/ids"
	"agent-repld/internal/worktreereap"
	"agent-repld/internal/wsm"
)

// noRegistry is a registry nothing reads: these tests only build.
type noRegistry struct{}

func (noRegistry) ListRepositories(context.Context) ([]wsm.Repository, error) { return nil, nil }
func (noRegistry) ListWorkspaces(context.Context) ([]wsm.Workspace, error)    { return nil, nil }

// buildTestReaper builds the reaper against the real git leaf (never run by
// these tests) and a registry nothing reads.
func buildTestReaper(t *testing.T) (*worktreereap.Reaper, *dlog.TestLogger, error) {
	t.Helper()
	log := dlog.NewTestLogger()
	git, err := gitclient.New(dlog.NewTestSurfaces())
	if err != nil {
		t.Fatalf("gitclient.New: %v", err)
	}
	reaper, err := buildWorktreeReaper(git, noRegistry{}, func() []ids.WorkspaceID { return nil }, t.TempDir(), log)
	return reaper, log, err
}

func TestTheReaperWindowsDefaultToTheProductionOnes(t *testing.T) {
	cases := []struct {
		name    string
		resolve func(string) (time.Duration, error)
		want    time.Duration
	}{
		{"idle threshold", resolveWorktreeReapIdle, worktreereap.DefaultIdleAfter},
		{"expiry", resolveWorktreeReapExpire, worktreereap.DefaultExpireAfter},
		{"start delay", resolveWorktreeReapStartDelay, worktreereap.DefaultStartDelay},
		{"cadence", resolveWorktreeReapEvery, worktreereap.DefaultEvery},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			got, err := tc.resolve("")

			// Assert.
			if err != nil || got != tc.want {
				t.Fatalf("unset %s = (%v, %v), want %v", tc.name, got, err, tc.want)
			}
		})
	}
}

func TestTheReaperIdleThresholdComesFromTheEnvironment(t *testing.T) {
	// Arrange, Act.
	got, err := resolveWorktreeReapIdle("36h")

	// Assert.
	if err != nil || got != 36*time.Hour {
		t.Fatalf("resolveWorktreeReapIdle(36h) = (%v, %v), want 36h", got, err)
	}
}

func TestTheReaperExpiryComesFromTheEnvironment(t *testing.T) {
	// Arrange, Act.
	got, err := resolveWorktreeReapExpire("240h")

	// Assert.
	if err != nil || got != 240*time.Hour {
		t.Fatalf("resolveWorktreeReapExpire(240h) = (%v, %v), want 240h", got, err)
	}
}

func TestAMalformedReaperExpiryIsABootFatalRecordedAtError(t *testing.T) {
	// Arrange.
	t.Setenv(envWorktreeReapIdle, "")
	t.Setenv(envWorktreeReapExpire, "fortnight")
	t.Setenv(envWorktreeReapStartDelay, "")
	t.Setenv(envWorktreeReapEvery, "")

	// Act.
	_, log, err := buildTestReaper(t)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), envWorktreeReapExpire) {
		t.Fatalf("buildWorktreeReaper = %v, want the expiry refused by name", err)
	}
	for _, r := range log.Records() {
		if r.Level == "error" && r.Operation == graphOperation && strings.Contains(r.Context["cause"].(string), envWorktreeReapExpire) {
			return
		}
	}
	t.Fatalf("records = %v, want the refusal at ERROR", log.Records())
}

func TestTheReaperBuildsFromUnsetKnobs(t *testing.T) {
	// Arrange.
	t.Setenv(envWorktreeReapIdle, "")
	t.Setenv(envWorktreeReapExpire, "")
	t.Setenv(envWorktreeReapStartDelay, "")
	t.Setenv(envWorktreeReapEvery, "")

	// Act.
	reaper, _, err := buildTestReaper(t)

	// Assert.
	if err != nil || reaper == nil {
		t.Fatalf("buildWorktreeReaper = (%v, %v), want a reaper", reaper, err)
	}
}

func TestARefusedReaperKnobIsABootFatalRecordedAtError(t *testing.T) {
	// Arrange.
	t.Setenv(envWorktreeReapIdle, "")
	t.Setenv(envWorktreeReapStartDelay, "")
	t.Setenv(envWorktreeReapEvery, "daily")

	// Act.
	_, log, err := buildTestReaper(t)

	// Assert.
	if err == nil {
		t.Fatal("buildWorktreeReaper = nil error for a malformed cadence, want a refusal")
	}
	found := false
	for _, r := range log.Records() {
		if r.Level == "error" && r.Operation == graphOperation {
			found = true
		}
	}
	if !found {
		t.Fatalf("records = %v, want the refusal at ERROR", log.Records())
	}
}

func TestTheWorktreeLockIsTheReapLockInTheRunDir(t *testing.T) {
	// Act
	got := worktreeLockPath("/run/agent-repl")

	// Assert
	if got != "/run/agent-repl/worktree-reap.lock" {
		t.Fatalf("worktreeLockPath = %q, want the reap lock in the run dir", got)
	}
}

// TestTheWorktreeLockSitesShareOnePath fails a site that spells the worktree
// lock's path by hand: the reaper and the merge workspace sweep exclude each
// other only while both name the same file.
func TestTheWorktreeLockSitesShareOnePath(t *testing.T) {
	// Arrange
	var text strings.Builder
	for _, name := range []string{"reaper.go", "graph.go"} {
		src, err := os.ReadFile(name)
		if err != nil {
			t.Fatalf("read %s: %v", name, err)
		}
		text.Write(src)
	}

	// Act
	shared := strings.Count(text.String(), "worktreeLockPath(")
	byHand := strings.Count(text.String(), "worktreeReapLockName)")

	// Assert: the helper's definition, the reaper and the merge workspace
	// sweep.
	if shared != 3 || byHand != 1 {
		t.Fatalf("the worktree lock is named through the helper %d times and by hand %d times, want 3 and 1 (the helper's own body)", shared, byHand)
	}
}
