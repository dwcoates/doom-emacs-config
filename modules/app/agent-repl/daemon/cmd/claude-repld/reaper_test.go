package main

import (
	"context"
	"testing"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/gitclient"
	"claude-repld/internal/ids"
	"claude-repld/internal/worktreereap"
	"claude-repld/internal/wsm"
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

func TestTheReaperBuildsFromUnsetKnobs(t *testing.T) {
	// Arrange.
	t.Setenv(envWorktreeReapIdle, "")
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
