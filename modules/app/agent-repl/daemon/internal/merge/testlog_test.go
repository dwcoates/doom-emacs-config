package merge

import (
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"agentrepl/logging"

	"agent-repld/internal/sourcescan"
	"agent-repld/internal/wsm"
)

// servedLog writes one test log of the harness workspace's lease and answers
// its token.
func servedLog(t *testing.T, h *harness, lease wsm.LeaseID, round int) (string, string) {
	t.Helper()
	if err := h.db.OpenMergeLedger(context.Background(), theWorkspace, lease); err != nil {
		t.Fatalf("OpenMergeLedger: %v", err)
	}
	log := h.o.testLog(lease, round)
	if err := writeLog(log.path, "run\n"); err != nil {
		t.Fatalf("writeLog: %v", err)
	}
	return log.token, log.path
}

func TestATokenResolvesToItsLogForItsWorkspace(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	token, path := servedLog(t, h, "lease-1", 2)

	// Act.
	got, err := h.o.TestLogPath(context.Background(), theWorkspace, token)

	// Assert.
	if err != nil || got != path {
		t.Fatalf("TestLogPath = (%q, %v), want %q", got, err, path)
	}
}

func TestATokenIsRefusedWhenItNamesNoLogOfTheWorkspace(t *testing.T) {
	tests := []struct {
		name  string
		token func(t *testing.T, h *harness) string
	}{
		{name: "a malformed token", token: func(*testing.T, *harness) string { return "nonsense" }},
		{name: "a round that is no number", token: func(*testing.T, *harness) string { return "lease-1/x" }},
		{name: "another workspace's merge", token: func(t *testing.T, h *harness) string { return "lease-other/1" }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			token := tt.token(t, h)

			// Act.
			_, err := h.o.TestLogPath(context.Background(), theWorkspace, token)

			// Assert.
			if refusal, refused := Refused(err); !refused || refusal.Arm != ArmUnknownMergeTestLog {
				t.Fatalf("TestLogPath = %v, want unknown_merge_test_log", err)
			}
			if _, logged := h.recordFor("warn", "daemon.merge.test_log"); !logged {
				t.Fatalf("the refusal was not recorded at WARN: %+v", h.logs.Records())
			}
		})
	}
}

func TestATokenWhoseLogAgedOutIsRefusedAtInfo(t *testing.T) {
	// Arrange: the workspace's own merge, its log since removed by retention.
	h := newHarness(t)
	token, path := servedLog(t, h, "lease-1", 1)
	if err := os.Remove(path); err != nil {
		t.Fatalf("remove: %v", err)
	}

	// Act.
	_, err := h.o.TestLogPath(context.Background(), theWorkspace, token)

	// Assert.
	if refusal, refused := Refused(err); !refused || refusal.Arm != ArmUnknownMergeTestLog {
		t.Fatalf("TestLogPath = %v, want unknown_merge_test_log", err)
	}
	if _, logged := h.recordFor("info", "daemon.merge.test_log"); !logged {
		t.Fatalf("the refusal was not recorded at INFO: %+v", h.logs.Records())
	}
	if _, warned := h.recordFor("warn", "daemon.merge.test_log"); warned {
		t.Fatalf("an aged-out log was recorded at WARN: %+v", h.logs.Records())
	}
}

func TestTildePathShortensTheHomeDirectory(t *testing.T) {
	tests := []struct {
		path, home, want string
	}{
		{path: "/home/u/.claude-emacs/merge-logs/a.log", home: "/home/u", want: "~/.claude-emacs/merge-logs/a.log"},
		{path: "/home/u", home: "/home/u", want: "~"},
		{path: "/home/user2/a.log", home: "/home/u", want: "/home/user2/a.log"},
		{path: "/tmp/a.log", home: "", want: "/tmp/a.log"},
	}
	for _, tt := range tests {
		t.Run(tt.path, func(t *testing.T) {
			// Act.
			got := tildePath(tt.path, tt.home)

			// Assert.
			if got != tt.want {
				t.Fatalf("tildePath = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestCreateLogMakesItsDirectoryAndTruncates(t *testing.T) {
	// Arrange: a log path whose directory does not exist yet.
	path := filepath.Join(t.TempDir(), mergeLogsDir, "lease-tests-1.log")

	// Act.
	file, err := createLog(path)
	if err != nil {
		t.Fatalf("createLog: %v", err)
	}
	if err := file.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Assert.
	info, err := os.Stat(path)
	if err != nil || info.Size() != 0 {
		t.Fatalf("stat = (%v, %v), want an empty log", info, err)
	}
}

func TestCreateLogRefusesWhenItsDirectoryCannotBeMade(t *testing.T) {
	// Arrange: a regular file where the log directory must be.
	root := t.TempDir()
	if err := os.WriteFile(filepath.Join(root, mergeLogsDir), nil, 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}

	// Act.
	_, err := createLog(filepath.Join(root, mergeLogsDir, "lease-tests-1.log"))

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "creating the log directory") {
		t.Fatalf("createLog = %v, want the directory refusal", err)
	}
}

func TestACreatedLogIsHeldUntilClosed(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), mergeLogsDir, "00bfb96b635b498a-tests-1.log")
	file, err := createLog(path)
	if err != nil {
		t.Fatalf("createLog: %v", err)
	}
	t.Cleanup(func() { _ = file.Close() })
	old := time.Now().Add(-logging.DefaultRetention - time.Hour)
	if err := os.Chtimes(path, old, old); err != nil {
		t.Fatalf("Chtimes: %v", err)
	}

	// Act: a retention pass over the directory while the log is open.
	report, err := logging.PruneDirectory(filepath.Dir(path), time.Now(), logging.DefaultRetention, IsLog)

	// Assert.
	if err != nil || len(report.Removed) != 0 || len(report.Held) != 1 {
		t.Fatalf("PruneDirectory = (%+v, %v), want the open log held", report, err)
	}
}

func TestIsLog(t *testing.T) {
	cases := []struct {
		base string
		want bool
	}{
		{"00bfb96b635b498a-tests-1.log", true},
		{"00bfb96b635b498a-tests-12.log", true},
		{"00bfb96b635b498a-pre_deploy-1.log", true},
		{"00bfb96b635b498a-deploy-2.log", true},
		{"00bfb96b635b498a-post_deploy-3.log", true},
		{"release-1760000000000000000.log", true},
		{"00bfb96b635b498a-tests-1.log.1", false},
		{"00bfb96b635b498a-linting-1.log", false},
		{"release-.log", false},
		{"notes.txt", false},
		{"00bfb96b635b498a-tests-.log", false},
	}
	for _, tc := range cases {
		t.Run(tc.base, func(t *testing.T) {
			// Act / Assert.
			if got := IsLog(tc.base); got != tc.want {
				t.Fatalf("IsLog(%q) = %v, want %v", tc.base, got, tc.want)
			}
		})
	}
}

func TestEveryLogTheQueueNamesLivesInLogsDirAndIsRetained(t *testing.T) {
	h := newHarness(t)
	r := &run{o: h.o, lease: wsm.Lease{ID: "00bfb96b635b498a"}}
	tests := []struct {
		name string
		path string
	}{
		{name: "a tests round's", path: h.o.testLog("00bfb96b635b498a", 3).path},
		{name: "a pre-deploy script's", path: r.hookLogPath(stagePreDeploy, tabRound{n: 1})},
		{name: "a deploy script's", path: r.hookLogPath(stageDeploy, tabRound{n: 2})},
		{name: "a post-deploy script's", path: r.hookLogPath(stagePostDeploy, tabRound{n: 1})},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Assert.
			if filepath.Dir(tt.path) != LogsDir(h.o.deps.StateDir) || !IsLog(filepath.Base(tt.path)) {
				t.Fatalf("log path %q is not a retained log in %q", tt.path, LogsDir(h.o.deps.StateDir))
			}
		})
	}
}

func TestWriteLogWritesTheWholeBody(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), mergeLogsDir, "lease-tests-2.log")

	// Act.
	err := writeLog(path, "nothing ran.\n")

	// Assert.
	got, readErr := os.ReadFile(path)
	if err != nil || readErr != nil || string(got) != "nothing ran.\n" {
		t.Fatalf("writeLog = %v, file = (%q, %v), want the body", err, got, readErr)
	}
}

func TestEveryLogIsCreatedThroughCreateLog(t *testing.T) {
	// Act: the production sites that open one of the queue's logs.
	viaHelper := sourcescan.Count(t, "createLog(")
	byHand := sourcescan.Count(t, "os.Create(")
	written := sourcescan.Count(t, "os.WriteFile(")

	// Assert: the gate run, writeLog, the script hook and the helper's own
	// definition, and no hand-rolled create, which would write a log the
	// retention pass could prune mid-run. The one os.WriteFile is the failing
	// tests file in the worktree (writeFailingTests), which is no log.
	if viaHelper != 4 || byHand != 0 || written != 1 {
		t.Fatalf("createLog( appears %d times, os.Create( %d and os.WriteFile( %d; want 4, 0 and 1", viaHelper, byHand, written)
	}
}
