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
	if err := os.MkdirAll(filepath.Dir(log.path), 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	if err := os.WriteFile(log.path, []byte("run\n"), 0o644); err != nil {
		t.Fatalf("write: %v", err)
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
		{name: "a log no longer there", token: func(t *testing.T, h *harness) string {
			token, path := servedLog(t, h, "lease-1", 1)
			if err := os.Remove(path); err != nil {
				t.Fatalf("remove: %v", err)
			}
			return token
		}},
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

func TestCreateTestLogMakesItsDirectoryAndTruncates(t *testing.T) {
	// Arrange: a log path whose directory does not exist yet.
	log := testLog{path: filepath.Join(t.TempDir(), mergeLogsDir, "lease-tests-1.log")}

	// Act.
	file, err := createTestLog(log)
	if err != nil {
		t.Fatalf("createTestLog: %v", err)
	}
	if err := file.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Assert.
	info, err := os.Stat(log.path)
	if err != nil || info.Size() != 0 {
		t.Fatalf("stat = (%v, %v), want an empty log", info, err)
	}
}

func TestCreateTestLogRefusesWhenItsDirectoryCannotBeMade(t *testing.T) {
	// Arrange: a regular file where the log directory must be.
	root := t.TempDir()
	if err := os.WriteFile(filepath.Join(root, mergeLogsDir), nil, 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	log := testLog{path: filepath.Join(root, mergeLogsDir, "lease-tests-1.log")}

	// Act.
	_, err := createTestLog(log)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "creating the test log directory") {
		t.Fatalf("createTestLog = %v, want the directory refusal", err)
	}
}

func TestACreatedTestLogIsHeldUntilClosed(t *testing.T) {
	// Arrange.
	log := testLog{path: filepath.Join(t.TempDir(), mergeLogsDir, "00bfb96b635b498a-tests-1.log")}
	file, err := createTestLog(log)
	if err != nil {
		t.Fatalf("createTestLog: %v", err)
	}
	t.Cleanup(func() { _ = file.Close() })
	old := time.Now().Add(-logging.DefaultRetention - time.Hour)
	if err := os.Chtimes(log.path, old, old); err != nil {
		t.Fatalf("Chtimes: %v", err)
	}

	// Act: a retention pass over the directory while the log is open.
	report, err := logging.PruneDirectory(filepath.Dir(log.path), time.Now(), logging.DefaultRetention, IsTestLog)

	// Assert.
	if err != nil || len(report.Removed) != 0 || len(report.Held) != 1 {
		t.Fatalf("PruneDirectory = (%+v, %v), want the open log held", report, err)
	}
}

func TestIsTestLog(t *testing.T) {
	cases := []struct {
		base string
		want bool
	}{
		{"00bfb96b635b498a-tests-1.log", true},
		{"00bfb96b635b498a-tests-12.log", true},
		{"00bfb96b635b498a-tests-1.log.1", false},
		{"notes.txt", false},
		{"00bfb96b635b498a-tests-.log", false},
	}
	for _, tc := range cases {
		t.Run(tc.base, func(t *testing.T) {
			// Act / Assert.
			if got := IsTestLog(tc.base); got != tc.want {
				t.Fatalf("IsTestLog(%q) = %v, want %v", tc.base, got, tc.want)
			}
		})
	}
}

func TestEveryTestLogLivesInTestLogsDir(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	log := h.o.testLog("00bfb96b635b498a", 3)

	// Assert.
	if filepath.Dir(log.path) != TestLogsDir(h.o.deps.StateDir) || !IsTestLog(filepath.Base(log.path)) {
		t.Fatalf("testLog path %q is not a test log in %q", log.path, TestLogsDir(h.o.deps.StateDir))
	}
}

func TestWriteTestLogWritesTheWholeBody(t *testing.T) {
	// Arrange.
	log := testLog{path: filepath.Join(t.TempDir(), mergeLogsDir, "lease-tests-2.log")}

	// Act.
	err := writeTestLog(log, "nothing ran.\n")

	// Assert.
	got, readErr := os.ReadFile(log.path)
	if err != nil || readErr != nil || string(got) != "nothing ran.\n" {
		t.Fatalf("writeTestLog = %v, file = (%q, %v), want the body", err, got, readErr)
	}
}

func TestEveryTestLogIsCreatedThroughCreateTestLog(t *testing.T) {
	// Act: the production sites that open a test log.
	viaHelper := sourcescan.Count(t, "createTestLog(")
	byHand := sourcescan.Count(t, "os.Create(") + sourcescan.Count(t, "os.WriteFile(log.path")

	// Assert: the gate run, writeTestLog and the helper's own definition, and
	// no hand-rolled create, which would write a log the retention pass could
	// prune mid-run.
	if viaHelper != 3 || byHand != 0 {
		t.Fatalf("createTestLog( appears %d times and a hand-rolled create %d times; want 3 and 0", viaHelper, byHand)
	}
}
