package merge

import (
	"context"
	"os"
	"path/filepath"
	"testing"

	"claude-repld/internal/wsm"
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
