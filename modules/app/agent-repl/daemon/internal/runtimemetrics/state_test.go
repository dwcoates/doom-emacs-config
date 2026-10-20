package runtimemetrics

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

// fakeLister answers arranged workspaces and repositories, or an error.
type fakeLister struct {
	workspaces []wsm.Workspace
	repos      []wsm.Repository
	err        error
}

func (f *fakeLister) ListWorkspaces(context.Context) ([]wsm.Workspace, error) { return f.workspaces, f.err }
func (f *fakeLister) ListRepositories(context.Context) ([]wsm.Repository, error) {
	return f.repos, f.err
}

func writeSized(t *testing.T, dir, name string, size int) {
	t.Helper()
	if err := os.WriteFile(filepath.Join(dir, name), make([]byte, size), 0o644); err != nil {
		t.Fatal(err)
	}
}

func TestLogRuntime(t *testing.T) {
	tests := []struct{ name, want string }{
		{"agent-repl-c66e3ae73cdf4323-daemon-1968687598.log", "daemon"},
		{"agent-repl-99808d49-shim-12.log", "shim"},
		{"agent-repl-c66e3ae73cdf4323-webapp-3987465320.log.1", "webapp"},
		{"agent-repl-c66e3ae73cdf4323-emacs-v2Wiej.log", "emacs"},
		{"daemon.run.log", "daemon"},
		{"agent-repl-c66e3ae73cdf4323-mystery-1.log", "other"},
		{"notes.txt", "other"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := logRuntime(tt.name); got != tt.want {
				t.Fatalf("logRuntime(%q) = %q, want %q", tt.name, got, tt.want)
			}
		})
	}
}

func TestTheStateCollectorServesLogsDatabaseAndWorkspaces(t *testing.T) {
	// Arrange
	s, _ := openSurface(t)
	logs := t.TempDir()
	writeSized(t, logs, "agent-repl-aaaa-daemon-1.log", 100)
	writeSized(t, logs, "daemon.run.log", 50)
	writeSized(t, logs, "agent-repl-aaaa-shim-2.log", 7)
	dbPath := filepath.Join(t.TempDir(), "wsm.db")
	writeSized(t, filepath.Dir(dbPath), "wsm.db", 4096)
	lister := &fakeLister{
		repos: []wsm.Repository{{ID: "r1", Name: "doom"}},
		workspaces: []wsm.Workspace{
			{ID: "w1", Repo: "r1"}, {ID: "w2", Repo: "r1"},
			{ID: "w3", Repo: "r1", Hidden: true}, {ID: "w4", Repo: "r1", Closed: true},
		},
	}
	if err := s.RegisterState(logs, dbPath, lister); err != nil {
		t.Fatalf("RegisterState: %v", err)
	}

	// Act
	body := scrape(t, s)

	// Assert
	mustContain(t, body,
		`agentrepl_log_bytes{runtime="daemon"} 150`,
		`agentrepl_log_files{runtime="daemon"} 2`,
		`agentrepl_log_bytes{runtime="shim"} 7`,
		`agentrepl_db_bytes{db="wsm",file="db"} 4096`,
		`agentrepl_workspaces{repository="doom",state="open"} 2`,
		`agentrepl_workspaces{repository="doom",state="hidden"} 1`,
		`agentrepl_workspaces{repository="doom",state="closed"} 1`,
	)
}

func TestAFailedWorkspaceReadIsRecordedOnceAndOmitsOnlyTheWorkspaces(t *testing.T) {
	// Arrange
	s, log := openSurface(t)
	logs := t.TempDir()
	writeSized(t, logs, "daemon.run.log", 1)
	lister := &fakeLister{err: errors.New("database is locked")}
	if err := s.RegisterState(logs, filepath.Join(t.TempDir(), "wsm.db"), lister); err != nil {
		t.Fatalf("RegisterState: %v", err)
	}

	// Act
	scrape(t, s)
	body := scrape(t, s)

	// Assert
	if strings.Contains(body, "agentrepl_workspaces{") {
		t.Fatalf("a failed read still served workspaces:\n%s", body)
	}
	mustContain(t, body, `agentrepl_log_files{runtime="daemon"} 1`)
	var errs []dlog.Record
	for _, r := range log.Records() {
		if r.Level == dlog.LevelError && r.Context["step"] == "repositories" {
			errs = append(errs, r)
		}
	}
	if len(errs) != 1 || !strings.Contains(errs[0].Context["error"].(string), "database is locked") {
		t.Fatalf("error records = %+v, want one repositories failure across two scrapes", errs)
	}
}

func TestTheOffSurfaceRegistersNoState(t *testing.T) {
	// Arrange
	var s *Surface

	// Act
	err := s.RegisterState(t.TempDir(), "wsm.db", &fakeLister{})

	// Assert
	if err != nil {
		t.Fatalf("RegisterState on the off surface = %v, want nil", err)
	}
}
