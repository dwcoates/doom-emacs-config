package server

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"claude-repld/internal/registry"
	"claude-repld/internal/session"
)

// fakeEnsurer records the workspaces it was asked to bring up.
type fakeEnsurer struct {
	calls []string
	err   error
}

func (f *fakeEnsurer) Ensure(workspace string) error {
	f.calls = append(f.calls, workspace)
	return f.err
}

// writeTranscript creates <configDir>/projects/<slug(cwd)>/<uuid>.jsonl.
func writeProjectTranscript(t *testing.T, configDir, cwd, uuid string) {
	t.Helper()
	dir := session.ProjectDir(configDir, cwd)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("mkdir %s: %v", dir, err)
	}
	path := filepath.Join(dir, uuid+".jsonl")
	if err := os.WriteFile(path, []byte("{}\n"), 0o644); err != nil {
		t.Fatalf("write %s: %v", path, err)
	}
}

// openerRig builds a WorkspaceOpener over a temp registry, capturing its log.
func openerRig(t *testing.T, dirs ...string) (*WorkspaceOpener, *registry.Registry, *fakeEnsurer, *[]string) {
	t.Helper()
	reg := openTestRegistry(t)
	ens := &fakeEnsurer{}
	var lines []string
	o := &WorkspaceOpener{
		Reg:        reg,
		Ensurer:    ens,
		ConfigDirs: func() []string { return dirs },
		Logf:       func(f string, a ...any) { lines = append(lines, fmt.Sprintf(f, a...)) },
	}
	return o, reg, ens, &lines
}

func TestBindWorkspaceBindsTheNewestOnDiskTranscript(t *testing.T) {
	// Arrange — a record with no vendor session id and a transcript on disk.
	cfg := t.TempDir()
	o, reg, _, _ := openerRig(t, cfg)
	if err := reg.Put(registry.Record{SessionID: "s_1", CWD: "/w", ConfigDir: cfg, CreatedAt: "2026-07-25T10:00:00Z"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	writeProjectTranscript(t, cfg, "/w", "uuid-disk")

	// Act
	bound := o.BindWorkspace("/w")

	// Assert
	rec, _ := reg.Get("s_1")
	if !bound || rec.ClaudeSessionID != "uuid-disk" {
		t.Fatalf("bound=%v ClaudeSessionID=%q; want true,uuid-disk", bound, rec.ClaudeSessionID)
	}
}

func TestBindWorkspaceLeavesAnAlreadyBoundRecordAlone(t *testing.T) {
	// Arrange — the record already names its conversation.
	cfg := t.TempDir()
	o, reg, _, _ := openerRig(t, cfg)
	if err := reg.Put(registry.Record{SessionID: "s_1", CWD: "/w", ConfigDir: cfg, ClaudeSessionID: "uuid-live", CreatedAt: "2026-07-25T10:00:00Z"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	writeProjectTranscript(t, cfg, "/w", "uuid-disk")

	// Act
	o.BindWorkspace("/w")

	// Assert
	rec, _ := reg.Get("s_1")
	if rec.ClaudeSessionID != "uuid-live" {
		t.Fatalf("ClaudeSessionID = %q; want the live id uuid-live to survive", rec.ClaudeSessionID)
	}
}

func TestBindWorkspaceNeverAdoptsATranscriptFromAnotherConfigDir(t *testing.T) {
	// Arrange — the only transcript lives under a DIFFERENT account root.
	own, other := t.TempDir(), t.TempDir()
	o, reg, _, _ := openerRig(t, own, other)
	if err := reg.Put(registry.Record{SessionID: "s_1", CWD: "/w", ConfigDir: own, CreatedAt: "2026-07-25T10:00:00Z"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	writeProjectTranscript(t, other, "/w", "uuid-elsewhere")

	// Act
	o.BindWorkspace("/w")

	// Assert
	rec, _ := reg.Get("s_1")
	if rec.ClaudeSessionID != "" {
		t.Fatalf("ClaudeSessionID = %q; a foreign-config-dir transcript must never be adopted", rec.ClaudeSessionID)
	}
}

func TestBindWorkspaceLoudLogsAForeignTranscriptAsAMigrationCandidate(t *testing.T) {
	// Arrange
	own, other := t.TempDir(), t.TempDir()
	o, reg, _, lines := openerRig(t, own, other)
	if err := reg.Put(registry.Record{SessionID: "s_1", CWD: "/w", ConfigDir: own, CreatedAt: "2026-07-25T10:00:00Z"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	writeProjectTranscript(t, other, "/w", "uuid-elsewhere")

	// Act
	o.BindWorkspace("/w")

	// Assert
	if !containsSubstring(*lines, "MIGRATION CANDIDATE") {
		t.Fatalf("no migration-candidate log line; got %v", *lines)
	}
}

func TestBindWorkspaceLoudLogsAWorkspaceWithNoRegistryRecord(t *testing.T) {
	// Arrange — a workspace the registry has never heard of.
	cfg := t.TempDir()
	o, _, _, lines := openerRig(t, cfg)

	// Act
	bound := o.BindWorkspace("/unknown")

	// Assert
	if bound {
		t.Fatalf("BindWorkspace bound something for a workspace with no record")
	}
	if !containsSubstring(*lines, "no registry record") {
		t.Fatalf("no loud line for the recordless workspace; got %v", *lines)
	}
}

func TestOpenEnsuresTheSessionEagerly(t *testing.T) {
	// Arrange — eager bring-up is what stops a known workspace rendering blue.
	cfg := t.TempDir()
	o, reg, ens, _ := openerRig(t, cfg)
	if err := reg.Put(registry.Record{SessionID: "s_1", CWD: "/w", ConfigDir: cfg, ClaudeSessionID: "uuid-live", CreatedAt: "2026-07-25T10:00:00Z"}); err != nil {
		t.Fatalf("put: %v", err)
	}

	// Act
	if err := o.Open(context.Background(), "/w"); err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Assert
	if len(ens.calls) != 1 || ens.calls[0] != "/w" {
		t.Fatalf("Ensure calls = %v; want [/w]", ens.calls)
	}
}

func TestOpenSurfacesAnEnsureFailure(t *testing.T) {
	// Arrange
	cfg := t.TempDir()
	o, reg, ens, _ := openerRig(t, cfg)
	ens.err = errBringUp
	if err := reg.Put(registry.Record{SessionID: "s_1", CWD: "/w", ConfigDir: cfg, CreatedAt: "2026-07-25T10:00:00Z"}); err != nil {
		t.Fatalf("put: %v", err)
	}

	// Act
	err := o.Open(context.Background(), "/w")

	// Assert
	if err == nil {
		t.Fatalf("Open swallowed the bring-up failure")
	}
}

func TestCloseStillFailsLoudlyBecauseItIsNotExposedDaemonSide(t *testing.T) {
	// Arrange
	o, _, _, _ := openerRig(t)

	// Act
	err := o.Close(context.Background(), "/w")

	// Assert
	if err == nil {
		t.Fatalf("Close returned nil; the unexposed verb must fail loudly, never no-op")
	}
}

func TestBindAllSkipsTerminalRecords(t *testing.T) {
	// Arrange — a dead conversation must not be re-bound at boot.
	cfg := t.TempDir()
	o, reg, _, _ := openerRig(t, cfg)
	if err := reg.Put(registry.Record{SessionID: "s_dead", CWD: "/w", ConfigDir: cfg, Terminal: true, CreatedAt: "2026-07-25T10:00:00Z"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	writeProjectTranscript(t, cfg, "/w", "uuid-disk")

	// Act
	o.BindAll()

	// Assert
	rec, _ := reg.Get("s_dead")
	if rec.ClaudeSessionID != "" {
		t.Fatalf("ClaudeSessionID = %q; a terminal record must stay unbound", rec.ClaudeSessionID)
	}
}

func TestBindAllBindsEveryUnboundRegisteredWorkspace(t *testing.T) {
	// Arrange — the boot sweep, which runs before any frontend connects.
	cfg := t.TempDir()
	o, reg, _, _ := openerRig(t, cfg)
	for _, n := range []string{"1", "2"} {
		if err := reg.Put(registry.Record{SessionID: "s_" + n, CWD: "/w" + n, ConfigDir: cfg, CreatedAt: "2026-07-25T10:00:00Z"}); err != nil {
			t.Fatalf("put %s: %v", n, err)
		}
		writeProjectTranscript(t, cfg, "/w"+n, "uuid-"+n)
	}

	// Act
	o.BindAll()

	// Assert
	for _, n := range []string{"1", "2"} {
		rec, _ := reg.Get("s_" + n)
		if rec.ClaudeSessionID != "uuid-"+n {
			t.Fatalf("s_%s ClaudeSessionID = %q; want uuid-%s", n, rec.ClaudeSessionID, n)
		}
	}
}

func TestBindWorkspaceIgnoresATranscriptForADifferentCWD(t *testing.T) {
	// Arrange — encoding collisions would be catastrophic, so pin the miss.
	cfg := t.TempDir()
	o, reg, _, _ := openerRig(t, cfg)
	if err := reg.Put(registry.Record{SessionID: "s_1", CWD: "/w", ConfigDir: cfg, CreatedAt: "2026-07-25T10:00:00Z"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	writeProjectTranscript(t, cfg, "/other", "uuid-other")

	// Act
	o.BindWorkspace("/w")

	// Assert
	rec, _ := reg.Get("s_1")
	if rec.ClaudeSessionID != "" {
		t.Fatalf("ClaudeSessionID = %q; want empty (no transcript for /w)", rec.ClaudeSessionID)
	}
}

// --- helpers --------------------------------------------------------------

var errBringUp = errors.New("bring-up failed")

func containsSubstring(lines []string, want string) bool {
	for _, l := range lines {
		if strings.Contains(l, want) {
			return true
		}
	}
	return false
}
