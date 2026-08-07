package server

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"claude-repld/internal/errclass"
	"claude-repld/internal/registry"
	"claude-repld/internal/session"
)

// ---------------------------------------------------------------------------
// THE TRANSCRIPT BACKUP PLANE. Its job is to make the resume ladder's hard
// fault reachable only when both the vendor transcript AND every copy of it
// are gone.
// ---------------------------------------------------------------------------

// backupWorkspace builds a workspace whose vendor transcript exists, and
// returns (cwd, configDir).
func backupWorkspace(t *testing.T, uuid, body string) (string, string) {
	t.Helper()
	cwd := t.TempDir()
	cfg := t.TempDir()
	path := session.TranscriptPath(session.ClaudeConfigDir(cfg), cwd, uuid)
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatalf("mkdir transcript dir: %v", err)
	}
	if err := os.WriteFile(path, []byte(body), 0o600); err != nil {
		t.Fatalf("write transcript: %v", err)
	}
	return cwd, cfg
}

// backupNames lists the backup file names cwd holds.
func backupNames(t *testing.T, cwd string) []string {
	t.Helper()
	entries, err := os.ReadDir(backupDir(cwd))
	if err != nil {
		if os.IsNotExist(err) {
			return nil
		}
		t.Fatalf("read backup dir: %v", err)
	}
	var out []string
	for _, e := range entries {
		out = append(out, e.Name())
	}
	return out
}

// tickingClock hands out strictly increasing instants, so backups written in
// one test do not share a stamp and the newest-first ordering is decidable
// without waiting on a real clock.
func tickingClock() func() time.Time {
	at := time.Date(2026, 8, 7, 12, 0, 0, 0, time.UTC)
	return func() time.Time {
		at = at.Add(time.Second)
		return at
	}
}

// TestATurnEndBacksUpTheTranscript is the writer's central claim: a turn that
// ended is a transcript at rest, and a copy of it exists afterwards.
func TestATurnEndBacksUpTheTranscript(t *testing.T) {
	// Arrange.
	cwd, cfg := backupWorkspace(t, "uuid-live", "{\"a\":1}\n")
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s1", CWD: cwd, ConfigDir: cfg, ClaudeSessionID: "uuid-live"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	r := &RegistryRegistrar{Reg: reg, Logf: t.Logf,
		Backups: &TranscriptBackups{Reg: reg, Logf: t.Logf, Now: tickingClock()}}

	// Act.
	r.TurnEndObserved("s1", 1786117000000)

	// Assert.
	names := backupNames(t, cwd)
	if len(names) != 1 {
		t.Fatalf("backups = %v, want exactly one", names)
	}
	if uuid, ok := backupUUID(names[0]); !ok || uuid != "uuid-live" {
		t.Fatalf("backup %q does not name its conversation", names[0])
	}
}

// TestARotationBacksUpTheRETIRINGConversation: a /clear retires a transcript
// forever, and the record already points at its successor by the time the
// rotation is observed. Backing up "the current transcript" would copy the new
// empty one and lose the only conversation that will never be appended to
// again.
func TestARotationBacksUpTheRETIRINGConversation(t *testing.T) {
	// Arrange.
	cwd, cfg := backupWorkspace(t, "uuid-old", "{\"a\":1}\n")
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s1", CWD: cwd, ConfigDir: cfg, ClaudeSessionID: "uuid-old"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	r := &RegistryRegistrar{Reg: reg, Logf: t.Logf,
		Backups: &TranscriptBackups{Reg: reg, Logf: t.Logf, Now: tickingClock()}}

	// Act — the vendor rotates to a brand-new conversation.
	rotated, previous, _ := r.AdoptVendorSessionID("s1", "uuid-new")

	// Assert.
	if !rotated || previous != "uuid-old" {
		t.Fatalf("rotation = (%v, %q), want (true, uuid-old)", rotated, previous)
	}
	names := backupNames(t, cwd)
	if len(names) != 1 {
		t.Fatalf("backups = %v, want exactly one", names)
	}
	if uuid, _ := backupUUID(names[0]); uuid != "uuid-old" {
		t.Fatalf("backed up %q, want the RETIRING conversation uuid-old", uuid)
	}
}

// TestPruningKeepsTheBound: an unbounded backup directory is a disk leak in
// every long-lived workspace.
func TestPruningKeepsTheBound(t *testing.T) {
	// Arrange.
	cwd, cfg := backupWorkspace(t, "uuid-live", "{\"a\":1}\n")
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s1", CWD: cwd, ConfigDir: cfg, ClaudeSessionID: "uuid-live"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	b := &TranscriptBackups{Reg: reg, Logf: t.Logf, Now: tickingClock()}

	// Act — more turns than the retention bound.
	for range defaultBackupRetention + 3 {
		b.Capture("s1")
	}

	// Assert.
	if names := backupNames(t, cwd); len(names) != defaultBackupRetention {
		t.Fatalf("kept %d backups (%v), want %d", len(names), names, defaultBackupRetention)
	}
}

// TestPruningKeepsTheNEWEST: keeping an arbitrary five would make the bound a
// lottery rather than a retention window.
func TestPruningKeepsTheNEWEST(t *testing.T) {
	// Arrange.
	cwd, cfg := backupWorkspace(t, "uuid-live", "{\"a\":1}\n")
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s1", CWD: cwd, ConfigDir: cfg, ClaudeSessionID: "uuid-live"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	b := &TranscriptBackups{Reg: reg, Logf: t.Logf, Now: tickingClock()}
	for range defaultBackupRetention + 3 {
		b.Capture("s1")
	}

	// Act.
	names := backupNames(t, cwd)

	// Assert — every survivor is newer than every backup that was pruned, and
	// the oldest stamps are the ones gone.
	oldest := backupStamp(names[0])
	for _, n := range names {
		if s := backupStamp(n); s < oldest {
			oldest = s
		}
	}
	if oldest <= "20260807T120003" {
		t.Fatalf("survivors %v include a backup older than the retention window", names)
	}
}

// TestTheRetentionBoundIsConfigurable pins the env knob; a workspace that
// turns over quickly may want a deeper window.
func TestTheRetentionBoundIsConfigurable(t *testing.T) {
	// Arrange.
	t.Setenv(backupRetentionEnv, "2")

	// Act.
	got := backupRetention()

	// Assert.
	if got != 2 {
		t.Fatalf("backupRetention() = %d, want 2", got)
	}
}

// TestAnUnusableRetentionSettingIsIgnored: "0" from a typo would silently
// disable the whole mechanism, which is exactly the quiet failure the plane
// exists to prevent.
func TestAnUnusableRetentionSettingIsIgnored(t *testing.T) {
	// Arrange.
	t.Setenv(backupRetentionEnv, "0")

	// Act.
	got := backupRetention()

	// Assert.
	if got != defaultBackupRetention {
		t.Fatalf("backupRetention() = %d, want the default %d", got, defaultBackupRetention)
	}
}

// TestABackupWriteFailureNeverFailsTheTurn: the conversation is intact; what
// was lost is the safety net under it, and refusing the turn would trade a
// recoverable loss for an unrecoverable one.
func TestABackupWriteFailureNeverFailsTheTurn(t *testing.T) {
	// Arrange — the backup directory's parent is a FILE, so no copy can land.
	cwd, cfg := backupWorkspace(t, "uuid-live", "{\"a\":1}\n")
	if err := os.MkdirAll(filepath.Join(cwd, ".claude"), 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	if err := os.WriteFile(filepath.Join(cwd, ".claude", "emacs"), []byte("not a directory"), 0o600); err != nil {
		t.Fatalf("write blocker: %v", err)
	}
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s1", CWD: cwd, ConfigDir: cfg, ClaudeSessionID: "uuid-live"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	var logged []string
	b := &TranscriptBackups{Reg: reg, Now: tickingClock(),
		Logf: func(f string, a ...any) { logged = append(logged, strings.ToLower(fmt.Sprintf(f, a...))) }}

	// Act — this must simply return.
	b.Capture("s1")

	// Assert — and it must have said so.
	var loud bool
	for _, line := range logged {
		if strings.Contains(line, "error") {
			loud = true
		}
	}
	if !loud {
		t.Fatalf("a failed backup logged %v, want a loud failure", logged)
	}
}

// TestADeletedTranscriptIsRecoveredAndResumed is the plane's whole point, end
// to end: the file is gone, the record still names it, and the respawn comes
// up resuming the very conversation that was lost.
func TestADeletedTranscriptIsRecoveredAndResumed(t *testing.T) {
	// Arrange — a workspace that ran a turn (so a backup exists), whose
	// transcript is then deleted.
	cwd, cfg := backupWorkspace(t, "uuid-live", "{\"a\":1}\n{\"b\":2}\n")
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{
		SessionID: "s1", CWD: cwd, ConfigDir: cfg,
		ClaudeSessionID: "uuid-live", LastTurnEndMs: 1786117000000,
	}); err != nil {
		t.Fatalf("put: %v", err)
	}
	(&TranscriptBackups{Reg: reg, Logf: t.Logf, Now: tickingClock()}).Capture("s1")
	if err := os.Remove(session.TranscriptPath(session.ClaudeConfigDir(cfg), cwd, "uuid-live")); err != nil {
		t.Fatalf("remove transcript: %v", err)
	}
	var got CreateOpts
	sp := NewShimSpawner(reg,
		func(string) (bool, error) { return false, nil }, nil,
		func(_ string, opts CreateOpts) (ShimHandle, error) { got = opts; return ShimHandle{}, nil },
		t.Logf)

	// Act.
	res, err := sp.EnsureShim(context.Background(), "s1")

	// Assert.
	if err != nil {
		t.Fatalf("EnsureShim over a restorable transcript: %v", err)
	}
	if got.Resume != "uuid-live" || res.Resumed != "uuid-live" {
		t.Fatalf("spawned resume=%q res.Resumed=%q, want uuid-live", got.Resume, res.Resumed)
	}
}

// TestARestoredTranscriptIsPutBackAtTheVendorPath: resuming it is only correct
// if the CLI can find it where it looks.
func TestARestoredTranscriptIsPutBackAtTheVendorPath(t *testing.T) {
	// Arrange.
	cwd, cfg := backupWorkspace(t, "uuid-live", "{\"a\":1}\n")
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s1", CWD: cwd, ConfigDir: cfg, ClaudeSessionID: "uuid-live"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	(&TranscriptBackups{Reg: reg, Logf: t.Logf, Now: tickingClock()}).Capture("s1")
	vendorPath := session.TranscriptPath(session.ClaudeConfigDir(cfg), cwd, "uuid-live")
	if err := os.Remove(vendorPath); err != nil {
		t.Fatalf("remove transcript: %v", err)
	}

	// Act.
	restored, err := restoreTranscript(cwd, cfg, "uuid-live")

	// Assert.
	if err != nil {
		t.Fatalf("restoreTranscript: %v", err)
	}
	if restored.Destination != vendorPath {
		t.Fatalf("restored to %s, want the vendor path %s", restored.Destination, vendorPath)
	}
	if _, statErr := os.Stat(vendorPath); statErr != nil {
		t.Fatalf("the vendor path is still missing after a restore: %v", statErr)
	}
}

// TestARestoreCountsWhatItRecovered: a restore is an invariant violation being
// repaired, and its record has to be worth reading.
func TestARestoreCountsWhatItRecovered(t *testing.T) {
	// Arrange.
	cwd, cfg := backupWorkspace(t, "uuid-live", "{\"a\":1}\n{\"b\":2}\n{\"c\":3}\n")
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s1", CWD: cwd, ConfigDir: cfg, ClaudeSessionID: "uuid-live"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	(&TranscriptBackups{Reg: reg, Logf: t.Logf, Now: tickingClock()}).Capture("s1")
	if err := os.Remove(session.TranscriptPath(session.ClaudeConfigDir(cfg), cwd, "uuid-live")); err != nil {
		t.Fatalf("remove transcript: %v", err)
	}

	// Act.
	restored, err := restoreTranscript(cwd, cfg, "uuid-live")

	// Assert.
	if err != nil {
		t.Fatalf("restoreTranscript: %v", err)
	}
	if restored.Records != 3 || restored.Bytes == 0 {
		t.Fatalf("restore reported records=%d bytes=%d, want 3 records and a non-zero size", restored.Records, restored.Bytes)
	}
}

// TestAWorkspaceWithNoBackupFallsThroughToTheEvidenceCheck: errNoBackup is the
// one restore outcome that is not a failure, and reading it as one would turn
// every brand-new workspace into a hard fault.
func TestAWorkspaceWithNoBackupFallsThroughToTheEvidenceCheck(t *testing.T) {
	// Arrange.
	cwd := t.TempDir()

	// Act.
	_, err := restoreTranscript(cwd, t.TempDir(), "uuid-live")

	// Assert.
	if !errors.Is(err, errNoBackup) {
		t.Fatalf("err = %v, want errNoBackup", err)
	}
}

// TestAnUnusableBackupIsAHardFault: a corrupt copy means the conversation is
// BOTH missing and unrecoverable, which is strictly worse news than having no
// backup — and quietly starting fresh on it would destroy the last thing that
// might have been repairable by hand.
func TestAnUnusableBackupIsAHardFault(t *testing.T) {
	// Arrange — a backup file that holds no conversation records.
	cwd, cfg := backupWorkspace(t, "uuid-live", "{\"a\":1}\n")
	if err := os.MkdirAll(backupDir(cwd), 0o755); err != nil {
		t.Fatalf("mkdir backups: %v", err)
	}
	empty := filepath.Join(backupDir(cwd), backupName("uuid-live", time.Date(2026, 8, 7, 12, 0, 0, 0, time.UTC)))
	if err := os.WriteFile(empty, []byte("\n \n"), 0o600); err != nil {
		t.Fatalf("write empty backup: %v", err)
	}
	if err := os.Remove(session.TranscriptPath(session.ClaudeConfigDir(cfg), cwd, "uuid-live")); err != nil {
		t.Fatalf("remove transcript: %v", err)
	}

	// Act.
	restored, err := attemptTranscriptRestore(t.Logf, "automatic_restore", "s1",
		CreateOpts{CWD: cwd, ConfigDir: cfg, Resume: "uuid-live"})

	// Assert.
	if restored {
		t.Fatal("an empty backup must not count as a restore")
	}
	if !errors.Is(err, errclass.ErrConversationUnresumable) {
		t.Fatalf("err = %v, want errclass.ErrConversationUnresumable", err)
	}
}

// TestAnUnusableBackupNamesBothFailures: a human told only "unresumable" does
// not know the backup was tried, so does not know the backup is also broken.
func TestAnUnusableBackupNamesBothFailures(t *testing.T) {
	// Arrange.
	cwd, cfg := backupWorkspace(t, "uuid-live", "{\"a\":1}\n")
	if err := os.MkdirAll(backupDir(cwd), 0o755); err != nil {
		t.Fatalf("mkdir backups: %v", err)
	}
	empty := filepath.Join(backupDir(cwd), backupName("uuid-live", time.Date(2026, 8, 7, 12, 0, 0, 0, time.UTC)))
	if err := os.WriteFile(empty, nil, 0o600); err != nil {
		t.Fatalf("write empty backup: %v", err)
	}

	// Act.
	_, err := attemptTranscriptRestore(t.Logf, "automatic_restore", "s1",
		CreateOpts{CWD: cwd, ConfigDir: cfg, Resume: "uuid-live"})

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "backup could not be restored") {
		t.Fatalf("err = %v, want both failures named", err)
	}
}

// TestARestoreOnlyConsidersTheNamedConversation: putting back some OTHER
// conversation because it happened to be newer would be the resume gate
// landing a workspace on a transcript it never asked for.
func TestARestoreOnlyConsidersTheNamedConversation(t *testing.T) {
	// Arrange — a newer backup of a DIFFERENT conversation.
	cwd, cfg := backupWorkspace(t, "uuid-wanted", "{\"a\":1}\n")
	if err := os.MkdirAll(backupDir(cwd), 0o755); err != nil {
		t.Fatalf("mkdir backups: %v", err)
	}
	base := time.Date(2026, 8, 7, 12, 0, 0, 0, time.UTC)
	if err := os.WriteFile(filepath.Join(backupDir(cwd), backupName("uuid-wanted", base)), []byte("{\"a\":1}\n"), 0o600); err != nil {
		t.Fatalf("write wanted backup: %v", err)
	}
	if err := os.WriteFile(filepath.Join(backupDir(cwd), backupName("uuid-other", base.Add(time.Hour))), []byte("{\"z\":9}\n"), 0o600); err != nil {
		t.Fatalf("write other backup: %v", err)
	}

	// Act.
	restored, err := restoreTranscript(cwd, cfg, "uuid-wanted")

	// Assert.
	if err != nil {
		t.Fatalf("restoreTranscript: %v", err)
	}
	if restored.UUID != "uuid-wanted" {
		t.Fatalf("restored %s, want the named conversation uuid-wanted", restored.UUID)
	}
}

// TestAStrayFileIsNotABackup: a human's notes.txt in the backup directory must
// never be restored as a transcript.
func TestAStrayFileIsNotABackup(t *testing.T) {
	// Arrange.
	cwd := t.TempDir()
	if err := os.MkdirAll(backupDir(cwd), 0o755); err != nil {
		t.Fatalf("mkdir backups: %v", err)
	}
	if err := os.WriteFile(filepath.Join(backupDir(cwd), "notes.txt"), []byte("hello"), 0o600); err != nil {
		t.Fatalf("write stray: %v", err)
	}

	// Act.
	_, err := restoreTranscript(cwd, t.TempDir(), "")

	// Assert.
	if !errors.Is(err, errNoBackup) {
		t.Fatalf("err = %v, want a stray file to leave the workspace with no backup", err)
	}
}
