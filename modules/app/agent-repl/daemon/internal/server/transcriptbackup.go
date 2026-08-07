// transcriptbackup.go makes transcript loss RECOVERABLE, so the resume
// ladder's hard fault is reached only when both the vendor transcript and
// every backup of it are gone.
//
// WHY THIS EXISTS. The ladder in freshgate.go refuses to replace a
// conversation it cannot resume, which is right and which is also, on its own,
// a dead end: a workspace whose transcript went missing is stuck until a human
// finds the file. The refusal is the correct LAST rung, not the correct only
// one. A transcript is an append-only .jsonl the daemon watches every turn
// boundary of, so it can be copied cheaply and put back exactly.
//
// WHERE THE BACKUPS LIVE. <workspace-root>/.claude/emacs/claude-session-backups/,
// beside the work they belong to rather than in a daemon-global cache. Two
// reasons, and both are about the file surviving the thing that lost the
// original: a worktree that is moved, archived, or copied to another machine
// carries its own conversation history with it, and a cache wipe (which is a
// routine act, ~/.cache being ~/.cache) cannot take the last copy with it.
//
// WHO OWNS THE WRITE. The daemon, at the two moments it — and only it — knows
// a transcript has just reached a stable state worth keeping:
//
//   - a turn ENDED, which is the vendor having finished appending to the file;
//   - the vendor uuid ROTATED (a /clear, a compact), which retires one
//     transcript for another and is the one moment a conversation stops being
//     appended to forever.
//
// The sidecar owns the session file plane and was the other candidate. It was
// not chosen because it is a per-file tail follower with no notion of turn
// boundaries or uuid rotation — the two facts that decide WHEN a copy is worth
// taking — and wiring those into it would mean teaching it the registry.
//
// A BACKUP FAILURE NEVER FAILS A TURN. It is loud (an ERROR through the
// workspace-owned canonical logger) and it is otherwise inert: refusing to end
// a turn because a copy could not be made would trade a recoverable loss for
// an unrecoverable one.
package server

import (
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/errclass"
	"claude-repld/internal/registry"
	"claude-repld/internal/session"
)

// backupDirName is the path, relative to the workspace root, that every
// backup for that workspace lives under.
var backupDirRelative = filepath.Join(".claude", "emacs", "claude-session-backups")

// backupSuffix is the extension every backup carries. It matches the vendor's
// own so a human who finds one knows what they are looking at, and so a
// restore is a copy rather than a conversion.
const backupSuffix = ".jsonl"

// defaultBackupRetention is how many backups a workspace keeps. It is a
// per-workspace bound, not a global one: the cost is proportional to the
// number of workspaces, which is the same thing the transcripts themselves
// already cost.
const defaultBackupRetention = 5

// backupRetentionEnv overrides defaultBackupRetention, following the
// AGENT_REPL_* convention the daemon's other knobs use. A value that is not a
// positive integer is IGNORED rather than read as zero: "0" from a typo would
// silently disable the whole mechanism.
const backupRetentionEnv = "AGENT_REPL_TRANSCRIPT_BACKUPS"

// backupRetention resolves how many backups to keep per workspace.
func backupRetention() int {
	raw := os.Getenv(backupRetentionEnv)
	if raw == "" {
		return defaultBackupRetention
	}
	n, err := strconv.Atoi(strings.TrimSpace(raw))
	if err != nil || n <= 0 {
		return defaultBackupRetention
	}
	return n
}

// backupDir is where cwd's backups live.
func backupDir(cwd string) string { return filepath.Join(cwd, backupDirRelative) }

// backupName renders one backup's file name. The uuid comes FIRST and is
// separated from the stamp by a dot, so the conversation a backup belongs to
// is recoverable from the name alone — which is what lets a restore find the
// newest copy of one specific conversation rather than the newest copy of
// anything.
//
// The stamp is UTC and lexicographically sortable, so newest-first is a string
// sort and does not depend on a filesystem's mtime resolution.
func backupName(uuid string, at time.Time) string {
	return uuid + "." + at.UTC().Format("20060102T150405.000Z") + backupSuffix
}

// backupUUID recovers the conversation uuid from a backup's file name, and
// reports whether the name is one of ours at all. A stray file a human dropped
// in the directory is not a backup and must never be restored as one.
func backupUUID(name string) (string, bool) {
	if !strings.HasSuffix(name, backupSuffix) {
		return "", false
	}
	base := strings.TrimSuffix(name, backupSuffix)
	dot := strings.IndexByte(base, '.')
	if dot <= 0 || dot == len(base)-1 {
		return "", false
	}
	return base[:dot], true
}

// TranscriptBackups copies a workspace's vendor transcript aside at the two
// boundaries that matter, and prunes what it keeps.
//
// It is a struct rather than free functions because the two callers are the
// registrar's observers, which already hold the registry and the log; nothing
// here has state of its own.
type TranscriptBackups struct {
	// Reg is the persistent session registry (required). It is what turns a
	// session id into the (config_dir, cwd, uuid) triple a transcript path is
	// built from.
	Reg *registry.Registry
	// Logf receives the account. Optional; a nil log makes the writer silent,
	// which is only ever right in a unit harness.
	Logf func(string, ...any)
	// Now is the clock the stamp comes from. Nil means time.Now.
	Now func() time.Time
}

func (b *TranscriptBackups) now() time.Time {
	if b == nil || b.Now == nil {
		return time.Now()
	}
	return b.Now()
}

func (b *TranscriptBackups) logf(format string, args ...any) {
	if b != nil && b.Logf != nil {
		b.Logf(format, args...)
	}
}

// Capture copies sessionID's CURRENT transcript aside. It is the turn-end
// entry point: whatever the record names right now is what has just been
// appended to.
func (b *TranscriptBackups) Capture(sessionID string) {
	if b == nil || b.Reg == nil {
		return
	}
	rec, ok := b.Reg.Get(sessionID)
	if !ok {
		return
	}
	b.CaptureConversation(rec.CWD, rec.ConfigDir, rec.ClaudeSessionID, "turn_end")
}

// CaptureConversation copies ONE named conversation's transcript aside. It is
// the rotation entry point, which must name the RETIRING uuid explicitly: by
// the time a rotation is observed the record already points at its successor,
// and capturing "the record's current transcript" would back up the new empty
// one and leave the retired conversation — the only one that will never be
// appended to again — with no copy at all.
func (b *TranscriptBackups) CaptureConversation(cwd, configDir, uuid, reason string) {
	if b == nil || cwd == "" || uuid == "" {
		return
	}
	src, exists := session.TranscriptExists(configDir, cwd, uuid)
	if !exists {
		// NOT AN ERROR. A turn can end before the vendor has flushed its first
		// line, and a rotation can retire a uuid that never had a file. Said at
		// ordinary volume so the absence is visible without crying wolf.
		b.logf("server: transcript backup SKIPPED cwd=%s uuid=%s reason=%s — the vendor transcript does not exist at %s",
			cwd, uuid, reason, src)
		return
	}
	dst := filepath.Join(backupDir(cwd), backupName(uuid, b.now()))
	bytes, err := copyFile(src, dst)
	if err != nil {
		// LOUD, AND THE TURN IS UNAFFECTED. The conversation is intact; what
		// was lost is the safety net under it, and a silent net is worse than
		// no net because nobody knows to look for the file.
		dlog.Tag(dlog.Logf(b.logf),
			"event", "transcript_backup_failure",
			"cwd", cwd,
			"config_dir", configDir,
			"claude_session_id", uuid,
			"reason", reason,
			"source", src,
			"destination", dst,
		)("ERROR: a transcript backup could not be written; this conversation has no recovery copy for this boundary: %v", err)
		return
	}
	b.logf("server: transcript backup WROTE cwd=%s uuid=%s reason=%s bytes=%d path=%s",
		cwd, uuid, reason, bytes, dst)
	b.prune(cwd)
}

// prune keeps the newest backupRetention() backups for cwd and removes the
// rest. It is deliberately workspace-wide rather than per-uuid: a workspace
// that rotates its conversation every day would otherwise keep five copies of
// every conversation it has ever had, forever.
func (b *TranscriptBackups) prune(cwd string) {
	dir := backupDir(cwd)
	entries, err := os.ReadDir(dir)
	if err != nil {
		b.logf("server: transcript backup prune FAILED cwd=%s dir=%s — old backups will accumulate: %v", cwd, dir, err)
		return
	}
	var names []string
	for _, e := range entries {
		if e.IsDir() {
			continue
		}
		if _, ok := backupUUID(e.Name()); ok {
			names = append(names, e.Name())
		}
	}
	keep := backupRetention()
	if len(names) <= keep {
		return
	}
	// Newest first. The stamp sits after the uuid, so this is a sort by
	// (conversation, time) — which is not the ordering we want. Sort on the
	// stamp alone.
	sort.Slice(names, func(i, j int) bool { return backupStamp(names[i]) > backupStamp(names[j]) })
	for _, name := range names[keep:] {
		path := filepath.Join(dir, name)
		if err := os.Remove(path); err != nil {
			b.logf("server: transcript backup prune could not remove %s: %v", path, err)
			continue
		}
		b.logf("server: transcript backup PRUNED cwd=%s path=%s (keeping the %d newest)", cwd, path, keep)
	}
}

// backupStamp returns the sortable time portion of a backup's name, or "" for
// a name that is not one of ours.
func backupStamp(name string) string {
	if _, ok := backupUUID(name); !ok {
		return ""
	}
	base := strings.TrimSuffix(name, backupSuffix)
	return base[strings.IndexByte(base, '.')+1:]
}

// backupRestore is what one successful restore did, for the record it is
// announced through.
type backupRestore struct {
	// Source is the backup that was put back.
	Source string
	// Destination is the vendor transcript path it was restored to.
	Destination string
	// UUID is the conversation the restore recovered.
	UUID string
	// Bytes and Records are what was copied. Records is the .jsonl line count,
	// which is the unit a human counts a conversation in.
	Bytes   int64
	Records int
}

// errNoBackup reports that no usable backup exists for this workspace. It is
// the ONE outcome of a restore attempt that is not a failure: it means the
// ladder should proceed to its evidence check, which is where a genuinely new
// workspace is told apart from a lost conversation.
var errNoBackup = errors.New("server: no transcript backup exists for this workspace")

// restoreTranscript puts a workspace's newest usable backup back at the vendor
// path the resume gate looks for.
//
// uuid, when non-empty, is the conversation the caller is trying to resume,
// and only backups of THAT conversation are considered. An empty uuid falls
// back to the workspace's newest backup of anything, which is the honest
// answer when the caller has no target: the newest conversation this workspace
// had is the one a restore should return it to.
//
// A restore is an INVARIANT VIOLATION BEING REPAIRED, not routine bookkeeping:
// a transcript went missing that nothing should have removed. It is therefore
// announced at ERROR level with the full structured record, by the caller that
// owns the log for this workspace.
func restoreTranscript(cwd, configDir, uuid string) (backupRestore, error) {
	dir := backupDir(cwd)
	entries, err := os.ReadDir(dir)
	if err != nil {
		// A missing directory is a workspace that never had a backup taken,
		// which is exactly errNoBackup and not a failure to report.
		if os.IsNotExist(err) {
			return backupRestore{}, errNoBackup
		}
		return backupRestore{}, fmt.Errorf("server: reading the transcript backup directory %s: %w", dir, err)
	}
	var candidates []string
	for _, e := range entries {
		if e.IsDir() {
			continue
		}
		found, ok := backupUUID(e.Name())
		if !ok {
			continue
		}
		if uuid != "" && found != uuid {
			continue
		}
		candidates = append(candidates, e.Name())
	}
	if len(candidates) == 0 {
		return backupRestore{}, errNoBackup
	}
	sort.Slice(candidates, func(i, j int) bool { return backupStamp(candidates[i]) > backupStamp(candidates[j]) })
	name := candidates[0]
	restoredUUID, _ := backupUUID(name)
	src := filepath.Join(dir, name)
	dst := session.TranscriptPath(configDir, cwd, restoredUUID)

	records, err := countRecords(src)
	if err != nil {
		return backupRestore{}, fmt.Errorf("server: transcript backup %s is unreadable: %w", src, err)
	}
	if records == 0 {
		// An empty backup is not a conversation. Restoring it would satisfy the
		// resume gate with a file the CLI then resumes into as if the exchange
		// had never happened, which is the silent loss this whole mechanism
		// exists to prevent — so it is a hard failure, not a fallthrough.
		return backupRestore{}, fmt.Errorf("server: transcript backup %s holds no conversation records", src)
	}
	bytes, err := copyFile(src, dst)
	if err != nil {
		return backupRestore{}, fmt.Errorf("server: restoring transcript backup %s to %s: %w", src, dst, err)
	}
	return backupRestore{Source: src, Destination: dst, UUID: restoredUUID, Bytes: bytes, Records: records}, nil
}

// newestBackupConversation names the conversation cwd's newest backup belongs
// to. It answers the question a restore-without-a-target leaves open: the
// restore put SOMETHING back, and the caller now needs to know which
// conversation to resume.
//
// It reads the same directory by the same rule restoreTranscript selects by,
// so the two cannot name different files.
func newestBackupConversation(cwd string) (string, bool) {
	entries, err := os.ReadDir(backupDir(cwd))
	if err != nil {
		return "", false
	}
	var names []string
	for _, e := range entries {
		if e.IsDir() {
			continue
		}
		if _, ok := backupUUID(e.Name()); ok {
			names = append(names, e.Name())
		}
	}
	if len(names) == 0 {
		return "", false
	}
	sort.Slice(names, func(i, j int) bool { return backupStamp(names[i]) > backupStamp(names[j]) })
	return backupUUID(names[0])
}

// attemptTranscriptRestore is the resume ladder's RESTORE RUNG, and it is the
// only one. Both gates that can find a transcript missing — the create's
// (Server.CreateSession) and the respawn's (ShimSpawner.EnsureShim) — call
// THIS, so a workspace cannot be recoverable through one door and lost through
// the other.
//
// Its three outcomes are the three the ladder needs, and they are distinct on
// purpose:
//
//   - (true, nil): the conversation is back at the vendor path and the caller
//     resumes it exactly as if it had never gone.
//   - (false, nil): no backup exists. The ladder proceeds to its evidence
//     check, which is where a genuinely new workspace is told apart from a
//     conversation that is simply gone.
//   - (false, err): a backup existed and could not be put back. This is a HARD
//     FAULT and never a fallthrough — a corrupt or unwritable copy means the
//     conversation is BOTH missing and unrecoverable, which is strictly worse
//     news than having no backup at all, and quietly starting fresh on it
//     would destroy the last thing that might still have been repairable by
//     hand.
func attemptTranscriptRestore(logf func(string, ...any), operation, sessionID string, opts CreateOpts) (bool, error) {
	restored, err := restoreTranscript(opts.CWD, opts.ConfigDir, opts.Resume)
	if errors.Is(err, errNoBackup) {
		return false, nil
	}
	if err != nil {
		// The SAME sentinel the ladder's bottom rung uses, because it is the
		// same fact: this workspace has a conversation that cannot be reached
		// and will not be replaced. The prose is what distinguishes the two —
		// it names BOTH failures, so a human knows the backup was tried.
		return false, fmt.Errorf("%w: cwd=%s config_dir=%s: its transcript is missing AND its backup could not be restored (%v); no blank conversation will be started in its place",
			errclass.ErrConversationUnresumable, opts.CWD, opts.ConfigDir, err)
	}
	// AN INVARIANT VIOLATION BEING REPAIRED, NOT ROUTINE BOOKKEEPING. Something
	// removed a transcript that nothing should have removed. The repair is
	// welcome and the cause is not, so this is ERROR-level and carries the full
	// structured record — a restore that scrolled past as an info line would
	// let a workspace silently lose its transcript on every single boot.
	dlog.Tag(dlog.Logf(logf),
		"event", "transcript_restored_from_backup",
		"operation", operation,
		"agent_repl_session_id", sessionID,
		"claude_session_id", restored.UUID,
		"cwd", opts.CWD,
		"config_dir", opts.ConfigDir,
		"source_backup", restored.Source,
		"restored_path", restored.Destination,
		"bytes", restored.Bytes,
		"records", restored.Records,
	)("ERROR: this workspace's Claude transcript was MISSING and has been restored from its backup; a transcript nothing should remove was removed")
	return true, nil
}

// countRecords counts a .jsonl's non-empty lines. It doubles as the
// readability check: a backup that cannot be read cannot be restored, and
// finding that out here is what keeps a corrupt copy from replacing nothing.
func countRecords(path string) (int, error) {
	raw, err := os.ReadFile(path)
	if err != nil {
		return 0, err
	}
	n := 0
	for _, line := range strings.Split(string(raw), "\n") {
		if strings.TrimSpace(line) != "" {
			n++
		}
	}
	return n, nil
}

// copyFile writes src to dst through a temporary file in dst's directory and
// one rename, so a reader never observes a half-written transcript or a
// half-written backup. It returns the byte count copied.
func copyFile(src, dst string) (int64, error) {
	if err := os.MkdirAll(filepath.Dir(dst), 0o755); err != nil {
		return 0, fmt.Errorf("creating %s: %w", filepath.Dir(dst), err)
	}
	in, err := os.Open(src)
	if err != nil {
		return 0, fmt.Errorf("opening %s: %w", src, err)
	}
	defer in.Close()
	tmp, err := os.CreateTemp(filepath.Dir(dst), ".transcript-*")
	if err != nil {
		return 0, fmt.Errorf("creating a temporary file beside %s: %w", dst, err)
	}
	tmpName := tmp.Name()
	n, copyErr := io.Copy(tmp, in)
	closeErr := tmp.Close()
	if copyErr != nil {
		os.Remove(tmpName)
		return 0, fmt.Errorf("copying %s: %w", src, copyErr)
	}
	if closeErr != nil {
		os.Remove(tmpName)
		return 0, fmt.Errorf("closing the copy of %s: %w", src, closeErr)
	}
	if err := os.Rename(tmpName, dst); err != nil {
		os.Remove(tmpName)
		return 0, fmt.Errorf("publishing %s: %w", dst, err)
	}
	return n, nil
}
