// Package registry persists the daemon's session records across
// restarts. The in-memory session map in internal/server dies with the
// process; this registry is the durable copy that lets a restarted
// daemon keep resolving the s_<hex> ids its frontends still hold
// (rehydrating each into a live --resume session on first access).
//
// Durability contract: every mutation writes through under a cross-process
// lock. Each file is atomic (temp + fsync + rename); the monotonic checkpoint
// sidecar is written before the session roster, so a crash between them can
// leave a checkpoint ahead but can never lose replay/backfill progress.
// Nothing depends on a shutdown hook to flush.
package registry

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"sync"
	"syscall"
	"time"
)

const (
	// fileVersionCheckpointIndex introduced the durable conversation checkpoint
	// index and bounded terminal-record retention.
	fileVersionCheckpointIndex = 2

	// TerminalRetention is the maximum number of terminal SessionView records
	// retained in the registry. Live records are never pruned. Conversation
	// replay/backfill state survives separately in Checkpoints.
	TerminalRetention = 128
)

// Record is one session's durable registry entry.
type Record struct {
	// SessionID is the daemon-minted s_<hex> id — the key frontends
	// hold, and the id under which the session rehydrates.
	SessionID      string `json:"session_id"`
	CWD            string `json:"cwd,omitempty"`
	Model          string `json:"model,omitempty"`
	PermissionMode string `json:"permission_mode,omitempty"`
	// ConfigDir is the session's CLAUDE_CONFIG_DIR (the account its CLI
	// runs under). Persisted because the transcript a rehydration
	// resumes lives under THIS root, not the daemon's own: without it a
	// restart would stat ~/.claude for a ~/.claude-chesscom transcript,
	// find nothing, and prune a perfectly live conversation.
	ConfigDir string `json:"config_dir,omitempty"`
	// ClaudeSessionID is the durable CLI session uuid (the --resume
	// target). Empty until system:init reports it; a record never
	// filled in cannot be rehydrated.
	ClaudeSessionID string `json:"claude_session_id,omitempty"`
	CreatedAt       string `json:"created_at,omitempty"`
	// Terminal marks a conversation that ended for a session-scoped
	// reason (user delete, shim death) — NOT a daemon shutdown, which
	// deliberately leaves records non-terminal so they rehydrate.
	Terminal    bool   `json:"terminal,omitempty"`
	DeathReason string `json:"death_reason,omitempty"`
	// TerminalAt records when the record became terminal. It orders the bounded
	// recent SessionView retention set; legacy records are migrated from
	// CreatedAt, with session id as the deterministic final tie-break.
	TerminalAt string `json:"terminal_at,omitempty"`
	// LastSeq is the highest agent-shim store seq the daemon has durably
	// observed for this session — the shimclient replay high-water mark
	// (design §4.4). Persisting it here (rather than in the shimclient,
	// which persists nothing itself) is what lets a restarted daemon
	// re-Subscribe from where it left off and reattach without re-replaying
	// or losing events. Zero means "never subscribed"; a fresh subscribe
	// from seq 0 then replays the whole session. See server.RegistrySeqStore.
	LastSeq uint64 `json:"last_seq,omitempty"`
	// BackfillState is the never-blue completion signal for this session's
	// on-disk transcript: "" (nothing to backfill), "pending", "done", or
	// "failed". See frontendv1.BackfillState for the full semantics.
	//
	// PERSISTED rather than derived fresh each boot, because the evidence it
	// is derived from does not survive one: the daemon re-Subscribes from
	// LastSeq, so the file-plane events that proved a backfill landed are
	// never re-delivered. Without this a restarted daemon would report a
	// long-since-backfilled session as PENDING forever.
	BackfillState string `json:"backfill_state,omitempty"`
	// QueuedPrompts are the prompts the daemon is currently HOLDING for
	// this session because a turn was in flight when they were submitted
	// (E4). Persisted for crash honesty: these are things the user typed
	// that the agent has not seen yet, so a daemon that dies holding them
	// must leave a record of what it was holding rather than lose them
	// with no trace. Empty for a session with nothing queued, which is
	// the overwhelmingly common case.
	QueuedPrompts []QueuedPrompt `json:"queued_prompts,omitempty"`
}

// ConversationIdentity names one transcript/store sequence space. The vendor
// uuid alone is insufficient: the same uuid may legitimately exist under
// different account roots and workspaces.
type ConversationIdentity struct {
	ConfigDir       string `json:"config_dir,omitempty"`
	CWD             string `json:"cwd"`
	ClaudeSessionID string `json:"claude_session_id"`
}

// ConversationCheckpoint is the compact durable state that must outlive
// terminal SessionView records.
type ConversationCheckpoint struct {
	ConversationIdentity
	LastSeq       uint64 `json:"last_seq,omitempty"`
	BackfillState string `json:"backfill_state,omitempty"`
}

// QueuedPrompt is one held prompt's durable form. Only the fields needed to
// know WHAT was held and WHEN are persisted: a classification is a live
// judgment about a turn that will not exist after a restart, so persisting one
// would preserve an answer to a question that no longer applies.
type QueuedPrompt struct {
	ID             string `json:"id"`
	Text           string `json:"text"`
	PermissionMode string `json:"permission_mode,omitempty"`
	QueuedAtMs     int64  `json:"queued_at_ms,omitempty"`
}

// fileShape is the on-disk JSON document.
type fileShape struct {
	Version     int                      `json:"version"`
	Sessions    []Record                 `json:"sessions"`
	Checkpoints []ConversationCheckpoint `json:"conversation_checkpoints,omitempty"`
}

type checkpointFileShape struct {
	Version     int                      `json:"version"`
	Checkpoints []ConversationCheckpoint `json:"conversation_checkpoints"`
}

// Registry is a write-through, crash-safe session record store.
type Registry struct {
	path string
	logf func(string, ...any)

	mu          sync.Mutex
	records     map[string]Record
	checkpoints map[ConversationIdentity]ConversationCheckpoint
	now         func() time.Time
	loadErr     error
}

// DefaultPath returns $AGENT_REPL_STATE_DIR/claude-repld-sessions.json,
// defaulting the root to ~/.claude-emacs — the same root the sentinel
// side channel and the hook scripts resolve.
func DefaultPath() (string, error) {
	root := os.Getenv("AGENT_REPL_STATE_DIR")
	if root == "" {
		home, err := os.UserHomeDir()
		if err != nil {
			return "", fmt.Errorf("registry: resolve home dir: %w", err)
		}
		root = filepath.Join(home, ".claude-emacs")
	}
	return filepath.Join(root, "claude-repld-sessions.json"), nil
}

// Open loads the registry at path. A missing file starts empty (first boot).
// A file that cannot be read or parsed records a sticky load error; Prepare
// returns it so production startup fails loudly. Corrupt bytes are copied to
// <path>.corrupt while the original remains in place, preventing a later
// restart from silently treating the registry as a first boot.
func Open(path string, logf func(string, ...any)) *Registry {
	r := &Registry{
		path: path, logf: logf,
		records:     map[string]Record{},
		checkpoints: map[ConversationIdentity]ConversationCheckpoint{},
		now:         time.Now,
	}
	r.records, r.checkpoints, r.loadErr = r.loadStateLocked()
	return r
}

// loadStateLocked reads the on-disk records and both checkpoint copies. A
// missing sessions file is a first boot. Any malformed/unsupported state
// returns an error; no partial registry is admitted.
//
// Callers hold r.mu (Open is pre-publication, mutate holds it).
func (r *Registry) loadStateLocked() (map[string]Record, map[ConversationIdentity]ConversationCheckpoint, error) {
	records := map[string]Record{}
	checkpoints := map[ConversationIdentity]ConversationCheckpoint{}
	data, err := os.ReadFile(r.path)
	if err != nil {
		if !os.IsNotExist(err) {
			loadErr := fmt.Errorf("registry: read %s: %w", r.path, err)
			r.logf("registry: READ FAILED for %s — refusing migration/compaction: %v", r.path, err)
			return records, checkpoints, loadErr
		}
		sidecar, sidecarErr := r.loadCheckpointSidecarLocked()
		if sidecarErr != nil {
			return records, checkpoints, sidecarErr
		}
		for _, cp := range sidecar {
			if err := mergeCheckpoint(checkpoints, cp, r.checkpointPath()); err != nil {
				return records, checkpoints, err
			}
		}
		return records, checkpoints, nil
	}
	var doc fileShape
	if err := json.Unmarshal(data, &doc); err != nil {
		r.logf("registry: CORRUPT file at %s — refusing migration/compaction: %v", r.path, err)
		corrupt := r.path + ".corrupt"
		if copyErr := os.WriteFile(corrupt, data, 0o600); copyErr != nil {
			r.logf("registry: could not preserve corrupt file at %s: %v", corrupt, copyErr)
		} else {
			r.logf("registry: corrupt file copied to %s; original retained so every restart still fails loudly", corrupt)
		}
		return records, checkpoints, fmt.Errorf("registry: parse %s: %w", r.path, err)
	}
	if doc.Version != 1 && doc.Version != fileVersionCheckpointIndex {
		err := fmt.Errorf("registry: unsupported version %d in %s", doc.Version, r.path)
		r.logf("registry: UNSUPPORTED version — refusing migration/compaction: %v", err)
		return records, checkpoints, err
	}
	for _, rec := range doc.Sessions {
		if rec.SessionID == "" {
			err := fmt.Errorf("registry: record with empty session_id in %s", r.path)
			r.logf("registry: INVALID record with empty session_id in %s — refusing migration/compaction", r.path)
			return records, checkpoints, err
		}
		records[rec.SessionID] = rec
	}
	for _, cp := range doc.Checkpoints {
		if err := mergeCheckpoint(checkpoints, cp, r.path); err != nil {
			r.logf("registry: INVALID embedded conversation checkpoint — refusing migration/compaction: %v", err)
			return records, checkpoints, err
		}
	}
	sidecar, err := r.loadCheckpointSidecarLocked()
	if err != nil {
		return records, checkpoints, err
	}
	for _, cp := range sidecar {
		if err := mergeCheckpoint(checkpoints, cp, r.checkpointPath()); err != nil {
			r.logf("registry: INVALID sidecar conversation checkpoint — refusing migration/compaction: %v", err)
			return records, checkpoints, err
		}
	}
	return records, checkpoints, nil
}

func mergeCheckpoint(dst map[ConversationIdentity]ConversationCheckpoint, cp ConversationCheckpoint, source string) error {
	id := cp.ConversationIdentity
	if id.CWD == "" || id.ClaudeSessionID == "" {
		return fmt.Errorf("registry: invalid conversation checkpoint in %s: config_dir=%q cwd=%q claude_session_id=%q",
			source, id.ConfigDir, id.CWD, id.ClaudeSessionID)
	}
	if !validBackfill(cp.BackfillState) {
		return fmt.Errorf("registry: checkpoint %+v in %s has invalid backfill_state %q",
			id, source, cp.BackfillState)
	}
	if prior, ok := dst[id]; ok {
		cp.LastSeq = max(cp.LastSeq, prior.LastSeq)
		cp.BackfillState = strongerBackfill(prior.BackfillState, cp.BackfillState)
	}
	dst[id] = cp
	return nil
}

func (r *Registry) checkpointPath() string { return r.path + ".checkpoints" }

func (r *Registry) loadCheckpointSidecarLocked() ([]ConversationCheckpoint, error) {
	data, err := os.ReadFile(r.checkpointPath())
	if os.IsNotExist(err) {
		return nil, nil
	}
	if err != nil {
		return nil, fmt.Errorf("registry: read checkpoint sidecar %s: %w", r.checkpointPath(), err)
	}
	var doc checkpointFileShape
	if err := json.Unmarshal(data, &doc); err != nil {
		return nil, fmt.Errorf("registry: parse checkpoint sidecar %s: %w", r.checkpointPath(), err)
	}
	if doc.Version != 1 {
		return nil, fmt.Errorf("registry: unsupported checkpoint sidecar version %d in %s",
			doc.Version, r.checkpointPath())
	}
	return doc.Checkpoints, nil
}

// lockFile takes the exclusive cross-process lock guarding the registry
// and returns the release func. The lock lives on a SIDE file
// (<path>.lock), never on the registry itself: saveLocked replaces the
// registry by rename, so a lock held on that inode would guard a file
// that no longer exists at the path.
func (r *Registry) lockFile() (func(), error) {
	if err := os.MkdirAll(filepath.Dir(r.path), 0o755); err != nil {
		return nil, fmt.Errorf("registry: mkdir %s: %w", filepath.Dir(r.path), err)
	}
	f, err := os.OpenFile(r.path+".lock", os.O_CREATE|os.O_RDWR, 0o600)
	if err != nil {
		return nil, fmt.Errorf("registry: open lock file: %w", err)
	}
	if err := syscall.Flock(int(f.Fd()), syscall.LOCK_EX); err != nil {
		if cerr := f.Close(); cerr != nil {
			r.logf("registry: close lock file after failed flock: %v", cerr)
		}
		return nil, fmt.Errorf("registry: lock %s: %w", r.path+".lock", err)
	}
	return func() {
		if err := syscall.Flock(int(f.Fd()), syscall.LOCK_UN); err != nil {
			r.logf("registry: unlock %s: %v", r.path+".lock", err)
		}
		if err := f.Close(); err != nil {
			r.logf("registry: close lock file: %v", err)
		}
	}, nil
}

type registryState struct {
	records     map[string]Record
	checkpoints map[ConversationIdentity]ConversationCheckpoint
}

type maintenanceStats struct {
	checkpointsCreated int
	recordsHydrated    int
	terminalPruned     int
}

// Prepare migrates/repairs the checkpoint index, hydrates retained records,
// compacts terminal SessionViews, and writes file version 2 atomically.
// Production calls this before serving; any load, migration, compaction, or
// save error is returned so startup fails loudly.
func (r *Registry) Prepare() error {
	stats, err := r.mutate(func(*registryState) error { return nil })
	if err != nil {
		r.logf("registry: prepare FAILED for %s: %v", r.path, err)
		return err
	}
	r.logf("registry: prepared version=%d checkpoints_created=%d records_hydrated=%d terminal_pruned=%d live=%d terminal_retained=%d",
		fileVersionCheckpointIndex, stats.checkpointsCreated, stats.recordsHydrated,
		stats.terminalPruned, r.liveCount(), r.terminalCount())
	return nil
}

// mutate performs one locked read-modify-maintain-write transaction against
// the file. It intentionally does NOT overlay this process's entire cache:
// doing that would let a draining daemon resurrect tombstones a replacement
// daemon already compacted. Only FN's explicit mutation is authoritative.
func (r *Registry) mutate(fn func(*registryState) error) (maintenanceStats, error) {
	r.mu.Lock()
	defer r.mu.Unlock()
	var zero maintenanceStats
	if r.loadErr != nil {
		return zero, r.loadErr
	}
	unlock, err := r.lockFile()
	if err != nil {
		return zero, err
	}
	defer unlock()

	records, checkpoints, loadErr := r.loadStateLocked()
	if loadErr != nil {
		return zero, loadErr
	}
	state := &registryState{records: records, checkpoints: checkpoints}
	if err := fn(state); err != nil {
		return zero, err
	}
	stats, err := r.maintain(state)
	if err != nil {
		return zero, err
	}
	oldRecords, oldCheckpoints := r.records, r.checkpoints
	r.records, r.checkpoints = state.records, state.checkpoints
	if err := r.saveLocked(); err != nil {
		r.records, r.checkpoints = oldRecords, oldCheckpoints
		return zero, err
	}
	if stats.terminalPruned > 0 {
		r.logf("registry: compacted terminal records pruned=%d retained=%d limit=%d checkpoints=%d",
			stats.terminalPruned, r.terminalCountLocked(), TerminalRetention, len(r.checkpoints))
	}
	return stats, nil
}

// Put upserts rec and writes through to disk.
func (r *Registry) Put(rec Record) error {
	if rec.SessionID == "" {
		return fmt.Errorf("registry: Put with empty session_id")
	}
	_, err := r.mutate(func(state *registryState) error {
		if rec.Terminal && rec.TerminalAt == "" {
			rec.TerminalAt = r.now().UTC().Format(time.RFC3339Nano)
		}
		state.records[rec.SessionID] = rec
		return nil
	})
	return err
}

// Update mutates id's record in place and writes through. Reports
// whether the record existed; an absent id still performs the
// lock-and-merge cycle but changes nothing.
func (r *Registry) Update(id string, fn func(*Record)) (bool, error) {
	found := false
	_, err := r.mutate(func(state *registryState) error {
		rec, ok := state.records[id]
		if !ok {
			return nil
		}
		found = true
		wasTerminal := rec.Terminal
		fn(&rec)
		if rec.Terminal && !wasTerminal && rec.TerminalAt == "" {
			rec.TerminalAt = r.now().UTC().Format(time.RFC3339Nano)
		}
		state.records[id] = rec
		return nil
	})
	return found, err
}

// Delete removes id's record and writes through. Deleting an absent id
// is a no-op (prune paths race benignly with each other).
func (r *Registry) Delete(id string) error {
	_, err := r.mutate(func(state *registryState) error {
		delete(state.records, id)
		return nil
	})
	return err
}

func conversationIdentity(rec Record) (ConversationIdentity, bool) {
	if rec.CWD == "" || rec.ClaudeSessionID == "" {
		return ConversationIdentity{}, false
	}
	return ConversationIdentity{
		ConfigDir: rec.ConfigDir, CWD: rec.CWD, ClaudeSessionID: rec.ClaudeSessionID,
	}, true
}

func validBackfill(state string) bool {
	switch state {
	case "", "pending", "done", "failed":
		return true
	default:
		return false
	}
}

func backfillRank(state string) int {
	switch state {
	case "pending":
		return 1
	case "done":
		return 2
	case "failed":
		return 3
	default:
		return 0
	}
}

// strongerBackfill preserves the never-downgrade contract: FAILED is terminal,
// DONE cannot return to PENDING, and PENDING beats no evidence.
func strongerBackfill(a, b string) string {
	if backfillRank(b) > backfillRank(a) {
		return b
	}
	return a
}

func (r *Registry) maintain(state *registryState) (maintenanceStats, error) {
	// Do not log individual checkpoint advances here: LastSeq updates on every
	// consumed event (well above once/second). Prepare and actual pruning are
	// logged with aggregate counts; every failure is returned to a loud caller.
	var stats maintenanceStats
	for id, cp := range state.checkpoints {
		if id.CWD == "" || id.ClaudeSessionID == "" {
			return stats, fmt.Errorf("registry: checkpoint identity incomplete: %+v", id)
		}
		if !validBackfill(cp.BackfillState) {
			return stats, fmt.Errorf("registry: checkpoint %+v has invalid backfill_state %q", id, cp.BackfillState)
		}
	}

	for sessionID, rec := range state.records {
		if !validBackfill(rec.BackfillState) {
			return stats, fmt.Errorf("registry: session %s has invalid backfill_state %q", sessionID, rec.BackfillState)
		}
		if rec.Terminal && rec.TerminalAt == "" {
			if _, err := time.Parse(time.RFC3339, rec.CreatedAt); err == nil {
				rec.TerminalAt = rec.CreatedAt
			} else {
				rec.TerminalAt = time.Time{}.UTC().Format(time.RFC3339)
			}
			state.records[sessionID] = rec
		}
		if rec.Terminal {
			if _, err := time.Parse(time.RFC3339, rec.TerminalAt); err != nil {
				return stats, fmt.Errorf("registry: terminal session %s has invalid terminal_at %q: %w",
					sessionID, rec.TerminalAt, err)
			}
		}
		id, ok := conversationIdentity(rec)
		if !ok {
			continue
		}
		cp, exists := state.checkpoints[id]
		if !exists {
			cp = ConversationCheckpoint{ConversationIdentity: id}
			stats.checkpointsCreated++
		}
		cp.LastSeq = max(cp.LastSeq, rec.LastSeq)
		cp.BackfillState = strongerBackfill(cp.BackfillState, rec.BackfillState)
		state.checkpoints[id] = cp
	}

	for sessionID, rec := range state.records {
		id, ok := conversationIdentity(rec)
		if !ok {
			continue
		}
		cp := state.checkpoints[id]
		changed := false
		if rec.LastSeq < cp.LastSeq {
			rec.LastSeq = cp.LastSeq
			changed = true
		}
		if stronger := strongerBackfill(rec.BackfillState, cp.BackfillState); stronger != rec.BackfillState {
			rec.BackfillState = stronger
			changed = true
		}
		if changed {
			state.records[sessionID] = rec
			stats.recordsHydrated++
		}
	}

	var terminal []Record
	for _, rec := range state.records {
		if rec.Terminal {
			terminal = append(terminal, rec)
		}
	}
	sort.Slice(terminal, func(i, j int) bool {
		if terminal[i].TerminalAt != terminal[j].TerminalAt {
			ti, _ := time.Parse(time.RFC3339, terminal[i].TerminalAt)
			tj, _ := time.Parse(time.RFC3339, terminal[j].TerminalAt)
			if !ti.Equal(tj) {
				return ti.After(tj)
			}
		}
		if terminal[i].CreatedAt != terminal[j].CreatedAt {
			return terminal[i].CreatedAt > terminal[j].CreatedAt
		}
		return terminal[i].SessionID > terminal[j].SessionID
	})
	for _, rec := range terminal[min(TerminalRetention, len(terminal)):] {
		if len(rec.QueuedPrompts) > 0 {
			return stats, fmt.Errorf(
				"registry: refusing to prune terminal session %s with %d undelivered queued prompt(s)",
				rec.SessionID, len(rec.QueuedPrompts))
		}
		delete(state.records, rec.SessionID)
		stats.terminalPruned++
	}
	return stats, nil
}

// CheckpointForSession returns sessionID's conversation checkpoint. Sessions
// without a complete durable identity have no conversation checkpoint.
func (r *Registry) CheckpointForSession(sessionID string) (ConversationCheckpoint, bool) {
	r.mu.Lock()
	defer r.mu.Unlock()
	rec, ok := r.records[sessionID]
	if !ok {
		return ConversationCheckpoint{}, false
	}
	id, ok := conversationIdentity(rec)
	if !ok {
		return ConversationCheckpoint{}, false
	}
	cp, ok := r.checkpoints[id]
	return cp, ok
}

// Checkpoint returns the checkpoint for the exact conversation identity.
func (r *Registry) Checkpoint(id ConversationIdentity) (ConversationCheckpoint, bool) {
	r.mu.Lock()
	defer r.mu.Unlock()
	cp, ok := r.checkpoints[id]
	return cp, ok
}

// Get returns id's record and whether it exists.
func (r *Registry) Get(id string) (Record, bool) {
	r.mu.Lock()
	defer r.mu.Unlock()
	rec, ok := r.records[id]
	return rec, ok
}

// All returns every record, sorted by session id for deterministic
// iteration.
func (r *Registry) All() []Record {
	r.mu.Lock()
	defer r.mu.Unlock()
	out := make([]Record, 0, len(r.records))
	for _, rec := range r.records {
		out = append(out, rec)
	}
	sort.Slice(out, func(i, j int) bool { return out[i].SessionID < out[j].SessionID })
	return out
}

func (r *Registry) liveCount() int {
	r.mu.Lock()
	defer r.mu.Unlock()
	return r.liveCountLocked()
}

func (r *Registry) liveCountLocked() int {
	n := 0
	for _, rec := range r.records {
		if !rec.Terminal {
			n++
		}
	}
	return n
}

func (r *Registry) terminalCount() int {
	r.mu.Lock()
	defer r.mu.Unlock()
	return r.terminalCountLocked()
}

func (r *Registry) terminalCountLocked() int {
	return len(r.records) - r.liveCountLocked()
}

// Flush validates, maintains, and atomically rewrites the CURRENT on-disk
// state. It deliberately does not re-assert this process's whole cache: a
// draining daemon must not resurrect records another daemon compacted.
// Every actual mutation is already write-through.
func (r *Registry) Flush() error {
	_, err := r.mutate(func(*registryState) error { return nil })
	return err
}

// saveLocked writes the checkpoint sidecar FIRST, then the inspectable v2
// registry document. If an old draining daemon later writes a v1 registry it
// cannot touch the sidecar; the next v2 Prepare repairs from it before pruning.
// Each file is atomic (temp + fsync + rename). A crash between them can leave a
// checkpoint ahead of a record, which is safe: checkpoints are monotonic.
func (r *Registry) saveLocked() error {
	checkpoints := make([]ConversationCheckpoint, 0, len(r.checkpoints))
	for _, cp := range r.checkpoints {
		checkpoints = append(checkpoints, cp)
	}
	sort.Slice(checkpoints, func(i, j int) bool {
		a, b := checkpoints[i].ConversationIdentity, checkpoints[j].ConversationIdentity
		if a.ConfigDir != b.ConfigDir {
			return a.ConfigDir < b.ConfigDir
		}
		if a.CWD != b.CWD {
			return a.CWD < b.CWD
		}
		return a.ClaudeSessionID < b.ClaudeSessionID
	})
	checkpointData, err := json.MarshalIndent(checkpointFileShape{
		Version: 1, Checkpoints: checkpoints,
	}, "", "  ")
	if err != nil {
		return fmt.Errorf("registry: marshal checkpoint sidecar: %w", err)
	}
	if err := r.writeAtomicLocked(r.checkpointPath(), checkpointData); err != nil {
		return err
	}

	doc := fileShape{
		Version:     fileVersionCheckpointIndex,
		Sessions:    make([]Record, 0, len(r.records)),
		Checkpoints: checkpoints,
	}
	for _, rec := range r.records {
		doc.Sessions = append(doc.Sessions, rec)
	}
	sort.Slice(doc.Sessions, func(i, j int) bool {
		return doc.Sessions[i].SessionID < doc.Sessions[j].SessionID
	})
	data, err := json.MarshalIndent(doc, "", "  ")
	if err != nil {
		return fmt.Errorf("registry: marshal: %w", err)
	}
	return r.writeAtomicLocked(r.path, data)
}

func (r *Registry) writeAtomicLocked(path string, data []byte) error {
	dir := filepath.Dir(path)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return fmt.Errorf("registry: mkdir %s: %w", dir, err)
	}
	tmp, err := os.CreateTemp(dir, filepath.Base(path)+".tmp-*")
	if err != nil {
		return fmt.Errorf("registry: temp file in %s: %w", dir, err)
	}
	if _, err := tmp.Write(data); err != nil {
		r.discardTemp(tmp)
		return fmt.Errorf("registry: write %s: %w", tmp.Name(), err)
	}
	if err := tmp.Sync(); err != nil {
		r.discardTemp(tmp)
		return fmt.Errorf("registry: fsync %s: %w", tmp.Name(), err)
	}
	if err := tmp.Close(); err != nil {
		r.removeTemp(tmp.Name())
		return fmt.Errorf("registry: close %s: %w", tmp.Name(), err)
	}
	if err := os.Rename(tmp.Name(), path); err != nil {
		r.removeTemp(tmp.Name())
		return fmt.Errorf("registry: rename %s -> %s: %w", tmp.Name(), path, err)
	}
	return nil
}

// discardTemp closes and removes a temp file after a failed write; the
// close error is logged (the write error that got us here is the one
// the caller surfaces).
func (r *Registry) discardTemp(tmp *os.File) {
	if err := tmp.Close(); err != nil {
		r.logf("registry: close temp %s: %v", tmp.Name(), err)
	}
	r.removeTemp(tmp.Name())
}

func (r *Registry) removeTemp(name string) {
	if err := os.Remove(name); err != nil {
		r.logf("registry: remove temp %s: %v", name, err)
	}
}
