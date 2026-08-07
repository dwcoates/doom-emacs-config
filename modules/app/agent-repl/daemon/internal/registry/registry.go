// Package registry persists the daemon's session records across
// restarts. The in-memory session map in internal/server dies with the
// process; this registry is the durable copy that lets a restarted
// daemon keep resolving the s_<hex> ids its frontends still hold
// (rehydrating each into a live --resume session on first access).
//
// Durability contract: session IDENTITY lives in tables beside the
// session-state manager's log, in the daemon's ONE SQLite store (see
// internal/statedb). Every mutation is a single transaction that reads the
// current rows, applies the caller's change, runs checkpoint maintenance and
// terminal compaction, and rewrites both tables — so a cursor, a replay floor
// and an identity can never land apart from each other, and a crash mid-write
// leaves the previous state exactly as it was. Nothing depends on a shutdown
// hook to flush.
//
// This replaces a JSON file plus a checkpoint sidecar, whose crash-safety came
// from atomic renames and whose "these three fields move together" property
// came from write-ordering discipline. The one-time import of that file lives
// in legacy.go; after it, the tables are the sole authority.
//
// THE JSON REGISTRY IS RETIRED, NOT BROKEN. $AGENT_REPL_STATE_DIR/
// claude-repld-sessions.json has not been written since that migration and
// never will be again: its mtime is frozen at the import, and a session created
// afterwards is deliberately absent from it. Anything reading it for identity
// is reading pre-migration history. legacy.go plants a `.RETIRED` deprecation
// record beside it saying exactly that, and sweeps the dead writer's orphaned
// `.tmp-<n>` partials and `.lock`, so the freeze cannot be mistaken for an
// atomic-write path that keeps failing.
package registry

import (
	"database/sql"
	"encoding/json"
	"errors"
	"fmt"
	"path/filepath"
	"sort"
	"sync"
	"time"

	"claude-repld/internal/statedb"
	"claude-repld/internal/stateroot"
)

const (
	// schemaVersion is the registry tables' revision, stamped in
	// registry_meta. Open refuses a database written by a NEWER schema than
	// this binary understands (loud, no silent downgrade).
	schemaVersion = 1

	// TerminalRetention is the maximum number of terminal SessionView records
	// retained in the registry. Live records are never pruned. Conversation
	// replay/backfill state survives separately in the checkpoint table.
	TerminalRetention = 128
)

// Record is one session's durable registry entry. The json tags are what the
// legacy import reads; the columns it maps onto are in schema.go.
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
	// DeathResolvedAtMs is when this record's death STOPPED BEING TRUE, unix
	// millis, or zero while it still is. It is the durable half of
	// frontendv1.SystemFailureItem.resolved_at_ms for the death card: the item
	// is re-derived from DeathReason on every SessionView push, so without a
	// persisted instant the card reopens, unresolved, on every boot forever.
	//
	// Only a WINDOW-shaped death has one. A supersede is the window: it says
	// "a newer session took this workspace", and that sentence stops describing
	// anything the moment the successor is genuinely up. A delete does not —
	// the conversation stays deleted — so nothing stamps it.
	DeathResolvedAtMs int64 `json:"death_resolved_at_ms,omitempty"`
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
	// NewestClearOrCompactSeq is the store seq of the newest CLEAR OR COMPACTION
	// observed on this conversation, whichever came last. It is the frontend
	// REPLAY FLOOR: a resync is served from this seq forward (INCLUSIVE of the
	// clear or compaction itself), so a reconnecting frontend never receives
	// history that clear or compaction already made irrelevant, and never has
	// to find the boundary for itself.
	//
	// PERSISTED for the same reason BackfillState is: the evidence does not
	// survive a restart. The daemon re-Subscribes from LastSeq, so a clear or
	// compaction observed before the restart is never re-delivered, and a floor
	// held only in memory would silently revert to replaying the whole
	// conversation that preceded it. Zero means "no clear and no compaction
	// seen on this conversation".
	NewestClearOrCompactSeq uint64 `json:"newest_clear_or_compact_seq,omitempty"`
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
	//
	// Stored as a JSON array in one column: it is an opaque held list, never
	// a thing the registry queries BY, so giving it a table would buy joins
	// nobody performs.
	QueuedPrompts []QueuedPrompt `json:"queued_prompts,omitempty"`
	// LastTurnEndMs is when this session's most recent turn ENDED, unix millis.
	// Zero means no turn has ever ended under this record.
	//
	// PERSISTED for the same reason BackfillState is, and for one more: it is
	// the ONLY input to the cache keep-alive policy, and that policy's whole
	// premise is that a decision is a time-since check against a durable
	// timestamp rather than a timer. A timer dies with the daemon and lies
	// across a laptop sleep; a persisted instant survives both, and the
	// discovery that too much time has passed is itself a decision the policy
	// takes (HibernationCacheExpired) rather than a gap it fails to notice.
	LastTurnEndMs int64 `json:"last_turn_end_ms,omitempty"`
	// Hibernated marks a session whose shim the daemon deliberately stopped and
	// which must NOT be revived implicitly. It is durable so a daemon restart
	// rehydrates the sleep rather than silently un-sleeping it, and so a
	// rehydrated hibernated session is outside the keep-alive loop by
	// construction rather than by a live flag nobody re-derives.
	Hibernated bool `json:"hibernated,omitempty"`
	// Hibernation is the typed account behind Hibernated. Zero value when
	// Hibernated is false; the two move together in one write.
	Hibernation HibernationDetail `json:"hibernation"`
	// Rewind is the UNCONSUMED rewind lineage this session's next spawn must
	// announce, zero when none is owed.
	//
	// DURABLE, AND WRITTEN BY THE SAME Update THAT FLIPS ClaudeSessionID. The
	// flip is the rewind's one destructive act; the lineage is the only account
	// of what that flip dropped. Held in memory instead, a daemon dying between
	// the flip and the respawn left a record naming a truncated conversation
	// with nothing left to say it had been truncated, and the SessionRewound
	// the frontends replay from was never emitted by anyone.
	//
	// ONE-SHOT: the spawner clears it in the Update that records the spawn that
	// consumed it. A lineage left standing would ride the next unrelated
	// respawn and announce a rewind that never happened.
	Rewind RewindLineage `json:"rewind,omitempty"`
}

// RewindLineage is the durable form of the frozen shim argv contract
// (--rewound-from, --rewind-retained-leaf, --rewind-dropped-turns).
//
// ALL THREE OR NONE. The shim rejects an empty dropped-turn list outright, so a
// partial lineage would turn an unrecorded rewind into a spawn that fails at
// startup and a session that comes back with no shim at all. Every writer and
// the loader enforce the pair, so a partial one is not representable durably.
type RewindLineage struct {
	// PreviousVendorSessionID is the transcript the rewind truncated — the seq
	// space it retired.
	PreviousVendorSessionID string `json:"previous_vendor_session_id,omitempty"`
	// RetainedLeafUUID is the last record kept: the final record of the last
	// real turn.
	RetainedLeafUUID string `json:"retained_leaf_uuid,omitempty"`
	// DroppedTurnIDs is the comma-separated turn_id list, in submission order.
	DroppedTurnIDs string `json:"dropped_turn_ids,omitempty"`
}

// Armed reports whether a complete lineage is waiting to be announced.
func (l RewindLineage) Armed() bool {
	return l.PreviousVendorSessionID != "" && l.RetainedLeafUUID != "" && l.DroppedTurnIDs != ""
}

// Partial reports a lineage that carries some of the three and not all of
// them — the one shape that must never be stored or spawned with.
func (l RewindLineage) Partial() bool {
	set := 0
	for _, f := range []string{l.PreviousVendorSessionID, l.RetainedLeafUUID, l.DroppedTurnIDs} {
		if f != "" {
			set++
		}
	}
	return set != 0 && set != 3
}

// Hibernation cause tokens. They are the durable spelling of
// frontendv1.HibernationDetail's cause arms; the registry stores a string
// rather than the enum-less oneof so a record stays readable by a binary that
// predates a future arm.
const (
	HibernationCauseIdleCutoff   = "idle_cutoff"
	HibernationCauseForced       = "forced"
	HibernationCauseCacheExpired = "cache_expired"
)

// HibernationDetail is the durable evidence of WHY and WHEN a session
// hibernated. Every field is carried rather than re-derived: the cutoff and TTL
// that tripped are daemon CONFIG at the moment of the transition, and a record
// that stored only the cause would report the current config's numbers for a
// sleep taken under different ones.
type HibernationDetail struct {
	// Cause is one of the HibernationCause* tokens. Empty exactly when the
	// session is not hibernated.
	Cause string `json:"cause,omitempty"`
	// SinceMs is when the session entered hibernation, unix millis.
	SinceMs int64 `json:"since_ms,omitempty"`
	// CutoffMs is the idle cutoff that tripped (idle_cutoff only).
	CutoffMs int64 `json:"cutoff_ms,omitempty"`
	// ElapsedMs is how long the session had actually been idle when the check
	// ran (cache_expired only).
	ElapsedMs int64 `json:"elapsed_ms,omitempty"`
	// TTLMs is the expected cache TTL the elapsed time exceeded
	// (cache_expired only).
	TTLMs int64 `json:"ttl_ms,omitempty"`
}

// ValidHibernationCause reports whether cause is a token this binary
// understands. The empty string is valid and means "not hibernated".
func ValidHibernationCause(cause string) bool {
	switch cause {
	case "", HibernationCauseIdleCutoff, HibernationCauseForced, HibernationCauseCacheExpired:
		return true
	default:
		return false
	}
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
	LastSeq uint64 `json:"last_seq,omitempty"`
	// NewestClearOrCompactSeq is the conversation's replay floor (see
	// Record.NewestClearOrCompactSeq). It belongs on the CHECKPOINT, not just
	// the record: every restart mints a fresh s_ session id for the same
	// conversation, and a floor filed only under the retired id would be lost
	// exactly when it is needed.
	NewestClearOrCompactSeq uint64 `json:"newest_clear_or_compact_seq,omitempty"`
	BackfillState           string `json:"backfill_state,omitempty"`
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

// Options configure a Registry.
type Options struct {
	// DB is an already-open state store, shared with its other owner (the
	// SSM). Preferred in production: sharing the handle is what makes a
	// registry write and a state-log write serialize instead of compete.
	// When set, DBPath is ignored and Close leaves the handle alone.
	DB *sql.DB
	// DBPath opens a store this Registry owns. Used when DB is nil.
	DBPath string
	// LegacyJSONPath is the pre-SQLite registry file to import ONCE, on the
	// first open of an empty database. Empty disables the import.
	LegacyJSONPath string
	// StorePath is the state store's path, recorded in the retired JSON
	// registry's deprecation record as the successor authority. It cannot be
	// derived when DB carries a handle whose path this package never saw, and
	// naming the wrong store in a signpost is worse than planting none.
	// Defaults to DBPath.
	StorePath string
	// Logf is the loud failure/anomaly logger. Nil discards.
	Logf func(string, ...any)
}

// Registry is a write-through, crash-safe session record store over the
// daemon's SQLite state store.
type Registry struct {
	db     *sql.DB
	ownsDB bool
	logf   func(string, ...any)

	// writeMu serializes this process's transactions. It is deliberately NOT
	// r.mu: r.mu guards the read cache and is taken by readers (including the
	// SSM's resolver, mid-event), so holding it while waiting for the store's
	// single connection would let a registry write and a state-log write
	// deadlock on each other.
	writeMu sync.Mutex

	mu          sync.Mutex
	records     map[string]Record
	checkpoints map[ConversationIdentity]ConversationCheckpoint
	now         func() time.Time
	loadErr     error
}

// DefaultDBPath returns the daemon's state store — the SAME database the SSM
// writes its log to, which is the whole point: identity and state move in one
// transaction only if they live in one store.
func DefaultDBPath() (string, error) {
	path, err := statedb.DefaultPath()
	if err != nil {
		return "", fmt.Errorf("registry: %w", err)
	}
	return path, nil
}

// LegacyJSONPath returns $AGENT_REPL_STATE_DIR/claude-repld-sessions.json,
// the pre-SQLite registry file. It is imported once and then left on disk as
// inert history; nothing reads it afterwards.
func LegacyJSONPath() (string, error) {
	root, err := stateroot.Root()
	if err != nil {
		return "", fmt.Errorf("registry: %w", err)
	}
	return filepath.Join(root, "claude-repld-sessions.json"), nil
}

// Open opens a registry over the state store at path, which this Registry
// owns. Convenience over OpenWith for tests and one-off tools; production
// shares the SSM's handle (see OpenWith and Options.DB).
func Open(path string, logf func(string, ...any)) *Registry {
	return OpenWith(Options{DBPath: path, Logf: logf})
}

// OpenWith opens a registry per opts. A store that cannot be opened, migrated,
// imported or read records a sticky load error: Prepare returns it so
// production startup fails loudly, and every mutation refuses rather than
// serving from fabricated empty state.
func OpenWith(opts Options) *Registry {
	logf := opts.Logf
	if logf == nil {
		logf = func(string, ...any) {}
	}
	r := &Registry{
		db: opts.DB, logf: logf,
		records:     map[string]Record{},
		checkpoints: map[ConversationIdentity]ConversationCheckpoint{},
		now:         time.Now,
	}
	if r.db == nil {
		db, err := statedb.Open(opts.DBPath)
		if err != nil {
			r.logf("registry: CORRUPT or unopenable state store at %s — refusing to serve: %v", opts.DBPath, err)
			r.loadErr = fmt.Errorf("registry: open state store: %w", err)
			return r
		}
		r.db, r.ownsDB = db, true
	}
	if err := migrate(r.db); err != nil {
		r.logf("registry: schema migration FAILED — refusing to serve: %v", err)
		r.loadErr = err
		return r
	}
	if err := r.importLegacyJSON(opts.LegacyJSONPath); err != nil {
		r.loadErr = err
		return r
	}
	// The import decision is settled, so the file's status is now certain:
	// retired, unread, unwritten. Say so on disk (see retireLegacyJSON) —
	// otherwise the frozen file and the dead writer's orphaned temp files keep
	// answering identity questions with a snapshot from the migration.
	storePath := opts.StorePath
	if storePath == "" {
		storePath = opts.DBPath
	}
	r.retireLegacyJSON(opts.LegacyJSONPath, storePath)
	records, checkpoints, err := loadState(r.db, r.logf)
	if err != nil {
		r.loadErr = err
		return r
	}
	r.records, r.checkpoints = records, checkpoints
	return r
}

// Close releases a store this Registry opened. A shared handle (Options.DB)
// belongs to its owner and is left open.
func (r *Registry) Close() error {
	if !r.ownsDB {
		return nil
	}
	return r.db.Close()
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

// Prepare repairs the checkpoint index, hydrates retained records, and
// compacts terminal SessionViews in one transaction. Production calls this
// before serving; any load, maintenance or write error is returned so startup
// fails loudly.
func (r *Registry) Prepare() error {
	stats, err := r.mutate(func(*registryState) error { return nil })
	if err != nil {
		r.logf("registry: prepare FAILED: %v", err)
		return err
	}
	r.logf("registry: prepared schema=%d checkpoints_created=%d records_hydrated=%d terminal_pruned=%d live=%d terminal_retained=%d",
		schemaVersion, stats.checkpointsCreated, stats.recordsHydrated,
		stats.terminalPruned, r.liveCount(), r.terminalCount())
	return nil
}

// mutate performs one read-modify-maintain-write TRANSACTION against the
// tables. It intentionally does NOT overlay this process's entire cache:
// doing that would let a draining daemon resurrect tombstones a replacement
// daemon already compacted. Only fn's explicit mutation is authoritative.
//
// The transaction is what makes a rotation's "adopt the new uuid AND reset the
// cursors" one indivisible write: either every row of the new state is visible
// or none of it is, with no window in which a reader sees the new identity
// carrying the retired seq space's cursors.
func (r *Registry) mutate(fn func(*registryState) error) (maintenanceStats, error) {
	var zero maintenanceStats
	if err := r.sticky(); err != nil {
		return zero, err
	}
	r.writeMu.Lock()
	defer r.writeMu.Unlock()

	// _txlock=immediate (internal/statedb): the write lock is taken here, so a
	// second daemon over the same file waits rather than losing an update.
	tx, err := r.db.Begin()
	if err != nil {
		return zero, fmt.Errorf("registry: begin transaction: %w", err)
	}
	committed := false
	defer func() {
		if committed {
			return
		}
		if err := tx.Rollback(); err != nil && !errors.Is(err, sql.ErrTxDone) {
			r.logf("registry: rollback FAILED: %v", err)
		}
	}()

	records, checkpoints, err := loadState(tx, r.logf)
	if err != nil {
		return zero, err
	}
	state := &registryState{records: records, checkpoints: checkpoints}
	if err := fn(state); err != nil {
		return zero, err
	}
	stats, err := r.maintain(state)
	if err != nil {
		return zero, err
	}
	if err := saveState(tx, state); err != nil {
		return zero, err
	}
	if err := tx.Commit(); err != nil {
		return zero, fmt.Errorf("registry: commit transaction: %w", err)
	}
	committed = true

	r.mu.Lock()
	r.records, r.checkpoints = state.records, state.checkpoints
	terminalRetained := r.terminalCountLocked()
	checkpointCount := len(r.checkpoints)
	r.mu.Unlock()

	if stats.terminalPruned > 0 {
		r.logf("registry: compacted terminal records pruned=%d retained=%d limit=%d checkpoints=%d",
			stats.terminalPruned, terminalRetained, TerminalRetention, checkpointCount)
	}
	return stats, nil
}

// sticky returns the load error recorded at Open, if any.
func (r *Registry) sticky() error {
	r.mu.Lock()
	defer r.mu.Unlock()
	return r.loadErr
}

// Put upserts rec and writes through to the store.
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
// read-modify-write cycle but changes nothing.
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
		// HIBERNATION AND ITS ACCOUNT ARE ONE FACT, checked where every write
		// passes. `hibernated` is the compatibility projection of the typed
		// detail, so a record carrying one without the other would make the
		// revival gate render a sleep it cannot explain — or, worse, explain a
		// sleep that is not happening. Refusing the write is what keeps the
		// pair from coming apart at all.
		if !ValidHibernationCause(rec.Hibernation.Cause) {
			return stats, fmt.Errorf("registry: session %s has invalid hibernation cause %q", sessionID, rec.Hibernation.Cause)
		}
		if rec.Hibernated != (rec.Hibernation.Cause != "") {
			return stats, fmt.Errorf("registry: session %s has hibernated=%v with cause %q; the flag and its typed account must move together",
				sessionID, rec.Hibernated, rec.Hibernation.Cause)
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
		cp.NewestClearOrCompactSeq = max(cp.NewestClearOrCompactSeq, rec.NewestClearOrCompactSeq)
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
		if rec.NewestClearOrCompactSeq < cp.NewestClearOrCompactSeq {
			rec.NewestClearOrCompactSeq = cp.NewestClearOrCompactSeq
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

// AllCheckpoints returns every conversation checkpoint, sorted by identity for
// deterministic iteration.
//
// A checkpoint OUTLIVES the session record that produced it, which is what
// makes this worth exposing separately from All: a conversation whose record
// has been pruned is still on disk and still resumable, and the checkpoint is
// the only thing that still remembers it exists.
func (r *Registry) AllCheckpoints() []ConversationCheckpoint {
	r.mu.Lock()
	defer r.mu.Unlock()
	out := make([]ConversationCheckpoint, 0, len(r.checkpoints))
	for _, cp := range r.checkpoints {
		out = append(out, cp)
	}
	sort.Slice(out, func(i, j int) bool {
		if out[i].ConfigDir != out[j].ConfigDir {
			return out[i].ConfigDir < out[j].ConfigDir
		}
		if out[i].CWD != out[j].CWD {
			return out[i].CWD < out[j].CWD
		}
		return out[i].ClaudeSessionID < out[j].ClaudeSessionID
	})
	return out
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

// Flush validates, maintains, and rewrites the CURRENT persisted state. It
// deliberately does not re-assert this process's whole cache: a draining
// daemon must not resurrect records another daemon compacted. Every actual
// mutation is already write-through.
func (r *Registry) Flush() error {
	_, err := r.mutate(func(*registryState) error { return nil })
	return err
}

// encodeQueuedPrompts renders a record's held prompts for their column. An
// empty list is stored as an empty string rather than "null", so the common
// case reads as plainly empty.
func encodeQueuedPrompts(prompts []QueuedPrompt) (string, error) {
	if len(prompts) == 0 {
		return "", nil
	}
	data, err := json.Marshal(prompts)
	if err != nil {
		return "", fmt.Errorf("registry: encode queued prompts: %w", err)
	}
	return string(data), nil
}

// decodeQueuedPrompts parses a queued_prompts column. Unparseable held prompts
// are an error, never an empty list: these are things the user typed that the
// agent has not seen, so silently dropping them is the one outcome forbidden.
func decodeQueuedPrompts(sessionID, raw string) ([]QueuedPrompt, error) {
	if raw == "" {
		return nil, nil
	}
	var prompts []QueuedPrompt
	if err := json.Unmarshal([]byte(raw), &prompts); err != nil {
		return nil, fmt.Errorf("registry: session %s has unparseable queued_prompts: %w", sessionID, err)
	}
	return prompts, nil
}
