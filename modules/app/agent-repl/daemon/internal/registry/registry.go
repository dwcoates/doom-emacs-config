// Package registry persists the daemon's session records across
// restarts. The in-memory session map in internal/server dies with the
// process; this registry is the durable copy that lets a restarted
// daemon keep resolving the s_<hex> ids its frontends still hold
// (rehydrating each into a live --resume session on first access).
//
// Durability contract: every mutation writes through to disk
// atomically (temp file + rename in the registry's own directory), so
// a SIGKILL or crash at any instant leaves either the previous or the
// next complete state on disk — never a torn file. Nothing depends on
// a shutdown hook to flush.
package registry

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"sync"
	"syscall"
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
	// LastSeq is the highest agent-shim store seq the daemon has durably
	// observed for this session — the shimclient replay high-water mark
	// (design §4.4). Persisting it here (rather than in the shimclient,
	// which persists nothing itself) is what lets a restarted daemon
	// re-Subscribe from where it left off and reattach without re-replaying
	// or losing events. Zero means "never subscribed"; a fresh subscribe
	// from seq 0 then replays the whole session. See server.RegistrySeqStore.
	LastSeq uint64 `json:"last_seq,omitempty"`
}

// fileShape is the on-disk JSON document.
type fileShape struct {
	Version  int      `json:"version"`
	Sessions []Record `json:"sessions"`
}

// Registry is a write-through, crash-safe session record store.
type Registry struct {
	path string
	logf func(string, ...any)

	mu      sync.Mutex
	records map[string]Record
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

// Open loads the registry at path. A missing file starts empty (first
// boot). A file that cannot be read or parsed is logged LOUDLY and the
// registry starts empty — the daemon must never refuse to boot over its
// own bookkeeping — with the unparseable file preserved at
// <path>.corrupt for post-mortem rather than silently overwritten.
func Open(path string, logf func(string, ...any)) *Registry {
	r := &Registry{path: path, logf: logf, records: map[string]Record{}}
	r.records = r.loadRecordsLocked()
	return r
}

// loadRecordsLocked reads the on-disk records. A missing file yields an
// empty set (first boot, silently). A file that cannot be read or parsed
// is logged LOUDLY and yields an empty set — the daemon must never
// refuse to boot over its own bookkeeping — with the unparseable bytes
// preserved at <path>.corrupt rather than silently overwritten.
//
// Callers hold r.mu (Open is pre-publication, mutate holds it).
func (r *Registry) loadRecordsLocked() map[string]Record {
	records := map[string]Record{}
	data, err := os.ReadFile(r.path)
	if err != nil {
		if !os.IsNotExist(err) {
			r.logf("registry: READ FAILED for %s — starting empty, existing sessions will NOT rehydrate: %v", r.path, err)
		}
		return records
	}
	var doc fileShape
	if err := json.Unmarshal(data, &doc); err != nil {
		r.logf("registry: CORRUPT file at %s — starting empty, existing sessions will NOT rehydrate: %v", r.path, err)
		corrupt := r.path + ".corrupt"
		if mvErr := os.Rename(r.path, corrupt); mvErr != nil {
			r.logf("registry: could not preserve corrupt file at %s: %v", corrupt, mvErr)
		} else {
			r.logf("registry: corrupt file preserved at %s", corrupt)
		}
		return records
	}
	for _, rec := range doc.Sessions {
		if rec.SessionID == "" {
			r.logf("registry: DROPPING record with empty session_id in %s (external edit?)", r.path)
			continue
		}
		records[rec.SessionID] = rec
	}
	return records
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

// mutate performs one read-modify-write cycle against the file under an
// exclusive cross-process lock: re-read what is CURRENTLY on disk, apply
// fn to it, write it back atomically, and adopt the result as this
// process's cache.
//
// The re-read is what makes two daemons on one registry path safe. A
// second daemon is not hypothetical (an agent rebuilds and bounces the
// binary while the old process is still draining, and master's adopt
// path can leave two alive briefly), and without it each process would
// rewrite the whole file from its OWN map, silently dropping every
// record the other had added since it loaded — the lost-update that
// would strand exactly the sessions this registry exists to save.
func (r *Registry) mutate(fn func(map[string]Record)) error {
	r.mu.Lock()
	defer r.mu.Unlock()
	unlock, err := r.lockFile()
	if err != nil {
		return err
	}
	defer unlock()
	// Merge our cache over what is on disk: another daemon's records are
	// preserved, and ours win for ids we both know (we are authoritative
	// for the sessions we serve).
	onDisk := r.loadRecordsLocked()
	for id, rec := range r.records {
		onDisk[id] = rec
	}
	fn(onDisk)
	r.records = onDisk
	return r.saveLocked()
}

// Put upserts rec and writes through to disk.
func (r *Registry) Put(rec Record) error {
	if rec.SessionID == "" {
		return fmt.Errorf("registry: Put with empty session_id")
	}
	return r.mutate(func(recs map[string]Record) { recs[rec.SessionID] = rec })
}

// Update mutates id's record in place and writes through. Reports
// whether the record existed; an absent id still performs the
// lock-and-merge cycle but changes nothing.
func (r *Registry) Update(id string, fn func(*Record)) (bool, error) {
	found := false
	err := r.mutate(func(recs map[string]Record) {
		rec, ok := recs[id]
		if !ok {
			return
		}
		found = true
		fn(&rec)
		recs[id] = rec
	})
	return found, err
}

// Delete removes id's record and writes through. Deleting an absent id
// is a no-op (prune paths race benignly with each other).
func (r *Registry) Delete(id string) error {
	return r.mutate(func(recs map[string]Record) { delete(recs, id) })
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

// Flush re-asserts the in-memory state onto disk, merged (under the
// cross-process lock) with whatever another daemon has written since.
// Every mutation already writes through, so this is a belt-and-suspenders
// step for the graceful-shutdown path, never the durability mechanism.
func (r *Registry) Flush() error {
	return r.mutate(func(map[string]Record) {})
}

// saveLocked writes the registry atomically: marshal, write to a temp
// file in the same directory, fsync, rename over the final path. A
// crash at any point leaves the previous complete file intact; a
// leftover temp file is inert (Open never reads it).
func (r *Registry) saveLocked() error {
	doc := fileShape{Version: 1, Sessions: make([]Record, 0, len(r.records))}
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
	dir := filepath.Dir(r.path)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return fmt.Errorf("registry: mkdir %s: %w", dir, err)
	}
	tmp, err := os.CreateTemp(dir, filepath.Base(r.path)+".tmp-*")
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
	if err := os.Rename(tmp.Name(), r.path); err != nil {
		r.removeTemp(tmp.Name())
		return fmt.Errorf("registry: rename %s -> %s: %w", tmp.Name(), r.path, err)
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
