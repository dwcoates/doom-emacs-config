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
)

// Record is one session's durable registry entry.
type Record struct {
	// SessionID is the daemon-minted s_<hex> id — the key frontends
	// hold, and the id under which the session rehydrates.
	SessionID      string `json:"session_id"`
	CWD            string `json:"cwd,omitempty"`
	Model          string `json:"model,omitempty"`
	PermissionMode string `json:"permission_mode,omitempty"`
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
	data, err := os.ReadFile(path)
	if err != nil {
		if !os.IsNotExist(err) {
			logf("registry: READ FAILED for %s — starting empty, existing sessions will NOT rehydrate: %v", path, err)
		}
		return r
	}
	var doc fileShape
	if err := json.Unmarshal(data, &doc); err != nil {
		logf("registry: CORRUPT file at %s — starting empty, existing sessions will NOT rehydrate: %v", path, err)
		corrupt := path + ".corrupt"
		if mvErr := os.Rename(path, corrupt); mvErr != nil {
			logf("registry: could not preserve corrupt file at %s: %v", corrupt, mvErr)
		} else {
			logf("registry: corrupt file preserved at %s", corrupt)
		}
		return r
	}
	for _, rec := range doc.Sessions {
		if rec.SessionID == "" {
			logf("registry: DROPPING record with empty session_id in %s (external edit?)", path)
			continue
		}
		r.records[rec.SessionID] = rec
	}
	return r
}

// Put upserts rec and writes through to disk.
func (r *Registry) Put(rec Record) error {
	if rec.SessionID == "" {
		return fmt.Errorf("registry: Put with empty session_id")
	}
	r.mu.Lock()
	defer r.mu.Unlock()
	r.records[rec.SessionID] = rec
	return r.saveLocked()
}

// Update mutates id's record in place and writes through. Reports
// whether the record existed; an absent id performs no write.
func (r *Registry) Update(id string, fn func(*Record)) (bool, error) {
	r.mu.Lock()
	defer r.mu.Unlock()
	rec, ok := r.records[id]
	if !ok {
		return false, nil
	}
	fn(&rec)
	r.records[id] = rec
	return true, r.saveLocked()
}

// Delete removes id's record and writes through. Deleting an absent id
// is a no-op (prune paths race benignly with each other).
func (r *Registry) Delete(id string) error {
	r.mu.Lock()
	defer r.mu.Unlock()
	if _, ok := r.records[id]; !ok {
		return nil
	}
	delete(r.records, id)
	return r.saveLocked()
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

// Flush rewrites the on-disk file from the in-memory state. Every
// mutation already writes through, so this is a belt-and-suspenders
// step for the graceful-shutdown path, never the durability mechanism.
func (r *Registry) Flush() error {
	r.mu.Lock()
	defer r.mu.Unlock()
	return r.saveLocked()
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
