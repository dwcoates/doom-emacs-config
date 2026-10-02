// Package history keeps this HOST's measured cost of every unit and item the
// test runner has run, so the next run can plan from what things actually
// take on this machine.
//
// It is a cache, never a record: it lives in ~/.cache/agent-repl beside the
// module's other host caches, not in the repository, because every number in it is a property of the machine
// that measured it. Every worktree on the host shares it, so a test measured
// by one workspace's run is planned well by every other's.
//
// Concurrent runs are safe by construction. Every read-modify-write holds an
// exclusive flock on a sibling lock file (the kernel drops it with its holder,
// so a dead run never wedges the next), and the new contents replace the old
// by rename, so a reader sees one complete file or the other and never half
// of one.
package history

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"syscall"
)

// Weight is how much of a new measurement replaces the old estimate. Half
// follows a real change within a couple of runs while one noisy run moves the
// estimate only halfway.
const Weight = 0.5

// Entry is one key's running estimate.
type Entry struct {
	Seconds float64 `json:"seconds"`
	Samples int     `json:"samples"`
}

type file struct {
	Version int              `json:"version"`
	Entries map[string]Entry `json:"entries"`
}

const version = 1

// Store is a loaded history.
type Store struct {
	path    string
	entries map[string]Entry
}

// DefaultPath is where the host's history lives: $AGENT_REPL_TEST_HISTORY
// when set, else ~/.cache/agent-repl/test-history.json -- the module's one
// host cache directory on every platform (the daemon's binaries, locks and
// sockets live there too), never the platform's own cache directory, which on
// macOS is ~/Library/Caches.
func DefaultPath() (string, error) {
	if p := os.Getenv("AGENT_REPL_TEST_HISTORY"); p != "" {
		return p, nil
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return "", fmt.Errorf("history: resolve the home directory: %w", err)
	}
	return filepath.Join(home, ".cache", "agent-repl", "test-history.json"), nil
}

// Load reads the history at path. A missing file is an empty history: the
// first run on a host has measured nothing yet.
func Load(path string) (*Store, error) {
	unlock, err := lock(path)
	if err != nil {
		return nil, err
	}
	defer unlock()
	entries, err := read(path)
	if err != nil {
		return nil, err
	}
	return &Store{path: path, entries: entries}, nil
}

// Get is a key's estimate, and whether there is one.
func (s *Store) Get(key string) (float64, bool) {
	e, ok := s.entries[key]
	return e.Seconds, ok
}

// Record folds measurements into the history on disk, re-reading it under the
// lock first so a concurrent run's measurements are kept, never overwritten.
func (s *Store) Record(measured map[string]float64) error {
	for key, secs := range measured {
		if secs < 0 {
			return fmt.Errorf("history: refusing a negative measurement %v for %q", secs, key)
		}
	}
	if err := os.MkdirAll(filepath.Dir(s.path), 0o755); err != nil {
		return fmt.Errorf("history: create %s: %w", filepath.Dir(s.path), err)
	}
	unlock, err := lock(s.path)
	if err != nil {
		return err
	}
	defer unlock()
	entries, err := read(s.path)
	if err != nil {
		return err
	}
	for key, secs := range measured {
		e, ok := entries[key]
		if !ok {
			entries[key] = Entry{Seconds: secs, Samples: 1}
			continue
		}
		entries[key] = Entry{Seconds: (1-Weight)*e.Seconds + Weight*secs, Samples: e.Samples + 1}
	}
	data, err := json.MarshalIndent(file{Version: version, Entries: entries}, "", " ")
	if err != nil {
		return fmt.Errorf("history: encode: %w", err)
	}
	tmp, err := os.CreateTemp(filepath.Dir(s.path), ".test-history-*")
	if err != nil {
		return fmt.Errorf("history: create a temp file beside %s: %w", s.path, err)
	}
	defer os.Remove(tmp.Name())
	if _, err := tmp.Write(data); err != nil {
		tmp.Close()
		return fmt.Errorf("history: write %s: %w", tmp.Name(), err)
	}
	if err := tmp.Close(); err != nil {
		return fmt.Errorf("history: close %s: %w", tmp.Name(), err)
	}
	if err := os.Rename(tmp.Name(), s.path); err != nil {
		return fmt.Errorf("history: replace %s: %w", s.path, err)
	}
	s.entries = entries
	return nil
}

func read(path string) (map[string]Entry, error) {
	data, err := os.ReadFile(path)
	if errors.Is(err, fs.ErrNotExist) {
		return map[string]Entry{}, nil
	}
	if err != nil {
		return nil, fmt.Errorf("history: read %s: %w", path, err)
	}
	var f file
	if err := json.Unmarshal(data, &f); err != nil {
		return nil, fmt.Errorf("history: %s is not a test history (delete it to start over): %w", path, err)
	}
	if f.Version != version {
		return nil, fmt.Errorf("history: %s is version %d, this runner reads version %d (delete it to start over)", path, f.Version, version)
	}
	if f.Entries == nil {
		f.Entries = map[string]Entry{}
	}
	return f.Entries, nil
}

// lock takes the exclusive flock beside path, creating the directory and the
// lock file as needed.
func lock(path string) (func(), error) {
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		return nil, fmt.Errorf("history: create %s: %w", filepath.Dir(path), err)
	}
	f, err := os.OpenFile(path+".lock", os.O_CREATE|os.O_RDWR, 0o644)
	if err != nil {
		return nil, fmt.Errorf("history: open the lock %s.lock: %w", path, err)
	}
	if err := syscall.Flock(int(f.Fd()), syscall.LOCK_EX); err != nil {
		f.Close()
		return nil, fmt.Errorf("history: lock %s.lock: %w", path, err)
	}
	return func() {
		syscall.Flock(int(f.Fd()), syscall.LOCK_UN)
		f.Close()
	}, nil
}
