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
	"strings"
	"syscall"

	"agentrepl/testrun/internal/atomicfile"
)

// Weight is how much of a new measurement replaces the old estimate. Half
// follows a real change within a couple of runs while one noisy run moves the
// estimate only halfway.
const Weight = 0.5

// PeakDecay is how much of a key's peak survives each new measurement: the
// peak is the larger of the new measurement and the old peak times PeakDecay.
// A peak follows a unit that got slower at once, and forgets a one-off slow run
// slowly: one decays by half over about 34 runs, so a cold build every few
// dozen runs stays within it. Measured over 4,659 recorded runs (2026-11-09):
// 0.95 let a cold build pass three times its peak four times, 0.98 twice, and
// slower decay no less.
const PeakDecay = 0.98

// Entry is one key's running estimate.
type Entry struct {
	Seconds float64 `json:"seconds"`
	Samples int     `json:"samples"`
	// Peak is the key's decaying peak (PeakDecay): what a healthy run of it
	// is expected to take at most, which its time limit is sized from. Nil in
	// an entry recorded before peaks were kept.
	Peak *float64 `json:"peak,omitempty"`
}

// Sample is one run's measurement of a key: its typical time, which the
// estimate follows, and its longest, which the peak follows. They differ only
// for a key measured several times in one run (a group's per-chunk overhead).
type Sample struct {
	Seconds float64
	Peak    float64
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

// DefaultPath is where the host's history of runs under conditions lives:
// $AGENT_REPL_TEST_HISTORY when set, else
// ~/.cache/agent-repl/test-history.<conditions>.json -- the module's one host
// cache directory on every platform (the daemon's binaries, locks and sockets
// live there too), never the platform's own cache directory, which on macOS
// is ~/Library/Caches.
//
// EACH SET OF RUN CONDITIONS KEEPS ITS OWN HISTORY. A unit's cost is a
// property of the machine AND of what it ran under: the same daemon suite
// took 124-132s with its units at the utility I/O tier and 51-55s at the
// default one (2026-10-14), so one estimate fed by both plans neither well.
// conditions names them (the run's root and its units' I/O tier), and is a
// single file-name component.
func DefaultPath(conditions string) (string, error) {
	if conditions == "" || strings.ContainsRune(conditions, filepath.Separator) {
		return "", fmt.Errorf("history: the run conditions %q are not one file-name component", conditions)
	}
	if p := os.Getenv("AGENT_REPL_TEST_HISTORY"); p != "" {
		return p, nil
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return "", fmt.Errorf("history: resolve the home directory: %w", err)
	}
	return filepath.Join(home, ".cache", "agent-repl", "test-history."+conditions+".json"), nil
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

// Peak is a key's decaying peak, and whether one was measured.
func (s *Store) Peak(key string) (float64, bool) {
	e, ok := s.entries[key]
	if !ok || e.Peak == nil {
		return 0, false
	}
	return *e.Peak, true
}

// Longest is the longest any unit (a key under prefix) is known to take:
// the largest of every such key's estimate and peak, and whether any is
// known.
func (s *Store) Longest(prefix string) (float64, bool) {
	longest, known := 0.0, false
	for key, e := range s.entries {
		if !strings.HasPrefix(key, prefix) {
			continue
		}
		known = true
		longest = max(longest, e.Seconds)
		if e.Peak != nil {
			longest = max(longest, *e.Peak)
		}
	}
	return longest, known
}

// Record folds measurements into the history on disk, re-reading it under the
// lock first so a concurrent run's measurements are kept, never overwritten.
func (s *Store) Record(measured map[string]Sample) error {
	for key, m := range measured {
		if m.Seconds < 0 || m.Peak < m.Seconds {
			return fmt.Errorf("history: refusing the measurement %+v for %q: it is negative, or its peak is below its typical time", m, key)
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
	for key, m := range measured {
		peak := m.Peak
		e, ok := entries[key]
		if !ok {
			entries[key] = Entry{Seconds: m.Seconds, Samples: 1, Peak: &peak}
			continue
		}
		if e.Peak != nil {
			peak = max(peak, *e.Peak*PeakDecay)
		}
		entries[key] = Entry{Seconds: (1-Weight)*e.Seconds + Weight*m.Seconds, Samples: e.Samples + 1, Peak: &peak}
	}
	data, err := json.MarshalIndent(file{Version: version, Entries: entries}, "", " ")
	if err != nil {
		return fmt.Errorf("history: encode: %w", err)
	}
	if err := atomicfile.Replace(s.path, ".test-history-*", data); err != nil {
		return fmt.Errorf("history: %w", err)
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
