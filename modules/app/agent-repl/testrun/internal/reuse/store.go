package reuse

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"sort"
	"time"

	"agentrepl/testrun/internal/atomicfile"
)

// Record says one suite passed whole on one set of inputs.
type Record struct {
	Version int    `json:"version"`
	Suite   string `json:"suite"`
	Key     string `json:"key"`
	// PassedAt is when the run that passed it ended.
	PassedAt time.Time `json:"passed_at"`
	// Checkout is the worktree the run tested.
	Checkout string `json:"checkout"`
	// Branch and Commit are the checkout's HEAD when the run started.
	Branch string `json:"branch"`
	Commit string `json:"commit"`
	// Dirty marks a checkout whose files differed from Commit: the passed
	// bytes are the key's, and Commit does NOT hold them, so a report never
	// claims it does.
	Dirty bool `json:"dirty"`
	// Units is how many units of the suite passed.
	Units int `json:"units"`
	// Manifest is the key's itemized content, so a later run can say which
	// input differs from this record.
	Manifest Manifest `json:"manifest"`
}

// recordVersion is the record file's format.
const recordVersion = 1

// Store holds the host's pass records: a cache of facts about bytes, shared by
// every worktree on the host, never committed.
//
// A record is written whole to a temp file and renamed into place, so a
// concurrent reader sees one complete record or none, and two runs recording
// the same key write equivalent records. Nothing is ever read back that a run
// did not write whole.
type Store struct {
	Dir string
}

// DefaultStoreDir is where the host's records live: $AGENT_REPL_TEST_PASSES
// when set, else ~/.cache/agent-repl/test-passes beside the run history.
func DefaultStoreDir() (string, error) {
	if p := os.Getenv("AGENT_REPL_TEST_PASSES"); p != "" {
		return p, nil
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return "", fmt.Errorf("reuse: resolve the home directory: %w", err)
	}
	return filepath.Join(home, ".cache", "agent-repl", "test-passes"), nil
}

var hexKey = regexp.MustCompile(`^[0-9a-f]{64}$`)
var suiteName = regexp.MustCompile(`^[A-Za-z0-9_-]+$`)

func (s Store) path(suite, key string) (string, error) {
	if !suiteName.MatchString(suite) {
		return "", fmt.Errorf("reuse: %q is not a suite name", suite)
	}
	if !hexKey.MatchString(key) {
		return "", fmt.Errorf("reuse: %q is not an input key", key)
	}
	return filepath.Join(s.Dir, suite, key+".json"), nil
}

// Lookup answers the record for suite's key, and whether there is one.
func (s Store) Lookup(suite, key string) (Record, bool, error) {
	p, err := s.path(suite, key)
	if err != nil {
		return Record{}, false, err
	}
	r, err := readRecord(p)
	if errors.Is(err, fs.ErrNotExist) {
		return Record{}, false, nil
	}
	if err != nil {
		return Record{}, false, err
	}
	if r.Suite != suite || r.Key != key {
		return Record{}, false, fmt.Errorf("reuse: %s records %s/%s, not %s/%s", p, r.Suite, r.Key, suite, key)
	}
	return r, true, nil
}

// Newest answers the suite's most recently passed record, whatever its key,
// and whether there is one: what a run that cannot reuse compares against to
// say which input changed.
func (s Store) Newest(suite string) (Record, bool, error) {
	if !suiteName.MatchString(suite) {
		return Record{}, false, fmt.Errorf("reuse: %q is not a suite name", suite)
	}
	entries, err := os.ReadDir(filepath.Join(s.Dir, suite))
	if errors.Is(err, fs.ErrNotExist) {
		return Record{}, false, nil
	}
	if err != nil {
		return Record{}, false, fmt.Errorf("reuse: list %s's records: %w", suite, err)
	}
	var records []Record
	for _, e := range entries {
		if e.IsDir() || filepath.Ext(e.Name()) != ".json" {
			continue
		}
		r, err := readRecord(filepath.Join(s.Dir, suite, e.Name()))
		if err != nil {
			return Record{}, false, err
		}
		records = append(records, r)
	}
	if len(records) == 0 {
		return Record{}, false, nil
	}
	sort.Slice(records, func(i, j int) bool { return records[i].PassedAt.After(records[j].PassedAt) })
	return records[0], true, nil
}

// Put writes r.
func (s Store) Put(r Record) error {
	p, err := s.path(r.Suite, r.Key)
	if err != nil {
		return err
	}
	r.Version = recordVersion
	data, err := json.Marshal(r)
	if err != nil {
		return fmt.Errorf("reuse: encode the record of %s: %w", r.Suite, err)
	}
	if err := os.MkdirAll(filepath.Dir(p), 0o755); err != nil {
		return fmt.Errorf("reuse: create %s: %w", filepath.Dir(p), err)
	}
	if err := atomicfile.Replace(p, ".record-*", data); err != nil {
		return fmt.Errorf("reuse: %w", err)
	}
	return nil
}

func readRecord(p string) (Record, error) {
	data, err := os.ReadFile(p)
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			return Record{}, err
		}
		return Record{}, fmt.Errorf("reuse: read %s: %w", p, err)
	}
	var r Record
	if err := json.Unmarshal(data, &r); err != nil {
		return Record{}, fmt.Errorf("reuse: %s is not a pass record (delete it): %w", p, err)
	}
	if r.Version != recordVersion {
		return Record{}, fmt.Errorf("reuse: %s is record version %d, this runner reads %d (delete it)", p, r.Version, recordVersion)
	}
	return r, nil
}
