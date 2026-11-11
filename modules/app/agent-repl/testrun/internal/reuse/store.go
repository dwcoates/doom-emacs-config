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

// Record says one unit passed on one set of inputs, or, as a unit's last
// outcome (Store.Last), that it failed on them.
type Record struct {
	Version int `json:"version"`
	// Suite is the unit's record name (UnitRecordName).
	Suite string `json:"suite"`
	Key   string `json:"key"`
	// PassedAt is when the run that passed it ended; for a failure, when the
	// run that failed it ended.
	PassedAt time.Time `json:"passed_at"`
	// Failed marks a last outcome that was a failure. A pass record never
	// carries it.
	Failed bool `json:"failed,omitempty"`
	// Checkout is the worktree the run tested.
	Checkout string `json:"checkout"`
	// Branch and Commit are the checkout's HEAD when the run started.
	Branch string `json:"branch"`
	Commit string `json:"commit"`
	// Dirty marks a checkout whose files differed from Commit: the passed
	// bytes are the key's, and Commit does NOT hold them, so a report never
	// claims it does.
	Dirty bool `json:"dirty"`
	// Units is how many of the runner's units (a package's chunks, a file's
	// one) passed.
	Units int `json:"units"`
	// Manifest is the key's itemized content, so a later run can say which
	// input differs from this record. Only a unit's newest pass and its last
	// outcome carry one (Store); a pass record under its key does not.
	Manifest Manifest `json:"manifest,omitempty"`
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

// suiteName is a whole suite's record name, or one unit's (UnitRecordName).
var suiteName = regexp.MustCompile(`^[A-Za-z0-9_-]+(@[A-Za-z0-9_.%\[\]-]+)?$`)

// lastFile is the name of a unit's last-outcome record in its directory, and
// newestFile its newest pass with that pass's manifest: no .json extension,
// so neither is ever read as a pass under a key.
const (
	lastFile   = "last.outcome"
	newestFile = "newest.pass"
)

// maxPasses is how many pass records a unit keeps: the newest, by when they
// passed. A unit's records are a cache of facts, and every run of a changed
// unit adds one, so without a bound the host's store grows with every run
// (2026-11-11: 835 MB of suite records, none ever deleted, on a disk with
// 670 MB left). Enough to keep each workspace's recent variants of a unit
// reusable across the host.
const maxPasses = 32

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

// Newest answers the unit's most recently passed record, whatever its key,
// with its manifest, and whether there is one: what a run that cannot reuse
// compares against to say which input changed.
func (s Store) Newest(suite string) (Record, bool, error) {
	return s.readNamed(suite, newestFile)
}

// passes is every pass record of the unit under its key, newest first.
func (s Store) passes(suite string) ([]Record, []string, error) {
	dir := filepath.Join(s.Dir, suite)
	entries, err := os.ReadDir(dir)
	if errors.Is(err, fs.ErrNotExist) {
		return nil, nil, nil
	}
	if err != nil {
		return nil, nil, fmt.Errorf("reuse: list %s's records: %w", suite, err)
	}
	var records []Record
	var paths []string
	for _, e := range entries {
		if e.IsDir() || filepath.Ext(e.Name()) != ".json" {
			continue
		}
		p := filepath.Join(dir, e.Name())
		r, err := readRecord(p)
		if err != nil {
			return nil, nil, err
		}
		records = append(records, r)
		paths = append(paths, p)
	}
	idx := make([]int, len(records))
	for i := range idx {
		idx[i] = i
	}
	sort.SliceStable(idx, func(a, b int) bool { return records[idx[a]].PassedAt.After(records[idx[b]].PassedAt) })
	outR, outP := make([]Record, len(idx)), make([]string, len(idx))
	for i, j := range idx {
		outR[i], outP[i] = records[j], paths[j]
	}
	return outR, outP, nil
}

// Put writes the pass r.
func (s Store) Put(r Record) error {
	if r.Failed {
		return fmt.Errorf("reuse: %s's record is a failure, which is a last outcome (PutLast), never a pass", r.Suite)
	}
	p, err := s.path(r.Suite, r.Key)
	if err != nil {
		return err
	}
	withManifest := r
	r.Manifest = nil
	if err := s.write(p, r); err != nil {
		return err
	}
	newest, ok, err := s.Newest(r.Suite)
	if err != nil {
		return err
	}
	if !ok || !newest.PassedAt.After(r.PassedAt) {
		if err := s.write(filepath.Join(s.Dir, r.Suite, newestFile), withManifest); err != nil {
			return err
		}
	}
	return s.prune(r.Suite)
}

// prune deletes the unit's pass records beyond its newest maxPasses.
func (s Store) prune(suite string) error {
	_, paths, err := s.passes(suite)
	if err != nil {
		return err
	}
	if len(paths) <= maxPasses {
		return nil
	}
	for _, p := range paths[maxPasses:] {
		if err := os.Remove(p); err != nil && !errors.Is(err, fs.ErrNotExist) {
			return fmt.Errorf("reuse: prune %s's old pass %s: %w", suite, p, err)
		}
	}
	return nil
}

// Last answers the unit's last outcome, pass or failure, and whether it has
// one: what says a unit that runs failed before.
func (s Store) Last(suite string) (Record, bool, error) {
	return s.readNamed(suite, lastFile)
}

// readNamed reads the unit's record file name (lastFile, newestFile).
func (s Store) readNamed(suite, name string) (Record, bool, error) {
	if !suiteName.MatchString(suite) {
		return Record{}, false, fmt.Errorf("reuse: %q is not a suite name", suite)
	}
	r, err := readRecord(filepath.Join(s.Dir, suite, name))
	if errors.Is(err, fs.ErrNotExist) {
		return Record{}, false, nil
	}
	if err != nil {
		return Record{}, false, err
	}
	if r.Suite != suite {
		return Record{}, false, fmt.Errorf("reuse: %s's %s records %s", suite, name, r.Suite)
	}
	return r, true, nil
}

// PutLast writes r as its unit's last outcome, replacing the one before.
func (s Store) PutLast(r Record) error {
	if !suiteName.MatchString(r.Suite) {
		return fmt.Errorf("reuse: %q is not a suite name", r.Suite)
	}
	if !hexKey.MatchString(r.Key) {
		return fmt.Errorf("reuse: %q is not an input key", r.Key)
	}
	return s.write(filepath.Join(s.Dir, r.Suite, lastFile), r)
}

func (s Store) write(p string, r Record) error {
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
