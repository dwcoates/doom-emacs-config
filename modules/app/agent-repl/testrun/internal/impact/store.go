package impact

import (
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"time"

	"agentrepl/testrun/internal/atomicfile"
)

// The outcomes a record carries (testtimes.Pass, Fail, Skip).
const (
	Pass = "pass"
	Fail = "fail"
)

// TestKey names one top-level test of one package pass: the unit a record
// belongs to. A subtest folds into its top-level test.
type TestKey struct {
	Suite   string `json:"suite"`
	Package string `json:"package"`
	// Tags are the build tags of the package's pass, "" for none.
	Tags string `json:"tags,omitempty"`
	Test string `json:"test"`
}

// String is the key as a log names it.
func (k TestKey) String() string {
	tags := ""
	if k.Tags != "" {
		tags = "[" + k.Tags + "]"
	}
	return k.Suite + ":" + k.Package + tags + "." + k.Test
}

// dirName is the key's directory under the store: a digest, so a test name
// never has to be a legal file name.
func (k TestKey) dirName() string {
	sum := sha256.Sum256([]byte(k.Suite + "\x1f" + k.Package + "\x1f" + k.Tags + "\x1f" + k.Test))
	return hex.EncodeToString(sum[:16])
}

// recordVersion is the record file's format.
const recordVersion = 1

// Record is one test's run: what it depended on and how it ended.
type Record struct {
	Version int     `json:"version"`
	Key     TestKey `json:"key"`
	Outcome string  `json:"outcome"`
	// At is when the run that wrote it ended.
	At time.Time `json:"at"`
	// Snapshot names the snapshot (Snapshot.ID) whose paths Deps indexes.
	Snapshot string `json:"snapshot"`
	// Deps is the dependency set as a bitmap over the snapshot's paths.
	Deps Bitmap `json:"deps"`
	// Facts are the non-file dependencies: the toolchain, the build tags.
	Facts map[string]string `json:"facts"`
}

// Entry is one test's result a run hands to Store.WriteRun.
type Entry struct {
	Key     TestKey
	Outcome string
	// Deps are the repository-relative paths the test depended on.
	Deps  []string
	Facts map[string]string
}

// Retention bounds the store's growth: records older than MaxAge are removed,
// and a test keeps at most MaxPasses passing and MaxFails failing records,
// the newest.
type Retention struct {
	MaxAge    time.Duration
	MaxPasses int
	MaxFails  int
}

// DefaultRetention is the retention a run prunes with: 30 days (the owner's
// "once no live tree matches them, say after 30 days"), enough passing
// variants for each workspace's recent tree to be told apart.
var DefaultRetention = Retention{MaxAge: 30 * 24 * time.Hour, MaxPasses: 16, MaxFails: 2}

// Store holds the host's records: a cache of facts about bytes, shared by
// every worktree and never committed. Every file is written whole through a
// temp file and a rename (atomicfile), so a concurrent reader sees one
// complete file or none.
//
// Layout under Dir:
//
//	snapshots/<snapshot id>.json            a run's paths and blob hashes
//	tests/<key digest>/<deps digest>-<outcome>.json   one record
type Store struct {
	Dir string
}

func (s Store) snapshotPath(id string) string {
	return filepath.Join(s.Dir, "snapshots", id+".json")
}

func (s Store) testDir(k TestKey) string { return filepath.Join(s.Dir, "tests", k.dirName()) }

// writeJSON writes v whole to path, making its directory.
func writeJSON(path string, v any) error {
	data, err := json.Marshal(v)
	if err != nil {
		return fmt.Errorf("impact: encode %s: %w", path, err)
	}
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		return fmt.Errorf("impact: make the directory of %s: %w", path, err)
	}
	return atomicfile.Replace(path, ".impact-*.tmp", data)
}

func readJSON(path string, v any) error {
	data, err := os.ReadFile(path)
	if err != nil {
		return err
	}
	if err := json.Unmarshal(data, v); err != nil {
		return fmt.Errorf("impact: %s is not a stored impact file: %w", path, err)
	}
	return nil
}

// depsDigest names a record by what it depends on and how it ended, so a
// rerun over the same bytes with the same outcome replaces its own record.
func depsDigest(snap Snapshot, deps Bitmap, facts map[string]string) string {
	h := sha256.New()
	for _, i := range deps.Positions(len(snap.Files)) {
		fmt.Fprintf(h, "%s\x1f%s\n", snap.Files[i], snap.Hashes[i])
	}
	h.Write([]byte(FactsDigest(facts)))
	return hex.EncodeToString(h.Sum(nil))[:32]
}

// WriteRun stores one run's results: the snapshot of hashes (path to blob
// hash, which must hold every path any entry names) and a record per entry,
// all stamped at. It answers how many records it wrote. An entry naming a
// path the snapshot does not hold is a defect in the caller, refused whole
// before anything is written.
func (s Store) WriteRun(at time.Time, hashes map[string]string, entries []Entry) (int, error) {
	snap := NewSnapshot(hashes)
	id := snap.ID()
	records := make([]Record, 0, len(entries))
	for _, e := range entries {
		if e.Outcome != Pass && e.Outcome != Fail {
			return 0, fmt.Errorf("impact: %s ended %q, which is neither %q nor %q", e.Key, e.Outcome, Pass, Fail)
		}
		bits := NewBitmap(len(snap.Files))
		for _, d := range e.Deps {
			i, ok := snap.Index(d)
			if !ok {
				return 0, fmt.Errorf("impact: %s depends on %s, which the run's snapshot of hashes does not hold", e.Key, d)
			}
			bits.Set(i)
		}
		records = append(records, Record{Version: recordVersion, Key: e.Key, Outcome: e.Outcome, At: at, Snapshot: id, Deps: bits, Facts: e.Facts})
	}
	if len(records) == 0 {
		return 0, nil
	}
	if _, err := os.Stat(s.snapshotPath(id)); errors.Is(err, fs.ErrNotExist) {
		if err := writeJSON(s.snapshotPath(id), snap); err != nil {
			return 0, err
		}
	} else if err != nil {
		return 0, fmt.Errorf("impact: look for snapshot %s: %w", id, err)
	}
	for _, r := range records {
		name := depsDigest(snap, r.Deps, r.Facts) + "-" + r.Outcome + ".json"
		if err := writeJSON(filepath.Join(s.testDir(r.Key), name), r); err != nil {
			return 0, err
		}
	}
	return len(records), nil
}

// Loaded is a record with the snapshot its bitmap indexes.
type Loaded struct {
	Record   Record
	Snapshot Snapshot
}

// Deps lists the record's dependency paths and their recorded hashes.
func (l Loaded) Deps() (paths, hashes []string) {
	for _, i := range l.Record.Deps.Positions(len(l.Snapshot.Files)) {
		paths = append(paths, l.Snapshot.Files[i])
		hashes = append(hashes, l.Snapshot.Hashes[i])
	}
	return paths, hashes
}

// snapshots caches the snapshots one pass of reads touches.
type snapshots map[string]Snapshot

func (s Store) snapshot(cache snapshots, id string) (Snapshot, error) {
	if snap, ok := cache[id]; ok {
		return snap, nil
	}
	var snap Snapshot
	if err := readJSON(s.snapshotPath(id), &snap); err != nil {
		return Snapshot{}, fmt.Errorf("impact: read snapshot %s: %w", id, err)
	}
	if err := snap.Validate(); err != nil {
		return Snapshot{}, fmt.Errorf("impact: snapshot %s: %w", id, err)
	}
	cache[id] = snap
	return snap, nil
}

// load reads every record of key, newest first. A store file that is not
// whole is an error, never skipped: nothing is read back that a run did not
// write whole.
func (s Store) load(cache snapshots, key TestKey) ([]Loaded, error) {
	dir := s.testDir(key)
	names, err := os.ReadDir(dir)
	if errors.Is(err, fs.ErrNotExist) {
		return nil, nil
	}
	if err != nil {
		return nil, fmt.Errorf("impact: list the records of %s: %w", key, err)
	}
	var out []Loaded
	for _, n := range names {
		if !strings.HasSuffix(n.Name(), ".json") {
			continue
		}
		var r Record
		if err := readJSON(filepath.Join(dir, n.Name()), &r); err != nil {
			return nil, fmt.Errorf("impact: read a record of %s: %w", key, err)
		}
		if r.Version != recordVersion || r.Key != key {
			return nil, fmt.Errorf("impact: %s holds version %d of %s, want version %d of %s", filepath.Join(dir, n.Name()), r.Version, r.Key, recordVersion, key)
		}
		snap, err := s.snapshot(cache, r.Snapshot)
		if err != nil {
			return nil, err
		}
		out = append(out, Loaded{Record: r, Snapshot: snap})
	}
	sort.Slice(out, func(i, j int) bool { return out[i].Record.At.After(out[j].Record.At) })
	return out, nil
}

// Load reads every record of key, newest first.
func (s Store) Load(key TestKey) ([]Loaded, error) { return s.load(snapshots{}, key) }

// PruneResult counts what Prune removed.
type PruneResult struct {
	Records, Snapshots int
}

// Prune removes the records past ret, then the snapshots no record names.
func (s Store) Prune(now time.Time, ret Retention) (PruneResult, error) {
	var res PruneResult
	used := map[string]bool{}
	keyDirs, err := os.ReadDir(filepath.Join(s.Dir, "tests"))
	if err != nil && !errors.Is(err, fs.ErrNotExist) {
		return res, fmt.Errorf("impact: list the store's tests: %w", err)
	}
	for _, kd := range keyDirs {
		dir := filepath.Join(s.Dir, "tests", kd.Name())
		files, err := os.ReadDir(dir)
		if err != nil {
			return res, fmt.Errorf("impact: list %s: %w", dir, err)
		}
		type kept struct {
			path string
			r    Record
		}
		var passes, fails []kept
		for _, f := range files {
			p := filepath.Join(dir, f.Name())
			var r Record
			if err := readJSON(p, &r); err != nil {
				return res, fmt.Errorf("impact: prune: read %s: %w", p, err)
			}
			if now.Sub(r.At) > ret.MaxAge {
				if err := os.Remove(p); err != nil {
					return res, fmt.Errorf("impact: prune: remove %s: %w", p, err)
				}
				res.Records++
				continue
			}
			if r.Outcome == Pass {
				passes = append(passes, kept{p, r})
			} else {
				fails = append(fails, kept{p, r})
			}
		}
		for _, group := range []struct {
			list []kept
			max  int
		}{{passes, ret.MaxPasses}, {fails, ret.MaxFails}} {
			sort.Slice(group.list, func(i, j int) bool { return group.list[i].r.At.After(group.list[j].r.At) })
			for i, k := range group.list {
				if i < group.max {
					used[k.r.Snapshot] = true
					continue
				}
				if err := os.Remove(k.path); err != nil {
					return res, fmt.Errorf("impact: prune: remove %s: %w", k.path, err)
				}
				res.Records++
			}
		}
	}
	snaps, err := os.ReadDir(filepath.Join(s.Dir, "snapshots"))
	if err != nil && !errors.Is(err, fs.ErrNotExist) {
		return res, fmt.Errorf("impact: list the store's snapshots: %w", err)
	}
	for _, sn := range snaps {
		id := strings.TrimSuffix(sn.Name(), ".json")
		if used[id] {
			continue
		}
		if err := os.Remove(filepath.Join(s.Dir, "snapshots", sn.Name())); err != nil {
			return res, fmt.Errorf("impact: prune: remove snapshot %s: %w", sn.Name(), err)
		}
		res.Snapshots++
	}
	return res, nil
}
