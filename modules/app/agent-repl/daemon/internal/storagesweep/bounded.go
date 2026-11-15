package storagesweep

import (
	"io/fs"
	"os"
	"path/filepath"
	"slices"
	"time"

	"agent-repld/internal/dlog"
)

// The bounded rule's record and reason.
const (
	opBounded = "daemon.storagesweep.bounded"

	ReasonOverBound Reason = "over_bound"
)

// DefaultGoBuildCacheBound is the size past which the host's Go build cache
// is trimmed back. The cache grew to 240 GB and filled the disk twice on
// 2026-11-11: test runs build with many flag sets (coverage, tags, race), and
// cmd/go trims only entries unused for five days. 64 GiB keeps several days
// of every flag set's builds.
const DefaultGoBuildCacheBound int64 = 64 << 30

// BoundedDir is a cache directory kept under a size: past Bound, its least
// recently used entries are deleted, oldest first, until it is back under.
//
// AN ENTRY IS A FILE INSIDE ONE OF THE DIRECTORY'S SUBDIRECTORIES, as the Go
// build cache lays out its outputs (<cache>/<xx>/<hash>-a, -d). Files directly
// in the directory are its own bookkeeping (cmd/go's trim.txt and README) and
// are never deleted. A file's modification time is its last use: cmd/go
// refreshes it on every hit.
type BoundedDir struct {
	// Name names the cache in records and reports.
	Name string
	// Dir is the cache. A missing one holds nothing to measure.
	Dir string
	// Bound is the size past which entries are deleted, in bytes.
	Bound int64
}

// Measured is a bounded cache's size as one sweep found it.
type Measured struct {
	Name  string
	Dir   string
	Bytes int64
	Bound int64
}

// boundedEntry is one entry of a bounded cache.
type boundedEntry struct {
	path  string
	bytes int64
	used  time.Time
}

// sweepBounded measures one bounded cache and, past its bound, deletes its
// least recently used entries until it is back under. The whole trim is one
// removal naming the cache and what was freed.
func (s *Sweeper) sweepBounded(b BoundedDir, report *Report) {
	var entries []boundedEntry
	var total int64
	err := filepath.WalkDir(b.Dir, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if !d.Type().IsRegular() || filepath.Dir(path) == b.Dir {
			return nil
		}
		info, err := d.Info()
		if err != nil {
			return err
		}
		entries = append(entries, boundedEntry{path: path, bytes: info.Size(), used: info.ModTime()})
		total += info.Size()
		return nil
	})
	if os.IsNotExist(err) {
		s.deps.Log.Debug(opBounded, "a bounded cache does not exist; nothing to measure", dlog.Context{"name": b.Name, "dir": b.Dir})
		return
	}
	if err != nil {
		report.Failures++
		s.deps.Log.Error(opBounded, "a bounded cache could not be measured; it is not trimmed", dlog.Context{
			"name": b.Name, "dir": b.Dir, "cause": err.Error(),
		})
		return
	}
	report.Measured = append(report.Measured, Measured{Name: b.Name, Dir: b.Dir, Bytes: total, Bound: b.Bound})
	if total <= b.Bound {
		s.deps.Log.Info(opBounded, "a bounded cache is within its bound", dlog.Context{
			"name": b.Name, "dir": b.Dir, "bytes": total, "bound": b.Bound, "entries": len(entries),
		})
		return
	}
	slices.SortStableFunc(entries, func(x, y boundedEntry) int { return x.used.Compare(y.used) })
	var freed int64
	deleted := 0
	for _, e := range entries {
		if total-freed <= b.Bound {
			break
		}
		if !report.DryRun {
			if err := os.Remove(e.path); err != nil && !os.IsNotExist(err) {
				report.Failures++
				s.deps.Log.Error(opBounded, "a bounded cache's entry could not be deleted; it is kept", dlog.Context{
					"name": b.Name, "path": e.path, "cause": err.Error(),
				})
				continue
			}
		}
		freed += e.bytes
		deleted++
	}
	report.Removed = append(report.Removed, Removal{Path: b.Dir, Reason: ReasonOverBound, Bytes: freed})
	message := "trimmed a bounded cache's least recently used entries back under its bound"
	if report.DryRun {
		message = "would trim a bounded cache's least recently used entries back under its bound"
	}
	s.deps.Log.Info(opBounded, message, dlog.Context{
		"name": b.Name, "dir": b.Dir, "bytes": total, "bound": b.Bound, "freed": freed, "deleted": deleted,
		"remaining": total - freed, "dry_run": report.DryRun, "oldest_kept": oldestKept(entries, deleted),
	})
}

// oldestKept is the last use of the oldest entry a trim of the first DELETED
// of ENTRIES (oldest first) left, "" when it left none.
func oldestKept(entries []boundedEntry, deleted int) string {
	if deleted >= len(entries) {
		return ""
	}
	return entries[deleted].used.UTC().Format(time.RFC3339)
}
