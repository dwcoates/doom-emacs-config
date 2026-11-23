package storagesweep

import (
	"context"
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"time"

	"agent-repld/internal/clock"
	"agent-repld/internal/dlog"
	"agentrepl/logging/logsite"
)

// The build cache rule's reasons.
const (
	// ReasonUnusedBuildCache: entries of the Go build cache nothing used for
	// BuildCacheBusinessDays business days. One Removal names a cache
	// subdirectory and counts the entries deleted from it (Removal.Entries);
	// the subdirectory itself stays.
	ReasonUnusedBuildCache Reason = "unused_build_cache"

	// KeepRecentlyUsedBuildCache: a build cache entry something used inside
	// BuildCacheBusinessDays business days.
	KeepRecentlyUsedBuildCache KeepReason = "recently_used_build_cache"
)

// BuildCacheBusinessDays is how many business days an unused build cache
// entry is kept (owner ruling 2026-11-12). A business day is a weekday, in
// the host's zone; holidays are not counted.
const BuildCacheBusinessDays = 2

// DefaultBuildCacheEvery is the cadence of the build cache's own trims,
// between the daily sweeps that trim it too. Measured 2026-11-12: the cache
// grows about 17 GB an hour, so a daily trim alone lets a day of entries
// past the window pile up.
const DefaultBuildCacheEvery = time.Hour

// BuildCacheFS is the filesystem the build cache rule walks and deletes
// through.
type BuildCacheFS interface {
	// ReadDir lists a directory, as os.ReadDir does.
	ReadDir(dir string) ([]fs.DirEntry, error)
	// Remove deletes one file, as os.Remove does.
	Remove(path string) error
}

// OSBuildCacheFS is the real filesystem.
type OSBuildCacheFS struct{}

// ReadDir is os.ReadDir.
func (OSBuildCacheFS) ReadDir(dir string) ([]fs.DirEntry, error) { return os.ReadDir(dir) }

// Remove is os.Remove.
func (OSBuildCacheFS) Remove(path string) error { return os.Remove(path) }

// buildCacheSubdir matches the cache's own subdirectories: the first byte of
// an entry's hash, in hex.
var buildCacheSubdir = regexp.MustCompile(`^[0-9a-f]{2}$`)

// isBuildCacheEntry reports whether name is one of the cache's entry files:
// an action record (`<hash>-a`) or its output (`<hash>-d`). Everything else
// in the cache (its README, its trim stamp, the fuzz corpus) is left alone.
func isBuildCacheEntry(name string) bool {
	return strings.HasSuffix(name, "-a") || strings.HasSuffix(name, "-d")
}

// businessDaysBefore is AT moved back DAYS business days, at the same hour
// of the day in AT's zone: each step back onto a weekday counts as one, and a
// step onto a Saturday or a Sunday counts as none. So from a Monday, two
// business days back is the Thursday before, and from a Saturday or a Sunday
// it is that same Thursday.
func businessDaysBefore(at time.Time, days int) time.Time {
	for days > 0 {
		at = at.AddDate(0, 0, -1)
		if day := at.Weekday(); day != time.Saturday && day != time.Sunday {
			days--
		}
	}
	return at
}

// sweepBuildCache applies the build cache rule: every entry file of the Go
// build cache whose modification time is more than BuildCacheBusinessDays
// business days old is deleted, one file at a time.
//
// THE MODIFICATION TIME IS THE ENTRY'S LAST USE: the go command moves it
// forward whenever it reads an entry whose stamp is over an hour old, which
// is what its own trim (five days) judges by too. An entry deleted here is
// one the go command rebuilds on its next miss.
//
// THE CACHE DIRECTORY ITSELF IS NEVER REMOVED, and nothing in it but entry
// files under its two-hex-digit subdirectories is: `go clean -cache` would
// throw away the entries in use as well.
//
// AN ENTRY THAT IS GONE BY THE TIME IT IS READ OR DELETED IS NOT A FAILURE:
// the go command trims the same cache on its own schedule.
func (s *Sweeper) sweepBuildCache(ctx context.Context, now time.Time, report *Report) {
	dir := s.deps.BuildCacheDir
	if dir == "" {
		s.deps.Log.Debug(opBuildCache, "this host has no Go build cache to trim", nil)
		return
	}
	cutoff := businessDaysBefore(now.In(s.deps.Zone), BuildCacheBusinessDays)
	fields := dlog.Context{
		"dir": dir, "cutoff": cutoff.Format(time.RFC3339), "business_days": BuildCacheBusinessDays, "dry_run": report.DryRun,
	}
	subdirs, err := s.deps.BuildCacheFS.ReadDir(dir)
	if errors.Is(err, fs.ErrNotExist) {
		s.deps.Log.Debug(opBuildCache, "there is no Go build cache to trim", fields)
		return
	}
	if err != nil {
		report.Failures++
		fields["cause"] = err.Error()
		s.deps.Log.Error(opBuildCache, "the Go build cache could not be listed; nothing in it is trimmed", fields)
		return
	}
	var entries, kept, failures int
	var bytes int64
	stopped := false
	for _, subdir := range subdirs {
		if ctx.Err() != nil {
			stopped = true
			break
		}
		if !subdir.IsDir() || !buildCacheSubdir.MatchString(subdir.Name()) {
			continue
		}
		trimmed := s.trimBuildCacheSubdir(filepath.Join(dir, subdir.Name()), cutoff, report)
		entries, kept, failures, bytes = entries+trimmed.entries, kept+trimmed.kept, failures+trimmed.failures, bytes+trimmed.bytes
	}
	report.Kept[KeepRecentlyUsedBuildCache] += kept
	report.Failures += failures
	fields["entries"], fields["bytes"], fields["kept"], fields["failures"] = entries, bytes, kept, failures
	message := logsite.Msg("trimmed the Go build cache of the entries nothing used inside the window")
	switch {
	case stopped:
		message = logsite.Msg("the Go build cache trim stopped with the daemon")
	case report.DryRun:
		message = logsite.Msg("would trim the Go build cache of the entries nothing used inside the window")
	}
	s.deps.Log.Log(dlog.LevelInfo, opBuildCache, message, fields)
}

// trimmedSubdir is what one cache subdirectory's trim did.
type trimmedSubdir struct {
	entries, kept, failures int
	bytes                   int64
}

// trimBuildCacheSubdir trims one cache subdirectory, appending its Removal to
// the report when anything went. Its failures are recorded once, at ERROR,
// with their count and the first one's path and cause.
func (s *Sweeper) trimBuildCacheSubdir(subdir string, cutoff time.Time, report *Report) trimmedSubdir {
	var out trimmedSubdir
	var firstPath string
	var firstCause error
	failed := func(path string, err error) {
		if out.failures == 0 {
			firstPath, firstCause = path, err
		}
		out.failures++
	}
	files, err := s.deps.BuildCacheFS.ReadDir(subdir)
	switch {
	case errors.Is(err, fs.ErrNotExist):
		return out
	case err != nil:
		failed(subdir, err)
	}
	for _, file := range files {
		if !file.Type().IsRegular() || !isBuildCacheEntry(file.Name()) {
			continue
		}
		path := filepath.Join(subdir, file.Name())
		info, err := file.Info()
		if errors.Is(err, fs.ErrNotExist) {
			continue
		}
		if err != nil {
			failed(path, err)
			continue
		}
		if !info.ModTime().Before(cutoff) {
			out.kept++
			continue
		}
		if !report.DryRun {
			err := s.deps.BuildCacheFS.Remove(path)
			if errors.Is(err, fs.ErrNotExist) {
				continue
			}
			if err != nil {
				failed(path, err)
				continue
			}
		}
		out.entries++
		out.bytes += info.Size()
	}
	if out.failures > 0 {
		s.deps.Log.Error(opBuildCache, "entries of the Go build cache could not be trimmed; they are kept", dlog.Context{
			"subdir": subdir, "failures": out.failures, "first_path": firstPath, "first_cause": firstCause.Error(),
			"cutoff": cutoff.Format(time.RFC3339), "dry_run": report.DryRun,
		})
	}
	if out.entries > 0 {
		report.Removed = append(report.Removed, Removal{Path: subdir, Reason: ReasonUnusedBuildCache, Bytes: out.bytes, Entries: out.entries})
	}
	return out
}

// TrimBuildCache runs the build cache rule alone, under the sweep's guard:
// with dryRun it judges and deletes nothing. It refuses as Sweep does.
func (s *Sweeper) TrimBuildCache(ctx context.Context, dryRun bool) (Report, error) {
	return s.guarded(dryRun, func(report *Report) error {
		s.sweepBuildCache(ctx, s.deps.Clock.Now(), report)
		return ctx.Err()
	})
}

// runBuildCacheTrims is the build cache's own schedule: one trim as the
// daemon starts, then one every BuildCacheEvery. It runs beside the daily
// sweep on its own goroutine, so the start's trim never holds the daemon's
// start; a trim that finds a sweep running leaves the cache to that sweep,
// which trims it too.
func (s *Sweeper) runBuildCacheTrims(ctx context.Context) {
	if s.deps.BuildCacheDir == "" {
		s.deps.Log.Debug(opRun, "this host has no Go build cache; its trims are not scheduled", nil)
		return
	}
	trim := func(ctx context.Context) {
		switch _, err := s.TrimBuildCache(ctx, false); {
		case errors.Is(err, ErrSweepRunning):
			s.deps.Log.Debug(opRun, "the scheduled build cache trim found a sweep already running", nil)
		case errors.Is(err, ErrHeldElsewhere):
			s.deps.Log.Debug(opRun, "the scheduled build cache trim found another daemon sweeping", nil)
		}
	}
	trim(ctx)
	_ = clock.Repeat(ctx, s.deps.Clock, s.deps.BuildCacheEvery, s.deps.BuildCacheEvery, trim)
}
