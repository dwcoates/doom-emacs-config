package storagesweep

import (
	"context"
	"errors"
	"io/fs"
	"path/filepath"
	"slices"
	"testing"
	"time"

	"agent-repld/internal/dlog"
)

// cacheFile is one entry of the scripted build cache.
type cacheFile struct {
	name string
	mod  time.Time
	size int64
	dir  bool
	// infoErr fails the entry's Info.
	infoErr error
}

func (f cacheFile) Name() string { return f.name }
func (f cacheFile) IsDir() bool  { return f.dir }
func (f cacheFile) Type() fs.FileMode {
	if f.dir {
		return fs.ModeDir
	}
	return 0
}
func (f cacheFile) Info() (fs.FileInfo, error) { return f, f.infoErr }
func (f cacheFile) Size() int64                { return f.size }
func (f cacheFile) Mode() fs.FileMode          { return f.Type() }
func (f cacheFile) ModTime() time.Time         { return f.mod }
func (f cacheFile) Sys() any                   { return nil }

// fakeCacheFS is the scripted filesystem the build cache rule walks: no file
// of it is real.
type fakeCacheFS struct {
	dirs       map[string][]cacheFile
	readErrs   map[string]error
	removeErrs map[string]error
	read       []string
	removed    []string
}

func newFakeCacheFS() *fakeCacheFS {
	return &fakeCacheFS{dirs: map[string][]cacheFile{}, readErrs: map[string]error{}, removeErrs: map[string]error{}}
}

func (f *fakeCacheFS) ReadDir(dir string) ([]fs.DirEntry, error) {
	f.read = append(f.read, dir)
	if err := f.readErrs[dir]; err != nil {
		return nil, err
	}
	files, found := f.dirs[dir]
	if !found {
		return nil, fs.ErrNotExist
	}
	out := make([]fs.DirEntry, len(files))
	for i, file := range files {
		out[i] = file
	}
	return out, nil
}

func (f *fakeCacheFS) Remove(path string) error {
	if err := f.removeErrs[path]; err != nil {
		return err
	}
	f.removed = append(f.removed, path)
	return nil
}

const cacheRoot = "/cache/go-build"

// cached gives the sweeper a build cache and puts files in its subdirectory
// sub, answering the subdirectory's path.
func (w *world) cached(sub string, files ...cacheFile) string {
	w.cacheDir = cacheRoot
	path := filepath.Join(cacheRoot, sub)
	if !slices.ContainsFunc(w.cache.dirs[cacheRoot], func(f cacheFile) bool { return f.name == sub }) {
		w.cache.dirs[cacheRoot] = append(w.cache.dirs[cacheRoot], cacheFile{name: sub, dir: true})
	}
	w.cache.dirs[path] = append(w.cache.dirs[path], files...)
	return path
}

func (w *world) trim(dryRun bool) Report {
	w.t.Helper()
	report, err := w.sweeper().TrimBuildCache(context.Background(), dryRun)
	if err != nil {
		w.t.Fatalf("TrimBuildCache: %v", err)
	}
	return report
}

func (w *world) buildCacheRecords(level string) []dlog.Record {
	return dlog.FilterRecords(w.log.Records(), dlog.RecordQuery{Level: level, Operation: opBuildCache})
}

// at is an instant in November 2026, whose 9th is a Monday, in zone.
func at(zone *time.Location, dayOfMonth, hour int) time.Time {
	return time.Date(2026, time.November, dayOfMonth, hour, 0, 0, 0, zone)
}

func TestBusinessDaysBefore(t *testing.T) {
	zone := time.FixedZone("host", -5*3600)
	tests := []struct {
		name string
		from time.Time
		want time.Time
	}{
		{"a Monday goes back to the Thursday before", at(zone, 9, 15), at(zone, 5, 15)},
		{"a Tuesday goes back to the Friday before", at(zone, 10, 15), at(zone, 6, 15)},
		{"a Wednesday goes back to the Monday", at(zone, 11, 15), at(zone, 9, 15)},
		{"a Thursday goes back to the Tuesday", at(zone, 12, 15), at(zone, 10, 15)},
		{"a Friday goes back to the Wednesday", at(zone, 13, 15), at(zone, 11, 15)},
		{"a Saturday goes back to the Thursday", at(zone, 14, 15), at(zone, 12, 15)},
		{"a Sunday goes back to the Thursday", at(zone, 15, 15), at(zone, 12, 15)},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := businessDaysBefore(tt.from, BuildCacheBusinessDays)

			// Assert.
			if !got.Equal(tt.want) {
				t.Fatalf("businessDaysBefore(%v) = %v, want %v", tt.from, got, tt.want)
			}
		})
	}
}

func TestBusinessDaysBeforeNoDaysIsTheSameInstant(t *testing.T) {
	// Arrange.
	from := at(time.UTC, 15, 15)

	// Act.
	got := businessDaysBefore(from, 0)

	// Assert.
	if !got.Equal(from) {
		t.Fatalf("businessDaysBefore(%v, 0) = %v, want it unmoved", from, got)
	}
}

// TestTheBuildCacheRuleCountsBusinessDays runs the rule on each kind of day:
// an entry last used a minute before the cutoff goes, and one last used at
// the cutoff stays.
func TestTheBuildCacheRuleCountsBusinessDays(t *testing.T) {
	tests := []struct {
		name   string
		now    time.Time
		cutoff time.Time
	}{
		{"a Monday run", at(time.UTC, 9, 15), at(time.UTC, 5, 15)},
		{"a Saturday run", at(time.UTC, 14, 15), at(time.UTC, 12, 15)},
		{"a Sunday run", at(time.UTC, 15, 15), at(time.UTC, 12, 15)},
		{"a midweek run", at(time.UTC, 11, 15), at(time.UTC, 9, 15)},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			w := newWorld(t)
			w.clock.Set(tt.now)
			sub := w.cached("0a",
				cacheFile{name: "old-a", mod: tt.cutoff.Add(-time.Minute), size: 4},
				cacheFile{name: "edge-a", mod: tt.cutoff, size: 8},
			)

			// Act.
			report := w.trim(false)

			// Assert.
			if want := []string{filepath.Join(sub, "old-a")}; !slices.Equal(w.cache.removed, want) {
				t.Fatalf("removed %v, want %v", w.cache.removed, want)
			}
			if report.Kept[KeepRecentlyUsedBuildCache] != 1 {
				t.Fatalf("kept %v, want the entry used at the cutoff", report.Kept)
			}
		})
	}
}

func TestTheBuildCacheRuleCountsItsDaysInTheHostsZone(t *testing.T) {
	// Arrange: Monday 01:00 UTC is still Sunday 20:00 on the host, so the
	// cutoff is the host's Thursday 20:00, not the Thursday of the UTC Monday.
	w := newWorld(t)
	w.zone = time.FixedZone("host", -5*3600)
	w.clock.Set(at(time.UTC, 16, 1))
	cutoff := at(w.zone, 12, 20)
	sub := w.cached("0a",
		cacheFile{name: "old-d", mod: cutoff.Add(-time.Minute)},
		cacheFile{name: "kept-d", mod: cutoff.Add(time.Minute)},
	)

	// Act.
	w.trim(false)

	// Assert.
	if want := []string{filepath.Join(sub, "old-d")}; !slices.Equal(w.cache.removed, want) {
		t.Fatalf("removed %v, want %v", w.cache.removed, want)
	}
}

func TestTheBuildCacheRuleDeletesOnlyEntryFilesUnderItsSubdirectories(t *testing.T) {
	// Arrange: everything is long unused; only the two entry files may go.
	w := newWorld(t)
	old := now.Add(-30 * day)
	sub := w.cached("0a",
		cacheFile{name: "h1-a", mod: old, size: 1},
		cacheFile{name: "h1-d", mod: old, size: 2},
		cacheFile{name: "notes.txt", mod: old},
		cacheFile{name: "nested-a", mod: old, dir: true},
	)
	w.cached("fuzz", cacheFile{name: "corpus-a", mod: old})
	w.cached("0a1", cacheFile{name: "h2-a", mod: old})
	w.cache.dirs[cacheRoot] = append(w.cache.dirs[cacheRoot],
		cacheFile{name: "README", mod: old}, cacheFile{name: "trim.txt", mod: old}, cacheFile{name: "ff", mod: old})

	// Act.
	report := w.trim(false)

	// Assert.
	if want := []string{filepath.Join(sub, "h1-a"), filepath.Join(sub, "h1-d")}; !slices.Equal(w.cache.removed, want) {
		t.Fatalf("removed %v, want only %v", w.cache.removed, want)
	}
	if want := []string{cacheRoot, sub}; !slices.Equal(w.cache.read, want) {
		t.Fatalf("listed %v, want only the cache and its one hex subdirectory %v", w.cache.read, want)
	}
	if want := []Removal{{Path: sub, Reason: ReasonUnusedBuildCache, Bytes: 3, Entries: 2}}; !slices.Equal(report.Removed, want) {
		t.Fatalf("report removed %+v, want %+v", report.Removed, want)
	}
}

func TestTheBuildCacheTrimIsRecordedWithItsEntriesAndBytes(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	old := now.Add(-30 * day)
	w.cached("0a", cacheFile{name: "h1-a", mod: old, size: 10}, cacheFile{name: "h2-d", mod: now, size: 99})
	w.cached("0b", cacheFile{name: "h3-d", mod: old, size: 5})

	// Act.
	w.trim(false)

	// Assert.
	infos := w.buildCacheRecords("info")
	if len(infos) != 1 || infos[0].Message != "trimmed the Go build cache of the entries nothing used inside the window" {
		t.Fatalf("want one INFO record of the trim, got %+v", infos)
	}
	c := infos[0].Context
	if c["entries"] != 2 || c["bytes"] != int64(15) || c["kept"] != 1 || c["failures"] != 0 || c["dir"] != cacheRoot || c["dry_run"] != false {
		t.Fatalf("trim record context = %v, want 2 entries, 15 bytes, 1 kept in %s", c, cacheRoot)
	}
	if want := businessDaysBefore(now.In(time.UTC), BuildCacheBusinessDays).Format(time.RFC3339); c["cutoff"] != want {
		t.Fatalf("trim record cutoff = %v, want %s", c["cutoff"], want)
	}
}

func TestADryRunTrimDeletesNothingAndReportsWhatWouldGo(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	sub := w.cached("0a", cacheFile{name: "h1-a", mod: now.Add(-30 * day), size: 10})

	// Act.
	report := w.trim(true)

	// Assert.
	if len(w.cache.removed) != 0 {
		t.Fatalf("a dry run removed %v", w.cache.removed)
	}
	if want := []Removal{{Path: sub, Reason: ReasonUnusedBuildCache, Bytes: 10, Entries: 1}}; !report.DryRun || !slices.Equal(report.Removed, want) {
		t.Fatalf("dry run report %+v, want %+v", report, want)
	}
	infos := w.buildCacheRecords("info")
	if len(infos) != 1 || infos[0].Message != "would trim the Go build cache of the entries nothing used inside the window" || infos[0].Context["dry_run"] != true {
		t.Fatalf("want one INFO record of what would go, got %+v", infos)
	}
}

func TestABuildCacheEntryThatCannotBeDeletedIsAnErrorAndTheTrimGoesOn(t *testing.T) {
	// Arrange: two entries refuse, a third goes.
	w := newWorld(t)
	old := now.Add(-30 * day)
	sub := w.cached("0a",
		cacheFile{name: "h1-a", mod: old, size: 1},
		cacheFile{name: "h2-a", mod: old, size: 2},
		cacheFile{name: "h3-a", mod: old, size: 4},
	)
	w.cache.removeErrs[filepath.Join(sub, "h1-a")] = errors.New("permission denied")
	w.cache.removeErrs[filepath.Join(sub, "h2-a")] = errors.New("busy")

	// Act.
	report := w.trim(false)

	// Assert.
	if want := []string{filepath.Join(sub, "h3-a")}; !slices.Equal(w.cache.removed, want) {
		t.Fatalf("removed %v, want %v", w.cache.removed, want)
	}
	if want := []Removal{{Path: sub, Reason: ReasonUnusedBuildCache, Bytes: 4, Entries: 1}}; report.Failures != 2 || !slices.Equal(report.Removed, want) {
		t.Fatalf("report %+v, want 2 failures and %+v", report, want)
	}
	errs := w.buildCacheRecords("error")
	if len(errs) != 1 {
		t.Fatalf("want one ERROR record for the subdirectory, got %+v", errs)
	}
	c := errs[0].Context
	if c["subdir"] != sub || c["failures"] != 2 || c["first_path"] != filepath.Join(sub, "h1-a") || c["first_cause"] != "permission denied" {
		t.Fatalf("failure record context = %v", c)
	}
}

func TestAnEntryWhoseAgeCannotBeReadIsAnErrorAndIsKept(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	sub := w.cached("0a", cacheFile{name: "h1-a", mod: now.Add(-30 * day), infoErr: errors.New("input/output error")})

	// Act.
	report := w.trim(false)

	// Assert.
	if len(w.cache.removed) != 0 || report.Failures != 1 || len(report.Removed) != 0 {
		t.Fatalf("removed %v, report %+v; want nothing removed and one failure", w.cache.removed, report)
	}
	errs := w.buildCacheRecords("error")
	if len(errs) != 1 || errs[0].Context["first_path"] != filepath.Join(sub, "h1-a") || errs[0].Context["first_cause"] != "input/output error" {
		t.Fatalf("want one ERROR record naming the unreadable entry, got %+v", errs)
	}
}

func TestAnEntryTheGoCommandAlreadyTrimmedIsNoFailure(t *testing.T) {
	// Arrange: one entry vanishes before it is read, another before it is
	// deleted.
	w := newWorld(t)
	old := now.Add(-30 * day)
	sub := w.cached("0a",
		cacheFile{name: "h1-a", mod: old, infoErr: fs.ErrNotExist},
		cacheFile{name: "h2-a", mod: old, size: 3},
	)
	w.cache.removeErrs[filepath.Join(sub, "h2-a")] = fs.ErrNotExist

	// Act.
	report := w.trim(false)

	// Assert.
	if report.Failures != 0 || len(report.Removed) != 0 || len(w.buildCacheRecords("error")) != 0 {
		t.Fatalf("report %+v, errors %+v; want no failure and nothing counted", report, w.buildCacheRecords("error"))
	}
}

func TestAnUnlistableCacheSubdirectoryIsAnErrorAndTheTrimGoesOn(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	bad := w.cached("0a", cacheFile{name: "h1-a", mod: now.Add(-30 * day)})
	good := w.cached("0b", cacheFile{name: "h2-a", mod: now.Add(-30 * day)})
	w.cache.readErrs[bad] = errors.New("permission denied")

	// Act.
	report := w.trim(false)

	// Assert.
	if want := []string{filepath.Join(good, "h2-a")}; report.Failures != 1 || !slices.Equal(w.cache.removed, want) {
		t.Fatalf("removed %v with %d failures, want %v and one failure", w.cache.removed, report.Failures, want)
	}
	errs := w.buildCacheRecords("error")
	if len(errs) != 1 || errs[0].Context["subdir"] != bad || errs[0].Context["first_cause"] != "permission denied" {
		t.Fatalf("want one ERROR record naming the unlistable subdirectory, got %+v", errs)
	}
}

func TestACacheSubdirectoryGoneBeforeItIsListedIsNoFailure(t *testing.T) {
	// Arrange: the cache names a subdirectory that is no longer there.
	w := newWorld(t)
	w.cacheDir = cacheRoot
	w.cache.dirs[cacheRoot] = []cacheFile{{name: "0a", dir: true}}

	// Act.
	report := w.trim(false)

	// Assert.
	if report.Failures != 0 || len(w.buildCacheRecords("error")) != 0 {
		t.Fatalf("report %+v, errors %+v; want no failure", report, w.buildCacheRecords("error"))
	}
}

func TestAnUnlistableBuildCacheIsAnErrorAndNothingIsTrimmed(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	w.cached("0a", cacheFile{name: "h1-a", mod: now.Add(-30 * day)})
	w.cache.readErrs[cacheRoot] = errors.New("permission denied")

	// Act.
	report := w.trim(false)

	// Assert.
	if report.Failures != 1 || len(w.cache.removed) != 0 {
		t.Fatalf("removed %v with %d failures, want nothing and one failure", w.cache.removed, report.Failures)
	}
	errs := w.buildCacheRecords("error")
	if len(errs) != 1 || errs[0].Context["dir"] != cacheRoot || errs[0].Context["cause"] != "permission denied" {
		t.Fatalf("want one ERROR record naming the cache and the cause, got %+v", errs)
	}
}

func TestAMissingBuildCacheIsNothingToTrim(t *testing.T) {
	// Arrange: the host names a cache directory nothing has made yet.
	w := newWorld(t)
	w.cacheDir = cacheRoot

	// Act.
	report := w.trim(false)

	// Assert.
	if report.Failures != 0 || len(report.Removed) != 0 || len(w.buildCacheRecords("error")) != 0 {
		t.Fatalf("report %+v, want nothing", report)
	}
}

func TestAHostWithNoBuildCacheListsNothing(t *testing.T) {
	// Arrange: no cache directory was given (GOCACHE=off).
	w := newWorld(t)

	// Act.
	report := w.trim(false)

	// Assert.
	if len(w.cache.read) != 0 || len(report.Removed) != 0 {
		t.Fatalf("listed %v, report %+v; want nothing asked of the filesystem", w.cache.read, report)
	}
}

func TestATrimStoppedWithTheDaemonSaysSoAndAnswersTheContextsError(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	w.cached("0a", cacheFile{name: "h1-a", mod: now.Add(-30 * day)})
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	_, err := w.sweeper().TrimBuildCache(ctx, false)

	// Assert.
	if !errors.Is(err, context.Canceled) || len(w.cache.removed) != 0 {
		t.Fatalf("TrimBuildCache = %v, removed %v; want context.Canceled and nothing removed", err, w.cache.removed)
	}
	infos := w.buildCacheRecords("info")
	if len(infos) != 1 || infos[0].Message != "the Go build cache trim stopped with the daemon" {
		t.Fatalf("want one INFO record that the trim stopped, got %+v", infos)
	}
}

func TestTheWholeSweepTrimsTheBuildCacheToo(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	sub := w.cached("0a", cacheFile{name: "h1-a", mod: now.Add(-30 * day), size: 6})

	// Act.
	report := w.sweep(false)

	// Assert.
	if want := []Removal{{Path: sub, Reason: ReasonUnusedBuildCache, Bytes: 6, Entries: 1}}; !slices.Equal(report.Removed, want) {
		t.Fatalf("sweep removed %+v, want %+v", report.Removed, want)
	}
}

func TestATrimAloneSweepsNothingElse(t *testing.T) {
	// Arrange: a shim directory the whole sweep would delete.
	w := newWorld(t)
	dir := w.shim("deadbeef", now)
	w.cached("0a", cacheFile{name: "h1-a", mod: now.Add(-30 * day)})

	// Act.
	w.trim(false)

	// Assert.
	if !exists(dir) {
		t.Fatal("a build cache trim deleted shim state")
	}
}

func TestATrimHeldByAnotherDaemonDeletesNothing(t *testing.T) {
	// Arrange: a second sweeper on the same lock trims while the first runs.
	w := newWorld(t)
	w.cached("0a", cacheFile{name: "h1-a", mod: now.Add(-30 * day)})
	outer := w.sweeper()
	inner := w.sweeper()
	var err error

	// Act.
	outer.guard.Do(w.log, func() { _, err = inner.TrimBuildCache(context.Background(), false) })

	// Assert.
	if !errors.Is(err, ErrHeldElsewhere) || len(w.cache.removed) != 0 {
		t.Fatalf("TrimBuildCache = %v, removed %v; want ErrHeldElsewhere and nothing removed", err, w.cache.removed)
	}
}

func TestTheBuildCacheIsTrimmedAtTheStartAndThenOnItsCadence(t *testing.T) {
	// Arrange: one entry to take at the start, and one that ages past the
	// cutoff only by the second trim.
	w := newWorld(t)
	sub := w.cached("0a", cacheFile{name: "h1-a", mod: now.Add(-30 * day)})
	ctx, cancel := context.WithCancel(context.Background())
	done := make(chan struct{})
	s := w.sweeper()

	// Act.
	go func() {
		defer close(done)
		s.runBuildCacheTrims(ctx)
	}()
	firstWait := <-w.clock.Asked
	afterStart := slices.Clone(w.cache.removed)
	w.cache.dirs[sub] = append(w.cache.dirs[sub], cacheFile{name: "h2-a", mod: now.Add(-30 * day)})
	w.clock.Fire <- now
	secondWait := <-w.clock.Asked
	cancel()
	<-done

	// Assert.
	if firstWait != DefaultBuildCacheEvery || secondWait != DefaultBuildCacheEvery {
		t.Fatalf("waits %v then %v, want the build cache cadence %v each", firstWait, secondWait, DefaultBuildCacheEvery)
	}
	if want := []string{filepath.Join(sub, "h1-a")}; !slices.Equal(afterStart, want) {
		t.Fatalf("removed before the first wait %v, want %v", afterStart, want)
	}
	if want := filepath.Join(sub, "h2-a"); !slices.Contains(w.cache.removed, want) {
		t.Fatalf("removed %v, want the cadence's trim to take %s", w.cache.removed, want)
	}
}

func TestAHostWithNoBuildCacheSchedulesNoTrims(t *testing.T) {
	// Arrange.
	w := newWorld(t)

	// Act.
	w.sweeper().runBuildCacheTrims(context.Background())

	// Assert.
	if len(w.clock.Asked) != 0 {
		t.Fatal("a host with no build cache waited for a trim")
	}
}

func TestRunStartsBothSchedulesAndReturnsWhenTheyStop(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	sub := w.cached("0a", cacheFile{name: "h1-a", mod: now.Add(-30 * day)})
	ctx, cancel := context.WithCancel(context.Background())
	result := make(chan error, 1)
	s := w.sweeper()

	// Act.
	go func() { result <- s.Run(ctx) }()
	waits := []time.Duration{<-w.clock.Asked, <-w.clock.Asked}
	removed := slices.Clone(w.cache.removed)
	cancel()
	err := <-result

	// Assert.
	slices.Sort(waits)
	if want := []time.Duration{DefaultStartDelay, DefaultBuildCacheEvery}; !slices.Equal(waits, want) {
		t.Fatalf("waits %v, want the sweep's start delay and the build cache cadence %v", waits, want)
	}
	if want := []string{filepath.Join(sub, "h1-a")}; !slices.Equal(removed, want) {
		t.Fatalf("removed by the start's trim %v, want %v", removed, want)
	}
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("Run = %v, want context.Canceled", err)
	}
}
