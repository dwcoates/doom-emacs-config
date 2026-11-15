package storagesweep

import (
	"os"
	"path/filepath"
	"testing"
)

// goCache lays out a Go build cache under the world's root: its bookkeeping
// files at the top, and three entries of 10 bytes last used 3, 2 and 1 days
// ago, in two of its subdirectories.
func (w *world) goCache() (dir, oldest, middle, newest string) {
	w.t.Helper()
	dir = filepath.Join(w.root, "go-build")
	w.write(filepath.Join(dir, "trim.txt"), "1700000000", now.Add(-30*day))
	w.write(filepath.Join(dir, "README"), "cache", now.Add(-30*day))
	oldest, middle, newest = filepath.Join(dir, "0a", "aaa-a"), filepath.Join(dir, "0b", "bbb-d"), filepath.Join(dir, "0a", "ccc-d")
	w.write(oldest, "0123456789", now.Add(-3*day))
	w.write(middle, "0123456789", now.Add(-2*day))
	w.write(newest, "0123456789", now.Add(-1*day))
	return dir, oldest, middle, newest
}

func TestTheBoundedRule(t *testing.T) {
	tests := []struct {
		name      string
		bound     int64
		dryRun    bool
		wantGone  []int
		wantFreed int64
	}{
		{name: "a cache within its bound is only measured", bound: 30},
		{name: "a cache past its bound loses its least recently used entries until it is back under", bound: 15, wantGone: []int{0, 1}, wantFreed: 20},
		{name: "a dry run deletes nothing and reports what would go", bound: 15, dryRun: true, wantFreed: 20},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			w := newWorld(t)
			dir, oldest, middle, newest := w.goCache()
			entries := []string{oldest, middle, newest}
			w.bounded = []BoundedDir{{Name: "go-build", Dir: dir, Bound: tt.bound}}

			// Act
			report := w.sweep(tt.dryRun)

			// Assert
			if len(report.Measured) != 1 || report.Measured[0] != (Measured{Name: "go-build", Dir: dir, Bytes: 30, Bound: tt.bound}) {
				t.Fatalf("measured %+v, want the cache's 30 bytes of entries against its bound", report.Measured)
			}
			gone := map[int]bool{}
			for _, i := range tt.wantGone {
				gone[i] = true
			}
			for i, e := range entries {
				if exists(e) == gone[i] {
					t.Errorf("entry %s exists %v, want gone %v", e, exists(e), gone[i])
				}
			}
			if !exists(filepath.Join(dir, "trim.txt")) || !exists(filepath.Join(dir, "README")) {
				t.Errorf("the cache's own bookkeeping was deleted")
			}
			if tt.wantFreed == 0 {
				if len(report.Removed) != 0 {
					t.Fatalf("removed %+v, want none", report.Removed)
				}
				return
			}
			if len(report.Removed) != 1 || report.Removed[0] != (Removal{Path: dir, Reason: ReasonOverBound, Bytes: tt.wantFreed}) {
				t.Fatalf("removed %+v, want one removal of the cache freeing %d bytes", report.Removed, tt.wantFreed)
			}
		})
	}
}

func TestAMissingBoundedCacheIsNothingToMeasure(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.bounded = []BoundedDir{{Name: "go-build", Dir: filepath.Join(w.root, "absent"), Bound: 1}}

	// Act
	report := w.sweep(false)

	// Assert
	if report.Failures != 0 || len(report.Measured) != 0 || len(w.errors()) != 0 {
		t.Fatalf("report %+v errors %v, want a quiet sweep that measured nothing", report, w.errors())
	}
}

func TestAnUnreadableBoundedCacheIsAnErrorAndIsNotTrimmed(t *testing.T) {
	// Arrange: a subdirectory the sweep cannot list.
	w := newWorld(t)
	dir, oldest, _, _ := w.goCache()
	locked := filepath.Join(dir, "0a")
	if err := os.Chmod(locked, 0o000); err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = os.Chmod(locked, 0o755) })
	w.bounded = []BoundedDir{{Name: "go-build", Dir: dir, Bound: 1}}

	// Act
	report := w.sweep(false)

	// Assert
	if err := os.Chmod(locked, 0o755); err != nil {
		t.Fatal(err)
	}
	errs := w.errors()
	if report.Failures != 1 || len(report.Removed) != 0 || !exists(oldest) || len(errs) != 1 ||
		errs[0].Operation != opBounded || errs[0].Context["name"] != "go-build" || errs[0].Context["dir"] != dir {
		t.Fatalf("report %+v errors %v, want one recorded failure and nothing deleted", report, errs)
	}
}

func TestAnEntryThatCannotBeDeletedIsAnErrorAndTheTrimGoesOn(t *testing.T) {
	// Arrange: the oldest entry's directory refuses deletions, so the trim
	// takes the next oldest in its place.
	w := newWorld(t)
	dir, oldest, middle, newest := w.goCache()
	w.write(filepath.Join(dir, "0c", "zzz-d"), "0123456789", now.Add(-4*day))
	stuck := filepath.Join(dir, "0c")
	if err := os.Chmod(stuck, 0o555); err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = os.Chmod(stuck, 0o755) })
	w.bounded = []BoundedDir{{Name: "go-build", Dir: dir, Bound: 25}}

	// Act
	report := w.sweep(false)

	// Assert
	errs := w.errors()
	if report.Failures != 1 || len(errs) != 1 || errs[0].Context["path"] != filepath.Join(stuck, "zzz-d") {
		t.Fatalf("report %+v errors %v, want the stuck entry's failure recorded", report, errs)
	}
	if exists(oldest) || exists(middle) || !exists(newest) {
		t.Fatalf("oldest %v middle %v newest %v, want the two next oldest deleted in the stuck one's place", exists(oldest), exists(middle), exists(newest))
	}
	if len(report.Removed) != 1 || report.Removed[0].Bytes != 20 {
		t.Fatalf("removed %+v, want 20 bytes freed", report.Removed)
	}
}
