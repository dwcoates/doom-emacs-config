package logging

import (
	"context"
	"os"
	"path/filepath"
	"reflect"
	"sort"
	"strings"
	"syscall"
	"testing"
	"time"
)

var retentionNow = time.Date(2026, 10, 20, 12, 0, 0, 0, time.UTC)

// expired and fresh are last-write instants either side of the retention.
var (
	expired = retentionNow.Add(-DefaultRetention - time.Hour)
	fresh   = retentionNow.Add(-time.Hour)
)

func TestOpenRotatingHoldsASharedLockOnTheCurrentFile(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "svc.log")

	// Act.
	openForTest(t, path, 1<<20, 5)

	// Assert.
	if got := lockOutcomeOf(t, path); got != lockHeld {
		t.Fatalf("lock outcome on the open current file = %v, want it held", got)
	}
}

func TestARollLocksTheFreshCurrentFileAndReleasesTheRetiredOne(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "svc.log")
	w := openForTest(t, path, 1<<20, 5)
	mustWrite(t, w, "old\n")

	// Act.
	if err := w.Roll(); err != nil {
		t.Fatalf("Roll: %v", err)
	}

	// Assert.
	if got := lockOutcomeOf(t, path); got != lockHeld {
		t.Errorf("lock outcome on the fresh current file = %v, want it held", got)
	}
	if got := lockOutcomeOf(t, path+".1"); got != lockTaken {
		t.Errorf("lock outcome on the retired generation = %v, want it free", got)
	}
}

func TestNamesFile(t *testing.T) {
	cases := []struct {
		name   string
		change func(t *testing.T, path string)
		want   bool
	}{
		{name: "the path still names the open file", change: func(*testing.T, string) {}, want: true},
		{name: "the path was removed", change: func(t *testing.T, path string) { mustRemove(t, path) }, want: false},
		{name: "the path names a newer file", change: func(t *testing.T, path string) {
			mustRemove(t, path)
			writeAged(t, path, "new\n", fresh)
		}, want: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			path := filepath.Join(t.TempDir(), "svc.log")
			writeAged(t, path, "x\n", fresh)
			f, err := os.Open(path)
			if err != nil {
				t.Fatalf("open: %v", err)
			}
			t.Cleanup(func() { _ = f.Close() })
			tc.change(t, path)

			// Act.
			got, err := namesFile(path, f)

			// Assert.
			if err != nil || got != tc.want {
				t.Fatalf("namesFile = (%v, %v), want %v", got, err, tc.want)
			}
		})
	}
}

func TestRenameIfPresentAnswersAnAbsentSourceAsNothingToMove(t *testing.T) {
	// Arrange.
	dir := t.TempDir()

	// Act.
	err := renameIfPresent(filepath.Join(dir, "gone.log.2"), filepath.Join(dir, "gone.log.3"))

	// Assert.
	if err != nil {
		t.Fatalf("renameIfPresent of an absent generation = %v, want nil", err)
	}
}

func TestCurrentExpired(t *testing.T) {
	cases := []struct {
		name string
		body string
		at   time.Time
		want bool
	}{
		{name: "a written file past the retention is expired", body: "x\n", at: expired, want: true},
		{name: "a written file inside the retention is not", body: "x\n", at: fresh, want: false},
		{name: "an empty file is never expired", body: "", at: expired, want: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			path := filepath.Join(t.TempDir(), "svc.log")
			writeAged(t, path, tc.body, tc.at)
			w := openForTest(t, path, 1<<20, 5)

			// Act.
			got, err := w.CurrentExpired(retentionNow)

			// Assert.
			if err != nil || got != tc.want {
				t.Fatalf("CurrentExpired = (%v, %v), want %v", got, err, tc.want)
			}
		})
	}
}

func TestExpiryOfAClosedWriterIsRefused(t *testing.T) {
	cases := []struct {
		name string
		act  func(w *RotatingFile) error
	}{
		{name: "CurrentExpired", act: func(w *RotatingFile) error { _, err := w.CurrentExpired(retentionNow); return err }},
		{name: "ExpireGenerations", act: func(w *RotatingFile) error { _, err := w.ExpireGenerations(retentionNow); return err }},
		{name: "Expire", act: func(w *RotatingFile) error { _, err := w.Expire(retentionNow); return err }},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			w := openForTest(t, filepath.Join(t.TempDir(), "svc.log"), 1<<20, 5)
			if err := w.Close(); err != nil {
				t.Fatalf("Close: %v", err)
			}

			// Act.
			err := tc.act(w)

			// Assert.
			if err == nil {
				t.Fatalf("%s on a closed writer succeeded, want it refused", tc.name)
			}
		})
	}
}

func TestExpireGenerationsRemovesOnlyTheGenerationsPastTheRetention(t *testing.T) {
	// Arrange: .1 is fresh, .2 and .3 are past the retention.
	path := filepath.Join(t.TempDir(), "svc.log")
	w := openForTest(t, path, 1<<20, 5)
	writeAged(t, path+".1", "one\n", fresh)
	writeAged(t, path+".2", "two\n", expired)
	writeAged(t, path+".3", "three\n", expired)

	// Act.
	removed, err := w.ExpireGenerations(retentionNow)

	// Assert.
	if err != nil || !reflect.DeepEqual(removed, []string{path + ".2", path + ".3"}) {
		t.Fatalf("ExpireGenerations = (%v, %v), want .2 and .3 removed", removed, err)
	}
	if got := readFile(t, path+".1"); got != "one\n" {
		t.Errorf("generation .1 holds %q, want it kept", got)
	}
}

func TestExpireRollsAnExpiredCurrentFileAndRemovesIt(t *testing.T) {
	// Arrange: a current file whose last write is past the retention.
	path := filepath.Join(t.TempDir(), "svc.log")
	writeAged(t, path, "stale\n", expired)
	w := openForTest(t, path, 1<<20, 5)

	// Act.
	result, err := w.Expire(retentionNow)

	// Assert: the stale file is gone and the next write starts a fresh one.
	if err != nil || !result.RolledCurrent || !reflect.DeepEqual(result.Removed, []string{path + ".1"}) {
		t.Fatalf("Expire = (%+v, %v), want the current rolled and its generation removed", result, err)
	}
	mustWrite(t, w, "after\n")
	if got := readFile(t, path); got != "after\n" {
		t.Errorf("the current log holds %q, want only the record written after the expiry", got)
	}
}

func TestExpireLeavesAFreshFamilyAlone(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "svc.log")
	writeAged(t, path, "live\n", fresh)
	writeAged(t, path+".1", "one\n", fresh)
	w := openForTest(t, path, 1<<20, 5)

	// Act.
	result, err := w.Expire(retentionNow)

	// Assert.
	if err != nil || result.RolledCurrent || len(result.Removed) != 0 {
		t.Fatalf("Expire = (%+v, %v), want nothing done", result, err)
	}
}

func TestExpireOnAgesAtOnceAndAtEveryTickUntilCancelled(t *testing.T) {
	// Arrange: a generation that expires only at the tick's later instant.
	path := filepath.Join(t.TempDir(), "svc.log")
	w := openForTest(t, path, 1<<20, 5)
	writeAged(t, path+".1", "one\n", fresh)
	ticks := make(chan time.Time)
	reports := make(chan ExpireResult)
	ctx, cancel := context.WithCancel(context.Background())
	done := make(chan struct{})

	// Act.
	go func() {
		defer close(done)
		ExpireOn(ctx, w, ticks, func() time.Time { return retentionNow }, func(r ExpireResult, err error) {
			if err != nil {
				t.Errorf("Expire reported %v", err)
			}
			reports <- r
		})
	}()
	first := <-reports
	ticks <- retentionNow.Add(DefaultRetention)
	second := <-reports
	cancel()
	<-done

	// Assert.
	if len(first.Removed) != 0 {
		t.Errorf("the immediate expiry removed %v, want nothing yet", first.Removed)
	}
	if !reflect.DeepEqual(second.Removed, []string{path + ".1"}) {
		t.Errorf("the tick's expiry removed %v, want .1", second.Removed)
	}
}

func TestPruneDirectory(t *testing.T) {
	owned := func(base string) bool { return filepath.Ext(base) == ".log" }
	type file struct {
		name string
		at   time.Time
	}
	cases := []struct {
		name string
		// files are created before act, each aged to its instant.
		files []file
		// hold opens a RotatingFile on this base before act.
		hold string
		// holdShared takes a shared flock on this name before act.
		holdShared  string
		wantRemoved []string
		wantHeld    []string
		wantKept    int
	}{
		{
			name:        "an expired family nobody holds is removed whole",
			files:       []file{{"a.log", expired}, {"a.log.1", expired}, {"a.log.2", expired}},
			wantRemoved: []string{"a.log", "a.log.1", "a.log.2"},
		},
		{
			name:     "a fresh family is kept",
			files:    []file{{"a.log", fresh}, {"a.log.1", fresh}},
			wantKept: 2,
		},
		{
			name:        "only the expired files of a family nobody holds are removed",
			files:       []file{{"a.log", fresh}, {"a.log.1", expired}},
			wantRemoved: []string{"a.log.1"},
			wantKept:    1,
		},
		{
			name:     "a held family is skipped whole",
			files:    []file{{"a.log", expired}, {"a.log.1", expired}},
			hold:     "a.log",
			wantHeld: []string{"a.log"},
		},
		{
			name:        "the generations of a family with no current file are removed",
			files:       []file{{"a.log.1", expired}, {"a.log.2", expired}},
			wantRemoved: []string{"a.log.1", "a.log.2"},
		},
		{
			name:        "a generation a retiring child still holds is kept",
			files:       []file{{"a.log.1", expired}, {"a.log.2", expired}},
			holdShared:  "a.log.1",
			wantRemoved: []string{"a.log.2"},
			wantHeld:    []string{"a.log.1"},
		},
		{
			name:  "a family owned answers false about is never touched",
			files: []file{{"a.txt", expired}, {"a.txt.1", expired}},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			dir := t.TempDir()
			for _, f := range tc.files {
				writeAged(t, filepath.Join(dir, f.name), "x\n", f.at)
			}
			if tc.hold != "" {
				w := openForTest(t, filepath.Join(dir, tc.hold), 1<<20, 5)
				// Opening touches nothing, but the age is re-applied so the
				// held family is expired by every measure.
				writeAged(t, w.Path(), "", expired)
			}
			if tc.holdShared != "" {
				holdShared(t, filepath.Join(dir, tc.holdShared))
			}

			// Act.
			report, err := PruneDirectory(dir, retentionNow, DefaultRetention, owned)

			// Assert.
			if err != nil {
				t.Fatalf("PruneDirectory: %v", err)
			}
			if got := baseNames(report.Removed); !reflect.DeepEqual(got, sorted(tc.wantRemoved)) {
				t.Errorf("Removed = %v, want %v", got, tc.wantRemoved)
			}
			if got := baseNames(report.Held); !reflect.DeepEqual(got, sorted(tc.wantHeld)) {
				t.Errorf("Held = %v, want %v", got, tc.wantHeld)
			}
			if report.Kept != tc.wantKept {
				t.Errorf("Kept = %d, want %d", report.Kept, tc.wantKept)
			}
			for _, name := range report.Removed {
				if _, err := os.Lstat(name); !os.IsNotExist(err) {
					t.Errorf("%s is reported removed but is still present", name)
				}
			}
		})
	}
}

func TestPruneDirectoryRefusals(t *testing.T) {
	cases := []struct {
		name      string
		dir       func(t *testing.T) string
		retention time.Duration
	}{
		{name: "a non-positive retention", dir: func(t *testing.T) string { return t.TempDir() }, retention: 0},
		{name: "a missing directory", dir: func(t *testing.T) string { return filepath.Join(t.TempDir(), "absent") }, retention: DefaultRetention},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			_, err := PruneDirectory(tc.dir(t), retentionNow, tc.retention, func(string) bool { return true })

			// Assert.
			if err == nil {
				t.Fatalf("PruneDirectory succeeded, want %s refused", tc.name)
			}
		})
	}
}

func TestAWriterOpeningAPrunedPathWritesTheFileThePathNames(t *testing.T) {
	// Arrange: an expired family nobody holds is pruned away.
	dir := t.TempDir()
	path := filepath.Join(dir, "a.log")
	writeAged(t, path, "stale\n", expired)
	if _, err := PruneDirectory(dir, retentionNow, DefaultRetention, func(string) bool { return true }); err != nil {
		t.Fatalf("PruneDirectory: %v", err)
	}

	// Act.
	w := openForTest(t, path, 1<<20, 5)
	mustWrite(t, w, "fresh\n")

	// Assert.
	if got := readFile(t, path); got != "fresh\n" {
		t.Fatalf("the path holds %q, want the record written after the prune", got)
	}
}

func TestSplitGeneration(t *testing.T) {
	cases := []struct {
		name     string
		wantBase string
		wantGen  int
	}{
		{"svc.log", "svc.log", 0},
		{"svc.log.1", "svc.log", 1},
		{"svc.log.12", "svc.log", 12},
		{"svc.log.0", "svc.log.0", 0},
		{"svc.log.01", "svc.log.01", 0},
		{"svc.log.-1", "svc.log.-1", 0},
		{"svc.log.", "svc.log.", 0},
		{".5", ".5", 0},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			base, gen := splitGeneration(tc.name)

			// Assert.
			if base != tc.wantBase || gen != tc.wantGen {
				t.Fatalf("splitGeneration(%q) = (%q, %d), want (%q, %d)", tc.name, base, gen, tc.wantBase, tc.wantGen)
			}
		})
	}
}

func writeAged(t *testing.T, path, body string, at time.Time) {
	t.Helper()
	if body != "" {
		if err := os.WriteFile(path, []byte(body), 0o644); err != nil {
			t.Fatalf("writing %q: %v", path, err)
		}
	} else if _, err := os.Lstat(path); os.IsNotExist(err) {
		if err := os.WriteFile(path, nil, 0o644); err != nil {
			t.Fatalf("creating %q: %v", path, err)
		}
	}
	if err := os.Chtimes(path, at, at); err != nil {
		t.Fatalf("aging %q: %v", path, err)
	}
}

func mustRemove(t *testing.T, path string) {
	t.Helper()
	if err := os.Remove(path); err != nil {
		t.Fatalf("removing %q: %v", path, err)
	}
}

func holdShared(t *testing.T, path string) {
	t.Helper()
	f, err := os.Open(path)
	if err != nil {
		t.Fatalf("opening %q: %v", path, err)
	}
	t.Cleanup(func() { _ = f.Close() })
	if err := syscall.Flock(int(f.Fd()), syscall.LOCK_SH); err != nil {
		t.Fatalf("locking %q: %v", path, err)
	}
}

func lockOutcomeOf(t *testing.T, path string) lockOutcome {
	t.Helper()
	f, outcome, err := lockExclusive(path)
	if err != nil {
		t.Fatalf("lockExclusive(%q): %v", path, err)
	}
	if f != nil {
		if err := f.Close(); err != nil {
			t.Fatalf("closing %q: %v", path, err)
		}
	}
	return outcome
}

func baseNames(paths []string) []string {
	names := make([]string, 0, len(paths))
	for _, p := range paths {
		names = append(names, filepath.Base(p))
	}
	return sorted(names)
}

func sorted(names []string) []string {
	out := append([]string{}, names...)
	sort.Strings(out)
	if len(out) == 0 {
		return []string{}
	}
	return out
}

func TestStartExpiringAgesAtOnceAndStopsWhenAsked(t *testing.T) {
	// Arrange: a generation already past the retention.
	path := filepath.Join(t.TempDir(), "svc.log")
	w := openForTest(t, path, 1<<20, 5)
	writeAged(t, path+".1", "old\n", time.Now().Add(-DefaultRetention-time.Hour))
	reports := make(chan ExpireResult, 1)

	// Act.
	stop := StartExpiring(w, func(r ExpireResult, err error) {
		if err != nil {
			t.Errorf("Expire reported %v", err)
		}
		reports <- r
	})
	first := <-reports
	stop()

	// Assert.
	if !reflect.DeepEqual(first.Removed, []string{path + ".1"}) {
		t.Fatalf("the immediate expiry removed %v, want .1", first.Removed)
	}
}

func TestExpireResultChanged(t *testing.T) {
	cases := []struct {
		name   string
		result ExpireResult
		want   bool
	}{
		{name: "nothing aged", result: ExpireResult{}, want: false},
		{name: "the current rolled", result: ExpireResult{RolledCurrent: true}, want: true},
		{name: "a generation removed", result: ExpireResult{Removed: []string{"svc.log.1"}}, want: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act / Assert.
			if got := tc.result.Changed(); got != tc.want {
				t.Fatalf("Changed() = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestEveryRestatementOfTheRetentionMatchesIt(t *testing.T) {
	// The log reader is built alone and Emacs is another language, so each
	// restates the three days; a change to one must change all three.
	cases := []struct {
		file string
		want string
	}{
		{file: filepath.Join("..", "..", "..", "bin", "logs-reader.go"), want: "const sinkRetention = 72 * time.Hour"},
		{file: filepath.Join("..", "..", "..", "lisp", "core.el"), want: "(defcustom agent-repl-log-retention-seconds (* 3 86400)"},
	}
	for _, tc := range cases {
		t.Run(filepath.Base(tc.file), func(t *testing.T) {
			// Arrange.
			source, err := os.ReadFile(tc.file)
			if err != nil {
				t.Fatalf("read %s: %v", tc.file, err)
			}

			// Act / Assert.
			if DefaultRetention != 72*time.Hour || !strings.Contains(string(source), tc.want) {
				t.Fatalf("%s must restate DefaultRetention (%v) as %q; update every restatement together", tc.file, DefaultRetention, tc.want)
			}
		})
	}
}
