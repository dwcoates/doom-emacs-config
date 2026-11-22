package impact

import (
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

func treeOf(hashes map[string]string) Tree {
	return func(p string) (string, bool, error) {
		h, ok := hashes[p]
		return h, ok, nil
	}
}

func TestBlobHashBytesMatchesGit(t *testing.T) {
	tests := []struct{ name, content, want string }{
		{"empty blob", "", "e69de29bb2d1d6434b8b29ae775ad8c2e48c5391"},
		{"hello newline", "hello\n", "ce013625030ba8dba906f756967f9e9ca394464a"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := BlobHashBytes([]byte(tt.content))
			// Assert
			if got != tt.want {
				t.Fatalf("BlobHashBytes(%q) = %s, want git's %s", tt.content, got, tt.want)
			}
		})
	}
}

func TestBlobHashReadsTheFileAndRefusesAMissingOne(t *testing.T) {
	// Arrange
	dir := t.TempDir()
	path := filepath.Join(dir, "f")
	if err := os.WriteFile(path, []byte("hello\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	// Act
	got, err := BlobHash(path)
	_, missing := BlobHash(filepath.Join(dir, "absent"))
	// Assert
	if err != nil || got != "ce013625030ba8dba906f756967f9e9ca394464a" {
		t.Fatalf("BlobHash = (%s, %v), want the blob hash of the file", got, err)
	}
	if missing == nil || !strings.Contains(missing.Error(), "impact: hash") {
		t.Fatalf("BlobHash of a missing file = %v, want an error naming the hash", missing)
	}
}

func TestSnapshotIsSortedAndContentNamed(t *testing.T) {
	// Arrange
	a := NewSnapshot(map[string]string{"b.go": "2", "a.go": "1"})
	b := NewSnapshot(map[string]string{"a.go": "1", "b.go": "2"})
	c := NewSnapshot(map[string]string{"a.go": "1", "b.go": "3"})
	// Assert
	if strings.Join(a.Files, ",") != "a.go,b.go" || a.Hashes[0] != "1" {
		t.Fatalf("NewSnapshot = %+v, want sorted paths with their hashes", a)
	}
	if a.ID() != b.ID() || a.ID() == c.ID() {
		t.Fatalf("IDs a=%s b=%s c=%s, want equal content named alike and changed content apart", a.ID(), b.ID(), c.ID())
	}
	if i, ok := a.Index("b.go"); !ok || i != 1 {
		t.Fatalf("Index(b.go) = (%d, %v), want (1, true)", i, ok)
	}
	if _, ok := a.Index("c.go"); ok {
		t.Fatal("Index(c.go) found a path the snapshot does not hold")
	}
}

func TestSnapshotValidateRefusesMalformed(t *testing.T) {
	tests := []struct {
		name string
		s    Snapshot
		want string
	}{
		{"out of step", Snapshot{Files: []string{"a"}, Hashes: nil}, "1 files and 0 hashes"},
		{"unsorted", Snapshot{Files: []string{"b", "a"}, Hashes: []string{"1", "2"}}, "not sorted"},
		{"repeated", Snapshot{Files: []string{"a", "a"}, Hashes: []string{"1", "2"}}, "not sorted"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			err := tt.s.Validate()
			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("Validate() = %v, want an error containing %q", err, tt.want)
			}
		})
	}
	if err := NewSnapshot(map[string]string{"a": "1"}).Validate(); err != nil {
		t.Fatalf("Validate() of a good snapshot = %v", err)
	}
}

func TestBitmapSetsCountsAndListsPositions(t *testing.T) {
	// Arrange
	b := NewBitmap(20)
	// Act
	for _, i := range []int{0, 7, 8, 19} {
		b.Set(i)
	}
	// Assert
	if b.Count() != 4 || !b.Has(7) || b.Has(6) || b.Has(500) {
		t.Fatalf("bitmap count=%d has7=%v has6=%v has500=%v", b.Count(), b.Has(7), b.Has(6), b.Has(500))
	}
	if got := b.Positions(20); len(got) != 4 || got[3] != 19 {
		t.Fatalf("Positions(20) = %v", got)
	}
	if got := b.Positions(8); len(got) != 2 {
		t.Fatalf("Positions(8) = %v, want only those below the limit", got)
	}
}

func TestFactsDigestIsOrderIndependent(t *testing.T) {
	if FactsDigest(map[string]string{"go": "1", "tags": "x"}) != FactsDigest(map[string]string{"tags": "x", "go": "1"}) {
		t.Fatal("FactsDigest depends on map order")
	}
	if FactsDigest(map[string]string{"go": "1"}) == FactsDigest(map[string]string{"go": "2"}) {
		t.Fatal("FactsDigest ignores a changed fact")
	}
}

var key = TestKey{Suite: "daemon", Package: "integration", Tags: "integration", Test: "TestA"}

func TestWriteRunThenDecideCases(t *testing.T) {
	facts := map[string]string{"go": "go1.24"}
	at := time.Date(2026, 11, 21, 0, 0, 0, 0, time.UTC)
	tests := []struct {
		name      string
		outcome   string
		recFacts  map[string]string
		tree      map[string]string
		wantSkip  bool
		wantInWhy string
	}{
		{"same bytes and a pass would skip", Pass, facts, map[string]string{"a.go": "1", "b.go": "2", "unrelated.go": "9"}, true, "exact set of 2 dependencies"},
		{"a changed dependency would run", Pass, facts, map[string]string{"a.go": "1", "b.go": "CHANGED"}, false, "b.go"},
		{"a deleted dependency would run", Pass, facts, map[string]string{"a.go": "1"}, false, "b.go (gone)"},
		{"only a failure would run", Fail, facts, map[string]string{"a.go": "1", "b.go": "2"}, false, "it last failed"},
		{"a changed toolchain would run", Pass, map[string]string{"go": "go1.23"}, map[string]string{"a.go": "1", "b.go": "2"}, false, "facts (toolchain, tags) differ"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			store := Store{Dir: t.TempDir()}
			n, err := store.WriteRun(at, map[string]string{"a.go": "1", "b.go": "2", "c.go": "3"},
				[]Entry{{Key: key, Outcome: tt.outcome, Deps: []string{"a.go", "b.go"}, Facts: tt.recFacts}})
			if err != nil || n != 1 {
				t.Fatalf("WriteRun = (%d, %v), want one record", n, err)
			}
			// Act
			got, err := store.Decide(key, facts, treeOf(tt.tree))
			// Assert
			if err != nil {
				t.Fatalf("Decide: %v", err)
			}
			if got.WouldSkip != tt.wantSkip || !strings.Contains(got.Reason, tt.wantInWhy) {
				t.Fatalf("Decide = %+v, want skip=%v with %q in its reason", got, tt.wantSkip, tt.wantInWhy)
			}
		})
	}
}

func TestDecideNoRecordWouldRun(t *testing.T) {
	// Act
	got, err := Store{Dir: t.TempDir()}.Decide(key, nil, treeOf(nil))
	// Assert
	if err != nil || got.WouldSkip || !strings.Contains(got.Reason, "no record") {
		t.Fatalf("Decide = (%+v, %v), want a would-run for a test with no record", got, err)
	}
}

func TestDecideUsesTheNearestOfSeveralPasses(t *testing.T) {
	// Arrange: two passes over different trees; the tree matches the older.
	store := Store{Dir: t.TempDir()}
	old := time.Date(2026, 11, 1, 0, 0, 0, 0, time.UTC)
	newer := old.Add(48 * time.Hour)
	if _, err := store.WriteRun(old, map[string]string{"a.go": "1"}, []Entry{{Key: key, Outcome: Pass, Deps: []string{"a.go"}}}); err != nil {
		t.Fatal(err)
	}
	if _, err := store.WriteRun(newer, map[string]string{"a.go": "2"}, []Entry{{Key: key, Outcome: Pass, Deps: []string{"a.go"}}}); err != nil {
		t.Fatal(err)
	}
	// Act
	got, err := store.Decide(key, nil, treeOf(map[string]string{"a.go": "1"}))
	// Assert
	if err != nil || !got.WouldSkip {
		t.Fatalf("Decide = (%+v, %v), want a would-skip on the older pass", got, err)
	}
}

func TestDecideNamesAtMostThreeChangedDependencies(t *testing.T) {
	// Arrange
	store := Store{Dir: t.TempDir()}
	hashes := map[string]string{"a": "1", "b": "1", "c": "1", "d": "1", "e": "1"}
	if _, err := store.WriteRun(time.Now(), hashes, []Entry{{Key: key, Outcome: Pass, Deps: []string{"a", "b", "c", "d", "e"}}}); err != nil {
		t.Fatal(err)
	}
	// Act
	got, err := store.Decide(key, nil, treeOf(map[string]string{"a": "2", "b": "2", "c": "2", "d": "2", "e": "2"}))
	// Assert
	if err != nil || got.WouldSkip || !strings.Contains(got.Reason, "and 2 more") {
		t.Fatalf("Decide = (%+v, %v), want three named and two more", got, err)
	}
}

func TestDecideReturnsTheTreeError(t *testing.T) {
	// Arrange
	store := Store{Dir: t.TempDir()}
	if _, err := store.WriteRun(time.Now(), map[string]string{"a": "1"}, []Entry{{Key: key, Outcome: Pass, Deps: []string{"a"}}}); err != nil {
		t.Fatal(err)
	}
	boom := errors.New("disk gone")
	// Act
	_, err := store.Decide(key, nil, func(string) (string, bool, error) { return "", false, boom })
	// Assert
	if !errors.Is(err, boom) || !strings.Contains(err.Error(), "hash a for daemon:integration[integration].TestA") {
		t.Fatalf("Decide = %v, want the tree's error naming the path and test", err)
	}
}

func TestWriteRunRefusesWhatItCannotStoreWhole(t *testing.T) {
	tests := []struct {
		name  string
		entry Entry
		want  string
	}{
		{"a dependency outside the snapshot", Entry{Key: key, Outcome: Pass, Deps: []string{"zzz.go"}}, "zzz.go, which the run's snapshot of hashes does not hold"},
		{"an outcome it cannot judge", Entry{Key: key, Outcome: "skip"}, `ended "skip"`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			store := Store{Dir: t.TempDir()}
			// Act
			n, err := store.WriteRun(time.Now(), map[string]string{"a.go": "1"}, []Entry{tt.entry})
			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) || n != 0 {
				t.Fatalf("WriteRun = (%d, %v), want nothing written and an error containing %q", n, err, tt.want)
			}
			if _, statErr := os.Stat(filepath.Join(store.Dir, "snapshots")); statErr == nil {
				t.Fatal("WriteRun left a snapshot behind after refusing the run")
			}
		})
	}
}

func TestWriteRunTwiceOverTheSameBytesKeepsOneRecord(t *testing.T) {
	// Arrange
	store := Store{Dir: t.TempDir()}
	e := Entry{Key: key, Outcome: Pass, Deps: []string{"a.go"}}
	// Act
	for _, at := range []time.Time{time.Now().Add(-time.Hour), time.Now()} {
		if _, err := store.WriteRun(at, map[string]string{"a.go": "1"}, []Entry{e}); err != nil {
			t.Fatal(err)
		}
	}
	got, err := store.Load(key)
	// Assert
	if err != nil || len(got) != 1 {
		t.Fatalf("Load = (%d records, %v), want the rerun to replace its own record", len(got), err)
	}
}

func TestLoadRefusesACorruptRecord(t *testing.T) {
	// Arrange
	store := Store{Dir: t.TempDir()}
	if _, err := store.WriteRun(time.Now(), map[string]string{"a.go": "1"}, []Entry{{Key: key, Outcome: Pass, Deps: []string{"a.go"}}}); err != nil {
		t.Fatal(err)
	}
	files, _ := filepath.Glob(filepath.Join(store.testDir(key), "*.json"))
	if err := os.WriteFile(files[0], []byte("{"), 0o644); err != nil {
		t.Fatal(err)
	}
	// Act
	_, err := store.Load(key)
	// Assert
	if err == nil || !strings.Contains(err.Error(), "is not a stored impact file") {
		t.Fatalf("Load = %v, want a refusal of the half-written record", err)
	}
}

func TestPruneRemovesOldRecordsExcessVariantsAndOrphanSnapshots(t *testing.T) {
	// Arrange
	store := Store{Dir: t.TempDir()}
	now := time.Date(2026, 12, 31, 0, 0, 0, 0, time.UTC)
	write := func(at time.Time, hash, outcome string) {
		t.Helper()
		if _, err := store.WriteRun(at, map[string]string{"a.go": hash}, []Entry{{Key: key, Outcome: outcome, Deps: []string{"a.go"}}}); err != nil {
			t.Fatal(err)
		}
	}
	write(now.Add(-40*24*time.Hour), "old", Pass)
	write(now.Add(-3*time.Hour), "p1", Pass)
	write(now.Add(-2*time.Hour), "p2", Pass)
	write(now.Add(-1*time.Hour), "p3", Pass)
	// Act
	res, err := store.Prune(now, Retention{MaxAge: 30 * 24 * time.Hour, MaxPasses: 2, MaxFails: 1})
	left, loadErr := store.Load(key)
	// Assert
	if err != nil || loadErr != nil {
		t.Fatalf("Prune = %v, Load = %v", err, loadErr)
	}
	if res.Records != 2 || res.Snapshots != 2 || len(left) != 2 {
		t.Fatalf("Prune = %+v with %d records left, want the 40-day record and the third-newest pass gone with their snapshots", res, len(left))
	}
}

func TestDeciderDecideEntryTreatsNonTopLevelNamesAsWouldRun(t *testing.T) {
	// Arrange
	store := Store{Dir: t.TempDir()}
	if _, err := store.WriteRun(time.Now(), map[string]string{"a.go": "1"}, []Entry{{Key: key, Outcome: Pass, Deps: []string{"a.go"}}}); err != nil {
		t.Fatal(err)
	}
	d := NewDecider(store, nil, treeOf(map[string]string{"a.go": "1"}))
	tests := []struct {
		name, test string
		wantSkip   bool
	}{
		{"a recorded top-level test", "TestA", true},
		{"a subtest path", "TestA/sub", false},
		{"an empty name", "", false},
		{"a new test", "TestNew", false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := d.DecideEntry("daemon", "integration", "integration", tt.test)
			// Assert
			if err != nil || got.WouldSkip != tt.wantSkip || got.Reason == "" {
				t.Fatalf("DecideEntry(%q) = (%+v, %v), want skip=%v with a reason", tt.test, got, err, tt.wantSkip)
			}
		})
	}
}

func TestDefaultStoreDirCases(t *testing.T) {
	t.Setenv("HOME", "/home/u")
	tests := []struct {
		name, env, want string
	}{
		{"the override", "/elsewhere", "/elsewhere"},
		{"beside the run history by default", "", "/home/u/.cache/agent-repl/test-impact"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			t.Setenv(EnvStore, tt.env)
			// Act
			got, err := DefaultStoreDir()
			// Assert
			if err != nil || got != tt.want {
				t.Fatalf("DefaultStoreDir = (%s, %v), want %s", got, err, tt.want)
			}
		})
	}
}
