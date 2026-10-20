package wsm

import (
	"context"
	"os"
	"path/filepath"
	"reflect"
	"sort"
	"testing"
	"time"

	"agent-repld/internal/dlog"
)

// writeCopy writes a stand-in pre-migration copy beside path, aged to at.
func writeCopy(t *testing.T, path, suffix string, at time.Time) string {
	t.Helper()
	copyPath := path + suffix
	if err := os.WriteFile(copyPath, []byte("copy"), 0o644); err != nil {
		t.Fatalf("write %s: %v", copyPath, err)
	}
	if err := os.Chtimes(copyPath, at, at); err != nil {
		t.Fatalf("age %s: %v", copyPath, err)
	}
	return copyPath
}

func present(t *testing.T, paths ...string) []string {
	t.Helper()
	var out []string
	for _, p := range paths {
		if _, err := os.Lstat(p); err == nil {
			out = append(out, filepath.Base(p))
		}
	}
	sort.Strings(out)
	return out
}

func TestPruneMigrationCopies(t *testing.T) {
	now := time.Date(2026, 10, 20, 12, 0, 0, 0, time.UTC)
	young := now.Add(-time.Hour)
	old := now.Add(-MigrationCopyRetention - time.Hour)
	cases := []struct {
		name   string
		copies map[string]time.Time
		want   []string
	}{
		{
			name:   "the newest copy is kept while it is young",
			copies: map[string]time.Time{".layout56.bak-2026-10-19": now.Add(-26 * time.Hour), ".layout57.bak-2026-10-20": young},
			want:   []string{"wsm.db.layout57.bak-2026-10-20"},
		},
		{
			name:   "the newest copy goes too once it is past the retention",
			copies: map[string]time.Time{".layout55.bak-2026-10-10": old.Add(-time.Hour), ".layout56.bak-2026-10-16": old},
		},
		{
			name:   "a lone young copy is kept",
			copies: map[string]time.Time{".layout57.bak-2026-10-20": young},
			want:   []string{"wsm.db.layout57.bak-2026-10-20"},
		},
		{
			name: "files that are not this database's migration copies are never touched",
			copies: map[string]time.Time{
				".realtest-bak-20260914-003327": old, ".layout3.refused-2026-09-10": old, "-wal": old,
			},
			want: []string{"wsm.db-wal", "wsm.db.layout3.refused-2026-09-10", "wsm.db.realtest-bak-20260914-003327"},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			path := filepath.Join(t.TempDir(), "wsm.db")
			var written []string
			for suffix, at := range tc.copies {
				written = append(written, writeCopy(t, path, suffix, at))
			}
			log := dlog.NewTestLogger()

			// Act.
			pruneMigrationCopies(path, now, log)

			// Assert.
			if got := present(t, written...); !reflect.DeepEqual(got, tc.want) {
				t.Fatalf("left %v, want %v", got, tc.want)
			}
		})
	}
}

func TestPruneMigrationCopiesRecordsWhatItRemoved(t *testing.T) {
	// Arrange.
	now := time.Now()
	path := filepath.Join(t.TempDir(), "wsm.db")
	gone := writeCopy(t, path, ".layout55.bak-2026-10-10", now.Add(-MigrationCopyRetention-time.Hour))
	kept := writeCopy(t, path, ".layout57.bak-2026-10-20", now)
	log := dlog.NewTestLogger()

	// Act.
	pruneMigrationCopies(path, now, log)

	// Assert.
	rec, ok := recordFor(log, "daemon.wsm.migration_copies", "info", "removed the pre-migration copies that can no longer be restored")
	if !ok {
		t.Fatalf("no removal record: %v", log.Records())
	}
	if rec.Context["kept"] != kept || !reflect.DeepEqual(rec.Context["removed_copies"], []string{gone}) {
		t.Fatalf("record context %v, want %s removed and %s kept", rec.Context, gone, kept)
	}
}

func TestPruneMigrationCopiesRecordsNothingWhenNothingIsRemoved(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "wsm.db")
	writeCopy(t, path, ".layout57.bak-2026-10-20", time.Now())
	log := dlog.NewTestLogger()

	// Act.
	pruneMigrationCopies(path, time.Now(), log)

	// Assert.
	if records := log.Records(); len(records) != 0 {
		t.Fatalf("records = %v, want none", records)
	}
}

func TestPruneMigrationCopiesRecordsAFailedRemoval(t *testing.T) {
	// Arrange: a directory the copy cannot be removed from.
	dir := t.TempDir()
	path := filepath.Join(dir, "wsm.db")
	stuck := writeCopy(t, path, ".layout55.bak-2026-10-10", time.Now().Add(-MigrationCopyRetention-time.Hour))
	if err := os.Chmod(dir, 0o555); err != nil {
		t.Fatalf("chmod: %v", err)
	}
	t.Cleanup(func() { _ = os.Chmod(dir, 0o755) })
	log := dlog.NewTestLogger()

	// Act.
	pruneMigrationCopies(path, time.Now(), log)

	// Assert.
	rec, ok := recordFor(log, "daemon.wsm.migration_copies", "error", "could not remove a pre-migration copy")
	if !ok || rec.Context["copy"] != stuck || rec.Context["error"] == "" {
		t.Fatalf("records = %v, want the failed removal with its copy and cause", log.Records())
	}
}

func TestPruneMigrationCopiesRecordsAnUnlistableDirectory(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "absent", "wsm.db")
	log := dlog.NewTestLogger()

	// Act.
	pruneMigrationCopies(path, time.Now(), log)

	// Assert.
	if _, ok := recordFor(log, "daemon.wsm.migration_copies", "error", "could not list the state directory to prune pre-migration copies"); !ok {
		t.Fatalf("records = %v, want the listing failure", log.Records())
	}
}

func TestEveryOpenThatSucceedsPrunesStaleCopies(t *testing.T) {
	cases := []struct {
		name string
		open func(ctx context.Context, path string, opts ...Option) (DB, error)
	}{
		{name: "the writing open", open: Open},
		{name: "the joining open", open: OpenJoining},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: a current database with a stale copy beside it.
			path := filepath.Join(t.TempDir(), "wsm.db")
			created, err := Open(context.Background(), path, WithUnsyncedWrites())
			if err != nil {
				t.Fatalf("create: %v", err)
			}
			created.Close()
			stale := writeCopy(t, path, ".layout40.bak-2026-10-01", time.Now().Add(-MigrationCopyRetention-time.Hour))

			// Act.
			handle, err := tc.open(context.Background(), path, WithUnsyncedWrites())
			if err != nil {
				t.Fatalf("open: %v", err)
			}
			defer handle.Close()

			// Assert.
			if _, err := os.Lstat(stale); !os.IsNotExist(err) {
				t.Fatalf("the stale copy survived %s", tc.name)
			}
		})
	}
}

func TestARefusedOpenPrunesNothing(t *testing.T) {
	// Arrange: a file stamped newer than this build, with a stale copy beside it.
	path := filepath.Join(t.TempDir(), "wsm.db")
	created, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("create: %v", err)
	}
	created.Close()
	stampRaw(t, path, LayoutVersion+1)
	stale := writeCopy(t, path, ".layout40.bak-2026-10-01", time.Now().Add(-MigrationCopyRetention-time.Hour))

	// Act.
	if _, err := Open(context.Background(), path, WithUnsyncedWrites()); err == nil {
		t.Fatal("Open on a newer layout succeeded, want it refused")
	}

	// Assert.
	if _, err := os.Lstat(stale); err != nil {
		t.Fatalf("a refused open removed the copy: %v", err)
	}
}
