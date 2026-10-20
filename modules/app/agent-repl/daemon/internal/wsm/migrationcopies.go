package wsm

import (
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"sort"
	"time"

	"agent-repld/internal/dlog"
)

// PRE-MIGRATION COPIES ARE KEPT ONLY WHILE THEY COULD STILL BE RESTORED.
//
// copyAsideBeforeMigrating writes one full `VACUUM INTO` copy of the state
// database before every migration, and nothing ever removed one: 58 copies and
// 12 GB by 2026-10-20, each up to 850 MB. A copy exists for one use, restoring
// the file as it stood before a migration that went wrong, and two facts bound
// how long that use lasts:
//
//   - ONLY THE NEWEST COPY CAN BE RESTORED. An older copy predates a migration
//     that already succeeded on top of it; restoring it would undo that one
//     too, and every row written since.
//   - THE NEWEST COPY DECAYS. Every hour the daemon runs on the migrated file,
//     restoring the copy throws away that hour's workspaces and merges. Past
//     MigrationCopyRetention a restore costs more than whatever it would save.
//
// So every writing or joining open that succeeds keeps at most the newest copy,
// and that one only while it is younger than MigrationCopyRetention. An open
// that FAILS prunes nothing: its refusal names the copy it was taken with.

// MigrationCopyRetention is how long the newest pre-migration copy is kept.
const MigrationCopyRetention = 72 * time.Hour

// migrationCopyName matches the basename copyAsideBeforeMigrating gives a
// copy of base: `<base>.layout<N>.bak-<YYYY-MM-DD>`.
func migrationCopyName(base string) *regexp.Regexp {
	return regexp.MustCompile(`^` + regexp.QuoteMeta(base) + `\.layout[0-9]+\.bak-[0-9]{4}-[0-9]{2}-[0-9]{2}$`)
}

// pruneMigrationCopies removes every pre-migration copy of the database at
// path except the newest, and the newest too once it is older than
// MigrationCopyRetention at now. A removal that fails is recorded at ERROR and
// does not fail the open: the database itself is sound, and the next open
// tries again.
func pruneMigrationCopies(path string, now time.Time, log dlog.Logger) {
	const op = "daemon.wsm.migration_copies"
	dir, base := filepath.Dir(path), filepath.Base(path)
	entries, err := os.ReadDir(dir)
	if err != nil {
		log.Error(op, "could not list the state directory to prune pre-migration copies", dlog.Context{
			"path": path, "error": err.Error(),
		})
		return
	}
	name := migrationCopyName(base)
	type copyFile struct {
		path string
		mod  time.Time
	}
	var copies []copyFile
	for _, entry := range entries {
		if !entry.Type().IsRegular() || !name.MatchString(entry.Name()) {
			continue
		}
		info, err := entry.Info()
		if errors.Is(err, fs.ErrNotExist) {
			continue
		}
		if err != nil {
			log.Error(op, "could not read a pre-migration copy's age", dlog.Context{
				"path": path, "copy": filepath.Join(dir, entry.Name()), "error": err.Error(),
			})
			continue
		}
		copies = append(copies, copyFile{path: filepath.Join(dir, entry.Name()), mod: info.ModTime()})
	}
	sort.Slice(copies, func(i, j int) bool { return copies[i].mod.After(copies[j].mod) })
	var removed []string
	kept := ""
	for i, c := range copies {
		if i == 0 && now.Sub(c.mod) < MigrationCopyRetention {
			kept = c.path
			continue
		}
		if err := os.Remove(c.path); err != nil && !errors.Is(err, fs.ErrNotExist) {
			log.Error(op, "could not remove a pre-migration copy", dlog.Context{
				"path": path, "copy": c.path, "error": err.Error(),
			})
			continue
		}
		removed = append(removed, c.path)
	}
	if len(removed) == 0 {
		return
	}
	log.Info(op, "removed the pre-migration copies that can no longer be restored", dlog.Context{
		"path": path, "removed": len(removed), "removed_copies": removed, "kept": kept,
		"retention": MigrationCopyRetention.String(),
	})
}
