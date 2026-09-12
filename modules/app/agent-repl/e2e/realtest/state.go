//go:build realtest

package realtest

import (
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"time"
)

// WHAT THE RUN IS ANSWERABLE FOR is whatever the state database holds: realtest
// 1 asserts that every workspace in it is drawn, and lists them with times. So
// the expected set comes from the database rather than from Emacs — asking
// Emacs which workspaces it drew and then checking that it drew them would be
// asserting the frontend against itself.
//
// A REALTEST NEVER OPENS THE OWNER'S LIVE DATABASE. IT READS A SNAPSHOT.
//
// The read used to be `sqlite3 -readonly` against ~/.claude-emacs/wsm.db
// itself. The INTENT behind that — a realtest may not write a byte of the
// owner's state — is right and survives here. The MECHANISM did not deliver it
// and could not read either.
//
// `wsm.db` is a WAL database, and every WAL reader needs the `-shm`
// shared-memory index. `-readonly` does not exempt a connection from that: if
// no `-shm` is on disk SQLite CREATES ONE BESIDE THE DATABASE even under
// `-readonly` (state_test.go asserts it), so the old read was writing into the
// owner's state directory, which is the one thing it promised not to do. And
// when that creation cannot happen the read does not degrade, it FAILS with
// `unable to open database file (14)` — the exact error that killed seven of
// eight realtests in under a hundredth of a second once the new sweep began
// stopping the daemon for realtest 3 and cold-starting the editor between
// tests. A read whose success depends on being allowed to write next to the
// owner's database is not a read-only read.
//
// A snapshot satisfies both halves: the owner's files are only ever read, and
// the copy — which the harness owns outright — may be opened read-write, so
// SQLite rebuilds the `-shm` and replays the `-wal` there. Nothing about the
// answer depends on whether a daemon happens to be running.
//
// WHY A COPY AND NOT `immutable=1`. The `file:...?immutable=1` URI also reads a
// WAL database with no `-shm`, and it is one flag rather than a directory of
// copied bytes. It is rejected because it is sound only when NOTHING can be
// writing, and this harness cannot promise that: the daemon is up and serving
// during realtests 2 and 4, and realtests 4 through 8 assert on registry rows
// they themselves just caused the daemon to write. `immutable=1` tells SQLite
// the file cannot change, so SQLite skips locking AND IGNORES THE `-wal`
// ENTIRELY — it would read the main database file alone and silently report
// the state as of the last checkpoint, which is precisely the stale truth a
// realtest would then assert on. One mechanism for every read (see
// `queryStateDB`), and the mechanism is the copy.
//
// WHY THE `-wal` TRAVELS WITH IT. A WAL database is not one file: copying the
// `.db` alone discards every transaction committed to the log and not yet
// checkpointed. bin/lib-realtest-backup.sh reasons this through for the backup
// path and this is the same rule for the read path. The `-shm` deliberately
// does NOT travel: it is a pure index that SQLite rebuilds from the `-wal` on
// the next read-write open, and a torn copy of it would be worse than its
// absence.

// StateDBPath is the workspace state database under the state root.
func StateDBPath(stateDir string) string { return stateDir + "/wsm.db" }

// stateFieldSep is the separator every snapshot read joins its fields with. A
// unit separator rather than the default pipe: a workspace name, a directory
// or a prompt's own text may legitimately contain a pipe and would otherwise
// split into the wrong number of fields and be reported as a malformed row.
const stateFieldSep = "\x1f"

// stateSnapshotAttempts bounds how many times a read will re-take its snapshot
// before giving up.
//
// A copy taken while the daemon checkpoints can catch the main database file
// mid-rewrite, so each snapshot is verified with `PRAGMA quick_check` before it
// is queried and a torn one is simply retaken. Bounded rather than unbounded:
// a database that is genuinely corrupt would otherwise spin forever instead of
// reporting what it found.
const stateSnapshotAttempts = 3

// stateSnapshotRoot is the directory snapshots are made under. Empty means the
// system temp directory, which is what every run uses; the tests point it at a
// directory of their own so they can assert that nothing is left behind.
var stateSnapshotRoot = ""

// takeStateSnapshot copies dbPath and its `-wal` sibling into a fresh
// directory and returns the copy's path together with the directory to remove.
//
// The directory is returned even on failure whenever it was created, so the
// caller can remove it on the error path too; a caller that only cleaned up
// after a successful read would leak a copy of the owner's state every time a
// read went wrong, which is the case where it matters most.
func takeStateSnapshot(dbPath string) (snapPath, dir string, err error) {
	if _, statErr := os.Stat(dbPath); statErr != nil {
		return "", "", fmt.Errorf("snapshot the state database %s: %w", dbPath, statErr)
	}
	dir, err = os.MkdirTemp(stateSnapshotRoot, "realtest-wsm-snapshot-")
	if err != nil {
		return "", "", fmt.Errorf("make a snapshot directory for %s: %w", dbPath, err)
	}
	snapPath = filepath.Join(dir, filepath.Base(dbPath))

	// The main database first and the log second: the main file is only
	// rewritten by a checkpoint, and a `-wal` copied after it can only be
	// newer, never older, than what it is being replayed onto.
	if err := copyStateFile(dbPath, snapPath); err != nil {
		return "", dir, err
	}
	// A `-wal` that is not there is not a failure: SQLite creates it on demand
	// and a cleanly closed database has none.
	if err := copyStateFile(dbPath+"-wal", snapPath+"-wal"); err != nil && !errors.Is(err, os.ErrNotExist) {
		return "", dir, err
	}
	return snapPath, dir, nil
}

// copyStateFile clones one file, reporting a missing source as os.ErrNotExist
// so its caller can decide whether that is allowed rather than guessing.
func copyStateFile(src, dest string) error {
	in, err := os.Open(src)
	if err != nil {
		return fmt.Errorf("copy %s aside: %w", src, err)
	}
	defer in.Close()

	out, err := os.OpenFile(dest, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0o600)
	if err != nil {
		return fmt.Errorf("copy %s aside to %s: %w", src, dest, err)
	}
	if _, err := io.Copy(out, in); err != nil {
		out.Close()
		return fmt.Errorf("copy %s aside to %s: %w", src, dest, err)
	}
	if err := out.Close(); err != nil {
		return fmt.Errorf("copy %s aside to %s: %w", src, dest, err)
	}
	return nil
}

// checkStateSnapshot is the torn-copy guard: `quick_check` on the copy, which
// also forces the `-wal` to be replayed into it before anything is asserted on
// what it holds.
func checkStateSnapshot(ctx context.Context, snapPath string) error {
	out, err := runSQLite(ctx, snapPath, "PRAGMA quick_check;")
	if err != nil {
		return err
	}
	if strings.TrimSpace(out) != "ok" {
		return fmt.Errorf("the snapshot %s did not pass quick_check: %s", snapPath, strings.TrimSpace(out))
	}
	return nil
}

// runSQLite runs one statement against a database THE HARNESS OWNS.
//
// No `-readonly` here, and that is the point of the snapshot: the copy must be
// opened read-write so SQLite can build the `-shm` it needs to read a WAL
// database at all. It is never handed the owner's path — `queryStateDB` is the
// only route to this and it always passes a snapshot.
func runSQLite(ctx context.Context, dbPath, query string) (string, error) {
	callCtx, cancel := context.WithTimeout(ctx, 30*time.Second)
	defer cancel()
	cmd := exec.CommandContext(callCtx, "sqlite3", "-separator", stateFieldSep, dbPath, query)
	out, err := cmd.CombinedOutput()
	if err != nil {
		return "", fmt.Errorf("query %s: %w; sqlite3 said: %s", dbPath, err, strings.TrimSpace(string(out)))
	}
	return string(out), nil
}

// queryStateDB is THE ONE ROUTE every realtest read of the owner's state takes:
// snapshot the database, verify the copy, query the copy, remove the copy.
//
// SNAPSHOT PER READ, not per run. A run's snapshot would be a photograph of the
// registry as it stood before the first act, and realtests 4 through 8 assert
// on rows they themselves just changed — a per-run snapshot would report the
// state before the act and the assertion would fail, or worse, pass on a stale
// row that happens to match. wsm.db is the owner's workspace registry, tens to
// hundreds of kilobytes, and a read happens a handful of times per realtest, so
// the copy costs milliseconds against a harness whose acts are measured in
// seconds. Correctness wins the trade outright.
//
// A snapshot that cannot be taken is returned as an error and NEVER as an empty
// result: a caller that saw zero rows here would assert against zero workspaces
// and report nonsense about the owner's editor. There is no fall back to the
// live file.
func queryStateDB(ctx context.Context, dbPath, query string) ([][]string, error) {
	var lastErr error
	for attempt := 1; attempt <= stateSnapshotAttempts; attempt++ {
		rows, retryable, err := queryStateDBOnce(ctx, dbPath, query)
		if err == nil {
			return rows, nil
		}
		lastErr = err
		if !retryable {
			return nil, err
		}
		if ctx.Err() != nil {
			return nil, fmt.Errorf("snapshot %s: %w (last attempt: %v)", dbPath, ctx.Err(), err)
		}
	}
	return nil, fmt.Errorf("snapshot %s: %d attempts all produced an unusable copy; last: %w",
		dbPath, stateSnapshotAttempts, lastErr)
}

// queryStateDBOnce is one snapshot-and-read. `retryable` marks the one failure
// worth taking a fresh copy for: a copy that did not pass its check, which a
// concurrent checkpoint can cause and a second copy usually fixes.
func queryStateDBOnce(ctx context.Context, dbPath, query string) (rows [][]string, retryable bool, err error) {
	snapPath, dir, err := takeStateSnapshot(dbPath)
	if dir != "" {
		defer os.RemoveAll(dir)
	}
	if err != nil {
		return nil, false, err
	}
	if err := checkStateSnapshot(ctx, snapPath); err != nil {
		return nil, true, err
	}
	out, err := runSQLite(ctx, snapPath, query)
	if err != nil {
		// Named by the owner's path rather than the snapshot's: the snapshot
		// is an implementation detail and is gone by the time anyone reads it.
		return nil, false, fmt.Errorf("read a snapshot of %s: %w", dbPath, err)
	}
	for _, line := range strings.Split(strings.TrimRight(out, "\n"), "\n") {
		if line == "" {
			continue
		}
		rows = append(rows, strings.Split(line, stateFieldSep))
	}
	return rows, false, nil
}

// ReadWorkspaces returns every workspace the state database holds.
//
// `closed` is carried through rather than filtered here: a closed workspace
// legitimately has no tab, so the caller needs the distinction to know what to
// assert, and a helper that quietly dropped them would make a missing tab
// indistinguishable from a closed workspace.
func ReadWorkspaces(ctx context.Context, dbPath string) (open []Workspace, closed []Workspace, err error) {
	rows, err := queryStateDB(ctx, dbPath, "SELECT id, dir, name, closed FROM workspaces ORDER BY id;")
	if err != nil {
		return nil, nil, fmt.Errorf("read the workspaces from %s: %w", dbPath, err)
	}

	for i, fields := range rows {
		if len(fields) != 4 {
			return nil, nil, fmt.Errorf("row %d of %s has %d fields, not 4: %q",
				i+1, dbPath, len(fields), strings.Join(fields, stateFieldSep))
		}
		ws := Workspace{ID: fields[0], Dir: fields[1], Name: fields[2]}
		switch fields[3] {
		case "0":
			open = append(open, ws)
		case "1":
			closed = append(closed, ws)
		default:
			return nil, nil, fmt.Errorf("workspace %s has an unreadable `closed` value %q", ws.ID, fields[3])
		}
	}
	return open, closed, nil
}
