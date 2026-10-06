package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"sync"
	"testing"

	"modernc.org/sqlite"
)

// memoryPath is the path an in-memory handle carries in its records and
// errors. It is never opened as a file.
const memoryPath = ":memory:"

// memoryDSN is a private in-memory database on the handle's one connection,
// with the writing connection's pragmas minus the journal mode: an in-memory
// database has no WAL, and its pages never reach a disk to be synced.
const memoryDSN = "file::memory:?_pragma=busy_timeout(5000)&_txlock=immediate&_pragma=foreign_keys(1)"

// isTestBinary is testing.Testing, reached through a variable so the refusal
// below can itself be tested from inside a test binary.
var isTestBinary = testingTesting

// testingTesting is the real answer, kept so a test can restore it.
var testingTesting = testing.Testing

// errNotATest is OpenInMemory's refusal outside a test binary.
var errNotATest = errors.New("wsm: an in-memory state database is a test seam; the state store must be reopen-durable, and this process is not a test binary")

// OpenInMemory opens a fresh state database that lives in memory, for a UNIT
// TEST whose subject is not the file: what a test writes there is gone when the
// handle closes, and nothing touches the disk (owner ruling, 2026-10-06).
//
// IT REFUSES OUTSIDE A TEST BINARY (testing.Testing), so no daemon can be
// handed a store that forgets everything on restart.
//
// IT IS NOT A SECOND SCHEMA PATH. The schema is created ONCE per test process,
// by the same createSchema a real file gets, into an in-memory template; every
// handle copies those pages into its own private database with SQLite's
// online-backup API, then runs the exact tail of Open: layout check,
// repository invariant, directory reconciliation. So tests stay isolated from
// each other, and no test pays for building the schema.
//
// WHY THE BACKUP API AND NOT sqlite3_deserialize: modernc.org/sqlite v1.46.1's
// Deserialize hands SQLite a buffer from its per-call TLS allocator together
// with SQLITE_DESERIALIZE_FREEONCLOSE, so closing the connection frees memory
// SQLite never allocated, and the test binary dies with SIGSEGV.
//
// The handle holds ONE connection, as every handle does. Its database lives on
// that connection, so if the pool ever replaced it the next statement would
// meet an empty database and fail loudly ("no such table"), never silently.
func OpenInMemory(ctx context.Context, opts ...Option) (DB, error) {
	if !isTestBinary() {
		return nil, errNotATest
	}
	if err := memoryTemplate(ctx); err != nil {
		return nil, err
	}
	s, err := openStore(ctx, memoryPath, memoryDSN, false, opts)
	if err != nil {
		return nil, err
	}
	return s.finishWritingOpen(ctx, restoreTemplate)
}

// templateURI names the template: a memdb database SHARED by every connection
// in this process that opens the same name, alive while one stays open.
const templateURI = "file:/agent-repl-wsm-schema-template?vfs=memdb"

var (
	templateOnce sync.Once
	// templateHandle holds the template's one connection for the life of the
	// process; a memdb database is discarded when its last connection closes.
	templateHandle *sql.DB
	templateErr    error
)

// memoryTemplate builds the template on first use; every in-memory handle
// after the first copies it as it stands.
func memoryTemplate(ctx context.Context) error {
	templateOnce.Do(func() {
		templateHandle, templateErr = buildTemplate(ctx)
	})
	return templateErr
}

func buildTemplate(ctx context.Context) (*sql.DB, error) {
	s, err := openStore(ctx, templateURI, templateURI, false, nil)
	if err != nil {
		return nil, fmt.Errorf("wsm: open the in-memory schema template: %w", err)
	}
	if err := s.createSchema(ctx); err != nil {
		s.db().Close()
		return nil, fmt.Errorf("wsm: create the in-memory schema template: %w", err)
	}
	return s.db(), nil
}

// restoreTemplate copies the template's pages into the handle's own (empty)
// in-memory database. The template is only read.
func restoreTemplate(ctx context.Context, handle *sql.DB) error {
	conn, err := handle.Conn(ctx)
	if err != nil {
		return fmt.Errorf("wsm: reach the in-memory database to load the schema template: %w", err)
	}
	defer conn.Close()
	err = conn.Raw(func(driverConn any) error {
		c, ok := driverConn.(restorer)
		if !ok {
			return fmt.Errorf("the driver connection %T cannot restore a backup", driverConn)
		}
		backup, err := c.NewRestore(templateURI)
		if err != nil {
			return err
		}
		if _, err := backup.Step(-1); err != nil {
			return errors.Join(err, backup.Finish())
		}
		return backup.Finish()
	})
	if err != nil {
		return fmt.Errorf("wsm: load the schema template into an in-memory database: %w", err)
	}
	return nil
}

// restorer is the part of modernc.org/sqlite's connection this file uses.
type restorer interface {
	NewRestore(srcURI string) (*sqlite.Backup, error)
}
