package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"sync"
	"sync/atomic"
	"testing"

	"modernc.org/sqlite"
)

// memoryPath is the path an in-memory handle carries in its records and
// errors. It is never opened as a file.
const memoryPath = ":memory:"

// memoryPragmas are the writing connection's pragmas minus the journal mode:
// an in-memory database has no WAL, and its pages never reach a disk to be
// synced.
const memoryPragmas = "&_pragma=busy_timeout(5000)&_txlock=immediate&_pragma=foreign_keys(1)"

// memoryNames numbers the in-memory databases this process opens, so each
// handle's name is its own.
var memoryNames atomic.Uint64

// memoryURI names a fresh in-memory database private to one handle: a memdb
// database every connection opening the same name shares, alive while one
// stays open (see memoryKeeper).
func memoryURI() string {
	return fmt.Sprintf("file:/agent-repl-wsm-%d?vfs=memdb", memoryNames.Add(1))
}

// memoryKeeper holds one connection to a handle's in-memory database for the
// handle's whole life, so the database outlives any connection the handle's
// pool replaces.
//
// THE POOL DOES REPLACE IT. A query whose context is cancelled interrupts its
// SQLite connection, and the driver reports an interrupted connection as not
// valid, so database/sql discards it instead of returning it to the pool. A
// private `:memory:` database lived on that one connection, so the next
// statement met a new, empty database ("no such table: merge_queue", the
// 2026-10-14 integration run). The named database the keeper holds is still
// there for the replacement to reopen.
type memoryKeeper struct {
	db   *sql.DB
	conn *sql.Conn
	// closed makes Close idempotent, as the handle's own *sql.DB Close is:
	// a store closed twice closes its keeper once.
	closed sync.Once
}

// keepMemoryDatabase opens and holds a connection to the database URI names.
func keepMemoryDatabase(ctx context.Context, uri string) (*memoryKeeper, error) {
	db, err := sql.Open("sqlite", uri)
	if err != nil {
		return nil, fmt.Errorf("wsm: open the in-memory database's keeper: %w", err)
	}
	conn, err := db.Conn(ctx)
	if err != nil {
		return nil, errors.Join(fmt.Errorf("wsm: hold the in-memory database's keeper connection: %w", err), db.Close())
	}
	return &memoryKeeper{db: db, conn: conn}, nil
}

// Close releases the keeper's connection, and with it the database once the
// handle's own connection is closed too. Only the first call closes anything;
// a later one answers nil, as *sql.DB's does.
func (k *memoryKeeper) Close() error {
	var err error
	k.closed.Do(func() { err = errors.Join(k.conn.Close(), k.db.Close()) })
	return err
}

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
// The handle holds ONE connection, as every handle does. Its database is a
// named memdb database that a keeper connection holds for the handle's life
// (memoryKeeper), so a connection the pool replaces reopens the same database.
func OpenInMemory(ctx context.Context, opts ...Option) (DB, error) {
	if !isTestBinary() {
		return nil, errNotATest
	}
	return openInMemory(ctx, opts)
}

// openInMemory is the in-memory open behind both proofs: OpenInMemory's test
// binary and OpenForTestRun's vendor-guarded seam.
func openInMemory(ctx context.Context, opts []Option) (DB, error) {
	if err := memoryTemplate(ctx); err != nil {
		return nil, err
	}
	uri := memoryURI()
	keeper, err := keepMemoryDatabase(ctx, uri)
	if err != nil {
		return nil, err
	}
	s, err := openStore(ctx, memoryPath, uri+memoryPragmas, false, opts)
	if err != nil {
		return nil, errors.Join(err, keeper.Close())
	}
	s.keeper = keeper
	db, err := s.finishWritingOpen(ctx, restoreTemplate)
	if err != nil {
		return nil, errors.Join(err, keeper.Close())
	}
	return db, nil
}

// templateURI names the template: a memdb database SHARED by every connection
// in this process that opens the same name, alive while one stays open.
const templateURI = "file:/agent-repl-wsm-schema-template?vfs=memdb"

var (
	templateOnce sync.Once
	// templateKeeper holds the template's database for the life of the
	// process; a memdb database is discarded when its last connection closes,
	// and templateHandle's pooled connection may be replaced (memoryKeeper).
	templateKeeper *memoryKeeper
	// templateHandle is the store the template was built through.
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
	keeper, err := keepMemoryDatabase(ctx, templateURI)
	if err != nil {
		return nil, err
	}
	s, err := openStore(ctx, templateURI, templateURI, false, nil)
	if err != nil {
		return nil, errors.Join(fmt.Errorf("wsm: open the in-memory schema template: %w", err), keeper.Close())
	}
	if err := s.createSchema(ctx); err != nil {
		return nil, errors.Join(fmt.Errorf("wsm: create the in-memory schema template: %w", err), s.db().Close(), keeper.Close())
	}
	templateKeeper = keeper
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
