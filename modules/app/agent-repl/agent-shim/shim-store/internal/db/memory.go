package db

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"io"
	"net/url"
	"sync"
	"sync/atomic"
	"testing"

	"agentrepl/shim-store/internal/logging"
	"modernc.org/sqlite"
)

// THE IN-MEMORY STORE IS A UNIT-TEST SEAM AND NOTHING ELSE (owner ruling,
// 2026-10-06): a full test run's hundreds of on-disk databases starved the
// owner's live store of disk bandwidth. It is unexported, so nothing outside
// this package can reach it, and it refuses outside a test binary
// (testing.Testing).
//
// WHAT IT IS. One memdb database per handle, SHARED by the handle's three
// pools (`file:/<name>?vfs=memdb` is one database to every connection in the
// process that opens the same name), so the write connection, the read pool
// and the checkpoint connection see one database exactly as they see one file.
//
// WHAT IT IS NOT. An in-memory database has no WAL: it runs SQLite's rollback
// journal in memory, so a reader and a committing writer take turns rather
// than overlap, and there is no -shm for the checkpoint job to read. A test
// whose subject is the WAL, the checkpoint, reader/writer overlap or the
// connections' pragmas opens a real file instead (newFileStore in the tests).
//
// THE SCHEMA IS BUILT ONCE PER TEST PROCESS, into a template database by the
// same openAt a file gets, and every handle copies the template's pages into
// its own database with SQLite's online-backup API before openAt runs on it.
// openAt then finds the schema current and builds nothing. Tests stay isolated:
// each handle's database has its own name and is discarded when its last
// connection closes.

// isTestBinary is testing.Testing, reached through a variable so the refusal
// can itself be tested from inside a test binary.
var isTestBinary = testingTesting

// testingTesting is the real answer, kept so a test can restore it.
var testingTesting = testing.Testing

// errNotATest is openInMemory's refusal outside a test binary.
var errNotATest = errors.New("shim-store db: an in-memory database is a unit-test seam, and this process is not a test binary")

// memorySeq names each in-memory database uniquely within the process.
var memorySeq atomic.Int64

// memoryTemplateName is the template database's memdb name.
const memoryTemplateName = "/shim-store-schema-template"

// memoryDSNs are the three pools' DSNs on the memdb database name: the file
// DSNs' pragmas minus everything that only means something for a file (the
// journal mode, the WAL's autocheckpoint and size limit, sync, the map).
func memoryDSNs(name string) poolDSNs {
	uri := func(pragmas []string, txlock bool) string {
		v := url.Values{"vfs": {"memdb"}, "_pragma": pragmas}
		if txlock {
			v.Set("_txlock", "immediate")
		}
		return "file:" + name + "?" + v.Encode()
	}
	return poolDSNs{
		write: uri([]string{"busy_timeout(5000)", "foreign_keys(ON)",
			fmt.Sprintf("cache_size(-%d)", WriteCacheKiB)}, true),
		read: uri([]string{"busy_timeout(5000)", "foreign_keys(ON)",
			fmt.Sprintf("cache_size(-%d)", ReadCacheKiB), "query_only(true)"}, false),
		checkpoint: uri([]string{"busy_timeout(5000)", "query_only(true)"}, false),
	}
}

// openInMemory is OpenWithOptions on a fresh in-memory database. The handle's
// path (in its records and errors) is its memdb name.
func openInMemory(log *logging.Logger, opts Options) (*DB, error) {
	if !isTestBinary() {
		return nil, errNotATest
	}
	if log == nil {
		panic("shim-store db: nil logger")
	}
	if err := memoryTemplate(); err != nil {
		return nil, err
	}
	name := fmt.Sprintf("/shim-store-%d", memorySeq.Add(1))
	dsns := memoryDSNs(name)
	// THE HOLDER keeps the database alive between the template's copy and
	// openAt's own write pool: a memdb database is discarded when its last
	// connection closes.
	holder, err := openPool(dsns.write, 1, fmt.Sprintf("the in-memory database %s", name))
	if err != nil {
		return nil, err
	}
	defer holder.Close() //nolint:errcheck // openAt's pools hold the database from here on
	if err := restoreTemplate(holder); err != nil {
		return nil, err
	}
	clock := opts.Now
	if clock == nil {
		clock = nowMillis
	}
	// NO finishOpen: it arms the writers' WAL reading, and there is no WAL.
	return openAt(dsns, name, log, opts, clock)
}

var (
	templateOnce sync.Once
	// templateHandle holds the template open for the life of the process.
	templateHandle *DB
	templateErr    error
)

// memoryTemplate builds the schema template on first use.
func memoryTemplate() error {
	templateOnce.Do(func() {
		log := logging.New(io.Discard, io.Discard, false)
		templateHandle, templateErr = openAt(memoryDSNs(memoryTemplateName), memoryTemplateName, log, Options{}, nowMillis)
		if templateErr != nil {
			templateErr = fmt.Errorf("shim-store db: build the in-memory schema template: %w", templateErr)
		}
	})
	return templateErr
}

// restoreTemplate copies the template's pages into the database behind pool.
// The template is only read.
func restoreTemplate(pool *sql.DB) error {
	conn, err := pool.Conn(context.Background())
	if err != nil {
		return storagef(err, "reaching the in-memory database to load the schema template")
	}
	defer conn.Close() //nolint:errcheck // returned to the pool; the pool's close reports
	err = conn.Raw(func(driverConn any) error {
		c, ok := driverConn.(interface {
			NewRestore(string) (*sqlite.Backup, error)
		})
		if !ok {
			return fmt.Errorf("the driver connection %T cannot restore a backup", driverConn)
		}
		backup, err := c.NewRestore(memoryDSNs(memoryTemplateName).read)
		if err != nil {
			return err
		}
		if _, err := backup.Step(-1); err != nil {
			return errors.Join(err, backup.Finish())
		}
		return backup.Finish()
	})
	if err != nil {
		return storagef(err, "loading the schema template into an in-memory database")
	}
	return nil
}
