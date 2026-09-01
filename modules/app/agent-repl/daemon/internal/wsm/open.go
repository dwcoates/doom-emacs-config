package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"net/url"
	"os"
	"path/filepath"
	"time"

	"claude-repld/internal/dlog"
)

// LayoutVersion is the schema version this build writes and is the ONLY
// version it opens. The file carries its own version in the layout table; a
// file stamped with anything else is refused rather than migrated, because the
// rebuild's store is recreated from scratch, never upgraded in place.
const LayoutVersion = 2

// Option configures an open. Options exist so the logger can be supplied
// without changing the two open functions' shape for callers that do not care.
type Option func(*store)

// WithLogger routes every operation's record to log instead of discarding it.
// Without it, an opened handle logs nowhere — which is what a test wants and
// what the daemon never does.
func WithLogger(log dlog.Logger) Option {
	return func(s *store) {
		if log != nil {
			s.log = log
		}
	}
}

// store is the one concrete DB. It owns a single *sql.DB with one connection,
// so every SELECT-then-write check in this package is race-free without a
// table lock and two writers can never lose an update on the same file.
type store struct {
	db       *sql.DB
	path     string
	readOnly bool
	log      dlog.Logger
}

// Open opens the workspace-state-manager database at path, creating the file
// and its schema when it does not exist. A file whose layout version is not
// exactly this build's REFUSES to open with a *LayoutError.
func Open(ctx context.Context, path string, opts ...Option) (DB, error) {
	if err := usablePath(path); err != nil {
		return nil, err
	}
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		return nil, fmt.Errorf("wsm: create db dir for %q: %w", path, err)
	}
	// modernc.org/sqlite reads PRAGMAs from _pragma query params. Each one is
	// here because its absence was a real bug: WAL for durable concurrent
	// reads, busy_timeout so a momentarily locked file waits instead of
	// erroring, _txlock=immediate so every transaction takes its write lock up
	// front (a deferred transaction that upgrades halfway through fails with
	// SQLITE_BUSY_SNAPSHOT rather than blocking, which is exactly how two
	// writers on one file lose an update), and foreign_keys so a workspace's
	// dependent rows cannot outlive it.
	dsn := path + "?_pragma=busy_timeout(5000)&_pragma=journal_mode(WAL)&_txlock=immediate&_pragma=foreign_keys(1)"
	s, err := openStore(ctx, path, dsn, false, opts)
	if err != nil {
		return nil, err
	}
	if err := s.ensureLayout(ctx); err != nil {
		s.db.Close()
		return nil, err
	}
	return s, nil
}

// OpenReadOnly opens an existing database for inspection. The handle is
// guaranteed to change nothing: mode=ro refuses to create a file, schema, WAL
// or any other residue, and query_only refuses every write at the engine.
func OpenReadOnly(ctx context.Context, path string, opts ...Option) (DB, error) {
	if err := usablePath(path); err != nil {
		return nil, err
	}
	if _, err := os.Stat(path); err != nil {
		return nil, fmt.Errorf("wsm: inspect read-only db path %q: %w", path, err)
	}
	dsn := (&url.URL{Scheme: "file", Path: path}).String() + "?mode=ro&_pragma=query_only(1)&_pragma=busy_timeout(5000)"
	s, err := openStore(ctx, path, dsn, true, opts)
	if err != nil {
		return nil, err
	}
	if err := s.checkLayout(ctx); err != nil {
		s.db.Close()
		return nil, err
	}
	return s, nil
}

// usablePath refuses the paths this store cannot be: it must be reopen-durable,
// so an empty path and an in-memory database are both errors.
func usablePath(path string) error {
	switch path {
	case "":
		return errors.New("wsm: empty db path")
	case ":memory:":
		return errors.New("wsm: in-memory db path is not allowed; the state store must be reopen-durable")
	}
	return nil
}

// openStore opens the handle and forces a real handshake, so a path that is not
// a database fails HERE rather than on the first unrelated query.
func openStore(ctx context.Context, path, dsn string, readOnly bool, opts []Option) (*store, error) {
	handle, err := sql.Open("sqlite", dsn)
	if err != nil {
		return nil, fmt.Errorf("wsm: open %q: %w", path, err)
	}
	// One connection: one writer, and append ordering that needs no extra lock.
	handle.SetMaxOpenConns(1)
	if err := handle.PingContext(ctx); err != nil {
		handle.Close()
		return nil, fmt.Errorf("wsm: open %q: %w", path, err)
	}
	s := &store{db: handle, path: path, readOnly: readOnly, log: discardLogger{}}
	for _, opt := range opts {
		opt(s)
	}
	return s, nil
}

// ensureLayout stamps a fresh file with this build's schema and refuses an
// existing file stamped with any other version.
func (s *store) ensureLayout(ctx context.Context) error {
	var count int
	err := s.db.QueryRowContext(ctx, `SELECT count(*) FROM sqlite_master WHERE type = 'table' AND name = 'layout'`).Scan(&count)
	if err != nil {
		return fmt.Errorf("wsm: probe layout table in %q: %w", s.path, err)
	}
	if count == 0 {
		if err := s.createSchema(ctx); err != nil {
			return err
		}
		s.log.Info("daemon.wsm.open", "created a fresh state database", dlog.Context{"path": s.path, "layout": LayoutVersion})
		return nil
	}
	return s.checkLayout(ctx)
}

// checkLayout reads the file's stamped version and refuses anything but an
// exact match.
func (s *store) checkLayout(ctx context.Context) error {
	var version int
	err := s.db.QueryRowContext(ctx, `SELECT version FROM layout WHERE id = 1`).Scan(&version)
	if errors.Is(err, sql.ErrNoRows) {
		refusal := &DecodeError{Table: "layout", Row: "1", Err: errors.New("no layout row")}
		s.log.Error("daemon.wsm.open", "state database carries no layout version", dlog.Context{"path": s.path, "error": refusal.Error()})
		return refusal
	}
	if err != nil {
		return fmt.Errorf("wsm: read layout version from %q: %w", s.path, err)
	}
	if version != LayoutVersion {
		refusal := &LayoutError{Path: s.path, File: version, Binary: LayoutVersion}
		s.log.Error("daemon.wsm.open", "refused a state database with a foreign layout version", dlog.Context{"path": s.path, "file_layout": version, "binary_layout": LayoutVersion})
		return refusal
	}
	return nil
}

// createSchema writes the whole schema and the layout stamp in ONE
// transaction, so a crash mid-create can never leave a half-schema file that
// the next open would read as a valid store.
func (s *store) createSchema(ctx context.Context) error {
	tx, err := s.db.BeginTx(ctx, nil)
	if err != nil {
		return fmt.Errorf("wsm: begin schema creation on %q: %w", s.path, err)
	}
	defer tx.Rollback()
	if _, err := tx.ExecContext(ctx, schemaDDL); err != nil {
		return fmt.Errorf("wsm: create schema in %q: %w", s.path, err)
	}
	if _, err := tx.ExecContext(ctx, `INSERT INTO layout (id, version) VALUES (1, ?)`, LayoutVersion); err != nil {
		return fmt.Errorf("wsm: stamp layout version in %q: %w", s.path, err)
	}
	if err := tx.Commit(); err != nil {
		return fmt.Errorf("wsm: commit schema creation on %q: %w", s.path, err)
	}
	return nil
}

// Close releases the handle.
func (s *store) Close() error { return s.db.Close() }

// ReadOnly reports whether this handle was opened read-only.
func (s *store) ReadOnly() bool { return s.readOnly }

// write runs fn inside one immediate transaction and logs the operation
// exactly once: DEBUG when it commits, ERROR when it refuses or fails.
// EVERY write in this package goes through it, so no mutation can escape the
// transaction discipline or the log.
func (s *store) write(ctx context.Context, op string, fields dlog.Context, fn func(context.Context, *sql.Tx) error) error {
	if s.readOnly {
		s.log.Error(op, "refused a write on a read-only handle", withError(fields, ErrReadOnly))
		return ErrReadOnly
	}
	tx, err := s.db.BeginTx(ctx, nil)
	if err != nil {
		wrapped := fmt.Errorf("wsm: begin transaction: %w", err)
		s.log.Error(op, "could not begin the transaction", withError(fields, wrapped))
		return wrapped
	}
	if err := fn(ctx, tx); err != nil {
		// The rollback is what makes a refusal mid-transaction leave nothing
		// behind; its own failure never masks the cause.
		_ = tx.Rollback()
		s.log.Error(op, "refused the write", withError(fields, err))
		return err
	}
	if err := tx.Commit(); err != nil {
		wrapped := fmt.Errorf("wsm: commit transaction: %w", err)
		s.log.Error(op, "could not commit the transaction", withError(fields, wrapped))
		return wrapped
	}
	s.log.Debug(op, "wrote durable state", fields)
	return nil
}

// read runs fn against the handle and logs a failed load at ERROR. Reads are
// all-or-nothing: fn returns the whole result or an error, never a partial one.
func (s *store) read(ctx context.Context, op string, fields dlog.Context, fn func(context.Context) error) error {
	if err := fn(ctx); err != nil {
		s.log.Error(op, "refused the read", withError(fields, err))
		return err
	}
	s.log.Debug(op, "read durable state", fields)
	return nil
}

// withError copies fields and stamps the error, so a caller's map is never
// mutated by the logging helper.
func withError(fields dlog.Context, err error) dlog.Context {
	out := make(dlog.Context, len(fields)+1)
	for k, v := range fields {
		out[k] = v
	}
	out["error"] = err.Error()
	return out
}

// discardLogger is the default sink: an opened handle logs nowhere until a
// WithLogger option supplies a real one.
type discardLogger struct{}

// Debug implements dlog.Logger.
func (discardLogger) Debug(string, string, dlog.Context) {}

// Info implements dlog.Logger.
func (discardLogger) Info(string, string, dlog.Context) {}

// Warn implements dlog.Logger.
func (discardLogger) Warn(string, string, dlog.Context) {}

// Error implements dlog.Logger.
func (discardLogger) Error(string, string, dlog.Context) {}

// With implements dlog.Logger.
func (d discardLogger) With(dlog.Context) dlog.Logger { return d }

// nanos renders an instant as the integer this store keeps time in.
func nanos(t time.Time) int64 { return t.UTC().UnixNano() }

// fromNanos rebuilds an instant from the stored integer.
func fromNanos(n int64) time.Time { return time.Unix(0, n).UTC() }

// nullNanos renders an optional instant, NULL when absent.
func nullNanos(t *time.Time) any {
	if t == nil {
		return nil
	}
	return nanos(*t)
}

// nullWorkspace renders an optional workspace reference, NULL when absent, so
// an unset parent is a NULL column rather than an empty-string sentinel.
func nullWorkspace(id *WorkspaceID) any {
	if id == nil {
		return nil
	}
	return string(*id)
}

// optTime rebuilds an optional instant from a nullable column.
func optTime(n sql.NullInt64) *time.Time {
	if !n.Valid {
		return nil
	}
	at := fromNanos(n.Int64)
	return &at
}

// interfaceCheck fails the build the moment the concrete store stops
// satisfying the seam, rather than at the first caller that assigns one.
var _ DB = (*store)(nil)
