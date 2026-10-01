package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"net/url"
	"os"
	"path/filepath"
	"sync"
	"time"

	"claude-repld/internal/dirpath"
	"claude-repld/internal/dlog"
)

// LayoutVersion is the schema version this build writes. The file carries its
// own version in the layout table; a file stamped OLDER is carried forward by
// the ordered migration list in migrate.go, because the workspace state is the
// user's data and is never thrown away over an additive schema change. Only a
// layout this build genuinely cannot interpret is refused — see LayoutError.
//
// A NEW VERSION IS A NEW ENTRY IN `migrations`. Bumping this constant alone
// makes the daemon refuse every database the previous build wrote.
const LayoutVersion = 17

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
	// mu guards the handle and the read-only flag across a PROMOTION, which
	// swaps both. Every other field is set once at open.
	mu       sync.RWMutex
	handle   *sql.DB
	path     string
	readOnly bool
	log      dlog.Logger
	// canonicalDir is dirpath.Canonical, the spelling the open's directory
	// reconciliation compares each row against (dirspelling.go). Injectable
	// so a test can model a case-folding volume on any host.
	canonicalDir func(string) (string, error)

	// leaseMu guards owned.
	leaseMu sync.Mutex
	// owned is every lease THIS HANDLE acquired and has not released. One
	// daemon process holds exactly one handle, so this set is exactly the
	// leases whose owning process is alive: see ForeignLeases and Close.
	owned map[LeaseID]Lease
}

// Open opens the workspace-state-manager database at path, creating the file
// and its schema when it does not exist. A file stamped with an OLDER layout
// is migrated forward in place; one this build cannot interpret refuses to
// open with a *LayoutError, and a migration that fails with a
// *MigrationError.
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
		s.handle.Close()
		return nil, err
	}
	// THE BOOT-TIME INVARIANT CHECK. The schema and the foreign_keys pragma
	// above make a workspace whose repository is unregistered impossible to
	// WRITE; this reports one that was already there, which only a handle
	// opened without the pragma can have left. See repoinvariant.go.
	if err := s.checkRepositoryInvariant(ctx); err != nil {
		s.handle.Close()
		return nil, err
	}
	// THE BOOT-TIME DIRECTORY RECONCILIATION: every row keyed by its
	// directory's on-disk spelling, or said loudly why not. See dirspelling.go.
	if err := s.reconcileDirSpellings(ctx); err != nil {
		s.handle.Close()
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
		s.handle.Close()
		return nil, err
	}
	return s, nil
}

// OpenJoining is a JOINING daemon's open. The incumbent is still the sole
// writer of every row, so the handle it answers is read-only (OpenReadOnly);
// but a file stamped older than this build is first carried forward by its
// ADDITIVE steps alone, through a writing handle that exists only for those
// steps. An additive step leaves the incumbent's statements working (see
// MigrationKind), so the incumbent keeps serving across it, and the
// successor then reads a file it can interpret. A chain with any BREAKING
// step is refused with a *LayoutError and changes nothing: the deploy restarts
// across it instead of handing over.
func OpenJoining(ctx context.Context, path string, opts ...Option) (DB, error) {
	if err := usablePath(path); err != nil {
		return nil, err
	}
	if _, err := os.Stat(path); err != nil {
		return nil, fmt.Errorf("wsm: inspect the joining db path %q: %w", path, err)
	}
	if err := migrateAdditiveWhileJoining(ctx, path, opts); err != nil {
		return nil, err
	}
	return OpenReadOnly(ctx, path, opts...)
}

// migrateAdditiveWhileJoining applies the additive steps that carry an older
// file to this build's layout, and nothing else: no schema is created, no row
// is reconciled, and the writing handle is closed before the answer.
func migrateAdditiveWhileJoining(ctx context.Context, path string, opts []Option) error {
	dsn := path + "?_pragma=busy_timeout(5000)&_pragma=journal_mode(WAL)&_txlock=immediate&_pragma=foreign_keys(1)"
	s, err := openStore(ctx, path, dsn, false, opts)
	if err != nil {
		return err
	}
	defer s.handle.Close()
	version, err := s.layoutVersion(ctx)
	if err != nil {
		return err
	}
	switch {
	case version == LayoutVersion:
		return nil
	case version > LayoutVersion:
		return s.refuseLayout(version, "a downgrade is not a migration")
	}
	plan, ok := planMigrations(version)
	if !ok {
		return s.refuseLayout(version, "no migration in this build leads from it to this build's layout")
	}
	if planKind(plan) != MigrationAdditive {
		return s.refuseLayout(version, "a breaking migration cannot run while the incumbent still writes it; the deploy restarts across it")
	}
	return s.migrateForward(ctx, version)
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
	s := &store{handle: handle, path: path, readOnly: readOnly, log: discardLogger{}, canonicalDir: dirpath.Canonical}
	for _, opt := range opts {
		opt(s)
	}
	return s, nil
}

// ensureLayout stamps a fresh file with this build's schema, and carries an
// existing file forward to it.
func (s *store) ensureLayout(ctx context.Context) error {
	var count int
	err := s.handle.QueryRowContext(ctx, `SELECT count(*) FROM sqlite_master WHERE type = 'table' AND name = 'layout'`).Scan(&count)
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
	version, err := s.layoutVersion(ctx)
	if err != nil {
		return err
	}
	switch {
	case version == LayoutVersion:
		return nil
	case version > LayoutVersion:
		return s.refuseLayout(version, "a downgrade is not a migration")
	default:
		return s.migrateForward(ctx, version)
	}
}

// checkLayout is the READ-ONLY open's layout gate. It refuses every mismatch,
// including an older file it could otherwise migrate: this handle is
// guaranteed to change nothing, and migrating is a change. The joining daemon
// that opens read-only does so alongside an incumbent that has already
// migrated the file, so an older layout here means there is no writer to carry
// it forward.
func (s *store) checkLayout(ctx context.Context) error {
	version, err := s.layoutVersion(ctx)
	if err != nil {
		return err
	}
	if version == LayoutVersion {
		return nil
	}
	if version > LayoutVersion {
		return s.refuseLayout(version, "a downgrade is not a migration")
	}
	return s.refuseLayout(version, "a read-only handle cannot migrate it forward")
}

// layoutVersion reads the file's stamped version. A file carrying no layout
// row is undecodable, not merely foreign.
func (s *store) layoutVersion(ctx context.Context) (int, error) {
	var version int
	err := s.handle.QueryRowContext(ctx, `SELECT version FROM layout WHERE id = 1`).Scan(&version)
	if errors.Is(err, sql.ErrNoRows) {
		refusal := &DecodeError{Table: "layout", Row: "1", Err: errors.New("no layout row")}
		s.log.Error("daemon.wsm.open", "state database carries no layout version", dlog.Context{"path": s.path, "error": refusal.Error()})
		return 0, refusal
	}
	if err != nil {
		return 0, fmt.Errorf("wsm: read layout version from %q: %w", s.path, err)
	}
	return version, nil
}

// refuseLayout builds and records the refusal of a layout this build cannot
// interpret. It changes nothing on disk: the file is left exactly as found.
func (s *store) refuseLayout(version int, reason string) error {
	refusal := &LayoutError{Path: s.path, File: version, Binary: LayoutVersion, Reason: reason}
	s.log.Error("daemon.wsm.open", "refused a state database with a foreign layout version", dlog.Context{
		"path": s.path, "file_layout": version, "binary_layout": LayoutVersion, "error": refusal.Error(),
	})
	return refusal
}

// createSchema writes the whole schema and the layout stamp in ONE
// transaction, so a crash mid-create can never leave a half-schema file that
// the next open would read as a valid store.
func (s *store) createSchema(ctx context.Context) error {
	tx, err := s.handle.BeginTx(ctx, nil)
	if err != nil {
		return fmt.Errorf("wsm: begin schema creation on %q: %w", s.path, err)
	}
	defer s.endTx(tx, "daemon.wsm.open", dlog.Context{"path": s.path})
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

// Close releases every lease this handle still owns, then the handle.
//
// A LEASE DOES NOT OUTLIVE THE PROCESS THAT TOOK IT. The handle is the
// process's one writer, so its close is the end of every operation that
// could still be holding a lease: a relaunch, a drain hold, a handover's
// quiesce hold still waiting on a holdout. Left behind, such a lease is read
// by every later daemon as a live hold -- the composer's `restarting` arm
// refused every prompt of five workspaces for hours after the 2026-09-27
// handover (see ForeignLeases for the boot's half).
//
// A MERGE LEASE IS KEPT: it is the merge ledger's durable identity, and the
// next boot's merge recovery is what resolves it.
//
// A lease another process already released -- the successor that adopted a
// handed-over workspace drains its quiesce hold -- is simply gone, and is
// recorded at DEBUG. Every failure is returned, joined with the close's own.
func (s *store) Close() error {
	released := s.releaseOwnedLeases(context.Background())
	return errors.Join(released, s.db().Close())
}

// db answers the handle in force. It is a method because a PROMOTION swaps it
// under every reader.
func (s *store) db() *sql.DB {
	s.mu.RLock()
	defer s.mu.RUnlock()
	return s.handle
}

// ReadOnly reports whether this handle is read-only right now.
func (s *store) ReadOnly() bool {
	s.mu.RLock()
	defer s.mu.RUnlock()
	return s.readOnly
}

// Promote turns a READ-ONLY handle into a writing one, IN PLACE.
//
// It exists for the handover's successor, which opens read-only because the
// incumbent is still the sole writer, and becomes a writer the moment it adopts
// its first workspace. The one-writer invariant holds across the swap because
// WSM contention is WORKSPACE-SCOPED: the incumbent stops writing a
// workspace's rows at that workspace's transfer notice, which is what the
// adoption answers.
//
// Promoting an already-writing handle is success: the rollout calls it at the
// first adoption and has no reason to remember whether that already happened.
func (s *store) Promote(ctx context.Context) error {
	const op = "daemon.wsm.promote"
	s.mu.Lock()
	defer s.mu.Unlock()
	if !s.readOnly {
		s.log.Debug(op, "the handle already writes; nothing to promote", dlog.Context{"path": s.path})
		return nil
	}
	dsn := s.path + "?_pragma=busy_timeout(5000)&_pragma=journal_mode(WAL)&_txlock=immediate&_pragma=foreign_keys(1)"
	handle, err := sql.Open("sqlite", dsn)
	if err != nil {
		s.log.Error(op, "the writing handle could not be opened", dlog.Context{"path": s.path, "error": err.Error()})
		return fmt.Errorf("wsm: promote %q: %w", s.path, err)
	}
	handle.SetMaxOpenConns(1)
	if err := handle.PingContext(ctx); err != nil {
		handle.Close()
		s.log.Error(op, "the writing handle could not be reached", dlog.Context{"path": s.path, "error": err.Error()})
		return fmt.Errorf("wsm: promote %q: %w", s.path, err)
	}
	previous := s.handle
	s.handle, s.readOnly = handle, false
	if err := previous.Close(); err != nil {
		// The writing handle is already in place; a stubborn read-only handle
		// is reported and nothing is rolled back.
		s.log.Warn(op, "the retired read-only handle would not close", dlog.Context{
			"path": s.path, "error": err.Error(),
		})
	}
	s.log.Info(op, "promoted the state handle to writing", dlog.Context{"path": s.path})
	return nil
}

// write runs fn inside one immediate transaction and logs the operation
// exactly once: DEBUG when it commits, ERROR when it refuses or fails.
// EVERY write in this package goes through it, so no mutation can escape the
// transaction discipline or the log.
func (s *store) write(ctx context.Context, op string, fields dlog.Context, fn func(context.Context, *sql.Tx) error) error {
	if s.readOnly {
		s.log.Error(op, "refused a write on a read-only handle", withError(fields, ErrReadOnly))
		return ErrReadOnly
	}
	tx, err := s.handle.BeginTx(ctx, nil)
	if err != nil {
		wrapped := fmt.Errorf("wsm: begin transaction: %w", err)
		s.log.Error(op, "could not begin the transaction", withError(fields, wrapped))
		return wrapped
	}
	if err := fn(ctx, tx); err != nil {
		// The rollback is what makes a refusal mid-transaction leave nothing
		// behind; its own failure is recorded by endTx and never masks the
		// cause, which is still what this write returns.
		s.endTx(tx, op, fields)
		// A WRITE ABOUT A RECORD THAT IS NOT THERE IS THE READ SIDE'S SHAPE,
		// and it gets the read side's level. `read` has always answered
		// ErrNotFound at DEBUG; a write that names a row the caller no longer
		// owns -- a fault about a workspace that has since been forgotten --
		// is the same statement of fact, and reporting it at ERROR made a
		// single forgotten workspace cost three ERRORs per shim death.
		if errors.Is(err, ErrNotFound) {
			s.log.Debug(op, "the write named no such record", withError(fields, err))
			return err
		}
		// A LEASE ALREADY HELD IS THE ARBITRATION ANSWERING, not the store
		// failing: the typed refusal names the standing holder, and every
		// caller (the drain, the queue, the merge) decides what it means and
		// states that at its own level. Recording it here at ERROR put an
		// ERROR beside every ordinary hold and every drain.
		var held *LeaseHeldError
		if errors.As(err, &held) {
			s.log.Debug(op, "the lease is already held; the arbitration refused the acquisition", withError(fields, err))
			return err
		}
		// A MERGE HOLD AFTER ITS MERGE'S RELEASE is the same arbitration
		// answering: the release won the race, and the caller takes the path
		// of a workspace with no merge and states that at its own level.
		if errors.Is(err, ErrMergeLeaseGone) {
			s.log.Debug(op, "the merge lease is gone; the merge hold was refused", withError(fields, err))
			return err
		}
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
//
// A LOOKUP THAT FINDS NOTHING IS AN ANSWER, not a failure. ErrNotFound is what
// every per-workspace rpc's unknown-workspace refusal is built from, so
// recording it at ERROR would put an error line on an ordinary refusal path
// and drown the reads that really did break. The error is returned unchanged
// either way — only the level the record carries differs.
//
// A CANCELLED CALLER IS NOT A BROKEN READ either. Every read here runs under
// its caller's context, and a standing stream's context is cancelled the
// moment the client leaves or the daemon exits in an orderly way, so recording
// that at ERROR would put an error line on every shutdown. It is recorded at
// INFO instead; the error is still returned unchanged.
func (s *store) read(ctx context.Context, op string, fields dlog.Context, fn func(context.Context) error) error {
	if err := fn(ctx); err != nil {
		if errors.Is(err, ErrNotFound) {
			s.log.Debug(op, "the read found no such record", withError(fields, err))
			return err
		}
		if errors.Is(err, context.Canceled) || errors.Is(err, context.DeadlineExceeded) {
			s.log.Info(op, "the read ended when its caller's context was cancelled",
				withError(fields, err))
			return err
		}
		s.log.Error(op, "refused the read", withError(fields, err))
		return err
	}
	s.log.Debug(op, "read durable state", fields)
	return nil
}

// endTx is the one way a transaction in this package ends without a Commit:
// it rolls tx back and records a failed rollback at ERROR.
//
// A FAILED ROLLBACK OUTLIVES ITS CALL. It leaves the connection inside the
// transaction, still holding its lock or snapshot, and every site used to
// discard the error, so nothing would have said why the next write waited or
// the WAL stopped folding. ErrTxDone is not a failure: it is what Rollback
// answers after a successful Commit.
func (s *store) endTx(tx *sql.Tx, op string, fields dlog.Context) {
	err := tx.Rollback()
	if err == nil || errors.Is(err, sql.ErrTxDone) {
		return
	}
	s.log.Error(op, "could not roll the transaction back, so its connection may still be inside it",
		withError(fields, fmt.Errorf("wsm: roll back transaction: %w", err)))
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
