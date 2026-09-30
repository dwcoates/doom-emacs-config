package dlog

import (
	"crypto/md5"
	"encoding/hex"
	"fmt"
	"path/filepath"
	"sync"
)

// Record is one captured record, for tests that assert the canonical log line
// of an error path.
type Record struct {
	Level     string
	Operation string
	Message   string
	Context   Context
}

// TestLogger is the no-op Logger every package's tests use in place of a real
// sink. It captures records so an error-path test can assert the canonical
// record and its context, and is safe for concurrent use.
type TestLogger struct {
	mu      sync.Mutex
	records []Record
	base    Context
}

// NewTestLogger returns a Logger that writes nowhere and remembers everything.
func NewTestLogger() *TestLogger { return &TestLogger{} }

// Debug implements Logger.
func (l *TestLogger) Debug(operation, message string, ctx Context) {
	l.append("debug", operation, message, ctx)
}

// Info implements Logger.
func (l *TestLogger) Info(operation, message string, ctx Context) {
	l.append("info", operation, message, ctx)
}

// Warn implements Logger.
func (l *TestLogger) Warn(operation, message string, ctx Context) {
	l.append("warn", operation, message, ctx)
}

// Error implements Logger.
func (l *TestLogger) Error(operation, message string, ctx Context) {
	l.append("error", operation, message, ctx)
}

// With implements Logger, returning a logger that shares this one's capture
// buffer and stamps the merged context onto every record.
func (l *TestLogger) With(ctx Context) Logger {
	return &derivedTestLogger{parent: l, base: merge(l.base, ctx)}
}

// Records returns a copy of everything captured so far.
func (l *TestLogger) Records() []Record {
	l.mu.Lock()
	defer l.mu.Unlock()
	out := make([]Record, len(l.records))
	copy(out, l.records)
	return out
}

func (l *TestLogger) append(level, operation, message string, ctx Context) {
	l.mu.Lock()
	defer l.mu.Unlock()
	l.records = append(l.records, Record{
		Level:     level,
		Operation: operation,
		Message:   message,
		Context:   merge(l.base, ctx),
	})
}

// derivedTestLogger is a TestLogger view with extra base context. A view
// TestSurfaces.Workspace handed out is bound to a workspace and tees its Warn
// and Error records exactly as the real workspace logger does.
type derivedTestLogger struct {
	parent *TestLogger
	base   Context
	// surfaces and workspaceID are set on a workspace-bound view only.
	surfaces    *TestSurfaces
	workspaceID string
}

func (l *derivedTestLogger) Debug(operation, message string, ctx Context) {
	l.parent.append("debug", operation, message, merge(l.base, ctx))
}

func (l *derivedTestLogger) Info(operation, message string, ctx Context) {
	l.parent.append("info", operation, message, merge(l.base, ctx))
}

func (l *derivedTestLogger) Warn(operation, message string, ctx Context) {
	l.parent.append("warn", operation, message, merge(l.base, ctx))
	l.tee(LevelWarn, operation, message)
}

func (l *derivedTestLogger) Error(operation, message string, ctx Context) {
	l.parent.append("error", operation, message, merge(l.base, ctx))
	l.tee(LevelError, operation, message)
}

func (l *derivedTestLogger) With(ctx Context) Logger {
	return &derivedTestLogger{parent: l.parent, base: merge(l.base, ctx), surfaces: l.surfaces, workspaceID: l.workspaceID}
}

// tee hands a workspace-bound view's record to the double's bound tee.
func (l *derivedTestLogger) tee(level, operation, message string) {
	if l.surfaces == nil || l.workspaceID == "" {
		return
	}
	l.surfaces.mu.Lock()
	tee := l.surfaces.tee
	l.surfaces.mu.Unlock()
	if tee == nil {
		return
	}
	tee.OnWorkspaceRecord(WorkspaceRecord{
		WorkspaceID: l.workspaceID,
		Level:       level,
		Operation:   operation,
		Message:     message,
	})
}

// merge builds a new Context from base overlaid with extra.
func merge(base, extra Context) Context {
	if len(base) == 0 && len(extra) == 0 {
		return nil
	}
	out := make(Context, len(base)+len(extra))
	for k, v := range base {
		out[k] = v
	}
	for k, v := range extra {
		out[k] = v
	}
	return out
}

// TestSurfaces is the Surfaces double for packages whose unit tests need one.
// It exists here rather than being re-hand-rolled per package so an addition
// to the Surfaces interface is absorbed in one place instead of breaking every
// package's private double.
//
// Every logger it hands out shares one capture buffer, so a test asserts the
// records of a workspace-bound component and a global one together, in order.
// Nothing is written to disk and no workspace directory has to exist.
type TestSurfaces struct {
	logger *TestLogger

	mu sync.Mutex
	// lookup is the bound minted-id lookup, when a test binds one. Unbound,
	// the double synthesizes a stable 16-character stand-in per directory:
	// a unit test of an unrelated component must not have to own a workspace
	// roster, and the SHAPE of the id is what its assertions can rely on.
	// The real surfaces refuse instead -- that refusal is asserted against
	// them, in this package.
	lookup WorkspaceIDLookup
	// clientRecords captures what ClientLog was asked to persist, keyed by
	// nothing: order is the assertion.
	clientRecords []ClientLogCall
	// evicted records every workspace directory Evict was called with.
	evicted []string
	// dirEvents records every DetachDir and AttachDir call, in order, as
	// "detach <dir>" and "attach <dir>".
	dirEvents []string
	// tee is the bound record tee, nil when none is bound.
	tee RecordTee
}

// ClientLogCall is one captured ClientLog call.
type ClientLogCall struct {
	Dir    string
	Record ClientRecord
}

// NewTestSurfaces returns Surfaces that write nowhere and remember everything.
func NewTestSurfaces() *TestSurfaces { return &TestSurfaces{logger: NewTestLogger()} }

// Global implements Surfaces.
func (s *TestSurfaces) Global() Logger { return s.logger }

// Workspace implements Surfaces. It never fails: a test that wants the
// resolution failure asserts it against the real surfaces.
func (s *TestSurfaces) Workspace(dir string) (Logger, error) {
	id, err := s.workspaceID(dir)
	if err != nil {
		return nil, err
	}
	hash, err := WorkspaceDirHash(dir)
	if err != nil {
		return nil, err
	}
	return &derivedTestLogger{
		parent: s.logger,
		base: merge(s.logger.base, Context{
			KeyWorkspaceDir:     dir,
			KeyWorkspaceID:      id,
			KeyWorkspaceDirHash: hash,
		}),
		surfaces:    s,
		workspaceID: id,
	}, nil
}

// BindRecordTee implements Surfaces: a workspace-bound logger this double
// handed out tees its Warn and Error records to it, as the real one does.
func (s *TestSurfaces) BindRecordTee(tee RecordTee) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.tee = tee
}

// WorkspaceOrCentral implements Surfaces with the production semantics: the
// workspace's logger when it resolves, and otherwise the global logger with
// the workspace named on every record.
func (s *TestSurfaces) WorkspaceOrCentral(dir string) Logger {
	log, err := s.Workspace(dir)
	if err == nil {
		return log
	}
	return s.logger.With(Context{
		KeyWorkspaceDir:        dir,
		KeyUnroutableWorkspace: dir,
	})
}

// BindWorkspaceIDs implements Surfaces.
func (s *TestSurfaces) BindWorkspaceIDs(lookup WorkspaceIDLookup) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.lookup = lookup
}

// workspaceID answers the bound lookup's id, or the synthetic stand-in.
func (s *TestSurfaces) workspaceID(dir string) (string, error) {
	s.mu.Lock()
	lookup := s.lookup
	s.mu.Unlock()
	if lookup != nil {
		return lookup(dir)
	}
	return syntheticWorkspaceID(dir)
}

// ShimSink implements Surfaces. No test process should inherit a fake
// descriptor, so this refuses rather than inventing one.
func (s *TestSurfaces) ShimSink(dir string) (Borrowed, error) {
	return nil, fmt.Errorf("TestSurfaces has no shim sink to borrow for %q", dir)
}

// ShimRollRequests implements Surfaces. Test surfaces never own a real shim
// target, so no hard-ceiling request can be emitted.
func (s *TestSurfaces) ShimRollRequests() <-chan ShimRollRequest { return nil }

// ClientLog implements Surfaces by capturing the call.
func (s *TestSurfaces) ClientLog(dir string, record ClientRecord) error {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.clientRecords = append(s.clientRecords, ClientLogCall{Dir: dir, Record: record})
	return nil
}

// Evict implements Surfaces by capturing the call.
func (s *TestSurfaces) Evict(dir string) error {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.evicted = append(s.evicted, dir)
	return nil
}

// DetachDir implements Surfaces by capturing the call.
func (s *TestSurfaces) DetachDir(dir string) error {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.dirEvents = append(s.dirEvents, "detach "+dir)
	return nil
}

// AttachDir implements Surfaces by capturing the call.
func (s *TestSurfaces) AttachDir(dir string) error {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.dirEvents = append(s.dirEvents, "attach "+dir)
	return nil
}

// DirEvents returns every DetachDir and AttachDir call, in order.
func (s *TestSurfaces) DirEvents() []string {
	s.mu.Lock()
	defer s.mu.Unlock()
	out := make([]string, len(s.dirEvents))
	copy(out, s.dirEvents)
	return out
}

// Close implements Surfaces.
func (s *TestSurfaces) Close() error { return nil }

// Records returns every record captured through any logger this handed out.
func (s *TestSurfaces) Records() []Record { return s.logger.Records() }

// ClientRecords returns every ClientLog call, in order.
func (s *TestSurfaces) ClientRecords() []ClientLogCall {
	s.mu.Lock()
	defer s.mu.Unlock()
	out := make([]ClientLogCall, len(s.clientRecords))
	copy(out, s.clientRecords)
	return out
}

// Evicted returns every workspace directory Evict was called with, in order.
func (s *TestSurfaces) Evicted() []string {
	s.mu.Lock()
	defer s.mu.Unlock()
	out := make([]string, len(s.evicted))
	copy(out, s.evicted)
	return out
}

// syntheticWorkspaceID is the TEST DOUBLE's stand-in for a minted workspace
// id: 16 hex characters, the width wsm mints, derived from the directory so a
// double's records for one directory agree with each other. Production never
// reaches it -- the real surfaces refuse an unresolved workspace instead.
func syntheticWorkspaceID(dir string) (string, error) {
	abs, err := filepath.Abs(dir)
	if err != nil {
		return "", fmt.Errorf("resolve workspace dir %q for a synthetic test id: %w", dir, err)
	}
	sum := md5.Sum([]byte(filepath.Clean(abs)))
	return hex.EncodeToString(sum[:])[:16], nil
}
