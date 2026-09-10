package dlog

import (
	"fmt"
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

// derivedTestLogger is a TestLogger view with extra base context.
type derivedTestLogger struct {
	parent *TestLogger
	base   Context
}

func (l *derivedTestLogger) Debug(operation, message string, ctx Context) {
	l.parent.append("debug", operation, message, merge(l.base, ctx))
}

func (l *derivedTestLogger) Info(operation, message string, ctx Context) {
	l.parent.append("info", operation, message, merge(l.base, ctx))
}

func (l *derivedTestLogger) Warn(operation, message string, ctx Context) {
	l.parent.append("warn", operation, message, merge(l.base, ctx))
}

func (l *derivedTestLogger) Error(operation, message string, ctx Context) {
	l.parent.append("error", operation, message, merge(l.base, ctx))
}

func (l *derivedTestLogger) With(ctx Context) Logger {
	return &derivedTestLogger{parent: l.parent, base: merge(l.base, ctx)}
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
	// clientRecords captures what ClientLog was asked to persist, keyed by
	// nothing: order is the assertion.
	clientRecords []ClientLogCall
	// evicted records every workspace directory Evict was called with.
	evicted []string
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
	id, err := LogWorkspaceID(dir)
	if err != nil {
		return nil, err
	}
	return s.logger.With(Context{KeyWorkspaceDir: dir, KeyWorkspaceID: id}), nil
}

// ShimSink implements Surfaces. No test process should inherit a fake
// descriptor, so this refuses rather than inventing one.
func (s *TestSurfaces) ShimSink(dir string) (Borrowed, error) {
	return nil, fmt.Errorf("TestSurfaces has no shim sink to borrow for %q", dir)
}

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
