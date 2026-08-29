package dlog

import "sync"

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
