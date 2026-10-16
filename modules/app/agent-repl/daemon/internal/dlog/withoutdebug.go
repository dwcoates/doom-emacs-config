package dlog

// WithoutDebug answers a Logger that drops l's debug records and passes every
// other level through unchanged, With included.
//
// IT IS FOR A QUERY THAT RE-RUNS A DECISION WHOSE RECORD IS KEPT ELSEWHERE:
// the footer's arm for a workspace it has published nothing for is resolved
// again on every roster row's read, and its debug decisions were the same
// "default" branch every time (some 400 records per workspace switch,
// 2026-10-15), while the published arm is recorded when it changes. Its
// warnings and errors still reach the log.
func WithoutDebug(l Logger) Logger {
	return withoutDebug{l}
}

type withoutDebug struct{ base Logger }

func (withoutDebug) Debug(string, string, Context) {}

func (w withoutDebug) Info(operation, message string, ctx Context) {
	w.base.Info(operation, message, ctx)
}

func (w withoutDebug) Warn(operation, message string, ctx Context) {
	w.base.Warn(operation, message, ctx)
}

func (w withoutDebug) Error(operation, message string, ctx Context) {
	w.base.Error(operation, message, ctx)
}

func (w withoutDebug) With(ctx Context) Logger {
	return withoutDebug{w.base.With(ctx)}
}

// Enabled implements levelReporter: debug is never persisted through it, and
// every other level is as l would answer.
func (w withoutDebug) Enabled(level string) bool {
	return level != LevelDebug && Enabled(w.base, level)
}
