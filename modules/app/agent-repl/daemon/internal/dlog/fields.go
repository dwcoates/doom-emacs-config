package dlog

// The two field builders every package's records share. A record's context is
// never a map another record holds, so each answers a fresh copy and leaves
// the caller's map as it was; daemon/fields_shape_test.go fails on a package
// that hand-rolls either.

// Merge answers a copy of BASE overlaid with EXTRA: a key in both takes
// EXTRA's value. Either may be nil.
func Merge(base, extra Context) Context {
	out := make(Context, len(base)+len(extra)+1)
	for k, v := range base {
		out[k] = v
	}
	for k, v := range extra {
		out[k] = v
	}
	return out
}

// WithCause answers a copy of FIELDS with ERR's text under "cause", the key
// every failure record names its error by. ERR is never nil: a record of a
// failure always has one.
func WithCause(fields Context, err error) Context {
	return Merge(fields, Context{"cause": err.Error()})
}

// TransientKey is the context key that marks a record TRANSIENT: a failure of
// something outside agent-repl that passes on its own (a network blip, a
// third-party endpoint's error), never a defect of agent-repl's. The deploy
// checks leave every record so marked out of their window
// (merge.ParseLogLines), so a passing outage never holds a merge queue.
const TransientKey = "transient"

// AsTransient answers a copy of FIELDS marked transient (TransientKey).
func AsTransient(fields Context) Context {
	return Merge(fields, Context{TransientKey: true})
}
