// Package logging owns the parts of the agent-repl logging contract that every
// Go runtime must answer identically.
//
// The daemon, the store and the sidecar are separate modules with separate
// sinks, but their records are read together. Anything whose divergence would
// make those records incomparable belongs here rather than in each module.
package logging

import "time"

// TimestampLayout is the representation every agent-repl runtime writes into a
// record's timestamp field: RFC 3339 in the machine's local zone, on a 24-hour
// clock, with fixed-width microseconds and an explicit numeric offset.
//
// Local zone rather than UTC because an operator reads these logs on the
// machine that wrote them. Fixed-width fractional digits rather than Go's
// RFC3339Nano because RFC3339Nano drops trailing zeros, which makes a record
// landing on a whole second sort out of order against its neighbors.
//
// proto/vocab/log-timestamp.json holds this same answer for the TypeScript and
// elisp runtimes, and each language's tests assert against it.
const TimestampLayout = "2006-01-02T15:04:05.000000-07:00"

// Timestamp renders an instant in TimestampLayout. The instant is converted to
// the local zone first, so a caller holding a UTC time still produces a
// contract-conforming record.
func Timestamp(at time.Time) string { return at.Local().Format(TimestampLayout) }
