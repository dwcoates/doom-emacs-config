package footer

import "time"

// Clock is the resolver's whole dependency on time. It exists because the R1
// dwell — the one-shot successor push that retires a momentary status — is a
// DAEMON-SIDE timer, and a test that waited on the real one would be waiting
// on wall-clock rather than on a fact.
type Clock interface {
	// Now is the current instant, which every `at` stamp and every clock cell
	// is taken from.
	Now() time.Time
	// AfterFunc runs f once, after d has passed. The returned Timer cancels a
	// dwell whose status was superseded before it elapsed.
	AfterFunc(d time.Duration, f func()) Timer
}

// Timer is one scheduled dwell.
type Timer interface {
	// Stop cancels the dwell, reporting whether it had not yet fired.
	Stop() bool
}

// SystemClock is the production Clock: the real one.
type SystemClock struct{}

// Now is time.Now.
func (SystemClock) Now() time.Time { return time.Now() }

// AfterFunc is time.AfterFunc.
func (SystemClock) AfterFunc(d time.Duration, f func()) Timer { return time.AfterFunc(d, f) }
