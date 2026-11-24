// Package ratewindow admits one record per interval and counts the ones it
// turns away, so a condition that repeats many times a second is recorded
// once per interval with the exact count of the records it stood for.
//
// Suppression hides the RECORD, never the fact: every turned-away record is
// counted, and the next admitted one carries the count.
package ratewindow

import "time"

// Window is one rate-limited record's state. Its zero value admits the first
// record at once. It is not safe for concurrent use: its owner holds the lock
// that guards the rest of the state the record reports.
type Window struct {
	// Every is the interval: one record is admitted per Every.
	Every time.Duration
	// openedAt is when the last admitted record was; zero before the first.
	openedAt time.Time
	// suppressed counts the records turned away since then.
	suppressed int
}

// Admit answers whether the record at NOW is written, and, when it is, how
// many records the window turned away since the last one it admitted. A
// turned-away record is counted and answers false.
func (w *Window) Admit(now time.Time) (admitted bool, suppressed int) {
	if !w.openedAt.IsZero() && now.Sub(w.openedAt) < w.Every {
		w.suppressed++
		return false, w.suppressed
	}
	suppressed = w.suppressed
	w.openedAt, w.suppressed = now, 0
	return true, suppressed
}
