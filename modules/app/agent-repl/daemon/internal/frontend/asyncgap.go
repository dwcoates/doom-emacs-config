// asyncgap.go carries the DAEMON-BUG classes of the async plane: the refusals a
// fold returns when the daemon has contradicted itself about a bubble.
//
// THEY ARE TYPED BECAUSE THE USER MUST BE TOLD. Every refusal here has the same
// user-visible consequence — a bubble that stops growing — which is exactly
// what a quiet agent looks like. A warn in the daemon log cannot tell the two
// apart for the person watching the screen, so these refusals carry their class
// and their bubble on the error VALUE, and the consumer turns them into failure
// cards addressed by (bubble, class) rather than riding them out.
package frontend

// AsyncGapKind names one daemon-bug class of async fold refusal. It is part of
// the card's ADDRESS — the uuid a consumer derives is per (bubble, kind) — so
// the strings are stable identifiers, not prose.
type AsyncGapKind string

const (
	// AsyncGapKindMismatch is an update whose arm does not match the bubble's
	// own kind: two sites in the daemon disagree about what the work IS.
	AsyncGapKindMismatch AsyncGapKind = "kind_mismatch"
	// AsyncGapSpoolRewind is a cumulative output restatement shorter than the
	// spool's own cursor: the source rewound under a fold that only appends.
	AsyncGapSpoolRewind AsyncGapKind = "spool_rewind"
	// AsyncGapJournalRewind is the same rewind on a workflow journal's cursor.
	AsyncGapJournalRewind AsyncGapKind = "journal_rewind"
)

// AsyncGapError is one classified daemon-bug refusal from an async fold.
//
// It is an ordinary error on the way out — every existing caller that only
// checks non-nil keeps working — and errors.As recovers the class for the one
// caller that has to say something to the user about it.
type AsyncGapError struct {
	// BubbleID is the bubble whose fold refused. Half of the card's address.
	BubbleID string
	// Gap is the class of refusal. The other half of the card's address, so
	// two different bugs on one bubble are two cards rather than one that
	// overwrites the other.
	Gap AsyncGapKind
	// Detail is the whole diagnostic sentence, naming both sides of the
	// disagreement. It is the log record AND the card's evidence, so the log
	// and the screen cannot say different things.
	Detail string
}

func (e *AsyncGapError) Error() string { return e.Detail }
