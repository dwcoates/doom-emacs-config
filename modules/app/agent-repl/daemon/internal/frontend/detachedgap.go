// detachedgap.go carries the DAEMON-BUG classes of the detached-work plane: the
// refusals a fold returns when the daemon has contradicted itself about a
// detached-work message.
//
// THEY ARE TYPED BECAUSE THE USER MUST BE TOLD. Every refusal here has the same
// user-visible consequence — work that stops growing — which is exactly what a
// quiet agent looks like. A warn in the daemon log cannot tell the two apart for
// the person watching the screen, so these refusals carry their class and their
// message id on the error VALUE, and the consumer turns them into failure cards
// addressed by (message, class) rather than riding them out.
package frontend

// DetachedGapKind names one daemon-bug class of detached-work fold refusal. It
// is part of the card's ADDRESS — the uuid a consumer derives is per (message,
// kind) — so the strings are stable identifiers, not prose.
type DetachedGapKind string

const (
	// DetachedGapKindMismatch is an update whose arm does not match the work's
	// own kind: two sites in the daemon disagree about what the work IS.
	DetachedGapKindMismatch DetachedGapKind = "kind_mismatch"
	// DetachedGapSpoolRewind is a cumulative output restatement shorter than the
	// spool's own cursor: the source rewound under a fold that only appends.
	DetachedGapSpoolRewind DetachedGapKind = "spool_rewind"
	// DetachedGapJournalRewind is the same rewind on a workflow journal's cursor.
	DetachedGapJournalRewind DetachedGapKind = "journal_rewind"
)

// DetachedGapError is one classified daemon-bug refusal from a detached-work
// fold.
//
// It is an ordinary error on the way out — every existing caller that only
// checks non-nil keeps working — and errors.As recovers the class for the one
// caller that has to say something to the user about it.
type DetachedGapError struct {
	// MessageID is the detached-work message whose fold refused. Half of the
	// card's address.
	MessageID string
	// Gap is the class of refusal. The other half of the card's address, so
	// two different bugs on one message are two cards rather than one that
	// overwrites the other.
	Gap DetachedGapKind
	// Detail is the whole diagnostic sentence, naming both sides of the
	// disagreement. It is the log record AND the card's evidence, so the log
	// and the screen cannot say different things.
	Detail string
}

func (e *DetachedGapError) Error() string { return e.Detail }
