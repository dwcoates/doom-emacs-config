package registry

import "fmt"

// ModelObservation is WHEN a report of a session's model was true, so the two
// authorities that report one can be ORDERED instead of racing.
//
// THE RACE IT ENDS. `rec.Model` has two writers: the shim's confirmation of a
// deliberate model change, and the `SystemInit` the SDK re-announces on every
// submit. Both used to write last-writer-wins with nothing to order them, so
// this interleaving silently undid a change the user had just made:
//
//   - the user selects Opus; the shim confirms it; the record says Opus;
//   - the `SystemInit` belonging to the submit that was ALREADY IN FLIGHT is
//     processed, announcing the Sonnet that submit began under;
//   - the record is rewritten to Sonnet, the picker repaints to the older
//     value, and the next respawn pins it.
//
// Carrying the instant an observation was TRUE AS OF — rather than the instant
// it happened to be processed — is what makes that interleaving unrepresentable
// rather than merely rare.
type ModelObservation struct {
	// Generation is the controller generation the observation was made under,
	// carried for diagnostics: it is the id the daemon log names a session's
	// bring-up by, so a rejected observation can be traced to the controller
	// that produced it.
	Generation string
	// GenOrdinal orders controller generations within one daemon process. A
	// controller that REPLACED another always observes with a strictly higher
	// ordinal, so a report left in flight by the generation it replaced can
	// never win.
	//
	// Zero is "no ordering at all" and is never a valid observation; see Valid.
	GenOrdinal uint64
	// StreamSeq is the file-plane seq the observation was true as of.
	//
	// For a `SystemInit` it is the seq of the event that carried it. For a
	// shim CONFIRMATION it is the highest seq the controller had consumed when
	// the shim answered — the confirmation is ground truth at that instant, so
	// every event already consumed is by construction older than it, and the
	// stale `SystemInit` above is refused on exactly that comparison.
	StreamSeq uint64
}

// Valid reports whether o carries an ordering at all.
//
// An observation with no generation ordinal cannot be ordered against
// anything, so it is refused rather than being given the benefit of the doubt:
// admitting it would restore precisely the last-writer-wins behavior this type
// exists to remove.
func (o ModelObservation) Valid() bool { return o.GenOrdinal > 0 }

// Supersedes reports whether o is STRICTLY NEWER than prev, and is the whole
// admission rule.
//
// Equal is deliberately not newer. A re-delivered event (a resync, a bounded
// replay) carries the seq it originally carried, and re-applying it is exactly
// the stale write the ordering exists to refuse — the value it would write is
// already the recorded one when it is genuinely the same observation.
func (o ModelObservation) Supersedes(prev ModelObservation) bool {
	if o.GenOrdinal != prev.GenOrdinal {
		return o.GenOrdinal > prev.GenOrdinal
	}
	return o.StreamSeq > prev.StreamSeq
}

// String renders the token for the daemon log.
func (o ModelObservation) String() string {
	return fmt.Sprintf("generation=%s/%d stream_seq=%d", o.Generation, o.GenOrdinal, o.StreamSeq)
}
