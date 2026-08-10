// Package daemonturn is the ONE NAME for a turn the daemon submitted on a
// session's behalf.
//
// The daemon opens turns nobody typed. Three of them cut the conversation's
// context: the warm compaction it schedules while the prompt cache is still
// provably warm (sessioncontroller/warmcompact.go), and the two shapes of the
// compact-first revival that cut a hibernated session's context before the next
// real prompt (sessioncontroller/revive.go) — one submitting `/compact`, one
// `/clear`. Each is submitted under a turn id carrying its family's prefix.
//
// THOSE PREFIXES ARE READ FAR FROM THE SITES THAT MINT THEM: by the
// conversation curator that must render none of such a turn
// (sessioncontroller/contextcutexclude.go), and by the store reconciliation
// that may infer a revival's compaction turn is over (ssm/compactionclaim.go).
// Re-declared at each reader, they would be several strings that have to be
// kept equal for those readings to agree, which is why they are declared here
// once and imported everywhere.
//
// WHY A PACKAGE OF ITS OWN. It has to sit below every reader — the session
// controller mints from here, the state machine matches from here, and the
// session controller already depends on the state machine — so the vocabulary
// cannot live in either of them. It depends on nothing itself, which is what
// keeps that true as either grows.
//
// THE TURN ID IS DURABLE EVIDENCE, not a live-state reading. It is written into
// the store with the turn, so a conversation re-pulled years later with no
// daemon alive still says which turns were the daemon's own — the property the
// keep-alive exclusion buys with a ledger table, a minted id already carries in
// its own name, and the property the internal re-drive's marker exists for
// (frontend/internalresume.go).
package daemonturn

import "strings"

const (
	// WarmCompactPrefix opens the turn id a warm compaction submits under:
	// `warm-compact:<session>:<nonce>`.
	WarmCompactPrefix = "warm-compact:"
	// ReviveCompactPrefix opens the turn id a compact-first revival submits its
	// `/compact` under: `revive-compact:<session>:<nonce>`.
	ReviveCompactPrefix = "revive-compact:"
	// ReviveClearPrefix opens the turn id a clear-first revival submits its
	// `/clear` under: `revive-clear:<session>:<nonce>`.
	ReviveClearPrefix = "revive-clear:"
)

// contextCutPrefixes is the closed set of turn-id families the daemon submits a
// context cut under. A new cut adds its prefix HERE, which is what makes the
// curator and the reconciliation learn about it together.
var contextCutPrefixes = []string{WarmCompactPrefix, ReviveCompactPrefix, ReviveClearPrefix}

// IsContextCut reports whether a turn id names a context cut the DAEMON
// submitted on the session's behalf, rather than anything a user asked for.
//
// AN EMPTY ID IS NEVER ONE. Plenty of records carry no turn id at all — every
// file-plane transcript line does, and so do the `compact_boundary` and clear
// records the feed's dividers are drawn from — and reading absence as "the
// daemon's" would classify arbitrary conversation as internal.
//
// THE MATCH IS ON THE HEAD, because that is where a minted id carries its
// family. An id that merely contains one of these strings somewhere inside it
// was minted by something else, and something else's turn is not this one's to
// speak for.
func IsContextCut(turnID string) bool {
	if turnID == "" {
		return false
	}
	for _, prefix := range contextCutPrefixes {
		if strings.HasPrefix(turnID, prefix) {
			return true
		}
	}
	return false
}
