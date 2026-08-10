package sessioncontroller

import (
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/daemonturn"
)

// contextcutexclude.go — WITHHOLDING THE DAEMON'S OWN CONTEXT CUTS FROM EVERY
// RENDERING.
//
// The daemon cuts a session's context three ways without anybody typing
// anything: the warm compaction it schedules while the prompt cache is still
// warm (warmcompact.go), and the two shapes of revival that cut a hibernated
// session's context before the next real prompt (revive.go) — one submitting
// `/compact`, one `/clear`. None of the three is anybody's conversation. All
// three were already HALF invisible, and the half that leaked is what this
// closes.
//
// WHAT LEAKED. The submitted `/compact` never rendered — the CLI writes it as
// `<command-message>compact</command-message>` bookkeeping, which
// withholdMachinery takes out — and the cut's own rendering, the orange rule
// and its stamp or the red clear bar, comes from the vendor's boundary record.
// What was left over was the turn's PLUMBING: the vendor's terminal result, and
// the CLI's "Not enough messages to compact." notice on a compaction that
// declined. The result rendered as a standalone duration chip — a small grey
// badge reading nothing but `6s`, attached to nothing, because the turn it
// closed had no bubble for the chip to sit in and no prompt above it to explain
// what it timed. 139 such turns were in the store when this was written.
//
// A TURN THAT IS INVISIBLE LEAVES NO RESIDUE. That is the whole rule, and the
// residue is the defect: a chip is a statement that a turn ran, so a chip over a
// turn the user cannot see is a statement they have no way to read.
//
// THE VERDICT IS THE TURN ID, read through the ONE vocabulary the minting sites
// use (internal/daemonturn). A turn id is written into the store with the turn,
// so this same reading holds on a re-pull years later with no daemon alive — the
// durability the keep-alive exclusion buys with a ledger table, a minted id
// already carries in its own name.
//
// A REAL TURN'S CHIP IS UNTOUCHED, and that is the point of deciding on the
// daemon's own id rather than on "the turn wrote no answer". A turn the user
// asked for that only ran tools, or that was interrupted or errored, produces
// exactly the same shape — and there the chip is the only thing on the feed
// saying the turn ran at all.
//
// THE CUT'S OWN DIVIDER SURVIVES, structurally rather than by exception. A
// divider is translated from the vendor's own boundary record, which is a
// file-plane line carrying an EMPTY request id — every ContextCompacted and
// ContextCleared event in the store does. So the rule "an empty id is never the
// daemon's" (daemonturn.IsContextCut) is what keeps the one thing a cut OWES the
// user on the feed.
//
// WHY HERE. Beside withholdKeepAlive, withholdMachinery and
// withholdNoResponsePlaceholders, in the curation block of pushConversation:
// every route that can put a conversation item in front of a user funnels
// through it — the live push, the ring resync, the history re-pull and the
// durable replay. A second exclusion anywhere else would be a second chance to
// disagree with this one.

// withholdDaemonContextCut removes every item of a translated delta belonging to
// a context cut the daemon submitted, and reports how many it withheld.
//
// SILENT PER ITEM, ACCOUNTED FOR IN ONE RECORD, exactly as the keep-alive
// exclusion is: a cut's items arrive a few at a time across several deltas, and
// a line per item would say the same thing repeatedly without saying more.
func (c *consumer) withholdDaemonContextCut(cd *frontendv1.ConversationDelta) int {
	withheld := c.withholdItems(cd, func(item *frontendv1.ConversationItem) withholdVerdict {
		if !daemonturn.IsContextCut(item.GetRequestId()) {
			return keepItem
		}
		return withholdItem("")
	})
	if withheld > 0 {
		c.logf("session-controller: daemon context cut items WITHHELD ws=%q session=%s seq=%d withheld=%d remaining=%d — the daemon submitted this cut, so its turn plumbing (the terminal result the feed would draw as a bare duration chip, and any notice the CLI answered with) is excluded from every rendering; the turns stay in the store and the cut's own divider is unaffected",
			c.workspace, c.sessionID, cd.GetThroughSeq(), withheld, len(cd.GetItems()))
	}
	return withheld
}
