package sessioncontroller

import (
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// keepaliveexclude.go — WITHHOLDING THE PING FROM EVERY RENDERING.
//
// Keep-alive turns are persisted, never deleted, and never rendered. The
// exclusion runs at the ONE conversation chokepoint — the curation block in
// pushConversation, beside curateMetaRecords and withholdMachinery — because
// every route that can put a conversation item in front of a user funnels
// through it: the live push, the ring resync, the history re-pull, and the
// durable replay. A second exclusion anywhere else would be a second chance to
// disagree with this one.
//
// THE VERDICT COMES FROM THE DURABLE WINDOW LEDGER, keyed on the item's OWN
// timestamp, never from whether a ping happens to be running right now. This
// same path runs on a resync replaying week-old history, and a live-state
// reading would render then what it withheld before. It is the merge lease
// provenance rule applied to a different question.

// KeepAliveWindowLedger answers the one question the exclusion asks. Satisfied
// by *statedb.KeepAliveWindows.
//
// Nil is the feature OFF, and it is loud at the site rather than silent: a
// daemon with no ledger cannot tell a ping's records from a user's, and the
// honest reading of that is "nothing is excluded", stated once per session
// rather than assumed.
type KeepAliveWindowLedger interface {
	// Covers reports whether tsMs falls inside any of workspace's keep-alive
	// windows.
	Covers(workspace string, tsMs int64) (bool, error)
	// Open records a ping's start.
	Open(w KeepAliveWindowRecord) error
	// Close stamps a ping's end.
	Close(turnID string, endedAtMs int64) error
}

// KeepAliveWindowRecord is one ping's interval, mirroring
// statedb.KeepAliveWindow so this package does not depend on the store's shape.
type KeepAliveWindowRecord struct {
	TurnID      string
	Workspace   string
	StartedAtMs int64
}

// withholdKeepAlive removes every item of a translated delta that falls inside
// a keep-alive window, and reports how many it withheld.
//
// A LEDGER READ FAILURE WITHHOLDS NOTHING AND SAYS SO. The alternative — drop
// the delta, or withhold everything — would hide real conversation because a
// bookkeeping table was unreadable, which is a far worse failure than showing a
// ping. Erring toward showing content and logging loudly is the only direction
// that cannot lose a user's own words.
func (c *consumer) withholdKeepAlive(cd *frontendv1.ConversationDelta) int {
	if c.keepAliveWindows == nil || cd == nil || len(cd.GetItems()) == 0 {
		return 0
	}
	kept := cd.Items[:0]
	withheld := 0
	for _, item := range cd.GetItems() {
		covered, err := c.keepAliveWindows.Covers(c.workspace, item.GetTsMs())
		if err != nil {
			c.logf("session-controller: keep-alive exclusion READ FAILED ws=%q session=%s seq=%d uuid=%s ts_ms=%d error=%v — the item is SHOWN rather than withheld; hiding real conversation because a bookkeeping table was unreadable is the worse failure",
				c.workspace, c.sessionID, cd.GetThroughSeq(), item.GetUuid(), item.GetTsMs(), err)
			kept = append(kept, item)
			continue
		}
		if covered {
			withheld++
			continue
		}
		kept = append(kept, item)
	}
	cd.Items = kept
	if withheld > 0 {
		c.logf("session-controller: keep-alive items WITHHELD ws=%q session=%s seq=%d withheld=%d remaining=%d — the turns stay in the store and stay excluded from every rendering",
			c.workspace, c.sessionID, cd.GetThroughSeq(), withheld, len(kept))
	}
	return withheld
}
