package sessioncontroller

import (
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// withhold.go — THE ONE FILTER EVERY WITHHOLDING CURATOR RUNS THROUGH.
//
// The curation block in pushConversation runs several independent curators
// (machinery.go, noresponse.go, tasknotification.go, keepaliveexclude.go), and
// every one of them answers the same question about each item — keep it or
// withhold it — and then performs the same mechanics: filter the delta's items
// IN PLACE, write the survivors back, and leave the delta itself pushed.
//
// Those mechanics are what lives here, once. Each curator supplies only its own
// verdict. Hand-rolling the loop per curator is what let three copies of it
// drift apart in the first place; a curator that gets the aliasing of the
// in-place filter subtly wrong drops items nobody asked it to.
//
// THE DELTA IS NEVER DROPPED, only emptied. Its through_seq is the frontend's
// replay cursor, so a delta every curator emptied is still pushed — withholding
// the frame would leave every client's cursor stuck behind the withheld record
// forever.

// withholdVerdict is one curator's answer for one item: whether to withhold it,
// and the canonical log line stating why.
//
// A withhold with an EMPTY log line is silent BY DESIGN, for the curator whose
// record is an aggregate rather than one line per item (keepaliveexclude.go).
// It is not a licence to withhold quietly: a curator that withholds one item
// for its own reason says so, per item, or accounts for all of them at once.
type withholdVerdict struct {
	Withhold bool
	LogLine  string
}

// keepItem is the verdict for an item that belongs in the feed.
var keepItem = withholdVerdict{}

// withholdItem is the verdict for an item that does not, carrying the canonical
// log line that states why.
func withholdItem(logLine string) withholdVerdict {
	return withholdVerdict{Withhold: true, LogLine: logLine}
}

// withholdItems filters cd's items in place through one curator's judge,
// emitting each withheld item's canonical log record, and reports how many it
// withheld.
//
// The judge sees every item in delta order and decides that item alone. It may
// log on its own account for an item it KEEPS (a verdict that could not be
// reached says so at the site), which is why it takes the consumer's logging
// path rather than being a pure predicate.
func (c *consumer) withholdItems(cd *frontendv1.ConversationDelta, judge func(*frontendv1.ConversationItem) withholdVerdict) int {
	if cd == nil || len(cd.GetItems()) == 0 {
		return 0
	}
	items := cd.GetItems()
	kept := items[:0]
	withheld := 0
	for _, it := range items {
		verdict := judge(it)
		if !verdict.Withhold {
			kept = append(kept, it)
			continue
		}
		withheld++
		if verdict.LogLine != "" {
			c.logf("%s", verdict.LogLine)
		}
	}
	cd.Items = kept
	return withheld
}
