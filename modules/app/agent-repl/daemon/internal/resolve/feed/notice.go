package feed

import (
	"strings"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedmarker"
)

// A VENDOR NOTICE AND THE FAULT IT REPORTS ARE ONE CARD. The vendor's tooling
// writes a failure into the conversation as assistant prose ("API Error: …"),
// and the turn then ends on the same failure, so the feed would draw the one
// fault twice: a notice row and the ending's marker. Instead a notice of a
// turn that ended on a fault is FOLDED into that ending's marker
// (frontend.v1.FeedOutcomeMarker.notices) and its own row is retired.
//
// THE FOLD LIVES AT THE ONE UPSERT DOOR and reads the feed's own stored rows,
// so it holds whichever of the two arrives first: the shim's stream settles
// the notice before the terminal, while the sidecar's transcript plane can
// restate it after. A replay re-draws both and the fold happens again, and an
// ending re-composed without its notices keeps the ones its stored row already
// carries. Only a FAULT ending folds (the notice's turn raised a workspace
// fault); a notice of a turn that went on, or was interrupted, stays its own
// neutral card.

// The notice card's label, and its detail for each classified subject.
const (
	noticeLabel                = feedmarker.VendorNoticeLabel
	noticeDetailUsageLimit     = "allowance exhausted"
	noticeDetailUsageTransit   = "allowance window changed"
	noticeDetailUsageWarning   = "approaching an allowance limit"
	noticeDetailVendorTrouble  = "temporary vendor trouble"
	opNoticeFolded             = "daemon.feed.notice_folded"
	opNoticeFoldedAfterEnding  = "daemon.feed.notice_folded_after_ending"
	opNoticeRepeatsVendorWords = "daemon.feed.notice_repeats_vendor_message"
)

// noticeMarker is the head of a notice drawn on its own: the neutral family,
// because a notice on its own raises no workspace status, with the subject the
// producer classified as its detail.
func noticeMarker(notice *conversationv1.AgentResponseSynthesizedNotice) *frontendv1.FeedOutcomeMarker {
	switch notice.GetSubject().(type) {
	case *conversationv1.AgentResponseSynthesizedNotice_UsageLimit:
		return neutralMarker(noticeLabel, noticeDetailUsageLimit)
	case *conversationv1.AgentResponseSynthesizedNotice_UsageTransition:
		return neutralMarker(noticeLabel, noticeDetailUsageTransit)
	case *conversationv1.AgentResponseSynthesizedNotice_UsageWarning:
		return neutralMarker(noticeLabel, noticeDetailUsageWarning)
	case *conversationv1.AgentResponseSynthesizedNotice_VendorTransient:
		return neutralMarker(noticeLabel, noticeDetailVendorTrouble)
	}
	return neutralMarker(noticeLabel, "")
}

// noticeSubject names the notice's subject for a log record.
func noticeSubject(notice *conversationv1.AgentResponseSynthesizedNotice) string {
	switch notice.GetSubject().(type) {
	case *conversationv1.AgentResponseSynthesizedNotice_UsageLimit:
		return "usage_limit"
	case *conversationv1.AgentResponseSynthesizedNotice_UsageTransition:
		return "usage_transition"
	case *conversationv1.AgentResponseSynthesizedNotice_UsageWarning:
		return "usage_warning"
	case *conversationv1.AgentResponseSynthesizedNotice_VendorTransient:
		return "vendor_transient"
	}
	return "unclassified"
}

// noticeText is a notice row's words, and whether ROW is a notice row at all.
func noticeText(row *frontendv1.FeedRow) (string, bool) {
	resp := row.GetActivity().GetResponse()
	if resp.GetNotice() == nil {
		return "", false
	}
	if success := resp.GetSuccess(); success != nil {
		return success.GetProse().GetProse().GetSource().GetMarkdown(), true
	}
	return resp.GetUpdate().GetProse().GetProse().GetSource().GetMarkdown(), true
}

// transientNotice reports whether ROW is a notice drawn on its own that
// reports trouble waiting cures (an overload, a timeout, a dropped
// connection), which the producer classified and noticeMarker states in the
// card's head. The head is the row's own record of it, so a replayed or
// scrolled-back row says the same.
func transientNotice(row *frontendv1.FeedRow) bool {
	notice := row.GetActivity().GetResponse().GetNotice()
	return notice != nil && notice.GetMarker().GetDetail().GetText() == noticeDetailVendorTrouble
}

// faultEndingMarker is ROW's marker when ROW is a turn ending that raised a
// fault (a vendor or an agent-repl fault), nil otherwise.
func faultEndingMarker(row *frontendv1.FeedRow) *frontendv1.FeedOutcomeMarker {
	marker := row.GetTurnEnded().GetErrored().GetMarker()
	if marker.GetVendorFault() == nil && marker.GetAgentReplFault() == nil {
		return nil
	}
	return marker
}

// foldNotice adds TEXT to MARKER's notices unless the marker already says
// those words, as its vendor message or as a notice. It reports whether the
// marker changed, and why it did not.
func foldNotice(marker *frontendv1.FeedOutcomeMarker, text string) (added bool, why string) {
	words := strings.TrimSpace(text)
	if words == "" {
		return false, "empty"
	}
	if strings.TrimSpace(marker.GetVendorFault().GetExpansion().GetMessage().GetText()) == words {
		return false, "vendor_message"
	}
	for _, existing := range marker.GetNotices() {
		if strings.TrimSpace(existing.GetText()) == words {
			return false, "already_folded"
		}
	}
	marker.Notices = append(marker.Notices, &frontendv1.FeedOutcomeNotice{Text: text})
	return true, ""
}

// foldNoticesIntoEnding folds every notice row of SNAPSHOT's turn on F into
// SNAPSHOT, when SNAPSHOT is a fault ending, keeping the notices the row's
// stored publication already carries. It answers the notice rows to retire
// once SNAPSHOT is published.
func (r *resolver) foldNoticesIntoEnding(s *wsState, f *feedState, id string, snapshot *frontendv1.FeedRow) []string {
	marker := faultEndingMarker(snapshot)
	if marker == nil {
		return nil
	}
	if stored := faultEndingMarker(f.rows[id]); stored != nil {
		for _, kept := range stored.GetNotices() {
			foldNotice(marker, kept.GetText())
		}
	}
	turn := snapshot.GetTurn().GetValue()
	var retire []string
	for _, rowID := range f.order {
		row := f.rows[rowID]
		text, ok := noticeText(row)
		if !ok || turn == "" || row.GetTurn().GetValue() != turn {
			continue
		}
		added, why := foldNotice(marker, text)
		retire = append(retire, rowID)
		r.logger(s.id).Info(opNoticeFolded,
			"a vendor notice of a turn that ended on a fault was folded into the ending's card; its own row is retired",
			dlog.Context{"feed": f.key, "turn": turn, "notice_row": rowID, "ending_row": id, "added": added, "not_added_because": why})
	}
	return retire
}

// noticeFoldedIntoEnding takes a notice row drawn AFTER its turn's fault
// ending: the notice is folded into the stored ending, which is published
// again, and the notice row is refused (and retired, had it been drawn). It
// reports whether ROW was taken that way.
func (r *resolver) noticeFoldedIntoEnding(s *wsState, f *feedState, at placement, row *frontendv1.FeedRow) bool {
	text, ok := noticeText(row)
	turn := row.GetTurn().GetValue()
	if !ok || turn == "" {
		return false
	}
	for _, endingID := range f.order {
		ending := f.rows[endingID]
		if ending.GetTurn().GetValue() != turn || faultEndingMarker(ending) == nil {
			continue
		}
		id := row.GetId().GetValue()
		r.logger(s.id).Info(opNoticeFoldedAfterEnding,
			"a vendor notice arrived after its turn's fault ending; it was folded into the ending's card instead of being drawn",
			dlog.Context{"feed": f.key, "turn": turn, "notice_row": id, "ending_row": endingID})
		// The notice row is placed nowhere, and retired if an earlier draw
		// placed it; the ending is restated carrying the notice's words.
		r.retire(s, at.feed, id)
		why := unclonable{
			operation: opNoticeFoldedAfterEnding,
			message:   "a fault ending could not be copied to take a late vendor notice",
			context:   dlog.Context{"turn": turn, "notice_row": id, "ending_row": endingID},
		}
		r.restateRow(s, placement{feed: at.feed, parent: ending.GetParent()}, ending, !f.nonDurable[endingID], why,
			func(refold *frontendv1.FeedRow) {
				if added, why := foldNotice(faultEndingMarker(refold), text); !added {
					r.logger(s.id).Debug(opNoticeRepeatsVendorWords,
						"a late vendor notice added nothing to its ending's card",
						dlog.Context{"feed": f.key, "turn": turn, "notice_row": id, "why": why})
				}
			})
		return true
	}
	return false
}
