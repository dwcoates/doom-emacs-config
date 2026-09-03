package feed

import (
	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/figures"
)

// THE RESPONSE'S COST CORNER. A FeedResponse's stamp is an API RESPONSE's
// figures, and the wire states usage on EXACTLY ONE unit per API response —
// the unit for the response's FIRST content block, which for the vendor's
// observed `[thinking, text]` shape is the THINKING unit, not the prose one. A
// bubble that read only its own unit's envelope therefore drew no stamp at all
// for the ordinary shape, which is why the grouping lives here: every activity
// is filed under the API response it arrived in, and the prose bubble stamps
// its response's figures.

// fileAPIResponse files one unit under the API response it arrived in and
// records that response's stamp when the unit is the one carrying its usage.
//
// A unit arriving WITH usage opens a new response; every unit after it, until
// the next usage-carrying one, belongs to that same response. Units seen
// before any usage sit in response zero. A unit is filed ONCE: its later
// frames restate the same unit and must never re-open a response, though a
// unit that states its usage on a later frame stamps the response it was
// already filed under.
func (s *wsState) fileAPIResponse(unit string, usage *conversationv1.TokenUsage) {
	response, filed := s.unitAPIResponse[unit]
	if !filed {
		if usage != nil {
			s.apiResponseSeq++
		}
		response = s.apiResponseSeq
		s.unitAPIResponse[unit] = response
	}
	if usage == nil {
		return
	}
	// The stamp is the EXPENSIVE sum and nothing else: both cache-miss buckets
	// together, because both were processed fresh. A stamp read off any single
	// counter would understate the bill.
	misses := usage.GetInputMisses()
	s.apiResponseUsage[response] = figures.Tokens(misses.GetWritten() + misses.GetUnwritten())
}

// apiResponseStamp answers the formatted stamp of the API response the unit
// arrived in, empty when that response has stated no usage — absence draws no
// stamp, never a zero.
func (s *wsState) apiResponseStamp(unit string) string {
	return s.apiResponseUsage[s.unitAPIResponse[unit]]
}
