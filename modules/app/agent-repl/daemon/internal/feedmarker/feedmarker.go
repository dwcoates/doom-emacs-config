// Package feedmarker builds the outcome markers a feed row heads with that
// more than one producer writes: the feed resolver drawing them live, and the
// state store's layout steps carrying a retired row shape onto today's. One
// spelling of each, so a row the store rewrote reads exactly as one the
// resolver drew.
package feedmarker

import frontendv1 "agentrepl/proto/frontend/v1"

// VendorNoticeLabel is the label of a vendor-synthesized notice's marker.
const VendorNoticeLabel = "vendor notice"

// Neutral is a marker of the user's own act, a hook's, or a notice standing
// on its own. It carries no expansion: the neutral family has none to carry.
// An empty DETAIL is no detail.
func Neutral(label, detail string) *frontendv1.FeedOutcomeMarker {
	m := &frontendv1.FeedOutcomeMarker{
		Label:  &frontendv1.FeedOutcomeMarkerLabel{Text: label},
		Family: &frontendv1.FeedOutcomeMarker_Neutral{Neutral: &frontendv1.FeedOutcomeMarkerNeutral{}},
	}
	if detail != "" {
		m.Detail = &frontendv1.FeedOutcomeMarkerDetail{Text: detail}
	}
	return m
}
