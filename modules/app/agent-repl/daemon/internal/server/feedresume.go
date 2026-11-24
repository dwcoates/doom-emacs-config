package server

import (
	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

// A REOPENED FEED RESUMES FROM WHAT ITS READER HOLDS (OpenFeedRequest.resume).
// The page a resumed open answers is the newest page the feed would have
// served, less every row the reader already holds at the same revision
// (FeedRow.revision), so a reader holding the feed as it stands is sent no
// row and draws nothing. `standing` still names the whole page, so the reader
// can tell a row that left from one that is merely unchanged.

// resumePage narrows PAGE, the newest page just composed for this open, to the
// rows RESUME does not hold at their revision, and answers the standing ids
// of the whole page. A page that failed is answered whole: its error is drawn
// where the rows would be, whatever the reader holds.
func resumePage(page *frontendv1.FeedPage, resume *agentreplv1.FeedResume) *agentreplv1.OpenFeedResumed {
	resumed := &agentreplv1.OpenFeedResumed{}
	success := page.GetSuccess()
	if success == nil {
		return resumed
	}
	held := make(map[string]string, len(resume.GetHeld()))
	for _, row := range resume.GetHeld() {
		held[row.GetId().GetValue()] = row.GetRevision().GetDigest()
	}
	newer := make([]*frontendv1.FeedRow, 0, len(success.GetRows()))
	for _, row := range success.GetRows() {
		resumed.Standing = append(resumed.Standing, row.GetId())
		if revision, ok := held[row.GetId().GetValue()]; ok && revision == row.GetRevision().GetDigest() {
			continue
		}
		newer = append(newer, row)
	}
	// THE PAGE IS THIS OPEN'S OWN: composed for it alone, so narrowing its row
	// list changes nothing another reader holds (the rows themselves are shared
	// and are not touched).
	success.Rows = newer
	return resumed
}
