package feed

// A RESPONSE'S ANSWERS are the prompts whose entries answer its tree's labels
// (frontend.v1.FeedResponse.answers):
//
//   - STAMPED on every upsert of a response row, by stampAnswers, from the
//     feed's own order: an entry answers the NEAREST response before its
//     prompt, on the same feed, whose tree holds a node with the entry's
//     label;
//   - RESTATED by restateAnswers whenever what they are read from changes: a
//     prompt's entries are drawn or change, a response's labels are drawn or
//     change, or either is removed (a rollback). Every response of the feed
//     is re-published, and upsert drops the ones that come out unchanged.

import (
	"slices"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
)

// responseLabels answers the labels of every node of ROW's trees, when ROW is
// a settled response; nil otherwise. A response still arriving links nothing:
// its tree is not yet whole.
func responseLabels(row *frontendv1.FeedRow) map[string]bool {
	var labels map[string]bool
	for _, block := range row.GetActivity().GetResponse().GetSuccess().GetProse().GetProse().GetBlocks() {
		for _, node := range block.GetTree().GetNodes() {
			if labels == nil {
				labels = map[string]bool{}
			}
			labels[node.GetLabel().GetText()] = true
		}
	}
	return labels
}

// promptEntryLabels answers the labels ROW's entries answer, in order, when
// ROW is a user prompt; nil otherwise.
func promptEntryLabels(row *frontendv1.FeedRow) []string {
	var labels []string
	for _, block := range row.GetUserPrompt().GetSuccess().GetBody().GetBlocks() {
		for _, prose := range block.GetText().GetProse().GetBlocks() {
			if entry := prose.GetEntry(); entry != nil {
				labels = append(labels, entry.GetLabel().GetText())
			}
		}
	}
	return labels
}

// answerTarget answers the row the entry answering LABEL, in the prompt at
// index AT of F's order, answers: the nearest response before it whose labels
// (as LABELSAT reads them) hold LABEL, or "".
func answerTarget(f *feedState, at int, label string, labelsAt func(int) map[string]bool) string {
	for i := at - 1; i >= 0; i-- {
		if labelsAt(i)[label] {
			return f.order[i]
		}
	}
	return ""
}

// stampAnswers states a response row's answers from F's order. It runs on the
// one write path (`upsert`), so every publication of a response restates
// them. ROW is the snapshot being published under ID, whose labels stand in
// for the stored row's.
func stampAnswers(f *feedState, id string, row *frontendv1.FeedRow) {
	resp := row.GetActivity().GetResponse()
	if resp == nil {
		return
	}
	resp.Answers = nil
	labels := responseLabels(row)
	at := f.indexOf(id)
	if len(labels) == 0 || at < 0 {
		// A row not yet placed has nothing after it until it is: its first
		// publication restates the feed's answers once it is (upsert).
		return
	}
	labelsAt := func(i int) map[string]bool {
		if f.order[i] == id {
			return labels
		}
		return responseLabels(f.rows[f.order[i]])
	}
	for i := at + 1; i < len(f.order); i++ {
		prompt := f.rows[f.order[i]]
		for _, label := range promptEntryLabels(prompt) {
			if answerTarget(f, i, label, labelsAt) == id {
				resp.Answers = append(resp.Answers, &frontendv1.FeedResponseAnswer{Label: label, Prompt: prompt.GetId()})
			}
		}
	}
}

// answersRead reports whether ROW is read by some response's answers: a user
// prompt with entries, or a settled response with a tree.
func answersRead(row *frontendv1.FeedRow) bool {
	return len(promptEntryLabels(row)) > 0 || len(responseLabels(row)) > 0
}

// answersChanged reports whether publishing SNAPSHOT over EXISTING (nil on a
// first draw) changes what any response's answers are read from: a prompt's
// entry labels, or a response's labels.
func answersChanged(existing, snapshot *frontendv1.FeedRow) bool {
	if !slices.Equal(promptEntryLabels(existing), promptEntryLabels(snapshot)) {
		return true
	}
	before, after := responseLabels(existing), responseLabels(snapshot)
	if len(before) != len(after) {
		return true
	}
	for label := range after {
		if !before[label] {
			return true
		}
	}
	return false
}

// restateAnswers re-publishes every response of the feed at ADDR, so each
// restates its answers from the feed as it now stands; upsert drops the ones
// that come out unchanged. WHY is logged with the count.
func (r *resolver) restateAnswers(s *wsState, addr feedid.Feed, why string) {
	f := r.feed(s, addr)
	var responses []string
	for _, id := range f.order {
		if isResponseRow(f.rows[id]) {
			responses = append(responses, id)
		}
	}
	for _, id := range responses {
		if row, ok := f.rows[id]; ok {
			r.upsert(s, placement{feed: addr}, row, !f.nonDurable[id])
		}
	}
	r.logger(s.id).Debug("daemon.feed.answers_restated",
		"what a response's answers are read from changed; every response of the feed restated them",
		dlog.Context{"feed": f.key, "why": why, "responses": len(responses)})
}

// promptEntryText answers what ROW's entry answering LABEL says, when ROW is a
// user prompt holding one: the first such entry.
func promptEntryText(row *frontendv1.FeedRow, label string) (string, bool) {
	for _, block := range row.GetUserPrompt().GetSuccess().GetBody().GetBlocks() {
		for _, prose := range block.GetText().GetProse().GetBlocks() {
			if entry := prose.GetEntry(); entry != nil && entry.GetLabel().GetText() == label {
				return entry.GetText().GetMarkdown(), true
			}
		}
	}
	return "", false
}

// PromptEntry answers what the entry answering LABEL of the root-feed user
// prompt row ID says, as last published.
func (r *resolver) PromptEntry(ws ids.WorkspaceID, id *frontendv1.FeedId, label string) (string, bool) {
	text, ok := r.rootRowText(ws, id, func(row *frontendv1.FeedRow) (string, bool) {
		return promptEntryText(row, label)
	})
	return text.Markdown, ok
}
