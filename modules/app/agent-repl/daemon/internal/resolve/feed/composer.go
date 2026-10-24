package feed

import (
	"context"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
)

// THE COMPOSER'S ENTRIES are a prompt still being typed: each entry targets a
// response by the one rule a sent prompt's entries are bound by (entryTarget),
// read as if the prompt were the root feed's next row. The server holds what
// the composer reported and asks the feed what it targets (ComposerTargets)
// and what a line opened beside an entry answers (EntryNeighbor), and follows
// LabelChanges to restate both when a response's tree changes.

// entryTarget answers the row an entry answering LABEL targets, for a prompt
// standing at index AT of F's order (len(f.order) for one not yet sent) that
// replies to REPLY ("" when it replies to nothing), reading each row's labels
// through LABELSAT:
//   - a reply to a response with a tree answers that response, when its tree
//     holds LABEL, and nothing otherwise;
//   - any other entry answers the nearest response before AT whose tree holds
//     LABEL, or nothing.
//
// A reply to a row with no tree (a prompt, a response with no tree) is bound
// as a prompt replying to nothing is.
func entryTarget(f *feedState, at int, reply, label string, labelsAt func(int) map[string]bool) string {
	if reply != "" {
		if i := f.indexOf(reply); i >= 0 && i < at {
			if replied := labelsAt(i); len(replied) > 0 {
				if replied[label] {
					return reply
				}
				return ""
			}
		}
	}
	for i := at - 1; i >= 0; i-- {
		if labelsAt(i)[label] {
			return f.order[i]
		}
	}
	return ""
}

// promptQuotedRow answers the row a user prompt ROW replies to: its quote's
// quoted row (frontend.v1.FeedQuoteBlock.quoted_row), or "" when it quotes
// nothing or its quote was recorded before the row was kept.
func promptQuotedRow(row *frontendv1.FeedRow) string {
	for _, block := range row.GetUserPrompt().GetSuccess().GetBody().GetBlocks() {
		if quote := block.GetQuote(); quote != nil {
			return quote.GetQuotedRow().GetValue()
		}
	}
	return ""
}

// composerTarget answers the row an entry answering LABEL targets in a
// prompt not yet sent that replies to REPLY: entryTarget, read as if the
// prompt were F's next row. The one reading ComposerTargets and EntryNeighbor
// share.
func composerTarget(f *feedState, reply *frontendv1.FeedId, label string) string {
	labelsAt := func(i int) map[string]bool { return responseLabels(f.rows[f.order[i]]) }
	return entryTarget(f, len(f.order), reply.GetValue(), label, labelsAt)
}

// rootFeedState answers WS's root feed, or nil when the resolver holds none.
// The caller holds r.mu.
func (r *resolver) rootFeedState(ws ids.WorkspaceID) *feedState {
	s, ok := r.workspaces[ws]
	if !ok {
		return nil
	}
	return s.feeds[r.feedKey(ws, feedid.Feed{Root: true})]
}

// ComposerTargets answers, for each of LABELS in order, the response row an
// entry answering it targets in a prompt not yet sent that replies to REPLY
// (nil for none), or nil where it targets nothing (entryTarget).
func (r *resolver) ComposerTargets(ws ids.WorkspaceID, reply *frontendv1.FeedId, labels []string) []*frontendv1.FeedId {
	r.mu.Lock()
	defer r.mu.Unlock()
	targets := make([]*frontendv1.FeedId, len(labels))
	f := r.rootFeedState(ws)
	if f == nil {
		return targets
	}
	for i, label := range labels {
		if id := composerTarget(f, reply, label); id != "" {
			targets[i] = f.rows[id].GetId()
		}
	}
	return targets
}

// ResponseNode answers whether the root-feed response row ID's tree holds a
// node labelled LABEL, as last published: the node a reader clicks in the
// feed to answer it.
func (r *resolver) ResponseNode(ws ids.WorkspaceID, id *frontendv1.FeedId, label string) bool {
	_, ok := r.rootRowText(ws, id, func(row *frontendv1.FeedRow) (string, bool) {
		return "", responseLabels(row)[label]
	})
	return ok
}

// EntryNeighbor answers the label a line opened beside the entry answering
// LABEL answers, in the tree of the response that entry targets
// (ComposerTargets, replying to REPLY). NEXT finds the node's next sibling,
// else its parent's next sibling, and so on up; otherwise the node's previous
// sibling, else its parent. ok is false when the label targets no response or
// its node has no such neighbor.
func (r *resolver) EntryNeighbor(ws ids.WorkspaceID, reply *frontendv1.FeedId, label string, next bool) (string, bool) {
	r.mu.Lock()
	defer r.mu.Unlock()
	f := r.rootFeedState(ws)
	if f == nil {
		return "", false
	}
	id := composerTarget(f, reply, label)
	if id == "" {
		return "", false
	}
	for _, block := range f.rows[id].GetActivity().GetResponse().GetSuccess().GetProse().GetProse().GetBlocks() {
		if neighbor, found, ok := treeNeighbor(block.GetTree(), label, next); found {
			return neighbor, ok
		}
	}
	return "", false
}

// treeNeighbor answers LABEL's neighbor in TREE (EntryNeighbor's rule).
// found is false when TREE holds no node labelled LABEL; ok is false when it
// does and the node has no neighbor that way.
func treeNeighbor(tree *frontendv1.ProseTree, label string, next bool) (neighbor string, found, ok bool) {
	nodes := tree.GetNodes()
	parent := make([]int, len(nodes))
	children := map[int][]int{}
	at := -1
	for i, n := range nodes {
		parent[i] = -1
		if b := n.GetBranch(); b != nil {
			parent[i] = int(b.GetParent().GetIndex())
		}
		children[parent[i]] = append(children[parent[i]], i)
		if at < 0 && n.GetLabel().GetText() == label {
			at = i
		}
	}
	if at < 0 {
		return "", false, false
	}
	position := func(i int) int {
		for k, c := range children[parent[i]] {
			if c == i {
				return k
			}
		}
		return -1
	}
	if !next {
		if k := position(at); k > 0 {
			return nodes[children[parent[at]][k-1]].GetLabel().GetText(), true, true
		}
		if parent[at] >= 0 {
			return nodes[parent[at]].GetLabel().GetText(), true, true
		}
		return "", true, false
	}
	for i := at; i >= 0; i = parent[i] {
		siblings := children[parent[i]]
		if k := position(i); k+1 < len(siblings) {
			return nodes[siblings[k+1]].GetLabel().GetText(), true, true
		}
	}
	return "", true, false
}

// WatchLabelChanges yields every workspace whose root feed changed what an
// entry can target: a response's tree drawn, changed or retired. It is the
// server's cue to restate what the composer's entries target.
func (r *resolver) WatchLabelChanges(ctx context.Context) <-chan ids.WorkspaceID {
	return r.labelChanges.Subscribe(ctx)
}
