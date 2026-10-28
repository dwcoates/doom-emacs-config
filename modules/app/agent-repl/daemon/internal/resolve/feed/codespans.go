package feed

import (
	"agent-repld/internal/codespans"
	"agent-repld/internal/ids"

	frontendv1 "agentrepl/proto/frontend/v1"
)

// RecentFinalResponses is how many of a workspace's latest final responses
// supply the inline code a prompt is marked up with.
const RecentFinalResponses = 2

// RecentCodeSpans answers the distinct inline code spans of the workspace's
// last RecentFinalResponses final responses (codespans.Extract), newest first.
func (r *resolver) RecentCodeSpans(ws ids.WorkspaceID) []string {
	r.mu.Lock()
	defer r.mu.Unlock()
	s, ok := r.workspaces[ws]
	if !ok {
		return nil
	}
	return r.recentCodeSpans(s)
}

func (r *resolver) recentCodeSpans(s *wsState) []string {
	var spans []string
	seen := map[string]bool{}
	finals := s.finalAnswers
	for i := len(finals) - 1; i >= 0 && i >= len(finals)-RecentFinalResponses; i-- {
		for _, span := range codespans.Extract(s.answerMarkdown[finals[i].GetValue()]) {
			if !seen[span] {
				seen[span] = true
				spans = append(spans, span)
			}
		}
	}
	return spans
}

// markCodeSpans rewrites every drawn markdown of a PROMPT's PARSED with the
// plain occurrences of SPANS marked up as inline code (codespans.Markup). The
// source is left as written: it is what a reply quotes and a search reads.
func markCodeSpans(parsed *frontendv1.ParsedProse, spans []string) {
	if len(spans) == 0 {
		return
	}
	for _, block := range parsed.GetBlocks() {
		switch b := block.GetBlock().(type) {
		case *frontendv1.ProseBlock_Plain:
			b.Plain.Markdown = codespans.Markup(b.Plain.GetMarkdown(), spans)
		case *frontendv1.ProseBlock_Entries:
			for _, node := range b.Entries.GetNodes() {
				if text := node.GetAnswer().GetText(); text != nil {
					text.Markdown = codespans.Markup(text.GetMarkdown(), spans)
				}
			}
		}
	}
}
