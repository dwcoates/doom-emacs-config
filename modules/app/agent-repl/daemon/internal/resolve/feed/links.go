package feed

import (
	"agent-repld/internal/codespans"
	"agent-repld/internal/ids"

	frontendv1 "agentrepl/proto/frontend/v1"
)

// drawLinks draws every link of PARSED, workspace WS's text: first every link
// the text was written with loses its inline code markers (unmarkLinks), then
// its pull request references are linked (linkPulls), then its Slack channels
// (linkChannels), then every code span naming one of its skills (linkSkills).
// It is the one place a
// drawn text's links are drawn, a response's (parsedProse) and a prompt's
// (DrawUserBlocks) alike, so the two cannot draw links differently. Only the
// text's end can still grow while it is ARRIVING.
func (r *resolver) drawLinks(ws ids.WorkspaceID, parsed *frontendv1.ParsedProse, arriving bool) {
	unmarkLinks(parsed)
	r.linkPulls(ws, parsed, arriving)
	r.linkChannels(parsed, arriving)
	r.linkSkills(ws, parsed, arriving)
}

// DrawLinks draws PARSED's links as the workspace's feed draws its own
// (drawLinks), for a settled text drawn outside the resolver: the prompt
// queue's mirror of an accepted prompt.
func (r *resolver) DrawLinks(ws ids.WorkspaceID, parsed *frontendv1.ParsedProse) {
	r.prime(ws)
	r.mu.Lock()
	defer r.mu.Unlock()
	r.drawLinks(ws, parsed, false)
}

// unmarkLinks rewrites every drawn markdown of PARSED, its trees' too, with no
// inline code marker in any link (codespans.Unmark): a link's text draws as
// plain text and its href carries no backtick. The source is left as written:
// it is what a reply quotes and a search reads.
//
// Regression (2026-11-19): a backtick inside a bare URL reached the renderer,
// which drew it in the link's text and its href.
func unmarkLinks(parsed *frontendv1.ParsedProse) {
	eachDrawnMarkdown(parsed, true, func(markdown string, _ bool) string {
		return codespans.Unmark(markdown)
	})
}
