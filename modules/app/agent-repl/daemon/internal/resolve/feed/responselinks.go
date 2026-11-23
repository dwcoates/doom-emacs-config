package feed

import (
	"context"
	"fmt"
	"net/url"
	"strings"

	"agent-repld/internal/codespans"
	"agent-repld/internal/dlog"
	"agent-repld/internal/externalbrowser"
	"agent-repld/internal/ids"
	"agent-repld/internal/mdinline"
	"agent-repld/internal/prose"

	frontendv1 "agentrepl/proto/frontend/v1"
)

// ResponseLinkSink stores the web links a workspace's responses named as
// reference links of the workspace (workspace.Verbs, through the daemon's
// graph). A url the workspace already holds is left exactly as it is, so a
// link the workspace stored itself, owned or not, is never interfered with.
type ResponseLinkSink interface {
	AddResponseLinks(ctx context.Context, ws ids.WorkspaceID, urls []string) error
}

// opResponseLinks is the operation storing a response's links is recorded
// under.
const opResponseLinks = "daemon.feed.response_links"

// responseProse is parsedProse for an agent's response. Once the response is
// SETTLED, every web link its text names is stored as a reference link of the
// workspace (noteResponseLinks); a response still arriving may end mid-url,
// so it stores none.
func (r *resolver) responseProse(s *wsState, text string, stage prose.Stage) *frontendv1.ParsedProse {
	parsed := r.parsedProse(s, text, stage)
	if stage == prose.Settled {
		r.noteResponseLinks(s.id, text)
	}
	return parsed
}

// noteResponseLinks hands the sink every web link TEXT names that this
// process has not already handed it for workspace WS. A response is drawn
// again on every replay and redraw, so the urls handed over are remembered:
// a link the workspace removed is not stored again by the next draw. A sink
// failure is logged at ERROR and raised on the topbar, and the urls stay
// unremembered so the next draw tries them again.
func (r *resolver) noteResponseLinks(ws ids.WorkspaceID, text string) {
	if r.deps.ResponseLinks == nil {
		return
	}
	seen := r.responseLinksSeen[ws]
	var fresh []string
	for _, u := range responseURLs(text) {
		if !seen[u] {
			fresh = append(fresh, u)
		}
	}
	if len(fresh) == 0 {
		return
	}
	if err := r.deps.ResponseLinks.AddResponseLinks(context.Background(), ws, fresh); err != nil {
		r.logger(ws).Error(opResponseLinks, "the web links a response named could not be stored as the workspace's links",
			dlog.Context{"urls": fresh, "error": err.Error()})
		r.raiseWarningFor(ws, opResponseLinks, fmt.Sprintf("the links this workspace's response named could not be saved: %v", err))
		return
	}
	if seen == nil {
		seen = map[string]bool{}
		r.responseLinksSeen[ws] = seen
	}
	for _, u := range fresh {
		seen[u] = true
	}
	r.logger(ws).Debug(opResponseLinks, "stored the web links a response named as the workspace's links", dlog.Context{"urls": fresh})
}

// responseURLs is every http or https url TEXT links, in the order it first
// appears: bare urls, autolinks and inline or reference link destinations,
// read as the renderer reads them (mdinline) with inline code markers removed
// (codespans.Unmark). Code spans and fenced code name no link.
func responseURLs(text string) []string {
	doc := mdinline.Read(codespans.Unmark(text))
	var out []string
	seen := map[string]bool{}
	for _, line := range doc.Lines {
		if line.Kind != mdinline.Prose {
			continue
		}
		for _, span := range doc.Spans(line.Text) {
			if span.Kind != mdinline.URL && span.Kind != mdinline.Autolink && span.Kind != mdinline.Link {
				continue
			}
			dest := strings.TrimSuffix(strings.TrimPrefix(span.Dest, "<"), ">")
			if externalbrowser.Validate(dest) != nil {
				continue
			}
			if parsed, err := url.Parse(dest); err != nil || parsed.Host == "" {
				continue
			}
			if !seen[dest] {
				seen[dest] = true
				out = append(out, dest)
			}
		}
	}
	return out
}
