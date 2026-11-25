package feed

import (
	"context"
	"net/url"
	"strings"
	"sync"

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
//
// IT IS CALLED OFF THE RESOLVER'S LOCK, by one drainer, in the order the
// responses were drawn (drainResponseLinks): storing writes and then reads
// the workspace database, whose one connection may be busy for seconds, and
// a draw holding the lock across that wedged every session watcher's route
// and every OpenFeed behind it (2026-11-23, lockwatch stall on
// feed.resolver).
type ResponseLinkSink interface {
	AddResponseLinks(ctx context.Context, ws ids.WorkspaceID, urls []string) error
}

// opResponseLinks is the operation storing a response's links is recorded
// under.
const opResponseLinks = "daemon.feed.response_links"

// responseLinkBatch is the fresh urls one settled response's draw handed the
// outbox for workspace ws.
type responseLinkBatch struct {
	ws   ids.WorkspaceID
	urls []string
}

// responseLinkOutbox is the batches drawn but not yet handed to the sink, in
// draw order, and whether the one drainer is running. Its own mutex guards
// it, never the resolver's, so a draw hands a batch over at once.
type responseLinkOutbox struct {
	mu       sync.Mutex
	batches  []responseLinkBatch
	draining bool
}

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

// noteResponseLinks hands the outbox every web link TEXT names that this
// process has not already handed over for workspace WS, and returns at once:
// the sink is called off the lock, by the drainer. A response is drawn again
// on every replay and redraw, so the urls handed over are remembered the
// moment they are handed, before the sink has them: a redraw while the store
// is pending hands none of them twice, and a link the workspace removed is
// not stored again by the next draw. A sink failure un-remembers them
// (settleResponseLinks), so the next draw tries them again. Called with r.mu
// held.
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
	if seen == nil {
		seen = map[string]bool{}
		r.responseLinksSeen[ws] = seen
	}
	for _, u := range fresh {
		seen[u] = true
	}
	r.handResponseLinks(responseLinkBatch{ws: ws, urls: fresh})
}

// handResponseLinks puts BATCH on the outbox and starts the drainer unless
// it is running. Exactly one drainer runs at a time: the flag is set and
// cleared under the outbox's mutex, in the same critical section that finds
// the outbox empty, so a batch put while the drainer is finishing is never
// left behind.
func (r *resolver) handResponseLinks(batch responseLinkBatch) {
	o := &r.responseLinkOutbox
	o.mu.Lock()
	o.batches = append(o.batches, batch)
	start := !o.draining
	o.draining = true
	o.mu.Unlock()
	if start {
		r.deps.Go(r.drainResponseLinks)
	}
}

// drainResponseLinks hands the sink every batch on the outbox, one at a time
// and in draw order, until it is empty. It runs OFF r.mu; only the step after
// each sink call returns takes it (settleResponseLinks). The sink's context
// is unbounded, as it was when the call ran under the lock.
func (r *resolver) drainResponseLinks() {
	o := &r.responseLinkOutbox
	for {
		o.mu.Lock()
		if len(o.batches) == 0 {
			o.draining = false
			o.mu.Unlock()
			return
		}
		batch := o.batches[0]
		o.batches = o.batches[1:]
		o.mu.Unlock()
		err := r.deps.ResponseLinks.AddResponseLinks(context.Background(), batch.ws, batch.urls)
		r.settleResponseLinks(batch, err)
	}
}

// settleResponseLinks records the sink's answer for BATCH under r.mu, which
// the workspace's logger and the remembered urls need. A failure is logged
// at ERROR and raised on the topbar, and the urls are un-remembered so the
// next draw hands them again.
func (r *resolver) settleResponseLinks(batch responseLinkBatch, err error) {
	r.mu.Lock()
	defer r.mu.Unlock()
	if err != nil {
		r.logger(batch.ws).Error(opResponseLinks, "the web links a response named could not be stored as the workspace's links",
			dlog.Context{"urls": batch.urls, "error": err.Error()})
		seen := r.responseLinksSeen[batch.ws]
		for _, u := range batch.urls {
			delete(seen, u)
		}
		return
	}
	r.logger(batch.ws).Debug(opResponseLinks, "stored the web links a response named as the workspace's links", dlog.Context{"urls": batch.urls})
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
