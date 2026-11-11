package feed

import (
	"context"
	"fmt"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/pullrefs"

	frontendv1 "agentrepl/proto/frontend/v1"
)

// PullReferenceStore is the durable record of the pull requests each
// workspace's text named (wsm.DB), which a bare `#<number>` resolves against
// (pullrefs.Known).
type PullReferenceStore interface {
	RecordPullReference(ctx context.Context, id ids.WorkspaceID, ref pullrefs.Ref, at time.Time) error
	PullReferences(ctx context.Context, id ids.WorkspaceID) ([]pullrefs.Ref, error)
}

const (
	// opPullReferences is the operation learning a workspace's pull requests
	// is recorded under.
	opPullReferences = "daemon.feed.pull_references"
	// opPullReferencesRestore is the operation loading them is recorded under.
	opPullReferencesRestore = "daemon.feed.pull_references_restore"
)

// SetPullTarget installs the GitHub repository a workspace's `#<number>`
// references link into, nil for none. It lives beside the workspaces' states
// rather than on one, so a reset that empties the feed keeps it.
func (r *resolver) SetPullTarget(ws ids.WorkspaceID, target *pullrefs.Target) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.setPullTarget(ws, target)
}

func (r *resolver) setPullTarget(ws ids.WorkspaceID, target *pullrefs.Target) {
	if target == nil {
		delete(r.pullTargets, ws)
		r.logger(ws).Debug("daemon.feed.pull_target", "the workspace's pull request references link nowhere", dlog.Context{})
		return
	}
	r.pullTargets[ws] = *target
	r.logger(ws).Debug("daemon.feed.pull_target", "the workspace's pull request references link into its repository", dlog.Context{
		"owner": target.Owner, "repo": target.Repo,
	})
}

func (r *resolver) pullTarget(ws ids.WorkspaceID) *pullrefs.Target {
	target, ok := r.pullTargets[ws]
	if !ok {
		return nil
	}
	return &target
}

// LinkPulls links PARSED's pull request references as the workspace's feed
// links its own (linkPulls), for a settled text drawn outside the resolver:
// the prompt queue's mirror of an accepted prompt.
func (r *resolver) LinkPulls(ws ids.WorkspaceID, parsed *frontendv1.ParsedProse) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.linkPulls(ws, parsed, false)
}

// knownPulls answers the pull requests workspace WS's text has named, loading
// them from the durable record the first time this process asks. They live
// beside the workspaces' states rather than on one, so a reset that empties
// the feed keeps them. A load that fails is logged at ERROR and raised on the
// topbar, and the workspace starts from none: what its text names from now on
// is still learned and recorded.
func (r *resolver) knownPulls(ws ids.WorkspaceID) *pullrefs.Known {
	if known, ok := r.pullKnown[ws]; ok {
		return known
	}
	var refs []pullrefs.Ref
	if r.deps.PullReferences != nil {
		loaded, err := r.deps.PullReferences.PullReferences(context.Background(), ws)
		switch {
		case err != nil:
			r.logger(ws).Error(opPullReferencesRestore,
				"the pull requests the workspace's text named could not be loaded; its bare references link into its own repository",
				dlog.Context{"error": err.Error()})
			r.raiseWarningFor(ws, opPullReferencesRestore,
				fmt.Sprintf("the pull requests this workspace named could not be loaded, so its references may link to the wrong repository: %v", err))
		case len(loaded) > 0:
			r.logger(ws).Info(opPullReferencesRestore, "loaded the pull requests the workspace's text named",
				dlog.Context{"pull_requests": len(loaded)})
		}
		refs = loaded
	}
	known := pullrefs.NewKnown(refs)
	r.pullKnown[ws] = known
	return known
}

// learnPulls adds REFS to KNOWN, workspace WS's known pull requests, and
// records each new one durably. A recording that fails is logged at ERROR and
// raised on the topbar, since a daemon restart would no longer know it; the
// workspace still knows it for as long as this process runs.
func (r *resolver) learnPulls(ws ids.WorkspaceID, known *pullrefs.Known, refs []pullrefs.Ref) {
	for _, ref := range refs {
		if !known.Learn(ref) {
			continue
		}
		fields := dlog.Context{"owner": ref.Owner, "repo": ref.Repo, "number": ref.Number}
		if r.deps.PullReferences != nil {
			if err := r.deps.PullReferences.RecordPullReference(context.Background(), ws, ref, r.deps.Now()); err != nil {
				fields["error"] = err.Error()
				r.logger(ws).Error(opPullReferences,
					"a pull request the workspace's text named could not be recorded; a daemon restart will not know it", fields)
				r.raiseWarningFor(ws, opPullReferences,
					fmt.Sprintf("a pull request this workspace named could not be saved, so its references may link to the wrong repository after a restart: %v", err))
				continue
			}
		}
		r.logger(ws).Debug(opPullReferences, "learned a pull request the workspace's text named", fields)
	}
}

// linkPulls rewrites every drawn markdown of PARSED, workspace WS's text, with
// its references linked (pullrefs.Linkify). First every pull request the text
// names by its full address is learned (pullrefs.Refs), so a bare reference
// after it in the same text already means it; then every `<repo>#<number>`
// the linking names is learned too. The source is left as written: it is what
// a reply quotes and a search reads. Only the text's end can still grow while
// it is ARRIVING.
func (r *resolver) linkPulls(ws ids.WorkspaceID, parsed *frontendv1.ParsedProse, arriving bool) {
	known := r.knownPulls(ws)
	eachDrawnMarkdown(parsed, true, func(markdown string, ends bool) string {
		r.learnPulls(ws, known, pullrefs.Refs(markdown, arriving && ends))
		return markdown
	})
	links := pullrefs.Links{Home: r.pullTarget(ws), Known: known}
	eachDrawnMarkdown(parsed, true, func(markdown string, ends bool) string {
		linked, named := pullrefs.Linkify(markdown, links, arriving && ends)
		r.learnPulls(ws, known, named)
		return linked
	})
}
