package feed

import (
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/pullrefs"

	frontendv1 "agentrepl/proto/frontend/v1"
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

// PullTarget answers a copy of the target SetPullTarget installed, nil for
// none.
func (r *resolver) PullTarget(ws ids.WorkspaceID) *pullrefs.Target {
	r.mu.Lock()
	defer r.mu.Unlock()
	return r.pullTarget(ws)
}

func (r *resolver) pullTarget(ws ids.WorkspaceID) *pullrefs.Target {
	target, ok := r.pullTargets[ws]
	if !ok {
		return nil
	}
	return &target
}

// linkPulls rewrites every drawn markdown of PARSED with its references linked
// into TARGET (pullrefs.Linkify); a nil target links nothing. The source is
// left as written: it is what a reply quotes and a search reads. Only the last
// block's end can still grow while a text is ARRIVING.
func linkPulls(parsed *frontendv1.ParsedProse, target *pullrefs.Target, arriving bool) {
	if target == nil {
		return
	}
	blocks := parsed.GetBlocks()
	for i, block := range blocks {
		growing := arriving && i == len(blocks)-1
		switch b := block.GetBlock().(type) {
		case *frontendv1.ProseBlock_Plain:
			b.Plain.Markdown = pullrefs.Linkify(b.Plain.GetMarkdown(), *target, growing)
		case *frontendv1.ProseBlock_Tree:
			nodes := b.Tree.GetNodes()
			for j, node := range nodes {
				if text := node.GetText(); text != nil {
					text.Markdown = pullrefs.Linkify(text.GetMarkdown(), *target, growing && j == len(nodes)-1)
				}
			}
		case *frontendv1.ProseBlock_Entries:
			for _, node := range b.Entries.GetNodes() {
				if text := node.GetAnswer().GetText(); text != nil {
					text.Markdown = pullrefs.Linkify(text.GetMarkdown(), *target, false)
				}
			}
		}
	}
}
