package merge

import (
	"context"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// This file is the PULL REQUEST A MERGE'S HEAD NAMES: the target of a merge
// that deploys through a GitHub pull request opens it
// (frontend.v1.FeedMergeBranchLineTarget.pull_request). The daemon holds no
// pull request's head branch, so the pull request is read off the owned links
// of the workspaces the merge is for, and named only when there is exactly
// one: the head never guesses between several.

// opHeadPullRequest is the operation the head's pull request records under.
const opHeadPullRequest = "daemon.merge.head_pull_request"

// headPullRequest answers the url of the pull request WS's merge deploys:
// the one deployable owned pull request (wsm.DeployablePullRequests) of WS.
// Empty when it owns none, when it owns several (the head cannot tell which,
// recorded at INFO), when LINE has no target, and when its links cannot be
// read (recorded at ERROR).
func (o *orchestrator) headPullRequest(ws ids.WorkspaceID, line branchLine) string {
	if line.target == "" {
		return ""
	}
	owners := []ids.WorkspaceID{ws}
	var urls []string
	for _, owner := range owners {
		links, err := o.deps.DB.WorkspaceLinks(context.Background(), owner)
		if err != nil {
			o.log(context.Background(), ws).Error(opHeadPullRequest, "could not read a workspace's links; the merge head's target opens no pull request",
				dlog.Context{"workspace": string(ws), "owner": string(owner), "error": err.Error()})
			return ""
		}
		urls = append(urls, wsm.DeployablePullRequests(links)...)
	}
	if len(urls) > 1 {
		o.log(context.Background(), ws).Info(opHeadPullRequest, "the merge's workspaces own several pull requests; the merge head's target opens none of them",
			dlog.Context{"workspace": string(ws), "pull_requests": urls})
		return ""
	}
	if len(urls) == 0 {
		return ""
	}
	return urls[0]
}

// WorkspaceLinksChanged draws again every head this daemon drew for a merge
// of WS, so its target opens the pull request WS's links now name
// (Orchestrator).
func (o *orchestrator) WorkspaceLinksChanged(ws ids.WorkspaceID) {
	o.heads.mu.Lock()
	defer o.heads.mu.Unlock()
	for merging, head := range o.heads.last {
		if merging == ws {
			o.drawHead(merging, head, headLinks)
		}
	}
}
