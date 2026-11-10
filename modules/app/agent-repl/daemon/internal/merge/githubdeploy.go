package merge

import (
	"cmp"
	"context"
	"slices"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/wsm"
)

// This file is the GITHUB MERGE QUEUE'S SHARE of the merge queue: a workspace
// whose owned pull request stands in its repository's GitHub merge queue is
// drawn as deploying, and in the sidebar's Merge Queue at the pull request's
// GitHub place. GitHub lands it, so nothing here runs a merge; the
// pull-request watcher (prwatch, deployments.go) tells the orchestrator every
// such pull request after each poll, and the orchestrator is the ONE place it
// meets the merges agent-repl runs:
//
//   - the merge facts every surface reads are composed here (surfaceFacts):
//     a merge agent-repl runs for the workspace wins, since it is what the
//     workspace's session is doing; otherwise the GitHub standing;
//   - the orchestrator's own facts (o.facts) stay the merges it runs, so its
//     admission, notices and composer gate never see a GitHub standing.
//
// The workspace's agent does nothing but own the pull request
// (wsm/pullrequestowner.go): its place and its status are the daemon's.

// GithubDeployment is one owned pull request standing in its GitHub merge
// queue.
type GithubDeployment struct {
	// Workspace owns the pull request.
	Workspace   ids.WorkspaceID
	PullRequest wsm.PullRequestKey
	// Place is its 1-based place in the GitHub queue; Total the queue's
	// length, every pull request included.
	Place, Total int
	// QueuedAt is when the pull request joined the queue.
	QueuedAt time.Time
}

// githubFacts are the merge facts a GitHub standing draws.
func githubFacts(d GithubDeployment) MergeFacts {
	return MergeFacts{State: StateMerging, Step: footer.StepGithubMergeQueue, QueuedAt: d.QueuedAt, GithubPlace: d.Place, GithubTotal: d.Total}
}

// byWorkspace keys DEPLOYMENTS by their owner, each owner at the pull request
// of its nearest to merging: a workspace owning two queued pull requests
// stands once, where it lands first.
func byWorkspace(deployments []GithubDeployment) map[ids.WorkspaceID]GithubDeployment {
	out := make(map[ids.WorkspaceID]GithubDeployment, len(deployments))
	for _, d := range deployments {
		if held, ok := out[d.Workspace]; ok && held.Place <= d.Place {
			continue
		}
		out[d.Workspace] = d
	}
	return out
}

// SetGithubDeployments installs DEPLOYMENTS, WHOLE: every workspace standing
// in a GitHub merge queue. A workspace no longer among them leaves the
// GitHub share. Each workspace whose standing changed is restated on the
// surfaces, and the roster's GitHub share is restated when any did.
func (o *orchestrator) SetGithubDeployments(ctx context.Context, deployments []GithubDeployment) {
	const op = "daemon.merge.github_deployments"
	next := byWorkspace(deployments)
	o.mu.Lock()
	prev := o.github
	o.github = next
	o.mu.Unlock()
	var changed []ids.WorkspaceID
	for ws, d := range next {
		if held, ok := prev[ws]; !ok || held != d {
			changed = append(changed, ws)
		}
	}
	for ws := range prev {
		if _, ok := next[ws]; !ok {
			changed = append(changed, ws)
		}
	}
	if len(changed) == 0 {
		return
	}
	slices.Sort(changed)
	for _, ws := range changed {
		o.surface(ws)
	}
	o.deps.Sidebar.SetGithubQueue(githubQueue(next))
	o.deps.Log.Global().Info(op, "the workspaces standing in GitHub merge queues changed; their statuses and the merge queue were restated",
		dlog.Context{"changed": ids.Strings(changed), "standing": len(next)})
}

// githubQueue is the roster's GitHub share of STANDING, in queue order.
func githubQueue(standing map[ids.WorkspaceID]GithubDeployment) []sidebar.GithubQueueEntry {
	out := make([]sidebar.GithubQueueEntry, 0, len(standing))
	for _, d := range standing {
		out = append(out, sidebar.GithubQueueEntry{Workspace: d.Workspace, Place: d.Place, Total: d.Total, QueuedAt: d.QueuedAt})
	}
	slices.SortFunc(out, func(a, b sidebar.GithubQueueEntry) int {
		if a.Place != b.Place {
			return a.Place - b.Place
		}
		return cmp.Compare(a.Workspace, b.Workspace)
	})
	return out
}

// surfaceFacts is THE ONE COMPOSITION of what the surfaces draw for WS: the
// merge the orchestrator runs for it, while one is in line or running;
// otherwise its GitHub standing, which is newer than any merge that already
// failed or landed; otherwise the orchestrator's last facts, or no merge.
func (o *orchestrator) surfaceFacts(ws ids.WorkspaceID) MergeFacts {
	o.mu.Lock()
	defer o.mu.Unlock()
	facts, ok := o.facts[ws]
	if ok && (facts.State == StateQueued || facts.State == StateMerging) {
		return facts
	}
	if d, standing := o.github[ws]; standing {
		return githubFacts(d)
	}
	if ok {
		return facts
	}
	return MergeFacts{State: "none"}
}

// surface installs WS's composed facts on both surfaces that draw them. The
// footer and the roster read the SAME value, so the two can never disagree.
func (o *orchestrator) surface(ws ids.WorkspaceID) {
	facts := o.surfaceFacts(ws)
	o.footerSetMerge(ws, facts)
	o.deps.Sidebar.SetMerge(ws, facts)
}
