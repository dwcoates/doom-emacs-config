package prwatch

import (
	"context"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/merge"
	"agent-repld/internal/wsm"
)

// The GITHUB DEPLOYMENTS: each owned pull request (its one owned-link
// subscription, wsm/pullrequestowner.go) standing in its GitHub merge queue,
// told to the merge orchestrator WHOLE after every poll, which draws its
// owner at the pull request's GitHub place in the agent-repl merge queue and
// as deploying (merge.GithubDeployment). The workspace does nothing but own
// the pull request: every place is the daemon's reading of GitHub.

// opDeployments is the operation the deployments record under.
const opDeployments = "daemon.prwatch.github_deployments"

// DeploymentSink is told every GitHub deployment, whole: the merge
// orchestrator (merge.Orchestrator.SetGithubDeployments).
type DeploymentSink interface {
	SetGithubDeployments(ctx context.Context, deployments []merge.GithubDeployment)
}

// placed is an owned pull request's place as its latest successful read
// found it, held between polls so a read that fails or waits out a rate
// limit leaves the place where it was last seen.
type placed struct {
	// Place and Total are the 1-based place and the queue's length.
	Place, Total int
	// QueuedAt is when the pull request joined the queue.
	QueuedAt time.Time
}

// ownerOf answers the subscriber of SUBS's owned-link subscription: the pull
// request's one owner, when it has one.
func ownerOf(subs []wsm.PullRequestSubscription) (wsm.PullRequestSubscription, bool) {
	for _, sub := range subs {
		if sub.Origin == wsm.PullRequestOriginOwnedLink {
			return sub, true
		}
	}
	return wsm.PullRequestSubscription{}, false
}

// placeOf answers where SNAP's read found PR in its queue; nil when it is in
// none. A queued pull request whose timeline names no joining is recorded at
// ERROR and answered nil: its time in the queue cannot be drawn, and a made-up
// one would be a wrong clock.
func (w *Watcher) placeOf(pr wsm.PullRequestKey, snap snapshot) *placed {
	q := snap.Queue
	if q == nil || q.Entry == nil || q.Position == nil {
		return nil
	}
	var joined time.Time
	for _, e := range q.Events {
		if !e.Removed {
			joined = e.At
		}
	}
	if joined.IsZero() {
		w.deps.Log.Error(opDeployments, "a queued pull request's timeline names no joining of the queue, so its owner is not drawn in the merge queue",
			dlog.Context{"pull_request": pr.String(), "events": len(q.Events), "invariant_violation": "a queued pull request's timeline holds its latest joining"})
		return nil
	}
	return &placed{Place: int(q.Position.Ahead) + 1, Total: int(q.Position.Total), QueuedAt: joined}
}

// tellDeployments tells the sink every owned pull request of SUBS standing in
// a GitHub merge queue, at its last read place, in queue order per queue.
// A pull request no longer owned leaves the held places.
func (w *Watcher) tellDeployments(ctx context.Context, order []wsm.PullRequestKey, byPR map[wsm.PullRequestKey][]wsm.PullRequestSubscription) {
	owned := map[wsm.PullRequestKey]bool{}
	var out []merge.GithubDeployment
	for _, pr := range order {
		owner, ok := ownerOf(byPR[pr])
		if !ok {
			continue
		}
		owned[pr] = true
		p := w.places[pr]
		if p == nil {
			continue
		}
		out = append(out, merge.GithubDeployment{Workspace: owner.Subscriber, PullRequest: pr, Place: p.Place, Total: p.Total, QueuedAt: p.QueuedAt})
	}
	for pr := range w.places {
		if !owned[pr] {
			delete(w.places, pr)
		}
	}
	w.deps.Log.Debug(opDeployments, "told the merge queue every owned pull request standing in a GitHub merge queue", dlog.Context{"deployments": len(out)})
	w.deps.Deployments.SetGithubDeployments(ctx, out)
}
