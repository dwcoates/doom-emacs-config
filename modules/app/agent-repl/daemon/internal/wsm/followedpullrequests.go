package wsm

import (
	"context"
	"database/sql"
	"time"

	"agent-repld/internal/dlog"
)

// This file is how a FOLLOWER LEARNS ITS FOLLOWED WORKSPACE'S PULL REQUESTS
// MERGED: a workspace that subscribes to another (SubscribeWorkspace) is
// subscribed by the daemon to the merge of each pull request the followed
// workspace owns (PullRequestOriginFollowedWorkspace), and holds it as a
// reference link. It is told the merge, and the end every subscription is
// told, never the queue, the checks or the comments on the way there (owner
// request, 2026-11-24).
//
// IT IS TAKEN IN THE WRITE THAT MAKES IT DUE, from either side: the
// subscription to the workspace taken while the workspace owns the pull
// request (SubscribeWorkspace), or the pull request stored as an owned link
// while the workspace is followed (SetWorkspaceLink). Both writes are one
// transaction each, so no ordering of the two can leave a follower without
// its pull request.
//
// A follower's own hold on the pull request is never changed: a link it
// already holds to the url keeps its label and ownership, and a subscription
// it already holds to the pull request keeps its events and origin. A pull
// request whose owned subscription already recorded its end is not followed,
// since its merge was already told or never comes.

// followedPullRequestEvents is what a follower's subscription to a followed
// workspace's pull request asks for: the merge alone.
func followedPullRequestEvents() PullRequestEvents {
	return NewPullRequestEvents(PullRequestEventMerged)
}

// FollowedPullRequest is one pull request a follower was subscribed to as
// its followed workspace's (followedPullRequestsInTx).
type FollowedPullRequest struct {
	// Subscriber is the follower.
	Subscriber WorkspaceID
	// PullRequest is the followed workspace's owned pull request.
	PullRequest PullRequestKey
}

// followOwnedPullRequestsInTx subscribes SUBSCRIBER to the merge of every
// owned pull request of FOLLOWED whose owned subscription recorded no end,
// stores each as SUBSCRIBER's reference link under FOLLOWED's label, and
// answers the pull requests it newly subscribed. ONLY, when set, limits it to
// that one pull request.
func followOwnedPullRequestsInTx(ctx context.Context, tx *sql.Tx, subscriber, followed WorkspaceID, only *PullRequestKey, at time.Time) ([]FollowedPullRequest, error) {
	query := `SELECT l.url, l.label, l.pr_owner, l.pr_repo, l.pr_number
		FROM workspace_links AS l
		LEFT JOIN pull_request_subscriptions AS p ON p.subscriber = l.workspace_id AND p.owner = l.pr_owner AND p.repo = l.pr_repo AND p.number = l.pr_number
		WHERE l.workspace_id = ? AND l.owned = 1 AND l.pr_number IS NOT NULL AND p.ended IS NULL`
	args := []any{followed}
	if only != nil {
		query += ` AND l.pr_owner = ? AND l.pr_repo = ? AND l.pr_number = ?`
		args = append(args, only.Owner, only.Repo, only.Number)
	}
	type owned struct {
		url, label string
		pr         PullRequestKey
	}
	links, err := listRows(ctx, tx, query+` ORDER BY l.set_seq`, func(row interface{ Scan(...any) error }) (owned, error) {
		var o owned
		err := row.Scan(&o.url, &o.label, &o.pr.Owner, &o.pr.Repo, &o.pr.Number)
		return o, err
	}, args...)
	if err != nil {
		return nil, err
	}
	var out []FollowedPullRequest
	for _, link := range links {
		if _, err := addReferenceLinkIfAbsentInTx(ctx, tx, subscriber, link.url, link.label, &link.pr); err != nil {
			return nil, err
		}
		taken, err := insertPullRequestSubscriptionIfAbsentInTx(ctx, tx, PullRequestSubscription{Subscriber: subscriber, PullRequest: link.pr,
			Events: followedPullRequestEvents(), Origin: PullRequestOriginFollowedWorkspace, SubscribedAt: at})
		if err != nil {
			return nil, err
		}
		if taken {
			out = append(out, FollowedPullRequest{Subscriber: subscriber, PullRequest: link.pr})
		}
	}
	return out, nil
}

// followOwnedPullRequestByFollowersInTx subscribes every workspace that holds
// a subscription to OWNER to the merge of PR, which OWNER owns
// (followOwnedPullRequestsInTx), and answers the followers it newly
// subscribed.
func followOwnedPullRequestByFollowersInTx(ctx context.Context, tx *sql.Tx, owner WorkspaceID, pr PullRequestKey, at time.Time) ([]FollowedPullRequest, error) {
	followers, err := listRows(ctx, tx, `SELECT subscriber FROM workspace_subscriptions WHERE followed = ? ORDER BY subscriber`,
		func(row interface{ Scan(...any) error }) (WorkspaceID, error) {
			var id WorkspaceID
			err := row.Scan(&id)
			return id, err
		}, owner)
	if err != nil {
		return nil, err
	}
	var out []FollowedPullRequest
	for _, follower := range followers {
		taken, err := followOwnedPullRequestsInTx(ctx, tx, follower, owner, &pr, at)
		if err != nil {
			return nil, err
		}
		out = append(out, taken...)
	}
	return out, nil
}

// logFollowedPullRequests records each subscription OP's write took for a
// follower to its followed workspace's pull request, once it committed.
func (s *store) logFollowedPullRequests(op string, taken []FollowedPullRequest) {
	for _, f := range taken {
		s.log.Info(op, "subscribed a follower to the merge of a pull request its followed workspace owns",
			dlog.Context{"subscriber": string(f.Subscriber), "pull_request": f.PullRequest.String(), "events": followedPullRequestEvents().String()})
	}
}
