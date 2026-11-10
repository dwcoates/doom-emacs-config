package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
)

// This file is ONE OWNER PER PULL REQUEST: the workspace that owns a pull
// request (an owned pull-request link, workspacelinks.go) holds its
// owned-link subscription, and no other open workspace may. The owner is the
// workspace the daemon draws at the pull request's place in its GitHub merge
// queue (merge.GithubDeployment), so two owners would stand at one place.
//
// THE STORE ENFORCES IT: a partial unique index admits one owned-link
// subscription per pull request, so no schedule of writers can record two.
// A closed workspace's subscriptions are deleted in the transaction that
// closes it (endWorkspaceSubscriptionsInTx), so its ownership ends with it.

// onePullRequestOwnerDDL is the layout-111 step, in three statements:
//
//  1. a workspace that owns a pull request through its link but holds the
//     REQUESTED subscription to it (taken before it owned the link) has that
//     row made the owned-link one, asking for every event (126,
//     AllPullRequestEvents), as storing the link owned does now;
//  2. every owned-link subscription but the earliest of each pull request
//     (the earliest subscribed, then the lowest subscriber id) is demoted to
//     a REQUESTED one: it keeps its notices and no longer owns;
//  3. the index is made.
//
// IT IS BREAKING: the previous build stores a second owner, which the index
// refuses.
const onePullRequestOwnerDDL = `
UPDATE pull_request_subscriptions SET origin = 'owned_link', events = events | 126
 WHERE origin = 'requested' AND EXISTS (
   SELECT 1 FROM workspace_links l WHERE l.workspace_id = pull_request_subscriptions.subscriber AND l.owned = 1
     AND l.pr_owner = pull_request_subscriptions.owner AND l.pr_repo = pull_request_subscriptions.repo AND l.pr_number = pull_request_subscriptions.number);
UPDATE pull_request_subscriptions SET origin = 'requested'
 WHERE origin = 'owned_link' AND EXISTS (
   SELECT 1 FROM pull_request_subscriptions o WHERE o.origin = 'owned_link'
     AND o.owner = pull_request_subscriptions.owner AND o.repo = pull_request_subscriptions.repo AND o.number = pull_request_subscriptions.number
     AND (o.subscribed_at < pull_request_subscriptions.subscribed_at
       OR (o.subscribed_at = pull_request_subscriptions.subscribed_at AND o.subscriber < pull_request_subscriptions.subscriber)));
CREATE UNIQUE INDEX pull_request_subscriptions_one_owner ON pull_request_subscriptions(owner, repo, number) WHERE origin = 'owned_link';
`

// PullRequestOwnedElsewhereError refuses storing a pull request owned while
// another open workspace owns it.
type PullRequestOwnedElsewhereError struct {
	PullRequest PullRequestKey
	// Owner is the workspace that owns it, OwnerName its name.
	Owner     WorkspaceID
	OwnerName string
}

func (e *PullRequestOwnedElsewhereError) Error() string {
	return fmt.Sprintf("wsm: %s is owned by workspace %s (%s)", e.PullRequest, e.OwnerName, e.Owner)
}

// refuseOtherOwnerInTx answers a *PullRequestOwnedElsewhereError when a
// workspace other than ID owns PR. It runs in the write that would make ID
// its owner, so no owner can be recorded between the check and the write;
// the index is the backstop that makes a second owner unrepresentable.
func refuseOtherOwnerInTx(ctx context.Context, tx *sql.Tx, id WorkspaceID, pr PullRequestKey) error {
	var owner WorkspaceID
	err := tx.QueryRowContext(ctx,
		`SELECT subscriber FROM pull_request_subscriptions WHERE origin = ? AND owner = ? AND repo = ? AND number = ? AND subscriber != ?`,
		string(PullRequestOriginOwnedLink), pr.Owner, pr.Repo, pr.Number, id).Scan(&owner)
	switch {
	case errors.Is(err, sql.ErrNoRows):
		return nil
	case err != nil:
		return err
	}
	var name string
	if err := scanWorkspaceRow(ctx, tx, owner, "name", &name); err != nil {
		return err
	}
	return &PullRequestOwnedElsewhereError{PullRequest: pr, Owner: owner, OwnerName: name}
}

// ownHeldSubscriptionInTx makes ID's subscription to PR, when it holds one
// taken for another reason (a requested one), the owned-link subscription,
// asking for every event: the row an owned link's subscription is. A
// workspace holds one subscription per pull request, so owning the link
// takes over the one it held.
func ownHeldSubscriptionInTx(ctx context.Context, tx *sql.Tx, id WorkspaceID, pr PullRequestKey) error {
	_, err := tx.ExecContext(ctx,
		`UPDATE pull_request_subscriptions SET origin = ?, events = events | ? WHERE subscriber = ? AND owner = ? AND repo = ? AND number = ? AND origin != ?`,
		string(PullRequestOriginOwnedLink), int64(AllPullRequestEvents()), id, pr.Owner, pr.Repo, pr.Number, string(PullRequestOriginOwnedLink))
	return err
}
