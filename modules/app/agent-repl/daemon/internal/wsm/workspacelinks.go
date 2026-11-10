package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"

	"agent-repld/internal/dlog"
)

// workspaceLinksDDL is the layout-85 addition: every link an agent stored for
// a workspace's task (agentrepl.v1.ChangeWorkspaceLinks), each with its label,
// and whether the topbar summary's menu shows it. set_seq orders a
// workspace's links by when each was last set, so "most recently set first"
// is an ORDER BY rather than a timestamp tie broken by luck. A close leaves
// the links standing: a reopened workspace has the same task. IT IS ADDITIVE:
// a new table the previous build never names.
const workspaceLinksDDL = `
CREATE TABLE workspace_links (
  workspace_id TEXT NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  url          TEXT NOT NULL,
  label        TEXT NOT NULL,
  set_seq      INTEGER NOT NULL,
  shown        INTEGER NOT NULL,
  PRIMARY KEY (workspace_id, url)
);
`

// linkOwnershipDDL is the layout-92 addition: whether the workspace's work
// made what each link points at (agentrepl.v1.WorkspaceLinkOwnership). IT IS
// ADDITIVE: the previous build's INSERT names no such column, so a link it
// stores takes 0, a reference, which is what every link set before links
// carried ownership reads as.
const linkOwnershipDDL = `
ALTER TABLE workspace_links ADD COLUMN owned INTEGER NOT NULL DEFAULT 0;
`

// ShownLinkLimit is how many of a workspace's links the topbar summary's menu
// shows at most.
const ShownLinkLimit = 5

// WorkspaceLink is one stored link.
type WorkspaceLink struct {
	// URL is the link, verbatim as it was set; the key a relabel, a removal
	// or a show names it by.
	URL string
	// Label is what the menu draws for it.
	Label string
	// Shown is true while the menu shows the link.
	Shown bool
	// Owned is true when the workspace's work made what the link points at,
	// false when it only references it (agentrepl.v1.WorkspaceLinkOwnership).
	Owned bool
	// PullRequest is the GitHub pull request the url names; nil for any
	// other url.
	PullRequest *PullRequestKey
	// Follow is the workspace's subscription to PullRequest, ended or not;
	// nil when it holds none.
	Follow *PullRequestSubscription
	// Announces is the url of the stored link this owned link announces
	// (agentrepl.v1.WorkspaceLinkOwned.announces), empty when it announces
	// nothing.
	Announces string
}

// LinkSet is one store or relabel of a link (SetWorkspaceLink).
type LinkSet struct {
	URL   string
	Label string
	// Owned is the link's ownership.
	Owned bool
	// Announces is the url of the link this owned link announces, which
	// must be a link the workspace already holds (an
	// *UnknownAnnouncedLinkError otherwise); empty for none.
	Announces string
	// PullRequest is the GitHub pull request the url names; nil for any
	// other url.
	PullRequest *PullRequestKey
	// Subscribe is the subscription an owned pull request takes when the
	// workspace holds none to it; nil leaves subscriptions untouched. Its
	// subscriber and pull request are the link's. One whose Ended is set is
	// recorded ended: the pull request had already merged or closed, so it is
	// never followed and the link says how it ended.
	Subscribe *PullRequestSubscription
}

// UnknownLinkError is a removal or a show naming a url the workspace does not
// hold. It is the store answering what was asked, never a failure.
type UnknownLinkError struct {
	// URL is the url as the caller named it.
	URL string
}

func (e *UnknownLinkError) Error() string {
	return fmt.Sprintf("wsm: the workspace holds no link %q", e.URL)
}

// errTooManyShown is a show naming more links than ShownLinkLimit. The verb
// refuses that before it reaches the store, so here it is a caller's defect.
var errTooManyShown = errors.New("wsm: a show names at most ShownLinkLimit links")

// WorkspaceLinks answers every link workspace ID holds, most recently set
// first.
func (s *store) WorkspaceLinks(ctx context.Context, id WorkspaceID) ([]WorkspaceLink, error) {
	var out []WorkspaceLink
	err := s.read(ctx, "daemon.wsm.workspace_links", dlog.Context{"workspace": string(id)}, func(ctx context.Context) error {
		var err error
		out, err = linksOf(ctx, s.q(ctx), id)
		return err
	})
	return out, err
}

// SetWorkspaceLink stores SET's url under its label and ownership, or
// relabels it when ID already holds it, making it the most recently set
// either way. A new link is shown while fewer than ShownLinkLimit are; a
// relabeled one keeps its placement. In the same transaction an owned pull
// request takes SET.Subscribe when ID holds no subscription to it, and a link
// no longer owned ends the subscription its ownership took. It answers every
// link ID now holds, read in the same transaction.
func (s *store) SetWorkspaceLink(ctx context.Context, id WorkspaceID, set LinkSet) ([]WorkspaceLink, error) {
	const op = "daemon.wsm.set_workspace_link"
	url, label := set.URL, set.Label
	fields := dlog.Context{"workspace": string(id), "url": url, "label": label, "owned": set.Owned, "announces": set.Announces}
	if set.Subscribe != nil && (!set.Owned || set.PullRequest == nil || set.Subscribe.PullRequest != *set.PullRequest || set.Subscribe.Subscriber != id) {
		err := fmt.Errorf("wsm: link of %s: a subscription is taken only for the owned pull request the link names", id)
		s.log.Error(op, "refused a link its caller should never have made", withError(fields, err))
		return nil, err
	}
	if set.Announces != "" && (!set.Owned || set.Announces == url) {
		err := fmt.Errorf("wsm: link of %s: only an owned link announces, and never itself", id)
		s.log.Error(op, "refused a link its caller should never have made", withError(fields, err))
		return nil, err
	}
	if url == "" || label == "" {
		err := fmt.Errorf("wsm: link of %s: a link names its url and its label", id)
		s.log.Error(op, "refused a link its caller should never have made", withError(fields, err))
		return nil, err
	}
	var out []WorkspaceLink
	err := s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		if err := requireWorkspaceRow(ctx, tx, id); err != nil {
			return err
		}
		var next, shown int64
		if err := tx.QueryRowContext(ctx,
			`SELECT COALESCE(MAX(set_seq), 0) + 1, COALESCE(SUM(shown), 0) FROM workspace_links WHERE workspace_id = ?`,
			id).Scan(&next, &shown); err != nil {
			return err
		}
		var announces any
		if set.Announces != "" {
			var held int
			if err := tx.QueryRowContext(ctx, `SELECT COUNT(*) FROM workspace_links WHERE workspace_id = ? AND url = ?`, id, set.Announces).Scan(&held); err != nil {
				return err
			}
			if held == 0 {
				return &UnknownAnnouncedLinkError{URL: set.Announces}
			}
			announces = set.Announces
		}
		var owner, repo, number any
		if pr := set.PullRequest; pr != nil {
			owner, repo, number = pr.Owner, pr.Repo, pr.Number
		}
		if _, err := tx.ExecContext(ctx,
			`INSERT INTO workspace_links (workspace_id, url, label, set_seq, shown, owned, pr_owner, pr_repo, pr_number, announces) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
			 ON CONFLICT(workspace_id, url) DO UPDATE SET label = excluded.label, set_seq = excluded.set_seq, owned = excluded.owned, announces = excluded.announces`,
			id, url, label, next, shown < ShownLinkLimit, set.Owned, owner, repo, number, announces); err != nil {
			return err
		}
		if err := s.followLinkInTx(ctx, tx, id, set); err != nil {
			return err
		}
		var err error
		out, err = linksOf(ctx, tx, id)
		return err
	})
	return out, err
}

// RemoveWorkspaceLink removes URL from ID's links, ending the subscription
// its ownership took, and answers the links ID still holds. A url ID does not
// hold is an *UnknownLinkError.
func (s *store) RemoveWorkspaceLink(ctx context.Context, id WorkspaceID, url string) ([]WorkspaceLink, error) {
	const op = "daemon.wsm.remove_workspace_link"
	var out []WorkspaceLink
	err := s.write(ctx, op, dlog.Context{"workspace": string(id), "url": url}, func(ctx context.Context, tx *sql.Tx) error {
		if err := requireWorkspaceRow(ctx, tx, id); err != nil {
			return err
		}
		if err := endOwnedFollowsInTx(ctx, tx, id, `AND url = ?`, url); err != nil {
			return err
		}
		res, err := tx.ExecContext(ctx, `DELETE FROM workspace_links WHERE workspace_id = ? AND url = ?`, id, url)
		if err != nil {
			return err
		}
		removed, err := touchedRow(res)
		if err != nil {
			return err
		}
		if !removed {
			return &UnknownLinkError{URL: url}
		}
		out, err = linksOf(ctx, tx, id)
		return err
	})
	return out, err
}

// ShowWorkspaceLinks makes URLS exactly the links ID's menu shows, every
// other one unshown, answering every link ID holds. Every url must be one ID
// holds (an *UnknownLinkError names the first that is not, and nothing
// changes), and there are one to ShownLinkLimit of them, without repeats.
func (s *store) ShowWorkspaceLinks(ctx context.Context, id WorkspaceID, urls []string) ([]WorkspaceLink, error) {
	const op = "daemon.wsm.show_workspace_links"
	fields := dlog.Context{"workspace": string(id), "urls": urls}
	if len(urls) == 0 || len(urls) > ShownLinkLimit || hasRepeat(urls) {
		err := fmt.Errorf("wsm: show of %s naming %d urls: %w", id, len(urls), errTooManyShown)
		s.log.Error(op, "refused a show its caller should never have made", withError(fields, err))
		return nil, err
	}
	var out []WorkspaceLink
	err := s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		if err := requireWorkspaceRow(ctx, tx, id); err != nil {
			return err
		}
		if _, err := tx.ExecContext(ctx, `UPDATE workspace_links SET shown = 0 WHERE workspace_id = ?`, id); err != nil {
			return err
		}
		for _, url := range urls {
			res, err := tx.ExecContext(ctx, `UPDATE workspace_links SET shown = 1 WHERE workspace_id = ? AND url = ?`, id, url)
			if err != nil {
				return err
			}
			shown, err := touchedRow(res)
			if err != nil {
				return err
			}
			if !shown {
				return &UnknownLinkError{URL: url}
			}
		}
		var err error
		out, err = linksOf(ctx, tx, id)
		return err
	})
	return out, err
}

// ClearWorkspaceLinks removes every link ID holds. Clearing a workspace with
// none is no change.
func (s *store) ClearWorkspaceLinks(ctx context.Context, id WorkspaceID) error {
	return s.write(ctx, "daemon.wsm.clear_workspace_links", dlog.Context{"workspace": string(id)}, func(ctx context.Context, tx *sql.Tx) error {
		if err := requireWorkspaceRow(ctx, tx, id); err != nil {
			return err
		}
		if err := endOwnedFollowsInTx(ctx, tx, id, ``); err != nil {
			return err
		}
		_, err := tx.ExecContext(ctx, `DELETE FROM workspace_links WHERE workspace_id = ?`, id)
		return err
	})
}

// requireWorkspaceRow answers ErrNotFound when ID is not registered, so a
// change to an unregistered workspace's links is refused rather than written
// against no workspace.
func requireWorkspaceRow(ctx context.Context, tx *sql.Tx, id WorkspaceID) error {
	var registered WorkspaceID
	return scanWorkspaceRow(ctx, tx, id, "id", &registered)
}

// followLinkInTx keeps SET's pull request's ownership subscription in step
// with the link: taken when owned and none is held, ended when no longer
// owned.
func (s *store) followLinkInTx(ctx context.Context, tx *sql.Tx, id WorkspaceID, set LinkSet) error {
	if set.PullRequest == nil {
		return nil
	}
	if !set.Owned {
		_, err := deletePullRequestSubscription(ctx, tx, id, *set.PullRequest, PullRequestOriginOwnedLink)
		return err
	}
	// ONE OWNER PER PULL REQUEST (pullrequestowner.go).
	if err := refuseOtherOwnerInTx(ctx, tx, id, *set.PullRequest); err != nil {
		return err
	}
	if set.Subscribe == nil {
		return ownHeldSubscriptionInTx(ctx, tx, id, *set.PullRequest)
	}
	sub := *set.Subscribe
	var ended, mergeSHA any
	if sub.Ended != nil {
		ended = "closed"
		if sub.Ended.Merged {
			ended, mergeSHA = "merged", sub.Ended.MergeSHA
		}
	}
	_, err := tx.ExecContext(ctx,
		`INSERT INTO pull_request_subscriptions (subscriber, owner, repo, number, events, origin, subscribed_at, told, ended, ended_merge_sha) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
		 ON CONFLICT(subscriber, owner, repo, number) DO NOTHING`,
		id, sub.PullRequest.Owner, sub.PullRequest.Repo, sub.PullRequest.Number, int64(sub.Events), string(PullRequestOriginOwnedLink), nanos(sub.SubscribedAt), sub.Told, ended, mergeSHA)
	return err
}

// endOwnedFollowsInTx ends the ownership subscriptions of ID's owned pull
// request links that WHERE (an `AND ...` clause over workspace_links, with
// ARGS) selects.
func endOwnedFollowsInTx(ctx context.Context, tx *sql.Tx, id WorkspaceID, where string, args ...any) error {
	_, err := tx.ExecContext(ctx, `DELETE FROM pull_request_subscriptions WHERE origin = ? AND subscriber = ? AND (owner, repo, number) IN (
		SELECT pr_owner, pr_repo, pr_number FROM workspace_links WHERE workspace_id = ? AND owned = 1 AND pr_number IS NOT NULL `+where+`)`,
		append([]any{string(PullRequestOriginOwnedLink), id, id}, args...)...)
	return err
}

// linksOf reads ID's links, most recently set first, each owned pull request
// with the workspace's subscription to it.
func linksOf(ctx context.Context, q querier, id WorkspaceID) ([]WorkspaceLink, error) {
	links, err := listRows(ctx, q, `SELECT url, label, shown, owned, pr_owner, pr_repo, pr_number, announces FROM workspace_links WHERE workspace_id = ? ORDER BY set_seq DESC`,
		func(row interface{ Scan(...any) error }) (WorkspaceLink, error) {
			var (
				link                   WorkspaceLink
				owner, repo, announces sql.NullString
				number                 sql.NullInt64
			)
			err := row.Scan(&link.URL, &link.Label, &link.Shown, &link.Owned, &owner, &repo, &number, &announces)
			link.Announces = announces.String
			if number.Valid {
				link.PullRequest = &PullRequestKey{Owner: owner.String, Repo: repo.String, Number: uint32(number.Int64)}
			}
			return link, err
		}, id)
	if err != nil {
		return nil, err
	}
	for i := range links {
		pr := links[i].PullRequest
		if pr == nil {
			continue
		}
		subs, err := listRows(ctx, q, `SELECT `+pullRequestColumns+` FROM pull_request_subscriptions WHERE subscriber = ? AND owner = ? AND repo = ? AND number = ?`,
			scanPullRequestSubscription, id, pr.Owner, pr.Repo, pr.Number)
		if err != nil {
			return nil, err
		}
		if len(subs) == 1 {
			links[i].Follow = &subs[0]
		}
	}
	return links, nil
}

// hasRepeat reports whether any url appears twice.
func hasRepeat(urls []string) bool {
	seen := make(map[string]bool, len(urls))
	for _, url := range urls {
		if seen[url] {
			return true
		}
		seen[url] = true
	}
	return false
}

// OwnedPullRequestsMerged reports whether LINKS hold at least one canonical
// owned pull request (an owned link naming a GitHub pull request) and every
// such pull request has merged, by its subscription's recorded end. A pull
// request still open, closed unmerged, or no longer followed (its merge
// cannot be known) is not merged. The ONE rule both closes answer by: a
// workspace whose owned pull requests have all merged is closed, and one
// whose owned pull requests have not is never closed by its finish
// (owner ruling, 2026-11-10).
func OwnedPullRequestsMerged(links []WorkspaceLink) (owns, merged bool) {
	merged = true
	for _, link := range links {
		if !link.Owned || link.PullRequest == nil {
			continue
		}
		owns = true
		if link.Follow == nil || link.Follow.Ended == nil || !link.Follow.Ended.Merged {
			merged = false
		}
	}
	return owns, owns && merged
}
