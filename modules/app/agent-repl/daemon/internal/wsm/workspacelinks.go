package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"

	"agent-repld/internal/dlog"
	"agent-repld/internal/gnsurl"
	"agent-repld/internal/slacklink"
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
	// State is how PullRequest stands, as the watcher last read it
	// (pullrequeststates.go); empty for any other url, and for a pull request
	// not yet read.
	State PullRequestState
	// Announces is the url of the stored link this owned link announces
	// (agentrepl.v1.WorkspaceLinkOwned.announces), empty when it announces
	// nothing.
	Announces string
	// SlackSubscribed is true when the url names a Slack thread the
	// workspace subscribes to (slackthreads.go): the subscription was taken
	// by the link itself or by any other link to the same thread.
	SlackSubscribed bool
}

// Subscribed reports whether the workspace is told about the link's updates
// through agent-repl: a pull request it follows and has not stopped
// following, or a Slack thread it subscribes to. The ONE rule the summary's
// menu draws its "subscribed" badge by (frontend.v1.TopbarTitleLinkMenuItem.subscribed).
func (l WorkspaceLink) Subscribed() bool {
	return (l.Follow != nil && l.Follow.Ended == nil) || l.SlackSubscribed
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

// SubpageOfOwnedLinkError is an owned set naming a GNS subpage of a GNS page
// the workspace already owns as a link (gnsurl.Page.Contains): the owned page
// stands for its subpages, so nothing is stored. It is the store answering
// what was asked, never a failure.
type SubpageOfOwnedLinkError struct {
	// URL is the url as the caller named it.
	URL string
	// OwnedPageURL is the stored owned link whose page contains it.
	OwnedPageURL string
}

func (e *SubpageOfOwnedLinkError) Error() string {
	return fmt.Sprintf("wsm: %q is a GNS subpage of the owned link %q", e.URL, e.OwnedPageURL)
}

// OwnedLinkUnshownError is a show that would leave an owned link unshown
// while it shows a reference, when the owned links alone fit the menu. It is
// the store answering what was asked, never a failure.
type OwnedLinkUnshownError struct {
	// OwnedURL is the owned link the show leaves out.
	OwnedURL string
	// ReferenceURL is a reference the show names in its place.
	ReferenceURL string
}

func (e *OwnedLinkUnshownError) Error() string {
	return fmt.Sprintf("wsm: a show leaves the owned link %q unshown while it shows the reference %q", e.OwnedURL, e.ReferenceURL)
}

// isLinkRefusal reports whether ERR is a links write refusing what was asked
// (the verb turns each into the rpc's arm), never the store failing.
func isLinkRefusal(err error) bool {
	var unknown *UnknownLinkError
	var unannounced *UnknownAnnouncedLinkError
	var ownedElsewhere *PullRequestOwnedElsewhereError
	var subpage *SubpageOfOwnedLinkError
	var ownedUnshown *OwnedLinkUnshownError
	return errors.As(err, &unknown) || errors.As(err, &unannounced) || errors.As(err, &ownedElsewhere) ||
		errors.As(err, &subpage) || errors.As(err, &ownedUnshown)
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
// either way. An owned GNS page that is a subpage of another GNS page ID
// owns is a *SubpageOfOwnedLinkError, and nothing changes. OWNED LINKS COME
// FIRST in the menu: a new link is shown while fewer than ShownLinkLimit
// are, and once the menu is full an owned set the menu leaves out (a new
// link, or a relabel of an unshown one) is shown in place of the oldest
// shown reference, if any is shown. A shown link, and a reference relabeled,
// keep their placement. In the same transaction an owned pull
// request takes SET.Subscribe when ID holds no subscription to it, and a link
// no longer owned ends the subscription its ownership took. It answers every
// link ID now holds, read in the same transaction.
func (s *store) SetWorkspaceLink(ctx context.Context, id WorkspaceID, set LinkSet) ([]WorkspaceLink, error) {
	const op = "daemon.wsm.set_workspace_link"
	url, label := set.URL, set.Label
	fields := dlog.Context{"workspace": string(id), "url": url, "label": label, "owned": set.Owned, "announces": set.Announces}
	if set.Subscribe != nil && (!set.Owned || set.PullRequest == nil || set.Subscribe.PullRequest != *set.PullRequest || set.Subscribe.Subscriber != id) {
		err := fmt.Errorf("wsm: link of %s: a subscription is taken only for the owned pull request the link names", id)
		s.log.Error(op, "refused a link its caller should never have made: its subscription is not for the owned pull request it names", withError(fields, err))
		return nil, err
	}
	if set.Announces != "" && (!set.Owned || set.Announces == url) {
		err := fmt.Errorf("wsm: link of %s: only an owned link announces, and never itself", id)
		s.log.Error(op, "refused a link its caller should never have made: it announces while unowned, or announces itself", withError(fields, err))
		return nil, err
	}
	if url == "" || label == "" {
		err := fmt.Errorf("wsm: link of %s: a link names its url and its label", id)
		s.log.Error(op, "refused a link its caller should never have made: it names no url or no label", withError(fields, err))
		return nil, err
	}
	var out []WorkspaceLink
	err := s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		if err := requireWorkspaceRow(ctx, tx, id); err != nil {
			return err
		}
		if set.Owned {
			if err := refuseSubpageOfOwnedInTx(ctx, tx, id, url); err != nil {
				return err
			}
		}
		var next int64
		if err := tx.QueryRowContext(ctx,
			`SELECT COALESCE(MAX(set_seq), 0) + 1 FROM workspace_links WHERE workspace_id = ?`, id).Scan(&next); err != nil {
			return err
		}
		shown, err := placeLinkInTx(ctx, tx, id, set)
		if err != nil {
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
			 ON CONFLICT(workspace_id, url) DO UPDATE SET label = excluded.label, set_seq = excluded.set_seq, shown = excluded.shown, owned = excluded.owned, announces = excluded.announces`,
			id, url, label, next, shown, set.Owned, owner, repo, number, announces); err != nil {
			return err
		}
		if err := s.followLinkInTx(ctx, tx, id, set); err != nil {
			return err
		}
		out, err = linksOf(ctx, tx, id)
		return err
	})
	return out, err
}

// AddWorkspaceLinkIfAbsent stores URL under LABEL as a reference link on ID
// when ID holds no link to it, and changes nothing when it does, whatever its
// label or ownership: a link the workspace set itself is never relabeled or
// demoted by a url the daemon read out of its responses. It is placed as any
// new reference is (placeLinkInTx), and answers whether it stored the link.
// PULLREQUEST is the pull request the url names, nil for any other url.
func (s *store) AddWorkspaceLinkIfAbsent(ctx context.Context, id WorkspaceID, url, label string, pullRequest *PullRequestKey) (bool, error) {
	const op = "daemon.wsm.add_workspace_link_if_absent"
	fields := dlog.Context{"workspace": string(id), "url": url, "label": label}
	if url == "" || label == "" {
		err := fmt.Errorf("wsm: link of %s: a link names its url and its label", id)
		s.log.Error(op, "refused a link its caller should never have made: it names no url or no label", withError(fields, err))
		return false, err
	}
	var added bool
	err := s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		if err := requireWorkspaceRow(ctx, tx, id); err != nil {
			return err
		}
		var err error
		added, err = addReferenceLinkIfAbsentInTx(ctx, tx, id, url, label, pullRequest)
		return err
	})
	return added, err
}

// addReferenceLinkIfAbsentInTx stores URL under LABEL as a reference link on
// ID when ID holds no link to it, placed as any new reference is
// (placeLinkInTx), and answers whether it stored the link. A link ID already
// holds is left as it is, whatever its label or ownership. PULLREQUEST is the
// pull request the url names, nil for any other url.
func addReferenceLinkIfAbsentInTx(ctx context.Context, tx *sql.Tx, id WorkspaceID, url, label string, pullRequest *PullRequestKey) (bool, error) {
	var held int
	if err := tx.QueryRowContext(ctx, `SELECT COUNT(*) FROM workspace_links WHERE workspace_id = ? AND url = ?`, id, url).Scan(&held); err != nil {
		return false, err
	}
	if held > 0 {
		return false, nil
	}
	shown, err := placeLinkInTx(ctx, tx, id, LinkSet{URL: url, Label: label})
	if err != nil {
		return false, err
	}
	var owner, repo, number any
	if pullRequest != nil {
		owner, repo, number = pullRequest.Owner, pullRequest.Repo, pullRequest.Number
	}
	if _, err := tx.ExecContext(ctx,
		`INSERT INTO workspace_links (workspace_id, url, label, set_seq, shown, owned, pr_owner, pr_repo, pr_number, announces)
		 VALUES (?, ?, ?, (SELECT COALESCE(MAX(set_seq), 0) + 1 FROM workspace_links WHERE workspace_id = ?), ?, 0, ?, ?, ?, NULL)`,
		id, url, label, id, shown, owner, repo, number); err != nil {
		return false, err
	}
	return true, nil
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
// changes), and there are one to ShownLinkLimit of them, without repeats. A
// show that leaves an owned link unshown while it shows a reference is an
// *OwnedLinkUnshownError, and nothing changes, unless ID's owned links alone
// are more than ShownLinkLimit.
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
		if err := refuseOwnedLinkUnshownInTx(ctx, tx, id, urls); err != nil {
			return err
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

// placeLinkInTx answers whether SET's link is shown once stored, unshowing
// the reference it takes the place of: a shown link stays shown; a new link,
// or an owned one the menu leaves out, is shown while the menu has room; once
// it is full, an owned one is shown in place of the oldest shown reference,
// and is otherwise left out; a reference relabeled keeps its placement.
func placeLinkInTx(ctx context.Context, tx *sql.Tx, id WorkspaceID, set LinkSet) (bool, error) {
	var held sql.NullBool
	if err := tx.QueryRowContext(ctx, `SELECT shown FROM workspace_links WHERE workspace_id = ? AND url = ?`, id, set.URL).Scan(&held); err != nil && !errors.Is(err, sql.ErrNoRows) {
		return false, err
	}
	if held.Valid && (held.Bool || !set.Owned) {
		return held.Bool, nil
	}
	var shown int
	if err := tx.QueryRowContext(ctx, `SELECT COUNT(*) FROM workspace_links WHERE workspace_id = ? AND shown = 1`, id).Scan(&shown); err != nil {
		return false, err
	}
	if shown < ShownLinkLimit {
		return true, nil
	}
	if !set.Owned {
		return false, nil
	}
	res, err := tx.ExecContext(ctx, `UPDATE workspace_links SET shown = 0 WHERE workspace_id = ? AND url = (
		SELECT url FROM workspace_links WHERE workspace_id = ? AND shown = 1 AND owned = 0 ORDER BY set_seq LIMIT 1)`, id, id)
	if err != nil {
		return false, err
	}
	return touchedRow(res)
}

// refuseOwnedLinkUnshownInTx answers an *OwnedLinkUnshownError when the show
// of URLS just written leaves an owned link of ID's unshown while it shows a
// reference, unless ID's owned links alone are more than ShownLinkLimit, in
// which case any choice among them stands. It names the most recently set
// owned link left out and the first reference URLS names.
func refuseOwnedLinkUnshownInTx(ctx context.Context, tx *sql.Tx, id WorkspaceID, urls []string) error {
	links, err := linksOf(ctx, tx, id)
	if err != nil {
		return err
	}
	owned, unshownOwned := 0, ""
	references := make(map[string]bool)
	for _, link := range links {
		switch {
		case link.Owned:
			owned++
			if !link.Shown && unshownOwned == "" {
				unshownOwned = link.URL
			}
		case link.Shown:
			references[link.URL] = true
		}
	}
	if owned > ShownLinkLimit || unshownOwned == "" {
		return nil
	}
	for _, url := range urls {
		if references[url] {
			return &OwnedLinkUnshownError{OwnedURL: unshownOwned, ReferenceURL: url}
		}
	}
	return nil
}

// refuseSubpageOfOwnedInTx answers a *SubpageOfOwnedLinkError when URL is a
// GNS page some other owned link of ID's contains, read in the set's own
// transaction so no concurrent set can slip a subpage past an owned page
// stored meanwhile. Nil for every url that is no GNS page.
func refuseSubpageOfOwnedInTx(ctx context.Context, tx *sql.Tx, id WorkspaceID, url string) error {
	page, ok := gnsurl.ParsePage(url)
	if !ok {
		return nil
	}
	owned, err := listRows(ctx, tx, `SELECT url FROM workspace_links WHERE workspace_id = ? AND owned = 1 AND url != ? ORDER BY set_seq`,
		func(row interface{ Scan(...any) error }) (string, error) {
			var held string
			return held, row.Scan(&held)
		}, id, url)
	if err != nil {
		return err
	}
	for _, held := range owned {
		if heldPage, isPage := gnsurl.ParsePage(held); isPage && heldPage.Contains(page) {
			return &SubpageOfOwnedLinkError{URL: url, OwnedPageURL: held}
		}
	}
	return nil
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

// linksOf reads ID's links, most recently set first, each pull request with
// its recorded state and the workspace's subscription to it.
func linksOf(ctx context.Context, q querier, id WorkspaceID) ([]WorkspaceLink, error) {
	links, err := listRows(ctx, q, `SELECT l.url, l.label, l.shown, l.owned, l.pr_owner, l.pr_repo, l.pr_number, l.announces, p.state
		FROM workspace_links AS l
		LEFT JOIN pull_request_states AS p ON p.owner = l.pr_owner AND p.repo = l.pr_repo AND p.number = l.pr_number
		WHERE l.workspace_id = ? ORDER BY l.set_seq DESC`,
		func(row interface{ Scan(...any) error }) (WorkspaceLink, error) {
			var (
				link                          WorkspaceLink
				owner, repo, announces, state sql.NullString
				number                        sql.NullInt64
			)
			err := row.Scan(&link.URL, &link.Label, &link.Shown, &link.Owned, &owner, &repo, &number, &announces, &state)
			link.Announces = announces.String
			link.State = PullRequestState(state.String)
			if number.Valid {
				link.PullRequest = &PullRequestKey{Owner: owner.String, Repo: repo.String, Number: uint32(number.Int64)}
			}
			return link, err
		}, id)
	if err != nil {
		return nil, err
	}
	slackThreads, err := slackThreadsOf(ctx, q, id)
	if err != nil {
		return nil, err
	}
	for i := range links {
		links[i].SlackSubscribed = slackThreadSubscribed(slackThreads, links[i].URL)
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

// slackThreadSubscribed reports whether SUBS hold a subscription to the
// Slack thread URL names (FindSlackThreadSubscription). A url that is no
// Slack message link names no thread, so it is never subscribed.
func slackThreadSubscribed(subs []SlackThreadSubscription, url string) bool {
	thread, err := slacklink.ParseThread(url)
	if err != nil {
		// Not a Slack message link: most links are not, and none of them
		// names a thread to subscribe to.
		return false
	}
	_, found := FindSlackThreadSubscription(subs, thread)
	return found
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
	for _, link := range OwnedPullRequestLinks(links) {
		owns = true
		if link.Follow == nil || link.Follow.Ended == nil || !link.Follow.Ended.Merged {
			merged = false
		}
	}
	return owns, owns && merged
}

// OwnedPullRequestOpen reports whether LINKS hold a canonical owned pull
// request that is still open: one the workspace follows whose subscription has
// recorded no end (frontend.v1.RosterRowOpenOwnedPullRequest). A pull request
// that merged or closed, or one no longer followed (its state cannot be
// known), is not open.
func OwnedPullRequestOpen(links []WorkspaceLink) bool {
	for _, link := range OwnedPullRequestLinks(links) {
		if link.Follow != nil && link.Follow.Ended == nil {
			return true
		}
	}
	return false
}

// OwnedPullRequestLinks is LINKS' canonical owned pull requests: the owned
// links naming a GitHub pull request, in LINKS' order. Every rule that reads a
// workspace's own pull requests reads them through it.
func OwnedPullRequestLinks(links []WorkspaceLink) []WorkspaceLink {
	var out []WorkspaceLink
	for _, link := range links {
		if link.Owned && link.PullRequest != nil {
			out = append(out, link)
		}
	}
	return out
}

// DeployablePullRequests is the urls of LINKS' owned pull requests that a
// deploy may still carry: every one but those whose subscription recorded a
// close without a merge. One still open, one merged, and one no longer
// followed are each counted, so a merge's head keeps naming its pull request
// once it merges.
func DeployablePullRequests(links []WorkspaceLink) []string {
	var out []string
	for _, link := range OwnedPullRequestLinks(links) {
		if link.Follow != nil && link.Follow.Ended != nil && !link.Follow.Ended.Merged {
			continue
		}
		out = append(out, link.URL)
	}
	return out
}
