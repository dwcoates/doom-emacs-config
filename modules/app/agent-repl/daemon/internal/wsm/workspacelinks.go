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
	// false when it only references it.
	Owned bool
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

// SetWorkspaceLink stores URL under LABEL with its ownership, or relabels it
// and sets its ownership anew when ID already holds it, making it the most
// recently set either way. A new link is shown while fewer than
// ShownLinkLimit are; a relabeled one keeps its placement. It answers every
// link ID now holds, read in the same transaction.
func (s *store) SetWorkspaceLink(ctx context.Context, id WorkspaceID, url, label string, owned bool) ([]WorkspaceLink, error) {
	const op = "daemon.wsm.set_workspace_link"
	fields := dlog.Context{"workspace": string(id), "url": url, "label": label, "owned": owned}
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
		if _, err := tx.ExecContext(ctx,
			`INSERT INTO workspace_links (workspace_id, url, label, set_seq, shown, owned) VALUES (?, ?, ?, ?, ?, ?)
			 ON CONFLICT(workspace_id, url) DO UPDATE SET label = excluded.label, set_seq = excluded.set_seq, owned = excluded.owned`,
			id, url, label, next, shown < ShownLinkLimit, owned); err != nil {
			return err
		}
		var err error
		out, err = linksOf(ctx, tx, id)
		return err
	})
	return out, err
}

// RemoveWorkspaceLink removes URL from ID's links, answering the links ID
// still holds. A url ID does not hold is an *UnknownLinkError.
func (s *store) RemoveWorkspaceLink(ctx context.Context, id WorkspaceID, url string) ([]WorkspaceLink, error) {
	const op = "daemon.wsm.remove_workspace_link"
	var out []WorkspaceLink
	err := s.write(ctx, op, dlog.Context{"workspace": string(id), "url": url}, func(ctx context.Context, tx *sql.Tx) error {
		if err := requireWorkspaceRow(ctx, tx, id); err != nil {
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

// linksOf reads ID's links, most recently set first.
func linksOf(ctx context.Context, q querier, id WorkspaceID) ([]WorkspaceLink, error) {
	return listRows(ctx, q, `SELECT url, label, shown, owned FROM workspace_links WHERE workspace_id = ? ORDER BY set_seq DESC`,
		func(row interface{ Scan(...any) error }) (WorkspaceLink, error) {
			var link WorkspaceLink
			err := row.Scan(&link.URL, &link.Label, &link.Shown, &link.Owned)
			return link, err
		}, id)
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
