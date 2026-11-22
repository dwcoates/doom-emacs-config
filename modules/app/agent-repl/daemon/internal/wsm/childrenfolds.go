package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"

	"agent-repld/internal/dlog"
)

// childrenFoldedDDL is the layout-108 addition: whether a workspace's
// children are folded (frontend.v1.RosterOpenRow.children_fold).
//
// IT IS ADDITIVE: the previous build names no such column, so a workspace it
// registers takes 0, expanded, which is what every row drew as before.
const childrenFoldedDDL = `
ALTER TABLE workspaces ADD COLUMN children_folded INTEGER NOT NULL DEFAULT 0;
`

// SetChildrenFolded records whether workspace ID's children are folded. A
// folded workspace's open descendants are hidden in the sidebar and their
// tabs are folded off the tab bar, as a folded subsection's are
// (resolve/sidebar's openTree). An unknown workspace is refused (ErrNotFound),
// never a write that touched nothing.
func (s *store) SetChildrenFolded(ctx context.Context, id WorkspaceID, folded bool) error {
	return s.write(ctx, "daemon.wsm.set_children_folded", dlog.Context{"workspace": string(id), "folded": folded},
		func(ctx context.Context, tx *sql.Tx) error {
			res, err := tx.ExecContext(ctx, `UPDATE workspaces SET children_folded = ? WHERE id = ?`, folded, id)
			if err != nil {
				return err
			}
			return requireOneRow(res, fmt.Sprintf("wsm: workspace %s", id))
		})
}

// OpenedFold is one fold an expand opened: the stamp kind and target the
// user's open of it is recorded under (AttentionKind).
type OpenedFold struct {
	Kind   AttentionKind
	Target string
}

// ExpandChildrenWithAncestry records workspace ID's children as expanded and
// expands every fold above it in the same write, so the row the user
// expanded is drawn: the children fold of each ancestor, the subsection fold
// its family's root sits in, and the repository section fold of that root. A
// row a fold covers is drawn only while it is selected, working or unviewed
// (Visibilities), so expanding only its own children shows nothing. It
// answers the folds above ID that were folded and are now open, nearest
// ancestor first, then the subsection, then the repository: ID's own fold is
// the caller's. An unknown workspace is refused (ErrNotFound) and writes
// nothing; a parent chain that cycles is a broken store and fails the write.
func (s *store) ExpandChildrenWithAncestry(ctx context.Context, id WorkspaceID) ([]OpenedFold, error) {
	var opened []OpenedFold
	err := s.write(ctx, "daemon.wsm.expand_children_with_ancestry", dlog.Context{"workspace": string(id)},
		func(ctx context.Context, tx *sql.Tx) error {
			opened = nil
			res, err := tx.ExecContext(ctx, `UPDATE workspaces SET children_folded = 0 WHERE id = ?`, id)
			if err != nil {
				return err
			}
			if err := requireOneRow(res, fmt.Sprintf("wsm: workspace %s", id)); err != nil {
				return err
			}
			var (
				repo       RepoID
				parent     sql.NullString
				subsection sql.NullString
			)
			if err := scanWorkspaceRow(ctx, tx, id, "repo_id, parent_id, subsection_id", &repo, &parent, &subsection); err != nil {
				return err
			}
			seen := map[string]bool{string(id): true}
			for parent.Valid {
				if seen[parent.String] {
					return fmt.Errorf("wsm: the recorded parents above workspace %s cycle at %s", id, parent.String)
				}
				seen[parent.String] = true
				ancestor := parent.String
				var folded bool
				if err := scanWorkspaceRow(ctx, tx, WorkspaceID(ancestor), "repo_id, parent_id, subsection_id, children_folded", &repo, &parent, &subsection, &folded); err != nil {
					return err
				}
				if !folded {
					continue
				}
				if _, err := tx.ExecContext(ctx, `UPDATE workspaces SET children_folded = 0 WHERE id = ?`, ancestor); err != nil {
					return err
				}
				opened = append(opened, OpenedFold{Kind: AttentionOpenedChildren, Target: ancestor})
			}
			// The walk ends on the family's root, whose subsection and repository
			// the family is drawn in.
			if subsection.Valid {
				var folded bool
				if err := tx.QueryRowContext(ctx, `SELECT folded FROM subsections WHERE id = ?`, subsection.String).Scan(&folded); err != nil {
					return err
				}
				if folded {
					if err := writeSubsectionFolded(ctx, tx, SubsectionID(subsection.String), false); err != nil {
						return err
					}
					opened = append(opened, OpenedFold{Kind: AttentionOpenedSubsection, Target: subsection.String})
				}
			}
			var folded bool
			if err := tx.QueryRowContext(ctx, `SELECT folded FROM repositories WHERE id = ?`, repo).Scan(&folded); err != nil {
				if errors.Is(err, sql.ErrNoRows) {
					return fmt.Errorf("wsm: workspace %s's root sits in repository %s, which is not registered", id, repo)
				}
				return err
			}
			if folded {
				if _, err := tx.ExecContext(ctx, `UPDATE repositories SET folded = 0 WHERE id = ?`, repo); err != nil {
					return err
				}
				opened = append(opened, OpenedFold{Kind: AttentionOpenedRepository, Target: string(repo)})
			}
			return nil
		})
	if err != nil {
		return nil, err
	}
	return opened, nil
}
