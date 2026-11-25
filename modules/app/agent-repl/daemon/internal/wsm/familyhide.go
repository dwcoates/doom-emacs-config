package wsm

import (
	"context"
	"database/sql"
	"time"

	"agent-repld/internal/dlog"
)

// hiddenWithDDL is the layout-165 addition: workspaces.hidden_with, the
// standing deploy check whose hide hid this workspace with it (HideFamily).
// A standing fixer is drawn only while it holds its queue (owner ruling,
// 2026-11-17), and the workspaces it made go out of sight with it and come
// back with it (owner ruling, 2026-11-17: "hide its kids too"). IT IS
// ADDITIVE: a nullable column the previous build never names.
const hiddenWithDDL = `
ALTER TABLE workspaces ADD COLUMN hidden_with TEXT;
CREATE INDEX workspaces_by_hidden_with ON workspaces(hidden_with) WHERE hidden_with IS NOT NULL;
`

// HideFamily hides ROOT and every open workspace its hide would otherwise be
// refused for -- its open descendants and the open workspaces landing into
// it (refuseOpenChildren) -- in one write, each of them marked as hidden
// with ROOT, and answers the members hidden with it. A
// member already hidden stays as it is and is not marked: ROOT's show
// leaves it hidden. A ROOT already hidden is re-stamped, as SetHidden
// re-stamps.
//
// THE FAMILY GOES TOGETHER, SO NO OPEN CHILD IS LEFT UNDER A HIDDEN
// PARENT: the rule refuseOpenChildren holds for one workspace's hide holds
// here for the whole family at once.
func (s *store) HideFamily(ctx context.Context, root WorkspaceID, at time.Time) ([]WorkspaceID, error) {
	var hidden []WorkspaceID
	err := s.write(ctx, "daemon.wsm.hide_family", dlog.Context{"workspace": string(root), "hidden_at": at}, func(ctx context.Context, tx *sql.Tx) error {
		members, err := familyOf(ctx, tx, root)
		if err != nil {
			return err
		}
		for _, id := range members {
			var was bool
			if err := scanWorkspaceRow(ctx, tx, id, "backlogged", &was); err != nil {
				return err
			}
			if was {
				continue
			}
			if err := updateWorkspaceColumns(ctx, tx, id, append(hideColumns(true, at), workspaceColumn{name: "backlogged_with", value: string(root)})...); err != nil {
				return err
			}
			hidden = append(hidden, id)
		}
		return updateWorkspaceColumns(ctx, tx, root, hideColumns(true, at)...)
	})
	return hidden, err
}

// ShowFamily shows ROOT and then every open workspace HideFamily hid with
// it, parent first, and answers the members shown, in the order shown. A
// member closed since is left closed; one shown since is not shown again. A
// ROOT whose own parent is not open is refused (ParentNotOpenError), nothing
// written, as SetHidden refuses it.
func (s *store) ShowFamily(ctx context.Context, root WorkspaceID) ([]WorkspaceID, error) {
	var shown []WorkspaceID
	err := s.write(ctx, "daemon.wsm.show_family", dlog.Context{"workspace": string(root)}, func(ctx context.Context, tx *sql.Tx) error {
		var was bool
		if err := scanWorkspaceRow(ctx, tx, root, "backlogged", &was); err != nil {
			return err
		}
		if was {
			if err := requireOpenParentOf(ctx, tx, root); err != nil {
				return err
			}
			if err := updateWorkspaceColumns(ctx, tx, root, hideColumns(false, time.Time{})...); err != nil {
				return err
			}
		}
		members, err := listRows(ctx, tx, `SELECT w.id FROM workspaces w WHERE w.backlogged_with = ? AND w.backlogged = 1 AND w.closed = 0 ORDER BY w.place`,
			func(row interface{ Scan(...any) error }) (WorkspaceID, error) {
				var id WorkspaceID
				return id, row.Scan(&id)
			}, string(root))
		if err != nil {
			return err
		}
		// PARENT FIRST: a member is shown once its parent is open, so the
		// family comes back in whatever order its places hold. A member
		// whose parent stays out of the Open section (a workspace landing
		// into ROOT whose own parent is hidden elsewhere) stays hidden, still
		// marked, as SetHidden would refuse to show it.
		for progress := true; progress; {
			progress = false
			for i := 0; i < len(members); i++ {
				err := requireOpenParentOf(ctx, tx, members[i])
				if IsFamilyRefusal(err) {
					continue
				}
				if err != nil {
					return err
				}
				if err := updateWorkspaceColumns(ctx, tx, members[i], hideColumns(false, time.Time{})...); err != nil {
					return err
				}
				shown = append(shown, members[i])
				members = append(members[:i], members[i+1:]...)
				i--
				progress = true
			}
		}
		return nil
	})
	return shown, err
}

// familyOf answers every open workspace ROOT's hide is refused for, at every
// depth: its open descendants and the workspaces landing into it, and theirs
// in turn, each once, in the order found.
func familyOf(ctx context.Context, tx *sql.Tx, root WorkspaceID) ([]WorkspaceID, error) {
	seen := map[WorkspaceID]bool{root: true}
	var out []WorkspaceID
	for queue := []WorkspaceID{root}; len(queue) > 0; queue = queue[1:] {
		descendants, err := openDescendants(ctx, tx, queue[0])
		if err != nil {
			return nil, err
		}
		landers, err := openLanders(ctx, tx, queue[0], descendants)
		if err != nil {
			return nil, err
		}
		for _, ref := range append(descendants, landers...) {
			if seen[ref.ID] {
				continue
			}
			seen[ref.ID] = true
			out = append(out, ref.ID)
			queue = append(queue, ref.ID)
		}
	}
	return out, nil
}
