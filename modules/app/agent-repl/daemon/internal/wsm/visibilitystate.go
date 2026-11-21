package wsm

import (
	"context"
	"database/sql"
	"fmt"

	"agent-repld/internal/dlog"
)

// A workspace's VISIBILITY is whether the user can see it, and it is THE ONE
// FACT every per-workspace allocation is gated on (owner-settled,
// 2026-11-21): the editor holds a perspective, a composer, a host stream and
// a page for exactly the workspaces that are not NotVisible, the daemon
// serves their web streams, reloads their pages in a deploy, and starts their
// sessions at boot (Runs). It is persisted here and kept current on every
// change of what it is derived from, by the one derivation (Visibilities),
// so no other layer works it out, and a boot or a deploy reads it before it
// allocates anything.
//
// It is NOT a keep-alive input: collapsing never touches a cache
// (KeptWarm, LetsCacheLapse).

// Visibility is a workspace's visibility.
type Visibility int

const (
	// NotVisible is a workspace the user cannot see: nothing is allocated
	// for it.
	NotVisible Visibility = iota
	// UnselectedVisible is a workspace the user can see and is not looking at.
	UnselectedVisible
	// Selected is the workspace the user is looking at, whatever the rules
	// would say of it otherwise.
	Selected
)

// String names the visibility as the store keeps it.
func (v Visibility) String() string {
	switch v {
	case NotVisible:
		return "not_visible"
	case UnselectedVisible:
		return "unselected_visible"
	case Selected:
		return "selected"
	default:
		return fmt.Sprintf("visibility(%d)", int(v))
	}
}

// Visible reports whether the user can see the workspace, selected or not.
func (v Visibility) Visible() bool { return v != NotVisible }

// parseVisibility reads a stored visibility; any other text is a corrupt
// record.
func parseVisibility(text string) (Visibility, error) {
	switch text {
	case "not_visible":
		return NotVisible, nil
	case "unselected_visible":
		return UnselectedVisible, nil
	case "selected":
		return Selected, nil
	default:
		return NotVisible, fmt.Errorf("a stored visibility is one of not_visible, unselected_visible and selected, not %q", text)
	}
}

// VisibilityInput is what one workspace's visibility is derived from.
type VisibilityInput struct {
	// ID is the workspace.
	ID WorkspaceID
	// Parent is the workspace it sits under in the Open section's tree, nil
	// for a top-level one.
	Parent *WorkspaceID
	// Open reports whether the workspace is one of the Open section's rows:
	// never a hidden, closed or hidden merge workspace, nor a thread.
	Open bool
	// Covered reports whether a fold covers it: its repository section's,
	// its subsection's, or any ancestor's children fold.
	Covered bool
	// Working reports whether a turn or detached work runs in it, or it asks
	// the user something.
	Working bool
	// Unviewed reports whether its last turn ended with a result the user has
	// not read: its name is still drawn full.
	Unviewed bool
	// Selected reports whether it is the user's selection.
	Selected bool
}

// Visibilities derives every workspace's visibility from INPUTS, THE ONE
// DERIVATION (owner-settled, 2026-11-21):
//
//   - only an Open workspace can be visible;
//   - an Open workspace no fold covers is visible (every top-level workspace
//     of an unfolded section, and every child of a visible, unfolded parent);
//   - one a fold covers, at any depth, is visible when it is Working or
//     Unviewed;
//   - the parent of a visible workspace is visible, recursively, so every
//     visible workspace has an unbroken chain of rows above it;
//   - the selection is Selected, whatever the rules above say.
//
// Every input's workspace answers; a parent the inputs do not carry ends the
// chain there.
func Visibilities(inputs []VisibilityInput) map[WorkspaceID]Visibility {
	byID := make(map[WorkspaceID]VisibilityInput, len(inputs))
	out := make(map[WorkspaceID]Visibility, len(inputs))
	for _, in := range inputs {
		byID[in.ID] = in
		out[in.ID] = NotVisible
	}
	mark := func(id WorkspaceID) {
		for {
			in, ok := byID[id]
			if !ok || !in.Open || out[id] == UnselectedVisible {
				return
			}
			out[id] = UnselectedVisible
			if in.Parent == nil {
				return
			}
			id = *in.Parent
		}
	}
	for _, in := range inputs {
		if in.Open && (!in.Covered || in.Working || in.Unviewed) {
			mark(in.ID)
		}
	}
	for _, in := range inputs {
		if !in.Selected {
			continue
		}
		// THE SELECTION'S PARENTS ARE VISIBLE TOO: the selection is a kind
		// of visible, so the chain above it is unbroken.
		if in.Parent != nil {
			mark(*in.Parent)
		}
		out[in.ID] = Selected
	}
	return out
}

// visibilityDDL is the layout-192 addition: workspaces.visibility, the
// persisted visibility, as its String names it. A row the previous build
// writes keeps its column's value; a fresh row is not_visible until the
// roster derives it. The fresh-file schema appends this same text
// (schema.go), and seedVisibility fills every existing row.
const visibilityDDL = `
ALTER TABLE workspaces ADD COLUMN visibility TEXT NOT NULL DEFAULT 'not_visible'
  CHECK (visibility IN ('not_visible', 'unselected_visible', 'selected'));
`

// seedVisibility fills the visibility of every workspace a file migrated to
// layout 192 already holds, by the one derivation (Visibilities) over what
// the file records: the open rows, their folds, their parents and the
// selection. Whether a workspace works, or its result is unread, is the
// roster's live fact and no part of the file, so the seed reads both as false;
// the roster's first render after the boot writes them in. The boot that
// migrates therefore starts nothing for a workspace only those facts made
// visible, which the roster then opens.
func seedVisibility(ctx context.Context, tx *sql.Tx, log dlog.Logger) error {
	rows, err := tx.QueryContext(ctx, `SELECT id, parent_id, closed, hidden, merge_of IS NOT NULL AND merge_revealed = 0 AND merged_at IS NULL, kind, is_current FROM workspaces`)
	if err != nil {
		return fmt.Errorf("wsm: read the workspaces to seed their visibility: %w", err)
	}
	var inputs []VisibilityInput
	for rows.Next() {
		var (
			id                               WorkspaceID
			parent                           sql.NullString
			closed, hidden, mergeHidden, cur bool
			kind                             string
		)
		if err := rows.Scan(&id, &parent, &closed, &hidden, &mergeHidden, &kind, &cur); err != nil {
			rows.Close()
			return fmt.Errorf("wsm: scan a workspace to seed its visibility: %w", err)
		}
		in := VisibilityInput{ID: id, Open: !closed && !hidden && !mergeHidden && kind != string(WorkspaceKindThread), Selected: cur}
		if parent.Valid {
			p := WorkspaceID(parent.String)
			in.Parent = &p
		}
		inputs = append(inputs, in)
	}
	if err := rows.Err(); err != nil {
		rows.Close()
		return fmt.Errorf("wsm: read the workspaces to seed their visibility: %w", err)
	}
	rows.Close()
	for i := range inputs {
		var covered bool
		if err := tx.QueryRowContext(ctx, hiddenByFoldQuery, inputs[i].ID, inputs[i].ID).Scan(&covered); err != nil {
			return fmt.Errorf("wsm: read whether a fold covers %s: %w", inputs[i].ID, err)
		}
		inputs[i].Covered = covered
	}
	for id, v := range Visibilities(inputs) {
		if _, err := tx.ExecContext(ctx, `UPDATE workspaces SET visibility = ? WHERE id = ?`, v.String(), string(id)); err != nil {
			return fmt.Errorf("wsm: seed the visibility of %s: %w", id, err)
		}
	}
	log.Info("daemon.wsm.migrate", "seeded every workspace's visibility from its record", dlog.Context{"workspaces": len(inputs)})
	return nil
}

// SetVisibilities persists each workspace's visibility in CHANGES, in one
// write. An unknown workspace is refused rather than silently affecting no
// rows.
//
// A READ-ONLY HANDLE HOLDS THEM, as it holds a status change instant: a
// joining successor's roster derives visibilities while the incumbent is still
// the writer, and the latest per workspace is written at the promotion
// (writeHeldVisibilitiesLocked), so none is lost and none is refused.
func (s *store) SetVisibilities(ctx context.Context, changes map[WorkspaceID]Visibility) error {
	const op = "daemon.wsm.set_visibilities"
	fields := dlog.Context{"workspaces": len(changes)}
	if len(changes) == 0 {
		return nil
	}
	s.mu.RLock()
	if s.ReadOnly() {
		s.holdVisibilitiesLocked(changes)
		s.mu.RUnlock()
		s.log.Debug(op, "held the workspaces' visibilities until this handle writes; the incumbent is still the writer", fields)
		return nil
	}
	s.mu.RUnlock()
	return s.writeVisibilities(ctx, op, fields, changes)
}

// writeVisibilities is the visibilities' one write.
func (s *store) writeVisibilities(ctx context.Context, op string, fields dlog.Context, changes map[WorkspaceID]Visibility) error {
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		for id, v := range changes {
			res, err := tx.ExecContext(ctx, `UPDATE workspaces SET visibility = ? WHERE id = ?`, v.String(), string(id))
			if err != nil {
				return fmt.Errorf("wsm: set the visibility of %s: %w", id, err)
			}
			if err := requireOneRow(res, fmt.Sprintf("wsm: workspace %s", id)); err != nil {
				return err
			}
		}
		return nil
	})
}

// holdVisibilitiesLocked keeps CHANGES as the latest held visibilities. The
// caller holds mu's read side.
func (s *store) holdVisibilitiesLocked(changes map[WorkspaceID]Visibility) {
	s.heldMu.Lock()
	defer s.heldMu.Unlock()
	if s.heldVisibilities == nil {
		s.heldVisibilities = make(map[WorkspaceID]Visibility)
	}
	for id, v := range changes {
		s.heldVisibilities[id] = v
	}
}

// writeHeldVisibilitiesLocked writes every held visibility and forgets them.
// The caller holds mu EXCLUSIVELY and the handle already writes. A failed
// write is recorded at ERROR by the write itself and is not held again: the
// roster's next render writes afresh.
func (s *store) writeHeldVisibilitiesLocked(ctx context.Context) {
	const op = "daemon.wsm.set_visibilities"
	s.heldMu.Lock()
	held := s.heldVisibilities
	s.heldVisibilities = nil
	s.heldMu.Unlock()
	if len(held) == 0 {
		return
	}
	fields := dlog.Context{"workspaces": len(held), "held": true}
	if err := s.writeVisibilities(ctx, op, fields, held); err == nil {
		s.log.Debug(op, "wrote the visibilities held while this handle was read-only", fields)
	}
}

// hiddenByFoldQuery answers, for the workspace its one argument names (twice,
// the second for the subsection), whether a fold covers it: the repository
// section its family's root sits in, the subsection the family sits in
// (rootSubsectionColumn), or the children fold of any ancestor. It is the
// store's reading of what resolve/sidebar's openTree draws as a folded tab,
// which the visibility seed reads (seedVisibility).
const hiddenByFoldQuery = `
WITH RECURSIVE up(id, parent_id, repo_id, children_folded, depth) AS (
  SELECT id, parent_id, repo_id, children_folded, 0 FROM workspaces WHERE id = ?
  UNION
  SELECT anc.id, anc.parent_id, anc.repo_id, anc.children_folded, up.depth + 1
  FROM workspaces AS anc JOIN up ON anc.id = up.parent_id
)
SELECT EXISTS (SELECT 1 FROM up WHERE depth > 0 AND children_folded = 1)
    OR EXISTS (SELECT 1 FROM repositories WHERE folded = 1
               AND id = (SELECT repo_id FROM up ORDER BY depth DESC LIMIT 1))
    OR EXISTS (SELECT 1 FROM subsections WHERE folded = 1
               AND id = (SELECT ` + rootSubsectionColumn + ` FROM workspaces WHERE id = ?))`
