package wsm

import (
	"context"
	"database/sql"
	"fmt"

	"agent-repld/internal/dlog"
)

// A workspace's VISIBILITY is whether it gets a number in the sidebar's Open
// section: an open workspace that no fold has rendered invisible as inactive
// (webapp/src/sidebar/AGENTS.md, "Visibility"). The selection holds its
// allocations without being visible, so Selected is the third value. It is THE ONE
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
	// Covered reports whether a fold covers it: its root's nearest container's
	// (its subsection's when it sits in one, else its repository section's),
	// or any ancestor's children fold. A collapsed repository section never
	// covers the rows of a subsection that is not itself collapsed.
	Covered bool
	// Working reports whether a turn or detached work runs in it, or it asks
	// the user something.
	Working bool
	// Unviewed reports whether its last turn ended with a result the user has
	// not read: its name is still drawn full.
	Unviewed bool
	// Starting reports whether its session is booting or not yet created: the
	// grey initializing mark (roster arm `init`) or the gap before it (`none`).
	Starting bool
	// TopPriority reports whether it has priority P0.5 or P1
	// (Workspace.TopPriority).
	TopPriority bool
	// AlwaysShown reports whether it is a standing mediator or holds its
	// repository's queue bypass mode (Workspace.AlwaysShown).
	AlwaysShown bool
	// Selected reports whether it is the user's selection.
	Selected bool
}

// VisibilityReason is WHY a workspace has its visibility: the one rule of
// the derivation (VisibilityReasons) that decided it. Every workspace has
// exactly one, so the workspaces counted by reason add up to the workspaces
// counted by visibility. Its String is the metrics' closed label vocabulary
// (agentrepl_workspaces_visible{reason}).
type VisibilityReason int

// The visibility reasons, in the PRECEDENCE the derivation applies when
// several of a workspace's inputs hold: the first that holds is its reason.
const (
	// ReasonNotOpen is a workspace that is no Open section row (backlogged,
	// closed, a hidden merge workspace, a thread) and not the selection: not
	// visible.
	ReasonNotOpen VisibilityReason = iota
	// ReasonCovered is an Open workspace a fold covers with nothing that
	// keeps it visible: not visible.
	ReasonCovered
	// ReasonSelected is the user's selection, whatever else holds: Selected.
	ReasonSelected
	// ReasonUncovered is an Open workspace no fold covers.
	ReasonUncovered
	// ReasonCoveredWorking is a covered workspace a turn or detached work runs
	// in, or that asks the user something.
	ReasonCoveredWorking
	// ReasonCoveredUnviewed is a covered workspace whose last result is unread.
	ReasonCoveredUnviewed
	// ReasonCoveredStarting is a covered workspace whose session is booting
	// or not yet created.
	ReasonCoveredStarting
	// ReasonCoveredTopPriority is a covered workspace of priority P0.5 or P1.
	ReasonCoveredTopPriority
	// ReasonCoveredAlwaysShown is a covered standing mediator or queue-bypass
	// holder, which a fold never hides (Workspace.AlwaysShown).
	ReasonCoveredAlwaysShown
	// ReasonParentOfVisible is a covered workspace none of whose own inputs
	// keeps it visible, visible only because a workspace under it is (the
	// selection included): the unbroken chain of rows above every visible one.
	ReasonParentOfVisible
)

// VisibilityReasonsAll is every reason, in precedence order, for a caller
// that states a value for each (a gauge set to 0 for a reason none holds).
var VisibilityReasonsAll = []VisibilityReason{
	ReasonNotOpen, ReasonCovered, ReasonSelected, ReasonUncovered, ReasonCoveredWorking,
	ReasonCoveredUnviewed, ReasonCoveredStarting, ReasonCoveredTopPriority, ReasonCoveredAlwaysShown, ReasonParentOfVisible,
}

// String names the reason as the metrics label it.
func (r VisibilityReason) String() string {
	switch r {
	case ReasonNotOpen:
		return "not_open"
	case ReasonCovered:
		return "covered"
	case ReasonSelected:
		return "selected"
	case ReasonUncovered:
		return "uncovered"
	case ReasonCoveredWorking:
		return "covered_working"
	case ReasonCoveredUnviewed:
		return "covered_unviewed"
	case ReasonCoveredStarting:
		return "covered_starting"
	case ReasonCoveredTopPriority:
		return "covered_top_priority"
	case ReasonCoveredAlwaysShown:
		return "covered_always_shown"
	case ReasonParentOfVisible:
		return "parent_of_visible"
	default:
		return fmt.Sprintf("visibility_reason(%d)", int(r))
	}
}

// Visibility answers the visibility the reason decides.
func (r VisibilityReason) Visibility() Visibility {
	switch r {
	case ReasonNotOpen, ReasonCovered:
		return NotVisible
	case ReasonSelected:
		return Selected
	case ReasonUncovered, ReasonCoveredWorking, ReasonCoveredUnviewed, ReasonCoveredStarting,
		ReasonCoveredTopPriority, ReasonCoveredAlwaysShown, ReasonParentOfVisible:
		return UnselectedVisible
	default:
		panic(fmt.Sprintf("wsm: visibility reason %d decides no visibility", int(r)))
	}
}

// ownReason answers the reason IN's own inputs give it, before any chain or
// selection: the first of the precedence that holds.
func ownReason(in VisibilityInput) VisibilityReason {
	switch {
	case !in.Open:
		return ReasonNotOpen
	case !in.Covered:
		return ReasonUncovered
	case in.Working:
		return ReasonCoveredWorking
	case in.Unviewed:
		return ReasonCoveredUnviewed
	case in.Starting:
		return ReasonCoveredStarting
	case in.TopPriority:
		return ReasonCoveredTopPriority
	case in.AlwaysShown:
		return ReasonCoveredAlwaysShown
	default:
		return ReasonCovered
	}
}

// Visibilities derives every workspace's visibility from INPUTS, THE ONE
// DERIVATION (owner-settled, 2026-11-21):
//
//   - only an Open workspace can be visible;
//   - an Open workspace no fold covers is visible (every top-level workspace
//     of an unfolded subsection, or of an unfolded section when it sits in no
//     subsection, and every child of an uncovered, unfolded parent);
//   - one a fold covers, at any depth, is visible when it is Working,
//     Unviewed or Starting, and an Open workspace of priority P0.5 or P1
//     (TopPriority), a standing mediator or a queue-bypass holder
//     (AlwaysShown) is always visible, whatever covers it;
//   - the parent of a visible workspace is visible, recursively, so every
//     visible workspace has an unbroken chain of rows above it;
//   - the selection is Selected, whatever the rules above say.
//
// Every input's workspace answers; a parent the inputs do not carry ends the
// chain there. It is the visibility of each workspace's reason
// (VisibilityReasons), so the two can never disagree.
func Visibilities(inputs []VisibilityInput) map[WorkspaceID]Visibility {
	reasons := VisibilityReasons(inputs)
	out := make(map[WorkspaceID]Visibility, len(reasons))
	for id, r := range reasons {
		out[id] = r.Visibility()
	}
	return out
}

// VisibilityReasons derives every workspace's visibility reason from INPUTS
// by the rules Visibilities states. A workspace whose own inputs keep it
// visible has that reason (ownReason's precedence); a covered one visible
// only through the chain above a visible workspace is ReasonParentOfVisible;
// the selection is ReasonSelected.
func VisibilityReasons(inputs []VisibilityInput) map[WorkspaceID]VisibilityReason {
	byID := make(map[WorkspaceID]VisibilityInput, len(inputs))
	out := make(map[WorkspaceID]VisibilityReason, len(inputs))
	for _, in := range inputs {
		byID[in.ID] = in
		out[in.ID] = ownReason(in)
	}
	// mark makes every Open workspace from PARENT up the chain visible: one
	// covered with no reason of its own is visible as a parent. The chain
	// ends at a workspace that is not Open, or not carried, or already
	// visible (whose own chain was marked when it was).
	mark := func(parent *WorkspaceID) {
		for parent != nil {
			id := *parent
			in, ok := byID[id]
			if !ok || !in.Open || out[id].Visibility().Visible() {
				return
			}
			out[id] = ReasonParentOfVisible
			parent = in.Parent
		}
	}
	for _, in := range inputs {
		if out[in.ID].Visibility().Visible() {
			mark(in.Parent)
		}
	}
	for _, in := range inputs {
		if !in.Selected {
			continue
		}
		// THE SELECTION'S PARENTS ARE VISIBLE TOO: the selection is a kind
		// of visible, so the chain above it is unbroken.
		mark(in.Parent)
		out[in.ID] = ReasonSelected
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
// the file records: the open rows, their folds, their parents, their
// priorities and the selection. Whether a workspace works, starts, or its
// result is unread, is the roster's live fact and no part of the file, so
// the seed reads all three as false;
// the roster's first render after the boot writes them in. The boot that
// migrates therefore starts nothing for a workspace only those facts made
// visible, which the roster then opens.
func seedVisibility(ctx context.Context, tx *sql.Tx, log dlog.Logger) error {
	rows, err := tx.QueryContext(ctx, `SELECT id, parent_id, closed, hidden, merge_of IS NOT NULL AND merge_revealed = 0 AND merged_at IS NULL, kind, is_current, priority FROM workspaces`)
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
			priority                         sql.NullInt64
		)
		if err := rows.Scan(&id, &parent, &closed, &hidden, &mergeHidden, &kind, &cur, &priority); err != nil {
			rows.Close()
			return fmt.Errorf("wsm: scan a workspace to seed its visibility: %w", err)
		}
		in := VisibilityInput{
			ID: id, Open: !closed && !hidden && !mergeHidden && kind != string(WorkspaceKindThread), Selected: cur,
			TopPriority: priority.Valid && isTopPriority(Priority(priority.Int64)),
		}
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
