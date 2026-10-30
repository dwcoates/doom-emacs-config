package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"slices"
	"strings"

	"agent-repld/internal/dlog"
)

// A FAMILY STAYS IN ONE LIST OF THE ROSTER (agentrepl.v1 family.proto). A
// child workspace merges into its parent's worktree and is drawn and tabbed
// right after it, so a parent never leaves the Open section while a child is
// open, and no open child is made under a parent outside it. The rules, each
// checked here IN THE SAME TRANSACTION AS THE WRITE IT GUARDS, so no race
// between a parent's close and a child's creation can leave an open child
// under a closed parent:
//
//   - A WORKSPACE WITH OPEN CHILDREN STAYS OPEN (refuseOpenChildren): it is
//     not hidden (SetHidden), closed by the user or an agent (MarkClosed), or
//     queued for a merge whose landing closes it (requestMerge). An open
//     workspace whose merge LANDS INTO it (Workspace.LandsInto) holds it open
//     the same way, child or not: its branch is where that work goes.
//   - A CHILD IS ONLY EVER OPEN UNDER AN OPEN PARENT (requireOpenParent): it
//     is not registered (RegisterWorkspace), re-parented (SetParent),
//     re-opened (Reopen) or shown (SetHidden) under a parent that is hidden,
//     closed, or CLOSING (a merge waiting in its queue closes it on landing).
//
// A HIDDEN CHILD COUNTS AS OPEN: hiding keeps its session, so a family is
// hidden child first. A HIDDEN MERGE WORKSPACE (Workspace.MergeHidden) is no
// open child: it is an async merge's machinery, drawn nowhere until revealed,
// and its requester stays free to be hidden or closed while it runs.
//
// The daemon's own closes (CloseByMerge, CloseByDaemon) are not refused: they
// record what already happened (a branch landed, a directory vanished). A
// family that broke the rules before they existed keeps drawing by the old
// rule: an open workspace whose parent is not open takes its parent's place
// (resolve/sidebar's openTree).

// FamilyRef names one workspace in a family refusal: its id and its dir, as
// the registry holds them.
type FamilyRef struct {
	ID  WorkspaceID
	Dir string
}

// HasOpenChildrenError refuses a write that would take Parent out of the Open
// section while Children are open. Nothing was written.
type HasOpenChildrenError struct {
	// Parent is the workspace the write would have taken out of Open.
	Parent FamilyRef
	// Children is every open descendant of Parent, hidden ones included, in
	// place order. Empty only when LandingInto is not.
	Children []FamilyRef
	// LandingInto is every open workspace, not among Children, whose merge
	// lands into Parent's branch, in place order. Empty only when Children is
	// not.
	LandingInto []FamilyRef
}

// Error implements error.
func (e *HasOpenChildrenError) Error() string {
	ids := make([]string, 0, len(e.Children)+len(e.LandingInto))
	for _, ref := range append(slices.Clone(e.Children), e.LandingInto...) {
		ids = append(ids, string(ref.ID))
	}
	return fmt.Sprintf("wsm: workspace %s still has open children or workspaces landing into it: %s", e.Parent.ID, strings.Join(ids, ", "))
}

// ParentState is why a parent is not open.
type ParentState int

// The states ParentNotOpenError names.
const (
	// ParentHidden is a parent in the hidden section, or a hidden merge
	// workspace.
	ParentHidden ParentState = iota + 1
	// ParentClosed is a closed parent.
	ParentClosed
	// ParentClosing is an open parent a merge waiting in its repository's
	// queue closes once it lands.
	ParentClosing
)

// String names the state for records, as the contract's arm does.
func (s ParentState) String() string {
	switch s {
	case ParentHidden:
		return "hidden"
	case ParentClosed:
		return "closed"
	case ParentClosing:
		return "closing"
	}
	return fmt.Sprintf("ParentState(%d)", int(s))
}

// ParentNotOpenError refuses a write that would make an open child of a
// parent outside the Open section. Nothing was written.
type ParentNotOpenError struct {
	// Parent is the parent that blocks the write.
	Parent FamilyRef
	// State is why it is not open.
	State ParentState
}

// Error implements error.
func (e *ParentNotOpenError) Error() string {
	return fmt.Sprintf("wsm: parent %s is %s", e.Parent.ID, e.State)
}

// IsFamilyRefusal reports whether err is one of the family rules refusing a
// write: an answer to what was asked, never a store failure.
func IsFamilyRefusal(err error) bool {
	var children *HasOpenChildrenError
	var parent *ParentNotOpenError
	return errors.As(err, &children) || errors.As(err, &parent)
}

// mergeHiddenSQL is the SQL spelling of Workspace.MergeHidden, over a
// workspaces row aliased `w`; TestMergeHiddenSQLAgreesWithMergeHidden holds
// the two spellings together.
const mergeHiddenSQL = `(w.merge_of IS NOT NULL AND w.merge_revealed = 0 AND w.merged_at IS NULL)`

// openChildSQL is the SQL spelling of an OPEN CHILD, over a workspaces row
// aliased `w`: not closed, and not a hidden merge workspace.
const openChildSQL = `w.closed = 0 AND NOT ` + mergeHiddenSQL

// openDescendants answers every open descendant of ID (openChildSQL), at
// every depth, in place order. A closed child's own open children count: the
// rule is about the parent's whole family, legacy rows included.
func openDescendants(ctx context.Context, q querier, id WorkspaceID) ([]FamilyRef, error) {
	return listRows(ctx, q, `WITH RECURSIVE family(id) AS (
			SELECT id FROM workspaces WHERE parent_id = ?
			UNION
			SELECT c.id FROM workspaces c JOIN family f ON c.parent_id = f.id
		)
		SELECT w.id, w.dir FROM workspaces w JOIN family f ON w.id = f.id
		WHERE `+openChildSQL+` ORDER BY w.place`,
		func(row interface{ Scan(...any) error }) (FamilyRef, error) {
			var ref FamilyRef
			return ref, row.Scan(&ref.ID, &ref.Dir)
		}, id)
}

// openLanders answers every open workspace (openChildSQL) whose merge lands
// into ID's branch -- whose creation job's target directory is ID's worktree --
// and that is not among ID's open descendants, in place order. A repository's
// main workspace has none: a merge onto the primary branch lands in the main
// checkout without landing into the workspace registered there
// (landsIntoColumn reads the same way).
func openLanders(ctx context.Context, q querier, id WorkspaceID, descendants []FamilyRef) ([]FamilyRef, error) {
	landers, err := listRows(ctx, q, `SELECT w.id, w.dir FROM workspaces w
		JOIN creation_jobs c ON c.workspace_id = w.id
		JOIN workspaces target ON target.dir = c.target_dir
		JOIN repositories r ON r.id = target.repo_id
		WHERE target.id = ? AND w.id != target.id AND target.dir != r.dir AND `+openChildSQL+` ORDER BY w.place`,
		func(row interface{ Scan(...any) error }) (FamilyRef, error) {
			var ref FamilyRef
			return ref, row.Scan(&ref.ID, &ref.Dir)
		}, id)
	if err != nil {
		return nil, err
	}
	return slices.DeleteFunc(landers, func(ref FamilyRef) bool {
		return slices.ContainsFunc(descendants, func(d FamilyRef) bool { return d.ID == ref.ID })
	}), nil
}

// OpenChildren answers PARENT's open direct children (openChildSQL), in place
// order: the workspaces it coordinates (agentrepl.v1.WorkspaceStatus.children).
func (s *store) OpenChildren(ctx context.Context, parent WorkspaceID) ([]FamilyRef, error) {
	var out []FamilyRef
	err := s.read(ctx, "daemon.wsm.open_children", dlog.Context{"parent": string(parent)}, func(ctx context.Context) error {
		var err error
		out, err = listRows(ctx, s.q(ctx), `SELECT w.id, w.dir FROM workspaces w WHERE w.parent_id = ? AND w.id != w.parent_id AND `+openChildSQL+` ORDER BY w.place`,
			func(row interface{ Scan(...any) error }) (FamilyRef, error) {
				var ref FamilyRef
				return ref, row.Scan(&ref.ID, &ref.Dir)
			}, parent)
		return err
	})
	return out, err
}

// familyQuerier is what a family rule reads through: the write's own
// transaction, or the handle for a verb's read ahead of it.
type familyQuerier interface {
	querier
	rowQuerier
}

// refuseOpenChildren answers HasOpenChildrenError when ID has an open
// descendant or an open workspace landing into it, nil when it has neither.
func refuseOpenChildren(ctx context.Context, q familyQuerier, id WorkspaceID) error {
	children, err := openDescendants(ctx, q, id)
	if err != nil {
		return err
	}
	landers, err := openLanders(ctx, q, id, children)
	if err != nil {
		return err
	}
	if len(children) == 0 && len(landers) == 0 {
		return nil
	}
	var dir string
	if err := scanWorkspaceRow(ctx, q, id, "dir", &dir); err != nil {
		return err
	}
	return &HasOpenChildrenError{Parent: FamilyRef{ID: id, Dir: dir}, Children: children, LandingInto: landers}
}

// closingSQL is the SQL spelling of MergeSource.Closes over every merge in
// the queue, in any state: whether one of them closes the workspace once it
// lands. Its placeholders are, in order: the workspace, the own-branch kind,
// the merged-upstream kind, the workspace kind, the workspace, the own-branch
// kind, the workspace, the workspace (closingArgs).
// TestClosingSQLAgreesWithMergeSourceCloses holds the two spellings together.
const closingSQL = `SELECT EXISTS (SELECT 1 FROM merge_queue q LEFT JOIN workspaces r ON r.id = q.workspace_id WHERE
	(q.source_detach_id IS NULL AND (
		(q.workspace_id = ? AND q.source_kind = ? AND q.source_keep_open = 0)
		OR (q.source_kind = ? AND q.source_workspace = ?)
		OR (q.source_kind = ? AND r.merge_close_on_landing = ?)))
	OR (q.source_detach_id IS NOT NULL AND q.source_close_on_landing = ?))`

// closingArgs binds closingSQL for workspace ID.
func closingArgs(id WorkspaceID) []any {
	return []any{id, int(MergeSourceOwnBranch), int(MergeSourceWorkspace), id, int(MergeSourceOwnBranch), id, id}
}

// isClosing reports whether a merge in the queue, in any state, closes ID
// once it lands.
func isClosing(ctx context.Context, q rowQuerier, id WorkspaceID) (bool, error) {
	var closing bool
	err := q.QueryRowContext(ctx, closingSQL, closingArgs(id)...).Scan(&closing)
	return closing, err
}

// requireOpenParent answers ParentNotOpenError when PARENT is hidden (or a
// hidden merge workspace), closed, or closing, nil when it is open. An unknown
// parent is ErrNotFound.
func requireOpenParent(ctx context.Context, q rowQuerier, parent WorkspaceID) error {
	var (
		dir                    string
		closed, hidden, merged bool
	)
	err := q.QueryRowContext(ctx, `SELECT w.dir, w.closed, w.hidden, `+mergeHiddenSQL+` FROM workspaces w WHERE w.id = ?`, parent).
		Scan(&dir, &closed, &hidden, &merged)
	if errors.Is(err, sql.ErrNoRows) {
		return fmt.Errorf("wsm: parent %s: %w", parent, ErrNotFound)
	}
	if err != nil {
		return err
	}
	ref := FamilyRef{ID: parent, Dir: dir}
	switch {
	case closed:
		return &ParentNotOpenError{Parent: ref, State: ParentClosed}
	case hidden || merged:
		return &ParentNotOpenError{Parent: ref, State: ParentHidden}
	}
	closing, err := isClosing(ctx, q, parent)
	if err != nil {
		return err
	}
	if closing {
		return &ParentNotOpenError{Parent: ref, State: ParentClosing}
	}
	return nil
}

// requireOpenParentOf answers requireOpenParent for ID's own recorded parent,
// nil for a top-level workspace.
func requireOpenParentOf(ctx context.Context, tx *sql.Tx, id WorkspaceID) error {
	var parent sql.NullString
	if err := scanWorkspaceRow(ctx, tx, id, "parent_id", &parent); err != nil {
		return err
	}
	if !parent.Valid {
		return nil
	}
	return requireOpenParent(ctx, tx, WorkspaceID(parent.String))
}

// RequireOpenParent answers ParentNotOpenError when PARENT is not open, nil
// when it is: the read a verb makes BEFORE work it could not undo (a worktree
// materialized for a child, a session brought up for a re-open). The write
// checks again in its own transaction, which is the guarantee; this read only
// spares the work.
func (s *store) RequireOpenParent(ctx context.Context, parent WorkspaceID) error {
	return s.familyRead(ctx, "daemon.wsm.require_open_parent", dlog.Context{"parent": string(parent)}, func(ctx context.Context) error {
		return requireOpenParent(ctx, s.q(ctx), parent)
	})
}

// RequireNoOpenChildren answers HasOpenChildrenError for the first of IDS
// with an open descendant, nil when none has: the read a verb makes BEFORE
// work it could not undo (a merge's held prompts flushed into the session).
// The write checks again in its own transaction, which is the guarantee.
func (s *store) RequireNoOpenChildren(ctx context.Context, ids ...WorkspaceID) error {
	return s.familyRead(ctx, "daemon.wsm.require_no_open_children", dlog.Context{"workspaces": len(ids)}, func(ctx context.Context) error {
		for _, id := range ids {
			if err := refuseOpenChildren(ctx, s.q(ctx), id); err != nil {
				return err
			}
		}
		return nil
	})
}

// familyRead runs a family rule's read ahead of a write. The rule refusing is
// an ANSWER, handed back to the caller to state at its own level; only a read
// that failed is a failure, and read records it.
func (s *store) familyRead(ctx context.Context, op string, fields dlog.Context, rule func(context.Context) error) error {
	var refusal error
	err := s.read(ctx, op, fields, func(ctx context.Context) error {
		err := rule(ctx)
		if IsFamilyRefusal(err) {
			refusal = err
			return nil
		}
		return err
	})
	if err != nil {
		return err
	}
	return refusal
}

// LandingCloses answers what a merge of this source closes once it lands,
// REQUESTER being the workspace whose queue entry it is and ORIGIN the
// requester's own merge origin (nil unless it is a merge workspace):
//
//   - CLOSES, the workspace the source itself concludes: the requester for an
//     own-branch merge that does not keep it open and for a branch already
//     merged upstream, the other workspace for a workspace source, none for a
//     named branch;
//   - ALSO, the workspace an async merge's one-shot names
//     (MergeOrigin.CloseOnLanding), which its merge workspace's own-branch
//     merge closes as well.
//
// A DETACHED REQUEST closes nothing itself: it never lands in place, and the
// merge workspace it hands off to carries its landing (HandOffDetachedMerge).
// The merge run reads its subject's closes from here, so the family rules and
// the landing can never disagree about what a merge closes.
func (m MergeSource) LandingCloses(requester WorkspaceID, origin *MergeOrigin) (closes, also WorkspaceID) {
	if m.Detach != "" {
		return "", ""
	}
	switch m.Kind {
	case MergeSourceOwnBranch:
		if !m.KeepOpen {
			closes = requester
		}
		if origin != nil {
			also = origin.CloseOnLanding
		}
	case MergeSourceWorkspace:
		closes = m.Workspace
	}
	return closes, also
}

// Closes answers every workspace a merge of this source closes once it lands
// or, for a detached request, once the merge workspace it hands off to lands:
// LandingCloses's two, and the one-shot a detached request names. Each once,
// none empty. closingSQL is its SQL spelling.
func (m MergeSource) Closes(requester WorkspaceID, origin *MergeOrigin) []WorkspaceID {
	closes, also := m.LandingCloses(requester, origin)
	if m.Detach != "" {
		also = m.CloseOnLanding
	}
	var out []WorkspaceID
	for _, ws := range []WorkspaceID{closes, also} {
		if ws != "" && !slices.Contains(out, ws) {
			out = append(out, ws)
		}
	}
	return out
}

// mergeOriginOf answers workspace ID's merge origin, nil unless it is a merge
// workspace. An unknown workspace is ErrNotFound.
func mergeOriginOf(ctx context.Context, tx *sql.Tx, id WorkspaceID) (*MergeOrigin, error) {
	ws, err := scanWorkspace(tx.QueryRowContext(ctx, `SELECT `+workspaceColumns+` FROM workspaces WHERE id = ?`, id))
	if errors.Is(err, sql.ErrNoRows) {
		return nil, fmt.Errorf("wsm: workspace %s: %w", id, ErrNotFound)
	}
	if err != nil {
		return nil, err
	}
	return ws.MergeOf, nil
}
