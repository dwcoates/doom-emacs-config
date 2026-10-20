package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"slices"
	"sort"
)

// THE REPOSITORY WALK is one order of a repository's entries: its ROOT
// workspaces in no subsection and its SUBSECTIONS, INTERLEAVED, so a
// subsection may stand between two unsectioned workspaces (agentrepl.v1
// ArrangeSidebar). The sidebar draws a repository's section in it and the
// tab bar walks it, a subsection's own workspaces where the subsection
// stands.
//
// IT IS HELD BY TWO COLUMNS AND NO THIRD ORDER. The unsectioned roots keep
// their registry place (place.go); a subsection keeps its `slot`, a value in
// that same place space: it stands just before the first unsectioned root
// whose place is at least its slot, and after every one when its slot is
// NULL. Subsections standing at one point keep their own `place` order. So:
//
//   - a root that leaves a point (dragged elsewhere, closed, forgotten,
//     filed into a subsection) moves no subsection: the slot names a point
//     in the order, not a workspace;
//   - a landing that shifts places up (shiftFrom) shifts every slot past the
//     freed place too, so no root crosses a subsection;
//   - every arrangement write goes through ONE pair, readWalk and writeWalk,
//     which permutes the repository's roots over the places they already
//     held and restates each slot from the walk it wrote. No other
//     workspace's place moves, so no other repository's slots can go stale.

// subsectionSlotsDDL is the layout-66 addition: where a subsection stands
// among its repository's unsectioned workspaces.
//
// IT IS ADDITIVE: every existing subsection takes NULL, after every
// unsectioned workspace, which is where every subsection drew before.
const subsectionSlotsDDL = `
ALTER TABLE subsections ADD COLUMN slot INTEGER;
`

// WalkEntry is one entry of a repository's walk: a root in no subsection
// (Subsection nil), or a subsection.
type WalkEntry[R any] struct {
	Root       R
	Subsection *Subsection
}

// Walk interleaves ROOTS, one repository's roots in no subsection, with
// SUBS, its subsections, into the repository's walk: roots in ascending
// PLACE, subsections in ascending Place, each subsection just before the
// first root whose place is at least its Slot, after every root when it has
// none. THE ONE STATEMENT OF THE INTERLEAVE: the store's writes and the
// roster's drawing both read it here. It sorts copies.
func Walk[R any](roots []R, place func(R) int64, subs []Subsection) []WalkEntry[R] {
	rs := slices.Clone(roots)
	sort.SliceStable(rs, func(i, j int) bool { return place(rs[i]) < place(rs[j]) })
	ss := slices.Clone(subs)
	sort.SliceStable(ss, func(i, j int) bool { return ss[i].Place < ss[j].Place })
	out := make([]WalkEntry[R], 0, len(rs)+len(ss))
	next := 0
	for k := range ss {
		for next < len(rs) && (ss[k].Slot == nil || place(rs[next]) < *ss[k].Slot) {
			out = append(out, WalkEntry[R]{Root: rs[next]})
			next++
		}
		out = append(out, WalkEntry[R]{Subsection: &ss[k]})
	}
	for ; next < len(rs); next++ {
		out = append(out, WalkEntry[R]{Root: rs[next]})
	}
	return out
}

// Anchor is where an arrangement puts something among a list's entries:
// BeforeWorkspace, BeforeSubsection, or AtEnd.
type Anchor interface{ anchor() }

// BeforeWorkspace is just before root workspace ID.
type BeforeWorkspace struct{ ID WorkspaceID }

// BeforeSubsection is just before subsection ID.
type BeforeSubsection struct{ ID SubsectionID }

// AtEnd is after every entry of the list.
type AtEnd struct{}

func (BeforeWorkspace) anchor()  {}
func (BeforeSubsection) anchor() {}
func (AtEnd) anchor()            {}

// walkStep is one entry of a stored walk: a root in no subsection, or a
// subsection and its roots in registry order.
type walkStep struct {
	root    WorkspaceID
	sub     *Subsection
	members []WorkspaceID
}

// repoWalk is one repository's stored walk, read and written whole.
type repoWalk []walkStep

// rootAt is one root workspace and its registry place.
type rootAt struct {
	id    WorkspaceID
	place int64
}

// readWalk reads REPO's walk: every root workspace of it, closed ones
// included (a closed root keeps its point, and its open children are drawn
// there), and every subsection.
func readWalk(ctx context.Context, tx *sql.Tx, repo RepoID) (repoWalk, error) {
	rows, err := tx.QueryContext(ctx,
		`SELECT id, place, subsection_id FROM workspaces WHERE repo_id = ? AND parent_id IS NULL ORDER BY place`, repo)
	if err != nil {
		return nil, err
	}
	defer rows.Close()
	var unsectioned []rootAt
	members := map[SubsectionID][]WorkspaceID{}
	for rows.Next() {
		var root rootAt
		var sub sql.NullString
		if err := rows.Scan(&root.id, &root.place, &sub); err != nil {
			return nil, err
		}
		if sub.Valid {
			members[SubsectionID(sub.String)] = append(members[SubsectionID(sub.String)], root.id)
			continue
		}
		unsectioned = append(unsectioned, root)
	}
	if err := rows.Err(); err != nil {
		return nil, err
	}
	subs, err := listRows(ctx, tx, `SELECT `+subsectionColumns+` FROM subsections WHERE repo_id = ? ORDER BY place`, scanSubsection, repo)
	if err != nil {
		return nil, err
	}
	var out repoWalk
	for _, entry := range Walk(unsectioned, func(r rootAt) int64 { return r.place }, subs) {
		if entry.Subsection == nil {
			out = append(out, walkStep{root: entry.Root.id})
			continue
		}
		out = append(out, walkStep{sub: entry.Subsection, members: members[entry.Subsection.ID]})
	}
	return out, nil
}

// roots answers every root of the walk in its order, a subsection's where it
// stands.
func (w repoWalk) roots() []WorkspaceID {
	var out []WorkspaceID
	for _, step := range w {
		if step.sub == nil {
			out = append(out, step.root)
			continue
		}
		out = append(out, step.members...)
	}
	return out
}

// without answers the walk less root workspaces IDS, wherever they stand.
func (w repoWalk) without(ids []WorkspaceID) repoWalk {
	out := make(repoWalk, 0, len(w))
	for _, step := range w {
		if step.sub == nil {
			if !slices.Contains(ids, step.root) {
				out = append(out, step)
			}
			continue
		}
		step.members = slices.DeleteFunc(slices.Clone(step.members), func(id WorkspaceID) bool { return slices.Contains(ids, id) })
		out = append(out, step)
	}
	return out
}

// subsectionIndex answers the index of subsection ID's step, -1 when the walk
// holds none.
func (w repoWalk) subsectionIndex(id SubsectionID) int {
	return slices.IndexFunc(w, func(step walkStep) bool { return step.sub != nil && step.sub.ID == id })
}

// rootIndex answers the index of unsectioned root ID's step, -1 when the
// walk holds none.
func (w repoWalk) rootIndex(id WorkspaceID) int {
	return slices.IndexFunc(w, func(step walkStep) bool { return step.sub == nil && step.root == id })
}

// rootSteps answers one unsectioned step per root of IDS.
func rootSteps(ids []WorkspaceID) []walkStep {
	out := make([]walkStep, 0, len(ids))
	for _, id := range ids {
		out = append(out, walkStep{root: id})
	}
	return out
}

// errAnchorKind is an anchor no list of the arrangement takes.
var errAnchorKind = errors.New("wsm: an anchor of no known kind")

// entryIndex answers where in W an entry placed at AT goes among the
// repository's entries: before an unsectioned root (ErrAnchorElsewhere when
// it is none), before a subsection (ErrUnknownSubsection when the walk holds
// none: the caller has already refused another repository's), or at the end.
func (w repoWalk) entryIndex(at Anchor) (int, error) {
	switch a := at.(type) {
	case BeforeWorkspace:
		i := w.rootIndex(a.ID)
		if i < 0 {
			return 0, ErrAnchorElsewhere
		}
		return i, nil
	case BeforeSubsection:
		i := w.subsectionIndex(a.ID)
		if i < 0 {
			return 0, &UnknownSubsectionError{ID: a.ID}
		}
		return i, nil
	case AtEnd:
		return len(w), nil
	default:
		return 0, fmt.Errorf("%w: %T", errAnchorKind, at)
	}
}

// writeWalk stores W, a reordering of a walk readWalk answered: each root
// takes, in the walk's order, the places those roots already held, ascending,
// and the subsection its step names (none for an unsectioned step); each
// subsection takes its ordinal among the walk's subsections as its place and
// the place of the first unsectioned root after it as its slot (NULL when
// none follows). No workspace outside the walk moves.
func writeWalk(ctx context.Context, tx *sql.Tx, w repoWalk) error {
	places, err := permutePlaces(ctx, tx, w.roots())
	if err != nil {
		return err
	}
	for _, step := range w {
		if step.sub == nil {
			if _, err := tx.ExecContext(ctx, `UPDATE workspaces SET subsection_id = NULL WHERE id = ?`, step.root); err != nil {
				return err
			}
			continue
		}
		for _, id := range step.members {
			if _, err := tx.ExecContext(ctx, `UPDATE workspaces SET subsection_id = ? WHERE id = ?`, step.sub.ID, id); err != nil {
				return err
			}
		}
	}
	ordinal := int64(0)
	for i, step := range w {
		if step.sub == nil {
			continue
		}
		ordinal++
		var slot any
		for _, after := range w[i+1:] {
			if after.sub == nil {
				slot = places[after.root]
				break
			}
		}
		// Through a negative place first, so UNIQUE (repo_id, place) never
		// sees two subsections on one place mid-way.
		if _, err := tx.ExecContext(ctx, `UPDATE subsections SET place = ?, slot = ? WHERE id = ?`, -ordinal, slot, step.sub.ID); err != nil {
			return err
		}
	}
	_, err = tx.ExecContext(ctx, `UPDATE subsections SET place = -place WHERE place < 0`)
	return err
}

// permutePlaces gives the workspaces of ORDER the places they already hold
// between them, ascending, in ORDER's order, and answers each one's new
// place. Every other workspace keeps its own. Every row goes through a
// negative place first, so the unique index never sees two rows on one
// place mid-way.
func permutePlaces(ctx context.Context, tx *sql.Tx, order []WorkspaceID) (map[WorkspaceID]int64, error) {
	held := make([]int64, 0, len(order))
	for _, id := range order {
		var at int64
		if err := scanWorkspaceRow(ctx, tx, id, "place", &at); err != nil {
			return nil, err
		}
		held = append(held, at)
	}
	slices.Sort(held)
	out := make(map[WorkspaceID]int64, len(order))
	for i, id := range order {
		out[id] = held[i]
		if _, err := tx.ExecContext(ctx, `UPDATE workspaces SET place = ? WHERE id = ?`, -held[i], id); err != nil {
			return nil, err
		}
	}
	_, err := tx.ExecContext(ctx, `UPDATE workspaces SET place = -place WHERE place < 0`)
	return out, err
}
