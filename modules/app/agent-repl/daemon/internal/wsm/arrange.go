package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"slices"
	"strings"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// THE SIDEBAR'S ARRANGEMENT is three durable orders (agentrepl.v1
// ArrangeSidebar), which the repository grouping draws and the tab bar walks:
//
//   - the REPOSITORIES' order, each row's `place` (unique, at least 1);
//   - each repository's WALK (walk.go): its roots in no subsection and its
//     SUBSECTIONS interleaved, a subsection standing where its `slot` says
//     among the unsectioned roots' registry places, subsections standing at
//     one point in their `place` order (unique within the repository);
//   - which subsection a ROOT workspace sits in (`subsection_id`, NULL for
//     none), its place among the others there being its registry place
//     (place.go).
//
// A FAMILY NEVER SPLITS ACROSS SUBSECTIONS (owner ruling, 2026-10-14): a
// workspace with a recorded parent sits wherever its parent does, at every
// depth. That is held by construction, not by every writer remembering to
// move a family: only a root STORES a subsection (the
// workspaces_child_stores_no_subsection triggers refuse a child that stores
// one), and every read derives a workspace's subsection from its root
// (rootSubsectionColumn). Moving a root moves its whole family; a child alone
// cannot be moved to a subsection at all (ErrHasParent).
//
// A WORKSPACE SITS ONLY IN A SUBSECTION OF ITS OWN REPOSITORY. The
// workspaces_subsection_same_repo triggers make SQLite refuse any write that
// would say otherwise, so the roster never has to decide what such a row
// means. Deleting a subsection returns its families to none (ON DELETE SET
// NULL); a repository's subsections go with the repository (ON DELETE
// CASCADE).

// arrangementDDL is the layout-58 addition: the repositories' place, the
// subsections table, and a workspace's subsection.
//
// IT IS ADDITIVE: the previous build names none of these columns. A
// repository it inserts states no place and takes 0, and
// repositories_land_unplaced moves it after every other one in the same
// statement, exactly as workspaces_land_unplaced does for a workspace. Every
// existing repository is placed in the order the roster drew repositories
// before (name, then id), so the first boot after the step draws the same
// picture.
const arrangementDDL = `
ALTER TABLE repositories ADD COLUMN place INTEGER NOT NULL DEFAULT 0;
UPDATE repositories SET place = (
  SELECT COUNT(*) FROM repositories AS earlier
   WHERE earlier.name < repositories.name
      OR (earlier.name = repositories.name AND earlier.id <= repositories.id));
CREATE UNIQUE INDEX repositories_by_place ON repositories(place);
CREATE TRIGGER repositories_land_unplaced AFTER INSERT ON repositories WHEN NEW.place = 0
BEGIN
  UPDATE repositories SET place = (SELECT MAX(place) FROM repositories) + 1 WHERE id = NEW.id;
END;
CREATE TABLE subsections (
  id      TEXT PRIMARY KEY,
  repo_id TEXT NOT NULL REFERENCES repositories(id) ON DELETE CASCADE,
  name    TEXT NOT NULL CHECK (trim(name) <> ''),
  place   INTEGER NOT NULL,
  UNIQUE (repo_id, place)
);
ALTER TABLE workspaces ADD COLUMN subsection_id TEXT REFERENCES subsections(id) ON DELETE SET NULL;
CREATE TRIGGER workspaces_subsection_same_repo_insert BEFORE INSERT ON workspaces
WHEN NEW.subsection_id IS NOT NULL
 AND NEW.repo_id IS NOT (SELECT repo_id FROM subsections WHERE id = NEW.subsection_id)
BEGIN
  SELECT RAISE(ABORT, 'wsm: a workspace sits only in a subsection of its own repository');
END;
CREATE TRIGGER workspaces_subsection_same_repo_update BEFORE UPDATE OF subsection_id, repo_id ON workspaces
WHEN NEW.subsection_id IS NOT NULL
 AND NEW.repo_id IS NOT (SELECT repo_id FROM subsections WHERE id = NEW.subsection_id)
BEGIN
  SELECT RAISE(ABORT, 'wsm: a workspace sits only in a subsection of its own repository');
END;
CREATE TRIGGER workspaces_child_stores_no_subsection_insert BEFORE INSERT ON workspaces
WHEN NEW.parent_id IS NOT NULL AND NEW.subsection_id IS NOT NULL
BEGIN
  SELECT RAISE(ABORT, 'wsm: a child workspace sits wherever its parent does and stores no subsection');
END;
CREATE TRIGGER workspaces_child_stores_no_subsection_update BEFORE UPDATE OF parent_id, subsection_id ON workspaces
WHEN NEW.parent_id IS NOT NULL AND NEW.subsection_id IS NOT NULL
BEGIN
  SELECT RAISE(ABORT, 'wsm: a child workspace sits wherever its parent does and stores no subsection');
END;
`

// rootSubsectionColumn is THE ONE READ of a workspace's subsection, as a
// column of a statement over `workspaces`: the subsection its ROOT stores,
// found by walking the recorded parents up from the row. A child's own
// column is always NULL (the child triggers), so this is where it learns
// where its family sits. A parent chain that cycles reaches no root and reads
// as none; ordinary creation cannot make one. UNION, not UNION ALL, so a
// cycle ends the walk rather than looping.
const rootSubsectionColumn = `(WITH RECURSIVE up(id, parent_id, subsection_id) AS (
  SELECT own.id, own.parent_id, own.subsection_id FROM workspaces AS own WHERE own.id = workspaces.id
  UNION
  SELECT anc.id, anc.parent_id, anc.subsection_id FROM workspaces AS anc JOIN up ON anc.id = up.parent_id
) SELECT subsection_id FROM up WHERE parent_id IS NULL)`

// ErrUnknownSubsection is a subsection id no subsection carries. It wraps
// ErrNotFound, so a caller asking only "was it there" still reads it.
var ErrUnknownSubsection = fmt.Errorf("wsm: unknown subsection: %w", ErrNotFound)

// UnknownSubsectionError names the subsection id no subsection carries. It is
// ErrUnknownSubsection (and so ErrNotFound) to errors.Is.
type UnknownSubsectionError struct {
	ID SubsectionID
}

func (e *UnknownSubsectionError) Error() string {
	return fmt.Sprintf("wsm: subsection %s: %v", e.ID, ErrUnknownSubsection)
}

// Unwrap makes the error ErrUnknownSubsection to errors.Is.
func (e *UnknownSubsectionError) Unwrap() error { return ErrUnknownSubsection }

// ErrUnknownRepository is a repository id no repository carries. It wraps
// ErrNotFound, as ErrUnknownSubsection does.
var ErrUnknownRepository = fmt.Errorf("wsm: unknown repository: %w", ErrNotFound)

// ErrOtherRepository refuses an arrangement that relates two things of
// different repositories: a subsection placed before another repository's,
// a workspace put into another repository's subsection, or a workspace moved
// onto another repository's place. Nothing is written.
var ErrOtherRepository = errors.New("wsm: the arrangement relates two repositories")

// ErrAnchorElsewhere refuses placing a workspace before one that does not
// sit in the destination (or that moves with it). Nothing is written.
var ErrAnchorElsewhere = errors.New("wsm: the workspace to place before is not in the destination")

// ErrHasParent refuses placing a workspace that has a recorded parent: it
// sits wherever its parent does, and moves only with it. Nothing is written.
var ErrHasParent = errors.New("wsm: the workspace has a parent and moves only with it")

// ErrNotSiblings refuses moving a workspace onto the tab of one that is not
// its sibling: one top-level and the other a child, or children of two
// parents. A child's tab always follows its parent's family, so there is no
// such tab to take. Nothing is written.
var ErrNotSiblings = errors.New("wsm: the two workspaces are not siblings")

// ErrBlankSubsectionName refuses a subsection name that is empty or only
// whitespace. Nothing is written.
var ErrBlankSubsectionName = errors.New("wsm: a subsection name is blank")

// isArrangementRefusal reports whether err is one of the arrangement's own
// answers rather than the store failing: the verb turns each into the rpc's
// arm and records it at its own level.
func isArrangementRefusal(err error) bool {
	return errors.Is(err, ErrOtherRepository) || errors.Is(err, ErrAnchorElsewhere) || errors.Is(err, ErrBlankSubsectionName) ||
		errors.Is(err, ErrHasParent) || errors.Is(err, ErrNotSiblings)
}

// subsectionName trims NAME and refuses it when nothing is left.
func subsectionName(name string) (string, error) {
	trimmed := strings.TrimSpace(name)
	if trimmed == "" {
		return "", ErrBlankSubsectionName
	}
	return trimmed, nil
}

// CreateSubsection records a subsection named NAME (trimmed) after REPO's
// other subsections. A blank name is ErrBlankSubsectionName, an unknown
// repository ErrUnknownRepository.
func (s *store) CreateSubsection(ctx context.Context, repo RepoID, name string) (Subsection, error) {
	const op = "daemon.wsm.create_subsection"
	fields := dlog.Context{"repo_id": string(repo), "name": name}
	trimmed, err := subsectionName(name)
	if err != nil {
		s.log.Debug(op, "refused a blank subsection name", withError(fields, err))
		return Subsection{}, err
	}
	out := Subsection{ID: NewSubsectionID(), Repo: repo, Name: trimmed}
	fields["subsection"] = string(out.ID)
	err = s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		if err := requireRepository(ctx, tx, repo); err != nil {
			return err
		}
		if err := tx.QueryRowContext(ctx,
			`SELECT COALESCE(MAX(place), 0) + 1 FROM subsections WHERE repo_id = ?`, repo).Scan(&out.Place); err != nil {
			return err
		}
		_, err := tx.ExecContext(ctx, `INSERT INTO subsections (id, repo_id, name, place) VALUES (?, ?, ?, ?)`,
			out.ID, out.Repo, out.Name, out.Place)
		return err
	})
	if err != nil {
		return Subsection{}, err
	}
	return out, nil
}

// RenameSubsection relabels subsection ID to NAME (trimmed). A blank name is
// ErrBlankSubsectionName, an unknown subsection ErrUnknownSubsection.
func (s *store) RenameSubsection(ctx context.Context, id SubsectionID, name string) error {
	const op = "daemon.wsm.rename_subsection"
	fields := dlog.Context{"subsection": string(id), "name": name}
	trimmed, err := subsectionName(name)
	if err != nil {
		s.log.Debug(op, "refused a blank subsection name", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `UPDATE subsections SET name = ? WHERE id = ?`, trimmed, id)
		if err != nil {
			return err
		}
		return requireSubsectionRow(res, id)
	})
}

// DeleteSubsection removes subsection ID. Its workspaces return to their
// repository's unsectioned workspaces, keeping their order among themselves,
// and take the subsection's point in the repository's walk: they draw where
// the subsection stood. An unknown subsection is ErrUnknownSubsection.
func (s *store) DeleteSubsection(ctx context.Context, id SubsectionID) error {
	const op = "daemon.wsm.delete_subsection"
	return s.write(ctx, op, dlog.Context{"subsection": string(id)}, func(ctx context.Context, tx *sql.Tx) error {
		sub, err := subsectionRow(ctx, tx, id)
		if err != nil {
			return err
		}
		walk, err := readWalk(ctx, tx, sub.Repo)
		if err != nil {
			return err
		}
		at := walk.subsectionIndex(id)
		rest := slices.Concat(walk[:at], rootSteps(walk[at].members), walk[at+1:])
		// The row goes first, so the walk's renumbering never meets its place.
		if _, err := tx.ExecContext(ctx, `DELETE FROM subsections WHERE id = ?`, id); err != nil {
			return err
		}
		return writeWalk(ctx, tx, rest)
	})
}

// MoveSubsection moves subsection ID among its repository's entries, to TO:
// before another of its subsections, before one of its roots in no
// subsection, or to the bottom. A subsection or workspace of another
// repository is ErrOtherRepository, a workspace that is not a root in no
// subsection ErrAnchorElsewhere, an unknown subsection ErrUnknownSubsection
// and an unknown workspace ErrNotFound.
func (s *store) MoveSubsection(ctx context.Context, id SubsectionID, to Anchor) error {
	const op = "daemon.wsm.move_subsection"
	fields := dlog.Context{"subsection": string(id), "to": AnchorField(to)}
	if before, ok := to.(BeforeSubsection); ok && before.ID == id {
		err := fmt.Errorf("wsm: move subsection: %s before itself", id)
		s.log.Error(op, "refused to move a subsection before itself", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		sub, err := subsectionRow(ctx, tx, id)
		if err != nil {
			return err
		}
		if err := requireAnchorOfRepository(ctx, tx, to, sub.Repo); err != nil {
			return err
		}
		walk, err := readWalk(ctx, tx, sub.Repo)
		if err != nil {
			return err
		}
		step := walk[walk.subsectionIndex(id)]
		rest := slices.Delete(slices.Clone(walk), walk.subsectionIndex(id), walk.subsectionIndex(id)+1)
		at, err := rest.entryIndex(to)
		if err != nil {
			return err
		}
		return writeWalk(ctx, tx, slices.Insert(rest, at, step))
	})
}

// requireAnchorOfRepository refuses an anchor that names a subsection or a
// workspace of a repository other than REPO (ErrOtherRepository), or one
// that does not exist (ErrUnknownSubsection, ErrNotFound).
func requireAnchorOfRepository(ctx context.Context, tx *sql.Tx, at Anchor, repo RepoID) error {
	var owner RepoID
	switch a := at.(type) {
	case BeforeSubsection:
		anchor, err := subsectionRow(ctx, tx, a.ID)
		if err != nil {
			return err
		}
		owner = anchor.Repo
	case BeforeWorkspace:
		if err := scanWorkspaceRow(ctx, tx, a.ID, "repo_id", &owner); err != nil {
			return err
		}
	case AtEnd:
		return nil
	default:
		return fmt.Errorf("%w: %T", errAnchorKind, at)
	}
	if owner != repo {
		return ErrOtherRepository
	}
	return nil
}

// AnchorField is an anchor as a log field.
func AnchorField(at Anchor) string {
	switch a := at.(type) {
	case BeforeWorkspace:
		return "before workspace " + string(a.ID)
	case BeforeSubsection:
		return "before subsection " + string(a.ID)
	case AtEnd:
		return "end"
	default:
		return fmt.Sprintf("unknown anchor %T", at)
	}
}

// MoveRepository moves repository ID just before repository BEFORE (nil:
// after every other one). An unknown one of either is ErrUnknownRepository.
func (s *store) MoveRepository(ctx context.Context, id RepoID, before *RepoID) error {
	const op = "daemon.wsm.move_repository"
	fields := dlog.Context{"repo_id": string(id), "before": ids.Optional(before)}
	if before != nil && *before == id {
		err := fmt.Errorf("wsm: move repository: %s before itself", id)
		s.log.Error(op, "refused to move a repository before itself", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		if err := requireRepository(ctx, tx, id); err != nil {
			return err
		}
		if before != nil {
			if err := requireRepository(ctx, tx, *before); err != nil {
				return err
			}
		}
		order, err := idsByPlace[RepoID](ctx, tx, `SELECT id FROM repositories ORDER BY place`)
		if err != nil {
			return err
		}
		return assignPlaces(ctx, tx, "repositories", moveBefore(order, id, before))
	})
}

// PlaceWorkspaces puts ROOT workspaces WS, in that order, and so their whole
// families, into subsection INTO (nil: none) of their own repository as one
// block, at AT: just before a root of the destination, just before a
// subsection (into none only), or after every entry of the destination (the
// bottom of the subsection, or of the repository). One workspace is the
// ordinary drag; several are an agent filing a batch. It is applied whole or
// not at all.
//
// Only the repository's roots' places move (writeWalk): their descendants
// are drawn, and walked by the tab bar, under them whatever their places,
// which order them only among their own siblings.
//
// Refusals, each writing nothing: no workspace, or one twice, or an anchor
// among them, or a subsection anchor into a subsection, is refused as
// malformed; an unknown workspace is ErrNotFound, one with a recorded parent
// ErrHasParent, workspaces of two repositories or a subsection of another
// repository ErrOtherRepository, an unknown subsection ErrUnknownSubsection,
// and a workspace anchor that is not a root in the destination
// ErrAnchorElsewhere.
func (s *store) PlaceWorkspaces(ctx context.Context, ws []WorkspaceID, into *SubsectionID, at Anchor) error {
	const op = "daemon.wsm.place_workspaces"
	fields := dlog.Context{"workspaces": ws, "into": ids.Optional(into), "at": AnchorField(at)}
	if err := malformedPlacement(ws, into, at); err != nil {
		s.log.Error(op, "refused a malformed placement", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		var repo RepoID
		for i, id := range ws {
			var own RepoID
			var parent sql.NullString
			if err := scanWorkspaceRow(ctx, tx, id, "repo_id, parent_id", &own, &parent); err != nil {
				return err
			}
			if parent.Valid {
				return ErrHasParent
			}
			if i > 0 && own != repo {
				return ErrOtherRepository
			}
			repo = own
		}
		if into != nil {
			sub, err := subsectionRow(ctx, tx, *into)
			if err != nil {
				return err
			}
			if sub.Repo != repo {
				return ErrOtherRepository
			}
		}
		if before, ok := at.(BeforeSubsection); ok {
			if err := requireAnchorOfRepository(ctx, tx, before, repo); err != nil {
				return err
			}
		}
		walk, err := readWalk(ctx, tx, repo)
		if err != nil {
			return err
		}
		walk = walk.without(ws)
		if into == nil {
			i, err := walk.entryIndex(at)
			if err != nil {
				return err
			}
			return writeWalk(ctx, tx, slices.Insert(walk, i, rootSteps(ws)...))
		}
		k := walk.subsectionIndex(*into)
		members := walk[k].members
		j := len(members)
		if before, ok := at.(BeforeWorkspace); ok {
			if j = slices.Index(members, before.ID); j < 0 {
				return ErrAnchorElsewhere
			}
		}
		walk[k].members = slices.Insert(slices.Clone(members), j, ws...)
		return writeWalk(ctx, tx, walk)
	})
}

// MoveWorkspace moves workspace WS onto TARGET's tab in the arrangement's
// walk, the tabs between shifting one family toward the tab WS left. The two
// must be SIBLINGS there: top-level workspaces of one repository, or
// children of one parent. WS lands just before TARGET when it comes from a
// later tab and just after TARGET (and so after TARGET's family, which is
// drawn under it) when it comes from an earlier one. A top-level WS also
// joins TARGET's subsection, so it lands in TARGET's list; a child stores no
// subsection and sits wherever its root does.
//
// Which side WS comes from is read in the same transaction as the write, so
// an arrangement landing in between cannot misplace it.
//
// Refusals, each writing nothing: WS and TARGET equal is refused as
// malformed; an unknown workspace is ErrNotFound, two repositories
// ErrOtherRepository, and any other pairing ErrNotSiblings.
func (s *store) MoveWorkspace(ctx context.Context, ws, target WorkspaceID) error {
	const op = "daemon.wsm.move_workspace"
	fields := dlog.Context{"workspace": ws, "target": target}
	if ws == target {
		err := fmt.Errorf("wsm: workspace %s cannot move onto its own tab", ws)
		s.log.Error(op, "refused a malformed move", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		var ownRepo, targetRepo RepoID
		var ownParent, targetParent sql.NullString
		if err := scanWorkspaceRow(ctx, tx, ws, "repo_id, parent_id", &ownRepo, &ownParent); err != nil {
			return err
		}
		if err := scanWorkspaceRow(ctx, tx, target, "repo_id, parent_id", &targetRepo, &targetParent); err != nil {
			return err
		}
		if ownRepo != targetRepo {
			return ErrOtherRepository
		}
		if ownParent != targetParent {
			return ErrNotSiblings
		}
		if ownParent.Valid {
			siblings, err := idsByPlace[WorkspaceID](ctx, tx, `SELECT id FROM workspaces WHERE parent_id = ? ORDER BY place`, ownParent.String)
			if err != nil {
				return err
			}
			after := slices.Index(siblings, ws) < slices.Index(siblings, target)
			_, err = permutePlaces(ctx, tx, moveBeside(siblings, ws, target, after))
			return err
		}
		walk, err := readWalk(ctx, tx, ownRepo)
		if err != nil {
			return err
		}
		roots := walk.roots()
		after := slices.Index(roots, ws) < slices.Index(roots, target)
		walk = walk.without([]WorkspaceID{ws})
		if i := walk.rootIndex(target); i >= 0 {
			if after {
				i++
			}
			return writeWalk(ctx, tx, slices.Insert(walk, i, walkStep{root: ws}))
		}
		for k, step := range walk {
			if step.sub != nil && slices.Contains(step.members, target) {
				walk[k].members = moveBeside(append(slices.Clone(step.members), ws), ws, target, after)
				return writeWalk(ctx, tx, walk)
			}
		}
		return fmt.Errorf("wsm: move workspace: root %s is in no entry of its repository's walk", target)
	})
}

// moveBeside answers ORDER with ID taken out and put back just before TARGET,
// or just after it when AFTER.
func moveBeside(order []WorkspaceID, id, target WorkspaceID, after bool) []WorkspaceID {
	out := slices.DeleteFunc(slices.Clone(order), func(x WorkspaceID) bool { return x == id })
	at := slices.Index(out, target)
	if after {
		at++
	}
	return slices.Insert(out, at, id)
}

// malformedPlacement refuses a placement of nothing, of one workspace twice,
// before one of the workspaces it places, or before a subsection INTO a
// subsection.
func malformedPlacement(ws []WorkspaceID, into *SubsectionID, at Anchor) error {
	if len(ws) == 0 {
		return errors.New("wsm: a placement names no workspace")
	}
	seen := map[WorkspaceID]bool{}
	for _, id := range ws {
		if seen[id] {
			return fmt.Errorf("wsm: a placement names workspace %s twice", id)
		}
		seen[id] = true
	}
	switch a := at.(type) {
	case BeforeWorkspace:
		if seen[a.ID] {
			return fmt.Errorf("wsm: a placement puts workspace %s before itself", a.ID)
		}
	case BeforeSubsection:
		if into != nil {
			return fmt.Errorf("wsm: a placement into subsection %s names subsection %s to stand before", *into, a.ID)
		}
	case AtEnd:
	default:
		return fmt.Errorf("%w: %T", errAnchorKind, at)
	}
	return nil
}

// requireRepository refuses an id no repository carries with
// ErrUnknownRepository.
func requireRepository(ctx context.Context, tx *sql.Tx, id RepoID) error {
	var found int
	err := tx.QueryRowContext(ctx, `SELECT 1 FROM repositories WHERE id = ?`, id).Scan(&found)
	if errors.Is(err, sql.ErrNoRows) {
		return fmt.Errorf("wsm: repository %s: %w", id, ErrUnknownRepository)
	}
	return err
}

// subsectionRow reads one subsection, ErrUnknownSubsection when none carries
// the id.
func subsectionRow(ctx context.Context, tx *sql.Tx, id SubsectionID) (Subsection, error) {
	sub, err := scanSubsection(tx.QueryRowContext(ctx, `SELECT `+subsectionColumns+` FROM subsections WHERE id = ?`, id))
	if errors.Is(err, sql.ErrNoRows) {
		return Subsection{}, &UnknownSubsectionError{ID: id}
	}
	return sub, err
}

// requireSubsectionRow refuses a write that touched no subsection with
// ErrUnknownSubsection.
func requireSubsectionRow(res sql.Result, id SubsectionID) error {
	n, err := res.RowsAffected()
	if err != nil {
		return err
	}
	if n == 0 {
		return &UnknownSubsectionError{ID: id}
	}
	return nil
}

// idsByPlace runs QUERY, which selects one id column, and answers the ids in
// the order it returned them.
func idsByPlace[ID ~string](ctx context.Context, tx *sql.Tx, query string, args ...any) ([]ID, error) {
	return listRows(ctx, tx, query, func(row interface{ Scan(...any) error }) (ID, error) {
		var id ID
		err := row.Scan(&id)
		return id, err
	}, args...)
}

// assignPlaces gives the rows of TABLE named by ORDER the places 1, 2, ... in
// that order. Every row goes through a negative place first, so no unique
// index sees two rows on one place mid-way.
func assignPlaces[ID ~string](ctx context.Context, tx *sql.Tx, table string, order []ID) error {
	for i, id := range order {
		if _, err := tx.ExecContext(ctx, `UPDATE `+table+` SET place = ? WHERE id = ?`, -(i + 1), id); err != nil {
			return err
		}
	}
	_, err := tx.ExecContext(ctx, `UPDATE `+table+` SET place = -place WHERE place < 0`)
	return err
}

// moveBefore answers ORDER with ID taken out and put back just before BEFORE,
// or last when BEFORE is nil.
func moveBefore[ID comparable](order []ID, id ID, before *ID) []ID {
	out := slices.DeleteFunc(slices.Clone(order), func(x ID) bool { return x == id })
	if before == nil {
		return append(out, id)
	}
	at := slices.Index(out, *before)
	return slices.Insert(out, at, id)
}

// ListSubsections loads every subsection, ordered by repository and then by
// each repository's stored order, all-or-nothing.
func (s *store) ListSubsections(ctx context.Context) ([]Subsection, error) {
	var out []Subsection
	err := s.read(ctx, "daemon.wsm.list_subsections", dlog.Context{}, func(ctx context.Context) error {
		loaded, err := listSubsections(ctx, s.db())
		out = loaded
		return err
	})
	if err != nil {
		return nil, err
	}
	return out, nil
}

// listSubsections loads every subsection through Q, all-or-nothing.
func listSubsections(ctx context.Context, q querier) ([]Subsection, error) {
	return listRows(ctx, q, `SELECT `+subsectionColumns+` FROM subsections ORDER BY repo_id, place`, scanSubsection)
}

// subsectionColumns is every column a Subsection is read from, in
// scanSubsection's order.
const subsectionColumns = `id, repo_id, name, place, folded, slot`

// scanSubsection reads one Subsection from a row of subsectionColumns.
func scanSubsection(row interface{ Scan(...any) error }) (Subsection, error) {
	var sub Subsection
	var slot sql.NullInt64
	if err := row.Scan(&sub.ID, &sub.Repo, &sub.Name, &sub.Place, &sub.Folded, &slot); err != nil {
		return Subsection{}, err
	}
	if slot.Valid {
		sub.Slot = &slot.Int64
	}
	return sub, nil
}

// Arrangement is the registry the roster's repository grouping and the tab
// bar are drawn from, READ AS ONE SNAPSHOT: every subsection a workspace
// names is among Subsections, and every repository a workspace or a
// subsection names is among Repositories. Read separately, a subsection
// deleted between two reads would leave a workspace naming one the list no
// longer holds.
type Arrangement struct {
	Workspaces   []Workspace
	Repositories []Repository
	Subsections  []Subsection
}

// ReadArrangement reads the workspaces, the repositories and the subsections
// in ONE read transaction, so they are one snapshot of the store.
func (s *store) ReadArrangement(ctx context.Context) (Arrangement, error) {
	var out Arrangement
	err := s.read(ctx, "daemon.wsm.read_arrangement", dlog.Context{}, func(ctx context.Context) error {
		tx, err := s.db().BeginTx(ctx, &sql.TxOptions{ReadOnly: true})
		if err != nil {
			return fmt.Errorf("wsm: begin the arrangement read: %w", err)
		}
		defer s.endTx(tx, "daemon.wsm.read_arrangement", dlog.Context{})
		var read Arrangement
		if read.Workspaces, err = listWorkspaces(ctx, tx); err != nil {
			return err
		}
		if read.Repositories, err = listRepositories(ctx, tx); err != nil {
			return err
		}
		if read.Subsections, err = listSubsections(ctx, tx); err != nil {
			return err
		}
		out = read
		return nil
	})
	return out, err
}

// nullableSubsection is a subsection id as a SQL argument, NULL for none.
func nullableSubsection(id *SubsectionID) any {
	if id == nil {
		return nil
	}
	return string(*id)
}

// nullSubsection decodes a nullable subsection column.
func nullSubsection(v sql.NullString) *SubsectionID {
	if !v.Valid {
		return nil
	}
	id := SubsectionID(v.String)
	return &id
}
