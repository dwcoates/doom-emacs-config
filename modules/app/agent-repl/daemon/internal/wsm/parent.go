package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// THE PARENT AND THE MERGE TARGET ARE TWO FACTS, CHANGED TOGETHER. A
// workspace's recorded parent (workspaces.parent_id) is its relationship: a
// child's parent coordinates it and the roster nests it there; no parent makes
// it a peer. Its creation job's target directory (creation_jobs.target_dir) is
// where its merge lands (agentrepl.v1.WorkspaceLandingTarget). A retarget writes
// both in ONE transaction, so one notice says both, and the workspace the merge
// lands into is read back from the target directory (Workspace.LandsInto),
// never stored a second time.
//
// A RETARGET KEEPS THE LANDING IT DOES NOT NAME. The workspace's branch was
// cut from where it lands now; moving the landing because the relationship
// changed would merge that branch into one it was never cut from. Only a
// create applies the relationship's default, because a create cuts the
// branch.

// ParentRefusal is why SetParent refused a retarget. Each is the answer to
// what was asked, never a store failure, and changes nothing.
type ParentRefusal int

// The refusals SetParent answers.
const (
	// ParentUnknown is a new parent the registry does not hold.
	ParentUnknown ParentRefusal = iota + 1
	// ParentCycle is a new parent that is the workspace or one of its
	// descendants.
	ParentCycle
	// ParentNoMergeLayout is a workspace with no creation job: the editor
	// registered it, so it has no merge geometry to retarget.
	ParentNoMergeLayout
	// ParentUnknownLandingTarget is a workspace to land into that the
	// registry does not hold, that is of another repository, or that is the
	// workspace itself or lands into it, directly or through others.
	ParentUnknownLandingTarget
)

// String names the refusal for records.
func (r ParentRefusal) String() string {
	switch r {
	case ParentUnknown:
		return "unknown_parent"
	case ParentCycle:
		return "cycle"
	case ParentNoMergeLayout:
		return "no_merge_layout"
	case ParentUnknownLandingTarget:
		return "unknown_landing_target"
	}
	return fmt.Sprintf("ParentRefusal(%d)", int(r))
}

// ParentRefusedError is SetParent's refusal.
type ParentRefusedError struct {
	// Refusal is why.
	Refusal ParentRefusal
	// Workspace is the workspace whose parent was asked to change.
	Workspace WorkspaceID
	// Asked is the retarget asked for.
	Asked Retarget
}

// Error implements error.
func (e *ParentRefusedError) Error() string {
	return fmt.Sprintf("wsm: workspace %s cannot be retargeted (%s): %s", e.Workspace, e.Asked, e.Refusal)
}

// Retarget is what a retarget asks for: the relationship to leave the
// workspace in, and where its merge lands from now on.
type Retarget struct {
	// KeepParent leaves the recorded parent as it stands; Parent is then
	// ignored.
	KeepParent bool
	// Parent is the new parent, nil to make the workspace a top-level peer.
	Parent *WorkspaceID
	// Landing is where the merge lands from now on, nil to keep where it
	// lands now.
	Landing *LandingChoice
}

// LandingChoice is an explicit landing target.
type LandingChoice struct {
	// Into is the workspace landed into, nil for the primary branch.
	Into *WorkspaceID
}

// String spells the retarget for records.
func (r Retarget) String() string {
	parent := "top-level"
	switch {
	case r.KeepParent:
		parent = "parent unchanged"
	case r.Parent != nil:
		parent = "parent " + string(*r.Parent)
	}
	landing := "landing kept"
	if r.Landing != nil {
		landing = "landing on the primary branch"
		if r.Landing.Into != nil {
			landing = "landing into " + string(*r.Landing.Into)
		}
	}
	return parent + ", " + landing
}

// ToParent is the retarget that makes PARENT (nil: none) the workspace's
// parent and keeps where it lands.
func ToParent(parent *WorkspaceID) Retarget { return Retarget{Parent: parent} }

// AsParentRefused answers err's ParentRefusedError, if it carries one.
func AsParentRefused(err error) (*ParentRefusedError, bool) {
	var refused *ParentRefusedError
	if errors.As(err, &refused) {
		return refused, true
	}
	return nil, false
}

// MergeTarget is one side of a retarget: the recorded parent and the
// checkout the merge lands in.
type MergeTarget struct {
	// Parent is the recorded parent, nil for top-level.
	Parent *WorkspaceID
	// ParentName is the parent's display name, empty for top-level.
	ParentName string
	// Dir is the checkout the merge lands in: a workspace's worktree, or the
	// repository's main checkout.
	Dir string
	// LandsInto is the workspace whose worktree Dir is, nil when Dir is the
	// repository's main checkout.
	LandsInto *WorkspaceID
	// LandsIntoName is LandsInto's display name, empty when it is nil.
	LandsIntoName string
	// CrossRepository is a Parent of another repository than the workspace's:
	// the workspace nests under it and it owns the workspace, but no branch
	// crosses repositories, so the merge can only land in the workspace's own
	// repository.
	CrossRepository bool
	// DefaultBranch is the repository's default branch, which a top-level
	// workspace's merge lands on.
	DefaultBranch string
}

// ParentChange is what a retarget changed: the target before and after.
type ParentChange struct {
	// Workspace is the retargeted workspace.
	Workspace WorkspaceID
	// Before is the target the workspace had.
	Before MergeTarget
	// After is the target it has now.
	After MergeTarget
}

// SetParent applies RETARGET to the workspace in one transaction: its recorded
// parent and the checkout its merge lands in. A new parent must be a registered
// workspace of any repository, and neither the workspace nor one of its
// descendants; a workspace with no creation job has no merge to retarget. A
// parent of ANOTHER repository nests and owns the workspace, but its merge can
// only land in the workspace's own repository. A workspace to land into must be
// registered in the workspace's own repository, and neither the workspace nor
// one landing into it (ParentUnknownLandingTarget). A new parent, or a workspace
// to land into, that is hidden, closed or closing is refused with
// ParentNotOpenError (family.go). Every check runs inside the transaction, so a
// retarget racing another cannot close a cycle the check did not see. An
// unknown workspace is ErrNotFound.
func (s *store) SetParent(ctx context.Context, id WorkspaceID, retarget Retarget) (ParentChange, error) {
	const op = "daemon.wsm.set_parent"
	fields := dlog.Context{"workspace": string(id), "retarget": retarget.String()}
	var change ParentChange
	err := s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		var err error
		change, err = setParent(ctx, tx, id, retarget)
		return err
	})
	if err != nil {
		return ParentChange{}, err
	}
	return change, nil
}

// RestoreParent puts change.Before back, but ONLY while the workspace still
// stands exactly as change.After left it: a retarget that has since been
// superseded is not overwritten. stale reports that it was superseded and
// nothing was written.
func (s *store) RestoreParent(ctx context.Context, change ParentChange) (stale bool, err error) {
	const op = "daemon.wsm.restore_parent"
	fields := dlog.Context{"workspace": string(change.Workspace),
		"from_parent": ids.Optional(change.After.Parent), "to_parent": ids.Optional(change.Before.Parent)}
	err = s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		current, err := readTarget(ctx, tx, change.Workspace)
		if err != nil {
			return err
		}
		if ids.Optional(current.parent) != ids.Optional(change.After.Parent) || current.targetDir != change.After.Dir {
			stale = true
			return nil
		}
		return writeTarget(ctx, tx, change.Workspace, change.Before.Parent, change.Before.Dir)
	})
	return stale, err
}

// storedTarget is a workspace's two stored target facts, as one transaction
// reads them.
type storedTarget struct {
	repo      RepoID
	parent    *WorkspaceID
	targetDir string
}

// setParent is SetParent's body, inside its transaction.
func setParent(ctx context.Context, tx *sql.Tx, id WorkspaceID, retarget Retarget) (ParentChange, error) {
	current, err := readTarget(ctx, tx, id)
	if err != nil {
		return ParentChange{}, err
	}
	refused := func(r ParentRefusal) error {
		return &ParentRefusedError{Refusal: r, Workspace: id, Asked: retarget}
	}
	if current.targetDir == "" {
		return ParentChange{}, refused(ParentNoMergeLayout)
	}
	repoDir, defaultBranch, err := repositoryTarget(ctx, tx, current.repo)
	if err != nil {
		return ParentChange{}, err
	}
	before, err := describeTarget(ctx, tx, current.repo, current.parent, current.targetDir, repoDir, defaultBranch)
	if err != nil {
		return ParentChange{}, err
	}
	parent := retarget.Parent
	if retarget.KeepParent {
		parent = current.parent
	}
	after := MergeTarget{Dir: before.Dir, LandsInto: before.LandsInto, LandsIntoName: before.LandsIntoName, DefaultBranch: defaultBranch}
	if retarget.Landing != nil {
		after.Dir, after.LandsInto, after.LandsIntoName = repoDir, nil, ""
	}
	if parent != nil {
		var (
			name, dir string
			repo      RepoID
		)
		err := tx.QueryRowContext(ctx, `SELECT name, dir, repo_id FROM workspaces WHERE id = ?`, *parent).Scan(&name, &dir, &repo)
		if errors.Is(err, sql.ErrNoRows) {
			return ParentChange{}, refused(ParentUnknown)
		}
		if err != nil {
			return ParentChange{}, err
		}
		// A WORKSPACE IS NEVER MOVED UNDER A PARENT THAT IS NOT OPEN
		// (family.go). A kept parent is not asked again: the workspace is not
		// moved.
		if !retarget.KeepParent {
			if err := requireOpenParent(ctx, tx, *parent); err != nil {
				return ParentChange{}, err
			}
			cycle, err := descendsFrom(ctx, tx, *parent, id)
			if err != nil {
				return ParentChange{}, err
			}
			if cycle {
				return ParentChange{}, refused(ParentCycle)
			}
		}
		chosen := *parent
		after.Parent, after.ParentName = &chosen, name
		after.CrossRepository = repo != current.repo
	}
	if retarget.Landing != nil && retarget.Landing.Into != nil {
		into := *retarget.Landing.Into
		var (
			name, dir string
			repo      RepoID
		)
		err := scanWorkspaceRow(ctx, tx, into, "name, dir, repo_id", &name, &dir, &repo)
		if errors.Is(err, ErrNotFound) || (err == nil && (repo != current.repo || IsMainWorktree(dir, repoDir))) {
			return ParentChange{}, refused(ParentUnknownLandingTarget)
		}
		if err != nil {
			return ParentChange{}, err
		}
		landsBack, err := landsInto(ctx, tx, into, id)
		if err != nil {
			return ParentChange{}, err
		}
		if landsBack {
			return ParentChange{}, refused(ParentUnknownLandingTarget)
		}
		// A MERGE NEVER LANDS INTO A WORKSPACE THAT IS NOT OPEN (family.go):
		// its branch is where the work goes.
		if err := requireOpenParent(ctx, tx, into); err != nil {
			return ParentChange{}, err
		}
		after.Dir, after.LandsInto, after.LandsIntoName = dir, &into, name
	}
	if err := writeTarget(ctx, tx, id, after.Parent, after.Dir); err != nil {
		return ParentChange{}, err
	}
	return ParentChange{Workspace: id, Before: before, After: after}, nil
}

// landsInto reports whether START is TARGET or its merge lands into TARGET,
// directly or through the workspaces it lands into. The chain is bounded by the
// number of workspaces: a longer walk means the stored targets already cycle,
// which no write admits, so it is corruption and is returned as an error.
func landsInto(ctx context.Context, tx *sql.Tx, start, target WorkspaceID) (bool, error) {
	var total int
	if err := tx.QueryRowContext(ctx, `SELECT COUNT(*) FROM workspaces`).Scan(&total); err != nil {
		return false, err
	}
	at := start
	for step := 0; step <= total; step++ {
		if at == target {
			return true, nil
		}
		var next sql.NullString
		if err := scanWorkspaceRow(ctx, tx, at, landsIntoColumn, &next); err != nil {
			return false, fmt.Errorf("wsm: walk the landing targets of %s at %s: %w", start, at, err)
		}
		if !next.Valid {
			return false, nil
		}
		at = WorkspaceID(next.String)
	}
	return false, fmt.Errorf("wsm: the recorded landing targets beyond %s cycle", start)
}

// readTarget reads a workspace's repository, recorded parent and merge target
// directory. A workspace with no creation job answers an empty target
// directory; an unknown workspace is ErrNotFound.
func readTarget(ctx context.Context, tx *sql.Tx, id WorkspaceID) (storedTarget, error) {
	var (
		out    storedTarget
		parent sql.NullString
		target sql.NullString
	)
	err := tx.QueryRowContext(ctx,
		`SELECT w.repo_id, w.parent_id, c.target_dir
		 FROM workspaces w LEFT JOIN creation_jobs c ON c.workspace_id = w.id
		 WHERE w.id = ?`, id).Scan(&out.repo, &parent, &target)
	if errors.Is(err, sql.ErrNoRows) {
		return storedTarget{}, fmt.Errorf("wsm: workspace %s: %w", id, ErrNotFound)
	}
	if err != nil {
		return storedTarget{}, err
	}
	if parent.Valid {
		p := WorkspaceID(parent.String)
		out.parent = &p
	}
	out.targetDir = target.String
	return out, nil
}

// writeTarget writes both stored target facts. Each statement must touch
// exactly one row: the caller read both rows inside this transaction.
//
// THE FAMILY'S SUBSECTION FOLLOWS THE PARENT (arrange.go): a workspace given a
// parent stops storing a subsection and sits wherever the parent does; one
// left with no parent stores the subsection it sat in until now, so it stays
// where it was drawn.
//
// WHO LANDS THE WORKSPACE IS RE-DECIDED WITH ITS OWNER: a child its new owner
// did not lend its context to is the owner's to land, and a workspace with no
// owner lands itself (owner_lands, derivation.go). A merge workspace keeps its
// own: its owner is its requester, which never lands it.
//
// All in the same statement as the parent, evaluated against the row as it
// stood.
func writeTarget(ctx context.Context, tx *sql.Tx, id WorkspaceID, parent *WorkspaceID, targetDir string) error {
	owner := nullWorkspace(parent)
	res, err := tx.ExecContext(ctx, `UPDATE workspaces SET parent_id = ?,
		subsection_id = CASE WHEN ? IS NULL THEN `+rootSubsectionColumn+` ELSE NULL END,
		owner_lands = CASE WHEN merge_of IS NOT NULL THEN owner_lands
			ELSE (? IS NOT NULL AND (context_from IS NULL OR context_from IS NOT ?)) END
		WHERE id = ?`, owner, owner, owner, owner, id)
	if err != nil {
		return err
	}
	if err := requireOneRow(res, fmt.Sprintf("wsm: workspace %s", id)); err != nil {
		return err
	}
	res, err = tx.ExecContext(ctx, `UPDATE creation_jobs SET target_dir = ? WHERE workspace_id = ?`, targetDir, id)
	if err != nil {
		return err
	}
	return requireOneRow(res, fmt.Sprintf("wsm: creation job of %s", id))
}

// repositoryTarget reads a repository's main checkout and default branch. A
// workspace's repository row is a foreign key, so its absence is corruption,
// never an answer.
func repositoryTarget(ctx context.Context, tx *sql.Tx, repo RepoID) (dir, defaultBranch string, err error) {
	err = tx.QueryRowContext(ctx, `SELECT dir, default_branch FROM repositories WHERE id = ?`, repo).Scan(&dir, &defaultBranch)
	if err != nil {
		return "", "", fmt.Errorf("wsm: repository %s: %w", repo, err)
	}
	return dir, defaultBranch, nil
}

// describeTarget names a stored target of a workspace of REPO for the change
// record: the parent's display name when there is a parent, whether it is of
// another repository, and the workspace DIR is the worktree of unless it is the
// repository's main checkout, REPODIR.
func describeTarget(ctx context.Context, tx *sql.Tx, repo RepoID, parent *WorkspaceID, dir, repoDir, defaultBranch string) (MergeTarget, error) {
	out := MergeTarget{Parent: parent, Dir: dir, DefaultBranch: defaultBranch}
	if !IsMainWorktree(dir, repoDir) {
		var into WorkspaceID
		err := tx.QueryRowContext(ctx, `SELECT id, name FROM workspaces WHERE dir = ?`, dir).Scan(&into, &out.LandsIntoName)
		switch {
		case err == nil:
			out.LandsInto = &into
		case !errors.Is(err, sql.ErrNoRows):
			return MergeTarget{}, fmt.Errorf("wsm: the workspace at the target %s: %w", dir, err)
		}
	}
	if parent == nil {
		return out, nil
	}
	var parentRepo RepoID
	err := tx.QueryRowContext(ctx, `SELECT name, repo_id FROM workspaces WHERE id = ?`, *parent).Scan(&out.ParentName, &parentRepo)
	if err != nil {
		return MergeTarget{}, fmt.Errorf("wsm: recorded parent %s: %w", *parent, err)
	}
	out.CrossRepository = parentRepo != repo
	return out, nil
}

// descendsFrom reports whether start is ancestor itself or lies beneath it,
// following recorded parents upward from start. The chain is bounded by the
// number of workspaces: a longer walk means the stored parents already cycle,
// which no write admits, so it is corruption and is returned as an error.
func descendsFrom(ctx context.Context, tx *sql.Tx, start, ancestor WorkspaceID) (bool, error) {
	var total int
	if err := tx.QueryRowContext(ctx, `SELECT COUNT(*) FROM workspaces`).Scan(&total); err != nil {
		return false, err
	}
	at := start
	for step := 0; step <= total; step++ {
		if at == ancestor {
			return true, nil
		}
		var parent sql.NullString
		if err := scanWorkspaceRow(ctx, tx, at, "parent_id", &parent); err != nil {
			return false, fmt.Errorf("wsm: walk the parents of %s at %s: %w", start, at, err)
		}
		if !parent.Valid {
			return false, nil
		}
		at = WorkspaceID(parent.String)
	}
	return false, fmt.Errorf("wsm: the recorded parents above %s cycle", start)
}
