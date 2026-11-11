package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"strings"

	"agent-repld/internal/dlog"
)

// RegisterPlacement is where a workspace is FILED by the insert that mints it
// (RegisterFacts.Placement): the creator decided, so the row is never drawn
// unsectioned for an instant and never asks to file itself. Exactly one of
// Into and New is set.
type RegisterPlacement struct {
	// Into is the NAME of an existing subsection of the workspace's
	// repository the workspace joins.
	Into string
	// New is a subsection created with the workspace in it.
	New *NewSubsectionPlacement
	// CreatedBy is the workspace whose agent files the new workspace, the
	// creator of New. Required beside New.
	CreatedBy WorkspaceID
}

// NewSubsectionPlacement is a subsection created by a register: its NAME and
// the workspaces gathered into it, by name, beside the registered one.
type NewSubsectionPlacement struct {
	Name   string
	Gather []string
}

// ErrPlacementOnChild refuses a placement on a workspace with a parent: a
// child sits wherever its root does (arrange.go). Nothing is written.
var ErrPlacementOnChild = errors.New("wsm: a child workspace is never placed; it sits wherever its owner does")

// ErrPlacementOnMerge refuses a placement on a merge workspace, which is
// drawn nowhere until revealed. Nothing is written.
var ErrPlacementOnMerge = errors.New("wsm: a merge workspace is never placed")

// UnknownPlacementSubsectionError names the subsection NAME the repository
// does not carry. Nothing is written.
type UnknownPlacementSubsectionError struct{ Name string }

func (e *UnknownPlacementSubsectionError) Error() string {
	return fmt.Sprintf("wsm: the repository has no subsection named %q", e.Name)
}

// GatherUnknownError names the gathered workspace NAME the repository does
// not carry. Nothing is written.
type GatherUnknownError struct{ Name string }

func (e *GatherUnknownError) Error() string {
	return fmt.Sprintf("wsm: the repository has no workspace named %q to gather", e.Name)
}

// GatherRefusedError names the gathered workspace that cannot be gathered
// and WHY. Nothing is written.
type GatherRefusedError struct {
	Workspace WorkspaceID
	Name      string
	Why       string
}

func (e *GatherRefusedError) Error() string {
	return fmt.Sprintf("wsm: workspace %q (%s) cannot be gathered: %s", e.Name, e.Workspace, e.Why)
}

// IsPlacementRefusal reports whether err is one of the placement's own
// answers rather than the store failing.
func IsPlacementRefusal(err error) bool {
	var unknown *UnknownPlacementSubsectionError
	var gatherUnknown *GatherUnknownError
	var gatherRefused *GatherRefusedError
	return errors.Is(err, ErrPlacementOnChild) || errors.Is(err, ErrPlacementOnMerge) || errors.Is(err, ErrBlankSubsectionName) ||
		errors.As(err, &unknown) || errors.As(err, &gatherUnknown) || errors.As(err, &gatherRefused)
}

// resolvedPlacement is a RegisterPlacement read against the store: the
// existing subsection joined, or the new one to create, and the families
// gathered into it.
type resolvedPlacement struct {
	into   *SubsectionID
	create *Subsection
	gather []WorkspaceID
}

// gatherCandidate is one workspace row a placement's gather names.
type gatherCandidate struct {
	id         WorkspaceID
	closed     bool
	kind       WorkspaceKind
	child      bool
	merge      bool
	main       bool
	subsection bool
}

// why answers the sentence naming what keeps the candidate from being
// gathered, empty when it can be.
func (c gatherCandidate) why() string {
	switch {
	case c.kind != WorkspaceKindRepository:
		return "it is not a repository workspace"
	case c.closed:
		return "it is closed"
	case c.child:
		return "it is a child, which sits wherever its owner does"
	case c.merge:
		return "it is a merge workspace"
	case c.main:
		return "it is a main workspace, which is never sorted by an agent"
	case c.subsection:
		return "it is already in a subsection"
	}
	return ""
}

func scanGatherCandidate(row interface{ Scan(...any) error }) (gatherCandidate, error) {
	var (
		c                         gatherCandidate
		kind                      string
		parent, merge, subsection sql.NullString
	)
	if err := row.Scan(&c.id, &kind, &c.closed, &parent, &merge, &subsection, &c.main); err != nil {
		return gatherCandidate{}, err
	}
	c.kind, c.child, c.merge, c.subsection = WorkspaceKind(kind), parent.Valid, merge.Valid, subsection.Valid
	return c, nil
}

// checkPlacement is the ONE judgment of a placement, read through Q: the
// pre-create refusal (RefusePlacement) and the register's own transaction
// both run it, so what the first accepts the second accepts unless the store
// moved between them, and then the second refuses with the same typed error.
// PARENT and MERGE are the facts of the workspace being placed.
func checkPlacement(ctx context.Context, q querier, repo RepoID, p RegisterPlacement, parent *WorkspaceID, merge bool) (resolvedPlacement, error) {
	if (p.Into == "") == (p.New == nil) {
		panic(fmt.Sprintf("wsm: a placement names exactly one of an existing subsection and a new one: %+v", p))
	}
	if p.New != nil && p.CreatedBy == "" {
		panic("wsm: a placement into a new subsection names the workspace creating it")
	}
	switch {
	case parent != nil:
		return resolvedPlacement{}, ErrPlacementOnChild
	case merge:
		return resolvedPlacement{}, ErrPlacementOnMerge
	}
	if p.New == nil {
		subs, err := listRows(ctx, q, `SELECT `+subsectionColumns+` FROM subsections WHERE repo_id = ? AND name = ? ORDER BY place`, scanSubsection, repo, strings.TrimSpace(p.Into))
		if err != nil {
			return resolvedPlacement{}, err
		}
		if len(subs) == 0 {
			return resolvedPlacement{}, &UnknownPlacementSubsectionError{Name: p.Into}
		}
		return resolvedPlacement{into: &subs[0].ID}, nil
	}
	name, err := subsectionName(p.New.Name)
	if err != nil {
		return resolvedPlacement{}, err
	}
	out := resolvedPlacement{create: &Subsection{ID: NewSubsectionID(), Repo: repo, Name: name, CreatedBy: &p.CreatedBy}}
	seen := map[WorkspaceID]bool{}
	for _, gathered := range p.New.Gather {
		// An open workspace of the name outranks a closed one.
		rows, err := listRows(ctx, q, `SELECT w.id, w.kind, w.closed, w.parent_id, w.merge_of, w.subsection_id, w.dir = r.dir
			FROM workspaces AS w JOIN repositories AS r ON r.id = w.repo_id
			WHERE w.repo_id = ? AND w.name = ? ORDER BY w.closed, w.place`, scanGatherCandidate, repo, gathered)
		if err != nil {
			return resolvedPlacement{}, err
		}
		if len(rows) == 0 {
			return resolvedPlacement{}, &GatherUnknownError{Name: gathered}
		}
		c := rows[0]
		why := c.why()
		if why == "" && seen[c.id] {
			why = "it is named twice"
		}
		if why != "" {
			return resolvedPlacement{}, &GatherRefusedError{Workspace: c.id, Name: gathered, Why: why}
		}
		seen[c.id] = true
		out.gather = append(out.gather, c.id)
	}
	return out, nil
}

// RefusePlacement answers the typed refusal of placing a new workspace
// (child of PARENT or not, a merge workspace or not) in repository REPODIR,
// nil when the placement would be accepted: the create asks before it makes a
// worktree. It writes nothing. A refusal is the answer, recorded at DEBUG;
// only a failed read is an error the store records.
func (s *store) RefusePlacement(ctx context.Context, repoDir string, p RegisterPlacement, parent *WorkspaceID, merge bool) error {
	const op = "daemon.wsm.refuse_placement"
	normalized, err := normalizeDir(repoDir)
	if err != nil {
		return err
	}
	fields := dlog.Context{"repo_dir": normalized, "placement": fmt.Sprintf("%+v", p)}
	var refusal error
	err = s.read(ctx, op, fields, func(ctx context.Context) error {
		var repo RepoID
		if err := s.q(ctx).QueryRowContext(ctx, `SELECT id FROM repositories WHERE dir = ?`, normalized).Scan(&repo); err != nil {
			if errors.Is(err, sql.ErrNoRows) {
				return fmt.Errorf("wsm: repository %s: %w", normalized, ErrUnknownRepository)
			}
			return err
		}
		_, refusal = checkPlacement(ctx, s.q(ctx), repo, p, parent, merge)
		if refusal != nil && !IsPlacementRefusal(refusal) {
			return refusal
		}
		return nil
	})
	if err != nil {
		return err
	}
	if refusal != nil {
		s.log.Debug(op, "the placement was refused; nothing was written", withError(fields, refusal))
	}
	return refusal
}

// applyPlacement writes a resolved placement for the freshly inserted
// workspace ID inside TX: the one assignment, or the new subsection with the
// gathered families and the workspace itself in it, through the same helpers
// a drag and an agent's subsection create write through.
func applyPlacement(ctx context.Context, tx *sql.Tx, id WorkspaceID, r resolvedPlacement) error {
	if r.create == nil {
		_, err := placeFamilies(ctx, tx, []WorkspaceID{id}, r.into, AtEnd{})
		return err
	}
	return fileIntoNewAgentSubsection(ctx, tx, r.create, append(append([]WorkspaceID{}, r.gather...), id))
}
