package wsm

import (
	"context"
	"database/sql"
	"errors"

	"agent-repld/internal/dlog"
)

// AN AGENT-CREATED SUBSECTION LASTS ONLY WHILE IT HAS A CONSTITUENT (owner
// request, 2026-10-26). An agent filing a new workspace may gather it and its
// related unsectioned workspaces into a subsection it creates
// (CreateAgentSubsection); the subsection records the creating workspace
// (Subsection.CreatedBy), and the write that leaves it with no open workspace
// deletes it, its closed families returning to no subsection as a user's
// delete returns them. A subsection the user created carries no creator and
// survives empty, unless the user's own close empties it (usercloses.go).
//
// THE DAEMON'S ONE-SHOTS SUBSECTION LASTS THE SAME WAY (oneshotssubsection.go):
// the prune below removes every subsection that lasts only while it has a
// constituent (emptyPrunedSubsectionsQuery), an agent's or the one-shots one.
//
// THE REMOVAL RIDES EVERY WRITE (writeTx), not the writes someone remembered
// could empty a subsection: a close, a merge, a placement, a retarget and a
// forget all commit through the one transaction path, which removes every
// agent-created subsection left empty before it commits. So no committed
// state holds an empty agent-created subsection, and a new way of emptying
// one needs no code of its own.

// agentSubsectionsDDL is the layout-80 addition: the workspace whose agent
// created a subsection. IT IS ADDITIVE: the previous build's INSERT names no
// such column, so a subsection it creates takes NULL, the user's kind, which
// is all that build can create.
const agentSubsectionsDDL = `
ALTER TABLE subsections ADD COLUMN created_by TEXT;
`

// opPruneAgentSubsection records each subsection that lasts only while it has
// a constituent (an agent's, or the one-shots one) a write left empty and so
// removed.
const opPruneAgentSubsection = "daemon.wsm.prune_agent_subsection"

// ErrNoOpenConstituent refuses an agent-created subsection none of whose
// members is open: it would be removed by the very write that created it.
// Nothing is written.
var ErrNoOpenConstituent = errors.New("wsm: an agent-created subsection would have no open workspace")

// emptySubsectionsQuery selects every subsection no open workspace sits in,
// a child counting where its root sits (rootSubsectionColumn), whoever
// created it. Its WHERE clause takes further `AND` conditions.
const emptySubsectionsQuery = `SELECT ` + subsectionColumns + ` FROM subsections
WHERE NOT EXISTS (SELECT 1 FROM workspaces WHERE NOT workspaces.closed AND ` + rootSubsectionColumn + ` = subsections.id)`

// prunedSubsectionCondition holds of every subsection that lasts only while it
// has a constituent: one an agent created, and the one-shots subsection.
const prunedSubsectionCondition = `(created_by IS NOT NULL OR one_shots)`

// emptyPrunedSubsectionsQuery selects every subsection that lasts only while
// it has a constituent and that no open workspace sits in.
const emptyPrunedSubsectionsQuery = emptySubsectionsQuery + ` AND ` + prunedSubsectionCondition

// emptyUserSubsectionsQuery selects every subsection the user created that no
// open workspace sits in: the ones the user's close removes (usercloses.go).
const emptyUserSubsectionsQuery = emptySubsectionsQuery + ` AND NOT ` + prunedSubsectionCondition

// CreateAgentSubsection records a subsection named NAME (trimmed), created by
// workspace CREATOR's agent, after the members' repository's other
// subsections, and places the families of MEMBERS in it, in that order, as
// one write. The members must all be of one repository (ErrOtherRepository) and
// at least one must be open (ErrNoOpenConstituent). A blank name is
// ErrBlankSubsectionName, an unknown creator or member ErrNotFound, and no
// member, or one twice, is malformed.
func (s *store) CreateAgentSubsection(ctx context.Context, creator WorkspaceID, name string, members []WorkspaceID) (Subsection, error) {
	const op = "daemon.wsm.create_agent_subsection"
	fields := dlog.Context{"created_by": string(creator), "name": name, "workspaces": members}
	trimmed, err := subsectionName(name)
	if err != nil {
		s.log.Debug(op, "refused a blank subsection name", withError(fields, err))
		return Subsection{}, err
	}
	out := Subsection{ID: NewSubsectionID(), Name: trimmed, CreatedBy: &creator}
	fields["subsection"] = string(out.ID)
	if err := malformedPlacement(members, &out.ID, AtEnd{}); err != nil {
		s.log.Error(op, "refused a malformed agent subsection", withError(fields, err))
		return Subsection{}, err
	}
	err = s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		var creatorRepo RepoID
		if err := scanWorkspaceRow(ctx, tx, creator, "repo_id", &creatorRepo); err != nil {
			return err
		}
		if err := refuseMainFamilies(ctx, tx, members); err != nil {
			return err
		}
		root, err := familyRoot(ctx, tx, members[0])
		if err != nil {
			return err
		}
		if err := scanWorkspaceRow(ctx, tx, root, "repo_id", &out.Repo); err != nil {
			return err
		}
		return fileIntoNewPrunedSubsection(ctx, tx, &out, members)
	})
	if err != nil {
		return Subsection{}, err
	}
	return out, nil
}

// fileIntoNewPrunedSubsection records SUB (an agent-created subsection, or
// the one-shots one, with its Repo set) after its repository's other
// subsections and places the families of MEMBERS in it, inside TX: the one
// write behind an agent's subsection create and a register that files its
// workspace into a new subsection. A subsection none of whose members is open
// is ErrNoOpenConstituent.
func fileIntoNewPrunedSubsection(ctx context.Context, tx *sql.Tx, sub *Subsection, members []WorkspaceID) error {
	if err := insertSubsection(ctx, tx, sub); err != nil {
		return err
	}
	if err := placeFamilies(ctx, tx, members, &sub.ID, AtEnd{}); err != nil {
		return err
	}
	empty, err := listRows(ctx, tx, emptyPrunedSubsectionsQuery+` AND id = ?`, scanSubsection, sub.ID)
	if err != nil {
		return err
	}
	if len(empty) > 0 {
		return ErrNoOpenConstituent
	}
	return nil
}

// pruneEmptySubsections removes, inside TX, every subsection that lasts only
// while it has a constituent and that no open workspace sits in, and answers
// them for the record the commit makes.
func pruneEmptySubsections(ctx context.Context, tx *sql.Tx) ([]Subsection, error) {
	empty, err := listRows(ctx, tx, emptyPrunedSubsectionsQuery, scanSubsection)
	if err != nil {
		return nil, err
	}
	for _, sub := range empty {
		if err := deleteSubsection(ctx, tx, sub.ID); err != nil {
			return nil, err
		}
	}
	return empty, nil
}

// recordPruned records each subsection the write OP removed, once its commit
// landed.
func (s *store) recordPruned(op string, pruned []Subsection) {
	for _, sub := range pruned {
		fields := dlog.Context{
			"subsection": string(sub.ID), "name": sub.Name, "repo_id": string(sub.Repo),
			"one_shots": sub.OneShots, "write": op,
		}
		if sub.CreatedBy != nil {
			fields["created_by"] = string(*sub.CreatedBy)
		}
		s.log.Info(opPruneAgentSubsection, "removed a subsection that lasts only while it has a constituent, as the write left it with no open workspace", fields)
	}
}
