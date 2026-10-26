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
// survives empty.
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

// opPruneAgentSubsection records each agent-created subsection a write left
// empty and so removed.
const opPruneAgentSubsection = "daemon.wsm.prune_agent_subsection"

// ErrNoOpenConstituent refuses an agent-created subsection none of whose
// members is open: it would be removed by the very write that created it.
// Nothing is written.
var ErrNoOpenConstituent = errors.New("wsm: an agent-created subsection would have no open workspace")

// emptyAgentSubsectionsQuery selects every agent-created subsection no open
// workspace sits in, a child counting where its root sits
// (rootSubsectionColumn).
const emptyAgentSubsectionsQuery = `SELECT ` + subsectionColumns + ` FROM subsections
WHERE created_by IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM workspaces WHERE NOT workspaces.closed AND ` + rootSubsectionColumn + ` = subsections.id)`

// CreateAgentSubsection records a subsection named NAME (trimmed), created by
// workspace CREATOR's agent, after the members' repository's other
// subsections, and places the families of MEMBERS in it, in that order, as
// one write, keeping their one-shot marks (an agent filing workspaces is no
// drag). The members must all be of one repository (ErrOtherRepository) and
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
		root, err := familyRoot(ctx, tx, members[0])
		if err != nil {
			return err
		}
		if err := scanWorkspaceRow(ctx, tx, root, "repo_id", &out.Repo); err != nil {
			return err
		}
		if err := insertSubsection(ctx, tx, &out); err != nil {
			return err
		}
		if err := placeFamilies(ctx, tx, members, &out.ID, AtEnd{}, KeepOneShotMarks); err != nil {
			return err
		}
		empty, err := listRows(ctx, tx, emptyAgentSubsectionsQuery+` AND id = ?`, scanSubsection, out.ID)
		if err != nil {
			return err
		}
		if len(empty) > 0 {
			return ErrNoOpenConstituent
		}
		return nil
	})
	if err != nil {
		return Subsection{}, err
	}
	return out, nil
}

// pruneEmptyAgentSubsections removes, inside TX, every agent-created
// subsection no open workspace sits in, and answers them for the record the
// commit makes.
func pruneEmptyAgentSubsections(ctx context.Context, tx *sql.Tx) ([]Subsection, error) {
	empty, err := listRows(ctx, tx, emptyAgentSubsectionsQuery, scanSubsection)
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

// recordPruned records each agent-created subsection the write OP removed,
// once its commit landed.
func (s *store) recordPruned(op string, pruned []Subsection) {
	for _, sub := range pruned {
		s.log.Info(opPruneAgentSubsection, "removed an agent-created subsection the write left with no open workspace", dlog.Context{
			"subsection": string(sub.ID), "name": sub.Name, "repo_id": string(sub.Repo),
			"created_by": string(*sub.CreatedBy), "write": op,
		})
	}
}
