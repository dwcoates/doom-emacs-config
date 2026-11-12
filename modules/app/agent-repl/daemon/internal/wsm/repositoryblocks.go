package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"slices"
	"sort"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// REPOSITORY BLOCKS (owner ruling, 2026-11-09; agentrepl.v1
// ArrangeMoveRepositoryBlock). A parent's children of one OTHER repository
// than its own form one block, drawn after its own-repository children, one
// block per repository. Which block a child is in is DERIVED from its own
// (parent_id, repo_id), never stored, so no write can persist a child outside
// its block: there is no column to get wrong. What is stored is only the
// ORDER of a parent's blocks, here, one row per arranged block.
//
// A block never arranged has no row and comes after the arranged ones, in
// the repositories' stored order (OrderRepositoryBlocks). A row whose block
// has no child left is kept, harmlessly: it answers where the block stands
// if a child of that repository returns, and reads skip blocks that are not
// present.

// repositoryBlocksDDL is the layout-124 addition: each parent's arranged
// order of its other-repository blocks.
//
// IT IS ADDITIVE: the previous build names no such table, so it neither
// reads nor writes it, and draws a parent's children as it always did. The
// trigger refuses a block of the parent's own repository: those children are
// no block. The fresh-file schema appends this same text (schema.go).
const repositoryBlocksDDL = `
CREATE TABLE repository_blocks (
  parent_id TEXT NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  repo_id   TEXT NOT NULL REFERENCES repositories(id) ON DELETE CASCADE,
  place     INTEGER NOT NULL CHECK (place >= 1),
  PRIMARY KEY (parent_id, repo_id),
  UNIQUE (parent_id, place)
);
CREATE TRIGGER repository_blocks_other_repository BEFORE INSERT ON repository_blocks
WHEN NEW.repo_id IS (SELECT repo_id FROM workspaces WHERE id = NEW.parent_id)
BEGIN
  SELECT RAISE(ABORT, 'wsm: a repository block is of another repository than its parent''s');
END;
`

// ErrNoRepositoryBlock refuses a block move naming a block the parent does
// not have: its own repository (whose children are no block), or a
// repository with no child under it. Nothing is written.
var ErrNoRepositoryBlock = errors.New("wsm: the parent has no block of that repository")

// RepositoryBlock is one arranged block: parent PARENT's children of
// repository REPO stand at PLACE among the parent's blocks (from 1, unique
// per parent).
type RepositoryBlock struct {
	Parent WorkspaceID
	Repo   RepoID
	Place  int64
}

// OrderRepositoryBlocks answers PRESENT, the repositories of one parent's
// blocks, in drawn order: the arranged ones (STORED, that parent's rows) by
// their place, then the rest by the repositories' stored place
// (REPOSITORY_PLACE). It is THE ONE statement of a parent's block order,
// which the store's moves and the roster both read, so the order a move
// writes is the order the roster draws.
//
// A present repository with no place in REPOSITORY_PLACE is a repository the
// registry does not carry, an invariant violation that panics.
func OrderRepositoryBlocks(present []RepoID, stored []RepositoryBlock, repositoryPlace map[RepoID]int64) []RepoID {
	arranged := map[RepoID]int64{}
	for _, b := range stored {
		arranged[b.Repo] = b.Place
	}
	out := slices.Clone(present)
	for _, repo := range out {
		if _, ok := repositoryPlace[repo]; !ok {
			panic(fmt.Sprintf("wsm: repository block %s names a repository the registry does not carry", repo))
		}
	}
	sort.SliceStable(out, func(i, j int) bool {
		a, aArranged := arranged[out[i]]
		b, bArranged := arranged[out[j]]
		switch {
		case aArranged && bArranged:
			return a < b
		case aArranged != bArranged:
			return aArranged
		default:
			return repositoryPlace[out[i]] < repositoryPlace[out[j]]
		}
	})
	return out
}

// MoveRepositoryBlock moves parent PARENT's block of repository REPO just
// before its block of repository BEFORE (nil: after every other block),
// storing the parent's whole block order. Refusals, each writing nothing:
// BEFORE naming REPO is malformed; an unknown parent is ErrNotFound; REPO or
// BEFORE naming no block of the parent is ErrNoRepositoryBlock.
func (s *store) MoveRepositoryBlock(ctx context.Context, parent WorkspaceID, repo RepoID, before *RepoID) error {
	const op = "daemon.wsm.move_repository_block"
	fields := dlog.Context{"parent": string(parent), "repo_id": string(repo), "before": ids.Optional(before)}
	if before != nil && *before == repo {
		err := fmt.Errorf("wsm: move repository block: %s before itself", repo)
		s.log.Error(op, "refused to move a repository block before itself", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		order, err := repositoryBlockOrder(ctx, tx, parent)
		if err != nil {
			return err
		}
		fields["blocks"] = order
		if !slices.Contains(order, repo) || (before != nil && !slices.Contains(order, *before)) {
			return ErrNoRepositoryBlock
		}
		order = moveBefore(order, repo, before)
		if _, err := tx.ExecContext(ctx, `DELETE FROM repository_blocks WHERE parent_id = ?`, parent); err != nil {
			return err
		}
		for i, r := range order {
			if _, err := tx.ExecContext(ctx,
				`INSERT INTO repository_blocks (parent_id, repo_id, place) VALUES (?, ?, ?)`, parent, r, i+1); err != nil {
				return err
			}
		}
		return nil
	})
}

// repositoryBlockOrder answers PARENT's present blocks inside TX in drawn
// order (OrderRepositoryBlocks): the repositories, other than the parent's,
// of its recorded children. An unknown parent is ErrNotFound.
func repositoryBlockOrder(ctx context.Context, tx *sql.Tx, parent WorkspaceID) ([]RepoID, error) {
	var own RepoID
	if err := scanWorkspaceRow(ctx, tx, parent, "repo_id", &own); err != nil {
		return nil, err
	}
	present, err := idsByPlace[RepoID](ctx, tx,
		`SELECT DISTINCT repo_id FROM workspaces WHERE parent_id = ? AND repo_id <> ?`, parent, own)
	if err != nil {
		return nil, err
	}
	stored, err := listRows(ctx, tx,
		`SELECT parent_id, repo_id, place FROM repository_blocks WHERE parent_id = ?`, scanRepositoryBlock, parent)
	if err != nil {
		return nil, err
	}
	repos, err := listRepositories(ctx, tx)
	if err != nil {
		return nil, err
	}
	return OrderRepositoryBlocks(present, stored, RepositoryPlaces(repos)), nil
}

// RepositoryPlaces answers each of REPOS' stored place, by id: the tie-break
// OrderRepositoryBlocks orders unarranged blocks by.
func RepositoryPlaces(repos []Repository) map[RepoID]int64 {
	out := make(map[RepoID]int64, len(repos))
	for _, r := range repos {
		out[r.ID] = r.Place
	}
	return out
}

// listRepositoryBlocks loads every arranged block through Q, all-or-nothing.
func listRepositoryBlocks(ctx context.Context, q querier) ([]RepositoryBlock, error) {
	return listRows(ctx, q, `SELECT parent_id, repo_id, place FROM repository_blocks ORDER BY parent_id, place`, scanRepositoryBlock)
}

// scanRepositoryBlock reads one RepositoryBlock from a row of its three
// columns.
func scanRepositoryBlock(row interface{ Scan(...any) error }) (RepositoryBlock, error) {
	var b RepositoryBlock
	err := row.Scan(&b.Parent, &b.Repo, &b.Place)
	return b, err
}
