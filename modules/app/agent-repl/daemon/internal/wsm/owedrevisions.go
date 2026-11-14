package wsm

import (
	"cmp"
	"context"
	"database/sql"
	"fmt"
	"slices"
	"strings"

	"agent-repld/internal/dlog"
)

// owedRevisionsDDL is the layout-145 addition: the revised final responses
// a workspace's agent has yet to be told of. IT IS ADDITIVE: a new table the
// previous build never names.
//
// A REVISION RUNS ON A FORK THAT PERSISTS NOTHING (internal/revision), so the
// vendor transcript keeps the response's original words. The revision is
// owed here until the workspace's next prompt that carries a workspace note
// tells the agent what its response now says, and is taken by that prompt.
//
// ONE ROW PER RESPONSE: a second revision of the same response replaces the
// words owed, and keeps the original the agent itself wrote. Any later
// binding forgets them, since they are about the conversation it replaced.
const owedRevisionsDDL = `
CREATE TABLE owed_response_revisions (
  workspace_id TEXT NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  unit         TEXT NOT NULL,
  original     TEXT NOT NULL,
  revised      TEXT NOT NULL,
  owed_at      INTEGER NOT NULL,
  PRIMARY KEY (workspace_id, unit)
);
`

// OwedRevision is one revised final response the workspace's agent has yet to
// be told of.
type OwedRevision struct {
	// Unit is the response's activity unit (conversation.v1.AgentActivityId).
	Unit string
	// Original is the response as the agent wrote it.
	Original string
	// Revised is what the response says now.
	Revised string
	// OwedAtNs is when the latest revision of it landed, epoch ns: the order
	// a prompt tells them in.
	OwedAtNs int64
}

// validOwedRevision refuses an owed revision missing any of its words.
func validOwedRevision(owed OwedRevision) error {
	if strings.TrimSpace(owed.Unit) == "" || strings.TrimSpace(owed.Original) == "" || strings.TrimSpace(owed.Revised) == "" || owed.OwedAtNs <= 0 {
		return fmt.Errorf("wsm: an owed revision names its unit, both its words and when it landed (unit %q, original %d chars, revised %d chars, owed at %d)",
			owed.Unit, len(owed.Original), len(owed.Revised), owed.OwedAtNs)
	}
	return nil
}

// OweRevision records that workspace ID's agent is owed OWED. A response
// already owed keeps its original and takes the newer words.
func (s *store) OweRevision(ctx context.Context, id WorkspaceID, owed OwedRevision) error {
	const op = "daemon.wsm.owe_revision"
	fields := dlog.Context{"workspace": string(id), "unit": owed.Unit}
	if err := validOwedRevision(owed); err != nil {
		s.log.Error(op, "refused to record a malformed owed revision", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx,
			`INSERT INTO owed_response_revisions (workspace_id, unit, original, revised, owed_at) VALUES (?, ?, ?, ?, ?)
			 ON CONFLICT(workspace_id, unit) DO UPDATE SET revised = excluded.revised, owed_at = excluded.owed_at`,
			id, owed.Unit, owed.Original, owed.Revised, owed.OwedAtNs)
		return err
	})
}

// TakeOwedRevisions takes every revision workspace ID's next prompt tells the
// agent of, oldest first: it answers them and deletes them in one
// transaction, so two prompts never both carry one. Empty when none is owed.
func (s *store) TakeOwedRevisions(ctx context.Context, id WorkspaceID) ([]OwedRevision, error) {
	const op = "daemon.wsm.take_owed_revisions"
	var out []OwedRevision
	err := s.write(ctx, op, dlog.Context{"workspace": string(id)}, func(ctx context.Context, tx *sql.Tx) error {
		out = nil
		rows, err := tx.QueryContext(ctx,
			`DELETE FROM owed_response_revisions WHERE workspace_id = ? RETURNING unit, original, revised, owed_at`, id)
		if err != nil {
			return err
		}
		defer rows.Close()
		for rows.Next() {
			var owed OwedRevision
			if err := rows.Scan(&owed.Unit, &owed.Original, &owed.Revised, &owed.OwedAtNs); err != nil {
				return err
			}
			out = append(out, owed)
		}
		return rows.Err()
	})
	sortOwedRevisions(out)
	return out, err
}

// RestoreOwedRevisions puts back revisions TakeOwedRevisions took for a
// prompt the queue then refused, so the next prompt tells them. A response
// revised again meanwhile keeps its newer words.
func (s *store) RestoreOwedRevisions(ctx context.Context, id WorkspaceID, owed []OwedRevision) error {
	const op = "daemon.wsm.restore_owed_revisions"
	fields := dlog.Context{"workspace": string(id), "revisions": len(owed)}
	for _, one := range owed {
		if err := validOwedRevision(one); err != nil {
			s.log.Error(op, "refused to restore a malformed owed revision", withError(fields, err))
			return err
		}
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		for _, one := range owed {
			if _, err := tx.ExecContext(ctx,
				`INSERT INTO owed_response_revisions (workspace_id, unit, original, revised, owed_at) VALUES (?, ?, ?, ?, ?)
				 ON CONFLICT(workspace_id, unit) DO NOTHING`,
				id, one.Unit, one.Original, one.Revised, one.OwedAtNs); err != nil {
				return err
			}
		}
		return nil
	})
}

// sortOwedRevisions orders OWED oldest first, the unit breaking a tie, so a
// prompt tells them in the order they landed.
func sortOwedRevisions(owed []OwedRevision) {
	slices.SortFunc(owed, func(a, b OwedRevision) int {
		return cmp.Or(cmp.Compare(a.OwedAtNs, b.OwedAtNs), cmp.Compare(a.Unit, b.Unit))
	})
}
