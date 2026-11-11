package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"strings"

	"agent-repld/internal/dlog"
)

// owedStartWorkDDL is the layout-123 addition: the work a queue-bypass switch
// owes the next start of the conversation it bound (owner request,
// 2026-11-11), and the summary a start carried that the workspace's next
// prompt has yet to deliver. IT IS ADDITIVE: two new tables the previous
// build never names.
//
// THE OWED WORK IS DURABLE UNTIL A START CARRIES IT OUT. It is written with
// the binding it belongs to, in the same transaction, read by every start of
// the workspace, and retired only by the start that succeeded with it, in the
// same transaction as the summary that start answered. Any later binding
// forgets both, since neither is about the conversation now bound.
//
// EXACTLY ONE KIND OF WORK PER ROW, which the CHECK holds: a compaction of the
// conversation the start resumes, or a summary of another one of the
// workspace's conversations.
const owedStartWorkDDL = `
CREATE TABLE owed_start_work (
  workspace_id TEXT PRIMARY KEY REFERENCES workspaces(id) ON DELETE CASCADE,
  compact      INTEGER NOT NULL DEFAULT 0,
  summary_of   TEXT,
  CHECK ((compact = 1) <> (summary_of IS NOT NULL))
);
CREATE TABLE owed_context_summaries (
  workspace_id TEXT PRIMARY KEY REFERENCES workspaces(id) ON DELETE CASCADE,
  summary      TEXT NOT NULL
);
`

// OwedStartWork is the work the next start of a workspace's conversation owes.
// Exactly one of its fields is set.
type OwedStartWork struct {
	// Compact asks the start to compact the conversation it resumes (a
	// compacted fork of the queue conversation).
	Compact bool
	// SummaryOf names another conversation of the workspace whose summary the
	// start carries into the one it resumes (the parked bypass conversation
	// plus a compacted summary of the queue conversation).
	SummaryOf string
}

// validOwedStartWork refuses owed work naming no kind, or both.
func validOwedStartWork(owed OwedStartWork) error {
	summarizes := strings.TrimSpace(owed.SummaryOf) != ""
	if owed.Compact == summarizes {
		return fmt.Errorf("wsm: owed start work is a compaction or a summary, exactly one (compact %v, summary of %q)", owed.Compact, owed.SummaryOf)
	}
	return nil
}

// insertOwedStartWork writes OWED for workspace WS inside the caller's
// transaction, which has already forgotten any earlier owed work.
func insertOwedStartWork(ctx context.Context, tx *sql.Tx, ws WorkspaceID, owed OwedStartWork) error {
	var summaryOf any
	if owed.SummaryOf != "" {
		summaryOf = owed.SummaryOf
	}
	_, err := tx.ExecContext(ctx, `INSERT INTO owed_start_work (workspace_id, compact, summary_of) VALUES (?, ?, ?)`,
		ws, owed.Compact, summaryOf)
	return err
}

// OwedStartWork answers the work the next start of workspace ID owes, and
// whether it owes any.
func (s *store) OwedStartWork(ctx context.Context, id WorkspaceID) (OwedStartWork, bool, error) {
	var (
		out   OwedStartWork
		found bool
	)
	err := s.read(ctx, "daemon.wsm.owed_start_work", dlog.Context{"workspace": string(id)}, func(ctx context.Context) error {
		var summaryOf sql.NullString
		err := s.q(ctx).QueryRowContext(ctx,
			`SELECT compact, summary_of FROM owed_start_work WHERE workspace_id = ?`, id).Scan(&out.Compact, &summaryOf)
		if errors.Is(err, sql.ErrNoRows) {
			return nil
		}
		if err != nil {
			return err
		}
		out.SummaryOf, found = summaryOf.String, true
		return nil
	})
	return out, found, err
}

// SettleOwedStartWork retires workspace ID's owed start work once a start
// carried it out, and records SUMMARY, the summary that start answered, for
// the workspace's next prompt to deliver: both in ONE transaction, so the
// work is never retired with its summary lost, nor the summary recorded with
// the work still owed. An empty SUMMARY records none (a compaction answers
// no summary). Owed work that does not stand is refused: only the start that
// carried it settles it.
func (s *store) SettleOwedStartWork(ctx context.Context, id WorkspaceID, summary string) error {
	const op = "daemon.wsm.settle_owed_start_work"
	return s.write(ctx, op, dlog.Context{"workspace": string(id), "summary": summary != ""}, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `DELETE FROM owed_start_work WHERE workspace_id = ?`, id)
		if err != nil {
			return err
		}
		if n, err := res.RowsAffected(); err != nil {
			return err
		} else if n != 1 {
			return fmt.Errorf("wsm: workspace %s owes no start work to settle", id)
		}
		if summary == "" {
			return nil
		}
		_, err = tx.ExecContext(ctx,
			`INSERT INTO owed_context_summaries (workspace_id, summary) VALUES (?, ?)
			 ON CONFLICT(workspace_id) DO UPDATE SET summary = excluded.summary`, id, summary)
		return err
	})
}

// TakeOwedContextSummary takes the summary workspace ID's next prompt
// delivers: it answers it and deletes it in one transaction, so two prompts
// never both carry it. False when none is owed.
func (s *store) TakeOwedContextSummary(ctx context.Context, id WorkspaceID) (string, bool, error) {
	const op = "daemon.wsm.take_owed_context_summary"
	var (
		summary string
		found   bool
	)
	err := s.write(ctx, op, dlog.Context{"workspace": string(id)}, func(ctx context.Context, tx *sql.Tx) error {
		err := tx.QueryRowContext(ctx, `DELETE FROM owed_context_summaries WHERE workspace_id = ? RETURNING summary`, id).Scan(&summary)
		if errors.Is(err, sql.ErrNoRows) {
			return nil
		}
		if err != nil {
			return err
		}
		found = true
		return nil
	})
	return summary, found, err
}

// RestoreOwedContextSummary puts back a summary TakeOwedContextSummary took
// for a prompt the queue then refused, so the next prompt carries it. A
// summary that already stands again is left as it is: it is the newer one.
func (s *store) RestoreOwedContextSummary(ctx context.Context, id WorkspaceID, summary string) error {
	const op = "daemon.wsm.restore_owed_context_summary"
	fields := dlog.Context{"workspace": string(id)}
	if summary == "" {
		err := fmt.Errorf("wsm: a restored context summary is never empty (workspace %s)", id)
		s.log.Error(op, "refused to restore an empty context summary", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx,
			`INSERT INTO owed_context_summaries (workspace_id, summary) VALUES (?, ?) ON CONFLICT(workspace_id) DO NOTHING`, id, summary)
		return err
	})
}
