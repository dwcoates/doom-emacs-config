package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"

	"google.golang.org/protobuf/proto"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
)

// workspaceContextUsageDDL is the layout-109 addition: the topbar context
// chip's last-known fact, kept so a workspace revived after the daemon
// restarts draws the figure and window its last session reported rather than
// a 0 it is not at. IT IS ADDITIVE: one new table the previous build never
// names.
//
// A row is EXACTLY ONE of two facts, and the CHECK makes any other shape
// unrepresentable: a reading (usage is the vendor's last
// conversation.v1.SessionContextUsage, cut 0), or a cut that discarded the
// transcript the last reading was read off with no fresh reading since (usage
// NULL, cut 1).
const workspaceContextUsageDDL = `
CREATE TABLE workspace_context_usage (
  workspace_id TEXT PRIMARY KEY REFERENCES workspaces(id) ON DELETE CASCADE,
  usage        BLOB,
  cut          INTEGER NOT NULL CHECK (cut IN (0, 1)),
  CHECK ((usage IS NULL) = (cut = 1))
);
`

// sessionContextBankDDL is the layout-181 addition: TOTAL SESSION CONTEXT,
// the context the workspace's sessions held before the last cut, summed over
// every cut (each /clear or completed compaction banks the context it
// discards). It lives on the row of the fact it accompanies, so a cut writes
// the discarded reading's retirement and its banking in ONE statement and a
// crash between the two is unrepresentable. IT IS ADDITIVE: the column takes
// 0 on every row the previous build writes, which is a session that has cut
// nothing banked.
const sessionContextBankDDL = `
ALTER TABLE workspace_context_usage ADD COLUMN banked_tokens INTEGER NOT NULL DEFAULT 0 CHECK (banked_tokens >= 0);
`

// ContextUsageRecord is a workspace's last-known context fact: Usage is the
// last reading, or Cut reports a cut has discarded it with no fresh reading
// since. Exactly one holds.
type ContextUsageRecord struct {
	// Usage is the vendor's last get_context_usage answer, nil when Cut.
	Usage *conversationv1.SessionContextUsage
	// Cut reports a context cut stands with no fresh reading.
	Cut bool
	// BankedTokens is the context every earlier cut discarded, summed: the
	// part of the workspace's total session context the live context is not.
	BankedTokens int64
}

// WorkspaceContextUsage answers workspace ID's last-known context fact, false
// when none was ever recorded. A stored reading that does not decode is a
// DecodeError.
func (s *store) WorkspaceContextUsage(ctx context.Context, id WorkspaceID) (ContextUsageRecord, bool, error) {
	var out ContextUsageRecord
	found := false
	err := s.read(ctx, "daemon.wsm.workspace_context_usage", dlog.Context{"workspace": string(id)}, func(ctx context.Context) error {
		var blob []byte
		var cut bool
		err := s.q(ctx).QueryRowContext(ctx,
			`SELECT usage, cut, banked_tokens FROM workspace_context_usage WHERE workspace_id = ?`, id).Scan(&blob, &cut, &out.BankedTokens)
		switch {
		case errors.Is(err, sql.ErrNoRows):
			return nil
		case err != nil:
			return err
		}
		if !cut {
			usage := &conversationv1.SessionContextUsage{}
			if err := proto.Unmarshal(blob, usage); err != nil {
				return &DecodeError{Table: "workspace_context_usage", Row: string(id), Field: "usage", Err: err}
			}
			out.Usage = usage
		}
		out.Cut, found = cut, true
		return nil
	})
	return out, found, err
}

// PutWorkspaceContextUsage records USAGE as workspace ID's last reading,
// retiring any cut standing. A nil reading is a bug in the caller, refused
// loudly.
func (s *store) PutWorkspaceContextUsage(ctx context.Context, id WorkspaceID, usage *conversationv1.SessionContextUsage) error {
	const op = "daemon.wsm.put_workspace_context_usage"
	fields := dlog.Context{"workspace": string(id), "total_tokens": usage.GetTotalTokens(), "max_tokens": usage.GetMaxTokens()}
	if usage == nil {
		err := fmt.Errorf("wsm: a workspace's context reading is a reading (workspace %s)", id)
		s.log.Error(op, "refused a nil context reading", withError(fields, err))
		return err
	}
	blob, err := proto.Marshal(usage)
	if err != nil {
		return fmt.Errorf("wsm: encode the context reading of %s: %w", id, err)
	}
	return s.putContextUsage(ctx, op, fields, id, blob, false, nil)
}

// PutWorkspaceContextCut records that a cut discarded workspace ID's last
// reading, so a revived workspace states the count is unknown until a fresh
// reading arrives, and records BANKED as the context every cut so far has
// discarded, in the same write. A negative BANKED is a bug in the caller,
// refused loudly.
func (s *store) PutWorkspaceContextCut(ctx context.Context, id WorkspaceID, banked int64) error {
	const op = "daemon.wsm.put_workspace_context_cut"
	fields := dlog.Context{"workspace": string(id), "banked_tokens": banked}
	if banked < 0 {
		err := fmt.Errorf("wsm: a workspace's banked context is not negative (workspace %s, %d)", id, banked)
		s.log.Error(op, "refused a negative banked context", withError(fields, err))
		return err
	}
	return s.putContextUsage(ctx, op, fields, id, nil, true, &banked)
}

// putContextUsage replaces workspace ID's context fact with BLOB and CUT. A
// non-nil BANKED replaces the banked context too; nil leaves it as it stands
// (a fresh row starts at 0).
func (s *store) putContextUsage(ctx context.Context, op string, fields dlog.Context, id WorkspaceID, blob []byte, cut bool, banked *int64) error {
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		var bankedValue int64
		if banked != nil {
			bankedValue = *banked
		}
		_, err := tx.ExecContext(ctx,
			`INSERT INTO workspace_context_usage (workspace_id, usage, cut, banked_tokens) VALUES (?, ?, ?, ?)
			 ON CONFLICT(workspace_id) DO UPDATE SET usage = excluded.usage, cut = excluded.cut,
			   banked_tokens = CASE WHEN ? THEN excluded.banked_tokens ELSE banked_tokens END`,
			id, blob, cut, bankedValue, banked != nil)
		return err
	})
}
