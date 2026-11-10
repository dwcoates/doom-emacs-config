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

// ContextUsageRecord is a workspace's last-known context fact: Usage is the
// last reading, or Cut reports a cut has discarded it with no fresh reading
// since. Exactly one holds.
type ContextUsageRecord struct {
	// Usage is the vendor's last get_context_usage answer, nil when Cut.
	Usage *conversationv1.SessionContextUsage
	// Cut reports a context cut stands with no fresh reading.
	Cut bool
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
			`SELECT usage, cut FROM workspace_context_usage WHERE workspace_id = ?`, id).Scan(&blob, &cut)
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
	return s.putContextUsage(ctx, op, fields, id, blob, false)
}

// PutWorkspaceContextCut records that a cut discarded workspace ID's last
// reading, so a revived workspace states the count is unknown until a fresh
// reading arrives.
func (s *store) PutWorkspaceContextCut(ctx context.Context, id WorkspaceID) error {
	return s.putContextUsage(ctx, "daemon.wsm.put_workspace_context_cut", dlog.Context{"workspace": string(id)}, id, nil, true)
}

// putContextUsage replaces workspace ID's context fact with BLOB and CUT.
func (s *store) putContextUsage(ctx context.Context, op string, fields dlog.Context, id WorkspaceID, blob []byte, cut bool) error {
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx,
			`INSERT INTO workspace_context_usage (workspace_id, usage, cut) VALUES (?, ?, ?)
			 ON CONFLICT(workspace_id) DO UPDATE SET usage = excluded.usage, cut = excluded.cut`, id, blob, cut)
		return err
	})
}
