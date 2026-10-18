package wsm

import (
	"context"
	"database/sql"

	"agent-repld/internal/promptorigin"
)

// THE WORKSPACE WHOSE AGENT SENT A PROMPT (agentrepl.v1
// SubmitPromptRequest.sent_by) is recorded with the prompt while it is held
// and on the turn it runs as, so the recipient's feed heads the prompt with
// the sender live and on every replay. It is a DAEMON fact: workspaces are the
// daemon's, so it rides the daemon's own turn record, exactly as a turn's
// address and mirror do, and never the shim's.
//
// IT IS SET EXACTLY WHEN THE ORIGIN IS PROMPT_ORIGIN_AGENT_SENT. Every write
// refuses a record breaking either half and every read fails on one, so a
// prompt an agent sent is never drawn without the workspace that sent it.
//
// It carries NO foreign key: a sender forgotten later leaves its id standing,
// and the feed draws that sender as one it cannot name, rather than losing the
// fact that a workspace sent the prompt.
const sentByDDL = `
ALTER TABLE turns ADD COLUMN sent_by TEXT;
ALTER TABLE held_prompts ADD COLUMN sent_by TEXT;
`

// sentByColumn is the column value of SENTBY, NULL for none.
func sentByColumn(sentBy *WorkspaceID) any {
	if sentBy == nil {
		return nil
	}
	return string(*sentBy)
}

// decodeSentBy reads the sender column of one row of TABLE, checked against
// the row's ORIGIN.
func decodeSentBy(table, row, origin string, column sql.NullString) (*WorkspaceID, error) {
	var sentBy *WorkspaceID
	if column.Valid {
		id := WorkspaceID(column.String)
		sentBy = &id
	}
	if err := promptorigin.CheckSenderNamed(origin, sentBy != nil); err != nil {
		return nil, &DecodeError{Table: table, Row: row, Field: "sent_by", Err: err}
	}
	return sentBy, nil
}

// TurnSenders answers the recorded sender of each named turn of one workspace
// that has one, all-or-nothing; a turn with none, or one this workspace never
// recorded, is absent from the answer.
func (s *store) TurnSenders(ctx context.Context, id WorkspaceID, turns []TurnID) (map[TurnID]WorkspaceID, error) {
	return turnFacts(ctx, s, "daemon.wsm.turn_senders", id, turns, func(t Turn) (WorkspaceID, bool) {
		if t.SentBy == nil {
			return "", false
		}
		return *t.SentBy, true
	})
}
