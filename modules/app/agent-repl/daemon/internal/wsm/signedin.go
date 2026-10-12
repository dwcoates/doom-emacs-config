package wsm

import "context"

// signedInTurnsDDL is the layout-42 addition, kept apart from the rest of the
// schema for the reason rolledBackTurnsDDL is: a fresh file gets it as part of
// schemaDDL, a layout-41 file from the 41 -> 42 migration. A row says a turn
// ended on an account cause a sign-in cures, and a sign-in made through
// agent-repl has since answered it: the feed never draws that turn's failure
// (its broken bubble and its errored ending) again.
const signedInTurnsDDL = `
CREATE TABLE signed_in_turns (
  workspace_id TEXT NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  turn_id      TEXT NOT NULL,
  signed_in_at INTEGER NOT NULL,
  PRIMARY KEY (workspace_id, turn_id)
);
`

// signedInTurns is the set of turns whose account failure a sign-in answered.
var signedInTurns = turnSet{
	table: "signed_in_turns", atColumn: "signed_in_at",
	recordOp: "daemon.wsm.record_signed_in_turns", loadOp: "daemon.wsm.signed_in_turns",
	what: "a sign-in",
}

// RecordSignedInTurns records that a sign-in answered the account failure each
// of a workspace's turns ended on, all or nothing. Recording a turn twice is
// not an error.
func (s *store) RecordSignedInTurns(ctx context.Context, id WorkspaceID, turns []TurnID) error {
	return s.recordTurnSet(ctx, signedInTurns, id, turns)
}

// SignedInTurns loads every turn of a workspace whose account failure a
// sign-in answered.
func (s *store) SignedInTurns(ctx context.Context, id WorkspaceID) ([]TurnID, error) {
	return s.loadTurnSet(ctx, signedInTurns, id)
}
