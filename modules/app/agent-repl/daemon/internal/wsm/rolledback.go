package wsm

import (
	"context"
)

// rolledBackTurnsDDL is the layout-17 addition, kept apart from the rest of the
// schema for the reason recordedRowsDDL is: a fresh file gets it as part of
// schemaDDL, a layout-16 file from the 16 -> 17 migration. A row says a turn
// was rolled back (agentrepl.v1.RollBack): the feed never draws it again.
const rolledBackTurnsDDL = `
CREATE TABLE rolled_back_turns (
  workspace_id   TEXT NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  turn_id        TEXT NOT NULL,
  rolled_back_at INTEGER NOT NULL,
  PRIMARY KEY (workspace_id, turn_id)
);
`

// rolledBackTurns is the rolled-back turns' set.
var rolledBackTurns = turnSet{
	table: "rolled_back_turns", atColumn: "rolled_back_at",
	recordOp: "daemon.wsm.record_rolled_back_turns", loadOp: "daemon.wsm.rolled_back_turns",
	what: "a rollback",
}

// RecordRolledBackTurns records that a workspace's turns were rolled back, all
// or nothing. Recording a turn twice is not an error: it is as rolled back as
// asked.
func (s *store) RecordRolledBackTurns(ctx context.Context, id WorkspaceID, turns []TurnID) error {
	return s.recordTurnSet(ctx, rolledBackTurns, id, turns)
}

// RolledBackTurns loads every turn of a workspace that was rolled back.
func (s *store) RolledBackTurns(ctx context.Context, id WorkspaceID) ([]TurnID, error) {
	return s.loadTurnSet(ctx, rolledBackTurns, id)
}
