package wsm

import "context"

// reconnectedTurnsDDL is the layout-82 addition, kept apart from the rest of
// the schema for the reason rolledBackTurnsDDL is: a fresh file gets it as
// part of schemaDDL, a layout-81 file from the 81 -> 82 migration. A row says
// a turn ended on a request that never reached the API, and the network has
// since come back: the feed never draws that turn's connection-failed ending
// again.
const reconnectedTurnsDDL = `
CREATE TABLE reconnected_turns (
  workspace_id    TEXT NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  turn_id         TEXT NOT NULL,
  reconnected_at  INTEGER NOT NULL,
  PRIMARY KEY (workspace_id, turn_id)
);
`

// reconnectedTurns is the set of turns whose connection failure the network
// coming back answered.
var reconnectedTurns = turnSet{
	table: "reconnected_turns", atColumn: "reconnected_at",
	recordOp: "daemon.wsm.record_reconnected_turns", loadOp: "daemon.wsm.reconnected_turns",
	what: "a reconnection",
}

// RecordReconnectedTurns records that the network coming back answered the
// connection failure each of a workspace's turns ended on, all or nothing.
// Recording a turn twice is not an error.
func (s *store) RecordReconnectedTurns(ctx context.Context, id WorkspaceID, turns []TurnID) error {
	return s.recordTurnSet(ctx, reconnectedTurns, id, turns)
}

// ReconnectedTurns loads every turn of a workspace whose connection failure
// the network coming back answered.
func (s *store) ReconnectedTurns(ctx context.Context, id WorkspaceID) ([]TurnID, error) {
	return s.loadTurnSet(ctx, reconnectedTurns, id)
}
