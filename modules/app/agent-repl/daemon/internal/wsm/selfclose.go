package wsm

import (
	"context"
	"time"

	"agent-repld/internal/dlog"
)

// selfCloseDDL is the layout-63 addition: close_when_turn_ends, set while a
// workspace that asked to close ITSELF waits for its asking turn to end
// (agentrepl.v1.CloseWorkspaceSuccess.closes_when_turn_ends), to when it
// asked. DURABLE because a landing in the daemon's own checkout restarts the
// daemon mid-turn: a close the restart forgot would leave the workspace open
// for good. Every close clears it (SetClosed), so a mark can never outlive
// its workspace's close, however that close came about.
const selfCloseDDL = `
ALTER TABLE workspaces ADD COLUMN close_when_turn_ends INTEGER;
`

// SetCloseWhenTurnEnds marks the workspace to close when its running turn
// ends, as of AT, or clears the mark with nil.
func (s *store) SetCloseWhenTurnEnds(ctx context.Context, id WorkspaceID, at *time.Time) error {
	var value any
	fields := dlog.Context{"marked": at != nil}
	if at != nil {
		value = nanos(*at)
	}
	return s.setWorkspaceColumns(ctx, "daemon.wsm.set_close_when_turn_ends", id, fields,
		workspaceColumn{name: "close_when_turn_ends", value: value})
}
