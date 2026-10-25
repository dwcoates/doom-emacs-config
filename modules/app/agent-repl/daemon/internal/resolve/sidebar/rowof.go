package sidebar

import (
	"google.golang.org/protobuf/proto"

	frontendv1 "agentrepl/proto/frontend/v1"
	"agentrepl/protohelpers/rosterwalk"

	"agent-repld/internal/ids"
)

// Row implements Resolver. The row is looked up in the grouping the roster
// shows first, then the hidden section and the recently-merged band, so it is
// the row a client of the roster draws; a clone is answered, never the
// published roster's own message.
func (r *resolver) Row(ws ids.WorkspaceID) (*frontendv1.RosterRow, uint64, bool) {
	r.mu.Lock()
	defer r.mu.Unlock()
	if r.last == nil {
		return nil, 0, false
	}
	row := findRow(r.last, string(ws))
	if row == nil {
		return nil, 0, false
	}
	cloned, ok := proto.Clone(row).(*frontendv1.RosterRow)
	if !ok {
		return nil, 0, false
	}
	return cloned, r.last.GetRevision(), true
}

// Thread implements Resolver: whether the last roster lists WS in its thread
// section, with that roster's revision.
func (r *resolver) Thread(ws ids.WorkspaceID) (uint64, bool) {
	r.mu.Lock()
	defer r.mu.Unlock()
	if r.last == nil {
		return 0, false
	}
	for _, row := range r.last.GetThreads().GetRows() {
		if row.GetThread().GetId() == string(ws) {
			return r.last.GetRevision(), true
		}
	}
	return 0, false
}

// Revision implements Resolver.
func (r *resolver) Revision() uint64 {
	r.mu.Lock()
	defer r.mu.Unlock()
	return r.last.GetRevision()
}

// findRow answers WS's row in ROSTER, nil when it carries none. Each
// workspace is in exactly one of the repository sections, the hidden section
// and the recently-merged band.
func findRow(roster *frontendv1.WorkspaceRoster, ws string) *frontendv1.RosterRow {
	for _, row := range rosterwalk.AllRows(roster) {
		if row.GetWorkspace().GetWorkspace().GetId() == ws {
			return row
		}
	}
	return nil
}
