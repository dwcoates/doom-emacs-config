package sidebar

import (
	"google.golang.org/protobuf/proto"

	frontendv1 "agentrepl/proto/frontend/v1"
	"agentrepl/protohelpers/rosterwalk"

	"agent-repld/internal/ids"
)

// Row implements Resolver. The row is looked up in the last push's streams
// (the Open section's walk, the hidden section, the closed rows), so it is
// the row a client of the roster draws; a clone is answered, never the
// published push's own message. A MERGED WORKSPACE IS IN NO STREAM, so its
// row is resolved on demand as a page resolves it (readRow), from the merged
// set the last push was resolved from, and stated with that push's revision.
func (r *resolver) Row(ws ids.WorkspaceID) (*frontendv1.RosterRow, uint64, bool) {
	r.mu.Lock()
	defer r.mu.Unlock()
	if r.last == nil {
		return nil, 0, false
	}
	if row := findRow(r.last, string(ws)); row != nil {
		cloned, ok := proto.Clone(row).(*frontendv1.RosterRow)
		if !ok {
			return nil, 0, false
		}
		return cloned, r.last.GetRevision(), true
	}
	for _, rec := range r.partition().merged {
		if rec.ID == ws {
			return r.readRow(rec, r.rowContext("")), r.last.GetRevision(), true
		}
	}
	return nil, 0, false
}

// Thread implements Resolver: whether the last push lists WS in its thread
// section, with that push's revision.
func (r *resolver) Thread(ws ids.WorkspaceID) (uint64, bool) {
	r.mu.Lock()
	defer r.mu.Unlock()
	if r.last == nil {
		return 0, false
	}
	for _, row := range r.last.GetWorkspaces().GetThreads().GetRows() {
		if row.GetThread().GetId() == string(ws) {
			return r.last.GetRevision(), true
		}
	}
	return 0, false
}

// Revision implements Resolver: the last push's revision.
func (r *resolver) Revision() uint64 {
	r.mu.Lock()
	defer r.mu.Unlock()
	return r.last.GetRevision()
}

// findRow answers WS's row in PUSH's streams, nil when they carry none. Each
// workspace is in at most one of the repository sections, the hidden section
// and the closed rows.
func findRow(push *frontendv1.RosterPush, ws string) *frontendv1.RosterRow {
	for _, row := range rosterwalk.AllRows(push) {
		if row.GetWorkspace().GetWorkspace().GetId() == ws {
			return row
		}
	}
	return nil
}
