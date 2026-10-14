package wsm

import (
	"database/sql"
	"errors"
	"fmt"
	"time"
)

// This file is the durable half of a workspace's CLOSE RECORD
// (frontend.v1.RosterRowClosure, docs/protobuf-design/roster-closure.md):
// when the workspace was closed, and who closed it, so the editor's switcher
// can offer the workspaces the USER most recently put away and leave out the
// ones the daemon or an agent closed on its own.

// closureDDL is the layout-52 addition. It adds:
//
//   - workspaces.closed_at: when the workspace was closed, in nanoseconds.
//   - workspaces.closed_by: who closed it, a CloseCause.
//
// Both stand and fall together, and only on a closed row. A row already
// closed when the columns land keeps NULL in both: nothing records when or by
// whom it was closed, so none is guessed.
//
// IT IS ADDITIVE: the previous build names neither column, so a close it
// writes leaves both NULL, which reads as a close with no record.
const closureDDL = `
ALTER TABLE workspaces ADD COLUMN closed_at INTEGER;
ALTER TABLE workspaces ADD COLUMN closed_by TEXT;
`

// CloseCause says who closed a workspace.
type CloseCause string

// The causes a close can have. A closed set: a stored value outside it is a
// corrupt row.
const (
	// CloseByUser is the user's own gesture: CloseWorkspace or KillWorkspace.
	CloseByUser CloseCause = "user"
	// CloseByAgent is an agent's request, through a workspace command file.
	CloseByAgent CloseCause = "agent"
	// CloseByMerge is the merge that landed the workspace.
	CloseByMerge CloseCause = "merge"
	// CloseByDaemon is the daemon itself, unasked: the workspace's directory
	// was gone, or a closed row's serving ownership was released.
	CloseByDaemon CloseCause = "daemon"
)

// valid reports whether a cause is one of the declared ones.
func (c CloseCause) valid() bool {
	switch c {
	case CloseByUser, CloseByAgent, CloseByMerge, CloseByDaemon:
		return true
	}
	return false
}

// Closure is a closed workspace's close record.
type Closure struct {
	// At is when the workspace was closed.
	At time.Time
	// By is who closed it.
	By CloseCause
}

// decodeClosure rebuilds a row's close record. The instant and the cause are
// written in one statement (MarkClosed) and cleared in one (Reopen), and only
// a closed row carries them, so a row with one and not the other, or an open
// row with either, is corrupt.
func decodeClosure(ws Workspace, at sql.NullInt64, by sql.NullString) (*Closure, error) {
	if at.Valid != by.Valid {
		return nil, &DecodeError{Table: "workspaces", Row: string(ws.ID), Field: "closed_by", Err: errors.New("a close records its instant and its cause together")}
	}
	if !at.Valid {
		return nil, nil
	}
	if !ws.Closed {
		return nil, &DecodeError{Table: "workspaces", Row: string(ws.ID), Field: "closed_at", Err: errors.New("only a closed workspace carries a close record")}
	}
	cause := CloseCause(by.String)
	if !cause.valid() {
		return nil, &DecodeError{Table: "workspaces", Row: string(ws.ID), Field: "closed_by", Err: fmt.Errorf("unknown close cause %q", by.String)}
	}
	return &Closure{At: fromNanos(at.Int64), By: cause}, nil
}
