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
//   - workspaces.closure_at: when the workspace was closed, in nanoseconds.
//   - workspaces.closure_by: who closed it, a CloseCause.
//
// Both stand and fall together, and only on a closed row. A row already
// closed when the columns land keeps NULL in both: nothing records when or by
// whom it was closed, so none is guessed.
//
// IT IS ADDITIVE: the previous build names neither column, so a close it
// writes leaves both NULL, which reads as a close with no record.
const closureDDL = `
ALTER TABLE workspaces ADD COLUMN closure_at INTEGER;
ALTER TABLE workspaces ADD COLUMN closure_by TEXT;
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
	// was gone, a closed row's serving ownership was released, or the merge a
	// merge workspace existed to run was taken off its queue.
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

// IsGesture reports whether a close is someone's ASK (the user's or an
// agent's) rather than the daemon recording what already happened (a merge
// landed, a directory vanished). Only an ask is refused by the family rules
// (family.go).
func (c CloseCause) IsGesture() bool {
	return c == CloseByUser || c == CloseByAgent
}

// CloseHow says how a workspace ended, as the edge that tells its followers
// puts it: closed, killed or nuked. THE ZERO VALUE IS A CLOSE, so a close
// path that names nothing (a landing's close, the daemon's housekeeping, the
// user's close) is told as a close.
type CloseHow int

// The ways a workspace ends.
const (
	// HowClosed is every close that is not a kill or a nuke.
	HowClosed CloseHow = iota
	// HowKilled is the forced stop (KillWorkspace): the session is killed and
	// the workspace closes, with its worktree, branch and records kept.
	HowKilled
	// HowNuked is NukeWorkspace: the session is killed, the worktree and
	// branch destroyed, and the record forgotten.
	HowNuked
)

// String names how in a notice.
func (h CloseHow) String() string {
	switch h {
	case HowClosed:
		return "closed"
	case HowKilled:
		return "killed"
	case HowNuked:
		return "nuked"
	}
	return fmt.Sprintf("CloseHow(%d)", int(h))
}

// Closure is a closed workspace's close record.
type Closure struct {
	// At is when the workspace was closed.
	At time.Time
	// By is who closed it.
	By CloseCause
	// How is how it ended. IT IS TOLD WITH THE CLOSE EDGE AND NEVER STORED: a
	// record read back (decodeClosure) carries the zero value, and the one
	// consumer is the notice to the workspace's followers (wsedges).
	How CloseHow
}

// decodeClosure rebuilds a row's close record. The instant and the cause are
// written in one statement (MarkClosed) and cleared in one (Reopen), and only
// a closed row carries them, so a row with one and not the other, or an open
// row with either, is corrupt.
func decodeClosure(ws Workspace, at sql.NullInt64, by sql.NullString) (*Closure, error) {
	if at.Valid != by.Valid {
		return nil, &DecodeError{Table: "workspaces", Row: string(ws.ID), Field: "closure_by", Err: errors.New("a close records its instant and its cause together")}
	}
	if !at.Valid {
		return nil, nil
	}
	if !ws.Closed {
		return nil, &DecodeError{Table: "workspaces", Row: string(ws.ID), Field: "closure_at", Err: errors.New("only a closed workspace carries a close record")}
	}
	cause := CloseCause(by.String)
	if !cause.valid() {
		return nil, &DecodeError{Table: "workspaces", Row: string(ws.ID), Field: "closure_by", Err: fmt.Errorf("unknown close cause %q", by.String)}
	}
	return &Closure{At: fromNanos(at.Int64), By: cause}, nil
}
