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

// closureHowDDL is the layout-219 addition: how each workspace ended
// (workspaces.closure_how, a CloseHow's name), so a find answers a kill apart
// from a close, and which child each folded production child was folded into
// (production_children.folded_into), and every site each production child
// was given, at its dispatch or by a fold, which outlives its ownership
// (production_child_named_sites), seeded from the sites owned and fixed now.
// IT IS ADDITIVE: two nullable columns and a table the previous build never
// names. A close it writes leaves closure_how NULL, read as HowUnrecorded, as
// every close made before this layout is.
const closureHowDDL = `
ALTER TABLE workspaces ADD COLUMN closure_how TEXT;
ALTER TABLE production_children ADD COLUMN folded_into TEXT;
CREATE TABLE production_child_named_sites (
  workspace_id TEXT NOT NULL,
  site         TEXT NOT NULL CHECK (length(site) = 12),
  PRIMARY KEY (workspace_id, site)
);
INSERT OR IGNORE INTO production_child_named_sites (workspace_id, site) SELECT workspace_id, site FROM production_child_sites;
INSERT OR IGNORE INTO production_child_named_sites (workspace_id, site) SELECT workspace_id, site FROM production_fixed_sites;
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
	// hidden merge workspace existed to run ended without landing.
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
	// HowUnrecorded is a close record read back with no kind: a close made
	// before the daemon recorded how a workspace ended (closureHowDDL). It is
	// only ever read, never written.
	HowUnrecorded
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
	case HowUnrecorded:
		return "unrecorded"
	}
	return fmt.Sprintf("CloseHow(%d)", int(h))
}

// Closure is a closed workspace's close record.
type Closure struct {
	// At is when the workspace was closed.
	At time.Time
	// By is who closed it.
	By CloseCause
	// How is how it ended: told with the close edge to the workspace's
	// followers (wsedges), and stored with the record, so a find answers it
	// (closureHowDDL). A record read back from before it was stored is
	// HowUnrecorded.
	How CloseHow
}

// decodeClosure rebuilds a row's close record. The instant and the cause are
// written in one statement (MarkClosed) and cleared in one (Reopen), and only
// a closed row carries them, so a row with one and not the other, or an open
// row with either, is corrupt.
func decodeClosure(ws Workspace, at sql.NullInt64, by, how sql.NullString) (*Closure, error) {
	if how.Valid && !at.Valid {
		return nil, &DecodeError{Table: "workspaces", Row: string(ws.ID), Field: "closure_how", Err: errors.New("only a close record carries how the workspace ended")}
	}
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
	kind := HowUnrecorded
	if how.Valid {
		var ok bool
		if kind, ok = closeHowNamed(how.String); !ok {
			return nil, &DecodeError{Table: "workspaces", Row: string(ws.ID), Field: "closure_how", Err: fmt.Errorf("unknown close kind %q", how.String)}
		}
	}
	return &Closure{At: fromNanos(at.Int64), By: cause, How: kind}, nil
}

// closeHowNamed is the CloseHow a stored closure_how names: one a close
// writes. HowUnrecorded is never stored, so it names none.
func closeHowNamed(name string) (CloseHow, bool) {
	for _, how := range []CloseHow{HowClosed, HowKilled, HowNuked} {
		if how.String() == name {
			return how, true
		}
	}
	return 0, false
}

// stored reports whether H is a kind a close writes: never HowUnrecorded,
// which only a record read back carries.
func (h CloseHow) stored() bool {
	_, ok := closeHowNamed(h.String())
	return ok
}
