package wsm

import (
	"database/sql"
	"errors"
	"fmt"
	"time"
)

// MergeQueuePause is one repository's standing pause
// (frontend.v1.RosterMergeQueuePaused): when it began, who asked for it, and
// whether it was a hard stop.
type MergeQueuePause struct {
	// At is when the queue was paused. Zero for a pause recorded before its
	// record was kept (layout 83).
	At time.Time
	// By is who asked for the pause.
	By MergeQueuePauser
	// Hard reports a hard stop (agentrepl.v1.UpdateMergeQueuePause.hard).
	Hard bool
}

// validate refuses a pause record a writer must never write: one with no
// instant, no pauser, or a workspace pauser with no name. Only a row written
// before layout 83 reads back unrecorded.
func (p MergeQueuePause) validate() error {
	if p.At.IsZero() {
		return errors.New("wsm: a merge queue pause names no instant")
	}
	switch p.By.Kind {
	case MergeQueuePauserUser:
		return nil
	case MergeQueuePauserWorkspace:
		if p.By.Name == "" || p.By.Workspace == "" {
			return errors.New("wsm: a merge queue pause by a workspace names no workspace")
		}
		return nil
	}
	return fmt.Errorf("wsm: a merge queue pause names undeclared pauser kind %d", int(p.By.Kind))
}

// MergeQueuePauser is who paused a queue.
type MergeQueuePauser struct {
	Kind MergeQueuePauserKind
	// Name is the pausing workspace's name as it stood when it paused the
	// queue; empty for any other kind.
	Name string
	// Workspace is the pausing workspace's id; empty for any other kind, and
	// for a pause written before layout 131.
	Workspace WorkspaceID
}

// MergeQueuePauserKind is which kind of asker paused a queue.
type MergeQueuePauserKind int

// The pauser kinds. The zero value is a pause whose asker was not recorded:
// one written before layout 83.
const (
	// MergeQueuePauserUnrecorded is a pause recorded before its asker was.
	MergeQueuePauserUnrecorded MergeQueuePauserKind = iota
	// MergeQueuePauserUser is the user, from the editor or the webapp.
	MergeQueuePauserUser
	// MergeQueuePauserWorkspace is a workspace's agent.
	MergeQueuePauserWorkspace
)

// column is the kind's paused_by value.
func (k MergeQueuePauserKind) column() string {
	switch k {
	case MergeQueuePauserUser:
		return "user"
	case MergeQueuePauserWorkspace:
		return "workspace"
	}
	panic(fmt.Sprintf("wsm: merge queue pauser kind %d has no column value", int(k)))
}

// mergeQueuePauserKindOf decodes a paused_by value: NULL is a pause written
// before layout 83.
func mergeQueuePauserKindOf(by sql.NullString) (MergeQueuePauserKind, error) {
	if !by.Valid {
		return MergeQueuePauserUnrecorded, nil
	}
	switch by.String {
	case "user":
		return MergeQueuePauserUser, nil
	case "workspace":
		return MergeQueuePauserWorkspace, nil
	}
	return 0, fmt.Errorf("undeclared pauser %q", by.String)
}

// mergeQueuePauseRecordDDL is the layout-83 addition: who paused a queue,
// when, and whether it was a hard stop, on the row that holds its pause. IT IS
// ADDITIVE: the previous build's pause writes name none of these columns, so a
// pause it writes reads back unrecorded, which is what a pause it wrote is.
const mergeQueuePauseRecordDDL = `
ALTER TABLE merge_queue_repos ADD COLUMN paused_at INTEGER;
ALTER TABLE merge_queue_repos ADD COLUMN paused_by TEXT;
ALTER TABLE merge_queue_repos ADD COLUMN paused_by_name TEXT;
ALTER TABLE merge_queue_repos ADD COLUMN paused_hard INTEGER NOT NULL DEFAULT 0;
`

// mergeQueuePauserWorkspaceDDL is the layout-131 addition: the id of the
// workspace that paused a queue, beside its name, so a pause is known as a
// workspace's own across a rename (the queue bypass holder lets through only
// its own pause at its turn's end). IT IS ADDITIVE: the previous build's
// pause writes name no such column, and a workspace pause read back with none
// is no workspace's own.
const mergeQueuePauserWorkspaceDDL = `
ALTER TABLE merge_queue_repos ADD COLUMN paused_by_workspace TEXT;
`
