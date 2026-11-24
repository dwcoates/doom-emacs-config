package wsm

import (
	"context"
	"database/sql"
	"fmt"
	"time"

	"agent-repld/internal/dlog"
)

// THE IDLE CLOCK IS ONE DURABLE FACT, MOVED ONLY BY REAL WORK (owner rule,
// 2026-11-23: a session is parked after StandDownGrace of inactivity, and
// that timer is not reset by deploys or by keep-alives).
//
// last_work_at is the one instant every park reads (cachelapse.Keeper's cold
// and idle parks, the keep-alive cycle's stand-down, and drain's idle sweep).
// RecordWork is its one writer, and its only callers are the edges of real
// work: a prompt the user or another agent sent being delivered
// (promptqueue), and a real turn or live detached work ending (the session
// watcher's free edge, which a keep-alive's own turn never raises as work).
//
// Nothing else can move it. A session start, a keep-alive's restore, its
// prompt, answer and turn end, a deploy's bounce and the turns it orphans, a
// successor's adoption and a takeover start write the session row or the
// turns, never this column: before it, sessions.last_engagement_at was the
// clock, and every bring-up rewrote it (2026-11-23 19:25, 9 parked
// workspaces engaged at the instant a keep-alive restored them, 14 idle for
// days engaged at one mass start 32 hours before). Because the instant is
// on the row, a successor reads the one its predecessor wrote, so a deploy
// cannot change when a session parks.

// lastWorkDDL is the layout-197 addition: last_work_at, when the workspace
// last did real work, in nanoseconds; NULL when it never has. Each existing
// row is filled from last_activity_at, the latest turn start or close, which
// no bring-up ever moved. IT IS ADDITIVE: the previous build's writes name no
// such column. The fresh-file schema appends this same text (schema.go).
const lastWorkDDL = `
ALTER TABLE workspaces ADD COLUMN last_work_at INTEGER;
UPDATE workspaces SET last_work_at = last_activity_at;
`

// WorkCause is the real work a RecordWork call records the end or the
// delivery of. It names the edge on the write's record.
type WorkCause string

const (
	// WorkPromptDelivered is a prompt the user or another agent sent being
	// delivered to the session.
	WorkPromptDelivered WorkCause = "prompt_delivered"
	// WorkEnded is a real turn or live detached work ending: the session
	// falling free after work.
	WorkEnded WorkCause = "work_ended"
)

// RecordWork is THE ONE WRITE of workspace ID's idle clock: real work of
// CAUSE happened at AT. It only ever moves the clock forward (advanceInstant),
// so a late write cannot pull it back. A workspace no longer registered is
// refused.
func (s *store) RecordWork(ctx context.Context, id WorkspaceID, at time.Time, cause WorkCause) error {
	return s.write(ctx, "daemon.wsm.record_work", dlog.Context{"workspace": string(id), "at": at, "cause": string(cause)}, func(ctx context.Context, tx *sql.Tx) error {
		res, err := advanceInstant(ctx, tx, "last_work_at", id, at)
		if err != nil {
			return fmt.Errorf("wsm: record the work of workspace %s: %w", id, err)
		}
		return requireOneRow(res, fmt.Sprintf("wsm: workspace %s", id))
	})
}

// IdleSince answers when the workspace last did real work: LastWorkAt, or
// its creation when it never has. Every park measures its grace from it.
func (w Workspace) IdleSince() time.Time {
	if w.LastWorkAt != nil {
		return *w.LastWorkAt
	}
	return w.CreatedAt
}

// WorkedSince answers whether the workspace did real work after SINCE, and
// when it last did. A keep-alive's turn and a turn a bounce orphaned are no
// work, so neither keeps a session up past a stand-down asked at SINCE.
func (w Workspace) WorkedSince(since time.Time) (time.Time, bool) {
	at := w.IdleSince()
	return at, at.After(since)
}
