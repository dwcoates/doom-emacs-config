package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"strings"
	"time"

	"agent-repld/internal/dlog"
)

// This file is the durable half of WORKSPACE SUBSCRIPTIONS
// (agentrepl.v1.SubscribeWorkspace): which workspace's agent is told which of
// another workspace's lifecycle edges. DURABLE for the reason a merge queue
// subscription is: a landing in the daemon's own checkout restarts the
// daemon, and a subscription a restart dropped would end at the very change
// it exists to report.
//
// A SUBSCRIPTION ENDS STRUCTURALLY: SetClosed deletes the closing
// subscriber's subscriptions in the transaction that marks it closed, and
// forgetting either workspace's row cascades to them, so no close or forget
// path can leave one standing. A closed workspace cannot take one either
// (ErrSubscriberClosed).

// workspaceSubscriptionsDDL is the layout-62 addition: one row per
// subscription, keyed by its subscriber and the workspace it follows, with
// the followed workspace indexed for the edge's lookup. The fresh-file schema
// appends this same text (schema.go), so a migrated file and a created one
// cannot differ.
const workspaceSubscriptionsDDL = `
CREATE TABLE workspace_subscriptions (
  subscriber    TEXT NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  followed      TEXT NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  events        INTEGER NOT NULL,
  subscribed_at INTEGER NOT NULL,
  PRIMARY KEY (subscriber, followed)
);
CREATE INDEX workspace_subscriptions_followed ON workspace_subscriptions(followed);
`

// WorkspaceEvent is one lifecycle edge a workspace subscription may ask for
// (agentrepl.v1.WorkspaceNoticeEvent). ITS VALUES ARE DURABLE bit positions
// and must not be renumbered.
type WorkspaceEvent int

// The events. The zero value is no event.
const (
	WorkspaceEventTurnStarted WorkspaceEvent = iota + 1
	WorkspaceEventTurnEnded
	WorkspaceEventNeedsInput
	WorkspaceEventLanded
	WorkspaceEventClosed
)

// workspaceEvents is every declared event, in order.
var workspaceEvents = []WorkspaceEvent{WorkspaceEventTurnStarted, WorkspaceEventTurnEnded, WorkspaceEventNeedsInput, WorkspaceEventLanded, WorkspaceEventClosed}

// String names the event in log records and in a notice.
func (e WorkspaceEvent) String() string {
	switch e {
	case WorkspaceEventTurnStarted:
		return "turn_started"
	case WorkspaceEventTurnEnded:
		return "turn_ended"
	case WorkspaceEventNeedsInput:
		return "needs_input"
	case WorkspaceEventLanded:
		return "landed"
	case WorkspaceEventClosed:
		return "closed"
	}
	return fmt.Sprintf("WorkspaceEvent(%d)", int(e))
}

// WorkspaceEvents is a set of events, stored as its bits.
type WorkspaceEvents uint32

// NewWorkspaceEvents is the set of EVENTS; a repeat is one member.
func NewWorkspaceEvents(events ...WorkspaceEvent) WorkspaceEvents {
	var set WorkspaceEvents
	for _, e := range events {
		set |= 1 << uint(e)
	}
	return set
}

// Has reports whether E is a member.
func (s WorkspaceEvents) Has(e WorkspaceEvent) bool { return s&(1<<uint(e)) != 0 }

// Valid reports whether the set is non-empty and holds only declared events.
func (s WorkspaceEvents) Valid() bool {
	return s != 0 && s&^NewWorkspaceEvents(workspaceEvents...) == 0
}

// String names the members in order, comma-separated.
func (s WorkspaceEvents) String() string {
	var names []string
	for _, e := range workspaceEvents {
		if s.Has(e) {
			names = append(names, e.String())
		}
	}
	return strings.Join(names, ",")
}

// WorkspaceSubscription is one workspace's subscription to another's edges.
type WorkspaceSubscription struct {
	// Subscriber is the workspace whose agent is told.
	Subscriber WorkspaceID
	// Followed is the workspace whose edges are told.
	Followed WorkspaceID
	// Events are the edges told.
	Events WorkspaceEvents
	// SubscribedAt is when the subscription took its current events.
	SubscribedAt time.Time
}

// ErrUnknownFollowed is a subscription refused because the workspace it
// follows is not registered.
var ErrUnknownFollowed = errors.New("wsm: the followed workspace is not registered")

// SubscribeWorkspace records SUB, replacing the events of the subscription
// its subscriber already holds to the same workspace, and answers whether it
// replaced one. Both workspaces' rows are checked in the same transaction, so
// a close or a forget cannot slip between the check and the write: an
// unregistered subscriber is ErrNotFound, a closed one ErrSubscriberClosed,
// and an unregistered followed workspace ErrUnknownFollowed.
func (s *store) SubscribeWorkspace(ctx context.Context, sub WorkspaceSubscription) (bool, error) {
	const op = "daemon.wsm.subscribe_workspace"
	fields := dlog.Context{"subscriber": string(sub.Subscriber), "followed": string(sub.Followed), "events": sub.Events.String()}
	if !sub.Events.Valid() || sub.Subscriber == sub.Followed {
		err := fmt.Errorf("wsm: workspace subscription of %s to %s: events %b, or a workspace following itself", sub.Subscriber, sub.Followed, uint32(sub.Events))
		s.log.Error(op, "refused a workspace subscription its caller should never have made", withError(fields, err))
		return false, err
	}
	var replaced bool
	err := s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		var closed bool
		err := tx.QueryRowContext(ctx, `SELECT closed FROM workspaces WHERE id = ?`, sub.Subscriber).Scan(&closed)
		switch {
		case errors.Is(err, sql.ErrNoRows):
			return fmt.Errorf("wsm: workspace subscription of %s: %w", sub.Subscriber, ErrNotFound)
		case err != nil:
			return err
		case closed:
			return ErrSubscriberClosed
		}
		var followed int
		if err := tx.QueryRowContext(ctx, `SELECT COUNT(*) FROM workspaces WHERE id = ?`, sub.Followed).Scan(&followed); err != nil {
			return err
		}
		if followed == 0 {
			return ErrUnknownFollowed
		}
		var held int
		if err := tx.QueryRowContext(ctx, `SELECT COUNT(*) FROM workspace_subscriptions WHERE subscriber = ? AND followed = ?`,
			sub.Subscriber, sub.Followed).Scan(&held); err != nil {
			return err
		}
		replaced = held > 0
		_, err = tx.ExecContext(ctx,
			`INSERT INTO workspace_subscriptions (subscriber, followed, events, subscribed_at) VALUES (?, ?, ?, ?)
			 ON CONFLICT(subscriber, followed) DO UPDATE SET events = excluded.events, subscribed_at = excluded.subscribed_at`,
			sub.Subscriber, sub.Followed, int64(sub.Events), nanos(sub.SubscribedAt))
		return err
	})
	return replaced, err
}

// UnsubscribeWorkspace ends SUBSCRIBER's subscription to FOLLOWED, and
// answers whether one stood.
func (s *store) UnsubscribeWorkspace(ctx context.Context, subscriber, followed WorkspaceID) (bool, error) {
	const op = "daemon.wsm.unsubscribe_workspace"
	fields := dlog.Context{"subscriber": string(subscriber), "followed": string(followed)}
	var ended bool
	err := s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `DELETE FROM workspace_subscriptions WHERE subscriber = ? AND followed = ?`, subscriber, followed)
		if err != nil {
			return err
		}
		ended, err = touchedRow(res)
		return err
	})
	return ended, err
}

// WorkspaceSubscribers loads every subscription that follows FOLLOWED, by
// subscriber, all-or-nothing. A row whose events are undeclared refuses the
// whole read: it would otherwise be told events nobody asked for.
func (s *store) WorkspaceSubscribers(ctx context.Context, followed WorkspaceID) ([]WorkspaceSubscription, error) {
	var out []WorkspaceSubscription
	err := s.read(ctx, "daemon.wsm.workspace_subscribers", dlog.Context{"followed": string(followed)}, func(ctx context.Context) error {
		rows, err := s.q(ctx).QueryContext(ctx,
			`SELECT subscriber, events, subscribed_at FROM workspace_subscriptions WHERE followed = ? ORDER BY subscriber`, followed)
		if err != nil {
			return err
		}
		defer rows.Close()
		var loaded []WorkspaceSubscription
		for rows.Next() {
			var (
				sub    = WorkspaceSubscription{Followed: followed}
				events int64
				at     int64
			)
			if err := rows.Scan(&sub.Subscriber, &events, &at); err != nil {
				return err
			}
			sub.Events = WorkspaceEvents(events)
			if !sub.Events.Valid() {
				return fmt.Errorf("wsm: workspace subscription of %s to %s holds undeclared events %b", sub.Subscriber, followed, events)
			}
			sub.SubscribedAt = fromNanos(at)
			loaded = append(loaded, sub)
		}
		if err := rows.Err(); err != nil {
			return err
		}
		out = loaded
		return nil
	})
	return out, err
}

// endWorkspaceSubscriptionsInTx deletes every workspace subscription WS holds
// as a subscriber, inside the caller's transaction, answering how many it
// ended. SetClosed calls it in the transaction that marks WS closed.
func endWorkspaceSubscriptionsInTx(ctx context.Context, tx *sql.Tx, ws WorkspaceID) (int64, error) {
	res, err := tx.ExecContext(ctx, `DELETE FROM workspace_subscriptions WHERE subscriber = ?`, ws)
	if err != nil {
		return 0, err
	}
	// A CLOSED WORKSPACE IS TOLD NOTHING MORE, so its untold notices go with
	// its subscriptions (workspacenotices.go).
	if _, err := tx.ExecContext(ctx, `DELETE FROM workspace_untold_notices WHERE subscriber = ?`, ws); err != nil {
		return 0, err
	}
	return res.RowsAffected()
}
