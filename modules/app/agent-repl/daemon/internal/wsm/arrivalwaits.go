package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"google.golang.org/protobuf/proto"

	"agent-repld/internal/dlog"
	"agent-repld/internal/promptorigin"
)

// A PROMPT DELIVERED ON ARRIVAL IS NEVER HELD (owner ruling, 2026-11-21).
// A daemon notice to an agent, and a prompt one workspace's agent sent
// another, go the moment they arrive (promptorigin.DeliveredOnArrival): into
// an idle session as its turn, into a running one as a join after the turn's
// current tool call. Before the ruling they were held prompts, drawn nowhere
// (a notice's origin draws no card), and one such hold once blocked a close.
//
// WHERE ONE GENUINELY CANNOT GO YET, IT WAITS UNDER A NAMED WAIT: one row of
// arrival_waits, separate from held_prompts so no hold machinery (the held
// section, a release, a fold, an edit, a close's check) can ever see it. The
// row names its reason (ArrivalWaitReason), and the reason names the one event
// that ends it. The row is durable, so a wait outlives a daemon restart, and
// it is deleted the instant the prompt is delivered.

// arrivalWaitsDDL adds arrival_waits.
//
// The fresh-file schema appends this same text (schema.go), so a migrated file
// and a created one cannot differ.
const arrivalWaitsDDL = `
CREATE TABLE arrival_waits (
  turn_id      TEXT PRIMARY KEY,
  workspace_id TEXT NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  said         BLOB NOT NULL,
  origin       TEXT NOT NULL,
  sent_by      TEXT,
  queued_at    INTEGER NOT NULL,
  reason       INTEGER NOT NULL,
  why          TEXT NOT NULL
);

CREATE INDEX arrival_waits_by_workspace ON arrival_waits(workspace_id);
`

// ArrivalWaitReason is why a prompt delivered on arrival cannot go yet, and so
// which event ends its wait. Its stored values are persisted and never reused.
type ArrivalWaitReason int

// The arrival wait reasons. Each names the event that ends it.
const (
	arrivalWaitUnspecified ArrivalWaitReason = iota
	// WaitSession: the workspace's session does not serve (no shim, no
	// started session, no watcher, a hibernation being revived, the shim
	// refusing the turn for having no session). Ended by the session coming
	// up (promptqueue's session-up release).
	WaitSession
	// WaitColdGate: the session is parked at its cold gate, which a prompt
	// never answers. Ended by the user's answer to the gate.
	WaitColdGate
	// WaitBounce: the workspace's shim is being replaced (a bounce's drain, a
	// restart lease). Ended by the replacement's session coming up.
	WaitBounce
	// WaitShutdown: the daemon is draining to exit (the drain lease). Ended
	// by the successor daemon's boot, which drives every stored wait.
	WaitShutdown
	// WaitMerge: a merge drives the workspace's session under its lease.
	// Ended by the lease's release.
	WaitMerge
	// WaitTurnEnd: a turn runs that takes no join (a context cut, a turn the
	// vendor started on its own, an ask blocking the vendor). Ended by that
	// turn's end.
	WaitTurnEnd
	// WaitVendor: the vendor refuses the session that is up (a usage limit, a
	// billing or authentication refusal, a retry). Ended by the vendor
	// serving the session again.
	WaitVendor
	// WaitShimCall: a shim call that opens no turn is in flight, and the
	// queue makes one call at a time. Ended by the call's settle.
	WaitShimCall
	arrivalWaitSentinel
)

// String names the reason in records.
func (r ArrivalWaitReason) String() string {
	switch r {
	case WaitSession:
		return "session"
	case WaitColdGate:
		return "cold_gate"
	case WaitBounce:
		return "bounce"
	case WaitShutdown:
		return "shutdown"
	case WaitMerge:
		return "merge"
	case WaitTurnEnd:
		return "turn_end"
	case WaitVendor:
		return "vendor"
	case WaitShimCall:
		return "shim_call"
	}
	return fmt.Sprintf("arrival_wait_reason(%d)", int(r))
}

// Valid reports whether the reason is a declared one.
func (r ArrivalWaitReason) Valid() bool {
	return r > arrivalWaitUnspecified && r < arrivalWaitSentinel
}

// ArrivalWait is one prompt delivered on arrival that waits, under its named
// reason, for the event that ends the wait.
type ArrivalWait struct {
	// Workspace is the workspace the prompt is addressed to.
	Workspace WorkspaceID
	// Turn is the turn minted for the prompt; it names the wait.
	Turn TurnID
	// Said is the prompt.
	Said *conversationv1.UserSaid
	// Origin is the prompt's origin, as conversation.v1.PromptOrigin spells it.
	Origin string
	// SentBy is the workspace whose agent sent it, set exactly when Origin is
	// PROMPT_ORIGIN_AGENT_SENT.
	SentBy *WorkspaceID
	// QueuedAt is when the prompt first arrived. A wait recorded again under
	// a new reason keeps it.
	QueuedAt time.Time
	// Reason is why it cannot go yet.
	Reason ArrivalWaitReason
	// Why is the reason in words, for the record.
	Why string
}

// ErrNotDeliveredOnArrival is a wait recorded for a prompt whose origin is
// held, not delivered on arrival.
var ErrNotDeliveredOnArrival = errors.New("wsm: only a prompt delivered on arrival waits under an arrival wait")

// ErrHeldOnArrival is a hold recorded for a prompt delivered on arrival, which
// is never held.
var ErrHeldOnArrival = errors.New("wsm: a prompt delivered on arrival is never held")

// validateArrivalWait refuses a wait the row must never carry.
func validateArrivalWait(w ArrivalWait) error {
	switch {
	case w.Said == nil:
		return errors.New("wsm: an arrival wait carries its prompt")
	case !w.Reason.Valid():
		return fmt.Errorf("wsm: undeclared arrival wait reason %d", int(w.Reason))
	case w.Why == "":
		return errors.New("wsm: an arrival wait says why it waits")
	case !promptorigin.DeliveredOnArrivalNamed(w.Origin):
		return fmt.Errorf("%w (origin %s)", ErrNotDeliveredOnArrival, w.Origin)
	}
	return promptorigin.CheckSenderNamed(w.Origin, w.SentBy != nil)
}

// PutArrivalWait records W, or records it again under its new reason. A wait
// recorded again keeps the place it first arrived in.
func (s *store) PutArrivalWait(ctx context.Context, w ArrivalWait) error {
	const op = "daemon.wsm.put_arrival_wait"
	fields := dlog.Context{"workspace": string(w.Workspace), "turn": string(w.Turn), "origin": w.Origin, "reason": w.Reason.String(), "why": w.Why}
	if err := validateArrivalWait(w); err != nil {
		s.log.Error(op, "refused an arrival wait the row must never carry", withError(fields, err))
		return err
	}
	said, err := proto.Marshal(w.Said)
	if err != nil {
		wrapped := fmt.Errorf("wsm: encode an arrival wait's prompt: %w", err)
		s.log.Error(op, "refused an arrival wait whose prompt does not encode", withError(fields, wrapped))
		return wrapped
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx,
			`INSERT INTO arrival_waits (turn_id, workspace_id, said, origin, sent_by, queued_at, reason, why)
			 VALUES (?, ?, ?, ?, ?, ?, ?, ?)
			 ON CONFLICT(turn_id) DO UPDATE SET reason = excluded.reason, why = excluded.why`,
			w.Turn, w.Workspace, said, w.Origin, sentByColumn(w.SentBy), nanos(w.QueuedAt), int(w.Reason), w.Why)
		return err
	})
}

// DeleteArrivalWait ends TURN's wait, and answers whether one stood.
func (s *store) DeleteArrivalWait(ctx context.Context, turn TurnID) (bool, error) {
	const op = "daemon.wsm.delete_arrival_wait"
	var stood bool
	err := s.write(ctx, op, dlog.Context{"turn": string(turn)}, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `DELETE FROM arrival_waits WHERE turn_id = ?`, turn)
		if err != nil {
			return err
		}
		stood, err = touchedRow(res)
		return err
	})
	return stood, err
}

// ArrivalWaits answers one workspace's waits, in the order they arrived.
func (s *store) ArrivalWaits(ctx context.Context, id WorkspaceID) ([]ArrivalWait, error) {
	return s.loadArrivalWaits(ctx, "daemon.wsm.arrival_waits", dlog.Context{"workspace": string(id)},
		`WHERE workspace_id = ?`, id)
}

// AllArrivalWaits answers every workspace's waits, in the order they arrived,
// for the boot's drive.
func (s *store) AllArrivalWaits(ctx context.Context) ([]ArrivalWait, error) {
	return s.loadArrivalWaits(ctx, "daemon.wsm.all_arrival_waits", nil, ``)
}

// arrivalWaitColumns is the one select list every wait read shares.
const arrivalWaitColumns = `turn_id, workspace_id, said, origin, sent_by, queued_at, reason, why`

func (s *store) loadArrivalWaits(ctx context.Context, op string, fields dlog.Context, where string, args ...any) ([]ArrivalWait, error) {
	var out []ArrivalWait
	err := s.read(ctx, op, fields, func(ctx context.Context) error {
		rows, err := s.q(ctx).QueryContext(ctx,
			`SELECT `+arrivalWaitColumns+` FROM arrival_waits `+where+` ORDER BY queued_at, turn_id`, args...)
		if err != nil {
			return err
		}
		defer rows.Close()
		for rows.Next() {
			w, err := scanArrivalWait(rows)
			if err != nil {
				return err
			}
			out = append(out, w)
		}
		return rows.Err()
	})
	if err != nil {
		return nil, err
	}
	return out, nil
}

// scanArrivalWait decodes one wait all-or-nothing: an unparseable prompt, an
// undeclared reason, or a sender that disagrees with the origin fails the read.
func scanArrivalWait(row interface{ Scan(...any) error }) (ArrivalWait, error) {
	var (
		w      ArrivalWait
		said   []byte
		sentBy sql.NullString
		queued int64
		reason int64
	)
	if err := row.Scan(&w.Turn, &w.Workspace, &said, &w.Origin, &sentBy, &queued, &reason, &w.Why); err != nil {
		return ArrivalWait{}, err
	}
	w.Said = &conversationv1.UserSaid{}
	if err := proto.Unmarshal(said, w.Said); err != nil {
		return ArrivalWait{}, &DecodeError{Table: "arrival_waits", Row: string(w.Turn), Field: "said", Err: err}
	}
	w.Reason = ArrivalWaitReason(reason)
	if !w.Reason.Valid() {
		return ArrivalWait{}, &DecodeError{Table: "arrival_waits", Row: string(w.Turn), Field: "reason", Err: fmt.Errorf("undeclared reason %d", reason)}
	}
	var err error
	if w.SentBy, err = decodeSentBy("arrival_waits", string(w.Turn), w.Origin, sentBy); err != nil {
		return ArrivalWait{}, err
	}
	w.QueuedAt = fromNanos(queued)
	return w, nil
}

// movedArrivalWhy is the reason a hold an earlier build left standing waits
// under once moved.
const movedArrivalWhy = "it was held by a build before prompts delivered on arrival were never held; it goes once the session serves"

// moveArrivalHolds is the layout-187 rewrite: every STANDING hold of a
// prompt delivered on arrival, which an earlier build held, becomes an arrival
// wait on the session (WaitSession), and its hold row is deleted, so no hold
// of one is left for the held machinery to see. The boot's drive delivers
// each once its session serves.
func moveArrivalHolds(ctx context.Context, tx *sql.Tx, log dlog.Logger) error {
	rows, err := tx.QueryContext(ctx, `SELECT turn_id, workspace_id, said, origin, sent_by, queued_at
		FROM held_prompts WHERE tombstone_kind IS NULL`)
	if err != nil {
		return err
	}
	type moving struct {
		turn, workspace, origin string
		said                    []byte
		sentBy                  sql.NullString
		queued                  int64
	}
	var move []moving
	for rows.Next() {
		var m moving
		if err := rows.Scan(&m.turn, &m.workspace, &m.said, &m.origin, &m.sentBy, &m.queued); err != nil {
			rows.Close()
			return err
		}
		if promptorigin.DeliveredOnArrivalNamed(m.origin) {
			move = append(move, m)
		}
	}
	if err := rows.Close(); err != nil {
		return err
	}
	for _, m := range move {
		if _, err := tx.ExecContext(ctx,
			`INSERT INTO arrival_waits (turn_id, workspace_id, said, origin, sent_by, queued_at, reason, why)
			 VALUES (?, ?, ?, ?, ?, ?, ?, ?)`,
			m.turn, m.workspace, m.said, m.origin, m.sentBy, m.queued, int(WaitSession), movedArrivalWhy); err != nil {
			return err
		}
		if _, err := tx.ExecContext(ctx, `DELETE FROM held_prompts WHERE turn_id = ?`, m.turn); err != nil {
			return err
		}
		log.Info("daemon.wsm.open", "moved a standing hold of a prompt delivered on arrival to a wait on its session", dlog.Context{
			"turn": m.turn, "workspace": m.workspace, "origin": m.origin,
		})
	}
	log.Info("daemon.wsm.open", "moved every standing hold of a prompt delivered on arrival to a wait on its session", dlog.Context{"moved": len(move)})
	return nil
}
