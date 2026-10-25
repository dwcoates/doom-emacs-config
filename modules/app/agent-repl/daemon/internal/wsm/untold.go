package wsm

import (
	"context"
	"database/sql"
	"encoding/json"
	"errors"
	"fmt"

	"agent-repld/internal/dlog"
)

// untoldTable is one subscription notifier's durable untold notices: a table
// keyed by the subscriber and the notice's subject, holding the turn the
// notice was submitted as and the JSON array of every change it says. Each
// notifier's subject is its own column (merge_queue_untold_notices.repo_key,
// workspace_untold_notices.followed); the read and the write are one each, so
// the two tables cannot be kept two ways.
type untoldTable struct {
	table   string
	subject string
	// noun names the notice in the table's records ("untold merge notice").
	noun string
}

// The untold tables.
var (
	mergeQueueUntold = untoldTable{table: "merge_queue_untold_notices", subject: "repo_key", noun: "untold merge notice"}
	workspaceUntold  = untoldTable{table: "workspace_untold_notices", subject: "followed", noun: "untold workspace notice"}
)

// readUntold answers SUBSCRIBER's untold notice about SUBJECT in T: its turn
// and sentences, and whether one is recorded. OP names the read.
func (s *store) readUntold(ctx context.Context, t untoldTable, op string, subscriber WorkspaceID, subject string) (TurnID, []string, bool, error) {
	fields := dlog.Context{"subscriber": string(subscriber), t.subject: subject}
	var (
		turn  TurnID
		said  []string
		found bool
	)
	err := s.read(ctx, op, fields, func(ctx context.Context) error {
		var raw, sentences string
		err := s.q(ctx).QueryRowContext(ctx,
			`SELECT turn_id, sentences FROM `+t.table+` WHERE subscriber = ? AND `+t.subject+` = ?`,
			subscriber, subject).Scan(&raw, &sentences)
		if errors.Is(err, sql.ErrNoRows) {
			return nil
		}
		if err != nil {
			return err
		}
		if err := json.Unmarshal([]byte(sentences), &said); err != nil {
			return fmt.Errorf("wsm: the %s of %s about %q holds unreadable sentences: %w", t.noun, subscriber, subject, err)
		}
		turn, found = TurnID(raw), true
		return nil
	})
	return turn, said, found, err
}

// recordUntold records SUBSCRIBER's untold notice about SUBJECT in T,
// replacing the one recorded before. A notice that says no change is refused:
// a notice is always of at least one change. OP names the write.
func (s *store) recordUntold(ctx context.Context, t untoldTable, op string, subscriber WorkspaceID, subject string, turn TurnID, sentences []string) error {
	fields := dlog.Context{"subscriber": string(subscriber), t.subject: subject, "turn": string(turn), "sentences": len(sentences)}
	if len(sentences) == 0 {
		err := fmt.Errorf("wsm: the %s of %s about %q says no change", t.noun, subscriber, subject)
		s.log.Error(op, "refused an "+t.noun+" that says no change", withError(fields, err))
		return err
	}
	said, err := json.Marshal(sentences)
	if err != nil {
		s.log.Error(op, "refused an "+t.noun+" whose sentences cannot be encoded", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx,
			`INSERT INTO `+t.table+` (subscriber, `+t.subject+`, turn_id, sentences) VALUES (?, ?, ?, ?)
			 ON CONFLICT(subscriber, `+t.subject+`) DO UPDATE SET turn_id = excluded.turn_id, sentences = excluded.sentences`,
			subscriber, subject, turn, string(said))
		return err
	})
}
