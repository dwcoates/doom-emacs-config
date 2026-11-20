package wsm

import (
	"context"
	"database/sql"
	"errors"
	"strings"
	"time"

	"agent-repld/internal/dlog"
)

// THE RECORDS THE DEPLOY CHECKS LEAVE OUT (owner ruling, 2026-11-19). A
// deploy check reads every warning and error since the last one, and some of
// those are no problem a deploy could cause: a fault of something outside
// agent-repl that passes on its own, or a one-off the user's own action
// causes. Each such KIND of record is kept here, so the daemon drops its
// records before it judges whether a window holds anything and before it
// writes the window's digest. A window that holds only such records runs no
// mediator.
//
// A KIND IS UNCONDITIONAL. It matches every record of its operation, or
// every one whose message holds a text. A record that is ignorable only in
// some circumstance is not a kind: it is the mediator's judgement, which it
// keeps in its notes file (docs/DEPLOY-MEDIATOR-NOTES.md).
//
// The table is the machine's, not a repository's: the checks of every
// repository read the same logs.

// ignoredRecordsDDL is the layout-171 addition: the kinds of record the
// deploy checks leave out, seeded with the kinds the owner ruled ignorable
// before the table existed: the spell checker's no-reply, GitHub's 404 for a
// gone parent comment, and every failure of the vendor query (owner ruling,
// 2026-11-19: a vendor or a network problem, never agent-repl's).
//
// IT IS AN ADDITIVE STEP: the build reads the table from its first deploy
// check, which runs while an older daemon may still serve. Marked a contract
// (as it first shipped, to follow the contract step 169), it was created
// only once that daemon had ended, and the check before then could not
// compose its brief (2026-11-19).
const ignoredRecordsDDL = `
CREATE TABLE deploy_ignored_records (
  operation        TEXT NOT NULL CHECK (operation <> ''),
  message_contains TEXT NOT NULL DEFAULT '',
  why              TEXT NOT NULL CHECK (why <> ''),
  added_at         INTEGER NOT NULL CHECK (added_at > 0),
  PRIMARY KEY (operation, message_contains)
);
INSERT INTO deploy_ignored_records (operation, message_contains, why, added_at) VALUES
  ('agent-repl.elisp-spell-no-reply-ws-s-buffer-s-word-s-process-s-pid-s-cause-s-outcome-reset', '',
   'a one-off user-driven error: the spell checker gave no reply for a word', 1795132800000000000),
  ('daemon.ghclient.post', 'Parent comment not found',
   'GitHub''s own 404 for a comment whose parent is gone; it passes on its own', 1795132800000000000),
  ('shim.engine.session', 'the vendor query could not be started',
   'the vendor or the network: every failure of the vendor query is one or the other', 1795132800000000000),
  ('shim.engine.session', 'the vendor query is gone',
   'the vendor or the network: every failure of the vendor query is one or the other', 1795132800000000000);
`

// IgnoredRecordKind is one kind of log record the deploy checks leave out.
type IgnoredRecordKind struct {
	// Operation is the record's operation, exactly. Required.
	Operation string
	// MessageContains is text the record's message must hold, or empty for
	// every record of the operation.
	MessageContains string
	// Why says why a record of this kind is no problem a deploy caused.
	Why string
	// AddedAt is when the kind was recorded.
	AddedAt time.Time
}

// Matches reports whether a record of OPERATION saying MESSAGE is of this
// kind.
func (k IgnoredRecordKind) Matches(operation, message string) bool {
	return operation == k.Operation && strings.Contains(message, k.MessageContains)
}

// IgnoredRecordKinds answers every kind of record the deploy checks leave
// out, by operation and then by the text its message must hold.
func (s *store) IgnoredRecordKinds(ctx context.Context) ([]IgnoredRecordKind, error) {
	const op = "daemon.wsm.ignored_record_kinds"
	var out []IgnoredRecordKind
	err := s.read(ctx, op, dlog.Context{}, func(ctx context.Context) error {
		out = nil
		rows, err := s.q(ctx).QueryContext(ctx,
			`SELECT operation, message_contains, why, added_at FROM deploy_ignored_records ORDER BY operation, message_contains`)
		if err != nil {
			return err
		}
		defer rows.Close()
		for rows.Next() {
			var (
				k     IgnoredRecordKind
				added int64
			)
			if err := rows.Scan(&k.Operation, &k.MessageContains, &k.Why, &added); err != nil {
				return err
			}
			k.AddedAt = fromNanos(added)
			out = append(out, k)
		}
		return rows.Err()
	})
	return out, err
}

// IgnoreRecordKind records K as a kind the deploy checks leave out. A kind
// already recorded keeps its place and takes K's reason.
func (s *store) IgnoreRecordKind(ctx context.Context, k IgnoredRecordKind) error {
	const op = "daemon.wsm.ignore_record_kind"
	fields := dlog.Context{"operation": k.Operation, "message_contains": k.MessageContains}
	if k.Operation == "" || strings.TrimSpace(k.Why) == "" || k.AddedAt.IsZero() {
		err := errors.New("wsm: an ignored record kind names its operation, why it is ignored and when it was recorded")
		s.log.Error(op, "refused an ignored record kind that names no operation, reason or time", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx,
			`INSERT INTO deploy_ignored_records (operation, message_contains, why, added_at) VALUES (?, ?, ?, ?)
			 ON CONFLICT(operation, message_contains) DO UPDATE SET why = excluded.why`,
			k.Operation, k.MessageContains, k.Why, nanos(k.AddedAt))
		return err
	})
}

// UnignoreRecordKind withdraws the kind of OPERATION whose message holds
// MESSAGECONTAINS, answering whether one was recorded.
func (s *store) UnignoreRecordKind(ctx context.Context, operation, messageContains string) (bool, error) {
	const op = "daemon.wsm.unignore_record_kind"
	removed := false
	err := s.write(ctx, op, dlog.Context{"operation": operation, "message_contains": messageContains}, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `DELETE FROM deploy_ignored_records WHERE operation = ? AND message_contains = ?`, operation, messageContains)
		if err != nil {
			return err
		}
		removed, err = touchedRow(res)
		return err
	})
	return removed, err
}
