package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
)

// THE FORK'S PORTED CONVERSATION. A fork ports the WHOLE conversation under
// the child's identities, and the prompt half of it is the daemon's own: the
// vendor transcript carries what the agent SAID, while what the person ASKED
// reaches a feed as a row the daemon draws. Porting only the transcript is
// what left a forked feed showing an answer to a question it did not show.
//
// The ported rows live in their own table rather than in `turns` because a
// turn row is a LIVE obligation — `OpenTurns` drives delivery, and
// `AllDisplacedTurns` drives the boot recovery — and the parent's turns are
// neither of those things in the child. They are history, and history is all
// they are: text, origin, and the place they sit in the conversation.

// PortedPrompt is one prompt the child inherited from its parent, under the
// child's own turn identity.
type PortedPrompt struct {
	// Workspace is the CHILD the row was ported into.
	Workspace WorkspaceID
	// Turn is the turn identity this row carries IN THE CHILD — the parent's
	// own id re-minted under the fork's mapping, so it agrees with the ported
	// transcript rather than naming the parent's live turn.
	Turn TurnID
	// Ordinal is the row's place in the ported conversation, oldest first. It
	// is the ordering key both the port and the replay read: the parent's own
	// clock is the parent's, and two ports of one conversation must not be
	// able to disagree about what came first.
	Ordinal int64
	// Text is the prompt's full text, as the parent recorded it.
	Text string
	// Origin is the prompt's origin, in the same spelling `turns` carries.
	Origin string
	// StartedAt is when the prompt's turn opened in the PARENT. It is carried
	// for the record; ordering is Ordinal's job.
	StartedAt time.Time
}

// scanPortedPrompt decodes one ported row.
func scanPortedPrompt(row interface{ Scan(...any) error }) (PortedPrompt, error) {
	var (
		p       PortedPrompt
		started int64
	)
	if err := row.Scan(&p.Workspace, &p.Turn, &p.Ordinal, &p.Text, &p.Origin, &started); err != nil {
		return PortedPrompt{}, err
	}
	p.StartedAt = fromNanos(started)
	return p, nil
}

// portedPromptColumns is the one select list every ported-prompt read shares.
const portedPromptColumns = `workspace_id, turn_id, ordinal, text, origin, started_at`

// PutPortedPrompts writes a child's whole ported conversation in ONE
// transaction. All or nothing: half a ported conversation is a feed that shows
// some of the parent's questions and not others, which is the defect this
// exists to close rather than a lesser version of it.
func (s *store) PutPortedPrompts(ctx context.Context, id WorkspaceID, rows []PortedPrompt) error {
	const op = "daemon.wsm.put_ported_prompts"
	fields := dlog.Context{"workspace": string(id), "rows": len(rows)}
	for _, row := range rows {
		if row.Turn == "" {
			err := errors.New("wsm: a ported prompt requires a turn id")
			s.log.Error(op, "refused a ported prompt with no turn id", withError(fields, err))
			return err
		}
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		for _, row := range rows {
			if _, err := tx.ExecContext(ctx,
				`INSERT INTO ported_prompts (workspace_id, turn_id, ordinal, text, origin, started_at)
				 VALUES (?, ?, ?, ?, ?, ?)
				 ON CONFLICT(workspace_id, turn_id) DO UPDATE SET
				   ordinal = excluded.ordinal, text = excluded.text,
				   origin = excluded.origin, started_at = excluded.started_at`,
				id, row.Turn, row.Ordinal, row.Text, row.Origin, nanos(row.StartedAt)); err != nil {
				return err
			}
		}
		return nil
	})
}

// PortedPrompts loads one workspace's ported conversation, oldest first,
// all-or-nothing.
func (s *store) PortedPrompts(ctx context.Context, id WorkspaceID) ([]PortedPrompt, error) {
	var out []PortedPrompt
	err := s.read(ctx, "daemon.wsm.ported_prompts", dlog.Context{"workspace": string(id)}, func(ctx context.Context) error {
		rows, err := s.db().QueryContext(ctx,
			`SELECT `+portedPromptColumns+` FROM ported_prompts WHERE workspace_id = ? ORDER BY ordinal, turn_id`, id)
		if err != nil {
			return err
		}
		defer rows.Close()
		var loaded []PortedPrompt
		for rows.Next() {
			p, err := scanPortedPrompt(rows)
			if err != nil {
				return err
			}
			loaded = append(loaded, p)
		}
		if err := rows.Err(); err != nil {
			return err
		}
		out = loaded
		return nil
	})
	if err != nil {
		return nil, err
	}
	return out, nil
}

// ConversationPrompts is what a FORK OF THIS WORKSPACE inherits: everything
// this workspace itself inherited, followed by every prompt of its own, in one
// order with contiguous ordinals from zero.
//
// THE TWO HALVES TRAVEL TOGETHER because a fork of a fork must carry the
// grandparent's questions too. Reading only `turns` would truncate the
// conversation at each generation, which is the same hole one level up.
func (s *store) ConversationPrompts(ctx context.Context, id WorkspaceID) ([]PortedPrompt, error) {
	inherited, err := s.PortedPrompts(ctx, id)
	if err != nil {
		return nil, err
	}
	var own []Turn
	err = s.read(ctx, "daemon.wsm.conversation_prompts", dlog.Context{"workspace": string(id)}, func(ctx context.Context) error {
		rows, err := s.db().QueryContext(ctx,
			`SELECT `+turnColumns+` FROM turns WHERE workspace_id = ? ORDER BY started_at, id`, id)
		if err != nil {
			return err
		}
		defer rows.Close()
		for rows.Next() {
			t, err := scanTurn(rows)
			if err != nil {
				return err
			}
			own = append(own, t)
		}
		return rows.Err()
	})
	if err != nil {
		return nil, err
	}

	out := make([]PortedPrompt, 0, len(inherited)+len(own))
	for _, row := range inherited {
		row.Ordinal = int64(len(out))
		out = append(out, row)
	}
	for _, t := range own {
		out = append(out, turnAsPortedPrompt(t, int64(len(out))))
	}
	return out, nil
}

// turnAsPortedPrompt is the one conversion from a workspace's own turn to the
// ConversationPrompts shape, shared by ConversationPrompts and
// RecentConversationPrompts so the two readers cannot drift into two notions
// of what a turn looks like ported.
func turnAsPortedPrompt(t Turn, ordinal int64) PortedPrompt {
	return PortedPrompt{
		Workspace: t.Workspace,
		Turn:      t.ID,
		Ordinal:   ordinal,
		Text:      t.Text,
		Origin:    t.Origin,
		StartedAt: t.StartedAt,
	}
}

// RecentConversationPrompts is a BOUNDED ConversationPrompts, for the one
// reader that only ever needs the newest rows: the fork-naming digest, which
// never quotes more than `limit` requests (titlesynth.MaxPrompts). It answers
// the same rows ConversationPrompts would, truncated to the most recent
// `limit`, oldest first, with ordinals renumbered from zero — but it reads
// only that tail of each table, with a SQL LIMIT on each query, rather than
// ConversationPrompts's whole-history load. A conversation with thousands of
// turns therefore costs one bounded query per table to name a fork from,
// not the whole history.
//
// It changes nothing about what a fork INHERITS: PutPortedPrompts writes, and
// ConversationPrompts is what forkconversation.go reads to build, the child's
// FULL ported copy. This helper has no other caller.
func (s *store) RecentConversationPrompts(ctx context.Context, id WorkspaceID, limit int) ([]PortedPrompt, error) {
	if limit <= 0 {
		return nil, nil
	}
	fields := dlog.Context{"workspace": string(id), "limit": limit}

	var ownTail []Turn
	err := s.read(ctx, "daemon.wsm.recent_conversation_prompts_own", fields, func(ctx context.Context) error {
		rows, err := s.db().QueryContext(ctx,
			`SELECT `+turnColumns+` FROM turns WHERE workspace_id = ? ORDER BY started_at DESC, id DESC LIMIT ?`, id, limit)
		if err != nil {
			return err
		}
		defer rows.Close()
		for rows.Next() {
			t, err := scanTurn(rows)
			if err != nil {
				return err
			}
			ownTail = append(ownTail, t)
		}
		return rows.Err()
	})
	if err != nil {
		return nil, err
	}
	reverseTurns(ownTail)

	var inheritedTail []PortedPrompt
	if remaining := limit - len(ownTail); remaining > 0 {
		err = s.read(ctx, "daemon.wsm.recent_conversation_prompts_inherited", fields, func(ctx context.Context) error {
			rows, err := s.db().QueryContext(ctx,
				`SELECT `+portedPromptColumns+` FROM ported_prompts WHERE workspace_id = ? ORDER BY ordinal DESC, turn_id DESC LIMIT ?`, id, remaining)
			if err != nil {
				return err
			}
			defer rows.Close()
			for rows.Next() {
				p, err := scanPortedPrompt(rows)
				if err != nil {
					return err
				}
				inheritedTail = append(inheritedTail, p)
			}
			return rows.Err()
		})
		if err != nil {
			return nil, err
		}
		reversePortedPrompts(inheritedTail)
	}

	out := make([]PortedPrompt, 0, len(inheritedTail)+len(ownTail))
	for _, row := range inheritedTail {
		row.Ordinal = int64(len(out))
		out = append(out, row)
	}
	for _, t := range ownTail {
		out = append(out, turnAsPortedPrompt(t, int64(len(out))))
	}
	return out, nil
}

// reverseTurns reverses a slice of turns in place, turning a DESC-ordered
// query result back into the oldest-first order every ConversationPrompts
// reader expects.
func reverseTurns(rows []Turn) {
	for i, j := 0, len(rows)-1; i < j; i, j = i+1, j-1 {
		rows[i], rows[j] = rows[j], rows[i]
	}
}

// reversePortedPrompts reverses a slice of ported prompts in place, the same
// way reverseTurns does for turns.
func reversePortedPrompts(rows []PortedPrompt) {
	for i, j := 0, len(rows)-1; i < j; i, j = i+1, j-1 {
		rows[i], rows[j] = rows[j], rows[i]
	}
}

// RemintPortedPrompts re-mints a conversation for one child: every turn id is
// carried through remint, the workspace becomes the child's, and the ordinals
// are re-numbered from zero so the child's copy is a whole conversation in its
// own right.
//
// It is a pure function so the fork's mapping — the SAME one the transcript is
// ported under — decides the ids and this package never mints one.
func RemintPortedPrompts(child WorkspaceID, rows []PortedPrompt, remint func(string) string) ([]PortedPrompt, error) {
	if remint == nil {
		return nil, errors.New("wsm: re-minting a ported conversation requires a mapping")
	}
	out := make([]PortedPrompt, 0, len(rows))
	for i, row := range rows {
		minted := remint(string(row.Turn))
		if minted == "" {
			return nil, fmt.Errorf("wsm: the fork mapping answered no turn id for %q", row.Turn)
		}
		out = append(out, PortedPrompt{
			Workspace: child,
			Turn:      ids.TurnID(minted),
			Ordinal:   int64(i),
			Text:      row.Text,
			Origin:    row.Origin,
			StartedAt: row.StartedAt,
		})
	}
	return out, nil
}
