package wsm

import (
	"context"
	"time"

	"agent-repld/internal/dlog"
)

// ConversationPrompt is one prompt of a workspace's conversation as a reader of
// its PROMPTS sees it: its own turns, and the turns it inherited through its
// fork lineage.
type ConversationPrompt struct {
	// Workspace is the workspace that recorded the turn: the reader's own, or
	// the ancestor it was inherited from.
	Workspace WorkspaceID
	// Turn is the turn's identity, as the recording workspace minted it.
	Turn TurnID
	// Ordinal is the prompt's place in the answer, oldest first, from zero.
	Ordinal int64
	// Text is the prompt's full text.
	Text string
	// Origin is the prompt's origin, in the spelling `turns` carries.
	Origin string
	// StartedAt is when the turn opened in the workspace that recorded it.
	StartedAt time.Time
}

// RecentConversationPrompts answers the most recent `limit` prompts of a
// workspace's conversation, oldest first, with ordinals from zero: its own
// turns, and, when those fall short, the turns it inherited — each ancestor's
// turns that started at or before the instant its fork's inherited rows are
// bounded by, and that were not open at that fork (Lineage).
//
// It reads only the tail of each level, with a SQL LIMIT, so a long
// conversation costs one bounded query per generation. Its one reader is the
// fork-naming digest, which never quotes more than `limit` requests.
func (s *store) RecentConversationPrompts(ctx context.Context, id WorkspaceID, limit int) ([]ConversationPrompt, error) {
	if limit <= 0 {
		return nil, nil
	}
	lineage, err := s.Lineage(ctx, id)
	if err != nil {
		return nil, err
	}
	// NEWEST FIRST, level by level: the workspace's own turns, then each
	// ancestor's within the bound its child's fork set on it.
	var newestFirst []Turn
	level := recentLevel{workspace: id}
	for i := 0; ; i++ {
		got, err := s.recentTurns(ctx, level, limit-len(newestFirst))
		if err != nil {
			return nil, err
		}
		newestFirst = append(newestFirst, got...)
		if len(newestFirst) >= limit || i == len(lineage) {
			break
		}
		link := lineage[i]
		level = recentLevel{workspace: link.Parent, throughMs: link.ThroughMs, excluded: link.Uninherited}
	}
	out := make([]ConversationPrompt, 0, len(newestFirst))
	for i := len(newestFirst) - 1; i >= 0; i-- {
		t := newestFirst[i]
		out = append(out, ConversationPrompt{
			Workspace: t.Workspace,
			Turn:      t.ID,
			Ordinal:   int64(len(out)),
			Text:      t.Text,
			Origin:    t.Origin,
			StartedAt: t.StartedAt,
		})
	}
	return out, nil
}

// recentLevel is one generation a RecentConversationPrompts walk reads.
type recentLevel struct {
	workspace WorkspaceID
	// throughMs bounds an inherited level's turns by their start; 0 for the
	// reader's own level, which is unbounded.
	throughMs int64
	// excluded are an inherited level's turns that were open at the fork.
	excluded []TurnID
}

// recentTurns reads up to `limit` of one level's turns, newest first.
func (s *store) recentTurns(ctx context.Context, level recentLevel, limit int) ([]Turn, error) {
	var out []Turn
	fields := dlog.Context{"workspace": string(level.workspace), "limit": limit, "through_ms": level.throughMs}
	err := s.read(ctx, "daemon.wsm.recent_conversation_prompts", fields, func(ctx context.Context) error {
		skip := make(map[TurnID]bool, len(level.excluded))
		for _, turn := range level.excluded {
			skip[turn] = true
		}
		query := `SELECT ` + turnColumns + ` FROM turns WHERE workspace_id = ? ORDER BY started_at DESC, id DESC`
		args := []any{level.workspace}
		if level.throughMs > 0 {
			query = `SELECT ` + turnColumns + ` FROM turns WHERE workspace_id = ? AND started_at <= ? ORDER BY started_at DESC, id DESC`
			args = append(args, nanos(time.UnixMilli(level.throughMs)))
		}
		rows, err := s.q(ctx).QueryContext(ctx, query, args...)
		if err != nil {
			return err
		}
		defer rows.Close()
		for rows.Next() && len(out) < limit {
			t, err := scanTurn(rows)
			if err != nil {
				return err
			}
			if skip[t.ID] {
				continue
			}
			out = append(out, t)
		}
		return rows.Err()
	})
	return out, err
}
