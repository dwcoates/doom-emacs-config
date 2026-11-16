package wsm

import (
	"context"
	"database/sql"
	"fmt"

	"agent-repld/internal/dlog"
)

// composerMentionsDDL is the layout-151 addition: the commands and workspaces
// each workspace's own final responses mentioned, with the instant of the
// latest mention, so the composer's completions are ordered by them from a
// record that outlives the newest page a boot replays
// (internal/composermentions). IT IS ADDITIVE: one new table the previous
// build never names.
//
// One row per (workspace, kind, name), and the instant only ever moves
// forward (PutComposerMentions), so a replay of an older response can never
// make its names newer.
const composerMentionsDDL = `
CREATE TABLE composer_mentions (
  workspace_id    TEXT NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  kind            TEXT NOT NULL CHECK (kind IN ('command', 'workspace')),
  name            TEXT NOT NULL CHECK (name <> ''),
  mentioned_at_ms INTEGER NOT NULL CHECK (mentioned_at_ms > 0),
  PRIMARY KEY (workspace_id, kind, name)
);
`

// ComposerMentionKind is what a composer mention names.
type ComposerMentionKind string

// The two kinds, spelled as the table's CHECK spells them.
const (
	// ComposerMentionCommand is a slash command, by name without the slash.
	ComposerMentionCommand ComposerMentionKind = "command"
	// ComposerMentionWorkspace is a workspace, by name.
	ComposerMentionWorkspace ComposerMentionKind = "workspace"
)

// ComposerMention is one name a workspace's final responses mentioned, and
// the instant (epoch ms) the latest response mentioning it settled.
type ComposerMention struct {
	Workspace     WorkspaceID
	Kind          ComposerMentionKind
	Name          string
	MentionedAtMs int64
}

// PutComposerMentions records MENTIONS, each moving its row's instant forward
// to its own when that is later and leaving it as it stands otherwise. A
// mention with an unknown kind, an empty name or no instant is a bug in the
// caller, refused loudly before anything is written.
func (s *store) PutComposerMentions(ctx context.Context, mentions []ComposerMention) error {
	const op = "daemon.wsm.put_composer_mentions"
	fields := dlog.Context{"mentions": len(mentions)}
	for _, m := range mentions {
		if (m.Kind != ComposerMentionCommand && m.Kind != ComposerMentionWorkspace) || m.Name == "" || m.MentionedAtMs <= 0 {
			err := fmt.Errorf("wsm: a composer mention has a known kind, a name and an instant (%+v)", m)
			s.log.Error(op, "refused a malformed composer mention", withError(fields, err))
			return err
		}
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		for _, m := range mentions {
			if _, err := tx.ExecContext(ctx,
				`INSERT INTO composer_mentions (workspace_id, kind, name, mentioned_at_ms) VALUES (?, ?, ?, ?)
				 ON CONFLICT(workspace_id, kind, name) DO UPDATE
				 SET mentioned_at_ms = max(mentioned_at_ms, excluded.mentioned_at_ms)`,
				m.Workspace, string(m.Kind), m.Name, m.MentionedAtMs); err != nil {
				return err
			}
		}
		return nil
	})
}

// ComposerMentions loads every workspace's recorded mentions, in no
// particular order. A stored kind the table's CHECK should have refused is a
// DecodeError.
func (s *store) ComposerMentions(ctx context.Context) ([]ComposerMention, error) {
	var out []ComposerMention
	err := s.read(ctx, "daemon.wsm.composer_mentions", dlog.Context{}, func(ctx context.Context) error {
		out = nil
		rows, err := s.q(ctx).QueryContext(ctx,
			`SELECT workspace_id, kind, name, mentioned_at_ms FROM composer_mentions`)
		if err != nil {
			return err
		}
		defer rows.Close()
		for rows.Next() {
			var m ComposerMention
			var kind string
			if err := rows.Scan(&m.Workspace, &kind, &m.Name, &m.MentionedAtMs); err != nil {
				return err
			}
			m.Kind = ComposerMentionKind(kind)
			if m.Kind != ComposerMentionCommand && m.Kind != ComposerMentionWorkspace {
				return &DecodeError{Table: "composer_mentions", Row: string(m.Workspace) + "/" + m.Name, Field: "kind", Err: fmt.Errorf("unknown kind %q", kind)}
			}
			out = append(out, m)
		}
		return rows.Err()
	})
	return out, err
}
