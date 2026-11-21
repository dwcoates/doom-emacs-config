package wsm

import (
	"context"
	"database/sql"
	"fmt"
	"time"

	"agent-repld/internal/dlog"
)

// userAttentionDDL is the layout-186 addition: when the user last gave a
// workspace a prompt, and when they last opened a fold themselves. The
// deploy's fold sweep reads both to leave what the user is working in
// unfolded (internal/foldsweep). The fresh-file schema appends this same
// text (schema.go), so a migrated file and a created one cannot differ.
//
// IT IS ADDITIVE: a new table the previous build never names. A row keys a
// kind and a target id, not a foreign key: a stamp of a target since deleted
// is inert, since nothing asks for that id again.
const userAttentionDDL = `
CREATE TABLE user_attention (
  kind   TEXT NOT NULL CHECK (kind IN ('prompt', 'opened_repository', 'opened_subsection', 'opened_children')),
  target TEXT NOT NULL,
  at     INTEGER NOT NULL,
  PRIMARY KEY (kind, target)
);
`

// AttentionKind is what a user_attention stamp records.
type AttentionKind string

// The kinds of stamp. The target of a prompt is a workspace id, of an opened
// fold the id of the repository, subsection or workspace whose fold it is.
const (
	// AttentionPrompt is the user's last prompt to a workspace, held or sent.
	AttentionPrompt AttentionKind = "prompt"
	// AttentionOpenedRepository is the user's last explicit open of a
	// repository's roster section.
	AttentionOpenedRepository AttentionKind = "opened_repository"
	// AttentionOpenedSubsection is the user's last explicit open of a
	// subsection.
	AttentionOpenedSubsection AttentionKind = "opened_subsection"
	// AttentionOpenedChildren is the user's last explicit open of a
	// workspace's children.
	AttentionOpenedChildren AttentionKind = "opened_children"
)

// UserAttention is one stamp.
type UserAttention struct {
	Kind   AttentionKind
	Target string
	At     time.Time
}

// StampUserAttention records that the user's attention of KIND fell on TARGET
// at AT. A stamp only ever moves forward: an older instant leaves a later one
// standing.
func (s *store) StampUserAttention(ctx context.Context, kind AttentionKind, target string, at time.Time) error {
	return s.write(ctx, "daemon.wsm.stamp_user_attention", dlog.Context{"kind": string(kind), "target": target, "at": at},
		func(ctx context.Context, tx *sql.Tx) error {
			_, err := tx.ExecContext(ctx,
				`INSERT INTO user_attention (kind, target, at) VALUES (?, ?, ?)
				 ON CONFLICT (kind, target) DO UPDATE SET at = MAX(at, excluded.at)`,
				string(kind), target, nanos(at))
			return err
		})
}

// ListUserAttention loads every stamp, all-or-nothing.
func (s *store) ListUserAttention(ctx context.Context) ([]UserAttention, error) {
	var out []UserAttention
	err := s.read(ctx, "daemon.wsm.list_user_attention", dlog.Context{}, func(ctx context.Context) error {
		loaded, err := listRows(ctx, s.q(ctx), `SELECT kind, target, at FROM user_attention ORDER BY kind, target`,
			func(row interface{ Scan(...any) error }) (UserAttention, error) {
				var stamp UserAttention
				var kind string
				var at int64
				if err := row.Scan(&kind, &stamp.Target, &at); err != nil {
					return UserAttention{}, err
				}
				stamp.Kind, stamp.At = AttentionKind(kind), fromNanos(at)
				return stamp, nil
			})
		if err != nil {
			return fmt.Errorf("wsm: list user attention: %w", err)
		}
		out = loaded
		return nil
	})
	if err != nil {
		return nil, err
	}
	return out, nil
}
