package wsm

import (
	"context"
	"errors"
	"fmt"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
)

// TurnStart is one recorded turn's start, with what decides whether it was an
// agent turn: its origin, and its text when that text may be a slash command.
// It is what a read over every workspace's turn history (FindWorkspaces's turn
// window) takes, without the full text of every prompt ever sent.
type TurnStart struct {
	// Workspace is the workspace the turn ran in.
	Workspace WorkspaceID
	// StartedAt is when the turn was opened.
	StartedAt time.Time
	// Origin is the turn's prompt origin, decoded.
	Origin conversationv1.PromptOrigin
	// CommandText is the turn's full text when it may begin with a slash
	// command, and empty when it certainly does not.
	CommandText string
}

// turnStartColumns select a turn's start and origin, and its text only when
// the text may begin with a slash command.
//
// THE TEXT IS WITHHELD ONLY WHEN IT PROVABLY BEGINS WITH NO COMMAND, so a
// reader parsing CommandText (sessioncommand.Parse, which trims with Go's
// unicode.IsSpace) answers exactly what it would over the whole text.
// SQLite's ltrim strips the ASCII whitespace named here; every other character
// unicode.IsSpace trims is outside ASCII. So a text whose first character
// after that ltrim is ASCII and not `/` begins with no command, and any other
// text is answered whole. This is an optimization: it keeps the read from
// carrying every prompt ever sent.
const turnStartColumns = `id, workspace_id, started_at, origin,
  CASE WHEN substr(ltrim(text, ' ' || char(9, 10, 11, 12, 13)), 1, 1) = '/'
         OR unicode(ltrim(text, ' ' || char(9, 10, 11, 12, 13))) > 127
       THEN text ELSE '' END`

// TurnStarts loads every recorded turn's start, ordered by workspace and then
// start, all-or-nothing: a row whose origin the vocabulary does not name fails
// the whole read as a DecodeError rather than being skipped, because every
// submit site names a non-UNSPECIFIED origin.
func (s *store) TurnStarts(ctx context.Context) ([]TurnStart, error) {
	var out []TurnStart
	err := s.read(ctx, "daemon.wsm.turn_starts", dlog.Context{}, func(ctx context.Context) error {
		loaded, err := listRows(ctx, s.q(ctx), `SELECT `+turnStartColumns+` FROM turns ORDER BY workspace_id, started_at, id`, scanTurnStart)
		out = loaded
		return err
	})
	if err != nil {
		return nil, err
	}
	return out, nil
}

// scanTurnStart decodes one turnStartColumns row.
func scanTurnStart(row interface{ Scan(...any) error }) (TurnStart, error) {
	var (
		start   TurnStart
		id      string
		started int64
		origin  string
	)
	if err := row.Scan(&id, &start.Workspace, &started, &origin, &start.CommandText); err != nil {
		return TurnStart{}, err
	}
	decoded, err := decodeTurnOrigin(origin)
	if err != nil {
		return TurnStart{}, &DecodeError{Table: "turns", Row: id, Field: "origin", Err: err}
	}
	start.StartedAt = fromNanos(started)
	start.Origin = decoded
	return start, nil
}

// errUnnamedTurnOrigin is a stored turn origin conversation.v1.PromptOrigin
// does not name, or names as UNSPECIFIED.
var errUnnamedTurnOrigin = errors.New("a turn's origin is a named conversation.v1.PromptOrigin")

// decodeTurnOrigin reads a stored turn origin, refusing one the vocabulary
// does not name and PROMPT_ORIGIN_UNSPECIFIED, which no submit site sends.
func decodeTurnOrigin(origin string) (conversationv1.PromptOrigin, error) {
	value, ok := conversationv1.PromptOrigin_value[origin]
	if !ok || conversationv1.PromptOrigin(value) == conversationv1.PromptOrigin_PROMPT_ORIGIN_UNSPECIFIED {
		return 0, fmt.Errorf("%w: %q", errUnnamedTurnOrigin, origin)
	}
	return conversationv1.PromptOrigin(value), nil
}
