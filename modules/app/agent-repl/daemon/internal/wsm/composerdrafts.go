package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"

	"agent-repld/internal/dlog"
)

// A COMPOSER DRAFT is the text a workspace's composer held when the editor
// closed the workspace (agentrepl.v1.ReportComposerDraft): the editor holds
// only the workspaces the roster marks held (Holds), so folding, hiding or
// leaving one kills its composer while the daemon keeps it. The draft is kept
// here, durably, so it outlives a daemon restart and a handover, until the
// editor reports it cleared or replaces it.

// composerDraftsDDL is the layout-192 addition: one kept draft per workspace,
// gone with its workspace. The fresh-file schema appends this same text
// (schema.go), so a migrated file and a created one cannot differ. A draft is
// never empty: an empty composer is no draft, and its row is deleted.
const composerDraftsDDL = `
CREATE TABLE composer_drafts (
  workspace_id TEXT PRIMARY KEY REFERENCES workspaces(id) ON DELETE CASCADE,
  text         TEXT NOT NULL CHECK (text <> '')
) STRICT;
`

// PutComposerDraft keeps TEXT as WS's composer draft, replacing any kept one.
// An empty TEXT is a bug in the caller (an empty composer is reported cleared,
// ClearComposerDraft) and is refused loudly rather than kept.
func (s *store) PutComposerDraft(ctx context.Context, ws WorkspaceID, text string) error {
	const op = "daemon.wsm.put_composer_draft"
	fields := dlog.Context{"workspace": string(ws), "chars": len(text)}
	if text == "" {
		err := fmt.Errorf("wsm: an empty composer draft for %q", ws)
		s.log.Error(op, "refused an empty composer draft", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx,
			`INSERT INTO composer_drafts (workspace_id, text) VALUES (?, ?)
			 ON CONFLICT(workspace_id) DO UPDATE SET text = excluded.text`,
			string(ws), text)
		return err
	})
}

// ClearComposerDraft forgets WS's composer draft. Clearing a workspace that
// keeps none changes nothing.
func (s *store) ClearComposerDraft(ctx context.Context, ws WorkspaceID) error {
	return s.write(ctx, "daemon.wsm.clear_composer_draft", dlog.Context{"workspace": string(ws)}, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx, `DELETE FROM composer_drafts WHERE workspace_id = ?`, string(ws))
		return err
	})
}

// ComposerDraft answers WS's kept composer draft, false when none is kept.
func (s *store) ComposerDraft(ctx context.Context, ws WorkspaceID) (string, bool, error) {
	var text string
	found := false
	err := s.read(ctx, "daemon.wsm.composer_draft", dlog.Context{"workspace": string(ws)}, func(ctx context.Context) error {
		err := s.q(ctx).QueryRowContext(ctx, `SELECT text FROM composer_drafts WHERE workspace_id = ?`, string(ws)).Scan(&text)
		if errors.Is(err, sql.ErrNoRows) {
			return nil
		}
		if err != nil {
			return err
		}
		found = true
		return nil
	})
	return text, found, err
}
