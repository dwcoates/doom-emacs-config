package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"

	"agent-repld/internal/dlog"
)

// workspaceBookDDL is the layout-87 addition: the book each workspace's
// conversation is stored under in the store, as its shim stated it on a
// StartSession answer (shim.v1.StartSessionResponse.book). The daemon reads
// the workspace's history from the store with it (storeread.v1), so showing a
// conversation never needs a shim up.
//
// A TABLE OF ITS OWN, NOT A SESSIONS COLUMN: the sessions row is composed
// whole on every start and does not exist before a workspace's first one,
// while the book is stated on every start answer, a refusal included, and
// outlives the session it was stated on. IT IS ADDITIVE: a new table the
// previous build never names.
const workspaceBookDDL = `
CREATE TABLE workspace_book (
  workspace_id  TEXT PRIMARY KEY REFERENCES workspaces(id) ON DELETE CASCADE,
  book_agent_id TEXT NOT NULL
);
`

// WorkspaceBook answers the book workspace ID's shim last stated, false when
// none has been stated since this layout.
func (s *store) WorkspaceBook(ctx context.Context, id WorkspaceID) (string, bool, error) {
	var book string
	found := false
	err := s.read(ctx, "daemon.wsm.workspace_book", dlog.Context{"workspace": string(id)}, func(ctx context.Context) error {
		err := s.q(ctx).QueryRowContext(ctx, `SELECT book_agent_id FROM workspace_book WHERE workspace_id = ?`, id).Scan(&book)
		switch {
		case errors.Is(err, sql.ErrNoRows):
			return nil
		case err != nil:
			return err
		}
		found = true
		return nil
	})
	return book, found, err
}

// SetWorkspaceBook records BOOK as workspace ID's book, replacing any earlier
// one (a rebind adopts another conversation's book).
func (s *store) SetWorkspaceBook(ctx context.Context, id WorkspaceID, book string) error {
	fields := dlog.Context{"workspace": string(id), "book": book}
	if book == "" {
		return fmt.Errorf("wsm: book of %s: a book names its agent", id)
	}
	return s.write(ctx, "daemon.wsm.set_workspace_book", fields, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx, `INSERT INTO workspace_book (workspace_id, book_agent_id) VALUES (?, ?)
			ON CONFLICT (workspace_id) DO UPDATE SET book_agent_id = excluded.book_agent_id`, id, book)
		return err
	})
}
