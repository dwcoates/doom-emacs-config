package wsm

import (
	"context"
	"database/sql"
	"errors"
	"time"

	"agent-repld/internal/dlog"
)

// intakeWorkspacesDDL is the layout-115 addition: intake_workspaces, one row
// per EPHEMERAL INTAKE WORKSPACE, a backlogged workspace the daemon creates to
// work out what a request is and hand it on (a card's `?` question, a Slack
// emoji ack), which the daemon closes when a turn of it ends. The row is
// written in the transaction that inserts the workspace backlogged
// (RegisterFacts.Intake), so an intake workspace is never one without the
// other, and never drawn visible.
//
// IT IS ADDITIVE: one new table the previous build never names.
const intakeWorkspacesDDL = `
CREATE TABLE intake_workspaces (
  workspace_id TEXT PRIMARY KEY REFERENCES workspaces(id) ON DELETE CASCADE,
  marked_at    INTEGER NOT NULL
);
`

// intakeCardsDDL is the layout-154 addition: intake_cards, one row per intake
// workspace made for a feed card's `?` question, naming the card it was asked
// about: the workspace whose feed holds the card (asker_id) and the card's
// feed row (row_value). It is written in the transaction that inserts the
// intake workspace (RegisterFacts.IntakeCard), so a card's intake is never
// one the card cannot find (CardAddressees).
//
// IT IS ADDITIVE: one new table the previous build never names.
const intakeCardsDDL = `
CREATE TABLE intake_cards (
  workspace_id TEXT PRIMARY KEY REFERENCES workspaces(id) ON DELETE CASCADE,
  asker_id     TEXT NOT NULL,
  row_value    TEXT NOT NULL
);
CREATE INDEX intake_cards_by_card ON intake_cards(asker_id, row_value);
`

// IntakeCard is the feed card a `?` question's intake workspace was made for:
// the card's row on the feed of the workspace that asked.
type IntakeCard struct {
	Asker WorkspaceID
	Row   string
}

// insertIntakeRow records workspace ID as an ephemeral intake workspace as of
// AT, inside the transaction that inserts it (RegisterFacts.Intake), and the
// card it was made for when CARD is set (RegisterFacts.IntakeCard).
func insertIntakeRow(ctx context.Context, tx *sql.Tx, id WorkspaceID, at time.Time, card *IntakeCard) error {
	if _, err := tx.ExecContext(ctx, `INSERT INTO intake_workspaces (workspace_id, marked_at) VALUES (?, ?)`, id, nanos(at)); err != nil {
		return err
	}
	if card == nil {
		return nil
	}
	_, err := tx.ExecContext(ctx, `INSERT INTO intake_cards (workspace_id, asker_id, row_value) VALUES (?, ?, ?)`, id, card.Asker, card.Row)
	return err
}

// CardAddressees is what already addresses one feed card's `?` questions.
type CardAddressees struct {
	// Work is the newest open workspace an intake made for the card asked
	// to be created (its requested_by), never itself an intake workspace;
	// nil when none is open.
	Work *WorkspaceID
	// OpenIntake is the newest of the card's intake workspaces still open,
	// nil when every one has been retired.
	OpenIntake *WorkspaceID
}

// CardAddressees answers what already addresses CARD's questions: the open
// workspace one of its intakes created, and its intake still taking a
// question in.
func (s *store) CardAddressees(ctx context.Context, card IntakeCard) (CardAddressees, error) {
	var out CardAddressees
	err := s.read(ctx, "daemon.wsm.card_addressees", dlog.Context{"asker": string(card.Asker), "row": card.Row}, func(ctx context.Context) error {
		work, err := optionalWorkspaceID(ctx, s.q(ctx), `SELECT w.id FROM workspaces w JOIN intake_cards c ON w.requested_by = c.workspace_id
WHERE c.asker_id = ? AND c.row_value = ? AND w.closed = 0 AND w.id NOT IN (SELECT workspace_id FROM intake_workspaces)
ORDER BY w.created_at DESC, w.id DESC LIMIT 1`, card.Asker, card.Row)
		if err != nil {
			return err
		}
		intake, err := optionalWorkspaceID(ctx, s.q(ctx), `SELECT w.id FROM workspaces w JOIN intake_cards c ON w.id = c.workspace_id
WHERE c.asker_id = ? AND c.row_value = ? AND w.closed = 0
ORDER BY w.created_at DESC, w.id DESC LIMIT 1`, card.Asker, card.Row)
		out = CardAddressees{Work: work, OpenIntake: intake}
		return err
	})
	return out, err
}

// IsIntake answers whether workspace ID is an ephemeral intake workspace.
func (s *store) IsIntake(ctx context.Context, id WorkspaceID) (bool, error) {
	intake := false
	err := s.read(ctx, "daemon.wsm.is_intake", dlog.Context{"workspace": string(id)}, func(ctx context.Context) error {
		var one int
		err := s.q(ctx).QueryRowContext(ctx, `SELECT 1 FROM intake_workspaces WHERE workspace_id = ?`, id).Scan(&one)
		if errors.Is(err, sql.ErrNoRows) {
			return nil
		}
		intake = err == nil
		return err
	})
	return intake, err
}
