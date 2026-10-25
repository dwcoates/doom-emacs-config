package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"time"

	"agent-repld/internal/dlog"
)

// THE COLD READ A USER PAID FOR, kept until the read is spent.
//
// A cold gate's "pay" answer agrees to ONE cold read: the context as it stood
// at the conversation's last request, re-read at full price by the next one.
// Paying sends no request, so the cache stays cold until a prompt is sent,
// and every bring-up in between -- a rollout's bounce, a daemon restart --
// asks the shim again and is refused cold again. Without this record the
// daemon forgot the answer with the shim it was given to, and the user was
// asked to pay for the same read twice (ship-gns, 2026-10-19: paid at
// 10:38:30, a build_stale bounce at 10:38:31 stood the gate again).
//
// THE READ IS NAMED BY WHAT MAKES IT THE SAME READ: the conversation, the
// instant of its last request, and the model the read is priced for. A request
// sent after the payment moves the last request, so a later lapse is a
// DIFFERENT read the user never agreed to, and the record no longer covers it.

// coldReadPaymentsDDL is the layout-57 addition. A new table, so the step is
// additive: the build before it never names it. The fresh-file schema appends
// this same text (schema.go), so a migrated file and a created one cannot
// differ.
const coldReadPaymentsDDL = `
CREATE TABLE cold_read_payments (
  workspace_id       TEXT PRIMARY KEY REFERENCES workspaces(id) ON DELETE CASCADE,
  vendor_session_id  TEXT NOT NULL,
  last_request_at_ms INTEGER NOT NULL,
  model              TEXT NOT NULL,
  paid_at            INTEGER NOT NULL
);
`

// ColdReadPayment is the one cold read a workspace's user agreed to pay for.
type ColdReadPayment struct {
	// VendorSessionID is the conversation whose context is re-read. Never
	// empty.
	VendorSessionID string
	// LastRequestAtMs is when that conversation's last request was made, in
	// unix millis, as the shim stated it on the gate (conversation.v1
	// SessionCold.last_request_at_ms). Never negative.
	LastRequestAtMs int64
	// Model is the model the read is priced for (conversation.v1
	// SessionCold.requested_model), empty when the shim named none.
	Model string
	// PaidAt is when the user paid.
	PaidAt time.Time
}

// validateColdReadPayment refuses a payment no gate's answer produces.
func validateColdReadPayment(p ColdReadPayment) error {
	if p.VendorSessionID == "" {
		return errors.New("a cold read payment names its conversation")
	}
	if p.LastRequestAtMs < 0 {
		return fmt.Errorf("a cold read payment's last request is a unix instant, got %d", p.LastRequestAtMs)
	}
	if p.PaidAt.IsZero() {
		return errors.New("a cold read payment states when it was paid")
	}
	return nil
}

// PutColdReadPayment records the cold read a workspace's user paid for,
// replacing any it had: a workspace has one conversation, so it has one read
// to pay for at a time. An unknown workspace is ErrNotFound. A payment no
// answer produces is a defect in the caller and is refused rather than stored.
func (s *store) PutColdReadPayment(ctx context.Context, id WorkspaceID, p ColdReadPayment) error {
	const op = "daemon.wsm.put_cold_read_payment"
	fields := dlog.Context{
		"workspace": string(id), "vendor_session_id": p.VendorSessionID,
		"last_request_at_ms": p.LastRequestAtMs, "model": p.Model,
	}
	if err := validateColdReadPayment(p); err != nil {
		err = fmt.Errorf("wsm: cold read payment of %s: %w", id, err)
		s.log.Error(op, "refused a malformed cold read payment", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		var existing string
		if err := scanWorkspaceRow(ctx, tx, id, "id", &existing); err != nil {
			return err
		}
		_, err := tx.ExecContext(ctx,
			`INSERT INTO cold_read_payments (workspace_id, vendor_session_id, last_request_at_ms, model, paid_at) VALUES (?, ?, ?, ?, ?)
			 ON CONFLICT(workspace_id) DO UPDATE SET
			   vendor_session_id = excluded.vendor_session_id, last_request_at_ms = excluded.last_request_at_ms,
			   model = excluded.model, paid_at = excluded.paid_at`,
			id, p.VendorSessionID, p.LastRequestAtMs, p.Model, p.PaidAt.UnixNano())
		return err
	})
}

// DropColdReadPayment deletes a workspace's cold read payment: the read it
// named is no longer the one a resume would make. A workspace with none is not
// an error.
func (s *store) DropColdReadPayment(ctx context.Context, id WorkspaceID) error {
	return s.write(ctx, "daemon.wsm.drop_cold_read_payment", dlog.Context{"workspace": string(id)}, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx, `DELETE FROM cold_read_payments WHERE workspace_id = ?`, id)
		return err
	})
}

// ColdReadPaymentOf loads a workspace's cold read payment; the bool is false
// when none stands. A stored row no answer produces is a DecodeError.
func (s *store) ColdReadPaymentOf(ctx context.Context, id WorkspaceID) (ColdReadPayment, bool, error) {
	var (
		out   ColdReadPayment
		found bool
	)
	err := s.read(ctx, "daemon.wsm.cold_read_payment", dlog.Context{"workspace": string(id)}, func(ctx context.Context) error {
		var (
			p      ColdReadPayment
			paidAt int64
		)
		err := s.q(ctx).QueryRowContext(ctx,
			`SELECT vendor_session_id, last_request_at_ms, model, paid_at FROM cold_read_payments WHERE workspace_id = ?`, id).
			Scan(&p.VendorSessionID, &p.LastRequestAtMs, &p.Model, &paidAt)
		if errors.Is(err, sql.ErrNoRows) {
			out, found = ColdReadPayment{}, false
			return nil
		}
		if err != nil {
			return err
		}
		p.PaidAt = time.Unix(0, paidAt).UTC()
		if err := validateColdReadPayment(p); err != nil {
			return &DecodeError{Table: "cold_read_payments", Row: string(id), Err: err}
		}
		out, found = p, true
		return nil
	})
	if err != nil {
		return ColdReadPayment{}, false, err
	}
	return out, found, nil
}
