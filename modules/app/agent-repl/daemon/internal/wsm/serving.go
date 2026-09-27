package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"

	"claude-repld/internal/dlog"
)

// ClaimServing records this daemon instance as the workspace's serving owner —
// the handover's per-workspace transfer. The claim is unconditional by design:
// the kernel lock is the arbitration, this row is who serves it now.
func (s *store) ClaimServing(ctx context.Context, id WorkspaceID, daemon InstanceID) error {
	const op = "daemon.wsm.claim_serving"
	fields := dlog.Context{"workspace": string(id), "instance": string(daemon)}
	if daemon == "" {
		err := errors.New("wsm: empty daemon instance id")
		s.log.Error(op, "refused a serving claim with no instance", withError(fields, err))
		return err
	}
	return s.setWorkspaceField(ctx, op, "serving_instance", id, string(daemon), fields)
}

// ClaimUnownedServing records daemon as the workspace's serving owner ONLY
// when no instance serves it or daemon already does, in one immediate
// transaction. It answers whether the claim stands and, when it does not,
// which instance holds the workspace.
//
// IT IS THE ARBITRATION BETWEEN AN INCUMBENT AND ITS SUCCESSOR over a
// workspace the incumbent released. The successor's adoption and the
// incumbent's reclaim of an adoption whose window expired both race for the
// same released row, and only the side this answers true may go on to dial
// the shim. ClaimServing is unconditional and cannot arbitrate that race,
// which is why neither side uses it there.
//
// A LOST CLAIM IS AN ANSWER, NOT A FAILURE: it is recorded at DEBUG and
// returned as false with the holder, never as an error.
func (s *store) ClaimUnownedServing(ctx context.Context, id WorkspaceID, daemon InstanceID) (bool, InstanceID, error) {
	const op = "daemon.wsm.claim_unowned_serving"
	fields := dlog.Context{"workspace": string(id), "instance": string(daemon)}
	if daemon == "" {
		err := errors.New("wsm: empty daemon instance id")
		s.log.Error(op, "refused a serving claim with no instance", withError(fields, err))
		return false, "", err
	}
	var (
		claimed bool
		holder  InstanceID
	)
	err := s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		var instance sql.NullString
		err := tx.QueryRowContext(ctx, `SELECT serving_instance FROM workspaces WHERE id = ?`, id).Scan(&instance)
		if errors.Is(err, sql.ErrNoRows) {
			return fmt.Errorf("wsm: workspace %s: %w", id, ErrNotFound)
		}
		if err != nil {
			return err
		}
		if instance.Valid && instance.String != "" && InstanceID(instance.String) != daemon {
			holder = InstanceID(instance.String)
			return nil
		}
		if _, err := tx.ExecContext(ctx, `UPDATE workspaces SET serving_instance = ? WHERE id = ?`, string(daemon), id); err != nil {
			return err
		}
		claimed = true
		return nil
	})
	if err != nil {
		return false, "", err
	}
	if !claimed {
		s.log.Debug(op, "another instance already serves the workspace; the claim did not stand",
			dlog.Context{"workspace": string(id), "instance": string(daemon), "holder": string(holder)})
	}
	return claimed, holder, nil
}

// Serving reports which daemon instance serves a workspace, nil when none does.
func (s *store) Serving(ctx context.Context, id WorkspaceID) (*InstanceID, error) {
	var out *InstanceID
	err := s.read(ctx, "daemon.wsm.serving", dlog.Context{"workspace": string(id)}, func(ctx context.Context) error {
		var instance sql.NullString
		err := s.db().QueryRowContext(ctx, `SELECT serving_instance FROM workspaces WHERE id = ?`, id).Scan(&instance)
		if errors.Is(err, sql.ErrNoRows) {
			return fmt.Errorf("wsm: workspace %s: %w", id, ErrNotFound)
		}
		if err != nil {
			return err
		}
		if instance.Valid && instance.String != "" {
			owner := InstanceID(instance.String)
			out = &owner
		}
		return nil
	})
	return out, err
}

// ReleaseServing gives up serving ownership, REFUSING when this instance does
// not hold it: a bystander can never give away another daemon's workspace.
func (s *store) ReleaseServing(ctx context.Context, id WorkspaceID, daemon InstanceID) error {
	fields := dlog.Context{"workspace": string(id), "instance": string(daemon)}
	return s.write(ctx, "daemon.wsm.release_serving", fields, func(ctx context.Context, tx *sql.Tx) error {
		var instance sql.NullString
		err := tx.QueryRowContext(ctx, `SELECT serving_instance FROM workspaces WHERE id = ?`, id).Scan(&instance)
		if errors.Is(err, sql.ErrNoRows) {
			return fmt.Errorf("wsm: workspace %s: %w", id, ErrNotFound)
		}
		if err != nil {
			return err
		}
		holder := InstanceID("")
		if instance.Valid {
			holder = InstanceID(instance.String)
		}
		if holder != daemon {
			return &ServingError{Workspace: id, Holder: holder, Claimant: daemon}
		}
		_, err = tx.ExecContext(ctx, `UPDATE workspaces SET serving_instance = NULL WHERE id = ?`, id)
		return err
	})
}
