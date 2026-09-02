package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"time"

	"claude-repld/internal/dlog"
)

// scanLease decodes one lease row all-or-nothing: an undeclared holder or
// policy fails the read rather than being read as the zero arm, which would
// silently turn a merge's refusal policy into something else.
func scanLease(row interface{ Scan(...any) error }) (Lease, error) {
	var (
		l        Lease
		holder   int64
		policy   int64
		acquired int64
	)
	if err := row.Scan(&l.ID, &l.Workspace, &holder, &policy, &acquired); err != nil {
		return Lease{}, err
	}
	l.Holder = LeaseHolder(holder)
	if !l.Holder.valid() {
		return Lease{}, &DecodeError{Table: "leases", Row: string(l.ID), Field: "holder", Err: fmt.Errorf("unknown lease holder %d", holder)}
	}
	l.Policy = LeasePolicy(policy)
	if !l.Policy.valid() {
		return Lease{}, &DecodeError{Table: "leases", Row: string(l.ID), Field: "policy", Err: fmt.Errorf("unknown lease policy %d", policy)}
	}
	l.AcquiredAt = fromNanos(acquired)
	return l, nil
}

// leaseColumns is the one select list every lease read shares.
const leaseColumns = `id, workspace_id, holder, policy, acquired_at`

// AcquireLease takes the workspace's occupancy lease for holder under policy.
// It REFUSES when the lease is already held, carrying the current holder: the
// caller says who is in the way instead of retrying blindly. The read of the
// current holder and the insert are one immediate transaction, so two
// acquisitions can never both see the lease free.
func (s *store) AcquireLease(ctx context.Context, id WorkspaceID, holder LeaseHolder, policy LeasePolicy) (Lease, error) {
	return s.AcquireLeaseAs(ctx, id, NewLeaseID(), holder, policy)
}

// AcquireLeaseAs acquires the lease under an identity the CALLER minted.
//
// It exists for the merge, whose lease id is also its LEDGER identity: the
// merge bubble is addressed by it and is drawn from the moment a merge is
// QUEUED, which is before any occupancy is taken. Minting at acquisition would
// mean the queued bubble and the admitted one had different addresses.
func (s *store) AcquireLeaseAs(ctx context.Context, id WorkspaceID, lease LeaseID, holder LeaseHolder, policy LeasePolicy) (Lease, error) {
	const op = "daemon.wsm.acquire_lease"
	if lease == "" {
		err := fmt.Errorf("wsm: a lease acquisition needs an identity")
		s.log.Error(op, "refused a lease acquisition with no identity",
			withError(dlog.Context{"workspace": string(id)}, err))
		return Lease{}, err
	}
	fields := dlog.Context{"workspace": string(id), "holder": holder.String(), "policy": policy.String()}
	if !holder.valid() || !policy.valid() {
		err := fmt.Errorf("wsm: undeclared lease holder %d or policy %d", int(holder), int(policy))
		s.log.Error(op, "refused an undeclared lease holder or policy", withError(fields, err))
		return Lease{}, err
	}
	var out Lease
	err := s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		current, err := scanLease(tx.QueryRowContext(ctx, `SELECT `+leaseColumns+` FROM leases WHERE workspace_id = ?`, id))
		switch {
		case err == nil:
			return &LeaseHeldError{Workspace: id, Lease: current.ID, Holder: current.Holder, Policy: current.Policy}
		case !errors.Is(err, sql.ErrNoRows):
			return err
		}
		out = Lease{ID: lease, Workspace: id, Holder: holder, Policy: policy, AcquiredAt: time.Now().UTC()}
		_, err = tx.ExecContext(ctx, `INSERT INTO leases (`+leaseColumns+`) VALUES (?, ?, ?, ?, ?)`,
			out.ID, out.Workspace, int(out.Holder), int(out.Policy), nanos(out.AcquiredAt))
		return err
	})
	if err != nil {
		return Lease{}, err
	}
	return out, nil
}

// ReleaseLease releases one acquisition. Releasing a lease that is not held is
// a refusal, never a no-op: it means the caller lost track of the arbitration.
func (s *store) ReleaseLease(ctx context.Context, leaseID LeaseID) error {
	return s.write(ctx, "daemon.wsm.release_lease", dlog.Context{"lease": string(leaseID)}, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `DELETE FROM leases WHERE id = ?`, leaseID)
		if err != nil {
			return err
		}
		return requireOneRow(res, fmt.Sprintf("wsm: lease %s", leaseID))
	})
}

// Lease loads a workspace's current lease; the bool reports whether one is held.
func (s *store) Lease(ctx context.Context, id WorkspaceID) (Lease, bool, error) {
	var (
		out  Lease
		held bool
	)
	err := s.read(ctx, "daemon.wsm.lease", dlog.Context{"workspace": string(id)}, func(ctx context.Context) error {
		l, err := scanLease(s.db().QueryRowContext(ctx, `SELECT `+leaseColumns+` FROM leases WHERE workspace_id = ?`, id))
		if errors.Is(err, sql.ErrNoRows) {
			out, held = Lease{}, false
			return nil
		}
		if err != nil {
			return err
		}
		out, held = l, true
		return nil
	})
	if err != nil {
		return Lease{}, false, err
	}
	return out, held, nil
}

// SetLeasePolicy changes what a held lease projects onto new submissions — a
// merge moving from refusing to parked.
func (s *store) SetLeasePolicy(ctx context.Context, leaseID LeaseID, p LeasePolicy) error {
	const op = "daemon.wsm.set_lease_policy"
	fields := dlog.Context{"lease": string(leaseID), "policy": p.String()}
	if !p.valid() {
		err := fmt.Errorf("wsm: undeclared lease policy %d", int(p))
		s.log.Error(op, "refused an undeclared lease policy", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `UPDATE leases SET policy = ? WHERE id = ?`, int(p), leaseID)
		if err != nil {
			return err
		}
		return requireOneRow(res, fmt.Sprintf("wsm: lease %s", leaseID))
	})
}
