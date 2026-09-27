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
	s.own(out)
	return out, nil
}

// ReleaseLease releases one acquisition. Releasing a lease that is not held is
// a refusal, never a no-op: it means the caller lost track of the arbitration.
func (s *store) ReleaseLease(ctx context.Context, leaseID LeaseID) error {
	err := s.write(ctx, "daemon.wsm.release_lease", dlog.Context{"lease": string(leaseID)}, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `DELETE FROM leases WHERE id = ?`, leaseID)
		if err != nil {
			return err
		}
		return requireOneRow(res, fmt.Sprintf("wsm: lease %s", leaseID))
	})
	if err != nil {
		return err
	}
	s.disown(leaseID)
	return nil
}

// own records a lease this handle acquired.
func (s *store) own(l Lease) {
	s.leaseMu.Lock()
	defer s.leaseMu.Unlock()
	if s.owned == nil {
		s.owned = map[LeaseID]Lease{}
	}
	s.owned[l.ID] = l
}

// disown forgets a lease this handle no longer holds.
func (s *store) disown(id LeaseID) {
	s.leaseMu.Lock()
	defer s.leaseMu.Unlock()
	delete(s.owned, id)
}

// ForeignLeases lists every held lease THIS HANDLE did not acquire.
//
// OWNERSHIP IS THE ACQUIRING PROCESS. A daemon holds exactly one state handle
// for its whole life, so a lease this handle did not take was taken by some
// other process. On an INCUMBENT'S BOOT that process is gone -- the boot claim
// is the kernel's proof no other incumbent runs, and a joining successor's
// boot reconciles nothing -- so every lease this answers there is an orphan
// whose owner died without its orderly close (Close releases what it owns).
// The boot sequence releases each of them, loudly.
func (s *store) ForeignLeases(ctx context.Context) ([]Lease, error) {
	var all []Lease
	err := s.read(ctx, "daemon.wsm.foreign_leases", nil, func(ctx context.Context) error {
		rows, err := s.db().QueryContext(ctx, `SELECT `+leaseColumns+` FROM leases ORDER BY acquired_at, id`)
		if err != nil {
			return err
		}
		defer rows.Close()
		var out []Lease
		for rows.Next() {
			l, err := scanLease(rows)
			if err != nil {
				return err
			}
			out = append(out, l)
		}
		if err := rows.Err(); err != nil {
			return err
		}
		all = out
		return nil
	})
	if err != nil {
		return nil, err
	}
	s.leaseMu.Lock()
	defer s.leaseMu.Unlock()
	foreign := make([]Lease, 0, len(all))
	for _, l := range all {
		if _, mine := s.owned[l.ID]; !mine {
			foreign = append(foreign, l)
		}
	}
	return foreign, nil
}

// releaseOwnedLeases releases every non-merge lease this handle still owns, in
// one transaction, recording what it released at INFO. A lease whose row is
// already gone was released by the process it was handed to, and is recorded
// at DEBUG.
func (s *store) releaseOwnedLeases(ctx context.Context) error {
	const op = "daemon.wsm.close"
	s.leaseMu.Lock()
	var owned []Lease
	for _, l := range s.owned {
		if l.Holder != HolderMerge {
			owned = append(owned, l)
		}
	}
	s.leaseMu.Unlock()
	if len(owned) == 0 {
		return nil
	}
	var released, gone []string
	err := s.write(ctx, op, dlog.Context{"leases": len(owned)}, func(ctx context.Context, tx *sql.Tx) error {
		released, gone = nil, nil
		for _, l := range owned {
			res, err := tx.ExecContext(ctx, `DELETE FROM leases WHERE id = ?`, l.ID)
			if err != nil {
				return err
			}
			n, err := res.RowsAffected()
			if err != nil {
				return err
			}
			entry := string(l.ID) + " " + string(l.Workspace) + " " + l.Holder.String()
			if n == 0 {
				gone = append(gone, entry)
				continue
			}
			released = append(released, entry)
		}
		return nil
	})
	if err != nil {
		return fmt.Errorf("wsm: release the leases this handle still owns: %w", err)
	}
	for _, l := range owned {
		s.disown(l.ID)
	}
	if len(gone) > 0 {
		s.log.Debug(op, "leases this handle took were already released by the process they were handed to",
			dlog.Context{"leases": gone})
	}
	if len(released) > 0 {
		s.log.Info(op, "released the leases this process still held as its state handle closed",
			dlog.Context{"leases": released})
	}
	return nil
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
