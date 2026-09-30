package db

import (
	"context"
	"database/sql"
	"strings"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-store/internal/logging"
)

// runclaim.go — the vendor task id ↔ shell run pairing (the `shell_run_claim`
// table), and the lookup the sidecar asks it through.
//
// WHY IT EXISTS. A detached shell's spool is named by the vendor's task id,
// and the sidecar reads a spool only once a spawning call claims it. A shell
// the vendor moved to the background on its own timeout inside a subagent is
// stated in its transcript only as prose, so no transcript line claimed its
// spool, the spool was never read, its terminal was never seen, and the run
// stayed live for good (2026-09-28). The vendor's task stream states the
// pairing for every run; the shim reads that stream and writes it here.
//
// THE STORE NEVER DERIVES ONE FROM THE OTHER. A claim is recorded exactly as
// the shim stated it. The one thing the store adds is the run's OWNING BOOK,
// read off the run's own `activity:` row — a fact the store already holds,
// written by whichever producer read the launching call.

// shellRunClaimsSQL answers every claim for a set of task ids, each with the
// book holding its run's activity row (NULL while none is on record). The `?`
// list is expanded to the asked ids. At package scope so the suite EXPLAINs
// the production text itself.
const shellRunClaimsSQL = `
	  SELECT c.vendor_task_id, c.run_id, e.book_agent_id FROM shell_run_claim c
	  LEFT JOIN entry e ON e.upsert_key = 'activity:' || c.run_id
	  WHERE c.vendor_task_id IN (%s)
	  ORDER BY c.vendor_task_id ASC, c.run_id ASC`

// ClaimedRun is one claim on record, with the book holding its run's
// launching call. Owner is empty while no producer has written that row.
type ClaimedRun struct {
	VendorTaskID string
	Run          string
	Owner        string
}

// validateShellRunClaim refuses a claim that names nothing on either side.
// BOTH HALVES ARE REQUIRED: a task id with no run claims nothing, and a run
// with no task id is a claim no spool could ever be matched to.
func validateShellRunClaim(c *storev1.ShellRunClaim, index int) error {
	field := "shell_run_claims[" + itoa(index) + "]"
	if c == nil {
		return invalidFieldf(field, "shell run claim is unset")
	}
	if c.GetVendorTaskId() == "" {
		return invalidFieldf(field+".vendor_task_id", "vendor_task_id is empty — a claim names the spool's task id it is looked up by")
	}
	if c.GetRun().GetValue() == "" {
		return invalidFieldf(field+".run", "run is unset or empty — a claim names the run the spool belongs to")
	}
	return nil
}

// applyShellRunClaims records one batch's claims inside the batch's own
// transaction. A claim already on record is ABSORBED, never rewritten: the
// row is the pair itself, so there is nothing a re-statement could change.
func (d *DB) applyShellRunClaims(ctx context.Context, tx *sql.Tx, claims []*storev1.ShellRunClaim, now int64) error {
	for _, c := range claims {
		if _, err := tx.ExecContext(ctx, `
INSERT INTO shell_run_claim(vendor_task_id, run_id, recorded_at_ms) VALUES (?, ?, ?)
ON CONFLICT(vendor_task_id, run_id) DO NOTHING`,
			c.GetVendorTaskId(), c.GetRun().GetValue(), now); err != nil {
			return storagef(err, "recording the claim of vendor task %s by run %s", c.GetVendorTaskId(), c.GetRun().GetValue())
		}
	}
	return nil
}

// ShellRunClaims answers every claim on record for the asked task ids, each
// with its run's owning book when that is on record.
//
// A TASK ID WITH NO CLAIM IS SIMPLY ABSENT, and a task id two runs claim is
// answered with both: the caller refuses to choose, exactly as it does for two
// transcript launches naming one task. No ids, or an empty id, is refused
// before any statement runs.
func (d *DB) ShellRunClaims(ctx context.Context, vendorTaskIDs []string) ([]ClaimedRun, error) {
	base := logging.Fields{Operation: "store.db.shell-run-claims", Table: "shell_run_claim"}
	if len(vendorTaskIDs) == 0 {
		return nil, d.refuse(base, invalidFieldf("vendor_task_ids", "vendor_task_ids is empty — the lookup names the spools it resolves"))
	}
	args := make([]any, len(vendorTaskIDs))
	for i, id := range vendorTaskIDs {
		if id == "" {
			return nil, d.refuse(base, invalidFieldf("vendor_task_ids["+itoa(i)+"]", "a vendor task id is empty — every asked id names a spool"))
		}
		args[i] = id
	}
	started := d.mono()
	query := strings.Replace(shellRunClaimsSQL, "%s", strings.TrimSuffix(strings.Repeat("?,", len(args)), ","), 1)
	rows, err := d.read.QueryContext(ctx, query, args...)
	if err != nil {
		return nil, d.refuse(base, storagef(err, "reading the claims of %d vendor task id(s)", len(args)))
	}
	defer rows.Close() //nolint:errcheck // the deferred close of a read
	var out []ClaimedRun
	for rows.Next() {
		var claim ClaimedRun
		var owner sql.NullString
		if err := rows.Scan(&claim.VendorTaskID, &claim.Run, &owner); err != nil {
			return nil, d.refuse(base, storagef(err, "reading a shell run claim"))
		}
		claim.Owner = owner.String
		out = append(out, claim)
	}
	if err := rows.Err(); err != nil {
		return nil, d.refuse(base, storagef(err, "reading the claims of %d vendor task id(s)", len(args)))
	}
	d.observeQuery(StatementShellRunClaims, "shell_run_claim", base, started, int64(len(out)))
	d.traceStatement(ctx, StatementShellRunClaims, "shell_run_claim", base, int64(len(out)))
	d.log.LogVerbose(base, "answered %d claim(s) for %d vendor task id(s)", len(out), len(args))
	return out, nil
}
