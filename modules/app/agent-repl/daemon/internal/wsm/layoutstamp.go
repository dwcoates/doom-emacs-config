package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"

	"agent-repld/internal/dlog"
)

// THE LAYOUT STAMP IS TWO NUMBERS, because a deploy is a handover.
//
// A joining successor applies the steps the incumbent keeps working across
// while the incumbent still serves, and the contract steps (what only
// removes a shape no running build reads) once it serves alone. One number
// cannot say "step 171 ran and contract step 169 did not", so a join once
// refused every additive step after a contract step, and a step that added a
// table had to be marked a contract to pass: the new build then read a table
// its own migration had not created yet (2026-11-19: the first deploy check
// after the landing of layout 171 failed on the missing
// deploy_ignored_records, and the queue stayed held).
//
// So the file says both:
//
//   - `layout.version`: EVERY step up to it has run. It is the one number a
//     build from before this stamp reads, and it never claims a step that did
//     not run, so such a build never takes a file that owes a contract for a
//     finished one.
//   - `layout.additive_through`: every step up to it that is NO CONTRACT has
//     run. NULL, and absent from a file no build with this stamp has
//     migrated, both mean `version`: nothing is owed.
//
// The contract steps between the two are OWED. A join moves
// additive_through alone; the takeover applies the owed contracts in order,
// and version catches up.

// layoutStampColumnDDL adds the second number to a file from before it.
const layoutStampColumnDDL = `ALTER TABLE layout ADD COLUMN additive_through INTEGER`

// layoutStamp is where a file stands (see above). AdditiveThrough is never
// below Version.
type layoutStamp struct {
	Version         int
	AdditiveThrough int
}

// String names the stamp for a record or a refusal.
func (st layoutStamp) String() string {
	if st.AdditiveThrough == st.Version {
		return fmt.Sprintf("layout %d", st.Version)
	}
	return fmt.Sprintf("layout %d with every step that is no contract run through %d", st.Version, st.AdditiveThrough)
}

// applied reports whether step m has run on a file at st.
func (st layoutStamp) applied(m migration) bool {
	return m.To <= st.Version || (m.Kind != MigrationContract && m.To <= st.AdditiveThrough)
}

// pending answers the steps of this build that have not run on a file at
// st, in the list's order.
func (st layoutStamp) pending() []migration {
	var out []migration
	for _, m := range migrations {
		if m.To > LayoutVersion {
			break
		}
		if !st.applied(m) {
			out = append(out, m)
		}
	}
	return out
}

// after answers the stamp of a file at st once step m has run on it, or why
// m is not the step that may run next: a contract step runs only as the very
// next step of the file, and any other only when every step between it and
// additive_through is a contract. Every step is applied through this, so no
// order of joins, takeovers and boots can stamp a step that did not run.
func (st layoutStamp) after(m migration) (layoutStamp, error) {
	if st.applied(m) {
		return st, fmt.Errorf("step %d (%s) has already run on a file at %s", m.To, m.Name, st)
	}
	if m.Kind == MigrationContract {
		if m.To != st.Version+1 {
			return st, fmt.Errorf("contract step %d (%s) is not the next step of a file at %s", m.To, m.Name, st)
		}
		st.Version = m.To
	} else {
		for _, earlier := range migrations {
			if earlier.To > st.AdditiveThrough && earlier.To < m.To && earlier.Kind != MigrationContract {
				return st, fmt.Errorf("step %d (%s) would run before step %d (%s) on a file at %s", m.To, m.Name, earlier.To, earlier.Name, st)
			}
		}
		st.AdditiveThrough = m.To
	}
	// VERSION CATCHES UP over every step that has run: it stops at the first
	// owed contract, or at the first step past additive_through.
	for _, next := range migrations {
		if next.To != st.Version+1 {
			continue
		}
		if next.Kind == MigrationContract || next.To > st.AdditiveThrough {
			break
		}
		st.Version = next.To
	}
	st.AdditiveThrough = max(st.AdditiveThrough, st.Version)
	return st, nil
}

// stampReader is what reads a stamp: the store's connection, or a step's
// transaction.
type stampReader interface {
	QueryRowContext(ctx context.Context, query string, args ...any) *sql.Row
}

// readLayoutStamp reads the stamp of the file at path through q, and whether
// the file carries the additive_through column at all. A file carrying no
// layout row is undecodable, not merely foreign.
func readLayoutStamp(ctx context.Context, q stampReader, path string) (st layoutStamp, column bool, err error) {
	err = q.QueryRowContext(ctx, `SELECT version FROM layout WHERE id = 1`).Scan(&st.Version)
	if errors.Is(err, sql.ErrNoRows) {
		return st, false, &DecodeError{Table: "layout", Row: "1", Err: errors.New("no layout row")}
	}
	if err != nil {
		return st, false, fmt.Errorf("wsm: read layout version from %q: %w", path, err)
	}
	st.AdditiveThrough = st.Version
	var columns int
	if err := q.QueryRowContext(ctx, `SELECT count(*) FROM pragma_table_info('layout') WHERE name = 'additive_through'`).Scan(&columns); err != nil {
		return st, false, fmt.Errorf("wsm: probe the layout stamp's columns in %q: %w", path, err)
	}
	if columns == 0 {
		return st, false, nil
	}
	var through sql.NullInt64
	if err := q.QueryRowContext(ctx, `SELECT additive_through FROM layout WHERE id = 1`).Scan(&through); err != nil {
		return st, true, fmt.Errorf("wsm: read the layout's additive steps from %q: %w", path, err)
	}
	if through.Valid {
		st.AdditiveThrough = int(through.Int64)
	}
	if st.AdditiveThrough < st.Version {
		return st, true, &DecodeError{Table: "layout", Row: "1", Err: fmt.Errorf("additive_through %d is below version %d", st.AdditiveThrough, st.Version)}
	}
	return st, true, nil
}

// layoutStamp reads the file's stamp, recording a file it cannot decode.
func (s *store) layoutStamp(ctx context.Context) (layoutStamp, error) {
	st, _, err := readLayoutStamp(ctx, s.q(ctx), s.path)
	var undecodable *DecodeError
	if errors.As(err, &undecodable) {
		s.log.Error("daemon.wsm.open", "state database carries no layout stamp this build can decode", dlog.Context{"path": s.path, "error": err.Error()})
	}
	return st, err
}

// ensureLayoutStampColumn adds the additive_through column to a file from
// before it, so a migrated file and a created one carry the same layout
// table. It writes, so only a file's sole writer calls it.
func (s *store) ensureLayoutStampColumn(ctx context.Context) error {
	_, column, err := readLayoutStamp(ctx, s.q(ctx), s.path)
	if err != nil || column {
		return err
	}
	if _, err := s.q(ctx).ExecContext(ctx, layoutStampColumnDDL); err != nil {
		return fmt.Errorf("wsm: add the layout stamp's additive_through column to %q: %w", s.path, err)
	}
	return nil
}

// stampStep writes the stamp of a file at from, once step m has run, inside
// the step's transaction. THE WRITE IS A COMPARE-AND-SET on the stamp the
// transaction read, so two appliers can never both stamp one step.
func stampStep(ctx context.Context, tx *sql.Tx, path string, column bool, from, to layoutStamp, m migration) error {
	if !column {
		if _, err := tx.ExecContext(ctx, layoutStampColumnDDL); err != nil {
			return fmt.Errorf("wsm: add the layout stamp's additive_through column to %q: %w", path, err)
		}
	}
	res, err := tx.ExecContext(ctx,
		`UPDATE layout SET version = ?, additive_through = ?
		 WHERE id = 1 AND version = ? AND COALESCE(additive_through, version) = ?`,
		to.Version, to.AdditiveThrough, from.Version, from.AdditiveThrough)
	if err != nil {
		return fmt.Errorf("wsm: stamp step %d in %q: %w", m.To, path, err)
	}
	rows, err := res.RowsAffected()
	if err != nil {
		return fmt.Errorf("wsm: stamp step %d in %q: rows affected: %w", m.To, path, err)
	}
	if rows != 1 {
		return fmt.Errorf("wsm: stamp step %d in %q touched %d rows, want exactly 1: the file no longer stands at %s", m.To, path, rows, from)
	}
	return nil
}
