package db

import (
	"context"
	"database/sql"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-store/internal/logging"
)

// shape.go — the residue shape catalog (owner ruling 2026-09-13,
// docs/REALTEST-JUDGEMENT-CALLS.md "the unmodelled-line shape catalog").
//
// A residue line a producer no longer persists takes its bytes out of the store
// with it, and with them the only evidence the vendor emits that line at all.
// The catalog keeps ONE ROW PER DISTINCT RECURSIVE KEY STRUCTURE — the key
// names and the scalar TYPES, never the values — with the first example
// verbatim, the kind, first/last seen and a count, so the vendor's API stays
// discoverable at a cost bounded by the number of shapes rather than by
// traffic.
//
// THE CATALOG IS WRITTEN IN THE BATCH'S OWN TRANSACTION and never in one of its
// own. A shape is observed while reading bytes whose cursor advance commits in
// that transaction, so a catalog write that committed separately could be lost
// while the advance survived — and the re-read that would have observed the
// shape a second time then never happens.

// Residue shape listing bounds. A caller that names no limit gets
// defaultResidueShapeLimit rows; one that names more than
// maxResidueShapeLimit gets maxResidueShapeLimit, because the catalog is a
// discovery surface a human reads, not a bulk export.
const (
	defaultResidueShapeLimit = 200
	maxResidueShapeLimit     = 1000
)

// validateShapeObservation refuses an observation that cannot be catalogued.
//
// EVERY FIELD BUT THE EXAMPLE IS REQUIRED, and the example alone is optional
// because a line can legitimately be empty bytes. An observation missing its
// hash or its rendering is not a shape at all: it would occupy a row nobody
// could match, read or explain.
func validateShapeObservation(s *storev1.ShapeObservation, index int) error {
	if s == nil {
		return invalidFieldf(shapeField(index), "shape observation is unset")
	}
	if s.GetShapeHash() == "" {
		return invalidFieldf(shapeField(index)+".shape_hash", "shape_hash is empty — it is the catalog's primary key")
	}
	if s.GetKind() == "" {
		return invalidFieldf(shapeField(index)+".kind", "kind is empty — every observation names the residue kind it was seen under")
	}
	if s.GetKeyStructure() == "" {
		return invalidFieldf(shapeField(index)+".key_structure", "key_structure is empty — it is what the hash was taken over and what a reader reads")
	}
	if s.GetSeenMs() <= 0 {
		return invalidFieldf(shapeField(index)+".seen_ms", "seen_ms is not a positive unix millis (%d) — first_seen and last_seen are taken from the OBSERVER's clock, never the store's", s.GetSeenMs())
	}
	return nil
}

// shapeField names the offending observation for a refusal, index and all.
func shapeField(index int) string {
	return "shapes[" + itoa(index) + "]"
}

// itoa renders a small non-negative index without dragging strconv into a file
// that needs nothing else from it.
func itoa(n int) string {
	if n == 0 {
		return "0"
	}
	var buf [20]byte
	i := len(buf)
	for n > 0 {
		i--
		buf[i] = byte('0' + n%10)
		n /= 10
	}
	return string(buf[i:])
}

// applyShapes folds one batch's observations into the catalog.
//
// INSERT-OR-UPDATE, AND THE FIRST INSERT OWNS THE EXAMPLE. A later observation
// raises the count and the last-seen instant and touches nothing else: the kind,
// the rendering and the example are the shape's identity as it was first seen,
// and replacing the example on every observation would make the catalog's one
// piece of readable evidence depend on which line happened to arrive last.
//
// LAST-SEEN IS A MAXIMUM, not an assignment. The producers are not ordered
// against each other and a replayed batch carries an old instant, so taking it
// unconditionally would walk the row's last-seen backwards.
func (d *DB) applyShapes(ctx context.Context, tx *sql.Tx, shapes []*storev1.ShapeObservation) error {
	for _, s := range shapes {
		if _, err := tx.ExecContext(ctx, `
INSERT INTO residue_shapes(shape_hash, kind, key_structure, first_example, first_seen_ms, last_seen_ms, count)
VALUES (?, ?, ?, ?, ?, ?, 1)
ON CONFLICT(shape_hash) DO UPDATE SET
  last_seen_ms = MAX(residue_shapes.last_seen_ms, excluded.last_seen_ms),
  count        = residue_shapes.count + 1`,
			s.GetShapeHash(), s.GetKind(), s.GetKeyStructure(), s.GetFirstExample(), s.GetSeenMs(), s.GetSeenMs()); err != nil {
			return storagef(err, "cataloguing residue shape %s", s.GetShapeHash())
		}
	}
	return nil
}

// ResidueShapes reads the catalog, most recently seen first.
//
// THE EXAMPLE IS OPT-IN. It is the one column carrying raw vendor bytes, and a
// listing that always dragged it along would make a structure survey as
// expensive as reading the lines the catalog exists to avoid keeping.
func (d *DB) ResidueShapes(ctx context.Context, kind *string, limit uint32, includeExample bool) ([]*storev1.ResidueShapeRow, error) {
	base := logging.Fields{Operation: "store.db.residue-shapes", Table: "residue_shapes"}
	if kind != nil && *kind == "" {
		return nil, d.refuse(base, invalidFieldf("kind", "kind is present with an empty value — asking for every kind is expressed by absence, never by an empty string"))
	}
	started := d.mono()

	example := `NULL`
	if includeExample {
		example = `first_example`
	}
	querySQL := `SELECT shape_hash, kind, key_structure, ` + example +
		`, first_seen_ms, last_seen_ms, count FROM residue_shapes`
	var args []any
	if kind != nil {
		querySQL += ` WHERE kind = ?`
		args = append(args, *kind)
	}
	// shape_hash breaks the tie so two reads of an unchanged catalog answer
	// identically; last_seen_ms alone is not unique.
	querySQL += ` ORDER BY last_seen_ms DESC, shape_hash ASC LIMIT ?`
	args = append(args, int64(residueShapeLimit(limit)))

	rows, err := d.read.QueryContext(ctx, querySQL, args...)
	if err != nil {
		return nil, d.refuse(base, storagef(err, "reading residue shapes"))
	}
	defer rows.Close() //nolint:errcheck // the deferred close of a read

	var out []*storev1.ResidueShapeRow
	for rows.Next() {
		row := &storev1.ResidueShapeRow{}
		var firstExample []byte
		if err := rows.Scan(&row.ShapeHash, &row.Kind, &row.KeyStructure, &firstExample,
			&row.FirstSeenMs, &row.LastSeenMs, &row.Count); err != nil {
			return nil, d.refuse(base, storagef(err, "scanning a residue shape row"))
		}
		row.FirstExample = firstExample
		out = append(out, row)
	}
	if err := rows.Err(); err != nil {
		return nil, d.refuse(base, storagef(err, "iterating residue shape rows"))
	}
	d.observeQuery(StatementResidueShapes, "residue_shapes", base, started, int64(len(out)))
	d.traceStatement(ctx, StatementResidueShapes, "residue_shapes", base, int64(len(out)))
	d.log.LogVerbose(base, "residue shapes read shapes=%d one_kind=%t example=%t", len(out), kind != nil, includeExample)
	return out, nil
}

// residueShapeLimit resolves the caller's limit against the catalog's bounds.
func residueShapeLimit(limit uint32) uint32 {
	switch {
	case limit == 0:
		return defaultResidueShapeLimit
	case limit > maxResidueShapeLimit:
		return maxResidueShapeLimit
	default:
		return limit
	}
}
