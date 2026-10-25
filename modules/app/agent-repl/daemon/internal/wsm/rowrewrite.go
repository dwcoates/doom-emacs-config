package wsm

import (
	"context"
	"database/sql"
	"fmt"

	"google.golang.org/protobuf/encoding/protowire"
	"google.golang.org/protobuf/reflect/protoreflect"

	"agent-repld/internal/dlog"
)

// THE ONE WAY A LAYOUT STEP EDITS STORED FEED ROWS: every row of
// durable_feed_rows is handed to the step's edit, and the rows it rewrote or
// dropped are written back in the step's transaction. Each step that carries a
// retired row shape onto today's (layout 73's parsed prose, layout 77's
// retired fields) or drops one nothing can draw (layout 79's retired merge
// tabs) runs on it, so none can edit rows its own way.

// rowEdit is what a step does to one stored row: keeps it, rewrites it to
// row, or drops it.
type rowEdit struct {
	rewrite bool
	drop    bool
	row     []byte
}

// editDurableRows hands every stored row to EDIT, then writes back each
// rewrite and deletes each drop, answering how many of each. A row EDIT
// refuses fails the whole step: its transaction rolls back, and nothing is
// edited.
func editDurableRows(ctx context.Context, tx *sql.Tx, edit func(raw []byte) (rowEdit, error)) (rewritten, dropped int, err error) {
	rows, err := tx.QueryContext(ctx, `SELECT workspace_id, row_id, row FROM durable_feed_rows`)
	if err != nil {
		return 0, 0, err
	}
	type edited struct {
		ws, id string
		edit   rowEdit
	}
	var edits []edited
	for rows.Next() {
		var ws, id string
		var raw []byte
		if err := rows.Scan(&ws, &id, &raw); err != nil {
			rows.Close()
			return 0, 0, err
		}
		e, err := edit(raw)
		if err != nil {
			rows.Close()
			return 0, 0, fmt.Errorf("durable feed row %s/%s: %w", ws, id, err)
		}
		if e.rewrite || e.drop {
			edits = append(edits, edited{ws: ws, id: id, edit: e})
		}
	}
	if err := rows.Close(); err != nil {
		return 0, 0, err
	}
	if err := rows.Err(); err != nil {
		return 0, 0, err
	}
	for _, e := range edits {
		if e.edit.drop {
			if _, err := tx.ExecContext(ctx, `DELETE FROM durable_feed_rows WHERE workspace_id = ? AND row_id = ?`, e.ws, e.id); err != nil {
				return 0, 0, err
			}
			dropped++
			continue
		}
		if _, err := tx.ExecContext(ctx, `UPDATE durable_feed_rows SET row = ? WHERE workspace_id = ? AND row_id = ?`, e.edit.row, e.ws, e.id); err != nil {
			return 0, 0, err
		}
		rewritten++
	}
	return rewritten, dropped, nil
}

// rewriteDurableRows hands every stored row to REWRITE and writes back each
// row it changed, recording how many under DONE.
func rewriteDurableRows(ctx context.Context, tx *sql.Tx, log dlog.Logger, done string, rewrite func(raw []byte) ([]byte, bool, error)) error {
	rewritten, _, err := editDurableRows(ctx, tx, func(raw []byte) (rowEdit, error) {
		out, changed, err := rewrite(raw)
		return rowEdit{rewrite: changed, row: out}, err
	})
	if err != nil {
		return err
	}
	log.Info("daemon.wsm.open", done, dlog.Context{"rows": rewritten})
	return nil
}

// dropDurableRows deletes every stored row DROP answers true for, recording
// how many under DONE.
func dropDurableRows(ctx context.Context, tx *sql.Tx, log dlog.Logger, done string, drop func(raw []byte) (bool, error)) error {
	_, dropped, err := editDurableRows(ctx, tx, func(raw []byte) (rowEdit, error) {
		gone, err := drop(raw)
		return rowEdit{drop: gone}, err
	})
	if err != nil {
		return err
	}
	log.Info("daemon.wsm.open", done, dlog.Context{"rows": dropped})
	return nil
}

// walkMessages visits MSG and every message reachable from it, a parent
// before its children, and answers whether any visit changed one. A visit's
// error ends the walk.
func walkMessages(msg protoreflect.Message, visit func(protoreflect.Message) (bool, error)) (bool, error) {
	changed, err := visit(msg)
	if err != nil {
		return false, err
	}
	var walkErr error
	descend := func(m protoreflect.Message) bool {
		c, err := walkMessages(m, visit)
		if err != nil {
			walkErr = err
			return false
		}
		changed = changed || c
		return true
	}
	msg.Range(func(fd protoreflect.FieldDescriptor, v protoreflect.Value) bool {
		switch {
		case fd.IsList() && fd.Message() != nil:
			list := v.List()
			for i := 0; i < list.Len(); i++ {
				if !descend(list.Get(i).Message()) {
					return false
				}
			}
		case fd.IsMap() && fd.MapValue().Message() != nil:
			v.Map().Range(func(_ protoreflect.MapKey, mv protoreflect.Value) bool {
				return descend(mv.Message())
			})
			if walkErr != nil {
				return false
			}
		case !fd.IsList() && !fd.IsMap() && fd.Message() != nil:
			return descend(v.Message())
		}
		return true
	})
	return changed, walkErr
}

// eachUnknown hands VISIT each of MSG's unknown fields in order: its number,
// its wire type, the length of its tag and its whole encoding, tag included.
// Unknown bytes that do not parse are an error, never read as absent.
func eachUnknown(msg protoreflect.Message, visit func(num protowire.Number, typ protowire.Type, tagLen int, field []byte)) error {
	unknown := msg.GetUnknown()
	for len(unknown) > 0 {
		num, typ, l := protowire.ConsumeTag(unknown)
		if l < 0 {
			return fmt.Errorf("%s's unknown fields do not parse: %w", msg.Descriptor().FullName(), protowire.ParseError(l))
		}
		m := protowire.ConsumeFieldValue(num, typ, unknown[l:])
		if m < 0 {
			return fmt.Errorf("%s's unknown field %d does not parse: %w", msg.Descriptor().FullName(), num, protowire.ParseError(m))
		}
		visit(num, typ, l, unknown[:l+m])
		unknown = unknown[l+m:]
	}
	return nil
}

// peekUnknown answers the bytes of MSG's retired field NUM and whether it is
// there, changing nothing. The field is length-delimited (a string or a
// message), as every retired field a step carries is; one of another wire
// type is refused.
func peekUnknown(msg protoreflect.Message, num protowire.Number) ([]byte, bool, error) {
	var value []byte
	found := false
	wrongType := false
	err := eachUnknown(msg, func(n protowire.Number, typ protowire.Type, tagLen int, field []byte) {
		if n != num {
			return
		}
		if typ != protowire.BytesType {
			wrongType = true
			return
		}
		value, _ = protowire.ConsumeBytes(field[tagLen:])
		found = true
	})
	if err != nil {
		return nil, false, err
	}
	if wrongType {
		return nil, false, fmt.Errorf("%s's retired field %d is not length-delimited", msg.Descriptor().FullName(), num)
	}
	return value, found, nil
}

// dropUnknown removes MSG's retired field NUM, of whatever wire type, and
// answers whether it was there.
func dropUnknown(msg protoreflect.Message, num protowire.Number) (bool, error) {
	var kept []byte
	found := false
	err := eachUnknown(msg, func(n protowire.Number, _ protowire.Type, _ int, field []byte) {
		if n == num {
			found = true
			return
		}
		kept = append(kept, field...)
	})
	if err != nil || !found {
		return false, err
	}
	msg.SetUnknown(kept)
	return true, nil
}

// carryUnknown carries MSG's retired length-delimited field NUM onto its
// successor: CARRY is handed the field's bytes, and only once it has written
// the successor is the field removed, so a carry that refuses leaves MSG as
// it was. A field beside its already-set SUCCESSOR is refused: no build wrote
// both, so the row is not one a step understands. It answers whether the
// field was carried.
func carryUnknown(msg protoreflect.Message, num protowire.Number, successor bool, carry func([]byte) error) (bool, error) {
	value, found, err := peekUnknown(msg, num)
	if err != nil || !found {
		return false, err
	}
	if successor {
		return false, fmt.Errorf("a %s carries both its retired field %d and its successor", msg.Descriptor().FullName(), num)
	}
	if err := carry(value); err != nil {
		return false, err
	}
	return dropUnknown(msg, num)
}
