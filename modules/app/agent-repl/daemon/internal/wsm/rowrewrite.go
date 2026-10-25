package wsm

import (
	"context"
	"database/sql"
	"fmt"

	"google.golang.org/protobuf/encoding/protowire"
	"google.golang.org/protobuf/reflect/protoreflect"

	"agent-repld/internal/dlog"
)

// THE ONE WAY A LAYOUT STEP RE-ENCODES STORED FEED ROWS: every row of
// durable_feed_rows is handed to a rewrite, and the rows it changed are
// written back in the step's transaction. Each step that carries a retired
// row shape onto today's (layout 73's parsed prose, layout 76's retired
// fields) runs on it, so none can re-encode rows its own way.

// rewriteDurableRows hands every stored row to REWRITE and writes back each
// row it changed, recording how many under DONE. A row REWRITE refuses fails
// the whole step: its transaction rolls back, and nothing is rewritten.
func rewriteDurableRows(ctx context.Context, tx *sql.Tx, log dlog.Logger, done string, rewrite func(raw []byte) ([]byte, bool, error)) error {
	rows, err := tx.QueryContext(ctx, `SELECT workspace_id, row_id, row FROM durable_feed_rows`)
	if err != nil {
		return err
	}
	type rewritten struct {
		ws, id string
		row    []byte
	}
	var rewrites []rewritten
	for rows.Next() {
		var ws, id string
		var raw []byte
		if err := rows.Scan(&ws, &id, &raw); err != nil {
			rows.Close()
			return err
		}
		out, changed, err := rewrite(raw)
		if err != nil {
			rows.Close()
			return fmt.Errorf("durable feed row %s/%s: %w", ws, id, err)
		}
		if changed {
			rewrites = append(rewrites, rewritten{ws: ws, id: id, row: out})
		}
	}
	if err := rows.Close(); err != nil {
		return err
	}
	if err := rows.Err(); err != nil {
		return err
	}
	for _, rw := range rewrites {
		if _, err := tx.ExecContext(ctx, `UPDATE durable_feed_rows SET row = ? WHERE workspace_id = ? AND row_id = ?`, rw.row, rw.ws, rw.id); err != nil {
			return err
		}
	}
	log.Info("daemon.wsm.open", done, dlog.Context{"rows": len(rewrites)})
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

// takeUnknown removes MSG's retired field NUM from its unknown fields and
// answers its bytes and whether it was there. The field is length-delimited
// (a string or a message), as every retired field a step carries is; one of
// another wire type is refused.
func takeUnknown(msg protoreflect.Message, num protowire.Number) ([]byte, bool, error) {
	unknown := msg.GetUnknown()
	var kept []byte
	var value []byte
	found := false
	for len(unknown) > 0 {
		n, typ, l := protowire.ConsumeTag(unknown)
		if l < 0 {
			return nil, false, protowire.ParseError(l)
		}
		m := protowire.ConsumeFieldValue(n, typ, unknown[l:])
		if m < 0 {
			return nil, false, protowire.ParseError(m)
		}
		if n == num {
			if typ != protowire.BytesType {
				return nil, false, fmt.Errorf("%s's retired field %d has wire type %d, not length-delimited", msg.Descriptor().FullName(), num, typ)
			}
			value, _ = protowire.ConsumeBytes(unknown[l:])
			found = true
		} else {
			kept = append(kept, unknown[:l+m]...)
		}
		unknown = unknown[l+m:]
	}
	if found {
		msg.SetUnknown(kept)
	}
	return value, found, nil
}

// hasUnknown reports whether MSG carries the unknown field NUM, leaving it.
func hasUnknown(msg protoreflect.Message, num protowire.Number) bool {
	unknown := msg.GetUnknown()
	for len(unknown) > 0 {
		n, typ, l := protowire.ConsumeTag(unknown)
		if l < 0 {
			return false
		}
		if n == num {
			return true
		}
		m := protowire.ConsumeFieldValue(n, typ, unknown[l:])
		if m < 0 {
			return false
		}
		unknown = unknown[l+m:]
	}
	return false
}
