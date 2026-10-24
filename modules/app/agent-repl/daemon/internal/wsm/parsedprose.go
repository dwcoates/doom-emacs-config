package wsm

import (
	"context"
	"database/sql"
	"fmt"

	frontendv1 "agentrepl/proto/frontend/v1"

	"google.golang.org/protobuf/encoding/protowire"
	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/reflect/protoreflect"

	"agent-repld/internal/dlog"
	"agent-repld/internal/prose"
)

// retiredTextTag is frontend.v1.FeedTextBlock's retired `text` field: the raw
// string a text block carried before ParsedProse (`prose`, field 2) replaced
// it. The proto reserves the number, so a row stored by a build before
// ParsedProse holds it as an unknown field, which the webapp refuses whole
// ("the daemon is newer than this bundle").
const retiredTextTag protowire.Number = 1

// rewriteParsedProseRows is layout 73's row rewrite: every stored row
// (durable_feed_rows) holding a text block in its retired raw shape is
// re-encoded with that text parsed, as a prompt (prose.ParsePrompt), the parse
// every FeedTextBlock is drawn with: the block of a user or agent prompt. So a
// prompt recorded before ParsedProse is drawn again whole. Every other row is
// left byte for byte as it was.
//
// IT IS ADDITIVE: the build before this step already reads and writes only
// `prose`, so a rewritten row is the shape it draws.
func rewriteParsedProseRows(ctx context.Context, tx *sql.Tx, log dlog.Logger) error {
	rows, err := tx.QueryContext(ctx, `SELECT workspace_id, row_id, row FROM durable_feed_rows`)
	if err != nil {
		return err
	}
	type rewrite struct {
		ws, id string
		row    []byte
	}
	var rewrites []rewrite
	for rows.Next() {
		var ws, id string
		var raw []byte
		if err := rows.Scan(&ws, &id, &raw); err != nil {
			rows.Close()
			return err
		}
		out, changed, err := rewriteRetiredTextBlocks(log, raw)
		if err != nil {
			rows.Close()
			return fmt.Errorf("durable feed row %s/%s: %w", ws, id, err)
		}
		if changed {
			rewrites = append(rewrites, rewrite{ws: ws, id: id, row: out})
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
	log.Info("daemon.wsm.open", "re-encoded the stored rows whose text blocks were raw text as parsed prose", dlog.Context{"rows": len(rewrites)})
	return nil
}

// rewriteRetiredTextBlocks re-encodes one stored frontend.v1.FeedRow whose
// text blocks, wherever they sit in it, carry the retired raw text, and
// reports whether it did.
func rewriteRetiredTextBlocks(log dlog.Logger, raw []byte) ([]byte, bool, error) {
	row := &frontendv1.FeedRow{}
	if err := proto.Unmarshal(raw, row); err != nil {
		return nil, false, err
	}
	changed, err := upgradeTextBlocks(log, row.ProtoReflect())
	if err != nil || !changed {
		return raw, false, err
	}
	out, err := proto.Marshal(row)
	if err != nil {
		return nil, false, err
	}
	return out, true, nil
}

// textBlockName is the message whose retired field the rewrite carries over.
var textBlockName = (&frontendv1.FeedTextBlock{}).ProtoReflect().Descriptor().FullName()

// upgradeTextBlocks parses the retired raw text of every text block reachable
// from MSG into its `prose`, and reports whether it changed any.
func upgradeTextBlocks(log dlog.Logger, msg protoreflect.Message) (bool, error) {
	changed := false
	if msg.Descriptor().FullName() == textBlockName {
		upgraded, err := upgradeTextBlock(log, msg.Interface().(*frontendv1.FeedTextBlock))
		if err != nil {
			return false, err
		}
		changed = upgraded
	}
	var walkErr error
	msg.Range(func(fd protoreflect.FieldDescriptor, v protoreflect.Value) bool {
		switch {
		case fd.IsList() && fd.Message() != nil:
			list := v.List()
			for i := 0; i < list.Len(); i++ {
				c, err := upgradeTextBlocks(log, list.Get(i).Message())
				if err != nil {
					walkErr = err
					return false
				}
				changed = changed || c
			}
		case fd.IsMap() && fd.MapValue().Message() != nil:
			v.Map().Range(func(_ protoreflect.MapKey, mv protoreflect.Value) bool {
				c, err := upgradeTextBlocks(log, mv.Message())
				if err != nil {
					walkErr = err
					return false
				}
				changed = changed || c
				return true
			})
			if walkErr != nil {
				return false
			}
		case !fd.IsList() && !fd.IsMap() && fd.Message() != nil:
			c, err := upgradeTextBlocks(log, v.Message())
			if err != nil {
				walkErr = err
				return false
			}
			changed = changed || c
		}
		return true
	})
	return changed, walkErr
}

// upgradeTextBlock moves BLOCK's retired raw text, when it carries one, into
// its `prose`, parsed as a prompt. A block carrying both is refused: no build
// wrote both, so the row is not one this rewrite understands.
func upgradeTextBlock(log dlog.Logger, block *frontendv1.FeedTextBlock) (bool, error) {
	unknown := block.ProtoReflect().GetUnknown()
	var kept []byte
	var text string
	found := false
	for len(unknown) > 0 {
		num, typ, n := protowire.ConsumeTag(unknown)
		if n < 0 {
			return false, protowire.ParseError(n)
		}
		m := protowire.ConsumeFieldValue(num, typ, unknown[n:])
		if m < 0 {
			return false, protowire.ParseError(m)
		}
		if num == retiredTextTag && typ == protowire.BytesType {
			value, _ := protowire.ConsumeBytes(unknown[n:])
			text, found = string(value), true
		} else {
			kept = append(kept, unknown[:n+m]...)
		}
		unknown = unknown[n+m:]
	}
	if !found {
		return false, nil
	}
	if block.GetProse() != nil {
		return false, fmt.Errorf("a text block carries both its retired raw text and its parsed prose")
	}
	block.ProtoReflect().SetUnknown(kept)
	block.Prose = prose.ParsePrompt(log, text)
	return true, nil
}
