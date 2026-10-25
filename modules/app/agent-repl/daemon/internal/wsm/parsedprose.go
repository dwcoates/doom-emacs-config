package wsm

import (
	"context"
	"database/sql"

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
	return rewriteDurableRows(ctx, tx, log, "re-encoded the stored rows whose text blocks were raw text as parsed prose",
		func(raw []byte) ([]byte, bool, error) { return rewriteRetiredTextBlocks(log, raw) })
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
	return walkMessages(msg, func(m protoreflect.Message) (bool, error) {
		if m.Descriptor().FullName() != textBlockName {
			return false, nil
		}
		return upgradeTextBlock(log, m.Interface().(*frontendv1.FeedTextBlock))
	})
}

// upgradeTextBlock moves BLOCK's retired raw text, when it carries one, into
// its `prose`, parsed as a prompt. A block carrying both is refused: no build
// wrote both, so the row is not one this rewrite understands.
func upgradeTextBlock(log dlog.Logger, block *frontendv1.FeedTextBlock) (bool, error) {
	return carryUnknown(block.ProtoReflect(), retiredTextTag, block.GetProse() != nil, func(text []byte) error {
		block.Prose = prose.ParsePrompt(log, string(text))
		return nil
	})
}
