package wsm

import (
	"bytes"
	"context"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"

	"google.golang.org/protobuf/encoding/protowire"
	"google.golang.org/protobuf/proto"

	"agent-repld/internal/dlog"
	"agent-repld/internal/prose"
)

// retiredTextBlock is a text block as a build before ParsedProse stored it:
// its raw TEXT in the retired field 1.
func retiredTextBlock(text string) *frontendv1.FeedTextBlock {
	block := &frontendv1.FeedTextBlock{}
	block.ProtoReflect().SetUnknown(protowire.AppendString(protowire.AppendTag(nil, retiredTextTag, protowire.BytesType), text))
	return block
}

// marshalRow encodes ROW, failing the test on an error.
func marshalRow(t *testing.T, row *frontendv1.FeedRow) []byte {
	t.Helper()
	raw, err := proto.Marshal(row)
	if err != nil {
		t.Fatal(err)
	}
	return raw
}

// agentPromptRow is an agent prompt row whose one block is BLOCK.
func agentPromptRow(block *frontendv1.FeedTextBlock) *frontendv1.FeedRow {
	return &frontendv1.FeedRow{Row: &frontendv1.FeedRow_AgentPrompt{AgentPrompt: &frontendv1.FeedAgentPrompt{
		Body: &frontendv1.FeedAgentPromptBody{Blocks: []*frontendv1.FeedAgentPromptBlock{{Block: &frontendv1.FeedAgentPromptBlock_Text{Text: block}}}},
	}}}
}

func TestRewriteRetiredTextBlocksParsesTheRetiredTextAsAPrompt(t *testing.T) {
	// Arrange
	const text = "3.1 - yes\nand more"
	raw := marshalRow(t, agentPromptRow(retiredTextBlock(text)))

	// Act
	out, changed, err := rewriteRetiredTextBlocks(dlog.NewTestLogger(), raw)

	// Assert
	if err != nil || !changed {
		t.Fatalf("rewriteRetiredTextBlocks = changed %v, %v; want a rewrite", changed, err)
	}
	row := &frontendv1.FeedRow{}
	if err := proto.Unmarshal(out, row); err != nil {
		t.Fatal(err)
	}
	block := row.GetAgentPrompt().GetBody().GetBlocks()[0].GetText()
	want := prose.ParsePrompt(dlog.NewTestLogger(), text)
	if len(block.ProtoReflect().GetUnknown()) != 0 || !proto.Equal(block.GetProse(), want) {
		t.Fatalf("block = %v (unknown %x), want prose %v and no unknown field", block, block.ProtoReflect().GetUnknown(), want)
	}
}

func TestRewriteRetiredTextBlocksLeavesARowWithNoRetiredTextAlone(t *testing.T) {
	// Arrange
	raw := marshalRow(t, agentPromptRow(&frontendv1.FeedTextBlock{Prose: prose.ParsePrompt(dlog.NewTestLogger(), "hello")}))

	// Act
	out, changed, err := rewriteRetiredTextBlocks(dlog.NewTestLogger(), raw)

	// Assert
	if err != nil || changed || !bytes.Equal(out, raw) {
		t.Fatalf("rewriteRetiredTextBlocks = changed %v, %v; want the row untouched", changed, err)
	}
}

func TestRewriteRetiredTextBlocksRefusesABlockCarryingBothShapes(t *testing.T) {
	// Arrange
	block := retiredTextBlock("raw")
	block.Prose = prose.ParsePrompt(dlog.NewTestLogger(), "parsed")
	raw := marshalRow(t, agentPromptRow(block))

	// Act
	_, _, err := rewriteRetiredTextBlocks(dlog.NewTestLogger(), raw)

	// Assert
	if err == nil {
		t.Fatal("rewriteRetiredTextBlocks accepted a block carrying both its raw text and its prose")
	}
}

func TestTheMigrationRewritesStoredRetiredTextBlocks(t *testing.T) {
	// Arrange -- a layout-72 file holding an agent prompt stored before ParsedProse.
	path := fixtureAt(t, 72)
	execRaw(t, path, `INSERT INTO durable_feed_rows (workspace_id, row_id, plane, order_key, row) VALUES ('ws-1', 'prompt-1', 0, 'k', ?)`,
		marshalRow(t, agentPromptRow(retiredTextBlock("sent text"))))

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open on a layout-72 database: %v", err)
	}
	defer handle.Close()

	// Assert
	raw := scalar[[]byte](t, handle.(*store), `SELECT row FROM durable_feed_rows WHERE row_id = 'prompt-1'`)
	row := &frontendv1.FeedRow{}
	if err := proto.Unmarshal(raw, row); err != nil {
		t.Fatal(err)
	}
	if got := row.GetAgentPrompt().GetBody().GetBlocks()[0].GetText().GetProse(); got == nil {
		t.Fatalf("stored row = %v; want its text block parsed", row)
	}
}
