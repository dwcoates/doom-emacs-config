// turnheads_test.go — SUBJECT: a page carries the prompts of the turns it
// touches that sit below it (docs/protobuf-design/complete-pages.md), over the
// wire, on the opening page and on a continuation.
package integration

import (
	"context"
	"fmt"
	"testing"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/proto/store/v1/storev1connect"
	"agentrepl/shim-store/internal/db"
)

// writeLongTurn writes one turn whose prompt is followed by a page of the
// agent's answers, every line stamped with the turn, so the newest page holds
// only steps and the prompt sits on the page below.
func writeLongTurn(ctx context.Context, t *testing.T, shim *producer, book, turn string) {
	t.Helper()
	writeTurnOf(ctx, t, shim, book, turn, db.PageSize)
}

// writeTurnOf writes one turn whose prompt is followed by `steps` answers.
func writeTurnOf(ctx context.Context, t *testing.T, shim *producer, book, turn string, steps int) {
	t.Helper()
	prompt := shim.agentEntry("w-"+turn, turn, promptLine(agentID(book), promptFact(turn, book, "ask")))
	prompt.Turn = turnID(turn)
	entries := []*storev1.StoreEntry{prompt}
	for i := 1; i <= steps; i++ {
		label := fmt.Sprintf("L%d", i)
		step := shim.agentEntry("w-"+turn+"-"+label, "u-"+turn+"-"+label, frameLine(agentID(book), responseFrame(book, "act-"+label, label)))
		step.Turn = turnID(turn)
		entries = append(entries, step)
	}
	shim.write(ctx, t, entries...)
}

func TestTurnHeadsOverTheWire(t *testing.T) {
	tests := []struct {
		name  string
		heads func(ctx context.Context, t *testing.T, cli storev1connect.ShimStoreClient, opened *storev1.OpenAgentSessionSuccess) []*storev1.StoreTurnHead
		want  []string
	}{
		{
			name: "the opening page carries the prompt below it",
			heads: func(_ context.Context, _ *testing.T, _ storev1connect.ShimStoreClient, opened *storev1.OpenAgentSessionSuccess) []*storev1.StoreTurnHead {
				return opened.GetPage().GetHeads()
			},
			want: []string{"prompt:ask"},
		},
		{
			name: "the continuation holding the prompt carries no head",
			heads: func(ctx context.Context, t *testing.T, cli storev1connect.ShimStoreClient, opened *storev1.OpenAgentSessionSuccess) []*storev1.StoreTurnHead {
				return readPage(ctx, t, cli, "main", assertPageMore(t, opened.GetPage())).GetHeads()
			},
			want: []string{},
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			store := startStore(t, storeOptions{})
			ctx, cancel := callContext(t)
			defer cancel()
			cli := store.client()
			writeLongTurn(ctx, t, streamProducer(cli), "main", "turn-1")
			opened := openSession(ctx, t, cli, "main", nil)

			// Act
			heads := test.heads(ctx, t, cli, opened)

			// Assert
			got := make([]string, 0, len(heads))
			for _, head := range heads {
				got = append(got, lineText(head.GetLine().GetLine()))
			}
			assertTexts(t, "the turn heads", got, test.want)
			for _, head := range heads {
				if head.GetAdjacent() == nil {
					t.Fatalf("head %v: want adjacent, the turn's steps follow its prompt directly", head)
				}
			}
			store.assertNoErrorRecords()
		})
	}
}

func TestATurnHeadWithStepsBelowThePageSaysStepsBetweenOverTheWire(t *testing.T) {
	// Arrange: one step more than a page, so the oldest step sits between the
	// prompt and the newest page.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	writeTurnOf(ctx, t, streamProducer(cli), "main", "turn-1", db.PageSize+1)

	// Act
	opened := openSession(ctx, t, cli, "main", nil)

	// Assert
	heads := opened.GetPage().GetHeads()
	if len(heads) != 1 || heads[0].GetStepsBetween() == nil {
		t.Fatalf("heads = %v, want the prompt with steps between", heads)
	}
	store.assertNoErrorRecords()
}
