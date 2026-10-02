// place_test.go — the conversation place on the wire.
//
// A book is served in DESCENDING CONVERSATION PLACE, never in arrival order;
// every served line carries its place, on the arm naming who established it;
// and a book can be read as it stood at an instant (`through`).
package integration

import (
	"context"
	"fmt"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	storev1connect "agentrepl/proto/store/v1/storev1connect"
	"agentrepl/shim-store/internal/testclose"

	"connectrpc.com/connect"
)

// writePlacedLine writes one response line of `book` stating a place.
func writePlacedLine(ctx context.Context, t *testing.T, shim *producer, book, label string, atMs int64, ordinal uint32) {
	t.Helper()
	entry := shim.agentEntry(
		fmt.Sprintf("w-%s-%s", book, label),
		fmt.Sprintf("u-%s-%s", book, label),
		frameLine(agentID(book), responseFrame(book, "act-"+label, label)),
	)
	entry.Place = &conversationv1.ConversationPlace{AtMs: atMs, Ordinal: ordinal}
	shim.write(ctx, t, entry)
}

// readThrough reads a book as it stood at an instant.
func readThrough(ctx context.Context, t *testing.T, cli storev1connect.ShimStoreClient, book string, atMs int64) *storev1.ReadAgentPageResponse {
	t.Helper()
	resp, err := cli.ReadAgentPage(ctx, connect.NewRequest(&storev1.ReadAgentPageRequest{
		Book:     agentID(book),
		Position: &storev1.ReadAgentPageRequest_Through{Through: &conversationv1.ConversationThrough{AtMs: atMs}},
	}))
	if err != nil {
		t.Fatalf("ReadAgentPage(%q through %d) transport error: %v", book, atMs, err)
	}
	return resp.Msg
}

func TestABookIsServedByPlaceNotByArrival(t *testing.T) {
	// Arrange: the line placed later in the conversation arrives first.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)
	writePlacedLine(ctx, t, shim, "main", "later", 200, 0)
	writePlacedLine(ctx, t, shim, "main", "earlier", 100, 0)

	// Act.
	opened := openSession(ctx, t, cli, "main", nil)

	// Assert.
	assertTexts(t, "the opening page", pageTexts(opened.GetPage()), []string{"later", "earlier"})
	store.assertNoErrorRecords()
}

func TestEveryServedLineCarriesItsPlace(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	writePlacedLine(ctx, t, streamProducer(cli), "main", "L1", 100, 2)

	// Act.
	opened := openSession(ctx, t, cli, "main", nil)

	// Assert.
	if got := placeText(opened.GetPage().GetLines()[0]); got != "recorded:100.2" {
		t.Fatalf("place = %q, want %q", got, "recorded:100.2")
	}
	store.assertNoErrorRecords()
}

func TestALineWrittenWithoutAPlaceIsServedAtItsReceiptInstant(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	writeNumberedLines(ctx, t, streamProducer(cli), "main", 1)

	// Act.
	opened := openSession(ctx, t, cli, "main", nil)

	// Assert.
	place := opened.GetPage().GetLines()[0].GetReceivedPlace()
	if place.GetAtMs() <= 0 || place.GetOrdinal() != 0 {
		t.Fatalf("place = %v, want a received place at a positive instant, ordinal 0", opened.GetPage().GetLines()[0].GetPlace())
	}
	store.assertNoErrorRecords()
}

func TestAWatchedLineCarriesItsPlace(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)
	writePlacedLine(ctx, t, shim, "main", "L0", 50, 0)
	opened := openSession(ctx, t, cli, "main", nil)
	stream := watchStream(ctx, t, cli, opened.GetWatch())
	defer testclose.OrFail(t, stream)

	// Act.
	writePlacedLine(ctx, t, shim, "main", "L1", 100, 1)

	// Assert.
	if got := receiveLines(t, stream, 1)[0].place; got != "recorded:100.1" {
		t.Fatalf("watched place = %q, want %q", got, "recorded:100.1")
	}
	store.assertNoErrorRecords()
}

func TestReadAgentPageThroughReadsTheBookAsItStoodThen(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)
	writePlacedLine(ctx, t, shim, "main", "before", 100, 0)
	writePlacedLine(ctx, t, shim, "main", "at", 200, 5)
	writePlacedLine(ctx, t, shim, "main", "after", 300, 0)

	// Act.
	resp := readThrough(ctx, t, cli, "main", 200)

	// Assert.
	assertTexts(t, "the book through 200", readTexts(resp.GetSuccess()), []string{"at", "before"})
	store.assertNoErrorRecords()
}

func TestReadAgentPageThroughAnUnknownBookIsRefusedAsUnknown(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	mark := store.logMark()

	// Act.
	resp := readThrough(ctx, t, store.client(), "nobody", 200)

	// Assert.
	if resp.GetFailure().GetUnknownAgent() == nil {
		t.Fatalf("result = %v, want the unknown_agent arm", resp.GetResult())
	}
	rec := assertExactlyOneNormalRecordAtLevel(t, recordsAtOperation(store.logRecordsAfter(mark), "store.rpc.read-agent-page"),
		"an unknown book read through an instant", "info")
	assertRefusalKeys(t, rec, "unknown_agent", "unknown_agent")
}

func TestAWriteStatingANonPositivePlaceIsRefused(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	shim := streamProducer(store.client())
	entry := shim.agentEntry("w-1", "u-1", frameLine(agentID("main"), responseFrame("main", "act-1", "L1")))
	entry.Place = &conversationv1.ConversationPlace{AtMs: 0}

	// Act.
	failure := shim.writeExpectingFailure(ctx, t, nil, entry)

	// Assert.
	assertWriteInvalidRequest(t, failure, "entries[0].place.at_ms")
}
