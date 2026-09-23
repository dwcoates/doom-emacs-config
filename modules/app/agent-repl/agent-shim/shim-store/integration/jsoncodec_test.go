// jsoncodec_test.go — SUBJECT: the JSON codec carries the WHOLE contract, not
// just the easy verbs.
//
// It is the same handler answering a different content type, which is what makes
// the surface curl-able for a human debugging it. That only holds if every rpc
// round-trips — including the two shapes protobuf-JSON encodes specially
// (`bytes` as base64, google.protobuf.Struct as an object) and the two server
// streams. The suite covered GetLiveWork alone, so a codec-visible regression in
// any of the rest would have gone unnoticed.
package integration

import (
	"bytes"
	"testing"

	storev1 "agentrepl/proto/store/v1"
)

func TestJSONCodecRoundTripsAWriteAndItsPage(t *testing.T) {
	// Arrange
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.jsonClient()
	shim := streamProducer(cli)

	// Act
	shim.write(ctx, t, shim.agentEntry("w-json-page", "u-json-page",
		frameLine(agentID("main"), responseFrame("main", "act-1", "over JSON"))))

	// Assert
	page := openSession(ctx, t, cli, "main", 10, nil)
	assertTexts(t, "the page over JSON", pageTexts(page.GetPage()), []string{"over JSON"})
	store.assertNoErrorRecords()
}

func TestJSONCodecRoundTripsCursorCarryBytes(t *testing.T) {
	// Arrange: `carry` is proto `bytes`, which protobuf-JSON base64-encodes. A
	// codec that mangled it would corrupt the partial line a sidecar resumes
	// from — silently, since the cursor is only ever read back at startup.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.jsonClient()
	sidecar := fileProducer(cli)
	carry := []byte{0x00, 0x01, 0xfe, 0xff, '{', '"', 'p', 'a', 'r', 't'}
	cursor := cursorState("16777232:424242", "/transcripts/live.jsonl", 65536, carry)

	// Act
	sidecar.writeWithCursor(ctx, t, cursor,
		sidecar.agentEntry("w-json-carry", "u-json-carry",
			frameLine(agentID("main"), responseFrame("main", "act-1", "L1"))))

	// Assert
	got := sidecarCursors(ctx, t, cli, nil)
	if len(got) != 1 {
		t.Fatalf("cursors = %d, want 1", len(got))
	}
	if !bytes.Equal(got[0].GetCarry(), carry) {
		t.Fatalf("carry round-tripped as %v, want %v", got[0].GetCarry(), carry)
	}
	if got[0].GetOffset() != cursor.GetOffset() {
		t.Fatalf("offset = %d, want %d", got[0].GetOffset(), cursor.GetOffset())
	}
}

func TestJSONCodecRoundTripsResidueStructRaw(t *testing.T) {
	// Arrange: `raw` is a google.protobuf.Struct, encoded as a bare JSON object
	// rather than a message. It is the ONE thing residue exists to carry, so a
	// codec that dropped it would turn durable evidence into an empty marker.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.jsonClient()
	shim := streamProducer(cli)

	// Act
	shim.write(ctx, t, shim.agentEntry("w-json-raw", "u-json-raw", vendorSpecificLine("hook")))

	// Assert: accepted rather than refused as residue with no raw record, which
	// is exactly what a dropped Struct would look like from the store's side.
	store.assertNoErrorRecords()
}

func TestJSONCodecRoundTripsUnparsedResidue(t *testing.T) {
	// Arrange: unparsed carries its bytes as a string, alongside a uint64
	// offset — which protobuf-JSON encodes as a STRING, not a number.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	shim := streamProducer(store.jsonClient())

	// Act
	shim.write(ctx, t, shim.agentEntry("w-json-unparsed", "u-json-unparsed",
		unparsedLine("/transcripts/live.jsonl", 1<<40, "unexpected EOF", `{"broken":`)))

	// Assert
	store.assertNoErrorRecords()
}

func TestJSONCodecServesTheAgentSessionWatchStream(t *testing.T) {
	// Arrange: a server STREAM over the JSON codec, which frames differently
	// from the binary one.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.jsonClient()
	shim := streamProducer(cli)
	seedBook(ctx, t, shim, "main", "json-tail")
	opened := openSession(ctx, t, cli, "main", 10, nil)
	stream := watchStream(ctx, t, cli, opened.GetWatch())
	defer closeOrFail(t, stream)

	// Act
	shim.write(ctx, t, shim.agentEntry("w-json-tail", "u-json-tail",
		frameLine(agentID("main"), responseFrame("main", "act-1", "tailed over JSON"))))

	// Assert
	assertTexts(t, "the tail over JSON", receivedTexts(receiveLines(t, stream, 1)), []string{"tailed over JSON"})
	store.assertNoErrorRecords()
}

func TestJSONCodecRoundTripsAContinuationPage(t *testing.T) {
	// Arrange
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.jsonClient()
	writeNumberedLines(ctx, t, streamProducer(cli), "main", 4)
	opened := openSession(ctx, t, cli, "main", 2, nil)

	// Act
	next := readPage(ctx, t, cli, "main", 2, assertPageMore(t, opened.GetPage()))

	// Assert
	assertTexts(t, "the continuation over JSON", readTexts(next), []string{"L2", "L1"})
	if len(readPointers(next)) != 2 || readPointers(next)[0] == "" {
		t.Fatalf("the continuation's pointers did not survive the JSON codec: %v", readPointers(next))
	}
}

func TestJSONCodecCarriesATypedFailureArm(t *testing.T) {
	// Arrange: the refusal path is part of the contract too, and the `kind`
	// oneof is what a caller switches on.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()

	// Act
	failure := openSessionExpectingFailure(ctx, t, store.jsonClient(),
		&storev1.OpenAgentSessionRequest{Agent: agentID(""), PageSize: 10})

	// Assert
	assertOpenInvalidRequest(t, failure, "agent")
}

func TestJSONCodecServesGetWorkflowsNotImplementedArm(t *testing.T) {
	// Arrange
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()

	// Act
	resp, err := store.jsonClient().GetWorkflow(ctx, connectGetWorkflow("work-1"))
	if err != nil {
		t.Fatalf("GetWorkflow over JSON: %v", err)
	}

	// Assert
	if resp.Msg.GetFailure().GetNotImplemented() == nil {
		t.Fatalf("GetWorkflow over JSON answered %v, want the not_implemented arm", resp.Msg.GetResult())
	}
}
