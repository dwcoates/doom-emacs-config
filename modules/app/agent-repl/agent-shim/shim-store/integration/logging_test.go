// logging_test.go — SUBJECT: every error is logged EXACTLY ONCE, by the layer
// that owns it, in the shape the logging contract mandates.
//
// Two layers each writing a normal-level record for one refusal is not
// redundancy: it is a count that lies to whoever alerts on it, and a reader who
// cannot tell one refusal from two. The storage layer traces a refused request
// at verbose — its statement and table are context — and the SERVER writes the
// single normal-level record, because only the server knows the rpc, the request
// id and the producer the refusal belongs to. The other way round for a storage
// failure, which the storage layer alone can describe.
package integration

import (
	"strings"
	"testing"
	"time"

	connect "connectrpc.com/connect"

	storev1 "agentrepl/proto/store/v1"
)

func TestARefusedWriteProducesExactlyOneNormalLevelRecord(t *testing.T) {
	// Arrange
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	shim := streamProducer(store.client())
	mark := store.logMark()

	// Act
	shim.writeExpectingFailure(ctx, t, nil,
		shim.agentEntry("w-once", "", frameLine(agentID("main"), responseFrame("main", "act-1", "x"))))

	// Assert
	rec := assertExactlyOneNormalRecord(t, store.logRecordsAfter(mark), "a refused write")
	if rec.Level != "warn" {
		t.Errorf("the refusal record is level %q, want warn", rec.Level)
	}
}

func TestTheOneRefusalRecordCarriesTheCallItBelongsTo(t *testing.T) {
	// Arrange: the storage layer's own trace names a statement and a table and
	// ties the refusal to nothing; the record that survives must name the call.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	shim := streamProducer(store.client())
	mark := store.logMark()

	// Act
	shim.writeExpectingFailure(ctx, t, nil,
		shim.agentEntry("w-corr", "", frameLine(agentID("main"), responseFrame("main", "act-1", "x"))))

	// Assert
	rec := assertExactlyOneNormalRecord(t, store.logRecordsAfter(mark), "a refused write")
	for _, key := range []string{"rpc", "refusal_site", "producer"} {
		if _, ok := rec.Context[key]; !ok {
			t.Errorf("the refusal record carries no %q: %v", key, rec.Context)
		}
	}
}

func TestAStalePointerIsNeverAnErrorRecord(t *testing.T) {
	// Arrange: a caller walked a book that moved. That is an ordinary race whose
	// recovery is a repaint — and a healthy store writing error records during
	// normal operation is exactly how an error log stops being read.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)
	writeNumberedLines(ctx, t, shim, "main", 1)
	writeNumberedLines(ctx, t, shim, "other", 1)
	otherBook := openSession(ctx, t, cli, "other", 10, nil)
	foreign := &storev1.StoreItemPointer{Value: pagePointers(otherBook.GetPage())[0]}
	mark := store.logMark()

	// Act
	assertOpenStalePointer(t, openSessionExpectingFailure(ctx, t, cli, &storev1.OpenAgentSessionRequest{
		Agent: agentID("main"), PageSize: 10, KnownThrough: foreign,
	}))

	// Assert
	records := store.logRecordsAfter(mark)
	assertNoErrorRecordIn(t, records, "a stale pointer")
	rec := assertExactlyOneNormalRecord(t, records, "a stale pointer")
	if rec.Context["refusal_site"] != "stale_pointer" {
		t.Errorf("the stale-pointer record's refusal_site is %v, want stale_pointer", rec.Context["refusal_site"])
	}
}

func TestAStalePointerProducesExactlyOneNormalLevelRecordOnRead(t *testing.T) {
	// Arrange
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)
	writeNumberedLines(ctx, t, shim, "main", 1)
	writeNumberedLines(ctx, t, shim, "other", 1)
	otherBook := openSession(ctx, t, cli, "other", 10, nil)
	foreign := &storev1.StoreItemPointer{Value: pagePointers(otherBook.GetPage())[0]}
	mark := store.logMark()

	// Act
	assertReadStalePointer(t, readPageExpectingFailure(ctx, t, cli, &storev1.ReadAgentPageRequest{
		Book: agentID("main"), PageSize: 10, Position: &storev1.ReadAgentPageRequest_After{After: foreign},
	}))

	// Assert
	assertExactlyOneNormalRecord(t, store.logRecordsAfter(mark), "a stale pointer on read")
}

func TestEveryRecordCarriesTheLoggingContractsRequiredFields(t *testing.T) {
	// Arrange: the contract is what makes records from different runtimes
	// interleave and compare without per-runtime normalization.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	shim := streamProducer(store.client())
	shim.write(ctx, t, shim.agentEntry("w-contract", "u-contract",
		frameLine(agentID("main"), responseFrame("main", "act-1", "L1"))))

	// Act
	records := store.logRecords()

	// Assert
	if len(records) == 0 {
		t.Fatal("the store wrote no records at all")
	}
	for i, rec := range records {
		if rec.Runtime != "store" {
			t.Fatalf("record %d runtime = %q, want store", i, rec.Runtime)
		}
		if rec.PID == 0 {
			t.Fatalf("record %d carries no pid", i)
		}
		switch rec.Level {
		case "debug", "info", "warn", "error":
		default:
			t.Fatalf("record %d level = %q, want one of debug/info/warn/error", i, rec.Level)
		}
		switch rec.Verbosity {
		case "normal", "verbose":
		default:
			t.Fatalf("record %d verbosity = %q, want normal or verbose", i, rec.Verbosity)
		}
		if rec.Operation == "" {
			t.Fatalf("record %d carries no operation", i)
		}
		if rec.Message == "" {
			t.Fatalf("record %d carries no message", i)
		}
		if rec.Context == nil {
			t.Fatalf("record %d carries no context object", i)
		}
	}
}

func TestEveryTimestampIsFixedWidthLocalRFC3339(t *testing.T) {
	// Arrange: SIX fractional digits and an explicit numeric offset, never a Z.
	// Fixed width is what makes records sort lexically, and a runtime that
	// resolved only to milliseconds pads rather than emitting a shorter field.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	shim := streamProducer(store.client())
	shim.write(ctx, t, shim.agentEntry("w-ts", "u-ts",
		frameLine(agentID("main"), responseFrame("main", "act-1", "L1"))))

	// Act
	records := store.logRecords()

	// Assert
	const layout = "2006-01-02T15:04:05.000000-07:00"
	for i, rec := range records {
		if strings.HasSuffix(rec.Timestamp, "Z") {
			t.Fatalf("record %d timestamp %q is UTC-suffixed; the contract requires a numeric offset", i, rec.Timestamp)
		}
		if len(rec.Timestamp) != len(layout) {
			t.Fatalf("record %d timestamp %q is %d chars, want the fixed width %d", i, rec.Timestamp, len(rec.Timestamp), len(layout))
		}
		if _, err := time.Parse(layout, rec.Timestamp); err != nil {
			t.Fatalf("record %d timestamp %q does not parse as %q: %v", i, rec.Timestamp, layout, err)
		}
	}
}

func TestACallersRequestIdReachesTheStoresRecords(t *testing.T) {
	// Arrange: correlation across runtimes is the whole point, so the header a
	// caller sends must appear as the top-level request_id — observed here
	// black-box, over a real socket, rather than in-process.
	store := startStore(t, storeOptions{verbose: true})
	ctx, cancel := callContext(t)
	defer cancel()
	seedBook(ctx, t, streamProducer(store.client()), "main", "reqid")
	mark := store.logMark()
	const requestID = "req-itest-0f1e2d3c"

	// OpenAgentSession writes its successful request boundary at debug, so this
	// subject enables the debug threshold and observes that record directly.
	req := connect.NewRequest(&storev1.OpenAgentSessionRequest{Agent: agentID("main"), PageSize: 10})
	req.Header().Set("X-Agent-Repl-Request-Id", requestID)

	// Act
	resp, err := store.client().OpenAgentSession(ctx, req)
	if err != nil {
		t.Fatalf("OpenAgentSession: %v", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("OpenAgentSession refused a well-formed open: %v", resp.Msg)
	}

	// Assert
	found := false
	for _, rec := range store.logRecordsAfter(mark) {
		if rec.RequestID == requestID {
			found = true
		}
	}
	if !found {
		t.Errorf("no record carried the caller's request_id %q", requestID)
	}
}

func TestARefusedCallsRequestIdReachesItsOneRecord(t *testing.T) {
	// Arrange: correlation matters most for the refusals, which is exactly the
	// record a reader goes looking for.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	mark := store.logMark()
	const requestID = "req-itest-refused-01"

	req := connect.NewRequest(&storev1.OpenAgentSessionRequest{Agent: agentID(""), PageSize: 10})
	req.Header().Set("X-Agent-Repl-Request-Id", requestID)

	// Act
	resp, err := store.client().OpenAgentSession(ctx, req)
	if err != nil {
		t.Fatalf("OpenAgentSession: %v", err)
	}
	if resp.Msg.GetFailure() == nil {
		t.Fatalf("OpenAgentSession accepted an empty agent: %v", resp.Msg)
	}

	// Assert
	rec := assertExactlyOneNormalRecord(t, store.logRecordsAfter(mark), "a refused open")
	if rec.RequestID != requestID {
		t.Errorf("the refusal record's request_id is %q, want %q", rec.RequestID, requestID)
	}
}
