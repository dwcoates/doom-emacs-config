package storeclient

import (
	"errors"
	"os"
	"path/filepath"
	"testing"

	storev1 "agentrepl/proto/store/v1"
	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"
)

func TestCursorsReturnsRecoveredCursors(t *testing.T) {
	// Arrange.
	want := cursor("1:2", "/tmp/session.jsonl", 4096)
	client := serve(t, &fakeStore{cursors: &storev1.GetSidecarCursorsResponse{
		Result: &storev1.GetSidecarCursorsResponse_Success{
			Success: &storev1.GetSidecarCursorsSuccess{Cursors: []*storev1.CursorState{want}},
		},
	}})

	// Act.
	got, err := client.Cursors(ctx(), "")

	// Assert.
	if err != nil {
		t.Fatalf("Cursors returned %v, want success", err)
	}
	if len(got) != 1 || !proto.Equal(got[0], want) {
		t.Fatalf("Cursors = %v, want %v", got, want)
	}
}

func TestCursorsEmptySuccessIsFreshStore(t *testing.T) {
	// Arrange.
	client := serve(t, &fakeStore{cursors: &storev1.GetSidecarCursorsResponse{
		Result: &storev1.GetSidecarCursorsResponse_Success{Success: &storev1.GetSidecarCursorsSuccess{}},
	}})

	// Act.
	got, err := client.Cursors(ctx(), "")

	// Assert.
	if err != nil {
		t.Fatalf("an empty cursor set was reported as a failure: %v", err)
	}
	if len(got) != 0 {
		t.Fatalf("Cursors = %v, want none", got)
	}
}

func TestCursorsFailureArmIsRefusal(t *testing.T) {
	// Arrange.
	client := serve(t, &fakeStore{cursors: &storev1.GetSidecarCursorsResponse{
		Result: &storev1.GetSidecarCursorsResponse_Failure{
			Failure: &storev1.GetSidecarCursorsFailure{Detail: "database is locked"},
		},
	}})

	// Act.
	_, err := client.Cursors(ctx(), "")

	// Assert.
	if !IsRefusal(err) {
		t.Fatalf("Cursors error = %v, want a refusal", err)
	}
}

func TestCursorsUnsetResultIsAnError(t *testing.T) {
	// Arrange.
	client := serve(t, &fakeStore{cursors: &storev1.GetSidecarCursorsResponse{}})

	// Act.
	_, err := client.Cursors(ctx(), "")

	// Assert.
	if err == nil {
		t.Fatal("a response carrying neither arm was read as an empty success")
	}
	if IsRefusal(err) {
		t.Fatalf("an unset oneof was reported as a store refusal: %v", err)
	}
}

func TestCursorsTransportErrorIsNotARefusal(t *testing.T) {
	// Arrange: a socket path nothing is listening on.
	client := clientTo(t, filepath.Join(os.TempDir(), "ar-absent.sock"))

	// Act.
	_, err := client.Cursors(ctx(), "")

	// Assert.
	if err == nil {
		t.Fatal("an unreachable store answered successfully")
	}
	if IsRefusal(err) {
		t.Fatalf("a transport failure was reported as a store refusal: %v", err)
	}
}

func TestCursorsPassesFileIDFilter(t *testing.T) {
	// Arrange.
	store := &fakeStore{cursors: &storev1.GetSidecarCursorsResponse{
		Result: &storev1.GetSidecarCursorsResponse_Success{Success: &storev1.GetSidecarCursorsSuccess{}},
	}}
	client := serve(t, store)

	// Act.
	if _, err := client.Cursors(ctx(), "7:9"); err != nil {
		t.Fatalf("Cursors returned %v", err)
	}

	// Assert.
	if got := store.lastCursorsReq.GetFileId(); got != "7:9" {
		t.Fatalf("store received file_id %q, want %q", got, "7:9")
	}
}

func TestCursorsOmitsFileIDWhenRecoveringAll(t *testing.T) {
	// Arrange.
	store := &fakeStore{cursors: &storev1.GetSidecarCursorsResponse{
		Result: &storev1.GetSidecarCursorsResponse_Success{Success: &storev1.GetSidecarCursorsSuccess{}},
	}}
	client := serve(t, store)

	// Act.
	if _, err := client.Cursors(ctx(), ""); err != nil {
		t.Fatalf("Cursors returned %v", err)
	}

	// Assert.
	if store.lastCursorsReq.FileId != nil {
		t.Fatalf("a full recovery sent a file_id filter %q", store.lastCursorsReq.GetFileId())
	}
}

func TestWriteBatchSuccessIsDurable(t *testing.T) {
	// Arrange.
	store := &fakeStore{write: &storev1.WriteBatchResponse{
		Result: &storev1.WriteBatchResponse_Success{Success: &storev1.WriteBatchSuccess{}},
	}}
	client := serve(t, store)

	// Act.
	_, err := client.WriteBatch(ctx(), &storev1.EntryBatch{
		Entries:       []*storev1.StoreEntry{{WriteId: "w1", UpsertKey: "activity:a1"}},
		CursorAdvance: cursor("1:2", "/tmp/session.jsonl", 128),
	}, nil)

	// Assert.
	if err != nil {
		t.Fatalf("WriteBatch returned %v, want success", err)
	}
	if store.lastWrite.GetProducer() != Producer {
		t.Fatalf("producer = %q, want %q", store.lastWrite.GetProducer(), Producer)
	}
}

// TestWriteBatchStatesTheBulkClass: every sidecar write is a copy of what the
// vendor already wrote, so it states BULK, and the store queues it behind any
// interactive write. The store refuses a write that states no class.
func TestWriteBatchStatesTheBulkClass(t *testing.T) {
	// Arrange.
	store := &fakeStore{write: &storev1.WriteBatchResponse{
		Result: &storev1.WriteBatchResponse_Success{Success: &storev1.WriteBatchSuccess{}},
	}}
	client := serve(t, store)

	// Act.
	if _, err := client.WriteBatch(ctx(), &storev1.EntryBatch{CursorAdvance: cursor("1:2", "/tmp/session.jsonl", 64)}, nil); err != nil {
		t.Fatalf("WriteBatch returned %v", err)
	}

	// Assert.
	if store.lastWrite.GetWriteClass().GetBulk() == nil {
		t.Fatalf("write_class = %v, want bulk", store.lastWrite.GetWriteClass())
	}
}

func TestWriteBatchCarriesCursorAdvance(t *testing.T) {
	// Arrange.
	store := &fakeStore{write: &storev1.WriteBatchResponse{
		Result: &storev1.WriteBatchResponse_Success{Success: &storev1.WriteBatchSuccess{}},
	}}
	client := serve(t, store)
	want := cursor("1:2", "/tmp/session.jsonl", 512)

	// Act.
	if _, err := client.WriteBatch(ctx(), &storev1.EntryBatch{CursorAdvance: want}, nil); err != nil {
		t.Fatalf("WriteBatch returned %v", err)
	}

	// Assert.
	if got := store.lastWrite.GetBatch().GetCursorAdvance(); !proto.Equal(got, want) {
		t.Fatalf("cursor advance = %v, want %v", got, want)
	}
}

func TestWriteBatchFailureArmIsRefusal(t *testing.T) {
	// Arrange.
	client := serve(t, &fakeStore{write: &storev1.WriteBatchResponse{
		Result: &storev1.WriteBatchResponse_Failure{
			Failure: &storev1.WriteBatchFailure{Detail: "transaction rolled back"},
		},
	}})

	// Act.
	_, err := client.WriteBatch(ctx(), &storev1.EntryBatch{}, nil)

	// Assert.
	var refusal *RefusalError
	if !errors.As(err, &refusal) {
		t.Fatalf("WriteBatch error = %v, want a refusal", err)
	}
	if refusal.Detail != "transaction rolled back" {
		t.Fatalf("refusal detail = %q, want the store's account", refusal.Detail)
	}
}

func TestWriteBatchUnsetResultIsAnError(t *testing.T) {
	// Arrange.
	client := serve(t, &fakeStore{write: &storev1.WriteBatchResponse{}})

	// Act.
	_, err := client.WriteBatch(ctx(), &storev1.EntryBatch{}, nil)

	// Assert.
	if err == nil {
		t.Fatal("a response carrying neither arm was read as durable")
	}
}

func TestWriteBatchConnectErrorIsNotARefusal(t *testing.T) {
	// Arrange.
	client := serve(t, &fakeStore{writeErr: connect.NewError(connect.CodeInternal, errors.New("boom"))})

	// Act.
	_, err := client.WriteBatch(ctx(), &storev1.EntryBatch{}, nil)

	// Assert.
	if err == nil {
		t.Fatal("a Connect error was read as durable")
	}
	if IsRefusal(err) {
		t.Fatalf("a Connect error was reported as a store refusal: %v", err)
	}
}

func TestWriteBatchRejectsNilBatch(t *testing.T) {
	// Arrange.
	client := serve(t, &fakeStore{})

	// Act.
	_, err := client.WriteBatch(ctx(), nil, nil)

	// Assert.
	if err == nil {
		t.Fatal("a nil batch was accepted")
	}
}

func TestWriteBatchUnreachableStoreFails(t *testing.T) {
	// Arrange.
	client := clientTo(t, filepath.Join(os.TempDir(), "ar-absent.sock"))

	// Act.
	_, err := client.WriteBatch(ctx(), &storev1.EntryBatch{}, nil)

	// Assert.
	if err == nil {
		t.Fatal("an unreachable store accepted a batch")
	}
}

// ---- ruling R-S2: the failure's KIND is what says whether a retry can help ----

func TestAStorageFailureRefusalCarriesItsKind(t *testing.T) {
	// Arrange. endpoint_write_batch.proto: the transaction failed in the
	// database and a retry MAY succeed, which is the recoverable outage.
	client := serve(t, &fakeStore{write: &storev1.WriteBatchResponse{
		Result: &storev1.WriteBatchResponse_Failure{
			Failure: &storev1.WriteBatchFailure{
				Detail: "database is locked",
				Kind:   &storev1.WriteBatchFailure_StorageFailure{StorageFailure: &storev1.WriteBatchStorageFailure{}},
			},
		},
	}})

	// Act.
	_, err := client.WriteBatch(ctx(), &storev1.EntryBatch{}, nil)

	// Assert.
	var refusal *RefusalError
	if !errors.As(err, &refusal) {
		t.Fatalf("WriteBatch error = %v, want a refusal", err)
	}
	if refusal.Kind != RefusalStorageFailure {
		t.Fatalf("refusal kind = %q, want %q", refusal.Kind, RefusalStorageFailure)
	}
}

func TestAnInvalidRequestRefusalNamesTheOffendingField(t *testing.T) {
	// Arrange. A retry of the same bytes cannot help, so the caller must be able
	// to tell this apart from an outage WITHOUT parsing the detail text, which
	// the proto documents as never switched on.
	client := serve(t, &fakeStore{write: &storev1.WriteBatchResponse{
		Result: &storev1.WriteBatchResponse_Failure{
			Failure: &storev1.WriteBatchFailure{
				Detail: "entry 3 carries no upsert_key",
				Kind: &storev1.WriteBatchFailure_InvalidRequest{
					InvalidRequest: &storev1.WriteBatchInvalidRequest{Field: "batch.entries[3].upsert_key"},
				},
			},
		},
	}})

	// Act.
	_, err := client.WriteBatch(ctx(), &storev1.EntryBatch{}, nil)

	// Assert.
	field, invalid := InvalidRequest(err)
	if !invalid {
		t.Fatalf("WriteBatch error = %v, want an invalid_request refusal", err)
	}
	if field != "batch.entries[3].upsert_key" {
		t.Fatalf("refused field = %q, want the field the store named", field)
	}
}

func TestAStorageFailureIsNotAnInvalidRequest(t *testing.T) {
	// Arrange. The two arms drive opposite reactions — suspend and recover
	// versus park the file — so confusing them either loops on bytes that can
	// never land or abandons a file over a transient database error.
	client := serve(t, &fakeStore{write: &storev1.WriteBatchResponse{
		Result: &storev1.WriteBatchResponse_Failure{
			Failure: &storev1.WriteBatchFailure{
				Detail: "database is locked",
				Kind:   &storev1.WriteBatchFailure_StorageFailure{StorageFailure: &storev1.WriteBatchStorageFailure{}},
			},
		},
	}})

	// Act.
	_, err := client.WriteBatch(ctx(), &storev1.EntryBatch{}, nil)

	// Assert.
	if _, invalid := InvalidRequest(err); invalid {
		t.Fatalf("a storage failure was read as an invalid request: %v", err)
	}
}

func TestAKindLessFailureIsTreatedAsAStorageFailure(t *testing.T) {
	// Arrange. An unset oneof is illegal on this contract — the kind is the arm
	// that says whether a retry can help — so a store that omits it has told the
	// producer nothing actionable. It is treated as the RECOVERABLE kind, so the
	// sidecar keeps trying rather than parking a file on a verdict the store
	// never actually gave.
	client := serve(t, &fakeStore{write: &storev1.WriteBatchResponse{
		Result: &storev1.WriteBatchResponse_Failure{
			Failure: &storev1.WriteBatchFailure{Detail: "something went wrong"},
		},
	}})

	// Act.
	_, err := client.WriteBatch(ctx(), &storev1.EntryBatch{}, nil)

	// Assert.
	var refusal *RefusalError
	if !errors.As(err, &refusal) {
		t.Fatalf("WriteBatch error = %v, want a refusal", err)
	}
	if refusal.Kind != RefusalStorageFailure {
		t.Fatalf("a kind-less failure resolved to %q, want %q", refusal.Kind, RefusalStorageFailure)
	}
}

func TestAKindLessFailureIsNeverAnInvalidRequest(t *testing.T) {
	// Arrange. Parking a file for the life of the process on a verdict the store
	// did not give would abandon a file the store may well accept next time.
	client := serve(t, &fakeStore{write: &storev1.WriteBatchResponse{
		Result: &storev1.WriteBatchResponse_Failure{
			Failure: &storev1.WriteBatchFailure{Detail: "something went wrong"},
		},
	}})

	// Act.
	_, err := client.WriteBatch(ctx(), &storev1.EntryBatch{}, nil)

	// Assert.
	if _, invalid := InvalidRequest(err); invalid {
		t.Fatalf("a kind-less failure was read as an invalid request: %v", err)
	}
}

func TestWriteBatchReturnsTheSkippedLegacyBookConflicts(t *testing.T) {
	// Arrange: the batch was durable but the store kept one row it would have
	// re-booked, naming the skip on the success arm.
	store := &fakeStore{write: &storev1.WriteBatchResponse{
		Result: &storev1.WriteBatchResponse_Success{Success: &storev1.WriteBatchSuccess{
			Skipped: []*storev1.WriteBatchSkippedEntry{
				{UpsertKey: "activity:msg_1:0", FromBook: "toolu_A", ToBook: "toolu_B"},
			},
		}},
	}}
	client := serve(t, store)

	// Act.
	skipped, err := client.WriteBatch(ctx(), &storev1.EntryBatch{
		Entries: []*storev1.StoreEntry{{WriteId: "w1", UpsertKey: "activity:msg_1:0"}},
	}, nil)

	// Assert.
	if err != nil {
		t.Fatalf("WriteBatch returned %v, want success", err)
	}
	if len(skipped) != 1 {
		t.Fatalf("skipped = %d, want 1", len(skipped))
	}
	if got := skipped[0]; got.UpsertKey != "activity:msg_1:0" || got.FromBook != "toolu_A" || got.ToBook != "toolu_B" {
		t.Fatalf("skipped[0] = %+v, want {activity:msg_1:0 toolu_A toolu_B}", got)
	}
}

func TestWriteBatchReturnsNoSkipsOnTheOrdinaryPath(t *testing.T) {
	// Arrange: a plain durable batch with an empty skipped list.
	store := &fakeStore{write: &storev1.WriteBatchResponse{
		Result: &storev1.WriteBatchResponse_Success{Success: &storev1.WriteBatchSuccess{}},
	}}
	client := serve(t, store)

	// Act.
	skipped, err := client.WriteBatch(ctx(), &storev1.EntryBatch{
		Entries: []*storev1.StoreEntry{{WriteId: "w1", UpsertKey: "activity:a1"}},
	}, nil)

	// Assert.
	if err != nil {
		t.Fatalf("WriteBatch returned %v, want success", err)
	}
	if skipped != nil {
		t.Fatalf("skipped = %v, want nil on the ordinary path", skipped)
	}
}
