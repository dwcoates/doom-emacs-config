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
	err := client.WriteBatch(ctx(), &storev1.EntryBatch{
		Entries:       []*storev1.StoreEntry{{WriteId: "w1", UpsertKey: "activity:a1"}},
		CursorAdvance: cursor("1:2", "/tmp/session.jsonl", 128),
	})

	// Assert.
	if err != nil {
		t.Fatalf("WriteBatch returned %v, want success", err)
	}
	if store.lastWrite.GetProducer() != Producer {
		t.Fatalf("producer = %q, want %q", store.lastWrite.GetProducer(), Producer)
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
	if err := client.WriteBatch(ctx(), &storev1.EntryBatch{CursorAdvance: want}); err != nil {
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
	err := client.WriteBatch(ctx(), &storev1.EntryBatch{})

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
	err := client.WriteBatch(ctx(), &storev1.EntryBatch{})

	// Assert.
	if err == nil {
		t.Fatal("a response carrying neither arm was read as durable")
	}
}

func TestWriteBatchConnectErrorIsNotARefusal(t *testing.T) {
	// Arrange.
	client := serve(t, &fakeStore{writeErr: connect.NewError(connect.CodeInternal, errors.New("boom"))})

	// Act.
	err := client.WriteBatch(ctx(), &storev1.EntryBatch{})

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
	err := client.WriteBatch(ctx(), nil)

	// Assert.
	if err == nil {
		t.Fatal("a nil batch was accepted")
	}
}

func TestWriteBatchUnreachableStoreFails(t *testing.T) {
	// Arrange.
	client := clientTo(t, filepath.Join(os.TempDir(), "ar-absent.sock"))

	// Act.
	err := client.WriteBatch(ctx(), &storev1.EntryBatch{})

	// Assert.
	if err == nil {
		t.Fatal("an unreachable store accepted a batch")
	}
}
