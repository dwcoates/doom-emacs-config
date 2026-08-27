package db

import (
	"bytes"
	"errors"
	"io"
	"strings"
	"testing"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-store/internal/logging"
	"google.golang.org/protobuf/proto"
)

// --- what the store still commits ------------------------------------------

func TestIngestAdvancesACursorWithNoRecords(t *testing.T) {
	// Arrange: the sidecar's exactly-once contract requires a reader position
	// to become durable on its own when a read produced no records.
	d := openTemp(t)

	// Act
	if _, err := d.Ingest("sidecar", cursorBatch(&storev1.CursorState{
		FileId: "dev:1", Path: "/t/a.jsonl", Offset: 4096, Carry: []byte("{\"partial\":"),
	})); err != nil {
		t.Fatalf("Ingest: %v", err)
	}

	// Assert
	got, err := d.Cursor("dev:1")
	if err != nil {
		t.Fatalf("Cursor: %v", err)
	}
	if got.GetOffset() != 4096 || got.GetPath() != "/t/a.jsonl" {
		t.Fatalf("cursor = %+v, want the advanced position", got)
	}
}

func TestIngestOfAnEmptyBatchIsANoOp(t *testing.T) {
	// Arrange
	d := openTemp(t)

	// Act
	res, err := d.Ingest("producer", &storev1.EntryBatch{})

	// Assert
	if err != nil {
		t.Fatalf("Ingest of an empty batch: %v", err)
	}
	if res != (Result{}) {
		t.Fatalf("result = %+v, want a zero result", res)
	}
}

// --- what the store now refuses --------------------------------------------

func TestIngestRefusesABatchCarryingRecords(t *testing.T) {
	// Arrange: StoreEntry names no session, no position and no owning message,
	// so the entry table's addressing has no source and a row written anyway
	// could never be read back.
	d := openTemp(t)

	// Act
	_, err := d.Ingest("producer", batch(streamEntry()))

	// Assert
	if !errors.Is(err, ErrRecordPersistenceUnreconciled) {
		t.Fatalf("error = %v, want ErrRecordPersistenceUnreconciled", err)
	}
}

func TestIngestRefusalNamesTheRecordKind(t *testing.T) {
	// Arrange: the refusal has to be actionable, so it says WHAT was refused
	// without opening the opaque payload.
	d := openTemp(t)
	entry := streamEntry()
	entry.Entry = &storev1.StoreEntry_AgentUpdate{AgentUpdate: &storev1.StoreAgentUpdate{
		AgentInfo: &storev1.StoreAgentUpdate_ServeableFrame{ServeableFrame: &storev1.StorePageLine{}},
	}}

	// Act
	_, err := d.Ingest("producer", batch(entry))

	// Assert
	if err == nil || !strings.Contains(err.Error(), "agent_update.serveable_frame") {
		t.Fatalf("error = %v, want it to name the refused record's kind", err)
	}
}

func TestIngestRefusalCommitsNoCursorAdvanceFromTheSameBatch(t *testing.T) {
	// Arrange: a batch is durable or nothing, so a refused batch must not leave
	// its reader position behind — that is the loss half of the exactly-once
	// contract.
	d := openTemp(t)
	refused := batch(streamEntry())
	refused.CursorAdvance = &storev1.CursorState{FileId: "dev:1", Path: "/t/a.jsonl", Offset: 77}

	// Act
	if _, err := d.Ingest("sidecar", refused); err == nil {
		t.Fatal("Ingest accepted a batch carrying records")
	}

	// Assert
	got, err := d.Cursor("dev:1")
	if err != nil {
		t.Fatalf("Cursor: %v", err)
	}
	if got != nil {
		t.Fatalf("cursor = %+v, want nothing persisted from a refused batch", got)
	}
}

func TestIngestRejectsARecordThatNamesNoPlane(t *testing.T) {
	// Arrange: every stored record names the producer that observed it, or it
	// cannot be attributed at all. The check survived the redesign — it just
	// reads the plane off StoreEntry instead of off an internal half.
	d := openTemp(t)

	// Act
	_, err := d.Ingest("producer", batch(&storev1.StoreEntry{}))

	// Assert
	if err == nil || !strings.Contains(err.Error(), "does not name the plane") {
		t.Fatalf("error = %v, want the unattributed-record refusal", err)
	}
}

func TestIngestPlaneRejectionPrecedesThePersistenceRefusal(t *testing.T) {
	// Arrange: a producer sending an unattributable record must learn THAT,
	// not the unrelated persistence gap.
	d := openTemp(t)

	// Act
	_, err := d.Ingest("producer", batch(&storev1.StoreEntry{}))

	// Assert
	if errors.Is(err, ErrRecordPersistenceUnreconciled) {
		t.Fatalf("error = %v, want the plane refusal rather than the persistence gap", err)
	}
}

func TestIngestLogsARejectedTransactionWithStoreContext(t *testing.T) {
	// Arrange
	var logs bytes.Buffer
	log := logging.New(&logs, io.Discard, false).With(logging.Fields{Component: "db"})
	d, err := OpenWithOptions(t.TempDir()+"/entries.db", log, Options{})
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(func() { d.Close() })
	logs.Reset()

	// Act
	if _, err := d.Ingest("producer-x", batch(streamEntry())); err == nil {
		t.Fatal("Ingest accepted a batch carrying records")
	}

	// Assert
	record, found := findRecord(t, &logs, "ingest", "error")
	if !found {
		t.Fatalf("rejection record missing: %s", logs.String())
	}
	if record.Context["producer"] != "producer-x" || record.Context["table"] != "entry" ||
		!strings.Contains(record.Message, "transaction rejected") {
		t.Fatalf("rejection lacks canonical ingest context: %#v", record)
	}
}

// --- kind extraction -------------------------------------------------------

func TestKindOfNamesTheAgentUpdateArm(t *testing.T) {
	// Arrange
	entry := streamEntry()
	entry.Entry = &storev1.StoreEntry_AgentUpdate{AgentUpdate: &storev1.StoreAgentUpdate{
		AgentInfo: &storev1.StoreAgentUpdate_Bash{Bash: &storev1.StoreAgentBash{}},
	}}

	// Act / Assert
	if got := kindOf(entry); got != "agent_update.bash" {
		t.Fatalf("kindOf = %q, want %q", got, "agent_update.bash")
	}
}

func TestKindOfNamesTheSessionUpdateArm(t *testing.T) {
	// Arrange: the session arm has no inner oneof, so its kind is the outer arm
	// alone rather than a name with a dangling suffix.
	entry := streamEntry()
	entry.Entry = &storev1.StoreEntry_SessionUpdate{}

	// Act / Assert
	if got := kindOf(entry); got != "session_update" {
		t.Fatalf("kindOf = %q, want %q", got, "session_update")
	}
}

func TestKindOfReportsARecordWithNoArmSet(t *testing.T) {
	// Arrange: a record that says nothing is reported as such rather than
	// crashing or being silently named after some arm.
	// Act / Assert
	if got := kindOf(streamEntry()); got != "unset" {
		t.Fatalf("kindOf = %q, want %q", got, "unset")
	}
}

func TestKindOfNamesAnAgentUpdateWithNoInnerArm(t *testing.T) {
	// Arrange
	entry := streamEntry()
	entry.Entry = &storev1.StoreEntry_AgentUpdate{AgentUpdate: &storev1.StoreAgentUpdate{}}

	// Act / Assert
	if got := kindOf(entry); got != "agent_update.unset" {
		t.Fatalf("kindOf = %q, want %q", got, "agent_update.unset")
	}
}

func TestOneofArmReportsAnAbsentOneof(t *testing.T) {
	// Arrange: a oneof this code names but the schema does not declare must be
	// loud in the data, because "no such oneof" and "unset" mean opposite
	// things about the record.
	m := (&storev1.StoreEntry{}).ProtoReflect()

	// Act / Assert
	if got := oneofArm(m, "not_a_oneof"); got != "no-such-oneof:not_a_oneof" {
		t.Fatalf("oneofArm = %q, want the loud absent-oneof marker", got)
	}
}

func TestOneofArmReportsAnInvalidMessageAsUnset(t *testing.T) {
	// Arrange
	var nilUpdate *storev1.StoreAgentUpdate

	// Act / Assert
	if got := oneofArm(nilUpdate.ProtoReflect(), "agent_info"); got != "unset" {
		t.Fatalf("oneofArm = %q, want %q", got, "unset")
	}
}

// --- plane extraction ------------------------------------------------------

func TestPlaneOfNamesTheStreamPlane(t *testing.T) {
	// Arrange / Act
	got, err := planeOf(streamEntry())

	// Assert
	if err != nil || got != planeStream {
		t.Fatalf("planeOf = (%d, %v), want (%d, nil)", got, err, planeStream)
	}
}

func TestPlaneOfNamesTheFilePlane(t *testing.T) {
	// Arrange
	entry := &storev1.StoreEntry{Plane: &storev1.Plane{Plane: &storev1.Plane_File{File: &storev1.PlaneFile{}}}}

	// Act
	got, err := planeOf(entry)

	// Assert
	if err != nil || got != planeFile {
		t.Fatalf("planeOf = (%d, %v), want (%d, nil)", got, err, planeFile)
	}
}

// --- proto sanity ----------------------------------------------------------

func TestStoreEntryRoundTripsThroughTheWire(t *testing.T) {
	// Arrange: the record is stored as an opaque blob, so its marshal/unmarshal
	// identity is the one property the store depends on.
	entry := streamEntry()
	entry.WriteId = "w-1"
	entry.UpsertKey = "u-1"

	// Act
	blob, err := proto.Marshal(entry)
	if err != nil {
		t.Fatalf("Marshal: %v", err)
	}
	got := &storev1.StoreEntry{}
	if err := proto.Unmarshal(blob, got); err != nil {
		t.Fatalf("Unmarshal: %v", err)
	}

	// Assert
	if got.GetWriteId() != "w-1" || got.GetUpsertKey() != "u-1" {
		t.Fatalf("round trip = %+v, want the write and upsert identities preserved", got)
	}
}
