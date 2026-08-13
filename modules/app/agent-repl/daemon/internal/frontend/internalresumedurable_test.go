package frontend

import (
	"testing"

	datav1 "agentrepl/proto/agentshim/data/v1"
	protocolv1 "agentrepl/proto/protocol/v1"

	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/types/known/anypb"
)

// THE MARKER MUST RIDE THE DURABLE ROW.
//
// The re-drive's invisibility is only as good as the field the curator filters
// on. If `request_id` were an in-memory correlation the daemon carried beside
// the event, a store re-pull — which reconstructs events from bytes on disk
// with no live daemon state to consult — would rehydrate an unmarked user
// message and render the daemon's private instruction to the user, possibly
// years later.
//
// It is not. The shim-store persists the WHOLE `protocolv1.Event` as a
// proto-marshalled blob (`agent-shim/shim-store/internal/db/ingest.go`) and
// unmarshals it verbatim on replay (`.../db/query.go`), so `Event.request_id`
// (core.proto field 5) is on disk with everything else.
//
// These tests prove that end to end rather than by inspection: the event goes
// through the same Marshal/Unmarshal round trip the store performs, and the
// curation is asserted on what comes back out.

// throughTheStore round-trips an event exactly as the shim-store does — marshal
// on ingest, unmarshal on replay — so a curation downstream of it is reading
// rehydrated bytes rather than the object the test built.
func throughTheStore(t *testing.T, ev *protocolv1.Event) *protocolv1.Event {
	t.Helper()
	blob, err := proto.Marshal(ev)
	if err != nil {
		t.Fatalf("marshal event as the store ingests it: %v", err)
	}
	out := &protocolv1.Event{}
	if err := proto.Unmarshal(blob, out); err != nil {
		t.Fatalf("unmarshal event as the store replays it: %v", err)
	}
	return out
}

// storedUserMessageEvent is one vendor user-message event as the store holds
// it, carrying requestID on the EVENT — the field the curator filters on.
func storedUserMessageEvent(t *testing.T, uuid, requestID, body string) *protocolv1.Event {
	t.Helper()
	vendor, err := anypb.New(&datav1.UserMessage{
		Uuid: uuid,
		Message: &datav1.ApiUserMessage{
			Content: &datav1.ApiUserMessage_ContentString{ContentString: body},
		},
	})
	if err != nil {
		t.Fatalf("pack vendor user message: %v", err)
	}
	return &protocolv1.Event{
		SessionId:    "s1",
		Seq:          10,
		ProducedAtMs: 1_700_000_000_000,
		RequestId:    requestID,
		Payload:      &protocolv1.Event_Vendor{Vendor: vendor},
	}
}

func TestAReDrivePromptRehydratedFromAStoreRowStillCuratesToNothing(t *testing.T) {
	// Arrange — a bare store row, with no live daemon state behind it. This is
	// what a re-pull years after the bounce actually has.
	stored := throughTheStore(t, storedUserMessageEvent(t, "u-1", resumeRequest, resumptionBodyFixture))

	// Act.
	curated, err := CurateEvent("ws", "s1", stored)

	// Assert.
	if err != nil {
		t.Fatalf("CurateEvent: %v", err)
	}
	if curated.Feed != nil {
		t.Fatalf("feed = %v, want the re-drive's instruction absent from a re-pulled row", curated.Feed.GetMessages())
	}
}

func TestTheRequestIDSurvivesTheStoresOwnRoundTrip(t *testing.T) {
	// Arrange — this is the property the filter rests on, asserted directly so
	// a schema change that dropped the field fails here rather than silently
	// un-hiding the instruction.
	original := storedUserMessageEvent(t, "u-1", resumeRequest, resumptionBodyFixture)

	// Act.
	stored := throughTheStore(t, original)

	// Assert.
	if stored.GetRequestId() != resumeRequest {
		t.Fatalf("request id = %q, want %q to be durable on the event row", stored.GetRequestId(), resumeRequest)
	}
}

func TestAUsersOwnPromptRehydratedFromAStoreRowStillCurates(t *testing.T) {
	// Arrange — the counterpart, so the round-trip test above cannot pass by
	// the curation failing for some unrelated reason.
	stored := throughTheStore(t, storedUserMessageEvent(t, "u-1", "req-7", "what is the status"))

	// Act.
	curated, err := CurateEvent("ws", "s1", stored)

	// Assert.
	if err != nil {
		t.Fatalf("CurateEvent: %v", err)
	}
	if curated.Feed == nil || len(curated.Feed.GetMessages()) != 1 {
		t.Fatalf("feed = %v, want the user's own prompt curated from a re-pulled row", curated.Feed.GetMessages())
	}
}

// resumptionBodyFixture stands in for the daemon's internal instruction. Its
// exact wording is the session controller's business; what matters here is that
// the curator drops it on the REQUEST ID rather than by recognizing the text,
// which would break the moment the wording changed.
const resumptionBodyFixture = "some daemon-internal continuation instruction"
