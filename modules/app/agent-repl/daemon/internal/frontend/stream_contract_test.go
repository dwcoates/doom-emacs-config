package frontend

import (
	"encoding/json"
	"os"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"google.golang.org/protobuf/encoding/protojson"
	"google.golang.org/protobuf/proto"
)

// The streaming identity CONTRACT, asserted at the daemon hop.
//
// The daemon is a CARRIER of a streamed message's identity, never a second
// source of it: the shim chooses the id (it is the only hop that sees the
// message_start naming it) and the webapp reconciles on what arrives. This
// suite pins that the daemon changes neither half, against the same fixture the
// shim and webapp suites read — so a hop that starts re-deriving identity fails
// in CI rather than rendering a stalled preview beside its final card.
//
// See the fixture's $comment for why its message deliberately holds two blocks.

const streamContractFixture = "../../../testdata/stream-contract/one-message.json"

type streamContract struct {
	Workspace      string            `json:"workspace"`
	SessionID      string            `json:"sessionId"`
	MessageID      string            `json:"messageId"`
	TsMs           string            `json:"tsMs"`
	FrontendFrames []json.RawMessage `json:"frontendFrames"`
}

// loadStreamContract reads the shared fixture and decodes each declared wire
// frame as real protojson, which also proves the fixture IS a valid frame
// stream rather than a hand-written approximation of one.
func loadStreamContract(t *testing.T) (streamContract, []*frontendv1.FrontendFrame) {
	t.Helper()
	raw, err := os.ReadFile(streamContractFixture)
	if err != nil {
		t.Fatalf("read fixture: %v", err)
	}
	var fx streamContract
	if err := json.Unmarshal(raw, &fx); err != nil {
		t.Fatalf("unmarshal fixture: %v", err)
	}
	frames := make([]*frontendv1.FrontendFrame, 0, len(fx.FrontendFrames))
	for i, rf := range fx.FrontendFrames {
		var frame frontendv1.FrontendFrame
		if err := protojson.Unmarshal(rf, &frame); err != nil {
			t.Fatalf("fixture frame %d is not valid FrontendFrame protojson: %v", i, err)
		}
		frames = append(frames, &frame)
	}
	return fx, frames
}

// TestStreamContractTypingPassthrough pins that the daemon relays a shim
// ContentDelta to the frontend UNCHANGED — same message id, same block index,
// same arm. Re-typing or re-keying it here would give the preview an identity
// the finished record could never be matched against.
func TestStreamContractTypingPassthrough(t *testing.T) {
	// Arrange
	fx, frames := loadStreamContract(t)
	seen := 0

	for _, frame := range frames {
		want := frame.GetTypingDelta()
		if want == nil {
			continue
		}
		seen++

		// Act: hand the daemon exactly the ContentDelta the shim emitted.
		got := TypingDeltaFromContentDelta(fx.Workspace, fx.SessionID, want.GetDelta())

		// Assert
		if !proto.Equal(got, want) {
			t.Errorf("TypingDelta not relayed unchanged:\n got %v\nwant %v", got, want)
		}
	}
	if seen == 0 {
		t.Fatal("fixture declared no typing frames; the contract would assert nothing")
	}
}

// TestStreamContractTypingKeepsMessageID pins the identity itself: every delta
// of one message carries that message's id, not a per-event envelope uuid.
func TestStreamContractTypingKeepsMessageID(t *testing.T) {
	// Arrange
	fx, frames := loadStreamContract(t)

	for _, frame := range frames {
		td := frame.GetTypingDelta()
		if td == nil {
			continue
		}
		// Act
		got := TypingDeltaFromContentDelta(fx.Workspace, fx.SessionID, td.GetDelta())
		// Assert
		if got.GetDelta().GetUuid() != fx.MessageID {
			t.Errorf("delta uuid = %q, want the streamed message id %q", got.GetDelta().GetUuid(), fx.MessageID)
		}
	}
}

// TestStreamContractRecordKeepsEnvelopeUUID pins the other half of the
// asymmetry the contract rests on: each finished record keeps its OWN envelope
// uuid (what tells the two records apart) while repeating the shared message id
// (what ties them to one stream). Collapsing either one is what made a
// multi-block message render twice.
func TestStreamContractRecordKeepsEnvelopeUUID(t *testing.T) {
	// Arrange
	fx, frames := loadStreamContract(t)
	uuids := map[string]bool{}
	seen := 0

	for _, frame := range frames {
		cd := frame.GetConversationDelta()
		if cd == nil {
			continue
		}
		for _, item := range cd.GetItems() {
			seen++
			// Act: rebuild the event the shim delivered and translate it.
			ev := &corev1.Event{
				SessionId:    fx.SessionID,
				ProducedAtMs: producedMs,
				Payload: &corev1.Event_Vendor{Vendor: mustAny(t, &datav1.AssistantMessage{
					Uuid:    item.GetUuid(),
					Message: item.GetAssistantMessage(),
				})},
			}
			got, err := ConversationDeltaFromEvent(fx.Workspace, ev)
			if err != nil {
				t.Fatalf("ConversationDeltaFromEvent: %v", err)
			}

			// Assert
			if len(got.GetItems()) != 1 {
				t.Fatalf("got %d items for one assistant record, want 1", len(got.GetItems()))
			}
			out := got.GetItems()[0]
			if out.GetUuid() != item.GetUuid() {
				t.Errorf("item uuid = %q, want the envelope uuid %q", out.GetUuid(), item.GetUuid())
			}
			if id := out.GetAssistantMessage().GetId(); id != fx.MessageID {
				t.Errorf("message id = %q, want %q", id, fx.MessageID)
			}
			uuids[out.GetUuid()] = true
		}
	}

	if seen == 0 {
		t.Fatal("fixture declared no conversation items; the contract would assert nothing")
	}
	if len(uuids) != seen {
		t.Errorf("got %d distinct record identities for %d records; they must not collide", len(uuids), seen)
	}
}
