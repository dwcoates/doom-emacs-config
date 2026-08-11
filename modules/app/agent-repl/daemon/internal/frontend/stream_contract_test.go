package frontend

import (
	"encoding/json"
	"os"
	"strings"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/ssm"

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
		got := TypingDeltaFromContentDelta(fx.Workspace, fx.SessionID, "", want.GetDelta())

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
		got := TypingDeltaFromContentDelta(fx.Workspace, fx.SessionID, "", td.GetDelta())
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
			// Each fixture item is one SDK assistant record. Reasoning arrives
			// as its own emission under a uuid DERIVED from the envelope's, so
			// the envelope uuid is recovered by cutting the derivation off, and
			// the record itself is rebuilt from whichever arm the item carries.
			envelopeUUID, _, _ := strings.Cut(item.GetUuid(), "#")
			// Act: rebuild the event the shim delivered and translate it.
			ev := &corev1.Event{
				SessionId:    fx.SessionID,
				ProducedAtMs: producedMs,
				Payload: &corev1.Event_Vendor{Vendor: mustAny(t, &datav1.AssistantMessage{
					Uuid:    envelopeUUID,
					Message: recordBodyFor(t, item, fx.MessageID),
				})},
			}
			got, _, err := conversationDeltaFromEvent(fx.Workspace, "", ev)
			if err != nil {
				t.Fatalf("conversationDeltaFromEvent: %v", err)
			}

			// Assert
			if len(got.GetItems()) != 1 {
				t.Fatalf("got %d items for one assistant record, want 1", len(got.GetItems()))
			}
			out := got.GetItems()[0]
			if out.GetUuid() != item.GetUuid() {
				t.Errorf("item uuid = %q, want %q", out.GetUuid(), item.GetUuid())
			}
			if out.GetAgent().GetThinking() == nil {
				if id := out.GetAgent().GetResponse().GetBody().GetId(); id != fx.MessageID {
					t.Errorf("message id = %q, want %q", id, fx.MessageID)
				}
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

// TestStreamContractThinkingNamesThePreviewItSettlesOnto pins the daemon half
// of the settle: a reasoning emission states the message it was stripped from
// and the slot it held, which is the very pair the fixture's typing deltas key
// their preview on. Drift here leaves the settled block rendering beside its
// own preview instead of replacing it.
func TestStreamContractThinkingNamesThePreviewItSettlesOnto(t *testing.T) {
	// Arrange
	fx, frames := loadStreamContract(t)
	seen := 0

	for _, frame := range frames {
		cd := frame.GetConversationDelta()
		if cd == nil {
			continue
		}
		for _, item := range cd.GetItems() {
			want := item.GetAgent().GetThinking()
			if want == nil {
				continue
			}
			seen++
			envelopeUUID, _, _ := strings.Cut(item.GetUuid(), "#")
			ev := &corev1.Event{
				SessionId:    fx.SessionID,
				ProducedAtMs: producedMs,
				Payload: &corev1.Event_Vendor{Vendor: mustAny(t, &datav1.AssistantMessage{
					Uuid:    envelopeUUID,
					Message: recordBodyFor(t, item, fx.MessageID),
				})},
			}

			// Act
			got, _, err := conversationDeltaFromEvent(fx.Workspace, fx.SessionID, ev)
			if err != nil {
				t.Fatalf("conversationDeltaFromEvent: %v", err)
			}

			// Assert
			th := got.GetItems()[0].GetAgent().GetThinking()
			if th.GetApiMessageId() != want.GetApiMessageId() || th.GetBlockIndex() != want.GetBlockIndex() {
				t.Errorf("stated origin = (%q, %d), want (%q, %d)",
					th.GetApiMessageId(), th.GetBlockIndex(), want.GetApiMessageId(), want.GetBlockIndex())
			}
		}
	}

	if seen == 0 {
		t.Fatal("fixture declared no thinking emissions; the contract would assert nothing")
	}
}

// TestStreamContractOneStreamCarriesOneFence pins FENCE CONTINUITY across the
// two frame kinds one streamed message produces.
//
// The fence is a workspace's staleness token, compared byte-wise and never
// parsed. A preview and the record that settles onto it are two frames of ONE
// stream, so they must carry the SAME token: if the typing frames and the
// conversation frames ever disagreed, a client gating on the fence would drop
// exactly one half of a message and render the other half forever.
//
// This is asserted at the DAEMON because the daemon composes the fence — the
// shim never sees one, and the webapp only decodes what it is handed.
func TestStreamContractOneStreamCarriesOneFence(t *testing.T) {
	// Arrange: one workspace's identities, minted into one token exactly as the
	// session controller mints the token it stamps on every push.
	fx, frames := loadStreamContract(t)
	fence := ssm.Fence(fx.SessionID, "gen_01")
	seenTyping, seenConversation := 0, 0

	for _, frame := range frames {
		// Act / Assert: the typing hop.
		if td := frame.GetTypingDelta(); td != nil {
			seenTyping++
			got := TypingDeltaFromContentDelta(fx.Workspace, fence, "", td.GetDelta())
			if got.GetFence() != fence {
				t.Errorf("typing frame fence = %q, want %q", got.GetFence(), fence)
			}
		}
		// Act / Assert: the durable hop, from the record the same message produced.
		cd := frame.GetConversationDelta()
		if cd == nil {
			continue
		}
		for _, item := range cd.GetItems() {
			seenConversation++
			envelopeUUID, _, _ := strings.Cut(item.GetUuid(), "#")
			ev := &corev1.Event{
				SessionId:    fx.SessionID,
				ProducedAtMs: producedMs,
				Payload: &corev1.Event_Vendor{Vendor: mustAny(t, &datav1.AssistantMessage{
					Uuid:    envelopeUUID,
					Message: recordBodyFor(t, item, fx.MessageID),
				})},
			}
			got, _, err := conversationDeltaFromEvent(fx.Workspace, fence, ev)
			if err != nil {
				t.Fatalf("conversationDeltaFromEvent: %v", err)
			}
			if got.GetFence() != fence {
				t.Errorf("conversation frame fence = %q, want %q", got.GetFence(), fence)
			}
		}
	}

	if seenTyping == 0 || seenConversation == 0 {
		t.Fatalf("fixture declared %d typing and %d conversation frames; continuity needs both", seenTyping, seenConversation)
	}
}

// TestStreamContractTheStreamFenceResolvesToItsOwnIdentities pins the other end
// of the continuity: the token both hops carry is the one this daemon minted,
// so it resolves back to the identities it was composed from rather than being
// an opaque string that merely happens to match itself.
func TestStreamContractTheStreamFenceResolvesToItsOwnIdentities(t *testing.T) {
	// Arrange
	fx, _ := loadStreamContract(t)
	fence := ssm.Fence(fx.SessionID, "gen_01")

	// Act
	sessionID, generationID := ssm.SplitFence(fence)

	// Assert
	if sessionID != fx.SessionID || generationID != "gen_01" {
		t.Errorf("SplitFence(%q) = (%q, %q), want (%q, %q)", fence, sessionID, generationID, fx.SessionID, "gen_01")
	}
}

// recordBodyFor rebuilds the ApiAssistantMessage the SDK delivered for one
// fixture item. A response item carries it directly; a thinking item carries
// only the block, because the emission is what the reasoning became once the
// daemon stripped it out of the body.
func recordBodyFor(t *testing.T, item *frontendv1.ConversationItem, messageID string) *datav1.ApiAssistantMessage {
	t.Helper()
	if body := item.GetAgent().GetResponse().GetBody(); body != nil {
		return body
	}
	thinking := item.GetAgent().GetThinking().GetBody()
	if thinking == nil {
		t.Fatalf("fixture item %q carries neither a response nor a thinking emission", item.GetUuid())
	}
	return &datav1.ApiAssistantMessage{
		Id: messageID,
		Content: []*datav1.ContentBlock{
			{Block: &datav1.ContentBlock_Thinking{Thinking: thinking}},
		},
	}
}
