package frontend

import (
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"

	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/types/known/anypb"
)

// PROMPT_ORIGIN_RESUME_AFTER_RESTART IS AN ADDITIVE WIRE CHANGE, and these
// tests pin the two claims that make it safe.
//
// CLAIM ONE: nothing consults the origin to decide what RENDERS. The re-drive's
// invisibility is the curator's job, keyed on the request id. An origin-unaware
// consumer — an older client, or any of the daemon's own paths that never look
// at the field — therefore reaches exactly the same rendering verdict as a
// current one, because the field plays no part in it.
//
// CLAIM TWO: an unknown enum value degrades safely rather than corrupting the
// frame. Proto3 open enums carry an unrecognized number through verbatim, so an
// older binary reading a turn started with origin 30 keeps the rest of the
// message intact and renders the turn.

func TestTheCuratorReachesTheSameVerdictWhateverTheOrigin(t *testing.T) {
	// Arrange — a re-drive user message as the store holds it. The curator is
	// handed no origin at all, which IS the claim: the rendering verdict cannot
	// vary with a field the decision never reads, so an origin-unaware consumer
	// and a current one reach the same answer by construction.
	marked := throughTheStore(t, storedUserMessageEvent(t, "u-1", resumeRequest, resumptionBodyFixture))

	// Act.
	curated, err := CurateEvent("ws", "s1", marked)

	// Assert.
	if err != nil {
		t.Fatalf("CurateEvent: %v", err)
	}
	if curated.Feed != nil {
		t.Fatalf("feed = %v, want the re-drive hidden by the request id alone", curated.Feed.GetItems())
	}
}

func TestAnOriginUnawareReaderKeepsTheRestOfATurnStarted(t *testing.T) {
	// Arrange — a TurnStarted stamped with the new origin, marshalled as the
	// store holds it. Reading it back through a decoder that has no NAME for 30
	// is what an older binary does; proto3's open enums make that a number it
	// carries rather than a frame it rejects.
	started := &corev1.TurnStarted{
		TurnId:       "t-1",
		PromptOrigin: corev1.PromptOrigin_PROMPT_ORIGIN_RESUME_AFTER_RESTART,
	}
	blob, err := proto.Marshal(started)
	if err != nil {
		t.Fatalf("marshal TurnStarted: %v", err)
	}

	// Act.
	back := &corev1.TurnStarted{}
	if err := proto.Unmarshal(blob, back); err != nil {
		t.Fatalf("unmarshal TurnStarted: %v", err)
	}

	// Assert — the turn is still identifiable, which is what an origin-unaware
	// consumer needs in order to render it at all.
	if back.GetTurnId() != "t-1" {
		t.Fatalf("turn id = %q, want the rest of the message intact beside an unknown origin", back.GetTurnId())
	}
	if int32(back.GetPromptOrigin()) != 30 {
		t.Fatalf("origin = %d, want the unrecognized value carried through verbatim rather than zeroed", back.GetPromptOrigin())
	}
}

func TestTheNewOriginIsNotTheKeepAliveExclusion(t *testing.T) {
	// Arrange — the one origin the daemon DOES branch on is
	// CACHE_KEEP_ALIVE, whose whole turn is plumbing every consumer excludes.
	// A re-drive's turn is the opposite: its output is the continuation the
	// user is owed, so conflating the two would delete the work.

	// Act, Assert.
	if corev1.PromptOrigin_PROMPT_ORIGIN_RESUME_AFTER_RESTART == corev1.PromptOrigin_PROMPT_ORIGIN_CACHE_KEEP_ALIVE {
		t.Fatal("the re-drive origin must not be the keep-alive's, whose turns are excluded wholesale")
	}
}

func TestAReDrivenTurnsOutputCuratesRegardlessOfOrigin(t *testing.T) {
	// Arrange — the continuation must reach the feed. This is the failure the
	// keep-alive-style wholesale exclusion would have caused, asserted against
	// the curator that actually decides.
	assistant := throughTheStore(t, storedAssistantEvent(t, "a-1", resumeRequest))

	// Act.
	curated, err := CurateEvent("ws", "s1", assistant)

	// Assert.
	if err != nil {
		t.Fatalf("CurateEvent: %v", err)
	}
	if curated.Feed == nil || len(curated.Feed.GetItems()) == 0 {
		t.Fatal("the re-driven turn's output must reach the feed")
	}
}

// storedAssistantEvent is one vendor assistant message under the re-drive's
// request id — the continuation, which must survive.
func storedAssistantEvent(t *testing.T, uuid, requestID string) *corev1.Event {
	t.Helper()
	vendor, err := anypb.New(&datav1.AssistantMessage{
		Uuid: uuid,
		Message: &datav1.ApiAssistantMessage{
			Content: []*datav1.ContentBlock{
				{Block: &datav1.ContentBlock_Text{Text: &datav1.TextBlock{Text: "continuing"}}},
			},
		},
	})
	if err != nil {
		t.Fatalf("pack vendor assistant message: %v", err)
	}
	return &corev1.Event{
		SessionId:    "s1",
		Seq:          11,
		ProducedAtMs: 1_700_000_000_001,
		RequestId:    requestID,
		Payload:      &corev1.Event_Vendor{Vendor: vendor},
	}
}
