// The CLI's slash-command bookkeeping, end to end over the REAL processes: a
// transcript "user" record whose content is raw command markup is written to
// the real shim-store, fanned out to the real TS shim, forwarded to the daemon,
// and must reach a connected frontend as NOTHING — while a real prompt written
// the same way still arrives.
//
// WHY THE RECORDS ARE INJECTED RATHER THAN PROVOKED. Same reason as
// clearcompact_e2e_test.go, whose helpers this file reuses READ-ONLY
// (liveSession, storeProducer.write, awaitItem, deltaItems): the sidecar is the
// sole producer of file-plane records and it produces them by tailing a real
// vendor transcript, which the `--fake` harness has none of. So the test writes
// to the store exactly the event shape the sidecar writes for a transcript line
// (handler.vendorEvent: file plane, PERSISTENT, no dedup key — the store
// derives the uuid: key itself) and exercises everything downstream for real.
package e2e

import (
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"google.golang.org/protobuf/types/known/anypb"
)

// machineryContent is a VERBATIM head of the record the Claude CLI appends when
// a slash command runs: type "user", not flagged isMeta, content that is
// bookkeeping markup rather than anything a user typed.
const machineryContent = "<command-message>compact</command-message>\n" +
	"<command-name>/compact</command-name>\n" +
	"<command-args></command-args>"

// sidecarUserLineEvent is what handler.vendorEvent builds for a transcript
// `user` line: the vendor Any carries datav1.TranscriptLine, the plane is FILE,
// the class PERSISTENT, and the dedup key is left EMPTY so the store derives
// its own uuid: key (handler.go §"dedupKey is set only where the store cannot
// derive it").
func sidecarUserLineEvent(t *testing.T, vendorSessionID, lineUUID, content string) *corev1.Event {
	t.Helper()
	a, err := anypb.New(&datav1.TranscriptLine{
		Line: &datav1.TranscriptLine_User{User: &datav1.UserLine{
			Envelope: &datav1.LineEnvelope{Uuid: lineUUID},
			Message: &datav1.ApiUserMessage{
				Content: &datav1.ApiUserMessage_ContentString{ContentString: content},
			},
		}},
	})
	if err != nil {
		t.Fatalf("anypb.New: %v", err)
	}
	return &corev1.Event{
		SessionId:    vendorSessionID,
		Plane:        corev1.Plane_PLANE_FILE,
		Class:        corev1.EventClass_EVENT_CLASS_PERSISTENT,
		ProducedAtMs: time.Now().UnixMilli(),
		Payload:      &corev1.Event_Vendor{Vendor: a},
	}
}

// TestE2EMachineryUserLineNeverReachesTheFrontend drives the defect itself: the
// CLI's own /compact bookkeeping is written to the store BEFORE a real prompt
// line, and the frontend must be shown only the real one.
//
// The real line is the synchronization point, and a sound one: the store
// preserves per-session write order and the daemon curates the stream in that
// order, so once the real line's item has arrived the machinery record has
// already been through the whole pipeline. No sleeping for a guessed duration.
func TestE2EMachineryUserLineNeverReachesTheFrontend(t *testing.T) {
	// Arrange
	h := newUDSHarness(t)
	cwd := t.TempDir()
	_, conn, vendorID, store := liveSession(t, h, cwd)
	const realPrompt = "carry on with the plan"

	// Act — the bookkeeping record, then a genuine prompt behind it.
	store.write(sidecarUserLineEvent(t, vendorID, "e2e-machinery-line", machineryContent))
	store.write(sidecarUserLineEvent(t, vendorID, "e2e-real-line", realPrompt))

	// Assert — the real prompt arrives, and nothing that arrived before it is
	// the machinery record.
	isRealPrompt := func(item *frontendv1.ConversationItem) bool {
		return item.GetUserMessage().GetContentString() == realPrompt
	}
	_, before := awaitItem(t, conn, cwd, "the real prompt's user item", isRealPrompt)
	for _, item := range before {
		if item.GetUuid() == "e2e-machinery-line" {
			t.Errorf("the CLI's slash-command bookkeeping reached the frontend as item uuid=%q", item.GetUuid())
		}
		if got := item.GetUserMessage().GetContentString(); got == machineryContent {
			t.Errorf("a user item carrying raw command markup reached the frontend: %q", got)
		}
	}
}

// TestE2EMachineryIsWithheldFromAReplayToo covers the RESYNC path: a frontend
// reconnecting and asking for everything must not be re-served the bookkeeping
// the live path already withheld.
func TestE2EMachineryIsWithheldFromAReplayToo(t *testing.T) {
	// Arrange
	h := newUDSHarness(t)
	cwd := t.TempDir()
	id, conn, vendorID, store := liveSession(t, h, cwd)
	const realPrompt = "and now the real one"
	store.write(sidecarUserLineEvent(t, vendorID, "e2e-machinery-replay", machineryContent))
	store.write(sidecarUserLineEvent(t, vendorID, "e2e-real-replay", realPrompt))
	// Both records are certainly curated once the second one's item has landed.
	awaitItem(t, conn, cwd, "the real prompt's user item", func(item *frontendv1.ConversationItem) bool {
		return item.GetUserMessage().GetContentString() == realPrompt
	})

	// Act — a fresh frontend asks for the whole conversation.
	fresh, state := dialForReplay(t, h, id, cwd)
	replayed := replayItems(t, fresh, state, cwd, "r-replay-machinery")

	// Assert
	sawReal := false
	for _, item := range replayed {
		if item.GetUuid() == "e2e-machinery-replay" {
			t.Errorf("the replay carried the CLI's bookkeeping record uuid=%q", item.GetUuid())
		}
		if item.GetUserMessage().GetContentString() == realPrompt {
			sawReal = true
		}
	}
	if !sawReal {
		t.Error("the replay carried no real prompt at all, so the assertion above proves nothing")
	}
}
