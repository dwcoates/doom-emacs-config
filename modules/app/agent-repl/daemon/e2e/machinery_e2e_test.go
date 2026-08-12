// The CLI's slash-command bookkeeping, end to end over the REAL processes: a
// transcript "user" record whose content is raw command markup is written to
// the real shim-store, fanned out to the real TS shim, forwarded to the daemon,
// and must reach a connected frontend as the COMMAND it records rather than as
// a prompt — while a real prompt written the same way still arrives as one.
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

// assertNotAPromptBubble is the guarantee both tests in this file have always
// been about: whatever the CLI's bookkeeping record becomes, it is never a
// purple prompt bubble full of markup nobody typed.
func assertNotAPromptBubble(t *testing.T, item *frontendv1.Message) {
	t.Helper()
	if um := item.GetUserMessage(); um != nil {
		t.Errorf("the CLI's slash-command bookkeeping reached the frontend AS A PROMPT uuid=%q content=%q",
			item.GetUuid(), um.GetContentString())
	}
}

// TestE2EMachineryUserLineArrivesAsAnInterceptedCommand drives the defect
// itself: the CLI's own /compact bookkeeping is written to the store BEFORE a
// real prompt line, and the frontend must be shown the bookkeeping as the
// COMMAND it records rather than as a user turn.
//
// WHAT CHANGED AND WHAT DID NOT. This test used to assert the record was
// WITHHELD outright. FROZEN-slash-command-durability.md Part 3 replaced
// withholding with classification: a slash command the CLI really ran is a fact
// about the conversation and discarding it left the feed with no account of it,
// so the record now arrives as a DaemonInterceptedCommandItem on its own
// identity. What this test was really guarding — that a machinery record never
// reaches the frontend AS A PROMPT — is untouched and is still asserted.
//
// The real line is the synchronization point, and a sound one: the store
// preserves per-session write order and the daemon curates the stream in that
// order, so once the real line's item has arrived the machinery record has
// already been through the whole pipeline. No sleeping for a guessed duration.
func TestE2EMachineryUserLineArrivesAsAnInterceptedCommand(t *testing.T) {
	// Arrange
	h := newUDSHarness(t)
	cwd := t.TempDir()
	_, conn, vendorID, store := liveSession(t, h, cwd)
	const realPrompt = "carry on with the plan"

	// Act — the bookkeeping record, then a genuine prompt behind it.
	store.write(sidecarUserLineEvent(t, vendorID, "e2e-machinery-line", machineryContent))
	store.write(sidecarUserLineEvent(t, vendorID, "e2e-real-line", realPrompt))

	// Assert — the real prompt arrives, and the machinery record that preceded
	// it arrived as the command it records and as nothing else.
	isRealPrompt := func(item *frontendv1.Message) bool {
		return item.GetUserMessage().GetContentString() == realPrompt
	}
	_, before := awaitItem(t, conn, cwd, "the real prompt's user item", isRealPrompt)
	classified := false
	for _, item := range before {
		if got := item.GetUserMessage().GetContentString(); got == machineryContent {
			t.Errorf("a user item carrying raw command markup reached the frontend: %q", got)
		}
		if item.GetUuid() != "e2e-machinery-line" {
			continue
		}
		classified = true
		assertNotAPromptBubble(t, item)
		if got := item.GetDaemonInterceptedCommand().GetCommand(); got != frontendv1.SessionCommand_SESSION_COMMAND_COMPACT {
			t.Errorf("the machinery record arrived as command %s, want SESSION_COMMAND_COMPACT — the record names /compact in its own <command-name> tag", got)
		}
		if item.GetDurable() == nil {
			t.Errorf("the machinery record is not durable — the CLI wrote a transcript record for it, so a record exists and Part 3 makes shape A durable")
		}
	}
	if !classified {
		t.Error("the CLI's slash-command bookkeeping never reached the frontend at all — Part 3 classifies it rather than discarding it, and a feed with no account of a command the user ran cannot be repaired by a reload")
	}
}

// TestE2EMachineryThatNamesNoCommandIsStillWithheld keeps the WITHHELD UNNAMED
// path covered: classification needs a command identity, and a machinery record
// that carries none (pure `<local-command-stdout>`, no `<command-name>`
// anywhere) would classify as UNSPECIFIED, which is a malformed frame. Those
// are still withheld, and this is the test that says so.
func TestE2EMachineryThatNamesNoCommandIsStillWithheld(t *testing.T) {
	// Arrange
	h := newUDSHarness(t)
	cwd := t.TempDir()
	_, conn, vendorID, store := liveSession(t, h, cwd)
	const unnamed = "<local-command-stdout>total 4\ndrwxr-xr-x</local-command-stdout>"
	const realPrompt = "and now something real"

	// Act
	store.write(sidecarUserLineEvent(t, vendorID, "e2e-machinery-unnamed", unnamed))
	store.write(sidecarUserLineEvent(t, vendorID, "e2e-real-after-unnamed", realPrompt))

	// Assert
	_, before := awaitItem(t, conn, cwd, "the real prompt's user item", func(item *frontendv1.Message) bool {
		return item.GetUserMessage().GetContentString() == realPrompt
	})
	for _, item := range before {
		if item.GetUuid() == "e2e-machinery-unnamed" {
			t.Errorf("an unnamed machinery record reached the frontend as %T — an UNSPECIFIED command is a malformed frame, so it must be withheld rather than pushed half-stated", item.GetPayload())
		}
	}
}

// TestE2EMachineryIsClassifiedOnAReplayToo covers the RESYNC path: a frontend
// reconnecting and asking for everything must be re-served the bookkeeping as
// the same intercepted command the live path delivered, and never as a prompt.
func TestE2EMachineryIsClassifiedOnAReplayToo(t *testing.T) {
	// Arrange
	h := newUDSHarness(t)
	cwd := t.TempDir()
	id, conn, vendorID, store := liveSession(t, h, cwd)
	const realPrompt = "and now the real one"
	store.write(sidecarUserLineEvent(t, vendorID, "e2e-machinery-replay", machineryContent))
	store.write(sidecarUserLineEvent(t, vendorID, "e2e-real-replay", realPrompt))
	// Both records are certainly curated once the second one's item has landed.
	awaitItem(t, conn, cwd, "the real prompt's user item", func(item *frontendv1.Message) bool {
		return item.GetUserMessage().GetContentString() == realPrompt
	})

	// Act — a fresh frontend asks for the whole conversation.
	fresh, state := dialForReplay(t, h, id, cwd)
	replayed := replayItems(t, fresh, state, cwd, "r-replay-machinery")

	// Assert
	sawReal, sawCommand := false, false
	for _, item := range replayed {
		if item.GetUuid() == "e2e-machinery-replay" {
			sawCommand = true
			assertNotAPromptBubble(t, item)
			if got := item.GetDaemonInterceptedCommand().GetCommand(); got != frontendv1.SessionCommand_SESSION_COMMAND_COMPACT {
				t.Errorf("the replayed machinery record arrived as command %s, want SESSION_COMMAND_COMPACT — a replay must re-derive the same answer for the same record", got)
			}
		}
		if item.GetUserMessage().GetContentString() == realPrompt {
			sawReal = true
		}
	}
	if !sawReal {
		t.Error("the replay carried no real prompt at all, so the assertions above prove nothing")
	}
	if !sawCommand {
		t.Error("the replay carried no intercepted-command message for the bookkeeping record — a durable command must reload as itself")
	}
}
