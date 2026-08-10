package frontend

import (
	"strings"
	"testing"

	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// THE MARKER IS THE INVISIBILITY THAT SURVIVES THE STORE.
//
// A vendor user line reaches the store through the file plane, which names no
// request id: the daemon's correlation is in-memory, and a re-drive — which
// mints no receipt — gets none at all. So the identity has to ride the message
// itself. These tests pin both halves of that: a marked instruction is
// suppressed wherever it turns up, and a message the user wrote is not, however
// closely it resembles one.

// markedUserItem is a curated user message whose BODY carries the marker and
// whose event request id is empty — exactly the shape a file-plane line has.
func markedUserItem(uuid, requestID string) *frontendv1.ConversationItem {
	return &frontendv1.ConversationItem{
		Uuid:   uuid,
		Source: frontendv1.ConversationSource_CONVERSATION_SOURCE_USER,
		Item: &frontendv1.ConversationItem_UserMessage{
			UserMessage: &datav1.ApiUserMessage{
				Content: &datav1.ApiUserMessage_ContentString{
					ContentString: MarkInternalResumeInstruction(requestID, "continue the interrupted work"),
				},
			},
		},
	}
}

// stringUserItem is a curated user message carrying body verbatim, with no
// request id — the unattributed shape every replayed line has.
func stringUserItem(uuid, body string) *frontendv1.ConversationItem {
	return &frontendv1.ConversationItem{
		Uuid:   uuid,
		Source: frontendv1.ConversationSource_CONVERSATION_SOURCE_USER,
		Item: &frontendv1.ConversationItem_UserMessage{
			UserMessage: &datav1.ApiUserMessage{
				Content: &datav1.ApiUserMessage_ContentString{ContentString: body},
			},
		},
	}
}

// blockUserItem is a curated user message whose body is content BLOCKS, whose
// first block carries body.
func blockUserItem(uuid, body string) *frontendv1.ConversationItem {
	return &frontendv1.ConversationItem{
		Uuid:   uuid,
		Source: frontendv1.ConversationSource_CONVERSATION_SOURCE_USER,
		Item: &frontendv1.ConversationItem_UserMessage{
			UserMessage: &datav1.ApiUserMessage{
				Content: &datav1.ApiUserMessage_ContentBlocks{
					ContentBlocks: &datav1.ApiContentBlocks{Blocks: []*datav1.ContentBlock{{
						Block: &datav1.ContentBlock_Text{Text: &datav1.TextBlock{Text: body}},
					}}},
				},
			},
		},
	}
}

func TestAMarkedInstructionWithNoRequestIDIsStillDropped(t *testing.T) {
	// Arrange — the incident's exact shape: the transcript's line, replayed,
	// with nothing but its own body to identify it by.
	items := []*frontendv1.ConversationItem{markedUserItem("u-1", resumeRequest)}

	// Act.
	got, _ := dropInternalResumePrompt(items)

	// Assert.
	if len(got) != 0 {
		t.Fatalf("items = %v, want the marked instruction removed with no request id present", resumeItemUUIDs(got))
	}
}

func TestAMarkedInstructionInBlockFormIsDropped(t *testing.T) {
	// Arrange — the vendor may normalize a submitted string into one text block.
	body := MarkInternalResumeInstruction(resumeRequest, "continue the interrupted work")
	items := []*frontendv1.ConversationItem{blockUserItem("u-1", body)}

	// Act.
	got, _ := dropInternalResumePrompt(items)

	// Assert.
	if len(got) != 0 {
		t.Fatalf("items = %v, want the marked instruction removed in block form too", resumeItemUUIDs(got))
	}
}

func TestTheDropReportsTheSuppressedReDriveID(t *testing.T) {
	// Arrange — the report is the delivery evidence the resumption is
	// discharged off, so it is asserted rather than assumed.
	items := []*frontendv1.ConversationItem{markedUserItem("u-1", resumeRequest)}

	// Act.
	_, suppressed := dropInternalResumePrompt(items)

	// Assert.
	if len(suppressed) != 1 || suppressed[0] != resumeRequest {
		t.Fatalf("suppressed = %v, want exactly %q", suppressed, resumeRequest)
	}
}

func TestAnOrdinaryPromptSuppressesNothing(t *testing.T) {
	// Arrange — the counterpart: nothing to discharge means nothing reported.
	items := []*frontendv1.ConversationItem{stringUserItem("u-1", "what is the status")}

	// Act.
	_, suppressed := dropInternalResumePrompt(items)

	// Assert.
	if len(suppressed) != 0 {
		t.Fatalf("suppressed = %v, want none for a prompt the user wrote", suppressed)
	}
}

func TestAPromptQuotingTheInstructionIsKept(t *testing.T) {
	// Arrange — the rule the whole design turns on: suppression keys on the
	// marker, never on the instruction's wording.
	items := []*frontendv1.ConversationItem{stringUserItem("u-1",
		"Your previous turn was interrupted by a planned restart of the tooling — why did that happen?")}

	// Act.
	got, _ := dropInternalResumePrompt(items)

	// Assert.
	if len(got) != 1 {
		t.Fatalf("items = %v, want a prompt that merely quotes the instruction kept", resumeItemUUIDs(got))
	}
}

func TestAPromptMentioningTheMarkerBelowItsFirstLineIsKept(t *testing.T) {
	// Arrange — a user pasting a daemon log into their prompt. The marker is
	// structural: it opens the message or it is somebody else's text.
	body := "look at this line from the log:\n" + MarkInternalResumeInstruction(resumeRequest, "continue")
	items := []*frontendv1.ConversationItem{stringUserItem("u-1", body)}

	// Act.
	got, _ := dropInternalResumePrompt(items)

	// Assert.
	if len(got) != 1 {
		t.Fatalf("items = %v, want a prompt quoting the marker mid-message kept", resumeItemUUIDs(got))
	}
}

func TestAnUnclosedMarkerIsKept(t *testing.T) {
	// Arrange — the marker must close on its own first line, so a truncated
	// opener is not a marker.
	items := []*frontendv1.ConversationItem{stringUserItem("u-1",
		internalResumeMarkerOpen+resumeRequest+"\nand then some text")}

	// Act.
	got, _ := dropInternalResumePrompt(items)

	// Assert.
	if len(got) != 1 {
		t.Fatalf("items = %v, want an unclosed marker treated as ordinary text", resumeItemUUIDs(got))
	}
}

func TestAMarkerCarryingSomeoneElsesRequestIDIsKept(t *testing.T) {
	// Arrange — the bracketed id must itself be a re-drive's, so a marker
	// naming an ordinary request cannot hide that request's prompt.
	items := []*frontendv1.ConversationItem{stringUserItem("u-1",
		internalResumeMarkerOpen+"req-7"+internalResumeMarkerClose+"\nplease do the thing")}

	// Act.
	got, _ := dropInternalResumePrompt(items)

	// Assert.
	if len(got) != 1 {
		t.Fatalf("items = %v, want a marker naming a non-re-drive id ignored", resumeItemUUIDs(got))
	}
}

func TestAMarkedInstructionCarriesTheInstructionAfterTheMarkerLine(t *testing.T) {
	// Arrange — the model has to receive the instruction itself, not only the
	// marker, or the re-drive says nothing.
	const instruction = "continue the interrupted work"

	// Act.
	got := MarkInternalResumeInstruction(resumeRequest, instruction)

	// Assert.
	if !strings.HasSuffix(got, "\n"+instruction) {
		t.Fatalf("marked text = %q, want the instruction to follow the marker line", got)
	}
}

func TestMarkingANonReDriveRequestIDPanics(t *testing.T) {
	// Arrange — marking someone else's request would make an arbitrary submit
	// invisible, which is worse than the visible re-drive this exists to fix.
	defer func() {
		if recover() == nil {
			t.Fatal("marking a non-re-drive request id must fail hard")
		}
	}()

	// Act, Assert (in the deferred recover).
	MarkInternalResumeInstruction("req-7", "continue")
}

func TestMarkedInternalResumeRequestIDReadsBackTheMintedID(t *testing.T) {
	// Arrange.
	marked := MarkInternalResumeInstruction(resumeRequest, "continue")

	// Act.
	got := MarkedInternalResumeRequestID(marked)

	// Assert.
	if got != resumeRequest {
		t.Fatalf("request id = %q, want %q", got, resumeRequest)
	}
}
