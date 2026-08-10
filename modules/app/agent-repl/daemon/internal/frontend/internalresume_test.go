package frontend

import (
	"testing"

	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// THE DAEMON'S OWN RE-DRIVE MUST BE UNRENDERABLE.
//
// A bounce that has to kill the shim re-drives the interrupted turn by
// submitting an instruction the user never wrote. That instruction is a real
// line in the vendor transcript — it has to be, or the model would not act on
// it — so it arrives on the live push, on a connect snapshot's backfill, on a
// resync replay and on a store re-pull alike.
//
// All four are the SAME curator, which is the point: these tests pin the
// curator's verdict, and every one of those routes inherits it.

// resumeUserItem is a curated user message with the given request id.
func resumeUserItem(uuid, requestID string) *frontendv1.ConversationItem {
	return &frontendv1.ConversationItem{
		Uuid:      uuid,
		RequestId: requestID,
		Source:    frontendv1.ConversationSource_CONVERSATION_SOURCE_USER,
		Item: &frontendv1.ConversationItem_UserMessage{
			UserMessage: &datav1.ApiUserMessage{},
		},
	}
}

// resumeAgentItem is a curated assistant emission with the given request id — the
// re-driven turn's actual work.
func resumeAgentItem(uuid, requestID string) *frontendv1.ConversationItem {
	return &frontendv1.ConversationItem{
		Uuid:      uuid,
		RequestId: requestID,
		Source:    frontendv1.ConversationSource_CONVERSATION_SOURCE_USER,
		Item:      &frontendv1.ConversationItem_Agent{Agent: &frontendv1.AgentEmission{}},
	}
}

// resumeItemUUIDs names what survived a curation, for assertions that read.
func resumeItemUUIDs(items []*frontendv1.ConversationItem) []string {
	var out []string
	for _, item := range items {
		out = append(out, item.GetUuid())
	}
	return out
}

const resumeRequest = InternalResumeRequestIDPrefix + "/ws/t-1"

func TestTheInternalResumePromptIsDroppedFromTheConversation(t *testing.T) {
	// Arrange.
	items := []*frontendv1.ConversationItem{resumeUserItem("u-1", resumeRequest)}

	// Act.
	got := dropInternalResumePrompt(items)

	// Assert.
	if len(got) != 0 {
		t.Fatalf("items = %v, want the internal instruction removed", resumeItemUUIDs(got))
	}
}

func TestTheReDrivenTurnsOutputSurvivesTheDrop(t *testing.T) {
	// Arrange — the output IS the continuation of the work the user asked for.
	// Filtering the whole request id would delete the work along with the ask.
	items := []*frontendv1.ConversationItem{
		resumeUserItem("u-1", resumeRequest),
		resumeAgentItem("a-1", resumeRequest),
	}

	// Act.
	got := dropInternalResumePrompt(items)

	// Assert.
	if len(got) != 1 || got[0].GetUuid() != "a-1" {
		t.Fatalf("items = %v, want only the continuation output kept", resumeItemUUIDs(got))
	}
}

func TestAnOrdinaryUserMessageIsUntouched(t *testing.T) {
	// Arrange.
	items := []*frontendv1.ConversationItem{resumeUserItem("u-1", "req-7")}

	// Act.
	got := dropInternalResumePrompt(items)

	// Assert.
	if len(got) != 1 || got[0].GetUuid() != "u-1" {
		t.Fatalf("items = %v, want the user's own prompt kept", resumeItemUUIDs(got))
	}
}

func TestAUserMessageWithNoRequestIDIsUntouched(t *testing.T) {
	// Arrange — plenty of events carry no request id, and reading absence as
	// "internal" would hide arbitrary user content.
	items := []*frontendv1.ConversationItem{resumeUserItem("u-1", "")}

	// Act.
	got := dropInternalResumePrompt(items)

	// Assert.
	if len(got) != 1 {
		t.Fatalf("items = %v, want an unattributed user message kept", resumeItemUUIDs(got))
	}
}

func TestARequestIDMerelyContainingThePrefixIsUntouched(t *testing.T) {
	// Arrange — the marker is a PREFIX, minted by the daemon at teardown. A id
	// that merely mentions it somewhere is not one the daemon minted.
	items := []*frontendv1.ConversationItem{
		resumeUserItem("u-1", "req-7:"+InternalResumeRequestIDPrefix+"x"),
	}

	// Act.
	got := dropInternalResumePrompt(items)

	// Assert.
	if len(got) != 1 {
		t.Fatalf("items = %v, want a non-prefixed id kept", resumeItemUUIDs(got))
	}
}

func TestIsInternalResumeRequestIDRejectsAnEmptyID(t *testing.T) {
	// Arrange, Act.
	got := IsInternalResumeRequestID("")

	// Assert.
	if got {
		t.Fatal("an empty request id must never read as an internal re-drive")
	}
}

func TestIsInternalResumeRequestIDAcceptsAMintedID(t *testing.T) {
	// Arrange, Act.
	got := IsInternalResumeRequestID(resumeRequest)

	// Assert.
	if !got {
		t.Fatalf("%q must read as an internal re-drive", resumeRequest)
	}
}
