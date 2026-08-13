package frontend

import (
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"
)

// commandBody is a minimal daemon-handled command message: the exact shape the
// ephemeral class was drawn around, with no lineage and no durability yet.
func commandBody(uuid string) *frontendv1.Message {
	return &frontendv1.Message{
		Uuid: uuid,
		Payload: &frontendv1.Message_DaemonInterceptedCommand{
			DaemonInterceptedCommand: &frontendv1.DaemonInterceptedCommandItem{
				Command: frontendv1.SessionCommand_SESSION_COMMAND_MODEL,
			},
		},
	}
}

// durableParent is a classified durable feed row, built through the constructor
// so the test cannot accidentally assert against a parent that would itself be
// refused.
func durableParent(t *testing.T, uuid string) *frontendv1.Message {
	t.Helper()
	parent, err := NewDurableFeedRow(commandBody(uuid))
	if err != nil {
		t.Fatalf("NewDurableFeedRow(%q) = err %v, want a durable feed row", uuid, err)
	}
	return parent
}

// TestNewEphemeralFeedRowClassifiesAndSelfRoots is the positive case: a valid
// ephemeral construction sets the ephemeral arm AND the self-referential
// lineage rule 1 states, in one act rather than in two the caller must pair.
func TestNewEphemeralFeedRowClassifiesAndSelfRoots(t *testing.T) {
	// Arrange.
	body := commandBody("session-command:req-1")

	// Act.
	got, err := NewEphemeralFeedRow(body)

	// Assert.
	if err != nil {
		t.Fatalf("NewEphemeralFeedRow = err %v, want a classified message", err)
	}
	if got.GetEphemeral() == nil {
		t.Fatalf("durability arm = %T, want the ephemeral arm set", got.GetDurability())
	}
	if got.GetDurable() != nil {
		t.Fatalf("durable arm is set on an ephemeral message, which claims a store record that will never exist")
	}
	if root := got.GetLineage().GetTopLevelMessageId(); root != got.GetUuid() {
		t.Fatalf("top_level_message_id = %q, want the message's own uuid %q — an ephemeral message is always a feed row", root, got.GetUuid())
	}
	if parent := got.GetLineage().GetParentMessageId(); parent != "" {
		t.Fatalf("parent_message_id = %q, want empty — an ephemeral message never names a parent", parent)
	}
}

// TestNewEphemeralFeedRowRefusesNamingADurableParent is rule 3: an ephemeral
// card may not attach itself into a paged conversation it will vanish from.
func TestNewEphemeralFeedRowRefusesNamingADurableParent(t *testing.T) {
	// Arrange.
	parent := durableParent(t, "durable-1")
	body := commandBody("session-command:req-2")
	body.Lineage = &frontendv1.MessageLineage{
		TopLevelMessageId: parent.GetUuid(),
		ParentMessageId:   parent.GetUuid(),
	}

	// Act.
	got, err := NewEphemeralFeedRow(body)

	// Assert.
	if err == nil {
		t.Fatalf("NewEphemeralFeedRow = %+v, want a refusal: an ephemeral message may not name a durable parent", got)
	}
	if got != nil {
		t.Fatalf("a refused construction returned a message %+v, want nil", got)
	}
	if !strings.Contains(err.Error(), "NEVER names a parent") {
		t.Fatalf("refusal = %q, want it to name the parent rule that was broken", err)
	}
}

// TestNewEphemeralFeedRowRefusesANonFeedRowRoot is rule 1 from the other side:
// a body naming someone else's root claims membership in a feed row that will
// outlive it, even though it names no parent.
func TestNewEphemeralFeedRowRefusesANonFeedRowRoot(t *testing.T) {
	// Arrange.
	body := commandBody("session-command:req-3")
	body.Lineage = &frontendv1.MessageLineage{TopLevelMessageId: "some-other-root"}

	// Act.
	got, err := NewEphemeralFeedRow(body)

	// Assert.
	if err == nil {
		t.Fatalf("NewEphemeralFeedRow = %+v, want a refusal: an ephemeral message's root is its own uuid", got)
	}
	if !strings.Contains(err.Error(), "some-other-root") {
		t.Fatalf("refusal = %q, want it to name the offending root", err)
	}
}

// TestNewDurableChildRefusesAnEphemeralParent is rule 2: the child would be
// written to the store naming a feed row no store row holds, so no page query
// could ever return it.
func TestNewDurableChildRefusesAnEphemeralParent(t *testing.T) {
	// Arrange.
	parent, err := NewEphemeralFeedRow(commandBody("session-command:req-4"))
	if err != nil {
		t.Fatalf("NewEphemeralFeedRow = err %v, want an ephemeral parent to test against", err)
	}
	child := commandBody("child-1")

	// Act.
	got, err := NewDurableChild(parent, child)

	// Assert.
	if err == nil {
		t.Fatalf("NewDurableChild = %+v, want a refusal: a durable child may not name an ephemeral parent", got)
	}
	if got != nil {
		t.Fatalf("a refused construction returned a message %+v, want nil", got)
	}
	if !strings.Contains(err.Error(), "EPHEMERAL") {
		t.Fatalf("refusal = %q, want it to say the parent is ephemeral", err)
	}
}

// TestNewDurableChildRefusesAnUnclassifiedParent keeps the rule-2 refusal from
// being sidestepped by a parent that simply states nothing: an unclassified
// parent is not assumed durable.
func TestNewDurableChildRefusesAnUnclassifiedParent(t *testing.T) {
	// Arrange.
	parent := commandBody("unclassified-1")
	parent.Lineage = FeedRowLineage(parent.GetUuid())

	// Act.
	got, err := NewDurableChild(parent, commandBody("child-2"))

	// Assert.
	if err == nil {
		t.Fatalf("NewDurableChild = %+v, want a refusal: the parent states no durability class", got)
	}
	if !strings.Contains(err.Error(), "NO durability class") {
		t.Fatalf("refusal = %q, want it to say the parent is unclassified", err)
	}
}

// TestNewDurableChildInheritsTheParentRoot pins that the denormalized root is
// taken from the parent rather than recomputed, which is what keeps a parent
// and its children from drifting apart.
func TestNewDurableChildInheritsTheParentRoot(t *testing.T) {
	// Arrange.
	parent := durableParent(t, "durable-2")

	// Act.
	got, err := NewDurableChild(parent, commandBody("child-3"))

	// Assert.
	if err != nil {
		t.Fatalf("NewDurableChild = err %v, want a classified child", err)
	}
	if got.GetDurable() == nil {
		t.Fatalf("durability arm = %T, want the durable arm set", got.GetDurability())
	}
	if root := got.GetLineage().GetTopLevelMessageId(); root != parent.GetUuid() {
		t.Fatalf("top_level_message_id = %q, want the parent's root %q", root, parent.GetUuid())
	}
	if p := got.GetLineage().GetParentMessageId(); p != parent.GetUuid() {
		t.Fatalf("parent_message_id = %q, want %q", p, parent.GetUuid())
	}
}

// TestNewEphemeralFeedRowRefusesAnAlreadyDurableBody holds the two arms
// mutually exclusive at construction, not only on the wire.
func TestNewEphemeralFeedRowRefusesAnAlreadyDurableBody(t *testing.T) {
	// Arrange.
	body := durableParent(t, "durable-3")

	// Act.
	got, err := NewEphemeralFeedRow(body)

	// Assert.
	if err == nil {
		t.Fatalf("NewEphemeralFeedRow = %+v, want a refusal: the body is already classified durable", got)
	}
}

// TestNewEphemeralFeedRowRefusesABlankUuid keeps the self-referential root from
// being a feed row that names nothing.
func TestNewEphemeralFeedRowRefusesABlankUuid(t *testing.T) {
	// Arrange.
	body := commandBody("")

	// Act.
	got, err := NewEphemeralFeedRow(body)

	// Assert.
	if err == nil {
		t.Fatalf("NewEphemeralFeedRow = %+v, want a refusal: a blank uuid has no root to name", got)
	}
}

// TestNewEphemeralFeedRowRefusesAPayloadlessBody keeps a classified message
// from being an empty card.
func TestNewEphemeralFeedRowRefusesAPayloadlessBody(t *testing.T) {
	// Arrange.
	body := &frontendv1.Message{Uuid: "no-payload"}

	// Act.
	got, err := NewEphemeralFeedRow(body)

	// Assert.
	if err == nil {
		t.Fatalf("NewEphemeralFeedRow = %+v, want a refusal: the body carries no payload arm", got)
	}
}

// TestNewEphemeralFeedRowDoesNotMutateTheCallerBody pins that the constructor
// returns a new message, so a caller cannot end up holding a half-classified
// copy of the same pointer.
func TestNewEphemeralFeedRowDoesNotMutateTheCallerBody(t *testing.T) {
	// Arrange.
	body := commandBody("session-command:req-5")

	// Act.
	if _, err := NewEphemeralFeedRow(body); err != nil {
		t.Fatalf("NewEphemeralFeedRow = err %v, want a classified message", err)
	}

	// Assert.
	if body.GetDurability() != nil {
		t.Fatalf("the caller's body was classified in place, want it left untouched")
	}
	if body.GetLineage() != nil {
		t.Fatalf("the caller's body had lineage written into it, want it left untouched")
	}
}

// --- ClassifyRecordDerived: the curation chokepoint -------------------------

// TestClassifyRecordDerivedStatesTheDurableArm is the defect this chokepoint
// closes: a curated message that reaches a frontend with NO durability class is
// malformed by the contract, because a reader cannot tell "no record exists"
// from "the record was not found".
func TestClassifyRecordDerivedStatesTheDurableArm(t *testing.T) {
	// Arrange.
	body := commandBody("u1")

	// Act.
	got, err := ClassifyRecordDerived([]*frontendv1.Message{body})

	// Assert.
	if err != nil {
		t.Fatalf("ClassifyRecordDerived = err %v, want a classified message", err)
	}
	if got[0].GetDurable() == nil {
		t.Fatalf("durability arm = %T, want the durable arm set", got[0].GetDurability())
	}
}

// TestClassifyRecordDerivedLeavesAnEphemeralMessageAlone keeps the chokepoint
// from becoming a SECOND authority over the class: the harness's local_command
// record is classified at its own producer, which is the only place that knows
// the shape.
func TestClassifyRecordDerivedLeavesAnEphemeralMessageAlone(t *testing.T) {
	// Arrange.
	already, err := NewEphemeralFeedRow(commandBody("u2"))
	if err != nil {
		t.Fatalf("NewEphemeralFeedRow = err %v, want an ephemeral message to test against", err)
	}

	// Act.
	got, err := ClassifyRecordDerived([]*frontendv1.Message{already})

	// Assert.
	if err != nil {
		t.Fatalf("ClassifyRecordDerived = err %v, want the already-classified message through untouched", err)
	}
	if got[0].GetEphemeral() == nil {
		t.Fatalf("durability arm = %T, want the ephemeral arm preserved", got[0].GetDurability())
	}
}

// TestClassifyRecordDerivedRefusesRatherThanServingAMessageShort keeps a
// refusal from silently becoming a hole in the conversation: the whole delta
// fails so the caller can report it, rather than one message vanishing where
// nothing downstream can attribute the loss.
func TestClassifyRecordDerivedRefusesRatherThanServingAMessageShort(t *testing.T) {
	// Arrange — a body with no uuid cannot root a feed row, so the constructor
	// refuses it.
	bad := commandBody("")

	// Act.
	got, err := ClassifyRecordDerived([]*frontendv1.Message{commandBody("u3"), bad})

	// Assert.
	if err == nil {
		t.Fatalf("ClassifyRecordDerived = %+v, want a refusal naming the unclassifiable message", got)
	}
	if got != nil {
		t.Fatalf("ClassifyRecordDerived returned %d message(s) beside its refusal, want none", len(got))
	}
}
