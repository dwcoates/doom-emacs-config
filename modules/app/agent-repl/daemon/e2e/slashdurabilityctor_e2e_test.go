// The two lineage rules that cross the durability classes, asserted at the
// CONSTRUCTOR: contract Part 5 tests 5 and 6.
//
// WHY THESE TWO ARE NOT DRIVEN OVER THE STACK. Every other test in this suite
// asserts what the running system DOES. These two assert what it CANNOT BE MADE
// TO DO, and a violation that reached a running stack would already be the
// failure. FROZEN-slash-command-durability.md Part 4 rule 4 is explicit: the
// rules are "enforced at the ephemeral constructor, refusing violations, rather
// than checked afterwards", and feed.proto restates it — "Those are refused at
// construction, so a violating message cannot be built and then noticed; a check
// applied afterwards is a check something can skip."
//
// So the seam under test is the constructor set, and the assertion is that it
// returns an error rather than a message.
//
// WHY THE PARENT IS A MESSAGE AND NOT AN ID, in NewDurableChild. The rule being
// enforced is about the parent's CLASS, and a bare id does not carry one. A
// constructor handed only a string could not tell an ephemeral parent from a
// durable one, so the refusal would have to happen somewhere that could — which
// is the "checked afterwards" the contract rules out.
//
// WHY THE EPHEMERAL CONSTRUCTOR TAKES NO PARENT AT ALL. This file was authored
// against a proposed pair that both took a parent, and the seam that landed goes
// further: NewEphemeralFeedRow has no parameter through which a parent can be
// named, so rule 1 is unrepresentable rather than refused. Rule 3 survives as a
// refusal because a caller can still hand in a BODY that already carries a
// lineage, and that is the shape test 6 drives.
package e2e

import (
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/frontend"
)

// durabilityTestBody returns the minimum message body the constructors accept:
// an identity and a payload arm, with the lineage left for the constructor to
// write. The payload is the intercepted-command item because it is the arm this
// contract's own ephemeral construction site uses; nothing here turns on which
// arm it is, only that one is set.
func durabilityTestBody(uuid string) *frontendv1.Message {
	return &frontendv1.Message{
		Uuid: uuid,
		Payload: &frontendv1.Message_DaemonInterceptedCommand{
			DaemonInterceptedCommand: &frontendv1.DaemonInterceptedCommandItem{
				Command: frontendv1.SessionCommand_SESSION_COMMAND_MODEL,
			},
		},
	}
}

// --- 5. a durable child may not name an ephemeral parent --------------------

// TestDurableChildMayNotNameAnEphemeralParent covers contract Part 5 test 5.
//
// A durable child naming an ephemeral root would be UNREACHABLE by any store
// query: a store record can only name ids that exist in the store, and an
// ephemeral message has no record anywhere. The child would be written,
// persisted, and then invisible to the one query that is supposed to find it —
// a silent hole in a paged conversation.
func TestDurableChildMayNotNameAnEphemeralParent(t *testing.T) {
	// Arrange — an ephemeral feed row, which is the only shape an ephemeral
	// message is allowed to take.
	parent, err := frontend.NewEphemeralFeedRow(durabilityTestBody("ephemeral-parent"))
	if err != nil {
		t.Fatalf("constructing an ephemeral feed row: %v, want it accepted (empty parent, self-referential root)", err)
	}

	// Act
	child, err := frontend.NewDurableChild(parent, durabilityTestBody("durable-child"))

	// Assert
	if err == nil {
		t.Fatalf("constructing a durable child of an ephemeral parent returned message %v and no error, want a refusal — the child would name a root no store record can exist for, so no page query could ever return it",
			child)
	}
	if child != nil {
		t.Errorf("the refused construction still returned a message %v, want nil — a refusal that hands back a half-built message is a message something can go on to publish", child)
	}
}

// --- 6. an ephemeral message may not name a durable parent ------------------

// TestEphemeralMessageMayNotNameADurableParent covers contract Part 5 test 6,
// the other direction.
//
// An ephemeral message that attached itself into a paged conversation would
// vanish from that conversation on the next read, leaving a hole exactly where
// a reader has every reason to expect a message. An ephemeral message is
// ALWAYS a feed row: empty parent_message_id, top_level_message_id equal to its
// own id.
//
// The attempt is made the only way the seam still allows it — a body handed in
// with the parent already written into its lineage. A constructor that silently
// overwrote that lineage would pass this test's first assertion and still hide
// the producer that believed it was attaching the card, which is why the
// constructor refuses instead of correcting.
func TestEphemeralMessageMayNotNameADurableParent(t *testing.T) {
	// Arrange — a durable feed row.
	parent, err := frontend.NewDurableFeedRow(durabilityTestBody("durable-parent"))
	if err != nil {
		t.Fatalf("constructing a durable feed row: %v, want it accepted", err)
	}
	if parent.GetLineage().GetTopLevelMessageId() != parent.GetUuid() {
		t.Fatalf("the durable feed row's top_level_message_id is %q, want its own uuid %q — a feed row names itself",
			parent.GetLineage().GetTopLevelMessageId(), parent.GetUuid())
	}

	attaching := durabilityTestBody("ephemeral-child")
	attaching.Lineage = &frontendv1.MessageLineage{
		TopLevelMessageId: parent.GetUuid(),
		ParentMessageId:   parent.GetUuid(),
	}

	// Act
	child, err := frontend.NewEphemeralFeedRow(attaching)

	// Assert
	if err == nil {
		t.Fatalf("constructing an ephemeral message that names a durable parent returned message %v and no error, want a refusal — the card would attach itself into a paged conversation it will simply vanish from",
			child)
	}
	if child != nil {
		t.Errorf("the refused construction still returned a message %v, want nil", child)
	}
}

// TestEphemeralMessageIsAlwaysAFeedRow pins the POSITIVE half of the ephemeral
// constructor's contract, so the two refusals above cannot be satisfied by a
// constructor that refuses everything.
//
// It is the shape the class is DEFINED by rather than a separate rule: empty
// parent, self-referential root.
func TestEphemeralMessageIsAlwaysAFeedRow(t *testing.T) {
	// Arrange & Act
	m, err := frontend.NewEphemeralFeedRow(durabilityTestBody("ephemeral-row"))

	// Assert
	if err != nil {
		t.Fatalf("constructing an ephemeral feed row: %v, want it accepted", err)
	}
	if m.GetEphemeral() == nil {
		t.Errorf("the constructed message does not carry the ephemeral arm, want it set — the arm IS the class")
	}
	if got := m.GetLineage().GetParentMessageId(); got != "" {
		t.Errorf("parent_message_id = %q, want empty — an ephemeral message is ALWAYS a feed row", got)
	}
	if got := m.GetLineage().GetTopLevelMessageId(); got != m.GetUuid() {
		t.Errorf("top_level_message_id = %q, want its own uuid %q", got, m.GetUuid())
	}
}

// The constructors' signatures are pinned here, so a seam that grew a second
// message type or dropped the error return fails at compile time rather than in
// whatever goes on to publish the message.
//
// NewEphemeralFeedRow's ABSENT parent parameter is itself part of what is
// pinned: a signature that grew one would make rule 1 representable again.
var _ func(*frontendv1.Message) (*frontendv1.Message, error) = frontend.NewEphemeralFeedRow
var _ func(*frontendv1.Message) (*frontendv1.Message, error) = frontend.NewDurableFeedRow
var _ func(*frontendv1.Message, *frontendv1.Message) (*frontendv1.Message, error) = frontend.NewDurableChild
