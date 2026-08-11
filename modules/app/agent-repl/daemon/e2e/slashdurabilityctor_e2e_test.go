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
// So the seam under test is the constructor pair, and the assertion is that it
// returns an error rather than a message.
//
// WHY THE PARENT IS A MESSAGE AND NOT AN ID. The rule being enforced is about
// the parent's CLASS, and a bare id does not carry one. A constructor handed
// only a string could not tell an ephemeral parent from a durable one, so the
// refusal would have to happen somewhere that could — which is the "checked
// afterwards" the contract rules out.
//
// THE SEAM THIS FILE REQUIRES, verbatim, in claude-repld/internal/frontend
// beside FeedRowLineage (which these two subsume for the classes they build):
//
//	// NewDurableMessage builds a message a store record exists for. parent is
//	// the containing message, nil for a feed row. Refuses an ephemeral parent.
//	func NewDurableMessage(uuid string, parent *frontendv1.Message) (*frontendv1.Message, error)
//
//	// NewEphemeralMessage builds a message no store record exists for and none
//	// ever will. parent must be nil: an ephemeral message is ALWAYS a feed row.
//	func NewEphemeralMessage(uuid string, parent *frontendv1.Message) (*frontendv1.Message, error)
//
// Until both land this package does not build, which is the intended signal:
// the refusal has no other place it can be asserted without becoming the
// after-the-fact check the contract forbids.
package e2e

import (
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/frontend"
)

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
	parent, err := frontend.NewEphemeralMessage("ephemeral-parent", nil)
	if err != nil {
		t.Fatalf("constructing an ephemeral feed row: %v, want it accepted (empty parent, self-referential root)", err)
	}

	// Act
	child, err := frontend.NewDurableMessage("durable-child", parent)

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
func TestEphemeralMessageMayNotNameADurableParent(t *testing.T) {
	// Arrange — a durable feed row.
	parent, err := frontend.NewDurableMessage("durable-parent", nil)
	if err != nil {
		t.Fatalf("constructing a durable feed row: %v, want it accepted", err)
	}
	if parent.GetLineage().GetTopLevelMessageId() != parent.GetUuid() {
		t.Fatalf("the durable feed row's top_level_message_id is %q, want its own uuid %q — a feed row names itself",
			parent.GetLineage().GetTopLevelMessageId(), parent.GetUuid())
	}

	// Act
	child, err := frontend.NewEphemeralMessage("ephemeral-child", parent)

	// Assert
	if err == nil {
		t.Fatalf("constructing an ephemeral child of a durable parent returned message %v and no error, want a refusal — the card would attach itself into a paged conversation it will simply vanish from",
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
	m, err := frontend.NewEphemeralMessage("ephemeral-row", nil)

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

// The two constructors' signatures are pinned here, so a seam that grew a
// second message type or dropped the error return fails at compile time rather
// than in whatever goes on to publish the message.
var _ func(string, *frontendv1.Message) (*frontendv1.Message, error) = frontend.NewEphemeralMessage
var _ func(string, *frontendv1.Message) (*frontendv1.Message, error) = frontend.NewDurableMessage
