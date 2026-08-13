package frontend

import (
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"
)

// msgWithLineage builds a bare message carrying exactly the lineage under test.
func msgWithLineage(uuid, root, parent string) *frontendv1.Message {
	return &frontendv1.Message{
		Uuid:    uuid,
		Lineage: &frontendv1.MessageLineage{TopLevelMessageId: root, ParentMessageId: parent},
	}
}

// TestOwnershipEqualsRootOfParentChain is the contract's central rule: a nested
// message's owner is the ROOT of its chain, not its immediate parent.
func TestOwnershipEqualsRootOfParentChain(t *testing.T) {
	// Arrange: root -> mid -> leaf, every one naming root.
	msgs := []*frontendv1.Message{
		msgWithLineage("root", "root", ""),
		msgWithLineage("mid", "root", "root"),
		msgWithLineage("leaf", "root", "mid"),
	}

	// Act.
	err := VerifyOwnershipRoots(msgs)

	// Assert.
	if err != nil {
		t.Fatalf("a chain whose every member names the true root was refused: %v", err)
	}
}

// TestOwnershipDisagreeingWithParentIsRefused covers the drift the
// denormalization exists to risk: a child filed under a different feed row than
// its container.
func TestOwnershipDisagreeingWithParentIsRefused(t *testing.T) {
	// Arrange.
	msgs := []*frontendv1.Message{
		msgWithLineage("root", "root", ""),
		msgWithLineage("leaf", "other-root", "root"),
	}

	// Act.
	err := VerifyOwnershipRoots(msgs)

	// Assert.
	if err == nil {
		t.Fatal("a child naming a root its parent does not was accepted; the record and its container would land on different pages")
	}
}

// TestFeedRowNamingAnotherRootIsRefused covers the parentless message whose
// owner is not its own uuid.
func TestFeedRowNamingAnotherRootIsRefused(t *testing.T) {
	// Arrange.
	msgs := []*frontendv1.Message{msgWithLineage("solo", "somebody-else", "")}

	// Act.
	err := VerifyOwnershipRoots(msgs)

	// Assert.
	if err == nil {
		t.Fatal("a feed row pointing at a different root was accepted; the contract calls that corruption, not a variant")
	}
}

// TestEmptyOwnerIsRefused covers the rootless message, which no page query can
// ever return.
func TestEmptyOwnerIsRefused(t *testing.T) {
	// Arrange.
	msgs := []*frontendv1.Message{msgWithLineage("solo", "", "")}

	// Act.
	err := VerifyOwnershipRoots(msgs)

	// Assert.
	if err == nil {
		t.Fatal("a message with an empty top_level_message_id was accepted")
	}
}

// TestChildClaimingItselfAsRootIsRefused covers the message that would be a
// page slot and a nested child at once.
func TestChildClaimingItselfAsRootIsRefused(t *testing.T) {
	// Arrange.
	msgs := []*frontendv1.Message{
		msgWithLineage("root", "root", ""),
		msgWithLineage("leaf", "leaf", "root"),
	}

	// Act.
	err := VerifyOwnershipRoots(msgs)

	// Assert.
	if err == nil {
		t.Fatal("a message naming a parent while claiming to be its own feed row was accepted")
	}
}

// TestParentChainCycleIsRefused covers corrupt containment: a cycle has no root
// to agree with and following it would hang.
func TestParentChainCycleIsRefused(t *testing.T) {
	// Arrange.
	msgs := []*frontendv1.Message{
		msgWithLineage("a", "root", "b"),
		msgWithLineage("b", "root", "a"),
	}

	// Act.
	err := VerifyOwnershipRoots(msgs)

	// Assert.
	if err == nil {
		t.Fatal("a cyclic parent chain was accepted; containment is a tree")
	}
}

// TestChainLeavingTheBatchIsAccepted covers the ordinary incremental case: a
// child whose parent was delivered earlier inherited that parent's root at
// construction, so the walk stopping at the batch edge is not a defect.
func TestChainLeavingTheBatchIsAccepted(t *testing.T) {
	// Arrange.
	msgs := []*frontendv1.Message{msgWithLineage("leaf", "root-elsewhere", "parent-elsewhere")}

	// Act.
	err := VerifyOwnershipRoots(msgs)

	// Assert.
	if err != nil {
		t.Fatalf("a child whose parent is outside the batch was refused: %v", err)
	}
}
