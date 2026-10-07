package integration

import (
	"testing"
)

// SUBJECT — usage when the response's FIRST content block is a tool_use.
//
// The vendor repeats `message.usage` on EVERY transcript line of one API
// response, so carrying it per line would count one response's tokens as many.
// The rule is therefore "the FIRST block's unit carries it", and that unit is
// not necessarily a prose or thinking block: a response that opens directly on a
// tool call must put the accounting on the CALL's unit — which is keyed by the
// vendor's tool_use_id rather than by <message id>:<ordinal>, so it is exactly
// the case a "block index 0" implementation would miss.

// TestUsageRidesATooluseFirstBlock asserts a response whose first block is a
// tool_use carries its usage on that call's unit.
func TestUsageRidesATooluseFirstBlock(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	cwd := "/work/usage-tooluse-probe"
	slug := cwdSlug(cwd)
	session := "4a4a4a4a-4a4a-44a4-84a4-4a4a4a4a4a4a"
	messageID := "msg_usage_first_tool_use"
	callID := "toolu_usage_first_block"

	// The captured line 8 is a real assistant tool_use line carrying its
	// response's usage; giving it a message id of its own makes it that
	// response's FIRST observed block.
	call := retargetSession(t, decodeRecord(t, captured.Lines[8]), session, cwd)
	call = setMessageID(t, call, messageID)
	call = setToolUseID(t, call, callID)
	if _, ok := call["message"].(map[string]any)["usage"]; !ok {
		t.Fatalf("captured line 8 carries no message.usage, so this subject would assert nothing")
	}

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := newGrowingFile(t, tree.sessionPath(slug, session))
	g.AppendLine(encodeRecord(t, call))
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert.
	entries := unitEntries(fake.Entries(), callID)
	if len(entries) == 0 {
		t.Fatalf("the tool call produced no unit under %q; keys were %v",
			"activity:"+callID, upsertKeysOf(fake.Entries()))
	}
	var carried bool
	for _, e := range entries {
		if activityOf(e.GetAgentUpdate().GetServeableFrame()).GetUsage() != nil {
			carried = true
		}
	}
	if !carried {
		t.Errorf("the response's first block is a tool_use and must carry its usage; unit %q carried none", callID)
	}
}

// TestUsageIsCarriedByExactlyOneUnitWhenTheFirstBlockIsATooluse asserts the
// other half — one response, one carrier — with a tool_use in front.
func TestUsageIsCarriedByExactlyOneUnitWhenTheFirstBlockIsATooluse(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	cwd := "/work/usage-tooluse-once-probe"
	slug := cwdSlug(cwd)
	session := "4b4b4b4b-4b4b-44b4-84b4-4b4b4b4b4b4b"
	messageID := "msg_usage_tool_use_then_thinking"
	callID := "toolu_usage_tool_use_then_thinking"

	call := retargetSession(t, decodeRecord(t, captured.Lines[8]), session, cwd)
	call = setMessageID(t, call, messageID)
	call = setToolUseID(t, call, callID)
	// The SAME response's second line: a thinking block, which must leave usage
	// unset because the tool call ahead of it already carried it.
	thinking := retargetSession(t, decodeRecord(t, captured.Lines[7]), session, cwd)
	thinking = setMessageID(t, thinking, messageID)

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := newGrowingFile(t, tree.sessionPath(slug, session))
	g.AppendLine(encodeRecord(t, call))
	g.AppendLine(encodeRecord(t, thinking))
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert.
	carriers := map[string]bool{}
	for _, line := range linesForBook(fake.Entries(), session) {
		a := activityOf(line)
		if a != nil && a.GetUsage() != nil {
			carriers[a.GetActivityId().GetValue()] = true
		}
	}
	if !carriers[callID] {
		t.Errorf("the first block's unit %q carried no usage", callID)
	}
	if carriers[messageID+":1"] {
		t.Errorf("unit %q is not its response's first block and must leave usage unset", messageID+":1")
	}
	if len(carriers) != 1 {
		t.Errorf("one API response has one usage carrier; %d carried it: %v",
			len(carriers), sortedStrings(keysOf(carriers)))
	}
}
