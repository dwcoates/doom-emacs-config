package integration

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
)

// SUBJECT — a SKILL invocation settled by its document, across polls.
//
// A Skill call is acknowledged immediately ("Launching skill: X") and settles
// only when the skill's own SKILL.md body arrives, as an `isMeta` user record
// naming the call in `sourceToolUseID`. The join is that field: DIRECT and
// STRUCTURAL, never "the next skill document belongs to the last skill call".
//
// THE UNIT TEST CANNOT DISTINGUISH THE TWO. internal/convert's
// TestSkillDocumentSettlesItsInvocationBySourceToolUseId converts one call and
// one document in one batch, which a name-matching or last-call-wins reader
// would satisfy identically. What separates them is TWO OPEN CALLS and a
// document arriving on a LATER POLL, after the second call — so the "next" call
// and the "named" call are different calls, and only one of them may settle.

// TestASkillDocumentSettlesItsOwnCallNotTheNextSkillCall opens two Skill calls
// and then lands the FIRST one's document.
func TestASkillDocumentSettlesItsOwnCallNotTheNextSkillCall(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	cwd := "/work/skill-join-probe"
	slug := cwdSlug(cwd)
	session := "8a8a8a8a-8a8a-48a8-88a8-8a8a8a8a8a8a"
	firstCall, secondCall := capturedBashCall1, capturedBashCall2

	// Act: both calls and both acknowledgements land first, so BOTH units are
	// open when the document arrives.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := newGrowingFile(t, tree.sessionPath(slug, session))
	g.AppendLine(encodeRecord(t, skillCall(t, captured, session, cwd, firstCall, "graphify")))
	g.AppendLine(encodeRecord(t, skillAck(t, session, cwd, firstCall, "graphify")))
	g.AppendLine(encodeRecord(t, skillCall(t, captured, session, cwd, secondCall, "profile")))
	g.AppendLine(encodeRecord(t, skillAck(t, session, cwd, secondCall, "profile")))
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// A LATER POLL, and it names the FIRST call.
	body := "# graphify\n\nthe skill body, verbatim\n"
	g.AppendLine(skillDocumentLine(t, session, cwd, firstCall, body))
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert: the first unit is settled, carrying the body...
	settled := latestSkillUse(fake.Entries(), firstCall)
	if settled == nil {
		t.Fatalf("no skill unit was written for the first call %q; keys were %v", firstCall, upsertKeysOf(fake.Entries()))
	}
	success := settled.GetSuccess()
	if success == nil {
		t.Fatalf("the first call was not settled by its own document; it stands at %v", settled.GetResult())
	}
	if got := success.GetDocument().GetMarkdown(); got != body {
		t.Errorf("the settled skill carries document %q, wanted the body verbatim %q", got, body)
	}

	// ...and the second is still OPEN, because nothing named it.
	other := latestSkillUse(fake.Entries(), secondCall)
	if other == nil {
		t.Fatalf("no skill unit was written for the second call %q at all", secondCall)
	}
	if other.GetSuccess() != nil {
		t.Errorf("the SECOND skill call was settled by a document naming the FIRST; the join is sourceToolUseID, not adjacency")
	}
	if other.GetStart() == nil {
		t.Errorf("the second skill call stands at %v, wanted its opening start frame — no document has named it", other.GetResult())
	}
}

// latestSkillUse answers the NEWEST skill frame written under a call's unit key
// — the state the store holds, since a write supersedes its row whole.
func latestSkillUse(entries []*storev1.StoreEntry, call string) *conversationv1.AgentSkillUse {
	var out *conversationv1.AgentSkillUse
	for _, e := range unitEntries(entries, call) {
		if use := activityOf(e.GetAgentUpdate().GetServeableFrame()).GetSkillUse(); use != nil {
			out = use
		}
	}
	return out
}

// skillCall re-points the captured transcript's real tool_use record at a Skill
// invocation with a given call id.
func skillCall(t *testing.T, captured capturedSession, session, cwd, callID, skill string) map[string]any {
	t.Helper()
	call := retargetSession(t, decodeRecord(t, captured.Lines[8]), session, cwd)
	call = renameToolUse(t, call, "Skill")
	call = setToolUseInput(t, call, map[string]any{"skill": skill})
	return setToolUseID(t, call, callID)
}

// skillAck re-points the corpus's REAL skill-launch tool result at a call.
func skillAck(t *testing.T, session, cwd, callID, skill string) map[string]any {
	t.Helper()
	ack := retargetSession(t, decodeRecord(t, corpusLine(t, "tool-results/skill.jsonl", 0)), session, cwd)
	ack = setToolUseID(t, ack, callID)
	return setNested(t, ack, "toolUseResult", "commandName", skill)
}

// skillDocumentLine spells the SKILL.md record the vendor writes after a skill
// launches.
//
// *** SYNTHETIC. THIS SHAPE IS NOT CAPTURED ANYWHERE. ***
//
// The FIELDS are pinned by production code — internal/convert reads `isMeta`,
// `sourceToolUseID` and the message's first text block, and nothing else of
// this record — so the join this subject drives is grounded. The surrounding
// envelope (parentUuid, userType, entrypoint, version) is a plausible
// reconstruction copied from the corpus's other user records, not evidence:
// grep testdata/corpus/ and testdata/projects/ and there is no `sourceToolUseID` in
// either tree. It is carried as a stated CONCERN rather than dressed up as a
// capture; the fix is a real capture of a skill invocation and its document.
func skillDocumentLine(t *testing.T, session, cwd, callID, body string) string {
	t.Helper()
	return encodeRecord(t, map[string]any{
		"parentUuid":      nil,
		"isSidechain":     false,
		"type":            "user",
		"isMeta":          true,
		"sourceToolUseID": callID,
		"uuid":            "doc-" + callID,
		"timestamp":       "2026-08-29T12:00:00.000Z",
		"message":         map[string]any{"role": "user", "content": []any{map[string]any{"type": "text", "text": body}}},
		"userType":        "external",
		"entrypoint":      "sdk-cli",
		"cwd":             cwd,
		"sessionId":       session,
		"version":         "2.1.215",
	})
}
