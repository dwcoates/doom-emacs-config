// The MERGE WINDOW end to end over the REAL processes: the merge skill's
// invocation is put into the real shim-store, fanned out to the real TS shim,
// forwarded to the daemon, and must reach a connected frontend as a Merge
// BUBBLE — with the session's own subsequent utterances folded inside it rather
// than in the top-level feed, and settled by the user's next prompt.
//
// WHY THE RECORDS ARE INJECTED RATHER THAN PROVOKED. Same reason as
// skillbody_e2e_test.go and asyncspecharness_test.go, whose helpers this file
// reuses READ-ONLY (liveSession, storeProducer.write, drainUntilItem,
// gateOnAnchor): the shim-claude-sidecar is the sole producer of file-plane
// records and it produces them by tailing a real vendor transcript, which the
// `--fake` harness has none of. Everything downstream — store ingest, fan-out,
// the shim's forward, the daemon's classification, folding and settlement, the
// frontend frames — runs for real.
package e2e

import (
	"testing"

	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"google.golang.org/protobuf/types/known/structpb"
)

// mergeSkillCallLine is the assistant record invoking the merge skill, in the
// shape the transcript writes: {"skill": "<name>", "args": "<verb and flags>"}.
func mergeSkillCallLine(t *testing.T, uuid, toolUseID, skill, args string) *datav1.TranscriptLine {
	t.Helper()
	input, err := structpb.NewStruct(map[string]any{"skill": skill, "args": args})
	if err != nil {
		t.Fatalf("structpb.NewStruct: %v", err)
	}
	return &datav1.TranscriptLine{Line: &datav1.TranscriptLine_Assistant{Assistant: &datav1.AssistantLine{
		Envelope: &datav1.LineEnvelope{Uuid: uuid},
		Message: &datav1.ApiAssistantMessage{Content: []*datav1.ContentBlock{
			{Block: &datav1.ContentBlock_ToolUse{ToolUse: &datav1.ToolUseBlock{Id: toolUseID, Name: "Skill", Input: input}}},
		}},
	}}}
}

// assistantProseLine is one ordinary assistant utterance — inside the window,
// this is the merge run talking.
func assistantProseLine(uuid, text string) *datav1.TranscriptLine {
	return &datav1.TranscriptLine{Line: &datav1.TranscriptLine_Assistant{Assistant: &datav1.AssistantLine{
		Envelope: &datav1.LineEnvelope{Uuid: uuid},
		Message: &datav1.ApiAssistantMessage{Content: []*datav1.ContentBlock{
			{Block: &datav1.ContentBlock_Text{Text: &datav1.TextBlock{Text: text}}},
		}},
	}}}
}

// feedProse returns every assistant utterance the TOP-LEVEL feed carried.
func feedProse(items []*frontendv1.ConversationItem) []string {
	var out []string
	for _, it := range items {
		for _, block := range it.GetAgent().GetResponse().GetBody().GetContent() {
			if text := block.GetText().GetText(); text != "" {
				out = append(out, text)
			}
		}
	}
	return out
}

// mergeProse returns every assistant utterance any delivery of bubbleID
// carried. A merge bubble advances by whole-bubble re-delivery, so the LAST
// delivery is the complete fold and earlier ones are its prefixes.
func mergeProse(seen asyncTraffic, bubbleID string) []string {
	var out []string
	for _, b := range seen.bubbles() {
		if b.GetId() != bubbleID || b.GetMerge() == nil {
			continue
		}
		out = out[:0]
		for _, em := range b.GetMerge().GetEmissions() {
			for _, block := range em.GetResponse().GetBody().GetContent() {
				if text := block.GetText().GetText(); text != "" {
					out = append(out, text)
				}
			}
		}
	}
	return out
}

// TestE2EAMergeInvocationOpensAWindowThatFoldsAndSettles drives the whole
// window: the invocation opens the bubble, the session's next utterance folds
// into it instead of the feed, and the user's own next prompt settles it.
func TestE2EAMergeInvocationOpensAWindowThatFoldsAndSettles(t *testing.T) {
	// Arrange
	h := newUDSHarness(t)
	cwd := t.TempDir()
	_, conn, vendorID, store := liveSession(t, h, cwd)
	const (
		toolUseID = "toolu_e2e_merge"
		inside    = "e2e-merge: cherry-picking the workspace onto master"
		takeBack  = "e2e-merge: thanks, that is enough"
		// The barrier is a SECOND prompt: the first one is the settle edge
		// itself, and the settlement push follows the delta that carries it.
		// Draining to the first prompt would stop before the settlement it
		// caused.
		barrierPrompt = "e2e-merge-barrier: the window's ending is now fully processed"
	)

	// Act
	store.write(sidecarLineEvent(t, vendorID, mergeSkillCallLine(t, "e2e-merge-call", toolUseID, "create-or-update-workspace", "merge")))
	store.write(sidecarLineEvent(t, vendorID, assistantProseLine("e2e-merge-inside", inside)))
	store.write(sidecarUserLineEvent(t, vendorID, "e2e-merge-takeback", takeBack))
	store.write(sidecarUserLineEvent(t, vendorID, "e2e-merge-barrier-line", barrierPrompt))

	// Assert
	seen := drainUntilItem(t, conn, cwd, "the barrier prompt's user item", func(it *frontendv1.ConversationItem) bool {
		return it.GetUserMessage().GetContentString() == barrierPrompt
	})

	bubbleID := gateOnAnchor(t, seen, toolUseID)
	opened := openedBubble(seen.bubbles(), bubbleID)
	if opened == nil {
		t.Fatalf("the async plane never delivered bubble %q at all", bubbleID)
	}
	if opened.GetMerge() == nil {
		t.Fatalf("the merge invocation opened a bubble of arm %T, want the merge arm: a merge run is a conversation, not detached work of another kind", opened.GetKind())
	}

	for _, text := range feedProse(seen.items) {
		if text == inside {
			t.Errorf("the merge run's utterance reached the TOP-LEVEL FEED: inside the window it belongs to the bubble, and the feed is the user's own conversation")
		}
	}
	var folded bool
	for _, text := range mergeProse(seen, bubbleID) {
		if text == inside {
			folded = true
		}
	}
	if !folded {
		t.Errorf("bubble %q folded %v, want the merge run's own utterance %q: the window claims every emission until the user takes the session back",
			bubbleID, mergeProse(seen, bubbleID), inside)
	}

	settled := lastSettledLiveness(seen, bubbleID)
	if settled == nil {
		t.Fatalf("bubble %q never settled: the user's own next prompt is the boundary async-bubble.proto names for a merge run (saw %d updates for it)",
			bubbleID, len(seen.updatesFor(bubbleID)))
	}
	if settled.GetDone() == nil {
		t.Errorf("bubble %q settled on outcome %T, want done: the user typing again is the window ending, not the merge failing", bubbleID, settled.GetOutcome())
	}
}

// TestE2EAMergeWindowReturnsTheFeedToTheUserAfterItSettles covers the edge the
// window is most dangerous at: once settled, the session's own emissions must
// reach the top-level feed again rather than being swallowed forever.
func TestE2EAMergeWindowReturnsTheFeedToTheUserAfterItSettles(t *testing.T) {
	// Arrange
	h := newUDSHarness(t)
	cwd := t.TempDir()
	_, conn, vendorID, store := liveSession(t, h, cwd)
	const (
		toolUseID     = "toolu_e2e_merge_after"
		after         = "e2e-merge-after: on it"
		barrierPrompt = "e2e-merge-after-barrier: everything is now fully processed"
	)

	// Act
	store.write(sidecarLineEvent(t, vendorID, mergeSkillCallLine(t, "e2e-after-call", toolUseID, "create-or-update-workspace", "merge")))
	store.write(sidecarUserLineEvent(t, vendorID, "e2e-after-takeback", "e2e-merge-after: thanks, that is enough"))
	store.write(sidecarLineEvent(t, vendorID, assistantProseLine("e2e-after-reply", after)))
	store.write(sidecarUserLineEvent(t, vendorID, "e2e-after-barrier-line", barrierPrompt))

	// Assert
	seen := drainUntilItem(t, conn, cwd, "the barrier prompt's user item", func(it *frontendv1.ConversationItem) bool {
		return it.GetUserMessage().GetContentString() == barrierPrompt
	})
	for _, text := range feedProse(seen.items) {
		if text == after {
			return
		}
	}
	t.Fatalf("the session's utterance after the window settled never reached the top-level feed (feed carried %v): a closed window must claim nothing",
		feedProse(seen.items))
}

// TestE2EANonMergeVerbOfTheSameSkillOpensNoBubble is the near-miss the
// classifier exists for: /create-or-update-workspace has seven verbs and only
// `merge` detaches. A window opened by any other one would swallow the user's
// whole conversation into a bubble.
func TestE2EANonMergeVerbOfTheSameSkillOpensNoBubble(t *testing.T) {
	// Arrange
	h := newUDSHarness(t)
	cwd := t.TempDir()
	_, conn, vendorID, store := liveSession(t, h, cwd)
	const (
		toolUseID     = "toolu_e2e_create"
		after         = "e2e-create: the workspace is up"
		barrierPrompt = "e2e-create-barrier: everything is now fully processed"
	)

	// Act
	store.write(sidecarLineEvent(t, vendorID, mergeSkillCallLine(t, "e2e-create-call", toolUseID, "create-or-update-workspace", "create feat/thing")))
	store.write(sidecarLineEvent(t, vendorID, assistantProseLine("e2e-create-reply", after)))
	store.write(sidecarUserLineEvent(t, vendorID, "e2e-create-barrier-line", barrierPrompt))

	// Assert
	seen := drainUntilItem(t, conn, cwd, "the barrier prompt's user item", func(it *frontendv1.ConversationItem) bool {
		return it.GetUserMessage().GetContentString() == barrierPrompt
	})
	for _, b := range seen.bubbles() {
		if b.GetMerge() != nil {
			t.Errorf("the `create` verb opened merge bubble %q: every verb but `merge` is an ordinary skill card", b.GetId())
		}
	}
	for _, text := range feedProse(seen.items) {
		if text == after {
			return
		}
	}
	t.Fatalf("the reply following a non-merge skill invocation never reached the feed (feed carried %v)", feedProse(seen.items))
}
