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

// mergeProse returns every assistant utterance the merge window delivered for
// bubbleID: the fold the OPEN carried, plus every MERGE-ARM update appended to
// it since.
//
// AMENDED: this used to read the last whole-bubble re-delivery, which was how a
// merge window advanced before the contract had an arm for it. The update
// oneof's own rule — "Never a re-send of the whole bubble" — retires that route,
// so the fold a client holds is now the open plus its appends, and that is what
// this reconstructs.
func mergeProse(seen asyncTraffic, bubbleID string) []string {
	var out []string
	appendEmissions := func(ems []*frontendv1.AgentEmission) {
		for _, em := range ems {
			for _, block := range em.GetResponse().GetBody().GetContent() {
				if text := block.GetText().GetText(); text != "" {
					out = append(out, text)
				}
			}
		}
	}
	for _, b := range seen.bubbles() {
		if b.GetId() == bubbleID && b.GetMerge() != nil {
			appendEmissions(b.GetMerge().GetEmissions())
		}
	}
	for _, u := range seen.updatesFor(bubbleID) {
		appendEmissions(u.GetMerge().GetEmissions())
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

// skillProse returns every assistant utterance a SKILL bubble was delivered:
// the fold its open carried, plus every skill-arm emissions update since.
func skillProse(seen asyncTraffic, bubbleID string) []string {
	var out []string
	appendEmissions := func(ems []*frontendv1.AgentEmission) {
		for _, em := range ems {
			for _, block := range em.GetResponse().GetBody().GetContent() {
				if text := block.GetText().GetText(); text != "" {
					out = append(out, text)
				}
			}
		}
	}
	for _, b := range seen.bubbles() {
		if b.GetId() == bubbleID && b.GetSkill() != nil {
			appendEmissions(b.GetSkill().GetEmissions())
		}
	}
	for _, u := range seen.updatesFor(bubbleID) {
		appendEmissions(u.GetSkill().GetEmissions().GetEmissions())
	}
	return out
}

// TestE2EANonMergeVerbOfTheSameSkillOpensASkillBubbleNotAMergeOne is the
// near-miss the classifier exists for: /create-or-update-workspace has seven
// verbs and only `merge` is the merge run. The other six are skill invocations
// like any other, and a MERGE bubble opened by one of them would render the
// wrong thing entirely.
//
// AMENDED: this asserted that the `create` verb opened no bubble at all and that
// its reply reached the top-level feed. Both were true while the merge skill was
// the only bubble-forming one. async-bubble.proto now says "Skill invocations
// are bubble-forming … `merge` is the one skill with an arm of its own; every
// other skill arrives as `skill`", so the reply belongs to the skill bubble and
// what must not happen is a MERGE bubble.
func TestE2EANonMergeVerbOfTheSameSkillOpensASkillBubbleNotAMergeOne(t *testing.T) {
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
			t.Errorf("the `create` verb opened merge bubble %q: every verb but `merge` is an ordinary skill invocation", b.GetId())
		}
	}
	bubbleID := gateOnAnchor(t, seen, toolUseID)
	opened := openedBubble(seen.bubbles(), bubbleID)
	if opened.GetSkill() == nil {
		t.Fatalf("the `create` verb opened a bubble of arm %T, want the skill arm every non-merge skill arrives on", opened.GetKind())
	}
	for _, text := range skillProse(seen, bubbleID) {
		if text == after {
			return
		}
	}
	t.Fatalf("the reply following the skill invocation never reached its bubble (bubble carried %v, feed carried %v)",
		skillProse(seen, bubbleID), feedProse(seen.items))
}
