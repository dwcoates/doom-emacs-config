package holds_test

import (
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/holds"
	"agent-repld/internal/wsm"
)

// textBody draws a body of one text block per text block of CONTENT, so a
// test can tell the drawn body is the said's own.
func textBody(content *conversationv1.UserContent) *frontendv1.FeedUserPromptBody {
	body := &frontendv1.FeedUserPromptBody{}
	for _, block := range content.GetBlocks() {
		if text := block.GetText(); text != nil {
			body.Blocks = append(body.Blocks, &frontendv1.FeedUserPromptBlock{
				Block: &frontendv1.FeedUserPromptBlock_Text{Text: &frontendv1.FeedTextBlock{Prose: &frontendv1.ParsedProse{Source: &frontendv1.ProseSource{Markdown: "drawn:" + text.GetText()}}}},
			})
		}
	}
	return body
}

// testWS is the workspace every test in this package addresses.
const testWS = ids.WorkspaceID("ws-holds")

// queuedAt is the instant held prompts are stamped with unless a test needs an
// order, so a conversion assertion never depends on wall-clock.
var queuedAt = time.UnixMilli(1_700_000_000_000)

// renderer is a test stand-in for the held section's prompt half: it takes
// the same setters the feed resolver does and renders through holds.Prompts.
type renderer struct {
	held    []wsm.HeldPrompt
	editing ids.TurnID
	// loading and relaunching are the shim start facts it was told.
	loading, relaunching bool
	log                  dlog.Logger
	cards                []holds.Card
}

// SetHeldPrompts records the standing holds, as the prompt queue states them,
// and renders them at once, as the feed resolver does on every change.
func (r *renderer) SetHeldPrompts(_ ids.WorkspaceID, held []wsm.HeldPrompt) {
	r.held = held
	r.render()
}

// FoldHeldPrompts records the standing holds a fold left, and renders them
// as SetHeldPrompts does: the fold itself is the feed resolver's to publish.
func (r *renderer) FoldHeldPrompts(ws ids.WorkspaceID, held []wsm.HeldPrompt, _ holds.Fold) {
	r.SetHeldPrompts(ws, held)
}

// SetEditing records the hold being edited, and renders again.
func (r *renderer) SetEditing(_ ids.WorkspaceID, turn ids.TurnID) {
	r.editing = turn
	r.render()
}

// SetShimLoading records whether a shim start is under way, and renders again.
func (r *renderer) SetShimLoading(_ ids.WorkspaceID, loading bool) {
	r.loading = loading
	r.render()
}

// SetRelaunching records whether the start relaunches a shim that died, and
// renders again.
func (r *renderer) SetRelaunching(_ ids.WorkspaceID, relaunching bool) {
	r.relaunching = relaunching
	r.render()
}

// render draws the cards from what the renderer was told, as the feed
// resolver does on every change.
func (r *renderer) render() {
	r.cards = holds.Prompts(r.held, r.editing, holds.StartOf(r.loading, r.relaunching), textBody, r.log)
}

// newRenderer builds a renderer plus the surfaces its records land in.
func newRenderer(t *testing.T) (*renderer, *dlog.TestSurfaces) {
	t.Helper()
	surfaces := dlog.NewTestSurfaces()
	return &renderer{log: surfaces.Global()}, surfaces
}

// latest answers the cards the renderer last rendered.
func latest(t *testing.T, r *renderer) []holds.Card {
	t.Helper()
	return r.cards
}

// said builds a user submission carrying one line of text.
func said(text string) *conversationv1.UserSaid {
	return &conversationv1.UserSaid{
		Content: &conversationv1.UserContent{
			Blocks: []*conversationv1.UserContentBlock{{
				Block: &conversationv1.UserContentBlock_Text{
					Text: &conversationv1.TextBlock{Text: text}},
			}},
		},
	}
}

// hold builds a standing hold with the given turn and text.
func hold(turn, text string) wsm.HeldPrompt {
	return wsm.HeldPrompt{
		Workspace: testWS,
		Turn:      ids.TurnID(turn),
		Said:      said(text),
		QueuedAt:  queuedAt,
	}
}

// verdict stamps a classification on a hold.
func verdict(h wsm.HeldPrompt, arm wsm.ClassificationArm, reason string) wsm.HeldPrompt {
	h.Classification = &wsm.Classification{Arm: arm, Reason: reason, At: queuedAt}
	return h
}

// prompts reads the rendered cards' prompts, failing on a nil one.
func prompts(t *testing.T, cards []holds.Card) []*frontendv1.HeldPrompt {
	t.Helper()
	out := make([]*frontendv1.HeldPrompt, 0, len(cards))
	for i, card := range cards {
		if card.Prompt == nil {
			t.Fatalf("card %d carried a nil prompt", i)
		}
		out = append(out, card.Prompt)
	}
	return out
}

// onlyPrompt reads the single rendered card's prompt.
func onlyPrompt(t *testing.T, cards []holds.Card) *frontendv1.HeldPrompt {
	t.Helper()
	got := prompts(t, cards)
	if len(got) != 1 {
		t.Fatalf("rendered %d prompts, want exactly 1", len(got))
	}
	return got[0]
}

// hasError reports whether any captured record was an error under operation.
func hasError(records []dlog.Record, operation string) bool {
	for _, rec := range records {
		if rec.Level == "error" && rec.Operation == operation {
			return true
		}
	}
	return false
}
