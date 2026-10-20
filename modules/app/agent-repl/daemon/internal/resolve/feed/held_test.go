package feed

import (
	"context"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/holds"
	"agent-repld/internal/sessionwatcher"
	"agent-repld/internal/wsm"
)

// THE HELD SECTION (held.go): what has not yet settled into the root feed's
// history, carried on the root feed's own publication log, and settled into a
// row in ONE publication.

// heldQueuedAt is the instant held prompts are stamped with unless a test needs
// an order.
var heldQueuedAt = time.UnixMilli(1_700_000_000_000)

// standingHold is a standing hold of TURN carrying TEXT, queued AFTER
// milliseconds past heldQueuedAt.
func standingHold(turn, text string, after int64) wsm.HeldPrompt {
	return wsm.HeldPrompt{
		Workspace: testWorkspace,
		Turn:      ids.TurnID(turn),
		Said: &conversationv1.UserSaid{Content: &conversationv1.UserContent{
			Blocks: []*conversationv1.UserContentBlock{{
				Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: text}},
			}},
		}},
		QueuedAt: heldQueuedAt.Add(time.Duration(after) * time.Millisecond),
	}
}

// mirrorOf is the acceptance mirror the prompt queue draws for TURN.
func mirrorOf(turn, text string) *frontendv1.FeedRow {
	return &frontendv1.FeedRow{
		Turn: &conversationv1.TurnId{Value: turn},
		Row: &frontendv1.FeedRow_UserPrompt{UserPrompt: &frontendv1.FeedUserPrompt{
			Result: &frontendv1.FeedUserPrompt_Success{Success: &frontendv1.FeedUserPromptSuccess{
				Body: &frontendv1.FeedUserPromptBody{Blocks: []*frontendv1.FeedUserPromptBlock{{
					Block: &frontendv1.FeedUserPromptBlock_Text{Text: &frontendv1.FeedTextBlock{Text: text}},
				}}},
			}},
		}},
	}
}

// mirror draws TURN's acceptance mirror, as the prompt queue does.
func (h *harness) mirror(turn, text string) {
	h.resolver.UpsertAtTurnAddress(testWorkspace, ids.TurnID(turn),
		feedid.RowKey{Kind: feedid.KindPrompt, ID: turn}, mirrorOf(turn, text))
}

// rootPublications opens the root feed's page and its tail, answering the
// held section the page was served with and the tail's publications.
func (h *harness) rootPublications() (*frontendv1.FeedHeld, <-chan Publication) {
	h.t.Helper()
	_, token, held, err := h.resolver.OpenPage(context.Background(), testWorkspace, rootFeed())
	if err != nil {
		h.t.Fatalf("OpenPage: %v", err)
	}
	ctx, cancel := context.WithCancel(context.Background())
	h.t.Cleanup(cancel)
	tail, err := h.resolver.Tail(ctx, testWorkspace, rootFeed(), token)
	if err != nil {
		h.t.Fatalf("Tail: %v", err)
	}
	return held, tail.Publications(ctx)
}

// next reads the next publication, failing when none arrives.
func next(t *testing.T, pubs <-chan Publication) Publication {
	t.Helper()
	select {
	case pub := <-pubs:
		return pub
	case <-time.After(2 * time.Second):
		t.Fatal("no publication arrived")
		return Publication{}
	}
}

// itemIDs answers the section's item ids, in order.
func itemIDs(held *frontendv1.FeedHeld) []string {
	out := make([]string, 0, len(held.GetItems()))
	for _, item := range held.GetItems() {
		out = append(out, item.GetId().GetValue())
	}
	return out
}

// sameIDs reports whether two id lists are equal.
func sameIDs(a, b []string) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

// heldNow answers the section as last published.
func (h *harness) heldNow() *frontendv1.FeedHeld {
	h.resolver.mu.Lock()
	defer h.resolver.mu.Unlock()
	return h.resolver.currentHeld(h.resolver.state(testWorkspace))
}

func TestStandingHoldsAreHeldInQueueOrderUnderTheirPromptRowIDs(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.resolver.SetHeldPrompts(testWorkspace, []wsm.HeldPrompt{
		standingHold("turn-2", "second", 10), standingHold("turn-1", "first", 0),
	})

	// Assert
	want := []string{h.promptRowID("turn-1"), h.promptRowID("turn-2")}
	if got := itemIDs(h.heldNow()); !sameIDs(got, want) {
		t.Fatalf("held ids = %v, want %v", got, want)
	}
}

func TestAHoldsChangeIsPublishedOnTheRootFeed(t *testing.T) {
	// Arrange
	h := newHarness(t)
	_, pubs := h.rootPublications()

	// Act
	h.resolver.SetHeldPrompts(testWorkspace, []wsm.HeldPrompt{standingHold("turn-1", "first", 0)})

	// Assert
	pub := next(t, pubs)
	if pub.Held == nil || len(pub.Held.GetRows()) != 0 {
		t.Fatalf("publication = %v, want a held change moving no row", pub)
	}
	if got := itemIDs(pub.Held.GetHeld()); !sameIDs(got, []string{h.promptRowID("turn-1")}) {
		t.Fatalf("held ids = %v, want the one prompt", got)
	}
}

func TestAnOrdinaryHoldsChangeNamesNoFold(t *testing.T) {
	// Arrange
	h := newHarness(t)
	_, pubs := h.rootPublications()

	// Act
	h.resolver.SetHeldPrompts(testWorkspace, []wsm.HeldPrompt{standingHold("turn-1", "first", 0)})

	// Assert
	if folds := next(t, pubs).Held.GetFolds(); len(folds) != 0 {
		t.Fatalf("folds = %v, want none", folds)
	}
}

func TestAFoldIsPublishedNamingBothHeldItems(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.resolver.SetHeldPrompts(testWorkspace, []wsm.HeldPrompt{
		standingHold("turn-1", "first", 0), standingHold("turn-2", "second", 10),
	})
	_, pubs := h.rootPublications()

	// Act
	h.resolver.FoldHeldPrompts(testWorkspace, []wsm.HeldPrompt{standingHold("turn-1", "first\n\nsecond", 0)},
		holds.Fold{From: "turn-2", Into: "turn-1"})

	// Assert
	pub := next(t, pubs)
	if got := itemIDs(pub.Held.GetHeld()); !sameIDs(got, []string{h.promptRowID("turn-1")}) {
		t.Fatalf("held ids = %v, want the entry folded into alone", got)
	}
	folds := pub.Held.GetFolds()
	if len(folds) != 1 || folds[0].GetFolded().GetValue() != h.promptRowID("turn-2") ||
		folds[0].GetInto().GetValue() != h.promptRowID("turn-1") {
		t.Fatalf("folds = %v, want turn-2's item folded into turn-1's", folds)
	}
}

func TestAFoldIsPublishedWhenTheSectionDrawsNeitherItem(t *testing.T) {
	// Arrange: the section looks the same before and after, yet the fold is
	// still the change's to name.
	h := newHarness(t)
	standing := []wsm.HeldPrompt{standingHold("turn-1", "first", 0)}
	h.resolver.SetHeldPrompts(testWorkspace, standing)
	_, pubs := h.rootPublications()

	// Act
	h.resolver.FoldHeldPrompts(testWorkspace, standing, holds.Fold{From: "turn-8", Into: "turn-9"})

	// Assert
	if folds := next(t, pubs).Held.GetFolds(); len(folds) != 1 {
		t.Fatalf("folds = %v, want the one fold", folds)
	}
}

func TestAnUnchangedSectionIsNotPublishedAgain(t *testing.T) {
	// Arrange
	h := newHarness(t)
	holds := []wsm.HeldPrompt{standingHold("turn-1", "first", 0)}
	h.resolver.SetHeldPrompts(testWorkspace, holds)
	_, pubs := h.rootPublications()

	// Act: the same holds, then a marker row.
	h.resolver.SetHeldPrompts(testWorkspace, holds)
	h.deliverPrompt("turn-9", "a marker")

	// Assert: the marker is the first publication after the pin.
	if pub := next(t, pubs); pub.Row.GetId().GetValue() != h.promptRowID("turn-9") {
		t.Fatalf("first publication = %v, want the marker row", pub)
	}
}

func TestTheOpenedRootPageCarriesTheSectionAtThePin(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.resolver.SetHeldPrompts(testWorkspace, []wsm.HeldPrompt{standingHold("turn-1", "first", 0)})

	// Act
	held, _ := h.rootPublications()

	// Assert
	if got := itemIDs(held); !sameIDs(got, []string{h.promptRowID("turn-1")}) {
		t.Fatalf("opened held ids = %v, want the one prompt", got)
	}
}

func TestAnOpenedSubFeedCarriesNoSection(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.resolver.SetHeldPrompts(testWorkspace, []wsm.HeldPrompt{standingHold("turn-1", "first", 0)})
	lease := ids.LeaseID("lease-7")

	// Act
	_, _, held, err := h.resolver.OpenPage(context.Background(), testWorkspace, feedid.Feed{Merge: &lease})

	// Assert
	if err != nil {
		t.Fatalf("OpenPage: %v", err)
	}
	if held != nil {
		t.Fatalf("a sub-feed was served a held section: %v", held)
	}
}

func TestAMirroredHoldSettlesAsOnePublicationCarryingItsRow(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.resolver.SetHeldPrompts(testWorkspace, []wsm.HeldPrompt{standingHold("turn-1", "first", 0)})
	_, pubs := h.rootPublications()

	// Act
	h.mirror("turn-1", "first")

	// Assert
	pub := next(t, pubs)
	if pub.Held == nil {
		t.Fatalf("publication = %v, want one held change", pub)
	}
	if got := itemIDs(pub.Held.GetHeld()); len(got) != 0 {
		t.Fatalf("held ids after the settle = %v, want none", got)
	}
	rows := pub.Held.GetRows()
	if len(rows) != 1 || rows[0].GetId().GetValue() != h.promptRowID("turn-1") || rows[0].GetUserPrompt() == nil {
		t.Fatalf("settled rows = %v, want the prompt's row", rows)
	}
}

func TestARetiredMirrorReturnsItsStandingHoldInOnePublication(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.resolver.SetHeldPrompts(testWorkspace, []wsm.HeldPrompt{standingHold("turn-1", "first", 0)})
	h.mirror("turn-1", "first")
	_, pubs := h.rootPublications()

	// Act
	h.resolver.OnPromptRetired(testWorkspace, &conversationv1.AgentPrompt{Id: &conversationv1.TurnId{Value: "turn-1"}})

	// Assert
	pub := next(t, pubs)
	if pub.Held == nil {
		t.Fatalf("publication = %v, want one held change", pub)
	}
	if got := itemIDs(pub.Held.GetHeld()); !sameIDs(got, []string{h.promptRowID("turn-1")}) {
		t.Fatalf("held ids = %v, want the prompt back", got)
	}
	rows := pub.Held.GetRows()
	if len(rows) != 1 || rows[0].GetRemoved() == nil {
		t.Fatalf("moved rows = %v, want the mirror's removal", rows)
	}
}

func TestAMirrorAtAnotherFeedLeavesTheRootSectionWithoutARow(t *testing.T) {
	// Arrange
	h := newHarness(t)
	lease := ids.LeaseID("lease-7")
	h.resolver.AddressTurn(testWorkspace, "turn-1", &sessionwatcher.OutputAddress{Feed: feedid.Feed{Merge: &lease}})
	h.resolver.SetHeldPrompts(testWorkspace, []wsm.HeldPrompt{standingHold("turn-1", "first", 0)})
	_, pubs := h.rootPublications()

	// Act
	h.mirror("turn-1", "first")

	// Assert
	pub := next(t, pubs)
	if pub.Held == nil || len(pub.Held.GetRows()) != 0 || len(pub.Held.GetHeld().GetItems()) != 0 {
		t.Fatalf("root publication = %v, want an emptied section moving no row", pub)
	}
}

func TestARetiredHoldIsNotHeld(t *testing.T) {
	// Arrange
	h := newHarness(t)
	retired := standingHold("turn-1", "gone", 0)
	retired.Tombstone = &wsm.Tombstone{Kind: "delivered", At: heldQueuedAt}

	// Act
	h.resolver.SetHeldPrompts(testWorkspace, []wsm.HeldPrompt{retired})

	// Assert
	if got := itemIDs(h.heldNow()); len(got) != 0 {
		t.Fatalf("held ids = %v, want none", got)
	}
}

func TestTheEditClaimMarksItsHeldPrompt(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.resolver.SetHeldPrompts(testWorkspace, []wsm.HeldPrompt{standingHold("turn-1", "first", 0)})

	// Act
	h.resolver.SetEditing(testWorkspace, "turn-1")

	// Assert
	if items := h.heldNow().GetItems(); len(items) != 1 || items[0].GetPrompt().GetEditing() == nil {
		t.Fatalf("held items = %v, want the prompt marked as being edited", items)
	}
}

func TestAResetKeepsTheSection(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.resolver.SetHeldPrompts(testWorkspace, []wsm.HeldPrompt{standingHold("turn-1", "first", 0)})

	// Act
	h.resolver.ResetWorkspace(testWorkspace, "a bind")

	// Assert
	if got := len(h.heldNow().GetItems()); got != 1 {
		t.Fatalf("held items after a reset = %d, want the prompt still held", got)
	}
}

// TestARootFeedWithNothingHeldOpensWithTheEmptySection pins that an empty held
// section is a complete answer: the root feed of a workspace holding nothing
// opens with it rather than leaving the section unstated.
func TestARootFeedWithNothingHeldOpensWithTheEmptySection(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	held, _ := h.rootPublications()

	// Assert.
	if held == nil || len(held.GetItems()) != 0 {
		t.Fatalf("held section = %v, want the stated empty section", held)
	}
}

// ingressEntry is an unaccepted ingress entry under KEY carrying TEXT.
func ingressEntry(key, text string) holds.IngressEntry {
	return holds.IngressEntry{
		Key:      key,
		Said:     standingHold("unused", text, 0).Said,
		QueuedAt: heldQueuedAt,
		Wait:     holds.IngressWait{Kind: holds.IngressNoSession},
	}
}

// ingressID is the item id an ingress entry under KEY is held under.
func (h *harness) ingressID(key string) string {
	h.resolver.mu.Lock()
	defer h.resolver.mu.Unlock()
	return h.resolver.ingressItemID(h.resolver.state(testWorkspace), key).GetValue()
}

func TestIngressEntriesAreHeldAfterTheStandingHoldsUnderTheirOwnIDs(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.resolver.SetHeldPrompts(testWorkspace, []wsm.HeldPrompt{standingHold("turn-1", "first", 0)})

	// Act
	h.resolver.SetIngressPending(testWorkspace, []holds.IngressEntry{ingressEntry("k-1", "a"), ingressEntry("k-2", "b")})

	// Assert
	want := []string{h.promptRowID("turn-1"), h.ingressID("k-1"), h.ingressID("k-2")}
	if got := itemIDs(h.heldNow()); !sameIDs(got, want) {
		t.Fatalf("held ids = %v, want %v", got, want)
	}
	if card := h.heldNow().GetItems()[1].GetIngress(); card.GetBadge().GetNoSession() == nil {
		t.Fatalf("ingress card = %v, want the no-session badge", card)
	}
}

func TestStatingIngressEntriesIsPublishedOnTheRootFeed(t *testing.T) {
	// Arrange
	h := newHarness(t)
	_, pubs := h.rootPublications()

	// Act
	h.resolver.SetIngressPending(testWorkspace, []holds.IngressEntry{ingressEntry("k-1", "a")})

	// Assert
	pub := next(t, pubs)
	if got := itemIDs(pub.Held.GetHeld()); !sameIDs(got, []string{h.ingressID("k-1")}) {
		t.Fatalf("published held ids = %v, want the ingress entry", got)
	}
}

func TestAClaimAloneChangesNothingDrawn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.resolver.SetIngressPending(testWorkspace, []holds.IngressEntry{ingressEntry("k-1", "a")})

	// Act
	h.resolver.ClaimIngress(testWorkspace, "k-1", "turn-1")

	// Assert
	if got := itemIDs(h.heldNow()); !sameIDs(got, []string{h.ingressID("k-1")}) {
		t.Fatalf("held ids = %v, want the entry still drawn until its turn is", got)
	}
}

func TestAClaimedEntrySettlesIntoItsHoldInOnePublication(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.resolver.SetIngressPending(testWorkspace, []holds.IngressEntry{ingressEntry("k-1", "a")})
	h.resolver.ClaimIngress(testWorkspace, "k-1", "turn-1")
	_, pubs := h.rootPublications()

	// Act
	h.resolver.SetHeldPrompts(testWorkspace, []wsm.HeldPrompt{standingHold("turn-1", "a", 0)})

	// Assert
	pub := next(t, pubs)
	if got := itemIDs(pub.Held.GetHeld()); !sameIDs(got, []string{h.promptRowID("turn-1")}) {
		t.Fatalf("held ids = %v, want the hold alone, never beside its entry", got)
	}
}

func TestAClaimedEntrySettlesIntoItsRowInOnePublication(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.resolver.SetIngressPending(testWorkspace, []holds.IngressEntry{ingressEntry("k-1", "a")})
	h.resolver.ClaimIngress(testWorkspace, "k-1", "turn-1")
	_, pubs := h.rootPublications()

	// Act
	h.mirror("turn-1", "a")

	// Assert
	pub := next(t, pubs)
	if pub.Held == nil {
		t.Fatalf("publication = %v, want one held change carrying the row", pub)
	}
	if got := itemIDs(pub.Held.GetHeld()); len(got) != 0 {
		t.Fatalf("held ids = %v, want none once the row is drawn", got)
	}
	rows := pub.Held.GetRows()
	if len(rows) != 1 || rows[0].GetId().GetValue() != h.promptRowID("turn-1") {
		t.Fatalf("settled rows = %v, want the prompt's row", rows)
	}
}

func TestASettledEntryNeverReturnsWhenItsHoldLeaves(t *testing.T) {
	// Arrange: the entry settled into its hold.
	h := newHarness(t)
	h.resolver.SetIngressPending(testWorkspace, []holds.IngressEntry{ingressEntry("k-1", "a")})
	h.resolver.ClaimIngress(testWorkspace, "k-1", "turn-1")
	h.resolver.SetHeldPrompts(testWorkspace, []wsm.HeldPrompt{standingHold("turn-1", "a", 0)})

	// Act: the hold is folded away before the ingress drops the entry.
	h.resolver.SetHeldPrompts(testWorkspace, nil)

	// Assert
	if got := itemIDs(h.heldNow()); len(got) != 0 {
		t.Fatalf("held ids = %v, want none: the entry already settled", got)
	}
}

func TestDroppingAnEntryRemovesIt(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.resolver.SetIngressPending(testWorkspace, []holds.IngressEntry{ingressEntry("k-1", "a"), ingressEntry("k-2", "b")})

	// Act
	h.resolver.DropIngress(testWorkspace, "k-1")

	// Assert
	if got := itemIDs(h.heldNow()); !sameIDs(got, []string{h.ingressID("k-2")}) {
		t.Fatalf("held ids = %v, want only the entry not dropped", got)
	}
}

func TestStatingEntriesForgetsTheClaimsOfEntriesNoLongerStated(t *testing.T) {
	// Arrange: k-1 claimed for turn-1, then gone from the ingress.
	h := newHarness(t)
	h.resolver.SetIngressPending(testWorkspace, []holds.IngressEntry{ingressEntry("k-1", "a")})
	h.resolver.ClaimIngress(testWorkspace, "k-1", "turn-1")
	h.resolver.SetIngressPending(testWorkspace, nil)

	// Act: an entry under the same key is stated again.
	h.resolver.SetIngressPending(testWorkspace, []holds.IngressEntry{ingressEntry("k-1", "a")})
	h.resolver.SetHeldPrompts(testWorkspace, []wsm.HeldPrompt{standingHold("turn-1", "a", 0)})

	// Assert
	want := []string{h.promptRowID("turn-1"), h.ingressID("k-1")}
	if got := itemIDs(h.heldNow()); !sameIDs(got, want) {
		t.Fatalf("held ids = %v, want %v: a forgotten claim settles nothing", got, want)
	}
}
