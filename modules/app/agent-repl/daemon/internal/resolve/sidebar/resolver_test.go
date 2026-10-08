package sidebar_test

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/resolve/sidebar"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/vocab"
	"claude-repld/internal/wsm"
)

func TestNewRefusesWithoutLogSurfaces(t *testing.T) {
	// Arrange, Act.
	_, err := sidebar.New(testColors(), nil, idleFooter{})

	// Assert.
	if err == nil {
		t.Fatal("sidebar.New accepted nil log surfaces")
	}
}

// idleFooter answers the idle status for every workspace: a status source for
// the construction tests, which publish no row.
type idleFooter struct{}

func (idleFooter) Status(ids.WorkspaceID) *frontendv1.FooterStatus {
	return &frontendv1.FooterStatus{Status: &frontendv1.FooterStatus_Idle{Idle: &frontendv1.FooterStatusIdle{}}}
}

func TestNewRefusesWithoutAFooterStatus(t *testing.T) {
	// Arrange, Act.
	_, err := sidebar.New(testColors(), dlog.NewTestSurfaces(), nil)

	// Assert: a roster with no footer has no status to project.
	if err == nil {
		t.Fatal("sidebar.New accepted no footer status")
	}
}

func TestNewRefusesAnEmptyRosterStatusTable(t *testing.T) {
	// Arrange, Act.
	_, err := sidebar.New(vocab.RenderColors{}, dlog.NewTestSurfaces(), idleFooter{})

	// Assert.
	if err == nil {
		t.Fatal("sidebar.New accepted render colors with no roster_status table")
	}
}

func TestNewRefusesARosterStatusTableMissingAnArm(t *testing.T) {
	// Arrange.
	colors := testColors()
	delete(colors.RosterStatus, "vendor_blocked")

	// Act.
	_, err := sidebar.New(colors, dlog.NewTestSurfaces(), idleFooter{})

	// Assert: an unpainted dot fails here, never on the wire.
	if err == nil {
		t.Fatal("sidebar.New accepted a table with no color for an arm it emits")
	}
}

func TestNewRefusesARosterStatusTableWithASurplusRow(t *testing.T) {
	// Arrange.
	colors := testColors()
	colors.RosterStatus["invented"] = "grey"

	// Act.
	_, err := sidebar.New(colors, dlog.NewTestSurfaces(), idleFooter{})

	// Assert: a colored state nothing can reach means the table has drifted.
	if err == nil {
		t.Fatal("sidebar.New accepted a table coloring a state that is no arm")
	}
}

func TestNewRefusesAMergeGlyphsTableMissingAnArm(t *testing.T) {
	// Arrange.
	colors := testColors()
	delete(colors.MergeGlyphs, "merging")

	// Act.
	_, err := sidebar.New(colors, dlog.NewTestSurfaces(), idleFooter{})

	// Assert.
	if err == nil {
		t.Fatal("sidebar.New accepted a table with no glyph for a merge arm")
	}
}

func TestRosterPublishesNothingBeforeAnyRegistry(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)

	// Act: a live frame with no registry behind it.
	r.OnLink(theWS, shimclient.LinkConnected)

	// Assert: a roster built from live frames alone would name no workspaces.
	if _, ok := r.Topic().Latest(); ok {
		t.Fatal("the roster published before any registry")
	}
}

func TestRosterPublishesOnTheFirstRegistry(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)

	// Act.
	r.SetRegistry(registry(workspace("w1", "one")))

	// Assert.
	if _, ok := r.Topic().Latest(); !ok {
		t.Fatal("the roster published nothing on its first registry")
	}
}

func TestRosterPublishesTheWholeRosterOnEveryPush(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)

	// Act.
	r.SetRegistry(registry(workspace("w1", "one")))

	// Assert: always whole, never a delta.
	roster := latest(t, r)
	switch {
	case roster.GetRepository() == nil:
		t.Fatal("the push carried no repository grouping")
	case roster.GetTask() == nil:
		t.Fatal("the push carried no task grouping")
	case roster.GetRecentlyMerged() == nil:
		t.Fatal("the push carried no recently-merged section")
	}
}

func TestRosterRepublishesWhenAFactChanges(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	ch := subscribe(t, r)
	r.SetRegistry(registry(workspace("w1", "one")))
	<-ch

	// Act.
	r.SetSummary(theWS, "a new prompt")

	// Assert.
	got := rowFor(repoRows(t, <-ch), "w1").GetDetail().GetSummary().GetText()
	if got != "a new prompt" {
		t.Fatalf("republished summary = %q, want the change", got)
	}
}

func TestRosterDoesNotRepublishAnIdenticalRender(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	ch := subscribe(t, r)
	reg := registry(workspace("w1", "one"))
	r.SetRegistry(reg)
	<-ch

	// Act: the same registry again, then a change that must still arrive.
	r.SetRegistry(reg)
	r.SetSummary(theWS, "a change")

	// Assert: the duplicate never reached the wire.
	got := rowFor(repoRows(t, <-ch), "w1").GetDetail().GetSummary().GetText()
	if got != "a change" {
		t.Fatalf("next delivery carried %q, want the change — a duplicate reached the wire", got)
	}
}

func TestOneStreamServesEveryWebview(t *testing.T) {
	// Arrange: the roster carries no workspace, so there is one topic to have.
	r, _ := newResolver(t)
	first := subscribe(t, r)
	second := subscribe(t, r)

	// Act.
	r.SetRegistry(registry(workspace("w1", "one")))

	// Assert.
	if len(repoRows(t, <-first)) != 1 || len(repoRows(t, <-second)) != 1 {
		t.Fatal("the two subscribers did not receive the same roster")
	}
}

// TestAReopeningSubscriberReplaysTheCurrentSelection is the reopen/reconnect
// regression: a webview whose roster stream ended (a daemon bounce, a
// producer_ended) reopens AFTER the selection was published, and its FIRST
// delivery must carry the correct current row — not a stale highlight it holds
// until a full reload. The topic retains its latest value, so a subscription
// opened after the publish replays it at once.
func TestAReopeningSubscriberReplaysTheCurrentSelection(t *testing.T) {
	// Arrange: a selection is published while no subscriber is attached.
	r, _ := newResolver(t)
	r.SetRegistry(registry(workspace("w1", "one"), workspace("w2", "two")))
	r.SetSelected(ids.WorkspaceID("w2"))

	// Act: the reopened stream subscribes only now.
	roster := <-subscribe(t, r)

	// Assert: the first delivery already states w2 as the current row.
	if !rowFor(repoRows(t, roster), "w2").GetCurrent().GetCurrent() {
		t.Fatal("the reopened subscriber's first roster did not mark w2 current")
	}
	if rowFor(repoRows(t, roster), "w1").GetCurrent().GetCurrent() {
		t.Fatal("the reopened subscriber's first roster marked the stale row current")
	}
}

// TestAReopeningSubscriberReplaysABootRestoredSelection is the (b) guard: a
// daemon that just rebuilt its resolver has published nothing from a live
// change, only the boot prime's registry snapshot (PublishRegistry ->
// SetRegistry carrying the durable Current). A reopening subscriber must still
// replay that restored selection, so the topic holds a publishable current
// roster with no live change behind it.
func TestAReopeningSubscriberReplaysABootRestoredSelection(t *testing.T) {
	// Arrange: the boot prime's registry snapshot is the ONLY publish.
	r, _ := newResolver(t)
	reg := registry(workspace("w1", "one"), workspace("w2", "two"))
	restored := ids.WorkspaceID("w2")
	reg.Current = &restored
	r.SetRegistry(reg)

	// Act: a webview reopens against the rebuilt resolver.
	roster := <-subscribe(t, r)

	// Assert.
	if !rowFor(repoRows(t, roster), "w2").GetCurrent().GetCurrent() {
		t.Fatal("the reopened subscriber did not replay the boot-restored selection")
	}
}

// TestALiveSelectionReachesAnAlreadyOpenSubscriber is the no-regression guard:
// serving a reopened subscriber its retained latest must not cost an
// already-open subscriber the later publishes. A selection made after the
// subscription still arrives on the wire.
func TestALiveSelectionReachesAnAlreadyOpenSubscriber(t *testing.T) {
	// Arrange: an open subscription drains the opening registry.
	r, _ := newResolver(t)
	ch := subscribe(t, r)
	r.SetRegistry(registry(workspace("w1", "one"), workspace("w2", "two")))
	<-ch

	// Act.
	r.SetSelected(ids.WorkspaceID("w2"))

	// Assert.
	if !rowFor(repoRows(t, <-ch), "w2").GetCurrent().GetCurrent() {
		t.Fatal("a live selection did not reach the already-open subscriber")
	}
}

func TestANukedWorkspaceLeavesTheRoster(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	r.SetRegistry(registry(workspace("w1", "one"), workspace("w2", "two")))

	// Act: the registry no longer carries it.
	r.SetRegistry(registry(workspace("w2", "two")))

	// Assert.
	if rowFor(repoRows(t, latest(t, r)), "w1") != nil {
		t.Fatal("a nuked workspace kept its row")
	}
}

func TestANukedWorkspaceLosesItsSelection(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	r.SetRegistry(registry(workspace("w1", "one"), workspace("w2", "two")))
	r.SetSelected(theWS)

	// Act.
	r.SetRegistry(registry(workspace("w2", "two")))

	// Assert.
	if got := latest(t, r).GetCurrent(); got != nil {
		t.Fatalf("current = %v, want no selection once the workspace is gone", got)
	}
}

func TestANukedWorkspacesSessionFactsAreForgotten(t *testing.T) {
	// Arrange: a workspace whose session was live, then nuked and re-registered.
	r, _ := newResolver(t)
	r.SetRegistry(registry(workspace("w1", "one")))
	r.OnLink(theWS, shimclient.LinkConnected)
	r.OnSessionStarted(theWS, &conversationv1.SessionStarted{VendorSessionId: "vendor-1"})

	// Act.
	r.SetRegistry(registry(workspace("w2", "two")))
	r.SetRegistry(registry(workspace("w1", "one")))

	// Assert: a lingering accumulation would resurrect the old row's dot.
	if got := statusName(rowFor(repoRows(t, latest(t, r)), "w1")); got != "none" {
		t.Fatalf("status = %q, want none — the live half outlived the nuke", got)
	}
}

func TestRosterRecordsAFrameForAWorkspaceTheRegistryDoesNotCarry(t *testing.T) {
	// Arrange.
	r, surfaces := newResolver(t)
	r.SetRegistry(registry(workspace("w1", "one")))

	// Act.
	r.OnLink(ids.WorkspaceID("w-stranger"), shimclient.LinkConnected)

	// Assert.
	var found bool
	for _, rec := range surfaces.Records() {
		if rec.Context["invariant_violation"] == "roster fact for a workspace the registry does not carry" {
			found = true
		}
	}
	if !found {
		t.Fatal("a frame for an unregistered workspace was not recorded as an invariant violation")
	}
}

func TestASessionUpdateWithNoArmChangesNothing(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))

	// Act.
	r.OnSessionUpdate(theWS, &conversationv1.SessionUpdate{})

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "ready" {
		t.Fatalf("status = %q, want the row unchanged", got)
	}
}

func TestANilSessionUpdateChangesNothing(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))

	// Act.
	r.OnSessionUpdate(theWS, nil)

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "ready" {
		t.Fatalf("status = %q, want the row unchanged", got)
	}
}

// TestNewRefusesASurplusMergeGlyphRow pins the direction the resolver's own
// copy of the assertion never checked: a merge_glyphs row naming no merge arm
// is a state the vocabulary paints and the resolver can never emit, so the
// vocabulary's assertion refuses it.
func TestNewRefusesASurplusMergeGlyphRow(t *testing.T) {
	// Arrange.
	colors := testColors()
	colors.MergeGlyphs["merge_teleported"] = "recycle"

	// Act.
	_, err := sidebar.New(colors, dlog.NewTestSurfaces(), idleFooter{})

	// Assert.
	if err == nil {
		t.Fatal("New accepted a merge_glyphs row naming no merge arm")
	}
}

// TestAViewIsPublishedBeforeAnotherChangeCanPublishItsOwn pins the roster's
// publication order: a change's view is published under the lock that
// rendered it, so a change made after it can never be overwritten by it. The
// result sink is told off the lock and is the window a later change runs in:
// here the accepted prompt's ack and first activity land inside it.
// (2026-10-02: the roster walked submitting, ready, thinking for an accepted
// prompt, a stale view published after a newer one.)
func TestAViewIsPublishedBeforeAnotherChangeCanPublishItsOwn(t *testing.T) {
	// Arrange: a restored result, which the next turn's start reports gone.
	var r sidebar.Resolver
	inSink := false
	r, _ = newResolver(t, sidebar.WithResultSink(
		func(ids.WorkspaceID, *wsm.TurnResult) {
			if inSink {
				return
			}
			inSink = true
			r.AckTurn(theWS)
			r.OnActivity(theWS, &conversationv1.AgentId{Value: "main"}, &conversationv1.AgentActivity{})
		}))
	rec := workspace(string(theWS), "one")
	rec.Result = &wsm.TurnResult{End: wsm.TurnResultDone, Read: true}
	r.SetRegistry(registry(rec))
	r.OnLink(theWS, shimclient.LinkConnected)
	r.OnSessionStarted(theWS, &conversationv1.SessionStarted{VendorSessionId: "vendor-1"})

	// Act
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})

	// Assert
	if !inSink {
		t.Fatal("the result sink was never told the cleared result")
	}
	if got := statusName(onlyRow(t, r)); got != "thinking" {
		t.Fatalf("latest status = %q, want thinking: the submitting view was published after the newer one", got)
	}
}
