package sidebar_test

import (
	"errors"
	"fmt"
	"slices"
	"strings"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	"agentrepl/protohelpers/rosterwalk"

	"agent-repld/internal/deployedversion"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/semver"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/vocab"
	"agent-repld/internal/wsm"

	"google.golang.org/protobuf/proto"
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

func (idleFooter) MergeStartedAt(ids.WorkspaceID) *int64 {
	return nil
}

func (idleFooter) TurnClock(ids.WorkspaceID) *frontendv1.FooterClock {
	return &frontendv1.FooterClock{}
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
	r.Settle()
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

	// Assert: always whole, never a delta: every stream is set.
	push := latestPush(t, r)
	switch {
	case push.GetWorkspaces().GetRepository() == nil:
		t.Fatal("the push carried no repository grouping")
	case push.GetRecentlyMerged() == nil:
		t.Fatal("the push carried no recently-merged stream")
	case push.GetMergeQueue() == nil:
		t.Fatal("the push carried no merge queue stream")
	case push.GetClosed() == nil:
		t.Fatal("the push carried no closed stream")
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
	got := rowFor(repoRows(t, (<-ch).GetWorkspaces()), "w1").GetDetail().GetSummary().GetText()
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
	got := rowFor(repoRows(t, (<-ch).GetWorkspaces()), "w1").GetDetail().GetSummary().GetText()
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
	if len(repoRows(t, (<-first).GetWorkspaces())) != 1 || len(repoRows(t, (<-second).GetWorkspaces())) != 1 {
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
	if !rowFor(repoRows(t, roster.GetWorkspaces()), "w2").GetCurrent().GetCurrent() {
		t.Fatal("the reopened subscriber's first roster did not mark w2 current")
	}
	if rowFor(repoRows(t, roster.GetWorkspaces()), "w1").GetCurrent().GetCurrent() {
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
	if !rowFor(repoRows(t, roster.GetWorkspaces()), "w2").GetCurrent().GetCurrent() {
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
	if !rowFor(repoRows(t, (<-ch).GetWorkspaces()), "w2").GetCurrent().GetCurrent() {
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
	rec.Result = &wsm.TurnResult{End: wsm.TurnResultDone}
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

// TestARegistryWithTheSelectionIsOnePush pins the select path's one push: the
// registry and the selection installed together reach an open subscriber as
// ONE roster. The sentinel registry published after it must be the very next
// delivery, so no second roster can have come between.
func TestARegistryWithTheSelectionIsOnePush(t *testing.T) {
	// Arrange: an open subscription drains the opening registry.
	r, _ := newResolver(t)
	ch := subscribe(t, r)
	r.SetRegistry(registry(workspace("w1", "one"), workspace("w2", "two")))
	<-ch

	// Act.
	r.SetRegistrySelected(registry(workspace("w1", "one"), workspace("w2", "two")), ids.WorkspaceID("w2"), sidebar.SelectStamp{})
	r.SetRegistry(registry(workspace("w1", "one"), workspace("w2", "two"), workspace("w3", "three")))

	// Assert.
	first := <-ch
	if !rowFor(repoRows(t, first.GetWorkspaces()), "w2").GetCurrent().GetCurrent() {
		t.Fatal("the select's one push did not mark w2 current")
	}
	if rowFor(repoRows(t, (<-ch).GetWorkspaces()), "w3") == nil {
		t.Fatal("a second roster arrived between the select's push and the sentinel")
	}
}

func TestARegistryWithTheSelectionMarksTheSelection(t *testing.T) {
	cases := []struct {
		name    string
		durable *ids.WorkspaceID
	}{
		{name: "a registry carrying no durable current"},
		{name: "a registry carrying another durable current", durable: func() *ids.WorkspaceID { w := ids.WorkspaceID("w1"); return &w }()},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			r, _ := newResolver(t)
			reg := registry(workspace("w1", "one"), workspace("w2", "two"))
			reg.Current = tc.durable

			// Act.
			r.SetRegistrySelected(reg, ids.WorkspaceID("w2"), sidebar.SelectStamp{})

			// Assert: the selection is installed after the registry, as the
			// two separate calls did.
			if got := latest(t, r).GetCurrent().GetWorkspace().GetId(); got != "w2" {
				t.Fatalf("current = %q, want w2", got)
			}
		})
	}
}

func TestSelectedAnswersTheRostersSelection(t *testing.T) {
	w1 := ids.WorkspaceID("w1")
	cases := []struct {
		name    string
		arrange func(r sidebar.Resolver)
		want    ids.WorkspaceID
		wantOK  bool
	}{
		{
			name:    "a roster that took no registry names no selection",
			arrange: func(sidebar.Resolver) {},
		},
		{
			name: "a registry carrying no durable current names no selection",
			arrange: func(r sidebar.Resolver) {
				r.SetRegistry(registry(workspace("w1", "one"), workspace("w2", "two")))
			},
		},
		{
			name: "a registry carrying a durable current names it",
			arrange: func(r sidebar.Resolver) {
				reg := registry(workspace("w1", "one"), workspace("w2", "two"))
				reg.Current = &w1
				r.SetRegistry(reg)
			},
			want: "w1", wantOK: true,
		},
		{
			name: "a select names the selected workspace",
			arrange: func(r sidebar.Resolver) {
				r.SetRegistrySelected(registry(workspace("w1", "one"), workspace("w2", "two")), ids.WorkspaceID("w2"), sidebar.SelectStamp{})
			},
			want: "w2", wantOK: true,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			r, _ := newResolver(t)
			tc.arrange(r)

			// Act.
			got, ok := r.Selected()

			// Assert.
			if got != tc.want || ok != tc.wantOK {
				t.Fatalf("Selected() = (%q, %v), want (%q, %v)", got, ok, tc.want, tc.wantOK)
			}
		})
	}
}

func TestARegistryWithASelectionItDoesNotCarryLeavesNoCurrent(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)

	// Act.
	r.SetRegistrySelected(registry(workspace("w1", "one")), ids.WorkspaceID("w2"), sidebar.SelectStamp{})

	// Assert.
	if got := latest(t, r).GetCurrent(); got != nil {
		t.Fatalf("current = %v, want none for a workspace the registry does not carry", got)
	}
}

// TestAKindlessDetachmentFailsHardAndChangesNothing pins the invariant the
// session watcher's gate guarantees (AgentDetachedWork.kind is required, and
// resolveDetachedLocked refuses an announcement without one): reaching the
// roster without a kind panics through protoarm.Must, before any state moves.
func TestAKindlessDetachmentFailsHardAndChangesNothing(t *testing.T) {
	// Arrange
	r := live(t, arrange(t))
	before := onlyRow(t, r)
	defer func() {
		// Assert
		got, _ := recover().(string)
		if !strings.Contains(got, "protoarm:") || !strings.Contains(got, "daemon.sidebar.on_detached_work") {
			t.Fatalf("panic = %q, want protoarm.Must naming daemon.sidebar.on_detached_work", got)
		}
		if after := onlyRow(t, r); !proto.Equal(before, after) {
			t.Fatalf("the row moved: before %v, after %v", before, after)
		}
	}()

	// Act: the roster alone, past the footer the test pair also feeds.
	roster := r.Resolver.(*fanned).Resolver
	roster.OnDetachedWork(theWS, agent("main"), &conversationv1.AgentDetachedWork{
		Owner: agent("main"),
		Work:  &conversationv1.DetachedWorkId{Value: "work-1"},
		Origin: &conversationv1.AgentDetachedWork_Detached{Detached: &conversationv1.DetachedWorkDetached{
			DetachedFromId: &conversationv1.AgentActivityId{Value: "unit-1"},
		}},
	})
	t.Fatal("a kindless detachment returned, want a panic")
}

func TestTheRosterStatesTheDeployedVersion(t *testing.T) {
	release := semver.Version{Minor: 129, Patch: 2}
	tests := []struct {
		name    string
		version *deployedversion.Version
		want    *frontendv1.RosterVersion
	}{
		{name: "a landing is stated as its release", version: &deployedversion.Version{Release: release}, want: &frontendv1.RosterVersion{Text: "0.129.2"}},
		{name: "a build past a landing is stated as the last landing's release", version: &deployedversion.Version{Release: release, Past: "0123456789ab"}, want: &frontendv1.RosterVersion{Text: "0.129.2"}},
		{name: "a withdrawn version is unset", version: nil, want: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r, _ := newResolver(t)
			r.SetRegistry(registry(workspace("w1", "one")))
			r.DeployedVersion(&deployedversion.Version{Release: semver.Version{Minor: 1}})

			// Act
			r.DeployedVersion(tt.version)

			// Assert
			if got := latest(t, r).GetVersion(); !proto.Equal(got, tt.want) {
				t.Fatalf("version = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestTheRosterStatesNoVersionBeforeOneIsResolved(t *testing.T) {
	// Arrange
	r, _ := newResolver(t)

	// Act
	r.SetRegistry(registry(workspace("w1", "one")))

	// Assert
	if got := latest(t, r).Version; got != nil {
		t.Fatalf("version = %v, want unset", got)
	}
}

func TestAVersionBeforeAnyRegistryIsDrawnOnceOneArrives(t *testing.T) {
	// Arrange
	r, _ := newResolver(t)
	r.DeployedVersion(&deployedversion.Version{Release: semver.Version{Minor: 129}})

	// Act
	r.SetRegistry(registry(workspace("w1", "one")))

	// Assert
	if got := latest(t, r).GetVersion().GetText(); got != "0.129.0" {
		t.Fatalf("version = %q, want 0.129.0", got)
	}
}

// selectionOf builds a select's change making CURRENT current, its row first
// and stamped at epoch, with the rows LEFT that stopped being current.
func selectionOf(current wsm.Workspace, left ...wsm.Workspace) wsm.Selection {
	stamped := epoch
	current.Current, current.LastSelectedAt = true, &stamped
	sel := wsm.Selection{Current: current.ID, Workspaces: []wsm.Workspace{current}, Repository: repo}
	for _, ws := range left {
		ws.Current = false
		sel.Workspaces = append(sel.Workspaces, ws)
		previous := ws.ID
		sel.Previous = &previous
	}
	return sel
}

func TestASelectionInstallsTheStoresRowsAndTheStampedSelection(t *testing.T) {
	started := epoch.Add(-30 * time.Millisecond)
	tests := []struct {
		name        string
		stamp       sidebar.SelectStamp
		wantStarted *int64
	}{
		{name: "a stamp with a switch instant states both", stamp: sidebar.SelectStamp{ID: "sel-1", SwitchStartedAt: &started}, wantStarted: proto.Int64(started.UnixMilli())},
		{name: "a stamp with no switch instant states the id alone", stamp: sidebar.SelectStamp{ID: "sel-1"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			r, _ := newResolver(t)
			w1 := workspace("w1", "one")
			w1.Current = true
			reg := registry(w1, workspace("w2", "two"))
			reg.Current = &w1.ID
			r.SetRegistry(reg)

			// Act.
			ok := r.SetSelection(selectionOf(workspace("w2", "two"), w1), tt.stamp)

			// Assert.
			if !ok {
				t.Fatal("SetSelection refused a change the registry carries")
			}
			roster := latest(t, r)
			current := roster.GetCurrent()
			if current.GetWorkspace().GetId() != "w2" || current.GetSelectionId() != "sel-1" {
				t.Fatalf("current = %v, want w2 under sel-1", current)
			}
			if !proto.Equal(wrapInt64(current.SwitchStartedAtMs), wrapInt64(tt.wantStarted)) {
				t.Fatalf("switch_started_at_ms = %v, want %v", current.SwitchStartedAtMs, tt.wantStarted)
			}
			if got := rowFor(repoRows(t, roster), "w2").GetLastSelected().GetAtMs(); got != epoch.UnixMilli() {
				t.Fatalf("w2's last-selected = %d, want the store's %d", got, epoch.UnixMilli())
			}
			if got, _ := r.Selected(); got != "w2" {
				t.Fatalf("Selected() = %q, want w2", got)
			}
		})
	}
}

// wrapInt64 makes an optional int64 comparable with proto.Equal.
func wrapInt64(v *int64) proto.Message {
	if v == nil {
		return (*frontendv1.RosterCurrentWorkspace)(nil)
	}
	return &frontendv1.RosterCurrentWorkspace{SwitchStartedAtMs: v}
}

func TestASelectionTheRegistryCannotTakeInstallsAndPublishesNothing(t *testing.T) {
	sub := wsm.Subsection{ID: "sub-1", Repo: repo.ID, Name: "later", Place: 1}
	tests := []struct {
		name string
		// seed installs the roster's registry, or nothing.
		seed func(r sidebar.Resolver)
		sel  func() wsm.Selection
	}{
		{
			name: "no registry was installed yet",
			seed: func(sidebar.Resolver) {},
			sel:  func() wsm.Selection { return selectionOf(workspace("w1", "one")) },
		},
		{
			name: "the registry carries no row the change names",
			seed: func(r sidebar.Resolver) { r.SetRegistry(registry(workspace("w1", "one"))) },
			sel:  func() wsm.Selection { return selectionOf(workspace("w9", "nine")) },
		},
		{
			name: "the registry carries no repository the change names",
			seed: func(r sidebar.Resolver) { r.SetRegistry(registry(workspace("w1", "one"))) },
			sel: func() wsm.Selection {
				sel := selectionOf(workspace("w1", "one"))
				sel.Repository = wsm.Repository{ID: "repo-9"}
				return sel
			},
		},
		{
			name: "the registry carries no subsection the change names",
			seed: func(r sidebar.Resolver) { r.SetRegistry(registry(workspace("w1", "one"))) },
			sel: func() wsm.Selection {
				sel := selectionOf(workspace("w1", "one"))
				sel.Subsection = &sub
				return sel
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			r, surfaces := newResolver(t)
			tt.seed(r)
			before := r.Revision()

			// Act.
			ok := r.SetSelection(tt.sel(), sidebar.SelectStamp{ID: "sel-1"})

			// Assert.
			if ok {
				t.Fatal("SetSelection took a change the registry cannot carry")
			}
			if got := r.Revision(); got != before {
				t.Fatalf("revision = %d, want %d: a refused change published a roster", got, before)
			}
			if _, selected := r.Selected(); selected {
				t.Fatal("a refused change installed a selection")
			}
			rec, found := dlog.FindRecord(surfaces.Records(), dlog.RecordQuery{
				Operation: "daemon.sidebar.set_selection", Message: "the roster's registry cannot take the select's change; the caller installs a whole registry"})
			if !found || rec.Level != "info" || rec.Context["selection_id"] != "sel-1" {
				t.Fatalf("the refusal's record = %+v (found %v), want info naming sel-1", rec, found)
			}
		})
	}
}

func TestTheSelectStampIsStatedOnlyWhileItsSelectionStands(t *testing.T) {
	w1, w2 := ids.WorkspaceID("w1"), ids.WorkspaceID("w2")
	tests := []struct {
		name string
		// then runs after a select stamped sel-1 made w2 current.
		then   func(r sidebar.Resolver)
		wantID string
	}{
		{
			name:   "the select's own roster states it",
			then:   func(sidebar.Resolver) {},
			wantID: "sel-1",
		},
		{
			name: "a registry carrying the same selection keeps it",
			then: func(r sidebar.Resolver) {
				reg := registry(workspace("w1", "one"), workspace("w2", "two"))
				reg.Current = &w2
				r.SetRegistry(reg)
			},
			wantID: "sel-1",
		},
		{
			name: "a registry moving the selection away and back drops it",
			then: func(r sidebar.Resolver) {
				reg := registry(workspace("w1", "one"), workspace("w2", "two"))
				reg.Current = &w1
				r.SetRegistry(reg)
				reg.Current = &w2
				r.SetRegistry(reg)
			},
		},
		{
			name: "a re-selection keeps it rather than its own",
			then: func(r sidebar.Resolver) {
				again := selectionOf(workspace("w2", "two"))
				again.Again = true
				r.SetSelection(again, sidebar.SelectStamp{ID: "sel-2"})
			},
			wantID: "sel-1",
		},
		{
			name: "an unstamped selection of the same workspace drops it",
			then: func(r sidebar.Resolver) {
				r.SetRegistrySelected(registry(workspace("w1", "one"), workspace("w2", "two")), w2, sidebar.SelectStamp{})
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			r, _ := newResolver(t)
			r.SetRegistry(registry(workspace("w1", "one"), workspace("w2", "two")))
			if !r.SetSelection(selectionOf(workspace("w2", "two")), sidebar.SelectStamp{ID: "sel-1"}) {
				t.Fatal("SetSelection refused")
			}

			// Act.
			tt.then(r)

			// Assert.
			current := latest(t, r).GetCurrent()
			if current.GetWorkspace().GetId() != "w2" || current.GetSelectionId() != tt.wantID {
				t.Fatalf("current = %v, want w2 under %q", current, tt.wantID)
			}
		})
	}
}

func TestAPatchedRegistryTakesTheStoresRowsWhole(t *testing.T) {
	// Arrange.
	stale := workspace("w3", "three")
	stale.Current = true
	folded := repo
	folded.Folded = true
	sub := wsm.Subsection{ID: "sub-1", Repo: repo.ID, Name: "later", Place: 1, Folded: true}
	reg := registry(workspace("w1", "one"), workspace("w2", "two"), stale)
	reg.Repositories = []wsm.Repository{folded}
	reg.Subsections = []wsm.Subsection{sub}
	sel := selectionOf(workspace("w2", "two"), workspace("w1", "one"))
	sel.Subsection = &wsm.Subsection{ID: "sub-1", Repo: repo.ID, Name: "later", Place: 1}

	// Act.
	got, ok := sidebar.PatchedRegistry(reg, true, sel)

	// Assert.
	if !ok {
		t.Fatal("PatchedRegistry refused a change the registry carries")
	}
	if got.Current == nil || *got.Current != "w2" {
		t.Fatalf("current = %v, want w2", got.Current)
	}
	for _, ws := range got.Workspaces {
		if want := ws.ID == "w2"; ws.Current != want {
			t.Fatalf("%s current = %v, want %v: the store makes one workspace current", ws.ID, ws.Current, want)
		}
	}
	if got.Workspaces[1].LastSelectedAt == nil {
		t.Fatal("w2 did not take the store's row")
	}
	if got.Repositories[0].Folded || got.Subsections[0].Folded {
		t.Fatalf("folds = %v/%v, want the store's expanded repository and subsection", got.Repositories[0].Folded, got.Subsections[0].Folded)
	}
	if !reg.Workspaces[2].Current || !reg.Repositories[0].Folded || !reg.Subsections[0].Folded {
		t.Fatal("PatchedRegistry wrote through to the registry it was handed")
	}
}

func TestAReSelectionPublishesNoNewRoster(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	r.SetRegistry(registry(workspace("w1", "one"), workspace("w2", "two")))
	if !r.SetSelection(selectionOf(workspace("w2", "two")), sidebar.SelectStamp{ID: "sel-1"}) {
		t.Fatal("SetSelection refused")
	}
	before := r.Revision()
	again := selectionOf(workspace("w2", "two"))
	again.Again = true

	// Act.
	ok := r.SetSelection(again, sidebar.SelectStamp{ID: "sel-2"})

	// Assert.
	if !ok {
		t.Fatal("SetSelection refused a re-selection")
	}
	if got := r.Revision(); got != before {
		t.Fatalf("revision = %d, want %d: a re-selection changes no view", got, before)
	}
}

func TestSetIncumbentArms(t *testing.T) {
	tests := []struct {
		name string
		// act installs the mirror and answers the roster published after it.
		act        func(t *testing.T, r sidebar.Resolver) *frontendv1.RosterPush
		wantStatus string
		// wantNewRevision says whether the act published a roster.
		wantNewRevision bool
	}{
		{
			name: "the caller's later writes to the map reach no render",
			act: func(t *testing.T, r sidebar.Resolver) *frontendv1.RosterPush {
				arms := map[ids.WorkspaceID]string{theWS: "thinking"}
				r.SetIncumbentArms(arms)
				arms[theWS] = "done"
				r.FooterStatusChanged(theWS)
				return latestPush(t, r)
			},
			wantStatus:      "thinking",
			wantNewRevision: true,
		},
		{
			name: "an unchanged mirror publishes nothing",
			act: func(t *testing.T, r sidebar.Resolver) *frontendv1.RosterPush {
				r.SetIncumbentArms(nil)
				return latestPush(t, r)
			},
			wantStatus:      "ready",
			wantNewRevision: false,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			r, _, _ := mirroredResolver(t)
			before := latestPush(t, r).GetRevision()

			// Act
			roster := tc.act(t, r)

			// Assert
			if got := statusName(onlyRow(t, r)); got != tc.wantStatus {
				t.Fatalf("status = %q, want %q", got, tc.wantStatus)
			}
			if moved := roster.GetRevision() != before; moved != tc.wantNewRevision {
				t.Fatalf("revision %d -> %d, want a new roster = %v", before, roster.GetRevision(), tc.wantNewRevision)
			}
		})
	}
}

// TestOnActivityRendersOnlyWhenItChangesTheRowsFacts pins OnActivity's
// decline: a turn's first activity and the answer to a retried call change
// the row's facts and are recorded; any other streamed frame declines, so it
// is not recorded and renders nothing.
func TestOnActivityRendersOnlyWhenItChangesTheRowsFacts(t *testing.T) {
	tests := []struct {
		name string
		// before is what reached the row ahead of the frame under test.
		before func(r sidebarResolver)
		// frame is the activity under test, from the agent named.
		agent      string
		frame      *conversationv1.AgentActivity
		wantRecord bool
		wantStatus string
	}{
		{
			name:       "the turn's first activity is taken",
			before:     func(sidebarResolver) {},
			agent:      "main",
			frame:      prose(),
			wantRecord: true,
			wantStatus: "thinking",
		},
		{
			name:       "a repeated activity declines",
			before:     func(r sidebarResolver) { r.OnActivity(theWS, agent("main"), prose()) },
			agent:      "main",
			frame:      prose(),
			wantRecord: false,
			wantStatus: "thinking",
		},
		{
			name: "a repeated activity answering the retried call is taken",
			before: func(r sidebarResolver) {
				r.OnActivity(theWS, agent("main"), prose())
				r.OnApiError(theWS, agent("main"), apiRetry())
			},
			agent:      "main",
			frame:      prose(),
			wantRecord: true,
			wantStatus: "thinking",
		},
		{
			name: "a repeated activity leaving the retried call unanswered declines",
			before: func(r sidebarResolver) {
				r.OnActivity(theWS, agent("main"), prose())
				r.OnApiError(theWS, agent("main"), apiRetry())
			},
			agent:      "sub-1",
			frame:      prose(),
			wantRecord: false,
			wantStatus: "api_retrying",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			r := live(t, arrange(t))
			r.SetTurn(theWS, &footer.TurnStarted{At: epoch})
			tt.before(r)
			before := len(dlog.FilterRecords(r.surfaces.Records(), dlog.RecordQuery{Operation: "daemon.sidebar.on_activity"}))

			// Act.
			r.OnActivity(theWS, agent(tt.agent), tt.frame)

			// Assert.
			recorded := len(dlog.FilterRecords(r.surfaces.Records(), dlog.RecordQuery{Operation: "daemon.sidebar.on_activity"})) > before
			if recorded != tt.wantRecord {
				t.Fatalf("on_activity recorded = %v, want %v", recorded, tt.wantRecord)
			}
			if got := statusName(onlyRow(t, r)); got != tt.wantStatus {
				t.Fatalf("status = %q, want %q", got, tt.wantStatus)
			}
		})
	}
}

// TestARegisteredWorkspaceWhoseSinkCannotResolveLogsCentrallyAsOrdinary pins
// logFor for a landed workspace whose worktree was reaped: its records go to
// the central sink naming it, and nothing records an invariant violation
// (TestALateFactsRefusalIsOfferedAgainMidWork, 2026-11-13).
func TestARegisteredWorkspaceWhoseSinkCannotResolveLogsCentrallyAsOrdinary(t *testing.T) {
	// Arrange: the workspace's directory can host no sink from the first
	// resolve, as a reaped worktree is: the real surfaces stat a directory
	// once, so a sink that resolved never stops resolving, and the roster
	// keeps the sink it resolved (kept.go).
	r, surfaces := newResolver(t)
	surfaces.BindWorkspaceIDs(func(string) (string, error) {
		return "", errors.New("the worktree was reaped")
	})
	r.SetRegistry(registry(workspace(string(theWS), "one")))

	// Act: a fact that moves the row's status arm.
	r.OnLink(theWS, shimclient.LinkConnected)

	// Assert.
	var routed bool
	r.Settle()
	for _, rec := range surfaces.Records() {
		if _, ok := rec.Context["invariant_violation"]; ok {
			t.Fatalf("record %s carries an invariant violation: %+v", rec.Operation, rec.Context)
		}
		if rec.Operation == "daemon.sidebar.status_arm_changed" && rec.Context["workspace_id"] == string(theWS) && rec.Context[dlog.KeyUnroutableWorkspace] != nil {
			routed = true
		}
	}
	if !routed {
		t.Fatalf("records = %+v, want the arm change on the central sink naming the workspace", surfaces.Records())
	}
}

// ---- the four snapshot streams on one push ----

func TestEachStreamCarriesItsOwnSnapshot(t *testing.T) {
	// Arrange: one workspace of each standing, and a queue holding the open
	// one.
	r, _ := newResolver(t)
	closed := workspace("c", "closed")
	closed.Closed = true
	r.SetMergeQueue(theRepo, sidebar.MergeQueue{Workspaces: []ids.WorkspaceID{"o"}, Line: inLine("o")})

	// Act.
	r.SetRegistry(queueView(workspace("o", "open"), backloggedWorkspace("h", "hidden"), closed, mergedAt("m", time.Hour)))

	// Assert.
	push := latestPush(t, r)
	tests := []struct {
		name string
		got  []string
		want []string
	}{
		{name: "workspaces holds the open and hidden rows", got: rowNames(rosterwalk.AllRows(&frontendv1.RosterPush{Workspaces: push.GetWorkspaces()})), want: []string{"open", "hidden"}},
		{name: "closed holds the closed row", got: rowNames(rosterwalk.ClosedRows(push.GetClosed())), want: []string{"closed"}},
		{name: "the merge queue holds the queued workspace", got: queueSubjectIDs(queueGroup(push, repo)), want: []string{"o"}},
		{name: "recently merged holds the merge's summary", got: []string{fmt.Sprint(push.GetRecentlyMerged().GetHeader().GetCount().GetWorkspaces())}, want: []string{"1"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if !equal(tt.got, tt.want) {
				t.Fatalf("got %v, want %v", tt.got, tt.want)
			}
		})
	}
}

// streamPointers names the message each stream of PUSH is.
func streamPointers(push *frontendv1.RosterPush) [4]proto.Message {
	return [4]proto.Message{push.GetWorkspaces(), push.GetMergeQueue(), push.GetRecentlyMerged(), push.GetClosed()}
}

func TestAnUnchangedStreamKeepsTheLastPushsMessage(t *testing.T) {
	const (
		workspaces = iota
		mergeQueue
		recentlyMerged
		closedRows
	)
	closed := workspace("c", "closed")
	closed.Closed = true
	tests := []struct {
		name string
		act  func(r sidebar.Resolver)
		// changed are the streams the act changes.
		changed []int
	}{
		{name: "an open row's change resends the workspaces stream alone", act: func(r sidebar.Resolver) { r.SetSummary("o", "a prompt") }, changed: []int{workspaces}},
		{name: "a closed row's change resends the closed stream alone", act: func(r sidebar.Resolver) { r.SetSummary("c", "a prompt") }, changed: []int{closedRows}},
		{name: "a queue's change resends the merge queue alone", act: func(r sidebar.Resolver) {
			r.SetMergeQueue(theRepo, sidebar.MergeQueue{Workspaces: []ids.WorkspaceID{"o"}, Line: inLine("o")})
		}, changed: []int{mergeQueue}},
		{name: "a merge landing resends the workspaces and the summary", act: func(r sidebar.Resolver) {
			r.SetRegistry(queueView(mergedAt("o", 0), closed, mergedAt("m", time.Hour)))
		}, changed: []int{workspaces, recentlyMerged}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			r, _ := newResolver(t)
			r.SetRegistry(queueView(workspace("o", "open"), closed, mergedAt("m", time.Hour)))
			before := latestPush(t, r)

			// Act.
			tt.act(r)

			// Assert: exactly the changed streams are new messages, and the
			// push took the next revision.
			after := latestPush(t, r)
			if after.GetRevision() != before.GetRevision()+1 {
				t.Fatalf("revision = %d, want %d", after.GetRevision(), before.GetRevision()+1)
			}
			was, now := streamPointers(before), streamPointers(after)
			for i := range was {
				if moved := was[i] != now[i]; moved != slices.Contains(tt.changed, i) {
					t.Fatalf("stream %d moved = %v, want %v", i, moved, slices.Contains(tt.changed, i))
				}
			}
		})
	}
}

func TestAnIdenticalRenderPublishesTheLastPushItself(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	reg := registry(workspace("o", "open"), mergedAt("m", time.Hour))
	r.SetRegistry(reg)
	before := latestPush(t, r)

	// Act.
	r.SetRegistry(reg)

	// Assert.
	if after := latestPush(t, r); after != before {
		t.Fatalf("push = %p revision %d, want the last push %p kept at %d", after, after.GetRevision(), before, before.GetRevision())
	}
}

func TestAMergeQueueReStampThatChangesNothingPublishesNothing(t *testing.T) {
	// Arrange: a waiting member's merge facts and its queue, published.
	r, _ := newResolver(t)
	ch := subscribe(t, r)
	r.SetRegistry(queueView(workspace("w1", "one"), workspace("w2", "two")))
	<-ch
	queue := sidebar.MergeQueue{Workspaces: []ids.WorkspaceID{"w1", "w2"}, Line: inLine("w1", "w2")}
	r.SetMergeQueue(theRepo, queue)
	r.SetMerge("w2", mergeStates["queued"])
	<-ch
	<-ch
	before := r.Revision()

	// Act: the orchestrator re-stamps the same facts and the same queue, then
	// a sentinel change.
	r.SetMerge("w2", mergeStates["queued"])
	r.SetMergeQueue(theRepo, queue)
	r.SetSummary("w1", "a sentinel")

	// Assert: the next push is the sentinel's, one revision on.
	next := <-ch
	if next.GetRevision() != before+1 || rowFor(innerRows(rosterwalk.OpenRows(next.GetWorkspaces())), "w1").GetDetail().GetSummary().GetText() != "a sentinel" {
		t.Fatalf("next push = revision %d, want the sentinel's at %d: a re-stamp that changes nothing was published", next.GetRevision(), before+1)
	}
}

func TestATransitionMovesEveryStreamItTouchesInOnePush(t *testing.T) {
	reopened := mergedAt("w", time.Hour)
	reopened.Closed = false
	tests := []struct {
		name          string
		before, after sidebar.Registry
		wantOpen      bool
		wantCount     uint32
	}{
		{name: "a merge landing leaves workspaces and joins the summary", before: registry(workspace("w", "w")), after: registry(mergedAt("w", 0)), wantOpen: false, wantCount: 1},
		{name: "a merged workspace reopened leaves the summary and joins workspaces", before: registry(mergedAt("w", time.Hour)), after: registry(reopened), wantOpen: true, wantCount: 0},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			r, _ := newResolver(t)
			ch := subscribe(t, r)
			r.SetRegistry(tt.before)
			first := <-ch

			// Act.
			r.SetRegistry(tt.after)

			// Assert: the one next push carries both sides of the move.
			push := <-ch
			if push.GetRevision() != first.GetRevision()+1 {
				t.Fatalf("revision = %d, want %d", push.GetRevision(), first.GetRevision()+1)
			}
			if open := rowFor(repoRows(t, push.GetWorkspaces()), "w") != nil; open != tt.wantOpen {
				t.Fatalf("open = %v, want %v", open, tt.wantOpen)
			}
			merged := push.GetRecentlyMerged()
			if got := merged.GetHeader().GetCount().GetWorkspaces(); got != tt.wantCount {
				t.Fatalf("merged count = %d, want %d", got, tt.wantCount)
			}
			if got := merged.GetMergedRevision().GetRevision(); got != push.GetRevision() {
				t.Fatalf("merged token = %d, want this push's revision %d", got, push.GetRevision())
			}
		})
	}
}

func TestARenderIsRecordedAtInfoOnlyWhenAPersonCanFeelIt(t *testing.T) {
	cases := []struct {
		name    string
		elapsed time.Duration
		want    string
	}{
		{name: "an instant render is debug", elapsed: 0, want: dlog.LevelDebug},
		{name: "a render just under the bound is debug", elapsed: 99 * time.Millisecond, want: dlog.LevelDebug},
		{name: "a render at the bound is info", elapsed: 100 * time.Millisecond, want: dlog.LevelInfo},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := sidebar.RenderLevel(tc.elapsed)

			// Assert
			if got != tc.want {
				t.Fatalf("RenderLevel(%v) = %s, want %s", tc.elapsed, got, tc.want)
			}
		})
	}
}

func TestARenderRecordsItsCauseAndWhatItWalked(t *testing.T) {
	// Arrange
	r, surfaces := newResolver(t)
	r.SetRegistry(registry(workspace("one", "one"), workspace("two", "two")))
	mark := len(surfaces.Records())

	// Act
	r.OnLink("one", shimclient.LinkConnected)

	// Assert
	rec, ok := dlog.FindRecord(surfaces.Records()[mark:], dlog.RecordQuery{Operation: "daemon.sidebar.rendered",
		Match: func(rec dlog.Record) bool { return rec.Context["cause"] == "daemon.sidebar.on_link" }})
	if !ok {
		t.Fatalf("no render record names the link edge as its cause: %v", surfaces.Records()[mark:])
	}
	for key, want := range map[string]int{"workspaces": 2, "arms_derived": 1, "arms_kept": 1, "rows_built": 1, "rows_kept": 1} {
		if got := rec.Context[key]; got != want {
			t.Errorf("the render record's %s = %v, want %d", key, got, want)
		}
	}
	if _, ok := rec.Context["elapsed_ms"].(int64); !ok {
		t.Errorf("the render record's elapsed_ms = %#v, want milliseconds", rec.Context["elapsed_ms"])
	}
}

func TestTheCurrentWorkspaceStatesItsDepartureLanding(t *testing.T) {
	selected := time.Date(2026, 11, 24, 12, 0, 0, 0, time.UTC)
	parent := ids.WorkspaceID("w1")
	tests := []struct {
		name       string
		workspaces func() []wsm.Workspace
		want       string
	}{
		{
			name: "the landing is the daemon's landing rule's",
			workspaces: func() []wsm.Workspace {
				w1, w2, w3 := workspace("w1", "one"), workspace("w2", "two"), workspace("w3", "three")
				w2.Parent = &parent
				later := selected.Add(time.Minute)
				w1.LastSelectedAt, w3.LastSelectedAt = &selected, &later
				return []wsm.Workspace{w1, w2, w3}
			},
			want: "w1",
		},
		{
			name:       "no other drawn tab states no landing",
			workspaces: func() []wsm.Workspace { return []wsm.Workspace{workspace("w2", "two")} },
			want:       "",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r, _ := newResolver(t)
			reg := registry(tt.workspaces()...)
			current := ids.WorkspaceID("w2")
			reg.Current = &current
			r.SetRegistry(reg)

			// Act
			roster := <-subscribe(t, r)

			// Assert
			if got := roster.GetWorkspaces().GetCurrent().GetDepartureLanding().GetId(); got != tt.want {
				t.Fatalf("departure landing = %q, want %q", got, tt.want)
			}
		})
	}
}
