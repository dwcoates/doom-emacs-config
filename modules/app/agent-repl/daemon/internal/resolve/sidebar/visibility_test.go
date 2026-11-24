package sidebar_test

import (
	"sync"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	"agentrepl/protohelpers/rosterwalk"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/wsm"
)

// visibilityName reads a row's visibility arm by name.
func visibilityName(v *frontendv1.RosterRowVisibility) string {
	switch v.GetVisibility().(type) {
	case *frontendv1.RosterRowVisibility_NotVisible:
		return "not_visible"
	case *frontendv1.RosterRowVisibility_UnselectedVisible:
		return "unselected_visible"
	case *frontendv1.RosterRowVisibility_Selected:
		return "selected"
	default:
		return "unset"
	}
}

// visibilities reads the visibility of every row the resolver last published,
// of every merged row it pages, and of every thread row, by id.
func visibilities(t *testing.T, r sidebar.Resolver) map[string]string {
	t.Helper()
	push := latestPush(t, r)
	out := map[string]string{}
	for _, row := range append(rosterwalk.AllRows(push), mergedRows(t, r)...) {
		out[row.GetWorkspace().GetWorkspace().GetId()] = visibilityName(row.GetVisibility())
	}
	for _, row := range push.GetWorkspaces().GetThreads().GetRows() {
		out[row.GetThread().GetId()] = visibilityName(row.GetVisibility())
	}
	return out
}

// foldedOne is theWS alone, in a collapsed repository section.
func foldedOne() sidebar.Registry {
	reg := registry(workspace(string(theWS), "one"))
	reg.Repositories[0].Folded = true
	return reg
}

// liveRegistry brings every workspace REG registers to a proven, idle session
// (see live), so none is the not-yet-started one a fold leaves visible.
func liveRegistry(r sidebar.Resolver, reg sidebar.Registry) {
	for _, ws := range reg.Workspaces {
		r.OnLink(ws.ID, shimclient.LinkConnected)
		r.OnSessionStarted(ws.ID, &conversationv1.SessionStarted{VendorSessionId: "vendor-" + string(ws.ID)})
	}
}

// working brings WS to a running turn: its session is live and a prompt's
// turn runs.
func working(r sidebarResolver, ws ids.WorkspaceID) {
	r.OnLink(ws, shimclient.LinkConnected)
	r.OnSessionStarted(ws, &conversationv1.SessionStarted{VendorSessionId: "vendor-" + string(ws)})
	r.SetTurn(ws, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})
	r.AckTurn(ws)
}

// finished runs one turn of WS to its end, its result unread.
func turnFinished(r sidebarResolver, ws ids.WorkspaceID) {
	working(r, ws)
	r.SetTurnEnded(ws, wsm.CloseCompleted)
}

func TestAFoldedIdleWorkspaceIsNotVisible(t *testing.T) {
	// Arrange
	r := arrange(t, foldedOne().Workspaces...)
	r.SetRegistry(foldedOne())
	live(t, r)

	// Act
	got := visibilities(t, r)

	// Assert
	if got[string(theWS)] != "not_visible" {
		t.Fatalf("visibility = %s, want not_visible: a fold covers an idle, read workspace", got[string(theWS)])
	}
}

func TestAFoldedWorkingWorkspaceIsVisible(t *testing.T) {
	// Arrange
	r := arrange(t)
	r.SetRegistry(foldedOne())

	// Act
	working(r, theWS)

	// Assert
	if got := visibilities(t, r)[string(theWS)]; got != "unselected_visible" {
		t.Fatalf("visibility = %s, want unselected_visible: a working workspace is visible under a fold", got)
	}
}

func TestAFoldedUnviewedWorkspaceIsVisible(t *testing.T) {
	// Arrange
	r := arrange(t)
	r.SetRegistry(foldedOne())

	// Act
	turnFinished(r, theWS)

	// Assert
	if got := visibilities(t, r)[string(theWS)]; got != "unselected_visible" {
		t.Fatalf("visibility = %s, want unselected_visible: an unread result keeps a folded workspace visible", got)
	}
}

func TestAFoldedUnviewedWorkspaceStaysVisibleWhileItsTabOpens(t *testing.T) {
	// Arrange: kept visible only by its unread result, which an open in
	// flight draws as `inactive`.
	r := arrange(t)
	r.SetRegistry(foldedOne())
	turnFinished(r, theWS)

	// Act
	r.SetOpening(theWS, true)

	// Assert: the open is the editor acting on the visibility, and moves none.
	if got := visibilities(t, r)[string(theWS)]; got != "unselected_visible" {
		t.Fatalf("visibility = %s, want unselected_visible while the tab opens", got)
	}
	if got := statusName(onlyRow(t, r)); got != "inactive" {
		t.Fatalf("status = %q, want the opening row drawn inactive", got)
	}
}

func TestAFoldedReadWorkspaceStaysNotVisibleWhileItsTabOpens(t *testing.T) {
	// Arrange: an idle workspace whose result is read, under a fold.
	r := arrange(t)
	r.SetRegistry(foldedOne())
	liveRegistry(r, foldedOne())

	// Act
	r.SetOpening(theWS, true)

	// Assert: the open shows nothing the fold hides.
	if got := visibilities(t, r)[string(theWS)]; got != "not_visible" {
		t.Fatalf("visibility = %s, want not_visible while the tab opens", got)
	}
}

func TestAFoldedStartingWorkspaceIsVisible(t *testing.T) {
	// Arrange
	r := arrange(t)
	r.SetRegistry(foldedOne())

	// Act: the session is dialing, which draws the grey init mark.
	r.OnLink(theWS, shimclient.LinkDialing)

	// Assert
	if got := visibilities(t, r)[string(theWS)]; got != "unselected_visible" {
		t.Fatalf("visibility = %s, want unselected_visible: a starting workspace is visible under a fold", got)
	}
}

func TestAFoldedWorkspaceWithNoSessionYetIsVisible(t *testing.T) {
	// Arrange: just created, before its session's first dial.
	r := arrange(t, foldedOne().Workspaces...)

	// Act
	r.SetRegistry(foldedOne())

	// Assert
	if got := visibilities(t, r)[string(theWS)]; got != "unselected_visible" {
		t.Fatalf("visibility = %s, want unselected_visible: a workspace not yet started is visible under a fold", got)
	}
}

func TestAFoldedWorkspaceOfTopPriorityIsVisible(t *testing.T) {
	tests := []struct {
		name     string
		priority wsm.Priority
		want     string
	}{
		{name: "P0.5", priority: wsm.PriorityP05, want: "unselected_visible"},
		{name: "P1", priority: wsm.PriorityP1, want: "unselected_visible"},
		{name: "P2", priority: wsm.PriorityP2, want: "not_visible"},
		{name: "P3", priority: wsm.PriorityP3, want: "not_visible"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			reg := foldedOne()
			reg.Workspaces[0] = prioritized(reg.Workspaces[0], tt.priority)
			r := arrange(t, reg.Workspaces...)

			// Act
			r.SetRegistry(reg)
			liveRegistry(r, reg)

			// Assert
			if got := visibilities(t, r)[string(theWS)]; got != tt.want {
				t.Fatalf("visibility = %s, want %s", got, tt.want)
			}
		})
	}
}

func TestTheParentOfAFoldedTopPriorityWorkspaceIsVisible(t *testing.T) {
	// Arrange
	parent, child := workspace("w-parent", "parent"), prioritized(workspace("w-child", "child"), wsm.PriorityP1)
	pid := ids.WorkspaceID("w-parent")
	child.Parent = &pid
	reg := registry(parent, child)
	reg.Repositories[0].Folded = true
	r := arrange(t, reg.Workspaces...)

	// Act
	r.SetRegistry(reg)

	// Assert
	got := visibilities(t, r)
	if got["w-parent"] != "unselected_visible" || got["w-child"] != "unselected_visible" {
		t.Fatalf("visibilities = %v, want the top-priority child and its parent visible", got)
	}
}

func TestAFoldedWorkspaceWhoseResultIsReadIsNotVisible(t *testing.T) {
	// Arrange
	r := arrange(t)
	r.SetRegistry(foldedOne())
	turnFinished(r, theWS)

	// Act
	r.SetInteracted(theWS, epoch)

	// Assert
	if got := visibilities(t, r)[string(theWS)]; got != "not_visible" {
		t.Fatalf("visibility = %s, want not_visible once the result is read", got)
	}
}

func TestAVisibleRowUnderAFoldHasANumberedSurfacedTab(t *testing.T) {
	// Arrange
	r := arrange(t)
	r.SetRegistry(foldedOne())

	// Act
	working(r, theWS)

	// Assert
	row := rosterwalk.FlattenOpen(rosterwalk.UnsectionedRows(latest(t, r).GetRepository().GetSections()[0]))[0]
	if _, numbered := row.GetTab().GetTab().(*frontendv1.RosterOpenRowTab_Numbered); !numbered || !row.GetSurfaced() {
		t.Fatalf("tab = %v surfaced = %v, want a numbered tab drawn inside the fold", row.GetTab(), row.GetSurfaced())
	}
}

func TestTheParentOfAVisibleWorkspaceUnderAFoldIsVisible(t *testing.T) {
	// Arrange: the family sits in a collapsed repository; one child works.
	reg := blockFamily()
	reg.Repositories[0].Folded = true
	r := arrange(t)
	r.SetRegistry(reg)

	// Act
	working(r, "w-a1")

	// Assert
	got := visibilities(t, r)
	if got["w-p"] != "unselected_visible" || got["w-a1"] != "unselected_visible" {
		t.Fatalf("parent %s, child %s; want both visible: the parent of a visible workspace is visible", got["w-p"], got["w-a1"])
	}
}

func TestASiblingOfAVisibleWorkspaceUnderAFoldStaysNotVisible(t *testing.T) {
	// Arrange
	reg := blockFamily()
	reg.Repositories[0].Folded = true
	r := arrange(t)
	r.SetRegistry(reg)
	liveRegistry(r, reg)

	// Act
	working(r, "w-a1")

	// Assert
	if got := visibilities(t, r)["w-g1"]; got != "not_visible" {
		t.Fatalf("sibling visibility = %s, want not_visible", got)
	}
}

func TestTheSelectionIsSelectedUnderAFold(t *testing.T) {
	// Arrange
	r := arrange(t)

	// Act
	r.SetRegistrySelected(foldedOne(), theWS, sidebar.SelectStamp{})

	// Assert
	if got := visibilities(t, r)[string(theWS)]; got != "selected" {
		t.Fatalf("visibility = %s, want selected", got)
	}
}

func TestAHiddenSelectionIsSelected(t *testing.T) {
	// Arrange
	r := arrange(t)

	// Act
	r.SetRegistrySelected(registry(hiddenWorkspace(string(theWS), "one")), theWS, sidebar.SelectStamp{})

	// Assert
	if got := visibilities(t, r)[string(theWS)]; got != "selected" {
		t.Fatalf("visibility = %s, want selected: the selection is selected whatever excludes it", got)
	}
}

func TestAHiddenWorkspaceNobodyIsOnIsNotVisible(t *testing.T) {
	// Arrange
	r := arrange(t)

	// Act
	r.SetRegistry(registry(hiddenWorkspace(string(theWS), "one")))

	// Assert
	if got := visibilities(t, r)[string(theWS)]; got != "not_visible" {
		t.Fatalf("visibility = %s, want not_visible", got)
	}
}

func TestAThreadIsSelectedOnlyWhileItIsTheSelection(t *testing.T) {
	cases := []struct {
		name     string
		selected ids.WorkspaceID
		want     string
	}{
		{name: "the selected thread", selected: "t-1", want: "selected"},
		{name: "a thread the user left", selected: "w-a", want: "not_visible"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			r, _ := newResolver(t)

			// Act
			r.SetRegistrySelected(withThreads(thread("t-1", "why", nil)), tc.selected, sidebar.SelectStamp{})

			// Assert
			if got := visibilities(t, r)["t-1"]; got != tc.want {
				t.Fatalf("thread visibility = %s, want %s", got, tc.want)
			}
		})
	}
}

// visibilitySink records every change the resolver hands its visibility sink.
type visibilitySink struct {
	mu      sync.Mutex
	changes []map[ids.WorkspaceID]wsm.Visibility
}

func (s *visibilitySink) record(changes map[ids.WorkspaceID]wsm.Visibility) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.changes = append(s.changes, changes)
}

func (s *visibilitySink) all() []map[ids.WorkspaceID]wsm.Visibility {
	s.mu.Lock()
	defer s.mu.Unlock()
	return append([]map[ids.WorkspaceID]wsm.Visibility(nil), s.changes...)
}

func TestEveryVisibilityChangeIsHandedToTheSink(t *testing.T) {
	// Arrange
	sink := &visibilitySink{}
	r, _ := newResolver(t, sidebar.WithVisibilitySink(sink.record))

	// Act: drawn first, then folded away.
	r.SetRegistry(registry(workspace(string(theWS), "one")))
	liveRegistry(r, registry(workspace(string(theWS), "one")))
	r.SetRegistry(foldedOne())
	liveRegistry(r, foldedOne())

	// Assert
	got := sink.all()
	if len(got) != 2 || got[0][theWS] != wsm.UnselectedVisible || got[1][theWS] != wsm.NotVisible {
		t.Fatalf("changes = %v, want visible then not visible", got)
	}
}

func TestAnOpenOfAFoldedUnviewedWorkspaceHandsTheSinkNoChange(t *testing.T) {
	// Arrange: kept visible only by its unread result. Each editor open raises
	// the opening marker and lowers it once the open answers; on 2026-11-23 the
	// marker's `inactive` moved the visibility both ways, so every open tore
	// the tab down and every teardown opened it again, about 800 times.
	sink := &visibilitySink{}
	resolver, surfaces := newResolver(t, sidebar.WithVisibilitySink(sink.record))
	r := sidebarResolver{Resolver: resolver, surfaces: surfaces}
	r.SetRegistry(foldedOne())
	turnFinished(r, theWS)
	before := len(sink.all())

	// Act: the open cycle the editor runs, more than once.
	for range 3 {
		r.SetOpening(theWS, true)
		r.SetOpening(theWS, false)
	}

	// Assert
	if got := sink.all()[before:]; len(got) != 0 {
		t.Fatalf("changes = %v, want none: an open acts on the visibility and moves none", got)
	}
	if got := r.Visibility(theWS); got != wsm.UnselectedVisible {
		t.Fatalf("visibility = %v, want unselected_visible after the opens", got)
	}
}

func TestAnUnchangedVisibilityIsNotHandedToTheSinkAgain(t *testing.T) {
	// Arrange
	sink := &visibilitySink{}
	r, _ := newResolver(t, sidebar.WithVisibilitySink(sink.record))
	r.SetRegistry(registry(workspace(string(theWS), "one")))

	// Act
	r.SetRegistry(registry(workspace(string(theWS), "one")))

	// Assert
	if got := sink.all(); len(got) != 1 {
		t.Fatalf("changes = %v, want the one change only", got)
	}
}

func TestAVisibilityTheStoreAlreadyHoldsIsNotWrittenAgain(t *testing.T) {
	// Arrange
	sink := &visibilitySink{}
	r, _ := newResolver(t, sidebar.WithVisibilitySink(sink.record))
	ws := workspace(string(theWS), "one")
	ws.Visibility = wsm.UnselectedVisible

	// Act
	r.SetRegistry(registry(ws))

	// Assert
	if got := sink.all(); len(got) != 0 {
		t.Fatalf("changes = %v, want none: the store already holds the derived value", got)
	}
}

func TestVisibilityAnswersTheLastRendersValue(t *testing.T) {
	// Arrange
	r, _ := newResolver(t)

	// Act
	r.SetRegistrySelected(registry(workspace(string(theWS), "one"), workspace("w2", "two")), theWS, sidebar.SelectStamp{})

	// Assert
	if r.Visibility(theWS) != wsm.Selected || r.Visibility("w2") != wsm.UnselectedVisible || r.Visibility("w-gone") != wsm.NotVisible {
		t.Fatalf("visibility = %v, %v, %v; want selected, visible, and not visible for a workspace never derived",
			r.Visibility(theWS), r.Visibility("w2"), r.Visibility("w-gone"))
	}
}

func TestVisibilityBeforeAnyRenderIsTheStoredOne(t *testing.T) {
	cases := []struct {
		name string
		ws   ids.WorkspaceID
		want wsm.Visibility
	}{
		{name: "a workspace stored selected", ws: "w-selected", want: wsm.Selected},
		{name: "a workspace stored visible", ws: "w-visible", want: wsm.UnselectedVisible},
		{name: "a workspace stored not visible", ws: "w-hidden", want: wsm.NotVisible},
		{name: "a workspace the registry does not carry", ws: "w-unknown", want: wsm.NotVisible},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: a resolver that booted or joined and has not rendered.
			r := sidebar.UnrenderedHolding(
				wsm.Workspace{ID: "w-selected", Visibility: wsm.Selected},
				wsm.Workspace{ID: "w-visible", Visibility: wsm.UnselectedVisible},
				wsm.Workspace{ID: "w-hidden", Visibility: wsm.NotVisible},
			)

			// Act
			got := r.Visibility(tc.ws)

			// Assert
			if got != tc.want {
				t.Fatalf("Visibility(%s) = %v, want %v", tc.ws, got, tc.want)
			}
		})
	}
}

// visibilityRecords answers every visibility change record of WS.
func visibilityRecords(surfaces *dlog.TestSurfaces, ws ids.WorkspaceID) []dlog.Record {
	var out []dlog.Record
	for _, rec := range dlog.FilterRecords(surfaces.Records(), dlog.RecordQuery{Operation: "daemon.sidebar.visibility_changed"}) {
		if rec.Context[dlog.KeyWorkspaceID] == string(ws) {
			out = append(out, rec)
		}
	}
	return out
}

func TestAVisibilityChangeIsRecordedWithItsCauseInputsAndRevision(t *testing.T) {
	// Arrange
	r, surfaces := newResolver(t)
	r.SetRegistry(registry(workspace(string(theWS), "one")))
	liveRegistry(r, registry(workspace(string(theWS), "one")))

	// Act: folded away.
	r.SetRegistry(foldedOne())

	// Assert
	recs := visibilityRecords(surfaces, theWS)
	if len(recs) == 0 {
		t.Fatalf("no visibility change record: %v", surfaces.Records())
	}
	got := recs[len(recs)-1]
	want := dlog.Context{
		"visibility": "not_visible", "previous": "unselected_visible", "cause": "daemon.sidebar.set_registry",
		"arm": "ready", "open": true, "covered": true, "working": false, "unviewed": false, "starting": false,
		"top_priority": false, "selected": false, "published": true, "revision": latestPush(t, r).GetRevision(),
	}
	for key, value := range want {
		if got.Context[key] != value {
			t.Fatalf("context[%q] = %v, want %v (record %v)", key, got.Context[key], value, got.Context)
		}
	}
	if got.Level != dlog.LevelInfo {
		t.Fatalf("level = %q, want info", got.Level)
	}
}

func TestAFoldedWorkspaceWithNoSessionYetIsRecordedVisibleForStarting(t *testing.T) {
	// Arrange
	r, surfaces := newResolver(t)

	// Act
	r.SetRegistry(foldedOne())

	// Assert
	recs := visibilityRecords(surfaces, theWS)
	if len(recs) != 1 {
		t.Fatalf("visibility records = %v, want the one change", recs)
	}
	ctx := recs[0].Context
	if ctx["visibility"] != "unselected_visible" || ctx["starting"] != true || ctx["covered"] != true || ctx["arm"] != "none" {
		t.Fatalf("context = %v, want visible under the fold because it is starting", ctx)
	}
}

func TestAVisibilityChangeAQuietRosterHoldsBackIsRecordedUnpublished(t *testing.T) {
	// Arrange
	r, gate, _, _, surfaces := quietResolver(t)
	gate.Set(true)

	// Act
	r.SetRegistry(foldedOne())

	// Assert
	recs := visibilityRecords(surfaces, theWS)
	got := recs[len(recs)-1]
	if got.Context["visibility"] != "not_visible" || got.Context["published"] != false {
		t.Fatalf("context = %v, want the change recorded unpublished", got.Context)
	}
	if _, ok := got.Context["revision"]; ok {
		t.Fatalf("context = %v, want no revision for a roster not published", got.Context)
	}
}

func TestTheNextPublicationIsToldItsRevisionOnce(t *testing.T) {
	// Arrange
	r, _ := newResolver(t)
	r.SetRegistry(registry(workspace(string(theWS), "one")))
	var told []uint64
	r.NotifyNextPublication(func(revision uint64) { told = append(told, revision) })

	// Act
	r.SetSummary(theWS, "a first summary")
	first := latestPush(t, r).GetRevision()
	r.SetSummary(theWS, "a second summary")

	// Assert
	if len(told) != 1 || told[0] != first {
		t.Fatalf("told = %v, want once, the revision %d", told, first)
	}
}

func TestACommitThatPublishesNothingTellsNoNextPublication(t *testing.T) {
	// Arrange
	r, gate, _, _, _ := quietResolver(t)
	var told []uint64
	r.NotifyNextPublication(func(revision uint64) { told = append(told, revision) })
	gate.Set(true)

	// Act
	r.SetSummary(theWS, "a summary while the lid is closed")

	// Assert
	if len(told) != 0 {
		t.Fatalf("told = %v, want nothing for a roster not published", told)
	}
}

// visibilityTelemetry records what the resolver tells its visibility
// telemetry.
type visibilityTelemetry struct {
	mu      sync.Mutex
	counts  []map[wsm.VisibilityReason]int
	changes []string
	reads   []sidebar.ResultReadBy
}

func (v *visibilityTelemetry) SetVisibilityCounts(counts map[wsm.VisibilityReason]int) {
	v.mu.Lock()
	defer v.mu.Unlock()
	v.counts = append(v.counts, counts)
}

func (v *visibilityTelemetry) CountVisibilityChange(from, to wsm.Visibility, reason wsm.VisibilityReason) {
	v.mu.Lock()
	defer v.mu.Unlock()
	v.changes = append(v.changes, from.String()+">"+to.String()+":"+reason.String())
}

func (v *visibilityTelemetry) CountResultRead(by sidebar.ResultReadBy) {
	v.mu.Lock()
	defer v.mu.Unlock()
	v.reads = append(v.reads, by)
}

func (v *visibilityTelemetry) last() map[wsm.VisibilityReason]int {
	v.mu.Lock()
	defer v.mu.Unlock()
	if len(v.counts) == 0 {
		return nil
	}
	return v.counts[len(v.counts)-1]
}

func TestTheVisibilityCountsNameEveryWorkspacesReason(t *testing.T) {
	// Arrange
	tel := &visibilityTelemetry{}
	plain, surfaces := newResolver(t, sidebar.WithVisibilityTelemetry(tel))
	r := sidebarResolver{Resolver: plain, surfaces: surfaces}
	reg := foldedOne()
	reg.Workspaces = append(reg.Workspaces, workspace("w2", "two"))
	reg.Workspaces[1].Repo = reg.Workspaces[0].Repo

	// Act: the folded workspace works, the other is idle under the same fold.
	r.SetRegistry(reg)
	liveRegistry(r, reg)
	working(r, theWS)

	// Assert
	got := tel.last()
	if got[wsm.ReasonCoveredWorking] != 1 || got[wsm.ReasonCovered] != 1 || len(got) != len(wsm.VisibilityReasonsAll) {
		t.Fatalf("counts = %v, want one covered_working, one covered, every reason named", got)
	}
}

func TestAnUnchangedVisibilityCountIsNotSetAgain(t *testing.T) {
	// Arrange
	tel := &visibilityTelemetry{}
	r, _ := newResolver(t, sidebar.WithVisibilityTelemetry(tel))
	r.SetRegistry(registry(workspace(string(theWS), "one")))
	before := len(tel.counts)

	// Act
	r.SetRegistry(registry(workspace(string(theWS), "one")))

	// Assert
	if len(tel.counts) != before {
		t.Fatalf("counts were set %d more times for a render that changed none", len(tel.counts)-before)
	}
}

func TestEveryVisibilityChangeIsCountedWithItsReason(t *testing.T) {
	// Arrange
	tel := &visibilityTelemetry{}
	r, _ := newResolver(t, sidebar.WithVisibilityTelemetry(tel))

	// Act: drawn first, then folded away while idle.
	r.SetRegistry(registry(workspace(string(theWS), "one")))
	liveRegistry(r, registry(workspace(string(theWS), "one")))
	r.SetRegistry(foldedOne())
	liveRegistry(r, foldedOne())

	// Assert
	want := []string{"not_visible>unselected_visible:uncovered", "unselected_visible>not_visible:covered"}
	if len(tel.changes) != 2 || tel.changes[0] != want[0] || tel.changes[1] != want[1] {
		t.Fatalf("changes = %v, want %v", tel.changes, want)
	}
}

func TestAResultReadIsCountedByWhatReadIt(t *testing.T) {
	tests := []struct {
		name string
		act  func(r sidebarResolver)
		want []sidebar.ResultReadBy
	}{
		{name: "an interaction that views the row", act: func(r sidebarResolver) {
			turnFinished(r, theWS)
			r.SetInteracted(theWS, epoch)
		}, want: []sidebar.ResultReadBy{sidebar.ReadByInteraction}},
		{name: "a repeat on a row already viewed is not counted again", act: func(r sidebarResolver) {
			turnFinished(r, theWS)
			r.SetInteracted(theWS, epoch)
			r.SetInteracted(theWS, epoch.Add(time.Second))
		}, want: []sidebar.ResultReadBy{sidebar.ReadByInteraction}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			tel := &visibilityTelemetry{}
			plain, surfaces := newResolver(t, sidebar.WithVisibilityTelemetry(tel))
			r := sidebarResolver{Resolver: plain, surfaces: surfaces}
			r.SetRegistry(registry(workspace(string(theWS), "one")))

			// Act
			tt.act(r)

			// Assert
			if len(tel.reads) != len(tt.want) || (len(tt.want) == 1 && tel.reads[0] != tt.want[0]) {
				t.Fatalf("reads = %v, want %v", tel.reads, tt.want)
			}
		})
	}
}

func TestAVisibilityChangeRecordNamesItsReason(t *testing.T) {
	// Arrange
	r, surfaces := newResolver(t)
	r.SetRegistry(registry(workspace(string(theWS), "one")))
	liveRegistry(r, registry(workspace(string(theWS), "one")))

	// Act: folded away while idle.
	r.SetRegistry(foldedOne())

	// Assert
	recs := visibilityRecords(surfaces, theWS)
	if len(recs) == 0 || recs[len(recs)-1].Context["reason"] != "covered" {
		t.Fatalf("visibility records = %v, want the last naming reason covered", recs)
	}
}

// subsectionFamily is a parent with a child and a grandchild, all in one
// subsection of a collapsed repository section, beside a workspace in no
// subsection.
func subsectionFamily(subsectionFolded bool) sidebar.Registry {
	sub := subsection("sub-1", "work", 1)
	sub.Folded = subsectionFolded
	parent := in(placed(workspace("s-p", "parent"), 1), sub)
	kid := in(placed(workspace("s-k", "kid"), 1), sub)
	kid.Parent = &parent.ID
	grand := in(placed(workspace("s-g", "grand"), 1), sub)
	grand.Parent = &kid.ID
	reg := registry(parent, kid, grand, placed(workspace("s-loose", "loose"), 2))
	reg.Repositories[0].Folded = true
	reg.Subsections = []wsm.Subsection{sub}
	return reg
}

func TestTheNearestFoldDecidesASubsectionsRows(t *testing.T) {
	tests := []struct {
		name             string
		subsectionFolded bool
		want             map[string]string
	}{
		{
			name:             "an unfolded subsection in a collapsed repository shows its whole family",
			subsectionFolded: false,
			want:             map[string]string{"s-p": "unselected_visible", "s-k": "unselected_visible", "s-g": "unselected_visible", "s-loose": "not_visible"},
		},
		{
			name:             "a folded subsection hides its rows at every depth",
			subsectionFolded: true,
			want:             map[string]string{"s-p": "not_visible", "s-k": "not_visible", "s-g": "not_visible", "s-loose": "not_visible"},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			reg := subsectionFamily(tt.subsectionFolded)
			r := arrange(t)
			r.SetRegistry(reg)

			// Act
			liveRegistry(r, reg)

			// Assert
			got := visibilities(t, r)
			for id, want := range tt.want {
				if got[id] != want {
					t.Errorf("%s visibility = %s, want %s", id, got[id], want)
				}
			}
		})
	}
}

func TestAFoldedSubsectionsWorkingGrandchildIsVisibleWithItsChain(t *testing.T) {
	// Arrange
	reg := subsectionFamily(true)
	r := arrange(t)
	r.SetRegistry(reg)
	liveRegistry(r, reg)

	// Act
	working(r, "s-g")

	// Assert
	got := visibilities(t, r)
	for _, id := range []string{"s-p", "s-k", "s-g"} {
		if got[id] != "unselected_visible" {
			t.Errorf("%s visibility = %s, want unselected_visible: a working grandchild keeps its chain drawn", id, got[id])
		}
	}
}
