package workspace

import (
	"agent-repld/internal/sourcescan"
	"context"
	"errors"
	"fmt"
	"path/filepath"
	"slices"
	"strings"
	"testing"
	"time"

	"google.golang.org/protobuf/proto"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/nameprefix"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/wsm"
)

func TestSelectRecordsTheSelectionInstant(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Select(context.Background(), "w1", Switch{History: RecordSelection}); err != nil {
		t.Fatalf("Select: %v", err)
	}

	// Assert.
	if f.db.current == nil || *f.db.current != "w1" || !f.db.currentAt.Equal(fixedNow) {
		t.Fatalf("current = (%v, %v), want w1 at %v", f.db.current, f.db.currentAt, fixedNow)
	}
}

func TestSelectClearsTheAttentionMarker(t *testing.T) {
	// Arrange: the user has now looked at whatever raised it.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.db.attention["w1"] = true

	// Act.
	if err := f.verbs.Select(context.Background(), "w1", Switch{History: RecordSelection}); err != nil {
		t.Fatalf("Select: %v", err)
	}

	// Assert.
	if f.db.attention["w1"] {
		t.Fatal("Select() left the attention marker set")
	}
}

func TestSelectTellsTheRoster(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Select(context.Background(), "w1", Switch{History: RecordSelection}); err != nil {
		t.Fatalf("Select: %v", err)
	}

	// Assert.
	if len(f.sidebar.selected) != 1 || f.sidebar.selected[0] != "w1" {
		t.Fatalf("roster selections = %v, want w1 once", f.sidebar.selected)
	}
}

func TestSelectRaisesTheCacheKeepersVisibilityEdges(t *testing.T) {
	tests := []struct {
		name     string
		previous string
		want     []ids.WorkspaceID
	}{
		{name: "the first selection reconciles the selected workspace", want: []ids.WorkspaceID{"w2"}},
		{name: "a switch reconciles the selected and the deselected workspace", previous: "w1", want: []ids.WorkspaceID{"w2", "w1"}},
		{name: "a re-selection changes no visibility", previous: "w2", want: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			f.workspace("w2", t.TempDir())
			if tt.previous != "" {
				previous := ids.WorkspaceID(tt.previous)
				f.db.current = &previous
			}

			// Act.
			if err := f.verbs.Select(context.Background(), "w2", Switch{History: RecordSelection}); err != nil {
				t.Fatalf("Select: %v", err)
			}

			// Assert.
			if !slices.Equal(f.visibilityEdges, tt.want) {
				t.Fatalf("visibility edges = %v, want %v", f.visibilityEdges, tt.want)
			}
		})
	}
}

func TestSelectIsIdempotent(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Select(context.Background(), "w1", Switch{History: RecordSelection}); err != nil {
		t.Fatalf("first Select: %v", err)
	}
	if err := f.verbs.Select(context.Background(), "w1", Switch{History: RecordSelection}); err != nil {
		t.Fatalf("second Select: %v", err)
	}

	// Assert.
	if *f.db.current != "w1" {
		t.Fatalf("current = %v, want w1", *f.db.current)
	}
}

func TestSelectRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange.
	f := newFixture(t)

	// Act.
	err := f.verbs.Select(context.Background(), "nope", Switch{History: RecordSelection})

	// Assert.
	asRefusal(t, err, ArmUnknownWorkspace)
}

func TestSetPriorityRecordsThePriority(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	priority := wsm.PriorityP05

	// Act.
	if err := f.verbs.SetPriority(context.Background(), "w1", &priority); err != nil {
		t.Fatalf("SetPriority: %v", err)
	}

	// Assert.
	got := f.db.priorities["w1"]
	if got == nil || *got != wsm.PriorityP05 {
		t.Fatalf("recorded priority = %v, want P05", got)
	}
}

func TestSetPriorityClearsThePriority(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.SetPriority(context.Background(), "w1", nil); err != nil {
		t.Fatalf("SetPriority: %v", err)
	}

	// Assert.
	got, recorded := f.db.priorities["w1"]
	if !recorded || got != nil {
		t.Fatalf("recorded priority = (%v, %v), want a recorded clear", got, recorded)
	}
}

func TestSetPriorityRepublishesTheRoster(t *testing.T) {
	// Arrange: the row draws the priority badge, so a priority change is a
	// roster push.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	priority := wsm.PriorityP2

	// Act.
	if err := f.verbs.SetPriority(context.Background(), "w1", &priority); err != nil {
		t.Fatalf("SetPriority: %v", err)
	}

	// Assert.
	if len(f.sidebar.registries) != 1 {
		t.Fatalf("roster republications = %d, want exactly one", len(f.sidebar.registries))
	}
}

func TestSetPriorityHandsTheTopbarTheNamesPrefix(t *testing.T) {
	p2 := wsm.PriorityP2
	tests := []struct {
		name     string
		priority *wsm.Priority
	}{
		{name: "set", priority: &p2},
		{name: "cleared", priority: nil},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the topbar's name wears the priority icon too.
			f := newFixture(t)
			f.workspace("w1", t.TempDir())

			// Act.
			if err := f.verbs.SetPriority(context.Background(), "w1", tc.priority); err != nil {
				t.Fatalf("SetPriority: %v", err)
			}

			// Assert: the one decision, over the record with its new priority.
			want := nameprefix.Of(wsm.Workspace{Priority: tc.priority})
			if len(f.topbarNamePrefixes) != 1 || !proto.Equal(f.topbarNamePrefixes[0], want) {
				t.Fatalf("topbar name prefixes = %v, want exactly [%v]", f.topbarNamePrefixes, want)
			}
		})
	}
}

// TestSelectPublishesTheStoresAnswerWithoutARegistryRead pins the switch's
// fast path: the roster takes the rows the store's write answered, as ONE
// push, and nothing reads the whole registry.
func TestSelectPublishesTheStoresAnswerWithoutARegistryRead(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Select(context.Background(), "w1", Switch{History: RecordSelection}); err != nil {
		t.Fatalf("Select: %v", err)
	}

	// Assert.
	if want := []string{"selection"}; !slices.Equal(f.sidebar.calls, want) {
		t.Fatalf("roster calls = %v, want %v", f.sidebar.calls, want)
	}
	if f.db.arrangementReads != 0 {
		t.Fatalf("registry reads = %d, want none", f.db.arrangementReads)
	}
	sel := f.sidebar.selections[0]
	if sel.Current != "w1" || len(sel.Workspaces) == 0 || sel.Workspaces[0].LastSelectedAt == nil || !sel.Workspaces[0].LastSelectedAt.Equal(fixedNow) {
		t.Fatalf("the roster took %+v, want w1's row stamped at %v", sel, fixedNow)
	}
}

// TestSelectStampsTheRosterSelection pins that the select's own id and the
// switch's instant reach the roster with the selection, for the sidebar's
// paint record and its switch latency.
func TestSelectStampsTheRosterSelection(t *testing.T) {
	started := fixedNow.Add(-40 * time.Millisecond)
	tests := []struct {
		name    string
		started *time.Time
	}{
		{name: "a switch with an instant carries it", started: &started},
		{name: "a switch with none carries none"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			f.verbs.(*verbs).mintSelection = func() string { return "sel-1" }

			// Act.
			if err := f.verbs.Select(context.Background(), "w1", Switch{History: RecordSelection, StartedAt: tt.started}); err != nil {
				t.Fatalf("Select: %v", err)
			}

			// Assert.
			want := sidebar.SelectStamp{ID: "sel-1", SwitchStartedAt: tt.started}
			if len(f.sidebar.stamps) != 1 || f.sidebar.stamps[0] != want {
				t.Fatalf("roster stamps = %+v, want %+v", f.sidebar.stamps, want)
			}
		})
	}
}

// TestSelectWithARosterRefusingTheChangeInstallsAWholeRegistry pins the arm
// for a roster that cannot take the store's answer: the whole registry is
// read after the write and installed with the selection and its stamp.
func TestSelectWithARosterRefusingTheChangeInstallsAWholeRegistry(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.sidebar.refuseSelection = true
	f.verbs.(*verbs).mintSelection = func() string { return "sel-1" }

	// Act.
	if err := f.verbs.Select(context.Background(), "w1", Switch{History: RecordSelection}); err != nil {
		t.Fatalf("Select: %v", err)
	}

	// Assert.
	if want := []string{"selection-refused", "registry+selected"}; !slices.Equal(f.sidebar.calls, want) {
		t.Fatalf("roster calls = %v, want %v", f.sidebar.calls, want)
	}
	if got := f.sidebar.registries[0].Current; got == nil || *got != "w1" {
		t.Fatalf("the installed registry's current = %v, want w1", got)
	}
	if got := f.sidebar.stamps[0].ID; got != "sel-1" {
		t.Fatalf("the installed stamp = %q, want sel-1", got)
	}
}

// TestSelectWithAnUnreadableRegistryStillTellsTheRosterTheSelection pins the
// failure arm: when the roster refuses the change and the registry read that
// follows fails, the read is recorded at ERROR and the selection still
// reaches the roster on its own push.
func TestSelectWithAnUnreadableRegistryStillTellsTheRosterTheSelection(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.sidebar.refuseSelection = true
	f.db.listWorkspacesErr = errFake

	// Act.
	_ = f.verbs.Select(context.Background(), "w1", Switch{History: RecordSelection})

	// Assert.
	if want := []string{"selection-refused", "selected"}; !slices.Equal(f.sidebar.calls, want) {
		t.Fatalf("roster calls = %v, want %v", f.sidebar.calls, want)
	}
	if got := recordLevel(f, "could not read the workspaces, repositories and subsections for the roster"); got != "error" {
		t.Fatalf("the failed registry read was recorded at %q, want error", got)
	}
}

// TestSelectThatCannotRecordFailsLoudly pins the store failure: the select
// answers the error, records it at ERROR with its select's id, and tells the
// roster nothing.
func TestSelectThatCannotRecordFailsLoudly(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.db.selectErr = errFake
	f.verbs.(*verbs).mintSelection = func() string { return "sel-1" }

	// Act.
	err := f.verbs.Select(context.Background(), "w1", Switch{History: RecordSelection})

	// Assert.
	if !errors.Is(err, errFake) {
		t.Fatalf("Select error = %v, want it to wrap %v", err, errFake)
	}
	rec, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Message: "could not record the selection"})
	if !ok || rec.Level != "error" || rec.Operation != opSelect {
		t.Fatalf("the failed write's record = %+v (found %v), want an error under %s", rec, ok, opSelect)
	}
	if rec.Context["selection_id"] != "sel-1" || rec.Context["cause"] == nil || rec.Context["history"] != "record" {
		t.Fatalf("the failed write's context = %v, want its select's id, history and cause", rec.Context)
	}
	if len(f.sidebar.calls) != 0 {
		t.Fatalf("roster calls = %v, want none", f.sidebar.calls)
	}
}

// TestSelectLogsItsPublishAtInfo pins the record a switch's push leaves,
// keyed by the select's id, so a log reader joins it to the sidebar's paint.
func TestSelectLogsItsPublishAtInfo(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.verbs.(*verbs).mintSelection = func() string { return "sel-1" }

	// Act.
	if err := f.verbs.Select(context.Background(), "w1", Switch{History: RecordSelection}); err != nil {
		t.Fatalf("Select: %v", err)
	}

	// Assert.
	for _, message := range []string{"selected the workspace", "published the selection to the roster from the store's answer"} {
		rec, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Message: message})
		if !ok || rec.Level != "info" || rec.Context["selection_id"] != "sel-1" {
			t.Fatalf("%q = %+v (found %v), want an info record naming sel-1", message, rec, ok)
		}
	}
}

// TestARegistryReadHoldsTheSelectionLockOnlyForASelect pins which whole
// registry reads hold the selection lock. A select's own read (a followed
// selection, or a select whose change the roster refused) is part of the
// select and holds it. A republish's and the opening roster's read hold it
// NOT, so a select is never left waiting behind a whole-registry read
// (2026-11-09: 15-230ms with 86 tabs); installRegistry keeps a read a select
// overtook from being installed instead.
func TestARegistryReadHoldsTheSelectionLockOnlyForASelect(t *testing.T) {
	stored := ids.WorkspaceID("w2")
	tests := []struct {
		name string
		// arrange readies the fixture for the install.
		arrange  func(f *fixture)
		install  func(ctx context.Context, v *verbs) error
		wantHeld bool
	}{
		{
			name:    "a verb's republish",
			install: func(ctx context.Context, v *verbs) error { return v.SetPriority(ctx, "w1", nil) },
		},
		{
			name:    "the published registry",
			install: func(ctx context.Context, v *verbs) error { return v.PublishRegistry(ctx) },
		},
		{
			name: "a followed selection",
			arrange: func(f *fixture) {
				f.sidebar.selected = []ids.WorkspaceID{"w1"}
				f.db.current = &stored
			},
			install:  func(ctx context.Context, v *verbs) error { return v.FollowStoredSelection(ctx) },
			wantHeld: true,
		},
		{
			name:    "a select whose change the roster refused",
			arrange: func(f *fixture) { f.sidebar.refuseSelection = true },
			install: func(ctx context.Context, v *verbs) error {
				return v.Select(ctx, "w1", Switch{History: RecordSelection})
			},
			wantHeld: true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			f.workspace("w2", t.TempDir())
			if tt.arrange != nil {
				tt.arrange(f)
			}
			v := f.verbs.(*verbs)
			var held []bool
			f.db.onReadArrangement = func() {
				free := v.selection.TryLock()
				if free {
					v.selection.Unlock()
				}
				held = append(held, !free)
			}

			// Act.
			if err := tt.install(context.Background(), v); err != nil {
				t.Fatalf("install: %v", err)
			}

			// Assert.
			if len(held) == 0 || slices.Contains(held, !tt.wantHeld) {
				t.Fatalf("selection lock held across each registry read = %v, want %v at every read", held, tt.wantHeld)
			}
		})
	}
}

// TestReselectingDoesNotRestampTheSelectionInstant pins the idempotence the
// contract states: re-selecting the workspace already being looked at is a
// success that CHANGES NO VIEW. The instant answers "when did the user last
// switch to this", and the user did not switch.
func TestReselectingDoesNotRestampTheSelectionInstant(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	if err := f.verbs.Select(context.Background(), "w1", Switch{History: RecordSelection}); err != nil {
		t.Fatalf("first Select: %v", err)
	}
	f.db.currentAt = fixedNow.Add(-time.Hour)

	// Act.
	if err := f.verbs.Select(context.Background(), "w1", Switch{History: RecordSelection}); err != nil {
		t.Fatalf("second Select: %v", err)
	}

	// Assert.
	if !f.db.currentAt.Equal(fixedNow.Add(-time.Hour)) {
		t.Fatalf("the selection instant = %v, want the re-selection to leave it alone", f.db.currentAt)
	}
}

// TestReselectingStillClearsTheAttentionMarker pins that idempotence does not
// swallow the clear: a notification can raise the marker while the workspace
// IS current, and looking at it again is what clears it.
func TestReselectingStillClearsTheAttentionMarker(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	if err := f.verbs.Select(context.Background(), "w1", Switch{History: RecordSelection}); err != nil {
		t.Fatalf("first Select: %v", err)
	}
	f.db.attention["w1"] = true

	// Act.
	if err := f.verbs.Select(context.Background(), "w1", Switch{History: RecordSelection}); err != nil {
		t.Fatalf("second Select: %v", err)
	}

	// Assert.
	if f.db.attention["w1"] {
		t.Fatal("re-selecting left the attention marker set")
	}
}

// recordLevel returns the level of the first captured record whose message
// equals want, or "" when none matches.
func recordLevel(f *fixture, want string) string {
	r, _ := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Message: want})
	return r.Level
}

func TestSelectLogsTheSwitchReceiptAtInfo(t *testing.T) {
	// Arrange: a switch to a workspace that is not already current.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Select(context.Background(), "w1", Switch{History: RecordSelection}); err != nil {
		t.Fatalf("Select: %v", err)
	}

	// Assert: a switch must leave an info-level trace in production logs.
	if got := recordLevel(f, "selected the workspace"); got != "info" {
		t.Fatalf("the select receipt is %q, want info", got)
	}
}

func TestSelectLogsTheReselectionReceiptAtInfo(t *testing.T) {
	// Arrange: the workspace is already current, so the next select re-selects it.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	if err := f.verbs.Select(context.Background(), "w1", Switch{History: RecordSelection}); err != nil {
		t.Fatalf("first Select: %v", err)
	}

	// Act.
	if err := f.verbs.Select(context.Background(), "w1", Switch{History: RecordSelection}); err != nil {
		t.Fatalf("second Select: %v", err)
	}

	// Assert.
	if got := recordLevel(f, "the workspace was already current; the selection instant stands"); got != "info" {
		t.Fatalf("the reselection receipt is %q, want info", got)
	}
}

// ---- FollowStoredSelection -------------------------------------------------

// followFixture arranges a roster naming w1 while WSM stores STORED, the state
// a peer daemon's select leaves this daemon in.
func followFixture(t *testing.T, stored *ids.WorkspaceID) *fixture {
	t.Helper()
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.workspace("w2", t.TempDir())
	f.sidebar.selected = []ids.WorkspaceID{"w1"}
	f.db.current = stored
	return f
}

func storedSelection(ws ids.WorkspaceID) *ids.WorkspaceID { return &ws }

func TestFollowStoredSelectionPushesTheStoredSelection(t *testing.T) {
	// Arrange: the peer daemon stamped w2.
	f := followFixture(t, storedSelection("w2"))

	// Act.
	if err := f.verbs.FollowStoredSelection(context.Background()); err != nil {
		t.Fatalf("FollowStoredSelection: %v", err)
	}

	// Assert: one push, carrying the registry and the stored selection.
	if want := []string{"registry+selected"}; !slices.Equal(f.sidebar.calls, want) {
		t.Fatalf("roster calls = %v, want %v", f.sidebar.calls, want)
	}
	if got, _ := f.sidebar.Selected(); got != "w2" {
		t.Fatalf("roster selection = %q, want w2", got)
	}
}

func TestFollowStoredSelectionPushesNothingWhenTheRosterAgrees(t *testing.T) {
	// Arrange: the roster already names what WSM stores.
	f := followFixture(t, storedSelection("w1"))

	// Act.
	if err := f.verbs.FollowStoredSelection(context.Background()); err != nil {
		t.Fatalf("FollowStoredSelection: %v", err)
	}

	// Assert.
	if len(f.sidebar.calls) != 0 {
		t.Fatalf("roster calls = %v, want none", f.sidebar.calls)
	}
}

func TestFollowStoredSelectionPushesNothingWhenWSMStoresNone(t *testing.T) {
	// Arrange.
	f := followFixture(t, nil)

	// Act.
	if err := f.verbs.FollowStoredSelection(context.Background()); err != nil {
		t.Fatalf("FollowStoredSelection: %v", err)
	}

	// Assert.
	if len(f.sidebar.calls) != 0 {
		t.Fatalf("roster calls = %v, want none", f.sidebar.calls)
	}
}

func TestFollowStoredSelectionFailsWhenTheStoredSelectionCannotBeRead(t *testing.T) {
	// Arrange.
	f := followFixture(t, storedSelection("w2"))
	f.db.currentErr = errFake

	// Act.
	err := f.verbs.FollowStoredSelection(context.Background())

	// Assert.
	if !errors.Is(err, errFake) {
		t.Fatalf("FollowStoredSelection error = %v, want it to wrap %v", err, errFake)
	}
	if got := recordLevel(f, "could not read the stored selection"); got != "error" {
		t.Fatalf("the failed read was recorded at %q, want error", got)
	}
}

func TestFollowStoredSelectionFailsWithoutAPushWhenTheRegistryCannotBeRead(t *testing.T) {
	// Arrange.
	f := followFixture(t, storedSelection("w2"))
	f.db.listWorkspacesErr = errFake

	// Act.
	err := f.verbs.FollowStoredSelection(context.Background())

	// Assert: the failure is answered and nothing half-built reaches the roster.
	if err == nil {
		t.Fatal("FollowStoredSelection answered nil for an unreadable registry")
	}
	if len(f.sidebar.calls) != 0 {
		t.Fatalf("roster calls = %v, want none", f.sidebar.calls)
	}
}

func TestFollowStoredSelectionLogsTheFollowAtInfo(t *testing.T) {
	// Arrange.
	f := followFixture(t, storedSelection("w2"))

	// Act.
	if err := f.verbs.FollowStoredSelection(context.Background()); err != nil {
		t.Fatalf("FollowStoredSelection: %v", err)
	}

	// Assert: a roster that moved on another daemon's word leaves a trace.
	if got := recordLevel(f, "the roster took the selection another daemon stored"); got != "info" {
		t.Fatalf("the follow receipt is %q, want info", got)
	}
}

// ---- RecordInteraction -----------------------------------------------------

func TestRecordInteractionStampsTheInstantThenTellsTheRoster(t *testing.T) {
	kinds := []Interaction{InteractionFeedScrolled, InteractionComposerTyped, InteractionFeedNavigated, InteractionLinkFollowed}
	for _, kind := range kinds {
		t.Run(string(kind), func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			f.workspace("w1", t.TempDir())

			// Act.
			if err := f.verbs.RecordInteraction(context.Background(), "w1", kind); err != nil {
				t.Fatalf("RecordInteraction: %v", err)
			}

			// Assert: every kind stamps the daemon clock's instant durably and
			// tells the roster the same instant.
			want := []interaction{{ws: "w1", at: fixedNow}}
			if !slices.Equal(f.db.interactedAt, want) {
				t.Fatalf("durable interaction instants = %v, want %v", f.db.interactedAt, want)
			}
			if !slices.Equal(f.sidebar.interacted, want) {
				t.Fatalf("roster interactions = %v, want %v", f.sidebar.interacted, want)
			}
		})
	}
}

func TestRecordInteractionIsRecordedAtInfoWithItsKind(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.RecordInteraction(context.Background(), "w1", InteractionComposerTyped); err != nil {
		t.Fatalf("RecordInteraction: %v", err)
	}

	// Assert.
	rec, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "info", Operation: opInteraction})
	if !ok {
		t.Fatalf("no info %s record", opInteraction)
	}
	if got := rec.Context["interaction"]; got != string(InteractionComposerTyped) {
		t.Fatalf("interaction = %v, want %s", got, InteractionComposerTyped)
	}
}

func TestRecordInteractionSelectsNothing(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.RecordInteraction(context.Background(), "w1", InteractionFeedScrolled); err != nil {
		t.Fatalf("RecordInteraction: %v", err)
	}

	// Assert: interacting with a workspace's page is not switching to it.
	if f.db.current != nil {
		t.Fatalf("current = %v, want RecordInteraction to have recorded no selection", f.db.current)
	}
}

func TestRecordInteractionRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange.
	f := newFixture(t)

	// Act.
	err := f.verbs.RecordInteraction(context.Background(), "nope", InteractionFeedScrolled)

	// Assert: an unregistered workspace has no row to view.
	if err == nil {
		t.Fatal("RecordInteraction() accepted a workspace the registry does not hold")
	}
	if len(f.db.interactedAt) != 0 || len(f.sidebar.interacted) != 0 {
		t.Fatalf("durable %v, roster %v, want nothing recorded for a refused interaction", f.db.interactedAt, f.sidebar.interacted)
	}
}

// TestRecordInteractionThatCannotRecordFailsLoudly pins the store failure: the
// interaction answers the error, records it at ERROR with its kind, and tells
// the roster nothing, so the row is never drawn viewed with no durable instant.
func TestRecordInteractionThatCannotRecordFailsLoudly(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.db.interactedErr = errFake

	// Act.
	err := f.verbs.RecordInteraction(context.Background(), "w1", InteractionComposerTyped)

	// Assert.
	if !errors.Is(err, errFake) {
		t.Fatalf("RecordInteraction error = %v, want it to wrap %v", err, errFake)
	}
	rec, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Message: "could not record the user's interaction instant"})
	if !ok || rec.Level != "error" || rec.Operation != opInteraction {
		t.Fatalf("the failed write's record = %+v (found %v), want an error under %s", rec, ok, opInteraction)
	}
	if rec.Context["interaction"] != "composer_typed" || rec.Context["cause"] == nil {
		t.Fatalf("the failed write's context = %v, want its kind and cause", rec.Context)
	}
	if len(f.sidebar.interacted) != 0 {
		t.Fatalf("roster interactions = %v, want none", f.sidebar.interacted)
	}
}

func TestSelectReturnsTheFeedToItsTailOnASwitch(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.workspace("w2", t.TempDir())
	if err := f.verbs.Select(context.Background(), "w1", Switch{History: RecordSelection}); err != nil {
		t.Fatalf("Select w1: %v", err)
	}

	// Act.
	if err := f.verbs.Select(context.Background(), "w2", Switch{History: RecordSelection}); err != nil {
		t.Fatalf("Select w2: %v", err)
	}

	// Assert.
	if !slices.Equal(f.host.tailReturns, []ids.WorkspaceID{"w1", "w2"}) {
		t.Fatalf("tail returns = %v, want [w1 w2]", f.host.tailReturns)
	}
}

func TestReselectingLeavesTheFeedWhereItIs(t *testing.T) {
	// Arrange: a re-selection is Emacs re-asserting, never a switch.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	if err := f.verbs.Select(context.Background(), "w1", Switch{History: RecordSelection}); err != nil {
		t.Fatalf("first Select: %v", err)
	}

	// Act.
	if err := f.verbs.Select(context.Background(), "w1", Switch{History: RecordSelection}); err != nil {
		t.Fatalf("second Select: %v", err)
	}

	// Assert.
	if !slices.Equal(f.host.tailReturns, []ids.WorkspaceID{"w1"}) {
		t.Fatalf("tail returns = %v, want [w1] (the first select only)", f.host.tailReturns)
	}
}

func TestSelectOnADaemonStandingDownLeavesTheSelectionStanding(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.hibernate("w1")
	f.fleet.standingDown = true

	// Act.
	_ = f.verbs.Select(context.Background(), "w1", Switch{History: RecordSelection})

	// Assert: the next daemon serves this selection.
	if f.db.current == nil || *f.db.current != "w1" {
		t.Fatalf("current = %v, want w1", f.db.current)
	}
}

func TestSetBackloggedRecordsTheFlagAndRepublishesTheRoster(t *testing.T) {
	tests := []struct {
		name       string
		backlogged bool
	}{{"hide", true}, {"show", false}}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			prior := f.db.workspaces["w1"]
			prior.Backlogged = !tt.backlogged
			f.db.workspaces["w1"] = prior

			// Act
			err := f.verbs.SetBacklogged(context.Background(), "w1", tt.backlogged)

			// Assert
			if err != nil {
				t.Fatalf("SetBacklogged: %v", err)
			}
			if got := f.db.workspaces["w1"].Backlogged; got != tt.backlogged {
				t.Fatalf("recorded backlogged = %v, want %v", got, tt.backlogged)
			}
			if len(f.sidebar.registries) != 1 {
				t.Fatalf("roster republications = %d, want exactly one", len(f.sidebar.registries))
			}
		})
	}
}

func TestSetBackloggedRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange
	f := newFixture(t)

	// Act
	err := f.verbs.SetBacklogged(context.Background(), "nope", true)

	// Assert
	asRefusal(t, err, ArmUnknownWorkspace)
	if len(f.sidebar.registries) != 0 {
		t.Fatalf("roster republications = %d, want none for a refusal", len(f.sidebar.registries))
	}
}

func TestSetBackloggedSurfacesAFailedWriteAtError(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.db.setBackloggedErr = errors.New("disk I/O error")

	// Act
	err := f.verbs.SetBacklogged(context.Background(), "w1", true)

	// Assert
	if err == nil {
		t.Fatal("a failed backlogged write was answered as success")
	}
	if !loggedAtError(f, opSetBacklogged) {
		t.Fatal("the failed write was not recorded at error")
	}
	if len(f.sidebar.registries) != 0 {
		t.Fatalf("roster republications = %d, want none for a failed write", len(f.sidebar.registries))
	}
	if len(f.visibilityEdges) != 0 {
		t.Fatalf("visibility edges = %v, want none for a failed write", f.visibilityEdges)
	}
}

func TestRecordBackloggedWritesRepublishesAndRaisesTheEdge(t *testing.T) {
	tests := []struct {
		name       string
		backlogged bool
	}{
		{name: "hiding", backlogged: true},
		{name: "showing", backlogged: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			prior := f.db.workspaces["w1"]
			prior.Backlogged = !tt.backlogged
			f.db.workspaces["w1"] = prior

			// Act
			err := f.verbs.(*verbs).recordBacklogged(context.Background(), f.log.Global(), "w1", tt.backlogged)

			// Assert
			if err != nil {
				t.Fatalf("recordBacklogged: %v", err)
			}
			if got := f.db.workspaces["w1"].Backlogged; got != tt.backlogged {
				t.Fatalf("recorded backlogged = %v, want %v", got, tt.backlogged)
			}
			if len(f.sidebar.registries) != 1 {
				t.Fatalf("roster republications = %d, want exactly one", len(f.sidebar.registries))
			}
			if !slices.Equal(f.visibilityEdges, []ids.WorkspaceID{"w1"}) {
				t.Fatalf("visibility edges = %v, want the workspace", f.visibilityEdges)
			}
		})
	}
}

func TestRecordBackloggedSurfacesAFailedWriteAtError(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.db.setBackloggedErr = errors.New("disk I/O error")

	// Act
	err := f.verbs.(*verbs).recordBacklogged(context.Background(), f.log.Global(), "w1", false)

	// Assert
	if err == nil {
		t.Fatal("a failed backlogged write was answered as success")
	}
	if !loggedAtError(f, opSetBacklogged) {
		t.Fatal("the failed write was not recorded at error")
	}
	if len(f.sidebar.registries) != 0 || len(f.visibilityEdges) != 0 {
		t.Fatalf("republications %d, edges %v; want none for a failed write", len(f.sidebar.registries), f.visibilityEdges)
	}
}

// TestTheBackloggedFlagHasOneWrite fails when a verb writes the backlogged flag by
// hand instead of through recordBacklogged, which would skip the roster push or
// the cache keeper's edge.
func TestTheBackloggedFlagHasOneWrite(t *testing.T) {
	// Act
	sites := sourcescan.Sites(t, "v.deps.DB.SetBacklogged(")

	// Assert
	if len(sites) != 1 || sites[0] != "select.go" {
		t.Fatalf("the backlogged flag is written at %v, want only recordBacklogged in select.go", sites)
	}
}

// hiddenMergeWorkspace registers WS as a merge workspace its async merge still
// withholds, with the backlogged flag as BACKLOGGED, and makes the fixture's merge
// queue reveal through the verbs' own durable reveal, as the orchestrator does.
func hiddenMergeWorkspace(t *testing.T, f *fixture, ws ids.WorkspaceID, hidden bool) {
	t.Helper()
	record := f.workspace(ws, t.TempDir())
	record.MergeOf = &wsm.MergeOrigin{Requester: "ws-requester", Request: "r-1"}
	record.Backlogged = hidden
	f.db.with(record)
	f.merge.reveal = func(ctx context.Context, id ids.WorkspaceID) error {
		_, err := f.verbs.RevealMergeWorkspace(ctx, id)
		return err
	}
}

// drawnOpen reports whether the last roster the sidebar was handed lists WS in
// its Open section.
func drawnOpen(t *testing.T, f *fixture, ws ids.WorkspaceID) bool {
	t.Helper()
	if len(f.sidebar.registries) == 0 {
		t.Fatal("no roster was published")
	}
	for _, record := range f.sidebar.registries[len(f.sidebar.registries)-1].Workspaces {
		if record.ID == ws {
			return record.ListedOpen()
		}
	}
	t.Fatalf("the published roster does not hold %s", ws)
	return false
}

func TestShowingAHiddenMergeWorkspaceRevealsItAndTheRosterDrawsIt(t *testing.T) {
	tests := []struct {
		name string
		// backlogged is the backlogged flag beside the unrevealed standing.
		backlogged bool
	}{
		{name: "withheld by its merge only", backlogged: false},
		{name: "withheld by its merge and backlogged by the flag", backlogged: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			hiddenMergeWorkspace(t, f, "ws-merge", tt.backlogged)

			// Act
			err := f.verbs.SetBacklogged(context.Background(), "ws-merge", false)

			// Assert
			if err != nil {
				t.Fatalf("SetBacklogged: %v", err)
			}
			if !slices.Equal(f.merge.reveals, []ids.WorkspaceID{"ws-merge"}) || f.merge.revealCauses[0] != "it was shown" {
				t.Fatalf("reveals = %v causes = %v, want the one reveal a show asks for", f.merge.reveals, f.merge.revealCauses)
			}
			if !drawnOpen(t, f, "ws-merge") {
				t.Fatalf("the roster does not draw the shown merge workspace: %+v", f.db.workspaces["ws-merge"])
			}
		})
	}
}

func TestShowingAnOrdinaryWorkspaceRevealsNothing(t *testing.T) {
	tests := []struct {
		name   string
		record func(wsm.Workspace) wsm.Workspace
	}{
		{name: "an ordinary backlogged workspace", record: func(ws wsm.Workspace) wsm.Workspace { return ws }},
		{name: "a revealed merge workspace the user hid", record: func(ws wsm.Workspace) wsm.Workspace {
			ws.MergeOf = &wsm.MergeOrigin{Requester: "ws-requester", Request: "r-1"}
			ws.MergeRevealed = true
			return ws
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			record := tt.record(f.workspace("w1", t.TempDir()))
			record.Backlogged = true
			f.db.with(record)

			// Act
			err := f.verbs.SetBacklogged(context.Background(), "w1", false)

			// Assert
			if err != nil {
				t.Fatalf("SetBacklogged: %v", err)
			}
			if len(f.merge.reveals) != 0 {
				t.Fatalf("reveals = %v, want none", f.merge.reveals)
			}
			if len(f.sidebar.registries) != 1 || !drawnOpen(t, f, "w1") {
				t.Fatalf("roster republications = %d, want the one that draws the shown workspace", len(f.sidebar.registries))
			}
		})
	}
}

func TestHidingARevealedMergeWorkspaceIsAnOrdinaryHide(t *testing.T) {
	// Arrange
	f := newFixture(t)
	record := f.workspace("ws-merge", t.TempDir())
	record.MergeOf = &wsm.MergeOrigin{Requester: "ws-requester", Request: "r-1"}
	record.MergeRevealed = true
	f.db.with(record)

	// Act
	err := f.verbs.SetBacklogged(context.Background(), "ws-merge", true)

	// Assert
	if err != nil {
		t.Fatalf("SetBacklogged: %v", err)
	}
	got := f.db.workspaces["ws-merge"]
	if !got.InBacklog() || got.MergeHidden() || !got.MergeRevealed {
		t.Fatalf("record = %+v, want it in the backlog and still revealed", got)
	}
}

func TestAShowWhoseRevealFailsIsAnErrorAndWritesNothing(t *testing.T) {
	// Arrange
	f := newFixture(t)
	hiddenMergeWorkspace(t, f, "ws-merge", true)
	f.db.revealErr = errors.New("database is locked")

	// Act
	err := f.verbs.SetBacklogged(context.Background(), "ws-merge", false)

	// Assert
	if !errors.Is(err, f.db.revealErr) {
		t.Fatalf("SetBacklogged = %v, want the reveal's failure", err)
	}
	if !loggedAtError(f, opRevealMergeWorkspace) {
		t.Fatal("the failed reveal was not recorded at error")
	}
	got := f.db.workspaces["ws-merge"]
	if !got.Backlogged || !got.MergeHidden() {
		t.Fatalf("record = %+v, want the backlogged flag and the merge's withholding both untouched", got)
	}
	if len(f.sidebar.registries) != 0 || len(f.visibilityEdges) != 0 {
		t.Fatalf("roster republications = %d, visibility edges = %v; want none for a failed show", len(f.sidebar.registries), f.visibilityEdges)
	}
}

func TestAShowIsRefusedByAnyErrorItsRevealAnswers(t *testing.T) {
	// Arrange
	f := newFixture(t)
	hiddenMergeWorkspace(t, f, "ws-merge", true)
	refused := errors.New("the reveal left it hidden")
	f.merge.reveal = func(context.Context, ids.WorkspaceID) error { return refused }

	// Act
	err := f.verbs.SetBacklogged(context.Background(), "ws-merge", false)

	// Assert
	if !errors.Is(err, refused) {
		t.Fatalf("SetBacklogged = %v, want the reveal's error", err)
	}
	if !f.db.workspaces["ws-merge"].Backlogged || len(f.sidebar.registries) != 0 {
		t.Fatalf("backlogged = %v, roster republications = %d; want the flag untouched and no roster",
			f.db.workspaces["ws-merge"].Backlogged, len(f.sidebar.registries))
	}
}

// A show and a select are the two callers that reveal a merge workspace for
// the user, and both go through the orchestrator's one reveal: the verbs ask
// it only from revealMergeHidden, and never write the reveal around it.
func TestAShowRevealsThroughTheOrchestratorsOneReveal(t *testing.T) {
	// Act
	asked := sourcescan.Sites(t, "v.deps.Merge.RevealMergeWorkspace(")
	written := sourcescan.Sites(t, "v.deps.DB.RevealMergeWorkspace(")

	// Assert
	if len(asked) != 1 || asked[0] != "select.go" || len(written) != 1 || written[0] != "select.go" {
		t.Fatalf("the reveal is asked for at %v and written at %v, want each once in select.go", asked, written)
	}
}

func TestSwapSwapsTheWorkspacesAndRepublishes(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.workspace("w2", t.TempDir())

	// Act.
	if err := f.verbs.Swap(context.Background(), "w1", "w2"); err != nil {
		t.Fatalf("Swap: %v", err)
	}

	// Assert.
	if want := [][2]ids.WorkspaceID{{"w1", "w2"}}; !slices.Equal(f.db.swaps, want) {
		t.Fatalf("swaps = %v, want %v", f.db.swaps, want)
	}
	if len(f.sidebar.registries) != 1 {
		t.Fatalf("roster republications = %d, want exactly one", len(f.sidebar.registries))
	}
	if !hasRecord(f, dlog.LevelInfo, opSwap) {
		t.Fatal("the swap was not recorded at info")
	}
}

func TestSwapRefusesAWorkspaceThisDaemonDoesNotServe(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.workspace("w2", t.TempDir())
	f.owner.standing = StandingTransferringAway

	// Act.
	err := f.verbs.Swap(context.Background(), "w1", "w2")

	// Assert.
	asRefusal(t, err, ArmTransferringAway)
	if len(f.db.swaps) != 0 {
		t.Fatalf("a refused swap wrote %v", f.db.swaps)
	}
}

func TestSwapRefusesTwoWorkspacesThatAreNotSiblings(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.workspace("w2", t.TempDir())
	f.db.swapErr = wsm.ErrNotSiblings

	// Act.
	err := f.verbs.Swap(context.Background(), "w1", "w2")

	// Assert.
	asRefusal(t, err, ArmNotSiblings)
	if len(f.sidebar.registries) != 0 {
		t.Fatalf("a refused swap republished %d times", len(f.sidebar.registries))
	}
	if loggedAtError(f, opSwap) {
		t.Fatal("a refused swap was logged at error")
	}
}

func TestSwapRefusesTwoRepositories(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.workspace("w2", t.TempDir())
	f.db.swapErr = wsm.ErrOtherRepository

	// Act.
	err := f.verbs.Swap(context.Background(), "w1", "w2")

	// Assert.
	asRefusal(t, err, ArmOtherRepository)
	if len(f.sidebar.registries) != 0 {
		t.Fatalf("a refused swap republished %d times", len(f.sidebar.registries))
	}
	if loggedAtError(f, opSwap) {
		t.Fatal("a refused swap was logged at error")
	}
}

func TestSwapSurfacesAStoreFailure(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.workspace("w2", t.TempDir())
	f.db.swapErr = errors.New("disk on fire")

	// Act.
	err := f.verbs.Swap(context.Background(), "w1", "w2")

	// Assert.
	if err == nil || !errors.Is(err, f.db.swapErr) {
		t.Fatalf("error = %v, want the store's failure", err)
	}
	if len(f.sidebar.registries) != 0 {
		t.Fatalf("a failed swap republished %d times", len(f.sidebar.registries))
	}
	if !loggedAtError(f, opSwap) {
		t.Fatal("the store failure was not logged at error")
	}
}

func TestOwnedPairNamesTheTargetOnTheWorkspacesLogger(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.workspace("w2", t.TempDir())

	// Act.
	log, err := f.verbs.(*verbs).ownedPair(context.Background(), "MoveWorkspace", "w1", "w2")
	log.Info(opMove, "probe", nil)

	// Assert.
	if err != nil {
		t.Fatalf("ownedPair: %v", err)
	}
	rec, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Operation: opMove})
	if !ok || rec.Context["target"] != "w2" {
		t.Fatalf("probe record = %+v (found %v), want it to name target w2", rec, ok)
	}
}

func TestOwnedPairRefusesATargetThisDaemonDoesNotKnow(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	_, err := f.verbs.(*verbs).ownedPair(context.Background(), "MoveWorkspace", "w1", "absent")

	// Assert.
	asRefusal(t, err, ArmUnknownWorkspace)
}

func TestMoveMovesTheWorkspaceAndRepublishes(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.workspace("w2", t.TempDir())

	// Act.
	if err := f.verbs.Move(context.Background(), "w1", "w2"); err != nil {
		t.Fatalf("Move: %v", err)
	}

	// Assert.
	if want := [][2]ids.WorkspaceID{{"w1", "w2"}}; !slices.Equal(f.db.moves, want) {
		t.Fatalf("moves = %v, want %v", f.db.moves, want)
	}
	if len(f.sidebar.registries) != 1 {
		t.Fatalf("roster republications = %d, want exactly one", len(f.sidebar.registries))
	}
	if !hasRecord(f, dlog.LevelInfo, opMove) {
		t.Fatal("the move was not recorded at info")
	}
}

func TestMoveRefusesAWorkspaceThisDaemonDoesNotServe(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.workspace("w2", t.TempDir())
	f.owner.standing = StandingTransferringAway

	// Act.
	err := f.verbs.Move(context.Background(), "w1", "w2")

	// Assert.
	asRefusal(t, err, ArmTransferringAway)
	if len(f.db.moves) != 0 {
		t.Fatalf("a refused move wrote %v", f.db.moves)
	}
}

func TestMoveTurnsTheStoresRefusalsIntoArms(t *testing.T) {
	tests := []struct {
		name  string
		store error
		arm   string
	}{
		{name: "two repositories", store: wsm.ErrOtherRepository, arm: ArmOtherRepository},
		{name: "not siblings", store: wsm.ErrNotSiblings, arm: ArmNotSiblings},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			f.workspace("w2", t.TempDir())
			f.db.moveErr = tt.store

			// Act.
			err := f.verbs.Move(context.Background(), "w1", "w2")

			// Assert.
			asRefusal(t, err, tt.arm)
			if len(f.sidebar.registries) != 0 {
				t.Fatalf("a refused move republished %d times", len(f.sidebar.registries))
			}
			if loggedAtError(f, opMove) {
				t.Fatal("a refused move was logged at error")
			}
		})
	}
}

func TestMoveSurfacesAStoreFailure(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.workspace("w2", t.TempDir())
	f.db.moveErr = errors.New("disk on fire")

	// Act.
	err := f.verbs.Move(context.Background(), "w1", "w2")

	// Assert.
	if err == nil || !errors.Is(err, f.db.moveErr) {
		t.Fatalf("error = %v, want the store's failure", err)
	}
	if len(f.sidebar.registries) != 0 {
		t.Fatalf("a failed move republished %d times", len(f.sidebar.registries))
	}
	if !loggedAtError(f, opMove) {
		t.Fatal("the store failure was not logged at error")
	}
}

func TestSetBackloggedTellsTheCacheKeeper(t *testing.T) {
	tests := []struct {
		name       string
		backlogged bool
	}{
		{name: "a hide", backlogged: true},
		{name: "a show", backlogged: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.workspace("w1", t.TempDir())

			// Act
			if err := f.verbs.SetBacklogged(context.Background(), "w1", tt.backlogged); err != nil {
				t.Fatalf("SetBacklogged: %v", err)
			}

			// Assert
			if !slices.Equal(f.visibilityEdges, []ids.WorkspaceID{"w1"}) {
				t.Fatalf("visibility edges = %v, want the workspace", f.visibilityEdges)
			}
		})
	}
}

func TestSetBackloggedTellsTheMergeQueueOfAHideOnly(t *testing.T) {
	tests := []struct {
		name       string
		backlogged bool
		want       []ids.WorkspaceID
	}{
		{name: "a hide is told", backlogged: true, want: []ids.WorkspaceID{"w1"}},
		{name: "a show is not", backlogged: false, want: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.workspace("w1", t.TempDir())

			// Act
			if err := f.verbs.SetBacklogged(context.Background(), "w1", tt.backlogged); err != nil {
				t.Fatalf("SetBacklogged: %v", err)
			}

			// Assert
			if !slices.Equal(f.merge.backlogged, tt.want) {
				t.Fatalf("the merge queue was told of hides %v, want %v", f.merge.backlogged, tt.want)
			}
		})
	}
}

func TestAFailedHideTellsTheMergeQueueNothing(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.db.setBackloggedErr = errFake

	// Act
	err := f.verbs.SetBacklogged(context.Background(), "w1", true)

	// Assert
	if err == nil {
		t.Fatal("SetBacklogged over a failed write succeeded")
	}
	if len(f.merge.backlogged) != 0 {
		t.Fatalf("the merge queue was told of hides %v over a failed write, want none", f.merge.backlogged)
	}
}

// TestHidingStandsNothingDownAtOnce pins the owner's request of 2026-10-13: a
// backlogged workspace's cache is left to lapse, and the cache keeper stands its
// session down only when it does.
func TestHidingStandsNothingDownAtOnce(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act
	if err := f.verbs.SetBacklogged(context.Background(), "w1", true); err != nil {
		t.Fatalf("SetBacklogged: %v", err)
	}

	// Assert
	if len(f.standDowns) != 0 {
		t.Fatalf("stand-downs = %v, want none at the hide", f.standDowns)
	}
}

func TestShowingAWorkspaceStartsNothingAndStandsNothingDown(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act
	if err := f.verbs.SetBacklogged(context.Background(), "w1", false); err != nil {
		t.Fatalf("SetBacklogged: %v", err)
	}

	// Assert
	if len(f.standDowns) != 0 {
		t.Fatalf("stand-downs = %v, want none for a show", f.standDowns)
	}
}

// hideFixture is a fixture with w1, w2 and w3 in that registry order, and
// CURRENT selected.
func hideFixture(t *testing.T, current ids.WorkspaceID) *fixture {
	t.Helper()
	f := newFixture(t)
	for i, id := range []ids.WorkspaceID{"w1", "w2", "w3"} {
		f.workspace(id, filepath.Join(t.TempDir(), string(id)))
		ws := f.db.workspaces[id]
		ws.Place = int64(i + 1)
		f.db.workspaces[id] = ws
	}
	f.db.current = &current
	return f
}

func TestHidingTheCurrentWorkspaceLandsOnItsLeftNeighbour(t *testing.T) {
	// Arrange
	f := hideFixture(t, "w3")

	// Act
	if err := f.verbs.SetBacklogged(context.Background(), "w3", true); err != nil {
		t.Fatalf("SetBacklogged: %v", err)
	}

	// Assert
	if f.db.current == nil || *f.db.current != "w2" {
		t.Fatalf("current = %v, want w2, the tab left of w3", f.db.current)
	}
	landed := false
	for _, r := range f.log.logger.Records() {
		if r.Operation == opSetBacklogged && r.Level == dlog.LevelInfo && r.Context["workspace"] == "w2" && r.Context["backlogged"] == "w3" && r.Context["place"] != nil {
			landed = true
		}
	}
	if !landed {
		t.Fatalf("no info record names the landing; records = %+v", f.log.logger.Records())
	}
}

func TestHidingTheLeftmostCurrentWorkspaceLandsOnItsRightNeighbour(t *testing.T) {
	// Arrange
	f := hideFixture(t, "w1")

	// Act
	if err := f.verbs.SetBacklogged(context.Background(), "w1", true); err != nil {
		t.Fatalf("SetBacklogged: %v", err)
	}

	// Assert
	if f.db.current == nil || *f.db.current != "w2" {
		t.Fatalf("current = %v, want w2, the tab right of w1", f.db.current)
	}
}

func TestHidingTheCurrentWorkspaceLandsOnTheOneSelectedBeforeIt(t *testing.T) {
	// Arrange: the user selected w1, then w3; w2 sits between them in the bar.
	f := newFixture(t)
	for i, id := range []ids.WorkspaceID{"w1", "w2", "w3", "w4"} {
		ws := f.workspace(id, t.TempDir())
		ws.Place = int64(i + 1)
		f.db.with(ws)
	}
	for minute, id := range map[int]ids.WorkspaceID{1: "w1", 2: "w4", 3: "w3"} {
		ws := f.db.workspaces[id]
		selected := time.Date(2026, 10, 20, 12, minute, 0, 0, time.UTC)
		ws.LastSelectedAt = &selected
		f.db.workspaces[id] = ws
	}
	f.db.current = &[]ids.WorkspaceID{"w3"}[0]

	// Act
	if err := f.verbs.SetBacklogged(context.Background(), "w3", true); err != nil {
		t.Fatalf("SetBacklogged: %v", err)
	}

	// Assert
	if f.db.current == nil || *f.db.current != "w4" {
		t.Fatalf("current = %v, want w4, the workspace selected before w3", f.db.current)
	}
	landed := false
	for _, r := range f.log.logger.Records() {
		if r.Operation == opSetBacklogged && r.Level == dlog.LevelInfo && r.Context["workspace"] == "w4" && r.Context["backlogged"] == "w3" && r.Context["by"] == string(sidebar.LandedByRepository) {
			landed = true
		}
	}
	if !landed {
		t.Fatalf("no info record names the repository landing; records = %+v", f.log.logger.Records())
	}
}

func TestHidingAnotherWorkspaceLeavesTheSelectionWhereItIs(t *testing.T) {
	// Arrange: the user stands on w1 and hides w2.
	f := hideFixture(t, "w1")

	// Act
	if err := f.verbs.SetBacklogged(context.Background(), "w2", true); err != nil {
		t.Fatalf("SetBacklogged: %v", err)
	}

	// Assert
	if f.db.current == nil || *f.db.current != "w1" {
		t.Fatalf("current = %v, want w1 unmoved", f.db.current)
	}
}

func TestHidingWithNoOtherDrawnTabLeavesTheSelectionAndSaysSo(t *testing.T) {
	// Arrange: w2 and w3 are already backlogged.
	f := hideFixture(t, "w1")
	for _, id := range []ids.WorkspaceID{"w2", "w3"} {
		ws := f.db.workspaces[id]
		ws.Backlogged = true
		f.db.workspaces[id] = ws
	}

	// Act
	if err := f.verbs.SetBacklogged(context.Background(), "w1", true); err != nil {
		t.Fatalf("SetBacklogged: %v", err)
	}

	// Assert
	if f.db.current == nil || *f.db.current != "w1" {
		t.Fatalf("current = %v, want w1 unmoved", f.db.current)
	}
	said := false
	for _, r := range f.log.logger.Records() {
		if r.Operation == opSetBacklogged && r.Level == dlog.LevelInfo && r.Message == "the tab bar draws no other workspace beside the backlogged one; the selection stays" {
			said = true
		}
	}
	if !said {
		t.Fatalf("no info record says the selection stays; records = %+v", f.log.logger.Records())
	}
}

func TestAConditionalSelectStandsWhenTheUserMovedOff(t *testing.T) {
	// Arrange: the select is conditional on w1, but the user is on w2.
	f := hideFixture(t, "w2")
	from := ids.WorkspaceID("w1")
	v := f.verbs.(*verbs)

	// Act
	applied, err := v.selectCurrent(context.Background(), f.log.logger, "w3", &from, Switch{History: RecordSelection})

	// Assert
	if err != nil || applied {
		t.Fatalf("selectCurrent = (%v, %v), want (false, nil)", applied, err)
	}
	if *f.db.current != "w2" {
		t.Fatalf("current = %v, want w2, the user's own switch", *f.db.current)
	}
	stood := false
	for _, r := range f.log.logger.Records() {
		if r.Operation == opSelect && r.Level == dlog.LevelInfo && r.Context["from"] == "w1" && r.Context["current"] == "w2" {
			stood = true
		}
	}
	if !stood {
		t.Fatalf("no info record says the user's switch stands; records = %+v", f.log.logger.Records())
	}
}

func TestAFailedCurrentReadFailsTheLandingLoudly(t *testing.T) {
	// Arrange
	f := hideFixture(t, "w1")
	f.db.currentErr = errors.New("disk I/O error")

	// Act
	err := f.verbs.SetBacklogged(context.Background(), "w1", true)

	// Assert
	if err == nil {
		t.Fatal("a landing whose current read failed was answered as success")
	}
	if !loggedAtError(f, opSetBacklogged) {
		t.Fatalf("the failed read was not recorded at error: err=%v records=%+v", err, f.log.logger.Records())
	}
}
func TestHidingLandsOnTheNeighbourTheUserSawBeforeTheHide(t *testing.T) {
	// Arrange: w2 is current between w1 and w3; once backlogged, its tab follows
	// the drawn ones, but the landing is read off the bar as it stood.
	f := newFixture(t)
	for i, id := range []ids.WorkspaceID{"w1", "w2", "w3"} {
		ws := f.workspace(id, t.TempDir())
		ws.Place = int64(i + 1)
		f.db.with(ws)
	}
	f.db.current = &[]ids.WorkspaceID{"w2"}[0]

	// Act.
	if err := f.verbs.SetBacklogged(context.Background(), "w2", true); err != nil {
		t.Fatalf("SetBacklogged: %v", err)
	}

	// Assert.
	if f.db.current == nil || *f.db.current != "w1" {
		t.Fatalf("current = %v, want w1, the left neighbour the user saw", f.db.current)
	}
}

func TestSetBackloggedAnswersAFamilyRefusal(t *testing.T) {
	tests := []struct {
		name       string
		backlogged bool
		err        error
	}{
		{name: "a hide of a parent with open children", backlogged: true, err: errOpenChildren},
		{name: "a show under a parent that is not open", backlogged: false, err: errParentNotOpen},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			f.db.setBackloggedErr = tt.err

			// Act
			err := f.verbs.SetBacklogged(context.Background(), "w1", tt.backlogged)

			// Assert
			requireFamilyAnswer(t, f, err, tt.err, opSetBacklogged)
			if len(f.visibilityEdges) != 0 {
				t.Fatalf("visibility edges = %v, want none for a refused write", f.visibilityEdges)
			}
		})
	}
}

func TestPlaceRefusal(t *testing.T) {
	tests := []struct {
		name string
		err  error
		arm  string
	}{
		{name: "two repositories", err: fmt.Errorf("swap: %w", wsm.ErrOtherRepository), arm: ArmOtherRepository},
		{name: "not siblings", err: fmt.Errorf("swap: %w", wsm.ErrNotSiblings), arm: ArmNotSiblings},
		{name: "any other error", err: errors.New("disk I/O error")},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := placeRefusal(dlog.NewTestLogger(), "MoveWorkspace", tt.err)

			// Assert.
			if tt.arm == "" {
				if got != nil {
					t.Fatalf("placeRefusal = %v, want nil", got)
				}
				return
			}
			if got == nil || got.Arm != tt.arm || got.Rpc != "MoveWorkspace" {
				t.Fatalf("placeRefusal = %+v, want MoveWorkspace's %s", got, tt.arm)
			}
		})
	}
}

func TestSwapAndMoveShareThePlaceRefusal(t *testing.T) {
	// Act.
	shared := sourcescan.Count(t, "placeRefusal(log, ")
	byHand := sourcescan.Count(t, "ArmNotSiblings, err.Error()")

	// Assert.
	if shared != 3 || byHand != 1 {
		t.Fatalf("placeRefusal calls = %d and hand-spelled not_siblings arms = %d, want Swap, Move and the arrangement sharing the one helper", shared, byHand)
	}
}

func TestSelectAsARecencyWalkStepLeavesTheSelectionInstant(t *testing.T) {
	// Arrange: w1 was last chosen an hour ago; the walk steps onto it now.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	earlier := fixedNow.Add(-time.Hour)
	ws := f.db.workspaces["w1"]
	ws.LastSelectedAt = &earlier
	f.db.workspaces["w1"] = ws

	// Act.
	if err := f.verbs.Select(context.Background(), "w1", Switch{History: KeepSelectionHistory}); err != nil {
		t.Fatalf("Select: %v", err)
	}

	// Assert: current moved, the history did not.
	if f.db.current == nil || *f.db.current != "w1" {
		t.Fatalf("current = %v, want w1", f.db.current)
	}
	if !slices.Equal(f.db.keptHistorySelects, []ids.WorkspaceID{"w1"}) {
		t.Fatalf("kept-history selects = %v, want w1 once", f.db.keptHistorySelects)
	}
	if got := f.db.workspaces["w1"].LastSelectedAt; got == nil || !got.Equal(earlier) {
		t.Fatalf("last selected = %v, want the standing %v", got, earlier)
	}
}

func TestSelectAsARecencyWalkStepKeepsItsInstantAsTheWalkLanding(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Select(context.Background(), "w1", Switch{History: KeepSelectionHistory}); err != nil {
		t.Fatalf("Select: %v", err)
	}

	// Assert: the step's instant is the daemon clock's, for the walk's end to commit.
	if !slices.Equal(f.db.keptHistoryAt, []time.Time{fixedNow}) {
		t.Fatalf("walk landing instants = %v, want [%v]", f.db.keptHistoryAt, fixedNow)
	}
}

func TestSelectAsARecencyWalkStepStillTellsTheRoster(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Select(context.Background(), "w1", Switch{History: KeepSelectionHistory}); err != nil {
		t.Fatalf("Select: %v", err)
	}

	// Assert.
	if len(f.sidebar.selected) != 1 || f.sidebar.selected[0] != "w1" {
		t.Fatalf("roster selections = %v, want w1 once", f.sidebar.selected)
	}
}

func TestSelectAsARecencyWalkStepLogsItsReceiptAtInfo(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Select(context.Background(), "w1", Switch{History: KeepSelectionHistory}); err != nil {
		t.Fatalf("Select: %v", err)
	}

	// Assert.
	r, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{
		Message: "selected the workspace as a step of a recency walk; the selection instant stands"})
	if !ok || r.Level != "info" || r.Context["history"] != "keep" {
		t.Fatalf("walk receipt = %+v (found %v), want info with history=keep", r, ok)
	}
}

func TestSelectAsARecencyWalkStepThatCannotRecordFailsLoudly(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.db.keepSelectedErr = errors.New("disk gone")

	// Act.
	err := f.verbs.Select(context.Background(), "w1", Switch{History: KeepSelectionHistory})

	// Assert: the error reaches the caller, is logged once with its cause,
	// and the roster was told nothing.
	if err == nil || !strings.Contains(err.Error(), "disk gone") {
		t.Fatalf("Select = %v, want the store's failure", err)
	}
	r, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Message: "could not record the selection"})
	if !ok || r.Level != "error" || r.Context["cause"] != "disk gone" || r.Context["history"] != "keep" {
		t.Fatalf("failure record = %+v (found %v), want error with cause and history", r, ok)
	}
	if len(f.sidebar.selected) != 0 {
		t.Fatalf("roster selections = %v, want none after a failed record", f.sidebar.selected)
	}
}

func TestSelectionHistoryNamesEachValue(t *testing.T) {
	tests := []struct {
		history SelectionHistory
		want    string
	}{
		{RecordSelection, "record"},
		{KeepSelectionHistory, "keep"},
	}
	for _, tt := range tests {
		t.Run(tt.want, func(t *testing.T) {
			// Act / Assert.
			if got := tt.history.String(); got != tt.want {
				t.Fatalf("String() = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestSelectionHistoryPanicsOnAnUnknownValue(t *testing.T) {
	// Arrange.
	defer func() {
		// Assert.
		if recover() == nil {
			t.Fatal("String() on an unknown value did not panic")
		}
	}()

	// Act.
	_ = SelectionHistory(99).String()
}

// TestSelectStartsNoShim pins the owner ruling of 2026-10-28: switching to a
// workspace starts no shim, whatever its session's state; the next prompt
// does.
func TestSelectStartsNoShim(t *testing.T) {
	cases := []struct {
		name    string
		arrange func(f *fixture)
	}{
		{name: "a hibernated workspace", arrange: func(f *fixture) { f.hibernate("w1") }},
		{name: "a workspace with no session", arrange: func(*fixture) {}},
		{name: "a hibernated workspace on a daemon standing down", arrange: func(f *fixture) {
			f.hibernate("w1")
			f.fleet.standingDown = true
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			tc.arrange(f)

			// Act.
			err := f.verbs.Select(context.Background(), "w1", Switch{History: RecordSelection})

			// Assert.
			if err != nil || len(f.fleet.startCalls) != 0 || len(f.fleet.started) != 0 {
				t.Fatalf("Select = %v after starts %v; want the selection and no start", err, f.fleet.startCalls)
			}
		})
	}
}

func TestSelectLeavesAParkedWorkspacesParkDrawn(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.hibernate("w1")

	// Act.
	if err := f.verbs.Select(context.Background(), "w1", Switch{History: RecordSelection}); err != nil {
		t.Fatalf("Select: %v", err)
	}

	// Assert.
	if len(f.topbarParked) != 0 {
		t.Fatalf("topbar park edges = %v, want the park left as it is", f.topbarParked)
	}
}

var backlogGate = time.Unix(1_700_000_000, 0).UTC()

func TestBacklogColdGateMovesAShownWorkspaceIntoTheBacklog(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act
	err := f.verbs.BacklogColdGate(context.Background(), "w1", backlogGate)

	// Assert
	if err != nil {
		t.Fatalf("BacklogColdGate: %v", err)
	}
	got := f.db.workspaces["w1"]
	if !got.Backlogged || got.BackloggedGateAt == nil || !got.BackloggedGateAt.Equal(backlogGate) {
		t.Fatalf("backlogged = %v for gate %v, want backlogged for %v", got.Backlogged, got.BackloggedGateAt, backlogGate)
	}
	if len(f.sidebar.registries) != 1 {
		t.Fatalf("roster republications = %d, want exactly one", len(f.sidebar.registries))
	}
	if !slices.Equal(f.visibilityEdges, []ids.WorkspaceID{"w1"}) {
		t.Fatalf("visibility edges = %v, want the workspace", f.visibilityEdges)
	}
	if !loggedAt(f, dlog.LevelInfo, opBacklogColdGate) {
		t.Fatal("the move was not recorded at info")
	}
}

func TestBacklogColdGateLeavesAWorkspaceTheStoreDeclines(t *testing.T) {
	// Arrange: the user is looking at it, which the store judges.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	current := f.db.workspaces["w1"]
	current.Current = true
	f.db.workspaces["w1"] = current

	// Act
	err := f.verbs.BacklogColdGate(context.Background(), "w1", backlogGate)

	// Assert
	if err != nil {
		t.Fatalf("BacklogColdGate: %v", err)
	}
	if f.db.workspaces["w1"].Backlogged {
		t.Fatal("the workspace the user is looking at was backlogged")
	}
	if len(f.sidebar.registries) != 0 || len(f.visibilityEdges) != 0 {
		t.Fatalf("republications %d, edges %v; want none for a declined move", len(f.sidebar.registries), f.visibilityEdges)
	}
}

func TestBacklogColdGateAnswersAFamilyRefusalAsNoMove(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.db.backlogErr = errOpenChildren

	// Act
	err := f.verbs.BacklogColdGate(context.Background(), "w1", backlogGate)

	// Assert
	if err != nil {
		t.Fatalf("BacklogColdGate = %v, want nil: a parent with open children stays shown", err)
	}
	if !loggedAt(f, dlog.LevelDebug, opBacklogColdGate) || loggedAtError(f, opBacklogColdGate) {
		t.Fatalf("the refusal was not recorded at debug alone: %v", f.log.logger.Records())
	}
	if len(f.sidebar.registries) != 0 || len(f.visibilityEdges) != 0 {
		t.Fatalf("republications %d, edges %v; want none for a refused move", len(f.sidebar.registries), f.visibilityEdges)
	}
}

func TestBacklogColdGateSurfacesAFailedWriteAtError(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.db.backlogErr = errors.New("disk I/O error")

	// Act
	err := f.verbs.BacklogColdGate(context.Background(), "w1", backlogGate)

	// Assert
	if err == nil {
		t.Fatal("a failed backlog write was answered as success")
	}
	if !loggedAtError(f, opBacklogColdGate) {
		t.Fatal("the failed write was not recorded at error")
	}
	if len(f.sidebar.registries) != 0 || len(f.visibilityEdges) != 0 {
		t.Fatalf("republications %d, edges %v; want none for a failed write", len(f.sidebar.registries), f.visibilityEdges)
	}
}

// TestEveryBacklogMoveEndsInBackloggedRecorded fails when a move into or
// out of the backlog republishes or raises the keeper's edge by hand.
func TestEveryBacklogMoveEndsInBackloggedRecorded(t *testing.T) {
	// Act
	sites := sourcescan.Sites(t, "v.backloggedRecorded(ctx, log, ws, ")

	// Assert
	if !slices.Equal(sites, []string{"select.go", "select.go"}) {
		t.Fatalf("backloggedRecorded is called at %v, want recordBacklogged and BacklogColdGate in select.go", sites)
	}
}

func TestABacklogInterruptsARunningTurnFirst(t *testing.T) {
	tests := []struct {
		name           string
		killErr        error
		wantBacklogged bool
		wantErr        bool
	}{
		{name: "the turn is interrupted, then the workspace is backlogged", wantBacklogged: true},
		{name: "a failed interrupt backlogs nothing", killErr: errors.New("shim unreachable"), wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			runningTurn(f, 0)
			f.shim.killTurnErr = tt.killErr

			// Act
			err := f.verbs.SetBacklogged(context.Background(), "w1", true)

			// Assert
			if (err != nil) != tt.wantErr {
				t.Fatalf("SetBacklogged error = %v, want error %v", err, tt.wantErr)
			}
			if len(f.shim.killedTurns) != 1 && tt.killErr == nil {
				t.Fatalf("killed turns = %+v, want one", f.shim.killedTurns)
			}
			if got := f.db.workspaces["w1"].Backlogged; got != tt.wantBacklogged {
				t.Fatalf("backlogged = %v, want %v", got, tt.wantBacklogged)
			}
			if tt.wantErr && !loggedAtError(f, opSetBacklogged) {
				t.Fatal("the failed interrupt was not recorded at error")
			}
		})
	}
}

func TestABacklogOfAnIdleWorkspaceInterruptsNothing(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act
	if err := f.verbs.SetBacklogged(context.Background(), "w1", true); err != nil {
		t.Fatalf("SetBacklogged: %v", err)
	}

	// Assert
	if len(f.shim.killedTurns) != 0 {
		t.Fatalf("killed turns = %+v, want none for an idle workspace", f.shim.killedTurns)
	}
}
