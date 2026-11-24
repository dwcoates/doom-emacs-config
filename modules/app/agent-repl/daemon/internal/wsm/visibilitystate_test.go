package wsm

import (
	"context"
	"errors"
	"testing"

	"agent-repld/internal/dlog"
)

// visibilityOf runs the one derivation over INPUTS and answers ID's result.
func visibilityOf(inputs []VisibilityInput, id WorkspaceID) Visibility {
	return Visibilities(inputs)[id]
}

func under(id WorkspaceID) *WorkspaceID { return &id }

func TestVisibilities(t *testing.T) {
	cases := []struct {
		name   string
		inputs []VisibilityInput
		id     WorkspaceID
		want   Visibility
	}{
		{name: "an open top-level workspace no fold covers is visible",
			inputs: []VisibilityInput{{ID: "a", Open: true}}, id: "a", want: UnselectedVisible},
		{name: "a workspace that is not open is never visible",
			inputs: []VisibilityInput{{ID: "a", Working: true, Unviewed: true}}, id: "a", want: NotVisible},
		{name: "a child of a visible unfolded parent is visible",
			inputs: []VisibilityInput{{ID: "p", Open: true}, {ID: "c", Parent: under("p"), Open: true}}, id: "c", want: UnselectedVisible},
		{name: "a covered workspace that is idle and read is not visible",
			inputs: []VisibilityInput{{ID: "a", Open: true, Covered: true}}, id: "a", want: NotVisible},
		{name: "a covered workspace that is working is visible",
			inputs: []VisibilityInput{{ID: "a", Open: true, Covered: true, Working: true}}, id: "a", want: UnselectedVisible},
		{name: "a covered workspace that is unviewed is visible",
			inputs: []VisibilityInput{{ID: "a", Open: true, Covered: true, Unviewed: true}}, id: "a", want: UnselectedVisible},
		{name: "a covered workspace that is starting is visible",
			inputs: []VisibilityInput{{ID: "a", Open: true, Covered: true, Starting: true}}, id: "a", want: UnselectedVisible},
		{name: "a covered workspace of priority P0.5 or P1 is visible",
			inputs: []VisibilityInput{{ID: "a", Open: true, Covered: true, TopPriority: true}}, id: "a", want: UnselectedVisible},
		{name: "a workspace of priority P0.5 or P1 that is not open is not visible",
			inputs: []VisibilityInput{{ID: "a", Covered: true, TopPriority: true}}, id: "a", want: NotVisible},
		{name: "the parent of a covered top-priority workspace is visible",
			inputs: []VisibilityInput{{ID: "p", Open: true, Covered: true}, {ID: "c", Parent: under("p"), Open: true, Covered: true, TopPriority: true}}, id: "p", want: UnselectedVisible},
		{name: "a sibling of a covered top-priority workspace stays not visible",
			inputs: []VisibilityInput{{ID: "p", Open: true, Covered: true}, {ID: "c", Parent: under("p"), Open: true, Covered: true, TopPriority: true}, {ID: "s", Parent: under("p"), Open: true, Covered: true}}, id: "s", want: NotVisible},
		{name: "the parent of a visible covered workspace is visible",
			inputs: []VisibilityInput{{ID: "p", Open: true, Covered: true}, {ID: "c", Parent: under("p"), Open: true, Covered: true, Working: true}}, id: "p", want: UnselectedVisible},
		{name: "every ancestor of a visible covered workspace is visible",
			inputs: []VisibilityInput{{ID: "g", Open: true, Covered: true}, {ID: "p", Parent: under("g"), Open: true, Covered: true}, {ID: "c", Parent: under("p"), Open: true, Covered: true, Unviewed: true}}, id: "g", want: UnselectedVisible},
		{name: "a sibling of a visible covered workspace stays not visible",
			inputs: []VisibilityInput{{ID: "p", Open: true, Covered: true}, {ID: "c", Parent: under("p"), Open: true, Covered: true, Working: true}, {ID: "s", Parent: under("p"), Open: true, Covered: true}}, id: "s", want: NotVisible},
		{name: "an ancestor that is not open breaks the chain",
			inputs: []VisibilityInput{{ID: "p", Covered: true}, {ID: "c", Parent: under("p"), Open: true, Covered: true, Working: true}}, id: "p", want: NotVisible},
		{name: "the selection is selected",
			inputs: []VisibilityInput{{ID: "a", Open: true, Selected: true}}, id: "a", want: Selected},
		{name: "an excluded selection is still selected",
			inputs: []VisibilityInput{{ID: "h", Selected: true}}, id: "h", want: Selected},
		{name: "a covered selection is selected",
			inputs: []VisibilityInput{{ID: "a", Open: true, Covered: true, Selected: true}}, id: "a", want: Selected},
		{name: "the parent of a covered selection is visible",
			inputs: []VisibilityInput{{ID: "p", Open: true, Covered: true}, {ID: "c", Parent: under("p"), Open: true, Covered: true, Selected: true}}, id: "p", want: UnselectedVisible},
		{name: "a selected parent of a visible child stays selected",
			inputs: []VisibilityInput{{ID: "p", Open: true, Covered: true, Selected: true}, {ID: "c", Parent: under("p"), Open: true, Covered: true, Working: true}}, id: "p", want: Selected},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := visibilityOf(tc.inputs, tc.id)

			// Assert
			if got != tc.want {
				t.Fatalf("visibility of %s = %v, want %v", tc.id, got, tc.want)
			}
		})
	}
}

func TestEveryInputAnswersAVisibility(t *testing.T) {
	// Arrange
	inputs := []VisibilityInput{{ID: "a", Open: true}, {ID: "b"}, {ID: "c", Parent: under("gone"), Open: true, Covered: true, Working: true}}

	// Act
	got := Visibilities(inputs)

	// Assert
	if len(got) != 3 || got["b"] != NotVisible || got["c"] != UnselectedVisible {
		t.Fatalf("visibilities = %v, want one answer per input, a missing parent ending the chain", got)
	}
}

func TestVisibilityReasons(t *testing.T) {
	cases := []struct {
		name   string
		inputs []VisibilityInput
		id     WorkspaceID
		want   VisibilityReason
	}{
		{name: "a workspace that is not open has not_open",
			inputs: []VisibilityInput{{ID: "a", Working: true}}, id: "a", want: ReasonNotOpen},
		{name: "a covered idle read workspace has covered",
			inputs: []VisibilityInput{{ID: "a", Open: true, Covered: true}}, id: "a", want: ReasonCovered},
		{name: "an open workspace no fold covers has uncovered",
			inputs: []VisibilityInput{{ID: "a", Open: true, Working: true, TopPriority: true}}, id: "a", want: ReasonUncovered},
		{name: "a covered working workspace has covered_working before every later input",
			inputs: []VisibilityInput{{ID: "a", Open: true, Covered: true, Working: true, Unviewed: true, Starting: true, TopPriority: true}}, id: "a", want: ReasonCoveredWorking},
		{name: "a covered unviewed workspace has covered_unviewed before starting and priority",
			inputs: []VisibilityInput{{ID: "a", Open: true, Covered: true, Unviewed: true, Starting: true, TopPriority: true}}, id: "a", want: ReasonCoveredUnviewed},
		{name: "a covered starting workspace has covered_starting before priority",
			inputs: []VisibilityInput{{ID: "a", Open: true, Covered: true, Starting: true, TopPriority: true}}, id: "a", want: ReasonCoveredStarting},
		{name: "a covered top-priority workspace has covered_top_priority",
			inputs: []VisibilityInput{{ID: "a", Open: true, Covered: true, TopPriority: true}}, id: "a", want: ReasonCoveredTopPriority},
		{name: "a covered mediator or bypass holder has covered_always_shown",
			inputs: []VisibilityInput{{ID: "a", Open: true, Covered: true, AlwaysShown: true}}, id: "a", want: ReasonCoveredAlwaysShown},
		{name: "a covered parent visible only through its child has parent_of_visible",
			inputs: []VisibilityInput{{ID: "p", Open: true, Covered: true}, {ID: "c", Parent: under("p"), Open: true, Covered: true, Working: true}}, id: "p", want: ReasonParentOfVisible},
		{name: "a covered parent with its own reason keeps it",
			inputs: []VisibilityInput{{ID: "p", Open: true, Covered: true, Unviewed: true}, {ID: "c", Parent: under("p"), Open: true, Covered: true, Working: true}}, id: "p", want: ReasonCoveredUnviewed},
		{name: "every ancestor of a visible covered workspace has parent_of_visible",
			inputs: []VisibilityInput{{ID: "g", Open: true, Covered: true}, {ID: "p", Parent: under("g"), Open: true, Covered: true}, {ID: "c", Parent: under("p"), Open: true, Covered: true, Working: true}}, id: "g", want: ReasonParentOfVisible},
		{name: "the parent of a covered selection has parent_of_visible",
			inputs: []VisibilityInput{{ID: "p", Open: true, Covered: true}, {ID: "c", Parent: under("p"), Open: true, Covered: true, Selected: true}}, id: "p", want: ReasonParentOfVisible},
		{name: "the selection has selected whatever else holds",
			inputs: []VisibilityInput{{ID: "a", Open: true, Covered: true, Working: true, Selected: true}}, id: "a", want: ReasonSelected},
		{name: "a selection that is not open has selected",
			inputs: []VisibilityInput{{ID: "h", Selected: true}}, id: "h", want: ReasonSelected},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := VisibilityReasons(tc.inputs)[tc.id]

			// Assert
			if got != tc.want {
				t.Fatalf("reason of %s = %v, want %v", tc.id, got, tc.want)
			}
		})
	}
}

func TestEveryReasonDecidesTheVisibilityTheDerivationAnswers(t *testing.T) {
	// Arrange
	inputs := []VisibilityInput{
		{ID: "n"}, {ID: "cv", Open: true, Covered: true}, {ID: "u", Open: true},
		{ID: "w", Open: true, Covered: true, Working: true}, {ID: "p", Open: true, Covered: true},
		{ID: "c", Parent: under("p"), Open: true, Covered: true, Unviewed: true}, {ID: "s", Open: true, Selected: true},
	}

	// Act
	reasons, visibilities := VisibilityReasons(inputs), Visibilities(inputs)

	// Assert
	for id, r := range reasons {
		if r.Visibility() != visibilities[id] {
			t.Errorf("%s: reason %v decides %v, the derivation answered %v", id, r, r.Visibility(), visibilities[id])
		}
	}
}

func TestEveryVisibilityReasonHasItsOwnName(t *testing.T) {
	// Arrange
	seen := map[string]VisibilityReason{}

	// Act and Assert
	for _, r := range VisibilityReasonsAll {
		name := r.String()
		if other, dup := seen[name]; dup {
			t.Fatalf("reasons %d and %d share the name %q", int(other), int(r), name)
		}
		seen[name] = r
	}
	if len(seen) != int(ReasonParentOfVisible)+1 {
		t.Fatalf("VisibilityReasonsAll holds %d reasons, want every one of %d", len(seen), int(ReasonParentOfVisible)+1)
	}
}

func TestAVisibilityRoundTripsThroughItsName(t *testing.T) {
	for _, v := range []Visibility{NotVisible, UnselectedVisible, Selected} {
		// Act
		got, err := parseVisibility(v.String())

		// Assert
		if err != nil || got != v {
			t.Fatalf("parseVisibility(%q) = %v, %v; want %v", v.String(), got, err, v)
		}
	}
}

func TestAnUnknownStoredVisibilityIsRefused(t *testing.T) {
	// Act
	_, err := parseVisibility("dimmed")

	// Assert
	if err == nil {
		t.Fatal("parseVisibility accepted an unknown visibility")
	}
}

func TestSetVisibilitiesPersistsEachChange(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	a, b := testWorkspaceNamed(t, s, "a"), testWorkspaceNamed(t, s, "b")

	// Act
	err := s.SetVisibilities(context.Background(), map[WorkspaceID]Visibility{a.ID: Selected, b.ID: UnselectedVisible})

	// Assert
	if err != nil {
		t.Fatalf("SetVisibilities: %v", err)
	}
	for id, want := range map[WorkspaceID]Visibility{a.ID: Selected, b.ID: UnselectedVisible} {
		got, err := s.Workspace(context.Background(), id)
		if err != nil || got.Visibility != want {
			t.Fatalf("Workspace(%s).Visibility = %v, %v; want %v", id, got.Visibility, err, want)
		}
	}
}

func TestANewWorkspaceIsNotVisibleUntilItsVisibilityIsSet(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	ws := testWorkspace(t, s)

	// Assert
	if ws.Visibility != NotVisible {
		t.Fatalf("a new workspace's visibility = %v, want not_visible", ws.Visibility)
	}
}

func TestSetVisibilitiesRefusesAnUnknownWorkspaceAndChangesNothing(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	a := testWorkspace(t, s)

	// Act
	err := s.SetVisibilities(context.Background(), map[WorkspaceID]Visibility{a.ID: Selected, "ws-nope": UnselectedVisible})

	// Assert
	if err == nil {
		t.Fatal("SetVisibilities affected no row for an unknown workspace and said nothing")
	}
	if !loggedOperation(log, "daemon.wsm.set_visibilities", "debug") {
		t.Fatalf("the refused write was not recorded: %v", log.Records())
	}
	if got, _ := s.Workspace(context.Background(), a.ID); got.Visibility != NotVisible {
		t.Fatalf("the known workspace's visibility = %v after the refused write, want not_visible", got.Visibility)
	}
}

func TestVisibilitiesOnAReadOnlyHandleAreHeldAndWrittenAtThePromotion(t *testing.T) {
	// Arrange
	ctx := context.Background()
	ro, id, log := readOnlyWithWorkspace(t)
	if err := ro.SetVisibilities(ctx, map[WorkspaceID]Visibility{id: UnselectedVisible}); err != nil {
		t.Fatalf("SetVisibilities on a read-only handle = %v, want held", err)
	}
	if err := ro.SetVisibilities(ctx, map[WorkspaceID]Visibility{id: Selected}); err != nil {
		t.Fatalf("SetVisibilities: %v", err)
	}

	// Act
	if err := ro.Promote(ctx); err != nil {
		t.Fatalf("Promote: %v", err)
	}

	// Assert
	got, err := ro.Workspace(ctx, id)
	if err != nil || got.Visibility != Selected {
		t.Fatalf("visibility after the promotion = %v, %v; want the latest held, selected", got.Visibility, err)
	}
	for _, r := range log.Records() {
		if r.Level == "error" || r.Level == "warn" {
			t.Fatalf("a held visibility recorded %s: %+v", r.Level, r)
		}
	}
}

func TestACorruptStoredVisibilityIsADecodeError(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if _, err := s.db().Exec(`PRAGMA ignore_check_constraints = ON`); err != nil {
		t.Fatalf("pragma: %v", err)
	}
	if _, err := s.db().Exec(`UPDATE workspaces SET visibility = 'dimmed' WHERE id = ?`, string(ws.ID)); err != nil {
		t.Fatalf("corrupt the row: %v", err)
	}

	// Act
	_, err := s.Workspace(context.Background(), ws.ID)

	// Assert
	var decode *DecodeError
	if !errors.As(err, &decode) || decode.Field != "visibility" {
		t.Fatalf("Workspace = %v, want a DecodeError on visibility", err)
	}
}

// seedOver runs the migration's seed over S's workspaces as they stand, with
// every visibility reset first, and answers each one's seeded visibility.
func seedOver(t *testing.T, s *store, ids ...WorkspaceID) map[WorkspaceID]Visibility {
	t.Helper()
	ctx := context.Background()
	if _, err := s.db().Exec(`UPDATE workspaces SET visibility = 'not_visible'`); err != nil {
		t.Fatalf("reset: %v", err)
	}
	tx, err := s.db().BeginTx(ctx, nil)
	if err != nil {
		t.Fatalf("begin: %v", err)
	}
	if err := seedVisibility(ctx, tx, dlog.NewTestLogger()); err != nil {
		tx.Rollback()
		t.Fatalf("seedVisibility: %v", err)
	}
	if err := tx.Commit(); err != nil {
		t.Fatalf("commit: %v", err)
	}
	out := map[WorkspaceID]Visibility{}
	for _, id := range ids {
		ws, err := s.Workspace(ctx, id)
		if err != nil {
			t.Fatalf("Workspace(%s): %v", id, err)
		}
		out[id] = ws.Visibility
	}
	return out
}

func TestTheVisibilitySeedDerivesFromTheRecord(t *testing.T) {
	// Arrange: one drawn, one hidden, one selected hidden, one under a
	// folded parent, and that parent.
	s, _ := testStore(t)
	drawn, hidden, picked := testWorkspaceNamed(t, s, "drawn"), testWorkspaceNamed(t, s, "hidden"), testWorkspaceNamed(t, s, "picked")
	parent, child := testWorkspaceNamed(t, s, "parent"), testWorkspaceNamed(t, s, "child")
	for _, stmt := range []struct {
		q    string
		args []any
	}{
		{`UPDATE workspaces SET hidden = 1, hidden_at = 1 WHERE id IN (?, ?)`, []any{string(hidden.ID), string(picked.ID)}},
		{`UPDATE workspaces SET is_current = 1 WHERE id = ?`, []any{string(picked.ID)}},
		{`UPDATE workspaces SET parent_id = ? WHERE id = ?`, []any{string(parent.ID), string(child.ID)}},
		{`UPDATE workspaces SET children_folded = 1 WHERE id = ?`, []any{string(parent.ID)}},
	} {
		if _, err := s.db().Exec(stmt.q, stmt.args...); err != nil {
			t.Fatalf("arrange %q: %v", stmt.q, err)
		}
	}

	// Act
	got := seedOver(t, s, drawn.ID, hidden.ID, picked.ID, parent.ID, child.ID)

	// Assert
	want := map[WorkspaceID]Visibility{drawn.ID: UnselectedVisible, hidden.ID: NotVisible, picked.ID: Selected,
		parent.ID: UnselectedVisible, child.ID: NotVisible}
	for id, v := range want {
		if got[id] != v {
			t.Errorf("seeded visibility of %s = %v, want %v", id, got[id], v)
		}
	}
}

func TestTheVisibilitySeedReadsARepositoryFold(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if _, err := s.db().Exec(`UPDATE repositories SET folded = 1 WHERE id = ?`, string(ws.Repo)); err != nil {
		t.Fatalf("fold: %v", err)
	}

	// Act
	got := seedOver(t, s, ws.ID)

	// Assert
	if got[ws.ID] != NotVisible {
		t.Fatalf("seeded visibility under a folded repository = %v, want not_visible", got[ws.ID])
	}
}

func TestTheVisibilityStepIsAdditiveAndSeedsItsColumn(t *testing.T) {
	// Arrange
	step := migrationTo(t, 192)
	path := fixtureAt(t, 191)

	// Act
	execRaw(t, path, step.DDL)

	// Assert
	if step.Name != "visibility" || step.Kind != MigrationAdditive || step.Rewrite == nil {
		t.Fatalf("migration 192 = %+v, want the visibility column, additive, seeded by a rewrite", step)
	}
	if got := rawScalar[int](t, path, `SELECT count(*) FROM pragma_table_info('workspaces') WHERE name = 'visibility'`); got != 1 {
		t.Fatalf("workspaces.visibility exists %d times, want once", got)
	}
}

func TestTheVisibilitySeedKeepsATopPriorityWorkspaceVisibleUnderAFold(t *testing.T) {
	tests := []struct {
		name     string
		priority *Priority
		want     Visibility
	}{
		{name: "P0.5", priority: &[]Priority{PriorityP05}[0], want: UnselectedVisible},
		{name: "P1", priority: &[]Priority{PriorityP1}[0], want: UnselectedVisible},
		{name: "P2", priority: &[]Priority{PriorityP2}[0], want: NotVisible},
		{name: "P3", priority: &[]Priority{PriorityP3}[0], want: NotVisible},
		{name: "none", priority: nil, want: NotVisible},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			if err := s.SetPriority(context.Background(), ws.ID, tt.priority); err != nil {
				t.Fatalf("SetPriority: %v", err)
			}
			if _, err := s.db().Exec(`UPDATE repositories SET folded = 1 WHERE id = ?`, string(ws.Repo)); err != nil {
				t.Fatalf("fold: %v", err)
			}

			// Act
			got := seedOver(t, s, ws.ID)

			// Assert
			if got[ws.ID] != tt.want {
				t.Fatalf("seeded visibility under a folded repository = %v, want %v", got[ws.ID], tt.want)
			}
		})
	}
}
