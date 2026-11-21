package sidebar_test

import (
	"agentrepl/protohelpers/rosterwalk"
	"testing"

	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/sidebar"
)

// heldIDs reads every row the resolver last published, and every merged row
// it pages, that carries the held marker, workspaces and threads alike, by id.
func heldIDs(t *testing.T, r sidebar.Resolver) map[string]bool {
	t.Helper()
	push := latestPush(t, r)
	out := map[string]bool{}
	for _, row := range append(rosterwalk.AllRows(push), mergedRows(t, r)...) {
		if row.GetHeld() != nil {
			out[row.GetWorkspace().GetWorkspace().GetId()] = true
		}
	}
	for _, row := range push.GetWorkspaces().GetThreads().GetRows() {
		if row.GetHeld() != nil {
			out[row.GetThread().GetId()] = true
		}
	}
	return out
}

// TestTheHeldMarkerNamesTheDrawnRowsAndTheSelection covers every section's
// rows: numberingRegistry holds drawn rows (a1, a2, p, k, b1, the reopened m),
// one hidden row (h), one row of a folded repository (c1) and a closed row (x).
func TestTheHeldMarkerNamesTheDrawnRowsAndTheSelection(t *testing.T) {
	drawn := []string{"a1", "a2", "p", "k", "m", "b1"}
	cases := []struct {
		name     string
		registry func() sidebar.Registry
		selected ids.WorkspaceID
		want     []string
	}{
		{name: "every drawn row is held, and no folded, hidden or closed one", registry: numberingRegistry, want: drawn},
		{name: "a folded subsection's rows are not held", registry: foldedSubsectionRegistry, want: []string{"a1", "p", "k", "m", "b1"}},
		{name: "a folded selection is held", registry: numberingRegistry, selected: "c1", want: append(append([]string{}, drawn...), "c1")},
		{name: "a hidden selection is held", registry: numberingRegistry, selected: "h", want: append(append([]string{}, drawn...), "h")},
		{name: "a closed selection is not held", registry: numberingRegistry, selected: "x", want: drawn},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			r, _ := newResolver(t)

			// Act
			if tc.selected == "" {
				r.SetRegistry(tc.registry())
			} else {
				r.SetRegistrySelected(tc.registry(), tc.selected, sidebar.SelectStamp{})
			}

			// Assert
			got := heldIDs(t, r)
			if len(got) != len(tc.want) {
				t.Fatalf("held = %v, want %v", got, tc.want)
			}
			for _, id := range tc.want {
				if !got[id] {
					t.Fatalf("held = %v, want %v", got, tc.want)
				}
			}
		})
	}
}

func TestASurfacedRowInsideAFoldIsHeld(t *testing.T) {
	// Arrange
	r, _ := newResolver(t)

	// Act
	r.SetRegistry(surfacedIn(foldedBlockFamily(), "w-a1"))

	// Assert
	got := heldIDs(t, r)
	if !got["w-p"] || !got["w-a1"] || len(got) != 2 {
		t.Fatalf("held = %v, want w-p and the surfaced w-a1 alone", got)
	}
}

func TestAThreadIsHeldOnlyWhileItIsTheSelection(t *testing.T) {
	cases := []struct {
		name     string
		selected ids.WorkspaceID
		want     bool
	}{
		{name: "the selected thread is held", selected: "t-1", want: true},
		{name: "a thread the user left is not held", selected: "w-a", want: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			r, _ := newResolver(t)

			// Act
			r.SetRegistrySelected(withThreads(thread("t-1", "why", nil)), tc.selected, sidebar.SelectStamp{})

			// Assert
			row := latest(t, r).GetThreads().GetRows()[0]
			if (row.GetHeld() != nil) != tc.want {
				t.Fatalf("thread held = %v, want %v", row.GetHeld() != nil, tc.want)
			}
		})
	}
}
