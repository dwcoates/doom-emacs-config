package pageheld

import (
	"slices"
	"testing"

	"agent-repld/internal/ids"
)

// counts records what a table counted.
type counts struct {
	loads   []LoadReason
	unloads []UnloadReason
}

func (c *counts) CountPageLoad(r LoadReason)     { c.loads = append(c.loads, r) }
func (c *counts) CountPageUnload(r UnloadReason) { c.unloads = append(c.unloads, r) }

func TestALoadIsHeldAndCounted(t *testing.T) {
	// Arrange
	c := &counts{}
	table := New(c)

	// Act
	table.Loaded("a", LoadAwake)

	// Assert
	if got := table.Reported(); len(got) != 1 || got["a"] != LoadAwake || len(c.loads) != 1 || c.loads[0] != LoadAwake {
		t.Fatalf("pages %v, loads %v; want a held for awake, counted once", got, c.loads)
	}
}

func TestAnUnloadDropsThePageAndIsCounted(t *testing.T) {
	// Arrange
	c := &counts{}
	table := New(c)
	table.Loaded("a", LoadShown)

	// Act
	table.Unloaded("a", UnloadDormant)

	// Assert
	if got := table.Reported(); len(got) != 0 || len(c.unloads) != 1 || c.unloads[0] != UnloadDormant {
		t.Fatalf("pages %v, unloads %v; want none held, one dormant unload", got, c.unloads)
	}
}

func TestARestatementIsHeldButNeverCounted(t *testing.T) {
	// Arrange
	c := &counts{}
	table := New(c)

	// Act
	table.Held("a", LoadSlot)
	table.Held("b", LoadShown)
	table.Forget("b")

	// Assert
	if got := table.Reported(); len(got) != 1 || got["a"] != LoadSlot || len(c.loads)+len(c.unloads) != 0 {
		t.Fatalf("pages %v, loads %v, unloads %v; want a held, nothing counted", got, c.loads, c.unloads)
	}
}

func TestEveryAnswerIsACopy(t *testing.T) {
	tests := []struct {
		name   string
		answer func(*Table) map[ids.WorkspaceID]LoadReason
	}{
		{name: "the snapshot", answer: (*Table).Snapshot},
		{name: "Emacs's report", answer: (*Table).Reported},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			table := hosted()
			table.Held("a", LoadSlot)
			report(t, table, []ids.WorkspaceID{"a"}, false)

			// Act
			delete(tt.answer(table), "a")

			// Assert
			if len(tt.answer(table)) != 1 {
				t.Fatal("changing the answer changed the table")
			}
		})
	}
}

func TestTheSnapshotIsTheHostsFramesLabelledWithEmacssReasons(t *testing.T) {
	// Arrange
	table := hosted()
	table.Held("a", LoadAwake)
	table.Held("emacs-only", LoadShown)

	// Act
	report(t, table, []ids.WorkspaceID{"a", "host-only"}, false)

	// Assert
	got := table.Snapshot()
	if len(got) != 2 || got["a"] != LoadAwake || got["host-only"] != LoadUnreported {
		t.Fatalf("snapshot = %v, want a for awake and host-only unreported, never emacs-only", got)
	}
}

func TestTheSnapshotHoldsNothingBeforeTheHostsFirstCensus(t *testing.T) {
	// Arrange
	table := New(&counts{})

	// Act
	table.Held("a", LoadAwake)

	// Assert
	if got := table.Snapshot(); len(got) != 0 {
		t.Fatalf("snapshot = %v, want none before the host reports", got)
	}
}

// host is the connection of the host document the tests attach.
const host = "host-1"

// hosted answers a table with the host document's page stream standing.
func hosted() *Table {
	table := New(&counts{})
	table.HostAttached(host)
	return table
}

// report hands TABLE the host's census of FRAMES, failing T when it is not
// taken.
func report(t *testing.T, table *Table, frames []ids.WorkspaceID, tick bool) Check {
	t.Helper()
	check, taken := table.HostReported(host, frames, tick)
	if !taken {
		t.Fatal("the census of a standing host was not taken")
	}
	return check
}

// census is one host report: its frames, and whether it is a tick.
type census struct {
	frames []ids.WorkspaceID
	tick   bool
}

func TestADisagreementBeginsOnlyWhenItStandsAcrossTwoTicks(t *testing.T) {
	tests := []struct {
		name string
		// emacs is Emacs's report before the first census.
		emacs map[ids.WorkspaceID]LoadReason
		// between runs after the first census.
		between func(*Table)
		later   []census
		want    []Cause
	}{
		{
			name:  "a frame Emacs reports nothing of, at two ticks",
			later: []census{{frames: []ids.WorkspaceID{"a"}, tick: true}},
			want:  []Cause{HostOnly},
		},
		{
			name:  "a page Emacs reports that the host holds no frame of, at two ticks",
			emacs: map[ids.WorkspaceID]LoadReason{"a": LoadShown},
			later: []census{{tick: true}},
			want:  []Cause{EmacsOnly},
		},
		{
			name:  "both ways at once",
			emacs: map[ids.WorkspaceID]LoadReason{"e": LoadShown},
			later: []census{{frames: []ids.WorkspaceID{"a"}, tick: true}},
			want:  []Cause{HostOnly, EmacsOnly},
		},
		{
			name:  "a change census never begins one",
			later: []census{{frames: []ids.WorkspaceID{"a"}}},
		},
		{
			name:    "Emacs's report of another workspace between the ticks",
			between: func(t *Table) { t.Loaded("b", LoadShown); t.Unloaded("b", UnloadReleased) },
			later:   []census{{frames: []ids.WorkspaceID{"a"}, tick: true}},
			want:    []Cause{HostOnly},
		},
		{
			name:    "Emacs's report of the workspace between the ticks: a report in flight",
			between: func(t *Table) { t.Forget("a") },
			later:   []census{{frames: []ids.WorkspaceID{"a"}, tick: true}},
		},
		{
			name: "a change census between the ticks that showed agreement",
			later: []census{
				{},
				{frames: []ids.WorkspaceID{"a"}},
				{frames: []ids.WorkspaceID{"a"}, tick: true},
			},
		},
		{
			name:  "a disagreement gone by the next tick",
			later: []census{{tick: true}},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			table := hosted()
			for ws, r := range tt.emacs {
				table.Held(ws, r)
			}
			first := []ids.WorkspaceID{"a"}
			if tt.emacs != nil && tt.emacs["a"] == LoadShown {
				first = nil
			}
			report(t, table, first, true)
			if tt.between != nil {
				tt.between(table)
			}

			// Act
			var began []Cause
			for _, c := range tt.later {
				for _, d := range report(t, table, c.frames, c.tick).Began {
					began = append(began, d.Cause)
				}
			}

			// Assert
			if !slices.Equal(began, tt.want) {
				t.Fatalf("began %v, want %v", began, tt.want)
			}
		})
	}
}

func TestABeganDisagreementNamesItsWorkspacesAndBothSets(t *testing.T) {
	// Arrange
	table := hosted()
	table.Held("c", LoadShown)
	table.Held("a", LoadShown)
	report(t, table, []ids.WorkspaceID{"b", "a"}, true)

	// Act
	check := report(t, table, []ids.WorkspaceID{"b", "a"}, true)

	// Assert
	want := []Disagreement{
		{Cause: HostOnly, Workspaces: []ids.WorkspaceID{"b"}, Host: []ids.WorkspaceID{"a", "b"}, Emacs: []ids.WorkspaceID{"a", "c"}},
		{Cause: EmacsOnly, Workspaces: []ids.WorkspaceID{"c"}, Host: []ids.WorkspaceID{"a", "b"}, Emacs: []ids.WorkspaceID{"a", "c"}},
	}
	if len(check.Began) != len(want) {
		t.Fatalf("began %+v, want %+v", check.Began, want)
	}
	for i, d := range check.Began {
		w := want[i]
		if d.Cause != w.Cause || !slices.Equal(d.Workspaces, w.Workspaces) || !slices.Equal(d.Host, w.Host) || !slices.Equal(d.Emacs, w.Emacs) {
			t.Fatalf("began %+v, want %+v", check.Began, want)
		}
	}
}

func TestAStandingCauseBeginsOnceAndClearsOnce(t *testing.T) {
	// Arrange
	table := hosted()
	frames := []ids.WorkspaceID{"a"}
	report(t, table, frames, true)
	began := report(t, table, frames, true)

	// Act
	again := report(t, table, append(frames, "b"), true)
	cleared := report(t, table, nil, true)
	after := report(t, table, nil, true)

	// Assert
	if len(began.Began) != 1 || len(again.Began) != 0 || len(again.Cleared) != 0 {
		t.Fatalf("began %+v, then %+v; want one begin while the cause stands", began, again)
	}
	if !slices.Equal(cleared.Cleared, []Cause{HostOnly}) || len(after.Cleared) != 0 {
		t.Fatalf("cleared %v, then %v; want host_only cleared once", cleared.Cleared, after.Cleared)
	}
}

func TestAnUndeclaredLoadReasonPanics(t *testing.T) {
	// Arrange
	table := New(&counts{})
	defer func() {
		// Assert
		if recover() == nil {
			t.Fatal("an undeclared load reason did not panic")
		}
	}()

	// Act
	table.Loaded("a", "whim")
}

func TestATableWithoutTelemetryPanics(t *testing.T) {
	defer func() {
		// Assert
		if recover() == nil {
			t.Fatal("New(nil) did not panic")
		}
	}()

	// Act
	New(nil)
}

func TestACensusFromAHostWithNoStandingStreamIsNotTaken(t *testing.T) {
	// Arrange
	table := New(&counts{})

	// Act
	_, taken := table.HostReported(host, []ids.WorkspaceID{"a"}, true)

	// Assert
	if taken || len(table.Snapshot()) != 0 {
		t.Fatalf("taken %v, snapshot %v; want the census refused", taken, table.Snapshot())
	}
}

func TestTheHostsFramesGoWithItsLastStream(t *testing.T) {
	tests := []struct {
		name    string
		streams int
		want    int
	}{
		{name: "its only stream ends", streams: 1, want: 0},
		{name: "one of two streams ends, a reattach still standing", streams: 2, want: 1},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			table := New(&counts{})
			for range tt.streams {
				table.HostAttached(host)
			}
			report(t, table, []ids.WorkspaceID{"a"}, false)

			// Act
			table.HostGone(host)

			// Assert
			if got := table.Snapshot(); len(got) != tt.want {
				t.Fatalf("snapshot = %v, want %d pages", got, tt.want)
			}
		})
	}
}

func TestAnotherDocumentsEndKeepsTheHostsFrames(t *testing.T) {
	// Arrange
	table := hosted()
	table.HostAttached("sidebar-1")
	report(t, table, []ids.WorkspaceID{"a"}, false)

	// Act
	table.HostGone("sidebar-1")

	// Assert
	if got := table.Snapshot(); len(got) != 1 {
		t.Fatalf("snapshot = %v, want the host's frame kept", got)
	}
}

func TestAHostThatEndsDropsItsPendingDisagreement(t *testing.T) {
	// Arrange
	table := hosted()
	report(t, table, []ids.WorkspaceID{"a"}, true)
	table.HostGone(host)
	table.HostAttached(host)

	// Act
	check := report(t, table, []ids.WorkspaceID{"a"}, true)

	// Assert
	if len(check.Began) != 0 {
		t.Fatalf("began %+v, want none: the fresh host's first tick", check.Began)
	}
}

func TestAStreamEndThatNoAttachPrecededPanics(t *testing.T) {
	// Arrange
	table := New(&counts{})
	defer func() {
		// Assert
		if recover() == nil {
			t.Fatal("HostGone with no stream standing did not panic")
		}
	}()

	// Act
	table.HostGone(host)
}
