package wsm

import (
	"context"
	"errors"
	"reflect"
	"testing"
)

func TestPutRevealGapWindowRoundTrips(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ctx := context.Background()
	want := RevealGapWindow{Model: "claude-opus-5", Kind: RevealKindProse, GapsMs: []int64{40, 0, 75}}

	// Act
	if err := s.PutRevealGapWindow(ctx, want); err != nil {
		t.Fatalf("PutRevealGapWindow: %v", err)
	}
	got, err := s.RevealGapWindows(ctx)

	// Assert
	if err != nil {
		t.Fatalf("RevealGapWindows: %v", err)
	}
	if !reflect.DeepEqual(got, []RevealGapWindow{want}) {
		t.Fatalf("RevealGapWindows() = %+v, want [%+v]", got, want)
	}
}

func TestPutRevealGapWindowReplacesTheStoredWindow(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ctx := context.Background()
	if err := s.PutRevealGapWindow(ctx, RevealGapWindow{Model: "m", Kind: RevealKindProse, GapsMs: []int64{1, 2, 3}}); err != nil {
		t.Fatalf("seed: %v", err)
	}

	// Act
	err := s.PutRevealGapWindow(ctx, RevealGapWindow{Model: "m", Kind: RevealKindProse, GapsMs: []int64{9}})

	// Assert
	if err != nil {
		t.Fatalf("PutRevealGapWindow: %v", err)
	}
	got, _ := s.RevealGapWindows(ctx)
	want := []RevealGapWindow{{Model: "m", Kind: RevealKindProse, GapsMs: []int64{9}}}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("RevealGapWindows() = %+v, want %+v", got, want)
	}
}

func TestRevealGapWindowsAreKeptPerModelAndKind(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ctx := context.Background()
	windows := []RevealGapWindow{
		{Model: "a", Kind: RevealKindProse, GapsMs: []int64{1}},
		{Model: "a", Kind: RevealKindThinking, GapsMs: []int64{2}},
		{Model: "b", Kind: RevealKindProse, GapsMs: []int64{3}},
	}

	// Act
	for _, w := range windows {
		if err := s.PutRevealGapWindow(ctx, w); err != nil {
			t.Fatalf("PutRevealGapWindow(%+v): %v", w, err)
		}
	}
	got, err := s.RevealGapWindows(ctx)

	// Assert
	if err != nil {
		t.Fatalf("RevealGapWindows: %v", err)
	}
	if !reflect.DeepEqual(got, windows) {
		t.Fatalf("RevealGapWindows() = %+v, want %+v", got, windows)
	}
}

func TestPutRevealGapWindowRefusesAnInvalidWindow(t *testing.T) {
	cases := []struct {
		name   string
		window RevealGapWindow
	}{
		{name: "no model", window: RevealGapWindow{Kind: RevealKindProse, GapsMs: []int64{1}}},
		{name: "unknown kind", window: RevealGapWindow{Model: "m", Kind: "tool", GapsMs: []int64{1}}},
		{name: "no gaps", window: RevealGapWindow{Model: "m", Kind: RevealKindProse}},
		{name: "a negative gap", window: RevealGapWindow{Model: "m", Kind: RevealKindProse, GapsMs: []int64{4, -1}}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)

			// Act
			err := s.PutRevealGapWindow(context.Background(), tc.window)

			// Assert
			if err == nil {
				t.Fatal("PutRevealGapWindow accepted an invalid window")
			}
			if !loggedOperation(log, "daemon.wsm.put_reveal_gap_window", "error") {
				t.Fatalf("no ERROR record: %v", log.Records())
			}
			if got, _ := s.RevealGapWindows(context.Background()); len(got) != 0 {
				t.Fatalf("RevealGapWindows() = %+v, want nothing stored", got)
			}
		})
	}
}

func TestRevealGapWindowsSurfacesACorruptRow(t *testing.T) {
	cases := []struct {
		name  string
		row   string
		field string
	}{
		{name: "unknown kind", row: `INSERT INTO reveal_burst_gaps VALUES ('m', 'tool', 0, 5)`, field: "kind"},
		{name: "negative gap", row: `INSERT INTO reveal_burst_gaps VALUES ('m', 'prose', 0, -5)`, field: "gap_ms"},
		{name: "a position gap", row: `INSERT INTO reveal_burst_gaps VALUES ('m', 'prose', 1, 5)`, field: "position"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			corrupt(t, s, tc.row)

			// Act
			got, err := s.RevealGapWindows(context.Background())

			// Assert
			var decode *DecodeError
			if !errors.As(err, &decode) || decode.Field != tc.field {
				t.Fatalf("RevealGapWindows() error = %v, want a DecodeError on %s", err, tc.field)
			}
			if got != nil {
				t.Fatalf("RevealGapWindows() = %+v, want nothing on a corrupt read", got)
			}
		})
	}
}

func TestRevealGapWindowsLeavesTheRetiredPerFragmentWindowsOut(t *testing.T) {
	// Arrange: a per-fragment window the build before layout 30 wrote.
	s, _ := testStore(t)
	corrupt(t, s, `INSERT INTO reveal_gaps VALUES ('m', 'prose', 0, 2)`)

	// Act
	got, err := s.RevealGapWindows(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("RevealGapWindows: %v", err)
	}
	if len(got) != 0 {
		t.Fatalf("RevealGapWindows() = %+v, want nothing from the retired reveal_gaps table", got)
	}
}

// TestTheMigrationAddsTheBurstGapWindowsBesideTheRetiredOnes pins the
// layout-30 step: the burst windows' table is new, and the per-fragment one
// the build before still writes is left in place.
func TestTheMigrationAddsTheBurstGapWindowsBesideTheRetiredOnes(t *testing.T) {
	// Arrange
	path := fixtureAt(t, 29)

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open on a layout-29 database: %v", err)
	}
	defer handle.Close()

	// Assert
	s := handle.(*store)
	for _, table := range []string{"reveal_burst_gaps", "reveal_gaps"} {
		if got := scalar[int](t, s, `SELECT count(*) FROM sqlite_master WHERE type = 'table' AND name = ?`, table); got != 1 {
			t.Fatalf("%s tables after the migration = %d, want 1", table, got)
		}
	}
}
