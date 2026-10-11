package wsm

import (
	"context"
	"errors"
	"math"
	"reflect"
	"testing"
)

func TestPutRevealSpeedWindowRoundTrips(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ctx := context.Background()
	want := RevealSpeedWindow{Model: "claude-opus-5", Kind: RevealKindProse, CharsPerSecond: []float64{40.5, 130, 75.25}}

	// Act
	if err := s.PutRevealSpeedWindow(ctx, want); err != nil {
		t.Fatalf("PutRevealSpeedWindow: %v", err)
	}
	got, err := s.RevealSpeedWindows(ctx)

	// Assert
	if err != nil {
		t.Fatalf("RevealSpeedWindows: %v", err)
	}
	if !reflect.DeepEqual(got, []RevealSpeedWindow{want}) {
		t.Fatalf("RevealSpeedWindows() = %+v, want [%+v]", got, want)
	}
}

func TestPutRevealSpeedWindowReplacesTheStoredWindow(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ctx := context.Background()
	if err := s.PutRevealSpeedWindow(ctx, RevealSpeedWindow{Model: "m", Kind: RevealKindProse, CharsPerSecond: []float64{1, 2, 3}}); err != nil {
		t.Fatalf("seed: %v", err)
	}

	// Act
	err := s.PutRevealSpeedWindow(ctx, RevealSpeedWindow{Model: "m", Kind: RevealKindProse, CharsPerSecond: []float64{9}})

	// Assert
	if err != nil {
		t.Fatalf("PutRevealSpeedWindow: %v", err)
	}
	got, _ := s.RevealSpeedWindows(ctx)
	want := []RevealSpeedWindow{{Model: "m", Kind: RevealKindProse, CharsPerSecond: []float64{9}}}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("RevealSpeedWindows() = %+v, want %+v", got, want)
	}
}

func TestRevealSpeedWindowsAreKeptPerModelAndKind(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ctx := context.Background()
	windows := []RevealSpeedWindow{
		{Model: "a", Kind: RevealKindProse, CharsPerSecond: []float64{1}},
		{Model: "a", Kind: RevealKindThinking, CharsPerSecond: []float64{2}},
		{Model: "b", Kind: RevealKindProse, CharsPerSecond: []float64{3}},
	}

	// Act
	for _, w := range windows {
		if err := s.PutRevealSpeedWindow(ctx, w); err != nil {
			t.Fatalf("PutRevealSpeedWindow(%+v): %v", w, err)
		}
	}
	got, err := s.RevealSpeedWindows(ctx)

	// Assert
	if err != nil {
		t.Fatalf("RevealSpeedWindows: %v", err)
	}
	if !reflect.DeepEqual(got, windows) {
		t.Fatalf("RevealSpeedWindows() = %+v, want %+v", got, windows)
	}
}

func TestPutRevealSpeedWindowRefusesAnInvalidWindow(t *testing.T) {
	cases := []struct {
		name   string
		window RevealSpeedWindow
	}{
		{name: "no model", window: RevealSpeedWindow{Kind: RevealKindProse, CharsPerSecond: []float64{1}}},
		{name: "unknown kind", window: RevealSpeedWindow{Model: "m", Kind: "tool", CharsPerSecond: []float64{1}}},
		{name: "no speeds", window: RevealSpeedWindow{Model: "m", Kind: RevealKindProse}},
		{name: "a zero speed", window: RevealSpeedWindow{Model: "m", Kind: RevealKindProse, CharsPerSecond: []float64{4, 0}}},
		{name: "a negative speed", window: RevealSpeedWindow{Model: "m", Kind: RevealKindProse, CharsPerSecond: []float64{4, -1}}},
		{name: "an infinite speed", window: RevealSpeedWindow{Model: "m", Kind: RevealKindProse, CharsPerSecond: []float64{math.Inf(1)}}},
		{name: "a NaN speed", window: RevealSpeedWindow{Model: "m", Kind: RevealKindProse, CharsPerSecond: []float64{math.NaN()}}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)

			// Act
			err := s.PutRevealSpeedWindow(context.Background(), tc.window)

			// Assert
			if err == nil {
				t.Fatal("PutRevealSpeedWindow accepted an invalid window")
			}
			if !loggedOperation(log, "daemon.wsm.put_reveal_speed_window", "error") {
				t.Fatalf("no ERROR record: %v", log.Records())
			}
			if got, _ := s.RevealSpeedWindows(context.Background()); len(got) != 0 {
				t.Fatalf("RevealSpeedWindows() = %+v, want nothing stored", got)
			}
		})
	}
}

func TestRevealSpeedWindowsSurfacesACorruptRow(t *testing.T) {
	cases := []struct {
		name  string
		row   string
		field string
	}{
		{name: "unknown kind", row: `INSERT INTO reveal_block_speeds VALUES ('m', 'tool', 0, 5)`, field: "kind"},
		{name: "zero speed", row: `INSERT INTO reveal_block_speeds VALUES ('m', 'prose', 0, 0)`, field: "chars_per_second"},
		{name: "negative speed", row: `INSERT INTO reveal_block_speeds VALUES ('m', 'prose', 0, -5)`, field: "chars_per_second"},
		{name: "a position gap", row: `INSERT INTO reveal_block_speeds VALUES ('m', 'prose', 1, 5)`, field: "position"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			corrupt(t, s, tc.row)

			// Act
			got, err := s.RevealSpeedWindows(context.Background())

			// Assert
			var decode *DecodeError
			if !errors.As(err, &decode) || decode.Field != tc.field {
				t.Fatalf("RevealSpeedWindows() error = %v, want a DecodeError on %s", err, tc.field)
			}
			if got != nil {
				t.Fatalf("RevealSpeedWindows() = %+v, want nothing on a corrupt read", got)
			}
		})
	}
}

func TestRevealSpeedWindowsLeavesTheRetiredGapWindowsOut(t *testing.T) {
	cases := []struct {
		name string
		row  string
	}{
		{name: "per-fragment gaps", row: `INSERT INTO reveal_gaps VALUES ('m', 'prose', 0, 2)`},
		{name: "burst gaps", row: `INSERT INTO reveal_burst_gaps VALUES ('m', 'prose', 0, 150)`},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: a gap window an earlier build wrote.
			s, _ := testStore(t)
			corrupt(t, s, tc.row)

			// Act
			got, err := s.RevealSpeedWindows(context.Background())

			// Assert
			if err != nil {
				t.Fatalf("RevealSpeedWindows: %v", err)
			}
			if len(got) != 0 {
				t.Fatalf("RevealSpeedWindows() = %+v, want nothing from a retired gap table", got)
			}
		})
	}
}

// TestTheMigrationAddsTheBlockSpeedWindowsBesideTheRetiredOnes pins the
// layout-31 step: the block speeds' table is new, and the burst-gap one the
// build before still writes is left in place.
func TestTheMigrationAddsTheBlockSpeedWindowsBesideTheRetiredOnes(t *testing.T) {
	// Arrange
	path := fixtureAt(t, 30)

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open on a layout-30 database: %v", err)
	}
	defer handle.Close()

	// Assert
	s := handle.(*store)
	for _, table := range []string{"reveal_block_speeds", "reveal_burst_gaps", "reveal_gaps"} {
		if got := scalar[int](t, s, `SELECT count(*) FROM sqlite_master WHERE type = 'table' AND name = ?`, table); got != 1 {
			t.Fatalf("%s tables after the migration = %d, want 1", table, got)
		}
	}
}

func TestValidRevealSpeed(t *testing.T) {
	cases := []struct {
		name  string
		speed float64
		want  bool
	}{
		{name: "a positive speed", speed: 45.5, want: true},
		{name: "zero", speed: 0, want: false},
		{name: "negative", speed: -1, want: false},
		{name: "infinite", speed: math.Inf(1), want: false},
		{name: "NaN", speed: math.NaN(), want: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := ValidRevealSpeed(tc.speed)

			// Assert
			if got != tc.want {
				t.Fatalf("ValidRevealSpeed(%v) = %v, want %v", tc.speed, got, tc.want)
			}
		})
	}
}
