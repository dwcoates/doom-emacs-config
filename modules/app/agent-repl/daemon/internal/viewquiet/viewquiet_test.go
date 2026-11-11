package viewquiet

import (
	"reflect"
	"testing"
)

type countingTelemetry struct{ skipped []string }

func (c *countingTelemetry) CountViewRenderSkipped(view string) { c.skipped = append(c.skipped, view) }

func TestSetMovesTheGateAndTellsFollowersInOrder(t *testing.T) {
	tests := []struct {
		name      string
		moves     []bool
		wantMoved []bool
		wantHeard []string
		wantQuiet bool
	}{
		{name: "entering", moves: []bool{true}, wantMoved: []bool{true}, wantHeard: []string{"a:true", "b:true"}, wantQuiet: true},
		{name: "entering then leaving", moves: []bool{true, false}, wantMoved: []bool{true, true},
			wantHeard: []string{"a:true", "b:true", "a:false", "b:false"}, wantQuiet: false},
		{name: "a repeated state is no move", moves: []bool{true, true}, wantMoved: []bool{true, false},
			wantHeard: []string{"a:true", "b:true"}, wantQuiet: true},
		{name: "leaving a gate never entered is no move", moves: []bool{false}, wantMoved: []bool{false}, wantHeard: nil, wantQuiet: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			g := New(nil)
			var heard []string
			for _, view := range []string{"a", "b"} {
				g.Follow(view, func(quiet bool) {
					// The gate reads the new state by the time a follower hears it.
					if g.Quiet() != quiet {
						t.Errorf("follower %s heard %v while the gate read %v", view, quiet, g.Quiet())
					}
					heard = append(heard, view+":"+map[bool]string{true: "true", false: "false"}[quiet])
				})
			}

			// Act
			var moved []bool
			for _, q := range tt.moves {
				moved = append(moved, g.Set(q))
			}

			// Assert
			if !reflect.DeepEqual(moved, tt.wantMoved) {
				t.Errorf("Set answered %v, want %v", moved, tt.wantMoved)
			}
			if !reflect.DeepEqual(heard, tt.wantHeard) {
				t.Errorf("followers heard %v, want %v", heard, tt.wantHeard)
			}
			if g.Quiet() != tt.wantQuiet {
				t.Errorf("Quiet() = %v, want %v", g.Quiet(), tt.wantQuiet)
			}
		})
	}
}

func TestANilGateIsNeverQuietAndCountsNothing(t *testing.T) {
	// Arrange
	var g *Gate

	// Act
	g.Follow("sidebar", func(bool) { t.Fatal("a nil gate told a follower") })
	g.Skipped("sidebar")

	// Assert
	if g.Quiet() {
		t.Fatal("a nil gate reads quiet")
	}
}

func TestSkippedCountsByView(t *testing.T) {
	tests := []struct {
		name string
		tel  *countingTelemetry
		want []string
	}{
		{name: "counted on the telemetry", tel: &countingTelemetry{}, want: []string{"footer", "feed"}},
		{name: "no telemetry counts nothing", tel: nil, want: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			var g *Gate
			if tt.tel != nil {
				g = New(tt.tel)
			} else {
				g = New(nil)
			}

			// Act
			g.Skipped("footer")
			g.Skipped("feed")

			// Assert
			if tt.tel != nil && !reflect.DeepEqual(tt.tel.skipped, tt.want) {
				t.Fatalf("counted %v, want %v", tt.tel.skipped, tt.want)
			}
		})
	}
}

func TestFollowRefusesAnUnnamedFollower(t *testing.T) {
	tests := []struct {
		name string
		view string
		fn   func(bool)
	}{
		{name: "no view name", view: "", fn: func(bool) {}},
		{name: "no function", view: "sidebar", fn: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			g := New(nil)
			defer func() {
				if recover() == nil {
					t.Fatal("Follow accepted a follower it cannot name or call")
				}
			}()

			// Act
			g.Follow(tt.view, tt.fn)
		})
	}
}
