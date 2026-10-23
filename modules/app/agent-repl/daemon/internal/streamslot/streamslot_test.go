package streamslot_test

import (
	"os"
	"strings"
	"testing"

	"agent-repld/internal/streamslot"
)

func TestASlotHoldsNothingUntilAStreamAttaches(t *testing.T) {
	var s streamslot.Slot[int]
	if v, ok := s.Get(); ok || v != 0 {
		t.Fatalf("Get() = %v, %v; want nothing standing", v, ok)
	}
}

func TestAnAttachedValueStandsUntilItsStreamReleases(t *testing.T) {
	// Arrange
	var s streamslot.Slot[int]
	gen, superseded := s.Attach(42)

	// Act / Assert
	if v, ok := s.Get(); !ok || v != 42 || superseded {
		t.Fatalf("after Attach: Get() = %v, %v, superseded %v; want 42 standing", v, ok, superseded)
	}
	if !s.Release(gen) {
		t.Fatal("the current stream's release was not current")
	}
	if v, ok := s.Get(); ok || v != 0 {
		t.Fatalf("after Release: Get() = %v, %v; want nothing standing", v, ok)
	}
}

func TestASupersededStreamsReleaseLeavesItsSuccessorStanding(t *testing.T) {
	// Arrange
	var s streamslot.Slot[int]
	first, _ := s.Attach(1)
	_, superseded := s.Attach(2)

	// Act
	current := s.Release(first)

	// Assert
	if current || !superseded {
		t.Fatalf("Release(first) current = %v, superseded = %v; want a stale release and a superseding attach", current, superseded)
	}
	if v, ok := s.Get(); !ok || v != 2 {
		t.Fatalf("Get() = %v, %v; want the successor's 2", v, ok)
	}
}

func TestUpdateMovesOnlyAStandingValue(t *testing.T) {
	tests := []struct {
		name     string
		attach   bool
		wantOK   bool
		wantVal  int
		wantGen  uint64
		wantPrev int
		released bool
	}{
		{name: "a standing stream", attach: true, wantOK: true, wantVal: 7, wantGen: 1, wantPrev: 3},
		{name: "no stream ever", attach: false, wantOK: false, wantVal: 0, wantGen: 0, wantPrev: 0},
		{name: "a released stream", attach: true, released: true, wantOK: false, wantVal: 0, wantGen: 1, wantPrev: 0},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			var s streamslot.Slot[int]
			if tt.attach {
				gen, _ := s.Attach(3)
				if tt.released {
					s.Release(gen)
				}
			}

			// Act
			previous, gen, attached := s.Update(7)

			// Assert
			v, ok := s.Get()
			if attached != tt.wantOK || ok != tt.wantOK || v != tt.wantVal || gen != tt.wantGen || previous != tt.wantPrev {
				t.Fatalf("Update = (%d, %d, %v), Get = (%d, %v); want (%d, %d, %v), (%d, %v)", previous, gen, attached, v, ok, tt.wantPrev, tt.wantGen, tt.wantOK, tt.wantVal, tt.wantOK)
			}
		})
	}
}

// TestEveryStreamScopedHolderIsBuiltOnSlot pins the holders that keep a value
// for one standing stream to the shared Slot: a holder that counted its own
// generations would drift from the superseding rule.
func TestEveryStreamScopedHolderIsBuiltOnSlot(t *testing.T) {
	for _, file := range []string{"../desktopnotify/focus.go", "../procmetrics/emacs.go"} {
		t.Run(file, func(t *testing.T) {
			src, err := os.ReadFile(file)
			if err != nil {
				t.Fatal(err)
			}
			if !strings.Contains(string(src), "streamslot.Slot[") || strings.Contains(string(src), "generation++") {
				t.Fatalf("%s holds a stream-scoped value without streamslot.Slot", file)
			}
		})
	}
}
