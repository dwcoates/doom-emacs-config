package sched

import (
	"reflect"
	"strings"
	"testing"
)

func names(chunk []Item) []string {
	out := []string{}
	for _, it := range chunk {
		out = append(out, it.Name)
	}
	return out
}

func TestPack(t *testing.T) {
	tests := []struct {
		name  string
		items []Item
		k     int
		want  [][]string
	}{
		{
			name:  "longest first onto the lightest chunk",
			items: []Item{{"a", 5}, {"b", 4}, {"c", 3}, {"d", 2}},
			k:     2,
			want:  [][]string{{"a", "d"}, {"b", "c"}},
		},
		{
			name:  "more chunks than items is one item per chunk",
			items: []Item{{"a", 1}, {"b", 1}},
			k:     5,
			want:  [][]string{{"a"}, {"b"}},
		},
		{
			name:  "one chunk holds everything",
			items: []Item{{"b", 1}, {"a", 2}},
			k:     1,
			want:  [][]string{{"a", "b"}},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			chunks := Pack(tt.items, tt.k)

			// Assert
			var got [][]string
			for _, c := range chunks {
				got = append(got, names(c))
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("chunks = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestPackIntoZeroChunksPanics(t *testing.T) {
	defer func() {
		// Assert
		if recover() == nil {
			t.Fatal("Pack into zero chunks did not panic")
		}
	}()

	// Act
	Pack([]Item{{"a", 1}}, 0)
}

func TestPlanRunSplitsASuiteAcrossIdleSlots(t *testing.T) {
	// Arrange: 8 one-second items, cheap overhead, 4 slots.
	var items []Item
	for _, n := range []string{"a", "b", "c", "d", "e", "f", "g", "h"} {
		items = append(items, Item{n, 1})
	}

	// Act
	p, err := PlanRun(nil, []Chunkable{{Group: "ert", Suite: "ert", Items: items, Overhead: 0.1}}, 4)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if got := len(p.Chunks["ert"]); got != 4 {
		t.Fatalf("chunks = %d, want 4 (one per slot)", got)
	}
	if p.Makespan != 2.1 {
		t.Fatalf("makespan = %v, want 2.1", p.Makespan)
	}
}

func TestPlanRunKeepsOneChunkWhenSplittingSavesLessThanTheSlack(t *testing.T) {
	// Arrange: three chunks would finish 0.2s sooner (0.4%) for 100s more work.
	items := []Item{{"a", 0.1}, {"b", 0.1}, {"c", 0.1}}

	// Act
	p, err := PlanRun(nil, []Chunkable{{Group: "g", Suite: "s", Items: items, Overhead: 50}}, 8)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if got := len(p.Chunks["g"]); got != 1 {
		t.Fatalf("chunks = %d, want 1", got)
	}
}

func TestPlanRunFillsAroundALongAtomicUnit(t *testing.T) {
	// Arrange: a 10s atomic unit holds one of two slots for the whole run, so
	// the splittable suite gets one slot and splitting it buys nothing.
	atomic := []Unit{{ID: "long", Suite: "x", Est: 10}}
	items := []Item{{"a", 2}, {"b", 2}, {"c", 2}}

	// Act
	p, err := PlanRun(atomic, []Chunkable{{Group: "g", Suite: "s", Items: items, Overhead: 1}}, 2)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if p.Makespan != 10 {
		t.Fatalf("makespan = %v, want 10", p.Makespan)
	}
	if got := len(p.Chunks["g"]); got != 1 {
		t.Fatalf("chunks = %d, want 1: an equal makespan goes to less work", got)
	}
}

func TestPlanRunExpandsAGroupDependencyToEveryChunk(t *testing.T) {
	// Arrange
	atomic := []Unit{
		{ID: "build", Suite: "e2e", Est: 1},
		{ID: "report", Suite: "e2e", Est: 1, Deps: []string{"e2e"}},
	}
	items := []Item{{"a", 4}, {"b", 4}}

	// Act
	p, err := PlanRun(atomic, []Chunkable{{Group: "e2e", Suite: "e2e", Items: items, Overhead: 0, Deps: []string{"build"}}}, 2)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	byID := map[string]Unit{}
	for _, u := range p.Units {
		byID[u.ID] = u
	}
	if got, want := byID["report"].Deps, []string{"e2e#00", "e2e#01"}; !reflect.DeepEqual(got, want) {
		t.Fatalf("report deps = %v, want %v", got, want)
	}
	if got, want := byID["e2e#01"].Deps, []string{"build"}; !reflect.DeepEqual(got, want) {
		t.Fatalf("chunk deps = %v, want %v", got, want)
	}
	if p.Makespan != 6 {
		t.Fatalf("makespan = %v, want 6", p.Makespan)
	}
}

func TestPlanRunRefusesAnInvalidInput(t *testing.T) {
	tests := []struct {
		name       string
		atomic     []Unit
		chunkables []Chunkable
		slots      int
		want       string
	}{
		{"zero slots", nil, nil, 0, "at least one slot"},
		{"empty group", nil, []Chunkable{{Group: "g"}}, 2, "no items"},
		{"duplicate group", nil, []Chunkable{{Group: "g", Items: []Item{{"a", 1}}}, {Group: "g", Items: []Item{{"b", 1}}}}, 2, "duplicate splittable group"},
		{"hash in an atomic ID", []Unit{{ID: "a#1"}}, nil, 2, "may not contain"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			_, err := PlanRun(tt.atomic, tt.chunkables, tt.slots)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("PlanRun error = %v, want it to mention %q", err, tt.want)
			}
		})
	}
}
