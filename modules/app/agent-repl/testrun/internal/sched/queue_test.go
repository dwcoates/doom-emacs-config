package sched

import (
	"reflect"
	"strings"
	"testing"
)

func ids(units []Unit) []string {
	out := []string{}
	for _, u := range units {
		out = append(out, u.ID)
	}
	return out
}

func drainOrder(t *testing.T, q *Queue) []string {
	t.Helper()
	var order []string
	for {
		u, ok := q.Next()
		if !ok {
			break
		}
		order = append(order, u.ID)
		q.Done(u.ID, true)
	}
	return order
}

func TestNewQueueRefusesAnInvalidDAG(t *testing.T) {
	tests := []struct {
		name  string
		units []Unit
		want  string
	}{
		{"empty id", []Unit{{Suite: "s"}}, "has no ID"},
		{"duplicate id", []Unit{{ID: "a"}, {ID: "a"}}, "duplicate unit ID"},
		{"negative estimate", []Unit{{ID: "a", Est: -1}}, "negative estimate"},
		{"unknown dependency", []Unit{{ID: "a", Deps: []string{"b"}}}, "unknown unit"},
		{"cycle", []Unit{{ID: "a", Deps: []string{"b"}}, {ID: "b", Deps: []string{"a"}}}, "cycle"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			_, err := NewQueue(tt.units)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("NewQueue error = %v, want it to mention %q", err, tt.want)
			}
		})
	}
}

func TestQueueOrder(t *testing.T) {
	tests := []struct {
		name  string
		units []Unit
		want  []string
	}{
		{
			name:  "independent units run longest first",
			units: []Unit{{ID: "short", Est: 1}, {ID: "long", Est: 9}, {ID: "mid", Est: 5}},
			want:  []string{"long", "mid", "short"},
		},
		{
			name:  "equal estimates break ties on ID",
			units: []Unit{{ID: "b", Est: 2}, {ID: "a", Est: 2}},
			want:  []string{"a", "b"},
		},
		{
			name: "a build unblocking a long chain beats a longer leaf",
			units: []Unit{
				{ID: "leaf", Est: 5},
				{ID: "build", Est: 2},
				{ID: "after", Est: 10, Deps: []string{"build"}},
			},
			want: []string{"build", "after", "leaf"},
		},
		{
			name: "a dependent waits for every dependency",
			units: []Unit{
				{ID: "merge", Est: 1, Deps: []string{"x", "y"}},
				{ID: "x", Est: 3},
				{ID: "y", Est: 2},
			},
			want: []string{"x", "y", "merge"},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			q, err := NewQueue(tt.units)
			if err != nil {
				t.Fatal(err)
			}

			// Act
			got := drainOrder(t, q)

			// Assert
			if !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("order = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestQueueDependentIsNotReadyUntilItsDependencyFinishes(t *testing.T) {
	// Arrange
	q, err := NewQueue([]Unit{{ID: "build", Est: 1}, {ID: "test", Est: 1, Deps: []string{"build"}}})
	if err != nil {
		t.Fatal(err)
	}
	q.Next()

	// Act
	_, ok := q.Next()

	// Assert
	if ok {
		t.Fatal("a unit was handed out while its dependency was still running")
	}
}

func TestQueueFailureCancelsEverythingDownstream(t *testing.T) {
	// Arrange
	q, err := NewQueue([]Unit{
		{ID: "build", Est: 3},
		{ID: "chunk", Est: 2, Deps: []string{"build"}},
		{ID: "report", Est: 1, Deps: []string{"chunk"}},
		{ID: "other", Est: 1},
	})
	if err != nil {
		t.Fatal(err)
	}
	u, _ := q.Next()

	// Act
	cancelled := q.Done(u.ID, false)

	// Assert
	if got, want := ids(cancelled), []string{"chunk", "report"}; !reflect.DeepEqual(got, want) {
		t.Fatalf("cancelled = %v, want %v", got, want)
	}
	if next, _ := q.Next(); next.ID != "other" {
		t.Fatalf("next = %q, want the unrelated unit", next.ID)
	}
	q.Done("other", true)
	if p := q.Pending(); p != 0 {
		t.Fatalf("pending = %d after every runnable unit finished", p)
	}
}

func TestQueueDoneOnAUnitThatIsNotRunningPanics(t *testing.T) {
	// Arrange
	q, err := NewQueue([]Unit{{ID: "a"}})
	if err != nil {
		t.Fatal(err)
	}
	defer func() {
		// Assert
		if recover() == nil {
			t.Fatal("Done on an unstarted unit did not panic")
		}
	}()

	// Act
	q.Done("a", true)
}

func TestSimulate(t *testing.T) {
	tests := []struct {
		name  string
		units []Unit
		slots int
		want  float64
	}{
		{"one slot sums everything", []Unit{{ID: "a", Est: 2}, {ID: "b", Est: 3}}, 1, 5},
		{"enough slots is the longest unit", []Unit{{ID: "a", Est: 2}, {ID: "b", Est: 3}}, 4, 3},
		{"LPT packs short units behind long ones", []Unit{{ID: "a", Est: 4}, {ID: "b", Est: 2}, {ID: "c", Est: 2}}, 2, 4},
		{"a chain is serial whatever the slots", []Unit{{ID: "a", Est: 2}, {ID: "b", Est: 3, Deps: []string{"a"}}}, 8, 5},
		{"no units is zero", nil, 3, 0},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := Simulate(tt.units, tt.slots)

			// Assert
			if err != nil {
				t.Fatal(err)
			}
			if got != tt.want {
				t.Fatalf("makespan = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestSimulateRefusesZeroSlots(t *testing.T) {
	// Act
	_, err := Simulate([]Unit{{ID: "a"}}, 0)

	// Assert
	if err == nil {
		t.Fatal("Simulate accepted zero slots")
	}
}
