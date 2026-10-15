package redundancy

import (
	"math/rand"
	"reflect"
	"slices"
	"strconv"
	"strings"
	"testing"
	"time"
)

const ms = time.Millisecond

// caught is every element (by universe) the named cases catch between them.
func caught(cases []Case, u Universe, ids []string) map[string]bool {
	out := map[string]bool{}
	for _, c := range cases {
		if !slices.Contains(ids, c.ID) {
			continue
		}
		for _, k := range c.Kills {
			out["m:"+k] = true
		}
		if u == Strict {
			for _, b := range c.Blocks {
				out["b:"+b] = true
			}
		}
	}
	return out
}

func allIDs(cases []Case) []string {
	var ids []string
	for _, c := range cases {
		ids = append(ids, c.ID)
	}
	return ids
}

func removedIDs(r Result) []string {
	var ids []string
	for _, rm := range r.Removed {
		ids = append(ids, rm.Case)
	}
	slices.Sort(ids)
	return ids
}

func TestMinimize(t *testing.T) {
	tests := []struct {
		name        string
		cases       []Case
		universe    Universe
		wantRemoved []string
		// wantBy maps a removed case to the kept cases it names.
		wantBy     map[string][]string
		wantSingle map[string]string
	}{
		{
			name: "a case whose kills and blocks another case holds is removed, dominated by it",
			cases: []Case{
				{ID: "Big", Cost: 5 * ms, Kills: []string{"m1", "m2"}, Blocks: []string{"b1", "b2"}},
				{ID: "Small", Cost: 5 * ms, Kills: []string{"m1"}, Blocks: []string{"b1"}},
			},
			wantRemoved: []string{"Small"},
			wantSingle:  map[string]string{"Small": "Big"},
		},
		{
			name: "a case two kept cases cover between them names both and no single dominator",
			cases: []Case{
				{ID: "A", Cost: 1 * ms, Kills: []string{"m1", "m3"}},
				{ID: "B", Cost: 1 * ms, Kills: []string{"m2", "m4"}},
				{ID: "AB", Cost: 9 * ms, Kills: []string{"m1", "m2"}},
			},
			wantRemoved: []string{"AB"},
			wantBy:      map[string][]string{"AB": {"A", "B"}},
			wantSingle:  map[string]string{"AB": ""},
		},
		{
			name: "a pinned case stays though another case dominates it",
			cases: []Case{
				{ID: "Big", Cost: 1 * ms, Kills: []string{"m1", "m2"}},
				{ID: "Logs", Cost: 1 * ms, Kills: []string{"m1"}, Pinned: "asserts a log record"},
			},
			wantRemoved: nil,
		},
		{
			name: "the only case killing a mutant stays however dear it is",
			cases: []Case{
				{ID: "Dear", Cost: 900 * ms, Kills: []string{"m1", "m2"}},
				{ID: "Cheap", Cost: 1 * ms, Kills: []string{"m1"}},
			},
			wantRemoved: []string{"Cheap"},
		},
		{
			name: "under strict a case reaching a block no other case reaches stays",
			cases: []Case{
				{ID: "Big", Cost: 1 * ms, Kills: []string{"m1"}, Blocks: []string{"b1"}},
				{ID: "Line", Cost: 1 * ms, Kills: []string{"m1"}, Blocks: []string{"b2"}},
			},
			universe:    Strict,
			wantRemoved: nil,
		},
		{
			name: "under kills-only the same case goes",
			cases: []Case{
				{ID: "Big", Cost: 1 * ms, Kills: []string{"m1"}, Blocks: []string{"b1"}},
				{ID: "Line", Cost: 2 * ms, Kills: []string{"m1"}, Blocks: []string{"b2"}},
			},
			universe:    KillsOnly,
			wantRemoved: []string{"Line"},
		},
		{
			name: "two cheap cases replace one dear case covering the same mutants",
			cases: []Case{
				{ID: "Dear", Cost: 100 * ms, Kills: []string{"m1", "m2"}},
				{ID: "C1", Cost: 1 * ms, Kills: []string{"m1"}},
				{ID: "C2", Cost: 1 * ms, Kills: []string{"m2"}},
			},
			wantRemoved: []string{"Dear"},
			wantBy:      map[string][]string{"Dear": {"C1", "C2"}},
		},
		{
			name: "of two identical cases at one cost exactly the earlier stays",
			cases: []Case{
				{ID: "First", Cost: 1 * ms, Kills: []string{"m1"}},
				{ID: "Second", Cost: 1 * ms, Kills: []string{"m1"}},
			},
			wantRemoved: []string{"Second"},
			wantSingle:  map[string]string{"Second": "First"},
		},
		{
			name: "a case that kills and reaches nothing is removed with nothing named",
			cases: []Case{
				{ID: "Real", Cost: 1 * ms, Kills: []string{"m1"}},
				{ID: "Empty", Cost: 1 * ms},
			},
			wantRemoved: []string{"Empty"},
			wantSingle:  map[string]string{"Empty": "Real"},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the table.

			// Act
			got, err := Minimize(tt.cases, tt.universe, 0)

			// Assert
			if err != nil {
				t.Fatal(err)
			}
			if r := removedIDs(got); !reflect.DeepEqual(r, tt.wantRemoved) {
				t.Fatalf("removed = %v, want %v", r, tt.wantRemoved)
			}
			for _, rm := range got.Removed {
				if want, ok := tt.wantBy[rm.Case]; ok && !reflect.DeepEqual(sorted(rm.CoveredBy), want) {
					t.Errorf("%s covered by %v, want %v", rm.Case, rm.CoveredBy, want)
				}
				if want, ok := tt.wantSingle[rm.Case]; ok && rm.DominatedBy != want {
					t.Errorf("%s dominated by %q, want %q", rm.Case, rm.DominatedBy, want)
				}
			}
			if !got.Optimal {
				t.Errorf("Optimal = false on a %d-case problem", len(tt.cases))
			}
			if !reflect.DeepEqual(caught(tt.cases, tt.universe, got.Kept), caught(tt.cases, tt.universe, allIDs(tt.cases))) {
				t.Errorf("kept %v catch less than the whole set", got.Kept)
			}
		})
	}
}

func sorted(s []string) []string {
	s = slices.Clone(s)
	slices.Sort(s)
	return s
}

// TestMinimizeFindsTheCheapestCover holds the search to a brute force over
// every subset, on seeded random problems small enough to enumerate.
func TestMinimizeFindsTheCheapestCover(t *testing.T) {
	// Arrange
	rng := rand.New(rand.NewSource(20261014))
	for trial := 0; trial < 200; trial++ {
		n := 2 + rng.Intn(9)
		var cases []Case
		for i := 0; i < n; i++ {
			c := Case{ID: "T" + strconv.Itoa(i), Cost: time.Duration(1+rng.Intn(20)) * ms}
			for m := 0; m < 8; m++ {
				if rng.Intn(3) == 0 {
					c.Kills = append(c.Kills, "m"+strconv.Itoa(m))
				}
			}
			cases = append(cases, c)
		}
		want := bruteForceCost(cases)

		// Act
		got, err := Minimize(cases, KillsOnly, 0)

		// Assert
		if err != nil {
			t.Fatal(err)
		}
		if got.KeptCost != want {
			t.Fatalf("trial %d: kept cost %v, brute force %v; cases %+v", trial, got.KeptCost, want, cases)
		}
		if !reflect.DeepEqual(caught(cases, KillsOnly, got.Kept), caught(cases, KillsOnly, allIDs(cases))) {
			t.Fatalf("trial %d: kept %v catch less than the whole set", trial, got.Kept)
		}
	}
}

func bruteForceCost(cases []Case) time.Duration {
	all := caught(cases, KillsOnly, allIDs(cases))
	best := time.Duration(-1)
	for mask := 0; mask < 1<<len(cases); mask++ {
		var ids []string
		var cost time.Duration
		for i, c := range cases {
			if mask&(1<<i) != 0 {
				ids = append(ids, c.ID)
				cost += c.Cost
			}
		}
		if (best < 0 || cost < best) && reflect.DeepEqual(caught(cases, KillsOnly, ids), all) {
			best = cost
		}
	}
	return best
}

func TestMinimizeOverheadPrefersFewerCasesAtEqualRuntime(t *testing.T) {
	// Arrange: one case or two zero-cost cases catch the same mutants.
	cases := []Case{
		{ID: "One", Cost: 0, Kills: []string{"m1", "m2"}},
		{ID: "Half1", Cost: 0, Kills: []string{"m1", "m3"}},
		{ID: "Half2", Cost: 0, Kills: []string{"m2", "m3"}},
	}

	// Act
	got, err := Minimize(cases, KillsOnly, ms)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if len(got.Kept) != 2 {
		t.Fatalf("kept %v, want two cases (One and a half)", got.Kept)
	}
}

func TestMinimizeOutOfBudgetIsNotOptimalButStillCovers(t *testing.T) {
	// Arrange: no case is essential or dominated, so the search must branch.
	cases := []Case{
		{ID: "A", Cost: 1 * ms, Kills: []string{"m1", "m2"}},
		{ID: "B", Cost: 1 * ms, Kills: []string{"m2", "m3"}},
		{ID: "C", Cost: 1 * ms, Kills: []string{"m3", "m1"}},
	}

	// Act
	got, err := minimize(cases, KillsOnly, 0, 0)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if got.Optimal {
		t.Error("Optimal = true with a search budget of zero")
	}
	if !reflect.DeepEqual(caught(cases, KillsOnly, got.Kept), caught(cases, KillsOnly, allIDs(cases))) {
		t.Errorf("kept %v catch less than the whole set", got.Kept)
	}
}

func TestMinimizeRefusesACaseMeasuredTwice(t *testing.T) {
	// Arrange
	cases := []Case{{ID: "A", Kills: []string{"m1"}}, {ID: "A", Kills: []string{"m2"}}}

	// Act
	_, err := Minimize(cases, Strict, 0)

	// Assert
	if err == nil || !strings.Contains(err.Error(), `"A" is measured twice`) {
		t.Fatalf("err = %v, want the duplicate named", err)
	}
}

func TestMinimizeListsTheRemovedByWhatTheySave(t *testing.T) {
	// Arrange
	cases := []Case{
		{ID: "Keep", Cost: 1 * ms, Kills: []string{"m1"}},
		{ID: "Slow", Cost: 30 * ms, Kills: []string{"m1"}},
		{ID: "Slower", Cost: 50 * ms, Kills: []string{"m1"}},
	}

	// Act
	got, err := Minimize(cases, KillsOnly, 0)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	var order []string
	for _, r := range got.Removed {
		order = append(order, r.Case)
	}
	if !reflect.DeepEqual(order, []string{"Slower", "Slow"}) {
		t.Fatalf("removed in order %v, want the dearest first", order)
	}
}

func TestParseUniverse(t *testing.T) {
	tests := []struct {
		in      string
		want    Universe
		wantErr bool
	}{
		{in: "strict", want: Strict},
		{in: "kills-only", want: KillsOnly},
		{in: "lines", wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.in, func(t *testing.T) {
			// Act
			got, err := ParseUniverse(tt.in)

			// Assert
			if (err != nil) != tt.wantErr {
				t.Fatalf("err = %v, wantErr %v", err, tt.wantErr)
			}
			if !tt.wantErr && (got != tt.want || got.String() != tt.in) {
				t.Fatalf("ParseUniverse(%q) = %v, want %v", tt.in, got, tt.want)
			}
		})
	}
}
