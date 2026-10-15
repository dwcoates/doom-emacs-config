package redundancy

import (
	"math"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
)

// budgetCases: S1 and S2 each reach a block only they reach in system a;
// Big kills everything, so kills-only keeps only Big.
func budgetCases() ([]Case, map[string]int) {
	cases := []Case{
		{ID: "Big", Cost: 2 * ms, Kills: []string{"m1", "m2"}, Blocks: []string{"a/x.go:1.1,2.1"}},
		{ID: "S1", Cost: 1 * ms, Kills: []string{"m1"}, Blocks: []string{"a/x.go:1.1,2.1", "a/x.go:3.1,4.1"}},
		{ID: "S2", Cost: 2 * ms, Kills: []string{"m2"}, Blocks: []string{"a/x.go:5.1,6.1"}},
	}
	statements := map[string]int{
		"a/x.go:1.1,2.1": 10, "a/x.go:3.1,4.1": 4, "a/x.go:5.1,6.1": 3, "a/x.go:7.1,8.1": 3,
	}
	return cases, statements
}

func near(a, b float64) bool { return math.Abs(a-b) < 1e-9 }

func TestApplyBudget(t *testing.T) {
	tests := []struct {
		name         string
		budget       Budget
		wantRestored []string
		wantAfter    float64
		wantRef      string
	}{
		{
			name:      "a removal within the population's budget stands",
			budget:    Budget{MaxDrop: 40},
			wantAfter: 50, // 17 of 20 reached; S1 and S2 alone reached 7
			wantRef:   "measured population",
		},
		{
			name:         "over the budget the best statements per runtime go back first",
			budget:       Budget{MaxDrop: 20},
			wantRestored: []string{"S1"}, // 4 statements for 1ms beats 3 for 2ms
			wantAfter:    70,
			wantRef:      "measured population",
		},
		{
			name:         "until every system fits",
			budget:       Budget{MaxDrop: 5},
			wantRestored: []string{"S1", "S2"},
			wantAfter:    85,
			wantRef:      "measured population",
		},
		{
			name:         "a pinned baseline is the reference, and its floor the limit",
			budget:       Budget{MaxDrop: 5, Baseline: map[string]SystemBaseline{"a": {Percent: 92, Statements: 100, Floor: 87}}},
			wantRestored: []string{"S1"}, // -7 points is past 87; -3 is not
			wantAfter:    89,
			wantRef:      "baseline",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			cases, statements := budgetCases()
			r, err := Minimize(cases, KillsOnly, 0)
			if err != nil {
				t.Fatal(err)
			}

			// Act
			got, restored, systems, err := ApplyBudget(cases, r, statements, []string{"a"}, tt.budget, 0)

			// Assert
			if err != nil {
				t.Fatal(err)
			}
			if !reflect.DeepEqual(restored, tt.wantRestored) {
				t.Fatalf("restored %v, want %v", restored, tt.wantRestored)
			}
			if len(systems) != 1 || !near(systems[0].After, tt.wantAfter) || systems[0].Reference != tt.wantRef {
				t.Fatalf("systems = %+v, want after %.1f against the %s", systems, tt.wantAfter, tt.wantRef)
			}
			for _, id := range tt.wantRestored {
				for _, rm := range got.Removed {
					if rm.Case == id {
						t.Errorf("%s was restored yet is still listed removed", id)
					}
				}
			}
			if len(got.Kept)+len(got.Removed) != len(cases) {
				t.Errorf("kept %v and removed %v do not partition the cases", got.Kept, got.Removed)
			}
		})
	}
}

func TestApplyBudgetRefuses(t *testing.T) {
	tests := []struct {
		name       string
		statements func(map[string]int) map[string]int
		budget     Budget
		want       string
	}{
		{
			name:       "a measurement with no statement counts",
			statements: func(map[string]int) map[string]int { return nil },
			want:       "carries no statement counts",
		},
		{
			name: "a block the statement counts do not hold",
			statements: func(s map[string]int) map[string]int {
				delete(s, "a/x.go:5.1,6.1")
				return s
			},
			want: "reaches block a/x.go:5.1,6.1",
		},
		{
			name:       "a system already under its floor",
			statements: func(s map[string]int) map[string]int { return s },
			budget:     Budget{MaxDrop: 5, Baseline: map[string]SystemBaseline{"a": {Percent: 80, Statements: 100, Floor: 85}}},
			want:       "a already under the coverage floor",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			cases, statements := budgetCases()
			r, err := Minimize(cases, KillsOnly, 0)
			if err != nil {
				t.Fatal(err)
			}

			// Act
			_, _, _, err = ApplyBudget(cases, r, tt.statements(statements), []string{"a"}, tt.budget, 0)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("err = %v, want %q", err, tt.want)
			}
		})
	}
}

func TestSystemOfIsTheLongestModulePrefix(t *testing.T) {
	modules := []string{"agent-repld", "agentrepl/shim-store", "agentrepl/shim-store/sub"}
	tests := []struct {
		block, want, wantErr string
	}{
		{block: "agent-repld/internal/x/y.go:1.1,2.2", want: "agent-repld"},
		{block: "agentrepl/shim-store/internal/db/x.go:1.1,2.2", want: "agentrepl/shim-store"},
		{block: "agentrepl/shim-store/sub/x.go:1.1,2.2", want: "agentrepl/shim-store/sub"},
		{block: "agentrepl/shim-storex/x.go:1.1,2.2", wantErr: "belongs to none of the measured modules"},
	}
	for _, tt := range tests {
		got, err := SystemOf(tt.block, modules)
		if tt.wantErr != "" {
			if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
				t.Errorf("SystemOf(%s) err = %v, want %q", tt.block, err, tt.wantErr)
			}
			continue
		}
		if err != nil || got != tt.want {
			t.Errorf("SystemOf(%s) = %q, %v; want %q", tt.block, got, err, tt.want)
		}
	}
}

func TestReadBaseline(t *testing.T) {
	tests := []struct {
		name string
		body string
		want string
	}{
		{name: "a whole baseline", body: `{"Commit":"abc","Measured":"2026-10-14","Systems":{"agent-repld":{"Percent":91.5,"Statements":1000,"Floor":86.5}}}`},
		{name: "no commit", body: `{"Systems":{"a":{"Percent":1,"Statements":1,"Floor":0}}}`, want: "names no commit or no system"},
		{name: "no system", body: `{"Commit":"abc"}`, want: "names no commit or no system"},
		{name: "a floor above the percent", body: `{"Commit":"abc","Systems":{"a":{"Percent":50,"Statements":10,"Floor":60}}}`, want: "impossible entry for a"},
		{name: "no statements", body: `{"Commit":"abc","Systems":{"a":{"Percent":50,"Statements":0,"Floor":40}}}`, want: "impossible entry for a"},
		{name: "not JSON", body: `{`, want: "decode the coverage baseline"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			path := filepath.Join(t.TempDir(), "coverage-baseline.json")
			if err := os.WriteFile(path, []byte(tt.body), 0o644); err != nil {
				t.Fatal(err)
			}

			// Act
			got, err := ReadBaseline(path)

			// Assert
			if tt.want == "" {
				if err != nil || got.Systems["agent-repld"].Floor != 86.5 {
					t.Fatalf("ReadBaseline = %+v, %v", got, err)
				}
				return
			}
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("err = %v, want %q", err, tt.want)
			}
		})
	}
}

func TestReadBaselineRefusesAMissingFile(t *testing.T) {
	// Act
	_, err := ReadBaseline(filepath.Join(t.TempDir(), "gone.json"))

	// Assert
	if err == nil || !strings.Contains(err.Error(), "read the coverage baseline") {
		t.Fatalf("err = %v", err)
	}
}

func TestApplyBudgetHoldsARequestTighterThanTheBaselineFloor(t *testing.T) {
	// Arrange: the pinned floor allows 5 points; the request allows 1.
	cases, statements := budgetCases()
	r, err := Minimize(cases, KillsOnly, 0)
	if err != nil {
		t.Fatal(err)
	}
	b := Budget{MaxDrop: 1, Baseline: map[string]SystemBaseline{"a": {Percent: 92, Statements: 100, Floor: 87}}}

	// Act
	_, restored, systems, err := ApplyBudget(cases, r, statements, []string{"a"}, b, 0)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if !reflect.DeepEqual(restored, []string{"S1", "S2"}) || !near(systems[0].Floor, 91) {
		t.Fatalf("restored %v, floor %.1f; want both back against a floor of 91", restored, systems[0].Floor)
	}
}
