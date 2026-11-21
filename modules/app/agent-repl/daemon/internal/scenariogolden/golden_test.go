package scenariogolden

import (
	"encoding/json"
	"fmt"
	"os"
	"slices"
	"strings"
	"testing"
)

func TestMapJSONLeaves(t *testing.T) {
	tests := []struct {
		name string
		in   string
		drop []string
		want string
	}{
		{name: "every leaf is mapped with the key it sits under", in: `{"a":"x","b":{"c":"y"}}`, want: `{"a":"a=x","b":{"c":"c=y"}}`},
		{name: "an array's elements carry their array's key", in: `{"list":["x","y"]}`, want: `{"list":["list=x","list=y"]}`},
		{name: "a dropped field is removed, wherever it sits", in: `{"a":"x","gone":"y","b":{"gone":"z"}}`, drop: []string{"gone"}, want: `{"a":"a=x","b":{}}`},
		{name: "non-string leaves reach the mapper too", in: `{"n":1,"t":true}`, want: `{"n":"n=1","t":"t=true"}`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			var v any
			if err := json.Unmarshal([]byte(tt.in), &v); err != nil {
				t.Fatal(err)
			}
			drop := func(key string) bool { return slices.Contains(tt.drop, key) }

			// Act
			got := mapJSONLeaves(v, func(key string, leaf any) any { return key + "=" + fmt.Sprint(leaf) }, drop)

			// Assert
			raw, err := json.Marshal(got)
			if err != nil {
				t.Fatal(err)
			}
			if string(raw) != tt.want {
				t.Fatalf("mapJSONLeaves = %s, want %s", raw, tt.want)
			}
		})
	}
}

func TestMapJSONLeavesMintsTokensInSortedKeyOrder(t *testing.T) {
	// Arrange: one object decoded twice, whose map iteration order Go randomizes.
	minted := func() string {
		var v any
		if err := json.Unmarshal([]byte(`{"z":"a","m":"b","a":"c"}`), &v); err != nil {
			t.Fatal(err)
		}
		n := 0
		out := mapJSONLeaves(v, func(_ string, _ any) any { n++; return n }, nil)
		raw, _ := json.Marshal(out)
		return string(raw)
	}

	// Act / Assert
	if got := minted(); got != `{"a":1,"m":2,"z":3}` {
		t.Fatalf("tokens = %s, want them minted in sorted key order", got)
	}
}

func TestTheScenarioGoldensWalkJSONOnlyThroughMapJSONLeaves(t *testing.T) {
	// A hand-rolled walk drifts from the shared one's sorted order and drops;
	// only mapJSONLeaves may recurse over a decoded value here.
	src, err := os.ReadFile("golden.go")
	if err != nil {
		t.Fatal(err)
	}
	if n := strings.Count(string(src), "\n\tvar walk func("); n != 1 {
		t.Fatalf("golden.go declares %d JSON walks, want one, inside mapJSONLeaves", n)
	}
}

func TestWarningOperations(t *testing.T) {
	tests := []struct {
		name string
		ops  []string
		want []string
	}{
		{name: "none recorded is none", ops: nil, want: nil},
		{name: "repeats collapse and sort", ops: []string{"b", "a", "b"}, want: []string{"a", "b"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := WarningOperations(tc.ops)

			// Assert
			if strings.Join(got, ",") != strings.Join(tc.want, ",") || len(got) != len(tc.want) {
				t.Fatalf("WarningOperations(%v) = %v, want %v", tc.ops, got, tc.want)
			}
		})
	}
}
