package gomeasure

import (
	"reflect"
	"regexp"
	"testing"
	"time"
)

func lines(texts ...string) []Line {
	out := make([]Line, len(texts))
	for i, t := range texts {
		out[i] = Line{At: time.Duration(i) * time.Millisecond, Text: t}
	}
	return out
}

func TestParseRun(t *testing.T) {
	tests := []struct {
		name         string
		in           []Line
		wantOutcomes map[string]Outcome
		wantElapsed  map[string]time.Duration
		wantTimedOut []string
	}{
		{
			name:         "a passing top-level test runs from its RUN to its result line",
			in:           lines("=== RUN   TestA", "output", "--- PASS: TestA (0.00s)", "PASS"),
			wantOutcomes: map[string]Outcome{"TestA": Pass},
			wantElapsed:  map[string]time.Duration{"TestA": 2 * time.Millisecond},
		},
		{
			name: "a subtest ends at the next sibling's RUN, though its result line comes with its parent's",
			in: lines("=== RUN   TestB", "=== RUN   TestB/x", "x out", "=== RUN   TestB/y",
				"--- FAIL: TestB (0.00s)", "    --- PASS: TestB/x (0.00s)", "    --- FAIL: TestB/y (0.00s)"),
			wantOutcomes: map[string]Outcome{"TestB": Fail, "TestB/x": Pass, "TestB/y": Fail},
			wantElapsed:  map[string]time.Duration{"TestB": 4 * time.Millisecond, "TestB/x": 2 * time.Millisecond, "TestB/y": time.Millisecond},
		},
		{
			name:         "a skip is its own outcome",
			in:           lines("=== RUN   TestS", "--- SKIP: TestS (0.00s)"),
			wantOutcomes: map[string]Outcome{"TestS": Skip},
			wantElapsed:  map[string]time.Duration{"TestS": time.Millisecond},
		},
		{
			name: "a -test.timeout panic names the tests that were running",
			in: lines("=== RUN   TestH", "=== RUN   TestH/sub", "panic: test timed out after 1s",
				"\trunning tests:", "\t\tTestH (1s)", "\t\tTestH/sub (1s)", "", "goroutine 1 [running]:"),
			wantOutcomes: map[string]Outcome{},
			wantElapsed:  map[string]time.Duration{"TestH": 0},
			wantTimedOut: []string{"TestH", "TestH/sub"},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := ParseRun(tt.in)

			// Assert
			if !reflect.DeepEqual(got.Outcomes, tt.wantOutcomes) {
				t.Errorf("outcomes = %v, want %v", got.Outcomes, tt.wantOutcomes)
			}
			for name, want := range tt.wantElapsed {
				if got.Elapsed[name] != want {
					t.Errorf("elapsed[%s] = %v, want %v", name, got.Elapsed[name], want)
				}
			}
			if !reflect.DeepEqual(got.TimedOut, tt.wantTimedOut) {
				t.Errorf("timed out = %v, want %v", got.TimedOut, tt.wantTimedOut)
			}
		})
	}
}

func TestParseRunRecordsEveryStartedName(t *testing.T) {
	// Arrange
	in := lines("=== RUN   TestA", "=== RUN   TestA/one", "--- PASS: TestA (0.00s)")

	// Act
	got := ParseRun(in)

	// Assert
	if !reflect.DeepEqual(got.Ran, map[string]bool{"TestA": true, "TestA/one": true}) {
		t.Fatalf("ran = %v", got.Ran)
	}
}

func TestRunPatternSelectsExactlyTheLeaf(t *testing.T) {
	tests := []struct {
		name    string
		match   []string
		nomatch []string
	}{
		{name: "TestA", match: []string{"TestA"}, nomatch: []string{"TestAB", "XTestA"}},
		{name: "TestA/a_(b)", match: []string{"TestA/a_(b)"}, nomatch: []string{"TestA/a_(b)#01"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			pattern := RunPattern(tt.name)

			// Assert: the go test runner matches level by level.
			levels := regexp.MustCompile("/").Split(pattern, -1)
			matches := func(name string) bool {
				parts := regexp.MustCompile("/").Split(name, -1)
				if len(parts) != len(levels) {
					return false
				}
				for i, p := range parts {
					if !regexp.MustCompile(levels[i]).MatchString(p) {
						return false
					}
				}
				return true
			}
			for _, n := range tt.match {
				if !matches(n) {
					t.Errorf("%s does not match %s", pattern, n)
				}
			}
			for _, n := range tt.nomatch {
				if matches(n) {
					t.Errorf("%s matches %s", pattern, n)
				}
			}
		})
	}
}

func TestTopPatternSelectsEveryNamedTopAndNoOther(t *testing.T) {
	// Arrange
	re := regexp.MustCompile(TopPattern([]string{"TestA", "TestB"}))

	// Act / Assert
	for name, want := range map[string]bool{"TestA": true, "TestB": true, "TestAB": false, "TestC": false} {
		if re.MatchString(name) != want {
			t.Errorf("TopPattern matches %s = %t, want %t", name, !want, want)
		}
	}
}

func TestTopIsTheFirstLevel(t *testing.T) {
	for in, want := range map[string]string{"TestA": "TestA", "TestA/x/y": "TestA"} {
		if got := Top(in); got != want {
			t.Errorf("Top(%q) = %q, want %q", in, got, want)
		}
	}
}
