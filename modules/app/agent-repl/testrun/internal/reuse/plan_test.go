package reuse

import (
	"agentrepl/testrun/roster"
	"bytes"
	"encoding/json"
	"strings"
	"testing"
	"time"
)

func TestThePlanSaysWhatRunsAndWhatIsReused(t *testing.T) {
	// Arrange
	at := time.Date(2026, 10, 14, 12, 0, 0, 0, time.UTC)
	verdicts := []Verdict{
		{Suite: lockSuite, Keyed: true, Key: Key{Digest: keyA}, Reused: &Record{PassedAt: at, Branch: "master", Commit: "c1", Dirty: true}},
		{Suite: lockSuite, Keyed: true, Key: Key{Digest: keyB}, Why: "no record"},
		{Suite: harnessSuite, Why: "always runs"},
	}

	// Act
	doc := BuildPlan(verdicts, []string{"x.go"}, true)

	// Assert
	r, k, h := doc.Suites[0], doc.Suites[1], doc.Suites[2]
	if !r.Reused || r.InputKey != keyA || r.Sha != "c1" || !r.Dirty || r.Command != "" || r.Why != "" {
		t.Errorf("reused = %+v", r)
	}
	if k.Reused || k.Why != "no record" || k.Command != "bin/test-all.sh --suites lock" || k.Kind != "unit+integration" {
		t.Errorf("keyed = %+v", k)
	}
	if h.InputKey != "" || h.Kind != "harness" || h.Width != 1 {
		t.Errorf("harness = %+v", h)
	}
	if doc.NoReuseFlag != "--no-reuse" || doc.Unplaced[0] != "x.go" || doc.Version != PlanVersion {
		t.Errorf("doc = %+v", doc)
	}
}

func TestThePlanJSONShape(t *testing.T) {
	// Arrange
	doc := BuildPlan([]Verdict{{Suite: harnessSuite, Why: "always runs"}}, nil, false)
	var b bytes.Buffer

	// Act
	err := WritePlanJSON(&b, doc)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	var back map[string]any
	if err := json.Unmarshal(b.Bytes(), &back); err != nil {
		t.Fatal(err)
	}
	for _, field := range []string{"version", "no_reuse_flag", "reuse_available", "suites", "unplaced"} {
		if _, ok := back[field]; !ok {
			t.Errorf("the plan lacks %q:\n%s", field, b.String())
		}
	}
	suite := back["suites"].([]any)[0].(map[string]any)
	for _, field := range []string{"suite", "kind", "reused", "why", "width", "command"} {
		if _, ok := suite[field]; !ok {
			t.Errorf("a suite lacks %q:\n%s", field, b.String())
		}
	}
}

func TestThePlanTextNamesNoReuse(t *testing.T) {
	// Arrange
	var b bytes.Buffer

	// Act
	err := WritePlanText(&b, BuildPlan(nil, nil, false))

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	for _, want := range []string{"reuse is unavailable here", "--no-reuse runs every selected suite"} {
		if !strings.Contains(b.String(), want) {
			t.Errorf("the plan lacks %q:\n%s", want, b.String())
		}
	}
}

func TestSuiteKind(t *testing.T) {
	tests := []struct {
		name  string
		suite string
		want  string
	}{
		{name: "a keyed system suite", suite: "daemon", want: "unit+integration"},
		{name: "the cross-system Go suite", suite: "e2e", want: "e2e"},
		{name: "the sandboxed Emacs client layer", suite: "e2e-emacs", want: "e2e"},
		{name: "the Doom tier is an integration suite", suite: "emacs-doom", want: "unit+integration"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, ok := roster.Lookup(tt.suite)
			if !ok {
				t.Fatalf("no roster suite %q", tt.suite)
			}

			// Act
			got := SuiteKind(s)

			// Assert
			if got != tt.want {
				t.Fatalf("SuiteKind(%s) = %q, want %q", tt.suite, got, tt.want)
			}
		})
	}
}
