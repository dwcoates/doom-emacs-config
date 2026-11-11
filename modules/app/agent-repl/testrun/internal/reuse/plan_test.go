package reuse

import (
	"bytes"
	"encoding/json"
	"strings"
	"testing"
	"time"
)

func TestThePlanSaysWhatRunsAndWhatIsReused(t *testing.T) {
	// Arrange
	at := time.Date(2026, 10, 14, 12, 0, 0, 0, time.UTC)
	reused := SuiteVerdict{SuiteUnits: SuiteUnits{Suite: lockSuite}, Units: []UnitVerdict{
		{Unit: Unit{Suite: "lock", Name: "."}, Keyed: true, Key: Key{Digest: keyA}, Reused: &Record{PassedAt: at, Branch: "master", Commit: "c1", Dirty: true}},
	}}
	part := SuiteVerdict{SuiteUnits: SuiteUnits{Suite: lockSuite}, Units: []UnitVerdict{
		{Unit: Unit{Suite: "lock", Name: "."}, Keyed: true, Key: Key{Digest: keyA}, Reused: &Record{PassedAt: at}},
		{Unit: Unit{Suite: "lock", Name: "vet"}, Keyed: true, Key: Key{Digest: keyB}, Why: Why{Kind: WhyNeverRecorded}},
	}}
	harness := SuiteVerdict{SuiteUnits: SuiteUnits{Suite: harnessSuite}, Units: []UnitVerdict{{Unit: Unit{Suite: "harness"}, Why: Why{Kind: WhyNoSandbox}}}}

	// Act
	doc := BuildPlan([]SuiteVerdict{reused, part, harness}, []string{"x.go"}, true)

	// Assert
	r, k, h := doc.Suites[0], doc.Suites[1], doc.Suites[2]
	if !r.Reused || r.InputKey != reused.CombinedKey().Digest || r.Sha != "c1" || !r.Dirty || r.Command != "" || r.Why != "" || r.Units != 1 || r.ReusedUnits != 1 {
		t.Errorf("reused = %+v", r)
	}
	if k.Reused || k.Why != "1 never recorded" || k.Command != "bin/test-all.sh --suites lock" || k.Kind != "unit+integration" ||
		k.Units != 2 || k.ReusedUnits != 1 || len(k.Running) != 1 || k.Running[0] != (PlanUnit{Unit: "vet", Why: "never recorded"}) {
		t.Errorf("partly reused = %+v", k)
	}
	if h.InputKey != "" || h.Kind != "harness" || h.Width != 1 || h.Why != "1 no sandbox" {
		t.Errorf("harness = %+v", h)
	}
	if doc.NoReuseFlag != "--no-reuse" || doc.Unplaced[0] != "x.go" || doc.Version != PlanVersion {
		t.Errorf("doc = %+v", doc)
	}
}

func TestThePlanJSONShape(t *testing.T) {
	// Arrange
	doc := BuildPlan([]SuiteVerdict{{SuiteUnits: SuiteUnits{Suite: harnessSuite}, Units: []UnitVerdict{{Why: Why{Kind: WhyNeverRecorded}}}}}, nil, false)
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
	for _, field := range []string{"suite", "kind", "reused", "units", "reused_units", "why", "running", "width", "command"} {
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
