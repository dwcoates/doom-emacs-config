package e2e

import (
	"fmt"
	"reflect"
	"strings"
	"testing"
)

// testScenarioFixtureStep and testScenarioOtherFixtureStep are steps no
// scenario lists: this file's own fixtures, which the source scans below
// leave out (scenarioUnitFile).
func testScenarioFixtureStep(*testing.T)      {}
func testScenarioOtherFixtureStep(*testing.T) {}

// testlowercaseFixture is named like a step whose case name is not capitalized.
func testlowercaseFixture(*testing.T) {}

// notAStepFixture is a function a scenario may not list.
func notAStepFixture(*testing.T) {}

func TestStepNameIsTheFunctionNameWithoutItsPrefix(t *testing.T) {
	tests := []struct {
		name string
		step func(*testing.T)
		want string
	}{
		{"one step", testScenarioFixtureStep, "ScenarioFixtureStep"},
		{"another step", testScenarioOtherFixtureStep, "ScenarioOtherFixtureStep"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := stepName(tt.step)

			// Assert
			if got != tt.want {
				t.Fatalf("stepName = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestStepNameRefusesAFunctionNotNamedAsAStep(t *testing.T) {
	tests := []struct {
		name string
		step func(*testing.T)
	}{
		{"a closure", func(*testing.T) {}},
		{"a function without the prefix", notAStepFixture},
		{"a case name not capitalized", testlowercaseFixture},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := recoverPanic(func() { stepName(tt.step) })

			// Assert
			if !strings.Contains(got, "is not a function named testXxx") {
				t.Fatalf("stepName panicked with %q, want it to refuse the step", got)
			}
		})
	}
}

// recordingRunner is a stepRunner that records what a scenario asked of it,
// and fails the steps named in fail.
type recordingRunner struct {
	calls []string
	fail  map[string]bool
}

func (r *recordingRunner) Helper()   {}
func (r *recordingRunner) Parallel() { r.calls = append(r.calls, "parallel") }
func (r *recordingRunner) Run(name string, _ func(*testing.T)) bool {
	r.calls = append(r.calls, "run "+name)
	return !r.fail[name]
}

func TestAScenarioRunsItsStepsInOrderBesideTheOtherScenarios(t *testing.T) {
	tests := []struct {
		name string
		fail map[string]bool
	}{
		{"every step passes", nil},
		{"a failed step leaves the later steps to run", map[string]bool{"ScenarioFixtureStep": true}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r := &recordingRunner{fail: tt.fail}

			// Act
			runScenario(r, []func(*testing.T){testScenarioFixtureStep, testScenarioOtherFixtureStep})

			// Assert
			want := []string{"parallel", "run ScenarioFixtureStep", "run ScenarioOtherFixtureStep"}
			if !reflect.DeepEqual(r.calls, want) {
				t.Fatalf("calls = %q, want %q", r.calls, want)
			}
		})
	}
}

func TestAScenarioWithNoStepsIsRefused(t *testing.T) {
	// Arrange
	r := &recordingRunner{}

	// Act
	got := recoverPanic(func() { runScenario(r, nil) })

	// Assert
	if !strings.Contains(got, "no steps") {
		t.Fatalf("runScenario(nil) panicked with %q, want it refused", got)
	}
	if len(r.calls) != 0 {
		t.Fatalf("calls = %q, want none before the refusal", r.calls)
	}
}

func TestEveryStepRunsInExactlyOneScenario(t *testing.T) {
	// Arrange
	pkg := parseE2EPackage(t)

	// Act
	listed := map[string][]string{}
	for scenarioName, steps := range pkg.scenarios {
		for _, s := range steps {
			listed[s] = append(listed[s], scenarioName)
		}
	}

	// Assert
	for _, s := range sortedKeys(pkg.steps) {
		if scenarios := listed[s]; len(scenarios) != 1 {
			t.Errorf("step %s (%s) is listed by %d scenarios %v, want exactly one: a step no scenario lists never runs", s, pkg.steps[s], len(scenarios), scenarios)
		}
	}
	for _, s := range sortedKeys(listed) {
		if _, ok := pkg.steps[s]; !ok {
			t.Errorf("scenarios %v list %s, which is not a step: a step is a package-level func testXxx(t *testing.T)", listed[s], s)
		}
	}
}

func TestEveryWorldBuildingTestIsAScenario(t *testing.T) {
	// Arrange
	pkg := parseE2EPackage(t)

	// Act
	var offenders []string
	for _, name := range sortedKeys(pkg.tests) {
		if _, isScenario := pkg.scenarios[name]; isScenario {
			continue
		}
		if builder := pkg.reachesWorldBuilder(name); builder != "" {
			offenders = append(offenders, name+" (reaches "+builder+")")
		}
	}

	// Assert
	if len(offenders) > 0 {
		t.Fatalf("top-level tests that build a world outside a scenario: %s; add the coverage as a step of the scenario whose subject it extends (scenarios_test.go)", strings.Join(offenders, ", "))
	}
}

// recoverPanic runs f and answers what it panicked with, or "".
func recoverPanic(f func()) (msg string) {
	defer func() {
		if r := recover(); r != nil {
			msg = fmtPanic(r)
		}
	}()
	f()
	return ""
}

func fmtPanic(r any) string {
	if s, ok := r.(string); ok {
		return s
	}
	return fmt.Sprint(r)
}
