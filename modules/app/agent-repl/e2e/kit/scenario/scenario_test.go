package scenario

import (
	"fmt"
	"reflect"
	"strings"
	"testing"
)

// testScenarioFixtureStep and testScenarioOtherFixtureStep are steps no
// scenario lists: this file's own fixtures.
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
			got := StepName(tt.step)

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
			got := recoverPanic(func() { StepName(tt.step) })

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
			Run(r, []func(*testing.T){testScenarioFixtureStep, testScenarioOtherFixtureStep})

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
	got := recoverPanic(func() { Run(r, nil) })

	// Assert
	if !strings.Contains(got, "no steps") {
		t.Fatalf("Run(nil) panicked with %q, want it refused", got)
	}
	if len(r.calls) != 0 {
		t.Fatalf("calls = %q, want none before the refusal", r.calls)
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
