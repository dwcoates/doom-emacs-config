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

func TestSeamClaimsReadsEachFormOfTheSeamLine(t *testing.T) {
	tests := []struct {
		name          string
		doc           string
		wantClaims    []seamClaim
		wantMalformed []string
	}{
		{"a family", "proves it.\n\nSeam: query-death\n", []seamClaim{{line: "query-death", family: "query-death"}}, nil},
		{"a crossing", "Seam: daemon -> shim, the interrupt of a live tool\n", []seamClaim{{line: "daemon -> shim, the interrupt of a live tool"}}, nil},
		{"two lines", "Seam: skill\nSeam: store -> daemon, the replayed page\n", []seamClaim{{line: "skill", family: "skill"}, {line: "store -> daemon, the replayed page"}}, nil},
		{"no line", "proves nothing.\n", nil, nil},
		{"a seam named only in prose", "the Seam: is not at the line's start\n", nil, nil},
		{"a crossing with nothing crossing", "Seam: daemon -> shim\n", nil, []string{"Seam: daemon -> shim"}},
		{"words that are no family", "Seam: the daemon and the shim\n", nil, []string{"Seam: the daemon and the shim"}},
		{"no space after the colon", "Seam:skill\n", nil, []string{"Seam:skill"}},
		{"nothing after the colon", "Seam:\n", nil, []string{"Seam:"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			claims, malformed := seamClaims(tt.doc)

			// Assert
			if !reflect.DeepEqual(claims, tt.wantClaims) {
				t.Errorf("claims = %+v, want %+v", claims, tt.wantClaims)
			}
			if !reflect.DeepEqual(malformed, tt.wantMalformed) {
				t.Errorf("malformed = %q, want %q", malformed, tt.wantMalformed)
			}
		})
	}
}

// seamExempt are the scenarios whose steps prove no seam, with why. A
// scenario named here is exempt with every step it lists.
var seamExempt = map[string]string{
	"TestHarness": "its subject is the e2e harness's own World machinery, which needs a world to test but is no system of agent-repl",
	"TestDoom":    "the Doom tier proves Emacs's own behavior under a booted Doom against the fake daemon (AGENTS.md, \"An e2e test proves a seam\"), which crosses no seam between real systems",
}

func TestEveryScenarioAndStepNamesItsSeam(t *testing.T) {
	// Arrange
	pkg := parseE2EPackage(t)

	// Act
	var offenders []string
	check := func(name, file string) {
		claims, malformed := seamClaims(pkg.docs[name])
		for _, line := range malformed {
			offenders = append(offenders, fmt.Sprintf("%s (%s) says %q, in neither form", name, file, line))
		}
		if len(claims) == 0 && len(malformed) == 0 {
			offenders = append(offenders, fmt.Sprintf("%s (%s) names no seam", name, file))
		}
	}
	for _, scenarioName := range sortedKeys(pkg.scenarios) {
		if _, exempt := seamExempt[scenarioName]; exempt {
			continue
		}
		check(scenarioName, pkg.tests[scenarioName])
		for _, step := range pkg.scenarios[scenarioName] {
			check(step, pkg.steps[step])
		}
	}

	// Assert
	if len(offenders) > 0 {
		t.Fatalf("every scenario and step names the seam it proves in its doc comment, on a line of its own: `Seam: <family>` (a family of testdata/scenario-goldens) or `Seam: <producer> -> <consumer>, <what crosses>`; a test that can name none does not belong in e2e (AGENTS.md, \"An e2e test proves a seam between real systems, and nothing else\"):\n%s", strings.Join(offenders, "\n"))
	}
}

func TestEverySeamExemptionNamesAScenario(t *testing.T) {
	// Arrange
	pkg := parseE2EPackage(t)

	// Act / Assert
	for _, name := range sortedKeys(seamExempt) {
		if _, ok := pkg.scenarios[name]; !ok {
			t.Errorf("seamExempt names %s, which is no scenario: drop the exemption", name)
		}
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
