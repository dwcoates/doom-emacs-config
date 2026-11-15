package scans

import (
	"agentrepl/e2e/kit"
	"fmt"
	"reflect"
	"strings"
	"testing"
)

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
	for _, s := range kit.SortedKeys(pkg.steps) {
		if scenarios := listed[s]; len(scenarios) != 1 {
			t.Errorf("step %s (%s) is listed by %d scenarios %v, want exactly one: a step no scenario lists never runs", s, pkg.steps[s], len(scenarios), scenarios)
		}
	}
	for _, s := range kit.SortedKeys(listed) {
		if _, ok := pkg.steps[s]; !ok {
			t.Errorf("scenarios %v list %s, which is not a step: a step is a package-level func testXxx(t *testing.T)", listed[s], s)
		}
	}
}

// worldBuildingPackages are the packages whose top-level tests may build a
// world, relative to the e2e package: the e2e package itself, and the
// harness that builds the worlds (kit/stack), whose own tests could build
// one as easily.
var worldBuildingPackages = []string{".", "kit/stack"}

func TestEveryWorldBuildingTestIsAScenario(t *testing.T) {
	for _, dir := range worldBuildingPackages {
		t.Run(dir, func(t *testing.T) {
			// Arrange
			pkg := parseTestPackage(t, dir)
			if len(pkg.tests) == 0 {
				t.Fatalf("the package %s declares no top-level test; the scan read nothing", dir)
			}

			// Act
			offenders := worldBuildingOutsideScenarios(pkg)

			// Assert
			if len(offenders) > 0 {
				t.Fatalf("top-level tests of %s that build a world outside a scenario: %s; add the coverage as a step of the scenario whose subject it extends (scenarios_test.go)", dir, strings.Join(offenders, ", "))
			}
		})
	}
}

// worldBuildingOutsideScenarios is every top-level test of PKG that reaches
// a world builder and is not a scenario, each with the builder it reaches.
func worldBuildingOutsideScenarios(pkg e2ePackage) []string {
	var offenders []string
	for _, name := range kit.SortedKeys(pkg.tests) {
		if _, isScenario := pkg.scenarios[name]; isScenario {
			continue
		}
		if builder := pkg.reachesWorldBuilder(name); builder != "" {
			offenders = append(offenders, name+" (reaches "+builder+")")
		}
	}
	return offenders
}

func TestWorldBuildingOutsideScenariosNamesEachOffender(t *testing.T) {
	tests := []struct {
		name string
		pkg  e2ePackage
		want []string
	}{
		{
			name: "a test that builds a world directly is named with its builder",
			pkg:  e2ePackage{tests: map[string]string{"TestA": "a_test.go"}, refs: map[string][]string{"TestA": {"NewWorld"}}},
			want: []string{"TestA (reaches NewWorld)"},
		},
		{
			name: "a test that reaches a builder through a helper is named",
			pkg: e2ePackage{tests: map[string]string{"TestA": "a_test.go"},
				refs: map[string][]string{"TestA": {"helper"}, "helper": {"SharedWorld"}}},
			want: []string{"TestA (reaches SharedWorld)"},
		},
		{
			name: "a scenario that builds worlds is not an offender",
			pkg: e2ePackage{tests: map[string]string{"TestS": "s_test.go"}, scenarios: map[string][]string{"TestS": {"testStep"}},
				refs: map[string][]string{"TestS": {"NewWorld"}}},
		},
		{
			name: "a test that builds no world is not an offender",
			pkg:  e2ePackage{tests: map[string]string{"TestA": "a_test.go"}, refs: map[string][]string{"TestA": {"helper"}, "helper": {"other"}}},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := worldBuildingOutsideScenarios(tt.pkg)

			// Assert
			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("got %v, want %v", got, tt.want)
			}
		})
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
	for _, scenarioName := range kit.SortedKeys(pkg.scenarios) {
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
	for _, name := range kit.SortedKeys(seamExempt) {
		if _, ok := pkg.scenarios[name]; !ok {
			t.Errorf("seamExempt names %s, which is no scenario: drop the exemption", name)
		}
	}
}
