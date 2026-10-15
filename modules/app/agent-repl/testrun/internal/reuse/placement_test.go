package reuse

import (
	"slices"
	"strings"
	"testing"

	"agentrepl/testrun/roster"
)

func TestUnplacedNamesEveryFileNoRegionHolds(t *testing.T) {
	// Arrange
	f := newFixture(t)
	lockSet, err := SuiteInputs(f.d, lockSuite)
	if err != nil {
		t.Fatal(err)
	}
	all := []roster.Suite{lockSuite, harnessSuite, {Name: "e2e", Kind: roster.E2E, Path: "e2e"}}
	regions, err := BuildRegions(f.d, all, []string{"scripts/"}, []string{"docs/", "/README.md"}, map[string]*InputSet{"lock": lockSet})
	if err != nil {
		t.Fatal(err)
	}
	files := []string{
		fixtureModule + "/lock/lock.go",           // the keyed suite's build graph
		fixtureModule + "/prompts/p.md",           // the keyed suite's declared input
		fixtureModule + "/bin/test-background.sh", // an always-run suite's own script
		fixtureModule + "/e2e/world_test.go",      // an always-run suite's own tree
		fixtureModule + "/scripts/x.sh",           // an always-run region
		fixtureModule + "/docs/guide.md",          // read by no suite
		"README.md",                               // read by no suite, at the repository root
		fixtureModule + "/newsystem/main.go",      // nowhere
		fixtureModule + "/lock/sub/inner.go",      // under a flat directory, so nowhere
	}

	// Act
	got := regions.Unplaced(files)

	// Assert
	want := []string{fixtureModule + "/lock/sub/inner.go", fixtureModule + "/newsystem/main.go"}
	if !slices.Equal(got, want) {
		t.Fatalf("Unplaced = %v, want %v", got, want)
	}
}

func TestAKeyedSuiteWithoutAnInputSetCannotBePlaced(t *testing.T) {
	// Arrange
	f := newFixture(t)

	// Act
	_, err := BuildRegions(f.d, []roster.Suite{lockSuite}, nil, nil, map[string]*InputSet{})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "lock has no input set") {
		t.Fatalf("err = %v, want the missing set named", err)
	}
}

func TestReportPlacement(t *testing.T) {
	tests := []struct {
		name     string
		unplaced []string
		ok       bool
	}{
		{"every file placed", nil, true},
		{"an unplaced file", []string{"x/y.go"}, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			log := &logLines{}

			// Act
			ok := ReportPlacement(log, tt.unplaced)

			// Assert
			if ok != tt.ok || len(log.errs) != len(tt.unplaced) {
				t.Fatalf("ok = %v, errors = %v", ok, log.errs)
			}
			for i, f := range tt.unplaced {
				if !strings.HasPrefix(log.errs[i], "placement: "+f+" is read by no suite and listed nowhere") {
					t.Fatalf("error = %q", log.errs[i])
				}
			}
		})
	}
}

func TestTheRostersPlacementListsAreNarrow(t *testing.T) {
	// No entry may take in the whole module or the whole repository: a list
	// that broad would place code no suite tests.
	for _, list := range [][]string{roster.AlwaysRunRegions, roster.ReadByNoSuite} {
		for _, p := range list {
			if p == "/" || p == "./" || p == "" || p == "/modules/" || p == "/modules/app/" {
				t.Errorf("%q is too broad to be a placement", p)
			}
		}
	}
}
