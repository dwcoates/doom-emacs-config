package cli

import (
	"errors"
	"reflect"
	"strings"
	"testing"

	"agentrepl/testrun/internal/cover"
	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/internal/suites"
)

func plannedForBaseline() []suites.Units {
	var report, build run.Spec
	report.ID = "daemon:coverage"
	build.ID = "daemon:./internal/x:build"
	var merge run.Spec
	merge.ID = "webapp:coverage"
	return []suites.Units{
		{Atomic: []run.Spec{build, report}, Splits: []suites.Split{{Group: "daemon:./internal/x"}, {Group: "daemon:./integration[integration]"}}},
		{Atomic: []run.Spec{merge}, Splits: []suites.Split{{Group: "webapp"}}},
	}
}

func TestBaselineUnitWaitsForEveryChunkAndReport(t *testing.T) {
	tests := []struct {
		name      string
		selected  []string
		selection []string
	}{
		{name: "every suite", selection: []string{"-every-suite"}},
		{name: "a selection, sorted and once each", selected: []string{"e2e-kit", "e2e", "e2e-kit"}, selection: []string{"-suite", "e2e", "-suite", "e2e-kit"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			l := suites.Layout{Repo: "/r", Module: "/r/m", Work: "/w"}
			d := Deps{Self: "/testrun", RunLog: "/logs/run-1/test-all.log", Git: func(string) (string, string, error) { return "master", "abc123", nil }}

			// Act
			u, ok, err := baselineUnit(l, d, tt.selected, plannedForBaseline())

			// Assert
			if err != nil || !ok || len(u.Atomic) != 1 {
				t.Fatalf("baselineUnit = %+v, %t, %v", u, ok, err)
			}
			sp := u.Atomic[0]
			wantArgv := append([]string{"/testrun", "coverage-baseline", "-module", "/r/m", "-work", "/w", "-commit", "abc123", "-out", "/logs/run-1/coverage-baseline.json"}, tt.selection...)
			if sp.ID != BaselineUnitID || !reflect.DeepEqual(sp.Argv, wantArgv) {
				t.Fatalf("spec %s %v, want %v", sp.ID, sp.Argv, wantArgv)
			}
			wantDeps := []string{"daemon:./internal/x", "daemon:./integration[integration]", "daemon:coverage", "webapp", "webapp:coverage"}
			if !reflect.DeepEqual(sp.Deps, wantDeps) {
				t.Fatalf("deps = %v, want %v", sp.Deps, wantDeps)
			}
		})
	}
}

// The argv baselineUnit writes is the one ParseBaselineArgs reads: the two
// halves of the coverage-baseline command line cannot drift apart.
func TestBaselineUnitArgvParses(t *testing.T) {
	for _, selected := range [][]string{nil, {"e2e", "e2e-kit"}} {
		// Arrange
		d := Deps{Self: "/testrun", RunLog: "/logs/x.log", Git: func(string) (string, string, error) { return "master", "c", nil }}
		u, _, err := baselineUnit(suites.Layout{Repo: "/r", Module: "/m", Work: "/w"}, d, selected, nil)
		if err != nil {
			t.Fatal(err)
		}

		// Act
		a, err := cover.ParseBaselineArgs(u.Atomic[0].Argv[2:])

		// Assert
		if err != nil || a.Selection.Every != (selected == nil) || !reflect.DeepEqual(a.Selection.Suites, selected) {
			t.Fatalf("selected %v parsed to %+v, %v", selected, a.Selection, err)
		}
	}
}

func TestBaselineUnitIsSkippedWithNoRunLog(t *testing.T) {
	// Act
	_, ok, err := baselineUnit(suites.Layout{}, Deps{}, nil, plannedForBaseline())

	// Assert
	if err != nil || ok {
		t.Fatalf("ok %t, err %v; want no unit and no error", ok, err)
	}
}

func TestBaselineUnitSurfacesAnUnresolvableCommit(t *testing.T) {
	// Arrange
	d := Deps{RunLog: "/logs/x.log", Git: func(string) (string, string, error) { return "", "", errors.New("not a checkout") }}

	// Act
	_, _, err := baselineUnit(suites.Layout{Repo: "/r"}, d, nil, nil)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "resolve the commit the coverage run measures: not a checkout") {
		t.Fatalf("err = %v", err)
	}
}
