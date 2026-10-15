package suites

import (
	"reflect"
	"strings"
	"testing"

	"agentrepl/testrun/roster"
)

func TestProgramUnitsBuildEachNeededProgramOnce(t *testing.T) {
	// Arrange: two suites ask for overlapping programs.
	l := Layout{Module: "/m", Work: "/w"}
	needed := []string{"shim-store", "agent-repld", "shim-store", "git"}

	// Act.
	specs, err := ProgramUnits(l, needed, func(p roster.Program) string { return p.Owner })

	// Assert: one unit each, in roster order, under its owner.
	if err != nil {
		t.Fatal(err)
	}
	var ids, suites []string
	for _, s := range specs {
		ids = append(ids, s.ID)
		suites = append(suites, s.Suite)
	}
	if want := []string{"program:agent-repld", "program:git", "program:shim-store"}; !reflect.DeepEqual(ids, want) {
		t.Fatalf("ids = %v, want %v", ids, want)
	}
	if want := []string{"daemon", "daemon", "store"}; !reflect.DeepEqual(suites, want) {
		t.Fatalf("suites = %v, want %v", suites, want)
	}
}

func TestTheDaemonProgramIsLinkedToTheRunsMockedGit(t *testing.T) {
	// Act.
	specs, err := ProgramUnits(Layout{Module: "/m", Work: "/w"}, []string{"agent-repld"}, func(p roster.Program) string { return p.Owner })

	// Assert.
	if err != nil || len(specs) != 1 {
		t.Fatalf("specs = %v, %v", specs, err)
	}
	want := []string{"go", "build", "-buildvcs=false", "-ldflags=-X agent-repld/internal/gitclient.gitBinary=/w/programs/git",
		"-o", "/w/programs/agent-repld", "./cmd/agent-repld"}
	if !reflect.DeepEqual(specs[0].Argv, want) || specs[0].Dir != "/m/daemon" {
		t.Fatalf("spec = %v in %s, want %v in /m/daemon", specs[0].Argv, specs[0].Dir, want)
	}
}

func TestProgramUnitsRefuseAProgramTheRosterDoesNotHold(t *testing.T) {
	// Act.
	_, err := ProgramUnits(Layout{}, []string{"no-such"}, func(p roster.Program) string { return p.Owner })

	// Assert.
	if err == nil || !strings.Contains(err.Error(), `"no-such"`) {
		t.Fatalf("err = %v, want one naming the program", err)
	}
}

func TestAPackageThatRunsProgramsWaitsForThemAndIsToldWhere(t *testing.T) {
	// Arrange.
	module := goPackageFixture(t, "func TestA(t *testing.T) {}\n")
	l := Layout{Module: module, Work: t.TempDir()}
	s := roster.Suite{Name: "store", Programs: map[string][]string{"p": {"shim-store"}}}

	// Act.
	u, err := goModuleUnitsForPackages(l, s, module, []string{"p"})

	// Assert.
	if err != nil {
		t.Fatal(err)
	}
	if !reflect.DeepEqual(u.Programs, []string{"shim-store"}) {
		t.Fatalf("Programs = %v, want the package's", u.Programs)
	}
	split := u.Splits[0]
	if !reflect.DeepEqual(split.Deps[1:], []string{"program:shim-store"}) {
		t.Fatalf("deps = %v, want the build and the program", split.Deps)
	}
	chunk := split.Chunk("store:p#00", split.Items)
	found := false
	for _, e := range chunk.Env {
		found = found || e == "AGENT_REPL_TEST_PROGRAMS="+l.ProgramsDir()
	}
	if !found {
		t.Fatalf("chunk env = %v, want the programs directory", chunk.Env)
	}
}

func TestTheTaggedIntegrationPassWaitsForItsPrograms(t *testing.T) {
	// Arrange.
	module := goPackageFixture(t, "func TestA(t *testing.T) {}\n")
	l := Layout{Module: module, Work: t.TempDir()}
	s := roster.Suite{Name: "daemon", IntegrationTags: "integration", IntegrationPackages: "./p",
		Programs: map[string][]string{"p": {"agent-repld", "git"}}}

	// Act.
	u, err := goIntegrationUnitsForPackages(l, s, module, []string{"p"})

	// Assert.
	if err != nil {
		t.Fatal(err)
	}
	if !reflect.DeepEqual(u.Programs, []string{"agent-repld", "git"}) {
		t.Fatalf("Programs = %v", u.Programs)
	}
	if got := u.Splits[0].Deps[1:]; !reflect.DeepEqual(got, []string{"program:agent-repld", "program:git"}) {
		t.Fatalf("deps = %v, want both programs", got)
	}
}

func TestProgramBuildPutsExtraFlagsBeforeTheOutput(t *testing.T) {
	// Arrange
	p, _ := roster.LookupProgram("shim-store")

	// Act
	argv, dir, err := ProgramBuild(Layout{Module: "/m", Work: "/w"}, p, "-overlay", "/o.json")

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	want := []string{"go", "build", "-buildvcs=false", "-overlay", "/o.json", "-o", "/w/programs/shim-store", "."}
	if !reflect.DeepEqual(argv, want) || dir != "/m/agent-shim/shim-store" {
		t.Fatalf("ProgramBuild = %v in %s, want %v", argv, dir, want)
	}
}

func TestProgramUnitsBuildWhatProgramBuildSays(t *testing.T) {
	// Every roster program's unit is exactly its ProgramBuild, so no build
	// of a program drifts from the run's.
	l := Layout{Module: "/m", Work: "/w"}
	for _, p := range roster.Programs {
		specs, err := ProgramUnits(l, []string{p.Name}, func(roster.Program) string { return "x" })
		if err != nil {
			t.Fatal(err)
		}
		argv, dir, err := ProgramBuild(l, p)
		if err != nil {
			t.Fatal(err)
		}
		if !reflect.DeepEqual(specs[0].Argv, argv) || specs[0].Dir != dir {
			t.Errorf("%s: unit %v in %s, ProgramBuild %v in %s", p.Name, specs[0].Argv, specs[0].Dir, argv, dir)
		}
	}
}
