package suites

import (
	"path/filepath"
	"reflect"
	"slices"
	"strings"
	"testing"

	"agentrepl/testrun/internal/gobuild"
	"agentrepl/testrun/roster"
	"agentrepl/testrun/testenv"
)

// layoutFor is l, with the real module root when the program p is an impact
// build, whose -coverpkg reads the module's go.mod files (every other
// program's build reads nothing, so a placeholder module serves).
func layoutFor(t *testing.T, l Layout, p roster.Program) Layout {
	t.Helper()
	if !p.Impact {
		return l
	}
	realModule, err := filepath.Abs("../../..")
	if err != nil {
		t.Fatal(err)
	}
	l.Module = realModule
	return l
}

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
	want := []string{"go", "build", "-buildvcs=false", "-ldflags=-w -X agent-repld/internal/gitclient.gitBinary=/w/programs/git",
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
	want := []string{"go", "build", "-buildvcs=false", "-ldflags=-w", "-overlay", "/o.json", "-o", "/w/programs/shim-store", "."}
	if !reflect.DeepEqual(argv, want) || dir != "/m/agent-shim/shim-store" {
		t.Fatalf("ProgramBuild = %v in %s, want %v", argv, dir, want)
	}
}

func TestProgramUnitsBuildWhatProgramBuildSays(t *testing.T) {
	// Every roster program's unit is exactly its ProgramBuild, so no build
	// of a program drifts from the run's.
	for _, p := range roster.Programs {
		l := layoutFor(t, Layout{Module: "/m", Work: "/w"}, p)
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

func TestProgramBuildLinksDebugInfoOnlyWhenAsked(t *testing.T) {
	tests := []struct {
		name      string
		program   string
		debugInfo bool
		want      []string
	}{
		{"a program without links drops DWARF", "shim-store", false,
			[]string{"go", "build", "-buildvcs=false", "-ldflags=-w", "-o", "/w/programs/shim-store", "."}},
		{"a program without links keeps DWARF on request, with no -ldflags", "shim-store", true,
			[]string{"go", "build", "-buildvcs=false", "-o", "/w/programs/shim-store", "."}},
		{"a linked program keeps its links and DWARF on request", "agent-repld", true,
			[]string{"go", "build", "-buildvcs=false", "-ldflags=-X agent-repld/internal/gitclient.gitBinary=/w/programs/git", "-o", "/w/programs/agent-repld", "./cmd/agent-repld"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			p, _ := roster.LookupProgram(tt.program)

			// Act
			argv, _, err := ProgramBuild(Layout{Module: "/m", Work: "/w", DebugInfo: tt.debugInfo}, p)

			// Assert
			if err != nil || !reflect.DeepEqual(argv, tt.want) {
				t.Fatalf("ProgramBuild = %v, %v; want %v", argv, err, tt.want)
			}
		})
	}
}

// TestEveryLinkInARunTakesItsFlagsFromLinkFlags pins that the run's two kinds
// of link, a module's test binaries and a roster program, both spell their
// -ldflags through gobuild.LinkFlags, so neither can drift to a different
// debug-information default.
func TestEveryLinkInARunTakesItsFlagsFromLinkFlags(t *testing.T) {
	for _, debugInfo := range []bool{false, true} {
		l := Layout{Module: "/m", Work: "/w", Self: "/testrun", DebugInfo: debugInfo}
		for _, p := range roster.Programs {
			// Arrange
			var links []string
			for _, link := range p.Links {
				links = append(links, "-X "+link.Var+"=/w/programs/"+link.Program)
			}

			// Act
			argv, _, err := ProgramBuild(layoutFor(t, l, p), p)

			// Assert
			if err != nil {
				t.Fatal(err)
			}
			if got, want := ldflagsOf(argv), gobuild.LinkFlags(debugInfo, links...); !reflect.DeepEqual(got, want) {
				t.Errorf("program %s (debug info %v): -ldflags %q, want LinkFlags' %q", p.Name, debugInfo, got, want)
			}
		}
		o := moduleBuild(l, "daemon", "/m/daemon", "", false).Options
		o.Packages = []string{"a"}
		args, err := gobuild.GoTestArgs(o)
		if err != nil {
			t.Fatal(err)
		}
		if got, want := ldflagsOf(args), gobuild.LinkFlags(debugInfo); !reflect.DeepEqual(got, want) {
			t.Errorf("module build (debug info %v): -ldflags %q, want LinkFlags' %q", debugInfo, got, want)
		}
	}
}

// ldflagsOf is every -ldflags argument of argv.
func ldflagsOf(argv []string) []string {
	var flags []string
	for _, a := range argv {
		if strings.HasPrefix(a, "-ldflags") {
			flags = append(flags, a)
		}
	}
	return flags
}

// TestEveryProgramUnitIsAPinnedBuild pins that a roster program's build unit
// takes its width and pins from pinnedBuild, as a module build's does, so no
// build unit holds fewer slots than the cores its go command uses.
func TestEveryProgramUnitIsAPinnedBuild(t *testing.T) {
	// Arrange
	var names []string
	for _, p := range roster.Programs {
		names = append(names, p.Name)
	}

	// Act
	realModule, err := filepath.Abs("../../..")
	if err != nil {
		t.Fatal(err)
	}
	specs, err := ProgramUnits(Layout{Module: realModule, Work: "/w"}, names, func(p roster.Program) string { return p.Owner })

	// Assert
	if err != nil || len(specs) != len(names) {
		t.Fatalf("specs = %v, %v", specs, err)
	}
	for _, sp := range specs {
		if want := pinnedBuild(sp); sp.Slots != want.Slots || !reflect.DeepEqual(sp.Env, want.Env) {
			t.Errorf("%s holds %d slots pinned by %v, want pinnedBuild's %d pinned by %v", sp.ID, sp.Slots, sp.Env, want.Slots, want.Env)
		}
	}
}

func TestAnImpactProgramIsBuiltAtomicallyInstrumentedAndLinkedToTheMockedGit(t *testing.T) {
	// Arrange
	realModule, err := filepath.Abs("../../..")
	if err != nil {
		t.Fatal(err)
	}
	p, ok := roster.LookupProgram("agent-repld-impact")
	if !ok || !p.Impact {
		t.Fatalf("the roster holds no impact build of the daemon: %+v", p)
	}

	// Act
	argv, dir, err := ProgramBuild(Layout{Module: realModule, Work: "/w"}, p)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	joined := strings.Join(argv, " ")
	for _, want := range []string{"-cover", "-covermode=atomic", "-coverpkg=./...", "-X agent-repld/internal/gitclient.gitBinary=/w/programs/git", "-o /w/programs/agent-repld-impact ./cmd/agent-repld"} {
		if !strings.Contains(joined, want) {
			t.Fatalf("ProgramBuild = %s, want %q in it", joined, want)
		}
	}
	if dir != filepath.Join(realModule, "daemon") {
		t.Fatalf("ProgramBuild dir = %s, want the daemon module", dir)
	}
}

func TestAnOrdinaryProgramIsNeverInstrumentedByAnImpactRun(t *testing.T) {
	// Arrange
	p, _ := roster.LookupProgram("agent-repld")

	// Act
	argv, _, err := ProgramBuild(Layout{Module: "/m", Work: "/w", Impact: true}, p)

	// Assert
	if err != nil || strings.Contains(strings.Join(argv, " "), "-cover") {
		t.Fatalf("ProgramBuild = %v, %v; want the ordinary build, whatever the run records", argv, err)
	}
}

func TestARecordedPackageRunsOneTestAtATimeAgainstTheImpactPrograms(t *testing.T) {
	// Arrange
	module := goPackageFixture(t, "func TestA(t *testing.T) {}\nfunc TestB(t *testing.T) {}\n")
	l := Layout{Module: module, Work: t.TempDir(), Self: "/testrun", Impact: true}
	s := roster.Suite{Name: "daemon", IntegrationTags: "integration", IntegrationPackages: "./p",
		TestParallel:   map[string]roster.Parallel{"p": {Tests: 8, Slots: 2}},
		ImpactPackages: []string{"p"},
		Programs:       map[string][]string{"p": {"agent-repld", "fakeshim", "git"}}}

	// Act
	u, err := goIntegrationUnitsForPackages(l, s, module, []string{"p"})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if want := []string{"agent-repld-impact", "fakeshim", "git"}; !reflect.DeepEqual(u.Programs, want) {
		t.Fatalf("Programs = %v, want the impact build in place of the daemon: %v", u.Programs, want)
	}
	split := u.Splits[0]
	if split.Slots != 2 {
		t.Fatalf("Slots = %d, want the two cores a recorded chunk measured", split.Slots)
	}
	chunk := split.Chunk("daemon:p[integration]#00", []string{"TestA", "TestB"})
	if !slices.Contains(chunk.Argv, "-test.parallel=1") {
		t.Fatalf("chunk argv = %v, want one test at a time", chunk.Argv)
	}
	windows := filepath.Join(l.Work, "impact", "daemon", "daemon_p_integration__00.jsonl")
	if chunk.Impact == nil || chunk.Impact.Suite != "daemon" || chunk.Impact.Package != "p" || chunk.Impact.Tags != "integration" || !strings.HasPrefix(chunk.Impact.Windows, filepath.Join(l.Work, "impact", "daemon")) {
		t.Fatalf("chunk.Impact = %+v (want windows under %s)", chunk.Impact, windows)
	}
	if !slices.Contains(chunk.Env, testenv.ImpactOut+"="+chunk.Impact.Windows) {
		t.Fatalf("chunk env = %v, want the windows file named", chunk.Env)
	}
}

func TestAPackageNotListedForImpactIsNeverRecorded(t *testing.T) {
	cases := []struct {
		name string
		l    func(module string) Layout
		s    roster.Suite
	}{
		{"a run that does not record", func(m string) Layout { return Layout{Module: m, Work: t.TempDir()} },
			roster.Suite{Name: "daemon", IntegrationTags: "integration", ImpactPackages: []string{"p"}}},
		{"a package the suite does not list", func(m string) Layout { return Layout{Module: m, Work: t.TempDir(), Impact: true} },
			roster.Suite{Name: "daemon", IntegrationTags: "integration"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			module := goPackageFixture(t, "func TestA(t *testing.T) {}\n")
			l := tc.l(module)
			s := tc.s
			s.IntegrationPackages = "./p"
			s.Programs = map[string][]string{"p": {"agent-repld"}}
			s.TestParallel = map[string]roster.Parallel{"p": {Tests: 8, Slots: 2}}
			// Act
			u, err := goIntegrationUnitsForPackages(l, s, module, []string{"p"})
			// Assert
			if err != nil {
				t.Fatal(err)
			}
			if !reflect.DeepEqual(u.Programs, []string{"agent-repld"}) {
				t.Fatalf("Programs = %v, want the ordinary daemon", u.Programs)
			}
			if chunk := u.Splits[0].Chunk("x", []string{"TestA"}); chunk.Impact != nil || !slices.Contains(chunk.Argv, "-test.parallel=8") {
				t.Fatalf("chunk = %+v, want an ordinary chunk", chunk)
			}
		})
	}
}

func TestAGoSuitesUnitsCarryTheProgramsItsIntegrationPassRuns(t *testing.T) {
	// Arrange: an ordinary package, and a tagged integration package whose
	// tests run programs.
	module, err := filepath.EvalSymlinks(goPackageFixture(t, "func TestA(t *testing.T) {}\n"))
	if err != nil {
		t.Fatal(err)
	}
	write(t, filepath.Join(module, "i", "i.go"), "package i\n", 0o644)
	write(t, filepath.Join(module, "i", "i_test.go"), "//go:build integration\n\npackage i\n\nimport \"testing\"\n\nfunc TestI(t *testing.T) {}\n", 0o644)
	l := Layout{Module: module, Work: t.TempDir(), Impact: true}
	s := roster.Suite{Name: "daemon", Kind: roster.GoModule, Path: ".", IntegrationTags: "integration", IntegrationPackages: "./i",
		ImpactPackages: []string{"i"},
		Programs:       map[string][]string{"i": {"agent-repld", "git"}}}

	// Act
	u, err := goModuleUnits(l, s)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if want := []string{"agent-repld-impact", "git"}; !reflect.DeepEqual(u.Programs, want) {
		t.Fatalf("Programs = %v, want the integration pass's programs, the daemon as its impact build: %v", u.Programs, want)
	}
}
