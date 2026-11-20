package suites

import (
	"path/filepath"
	"reflect"
	"slices"
	"strings"
	"testing"

	"agentrepl/testrun/internal/gobuild"
	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/roster"
)

// moduleFixture is a module of the packages pkgs, each with the test source
// tests[rel] ("" is a test file with helpers only).
func moduleFixture(t *testing.T, tests map[string]string) string {
	t.Helper()
	module := t.TempDir()
	write(t, filepath.Join(module, "go.mod"), "module x\n\ngo 1.24\n", 0o644)
	for rel, body := range tests {
		name := filepath.Base(rel)
		if rel == "." {
			name = "x"
		}
		write(t, filepath.Join(module, rel, name+".go"), "package "+name+"\n", 0o644)
		write(t, filepath.Join(module, rel, name+"_test.go"), "package "+name+"\nimport \"testing\"\n"+body, 0o644)
	}
	return module
}

// buildUnits are the units of u that run testrun's module build.
func buildUnits(u Units) []run.Spec {
	var out []run.Spec
	for _, a := range u.Atomic {
		if len(a.Argv) > 1 && a.Argv[1] == "build-tests" {
			out = append(out, a)
		}
	}
	return out
}

// builtPackages are the packages a build unit's argv names after "--".
func builtPackages(sp run.Spec) []string {
	i := slices.Index(sp.Argv, "--")
	if i < 0 {
		return nil
	}
	return sp.Argv[i+1:]
}

func TestAModulePlansOneBuildOverEveryPackage(t *testing.T) {
	// Arrange: two packages with tests and one whose test files hold none.
	module := moduleFixture(t, map[string]string{
		"a": "func TestA(t *testing.T) {}\n",
		"b": "func TestB(t *testing.T) {}\n",
		"c": "func helper() {}\n",
	})
	l := Layout{Module: module, Work: t.TempDir(), Self: "/testrun"}

	// Act
	u, err := goModuleUnitsForPackages(l, roster.Suite{Name: "m"}, module, []string{"a", "b", "c"})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	builds := buildUnits(u)
	if len(builds) != 1 {
		t.Fatalf("the module plans %d build units, want 1: %+v", len(builds), builds)
	}
	b := builds[0]
	wantArgv := []string{"/testrun", "build-tests", "-module", module, "-bindir", filepath.Join(l.Work, "bin", "m"), "--", "a", "b", "c"}
	if b.ID != "m:build" || b.Dir != module || !reflect.DeepEqual(b.Argv, wantArgv) {
		t.Fatalf("build = %s in %s: %v, want m:build in the module: %v", b.ID, b.Dir, b.Argv, wantArgv)
	}
	if b.Slots != BuildSlots || !reflect.DeepEqual(b.Env, WidePinnedEnv(BuildSlots)) {
		t.Fatalf("build holds %d slots pinned by %v, want %d pinned by %v", b.Slots, b.Env, BuildSlots, WidePinnedEnv(BuildSlots))
	}
	for _, sp := range u.Splits {
		if !reflect.DeepEqual(sp.Deps, []string{"m:build"}) {
			t.Errorf("split %s waits on %v, want the module build", sp.Group, sp.Deps)
		}
		chunk := sp.Chunk(sp.Group+"#00", sp.Items)
		if want := gobuild.BinPath(filepath.Join(l.Work, "bin", "m"), sp.Package); chunk.Argv[2] != want {
			t.Errorf("split %s runs %s, want its package's binary %s", sp.Group, chunk.Argv[2], want)
		}
	}
	var check *run.Spec
	for i := range u.Atomic {
		if u.Atomic[i].ID == "m:c:compiled" {
			check = &u.Atomic[i]
		}
	}
	if check == nil || !reflect.DeepEqual(check.Deps, []string{"m:build"}) {
		t.Fatalf("the test-less package's compiled check = %+v, want one after m:build", check)
	}
}

func TestAnIntegrationPassHasABuildOfItsOwnUnderItsTags(t *testing.T) {
	// Arrange
	module := moduleFixture(t, map[string]string{"integration": "func TestI(t *testing.T) {}\n"})
	l := Layout{Module: module, Work: t.TempDir(), Self: "/testrun"}
	s := roster.Suite{Name: "m", IntegrationTags: "integration", IntegrationPackages: "./integration"}

	// Act
	u, err := goIntegrationUnitsForPackages(l, s, module, []string{"integration"})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	builds := buildUnits(u)
	if len(builds) != 1 || builds[0].ID != "m:build[integration]" {
		t.Fatalf("integration builds = %+v, want one m:build[integration]", builds)
	}
	if !slices.Contains(builds[0].Argv, "-tags") || !reflect.DeepEqual(builtPackages(builds[0]), []string{"integration"}) {
		t.Fatalf("integration build argv = %v", builds[0].Argv)
	}
	if !reflect.DeepEqual(u.Splits[0].Deps, []string{"m:build[integration]"}) || u.Splits[0].Tags != "integration" {
		t.Fatalf("integration split = %+v", u.Splits[0])
	}
}

func TestTheE2EPrebuildRunsBetweenTheBuildAndTheRootPackagesChunks(t *testing.T) {
	// Arrange
	module := moduleFixture(t, map[string]string{
		".":    "func TestRoot(t *testing.T) {}\n",
		"util": "func TestUtil(t *testing.T) {}\n",
	})
	l := Layout{Module: module, Work: t.TempDir(), Self: "/testrun"}

	// Act
	u, err := e2eUnitsForPackages(l, roster.Suite{Name: "e2e"}, module, []string{".", "util"})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	builds := buildUnits(u)
	if len(builds) != 1 || !reflect.DeepEqual(builtPackages(builds[0]), []string{".", "util"}) {
		t.Fatalf("e2e builds = %+v, want one over both packages", builds)
	}
	deps := map[string][]string{}
	for _, a := range u.Atomic {
		deps[a.ID] = a.Deps
	}
	for _, sp := range u.Splits {
		deps[sp.Group] = sp.Deps
	}
	if !reflect.DeepEqual(deps["e2e:.:prebuild"], []string{"e2e:build"}) {
		t.Fatalf("the prebuild waits on %v, want the build", deps["e2e:.:prebuild"])
	}
	if !reflect.DeepEqual(deps["e2e:."], []string{"e2e:.:prebuild"}) || !reflect.DeepEqual(deps["e2e:util"], []string{"e2e:build"}) {
		t.Fatalf("the root's chunks wait on %v and util's on %v", deps["e2e:."], deps["e2e:util"])
	}
}

func TestFinishBuildsOnlyThePackagesWhoseUnitsRemain(t *testing.T) {
	b := GoBuild{ID: "m:build", Suite: "m", Options: gobuild.Options{Module: "/m", BinDir: "/bin", Self: "/testrun"},
		Needs: map[string]string{"m:c:compiled": "c", "m:.:prebuild": "."}}
	split := func(group, rel string) Split { return Split{Group: group, Suite: "m", Package: rel} }
	tests := []struct {
		name   string
		units  Units
		builds [][]string
	}{
		{
			name:   "every package's units remain",
			units:  Units{Splits: []Split{split("m:a", "a"), split("m:b", "b"), split("m:.", ".")}, Atomic: []run.Spec{unit("m:c:compiled"), unit("m:.:prebuild")}},
			builds: [][]string{{".", "a", "b", "c"}},
		},
		{
			name:   "a dropped split's package is not built",
			units:  Units{Splits: []Split{split("m:b", "b")}},
			builds: [][]string{{"b"}},
		},
		{
			name:   "a split renamed by a selection still builds its package",
			units:  Units{Splits: []Split{split("m:a/TestA/a_case", "a")}},
			builds: [][]string{{"a"}},
		},
		{
			name:   "another pass's split is not this build's",
			units:  Units{Splits: []Split{{Group: "m:a[integration]", Suite: "m", Package: "a", Tags: "integration"}}},
			builds: nil,
		},
		{
			name:   "a package kept by an atomic unit alone is built",
			units:  Units{Atomic: []run.Spec{unit("m:.:prebuild")}},
			builds: [][]string{{"."}},
		},
		{
			name:   "no package remains, so no build is placed",
			units:  Units{Atomic: []run.Spec{unit("m:vet")}},
			builds: nil,
		},
		{
			name:   "a build placed before is replaced, never doubled",
			units:  Units{Splits: []Split{split("m:a", "a")}, Atomic: []run.Spec{b.Spec([]string{"a", "b"})}},
			builds: [][]string{{"a"}},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			u := tt.units
			u.GoBuilds = []GoBuild{b}

			// Act
			got := u.Finish()

			// Assert
			var builds [][]string
			for _, sp := range buildUnits(got) {
				builds = append(builds, builtPackages(sp))
			}
			if !reflect.DeepEqual(builds, tt.builds) {
				t.Fatalf("builds = %v, want %v", builds, tt.builds)
			}
		})
	}
}

func TestGoBuildBuilds(t *testing.T) {
	b := GoBuild{ID: "m:build[integration]", Suite: "m", Options: gobuild.Options{Tags: "integration"}}
	tests := []struct {
		name  string
		split Split
		rel   string
		ok    bool
	}{
		{"a split of the build's pass", Split{Group: "m:a[integration]", Suite: "m", Package: "a", Tags: "integration"}, "a", true},
		{"a split whose group a selection renamed", Split{Group: "m:a[integration]/TestA/x", Suite: "m", Package: "a", Tags: "integration"}, "a", true},
		{"a split of the suite's untagged pass", Split{Group: "m:a", Suite: "m", Package: "a"}, "", false},
		{"a split of another suite", Split{Group: "n:a[integration]", Suite: "n", Package: "a", Tags: "integration"}, "", false},
		{"a split that runs no Go binary", Split{Group: "m/f.el", Suite: "m", Tags: "integration"}, "", false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			rel, ok := b.Builds(tt.split)

			// Assert
			if rel != tt.rel || ok != tt.ok {
				t.Fatalf("Builds(%+v) = %q, %t; want %q, %t", tt.split, rel, ok, tt.rel, tt.ok)
			}
		})
	}
}

// A SELECTION RENAMES A SPLIT'S GROUP (one subtest path's own group), and the
// renamed split still runs, and the build still builds, its package's binary.
func TestARenamedSplitRunsTheBinaryItsBuildBuilds(t *testing.T) {
	// Arrange
	module := moduleFixture(t, map[string]string{
		"cmd/repld": "func TestA(t *testing.T) { t.Run(\"a case\", func(t *testing.T) {}) }\n",
		"other":     "func TestZ(t *testing.T) {}\n",
	})
	l := Layout{Module: module, Work: t.TempDir(), Self: "/testrun"}
	u, err := goModuleUnitsForPackages(l, roster.Suite{Name: "m"}, module, []string{"cmd/repld", "other"})
	if err != nil {
		t.Fatal(err)
	}
	i := slices.IndexFunc(u.Splits, func(sp Split) bool { return sp.Package == "cmd/repld" })
	if i < 0 {
		t.Fatalf("no split runs cmd/repld: %+v", u.Splits)
	}
	narrowed := u.Splits[i]
	narrowed.Group += "/TestA/a_case"
	u.Splits = []Split{narrowed}

	// Act
	got := u.Finish()

	// Assert
	builds := buildUnits(got)
	if len(builds) != 1 || !reflect.DeepEqual(builtPackages(builds[0]), []string{"cmd/repld"}) {
		t.Fatalf("builds = %+v, want one over cmd/repld alone", builds)
	}
	chunk := narrowed.Chunk(narrowed.Group+"#00", narrowed.Items)
	if want := gobuild.BinPath(filepath.Join(l.Work, "bin", "m"), "cmd/repld"); chunk.Argv[2] != want {
		t.Fatalf("the renamed split runs %s, want the binary its build makes, %s", chunk.Argv[2], want)
	}
}

func unit(id string) run.Spec {
	var sp run.Spec
	sp.ID = id
	return sp
}

func TestANeedNamingTwoPackagesPanics(t *testing.T) {
	// Arrange
	var b GoBuild
	b.need("m:a", "a")
	defer func() {
		// Assert
		if recover() == nil {
			t.Fatal("one unit recorded as running two packages' binaries did not panic")
		}
	}()

	// Act
	b.need("m:a", "b")
}

func TestWidePinnedEnvPinsTheCoresItsSlotsGive(t *testing.T) {
	// Arrange
	t.Setenv("GOFLAGS", "-mod=mod")

	// Act
	got := WidePinnedEnv(2)

	// Assert
	if want := []string{"GOFLAGS=-mod=mod -p=2 -trimpath", "GOMAXPROCS=2"}; !reflect.DeepEqual(got, want) {
		t.Fatalf("WidePinnedEnv(2) = %v, want %v", got, want)
	}
}

// THE RULE (the /agent-repl-tests skill, "A Go module's test binaries are
// built by one command"): every Go suite of the roster plans one build unit
// per pass (its ordinary pass, and its tagged integration pass), and no unit
// builds one package's binary alone.
func TestEveryRosterGoSuitePlansOneBuildPerPass(t *testing.T) {
	module, err := filepath.Abs(filepath.Join("..", "..", ".."))
	if err != nil {
		t.Fatal(err)
	}
	l := Layout{Repo: filepath.Join(module, "..", "..", ".."), Module: module, Work: t.TempDir(), Self: "/testrun"}
	for _, s := range roster.Suites {
		if s.Kind != roster.GoModule && s.Kind != roster.E2E {
			continue
		}
		t.Run(s.Name, func(t *testing.T) {
			// Act
			u, err := Build(l, s)

			// Assert
			if err != nil {
				t.Fatal(err)
			}
			want := []string{ModuleBuildID(s.Name, "")}
			if s.IntegrationTags != "" {
				want = append(want, ModuleBuildID(s.Name, s.IntegrationTags))
			}
			var got []string
			for _, b := range buildUnits(u) {
				got = append(got, b.ID)
			}
			if !reflect.DeepEqual(got, want) {
				t.Fatalf("%s plans the build units %v, want exactly %v", s.Name, got, want)
			}
			for _, a := range u.Atomic {
				if i := slices.Index(a.Argv, "test"); i > 0 && a.Argv[i-1] == "go" && slices.Contains(a.Argv, "-c") || strings.Contains(strings.Join(a.Argv, " "), "go test -c") {
					t.Errorf("%s builds a binary of its own: %s %v", s.Name, a.ID, a.Argv)
				}
			}
		})
	}
}

func TestPinnedBuildHoldsBuildSlotsPinnedByWidePinnedEnv(t *testing.T) {
	// Arrange
	sp := spec("x:build", "x", "/m", []string{"go", "build"})

	// Act
	got := pinnedBuild(sp)

	// Assert
	if got.Slots != BuildSlots || !reflect.DeepEqual(got.Env, WidePinnedEnv(BuildSlots)) {
		t.Fatalf("pinnedBuild = %d slots pinned by %v, want %d pinned by %v", got.Slots, got.Env, BuildSlots, WidePinnedEnv(BuildSlots))
	}
	if got.ID != sp.ID || !reflect.DeepEqual(got.Argv, sp.Argv) || got.Dir != sp.Dir {
		t.Fatalf("pinnedBuild changed the unit's command: %+v from %+v", got, sp)
	}
}
