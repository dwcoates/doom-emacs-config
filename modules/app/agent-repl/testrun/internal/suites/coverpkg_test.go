package suites

import (
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"

	"agentrepl/testrun/roster"
	"agentrepl/testrun/testenv"
)

// systemTree lays out a go.mod for every Go coverage system under a module
// root: the daemon requires logging and the (non-system) proto module.
func systemTree(t *testing.T) Layout {
	t.Helper()
	root := t.TempDir()
	mods := map[string]string{
		"daemon":                         "module agent-repld\n\ngo 1.24\n\nrequire (\n\tagentrepl/logging v0.0.0 // indirect\n\tagentrepl/proto v0.0.0\n)\n",
		"agent-shim/claude/shim-sidecar": "module agentrepl/shim-claude-sidecar\n\nrequire agentrepl/shim-lock v0.0.0\n",
		"agent-shim/shim-store":          "module agentrepl/shim-store\n",
		"agent-shim/shim-lock":           "module agentrepl/shim-lock\n",
		"agent-shim/logging/go":          "module agentrepl/logging\n",
		"proto/helpers/go":               "module agentrepl/protohelpers\n",
	}
	for dir, body := range mods {
		if err := os.MkdirAll(filepath.Join(root, dir), 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(filepath.Join(root, dir, "go.mod"), []byte(body), 0o644); err != nil {
			t.Fatal(err)
		}
	}
	return Layout{Module: root, Work: "/w", Coverage: true}
}

func TestGoModFileReadsTheModuleAndItsRequires(t *testing.T) {
	// Arrange
	l := systemTree(t)

	// Act
	module, requires, err := goModFile(filepath.Join(l.Module, "daemon"))

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if module != "agent-repld" || !reflect.DeepEqual(requires, []string{"agentrepl/logging", "agentrepl/proto"}) {
		t.Fatalf("module %q, requires %v", module, requires)
	}
}

func TestGoModFileRefuses(t *testing.T) {
	tests := []struct {
		name, body, want string
	}{
		{name: "a missing go.mod", want: "read"},
		{name: "a go.mod naming no module", body: "go 1.24\n", want: "names no module"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			dir := t.TempDir()
			if tt.body != "" {
				if err := os.WriteFile(filepath.Join(dir, "go.mod"), []byte(tt.body), 0o644); err != nil {
					t.Fatal(err)
				}
			}

			// Act
			_, _, err := goModFile(dir)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("err = %v, want %q", err, tt.want)
			}
		})
	}
}

func TestSystemModulesMapsEveryGoCoverageSystem(t *testing.T) {
	// Act
	got, err := SystemModules(systemTree(t))

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	want := map[string]string{
		"agent-repld": "daemon", "agentrepl/shim-claude-sidecar": "sidecar", "agentrepl/shim-store": "store",
		"agentrepl/shim-lock": "lock", "agentrepl/logging": "logging", "agentrepl/protohelpers": "protohelpers",
	}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("SystemModules = %v\nwant %v", got, want)
	}
}

func TestCoverPkgNamesTheModuleAndTheSystemsItRequires(t *testing.T) {
	// Arrange
	l := systemTree(t)

	// Act
	got, err := coverPkg(l, filepath.Join(l.Module, "daemon"))

	// Assert: logging is a system, proto is not.
	if err != nil {
		t.Fatal(err)
	}
	if got != "./...,agentrepl/logging/..." {
		t.Fatalf("coverPkg = %q", got)
	}
}

func TestACoverageRunInstrumentsEverySystemProgramAndNoFake(t *testing.T) {
	// Arrange
	l := systemTree(t)
	var names []string
	for _, p := range roster.Programs {
		names = append(names, p.Name)
	}

	// Act
	specs, err := ProgramUnits(l, names, func(p roster.Program) string { return p.Owner })

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	for _, s := range specs {
		p, _ := roster.LookupProgram(strings.TrimPrefix(s.ID, "program:"))
		covered := strings.Contains(strings.Join(s.Argv, " "), " -cover -coverpkg=./...")
		if covered == p.Fake {
			t.Errorf("%s: instrumented %t, fake %t; argv %v", p.Name, covered, p.Fake, s.Argv)
		}
	}
	for _, s := range specs {
		if s.ID == "program:shim-claude-sidecar" && !strings.Contains(strings.Join(s.Argv, " "), "-coverpkg=./...,agentrepl/shim-lock/...") {
			t.Errorf("the sidecar's build does not cover the lock it requires: %v", s.Argv)
		}
	}
}

func envHas(env []string, kv string) bool {
	for _, e := range env {
		if e == kv {
			return true
		}
	}
	return false
}

func TestSpawnedSystemCoverageReachesTheChunksThatSpawnThem(t *testing.T) {
	tests := []struct {
		name     string
		coverage bool
		programs []string
		want     bool
	}{
		{name: "a coverage run of a package that spawns programs", coverage: true, programs: []string{"agent-repld"}, want: true},
		{name: "a coverage run of a package that spawns nothing", coverage: true, want: false},
		{name: "an ordinary run of a package that spawns programs", coverage: false, programs: []string{"agent-repld"}, want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			module := goPackageFixture(t, "func TestA(t *testing.T) {}\n")
			l := Layout{Module: module, Work: t.TempDir(), Self: "/testrun", Coverage: tt.coverage}
			s := roster.Suite{Name: "m", Programs: map[string][]string{"p": tt.programs}}
			spawned := testenv.Coverage + "=" + SpawnedCovDir(l, "m", 0)

			// Act
			u, err := goModuleUnitsForPackages(l, s, module, []string{"p"})

			// Assert
			if err != nil {
				t.Fatal(err)
			}
			chunk := u.Splits[0].Chunk("m:p#00", []string{"TestA"})
			goCoverDir := "GOCOVERDIR=" + SpawnedCovDir(l, "m", 0)
			if got := envHas(chunk.Env, spawned) && envHas(chunk.Env, testenv.ProgramsInstrumented+"=1") && envHas(chunk.Env, goCoverDir); got != tt.want {
				t.Fatalf("chunk carries spawned coverage = %t, want %t; env %v", got, tt.want, chunk.Env)
			}
			for _, unit := range u.Atomic {
				if unit.ID == "m:p:build" && envHas(unit.Env, spawned) != tt.want {
					t.Fatalf("build carries spawned coverage = %t, want %t", !tt.want, tt.want)
				}
			}
		})
	}
}

func TestACoverageRunsE2EChunksMeasureTheSystemsTheySpawn(t *testing.T) {
	// Arrange
	module := goPackageFixture(t, "func TestA(t *testing.T) {}\n")
	l := Layout{Module: module, Work: t.TempDir(), Self: "/testrun", Coverage: true}
	s := roster.Suite{Name: "e2e", Programs: map[string][]string{"p": {"shim-store"}}}

	// Act
	u, err := e2eUnitsForPackages(l, s, module, []string{"p"})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	chunk := u.Splits[0].Chunk("e2e:p#00", []string{"TestA"})
	if !envHas(chunk.Env, testenv.Coverage+"="+SpawnedCovDir(l, "e2e", 0)) || !envHas(chunk.Env, testenv.ProgramsInstrumented+"=1") {
		t.Fatalf("e2e chunk env %v lacks spawned coverage", chunk.Env)
	}
	if strings.Contains(strings.Join(chunk.Argv, " "), "-test.gocoverdir") {
		t.Fatalf("the e2e module's own code was measured: %v", chunk.Argv)
	}
}

func TestACoverageRunsTaggedIntegrationPassMeasuresTheSystemsItSpawns(t *testing.T) {
	// Arrange: the daemon's integration tier is a build-tagged pass of its own.
	module := goPackageFixture(t, "func TestA(t *testing.T) {}\n")
	l := Layout{Module: module, Work: t.TempDir(), Self: "/testrun", Coverage: true}
	s := roster.Suite{Name: "daemon", IntegrationTags: "integration", Programs: map[string][]string{"p": {"agent-repld"}}}

	// Act
	u, err := goIntegrationUnitsForPackages(l, s, module, []string{"p"})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	chunk := u.Splits[0].Chunk("daemon:p[integration]#00", []string{"TestA"})
	if !envHas(chunk.Env, testenv.Coverage+"="+SpawnedCovDir(l, "daemon-integration", 0)) || !envHas(chunk.Env, testenv.ProgramsInstrumented+"=1") {
		t.Fatalf("the tagged integration chunk's env %v lacks spawned coverage", chunk.Env)
	}
}

func TestGoSuitePackagesListsBothPasses(t *testing.T) {
	// Arrange: p is ordinary; q's tests compile only under -tags integration.
	module := realFixture(t)
	write(t, filepath.Join(module, "q", "q.go"), "package q\n", 0o644)
	write(t, filepath.Join(module, "q", "q_test.go"), "//go:build integration\n\npackage q\nimport \"testing\"\nfunc TestI(t *testing.T) {}\n", 0o644)
	s := roster.Suite{Name: "m", Path: "/", IntegrationTags: "integration", IntegrationPackages: "./q"}
	l := Layout{Repo: module, Module: module}

	// Act
	pkgs, ipkgs, err := GoSuitePackages(l, s)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if !reflect.DeepEqual(pkgs, []string{"p"}) || !reflect.DeepEqual(ipkgs, []string{"q"}) {
		t.Fatalf("packages %v, tagged %v", pkgs, ipkgs)
	}
}

func TestGoSuitePackagesRefusesATagWithNoTaggedPackage(t *testing.T) {
	// Arrange: r has no tests at all.
	module := realFixture(t)
	write(t, filepath.Join(module, "r", "r.go"), "package r\n", 0o644)
	s := roster.Suite{Name: "m", Path: "/", IntegrationTags: "integration", IntegrationPackages: "./r"}

	// Act
	_, _, err := GoSuitePackages(Layout{Repo: module, Module: module}, s)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "no integration-tagged tested packages") {
		t.Fatalf("err = %v", err)
	}
}

// realFixture is goPackageFixture at its real path: go list answers real
// paths, and a temp directory may sit behind a symlink (/var on macOS).
func realFixture(t *testing.T) string {
	t.Helper()
	module, err := filepath.EvalSymlinks(goPackageFixture(t, "func TestA(t *testing.T) {}\n"))
	if err != nil {
		t.Fatal(err)
	}
	return module
}
