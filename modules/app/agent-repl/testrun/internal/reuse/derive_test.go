package reuse

import (
	"errors"
	"path/filepath"
	"slices"
	"strings"
	"testing"

	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/internal/suites"
	"agentrepl/testrun/roster"
)

// unitsByName indexes su's units.
func unitsByName(su SuiteUnits) map[string]Unit {
	out := map[string]Unit{}
	for _, u := range su.Units {
		out[u.Name] = u
	}
	return out
}

func TestDeriveAScriptSuiteIsOneWholeUnit(t *testing.T) {
	// Arrange
	f := newFixture(t)

	// Act
	su, err := Derive(f.d, harnessSuite, harnessUnits(), true)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if len(su.Units) != 1 || su.Units[0].Name != "" || !slices.Equal(su.Units[0].Specs, []string{"harness"}) {
		t.Fatalf("units = %+v, want the one whole-suite unit", su.Units)
	}
	if !su.Units[0].Set.Contains(fixtureModule+"/bin/test-x.sh") || !su.Envelope.Contains(fixtureModule+"/bin/lib-x.sh") {
		t.Fatalf("set = %+v, want the script and its declared input", su.Units[0].Set)
	}
}

func TestDeriveANeverReusedSuiteIsOneUnkeyedUnit(t *testing.T) {
	// Arrange
	f := newFixture(t)
	s := roster.Suite{Name: "e2e-emacs", Kind: roster.Script, Path: "bin/test-x.sh", NeverReused: "docker"}

	// Act
	su, err := Derive(f.d, s, suites.Units{Atomic: []run.Spec{spec("e2e-emacs")}}, true)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if len(su.Units) != 1 || su.Units[0].Never != "docker" || su.Units[0].Set != nil || su.Envelope != nil {
		t.Fatalf("units = %+v, want one never-reused unit with no inputs", su)
	}
}

func TestDeriveGoCutsAModuleIntoPackagesVetsAndInfra(t *testing.T) {
	// Arrange
	f := newFixture(t)
	write(t, f.d.Repo, fixtureModule+"/lock/x/x.go", "package x")
	lockDir := filepath.Join(f.d.Module, "lock")
	f.d.GoList = func(dir, tags string, tests bool, patterns ...string) ([]GoPackage, error) {
		pkgs, err := fakeGoList(dir, tags, tests, patterns...)
		if dir == lockDir && tests {
			mod := &GoModule{GoMod: filepath.Join(lockDir, "go.mod")}
			x := filepath.Join(lockDir, "x")
			pkgs = append(pkgs, GoPackage{ImportPath: "lock/x", Dir: x, Module: mod}, GoPackage{ImportPath: "lock/x.test", Dir: x, Module: mod, Deps: []string{"lock/x"}})
		}
		return pkgs, err
	}
	u := lockUnits()
	u.Atomic = append(u.Atomic, spec("lock:vet[perf]"), spec("lock:build[integration]"), spec("lock:x:compiled"), spec("lock:coverage"))

	// Act
	su, err := Derive(f.d, lockSuite, u, true)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	byName := unitsByName(su)
	if !slices.Equal(byName["."].Specs, []string{"lock:."}) {
		t.Errorf("the root package's specs = %v", byName["."].Specs)
	}
	if !slices.Equal(byName["x"].Specs, []string{"lock:x:compiled"}) {
		t.Errorf("a compiled-only package's specs = %v", byName["x"].Specs)
	}
	for _, vet := range []string{"vet", "vet[perf]"} {
		if byName[vet].Set == nil {
			t.Errorf("%s has no input set", vet)
		}
	}
	if byName["coverage"].Never == "" {
		t.Error("the coverage report is keyed")
	}
	if !slices.Equal(su.Infra, []string{"lock:build", "lock:build[integration]"}) {
		t.Errorf("infra = %v, want the module builds", su.Infra)
	}
	if !byName["."].Set.Contains(fixtureModule + "/prompts/p.md") {
		t.Error("the root package's declared input is not in its set")
	}
	if byName["vet"].Set.Contains(fixtureModule + "/prompts/p.md") {
		t.Error("a unit's declared input leaked into another unit's set")
	}
}

func TestDeriveGoPlacesThePackagesATestBinaryLinksAndTheProgramsItStarts(t *testing.T) {
	// Arrange
	f := newFixture(t)
	write(t, f.d.Repo, fixtureModule+"/lock/dep/dep.go", "package dep")
	write(t, f.d.Repo, fixtureModule+"/zettel/go.mod", "module zettel")
	write(t, f.d.Repo, fixtureModule+"/zettel/main.go", "package main")
	lockDir := filepath.Join(f.d.Module, "lock")
	f.d.GoList = func(dir, tags string, tests bool, patterns ...string) ([]GoPackage, error) {
		if dir != lockDir || !tests {
			return fakeGoList(dir, tags, tests, patterns...)
		}
		mod := &GoModule{GoMod: filepath.Join(lockDir, "go.mod")}
		return []GoPackage{
			{ImportPath: "lock", Dir: lockDir, Module: mod},
			{ImportPath: "lock/dep", Dir: filepath.Join(lockDir, "dep"), Module: mod},
			{ImportPath: "lock.test", Dir: lockDir, Module: mod, Deps: []string{"lock", "lock/dep"}},
		}, nil
	}
	s := lockSuite
	s.Programs = map[string][]string{".": {"zettel"}}

	// Act
	su, err := Derive(f.d, s, lockUnits(), true)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	set := unitsByName(su)["."].Set
	for _, p := range []string{"/lock/dep/dep.go", "/zettel/main.go", "/testrun/main.go"} {
		if !set.Contains(fixtureModule + p) {
			t.Errorf("the package's set does not place %s", p)
		}
	}
}

func TestDeriveGoFailures(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(f *fixture, s *roster.Suite, u *suites.Units)
		want    string
	}{
		{"a spec of no known kind", func(_ *fixture, _ *roster.Suite, u *suites.Units) {
			u.Atomic = append(u.Atomic, spec("lock:mystery"))
		}, "is no package's, vet's or build's"},
		{"a group of another suite", func(_ *fixture, _ *roster.Suite, u *suites.Units) {
			u.Splits[0].Group = "other:."
		}, "no package group of lock"},
		{"a listing without the test binary", func(f *fixture, _ *roster.Suite, _ *suites.Units) {
			f.d.GoList = func(dir, tags string, _ bool, patterns ...string) ([]GoPackage, error) {
				return fakeGoList(dir, tags, false, patterns...)
			}
		}, "holds no test binary lock.test"},
		{"a program the roster does not declare", func(_ *fixture, s *roster.Suite, _ *suites.Units) {
			s.Programs = map[string][]string{".": {"nope"}}
		}, `runs the program "nope"`},
		{"a declared unit the suite does not have", func(_ *fixture, s *roster.Suite, _ *suites.Units) {
			s.Reuse = &roster.Reuse{Units: map[string][]string{"gone": {"prompts/"}}}
		}, "names gone, which is no unit of the suite"},
		{"a listing that fails", func(f *fixture, _ *roster.Suite, _ *suites.Units) {
			f.d.GoList = func(string, string, bool, ...string) ([]GoPackage, error) { return nil, errors.New("go list broke") }
		}, "go list broke"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			s, u := lockSuite, lockUnits()
			tt.arrange(&f, &s, &u)

			// Act
			_, err := Derive(f.d, s, u, true)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("err = %v, want %q", err, tt.want)
			}
		})
	}
}

func TestATargetedDerivationDoesNotCheckTheDeclaredUnits(t *testing.T) {
	// Arrange
	f := newFixture(t)
	s := lockSuite
	s.Reuse = &roster.Reuse{Units: map[string][]string{"elsewhere": {"prompts/"}}}

	// Act
	_, err := Derive(f.d, s, lockUnits(), false)

	// Assert
	if err != nil {
		t.Fatalf("err = %v, want a narrowed suite's missing units allowed", err)
	}
}

func TestGoGroupUnit(t *testing.T) {
	tests := []struct {
		group, name, rel, tags string
		wantErr                bool
	}{
		{group: "daemon:internal/merge", name: "internal/merge", rel: "internal/merge"},
		{group: "daemon:integration[integration]", name: "integration[integration]", rel: "integration", tags: "integration"},
		{group: "daemon:.", name: ".", rel: "."},
		{group: "sidecar:x", wantErr: true},
		{group: "daemon:", wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.group, func(t *testing.T) {
			// Act
			name, rel, tags, err := goGroupUnit("daemon", tt.group)

			// Assert
			if (err != nil) != tt.wantErr {
				t.Fatalf("err = %v, want error %v", err, tt.wantErr)
			}
			if !tt.wantErr && (name != tt.name || rel != tt.rel || tags != tt.tags) {
				t.Fatalf("goGroupUnit = %q %q %q, want %q %q %q", name, rel, tags, tt.name, tt.rel, tt.tags)
			}
		})
	}
}

// webFixture adds a vitest package web/ to f's checkout: two test files
// importing their own sources, a config naming a setup file, and the
// runner's reporter.
func webFixture(t *testing.T, f fixture) roster.Suite {
	t.Helper()
	for p, body := range map[string]string{
		"/web/package.json":                     "{}",
		"/web/vitest.config.ts":                 `import { a } from "./alias.ts"; export default { test: { setupFiles: ["./test/setup.ts"] } }`,
		"/web/vitest.integration.config.ts":     `import base from "./vitest.config"; export default base`,
		"/web/alias.ts":                         "export const a = 1",
		"/web/test/setup.ts":                    `import "../src/dom";`,
		"/web/src/dom.ts":                       "export {}",
		"/web/src/a.ts":                         "export const a = 1",
		"/web/src/b.ts":                         "export const b = 1",
		"/web/test/a.test.ts":                   `import { a } from "../src/a.js";`,
		"/web/test/b.test.ts":                   `import { b } from "../src/b";`,
		"/web/test/integration/i.test.ts":       `import { a } from "../../src/a";`,
		"/testrun/vitest/items-reporter.mjs":    "r",
		"/web/node_modules/dep/index.js":        "x",
		"/web/test/integration/fixture.json":    "{}",
		"/web/test/integration/unimported.json": "{}",
	} {
		write(t, f.d.Repo, fixtureModule+p, body)
	}
	return roster.Suite{Name: "web", Kind: roster.Vitest, Path: "web", IntegrationConfig: "vitest.integration.config.ts",
		Reuse: &roster.Reuse{Generated: []string{"web/node_modules/"}, Units: map[string][]string{"test/integration/i.test.ts": {"web/test/integration/fixture.json"}}}}
}

func webUnits() suites.Units {
	split := func(g string) suites.Split {
		return suites.Split{Group: g, Chunk: func(id string, _ []string) run.Spec { return spec(id) }}
	}
	return suites.Units{
		Atomic: []run.Spec{spec("web:typecheck"), spec("web:integration-build")},
		Splits: []suites.Split{split("web/test/a.test.ts"), split("web/test/b.test.ts"), split("web[integration]/test/integration/i.test.ts")},
	}
}

func TestDeriveVitestKeysEachFileOnItsOwnImportGraph(t *testing.T) {
	// Arrange
	f := newFixture(t)
	s := webFixture(t, f)

	// Act
	su, err := Derive(f.d, s, webUnits(), true)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	byName := unitsByName(su)
	a := byName["test/a.test.ts"].Set
	for _, p := range []string{"test/a.test.ts", "src/a.ts", "vitest.config.ts", "alias.ts", "test/setup.ts", "src/dom.ts", "package.json"} {
		if !a.Contains(fixtureModule + "/web/" + p) {
			t.Errorf("a.test.ts's set does not place %s", p)
		}
	}
	if !a.Contains(fixtureModule + "/testrun/vitest/items-reporter.mjs") {
		t.Error("a.test.ts's set does not place the runner's reporter")
	}
	if a.Contains(fixtureModule+"/web/src/b.ts") || a.Contains(fixtureModule+"/web/test/b.test.ts") {
		t.Error("a.test.ts's set places another file's graph")
	}
	i := byName["test/integration/i.test.ts"].Set
	for _, p := range []string{"vitest.integration.config.ts", "vitest.config.ts", "test/setup.ts", "test/integration/fixture.json"} {
		if !i.Contains(fixtureModule + "/web/" + p) {
			t.Errorf("the integration file's set does not place %s", p)
		}
	}
	if i.Contains(fixtureModule + "/web/test/integration/unimported.json") {
		t.Error("the integration file's set places what nothing names")
	}
	tc := byName["typecheck"].Set
	if !tc.Contains(fixtureModule+"/web/src/b.ts") || !tc.Contains(fixtureModule+"/web/test/integration/unimported.json") {
		t.Error("the typecheck does not read the package's whole tree")
	}
	if !slices.Equal(su.Infra, []string{"web:integration-build"}) {
		t.Errorf("infra = %v", su.Infra)
	}
	if byName["test/a.test.ts"].Desc == byName["test/integration/i.test.ts"].Desc {
		t.Error("a unit file's facts do not name its config")
	}
}

func TestDeriveVitestFailures(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(f fixture, u *suites.Units)
		want    string
	}{
		{"a spec of no known kind", func(_ fixture, u *suites.Units) { u.Atomic = append(u.Atomic, spec("web:mystery")) }, "is no file's, typecheck's or build's"},
		{"a group of no file", func(_ fixture, u *suites.Units) { u.Splits[0].Group = "web" }, "is no file group of web"},
		{"one file under two configs", func(_ fixture, u *suites.Units) {
			u.Splits = append(u.Splits, suites.Split{Group: "web[integration]/test/a.test.ts"})
		}, "runs test/a.test.ts under two configs"},
		{"a setup file that is not there", func(f fixture, _ *suites.Units) {
			write(t, f.d.Repo, fixtureModule+"/web/vitest.config.ts", `export default { test: { setupFiles: ["./test/gone.ts"] } }`)
		}, "names the setup file"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			s := webFixture(t, f)
			u := webUnits()
			tt.arrange(f, &u)

			// Act
			_, err := Derive(f.d, s, u, true)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("err = %v, want %q", err, tt.want)
			}
		})
	}
}

// lispFixture adds an ERT suite lisp/ to f's checkout: config.el loading one
// module, the helpers, two test files of which one loads a sibling, the
// runner's driver and the fake daemon.
func lispFixture(t *testing.T, f fixture) roster.Suite {
	t.Helper()
	for p, body := range map[string]string{
		"/config.el":                          `(agent-repl--load-module "core")`,
		"/lisp/core.el":                       "(provide 'core)",
		"/lisp/test-helpers.el":               `(load (expand-file-name "../config.el"))`,
		"/lisp/test-a.el":                     `(load (expand-file-name "test-helpers.el" dir)) (load "a-fixtures.el")`,
		"/lisp/a-fixtures.el":                 "(defvar a 1)",
		"/lisp/test-b.el":                     "(ert-deftest b ())",
		"/lisp/testsupport/fakedaemon/go.mod": "module fakedaemon",
		"/testrun/ert/driver.el":              "d",
	} {
		write(t, f.d.Repo, fixtureModule+p, body)
	}
	return roster.Suite{Name: "ert", Kind: roster.ERT, Path: "lisp", Reuse: &roster.Reuse{}}
}

func lispUnits() suites.Units {
	split := func(g string) suites.Split {
		return suites.Split{Group: g, Deps: []string{"ert:fakedaemon"}, Chunk: func(id string, _ []string) run.Spec { return spec(id) }}
	}
	return suites.Units{Atomic: []run.Spec{spec("ert:fakedaemon")}, Splits: []suites.Split{split("ert/test-a.el"), split("ert/test-b.el")}}
}

func TestDeriveERTKeysEachFileOnWhatItLoads(t *testing.T) {
	// Arrange
	f := newFixture(t)
	s := lispFixture(t, f)

	// Act
	su, err := Derive(f.d, s, lispUnits(), true)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	byName := unitsByName(su)
	a, b := byName["test-a.el"].Set, byName["test-b.el"].Set
	for _, p := range []string{"/config.el", "/lisp/core.el", "/lisp/test-helpers.el", "/lisp/test-a.el", "/lisp/a-fixtures.el", "/testrun/ert/driver.el", "/lisp/testsupport/fakedaemon/go.mod"} {
		if !a.Contains(fixtureModule + p) {
			t.Errorf("test-a.el's set does not place %s", p)
		}
	}
	if b.Contains(fixtureModule+"/lisp/test-a.el") || b.Contains(fixtureModule+"/lisp/a-fixtures.el") {
		t.Error("test-b.el's set places what only test-a.el loads")
	}
	if !slices.Equal(su.Infra, []string{"ert:fakedaemon"}) {
		t.Errorf("infra = %v", su.Infra)
	}
}

func TestDeriveERTFailures(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(f fixture, u *suites.Units)
		want    string
	}{
		{"a spec of no known kind", func(_ fixture, u *suites.Units) { u.Atomic = append(u.Atomic, spec("ert:mystery")) }, "is no file's or the fake daemon's build"},
		{"a group of no file", func(_ fixture, u *suites.Units) { u.Splits[0].Group = "ert" }, "is no file group of ert"},
		{"a module config.el loads that is not there", func(f fixture, _ *suites.Units) {
			write(t, f.d.Repo, fixtureModule+"/config.el", `(agent-repl--load-module "gone")`)
		}, "loads the module gone"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			s := lispFixture(t, f)
			u := lispUnits()
			tt.arrange(f, &u)

			// Act
			_, err := Derive(f.d, s, u, true)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("err = %v, want %q", err, tt.want)
			}
		})
	}
}

func TestTheEnvelopeHoldsEveryUnitsInputs(t *testing.T) {
	// Arrange
	f := newFixture(t)
	s := lispFixture(t, f)

	// Act
	su, err := Derive(f.d, s, lispUnits(), true)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if !su.Envelope.Contains(fixtureModule + "/config.el") {
		t.Fatal("the envelope does not hold a unit's input")
	}
}
