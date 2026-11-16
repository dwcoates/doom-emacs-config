package reuse

import (
	"errors"
	"path/filepath"
	"reflect"
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
	s := roster.Suite{Name: "whole-module-scan", Kind: roster.Script, Path: "bin/test-x.sh", NeverReused: "a scan"}

	// Act
	su, err := Derive(f.d, s, suites.Units{Atomic: []run.Spec{spec("whole-module-scan")}}, true)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if len(su.Units) != 1 || su.Units[0].Never != "a scan" || su.Units[0].Set != nil || su.Envelope != nil {
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
	su, err := Derive(f.d, lockSuite, u, false)

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

func TestDeriveGoKeysASubtestSelectionOnItsPackagesTestBinary(t *testing.T) {
	// Arrange: a selection narrowed to one subtest groups it under its path.
	f := newFixture(t)
	write(t, f.d.Repo, fixtureModule+"/lock/dep/dep.go", "package dep")
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
	u := lockUnits()
	u.Splits[0].Group = "lock:./TestLock/a_case"

	// Act
	su, err := Derive(f.d, lockSuite, u, false)

	// Assert
	if err != nil {
		t.Fatalf("Derive = %v, want the subtest keyed on its package's test binary", err)
	}
	if set := unitsByName(su)["./TestLock/a_case"].Set; set == nil || !set.Contains(fixtureModule+"/lock/dep/dep.go") {
		t.Fatalf("the subtest's set does not place the package's test binary's links")
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
		"/web/vitest.config.ts":                 `import { a } from "./alias.ts"; export default { test: { setupFiles: ["./test/setup.ts"], globalSetup: "./test/global.ts" } }`,
		"/web/test/global.ts":                   "export default () => {}",
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
	// Each file is a batch of its own, named by the file (suites.BatchFiles
	// for files the roster never reuses), so each unit is one file's.
	split := func(g, file string) suites.Split {
		return suites.Split{Group: g, Items: []string{file}, Chunk: func(id string, _ []string) run.Spec { return spec(id) }}
	}
	return suites.Units{
		Atomic: []run.Spec{spec("web:typecheck"), spec("web:knip"), spec("web:integration-build")},
		Splits: []suites.Split{split("web/test/a.test.ts", "test/a.test.ts"), split("web/test/b.test.ts", "test/b.test.ts"),
			split("web[integration]/test/integration/i.test.ts", "test/integration/i.test.ts")},
	}
}

// webBatchUnits is webUnits with test/a.test.ts and test/b.test.ts in one
// batch, "both".
func webBatchUnits() suites.Units {
	u := webUnits()
	u.Splits = append([]suites.Split{{Group: "web/both", Items: []string{"test/a.test.ts", "test/b.test.ts"}, Chunk: u.Splits[0].Chunk}}, u.Splits[2:]...)
	return u
}

func TestDeriveVitestKeysABatchOnEveryMembersGraph(t *testing.T) {
	// Arrange
	f := newFixture(t)
	s := webFixture(t, f)

	// Act
	su, err := Derive(f.d, s, webBatchUnits(), true)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	both, ok := unitsByName(su)["both"]
	if !ok || !slices.Equal(both.Members, []string{"test/a.test.ts", "test/b.test.ts"}) {
		t.Fatalf("units = %+v, want the batch both holding both files", su.Units)
	}
	for _, p := range []string{"test/a.test.ts", "src/a.ts", "test/b.test.ts", "src/b.ts", "vitest.config.ts"} {
		if !both.Set.Contains(fixtureModule + "/web/" + p) {
			t.Errorf("the batch's set does not place %s", p)
		}
	}
	if !strings.Contains(both.Desc, `members=["test/a.test.ts" "test/b.test.ts"]`) {
		t.Errorf("the batch's facts %q do not name its members", both.Desc)
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
	for _, p := range []string{"test/a.test.ts", "src/a.ts", "vitest.config.ts", "alias.ts", "test/setup.ts", "test/global.ts", "src/dom.ts", "package.json"} {
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
	for _, d := range []string{"web", "web/src", "web/test", "web/test/integration"} {
		if !slices.Contains(a.Dirs(), fixtureModule+"/"+d) {
			t.Errorf("a.test.ts's set does not list %s, which vitest's discovery walks", d)
		}
	}
	if slices.Contains(a.Dirs(), fixtureModule+"/web/node_modules") {
		t.Error("a.test.ts's set lists the generated node_modules")
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
	for _, check := range []string{"typecheck", "knip"} {
		set := byName[check].Set
		if set == nil || !set.Contains(fixtureModule+"/web/src/b.ts") || !set.Contains(fixtureModule+"/web/test/integration/unimported.json") {
			t.Errorf("the %s does not read the package's whole tree", check)
		}
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
		{"a spec of no known kind", func(_ fixture, u *suites.Units) { u.Atomic = append(u.Atomic, spec("web:mystery")) }, "is no file's, typecheck's, knip's or build's"},
		{"a group of no batch", func(_ fixture, u *suites.Units) { u.Splits[0].Group = "web" }, "is no batch group of web"},
		{"one file in two batches", func(_ fixture, u *suites.Units) {
			u.Splits = append(u.Splits, suites.Split{Group: "web[integration]/again", Items: []string{"test/a.test.ts"}})
		}, "runs test/a.test.ts in two batches"},
		{"two splits cut into one batch", func(_ fixture, u *suites.Units) {
			u.Splits = append(u.Splits, suites.Split{Group: "web[integration]/test/a.test.ts", Items: []string{"test/c.test.ts"}})
		}, "cuts two splits into the batch test/a.test.ts"},
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
	// Each file is a batch of its own, named by the file.
	split := func(g, file string) suites.Split {
		return suites.Split{Group: g, Items: []string{file}, Deps: []string{"ert:fakedaemon"}, Chunk: func(id string, _ []string) run.Spec { return spec(id) }}
	}
	return suites.Units{Atomic: []run.Spec{spec("ert:fakedaemon")}, Splits: []suites.Split{split("ert/test-a.el", "test-a.el"), split("ert/test-b.el", "test-b.el")}}
}

func TestDeriveERTKeysABatchOnWhatEveryMemberLoads(t *testing.T) {
	// Arrange
	f := newFixture(t)
	s := lispFixture(t, f)
	u := lispUnits()
	u.Splits = []suites.Split{{Group: "ert/pooled", Items: []string{"test-a.el", "test-b.el"}, Deps: []string{"ert:fakedaemon"}, Chunk: u.Splits[0].Chunk}}

	// Act
	su, err := Derive(f.d, s, u, true)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	pooled, ok := unitsByName(su)["pooled"]
	if !ok || !slices.Equal(pooled.Members, []string{"test-a.el", "test-b.el"}) {
		t.Fatalf("units = %+v, want the batch pooled holding both files", su.Units)
	}
	for _, p := range []string{"/lisp/test-a.el", "/lisp/a-fixtures.el", "/lisp/test-b.el"} {
		if !pooled.Set.Contains(fixtureModule + p) {
			t.Errorf("the batch's set does not place %s", p)
		}
	}
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
	if !slices.Contains(a.Dirs(), fixtureModule+"/lisp") {
		t.Error("test-a.el's set does not list the suite's directory, Emacs's default directory")
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
		{"a group of no batch", func(_ fixture, u *suites.Units) { u.Splits[0].Group = "ert" }, "is no batch group of ert"},
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

func TestCheckDeclaredUnitsTakesAFileInsideABatchForAUnitOfTheSuite(t *testing.T) {
	tests := []struct {
		name     string
		declared string
		wantErr  bool
	}{
		{name: "a member of a batch", declared: "test/a.test.ts"},
		{name: "the batch itself", declared: "both"},
		{name: "a file no batch holds", declared: "test/gone.test.ts", wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s := roster.Suite{Name: "web", Reuse: &roster.Reuse{Units: map[string][]string{tt.declared: {"x"}}}}
			units := []Unit{{Suite: "web", Name: "both", Members: []string{"test/a.test.ts", "test/b.test.ts"}}}

			// Act
			err := checkDeclaredUnits(s, units)

			// Assert
			if (err != nil) != tt.wantErr {
				t.Fatalf("checkDeclaredUnits = %v, want an error %v", err, tt.wantErr)
			}
		})
	}
}

func TestRunnerPackagesLeaveOutTheRoster(t *testing.T) {
	tests := []struct {
		name    string
		listed  []GoPackage
		listErr error
		want    []string
		wantErr string
	}{
		{name: "the roster is left out and the rest kept",
			listed: []GoPackage{{ImportPath: "agentrepl/testrun"}, {ImportPath: rosterPackage}, {ImportPath: "agentrepl/testrun/internal/plan"}},
			want:   []string{"agentrepl/testrun", "agentrepl/testrun/internal/plan"}},
		{name: "a failed listing fails", listErr: errors.New("go list broke"), wantErr: "go list broke"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.d.GoList = func(string, string, bool, ...string) ([]GoPackage, error) { return tt.listed, tt.listErr }

			// Act
			pkgs, err := runnerPackages(f.d)

			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("runnerPackages = %v, %v; want %q", pkgs, err, tt.wantErr)
				}
				return
			}
			var got []string
			for _, p := range pkgs {
				got = append(got, p.ImportPath)
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("runnerPackages = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestAUnitsDescriptionHoldsItsOwnSuitesWholeEntry(t *testing.T) {
	tests := []struct {
		name string
		edit func(*roster.Suite)
	}{
		{name: "its width", edit: func(s *roster.Suite) { s.Slots = 4 }},
		{name: "a unit it never reuses", edit: func(s *roster.Suite) { s.Reuse.Never = map[string]string{".": "reads the clock"} }},
		{name: "its scripts", edit: func(s *roster.Suite) { s.Reuse.Scripts = []string{"bin"} }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			before := lockSuite
			after := lockSuite
			reuse := *lockSuite.Reuse
			after.Reuse = &reuse
			tt.edit(&after)

			// Act
			was, now := unitDesc(before, "."), unitDesc(after, ".")

			// Assert
			if was == now {
				t.Fatalf("unitDesc = %q both before and after the edit; want the entry's change to move it", now)
			}
		})
	}
}

func TestAUnitsDescriptionIsTheSameForTheSameEntry(t *testing.T) {
	// Arrange: an equal entry built separately, its maps filled in another order.
	a := roster.Suite{Name: "s", Reuse: &roster.Reuse{Units: map[string][]string{"x": {"a"}, "y": {"b"}}}}
	b := roster.Suite{Name: "s", Reuse: &roster.Reuse{Units: map[string][]string{"y": {"b"}, "x": {"a"}}}}

	// Act
	da, db := unitDesc(a, "x"), unitDesc(b, "x")

	// Assert
	if da != db {
		t.Fatalf("unitDesc = %q and %q for one entry; want them equal, with no pointer in either", da, db)
	}
}
