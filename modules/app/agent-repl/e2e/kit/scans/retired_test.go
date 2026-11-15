package scans

import (
	"agentrepl/testrun/gofunc"
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"
)

// STEPS RETIRED TO A SYSTEM'S OWN TIER (owner ruling, 2026-11-13: the
// scenario matrix counts integration-tier coverage).
//
// An e2e step whose assertion one system proves alone is retired into that
// system's integration tier (AGENTS.md, "An e2e test proves a seam between
// real systems, and nothing else"). Once it is gone no e2e test drives its
// scenario, so the matrix would read it uncovered though both halves are
// proven. Section (f) of the document pins each such step: the scenario it
// drove, and its replacement tests in the runner's own selection syntax
// (`-go DIR=TestName`, `-vitest FILE=suite test`, AGENTS.md's
// `agent-repld test run`). A pinned replacement that does not exist fails
// the check, so a renamed or deleted replacement cannot leave the scenario
// counted. A scenario counts as covered by its retirement only when the pins
// prove both halves: one replacement in the shim's tier and one in the
// daemon's.

// retiredHeading opens the document's section of retired steps.
const retiredHeading = "## (f) Steps retired to an integration tier"

// oneTierHeading opens the document's section of steps retired to ONE
// system's tier: their replacements prove one half, so they count nothing
// covered, and the section only pins them, so a replacement renamed or
// deleted fails the check rather than going unnoticed.
const oneTierHeading = "## (g) Steps retired to one system's tier"

// retiredStep is one row of that section.
type retiredStep struct {
	// line is the row's line in the document, 1-based.
	line int
	// step is the retired step's path, Scenario/Xxx.
	step string
	// scenario is the scenario it drove.
	scenario string
	// pins are its replacement tests.
	pins []retiredPin
}

// retiredPin is one replacement test: `-go DIR=TestName` or `-vitest
// FILE=suite test`, paths relative to the agent-repl module.
type retiredPin struct {
	flag, path, name string
}

func (p retiredPin) String() string { return fmt.Sprintf("`%s %s=%s`", p.flag, p.path, p.name) }

// The tiers a replacement may sit in, by the path it names.
const (
	tierShim   = "shim"
	tierDaemon = "daemon"
)

// tier is the system whose tier the pinned test is in, or "".
func (p retiredPin) tier() string {
	switch {
	case p.flag == "-vitest" && strings.HasPrefix(p.path, "agent-shim/claude/shim/"):
		return tierShim
	case p.flag == "-go" && strings.HasPrefix(p.path, "daemon/"):
		return tierDaemon
	}
	return ""
}

// retiredPinForm is one pin in a replacement cell.
var retiredPinForm = regexp.MustCompile("`(-go|-vitest) ([^=` ]+)=([^`]+)`")

// parseRetired reads the document's section of retired steps: every table row
// under retiredHeading, through matrixCells, as table (a)'s rows are read.
func parseRetired(t *testing.T, lines []string) []retiredStep {
	t.Helper()
	return parseRetiredUnder(t, lines, retiredHeading)
}

// parseRetiredUnder reads the retired steps' table under HEADING.
func parseRetiredUnder(t *testing.T, lines []string, heading string) []retiredStep {
	t.Helper()
	var out []retiredStep
	in := false
	for i, line := range lines {
		if strings.HasPrefix(line, heading) {
			in = true
			continue
		}
		if !in {
			continue
		}
		if strings.HasPrefix(line, "## ") {
			break
		}
		if !strings.HasPrefix(line, "| `") {
			continue
		}
		cells := matrixCells(line)
		if len(cells) != 3 {
			t.Fatalf("%s:%d: a retired step's row has %d cells, want 3 (step, scenario, replacements): %s", matrixPath, i+1, len(cells), line)
		}
		step := retiredStep{line: i + 1, step: strings.Trim(cells[0], "`"), scenario: parseScenarioCell(cells[1])}
		for _, m := range retiredPinForm.FindAllStringSubmatch(cells[2], -1) {
			step.pins = append(step.pins, retiredPin{flag: m[1], path: m[2], name: m[3]})
		}
		out = append(out, step)
	}
	return out
}

// checkRetired answers, by scenario, the pins of every retired step whose
// pins prove both halves, and fails the test for every row that is not one:
// a scenario the vendor does not register, a step that is still live, a pin
// naming no tier or no test that exists, or a row missing a half.
func checkRetired(t errorReporter, steps []retiredStep, canonical map[string]bool, live func(step string) bool, moduleDir string) map[string][]retiredPin {
	t.Helper()
	out := map[string][]retiredPin{}
	for _, s := range steps {
		where := fmt.Sprintf("%s:%d", matrixPath, s.line)
		tiers, ok := checkPins(t, s, canonical, live, moduleDir)
		for _, tier := range []string{tierShim, tierDaemon} {
			if !tiers[tier] {
				t.Errorf("%s: %s names no replacement in the %s's tier: a retired step counts its scenario covered only when its pins prove both halves", where, s.step, tier)
				ok = false
			}
		}
		if ok {
			out[s.scenario] = append(out[s.scenario], s.pins...)
		}
	}
	return out
}

// checkPins is what every retired step's row must be, in either section: its
// scenario one the vendor registers, its step no longer live, and every pin
// an existing test in the shim's tier or the daemon's. It fails the test for
// each fault, and answers the tiers the existing pins sit in and whether the
// row had no fault.
func checkPins(t errorReporter, s retiredStep, canonical map[string]bool, live func(step string) bool, moduleDir string) (map[string]bool, bool) {
	t.Helper()
	where := fmt.Sprintf("%s:%d", matrixPath, s.line)
	ok := true
	if !canonical[s.scenario] {
		t.Errorf("%s: retired step %s names %q, which the mocked vendor does not register", where, s.step, s.scenario)
		ok = false
	}
	if live(s.step) {
		t.Errorf("%s: %s is still a step of its scenario, so it is not retired", where, s.step)
		ok = false
	}
	tiers := map[string]bool{}
	for _, p := range s.pins {
		tier := p.tier()
		if tier == "" {
			t.Errorf("%s: %s names %s, which is in neither the shim's tier (-vitest agent-shim/claude/shim/...) nor the daemon's (-go daemon/...)", where, s.step, p)
			ok = false
			continue
		}
		if err := pinnedTestExists(moduleDir, p); err != nil {
			t.Errorf("%s: %s names %s: %v", where, s.step, p, err)
			ok = false
			continue
		}
		tiers[tier] = true
	}
	return tiers, ok
}

// checkOneTier fails the test for every row of the one-tier section that
// is not one: a fault checkPins finds, a row pinning no replacement, or a
// row whose pins prove both halves, which belongs in section (f), where it
// counts its scenario covered.
func checkOneTier(t errorReporter, steps []retiredStep, canonical map[string]bool, live func(step string) bool, moduleDir string) {
	t.Helper()
	for _, s := range steps {
		where := fmt.Sprintf("%s:%d", matrixPath, s.line)
		if len(s.pins) == 0 {
			t.Errorf("%s: %s pins no replacement test", where, s.step)
			continue
		}
		tiers, _ := checkPins(t, s, canonical, live, moduleDir)
		if tiers[tierShim] && tiers[tierDaemon] {
			t.Errorf("%s: %s's pins prove both halves; move its row to section (f), where it counts %s covered", where, s.step, s.scenario)
		}
	}
}

// errorReporter is the part of *testing.T checkRetired reports through.
type errorReporter interface {
	Helper()
	Errorf(format string, args ...any)
}

// pinnedTestExists reports why the pinned test is not there, or nil.
func pinnedTestExists(moduleDir string, p retiredPin) error {
	switch p.flag {
	case "-go":
		return goTestExists(filepath.Join(moduleDir, p.path), p.name)
	case "-vitest":
		return vitestTestExists(filepath.Join(moduleDir, p.path), p.name)
	}
	return fmt.Errorf("unknown selection %s", p.flag)
}

// goTestExists reports whether a test file of the Go package in dir, under any
// build tags, declares name as a top-level test (func TestXxx(t *testing.T)).
func goTestExists(dir, name string) error {
	files, err := filepath.Glob(filepath.Join(dir, "*_test.go"))
	if err != nil {
		return err
	}
	if len(files) == 0 {
		return fmt.Errorf("%s holds no Go test file", dir)
	}
	fset := token.NewFileSet()
	for _, path := range files {
		f, err := parser.ParseFile(fset, path, nil, 0)
		if err != nil {
			return fmt.Errorf("parse %s: %w", path, err)
		}
		for _, decl := range f.Decls {
			if fn, ok := decl.(*ast.FuncDecl); ok && fn.Name.Name == name && gofunc.IsTestFunc(fn, "Test", "T") {
				return nil
			}
		}
	}
	return fmt.Errorf("no test file in %s declares %s", dir, name)
}

// vitestCall is the call a test name literal opens: a describe block or a
// test, as the mask ends just before the literal.
var vitestCall = regexp.MustCompile(`\b(describe|test|it)(\.\w+)*\(\s*$`)

// vitestTestExists reports whether the vitest file names a test whose full
// name (the name of the describe block opened last before it, a space, its
// own) is name, read through scanTS as the webapp layer's files are.
func vitestTestExists(path, name string) error {
	src, err := os.ReadFile(path)
	if err != nil {
		return err
	}
	ts := scanTS(string(src))
	var suites, tests []string
	for _, lit := range ts.literals {
		if !lit.plain {
			continue
		}
		m := vitestCall.FindStringSubmatch(ts.mask[:lit.start])
		if m == nil {
			continue
		}
		if m[1] == "describe" {
			suites = append(suites, lit.value)
			continue
		}
		tests = append(tests, lit.value)
		// A test belongs to the describe block opened last before it.
		full := lit.value
		if len(suites) > 0 {
			full = suites[len(suites)-1] + " " + lit.value
		}
		if full == name {
			return nil
		}
	}
	return fmt.Errorf("%s names no test %q (%d describe blocks, %d tests)", path, name, len(suites), len(tests))
}
