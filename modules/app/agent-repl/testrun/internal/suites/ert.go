package suites

import (
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"sort"
	"strconv"
	"strings"

	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/internal/testtimes"
	"agentrepl/testrun/roster"
)

// ertAggregator is the file whose load list IS the ERT roster.
const ertAggregator = "test-agent-repl.el"

var ertLoadLine = regexp.MustCompile(`\(load \(expand-file-name "(test-[^"]+\.el)" dir\)`)

// ertHelpers is the shared helpers file every ERT chunk loads first.
const ertHelpers = "test-helpers.el"

// ERTFiles answers ERT suite s's files in lispDir (its resolved Path): the
// roster its chunks are checked against, helpers first, and the items it
// runs. A suite with its own glob (roster.Suite.ERTFiles) runs exactly the
// files it matches, in name order, beside the helpers it loads but does not
// run. Any other runs every file the aggregator loads, the helpers included,
// and refuses an aggregator that loads a file another ERT suite of the same
// directory runs: that file would run in both.
func ERTFiles(lispDir string, s roster.Suite) (rosterFiles, items []string, err error) {
	if s.ERTFiles != "" {
		matches, err := filepath.Glob(filepath.Join(lispDir, s.ERTFiles))
		if err != nil {
			return nil, nil, fmt.Errorf("suites: %s's files %q: %w", s.Name, s.ERTFiles, err)
		}
		for _, m := range matches {
			items = append(items, filepath.Base(m))
		}
		if len(items) == 0 {
			return nil, nil, fmt.Errorf("suites: %s's files %q match nothing in %s", s.Name, s.ERTFiles, lispDir)
		}
		sort.Strings(items)
		return append([]string{ertHelpers}, items...), items, nil
	}
	files, err := ERTRoster(lispDir)
	if err != nil {
		return nil, nil, err
	}
	for _, other := range roster.Suites {
		if other.Kind != roster.ERT || other.ERTFiles == "" || other.Path != s.Path {
			continue
		}
		for _, f := range files {
			ok, err := filepath.Match(other.ERTFiles, f)
			if err != nil {
				return nil, nil, fmt.Errorf("suites: %s's files %q: %w", other.Name, other.ERTFiles, err)
			}
			if ok {
				return nil, nil, fmt.Errorf("suites: %s loads %s, which the %s suite runs: a file runs in one ERT suite alone", filepath.Join(lispDir, ertAggregator), f, other.Name)
			}
		}
	}
	return files, files, nil
}

// ERTRoster is every test file the aggregator loads, in its order.
func ERTRoster(lispDir string) ([]string, error) {
	path := filepath.Join(lispDir, ertAggregator)
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("suites: read the ERT aggregator: %w", err)
	}
	var files []string
	seen := map[string]bool{}
	for _, m := range ertLoadLine.FindAllSubmatch(data, -1) {
		f := string(m[1])
		if seen[f] {
			return nil, fmt.Errorf("suites: %s loads %s twice", path, f)
		}
		seen[f] = true
		files = append(files, f)
	}
	if len(files) == 0 || files[0] != ertHelpers {
		return nil, fmt.Errorf("suites: %s must load test-helpers.el first and then every test file; found %v", path, files)
	}
	return files, nil
}

func ertUnits(l Layout, s roster.Suite) (Units, error) {
	lispDir := l.resolve(s.Path)
	files, items, err := ERTFiles(lispDir, s)
	if err != nil {
		return Units{}, err
	}
	fakeDir := filepath.Join(lispDir, "testsupport", "fakedaemon")
	fake := filepath.Join(l.Work, "ert", "fakedaemon")
	build := spec(s.Name+":fakedaemon", s.Name, fakeDir,
		// -buildvcs=false: a test build never asks git to stamp the binary
		// (no test runs real git, owner rule).
		[]string{"go", "build", "-buildvcs=false", "-o", fake, "."}, "GOPROXY=off", GoFlagsEnv("-mod=mod"))
	driver := filepath.Join(l.Module, "testrun", "ert", "driver.el")
	rosterForm := lispList(files)
	named := func(id string, items []string, byItem map[string][]string) run.Spec {
		form := fmt.Sprintf("(agent-repl-testrun-ert %s %s %s)",
			strconv.Quote(lispDir+"/"), lispList(items), rosterForm)
		if names := lispNamesByFile(items, byItem); names != "" {
			form = fmt.Sprintf("(agent-repl-testrun-ert %s %s %s %s)",
				strconv.Quote(lispDir+"/"), lispList(items), rosterForm, names)
		}
		sp := spec(id, s.Name, lispDir,
			[]string{"emacs", "-batch", "-Q", "-l", "ert", "-l", driver, "--eval", form},
			"AGENT_REPL_ITEST_FAKEDAEMON="+fake)
		sp.Items = func(out []byte) (map[string]float64, error) { return ParseItemLines(out, items) }
		sp.Tests = func(out []byte, _ bool) ([]testtimes.Test, error) { return testtimes.ParseERT(s.Name, out) }
		return sp
	}
	chunk := func(id string, items []string) run.Spec { return named(id, items, nil) }
	splits := batchSplits(s.Name, s.Name, BatchFiles(items, ERTArea, neverReused(s), PooledArea, s.Batching()), chunk, named)
	for i := range splits {
		splits[i].Deps = []string{build.ID}
		// THE DRIVER SCOPES EACH FILE'S NAMES TO THAT FILE, so two named
		// files of one batch share a chunk (2026-11-13: two -ert FILE=NAMES
		// flags checked every name against each file).
		splits[i].ScopesNamesPerItem = true
	}
	return Units{Atomic: []run.Spec{build}, Splits: splits}, nil
}

// lispNamesByFile is the driver's NAMES argument: an alist of each named
// item of ITEMS to its test symbols, in ITEMS' order, or "" when no item is
// named. An item with no entry runs whole.
func lispNamesByFile(items []string, byItem map[string][]string) string {
	var entries []string
	for _, item := range items {
		if names := byItem[item]; len(names) > 0 {
			entries = append(entries, "(cons "+strconv.Quote(item)+" "+lispSymbols(names)+")")
		}
	}
	if len(entries) == 0 {
		return ""
	}
	return "(list " + strings.Join(entries, " ") + ")"
}

// lispSymbols is a quoted list of symbols, each name interned exactly.
func lispSymbols(names []string) string {
	q := make([]string, len(names))
	for i, name := range names {
		q[i] = "(intern " + strconv.Quote(name) + ")"
	}
	return "(list " + strings.Join(q, " ") + ")"
}

func lispList(items []string) string {
	q := make([]string, len(items))
	for i, it := range items {
		q[i] = strconv.Quote(it)
	}
	return "'(" + strings.Join(q, " ") + ")"
}
