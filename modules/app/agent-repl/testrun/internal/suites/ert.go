package suites

import (
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"

	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/roster"
)

// ertAggregator is the file whose load list IS the ERT roster.
const ertAggregator = "test-agent-repl.el"

var ertLoadLine = regexp.MustCompile(`\(load \(expand-file-name "(test-[^"]+\.el)" dir\)`)

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
	if len(files) == 0 || files[0] != "test-helpers.el" {
		return nil, fmt.Errorf("suites: %s must load test-helpers.el first and then every test file; found %v", path, files)
	}
	return files, nil
}

func ertUnits(l Layout, s roster.Suite) (Units, error) {
	lispDir := l.resolve(s.Path)
	files, err := ERTRoster(lispDir)
	if err != nil {
		return Units{}, err
	}
	fakeDir := filepath.Join(lispDir, "testsupport", "fakedaemon")
	fake := filepath.Join(l.Work, "ert", "fakedaemon")
	build := spec(s.Name+":fakedaemon", s.Name, fakeDir,
		// -buildvcs=false: a test build never asks git to stamp the binary
		// (no test runs real git, owner rule).
		[]string{"go", "build", "-buildvcs=false", "-o", fake, "."}, "GOPROXY=off", "GOFLAGS=-mod=mod -p=1")
	driver := filepath.Join(l.Module, "testrun", "ert", "driver.el")
	rosterForm := lispList(files)
	chunk := func(id string, items []string) run.Spec {
		form := fmt.Sprintf("(agent-repl-testrun-ert %s %s %s)",
			strconv.Quote(lispDir+"/"), lispList(items), rosterForm)
		sp := spec(id, s.Name, lispDir,
			[]string{"emacs", "-batch", "-Q", "-l", "ert", "-l", driver, "--eval", form},
			"AGENT_REPL_ITEST_FAKEDAEMON="+fake)
		sp.Items = func(out []byte) (map[string]float64, error) { return ParseItemLines(out, items) }
		return sp
	}
	return Units{
		Atomic: []run.Spec{build},
		Splits: []Split{{Group: s.Name, Suite: s.Name, Items: files, Deps: []string{build.ID}, Chunk: chunk}},
	}, nil
}

func lispList(items []string) string {
	q := make([]string, len(items))
	for i, it := range items {
		q[i] = strconv.Quote(it)
	}
	return "'(" + strings.Join(q, " ") + ")"
}
