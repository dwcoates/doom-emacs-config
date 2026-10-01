package suites

import (
	"bufio"
	"bytes"
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
		[]string{"go", "build", "-o", fake, "."}, "GOPROXY=off", "GOFLAGS=-mod=mod -p=1")
	driver := filepath.Join(l.Module, "testrun", "ert", "driver.el")
	rosterForm := lispList(files)
	chunk := func(id string, items []string) run.Spec {
		form := fmt.Sprintf("(agent-repl-testrun-ert %s %s %s)",
			strconv.Quote(lispDir+"/"), lispList(items), rosterForm)
		sp := spec(id, s.Name, lispDir,
			[]string{"emacs", "-batch", "-Q", "-l", "ert", "-l", driver, "--eval", form},
			"AGENT_REPL_ITEST_FAKEDAEMON="+fake)
		sp.Items = func(out []byte) (map[string]float64, error) { return ParseERTItems(out, items) }
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

var ertItemLine = regexp.MustCompile(`^TESTRUN-ITEM (\S+) ([0-9.]+)$`)

// ParseERTItems reads the driver's per-file lines, and insists on exactly one
// for every file the chunk was given.
func ParseERTItems(out []byte, want []string) (map[string]float64, error) {
	got := map[string]float64{}
	sc := bufio.NewScanner(bytes.NewReader(out))
	sc.Buffer(make([]byte, 1024*1024), 64*1024*1024)
	for sc.Scan() {
		m := ertItemLine.FindStringSubmatch(sc.Text())
		if m == nil {
			continue
		}
		secs, err := strconv.ParseFloat(m[2], 64)
		if err != nil {
			return nil, fmt.Errorf("unreadable seconds in %q: %w", sc.Text(), err)
		}
		if _, dup := got[m[1]]; dup {
			return nil, fmt.Errorf("%s reported twice", m[1])
		}
		got[m[1]] = secs
	}
	if err := sc.Err(); err != nil {
		return nil, err
	}
	return got, matchItems(got, want)
}

// matchItems insists the reported items are exactly the wanted ones.
func matchItems(got map[string]float64, want []string) error {
	var missing []string
	for _, w := range want {
		if _, ok := got[w]; !ok {
			missing = append(missing, w)
		}
	}
	if len(missing) > 0 {
		return fmt.Errorf("no timing reported for %v", missing)
	}
	if len(got) != len(want) {
		return fmt.Errorf("timings reported for %d items, but the chunk ran %d", len(got), len(want))
	}
	return nil
}
