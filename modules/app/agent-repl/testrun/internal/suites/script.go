package suites

import (
	"bufio"
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"

	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/roster"
)

func splitScriptUnits(l Layout, s roster.Suite) (Units, error) {
	path, err := executableScriptPath(l, s)
	if err != nil {
		return Units{}, err
	}
	cmd := exec.Command(path, "--list")
	cmd.Dir = filepath.Dir(path)
	cmd.Env = append(os.Environ(), PinnedEnv()...)
	out, err := cmd.Output()
	if err != nil {
		return Units{}, fmt.Errorf("suites: list %s's harness items: %w", s.Name, err)
	}
	items, err := parseScriptList(out)
	if err != nil {
		return Units{}, fmt.Errorf("suites: list %s's harness items: %w", s.Name, err)
	}
	return splitScriptUnitsForItems(path, s, items), nil
}

func splitScriptUnitsForItems(path string, s roster.Suite, items []string) Units {
	chunk := func(id string, selected []string) run.Spec {
		sp := spec(id, s.Name, filepath.Dir(path), []string{path, "--only", strings.Join(selected, ",")})
		sp.Items = func(out []byte) (map[string]float64, error) { return ParseScriptItems(out, selected) }
		return sp
	}
	return Units{Splits: []Split{{Group: s.Name, Suite: s.Name, Items: items, Chunk: chunk}}}
}

func parseScriptList(out []byte) ([]string, error) {
	var items []string
	seen := map[string]bool{}
	sc := bufio.NewScanner(bytes.NewReader(out))
	for sc.Scan() {
		name := strings.TrimSpace(sc.Text())
		if name == "" || strings.ContainsAny(name, " \t,") {
			return nil, fmt.Errorf("invalid item name %q", name)
		}
		if seen[name] {
			return nil, fmt.Errorf("item %q was listed twice", name)
		}
		seen[name] = true
		items = append(items, name)
	}
	if err := sc.Err(); err != nil {
		return nil, err
	}
	if len(items) == 0 {
		return nil, fmt.Errorf("no items listed")
	}
	return items, nil
}

func ParseScriptItems(out []byte, want []string) (map[string]float64, error) {
	got := map[string]float64{}
	sc := bufio.NewScanner(bytes.NewReader(out))
	for sc.Scan() {
		fields := strings.Fields(sc.Text())
		if len(fields) != 3 || fields[0] != "TESTRUN-ITEM" {
			continue
		}
		secs, err := strconv.ParseFloat(fields[2], 64)
		if err != nil || secs < 0 {
			return nil, fmt.Errorf("unreadable item timing %q", sc.Text())
		}
		if _, exists := got[fields[1]]; exists {
			return nil, fmt.Errorf("item %s reported twice", fields[1])
		}
		got[fields[1]] = secs
	}
	if err := sc.Err(); err != nil {
		return nil, err
	}
	return got, matchItems(got, want)
}
