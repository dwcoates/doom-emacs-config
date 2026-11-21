package suites

import (
	"bufio"
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"agentrepl/testrun/internal/command"
	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/internal/testtimes"
	"agentrepl/testrun/roster"
)

func splitScriptUnits(l Layout, s roster.Suite) (Units, error) {
	path, items, err := listHarness(l, s)
	if err != nil {
		return Units{}, err
	}
	return splitScriptUnitsForItems(path, s, items), nil
}

// listHarness answers split harness suite S's script and the groups its
// --list prints: the one listing of a harness's groups, for its units and a
// rerun's existence check (Lister.HarnessGroups) alike.
func listHarness(l Layout, s roster.Suite) (string, []string, error) {
	path, err := executableScriptPath(l, s)
	if err != nil {
		return "", nil, err
	}
	cmd := exec.Command(path, "--list")
	cmd.Dir = filepath.Dir(path)
	cmd.Env = append(os.Environ(), PinnedEnv()...)
	out, err := command.Output(cmd)
	if err != nil {
		return "", nil, fmt.Errorf("suites: list %s's harness items: %w", s.Name, err)
	}
	items, err := parseScriptList(out)
	if err != nil {
		return "", nil, fmt.Errorf("suites: list %s's harness items: %w", s.Name, err)
	}
	return path, items, nil
}

func splitScriptUnitsForItems(path string, s roster.Suite, items []string) Units {
	chunk := func(id string, selected []string) run.Spec {
		sp := spec(id, s.Name, filepath.Dir(path), []string{path, "--only", strings.Join(selected, ",")})
		sp.Items = func(out []byte) (map[string]float64, error) { return ParseItemLines(out, selected) }
		// A HARNESS GROUP IS ITS FINEST TEST: each group's time is its item's.
		sp.Tests = func(out []byte, passed bool) ([]testtimes.Test, error) {
			items, err := ParseItemLines(out, selected)
			if err != nil {
				return nil, err
			}
			outcome := testtimes.Pass
			if !passed {
				outcome = testtimes.Fail
			}
			return testtimes.OfItems(s.Name, items, outcome), nil
		}
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
