package suites

import (
	"bufio"
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"slices"
	"strconv"
	"strings"

	"agentrepl/testrun/internal/command"
	"agentrepl/testrun/roster"
)

// THE TREE'S OWN LISTING OF ITS CASES, for a rerun's existence check
// (selection.Prune): each kind of test is listed by the tool that runs it, as
// it plans, never guessed from the text of a file. A Go package's top-level
// tests are read by go/build (GoTopLevelTestsTagged), a vitest file's tests
// are what `vitest list` collects, an ERT file's are what loading it defines,
// and a split harness's groups are what its --list prints.

// Lister lists the cases the tree holds.
type Lister struct{}

// GoTests answers every top-level test of the Go package in DIR, under no
// build tag and under TAGS (the suite's integration tags, "" for none): a
// name either build defines is one the package holds.
func (Lister) GoTests(dir, tags string) ([]string, error) {
	names, err := GoTopLevelTestsTagged(dir, "")
	if err != nil || tags == "" {
		return names, err
	}
	tagged, err := GoTopLevelTestsTagged(dir, tags)
	if err != nil {
		return nil, err
	}
	return append(names, tagged...), nil
}

// VitestNames answers the full names `vitest list` collects in each of
// FILES (relative to DIR, the package), under CONFIG ("" is the default):
// each a test's describe blocks and its own name joined by " > ", as vitest
// matches a `-t` pattern against it (VitestNamePattern). A file it collects
// nothing in is absent.
func (Lister) VitestNames(module, dir, config string, files []string) (map[string][]string, error) {
	if err := runEnsure(fmt.Sprintf("ensure the npm deps of %s", dir), filepath.Join(module, "bin", "ensure-deps.sh"), dir); err != nil {
		return nil, err
	}
	workers, err := vitestListWorkers(dir)
	if err != nil {
		return nil, err
	}
	// A config that collects none of FILES lists nothing, which is no
	// failure.
	args := append([]string{"vitest", "list", "--json", "--passWithNoTests"}, workers...)
	if config != "" {
		args = append(args, "--config", config)
	}
	list := exec.Command("npx", append(args, files...)...)
	list.Dir = dir
	list.Env = os.Environ()
	out, err := command.Output(list)
	if err != nil {
		return nil, fmt.Errorf("suites: list the vitest tests of %s: %w", strings.Join(files, ", "), err)
	}
	return parseVitestNames(dir, out)
}

// vitestListWorkers answers the worker flags `vitest list` takes in DIR, by
// the vitest the package installs: one worker, since listing runs no test.
// Vitest before 5 needs its floor set too, or the pool's default floor
// conflicts with the one-worker ceiling; vitest 5 has no floor, and its
// `list` refuses the flag.
func vitestListWorkers(dir string) ([]string, error) {
	path := filepath.Join(dir, "node_modules", "vitest", "package.json")
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("suites: read the vitest version of %s: %w", dir, err)
	}
	var pkg struct {
		Version string `json:"version"`
	}
	if err := json.Unmarshal(data, &pkg); err != nil {
		return nil, fmt.Errorf("suites: read the vitest version in %s: %w", path, err)
	}
	major, _, _ := strings.Cut(pkg.Version, ".")
	n, err := strconv.Atoi(major)
	if err != nil {
		return nil, fmt.Errorf("suites: %s names no vitest version (%q)", path, pkg.Version)
	}
	if n >= 5 {
		return []string{"--maxWorkers=1"}, nil
	}
	return []string{"--maxWorkers=1", "--minWorkers=1"}, nil
}

// parseVitestNames reads `vitest list --json`: each collected test's file
// and full name ("describe > test"), answered by file relative to DIR.
func parseVitestNames(dir string, out []byte) (map[string][]string, error) {
	var entries []struct {
		Name string `json:"name"`
		File string `json:"file"`
	}
	if err := json.Unmarshal(out, &entries); err != nil {
		return nil, fmt.Errorf("suites: vitest list in %s printed no test list: %w", dir, err)
	}
	names := map[string][]string{}
	for _, e := range entries {
		if e.Name == "" || e.File == "" {
			return nil, fmt.Errorf("suites: vitest list in %s printed a test with no name or file: %+v", dir, e)
		}
		rel, err := under(dir, e.File)
		if err != nil {
			return nil, err
		}
		names[filepath.ToSlash(rel)] = append(names[filepath.ToSlash(rel)], e.Name)
	}
	return names, nil
}

// ertNameLine is the ERT driver's line naming one test a listed file
// defines (agent-repl-testrun-ert-list).
const ertNameLine = "TESTRUN-NAME "

// ERTNames answers the tests each of FILES (bare names in LISPDIR, the ERT
// suite's directory) defines, by loading the shared helpers and then each
// file in a batch Emacs, as a run's chunk loads them (testrun/ert/driver.el).
// A file that defines none is absent.
func (Lister) ERTNames(module, lispDir string, files []string) (map[string][]string, error) {
	driver := filepath.Join(module, "testrun", "ert", "driver.el")
	form := fmt.Sprintf("(agent-repl-testrun-ert-list %q %s)", lispDir+"/", lispList(files))
	list := exec.Command("emacs", "-batch", "-Q", "-l", "ert", "-l", driver, "--eval", form)
	list.Dir = lispDir
	list.Env = os.Environ()
	out, err := command.Output(list)
	if err != nil {
		return nil, fmt.Errorf("suites: list the ERT tests of %s: %w", strings.Join(files, ", "), err)
	}
	return parseERTNames(out, files)
}

// parseERTNames reads the driver's TESTRUN-NAME lines, refusing a line that
// names a file it was not asked to list.
func parseERTNames(out []byte, files []string) (map[string][]string, error) {
	names := map[string][]string{}
	sc := bufio.NewScanner(bytes.NewReader(out))
	for sc.Scan() {
		rest, ok := strings.CutPrefix(sc.Text(), ertNameLine)
		if !ok {
			continue
		}
		file, name, ok := strings.Cut(rest, " ")
		if !ok || name == "" {
			return nil, fmt.Errorf("suites: the ERT listing printed an unreadable line %q", sc.Text())
		}
		if !slices.Contains(files, file) {
			return nil, fmt.Errorf("suites: the ERT listing named %s, which it was not asked to list", file)
		}
		names[file] = append(names[file], name)
	}
	if err := sc.Err(); err != nil {
		return nil, fmt.Errorf("suites: read the ERT listing: %w", err)
	}
	return names, nil
}

// HarnessGroups answers the groups split harness suite S lists (--list).
func (Lister) HarnessGroups(l Layout, s roster.Suite) ([]string, error) {
	_, groups, err := listHarness(l, s)
	return groups, err
}
