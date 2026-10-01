package suites

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"

	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/roster"
)

// goTestedPackages lists a module's packages that have test files, as paths
// relative to the module ("." for the root).
func goTestedPackages(dir string) ([]string, error) {
	cmd := exec.Command("go", "list", "-f", "{{if or .TestGoFiles .XTestGoFiles}}{{.Dir}}{{end}}", "./...")
	cmd.Dir = dir
	cmd.Env = append(os.Environ(), PinnedEnv()...)
	out, err := cmd.Output()
	if err != nil {
		var stderr string
		if ee, ok := err.(*exec.ExitError); ok {
			stderr = string(ee.Stderr)
		}
		return nil, fmt.Errorf("suites: go list in %s: %w\n%s", dir, err, stderr)
	}
	var pkgs []string
	for line := range strings.SplitSeq(strings.TrimSpace(string(out)), "\n") {
		if line == "" {
			continue
		}
		rel, err := filepath.Rel(dir, line)
		if err != nil {
			return nil, fmt.Errorf("suites: %s is not under %s: %w", line, dir, err)
		}
		pkgs = append(pkgs, rel)
	}
	sort.Strings(pkgs)
	return pkgs, nil
}

// goPackageArg is a relative package directory as a `go test` argument.
func goPackageArg(rel string) string {
	if rel == "." {
		return "."
	}
	return "./" + rel
}

// goModuleUnits is one unit per tested package, each writing its coverage
// counters into its own directory, and one report unit that merges them into
// the module's function report, the shape report-nonlisp-coverage.sh prints.
func goModuleUnits(l Layout, s roster.Suite) (Units, error) {
	dir := l.resolve(s.Path)
	pkgs, err := goTestedPackages(dir)
	if err != nil {
		return Units{}, err
	}
	if len(pkgs) == 0 {
		return Units{}, fmt.Errorf("suites: %s has no tested packages under %s", s.Name, dir)
	}
	covRoot := filepath.Join(l.Work, "cover", s.Name)
	var units []run.Spec
	var deps []string
	for i, rel := range pkgs {
		covDir := filepath.Join(covRoot, fmt.Sprintf("%03d", i))
		if err := os.MkdirAll(covDir, 0o755); err != nil {
			return Units{}, fmt.Errorf("suites: create %s: %w", covDir, err)
		}
		id := s.Name + ":" + rel
		units = append(units, spec(id, s.Name, dir, []string{
			"go", "test", "-count=1", "-parallel=1", "-cover", "-coverpkg=./...",
			goPackageArg(rel), "-args", "-test.gocoverdir=" + covDir,
		}))
		deps = append(deps, id)
	}
	report := spec(s.Name+":coverage", s.Name, dir,
		[]string{l.Self, "cover-report", "-name", s.Name, "-module", dir, "-covdirs", covRoot})
	report.Deps = deps
	return Units{Atomic: append(units, report)}, nil
}
