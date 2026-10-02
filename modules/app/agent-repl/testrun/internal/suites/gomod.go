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

// goModuleUnits is every tested package of the module split the one Go way
// (goPkg), all writing coverage counters into one directory per package, plus
// the module's vet unit and one report unit that merges the counters into the
// module's function report, the shape report-nonlisp-coverage.sh prints.
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
	vet := spec(s.Name+":vet", s.Name, dir, append(append([]string{"go", "vet"}, goTestVetFlags...), "./..."))
	u := Units{Atomic: []run.Spec{vet}}
	var reportDeps []string
	for i, rel := range pkgs {
		p := goPkg{
			Suite: s.Name, Module: dir, Rel: rel,
			Bin:    filepath.Join(l.Work, "bin", s.Name, fmt.Sprintf("%03d.test", i)),
			CovDir: filepath.Join(covRoot, fmt.Sprintf("%03d", i)),
		}
		build, split, err := p.units()
		if err != nil {
			return Units{}, err
		}
		u.Atomic = append(u.Atomic, build)
		if split == nil {
			continue
		}
		u.Splits = append(u.Splits, *split)
		reportDeps = append(reportDeps, split.Group)
	}
	if len(reportDeps) == 0 {
		return Units{}, fmt.Errorf("suites: %s has test files but no test to run under %s", s.Name, dir)
	}
	report := spec(s.Name+":coverage", s.Name, dir,
		[]string{l.Self, "cover-report", "-name", s.Name, "-module", dir, "-covdirs", covRoot})
	report.Deps = reportDeps
	u.Atomic = append(u.Atomic, report)
	return u, nil
}
