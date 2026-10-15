package suites

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"

	"agentrepl/testrun/internal/command"
	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/roster"
)

// goTestedPackages lists a module's packages that have test files, as paths
// relative to the module ("." for the root).
func goTestedPackages(dir string) ([]string, error) {
	return goTestedPackagesTagged(dir, "", "./...")
}

// goTestedPackagesTagged is goTestedPackages under build TAGS, over PATTERN.
func goTestedPackagesTagged(dir, tags, pattern string) ([]string, error) {
	args := []string{"list"}
	if tags != "" {
		args = append(args, "-tags", tags)
	}
	args = append(args, "-f", "{{if or .TestGoFiles .XTestGoFiles}}{{.Dir}}{{end}}", pattern)
	cmd := exec.Command("go", args...)
	cmd.Dir = dir
	cmd.Env = append(os.Environ(), PinnedEnv()...)
	out, err := command.Output(cmd)
	if err != nil {
		return nil, fmt.Errorf("suites: list the tested packages: %w", err)
	}
	return parseGoList(dir, out)
}

// parseGoList reads goTestedPackages' `go list` output: one absolute package
// directory per line, answered relative to the module dir and sorted.
func parseGoList(dir string, out []byte) ([]string, error) {
	var pkgs []string
	for line := range strings.SplitSeq(strings.TrimSpace(string(out)), "\n") {
		if line == "" {
			continue
		}
		rel, err := under(dir, line)
		if err != nil {
			return nil, err
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
// (goPkg), plus the module's vet unit. Coverage adds counters and one report
// unit only when the caller explicitly asks for it.
func goModuleUnits(l Layout, s roster.Suite) (Units, error) {
	dir := l.resolve(s.Path)
	pkgs, ipkgs, err := GoSuitePackages(l, s)
	if err != nil {
		return Units{}, err
	}
	u, err := goModuleUnitsForPackages(l, s, dir, pkgs)
	if err != nil || s.IntegrationTags == "" {
		return u, err
	}
	iu, err := goIntegrationUnitsForPackages(l, s, dir, ipkgs)
	if err != nil {
		return Units{}, err
	}
	u.Atomic = append(u.Atomic, iu.Atomic...)
	u.Splits = append(u.Splits, iu.Splits...)
	return u, nil
}

// GoSuitePackages is a Go suite's tested packages, relative to its module:
// the ordinary pass's, and its build-tagged integration pass's (none without
// IntegrationTags). Every caller that runs a suite's packages lists them
// here: the run, and `testrun redundancy`'s -roster-suite.
func GoSuitePackages(l Layout, s roster.Suite) ([]string, []string, error) {
	dir := l.resolve(s.Path)
	pkgs, err := goTestedPackages(dir)
	if err != nil {
		return nil, nil, err
	}
	if len(pkgs) == 0 {
		return nil, nil, fmt.Errorf("suites: %s has no tested packages under %s", s.Name, dir)
	}
	if s.IntegrationTags == "" {
		return pkgs, nil, nil
	}
	ipkgs, err := goTestedPackagesTagged(dir, s.IntegrationTags, s.IntegrationPackages)
	if err != nil {
		return nil, nil, err
	}
	if len(ipkgs) == 0 {
		return nil, nil, fmt.Errorf("suites: %s has no %s-tagged tested packages in %s under %s", s.Name, s.IntegrationTags, s.IntegrationPackages, dir)
	}
	return pkgs, ipkgs, nil
}

// goIntegrationUnitsForPackages is the suite's build-tagged integration pass:
// its own vet over the tagged packages, and each package built and split under
// the tags, exactly as the ordinary pass does it. Its test binaries are never
// instrumented (in-process coverage is the ordinary pass's question), but
// under coverage the systems its tests spawn are: that is the integration
// tier's reach.
func goIntegrationUnitsForPackages(l Layout, s roster.Suite, dir string, pkgs []string) (Units, error) {
	tag := s.IntegrationTags
	vet := goVetTagged(s.Name, dir, tag, s.IntegrationPackages)
	u := Units{Atomic: []run.Spec{vet}}
	for i, rel := range pkgs {
		p := goPkg{
			Suite: s.Name, Module: dir, Rel: rel, Tags: tag, Parallel: s.TestParallel[rel],
			Bin:      filepath.Join(l.Work, "bin", s.Name+"-"+tag, fmt.Sprintf("%03d.test", i)),
			Programs: s.Programs[rel], ProgramsDir: l.ProgramsDir(),
		}
		if l.Coverage && len(p.Programs) > 0 {
			p.SpawnedCovDir = SpawnedCovDir(l, s.Name+"-"+tag, i)
		}
		u.Programs = append(u.Programs, p.Programs...)
		build, split, err := p.units()
		if err != nil {
			return Units{}, err
		}
		u.Atomic = append(u.Atomic, build)
		if split != nil {
			u.Splits = append(u.Splits, *split)
		}
	}
	if len(u.Splits) == 0 {
		return Units{}, fmt.Errorf("suites: %s has %s-tagged test files but no test to run under %s", s.Name, tag, dir)
	}
	return u, nil
}

func goModuleUnitsForPackages(l Layout, s roster.Suite, dir string, pkgs []string) (Units, error) {
	covRoot := filepath.Join(l.Work, "cover", s.Name)
	u := Units{Atomic: goVets(s, dir)}
	var reportDeps []string
	hasTests := false
	for i, rel := range pkgs {
		p := goPkg{
			Suite: s.Name, Module: dir, Rel: rel, Parallel: s.TestParallel[rel],
			Bin:      filepath.Join(l.Work, "bin", s.Name, fmt.Sprintf("%03d.test", i)),
			Programs: s.Programs[rel], ProgramsDir: l.ProgramsDir(),
		}
		u.Programs = append(u.Programs, p.Programs...)
		if l.Coverage {
			p.CovDir = filepath.Join(covRoot, fmt.Sprintf("%03d", i))
			if len(p.Programs) > 0 {
				p.SpawnedCovDir = SpawnedCovDir(l, s.Name, i)
			}
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
		hasTests = true
		if l.Coverage {
			reportDeps = append(reportDeps, split.Group)
		}
	}
	if !hasTests {
		return Units{}, fmt.Errorf("suites: %s has test files but no test to run under %s", s.Name, dir)
	}
	if l.Coverage {
		report := spec(s.Name+":coverage", s.Name, dir,
			[]string{l.Self, "cover-report", "-name", s.Name, "-module", dir, "-covdirs", covRoot})
		report.Deps = reportDeps
		u.Atomic = append(u.Atomic, report)
	}
	return u, nil
}
