package suites

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"

	"agentrepl/testrun/internal/command"
	"agentrepl/testrun/internal/gobuild"
	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/roster"
)

// goPackages is one pass's packages, as paths relative to the module ("."
// for the root), each list sorted: those with test files, whose tests the
// pass builds and runs, and those without, which the pass only vets.
type goPackages struct {
	Tested, Untested []string
}

// goTestedPackages lists a module's packages that have test files.
func goTestedPackages(dir string) ([]string, error) {
	p, err := goPackagesTagged(dir, "", "./...")
	return p.Tested, err
}

// goPackagesTagged lists the packages of PATTERN under build TAGS, by
// whether they have test files, in one `go list`.
func goPackagesTagged(dir, tags, pattern string) (goPackages, error) {
	args := []string{"list"}
	if tags != "" {
		args = append(args, "-tags", tags)
	}
	args = append(args, "-f", "{{.Dir}}{{if or .TestGoFiles .XTestGoFiles}}"+goListTested+"{{end}}", pattern)
	cmd := exec.Command("go", args...)
	cmd.Dir = dir
	cmd.Env = append(os.Environ(), PinnedEnv()...)
	out, err := command.Output(cmd)
	if err != nil {
		return goPackages{}, fmt.Errorf("suites: list the packages: %w", err)
	}
	return parseGoList(dir, out)
}

// goListTested follows the directory of a package with test files on its
// line of goPackagesTagged's `go list` output. A tab cannot end a directory
// name the go command lists.
const goListTested = "\ttested"

// parseGoList reads goPackagesTagged's `go list` output: one absolute
// package directory per line, followed by goListTested for a package with
// test files, answered relative to the module dir.
func parseGoList(dir string, out []byte) (goPackages, error) {
	var p goPackages
	for line := range strings.SplitSeq(strings.TrimSpace(string(out)), "\n") {
		if line == "" {
			continue
		}
		pkgDir, tested := strings.CutSuffix(line, goListTested)
		rel, err := under(dir, pkgDir)
		if err != nil {
			return goPackages{}, err
		}
		if tested {
			p.Tested = append(p.Tested, rel)
		} else {
			p.Untested = append(p.Untested, rel)
		}
	}
	sort.Strings(p.Tested)
	sort.Strings(p.Untested)
	return p, nil
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
	pkgs, ipkgs, err := goSuitePackages(l, s)
	if err != nil {
		return Units{}, err
	}
	u, err := goModuleUnitsForPackages(l, s, dir, pkgs.Tested, pkgs.Untested...)
	if err != nil || s.IntegrationTags == "" {
		return u, err
	}
	iu, err := goIntegrationUnitsForPackages(l, s, dir, ipkgs.Tested, ipkgs.Untested...)
	if err != nil {
		return Units{}, err
	}
	u.Atomic = append(u.Atomic, iu.Atomic...)
	u.Splits = append(u.Splits, iu.Splits...)
	u.GoBuilds = append(u.GoBuilds, iu.GoBuilds...)
	u.GoVets = append(u.GoVets, iu.GoVets...)
	return u.Finish(), nil
}

// GoSuitePackages is a Go suite's tested packages, relative to its module:
// the ordinary pass's, and its build-tagged integration pass's (none without
// IntegrationTags). Every caller that runs a suite's packages lists them
// here: the run, and `testrun redundancy`'s -roster-suite.
func GoSuitePackages(l Layout, s roster.Suite) ([]string, []string, error) {
	pkgs, ipkgs, err := goSuitePackages(l, s)
	return pkgs.Tested, ipkgs.Tested, err
}

// goSuitePackages is GoSuitePackages with each pass's untested packages too.
func goSuitePackages(l Layout, s roster.Suite) (goPackages, goPackages, error) {
	dir := l.resolve(s.Path)
	pkgs, err := goPackagesTagged(dir, "", "./...")
	if err != nil {
		return goPackages{}, goPackages{}, err
	}
	if len(pkgs.Tested) == 0 {
		return goPackages{}, goPackages{}, fmt.Errorf("suites: %s has no tested packages under %s", s.Name, dir)
	}
	if s.IntegrationTags == "" {
		return pkgs, goPackages{}, nil
	}
	ipkgs, err := goPackagesTagged(dir, s.IntegrationTags, s.IntegrationPackages)
	if err != nil {
		return goPackages{}, goPackages{}, err
	}
	if len(ipkgs.Tested) == 0 {
		return goPackages{}, goPackages{}, fmt.Errorf("suites: %s has no %s-tagged tested packages in %s under %s", s.Name, s.IntegrationTags, s.IntegrationPackages, dir)
	}
	return pkgs, ipkgs, nil
}

// goIntegrationUnitsForPackages is the suite's build-tagged integration pass:
// its own vet over the tagged packages, and each package built and split under
// the tags, exactly as the ordinary pass does it. Its test binaries are never
// instrumented (in-process coverage is the ordinary pass's question), but
// under coverage the systems its tests spawn are: that is the integration
// tier's reach.
func goIntegrationUnitsForPackages(l Layout, s roster.Suite, dir string, pkgs []string, untested ...string) (Units, error) {
	tag := s.IntegrationTags
	var u Units
	b := moduleBuild(l, s.Name, dir, tag, false)
	v := moduleVet(l, s.Name, dir, tag)
	for i, rel := range pkgs {
		p := b.pkgOf(l, s, rel)
		if l.Coverage && len(p.Programs) > 0 {
			p.SpawnedCovDir = SpawnedCovDir(l, s.Name+"-"+tag, i)
		}
		if err := u.addPackage(&b, p); err != nil {
			return Units{}, err
		}
		u.Atomic = append(u.Atomic, v.check(p.group(), rel))
	}
	if len(u.Splits) == 0 {
		return Units{}, fmt.Errorf("suites: %s has %s-tagged test files but no test to run under %s", s.Name, tag, dir)
	}
	u.addUntestedVets(&v, s.Name, tag, untested)
	u.GoBuilds = append(u.GoBuilds, b)
	u.GoVets = append(u.GoVets, v)
	return u.Finish(), nil
}

// addUntestedVets adds the vet check of each package of a pass (tags) that
// has no test files, which the pass only vets. Its unit ID prefix is the one
// its tests would have had (goPkg.group).
func (u *Units) addUntestedVets(v *GoVet, suite, tags string, rels []string) {
	for _, rel := range rels {
		u.Atomic = append(u.Atomic, v.check(packageGroup(suite, rel, tags), rel))
	}
}

// moduleBuild is the build of one pass (tags) over the module in dir, its
// binaries under the run's scratch directory.
func moduleBuild(l Layout, suite, dir, tags string, cover bool) GoBuild {
	pass := suite
	if tags != "" {
		pass += "-" + tags
	}
	return GoBuild{
		ID: ModuleBuildID(suite, tags), Suite: suite,
		Options: gobuild.Options{Module: dir, BinDir: filepath.Join(l.Work, "bin", pass), Tags: tags, Cover: cover, DebugInfo: l.DebugInfo, Self: l.Self},
	}
}

// pkgOf is the package rel of suite s as this build makes it: its binary
// under the build's directory, its units after the build, the build's tags.
func (b GoBuild) pkgOf(l Layout, s roster.Suite, rel string) goPkg {
	return goPkg{
		Suite: s.Name, Module: b.Options.Module, Rel: rel, Tags: b.Options.Tags, Parallel: s.TestParallel[rel],
		Bin: gobuild.BinPath(b.Options.BinDir, rel), Self: l.Self, After: b.ID,
		Programs: s.Programs[rel], ProgramsDir: l.ProgramsDir(),
		Counts: filepath.Join(l.Work, "log-records", s.Name),
		Phases: filepath.Join(l.Work, "phases", s.Name),
	}
}

// addPackage adds p's units to u, each atomic one recorded as needing p's
// binary from b, and the programs p's tests spawn.
func (u *Units) addPackage(b *GoBuild, p goPkg, extra ...run.Spec) error {
	check, split, err := p.units()
	if err != nil {
		return err
	}
	u.Programs = append(u.Programs, p.Programs...)
	for _, a := range extra {
		b.need(a.ID, p.Rel)
		u.Atomic = append(u.Atomic, a)
	}
	if check != nil {
		b.need(check.ID, p.Rel)
		u.Atomic = append(u.Atomic, *check)
	}
	if split != nil {
		// The split names its package itself (GoBuild.Builds).
		if rel, ok := b.Builds(*split); !ok || rel != p.Rel {
			panic(fmt.Sprintf("suites: %s's split %s is not one of build %s's", p.Rel, split.Group, b.ID))
		}
		u.Splits = append(u.Splits, *split)
	}
	return nil
}

func goModuleUnitsForPackages(l Layout, s roster.Suite, dir string, pkgs []string, untested ...string) (Units, error) {
	covRoot := filepath.Join(l.Work, "cover", s.Name)
	u := Units{Atomic: goTaggedVets(s, dir)}
	b := moduleBuild(l, s.Name, dir, "", l.Coverage)
	v := moduleVet(l, s.Name, dir, "")
	var reportDeps []string
	for i, rel := range pkgs {
		p := b.pkgOf(l, s, rel)
		if l.Coverage {
			p.CovDir = filepath.Join(covRoot, fmt.Sprintf("%03d", i))
			if len(p.Programs) > 0 {
				p.SpawnedCovDir = SpawnedCovDir(l, s.Name, i)
			}
		}
		n := len(u.Splits)
		if err := u.addPackage(&b, p); err != nil {
			return Units{}, err
		}
		u.Atomic = append(u.Atomic, v.check(p.group(), rel))
		if l.Coverage && len(u.Splits) > n {
			reportDeps = append(reportDeps, u.Splits[n].Group)
		}
	}
	if len(u.Splits) == 0 {
		return Units{}, fmt.Errorf("suites: %s has test files but no test to run under %s", s.Name, dir)
	}
	if l.Coverage {
		report := spec(s.Name+":coverage", s.Name, dir,
			[]string{l.Self, "cover-report", "-name", s.Name, "-module", dir, "-covdirs", covRoot})
		report.Deps = reportDeps
		u.Atomic = append(u.Atomic, report)
	}
	u.addUntestedVets(&v, s.Name, "", untested)
	u.GoBuilds = append(u.GoBuilds, b)
	u.GoVets = append(u.GoVets, v)
	return u.Finish(), nil
}
