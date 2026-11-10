package suites

import (
	"fmt"
	"path/filepath"
	"strconv"

	"agentrepl/testrun/internal/gobuild"
	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/roster"
)

// e2eUnits is the root package split the one Go way (goPkg), with a build
// that also installs the npm deps and fills the shared prebuilt directory
// every chunk reads; every other package of the e2e module is an ordinary Go
// package. The module is vetted like every Go module (goVet). Under
// coverage the e2e module's own code is not measured (it is the harness, not
// a system), but the systems its tests spawn are (SpawnedCovDir).
func e2eUnits(l Layout, s roster.Suite) (Units, error) {
	dir := l.resolve(s.Path)
	pkgs, err := goTestedPackages(dir)
	if err != nil {
		return Units{}, err
	}
	return e2eUnitsForPackages(l, s, dir, pkgs)
}

func e2eUnitsForPackages(l Layout, s roster.Suite, dir string, pkgs []string) (Units, error) {
	work := filepath.Join(l.Work, "e2e")
	u := Units{Atomic: goVets(s, dir)}
	b := moduleBuild(l, s.Name, dir, "", false)
	for i, rel := range pkgs {
		p := goPkg{Suite: s.Name, Module: dir, Rel: rel, Parallel: s.TestParallel[rel],
			Bin: gobuild.BinPath(b.Options.BinDir, rel), Self: l.Self, After: b.ID,
			Programs: s.Programs[rel], ProgramsDir: l.ProgramsDir()}
		u.Programs = append(u.Programs, p.Programs...)
		if l.Coverage {
			p.SpawnedCovDir = SpawnedCovDir(l, s.Name, i)
		}
		var extra []run.Spec
		if rel == "." {
			pre := p.prebuild(filepath.Join(work, "prebuilt"), strconv.Quote(filepath.Join(l.Module, "bin", "ensure-e2e-deps.sh")))
			extra = append(extra, pre)
			p.Timeout = "45m"
		}
		if err := u.addPackage(&b, p, extra...); err != nil {
			return Units{}, err
		}
	}
	if len(u.Splits) == 0 {
		return Units{}, fmt.Errorf("suites: %s has no tests under %s", s.Name, dir)
	}
	u.GoBuilds = append(u.GoBuilds, b)
	return u.Finish(), nil
}
