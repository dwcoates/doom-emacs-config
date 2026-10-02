package suites

import (
	"fmt"
	"path/filepath"
	"strconv"

	"agentrepl/testrun/roster"
)

// e2eUnits is the root package split the one Go way (goPkg), with a build
// that also installs the npm deps and fills the shared prebuilt directory
// every chunk reads; every other package of the e2e module is an ordinary Go
// package. The e2e suite runs without coverage, as it always has here.
func e2eUnits(l Layout, s roster.Suite) (Units, error) {
	dir := l.resolve(s.Path)
	pkgs, err := goTestedPackages(dir)
	if err != nil {
		return Units{}, err
	}
	work := filepath.Join(l.Work, "e2e")
	var u Units
	for i, rel := range pkgs {
		p := goPkg{Suite: s.Name, Module: dir, Rel: rel, Bin: filepath.Join(work, fmt.Sprintf("%03d.test", i))}
		if rel == "." {
			prebuilt := filepath.Join(work, "prebuilt")
			p.sharePrebuilt(prebuilt, strconv.Quote(filepath.Join(l.Module, "bin", "ensure-e2e-deps.sh")))
			p.Timeout = "45m"
		}
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
		return Units{}, fmt.Errorf("suites: %s has no tests under %s", s.Name, dir)
	}
	return u, nil
}
