package reuse

import (
	"fmt"
	"sort"
	"strings"

	"agentrepl/testrun/internal/gitinfo"
	"agentrepl/testrun/roster"
)

// Regions is how the repository is divided for the placement check: what
// every keyed suite reads, what only always-run suites read, and what no suite
// reads.
type Regions struct {
	keyed  map[string]*InputSet
	always *InputSet
	nobody *InputSet
}

// BuildRegions places every keyed suite's inputs (sets, by suite name), each
// always-run suite's own path, and the roster's always-run regions and paths
// no suite reads (roster.AlwaysRunRegions, roster.ReadByNoSuite).
func BuildRegions(d Deps, all []roster.Suite, alwaysRun, readByNone []string, sets map[string]*InputSet) (Regions, error) {
	r := Regions{keyed: sets, always: NewInputSet(), nobody: NewInputSet()}
	for _, s := range all {
		if s.Reuse != nil {
			if _, ok := sets[s.Name]; !ok {
				return Regions{}, fmt.Errorf("reuse: the keyed suite %s has no input set to place", s.Name)
			}
			continue
		}
		p := s.Path
		if s.Kind != roster.Script && s.Kind != roster.SplitScript {
			p += "/"
		}
		if err := placeRosterPath(d, r.always, p, false); err != nil {
			return Regions{}, fmt.Errorf("reuse: place %s's path: %w", s.Name, err)
		}
	}
	for _, p := range alwaysRun {
		if err := placeRosterPath(d, r.always, p, false); err != nil {
			return Regions{}, err
		}
	}
	for _, p := range readByNone {
		if err := placeRosterPath(d, r.nobody, p, false); err != nil {
			return Regions{}, err
		}
	}
	return r, nil
}

// Unplaced is every file of files (repository-relative) that no region
// places, sorted.
func (r Regions) Unplaced(files []string) []string {
	var out []string
	for _, f := range files {
		if r.always.Contains(f) || r.nobody.Contains(f) {
			continue
		}
		placed := false
		for _, set := range r.keyed {
			if set.Contains(f) {
				placed = true
				break
			}
		}
		if !placed {
			out = append(out, f)
		}
	}
	sort.Strings(out)
	return out
}

// ExecRepoFiles lists the checkout's files the way a commit would see them:
// tracked, plus untracked ones git does not ignore.
func ExecRepoFiles(repo string) ([]string, error) {
	out, err := gitinfo.Output(repo, "ls-files", "-z", "--cached", "--others", "--exclude-standard")
	if err != nil {
		return nil, fmt.Errorf("reuse: list %s's files: %w", repo, err)
	}
	var files []string
	for _, f := range strings.Split(string(out), "\x00") {
		if f != "" {
			files = append(files, f)
		}
	}
	return files, nil
}

// ReportPlacement logs each unplaced file as its own failure line, and
// answers whether the check passed. An unplaced file fails the run, but
// reuse is untouched by it: a file outside every keyed suite's input set is a
// file no keyed suite can read.
func ReportPlacement(log Logger, unplaced []string) bool {
	for _, f := range unplaced {
		log.Errorf("placement: %s is read by no suite and listed nowhere (testrun/roster: a keyed suite's Reuse.Inputs, AlwaysRunRegions or ReadByNoSuite)", f)
	}
	return len(unplaced) == 0
}
