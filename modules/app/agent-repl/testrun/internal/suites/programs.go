package suites

import (
	"fmt"
	"path/filepath"
	"strings"

	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/roster"
)

// EVERY SHARED PROGRAM IS BUILT ONCE PER RUN. The daemon's integration suite
// and the e2e suite both run the daemon and its fakes, and the e2e suite runs
// the store, the sidecar and the lock helper, which their own suites run too;
// each used to build its own copy. A run now builds each program ONCE into
// its programs directory (one unit per program, under the program's own
// suite), and every chunk that runs one waits for that unit and reads the
// binary from AGENT_REPL_TEST_PROGRAMS (testenv.Program).

// ProgramsDir is the run's programs directory.
func (l Layout) ProgramsDir() string { return filepath.Join(l.Work, "programs") }

// ProgramUnitID is the unit that builds the program named name.
func ProgramUnitID(name string) string { return "program:" + name }

// ProgramUnits answers one build unit per program the run needs, in roster
// order, each reported under owner(program). A name the roster does not hold
// is a defect in the roster, answered as an error.
func ProgramUnits(l Layout, needed []string, owner func(roster.Program) string) ([]run.Spec, error) {
	want := map[string]bool{}
	for _, name := range needed {
		if _, ok := roster.LookupProgram(name); !ok {
			return nil, fmt.Errorf("suites: a suite runs the program %q, which roster.Programs does not hold", name)
		}
		want[name] = true
	}
	var specs []run.Spec
	for _, p := range roster.Programs {
		if !want[p.Name] {
			continue
		}
		argv := []string{"go", "build", "-buildvcs=false"}
		if len(p.Links) > 0 {
			var links []string
			for _, link := range p.Links {
				links = append(links, "-X "+link.Var+"="+filepath.Join(l.ProgramsDir(), link.Program))
			}
			argv = append(argv, "-ldflags="+strings.Join(links, " "))
		}
		argv = append(argv, "-o", filepath.Join(l.ProgramsDir(), p.Name), p.Package)
		specs = append(specs, spec(ProgramUnitID(p.Name), owner(p), filepath.Join(l.Module, p.Module), argv))
	}
	return specs, nil
}
