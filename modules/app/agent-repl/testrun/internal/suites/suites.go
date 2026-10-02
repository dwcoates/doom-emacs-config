// Package suites turns each roster suite into the units the scheduler runs.
//
// EVERY UNIT IS PINNED TO ONE CORE by its own command line, because the
// scheduler's whole model is "one unit, one slot, one core":
//
//   - every Go program in every unit -- the toolchain, every test binary and
//     every system a test spawns -- runs with GOMAXPROCS=2, and the go command
//     with GOFLAGS -p=1 (one package built or tested at a time); Go test units
//     add -parallel=1. Left at its default, every one of the dozens of Go
//     processes a full run keeps alive sized itself to the WHOLE machine, and
//     their idle scheduler threads spinning against each other doubled the
//     run's total CPU. ONE P was measured too, and rejected: it serialized
//     every goroutine of the daemon, store and sidecar the e2e worlds run,
//     and e2e waits missed their bounds in every full run (3 of 3) and in an
//     e2e-only run; at two, 0 of 4. Two also keeps real parallelism in the
//     systems under test, which a single P would hide from them;
//   - vitest runs with --maxWorkers=1 --minWorkers=1;
//   - Emacs is single-threaded.
//
// The parallelism lives in the scheduler, which can see every suite at once,
// never inside a suite, which can only see itself.
package suites

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/roster"
)

// Layout is where everything is.
type Layout struct {
	// Repo is the repository root.
	Repo string
	// Module is modules/app/agent-repl.
	Module string
	// Work is this run's scratch directory: prebuilt binaries, coverage data.
	Work string
	// Self is the testrun binary, for the units it runs itself.
	Self string
	// Coverage adds expensive coverage instrumentation and report units.
	Coverage bool
}

// Split is a splittable suite piece before the planner chunks it.
type Split struct {
	Group string
	Suite string
	Items []string
	Deps  []string
	// OverheadCap bounds history's per-process estimate when the suite can
	// prove a tighter startup bound. Zero leaves the measured value unbounded.
	OverheadCap float64
	// Chunk builds the spec of one chunk from its items.
	Chunk func(id string, items []string) run.Spec
}

// Units is everything one suite contributes.
type Units struct {
	Atomic []run.Spec
	Splits []Split
}

// Build turns one suite into units.
func Build(l Layout, s roster.Suite) (Units, error) {
	switch s.Kind {
	case roster.Script:
		return scriptUnits(l, s)
	case roster.SplitScript:
		return splitScriptUnits(l, s)
	case roster.ERT:
		return ertUnits(l, s)
	case roster.GoModule:
		return goModuleUnits(l, s)
	case roster.Vitest:
		return vitestUnits(l, s)
	case roster.E2E:
		return e2eUnits(l, s)
	}
	panic(fmt.Sprintf("suites: suite %q has unknown kind %d", s.Name, s.Kind))
}

// PinnedEnv is the environment every unit adds to its own: the pins the
// package comment promises, plus the GOFLAGS the caller already had.
func PinnedEnv() []string {
	flags := strings.TrimSpace(os.Getenv("GOFLAGS") + " -p=1")
	return []string{"GOFLAGS=" + flags, "GOMAXPROCS=2"}
}

func spec(id, suite, dir string, argv []string, env ...string) run.Spec {
	s := run.Spec{Argv: argv, Dir: dir, Env: append(PinnedEnv(), env...)}
	s.ID, s.Suite = id, suite
	return s
}

// resolve is a roster path: relative to the module, or to the repository when
// it starts with "/".
func (l Layout) resolve(p string) string {
	if strings.HasPrefix(p, "/") {
		return filepath.Join(l.Repo, p[1:])
	}
	return filepath.Join(l.Module, p)
}

func scriptUnits(l Layout, s roster.Suite) (Units, error) {
	path, err := executableScriptPath(l, s)
	if err != nil {
		return Units{}, err
	}
	u := spec(s.Name, s.Name, filepath.Dir(path), append([]string{path}, s.Args...))
	u.MayDecline = s.MayDecline
	return Units{Atomic: []run.Spec{u}}, nil
}

func executableScriptPath(l Layout, s roster.Suite) (string, error) {
	path := l.resolve(s.Path)
	info, err := os.Stat(path)
	if err != nil {
		return "", fmt.Errorf("suites: %s's runner %s: %w", s.Name, path, err)
	}
	if info.Mode()&0o111 == 0 {
		return "", fmt.Errorf("suites: %s's runner %s is not executable", s.Name, path)
	}
	return path, nil
}
