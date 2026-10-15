package vitestmeasure

import (
	"context"
	"flag"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"

	"agentrepl/testrun/internal/redundancy"
	"agentrepl/testrun/internal/redundancy/proc"
	"agentrepl/testrun/internal/run"
)

// Args are `testrun redundancy vitest`'s arguments.
type Args struct {
	Config
	Common redundancy.Common
}

// ParseArgs reads `testrun redundancy vitest -package DIR -mutate GLOBS
// -out DIR [...]`.
func ParseArgs(argv []string, stderr io.Writer) (Args, error) {
	fs := flag.NewFlagSet("testrun redundancy vitest", flag.ContinueOnError)
	fs.SetOutput(stderr)
	var a Args
	var mutate string
	finish := a.Common.Register(fs)
	fs.StringVar(&a.Package, "package", "", "the npm package directory, e.g. webapp (required)")
	fs.StringVar(&a.System, "system", "", "its coverage system's name in testrun/coverage-baseline.json (default: the package directory's name)")
	fs.StringVar(&a.ConfigFile, "config", "vitest.config.ts", "its vitest config, relative to -package")
	fs.StringVar(&mutate, "mutate", "", "comma-separated source globs Stryker mutates, relative to -package (required)")
	fs.IntVar(&a.Runs, "runs", 3, "traced runs per test, unioned")
	if err := fs.Parse(argv); err != nil {
		return Args{}, err
	}
	if fs.NArg() != 0 {
		return Args{}, fmt.Errorf("unexpected arguments: %s", strings.Join(fs.Args(), " "))
	}
	if err := finish(); err != nil {
		return Args{}, err
	}
	if a.Package == "" {
		return Args{}, fmt.Errorf("-package is required")
	}
	if a.Mutate = redundancy.SplitList(mutate); len(a.Mutate) == 0 {
		return Args{}, fmt.Errorf("-mutate is required")
	}
	var err error
	// The real path: vitest instruments only files under its root, and a
	// package reached through a symlink (/tmp is /private/tmp) left its root
	// on the link and every source past it, so no test was covered.
	if a.Package, err = redundancy.RealPath(a.Package); err != nil {
		return Args{}, err
	}
	if a.System == "" {
		a.System = filepath.Base(a.Package)
	}
	root, err := redundancy.FindRoot(a.Package)
	if err != nil {
		return Args{}, err
	}
	a.Trace = filepath.Join(root, "testrun", "vitest", "redundancy-trace.mjs")
	a.Delete, a.Pins = a.Common.Delete, a.Common.Pins
	return a, nil
}

// Command is `testrun redundancy vitest`: measure, then write
// measurement.json and report.md under -out (redundancy.Run).
func Command(ctx context.Context, log *run.Log, argv []string, exec proc.Exec) int {
	if code := redundancy.RefusedAtNormalPriority(log); code != 0 {
		return code
	}
	a, err := ParseArgs(argv, os.Stderr)
	if err != nil {
		log.Errorf("redundancy: %v", err)
		return 2
	}
	return redundancy.Run(log, a.Common, a.System+" "+a.ConfigFile, func(scratch string) (*redundancy.Measurement, error) {
		cfg := a.Config
		cfg.Scratch = scratch
		cfg.Gate = proc.Gate{Log: log, MaxLoad: a.Common.MaxLoad, QuietWait: a.Common.QuietWait}
		return Measure(ctx, cfg, exec, log)
	})
}
