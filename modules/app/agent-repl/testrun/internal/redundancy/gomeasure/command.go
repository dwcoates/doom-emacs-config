package gomeasure

import (
	"context"
	"flag"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
	"time"

	"agentrepl/testrun/internal/redundancy"
	"agentrepl/testrun/internal/run"
)

// EnvBackground is the marker bin/background.sh exports; the measurement is
// the heaviest test load there is and never runs at normal priority.
const EnvBackground = "AGENT_REPL_BACKGROUND_PRIORITY"

// DefaultOverhead is the fixed weight each kept case adds to its runtime,
// so that among sets of equal runtime the smaller one wins.
const DefaultOverhead = 100 * time.Microsecond

// Args are `testrun redundancy go`'s arguments.
type Args struct {
	Config
	Out         string
	Universe    redundancy.Universe
	Overhead    time.Duration
	KeepScratch bool
}

func splitList(s string) []string {
	var out []string
	for _, p := range strings.Split(s, ",") {
		if p = strings.TrimSpace(p); p != "" {
			out = append(out, p)
		}
	}
	return out
}

// ParseArgs reads `testrun redundancy go`'s command line.
func ParseArgs(argv []string, stderr io.Writer) (Args, error) {
	fs := flag.NewFlagSet("testrun redundancy go", flag.ContinueOnError)
	fs.SetOutput(stderr)
	var a Args
	var tests, mutate, pinCalls, pins, universe string
	fs.StringVar(&a.Module, "module", "", "the Go module's root (required)")
	fs.StringVar(&a.Package, "pkg", "", "the package under measurement, relative to -module, e.g. ./internal/workspace (required)")
	fs.StringVar(&tests, "tests", "", "comma-separated test files of -pkg whose cases are the candidates (required)")
	fs.StringVar(&mutate, "mutate", "", "comma-separated packages to mutate, relative to -module (default: -pkg)")
	fs.IntVar(&a.Runs, "runs", 3, "coverage runs per case, unioned")
	fs.StringVar(&pinCalls, "pin-calls", "FindRecord,Records", "comma-separated calls that make a test a log-record assertion")
	fs.StringVar(&pins, "pin", "", "comma-separated tests kept whatever they cover")
	fs.StringVar(&a.Out, "out", "", "directory the measurement and report are written to (required)")
	fs.StringVar(&universe, "universe", "kills-only", "kills-only or strict (killed mutants and reached blocks)")
	fs.DurationVar(&a.Overhead, "overhead", DefaultOverhead, "fixed weight per kept case")
	fs.BoolVar(&a.KeepScratch, "keep-scratch", false, "keep the scratch directory")
	fs.StringVar(&a.Go, "go", "go", "the go command")
	if err := fs.Parse(argv); err != nil {
		return Args{}, err
	}
	if fs.NArg() != 0 {
		return Args{}, fmt.Errorf("unexpected arguments: %s", strings.Join(fs.Args(), " "))
	}
	for name, v := range map[string]string{"-module": a.Module, "-pkg": a.Package, "-tests": tests, "-out": a.Out} {
		if v == "" {
			return Args{}, fmt.Errorf("%s is required", name)
		}
	}
	u, err := redundancy.ParseUniverse(universe)
	if err != nil {
		return Args{}, err
	}
	a.Universe = u
	a.Tests = splitList(tests)
	a.Mutate = splitList(mutate)
	if len(a.Mutate) == 0 {
		a.Mutate = []string{a.Package}
	}
	a.PinCalls = splitList(pinCalls)
	a.Pins = splitList(pins)
	if a.Module, err = filepath.Abs(a.Module); err != nil {
		return Args{}, fmt.Errorf("resolve -module: %w", err)
	}
	return a, nil
}

// Command is `testrun redundancy go`: measure, then write measurement.json
// and report.md under -out.
func Command(ctx context.Context, log *run.Log, argv []string, exec Exec) int {
	if os.Getenv(EnvBackground) == "" {
		log.Errorf("redundancy: refused at normal priority; run it through bin/background.sh")
		return 78
	}
	a, err := ParseArgs(argv, os.Stderr)
	if err != nil {
		log.Errorf("redundancy: %v", err)
		return 2
	}
	if err := os.MkdirAll(a.Out, 0o755); err != nil {
		log.Errorf("redundancy: create -out %s: %v", a.Out, err)
		return 1
	}
	scratch, err := os.MkdirTemp("/tmp", "agent-repl-redundancy-")
	if err != nil {
		log.Errorf("redundancy: make the scratch directory: %v", err)
		return 1
	}
	if a.KeepScratch {
		log.Infof("redundancy: scratch kept at %s", scratch)
	} else {
		defer os.RemoveAll(scratch)
	}
	a.Scratch = scratch
	m, err := Measure(ctx, a.Config, exec, log)
	if err != nil {
		log.Errorf("redundancy: measure %s %s: %v", a.Package, strings.Join(a.Tests, ","), err)
		return 1
	}
	if err := WriteOutputs(a.Out, m, a.Universe, a.Overhead); err != nil {
		log.Errorf("redundancy: %v", err)
		return 1
	}
	log.Infof("redundancy: wrote %s and %s", filepath.Join(a.Out, "measurement.json"), filepath.Join(a.Out, "report.md"))
	return 0
}

// WriteOutputs writes measurement.json and report.md under dir.
func WriteOutputs(dir string, m *redundancy.Measurement, u redundancy.Universe, overhead time.Duration) error {
	if err := redundancy.WriteMeasurement(filepath.Join(dir, "measurement.json"), m); err != nil {
		return err
	}
	f, err := os.Create(filepath.Join(dir, "report.md"))
	if err != nil {
		return fmt.Errorf("redundancy: create the report: %w", err)
	}
	if err := redundancy.Report(f, m, u, overhead); err != nil {
		f.Close()
		return err
	}
	if err := f.Close(); err != nil {
		return fmt.Errorf("redundancy: close the report: %w", err)
	}
	return nil
}
