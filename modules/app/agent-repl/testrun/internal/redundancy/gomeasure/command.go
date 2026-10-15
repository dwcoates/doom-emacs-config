package gomeasure

import (
	"context"
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
	"time"

	"agentrepl/testrun/internal/redundancy"
	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/internal/suites"
	"agentrepl/testrun/roster"
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
	Budget      redundancy.Budget
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

// rosterSuites are the suites of roster Go suite name: every ordinary tested
// package and every package of its tagged integration pass, each with the
// programs the roster says it spawns. Its module is module, or the suite's
// own found from the working directory when module is "".
func rosterSuites(name, module string) ([]Suite, string, error) {
	s, ok := roster.Lookup(name)
	if !ok || s.Kind != roster.GoModule {
		return nil, "", fmt.Errorf("-roster-suite %q is no Go suite of the roster", name)
	}
	start := module
	if start == "" {
		wd, err := os.Getwd()
		if err != nil {
			return nil, "", fmt.Errorf("resolve the working directory: %w", err)
		}
		start = wd
	}
	root, err := FindRoot(start)
	if err != nil {
		return nil, "", err
	}
	l := suites.Layout{Module: root}
	if module == "" {
		module = filepath.Join(root, s.Path)
	}
	pkgs, ipkgs, err := suites.GoSuitePackages(l, s)
	if err != nil {
		return nil, "", err
	}
	var out []Suite
	for _, rel := range pkgs {
		out = append(out, Suite{Package: "./" + rel, Programs: s.Programs[rel]})
	}
	for _, rel := range ipkgs {
		out = append(out, Suite{Package: "./" + rel, Tags: s.IntegrationTags, Programs: s.Programs[rel]})
	}
	return out, module, nil
}

// readDeleteSet reads a -delete file: one case per line; blank lines and
// lines starting with # are skipped.
func readDeleteSet(path string) ([]string, error) {
	b, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("read -delete %s: %w", path, err)
	}
	var out []string
	for _, l := range strings.Split(string(b), "\n") {
		if l = strings.TrimSpace(l); l != "" && !strings.HasPrefix(l, "#") {
			out = append(out, l)
		}
	}
	if len(out) == 0 {
		return nil, fmt.Errorf("-delete %s names no case", path)
	}
	return out, nil
}

// readSuites reads a -suites file.
func readSuites(path string) ([]Suite, error) {
	b, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("read -suites %s: %w", path, err)
	}
	var s []Suite
	if err := json.Unmarshal(b, &s); err != nil {
		return nil, fmt.Errorf("decode -suites %s: %w", path, err)
	}
	if len(s) == 0 {
		return nil, fmt.Errorf("-suites %s names no suite", path)
	}
	for i, x := range s {
		if x.Package == "" {
			return nil, fmt.Errorf("-suites %s: suite %d names no Package", path, i)
		}
	}
	return s, nil
}

// ParseArgs reads `testrun redundancy go`'s command line.
func ParseArgs(argv []string, stderr io.Writer) (Args, error) {
	fs := flag.NewFlagSet("testrun redundancy go", flag.ContinueOnError)
	fs.SetOutput(stderr)
	var a Args
	var pkg, tests, tags, programs, suitesFile, mutate, pinCalls, pins, universe string
	fs.StringVar(&a.Module, "module", "", "the Go module's root (required)")
	fs.StringVar(&pkg, "pkg", "", "one package under measurement, relative to -module, e.g. ./internal/workspace")
	fs.StringVar(&tests, "tests", "", "comma-separated test files of -pkg whose cases are measured (default: all of its tests)")
	fs.StringVar(&tags, "tags", "", "the build tags -pkg's tests compile under")
	fs.StringVar(&programs, "programs", "", "comma-separated roster programs -pkg's tests spawn")
	fs.StringVar(&suitesFile, "suites", "", "a JSON file listing the suites to measure ([{Package, Tags, Tests, Programs, Kept}]), instead of -pkg")
	rosterSuite := fs.String("roster-suite", "", "measure every package of this roster Go suite (e.g. daemon), its integration pass included, instead of -pkg; -module defaults to its module")
	fs.StringVar(&mutate, "mutate", "", "comma-separated packages to mutate, relative to -module (default: the one -pkg)")
	fs.IntVar(&a.Runs, "runs", 3, "coverage runs per case, unioned")
	fs.StringVar(&pinCalls, "pin-calls", "FindRecord,Records", "comma-separated calls that make a test a log-record assertion")
	fs.StringVar(&pins, "pin", "", "comma-separated tests kept whatever they cover")
	fs.StringVar(&a.Out, "out", "", "directory the measurement and report are written to (required)")
	fs.StringVar(&universe, "universe", "kills-only", "kills-only or strict (killed mutants and reached blocks)")
	fs.DurationVar(&a.Overhead, "overhead", DefaultOverhead, "fixed weight per kept case")
	fs.BoolVar(&a.KeepScratch, "keep-scratch", false, "keep the scratch directory")
	deleteFile := fs.String("delete", "", "a file naming a deletion set to verify, one case ID or top-level test per line, instead of minimizing")
	fs.BoolVar(&a.Estimate, "estimate", false, "with -delete: stop after the unit tier and report what the rest would cost")
	maxDrop := fs.Float64("max-coverage-drop", redundancy.DefaultMaxDrop, "the most statement-coverage points any system may lose")
	baseline := fs.String("baseline", "", "the pinned coverage baseline (testrun/coverage-baseline.json); without it each system is held to the measured population's own coverage")
	fs.StringVar(&a.Go, "go", "go", "the go command")
	fs.DurationVar(&a.HangBound, "hang-bound", DefaultHangBound, "how long a case that hung reruns alone to confirm the hang; just over twice the longest real wait the measured tests allow themselves")
	if err := fs.Parse(argv); err != nil {
		return Args{}, err
	}
	if fs.NArg() != 0 {
		return Args{}, fmt.Errorf("unexpected arguments: %s", strings.Join(fs.Args(), " "))
	}
	if a.Out == "" {
		return Args{}, fmt.Errorf("-out is required")
	}
	named := 0
	for _, v := range []string{pkg, suitesFile, *rosterSuite} {
		if v != "" {
			named++
		}
	}
	switch {
	case named != 1:
		return Args{}, fmt.Errorf("name exactly one of -pkg, -suites and -roster-suite")
	case pkg == "" && (tests != "" || tags != "" || programs != ""):
		return Args{}, fmt.Errorf("-tests, -tags and -programs describe -pkg; with -suites or -roster-suite each suite says its own")
	case *rosterSuite != "":
		s, module, err := rosterSuites(*rosterSuite, a.Module)
		if err != nil {
			return Args{}, err
		}
		a.Suites, a.Module = s, module
	case pkg != "":
		a.Suites = []Suite{{Package: pkg, Tags: tags, Tests: splitList(tests), Programs: splitList(programs)}}
	default:
		s, err := readSuites(suitesFile)
		if err != nil {
			return Args{}, err
		}
		a.Suites = s
	}
	if a.Module == "" {
		return Args{}, fmt.Errorf("-module is required")
	}
	u, err := redundancy.ParseUniverse(universe)
	if err != nil {
		return Args{}, err
	}
	a.Universe = u
	if a.Budget, err = redundancy.LoadBudget(*maxDrop, *baseline); err != nil {
		return Args{}, err
	}
	a.Mutate = splitList(mutate)
	if len(a.Mutate) == 0 {
		if len(a.Suites) != 1 {
			return Args{}, fmt.Errorf("-mutate is required when -suites names several")
		}
		a.Mutate = []string{a.Suites[0].Package}
	}
	if *deleteFile != "" {
		if a.Delete, err = readDeleteSet(*deleteFile); err != nil {
			return Args{}, err
		}
	} else if a.Estimate {
		return Args{}, fmt.Errorf("-estimate needs -delete")
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
		var names []string
		for _, s := range a.Suites {
			names = append(names, s.Package)
		}
		log.Errorf("redundancy: measure %s: %v", strings.Join(names, ", "), err)
		return 1
	}
	if err := WriteOutputs(a.Out, m, redundancy.Options{Universe: a.Universe, Overhead: a.Overhead, Budget: a.Budget}); err != nil {
		log.Errorf("redundancy: %v", err)
		return 1
	}
	log.Infof("redundancy: wrote %s and %s", filepath.Join(a.Out, "measurement.json"), filepath.Join(a.Out, "report.md"))
	return 0
}

// WriteOutputs writes measurement.json and report.md under dir.
func WriteOutputs(dir string, m *redundancy.Measurement, o redundancy.Options) error {
	if err := redundancy.WriteMeasurement(filepath.Join(dir, "measurement.json"), m); err != nil {
		return err
	}
	f, err := os.Create(filepath.Join(dir, "report.md"))
	if err != nil {
		return fmt.Errorf("redundancy: create the report: %w", err)
	}
	if err := redundancy.Report(f, m, o); err != nil {
		f.Close()
		return err
	}
	if err := f.Close(); err != nil {
		return fmt.Errorf("redundancy: close the report: %w", err)
	}
	return nil
}
