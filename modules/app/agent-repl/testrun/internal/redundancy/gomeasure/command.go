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

	"agentrepl/testrun/caselist"
	"agentrepl/testrun/internal/redundancy"
	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/internal/suites"
	"agentrepl/testrun/roster"
)

// EnvBackground is redundancy's, kept here for this package's callers.
const EnvBackground = redundancy.EnvBackground

// DefaultOverhead is redundancy's.
const DefaultOverhead = redundancy.DefaultOverhead

// DefaultPinCalls are the calls that assert a canonical log record: the
// unit tier's dlog test logger (FindRecord, Records) and the integration
// harness's record and operation awaits. A test reaching one stays whatever
// it covers (owner rule: a test pinning an error path and its canonical log
// record stays). The harness's AwaitShimLoggedRequest reads the fake shim's
// request log, not a record, and is not one.
var DefaultPinCalls = []string{
	"FindRecord", "Records",
	"AwaitLogRecord", "AwaitLogRecordWithin", "AwaitLogRecordAfter",
	"AwaitWorkspaceLogRecord", "AwaitWorkspaceLogRecordInState", "AwaitRunLogRecordFromAnyProcess",
	"AwaitRunLogOperation", "AwaitWorkspaceLogOperation", "AwaitWorkspaceLogOperationCount",
}

// Args are `testrun redundancy go`'s arguments: the measurement's own and
// the flags every adapter shares.
type Args struct {
	Config
	Common redundancy.Common
}

// rosterSuites are the suites of the named roster Go suites (comma
// separated): every ordinary tested package and every package of a tagged
// integration pass, each with the programs the roster says it spawns; an
// e2e suite's root package fills its shared prebuild first, as a run's does.
// The measurement's module is module, or the one named suite's own, found
// from the working directory, when module is "". With several suites each
// names its own module.
func rosterSuites(names, module string) ([]Suite, string, error) {
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
	list := caselist.SplitList(names)
	if len(list) > 1 && module == "" {
		return nil, "", fmt.Errorf("-roster-suite names several suites, so -module must name the mutated one")
	}
	l := suites.Layout{Module: root}
	var out []Suite
	for _, name := range list {
		s, ok := roster.Lookup(name)
		if !ok || (s.Kind != roster.GoModule && s.Kind != roster.E2E) {
			return nil, "", fmt.Errorf("-roster-suite %q is no Go suite of the roster", name)
		}
		if module == "" {
			module = filepath.Join(root, s.Path)
		}
		own := ""
		if len(list) > 1 {
			own = s.Path
		}
		pkgs, ipkgs, err := suites.GoSuitePackages(l, s)
		if err != nil {
			return nil, "", err
		}
		for _, rel := range pkgs {
			out = append(out, Suite{Package: "./" + rel, Programs: s.Programs[rel], Module: own, Prebuild: s.Kind == roster.E2E && rel == "."})
		}
		for _, rel := range ipkgs {
			out = append(out, Suite{Package: "./" + rel, Tags: s.IntegrationTags, Programs: s.Programs[rel], Module: own})
		}
	}
	return out, module, nil
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
	var pkg, tests, tags, programs, suitesFile, mutate, pinCalls string
	finish := a.Common.Register(fs)
	fs.StringVar(&a.Module, "module", "", "the Go module's root (required)")
	fs.StringVar(&pkg, "pkg", "", "one package under measurement, relative to -module, e.g. ./internal/workspace")
	fs.StringVar(&tests, "tests", "", "comma-separated test files of -pkg whose cases are measured (default: all of its tests)")
	fs.StringVar(&tags, "tags", "", "the build tags -pkg's tests compile under")
	fs.StringVar(&programs, "programs", "", "comma-separated roster programs -pkg's tests spawn")
	fs.StringVar(&suitesFile, "suites", "", "a JSON file listing the suites to measure ([{Package, Tags, Tests, Programs, Kept}]), instead of -pkg")
	rosterSuite := fs.String("roster-suite", "", "measure every package of these roster Go suites (comma separated, e.g. daemon or daemon,e2e), integration passes included, instead of -pkg; -module defaults to the one suite's module")
	fs.StringVar(&mutate, "mutate", "", "comma-separated packages to mutate, relative to -module (default: the one -pkg)")
	fs.IntVar(&a.Runs, "runs", 3, "coverage runs per case, unioned")
	fs.StringVar(&pinCalls, "pin-calls", strings.Join(DefaultPinCalls, ","), "comma-separated calls that make a test a log-record assertion")
	fs.BoolVar(&a.Estimate, "estimate", false, "with -delete: stop after the unit tier and report what the rest would cost")
	fs.StringVar(&a.Go, "go", "go", "the go command")
	fs.DurationVar(&a.HangBound, "hang-bound", DefaultHangBound, "how long a case that hung reruns alone to confirm the hang; just over twice the longest real wait the measured tests allow themselves")
	if err := fs.Parse(argv); err != nil {
		return Args{}, err
	}
	if fs.NArg() != 0 {
		return Args{}, fmt.Errorf("unexpected arguments: %s", strings.Join(fs.Args(), " "))
	}
	if err := finish(); err != nil {
		return Args{}, err
	}
	a.Delete, a.Pins, a.MaxLoad, a.QuietWait, a.Procs = a.Common.Delete, a.Common.Pins, a.Common.MaxLoad, a.Common.QuietWait, a.Common.Procs
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
		a.Suites = []Suite{{Package: pkg, Tags: tags, Tests: caselist.SplitList(tests), Programs: caselist.SplitList(programs)}}
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
	a.Mutate = caselist.SplitList(mutate)
	if len(a.Mutate) == 0 {
		if len(a.Suites) != 1 {
			return Args{}, fmt.Errorf("-mutate is required when -suites names several")
		}
		a.Mutate = []string{a.Suites[0].Package}
	}
	if a.Estimate && len(a.Delete) == 0 {
		return Args{}, fmt.Errorf("-estimate needs -delete")
	}
	a.PinCalls = caselist.SplitList(pinCalls)
	var err error
	// The real path, as go list answers every package's directory.
	if a.Module, err = redundancy.RealPath(a.Module); err != nil {
		return Args{}, err
	}
	return a, nil
}

// Command is `testrun redundancy go`: measure, then write measurement.json
// and report.md under -out (redundancy.Run).
func Command(ctx context.Context, log *run.Log, argv []string, exec Exec) int {
	if code := redundancy.RefusedAtNormalPriority(log); code != 0 {
		return code
	}
	a, err := ParseArgs(argv, os.Stderr)
	if err != nil {
		log.Errorf("redundancy: %v", err)
		return 2
	}
	var names []string
	for _, s := range a.Suites {
		names = append(names, s.Package)
	}
	return redundancy.Run(log, a.Common, strings.Join(names, ", "), func(scratch string) (*redundancy.Measurement, error) {
		cfg := a.Config
		cfg.Scratch = scratch
		return Measure(ctx, cfg, exec, log)
	})
}
