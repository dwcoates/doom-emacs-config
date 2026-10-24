package cli

import (
	"errors"
	"flag"
	"fmt"
	"io"
	"slices"
	"strconv"
	"strings"
	"time"

	testrunv1 "agentrepl/proto/testrun/v1"

	"agentrepl/testrun/internal/reuse"
	"agentrepl/testrun/internal/selection"
	"agentrepl/testrun/roster"
)

// Args is a parsed `testrun run` command line.
type Args struct {
	// Module is the agent-repl module root.
	Module string
	// Record stages the run's suite timings for `testrun finish-record` to
	// commit to test_time.csv. `run` itself never writes the canonical
	// history file: see RecordOut and "testrun run only STAGES --record" in
	// run.go.
	Record bool
	// RecordOut is where a recording run stages its timing rows (see
	// PendingRecord). Required when the run records; bin/test-all.sh offers
	// it on every run, since a selection file can ask for a record.
	RecordOut string
	// Coverage enables the expensive coverage instrumentation and reports.
	Coverage bool
	// Selected is the --suites narrowing; EMPTY MEANS EVERY SUITE.
	Selected []string
	// NoReuse (--no-reuse) runs every selected suite even when an earlier
	// run passed its exact inputs (agentrepl/testrun/internal/reuse).
	NoReuse bool
	// Targeted marks a run narrowed below whole suites (a test-name filter):
	// it neither reuses nor records a pass, because a pass of some of a
	// suite's tests says nothing about the rest.
	Targeted bool
	// Selection (--selection) is a file holding a testrun.v1.TestRunRequest:
	// the run runs exactly the tests it names (internal/selection), and is
	// Targeted.
	Selection string
	// EventsFD (--events-fd) is the file descriptor the run writes its
	// testrun.v1.TestRunEvent stream to; zero writes none.
	EventsFD int
}

// ReuseMode is how the run treats pass records. A --record run measures and a
// --coverage run instruments every suite it selects, so neither may skip one.
func (a Args) ReuseMode() reuse.Mode {
	switch {
	case a.Targeted:
		return reuse.Targeted
	case a.NoReuse, a.Record, a.Coverage:
		return reuse.NoReuse
	}
	return reuse.Reusing
}

// ParseArgs reads `--module DIR [--record --record-out PATH] [--coverage]
// [--no-reuse] [--suites a,b] [--selection PATH] [--events-fd N]`. An unknown suite is an error, never a silently empty run:
// a caller that misspells a suite must not be told it passed.
func ParseArgs(argv []string) (Args, error) {
	var a Args
	for i := 0; i < len(argv); i++ {
		arg := argv[i]
		switch {
		case arg == "--record":
			a.Record = true
		case arg == "--record-out":
			if i+1 >= len(argv) {
				return Args{}, errors.New("--record-out needs a path")
			}
			i++
			a.RecordOut = argv[i]
		case arg == "--coverage":
			a.Coverage = true
		case arg == "--selection":
			if i+1 >= len(argv) {
				return Args{}, errors.New("--selection needs a path")
			}
			i++
			a.Selection = argv[i]
			a.Targeted = true
		case arg == "--events-fd":
			if i+1 >= len(argv) {
				return Args{}, errors.New("--events-fd needs a file descriptor")
			}
			i++
			fd, err := strconv.Atoi(argv[i])
			if err != nil || fd < 3 {
				return Args{}, fmt.Errorf("--events-fd %q is not a file descriptor past stderr", argv[i])
			}
			a.EventsFD = fd
		case arg == "--no-reuse":
			a.NoReuse = true
		case arg == "--module":
			if i+1 >= len(argv) {
				return Args{}, errors.New("--module needs a directory")
			}
			i++
			a.Module = argv[i]
		case arg == "--suites":
			if i+1 >= len(argv) {
				return Args{}, errors.New("--suites needs a comma-separated suite list")
			}
			i++
			sel, err := parseSuites(argv[i])
			if err != nil {
				return Args{}, err
			}
			a.Selected = append(a.Selected, sel...)
		case strings.HasPrefix(arg, "--suites="):
			sel, err := parseSuites(strings.TrimPrefix(arg, "--suites="))
			if err != nil {
				return Args{}, err
			}
			a.Selected = append(a.Selected, sel...)
		default:
			return Args{}, fmt.Errorf("unknown argument '%s', expected --record, --record-out, --coverage, --no-reuse, --suites <list>, --selection <path> or --events-fd <n>", arg)
		}
	}
	if a.Module == "" {
		return Args{}, errors.New("--module is required")
	}
	if a.Selection != "" && (len(a.Selected) > 0 || a.Record || a.Coverage) {
		// A selection names its own tests; a suite list, a canonical timing
		// history or a coverage report is a whole-suite run's.
		return Args{}, errors.New("--selection cannot be combined with --suites, --record or --coverage")
	}
	if a.Record && a.Coverage {
		// Instrumentation and the report units inflate every Go and vitest
		// suite's unit time, so a coverage run's figures are not comparable
		// with the canonical history's.
		return Args{}, errors.New("--record and --coverage cannot be combined: coverage instrumentation changes the suite timings --record keeps")
	}
	if err := a.coverageOnlyNamed(); err != nil {
		return Args{}, err
	}
	if a.Record && a.RecordOut == "" {
		return Args{}, errors.New("--record needs --record-out: the path `run` stages this run's timing rows at, for a later `testrun finish-record` to commit outside the git-state net")
	}
	return a, nil
}

// WithRequest lowers a selection file's request onto the run's arguments. A
// request of whole suites IS an ordinary suite run (--suites, or every suite
// for every_suite): it reuses, covers and records exactly as one, and is no
// longer Targeted. A narrower request stays Targeted, and may neither cover
// nor record, since both are whole suites' measurements.
func (a Args) WithRequest(req *testrunv1.TestRunRequest) (Args, error) {
	names, whole, err := selection.Whole(req)
	if err != nil {
		return Args{}, err
	}
	switch {
	case req.GetRecord() && req.GetCoverage():
		return Args{}, errors.New("record and coverage cannot be combined: coverage instrumentation changes the suite timings record keeps")
	case req.GetCoverage() && !whole:
		return Args{}, errors.New("coverage measures whole suites: select suites (suite or every_suite), not tests within them")
	case req.GetRecord() && (!whole || len(names) > 0):
		return Args{}, errors.New("record keeps the whole roster's timings: the request's one selection must be every_suite")
	case req.GetRecord() && a.RecordOut == "":
		return Args{}, errors.New("record needs --record-out: the path the run stages its timing rows at, for `testrun finish-record` to commit")
	}
	if !whole {
		return a, nil
	}
	a.Selection, a.Targeted = "", false
	a.Selected = names
	a.Coverage, a.Record = req.GetCoverage(), req.GetRecord()
	if err := a.coverageOnlyNamed(); err != nil {
		return Args{}, err
	}
	return a, nil
}

// FinishRecordArgs is a parsed `testrun finish-record` command line.
type FinishRecordArgs struct {
	// Module is the agent-repl module root.
	Module string
	// RecordOut is the path a prior `testrun run --record` staged its rows
	// at.
	RecordOut string
}

// ParseFinishRecordArgs reads `--module DIR --record-out PATH`.
func ParseFinishRecordArgs(argv []string) (FinishRecordArgs, error) {
	var a FinishRecordArgs
	for i := 0; i < len(argv); i++ {
		switch argv[i] {
		case "--module":
			if i+1 >= len(argv) {
				return FinishRecordArgs{}, errors.New("--module needs a directory")
			}
			i++
			a.Module = argv[i]
		case "--record-out":
			if i+1 >= len(argv) {
				return FinishRecordArgs{}, errors.New("--record-out needs a path")
			}
			i++
			a.RecordOut = argv[i]
		default:
			return FinishRecordArgs{}, fmt.Errorf("unknown argument '%s', expected --module or --record-out", argv[i])
		}
	}
	if a.Module == "" {
		return FinishRecordArgs{}, errors.New("--module is required")
	}
	if a.RecordOut == "" {
		return FinishRecordArgs{}, errors.New("--record-out is required")
	}
	return a, nil
}

func parseSuites(spec string) ([]string, error) {
	if spec == "" {
		return nil, errors.New("--suites needs at least one suite name")
	}
	var out []string
	for name := range strings.SplitSeq(spec, ",") {
		if name == "" {
			return nil, fmt.Errorf("--suites contains an empty suite name: '%s'", spec)
		}
		if _, ok := roster.Lookup(name); !ok {
			return nil, fmt.Errorf("--suites names an unknown suite '%s'; known suites: %s", name, strings.Join(roster.Names(), " "))
		}
		out = append(out, name)
	}
	return out, nil
}

// Selects reports whether a suite is part of the run.
func (a Args) Selects(name string) bool {
	named := slices.Contains(a.Selected, name)
	if s, ok := roster.Lookup(name); ok {
		switch s.Runs {
		case roster.RunsInCoverage:
			if !a.Coverage {
				return false
			}
		case roster.RunsWhenNamed:
			return named
		}
	}
	return len(a.Selected) == 0 || named
}

// skipReason says why a run that selects suites by name or in full skips s:
// its run condition, or its absence from --suites.
func (a Args) skipReason(s roster.Suite) string {
	switch {
	case s.Runs == roster.RunsInCoverage && !a.Coverage:
		return "runs only in a coverage run"
	case s.Runs == roster.RunsWhenNamed:
		return "runs only when named"
	}
	return "not selected by --suites"
}

// coverageOnlyNamed refuses a run without coverage that names a suite which
// runs only in a coverage run: naming it and running nothing would read as a
// pass.
func (a Args) coverageOnlyNamed() error {
	if a.Coverage {
		return nil
	}
	for _, name := range a.Selected {
		if s, ok := roster.Lookup(name); ok && s.Runs == roster.RunsInCoverage {
			return fmt.Errorf("suite %s runs only in a coverage run; ask for coverage", name)
		}
	}
	return nil
}

// CoverArgs is a parsed `testrun cover-report` command line.
type CoverArgs struct {
	// Name is the Go module's suite name.
	Name string
	// Module is the Go module directory.
	Module string
	// CovDirs holds one coverage directory per package unit.
	CovDirs string
}

// ParseCoverArgs reads `-name N -module DIR -covdirs ROOT`, every one required.
func ParseCoverArgs(argv []string) (CoverArgs, error) {
	fs := flag.NewFlagSet("cover-report", flag.ContinueOnError)
	fs.SetOutput(io.Discard)
	var a CoverArgs
	fs.StringVar(&a.Name, "name", "", "the module's suite name")
	fs.StringVar(&a.Module, "module", "", "the Go module directory")
	fs.StringVar(&a.CovDirs, "covdirs", "", "the directory holding one coverage directory per package unit")
	if err := fs.Parse(argv); err != nil {
		return CoverArgs{}, fmt.Errorf("cover-report: %w", err)
	}
	if fs.NArg() > 0 {
		return CoverArgs{}, fmt.Errorf("cover-report: unexpected arguments %v", fs.Args())
	}
	if a.Name == "" || a.Module == "" || a.CovDirs == "" {
		return CoverArgs{}, errors.New("cover-report needs -name, -module and -covdirs")
	}
	return a, nil
}

// WardenArgs is a parsed `testrun warden` command line (internal/life).
type WardenArgs struct {
	// Runner is the runner the warden watches, and its session id.
	Runner int
	// Since is when the runner started.
	Since time.Time
	// Sentinel is the runner's sentinel.
	Sentinel int
}

// ParseWardenArgs reads `--runner PID --since UNIX --sentinel PID`, every one
// required.
func ParseWardenArgs(argv []string) (WardenArgs, error) {
	fs := flag.NewFlagSet("warden", flag.ContinueOnError)
	fs.SetOutput(io.Discard)
	runner := fs.Int("runner", 0, "the runner's pid, its session id")
	since := fs.Int64("since", 0, "when the runner started, in Unix seconds")
	sentinel := fs.Int("sentinel", 0, "the runner's sentinel's pid")
	if err := fs.Parse(argv); err != nil {
		return WardenArgs{}, fmt.Errorf("warden: %w", err)
	}
	if fs.NArg() > 0 {
		return WardenArgs{}, fmt.Errorf("warden: unexpected arguments %v", fs.Args())
	}
	if *runner <= 0 || *since <= 0 || *sentinel <= 0 {
		return WardenArgs{}, errors.New("warden needs --runner, --since and --sentinel")
	}
	return WardenArgs{Runner: *runner, Since: time.Unix(*since, 0), Sentinel: *sentinel}, nil
}
