package cli

import (
	"errors"
	"fmt"
	"slices"
	"strings"

	"agentrepl/testrun/roster"
)

// Args is a parsed `testrun run` command line.
type Args struct {
	// Module is the agent-repl module root.
	Module string
	// Record appends the run's suite timings to test_time.csv.
	Record bool
	// Coverage enables the expensive coverage instrumentation and reports.
	Coverage bool
	// Selected is the --suites narrowing; EMPTY MEANS EVERY SUITE.
	Selected []string
}

// ParseArgs reads `--module DIR [--record] [--coverage] [--suites a,b]`. An unknown suite
// is an error, never a silently empty run: a caller that misspells a suite
// must not be told it passed.
func ParseArgs(argv []string) (Args, error) {
	var a Args
	for i := 0; i < len(argv); i++ {
		arg := argv[i]
		switch {
		case arg == "--record":
			a.Record = true
		case arg == "--coverage":
			a.Coverage = true
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
			return Args{}, fmt.Errorf("unknown argument '%s', expected --record, --coverage, or --suites <list>", arg)
		}
	}
	if a.Module == "" {
		return Args{}, errors.New("--module is required")
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
	return len(a.Selected) == 0 || slices.Contains(a.Selected, name)
}
