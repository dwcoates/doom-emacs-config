package main

import (
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"

	"google.golang.org/protobuf/encoding/protojson"
	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	testrunv1 "agentrepl/proto/testrun/v1"
)

// testredundancycli.go — `agent-repld test redundancy go|vitest|report`: a
// `testrun redundancy` measurement, run by the daemon (agentrepl.v1
// RunRedundancy) from the workspace's worktree.
//
// The flags are the tool's own (modules/app/agent-repl/AGENTS.md, "Which
// tests are redundant"). A flag left off is left unset in the request, so the
// tool's own default holds. Paths are relative to the working directory (or
// absolute); -out and -in are made absolute, every other path relative to the
// worktree. -delete and -suites name files here, read into the request.

// redundancyFlags are one adapter's flags, bound to a request.
type redundancyFlags struct {
	fs  *flag.FlagSet
	req *testrunv1.RedundancyRequest
	// finish completes the request once the flags have parsed: paths
	// resolved, files read, unset optionals left unset.
	finish func(set map[string]bool, worktree, cwd string) error
}

func testRedundancy(c *cliCall, args []string) int {
	if len(args) == 0 || strings.HasPrefix(args[0], "-") {
		return c.usageFailed("name the measurement: go, vitest or report")
	}
	fs, stateDir := c.flags()
	workspace := fs.String("workspace", here, "the workspace whose worktree runs the measurement")
	rf, err := redundancyFlagsFor(args[0], fs)
	if err != nil {
		return c.usageFailed("%v", err)
	}
	positionals, ok := c.parse(fs, args[1:])
	if !ok {
		return exitFailure
	}
	if len(positionals) > 0 {
		return c.usageFailed("unexpected arguments %v; every setting is a flag", positionals)
	}
	set := map[string]bool{}
	fs.Visit(func(f *flag.Flag) { set[f.Name] = true })
	if !c.dial(*stateDir) {
		return exitFailure
	}
	ref, ok := c.named(*workspace)
	if !ok {
		return exitFailure
	}
	cwd, err := c.deps.getwd()
	if err != nil {
		c.failed("read the working directory: %v", err)
		return exitFailure
	}
	if err := rf.finish(set, canonicalDir(ref.GetDir()), canonicalDir(cwd)); err != nil {
		return c.usageFailed("%v", err)
	}
	stream, err := openDaemonStream(c.ctx, c.client.RunRedundancy, &agentreplv1.RunRedundancyRequest{Workspace: ref, Redundancy: rf.req})
	if errors.Is(err, errDryRun) {
		return exitSuccess
	}
	if err != nil {
		c.failed("the daemon failed the call: %v", err)
		return exitFailure
	}
	defer stream.Close() //nolint:errcheck // the stream has ended either way
	code, ended := exitFailure, false
	for stream.Receive() {
		frame := stream.Msg()
		switch {
		case frame.GetError() != nil:
			ended = true
			text, _ := protojson.Marshal(frame) //nolint:errcheck // a generated message always renders
			fmt.Fprintln(c.out, string(text))
			code = exitRefused
			if frame.GetError().GetRunnerFailed() != nil {
				code = exitFailure
			}
		case frame.GetEnded() != nil:
			code, ended = exitSuccess, true
			if frame.GetEnded().GetExitCode() != 0 {
				c.failed("the measurement exited %d", frame.GetEnded().GetExitCode())
				code = exitTestsFailed
			}
		default:
			fmt.Fprintln(c.out, frame.GetOutputLine())
		}
	}
	if err := stream.Err(); err != nil && !errors.Is(err, io.EOF) {
		c.failed("the measurement's stream broke: %v", err)
		return exitFailure
	}
	if !ended {
		c.failed("the measurement did not report how it ended")
	}
	return code
}

// redundancyFlagsFor binds the named adapter's flags to fs.
func redundancyFlagsFor(adapter string, fs *flag.FlagSet) (redundancyFlags, error) {
	switch adapter {
	case "go":
		return goRedundancyFlags(fs), nil
	case "vitest":
		return vitestRedundancyFlags(fs), nil
	case "report":
		return reportRedundancyFlags(fs), nil
	}
	return redundancyFlags{}, fmt.Errorf("unknown measurement %q: go, vitest or report", adapter)
}

// measurementFlags binds the flags every measurement shares; its finish
// fills m.
func measurementFlags(fs *flag.FlagSet, m *testrunv1.RedundancyMeasurement) func(set map[string]bool, repoPath func(string) (string, error)) error {
	out := fs.String("out", "", "directory the measurement and report are written to (required)")
	universe := fs.String("universe", "", "kills-only or strict")
	maxDrop := fs.Float64("max-coverage-drop", 0, "the most statement-coverage points any system may lose")
	baseline := fs.String("baseline", "", "the pinned coverage baseline")
	deleteFile := fs.String("delete", "", "a file naming a deletion set to verify, one case per line")
	pins := fs.String("pin", "", "comma-separated cases kept whatever they cover")
	overhead := fs.Duration("overhead", 0, "fixed weight per kept case")
	fs.BoolVar(&m.KeepScratch, "keep-scratch", false, "keep the scratch directory")
	maxLoad := fs.Float64("max-load", 0, "the most one-minute load average a phase or mutant starts under")
	quietWait := fs.Duration("quiet-wait", 0, "how long to wait for the load to drop before failing")
	procs := fs.Int("procs", 0, "GOMAXPROCS for every process the measurement runs")
	return func(set map[string]bool, repoPath func(string) (string, error)) error {
		if *out == "" {
			return errors.New("-out is required")
		}
		abs, err := filepath.Abs(*out)
		if err != nil {
			return fmt.Errorf("resolve -out %s: %w", *out, err)
		}
		m.OutDir = abs
		if m.Universe, err = redundancyUniverse(*universe); err != nil {
			return err
		}
		if set["max-coverage-drop"] {
			m.MaxCoverageDrop = proto.Float64(*maxDrop)
		}
		if *baseline != "" {
			if m.Baseline, err = repoPath(*baseline); err != nil {
				return err
			}
		}
		if *deleteFile != "" {
			if m.DeleteCases, err = readDeleteSet(*deleteFile); err != nil {
				return err
			}
		}
		m.Pins = commaItems(*pins)
		if set["overhead"] {
			m.OverheadUs = proto.Int64(overhead.Microseconds())
		}
		if set["max-load"] {
			m.MaxLoad = proto.Float64(*maxLoad)
		}
		if set["quiet-wait"] {
			m.QuietWaitMs = proto.Int64(quietWait.Milliseconds())
		}
		if set["procs"] {
			m.Procs = proto.Int32(int32(*procs))
		}
		return nil
	}
}

func goRedundancyFlags(fs *flag.FlagSet) redundancyFlags {
	g := &testrunv1.RedundancyGo{Measurement: &testrunv1.RedundancyMeasurement{}}
	common := measurementFlags(fs, g.Measurement)
	module := fs.String("module", "", "the Go module whose packages are mutated")
	pkg := fs.String("pkg", "", "one package under measurement, relative to -module")
	tests := fs.String("tests", "", "comma-separated test files of -pkg whose cases are measured")
	tags := fs.String("tags", "", "the build tags -pkg's tests compile under")
	programs := fs.String("programs", "", "comma-separated roster programs -pkg's tests spawn")
	rosterSuite := fs.String("roster-suite", "", "measure every package of these roster Go suites (comma separated)")
	suites := fs.String("suites", "", "a JSON file listing the suites to measure ([{Package, Tags, Tests, Programs, Kept, Module}])")
	mutate := fs.String("mutate", "", "comma-separated packages to mutate, relative to -module")
	runs := fs.Int("runs", 0, "coverage runs per case, unioned")
	pinCalls := fs.String("pin-calls", "", "comma-separated calls that make a test a log-record assertion")
	fs.BoolVar(&g.Estimate, "estimate", false, "with -delete: stop after the unit tier and report the rest's cost")
	hangBound := fs.Duration("hang-bound", 0, "how long a case that hung reruns alone to confirm the hang")
	req := &testrunv1.RedundancyRequest{Command: &testrunv1.RedundancyRequest_Go{Go: g}}
	return redundancyFlags{fs: fs, req: req, finish: func(set map[string]bool, worktree, cwd string) error {
		repoPath := worktreePath(worktree, cwd)
		if err := common(set, repoPath); err != nil {
			return err
		}
		if *module != "" {
			rel, err := repoPath(*module)
			if err != nil {
				return err
			}
			g.ModuleDir = rel
		}
		named := 0
		for _, v := range []string{*pkg, *rosterSuite, *suites} {
			if v != "" {
				named++
			}
		}
		switch {
		case named != 1:
			return errors.New("name exactly one of -pkg, -roster-suite and -suites")
		case *pkg != "":
			g.Subject = &testrunv1.RedundancyGo_Package{Package: &testrunv1.RedundancyGoPackage{Package: *pkg,
				TestFiles: commaItems(*tests), Tags: *tags, Programs: commaItems(*programs)}}
		case *rosterSuite != "":
			g.Subject = &testrunv1.RedundancyGo_RosterSuites{RosterSuites: &testrunv1.RedundancyGoRosterSuites{Names: commaItems(*rosterSuite)}}
		default:
			list, err := readGoSuites(*suites)
			if err != nil {
				return err
			}
			g.Subject = &testrunv1.RedundancyGo_Suites{Suites: &testrunv1.RedundancyGoSuiteList{Suites: list}}
		}
		g.Mutate = commaItems(*mutate)
		if set["runs"] {
			g.Runs = proto.Int32(int32(*runs))
		}
		if set["pin-calls"] {
			g.PinCalls = &testrunv1.RedundancyGoPinCalls{Calls: commaItems(*pinCalls)}
		}
		if set["hang-bound"] {
			g.HangBoundMs = proto.Int64(hangBound.Milliseconds())
		}
		return nil
	}}
}

func vitestRedundancyFlags(fs *flag.FlagSet) redundancyFlags {
	v := &testrunv1.RedundancyVitest{Measurement: &testrunv1.RedundancyMeasurement{}}
	common := measurementFlags(fs, v.Measurement)
	pkg := fs.String("package", "", "the npm package directory (required)")
	fs.StringVar(&v.System, "system", "", "its coverage system's name in the baseline")
	fs.StringVar(&v.Config, "config", "", "its vitest config, relative to -package")
	mutate := fs.String("mutate", "", "comma-separated source globs Stryker mutates, relative to -package (required)")
	runs := fs.Int("runs", 0, "traced runs per test, unioned")
	req := &testrunv1.RedundancyRequest{Command: &testrunv1.RedundancyRequest_Vitest{Vitest: v}}
	return redundancyFlags{fs: fs, req: req, finish: func(set map[string]bool, worktree, cwd string) error {
		repoPath := worktreePath(worktree, cwd)
		if err := common(set, repoPath); err != nil {
			return err
		}
		if *pkg == "" {
			return errors.New("-package is required")
		}
		rel, err := repoPath(*pkg)
		if err != nil {
			return err
		}
		v.PackageDir = rel
		if v.Mutate = commaItems(*mutate); len(v.Mutate) == 0 {
			return errors.New("-mutate is required")
		}
		if set["runs"] {
			v.Runs = proto.Int32(int32(*runs))
		}
		return nil
	}}
}

func reportRedundancyFlags(fs *flag.FlagSet) redundancyFlags {
	r := &testrunv1.RedundancyReport{}
	in := fs.String("in", "", "a measurement.json (required)")
	universe := fs.String("universe", "", "kills-only or strict")
	overhead := fs.Duration("overhead", 0, "fixed weight per kept case")
	maxDrop := fs.Float64("max-coverage-drop", 0, "the most statement-coverage points any system may lose")
	baseline := fs.String("baseline", "", "the pinned coverage baseline")
	req := &testrunv1.RedundancyRequest{Command: &testrunv1.RedundancyRequest_Report{Report: r}}
	return redundancyFlags{fs: fs, req: req, finish: func(set map[string]bool, worktree, cwd string) error {
		if *in == "" {
			return errors.New("-in is required")
		}
		abs, err := filepath.Abs(*in)
		if err != nil {
			return fmt.Errorf("resolve -in %s: %w", *in, err)
		}
		r.MeasurementFile = abs
		if r.Universe, err = redundancyUniverse(*universe); err != nil {
			return err
		}
		if set["overhead"] {
			r.OverheadUs = proto.Int64(overhead.Microseconds())
		}
		if set["max-coverage-drop"] {
			r.MaxCoverageDrop = proto.Float64(*maxDrop)
		}
		if *baseline != "" {
			if r.Baseline, err = worktreePath(worktree, cwd)(*baseline); err != nil {
				return err
			}
		}
		return nil
	}}
}

func redundancyUniverse(name string) (testrunv1.RedundancyUniverse, error) {
	switch name {
	case "":
		return testrunv1.RedundancyUniverse_REDUNDANCY_UNIVERSE_UNSPECIFIED, nil
	case "kills-only":
		return testrunv1.RedundancyUniverse_REDUNDANCY_UNIVERSE_KILLS_ONLY, nil
	case "strict":
		return testrunv1.RedundancyUniverse_REDUNDANCY_UNIVERSE_STRICT, nil
	}
	return 0, fmt.Errorf("unknown -universe %q: kills-only or strict", name)
}

// readDeleteSet reads a -delete file: one case per line; blank lines and
// lines starting with # are skipped, as the tool reads it.
func readDeleteSet(path string) ([]string, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("read -delete %s: %w", path, err)
	}
	var out []string
	for _, l := range strings.Split(string(data), "\n") {
		if l = strings.TrimSpace(l); l != "" && !strings.HasPrefix(l, "#") {
			out = append(out, l)
		}
	}
	if len(out) == 0 {
		return nil, fmt.Errorf("-delete %s names no case", path)
	}
	return out, nil
}

// readGoSuites reads a -suites file, the tool's JSON suite list.
func readGoSuites(path string) ([]*testrunv1.RedundancyGoSuite, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("read -suites %s: %w", path, err)
	}
	var list []struct {
		Package, Tags, Module string
		Tests, Programs       []string
		Kept                  bool
	}
	if err := json.Unmarshal(data, &list); err != nil {
		return nil, fmt.Errorf("decode -suites %s: %w", path, err)
	}
	if len(list) == 0 {
		return nil, fmt.Errorf("-suites %s names no suite", path)
	}
	out := make([]*testrunv1.RedundancyGoSuite, 0, len(list))
	for _, s := range list {
		out = append(out, &testrunv1.RedundancyGoSuite{Package: s.Package, Tags: s.Tags, TestFiles: s.Tests, Programs: s.Programs, Kept: s.Kept, Module: s.Module})
	}
	return out, nil
}

// commaItems splits a comma-separated flag, dropping empty entries.
func commaItems(list string) []string {
	var out []string
	for _, item := range strings.Split(list, ",") {
		if item = strings.TrimSpace(item); item != "" {
			out = append(out, item)
		}
	}
	return out
}
