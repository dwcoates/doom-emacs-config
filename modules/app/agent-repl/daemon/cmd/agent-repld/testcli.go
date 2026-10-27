package main

import (
	"errors"
	"fmt"
	"io"
	"path/filepath"
	"strings"

	"google.golang.org/protobuf/encoding/protojson"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	testrunv1 "agentrepl/proto/testrun/v1"
)

// testcli.go — `agent-repld test`: every test run on the host goes through
// the daemon, the one runner of tests (bin/background.sh refuses any other).
//
//	agent-repld test run [-workspace WS] SELECTION... [OPTIONS]
//	agent-repld test redundancy go|vitest|report [-workspace WS] FLAGS...
//
// `run`'s selections are flags, each repeated as needed:
//
//	-go DIR                        every test of the Go package in DIR
//	-go DIR=TestA,TestB/case/sub   those tests; TestB/case/sub is one subtest path
//	-vitest FILE                   every test of the vitest file
//	-vitest 'FILE=a suite a test'  one test (or describe block) by its full name;
//	                               repeat the flag for more names of the file
//	-ert FILE                      every test of the ERT file
//	-ert FILE=test-a,test-b        those ert-deftests
//	-harness SCRIPT                the whole shell harness
//	-harness SCRIPT=group1,group2  those groups of a harness that lists groups
//	-harness SCRIPT=TestA,TestB    those top-level Go tests of a sandboxed suite (bin/test-e2e-emacs.sh)
//	-suite NAME                    one roster suite, whole
//	-every-suite                   the whole roster: the merge queue's gate, run
//	                               by an agent only with a stated reason
//
// and its options:
//
//	-coverage                instrument and report (whole suites only)
//	-record                  append the timings (-every-suite alone)
//	-runs N                  repeat the run N times, each its own run
//	-stop-at-first-failure   stop repeating at the first failing run
//	-cpu-load N              hold N busy loops for the run (with -as-user only)
//	-as-user                 the user asks, not the calling workspace
//
// A path is relative to the working directory (or absolute), and must lie in
// the workspace's worktree. Each frame of the run's stream is printed as one
// line of protojson as it arrives. The command exits 0 when every run passed,
// exitTestsFailed when one ran and failed, and exitRefused when the daemon or
// the runner refused it.
//
// `redundancy` takes `testrun redundancy`'s own flags (modules/app/agent-repl
// AGENTS.md, "Which tests are redundant") and prints the measurement's output
// as it comes; it exits 0 when the measurement succeeded.

// exitTestsFailed is a run that ran and failed, as `go test` reports one.
const exitTestsFailed = 1

// testNoun is the test commands' noun.
const testNoun = "test"

// testSelections are `test run`'s selection flags.
type testSelections struct {
	goSel, vitest, ert, harness, suites []string
	every                               bool
}

func (t testSelections) empty() bool {
	return len(t.goSel)+len(t.vitest)+len(t.ert)+len(t.harness)+len(t.suites) == 0 && !t.every
}

func testRun(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	workspace := fs.String("workspace", here, "the workspace whose worktree runs the tests")
	var sel testSelections
	fs.Var(repeatedFlag{values: &sel.goSel, noun: "selection"}, "go", "a Go package's tests: DIR, or DIR=TestA,TestB/sub/path")
	fs.Var(repeatedFlag{values: &sel.vitest, noun: "selection"}, "vitest", "a vitest file's tests: FILE, or FILE=full name")
	fs.Var(repeatedFlag{values: &sel.ert, noun: "selection"}, "ert", "an ERT file's tests: FILE, or FILE=test-a,test-b")
	fs.Var(repeatedFlag{values: &sel.harness, noun: "selection"}, "harness", "a shell harness: SCRIPT, or SCRIPT=group1,group2")
	fs.Var(repeatedFlag{values: &sel.suites, noun: "selection"}, "suite", "one roster suite, whole")
	fs.BoolVar(&sel.every, "every-suite", false, "the whole roster (the merge queue's gate; only with a stated reason)")
	coverage := fs.Bool("coverage", false, "instrument the run and report coverage (whole suites only)")
	record := fs.Bool("record", false, "append the run's timings to the history (-every-suite alone)")
	runs := fs.Uint("runs", 1, "how many times the run repeats, each its own run")
	stop := fs.Bool("stop-at-first-failure", false, "stop repeating at the first run that fails")
	cpuLoad := fs.Uint("cpu-load", 0, "busy loops held for the run's length (the user's alone: needs -as-user)")
	asUser := fs.Bool("as-user", false, "the user asks, not the calling workspace")
	positionals, ok := c.parse(fs, args)
	if !ok {
		return exitFailure
	}
	if len(positionals) > 0 {
		return c.usageFailed("unexpected arguments %v; every selection is a flag", positionals)
	}
	if sel.empty() {
		return c.usageFailed("name the tests to run: the full suite is the merge queue's to run")
	}
	if *cpuLoad > 0 && !*asUser {
		return c.usageFailed("-cpu-load is the user's alone: pass -as-user only when the user asks for load")
	}
	if !c.dial(*stateDir) {
		return exitFailure
	}
	ref, ok := c.named(*workspace)
	if !ok {
		return exitFailure
	}
	requester, ok := c.requester(*asUser)
	if !ok {
		return exitFailure
	}
	cwd, err := c.deps.getwd()
	if err != nil {
		c.failed("read the working directory: %v", err)
		return exitFailure
	}
	run, err := buildSelections(canonicalDir(ref.GetDir()), canonicalDir(cwd), sel)
	if err != nil {
		return c.usageFailed("%v", err)
	}
	run.Coverage, run.Record = *coverage, *record
	req := &agentreplv1.RunTestsRequest{Workspace: ref, Run: run, Runs: uint32(*runs), StopAtFirstFailure: *stop, Requester: requester}
	if *cpuLoad > 0 {
		req.CpuLoad = &agentreplv1.RunTestsCpuLoad{Loops: uint32(*cpuLoad)}
	}
	return streamTestRun(c, req)
}

// streamTestRun calls RunTests and prints every frame as it arrives. Every
// run must pass for the command to: a failed run anywhere in a repeat fails
// it.
func streamTestRun(c *cliCall, req *agentreplv1.RunTestsRequest) int {
	stream, err := openDaemonStream(c.ctx, c.client.RunTests, req)
	if errors.Is(err, errDryRun) {
		return exitSuccess
	}
	if err != nil {
		c.failed("the daemon failed the call: %v", err)
		return exitFailure
	}
	defer stream.Close() //nolint:errcheck // the stream has ended either way
	code, failed := exitFailure, false
	for stream.Receive() {
		frame := stream.Msg()
		text, err := protojson.Marshal(frame)
		if err != nil {
			c.failed("render a frame: %v", err)
			return exitFailure
		}
		fmt.Fprintln(c.out, string(text))
		switch {
		case frame.GetError().GetRunnerFailed() != nil:
			code = exitFailure
		case frame.GetError() != nil, frame.GetEvent().GetRefused() != nil:
			code = exitRefused
		case frame.GetEvent().GetEnded().GetPassed() != nil:
			code = exitSuccess
		case frame.GetEvent().GetEnded().GetFailed() != nil:
			code, failed = exitTestsFailed, true
		}
	}
	if failed && code == exitSuccess {
		// A later run passed; an earlier one failed, and that is the answer.
		code = exitTestsFailed
	}
	if err := stream.Err(); err != nil && !errors.Is(err, io.EOF) {
		c.failed("the run's stream broke: %v", err)
		return exitFailure
	}
	if code == exitFailure {
		c.failed("the run did not report how it ended")
	}
	return code
}

// buildSelections turns the flags into a TestRunRequest, every path made
// relative to the worktree (the repository root the runner resolves against).
func buildSelections(worktree, cwd string, sel testSelections) (*testrunv1.TestRunRequest, error) {
	run := &testrunv1.TestRunRequest{}
	repoPath := worktreePath(worktree, cwd)
	goSel, vitestSel, ertSel, harnessSel := sel.goSel, sel.vitest, sel.ert, sel.harness
	if sel.every {
		run.Selections = append(run.Selections, &testrunv1.TestSelection{Target: &testrunv1.TestSelection_EverySuite{EverySuite: &testrunv1.TestSelectionEverySuite{}}})
	}
	for _, name := range sel.suites {
		run.Selections = append(run.Selections, &testrunv1.TestSelection{Target: &testrunv1.TestSelection_Suite{Suite: &testrunv1.TestSelectionSuite{Name: name}}})
	}
	for _, v := range goSel {
		dir, names, named := strings.Cut(v, "=")
		path, err := repoPath(dir)
		if err != nil {
			return nil, err
		}
		pkg := &testrunv1.TestSelectionGoPackage{PackageDir: path}
		if !named {
			pkg.Tests = &testrunv1.TestSelectionGoPackage_All{All: &testrunv1.TestSelectionGoAllTests{}}
		} else {
			tests, err := goTests(v, names)
			if err != nil {
				return nil, err
			}
			pkg.Tests = &testrunv1.TestSelectionGoPackage_Named{Named: &testrunv1.TestSelectionGoNamedTests{Tests: tests}}
		}
		run.Selections = append(run.Selections, &testrunv1.TestSelection{Target: &testrunv1.TestSelection_GoPackage{GoPackage: pkg}})
	}
	vitest := map[string]*testrunv1.TestSelectionVitestFile{}
	for _, v := range vitestSel {
		file, name, named := strings.Cut(v, "=")
		path, err := repoPath(file)
		if err != nil {
			return nil, err
		}
		sel, seen := vitest[path]
		if !seen {
			sel = &testrunv1.TestSelectionVitestFile{File: path}
			vitest[path] = sel
			run.Selections = append(run.Selections, &testrunv1.TestSelection{Target: &testrunv1.TestSelection_VitestFile{VitestFile: sel}})
		}
		switch {
		case !named || sel.GetAll() != nil:
			sel.Tests = &testrunv1.TestSelectionVitestFile_All{All: &testrunv1.TestSelectionVitestAllTests{}}
		case strings.TrimSpace(name) == "":
			return nil, fmt.Errorf("-vitest %s: an empty test name", v)
		default:
			if sel.GetNamed() == nil {
				sel.Tests = &testrunv1.TestSelectionVitestFile_Named{Named: &testrunv1.TestSelectionVitestNamedTests{}}
			}
			sel.GetNamed().FullNames = append(sel.GetNamed().FullNames, name)
		}
	}
	for _, v := range ertSel {
		file, names, named := strings.Cut(v, "=")
		path, err := repoPath(file)
		if err != nil {
			return nil, err
		}
		sel := &testrunv1.TestSelectionErtFile{File: path, Tests: &testrunv1.TestSelectionErtFile_All{All: &testrunv1.TestSelectionErtAllTests{}}}
		if named {
			list, err := commaList("-ert "+v, names)
			if err != nil {
				return nil, err
			}
			sel.Tests = &testrunv1.TestSelectionErtFile_Named{Named: &testrunv1.TestSelectionErtNamedTests{TestNames: list}}
		}
		run.Selections = append(run.Selections, &testrunv1.TestSelection{Target: &testrunv1.TestSelection_ErtFile{ErtFile: sel}})
	}
	for _, v := range harnessSel {
		script, groups, named := strings.Cut(v, "=")
		path, err := repoPath(script)
		if err != nil {
			return nil, err
		}
		sel := &testrunv1.TestSelectionHarness{Script: path, Groups: &testrunv1.TestSelectionHarness_All{All: &testrunv1.TestSelectionHarnessAllGroups{}}}
		if named {
			list, err := commaList("-harness "+v, groups)
			if err != nil {
				return nil, err
			}
			sel.Groups = &testrunv1.TestSelectionHarness_Named{Named: &testrunv1.TestSelectionHarnessNamedGroups{Names: list}}
		}
		run.Selections = append(run.Selections, &testrunv1.TestSelection{Target: &testrunv1.TestSelection_Harness{Harness: sel}})
	}
	return run, nil
}

// worktreePath answers a function turning a path (relative to cwd, or
// absolute) into one relative to the worktree, refusing a path outside it.
func worktreePath(worktree, cwd string) func(string) (string, error) {
	return func(p string) (string, error) {
		abs := p
		if !filepath.IsAbs(abs) {
			abs = filepath.Join(cwd, p)
		}
		rel, err := filepath.Rel(worktree, filepath.Clean(abs))
		if err != nil || rel == ".." || strings.HasPrefix(rel, ".."+string(filepath.Separator)) {
			return "", fmt.Errorf("%s lies outside the workspace's worktree %s", p, worktree)
		}
		return filepath.ToSlash(rel), nil
	}
}

// goTests reads a -go flag's test list: each TestName whole, or
// TestName/segment/... as one subtest path.
func goTests(flagValue, names string) ([]*testrunv1.TestSelectionGoTest, error) {
	list, err := commaList("-go "+flagValue, names)
	if err != nil {
		return nil, err
	}
	var tests []*testrunv1.TestSelectionGoTest
	byName := map[string]*testrunv1.TestSelectionGoTest{}
	for _, n := range list {
		top, path, sub := strings.Cut(n, "/")
		t, seen := byName[top]
		if !seen {
			t = &testrunv1.TestSelectionGoTest{Name: top}
			byName[top] = t
			tests = append(tests, t)
		}
		switch {
		case !sub || t.GetWhole() != nil:
			t.Scope = &testrunv1.TestSelectionGoTest_Whole{Whole: &testrunv1.TestSelectionGoWholeTest{}}
		default:
			if t.GetSubtests() == nil {
				t.Scope = &testrunv1.TestSelectionGoTest_Subtests{Subtests: &testrunv1.TestSelectionGoSubtests{}}
			}
			t.GetSubtests().Paths = append(t.GetSubtests().Paths, &testrunv1.TestSelectionGoSubtestPath{Segments: strings.Split(path, "/")})
		}
	}
	return tests, nil
}

// commaList splits a flag's comma list, refusing an empty entry.
func commaList(what, list string) ([]string, error) {
	var out []string
	for _, item := range strings.Split(list, ",") {
		if strings.TrimSpace(item) == "" {
			return nil, fmt.Errorf("%s: an empty name in the list", what)
		}
		out = append(out, item)
	}
	return out, nil
}
