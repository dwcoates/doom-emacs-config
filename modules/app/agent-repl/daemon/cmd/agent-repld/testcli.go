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
	workspacev1 "agentrepl/proto/workspace/v1"
)

// testcli.go — `agent-repld test run`: run exactly the named tests through
// the daemon, the one runner of tests on the host.
//
//	agent-repld test run [-workspace WS] SELECTION...
//
// Each selection is one flag, repeated as needed:
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
//
// A path is relative to the working directory (or absolute), and must lie in
// the workspace's worktree. Each frame of the run's stream is printed as one
// line of protojson as it arrives. The command exits 0 when the run passed,
// exitTestsFailed when it ran and failed, and exitRefused when the daemon or
// the runner refused it.

// exitTestsFailed is a run that ran and failed, as `go test` reports one.
const exitTestsFailed = 1

// testNoun is the test commands' noun.
const testNoun = "test"

// selectionFlag collects one kind of selection's repeated flag values.
type selectionFlag struct{ values *[]string }

func (f selectionFlag) String() string { return "" }
func (f selectionFlag) Set(value string) error {
	if strings.TrimSpace(value) == "" {
		return errors.New("an empty selection")
	}
	*f.values = append(*f.values, value)
	return nil
}

func testRun(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	workspace := fs.String("workspace", here, "the workspace whose worktree runs the tests")
	var goSel, vitestSel, ertSel, harnessSel []string
	fs.Var(selectionFlag{&goSel}, "go", "a Go package's tests: DIR, or DIR=TestA,TestB/sub/path")
	fs.Var(selectionFlag{&vitestSel}, "vitest", "a vitest file's tests: FILE, or FILE=full name")
	fs.Var(selectionFlag{&ertSel}, "ert", "an ERT file's tests: FILE, or FILE=test-a,test-b")
	fs.Var(selectionFlag{&harnessSel}, "harness", "a shell harness: SCRIPT, or SCRIPT=group1,group2")
	positionals, ok := c.parse(fs, args)
	if !ok {
		return exitFailure
	}
	if len(positionals) > 0 {
		return c.usageFailed("unexpected arguments %v; every selection is a flag", positionals)
	}
	if len(goSel)+len(vitestSel)+len(ertSel)+len(harnessSel) == 0 {
		return c.usageFailed("name the tests to run: the full suite is the merge queue's to run")
	}
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
	run, err := buildSelections(canonicalDir(ref.GetDir()), canonicalDir(cwd), goSel, vitestSel, ertSel, harnessSel)
	if err != nil {
		return c.usageFailed("%v", err)
	}
	return streamTestRun(c, ref, run)
}

// streamTestRun calls RunTests and prints every frame as it arrives.
func streamTestRun(c *cliCall, ref *workspacev1.WorkspaceRef, run *testrunv1.TestRunRequest) int {
	stream, err := openDaemonStream(c.ctx, c.client.RunTests, &agentreplv1.RunTestsRequest{Workspace: ref, Run: run})
	if errors.Is(err, errDryRun) {
		return exitSuccess
	}
	if err != nil {
		c.failed("the daemon failed the call: %v", err)
		return exitFailure
	}
	defer stream.Close() //nolint:errcheck // the stream has ended either way
	code := exitFailure
	for stream.Receive() {
		frame := stream.Msg()
		text, err := protojson.Marshal(frame)
		if err != nil {
			c.failed("render a frame: %v", err)
			return exitFailure
		}
		fmt.Fprintln(c.out, string(text))
		switch {
		case frame.GetError() != nil, frame.GetEvent().GetRefused() != nil:
			code = exitRefused
		case frame.GetEvent().GetEnded().GetPassed() != nil:
			code = exitSuccess
		case frame.GetEvent().GetEnded().GetFailed() != nil:
			code = exitTestsFailed
		}
	}
	if err := stream.Err(); err != nil && !errors.Is(err, io.EOF) {
		c.failed("the run's stream broke: %v", err)
		return exitFailure
	}
	if code == exitFailure {
		c.failed("the run's stream ended without its end")
	}
	return code
}

// buildSelections turns the flags into a TestRunRequest, every path made
// relative to the worktree (the repository root the runner resolves against).
func buildSelections(worktree, cwd string, goSel, vitestSel, ertSel, harnessSel []string) (*testrunv1.TestRunRequest, error) {
	run := &testrunv1.TestRunRequest{}
	repoPath := func(p string) (string, error) {
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
