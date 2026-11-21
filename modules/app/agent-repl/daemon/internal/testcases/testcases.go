// Package testcases is the shape of one test case as the daemon records it
// for a workspace (`agent-repld test run -rerun`): a testrun.v1.TestSelection
// naming exactly ONE case, the shape testrun's rerun takes
// (TestRunRequest.drop_missing). It splits a run's selections into their
// cases, builds a selection from the names a command line or a failing run
// gives, and spells a selection back as `agent-repld test run` flags, so the
// record, the brief's commands and the command line all read one shape.
package testcases

import (
	"fmt"
	"path/filepath"
	"strings"

	testrunv1 "agentrepl/proto/testrun/v1"

	"agent-repld/internal/shellword"
)

// Of answers every case SELS name, each its own selection, in order: one
// test, one vitest name, one ERT test, one harness group, or one whole
// target. A Go subtest path is recorded as its top-level test, the case a
// rerun can tell the tree still holds (no listing holds subtests). The whole
// roster (`every_suite`) is the merge queue's gate and no case of a
// workspace's, so it yields none.
func Of(sels []*testrunv1.TestSelection) []*testrunv1.TestSelection {
	var out []*testrunv1.TestSelection
	for _, sel := range sels {
		switch t := sel.GetTarget().(type) {
		case *testrunv1.TestSelection_GoPackage:
			dir := t.GoPackage.GetPackageDir()
			if t.GoPackage.GetAll() != nil {
				out = append(out, GoPackage(dir, nil))
				continue
			}
			for _, test := range t.GoPackage.GetNamed().GetTests() {
				out = append(out, GoPackage(dir, []string{test.GetName()}))
			}
		case *testrunv1.TestSelection_VitestFile:
			file := t.VitestFile.GetFile()
			if t.VitestFile.GetAll() != nil {
				out = append(out, VitestFile(file, ""))
				continue
			}
			for _, name := range t.VitestFile.GetNamed().GetFullNames() {
				out = append(out, VitestFile(file, name))
			}
		case *testrunv1.TestSelection_ErtFile:
			file := t.ErtFile.GetFile()
			if t.ErtFile.GetAll() != nil {
				out = append(out, ErtFile(file, nil))
				continue
			}
			for _, name := range t.ErtFile.GetNamed().GetTestNames() {
				out = append(out, ErtFile(file, []string{name}))
			}
		case *testrunv1.TestSelection_Harness:
			script := t.Harness.GetScript()
			if t.Harness.GetAll() != nil {
				out = append(out, Harness(script, nil))
				continue
			}
			for _, group := range t.Harness.GetNamed().GetNames() {
				out = append(out, Harness(script, []string{group}))
			}
		case *testrunv1.TestSelection_Suite:
			out = append(out, Suite(t.Suite.GetName()))
		}
	}
	return Add(nil, out)
}

// Add answers RECORD with each of CASES it does not already hold appended,
// in order: the first of every case named twice is kept.
func Add(record, cases []*testrunv1.TestSelection) []*testrunv1.TestSelection {
	seen := map[string]bool{}
	for _, sel := range record {
		seen[Key(sel)] = true
	}
	out := record
	for _, sel := range cases {
		key := Key(sel)
		if !seen[key] {
			seen[key] = true
			out = append(out, sel)
		}
	}
	return out
}

// Key is a case's identity: two selections naming the same case share it.
// Its parts are joined by the ASCII unit separator, which no path or test
// name holds, and never by NUL, which SQLite's text functions stop at.
func Key(sel *testrunv1.TestSelection) string {
	return strings.Join(append([]string{kindOf(sel), whereOf(sel)}, namesOf(sel)...), "\x1f")
}

// GoPackage is the Go package in DIR (repository-relative), every test of it
// when NAMES is empty, otherwise those: each TestName whole, or
// TestName/segment/... as one subtest path.
func GoPackage(dir string, names []string) *testrunv1.TestSelection {
	pkg := &testrunv1.TestSelectionGoPackage{PackageDir: dir}
	if len(names) == 0 {
		pkg.Tests = &testrunv1.TestSelectionGoPackage_All{All: &testrunv1.TestSelectionGoAllTests{}}
	} else {
		pkg.Tests = &testrunv1.TestSelectionGoPackage_Named{Named: &testrunv1.TestSelectionGoNamedTests{Tests: goTests(names)}}
	}
	return &testrunv1.TestSelection{Target: &testrunv1.TestSelection_GoPackage{GoPackage: pkg}}
}

// goTests reads Go test names: each TestName whole, or TestName/segment/...
// as one subtest path, a test named both ways running whole.
func goTests(names []string) []*testrunv1.TestSelectionGoTest {
	var tests []*testrunv1.TestSelectionGoTest
	byName := map[string]*testrunv1.TestSelectionGoTest{}
	for _, n := range names {
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
	return tests
}

// VitestFile is the vitest FILE (repository-relative): the whole file when
// NAME is "", otherwise its test (or describe block) of that full name.
func VitestFile(file, name string) *testrunv1.TestSelection {
	f := &testrunv1.TestSelectionVitestFile{File: file}
	if name == "" {
		f.Tests = &testrunv1.TestSelectionVitestFile_All{All: &testrunv1.TestSelectionVitestAllTests{}}
	} else {
		f.Tests = &testrunv1.TestSelectionVitestFile_Named{Named: &testrunv1.TestSelectionVitestNamedTests{FullNames: []string{name}}}
	}
	return &testrunv1.TestSelection{Target: &testrunv1.TestSelection_VitestFile{VitestFile: f}}
}

// ErtFile is the ERT FILE: every test of it when NAMES is empty, otherwise
// those ert-deftests.
func ErtFile(file string, names []string) *testrunv1.TestSelection {
	f := &testrunv1.TestSelectionErtFile{File: file, Tests: &testrunv1.TestSelectionErtFile_All{All: &testrunv1.TestSelectionErtAllTests{}}}
	if len(names) > 0 {
		f.Tests = &testrunv1.TestSelectionErtFile_Named{Named: &testrunv1.TestSelectionErtNamedTests{TestNames: names}}
	}
	return &testrunv1.TestSelection{Target: &testrunv1.TestSelection_ErtFile{ErtFile: f}}
}

// Harness is the shell harness SCRIPT (repository-relative): whole when
// GROUPS is empty, otherwise those groups.
func Harness(script string, groups []string) *testrunv1.TestSelection {
	h := &testrunv1.TestSelectionHarness{Script: script, Groups: &testrunv1.TestSelectionHarness_All{All: &testrunv1.TestSelectionHarnessAllGroups{}}}
	if len(groups) > 0 {
		h.Groups = &testrunv1.TestSelectionHarness_Named{Named: &testrunv1.TestSelectionHarnessNamedGroups{Names: groups}}
	}
	return &testrunv1.TestSelection{Target: &testrunv1.TestSelection_Harness{Harness: h}}
}

// Suite is the roster suite NAME, whole.
func Suite(name string) *testrunv1.TestSelection {
	return &testrunv1.TestSelection{Target: &testrunv1.TestSelection_Suite{Suite: &testrunv1.TestSelectionSuite{Name: name}}}
}

// Flags spells SEL as the `agent-repld test run` flags that select it, its
// path made absolute under WORKTREE and quoted for the shell. A vitest flag
// carries one full name, so a selection naming several spells one flag each.
func Flags(worktree string, sel *testrunv1.TestSelection) []string {
	kind, names := kindOf(sel), namesOf(sel)
	if kind == "suite" {
		return []string{"-suite " + shellword.Quote(whereOf(sel))}
	}
	path := whereOf(sel)
	if kind != "ert" || strings.ContainsRune(path, '/') {
		path = filepath.Join(worktree, filepath.FromSlash(path))
	}
	flag := func(names []string) string {
		value := path
		if len(names) > 0 {
			value += "=" + strings.Join(names, ",")
		}
		return "-" + kind + " " + shellword.Quote(value)
	}
	if kind != "vitest" || len(names) <= 1 {
		return []string{flag(names)}
	}
	out := make([]string, len(names))
	for i, n := range names {
		out[i] = flag([]string{n})
	}
	return out
}

// kindOf is SEL's flag kind.
func kindOf(sel *testrunv1.TestSelection) string {
	switch sel.GetTarget().(type) {
	case *testrunv1.TestSelection_GoPackage:
		return "go"
	case *testrunv1.TestSelection_VitestFile:
		return "vitest"
	case *testrunv1.TestSelection_ErtFile:
		return "ert"
	case *testrunv1.TestSelection_Harness:
		return "harness"
	case *testrunv1.TestSelection_Suite:
		return "suite"
	}
	panic(fmt.Sprintf("testcases: a case has target %T, which no case has", sel.GetTarget()))
}

// whereOf is SEL's target: its package, file, script or suite.
func whereOf(sel *testrunv1.TestSelection) string {
	switch t := sel.GetTarget().(type) {
	case *testrunv1.TestSelection_GoPackage:
		return t.GoPackage.GetPackageDir()
	case *testrunv1.TestSelection_VitestFile:
		return t.VitestFile.GetFile()
	case *testrunv1.TestSelection_ErtFile:
		return t.ErtFile.GetFile()
	case *testrunv1.TestSelection_Harness:
		return t.Harness.GetScript()
	case *testrunv1.TestSelection_Suite:
		return t.Suite.GetName()
	}
	panic(fmt.Sprintf("testcases: a case has target %T, which no case has", sel.GetTarget()))
}

// namesOf is SEL's names within its target, none when it selects it whole:
// Go tests as TestName or TestName/sub/path.
func namesOf(sel *testrunv1.TestSelection) []string {
	switch t := sel.GetTarget().(type) {
	case *testrunv1.TestSelection_GoPackage:
		var names []string
		for _, test := range t.GoPackage.GetNamed().GetTests() {
			if paths := test.GetSubtests().GetPaths(); len(paths) > 0 {
				for _, p := range paths {
					names = append(names, test.GetName()+"/"+strings.Join(p.GetSegments(), "/"))
				}
				continue
			}
			names = append(names, test.GetName())
		}
		return names
	case *testrunv1.TestSelection_VitestFile:
		return t.VitestFile.GetNamed().GetFullNames()
	case *testrunv1.TestSelection_ErtFile:
		return t.ErtFile.GetNamed().GetTestNames()
	case *testrunv1.TestSelection_Harness:
		return t.Harness.GetNamed().GetNames()
	}
	return nil
}
