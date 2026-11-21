package merge

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"strings"

	testrunv1 "agentrepl/proto/testrun/v1"

	"agentrepl/testrun/roster"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/shellword"
	"agent-repld/internal/testcases"
)

// This file reads a failing gate run's archive for every failing TEST, not
// just every failing suite, and writes them where the fixing agent works
// (owner ruling, 2026-10-12): the remediation runs exactly these tests while
// it fixes, and leaves the selected suites to the gate's next run. The
// contract is modules/app/agent-repl/AGENTS.md, "The merge queue's test gate
// and remediation contract".
//
// THE ARCHIVE IS THE RUNNER'S OWN OUTPUT (testrun): each unit's output is
// printed whole between "unit ID [SUITE] output:" and the unit's verdict line,
// so every failure line is read inside the unit that printed it. Each kind of
// suite prints its failures its own way, and each is read by its own pattern:
//
//	Go (GoModule, E2E)             "--- FAIL: TestName (1.23s)"      go test -v
//	ERT                            "FAILED  3/40  name (0.1 sec) at test-x.el:12"
//	vitest                         " FAIL  test/x.test.ts > describe > name"
//	script harnesses               "FAIL - what failed" / "  FAIL: what failed"
//
// A FAILED UNIT WHOSE TESTS CANNOT BE NAMED (a build, vet, typecheck or knip unit, a
// crash, a timeout) IS SAID SO in the file, naming the unit, so the agent knows
// to read that unit's block in the archive.

// FailingTestsFile is the file, at the root of the merging worktree, that
// names the failing tests of the gate run a fixing attempt repairs. It is kept
// out of commits by the repository's info/exclude, never by a tracked
// .gitignore.
const FailingTestsFile = ".agent-repl-merge-failing-tests"

// goFailLine is `go test -v`'s line for a failing test or subtest.
var goFailLine = regexp.MustCompile(`^\s*--- FAIL: (\S+) \(`)

// ertFailLine is ERT's batch line for a test with an unexpected result,
// carrying the file it is defined in.
var ertFailLine = regexp.MustCompile(`^\s*(?:FAILED|ABORTED|QUIT|PASSED)\s+\d+/\d+\s+(\S+) \(.*sec\) at ([^:\s]+):\d+\s*$`)

// vitestFailLine is vitest's default reporter's line for one failing test,
// and vitestFileFailLine its line for a test file that failed as a whole.
var (
	vitestFailLine     = regexp.MustCompile(`^\s*FAIL\s+(\S+)\s+>\s+(.+?)\s*$`)
	vitestFileFailLine = regexp.MustCompile(`^\s*FAIL\s+(\S+)\s+\[\s*\S+\s*\]\s*$`)
)

// scriptFailLine is the module's shell harnesses' failure line.
var scriptFailLine = regexp.MustCompile(`^\s*FAIL(?::\s+|\s+-\s+)(.+?)\s*$`)

// ansiEscape is a terminal color escape, stripped before a line is read.
var ansiEscape = regexp.MustCompile(`\x1b\[[0-9;]*[A-Za-z]`)

// unitCouldNotStart is the runner's line for a unit that never started.
var unitCouldNotStart = regexp.MustCompile(`unit (\S+) \(([A-Za-z0-9_-]+)\) could not start`)

// failingTests is one failing run's failing tests, by suite, in the worktree
// the run tested.
type failingTests struct {
	worktree string
	suites   []*failingSuite
}

// failingSuite is one suite's failing tests: each group shares one targeted
// invocation; unnamed are the failed units whose tests could not be named.
type failingSuite struct {
	name    string
	groups  []*failingGroup
	unnamed []string
}

// TestRunCommand is how a fixing agent runs tests: through the daemon, which
// starts every test run (bin/background.sh refuses any other), by the
// installed front door.
const TestRunCommand = "~/.claude/skills/agent-repl-workspaces/agent-repld test run"

// failingGroup is the failing tests one selection runs.
type failingGroup struct {
	key string
	// selections answers the selections that run names, the group's raw test
	// names, built when the file is rendered so every chunk's failures of the
	// group are in it. ONE SHAPE FEEDS THE BRIEF AND THE RECORD: the brief's
	// commands spell these selections (testcases.Flags), and the workspace's
	// record of its cases takes them (failingTests.cases).
	selections func(names []string) []*testrunv1.TestSelection
	// names are the raw names selections takes; shown is how each is listed.
	names []string
	shown []string
}

// suite answers the named suite's entry, adding it.
func (f *failingTests) suite(name string) *failingSuite {
	for _, s := range f.suites {
		if s.name == name {
			return s
		}
	}
	s := &failingSuite{name: name}
	f.suites = append(f.suites, s)
	return s
}

// add records one failing test under the group key: name is what the
// group's selections take, shown how the file lists it.
func (s *failingSuite) add(key string, selections func([]string) []*testrunv1.TestSelection, name, shown string) {
	for _, g := range s.groups {
		if g.key == key {
			if !slices.Contains(g.shown, shown) {
				g.names, g.shown = append(g.names, name), append(g.shown, shown)
			}
			return
		}
	}
	s.groups = append(s.groups, &failingGroup{key: key, selections: selections, names: []string{name}, shown: []string{shown}})
}

// flags spells the group's selections as `agent-repld test run` flags, their
// paths under WORKTREE.
func (g *failingGroup) flags(worktree string) []string {
	var out []string
	for _, sel := range g.selections(g.names) {
		out = append(out, testcases.Flags(worktree, sel)...)
	}
	return out
}

// command is the group's `agent-repld test run` command line.
func (g *failingGroup) command(worktree string) string {
	return TestRunCommand + " " + strings.Join(g.flags(worktree), " ")
}

// cases answers every case the failing run names, each its own selection
// (testcases.Of): each group's tests, and for each failed unit whose tests
// could not be named, the narrowest selection that runs it (unnamedCase).
func (f failingTests) cases() []*testrunv1.TestSelection {
	var sels []*testrunv1.TestSelection
	for _, s := range f.suites {
		for _, g := range s.groups {
			sels = append(sels, g.selections(g.names)...)
		}
		for _, unit := range s.unnamed {
			sels = append(sels, unnamedCase(f.worktree, s.name, unit))
		}
	}
	return testcases.Of(sels)
}

// unnamedCase is the narrowest selection that runs UNIT of suite SUITE, a
// failed unit whose tests could not be named: the Go package the unit is of
// (a package's build, vet or test chunk), where it names one that is in
// WORKTREE, otherwise the whole suite.
func unnamedCase(worktree, suite, unit string) *testrunv1.TestSelection {
	s, known := roster.Lookup(suite)
	if known && (s.Kind == roster.GoModule || s.Kind == roster.E2E) {
		if rest, ok := strings.CutPrefix(unit, suite+":"); ok {
			pkg := rest
			if i := strings.IndexAny(pkg, "#:["); i >= 0 {
				pkg = pkg[:i]
			}
			dir := filepath.ToSlash(filepath.Join(moduleRoot, s.Path, pkg))
			if info, err := os.Stat(filepath.Join(worktree, dir)); pkg != "" && err == nil && info.IsDir() {
				return testcases.GoPackage(dir, nil)
			}
		}
	}
	return testcases.Suite(suite)
}

// repoPath is PATH, a path under the agent-repl module, relative to the
// repository: a roster path starting with "/" is the repository's own.
func repoPath(path string) string {
	if rest, ok := strings.CutPrefix(path, "/"); ok {
		return rest
	}
	return filepath.ToSlash(filepath.Join(moduleRoot, path))
}

// readFailingTests reads every failing test out of a run's archived output.
// worktree is the worktree the run tested, which every invocation is spelled
// against.
func readFailingTests(output, worktree string) failingTests {
	found := failingTests{worktree: worktree}
	type block struct {
		unit, suite string
		lines       []string
	}
	var open *block
	for _, raw := range strings.Split(output, "\n") {
		line := ansiEscape.ReplaceAllString(raw, "")
		if open != nil {
			if m := unitVerdict.FindStringSubmatch(line); m != nil && m[1] == open.unit {
				if m[3] == "FAILED" {
					readUnit(&found, open.unit, open.suite, open.lines)
				}
				open = nil
				continue
			}
			open.lines = append(open.lines, line)
			continue
		}
		if m := unitBegin.FindStringSubmatch(line); m != nil {
			open = &block{unit: m[1], suite: m[2]}
			continue
		}
		if m := unitVerdict.FindStringSubmatch(line); m != nil && m[3] == "FAILED" {
			// A verdict with no output block: the unit printed nothing.
			readUnit(&found, m[1], m[2], nil)
			continue
		}
		if m := unitCouldNotStart.FindStringSubmatch(line); m != nil {
			s := found.suite(m[2])
			s.unnamed = append(s.unnamed, m[1])
		}
	}
	// A SUITE THE RUN FAILED WITH NO FAILED UNIT OF ITS OWN failed before any
	// unit ran (its units could not be planned): nothing in it can be named.
	for name, state := range parseSuiteStates(output) {
		if state != suiteStateFailed {
			continue
		}
		if s := found.suite(name); len(s.groups) == 0 && len(s.unnamed) == 0 {
			s.unnamed = append(s.unnamed, name)
		}
	}
	slices.SortStableFunc(found.suites, func(a, b *failingSuite) int {
		return slices.Index(AllSuites, a.name) - slices.Index(AllSuites, b.name)
	})
	return found
}

// readUnit reads one failed unit's block for the tests that failed in it.
func readUnit(found *failingTests, unit, suiteName string, lines []string) {
	s := found.suite(suiteName)
	suite, known := roster.Lookup(suiteName)
	named := false
	switch {
	case !known:
	case suite.Kind == roster.GoModule || suite.Kind == roster.E2E:
		rel, _, ok := goUnitPackage(unit, suiteName)
		if ok {
			dir := repoPath(filepath.Join(suite.Path, rel))
			named = readGoUnit(s, lines, unit[:strings.LastIndex(unit, "#")], func(names []string) []*testrunv1.TestSelection {
				return []*testrunv1.TestSelection{testcases.GoPackage(dir, deepest(names))}
			})
		}
	case suite.Kind == roster.ERT:
		named = readERTUnit(s, suite, lines)
	case suite.Kind == roster.Vitest:
		named = readVitestUnit(s, unit, suite, lines)
	case suite.Kind == roster.Script || suite.Kind == roster.SplitScript:
		named = readScriptUnit(s, suite, lines)
	}
	if !named {
		s.unnamed = append(s.unnamed, unit)
	}
}

// goUnitPackage reads a Go test chunk's unit id, "<suite>:<pkg>#NN" or
// "<suite>:<pkg>[<tags>]#NN", for the package and build tags it ran. A build
// or vet unit is no test chunk.
func goUnitPackage(unit, suite string) (rel, tags string, ok bool) {
	rest, prefixed := strings.CutPrefix(unit, suite+":")
	hash := strings.LastIndex(rest, "#")
	if !prefixed || hash < 0 {
		return "", "", false
	}
	rest = rest[:hash]
	if open := strings.Index(rest, "["); open >= 0 && strings.HasSuffix(rest, "]") {
		return rest[:open], rest[open+1 : len(rest)-1], true
	}
	return rest, "", true
}

// readGoUnit reads `go test -v` failure lines: every failing test and
// subtest, by its full path.
func readGoUnit(s *failingSuite, lines []string, key string, selections func([]string) []*testrunv1.TestSelection) bool {
	named := false
	for _, line := range lines {
		if m := goFailLine.FindStringSubmatch(line); m != nil {
			s.add(key, selections, m[1], m[1])
			named = true
		}
	}
	return named
}

// deepest keeps the failing tests nothing else failing is nested under: go
// fails a test whose subtest failed, and naming the subtest path runs just
// the failure.
func deepest(names []string) []string {
	var out []string
	for _, n := range names {
		nested := slices.ContainsFunc(names, func(o string) bool { return strings.HasPrefix(o, n+"/") })
		if !nested && !slices.Contains(out, n) {
			out = append(out, n)
		}
	}
	return out
}

// readERTUnit reads ERT's unexpected-result lines, grouping the tests by the
// file that defines them.
func readERTUnit(s *failingSuite, suite roster.Suite, lines []string) bool {
	byFile := map[string][]string{}
	var files []string
	for _, line := range lines {
		m := ertFailLine.FindStringSubmatch(line)
		if m == nil {
			continue
		}
		file := filepath.Base(m[2])
		if _, seen := byFile[file]; !seen {
			files = append(files, file)
		}
		if !slices.Contains(byFile[file], m[1]) {
			byFile[file] = append(byFile[file], m[1])
		}
	}
	for _, file := range files {
		path := repoPath(filepath.Join(suite.Path, file))
		selections := func(names []string) []*testrunv1.TestSelection {
			return []*testrunv1.TestSelection{testcases.ErtFile(path, names)}
		}
		for _, name := range byFile[file] {
			s.add("ert:"+file, selections, name, file+": "+name)
		}
	}
	return len(files) > 0
}

// readVitestUnit reads vitest's failure lines, grouping the tests by file.
// The unit id says whether the chunk ran under the suite's integration
// config.
func readVitestUnit(s *failingSuite, unit string, suite roster.Suite, lines []string) bool {
	if strings.Contains(unit, ":") {
		// A suite-wide check (typecheck, knip) or the coverage unit: no test
		// to name.
		return false
	}
	dir := repoPath(suite.Path)
	byFile := map[string][]string{}
	whole := map[string]bool{}
	var files []string
	note := func(file string) {
		if _, seen := byFile[file]; !seen && !whole[file] {
			files = append(files, file)
		}
	}
	for _, line := range lines {
		if m := vitestFileFailLine.FindStringSubmatch(line); m != nil {
			note(m[1])
			whole[m[1]] = true
			continue
		}
		m := vitestFailLine.FindStringSubmatch(line)
		if m == nil {
			continue
		}
		note(m[1])
		name := strings.Join(strings.Split(m[2], " > "), " ")
		if !slices.Contains(byFile[m[1]], name) {
			byFile[m[1]] = append(byFile[m[1]], name)
		}
	}
	for _, file := range files {
		path := filepath.ToSlash(filepath.Join(dir, file))
		// A vitest selection carries ONE full name; "" is the whole file,
		// which runs every name of it anyway.
		selections := func(names []string) []*testrunv1.TestSelection {
			if slices.Contains(names, "") {
				return []*testrunv1.TestSelection{testcases.VitestFile(path, "")}
			}
			out := make([]*testrunv1.TestSelection, 0, len(names))
			for _, n := range names {
				out = append(out, testcases.VitestFile(path, n))
			}
			return out
		}
		if whole[file] || len(byFile[file]) == 0 {
			s.add("vitest:"+file, selections, "", file+": the whole file failed")
			continue
		}
		for _, name := range byFile[file] {
			s.add("vitest:"+file, selections, name, file+": "+name)
		}
	}
	return len(files) > 0
}

// readScriptUnit reads a shell harness's failure lines. A harness runs whole:
// its cases have no names of their own to select by.
func readScriptUnit(s *failingSuite, suite roster.Suite, lines []string) bool {
	script := repoPath(suite.Path)
	selections := func([]string) []*testrunv1.TestSelection {
		return []*testrunv1.TestSelection{testcases.Harness(script, nil)}
	}
	named := false
	for _, line := range lines {
		if m := scriptFailLine.FindStringSubmatch(line); m != nil {
			s.add("script:"+suite.Name, selections, m[1], m[1])
			named = true
		}
	}
	return named
}

// render writes the failing tests as the file the fixing agent reads.
func (f failingTests) render(archive string) string {
	var b strings.Builder
	// PLAIN TEXT, NO MARKDOWN HEADING (owner ruling, 2026-11-19): the brief
	// carries this inline, and the feed drew a `#` line as a heading.
	b.WriteString("The failing tests of the merge queue's last test gate run.\n")
	b.WriteString(fmt.Sprintf("The whole run is archived at %s.\n", archive))
	b.WriteString("Run exactly these while fixing, through the daemon, with the command given\n")
	b.WriteString("for each group or the one that runs them all.\n")
	b.WriteString("After your turn the queue runs the selected suites again.\n")
	if len(f.suites) == 0 {
		b.WriteString("\nNo failing test could be attributed to a suite: read the archive.\n")
		return b.String()
	}
	var all []string
	for _, s := range f.suites {
		for _, g := range s.groups {
			for _, flag := range g.flags(f.worktree) {
				if !slices.Contains(all, flag) {
					all = append(all, flag)
				}
			}
		}
	}
	if len(all) > 0 {
		b.WriteString(fmt.Sprintf("\nrun all: %s %s\n", TestRunCommand, strings.Join(all, " ")))
	}
	for _, s := range f.suites {
		b.WriteString(fmt.Sprintf("\nsuite %s:\n", s.name))
		for _, g := range s.groups {
			b.WriteString(fmt.Sprintf("\nrun: %s\n", g.command(f.worktree)))
			for _, test := range g.shown {
				b.WriteString(fmt.Sprintf("- %s\n", test))
			}
		}
		for _, unit := range s.unnamed {
			b.WriteString(fmt.Sprintf("\nunnamed: the tests of %s could not be named (a build, vet, typecheck or knip failure, a crash or a timeout); read its output in the archive: grep -n %s %s\n",
				unit, shellword.Quote(regexp.QuoteMeta(unit)), archive))
		}
	}
	return b.String()
}

// writeFailingTests reads the failing gate run's archive and writes its
// failing tests to FailingTestsFile at the root of the merging worktree,
// excluded from commits through the repository's info/exclude. It answers the
// file's body, which the fixing brief also carries inline: the agent is handed
// the failing tests in the prompt itself, and the file keeps them at hand
// through the turn.
func (r *run) writeFailingTests(ctx context.Context, archive string) (string, error) {
	const op = "daemon.merge.fixes"
	output, err := os.ReadFile(archive)
	if err != nil {
		return "", fmt.Errorf("merge: reading the failing run's archive %s: %w", archive, err)
	}
	if err := r.excludeFromCommits(ctx, FailingTestsFile); err != nil {
		return "", err
	}
	found := readFailingTests(string(output), r.subject.dir)
	// THE CASES ARE RECORDED FIRST, so an attempt that cannot record them
	// fails before it hands the agent anything.
	if err := r.o.recordFailingCases(ctx, r.ws, found, dlog.Context{"archive": archive}); err != nil {
		return "", err
	}
	path := filepath.Join(r.subject.dir, FailingTestsFile)
	body := found.render(archive)
	if err := os.WriteFile(path, []byte(body), 0o644); err != nil {
		return "", fmt.Errorf("merge: writing the failing tests to %s: %w", path, err)
	}
	named, unnamed := 0, 0
	for _, s := range found.suites {
		for _, g := range s.groups {
			named += len(g.names)
		}
		unnamed += len(s.unnamed)
	}
	r.o.log(ctx, r.ws).Info(op, "wrote the failing tests the fixing attempt runs", dlog.Context{
		"workspace": string(r.ws), "path": path, "archive": archive, "tests": named, "unnamed_units": unnamed})
	return body, nil
}

// excludeFromCommits adds name to the repository's info/exclude, once. The
// exclude file lives in the COMMON git directory: git reads info/exclude only
// from there, for every worktree of the repository.
func (r *run) excludeFromCommits(ctx context.Context, name string) error {
	common, err := r.git.CommonDir(ctx, r.subject.dir)
	if err != nil {
		return fmt.Errorf("merge: resolving the git directory of %s: %w", r.subject.dir, err)
	}
	path := filepath.Join(common, "info", "exclude")
	body, err := os.ReadFile(path)
	if err != nil && !os.IsNotExist(err) {
		return fmt.Errorf("merge: reading %s: %w", path, err)
	}
	entry := "/" + name
	for _, line := range strings.Split(string(body), "\n") {
		if strings.TrimSpace(line) == entry {
			return nil
		}
	}
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		return fmt.Errorf("merge: creating %s: %w", filepath.Dir(path), err)
	}
	file, err := os.OpenFile(path, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0o644)
	if err != nil {
		return fmt.Errorf("merge: opening %s: %w", path, err)
	}
	prefix := ""
	if len(body) > 0 && !strings.HasSuffix(string(body), "\n") {
		prefix = "\n"
	}
	if _, err := file.WriteString(prefix + entry + "\n"); err != nil {
		file.Close()
		return fmt.Errorf("merge: writing %s: %w", path, err)
	}
	if err := file.Close(); err != nil {
		return fmt.Errorf("merge: closing %s: %w", path, err)
	}
	r.o.log(ctx, r.ws).Info("daemon.merge.fixes", "excluded a merge file from commits in the repository's info/exclude", dlog.Context{
		"workspace": string(r.ws), "exclude": path, "entry": entry})
	return nil
}

// recordFailingCases adds every case a failing gate run names (found.cases)
// to workspace WS's record of its cases, which `agent-repld test run -rerun`
// runs: the branch's agent may never have run them itself. A failure to
// record is the gate's, failing the fixing attempt loudly.
func (o *orchestrator) recordFailingCases(ctx context.Context, ws ids.WorkspaceID, found failingTests, fields dlog.Context) error {
	const op = "daemon.merge.fixes"
	cases := found.cases()
	fields = dlog.Merge(fields, dlog.Context{"workspace": string(ws), "cases": len(cases)})
	if len(cases) == 0 {
		o.log(ctx, ws).Info(op, "the failing gate run named no case to record", fields)
		return nil
	}
	if err := o.deps.DB.RecordTestCases(ctx, ws, cases, o.nowMS()); err != nil {
		o.log(ctx, ws).Error(op, "the failing gate run's cases could not be recorded for the workspace's reruns", dlog.WithCause(fields, err))
		return fmt.Errorf("merge: recording the failing tests' cases: %w", err)
	}
	o.log(ctx, ws).Info(op, "recorded the failing gate run's cases for the workspace's reruns", fields)
	return nil
}
