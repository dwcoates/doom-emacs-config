package merge

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"strings"

	"agentrepl/testrun/roster"

	"agent-repld/internal/dlog"
	"agent-repld/internal/shellword"
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
//	Go (GoModule, E2E, e2e-emacs)  "--- FAIL: TestName (1.23s)"      go test -v
//	ERT                            "FAILED  3/40  name (0.1 sec) at test-x.el:12"
//	vitest                         " FAIL  test/x.test.ts > describe > name"
//	script harnesses               "FAIL - what failed" / "  FAIL: what failed"
//
// A FAILED UNIT WHOSE TESTS CANNOT BE NAMED (a build, vet or typecheck unit, a
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

// failingTests is one failing run's failing tests, by suite.
type failingTests struct {
	suites []*failingSuite
}

// failingSuite is one suite's failing tests: each group shares one targeted
// invocation; unnamed are the failed units whose tests could not be named.
type failingSuite struct {
	name    string
	groups  []*failingGroup
	unnamed []string
}

// failingGroup is the failing tests one invocation runs.
type failingGroup struct {
	key     string
	command string
	tests   []string
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

// add records one failing test under the group key, with its invocation.
func (s *failingSuite) add(key, command, test string) {
	for _, g := range s.groups {
		if g.key == key {
			if !slices.Contains(g.tests, test) {
				g.tests = append(g.tests, test)
			}
			return
		}
	}
	s.groups = append(s.groups, &failingGroup{key: key, command: command, tests: []string{test}})
}

// readFailingTests reads every failing test out of a run's archived output.
// module is the absolute path of the agent-repl module in the worktree the
// run tested, which every invocation is spelled against.
func readFailingTests(output, module string) failingTests {
	var found failingTests
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
					readUnit(&found, open.unit, open.suite, open.lines, module)
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
			readUnit(&found, m[1], m[2], nil, module)
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
func readUnit(found *failingTests, unit, suiteName string, lines []string, module string) {
	s := found.suite(suiteName)
	suite, known := roster.Lookup(suiteName)
	named := false
	switch {
	case !known:
	case suiteName == "e2e-emacs":
		named = readGoUnit(s, lines, "e2e-emacs", func(pattern string) string {
			return fmt.Sprintf("%s -run '%s'", filepath.Join(module, suite.Path), pattern)
		})
	case suite.Kind == roster.GoModule || suite.Kind == roster.E2E:
		rel, tags, ok := goUnitPackage(unit, suiteName)
		if ok {
			dir := filepath.Join(module, suite.Path)
			named = readGoUnit(s, lines, unit[:strings.LastIndex(unit, "#")], func(pattern string) string {
				tagArg := ""
				if tags != "" {
					tagArg = " -tags " + tags
				}
				return fmt.Sprintf("cd %s && %s go test%s -count=1 -run '%s' %s",
					dir, filepath.Join(module, "bin", "background.sh"), tagArg, pattern, goPackageArg(rel))
			})
		}
	case suite.Kind == roster.ERT:
		named = readERTUnit(s, lines, module)
	case suite.Kind == roster.Vitest:
		named = readVitestUnit(s, unit, suite, lines, module)
	case suite.Kind == roster.Script || suite.Kind == roster.SplitScript:
		named = readScriptUnit(s, suite, lines, module)
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

// goPackageArg spells a package directory relative to its module for go test.
func goPackageArg(rel string) string {
	if rel == "." {
		return "."
	}
	return "./" + rel
}

// readGoUnit reads `go test -v` failure lines. The invocation runs the failing
// top-level tests; a failing subtest is named in full and run through its
// parent.
func readGoUnit(s *failingSuite, lines []string, key string, command func(pattern string) string) bool {
	var names, tops []string
	for _, line := range lines {
		m := goFailLine.FindStringSubmatch(line)
		if m == nil {
			continue
		}
		names = append(names, m[1])
		top, _, _ := strings.Cut(m[1], "/")
		if !slices.Contains(tops, top) {
			tops = append(tops, top)
		}
	}
	if len(names) == 0 {
		return false
	}
	quoted := make([]string, len(tops))
	for i, top := range tops {
		quoted[i] = regexp.QuoteMeta(top)
	}
	for _, name := range names {
		s.add(key, command("^("+strings.Join(quoted, "|")+")$"), name)
	}
	return true
}

// readERTUnit reads ERT's unexpected-result lines, grouping the tests by the
// file that defines them.
func readERTUnit(s *failingSuite, lines []string, module string) bool {
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
		command := fmt.Sprintf("cd %s && bin/background.sh emacs -batch -Q -l ert -l lisp/%s --eval \"(ert-run-tests-batch-and-exit '(member %s))\"",
			module, file, strings.Join(byFile[file], " "))
		for _, name := range byFile[file] {
			s.add("ert:"+file, command, file+": "+name)
		}
	}
	return len(files) > 0
}

// readVitestUnit reads vitest's failure lines, grouping the tests by file.
// The unit id says whether the chunk ran under the suite's integration
// config.
func readVitestUnit(s *failingSuite, unit string, suite roster.Suite, lines []string, module string) bool {
	if strings.Contains(unit, ":") {
		// The typecheck (or coverage) unit: no test to name.
		return false
	}
	config := ""
	if strings.HasPrefix(unit, suite.Name+"[integration]") {
		config = " --config " + suite.IntegrationConfig
	}
	dir := filepath.Join(module, suite.Path)
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
	runner := filepath.Join(module, "bin", "background.sh")
	for _, file := range files {
		if whole[file] || len(byFile[file]) == 0 {
			s.add("vitest:"+file, fmt.Sprintf("cd %s && %s npx vitest run%s %s", dir, runner, config, file), file+": the whole file failed")
			continue
		}
		patterns := make([]string, len(byFile[file]))
		for i, name := range byFile[file] {
			patterns[i] = regexp.QuoteMeta(name)
		}
		command := fmt.Sprintf("cd %s && %s npx vitest run%s %s -t %s", dir, runner, config, file, shellword.Quote(strings.Join(patterns, "|")))
		for _, name := range byFile[file] {
			s.add("vitest:"+file, command, file+": "+name)
		}
	}
	return len(files) > 0
}

// readScriptUnit reads a shell harness's failure lines. A harness runs whole:
// its cases have no names of their own to select by.
func readScriptUnit(s *failingSuite, suite roster.Suite, lines []string, module string) bool {
	script := filepath.Join(module, suite.Path)
	if strings.HasPrefix(suite.Path, "/") {
		script = filepath.Join(filepath.Dir(filepath.Dir(filepath.Dir(module))), suite.Path[1:])
	}
	command := strings.Join(append([]string{script}, suite.Args...), " ")
	named := false
	for _, line := range lines {
		if m := scriptFailLine.FindStringSubmatch(line); m != nil {
			s.add("script:"+suite.Name, command, m[1])
			named = true
		}
	}
	return named
}

// render writes the failing tests as the file the fixing agent reads.
func (f failingTests) render(archive string) string {
	var b strings.Builder
	b.WriteString("# The failing tests of the merge queue's last test gate run.\n")
	b.WriteString(fmt.Sprintf("# The whole run is archived at %s.\n", archive))
	b.WriteString("# Run exactly these while fixing, with the command given for each group.\n")
	b.WriteString("# After your turn the queue runs the selected suites again.\n")
	if len(f.suites) == 0 {
		b.WriteString("\nNo failing test could be attributed to a suite: read the archive.\n")
		return b.String()
	}
	for _, s := range f.suites {
		b.WriteString(fmt.Sprintf("\n## %s\n", s.name))
		for _, g := range s.groups {
			b.WriteString(fmt.Sprintf("\nrun: %s\n", g.command))
			for _, test := range g.tests {
				b.WriteString(fmt.Sprintf("- %s\n", test))
			}
		}
		for _, unit := range s.unnamed {
			b.WriteString(fmt.Sprintf("\nunnamed: the tests of %s could not be named (a build, vet or typecheck failure, a crash or a timeout); read its output in the archive: grep -n %s %s\n",
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
	module := filepath.Join(r.subject.dir, moduleRoot)
	found := readFailingTests(string(output), module)
	path := filepath.Join(r.subject.dir, FailingTestsFile)
	body := found.render(archive)
	if err := os.WriteFile(path, []byte(body), 0o644); err != nil {
		return "", fmt.Errorf("merge: writing the failing tests to %s: %w", path, err)
	}
	named, unnamed := 0, 0
	for _, s := range found.suites {
		for _, g := range s.groups {
			named += len(g.tests)
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
