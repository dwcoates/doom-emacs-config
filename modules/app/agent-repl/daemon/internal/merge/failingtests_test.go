package merge

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
)

// testModule is the module path every invocation in these tests is spelled
// against.
const testModule = "/wt/modules/app/agent-repl"

// failedUnit is the runner's account of one failed unit: its output block and
// its FAILED verdict.
func failedUnit(unit, suite string, lines ...string) string {
	out := "[agent-repl-tests] unit " + unit + " [" + suite + "] output:\n"
	for _, line := range lines {
		out += line + "\n"
	}
	return out + "[agent-repl-tests] ERROR: unit " + unit + " [" + suite + "] FAILED with exit code 1 after 2.000s\n"
}

// suiteOf answers the named suite's failing tests, failing the test when the
// run named none for it.
func suiteOf(t *testing.T, found failingTests, name string) *failingSuite {
	t.Helper()
	for _, s := range found.suites {
		if s.name == name {
			return s
		}
	}
	t.Fatalf("no failing tests read for %s; read %+v", name, found.suites)
	return nil
}

func TestReadFailingTestsNamesAGoUnitsFailingTests(t *testing.T) {
	// Arrange.
	out := failedUnit("daemon:internal/merge#00", "daemon", "=== RUN   TestA", "--- FAIL: TestA (0.01s)", "--- PASS: TestB (0.00s)", "FAIL")

	// Act.
	s := suiteOf(t, readFailingTests(out, testModule), "daemon")

	// Assert.
	if len(s.groups) != 1 || !slices.Equal(s.groups[0].shown, []string{"TestA"}) {
		t.Fatalf("groups = %+v, want TestA alone", s.groups)
	}
}

func TestReadFailingTestsRunsAGoPackageFromItsModule(t *testing.T) {
	// Arrange.
	out := failedUnit("daemon:internal/merge#00", "daemon", "--- FAIL: TestA (0.01s)")

	// Act.
	s := suiteOf(t, readFailingTests(out, testModule), "daemon")

	// Assert.
	want := TestRunCommand + " -go '/wt/modules/app/agent-repl/daemon/internal/merge=TestA'"
	if got := s.groups[0].command(); got != want {
		t.Fatalf("command = %q, want %q", got, want)
	}
}

func TestReadFailingTestsRunsAnIntegrationPackageUnderItsTags(t *testing.T) {
	// Arrange.
	out := failedUnit("daemon:integration[integration]#03", "daemon", "--- FAIL: TestMergeLands (3.10s)")

	// Act.
	s := suiteOf(t, readFailingTests(out, testModule), "daemon")

	// Assert.
	if got := s.groups[0].command(); got != TestRunCommand+" -go '/wt/modules/app/agent-repl/daemon/integration=TestMergeLands'" {
		t.Fatalf("command = %q, want the integration package by its directory (the runner knows its tags)", got)
	}
}

func TestReadFailingTestsRunsJustTheFailingSubtest(t *testing.T) {
	// Arrange.
	out := failedUnit("daemon:internal/merge#00", "daemon", "    --- FAIL: TestA/one_case (0.00s)", "--- FAIL: TestA (0.00s)")

	// Act.
	s := suiteOf(t, readFailingTests(out, testModule), "daemon")

	// Assert.
	if !slices.Equal(s.groups[0].shown, []string{"TestA/one_case", "TestA"}) || !strings.HasSuffix(s.groups[0].command(), "=TestA/one_case'") {
		t.Fatalf("group = %+v (%s), want the subtest listed and run alone by its path", s.groups[0], s.groups[0].command())
	}
}

func TestReadFailingTestsRunsTheCrossSystemSuiteFromItsDirectory(t *testing.T) {
	// Arrange.
	out := failedUnit("e2e:.#02", "e2e", "--- FAIL: TestWebappLayerSurfaces (31.32s)")

	// Act.
	s := suiteOf(t, readFailingTests(out, testModule), "e2e")

	// Assert.
	if got := s.groups[0].command(); got != TestRunCommand+" -go '/wt/modules/app/agent-repl/e2e=TestWebappLayerSurfaces'" {
		t.Fatalf("command = %q, want the e2e package by its directory", got)
	}
}

func TestReadFailingTestsLeavesABuildUnitUnnamed(t *testing.T) {
	// Arrange.
	out := failedUnit("daemon:internal/merge:build", "daemon", "internal/merge/x.go:3:1: syntax error")

	// Act.
	s := suiteOf(t, readFailingTests(out, testModule), "daemon")

	// Assert.
	if len(s.groups) != 0 || !slices.Equal(s.unnamed, []string{"daemon:internal/merge:build"}) {
		t.Fatalf("suite = %+v, want the build unit unnamed", s)
	}
}

func TestReadFailingTestsLeavesAGoChunkWithNoFailLineUnnamed(t *testing.T) {
	// Arrange: a chunk that timed out prints no --- FAIL line.
	out := failedUnit("daemon:integration[integration]#01", "daemon", "panic: test timed out after 10m0s")

	// Act.
	s := suiteOf(t, readFailingTests(out, testModule), "daemon")

	// Assert.
	if !slices.Equal(s.unnamed, []string{"daemon:integration[integration]#01"}) {
		t.Fatalf("unnamed = %v, want the timed-out chunk", s.unnamed)
	}
}

func TestReadFailingTestsRunsTheEmacsClientLayersTestsThroughItsScript(t *testing.T) {
	// Arrange.
	out := failedUnit("e2e-emacs", "e2e-emacs", "--- FAIL: TestEmacsRestartKeepsTheSelectedWorkspace (8.74s)", "FAIL")

	// Act.
	s := suiteOf(t, readFailingTests(out, testModule), "e2e-emacs")

	// Assert.
	want := TestRunCommand + " -harness '/wt/modules/app/agent-repl/bin/test-e2e-emacs.sh'"
	if got := s.groups[0].command(); got != want {
		t.Fatalf("command = %q, want %q", got, want)
	}
}

func TestReadFailingTestsNamesAnERTTestWithItsFile(t *testing.T) {
	// Arrange.
	out := failedUnit("ert#00", "ert",
		"Test agent-repl-test-wire-verbs-merge-queue-action-arms-pinned condition:",
		"   FAILED  205/517  agent-repl-test-wire-verbs-merge-queue-action-arms-pinned (0.007587 sec) at test-wire-verbs.el:1537",
		"1 unexpected results:",
		"   FAILED  agent-repl-test-wire-verbs-merge-queue-action-arms-pinned")

	// Act.
	s := suiteOf(t, readFailingTests(out, testModule), "ert")

	// Assert.
	if !slices.Equal(s.groups[0].shown, []string{"test-wire-verbs.el: agent-repl-test-wire-verbs-merge-queue-action-arms-pinned"}) {
		t.Fatalf("tests = %v, want the test named with its file", s.groups[0].shown)
	}
}

func TestReadFailingTestsRunsERTTestsByNameInTheirFile(t *testing.T) {
	// Arrange.
	out := failedUnit("ert#00", "ert", "   FAILED  1/2  agent-repl-a (0.1 sec) at test-core.el:12")

	// Act.
	s := suiteOf(t, readFailingTests(out, testModule), "ert")

	// Assert.
	want := TestRunCommand + " -ert '/wt/modules/app/agent-repl/lisp/test-core.el=agent-repl-a'"
	if got := s.groups[0].command(); got != want {
		t.Fatalf("command = %q, want %q", got, want)
	}
}

func TestReadFailingTestsLeavesAnERTUnitWithNoLocatedFailureUnnamed(t *testing.T) {
	// Arrange: the chunk broke before any test reported.
	out := failedUnit("ert#01", "ert", "Error running tests")

	// Act.
	s := suiteOf(t, readFailingTests(out, testModule), "ert")

	// Assert.
	if !slices.Equal(s.unnamed, []string{"ert#01"}) {
		t.Fatalf("unnamed = %v, want the broken chunk", s.unnamed)
	}
}

func TestReadFailingTestsNamesAVitestTestByItsFullName(t *testing.T) {
	// Arrange.
	out := failedUnit("webapp#00", "webapp", " FAIL  test/link.test.ts > links > opens a file")

	// Act.
	s := suiteOf(t, readFailingTests(out, testModule), "webapp")

	// Assert.
	if !slices.Equal(s.groups[0].shown, []string{"test/link.test.ts: links opens a file"}) {
		t.Fatalf("tests = %v, want the file and full name", s.groups[0].shown)
	}
}

func TestReadFailingTestsRunsAVitestTestByItsFullName(t *testing.T) {
	// Arrange.
	out := failedUnit("webapp#00", "webapp", " FAIL  test/link.test.ts > links > opens a (file)")

	// Act.
	s := suiteOf(t, readFailingTests(out, testModule), "webapp")

	// Assert.
	want := TestRunCommand + ` -vitest '/wt/modules/app/agent-repl/webapp/test/link.test.ts=links opens a (file)'`
	if got := s.groups[0].command(); got != want {
		t.Fatalf("command = %q, want %q", got, want)
	}
}

func TestReadFailingTestsRunsAnIntegrationVitestFileByItsPath(t *testing.T) {
	// Arrange.
	out := failedUnit("webapp[integration]#00", "webapp", " FAIL  test/integration/sidebar.integration.test.ts > arm coverage > covers every roster status arm")

	// Act.
	s := suiteOf(t, readFailingTests(out, testModule), "webapp")

	// Assert.
	if got := s.groups[0].command(); got != TestRunCommand+` -vitest '/wt/modules/app/agent-repl/webapp/test/integration/sidebar.integration.test.ts=arm coverage covers every roster status arm'` {
		t.Fatalf("command = %q, want the integration file by its path (the runner knows its config)", got)
	}
}

func TestReadFailingTestsRunsAVitestFileThatFailedWholeWithNoPattern(t *testing.T) {
	// Arrange.
	out := failedUnit("shim#00", "shim", " FAIL  test/engine/session.test.ts [ test/engine/session.test.ts ]")

	// Act.
	s := suiteOf(t, readFailingTests(out, testModule), "shim")

	// Assert.
	if got := s.groups[0].command(); got != TestRunCommand+" -vitest '/wt/modules/app/agent-repl/agent-shim/claude/shim/test/engine/session.test.ts'" {
		t.Fatalf("command = %q, want the whole file run", got)
	}
}

func TestReadFailingTestsReadsAColoredVitestLine(t *testing.T) {
	// Arrange.
	out := failedUnit("webapp#00", "webapp", "\x1b[31m FAIL \x1b[39m test/a.test.ts > b")

	// Act.
	s := suiteOf(t, readFailingTests(out, testModule), "webapp")

	// Assert.
	if !slices.Equal(s.groups[0].shown, []string{"test/a.test.ts: b"}) {
		t.Fatalf("tests = %v, want the colored line read", s.groups[0].shown)
	}
}

func TestReadFailingTestsLeavesTheTypecheckUnitUnnamed(t *testing.T) {
	// Arrange.
	out := failedUnit("webapp:typecheck", "webapp", "src/a.ts(3,1): error TS2304: Cannot find name 'x'.")

	// Act.
	s := suiteOf(t, readFailingTests(out, testModule), "webapp")

	// Assert.
	if !slices.Equal(s.unnamed, []string{"webapp:typecheck"}) {
		t.Fatalf("unnamed = %v, want the typecheck unit", s.unnamed)
	}
}

func TestReadFailingTestsNamesAHarnessesFailedCases(t *testing.T) {
	tests := []struct {
		name string
		line string
		want string
	}{
		{name: "the dash form", line: "FAIL - scan: every test entry point routes through bin/background.sh", want: "scan: every test entry point routes through bin/background.sh"},
		{name: "the colon form", line: "  FAIL: the lock is released", want: "the lock is released"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			out := failedUnit("background-harness", "background-harness", "ok   - a pass", tt.line)

			// Act.
			s := suiteOf(t, readFailingTests(out, testModule), "background-harness")

			// Assert.
			if !slices.Equal(s.groups[0].shown, []string{tt.want}) || s.groups[0].command() != TestRunCommand+" -harness '/wt/modules/app/agent-repl/bin/test-background.sh'" {
				t.Fatalf("group = %+v, want %q run through its harness", s.groups[0], tt.want)
			}
		})
	}
}

func TestReadFailingTestsRunsARepositoryHarnessFromTheRepositoryRoot(t *testing.T) {
	// Arrange.
	out := failedUnit("precommit-harness", "precommit-harness", "  FAIL: the hook refuses")

	// Act.
	s := suiteOf(t, readFailingTests(out, testModule), "precommit-harness")

	// Assert.
	if got := s.groups[0].command(); got != TestRunCommand+" -harness '/wt/.githooks/test-pre-commit.sh'" {
		t.Fatalf("command = %q, want the hook's harness at the repository root", got)
	}
}

func TestReadFailingTestsLeavesAUnitThatNeverStartedUnnamed(t *testing.T) {
	// Arrange.
	out := "[agent-repl-tests] ERROR: unit store:vet (store) could not start: exec: go: not found\n"

	// Act.
	s := suiteOf(t, readFailingTests(out, testModule), "store")

	// Assert.
	if !slices.Equal(s.unnamed, []string{"store:vet"}) {
		t.Fatalf("unnamed = %v, want the unit that never started", s.unnamed)
	}
}

func TestReadFailingTestsLeavesASuiteThatFailedWithNoUnitUnnamed(t *testing.T) {
	// Arrange.
	out := "[agent-repl-tests] ERROR: sidecar failed after 1s with exit code 2\n"

	// Act.
	s := suiteOf(t, readFailingTests(out, testModule), "sidecar")

	// Assert.
	if !slices.Equal(s.unnamed, []string{"sidecar"}) {
		t.Fatalf("unnamed = %v, want the suite itself", s.unnamed)
	}
}

func TestReadFailingTestsIgnoresAUnitNotRunForItsDependency(t *testing.T) {
	// Arrange.
	out := "[agent-repl-tests] ERROR: unit daemon:internal/merge#00 [daemon] NOT RUN: its dependency daemon:internal/merge:build did not pass\n"

	// Act.
	found := readFailingTests(out, testModule)

	// Assert.
	if len(found.suites) != 0 {
		t.Fatalf("suites = %+v, want none: the dependency's failure is the one to read", found.suites)
	}
}

func TestReadFailingTestsIgnoresAPassingUnit(t *testing.T) {
	// Arrange.
	out := "[agent-repl-tests] unit daemon:x#00 [daemon] output:\n--- FAIL: TestFlakyButRetried (0.0s)\n[agent-repl-tests] unit daemon:x#00 [daemon] ok, 1.000s wall, 1.000s cpu\n"

	// Act.
	found := readFailingTests(out, testModule)

	// Assert.
	if len(found.suites) != 0 {
		t.Fatalf("suites = %+v, want none from a unit that passed", found.suites)
	}
}

func TestReadFailingTestsOrdersSuitesAsTheRoster(t *testing.T) {
	// Arrange.
	out := failedUnit("e2e:.#00", "e2e", "--- FAIL: TestE (1s)") + failedUnit("ert#00", "ert", "   FAILED  1/1  a (0.1 sec) at test-a.el:1")

	// Act.
	found := readFailingTests(out, testModule)

	// Assert.
	if len(found.suites) != 2 || found.suites[0].name != "ert" || found.suites[1].name != "e2e" {
		t.Fatalf("suites in order %v, want ert then e2e", found.suites)
	}
}

func TestRenderSaysWhenNoSuiteCouldBeAttributed(t *testing.T) {
	// Act.
	text := failingTests{}.render("/state/merge-logs/l-tests-1.log")

	// Assert.
	if !strings.Contains(text, "No failing test could be attributed to a suite: read the archive.") {
		t.Fatalf("text = %q, want the archive pointed to", text)
	}
}

func TestRenderPointsAnUnnamedUnitToItsBlockInTheArchive(t *testing.T) {
	// Arrange.
	found := failingTests{suites: []*failingSuite{{name: "daemon", unnamed: []string{"daemon:vet"}}}}

	// Act.
	text := found.render("/logs/x.log")

	// Assert.
	if !strings.Contains(text, "the tests of daemon:vet could not be named") || !strings.Contains(text, "grep -n 'daemon:vet' /logs/x.log") {
		t.Fatalf("text = %q, want the unnamed unit and where to read it", text)
	}
}

func TestRenderListsEachGroupsCommandAndTests(t *testing.T) {
	// Arrange.
	flags := func(names []string) []string { return []string{selectionFlag("go", "/p", names)} }
	found := failingTests{suites: []*failingSuite{{name: "daemon", groups: []*failingGroup{{flags: flags, names: []string{"TestA", "TestB"}, shown: []string{"TestA", "TestB"}}}}}}

	// Act.
	text := found.render("/logs/x.log")

	// Assert.
	if !strings.Contains(text, "## daemon\n\nrun: "+TestRunCommand+" -go '/p=TestA,TestB'\n- TestA\n- TestB\n") {
		t.Fatalf("text = %q, want the group's command then its tests", text)
	}
}

// failingTestsRun is a run whose worktree is the harness's source directory.
func failingTestsRun(t *testing.T, h *harness) *run {
	t.Helper()
	r := ledgeredRun(t, h)
	r.subject = subject{dir: h.sourceD, branch: "feature", targetDir: h.targetD}
	return r
}

// writeArchive writes a failing run's archive and answers its path.
func writeArchive(t *testing.T, body string) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), "tests-1.log")
	if err := os.WriteFile(path, []byte(body), 0o644); err != nil {
		t.Fatalf("writing the archive: %v", err)
	}
	return path
}

func TestWriteFailingTestsWritesTheFileAtTheWorktreesRoot(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	r := failingTestsRun(t, h)
	archive := writeArchive(t, failedUnit("daemon:internal/merge#00", "daemon", "--- FAIL: TestA (0.01s)"))

	// Act.
	_, err := r.writeFailingTests(context.Background(), archive)

	// Assert.
	body, readErr := os.ReadFile(filepath.Join(h.sourceD, FailingTestsFile))
	if err != nil || readErr != nil || !strings.Contains(string(body), "- TestA") {
		t.Fatalf("err %v, read %v, body %q: want TestA written at the worktree's root", err, readErr, body)
	}
}

func TestWriteFailingTestsAnswersTheBodyItWrote(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	r := failingTestsRun(t, h)
	archive := writeArchive(t, failedUnit("daemon:internal/merge#00", "daemon", "--- FAIL: TestA (0.01s)"))

	// Act.
	answered, err := r.writeFailingTests(context.Background(), archive)

	// Assert.
	written, readErr := os.ReadFile(filepath.Join(h.sourceD, FailingTestsFile))
	if err != nil || readErr != nil || answered != string(written) {
		t.Fatalf("err %v, read %v: answered %q, wrote %q, want the same body", err, readErr, answered, written)
	}
}

func TestWriteFailingTestsExcludesTheFileFromCommits(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	r := failingTestsRun(t, h)
	archive := writeArchive(t, "")

	// Act.
	_, err := r.writeFailingTests(context.Background(), archive)

	// Assert.
	body, readErr := os.ReadFile(filepath.Join(string(h.repoKey()), "info", "exclude"))
	if err != nil || readErr != nil || !strings.Contains(string(body), "/"+FailingTestsFile+"\n") {
		t.Fatalf("err %v, read %v, exclude %q: want the file excluded", err, readErr, body)
	}
}

func TestWriteFailingTestsExcludesTheFileOnce(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	r := failingTestsRun(t, h)
	archive := writeArchive(t, "")

	// Act.
	for range 2 {
		if _, err := r.writeFailingTests(context.Background(), archive); err != nil {
			t.Fatalf("writeFailingTests: %v", err)
		}
	}

	// Assert.
	body, _ := os.ReadFile(filepath.Join(string(h.repoKey()), "info", "exclude"))
	if got := strings.Count(string(body), "/"+FailingTestsFile); got != 1 {
		t.Fatalf("exclude lists the file %d times, want once: %q", got, body)
	}
}

func TestWriteFailingTestsKeepsTheExcludesOwnLines(t *testing.T) {
	// Arrange: the repository's exclude, in its common git directory, whose
	// last line has no newline.
	h := newHarness(t)
	r := failingTestsRun(t, h)
	exclude := filepath.Join(string(h.repoKey()), "info", "exclude")
	if err := os.MkdirAll(filepath.Dir(exclude), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(exclude, []byte("*.swp"), 0o644); err != nil {
		t.Fatal(err)
	}

	// Act.
	_, err := r.writeFailingTests(context.Background(), writeArchive(t, ""))

	// Assert.
	body, _ := os.ReadFile(exclude)
	if err != nil || string(body) != "*.swp\n/"+FailingTestsFile+"\n" {
		t.Fatalf("err %v, exclude %q: want the owner's line kept and ours on its own", err, body)
	}
}

func TestWriteFailingTestsFailsLoudlyOnAMissingArchive(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	r := failingTestsRun(t, h)

	// Act.
	_, err := r.writeFailingTests(context.Background(), filepath.Join(t.TempDir(), "gone.log"))

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "gone.log") {
		t.Fatalf("err = %v, want the missing archive named", err)
	}
}

// fixingBriefValues answers the values the first fixing brief was spliced
// with, failing the test when no fixing brief was composed.
func fixingBriefValues(t *testing.T, h *harness) map[string]string {
	t.Helper()
	h.mu.Lock()
	defer h.mu.Unlock()
	for _, values := range h.briefValues {
		if _, ok := values["failing_tests"]; ok {
			return values
		}
	}
	t.Fatal("no fixing brief was composed")
	return nil
}

func TestAFixingAttemptsBriefNamesTheFailingTestsFile(t *testing.T) {
	// Arrange: one failing gate, then a pass.
	h := newHarness(t)
	landing(h, 1)
	h.runner.runs = nil
	h.gateFails("daemon")
	h.gatePasses("daemon")

	// Act.
	admitted(t, h)

	// Assert.
	if got := fixingBriefValues(t, h)["failing_tests_file"]; got != filepath.Join(h.sourceD, FailingTestsFile) {
		t.Fatalf("failing_tests_file = %q, want the worktree's file", got)
	}
}

func TestAFixingAttemptsBriefCarriesTheFailingTestCasesInline(t *testing.T) {
	// Arrange: one gate failing in one named test, then a pass.
	h := newHarness(t)
	landing(h, 1)
	h.runner.runs = []scriptedRun{{
		Output: failedUnit("daemon:internal/merge#00", "daemon", "--- FAIL: TestInline (0.01s)") +
			"[agent-repl-tests] ERROR: daemon failed after 4s with exit code 1\n",
		Code: 1,
	}}
	h.gatePasses("daemon")

	// Act.
	admitted(t, h)

	// Assert.
	got := fixingBriefValues(t, h)["failing_tests"]
	written, err := os.ReadFile(filepath.Join(h.sourceD, FailingTestsFile))
	if err != nil || got != string(written) || !strings.Contains(got, "- TestInline") || !strings.Contains(got, TestRunCommand+" -go '"+filepath.Join(h.sourceD, moduleRoot, "daemon", "internal", "merge")+"=TestInline'") {
		t.Fatalf("err %v: brief carries %q, file holds %q, want the file's body naming TestInline and its targeted run", err, got, written)
	}
}

func TestAResumedFixingAttemptsBriefCarriesTheFailingTestCasesInline(t *testing.T) {
	// Arrange: a fixing turn no store knows, so the resume submits its brief.
	h := newHarness(t)
	interruptedAt(t, h, atFix(t))
	landsAfterResume(h)

	// Act.
	resume(t, h)

	// Assert.
	if got := fixingBriefValues(t, h)["failing_tests"]; !strings.Contains(got, "- TestResumed") {
		t.Fatalf("failing_tests = %q, want the resumed attempt's failing test named", got)
	}
}

func TestAResumedFixingAttemptWithNoArchiveFailsLoudly(t *testing.T) {
	// Arrange: a fixing record whose archive is gone.
	h := newHarness(t)
	gone := filepath.Join(t.TempDir(), "gone.log")
	interruptedAt(t, h, func(d *progressDoc) {
		atFix(t)(d)
		d.FailingArchive = gone
	})

	// Act.
	resume(t, h)

	// Assert.
	if facts := h.footer.last(); facts.State != StateFailed || !strings.Contains(facts.Detail, gone) {
		t.Fatalf("facts = %+v, want a failure naming the missing archive", facts)
	}
	if rec, ok := h.recordFor("error", "daemon.merge.fault"); !ok || !strings.Contains(fmt.Sprint(rec.Context["summary"]), gone) {
		t.Fatalf("record %+v (found %v), want the fault at ERROR naming the missing archive", rec, ok)
	}
	h.mu.Lock()
	defer h.mu.Unlock()
	for _, values := range h.briefValues {
		if _, ok := values["failing_tests"]; ok {
			t.Fatalf("a fixing brief was composed with no archive: %v", values)
		}
	}
}

func TestAFixingAttemptWritesTheFailingTestsBeforeItsBrief(t *testing.T) {
	// Arrange: one failing gate, then a pass.
	h := newHarness(t)
	landing(h, 1)
	h.runner.runs = nil
	h.gateFails("daemon")
	h.gatePasses("daemon")

	// Act.
	admitted(t, h)

	// Assert.
	if _, err := os.Stat(filepath.Join(h.sourceD, FailingTestsFile)); err != nil {
		t.Fatalf("the failing tests file is not in the worktree: %v", err)
	}
}

func TestRenderRunsEveryFailingTestInOneCommand(t *testing.T) {
	// Arrange.
	out := failedUnit("daemon:internal/merge#00", "daemon", "--- FAIL: TestA (0.01s)") +
		failedUnit("ert#00", "ert", "   FAILED  1/2  agent-repl-a (0.1 sec) at test-core.el:12")

	// Act.
	text := readFailingTests(out, testModule).render("/logs/x.log")

	// Assert.
	// In roster order: the ERT suite before the daemon's.
	want := "\nrun all: " + TestRunCommand + " -ert '/wt/modules/app/agent-repl/lisp/test-core.el=agent-repl-a' -go '/wt/modules/app/agent-repl/daemon/internal/merge=TestA'\n"
	if !strings.Contains(text, want) {
		t.Fatalf("text = %q, want the one command running every failing test: %q", text, want)
	}
}

func TestReadFailingTestsGathersAPackagesFailuresAcrossItsChunks(t *testing.T) {
	// Arrange: one package's tests failed in two chunks.
	out := failedUnit("daemon:internal/merge#00", "daemon", "--- FAIL: TestA (0.01s)") +
		failedUnit("daemon:internal/merge#01", "daemon", "--- FAIL: TestZ (0.01s)")

	// Act.
	s := suiteOf(t, readFailingTests(out, testModule), "daemon")

	// Assert.
	if len(s.groups) != 1 || !strings.HasSuffix(s.groups[0].command(), "=TestA,TestZ'") {
		t.Fatalf("groups = %+v, want one command naming both chunks' failures", s.groups)
	}
}

func TestDeepest(t *testing.T) {
	cases := []struct {
		name  string
		names []string
		want  []string
	}{
		{name: "a parent failed by its subtest", names: []string{"TestA/x/y", "TestA/x", "TestA"}, want: []string{"TestA/x/y"}},
		{name: "siblings each kept", names: []string{"TestA/x", "TestA/y", "TestA", "TestB"}, want: []string{"TestA/x", "TestA/y", "TestB"}},
		{name: "a name sharing a prefix is no parent", names: []string{"TestAB", "TestA"}, want: []string{"TestAB", "TestA"}},
		{name: "a repeat named once", names: []string{"TestA", "TestA"}, want: []string{"TestA"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := deepest(tc.names)

			// Assert.
			if !slices.Equal(got, tc.want) {
				t.Fatalf("deepest = %q, want %q", got, tc.want)
			}
		})
	}
}

// THE FIXING AGENT IS NEVER HANDED A HAND-RUN TEST COMMAND: bin/background.sh
// refuses every test run the daemon did not start.
func TestNoFailingTestCommandRunsATestByHand(t *testing.T) {
	// Arrange: a failure of every kind of suite.
	out := failedUnit("daemon:internal/merge#00", "daemon", "--- FAIL: TestA (0.01s)") +
		failedUnit("e2e-emacs", "e2e-emacs", "--- FAIL: TestE (1.00s)") +
		failedUnit("ert#00", "ert", "   FAILED  1/2  agent-repl-a (0.1 sec) at test-core.el:12") +
		failedUnit("webapp#00", "webapp", " FAIL  test/a.test.ts > b") +
		failedUnit("background-harness", "background-harness", "  FAIL: x")

	// Act.
	text := readFailingTests(out, testModule).render("/logs/x.log")

	// Assert.
	for _, line := range strings.Split(text, "\n") {
		if strings.HasPrefix(line, "run") && !strings.Contains(line, ": "+TestRunCommand+" ") {
			t.Fatalf("line %q runs tests by hand, want %s", line, TestRunCommand)
		}
	}
}
