package daemonrules

import (
	"fmt"
	"go/ast"
	"go/token"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"strconv"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// integrationHarnessDir is the package these rules hold, under the daemon module's root.
const integrationHarnessDir = "integration/harness"

// TestEveryDaemonKeepsItsFailureLogsThroughOneHelper pins the extraction: an
// owned daemon and a view both keep their logs through keepFailureLogs under
// registerLogChecks, and no other harness file calls preserveFailureLogs by
// hand.
func TestEveryDaemonKeepsItsFailureLogsThroughOneHelper(t *testing.T) {
	sourcescan.InPackage(t, integrationHarnessDir)
	// Arrange
	files := sourcescan.Production(t, integrationHarnessDir)

	// Act
	var handRolled, helperSites []string
	for _, f := range files {
		src := string(f.Source)
		if f.Name != "artifacts.go" && strings.Contains(src, "preserveFailureLogs(") {
			handRolled = append(handRolled, f.Name)
		}
		if strings.Contains(src, "registerLogChecks(t, func() { keepFailureLogs(t, ") {
			helperSites = append(helperSites, f.Name)
		}
	}

	// Assert
	if len(handRolled) != 0 {
		t.Errorf("files calling preserveFailureLogs by hand: %v; keep failure logs through keepFailureLogs", handRolled)
	}
	if strings.Join(helperSites, ",") != "daemon.go,view.go" {
		t.Errorf("files keeping logs through the helper = %v, want daemon.go and view.go", helperSites)
	}
}

func TestNoTestSetsTheRetiredTestAllVariable(t *testing.T) {
	sourcescan.InPackage(t, integrationHarnessDir)
	// Arrange: the daemon no longer reads AGENT_REPL_TEST_ALL_SCRIPT; a merge
	// gate runs the script checked in to the tree it tests
	// (Repo.TestAllScript), so a test still setting it would set nothing.
	var files []string
	for _, glob := range []string{"../*.go", "../../../e2e/*.go", "../../../e2e/kit/*.go"} {
		matched, err := filepath.Glob(glob)
		if err != nil {
			t.Fatalf("glob %s: %v", glob, err)
		}
		files = append(files, matched...)
	}
	if len(files) == 0 {
		t.Fatal("found no test files to read")
	}

	// Act
	var offenders []string
	for _, file := range files {
		body, err := os.ReadFile(file)
		if err != nil {
			t.Fatalf("read %s: %v", file, err)
		}
		if strings.Contains(string(body), "AGENT_REPL_TEST_ALL"+"_SCRIPT") {
			offenders = append(offenders, file)
		}
	}

	// Assert
	if len(offenders) != 0 {
		t.Fatalf("%v name the retired test-all variable; check the script in with Repo.TestAllScript", offenders)
	}
}

// TestEveryHarnessExecutableIsWrittenThroughWriteExecutable pins that no
// harness source writes an executable by hand.
func TestEveryHarnessExecutableIsWrittenThroughWriteExecutable(t *testing.T) {
	sourcescan.InPackage(t, integrationHarnessDir)
	// Act
	var byHand []string
	for _, f := range sourcescan.Production(t, integrationHarnessDir) {
		for i, line := range strings.Split(string(f.Source), "\n") {
			if strings.Contains(line, "os.WriteFile(") && strings.Contains(line, "0o755") {
				byHand = append(byHand, fmt.Sprintf("%s:%d", f.Name, i+1))
			}
		}
	}

	// Assert
	if len(byHand) != 0 {
		t.Fatalf("executables written by hand at %v, want writeExecutable", byHand)
	}
}

// Every daemonProc is made by newDaemonProc, so none lacks its fault latch.
func TestEveryDaemonProcIsMadeByNewDaemonProc(t *testing.T) {
	sourcescan.InPackage(t, integrationHarnessDir)
	// Act
	sites := sourcescan.Sites(t, integrationHarnessDir, "&daemonProc{")

	// Assert
	if strings.Join(sites, ",") != "daemon.go" {
		t.Fatalf("&daemonProc{ literals in %v, want only newDaemonProc's in daemon.go", sites)
	}
}

// Every run budget is a child of the record latch, so no test-facing wait
// outlives its daemon's fault.
func TestEveryRunBudgetIsAChildOfTheFaultLatches(t *testing.T) {
	sourcescan.InPackage(t, integrationHarnessDir)
	// Act
	sites := sourcescan.Sites(t, integrationHarnessDir, "WithBound(d.watchRecords(t, d.fault.ctx)")
	views := sourcescan.Sites(t, integrationHarnessDir, "WithBound(v.watchRecords(t, d.fault.ctx)")

	// Assert
	if strings.Join(sites, ",") != "daemon.go" || strings.Join(views, ",") != "view.go" {
		t.Fatalf("run budgets under the latches: owner %v, view %v; want StartDaemon's and View's", sites, views)
	}
}

// TestEveryBlockingHarnessLockGoesThroughTakeExclusive pins the call sites:
// a blocking LOCK_EX written by hand outside takeExclusive is a drifted copy.
func TestEveryBlockingHarnessLockGoesThroughTakeExclusive(t *testing.T) {
	sourcescan.InPackage(t, integrationHarnessDir)
	for _, file := range []string{"runroot.go", "flock.go"} {
		body, err := os.ReadFile(file)
		if err != nil {
			t.Fatalf("read %s: %v", file, err)
		}
		for i, line := range strings.Split(string(body), "\n") {
			if strings.Contains(line, "syscall.LOCK_EX)") && !strings.Contains(line, "return syscall.Flock(int(f.Fd()), syscall.LOCK_EX)") {
				t.Fatalf("%s:%d takes a blocking flock by hand: %s", file, i+1, strings.TrimSpace(line))
			}
		}
	}
}

// wallBound is a hand-rolled wall-clock bound, or a deadline read off a
// context, written out in the harness's source.
var wallBound = regexp.MustCompile(`context\.WithTimeout\(|context\.WithDeadline\(|time\.After\(|time\.NewTimer\(|time\.Now\(\)\.(Add|After|Before)\(|\.Deadline\(\)`)

// suiteWallBound is wallBound for the suite's tests, less time.Now().Add,
// which they use for timestamps they hand the daemon as data; a deadline built
// from one is still caught where it is checked.
var suiteWallBound = regexp.MustCompile(`context\.WithTimeout\(|context\.WithDeadline\(|time\.After\(|time\.NewTimer\(|time\.Now\(\)\.(After|Before)\(|\.Deadline\(\)`)

// wallBoundAllowed is every wall-clock bound the harness and the suite keep on
// purpose, by file and the line's text, with why the granted clock is not the
// right clock for it.
var wallBoundAllowed = map[string]map[string]string{
	"harness/daemon.go": {
		`AGENT_REPL_LOG_LEVEL_UNTIL=%d", time.Now().Add(5*time.Minute)`:            "an instant handed to the daemon as configuration, not a bound on a wait",
		`ctx, cancel := context.WithTimeout(context.Background(), DefaultTimeout)`: "the coverage-only stand-down of strays, best effort and reporting nothing; a stray launchd has adopted is outside this binary's process tree",
	},
	"harness/procstate.go": {
		`deadline := time.NewTimer(bound)`:     "awaitFrozen confirms a stray's SIGSTOP; a stray launchd has adopted is outside this binary's process tree",
		`report := time.NewTimer(reportAfter)`: "stopFrozen's stall report on a stray; a stray launchd has adopted is outside this binary's process tree",
	},
	"harness/proctable_darwin.go": {
		`deadline := time.NewTimer(freezeBound)`: "procArgs reads the argv of any process this user owns, most of them outside this binary's process tree",
	},
}

// TestEveryWallBoundIsAllowedByName pins that every bound the harness and the
// integration suite put on a wait is kept on the granted clock
// (grantedclock.WithBound): a hand-rolled wall-clock bound in either fails
// here unless wallBoundAllowed names it with its reason, and an allowance whose
// line is gone fails too, so the list never outlives what it excuses.
func TestEveryWallBoundIsAllowedByName(t *testing.T) {
	sourcescan.InPackage(t, integrationHarnessDir)
	// Arrange: the harness's own sources and the suite's tests. The harness's
	// tests exercise wall-clock waits directly and are not bounds on the
	// daemon.
	var files []string
	for _, glob := range []string{"*.go", "../*_test.go"} {
		matches, err := filepath.Glob(glob)
		if err != nil {
			t.Fatalf("glob %s: %v", glob, err)
		}
		files = append(files, matches...)
	}

	// Act
	found := map[string]map[string]bool{}
	var unallowed []string
	for _, path := range files {
		if filepath.Dir(path) == "." && strings.HasSuffix(path, "_test.go") {
			continue
		}
		body, err := os.ReadFile(path)
		if err != nil {
			t.Fatalf("read %s: %v", path, err)
		}
		key, pattern := filepath.ToSlash(filepath.Join("harness", path)), wallBound
		if strings.HasPrefix(path, "..") {
			key, pattern = filepath.Base(path), suiteWallBound
		}
		for i, line := range strings.Split(string(body), "\n") {
			if !pattern.MatchString(line) || strings.HasPrefix(strings.TrimSpace(line), "//") {
				continue
			}
			allowed := false
			for text := range wallBoundAllowed[key] {
				if strings.Contains(line, text) {
					allowed = true
					if found[key] == nil {
						found[key] = map[string]bool{}
					}
					found[key][text] = true
				}
			}
			if !allowed {
				unallowed = append(unallowed, key+":"+strconv.Itoa(i+1)+": "+strings.TrimSpace(line))
			}
		}
	}

	// Assert
	if len(unallowed) > 0 {
		t.Errorf("wall-clock bounds not kept on the granted clock (use grantedclock.WithBound, or allow one by name with its reason):\n%s", strings.Join(unallowed, "\n"))
	}
	for file, texts := range wallBoundAllowed {
		for text := range texts {
			if !found[file][text] {
				t.Errorf("wallBoundAllowed names %s in %s, which no longer holds it", text, file)
			}
		}
	}
}

func TestStartDaemonAppliesHostNeutralCheckoutEnvAfterTheInheritedEnvironment(t *testing.T) {
	sourcescan.InPackage(t, integrationHarnessDir)
	// The one call site must state the cleared checkout after os.Environ() and
	// before ExtraEnv, so it beats the host's value and a test can still set one.
	assertAppendedBetweenInheritedAndExtra(t, "HostNeutralCheckoutEnv")
}

func TestStartDaemonAppliesHostNeutralNamingEnvAfterTheInheritedEnvironment(t *testing.T) {
	sourcescan.InPackage(t, integrationHarnessDir)
	// The one call site must state the cleared prefix after os.Environ() and
	// before ExtraEnv, so it beats the host's value and a test can still set one.
	assertAppendedBetweenInheritedAndExtra(t, "HostNeutralNamingEnv")
}

// assertAppendedBetweenInheritedAndExtra asserts that StartDaemon appends
// FN's env after os.Environ() and before opts.ExtraEnv: late enough to beat
// the host's values, early enough that a test's own ExtraEnv still wins.
func assertAppendedBetweenInheritedAndExtra(t *testing.T, fn string) {
	t.Helper()
	src, err := os.ReadFile("daemon.go")
	if err != nil {
		t.Fatal(err)
	}
	body := string(src)
	inherited := strings.Index(body, "env := append(os.Environ(),")
	neutral := regexp.MustCompile(`env = append\(env, ` + fn + `\(\)\.\.\.\)`).FindStringIndex(body)
	extra := strings.Index(body, "opts.ExtraEnv...")
	if inherited < 0 || neutral == nil || extra < 0 || !(inherited < neutral[0] && neutral[0] < extra) {
		t.Fatalf("StartDaemon must append %s() between os.Environ() and opts.ExtraEnv (offsets %d, %v, %d)", fn, inherited, neutral, extra)
	}
}

func TestEveryHostNeutralEnvIsBuiltByStatedEmpty(t *testing.T) {
	sourcescan.InPackage(t, integrationHarnessDir)
	// Each host-neutral env states its keys through statedEmpty, so no second
	// spelling of "clear this key" can drift from the first.
	src, err := os.ReadFile("daemon.go")
	if err != nil {
		t.Fatal(err)
	}
	for _, fn := range []string{"HostNeutralNamingEnv", "HostNeutralCheckoutEnv", "HostNeutralSlackEnv"} {
		body := regexp.MustCompile(`func ` + fn + `\(\) \[\]string \{\n\treturn statedEmpty\(`)
		if !body.Match(src) {
			t.Fatalf("%s does not build its env with statedEmpty", fn)
		}
	}
}

func TestStartDaemonAppliesHostNeutralNewsDigestEnvAfterTheInheritedEnvironment(t *testing.T) {
	sourcescan.InPackage(t, integrationHarnessDir)
	// The one call site must state the delayed schedule after os.Environ()
	// and before ExtraEnv, so it beats the host's value and a digest test can
	// still set its own.
	assertAppendedBetweenInheritedAndExtra(t, "HostNeutralNewsDigestEnv")
}

func TestStartDaemonAppliesHostNeutralSlackEnvAfterTheInheritedEnvironment(t *testing.T) {
	sourcescan.InPackage(t, integrationHarnessDir)
	// The one call site must state the cleared teams after os.Environ() and
	// before ExtraEnv, so it beats the host's value and a test can still set one.
	assertAppendedBetweenInheritedAndExtra(t, "HostNeutralSlackEnv")
}

func TestStartDaemonAppliesHostNeutralUserEnvEnvAfterTheInheritedEnvironment(t *testing.T) {
	sourcescan.InPackage(t, integrationHarnessDir)
	// The one call site must state the cleared env file after os.Environ() and
	// before ExtraEnv, so it beats the host's value and a test can still set one.
	assertAppendedBetweenInheritedAndExtra(t, "HostNeutralUserEnvEnv")
}

func TestEveryLoopingStandInIsStartedByStandIn(t *testing.T) {
	sourcescan.InPackage(t, integrationHarnessDir)
	// Arrange.
	src, err := os.ReadFile("kill_test.go")
	if err != nil {
		t.Fatal(err)
	}

	// Act: shells started by hand, outside standInTo and groupUnderKill.
	byHand := strings.Count(string(src), "exec.Command("+`"/bin/sh"`)

	// Assert: standInTo, groupUnderKill and the reaped-flag table, which
	// leaves its group to the Kill under test.
	if byHand != 3 {
		t.Fatalf("kill_test.go starts %d shells by hand, want 3: start a looping stand-in with standIn", byHand)
	}
}

// TestEveryStallReportListsThroughStallListing pins that both stuck-kill
// reports share the one listing, so their columns cannot drift apart.
func TestEveryStallReportListsThroughStallListing(t *testing.T) {
	sourcescan.InPackage(t, integrationHarnessDir)
	// Arrange
	src, err := os.ReadFile("daemon.go")
	if err != nil {
		t.Fatal(err)
	}

	// Act
	direct := strings.Count(string(src), `"pid,ppid,pgid,stat,wchan,flags,time,command"`)
	shared := strings.Count(string(src), "stallListing(")

	// Assert
	if direct != 1 || shared != 3 {
		t.Fatalf("the listing's columns are spelled %d times and stallListing named %d times, want 1 and 3 (its definition and both reports)", direct, shared)
	}
}

// TestTheHarnessKillsAStoppedGroupThroughOneHelper pins that the harness
// SIGKILLs and continues a process group only through killStoppedGroup, so
// Kill's own group kill and awaitGroupEnd's re-kill cannot drift apart: no
// production file of the package signals a group's SIGKILL or SIGCONT itself.
func TestTheHarnessKillsAStoppedGroupThroughOneHelper(t *testing.T) {
	sourcescan.InPackage(t, integrationHarnessDir)
	// Arrange
	files := sourcescan.Production(t, integrationHarnessDir)
	handRolled := regexp.MustCompile(`signalGroup\([^)]*syscall\.SIG(KILL|CONT)\)`)

	for _, f := range files {
		// Act
		loc := handRolled.FindIndex(f.Source)

		// Assert
		if loc != nil {
			t.Errorf("%s signals a group's SIGKILL or SIGCONT by hand (%q); use killStoppedGroup", f.Name, f.Source[loc[0]:loc[1]])
		}
	}
}

// TestNoTurnCountWaitCountsTheTurnEndedOperation pins that a wait for N turns
// counts their durable closes (AwaitTurnsClosed): one turn's close writes
// several OpTurnEnded records, so a count of the operation is met before the
// N-th turn has left the watcher.
func TestNoTurnCountWaitCountsTheTurnEndedOperation(t *testing.T) {
	sourcescan.InPackage(t, integrationHarnessDir)
	// Arrange
	files, err := filepath.Glob(filepath.Join("..", "*_test.go"))
	if err != nil || len(files) == 0 {
		t.Fatalf("glob the integration tests = %v, %v, want files", files, err)
	}
	counting := regexp.MustCompile(`AwaitWorkspaceLogOperationCount\([^)]*harness\.OpTurnEnded`)

	// Act
	var found []string
	for _, f := range files {
		body, err := os.ReadFile(f)
		if err != nil {
			t.Fatalf("read %s: %v", f, err)
		}
		if counting.Match(body) {
			found = append(found, f)
		}
	}

	// Assert
	if len(found) != 0 {
		t.Fatalf("integration tests count harness.OpTurnEnded records instead of AwaitTurnsClosed: %v", found)
	}
}

func TestStartDaemonAppliesNoBuildCacheEnvAfterTheInheritedEnvironment(t *testing.T) {
	sourcescan.InPackage(t, integrationHarnessDir)
	// The one call site must state it after os.Environ() and before ExtraEnv,
	// so it beats the host's cache and a test can still name one.
	assertAppendedBetweenInheritedAndExtra(t, "NoBuildCacheEnv")
}

// TestHarnessTakesKernelEventsOnlyThroughKernelevent pins that the harness
// holds no kernel-event code of its own: every exit and path wait goes
// through internal/kernelevent, the one implementation production shares.
func TestHarnessTakesKernelEventsOnlyThroughKernelevent(t *testing.T) {
	sourcescan.InPackage(t, integrationHarnessDir)
	// Arrange
	forbidden := []string{"unix.Kqueue(", "unix.Kevent(", "unix.PidfdOpen(", "unix.InotifyInit"}

	for _, f := range sourcescan.Production(t, integrationHarnessDir) {
		t.Run(f.Name, func(t *testing.T) {
			// Act, Assert
			for _, call := range forbidden {
				if strings.Contains(string(f.Source), call) {
					t.Errorf("%s calls %s; take the event through internal/kernelevent", f.Name, call)
				}
			}
		})
	}
}

func TestEveryEntryPushSendsStampedPush(t *testing.T) {
	sourcescan.InPackage(t, integrationHarnessDir)
	// A push variant that builds its own command can drop a stamp the others
	// carry; only the op constants may spell the two entry ops.
	src, err := os.ReadFile("shimctl.go")
	if err != nil {
		t.Fatal(err)
	}
	for _, op := range []string{`"push_agent_frame"`, `"push_user_prompt"`} {
		if n := strings.Count(string(src), op); n != 1 {
			t.Fatalf("shimctl.go spells %s %d times, want once, in its op constant", op, n)
		}
	}
}

// TestEveryDaemonTestHolderIsHoldtests pins the one test holder: the daemon's tests stand a holder up only through
// holdtest.Start, never by serving sockethold.Serve themselves.
func TestEveryDaemonTestHolderIsHoldtests(t *testing.T) {
	sourcescan.InPackage(t, integrationHarnessDir)
	tests := []struct {
		name string
		dir  string
	}{
		{name: "the integration harness", dir: "."},
		{name: "the command's tests", dir: "../../cmd/agent-repld"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			var served []string

			// Act
			sourcescan.EachSourceIn(t, tt.dir, true, func(fset *token.FileSet, f *ast.File) {
				ast.Inspect(f, func(n ast.Node) bool {
					if sourcescan.IsSelector(n, "sockethold", "Serve") {
						served = append(served, fset.Position(n.Pos()).String())
					}
					return true
				})
			})

			// Assert
			if len(served) > 0 {
				t.Fatalf("sockethold.Serve called directly at %v; stand a test holder up with holdtest.Start", served)
			}
		})
	}
}

// handRolledSuccessor is a StartDaemon option copying another daemon's
// profile directory: the successor shape SuccessorOpts owns.
var handRolledSuccessor = regexp.MustCompile(`ProfileDir:\s+[\w.]+\.ProfileDir\b`)

// TestEverySuccessorStartsThroughSuccessorOpts keeps the integration suite's
// successors on the one shape, so a hand-rolled one cannot drift from it (a
// successor on a fresh account root resumes nothing).
func TestEverySuccessorStartsThroughSuccessorOpts(t *testing.T) {
	sourcescan.InPackage(t, integrationHarnessDir)
	// Arrange
	files, err := filepath.Glob(filepath.Join("..", "*.go"))
	if err != nil || len(files) == 0 {
		t.Fatalf("glob the integration suite: %v (%d files)", err, len(files))
	}

	// Act, Assert
	for _, file := range files {
		src, err := os.ReadFile(file)
		if err != nil {
			t.Fatalf("read %s: %v", file, err)
		}
		if loc := handRolledSuccessor.FindIndex(src); loc != nil {
			t.Errorf("%s hand-rolls a successor (%q); start it with SuccessorOpts", file, src[loc[0]:loc[1]])
		}
	}
}

// TestEveryTurnEndedWaitNamesOpTurnEnded pins that the integration suite waits
// on a turn's durable close through OpTurnEnded alone: a test that spelled the
// operation by hand would keep passing on a stale spelling after a rename.
func TestEveryTurnEndedWaitNamesOpTurnEnded(t *testing.T) {
	sourcescan.InPackage(t, integrationHarnessDir)
	// Arrange
	opTurnEnded := harnessStrings(t, "OpTurnEnded")[0]
	files, err := filepath.Glob(filepath.Join("..", "*_test.go"))
	if err != nil || len(files) == 0 {
		t.Fatalf("glob the integration tests = %v, %v, want files", files, err)
	}

	// Act
	var spelled []string
	for _, f := range files {
		body, err := os.ReadFile(f)
		if err != nil {
			t.Fatalf("read %s: %v", f, err)
		}
		if strings.Contains(string(body), fmt.Sprintf("%q", opTurnEnded)) {
			spelled = append(spelled, f)
		}
	}

	// Assert
	if len(spelled) != 0 {
		t.Fatalf("integration tests spell %q by hand instead of harness.OpTurnEnded: %v", opTurnEnded, spelled)
	}
}

// TestNoIntegrationTestSpellsOutTheWorkspaceRestartWarnings covers the call
// sites: a test that declares the whole restart set by hand would drift from
// WorkspaceRestartWarnings unseen, so it must call
// ExpectWorkspaceRestartWarnings instead.
func TestNoIntegrationTestSpellsOutTheWorkspaceRestartWarnings(t *testing.T) {
	sourcescan.InPackage(t, integrationHarnessDir)
	// Arrange
	files, err := filepath.Glob(filepath.Join("..", "*_test.go"))
	if err != nil || len(files) == 0 {
		t.Fatalf("glob the integration tests = (%v, %v), want files", files, err)
	}
	call := warningDeclaration
	restartWarnings := harnessStrings(t, "WorkspaceRestartWarnings")

	for _, file := range files {
		raw, err := os.ReadFile(file)
		if err != nil {
			t.Fatalf("read %s: %v", file, err)
		}

		// Act
		for _, match := range call.FindAllStringSubmatch(string(raw), -1) {
			// Assert
			spelled := 0
			declared := match[1] + match[2]
			for _, op := range restartWarnings {
				if strings.Contains(declared, `"`+op+`"`) {
					spelled++
				}
			}
			if spelled == len(restartWarnings) {
				t.Fatalf("%s declares the whole workspace-restart set by hand; call ExpectWorkspaceRestartWarnings:\n%s", file, match[0])
			}
		}
	}
}

// warningDeclaration matches one warning declaration in a test's source: a
// call, or an Opts.ExpectWarnings literal that declares a boot's warnings
// before the process starts. Its first or second group is what it declares.
var warningDeclaration = regexp.MustCompile(`(?s)ExpectWarnings(?:For)?(?:\((.*?)\)\n|: \[\]string\{(.*?)\})`)

func TestWarningDeclarationReadsEveryDeclarationForm(t *testing.T) {
	sourcescan.InPackage(t, integrationHarnessDir)
	cases := []struct {
		name, src, want string
	}{
		{name: "a call", src: "d.ExpectWarnings(\"a.b\", \"c.d\")\n", want: `"a.b", "c.d"`},
		{name: "a per-workspace call", src: "d.ExpectWarningsFor(ws, \"a.b\")\n", want: `ws, "a.b"`},
		{name: "an Opts literal", src: "harness.Opts{ExpectWarnings: []string{\"a.b\"}, StateDir: dir}", want: `"a.b"`},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			m := warningDeclaration.FindStringSubmatch(tc.src)

			// Assert
			if m == nil || m[1]+m[2] != tc.want {
				t.Fatalf("warningDeclaration on %q = %q, want %q", tc.src, m, tc.want)
			}
		})
	}
}

// harnessStrings reads the string value of the integration harness's
// constant NAME, or the strings of its []string variable NAME, off its
// production sources: the rules need the harness's spellings, and importing
// the harness would link most of the daemon into the rule suite.
func harnessStrings(t *testing.T, name string) []string {
	t.Helper()
	for _, d := range sourcescan.StringDecls(t, integrationHarnessDir) {
		if d.Name == name {
			return d.Values
		}
	}
	t.Fatalf("the integration harness declares no %s spelled in string literals", name)
	return nil
}

func TestHarnessStringsReadsAConstantAndAListOfTheHarness(t *testing.T) {
	sourcescan.InPackage(t, integrationHarnessDir)
	// Act
	op := harnessStrings(t, "OpTurnEnded")
	warnings := harnessStrings(t, "WorkspaceRestartWarnings")

	// Assert
	if len(op) != 1 || !strings.HasPrefix(op[0], "daemon.") {
		t.Fatalf("OpTurnEnded = %v, want one daemon operation", op)
	}
	if len(warnings) < 2 || !slices.Contains(warnings, "daemon.rollout.relaunch") {
		t.Fatalf("WorkspaceRestartWarnings = %v, want the restart set", warnings)
	}
}
