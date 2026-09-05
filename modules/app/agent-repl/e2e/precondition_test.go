package e2e

import (
	"fmt"
	"os"
	"sort"
	"strings"
	"sync"
	"testing"
)

// ---------------------------------------------------------------------------
// A SKIP IS NOT A PASS.
//
// This suite exists to cover a cross-system stack, so a run that covered
// nothing must never read as a green run. The webapp layer already states the
// same rule for itself ("A run that found no daemon must never look like a
// pass"), and this file is where the Go side of the suite holds it.
//
// Two DIFFERENT kinds of unmet precondition exist here, and they are treated
// differently on purpose:
//
//  1. A DEPENDENCY precondition — something the reader of this repo installs
//     with one named command in one named directory (`node`, `npm`, the
//     shim's node_modules, the webapp's node_modules). The harness still
//     INSTALLS NOTHING: a missing dependency names the exact command and
//     directory instead of running it. But it now FAILS the run rather than
//     skipping it, because the default reader of a green summary line is
//     someone trusting that line. Route these through requireDependency.
//
//  2. An ENVIRONMENT gate — a layer that can only run in a place this process
//     is not, and that no command run from here could supply (the Emacs
//     client layer needs the e2e sandbox container; a host run is not a
//     broken checkout, it is the wrong place). Those stay skips, routed
//     through noteEnvironmentSkip so they land in the end-of-run summary.
//
// OPT-OUT: set
//
//	AGENT_REPL_E2E_ALLOW_MISSING_DEPS=1
//
// to turn kind (1) back into a skip, in the style of this suite's other
// escape hatches (AGENT_REPL_SANDBOX_NO_GATE and friends). It is for someone
// poking at a single test locally who genuinely does not want to install the
// dependency; it is never for CI, and a run that used it says so loudly in
// the summary block below.
//
// Either way, EVERY skip this suite takes is recorded and reprinted in one
// loud block after the last test, so the output alone answers whether the run
// exercised what it claims.
// ---------------------------------------------------------------------------

// allowMissingDepsEnv turns an absent dependency precondition back into a
// skip instead of a failure. Set it to "1".
const allowMissingDepsEnv = "AGENT_REPL_E2E_ALLOW_MISSING_DEPS"

// allowMissingDeps reports whether the opt-out is engaged.
func allowMissingDeps() bool {
	return os.Getenv(allowMissingDepsEnv) == "1"
}

// skipKind distinguishes the two entries the summary block prints.
type skipKind int

const (
	// skipDependency is an installable dependency that was absent while the
	// opt-out was engaged. Without the opt-out this is a failure, not a skip.
	skipDependency skipKind = iota
	// skipEnvironment is a place this process is not, which no command run
	// from here could supply.
	skipEnvironment
)

// skipRecord is one test that did not run, and why.
type skipRecord struct {
	kind   skipKind
	test   string
	reason string
}

var (
	skipMu      sync.Mutex
	skipRecords []skipRecord
)

// recordSkip files one unmet precondition for the end-of-run summary.
func recordSkip(kind skipKind, test, reason string) {
	skipMu.Lock()
	defer skipMu.Unlock()
	skipRecords = append(skipRecords, skipRecord{kind: kind, test: test, reason: reason})
}

// requireDependency reports an absent INSTALLABLE dependency. reason must
// name the exact command and directory that supply it, because that message
// is the whole value of catching this here.
//
// It FAILS the calling test by default — a suite that covered nothing must
// not report a pass — and skips it only under the allowMissingDepsEnv
// opt-out, in which case the skip is recorded for the summary block.
func requireDependency(t *testing.T, format string, args ...any) {
	t.Helper()
	reason := fmt.Sprintf(format, args...)
	if allowMissingDeps() {
		recordSkip(skipDependency, t.Name(), reason)
		t.Skipf("%s [skipped only because %s=1]", reason, allowMissingDepsEnv)
	}
	t.Fatalf("%s\n\n"+
		"This is a FAILURE, not a skip: this suite's whole purpose is cross-system coverage, "+
		"and a run that covered nothing must never look like a pass. "+
		"Install the dependency named above, or set %s=1 to skip this test deliberately.",
		reason, allowMissingDepsEnv)
}

// noteEnvironmentSkip records and takes a legitimate ENVIRONMENT skip: a
// layer that can only run somewhere this process is not. Unlike a dependency,
// no command run from here supplies it, so this stays a skip — but it is
// reprinted in the end-of-run summary so it cannot hide behind a green line.
func noteEnvironmentSkip(t *testing.T, format string, args ...any) {
	t.Helper()
	reason := fmt.Sprintf(format, args...)
	recordSkip(skipEnvironment, t.Name(), reason)
	t.Skip(reason)
}

// reportSkipSummary prints the loud end-of-run block naming every test that
// did not run and why. Called by runSuite after the last test has reported,
// so it is the last thing before the package's own PASS/FAIL line.
func reportSkipSummary() {
	skipMu.Lock()
	records := append([]skipRecord(nil), skipRecords...)
	skipMu.Unlock()
	if len(records) == 0 {
		return
	}
	sort.SliceStable(records, func(i, j int) bool {
		if records[i].kind != records[j].kind {
			return records[i].kind < records[j].kind
		}
		return records[i].test < records[j].test
	})

	var b strings.Builder
	b.WriteString("\n")
	b.WriteString("================================================================\n")
	b.WriteString(fmt.Sprintf("e2e: THIS RUN DID NOT EXERCISE EVERYTHING — %d test(s) skipped\n", len(records)))
	b.WriteString("================================================================\n")
	for _, r := range records {
		label := "ENVIRONMENT"
		if r.kind == skipDependency {
			label = "MISSING DEPENDENCY"
		}
		b.WriteString(fmt.Sprintf("  [%s] %s\n      %s\n", label, r.test,
			strings.ReplaceAll(r.reason, "\n", "\n      ")))
	}
	if allowMissingDeps() {
		b.WriteString(fmt.Sprintf(
			"\n  %s=1 was set: absent dependencies were DOWNGRADED from failures to skips.\n"+
				"  A green line from this run does not mean those layers passed.\n", allowMissingDepsEnv))
	}
	b.WriteString("================================================================\n")
	writeWhereItCannotBeMissed(b.String())
}

// writeWhereItCannotBeMissed prints the summary somewhere a reader of a GREEN
// run will actually see it.
//
// `go test` pipes a test binary's stderr and prints it only when the package
// fails or `-v` is passed — which is exactly the case this block exists for: a
// run that skipped everything and still printed `ok`. So the controlling
// terminal is the primary channel when there is one, and stderr is the
// fallback for a run with no tty (CI, a redirect), where the captured log is
// the only place left to put it.
func writeWhereItCannotBeMissed(msg string) {
	if tty, err := os.OpenFile("/dev/tty", os.O_WRONLY, 0); err == nil {
		defer tty.Close()
		if _, err := fmt.Fprint(tty, msg); err == nil {
			return
		}
	}
	fmt.Fprint(os.Stderr, msg)
}
