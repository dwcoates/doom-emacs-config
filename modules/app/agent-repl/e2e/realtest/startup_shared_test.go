//go:build realtest

package realtest

import (
	"bufio"
	"context"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"sort"
	"strconv"
	"strings"
	"syscall"
	"testing"
	"time"
)

// WHAT REALTESTS 2 AND 3 SHARE.
//
// docs/REALTEST-PLAN.md, "Startup and shape", items 2 and 3 are the same
// measurement as item 1 taken under a different precondition: item 2 restarts
// Emacs with the daemon still up and asserts the daemon is ADOPTED, item 3
// starts Emacs with the daemon down and asserts one is SPAWNED. Everything
// between those two preconditions — the phase table, the tab and panel
// assertions, the key self-test, the log harvest — is realtest 1's, and both
// tests call realtest 1's own helpers (coldStart, waitForUsable, waitForShown,
// assertEveryWorkspaceDrawn, assertEveryWorkspacePainted, verifyVendorGuard,
// proveKeyDriver, waitUntil) rather than restating them.
//
// This file holds only the pieces realtest 1 has no use for and therefore does
// not export: quitting a standing editor as the ACT of a restart, finding the
// resident daemon, and reading the startup feedback records that realtest 3
// asserts the user saw during the wait. The run tail — re-enumerate, harvest,
// *Messages*, manifest, verdict — is here too, because realtests 2 and 3 close
// identically and a second spelling of a harvest is how two runs start
// disagreeing about what a clean run is.
//
// REALTEST 1 PREDATES THIS FILE and still carries its own inline copy of that
// tail. Collapsing the two onto finishStartupRun is deliberately NOT done here:
// realtest 1 is certified twice-green (docs/REALTEST-PLAN.md, status table) and
// this author has no authorized run to re-certify it with. Whoever next has a
// run of realtest 1 in hand should collapse them.

// takeoverEnv authorizes quitting a standing Emacs, exactly as it does for
// bin/realtest.sh's second refusal. The script reads it before the run and the
// operator's shell is what sets it, so `go test` inherits the same value the
// script saw: there is one flag, not two.
const takeoverEnv = "AGENT_REPL_REALTEST_TAKEOVER"

// quitCeiling is how long a standing Emacs may take to stop answering its
// socket after `(kill-emacs)`. It matches bin/realtest.sh's own wait (60
// one-second polls) on purpose: the script and the test are waiting for the
// same process to do the same thing, and two different bounds for one event
// would eventually disagree about whether a takeover worked.
const quitCeiling = 60 * time.Second

// requireMeasuredBudgets is realtest 1's budget gate: outside a measurement
// run, a phase with no number behind it fails the run rather than passing it
// silently. budgets.go carries the reasoning.
func requireMeasuredBudgets(t *testing.T, measureOnly bool) {
	t.Helper()
	if measureOnly {
		return
	}
	unmeasured := UnmeasuredBudgets()
	if len(unmeasured) == 0 {
		return
	}
	names := make([]string, 0, len(unmeasured))
	for _, phase := range unmeasured {
		names = append(names, string(phase))
	}
	t.Fatalf("these phases have no measured budget yet: %s.\n"+
		"Set them in e2e/realtest/budgets.go from an observed measurement, "+
		"or run with %s=1 to take that measurement. A run must not report green on a gate with no number in it.",
		strings.Join(names, ", "), measureEnv)
}

// startupRunDir resolves the directory a startup realtest writes its manifest,
// probe answers and compiled key helper into.
//
// AGENT_REPL_REALTEST_OUT wins when bin/realtest.sh set it, so the script's
// readiness.json and backups.txt sit beside the run's own artifacts; slug is
// only used for the fallback name, which is what a run started by hand gets.
func startupRunDir(t *testing.T, home, slug string) string {
	t.Helper()
	dir := os.Getenv(outEnv)
	if dir == "" {
		dir = filepath.Join(home, ".claude-emacs", "realtest",
			fmt.Sprintf("%s-%s", slug, time.Now().Format("20060102-150405")))
	}
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("create the run directory %s: %v", dir, err)
	}
	return dir
}

// quitStandingEmacs quits the editor that is answering, and reports whether
// there was one.
//
// THIS IS THE ONE WRITE REALTEST 2 PERFORMS, and it is the test's ACT rather
// than its setup: the plan's item 2 is "quit and restart Emacs", so the quit
// belongs inside the run window where its records are harvested like any
// other. It is `(kill-emacs)` through the existing Client.Kill — deliberately
// not `save-buffers-kill-emacs`, which prompts, and a prompt on a headless run
// hangs holding the owner's editor open on a modal question nobody will answer
// (emacsclient.go says the same thing at the source).
//
// It refuses without the takeover flag rather than deciding for the owner,
// which is bin/realtest.sh's second refusal restated at the one other place
// that can close the editor. The script's refusal cannot cover this one: the
// script checks before the run, and by the time realtest 2 quits, the Emacs it
// is quitting may be one the run itself is responsible for.
func quitStandingEmacs(ctx context.Context, t *testing.T, client *Client, why string) bool {
	t.Helper()
	if !client.Alive(ctx) {
		t.Logf("no Emacs is answering %s, so there is nothing to quit for %s", client.Socket, why)
		return false
	}
	if os.Getenv(takeoverEnv) != "1" {
		t.Fatalf("an Emacs is answering %s and %s requires quitting it. That is the owner's editor, "+
			"with the owner's unsaved work in it, and this test does not make that decision: set %s=1 to say go ahead. "+
			"bin/realtest.sh takes the state backups before it reads the same flag.",
			client.Socket, why, takeoverEnv)
	}
	idle, err := client.IdleSeconds(ctx)
	if err != nil {
		// Not fatal, and not silent either: how long the editor has been idle
		// is the only thing the editor itself can say about whether a human is
		// in it, and a run that could not ask should say so rather than imply
		// it asked and got "nobody".
		t.Logf("could not read how long the standing Emacs has been idle before quitting it: %v", err)
	} else {
		t.Logf("the standing Emacs has been idle for %.1fs; quitting it for %s", idle, why)
	}
	if err := client.Kill(ctx); err != nil {
		// A server that dies mid-call cannot answer the call that killed it,
		// so a non-zero exit is expected here and is not the verdict. Whether
		// Emacs is gone is decided by the socket poll below.
		t.Logf("emacsclient (kill-emacs) reported: %v", err)
	}
	waitUntil(ctx, t, "the standing Emacs to stop answering its socket", quitCeiling,
		func() bool { return !client.Alive(ctx) })
	if client.Alive(ctx) {
		t.Fatalf("the standing Emacs is still answering %s %s after (kill-emacs); the run stops rather than "+
			"launching a second Emacs onto the same socket", client.Socket, quitCeiling)
	}
	t.Logf("the standing Emacs has exited")
	return true
}

// daemonPattern is what a resident daemon looks like to pgrep. It matches
// bin/realtest.sh's own vendor-guard enumeration and realtest 1's
// verifyVendorGuard, so all three agree on what "the daemon is running" means.
const daemonPattern = "claude-repld"

// daemonPIDs is every resident daemon currently running.
//
// A LIST, not one pid, and every caller here reads the length. Two daemons on
// one machine is not a state any startup realtest can reason about — the
// frontend adopts whichever address file is current, so "the daemon" would be
// ambiguous in exactly the assertion that matters — and a test that quietly
// took the first would report an adoption of a process nobody chose.
//
// An empty list with no error means no daemon is running, which is realtest
// 3's whole precondition and is therefore never an error here.
func daemonPIDs(ctx context.Context) ([]int, error) {
	callCtx, cancel := context.WithTimeout(ctx, 10*time.Second)
	defer cancel()
	out, err := exec.CommandContext(callCtx, "pgrep", "-f", daemonPattern).Output()
	if err != nil {
		// pgrep exits 1 when nothing matched, which is an answer and not a
		// failure. Any other exit is a failure to ask the question at all.
		if exitErr, ok := err.(*exec.ExitError); ok && exitErr.ExitCode() == 1 {
			return nil, nil
		}
		return nil, fmt.Errorf("pgrep -f %q: %w", daemonPattern, err)
	}
	var pids []int
	for _, field := range strings.Fields(string(out)) {
		pid, convErr := strconv.Atoi(field)
		if convErr != nil {
			return nil, fmt.Errorf("pgrep -f %q answered %q, which is not a pid: %w", daemonPattern, field, convErr)
		}
		pids = append(pids, pid)
	}
	sort.Ints(pids)
	return pids, nil
}

// processAlive asks the kernel whether a pid still names a live process.
//
// Signal 0 rather than `ps`: it costs no subprocess, and it is the same
// question. A pid this run recorded before the quit and finds alive after is
// the strongest available evidence that it is the SAME daemon, not a
// replacement that happens to answer.
func processAlive(pid int) bool {
	process, err := os.FindProcess(pid)
	if err != nil {
		return false
	}
	return process.Signal(syscall.Signal(0)) == nil
}

// ---- the startup feedback the user sees while waiting ----------------------
//
// WHAT THE OWNER WATCHES DURING A BRING-UP, and what realtest 3 asserts was
// actually produced. The module reports its own startup in two places, and
// both of them leave a record on the module log because both go through the
// one log function (lisp/core.el, `agent-repl--emit-log-record`):
//
//	THE MODE-LINE SEGMENT. `agent-repl-daemon--set-lifecycle` (lisp/daemon.el)
//	is the single chokepoint the lifecycle state moves through, and it writes
//	`elisp.daemon.lifecycle state=STATE` on every transition before repainting
//	the segment. So the segment's states are assertable from the log without
//	asking Emacs what its mode line says — which would answer only what it says
//	NOW, long after the transition it is being asked about.
//
//	THE MINIBUFFER ECHO. The same chokepoint calls `agent-repl--phase-echo`
//	(lisp/core.el), which produces the record and the echo-area line in ONE
//	call. The record is written whether or not the line is echoed — a phase
//	reached while the user is typing is recorded quietly rather than dropped —
//	so the record is evidence the feedback path ran, and *Messages* (harvested
//	separately, verbatim) is where the line itself is read back.
//
// The echo lines are matched on the record's `message`, which is the formatted
// text: "starting the daemon…", "linking to the daemon…", "daemon ready",
// "loading workspaces (n/m)…".
//
// ONE TRAP, SPELLED OUT BECAUSE IT COSTS A WRONG GREEN. lisp/daemon.el ALSO
// writes a plain info record whose message is "starting the daemon..." with
// three ASCII dots, immediately before the spawn, kept for the log while the
// minibuffer line is issued once by the lifecycle transition. It is a
// different record from the echo, and a pattern that matched both would report
// the user-visible echo as present on a run where only the log line was
// written. Every echo pattern below therefore anchors on the U+2026 HORIZONTAL
// ELLIPSIS the echo table uses, never on ".".

// startupFeedbackMarker is one feedback record a startup realtest asserts on.
type startupFeedbackMarker struct {
	// ID names the marker in failure messages.
	ID string
	// Re matches the record's `message`, for the same reason phases.go
	// matches on message rather than operation: the operation name is derived
	// from the format string and folds the arguments away, and here the
	// arguments ARE the distinction (state=starting versus state=adopted, an
	// ellipsis versus three dots).
	Re *regexp.Regexp
	// What says, in the owner's terms, which feedback this record is evidence
	// of, so a failure names the thing the user did not see rather than the
	// string that was missing.
	What string
}

// startupFeedbackSighting is what a run saw of one marker.
type startupFeedbackSighting struct {
	// First is the earliest matching record at or after the spawn.
	First time.Time
	// Raw is that record's message, verbatim.
	Raw string
	// Count is how many matched in the whole window, reported so a
	// once-per-transition line that fired repeatedly reads as the finding it
	// is rather than as a pass.
	Count int
}

// The markers. Spelled once, here, so realtests 2 and 3 assert the presence
// and the absence of the SAME strings: an adoption asserted with one spelling
// and denied with another is not an assertion about the product.
var (
	// feedbackLifecycleStarting is the mode-line segment entering "daemon:
	// starting…", which happens on the SPAWN path only.
	feedbackLifecycleStarting = startupFeedbackMarker{
		ID:   "lifecycle:starting",
		Re:   regexp.MustCompile(`^elisp\.daemon\.lifecycle state=starting(\s|$)`),
		What: "the mode-line segment reporting the daemon spawn",
	}
	// feedbackLifecycleLinking is the segment entering "daemon: linking…",
	// which BOTH paths pass through: `agent-repl-daemon--report-provenance`
	// sets it the moment an address exists, whether it was adopted or booted.
	feedbackLifecycleLinking = startupFeedbackMarker{
		ID:   "lifecycle:linking",
		Re:   regexp.MustCompile(`^elisp\.daemon\.lifecycle state=linking(\s|$)`),
		What: "the mode-line segment reporting the link being established",
	}
	// feedbackLifecycleAdopted is the segment's ADOPTION outcome: link-up
	// with no daemon process of this Emacs's own.
	feedbackLifecycleAdopted = startupFeedbackMarker{
		ID:   "lifecycle:adopted",
		Re:   regexp.MustCompile(`^elisp\.daemon\.lifecycle state=adopted(\s|$)`),
		What: "the mode-line segment reporting an adopted daemon",
	}
	// feedbackLifecycleReady is the segment's SPAWN outcome: link-up with the
	// daemon this Emacs started still live.
	feedbackLifecycleReady = startupFeedbackMarker{
		ID:   "lifecycle:ready",
		Re:   regexp.MustCompile(`^elisp\.daemon\.lifecycle state=ready(\s|$)`),
		What: "the mode-line segment reporting the daemon this Emacs spawned as ready",
	}
	// feedbackEchoStarting is the minibuffer line for the spawn. Anchored on
	// the ellipsis; see the ASCII-dots trap above.
	feedbackEchoStarting = startupFeedbackMarker{
		ID:   "echo:starting the daemon",
		Re:   regexp.MustCompile(`^starting the daemon\x{2026}$`),
		What: `the minibuffer echo "starting the daemon…"`,
	}
	feedbackEchoLinking = startupFeedbackMarker{
		ID:   "echo:linking to the daemon",
		Re:   regexp.MustCompile(`^linking to the daemon\x{2026}$`),
		What: `the minibuffer echo "linking to the daemon…"`,
	}
	feedbackEchoAdopted = startupFeedbackMarker{
		ID:   "echo:daemon adopted",
		Re:   regexp.MustCompile(`^daemon adopted$`),
		What: `the minibuffer echo "daemon adopted"`,
	}
	feedbackEchoReady = startupFeedbackMarker{
		ID:   "echo:daemon ready",
		Re:   regexp.MustCompile(`^daemon ready$`),
		What: `the minibuffer echo "daemon ready"`,
	}
	// feedbackEchoLoadingWorkspaces is the per-workspace progress line. It is
	// emitted from `agent-repl-daemon-on-open-progress-change` only when the
	// painted count MOVES while workspaces are still opening, which is why
	// realtest 2 and 3 read the feedback AFTER the show phase: a panel does
	// not paint until Emacs is brought forward, so the count that this line
	// reports moves there.
	feedbackEchoLoadingWorkspaces = startupFeedbackMarker{
		ID:   "echo:loading workspaces",
		Re:   regexp.MustCompile(`^loading workspaces \(\d+/\d+\)\x{2026}$`),
		What: `the minibuffer echo "loading workspaces (n/m)…"`,
	}
	// feedbackDaemonStarted and feedbackDaemonAdopted are the two provenance
	// records phases.go reads as PhaseDaemonSpawned. They are restated here so
	// a startup realtest can assert the ABSENCE of the path it forbids, which
	// Phases.DaemonPath cannot express: it records whichever fired FIRST and
	// says nothing about a second one firing after it.
	feedbackDaemonStarted = startupFeedbackMarker{
		ID:   "daemon:started",
		Re:   regexp.MustCompile(`^elisp\.daemon\.started(\s|$)`),
		What: "this Emacs spawning a daemon of its own",
	}
	feedbackDaemonAdopted = startupFeedbackMarker{
		ID:   "daemon:adopted",
		Re:   regexp.MustCompile(`^elisp\.daemon\.adopted(\s|$)`),
		What: "this Emacs adopting a daemon that was already answering",
	}
)

// readStartupFeedback reads the feedback records out of the Emacs log sinks.
//
// It reads the same sinks, from the same snapshot offsets and the same spawn
// cutoff, as ReadPhases — and for the same reasons (phases.go's ReadPhases
// documents both). It is a separate reader rather than more rows in
// phases.go's marker table because these are not PHASES: none of them bounds
// an interval anything is measured over, they are assertions that a user-
// visible thing was produced, and putting them in the phase table would put
// them in the measurement report and in the budget check where neither belongs.
func readStartupFeedback(sources []Source, snap Snapshot, spawnedAt time.Time,
	markers []startupFeedbackMarker) (map[string]startupFeedbackSighting, error) {
	seen := make(map[string]startupFeedbackSighting)
	for _, src := range sources {
		if !isEmacsPhaseSource(src) {
			continue
		}
		reads, _, err := resolveReads(src, snap)
		if err != nil {
			return nil, err
		}
		for _, r := range reads {
			if err := readFeedbackRecords(seen, r.path, r.offset, spawnedAt, markers); err != nil {
				return nil, err
			}
		}
	}
	return seen, nil
}

// readFeedbackRecords reads ONE file from `offset` to end and folds its
// feedback markers into `seen`.
//
// A file that does not exist is not an error, exactly as in readPhaseRecords: a
// workspace whose sink holds no records yet has no file behind its link.
func readFeedbackRecords(seen map[string]startupFeedbackSighting, path string, offset int64,
	spawnedAt time.Time, markers []startupFeedbackMarker) error {
	file, err := os.Open(path)
	if err != nil {
		if os.IsNotExist(err) {
			return nil
		}
		return fmt.Errorf("open the Emacs log %s to read the startup feedback: %w", path, err)
	}
	defer file.Close()
	if offset > 0 {
		if _, err := file.Seek(offset, 0); err != nil {
			return fmt.Errorf("seek %s to the snapshot offset %d: %w", path, offset, err)
		}
	}

	scanner := bufio.NewScanner(file)
	scanner.Buffer(make([]byte, 0, 1<<20), 1<<24)
	for scanner.Scan() {
		text := scanner.Text()
		if strings.TrimSpace(text) == "" {
			continue
		}
		var rec record
		if err := json.Unmarshal([]byte(text), &rec); err != nil {
			// The harvester reports a malformed line in its own right. A line
			// this reader cannot parse simply carries no marker.
			continue
		}
		at, err := time.Parse(time.RFC3339Nano, rec.Timestamp)
		if err != nil || at.Before(spawnedAt) {
			continue
		}
		for _, marker := range markers {
			if !marker.Re.MatchString(rec.Message) {
				continue
			}
			sighting := seen[marker.ID]
			sighting.Count++
			if sighting.First.IsZero() || at.Before(sighting.First) {
				sighting.First = at
				sighting.Raw = rec.Message
			}
			seen[marker.ID] = sighting
			break
		}
	}
	if err := scanner.Err(); err != nil {
		return fmt.Errorf("read the Emacs log %s: %w", path, err)
	}
	return nil
}

// assertFeedbackPresent fails once per marker the run did not produce, and
// logs each one it did with the time it arrived relative to the spawn.
//
// One error per marker rather than one naming all of them: "the user saw no
// mode-line state at all" and "the user saw the states but no minibuffer line"
// are different defects in different files, and a single message naming both
// sends the reader to neither.
func assertFeedbackPresent(t *testing.T, seen map[string]startupFeedbackSighting, spawnedAt time.Time,
	want []startupFeedbackMarker) {
	t.Helper()
	for _, marker := range want {
		sighting, ok := seen[marker.ID]
		if !ok {
			t.Errorf("no record of %s was written inside the run (nothing matching `%s` on any Emacs sink "+
				"at or after the spawn), so the user got no such feedback while the startup ran",
				marker.What, marker.Re)
			continue
		}
		t.Logf("  feedback %-28s %s from spawn (%d record(s)): %s",
			marker.ID, sighting.First.Sub(spawnedAt).Round(time.Millisecond), sighting.Count, sighting.Raw)
	}
}

// assertFeedbackAbsent fails once per marker the run produced and must not
// have. This is how a startup realtest denies the path it forbids: realtest 2
// forbids the spawn records, realtest 3 forbids the adoption records, and a
// test that only asserted its own path would pass on a run that took both.
func assertFeedbackAbsent(t *testing.T, seen map[string]startupFeedbackSighting, unwanted []startupFeedbackMarker) {
	t.Helper()
	for _, marker := range unwanted {
		sighting, ok := seen[marker.ID]
		if !ok {
			continue
		}
		t.Errorf("%d record(s) of %s were written inside the run, which this realtest's precondition forbids. "+
			"The first was: %s", sighting.Count, marker.What, sighting.Raw)
	}
}

// logMeasurements prints one phase table at the site, which is what lets a
// budget be sized from the run's own output rather than from a file somebody
// has to go find.
func logMeasurements(t *testing.T, heading string, measurements []Measurement) {
	t.Helper()
	t.Logf("%s", heading)
	for _, m := range measurements {
		if m.Note != "" {
			t.Logf("  phase %-14s %-14s NOT OBSERVED: %s", m.Phase, m.Workspace, m.Note)
			continue
		}
		t.Logf("  phase %-14s %-14s %s from spawn", m.Phase, m.Workspace, m.Elapsed.Round(time.Millisecond))
	}
}

// finishStartupRun closes the window, harvests every log, folds in *Messages*,
// writes the manifest, and turns the budgets and the findings into the run's
// verdict.
//
// THE HARVEST IS THE REMEDIATION BAR, not the timings: every WARN and ERROR
// written inside the run window across every log, with no allowlist, and the
// run fails when the count is non-zero (docs/REALTEST-PLAN.md, "The loop").
// The sources are re-enumerated first so rotation siblings and workspace sinks
// the run itself created are read.
func finishStartupRun(ctx context.Context, t *testing.T, client *Client, env Env, snapshot Snapshot,
	openWorkspaces []Workspace, runDir string, manifest *Manifest, measureOnly bool) {
	t.Helper()

	manifest.Ended = time.Now()
	sources, err := EnumerateSources(env)
	if err != nil {
		t.Fatalf("re-enumerate the logs after the run: %v", err)
	}
	harvest, err := HarvestSources(sources, snapshot, Window{Start: manifest.Started, End: manifest.Ended}, openWorkspaces)
	if err != nil {
		t.Fatalf("harvest the logs: %v", err)
	}
	manifest.Findings = harvest.Findings
	manifest.InfoCounts = harvest.InfoCounts

	messages, msgErr := client.Messages(ctx)
	if msgErr != nil {
		// A *Messages* buffer that cannot be read is itself a finding: it is
		// one of the sources the remediation bar covers, and a run that
		// silently skipped it would be claiming a clean harvest it did not
		// perform.
		manifest.Findings = append(manifest.Findings, Finding{
			Kind:      KindMalformed,
			Source:    "*Messages*",
			Path:      "(emacs buffer)",
			Workspace: GlobalWorkspace,
			Note:      fmt.Sprintf("Emacs's *Messages* buffer could not be read, so this source was not harvested: %v", msgErr),
		})
	} else {
		// The whole buffer IS the window: this run started the process.
		manifest.Findings = append(manifest.Findings, HarvestMessages(messages, 0, openWorkspaces)...)
		if err := os.WriteFile(filepath.Join(runDir, "Messages.txt"), []byte(messages), 0o644); err != nil {
			t.Fatalf("preserve the *Messages* buffer: %v", err)
		}
	}
	sortFindings(manifest.Findings)

	path, err := manifest.Write(runDir)
	if err != nil {
		t.Fatalf("write the run manifest: %v", err)
	}
	t.Logf("manifest: %s", path)

	if measureOnly {
		t.Logf("phase budgets: NOT ENFORCED (this is a measurement run; %s=1)", measureEnv)
		for _, breach := range manifest.BudgetBreaches {
			t.Logf("  would have breached: %s", breach)
		}
	} else if len(manifest.BudgetBreaches) > 0 {
		for _, breach := range manifest.BudgetBreaches {
			t.Errorf("%s", breach)
		}
	}

	if len(manifest.Findings) > 0 {
		t.Errorf("the log harvest found %d warning(s), error(s) or non-record(s) inside the run window; "+
			"every one is in %s, verbatim. There is no allowlist: nothing here is fixed by this test, "+
			"and the owner rules on each one (docs/REALTEST-PLAN.md).",
			len(manifest.Findings), path)
		for _, finding := range manifest.Findings {
			t.Logf("  [%s] %s %s (%s) %s | %s", finding.Kind, finding.Workspace, finding.Source,
				finding.Level, finding.Note, finding.Raw)
		}
	}
}
