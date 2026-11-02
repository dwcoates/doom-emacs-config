package harness

import (
	"context"
	"errors"
	"os"
	"os/exec"
	"strings"
	"syscall"
	"testing"
	"time"

	"agent-repld/integration/grantedclock"
	"agent-repld/internal/sourcescan"
)

func TestProcessEndFaultClassifiesEveryEnd(t *testing.T) {
	cases := []struct {
		name            string
		asked, expected bool
		end             ProcessEnd
		err             error
		wantFault       string
	}{
		{name: "a clean exit of its own accord is no fault", end: ProcessEnd{}},
		{name: "an unasked failing exit is a fault", end: ProcessEnd{Code: 3}, wantFault: "exited unasked, exit code 3"},
		{name: "an unasked kill is a fault", end: ProcessEnd{Signal: syscall.SIGKILL}, wantFault: "exited unasked, killed by killed"},
		{name: "an asked failing exit is no fault", asked: true, end: ProcessEnd{Signal: syscall.SIGKILL}},
		{name: "an expected failing exit is no fault", expected: true, end: ProcessEnd{Code: 1}},
		{name: "an unasked exit too early to watch is a fault", err: errEndUnwatchable, wantFault: "before its exit could be watched"},
		{name: "an asked exit too early to watch is no fault", asked: true, err: errEndUnwatchable},
		{name: "a watch that failed is a fault", err: errors.New("kqueue: boom"), wantFault: "watch the daemon (pid 7)'s exit: kqueue: boom"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newProcessFault()
			if tc.asked {
				f.ask()
			}
			f.failureExpected = tc.expected

			// Act
			got := f.processEndFault(7, tc.end, tc.err, "panic: boom\n")

			// Assert
			switch {
			case tc.wantFault == "" && got != nil:
				t.Fatalf("processEndFault = %v, want no fault", got)
			case tc.wantFault != "" && (got == nil || !strings.Contains(got.Error(), tc.wantFault)):
				t.Fatalf("processEndFault = %v, want a fault naming %q", got, tc.wantFault)
			}
		})
	}
}

func TestStderrTailKeepsTheLastLinesOrSaysItIsEmpty(t *testing.T) {
	long := strings.Repeat("early\n", stderrTailLines) + "the panic\n"
	cases := []struct {
		name, stderr, want, notWant string
	}{
		{name: "an empty stderr says so", stderr: "", want: "its stderr is empty"},
		{name: "a short stderr is kept whole", stderr: "one\ntwo\n", want: "its stderr ends:\none\ntwo"},
		{name: "a long stderr keeps its last lines", stderr: "first\n" + long, want: "the panic", notWant: "first"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := stderrTail(tc.stderr)

			// Assert
			if !strings.Contains(got, tc.want) || tc.notWant != "" && strings.Contains(got, tc.notWant) {
				t.Fatalf("stderrTail = %q, want it to hold %q and not %q", got, tc.want, tc.notWant)
			}
		})
	}
}

// startLatchedChild starts a child that exits with CODE once its stdin
// closes, under a bare Daemon whose process latch watches it, and answers
// the daemon, the release of the child, and the watch's end.
func startLatchedChild(t *testing.T, code string) (*Daemon, func(), <-chan struct{}) {
	t.Helper()
	cmd := exec.Command("/bin/sh", "-c", "read _; echo dying >&2; exit "+code)
	stdin, err := cmd.StdinPipe()
	if err != nil {
		t.Fatalf("stdin: %v", err)
	}
	stderr := t.TempDir() + "/stderr"
	f, err := os.Create(stderr)
	if err != nil {
		t.Fatalf("create stderr: %v", err)
	}
	t.Cleanup(func() { _ = f.Close() })
	cmd.Stderr = f
	if err := cmd.Start(); err != nil {
		t.Fatalf("start: %v", err)
	}
	t.Cleanup(func() { _ = cmd.Process.Kill(); _ = cmd.Wait() })
	d := &Daemon{t: t, daemonProc: newDaemonProc(cmd)}
	d.stderrPath = stderr
	watched := d.watchProcessEnd(t)
	return d, func() { _ = stdin.Close() }, watched
}

func TestTheProcessLatchEndsEveryWaitAtAnUnaskedFailingExit(t *testing.T) {
	// Arrange
	d, release, _ := startLatchedChild(t, "3")
	wait, cancel := grantedclock.WithBound(d.fault.ctx, time.Hour)
	defer cancel()

	// Act
	release()

	// Assert: the wait ends on the exit, long before its bound, naming the
	// exit and the process's stderr.
	<-wait.Done()
	why := grantedclock.Explain(wait)
	if why == nil || !strings.Contains(why.Error(), "exited unasked, exit code 3") || !strings.Contains(why.Error(), "dying") {
		t.Fatalf("Explain = %v, want the unasked exit code 3 with the stderr tail", why)
	}
}

func TestTheProcessLatchStaysOpenForAnExcusedExit(t *testing.T) {
	cases := []struct {
		name  string
		code  string
		setup func(d *Daemon)
	}{
		{name: "an asked failing exit", code: "3", setup: func(d *Daemon) { d.fault.ask() }},
		{name: "an expected failing exit", code: "3", setup: func(d *Daemon) { d.ExpectFailedExit() }},
		{name: "a clean exit", code: "0", setup: func(*Daemon) {}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			d, release, watched := startLatchedChild(t, tc.code)
			tc.setup(d)

			// Act
			release()
			<-watched

			// Assert
			if cause := context.Cause(d.fault.ctx); cause != nil {
				t.Fatalf("the latch closed for an excused exit: %v", cause)
			}
		})
	}
}

func TestRecordFaultFailsExactlyWhatTheSweepFails(t *testing.T) {
	cases := []struct {
		name      string
		declared  []string
		record    LogRecord
		otherPID  bool
		wantFault bool
	}{
		{name: "an undeclared warning", record: warnFor("", "daemon.boom"), wantFault: true},
		{name: "an undeclared error", record: LogRecord{Level: "error", Operation: "daemon.boom", Message: "m"}, wantFault: true},
		{name: "a declared warning", declared: []string{"daemon.boom"}, record: warnFor("", "daemon.boom")},
		{name: "an info record", record: LogRecord{Level: "info", Operation: "daemon.boom", Message: "m"}},
		{name: "another process's warning", record: warnFor("", "daemon.boom"), otherPID: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			d := newOwnerForViews(t, t)
			d.ExpectWarnings(tc.declared...)
			if tc.otherPID {
				process, err := os.FindProcess(viewPID + 1)
				if err != nil {
					t.Fatalf("FindProcess: %v", err)
				}
				d.cmd = &exec.Cmd{Process: process}
			}
			appendRecords(t, d, tc.record)

			// Act
			cause, err := d.recordFault(map[string]int{})

			// Assert
			if err != nil {
				t.Fatalf("recordFault: %v", err)
			}
			if tc.wantFault != (cause != nil) {
				t.Fatalf("recordFault = %v, want a fault: %v", cause, tc.wantFault)
			}
			if tc.wantFault && !strings.Contains(cause.Error(), "daemon.boom") {
				t.Fatalf("recordFault = %v, want it to name daemon.boom", cause)
			}
		})
	}
}

func TestTheRecordLatchEndsItsContextWithTheRecord(t *testing.T) {
	// Arrange
	d := newOwnerForViews(t, t)
	ctx := d.watchRecords(t, context.Background())
	wait, cancel := grantedclock.WithBound(ctx, time.Hour)
	defer cancel()

	// Act
	appendRecords(t, d, warnFor("", "daemon.boom"))

	// Assert
	<-wait.Done()
	if why := grantedclock.Explain(wait); why == nil || !strings.Contains(why.Error(), "WARN daemon.boom") {
		t.Fatalf("Explain = %v, want the WARN daemon.boom record", why)
	}
}

func TestTheRecordLatchReportsARecordOnce(t *testing.T) {
	// Arrange
	d := newOwnerForViews(t, t)
	appendRecords(t, d, warnFor("", "daemon.boom"))
	read := map[string]int{}
	if cause, err := d.recordFault(read); cause == nil || err != nil {
		t.Fatalf("first read = (%v, %v), want the fault", cause, err)
	}

	// Act
	cause, err := d.recordFault(read)

	// Assert
	if cause != nil || err != nil {
		t.Fatalf("second read = (%v, %v), want nothing new", cause, err)
	}
}

func TestTheRecordLatchOfTheOwnerLeavesAViewsClaimedRecords(t *testing.T) {
	// Arrange
	owner := newOwnerForViews(t, t)
	viewTB := NewRecordingTB(t, "TestMine")
	defer viewTB.RunCleanups()
	view := owner.View(viewTB)
	mine := t.TempDir()
	view.Claim(mine)
	appendRecords(t, owner, warnFor(mine, "daemon.mine"))

	// Act
	ownerCause, _ := owner.recordFault(map[string]int{})
	viewCause, _ := view.recordFault(map[string]int{})

	// Assert
	if ownerCause != nil {
		t.Fatalf("the owner's latch = %v, want it to leave the view's claimed record", ownerCause)
	}
	if viewCause == nil || !strings.Contains(viewCause.Error(), "daemon.mine") {
		t.Fatalf("the view's latch = %v, want its claimed record", viewCause)
	}
}

// Every daemonProc is made by newDaemonProc, so none lacks its fault latch.
func TestEveryDaemonProcIsMadeByNewDaemonProc(t *testing.T) {
	// Act
	sites := sourcescan.Sites(t, "&daemonProc{")

	// Assert
	if strings.Join(sites, ",") != "daemon.go" {
		t.Fatalf("&daemonProc{ literals in %v, want only newDaemonProc's in daemon.go", sites)
	}
}

// Every run budget is a child of the record latch, so no test-facing wait
// outlives its daemon's fault.
func TestEveryRunBudgetIsAChildOfTheFaultLatches(t *testing.T) {
	// Act
	sites := sourcescan.Sites(t, "WithBound(d.watchRecords(t, d.fault.ctx)")
	views := sourcescan.Sites(t, "WithBound(v.watchRecords(t, d.fault.ctx)")

	// Assert
	if strings.Join(sites, ",") != "daemon.go" || strings.Join(views, ",") != "view.go" {
		t.Fatalf("run budgets under the latches: owner %v, view %v; want StartDaemon's and View's", sites, views)
	}
}

func TestSweepIsTheSubjectStandsOnlyTheRecordLatchAside(t *testing.T) {
	// Arrange
	d := newOwnerForViews(t, t)
	d.SweepIsTheSubject()
	appendRecords(t, d, warnFor("", "daemon.boom"))

	// Act
	cause, err := d.recordFault(map[string]int{})

	// Assert: the latch stays open, and the sweep still names the record.
	if cause != nil || err != nil {
		t.Fatalf("recordFault = (%v, %v), want the latch to stand aside", cause, err)
	}
	if got := operations(d.UnexpectedWarnings()); len(got) != 1 || got[0] != "daemon.boom" {
		t.Fatalf("UnexpectedWarnings = %v, want the sweep to still name daemon.boom", got)
	}
}
