package harness

import (
	"agent-repld/internal/kernelevent"
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
)

// A WAIT LEARNS OF ITS DAEMON'S FAILURE THE MOMENT IT HAPPENS, NOT WHEN ITS
// BOUND RUNS OUT (the agent-repl-tests skill, "Synchronization: events, never
// time"). Every wait this harness makes is a child of the daemon's run budget
// (Daemon.ctx), and the run budget is a child of two FAULT LATCHES:
//
//   - the PROCESS latch (processFault), shared by the owner and every view,
//     closes when the daemon exits without the harness asking it to and not
//     cleanly (a crash, a panic, a refused boot): no wait on that process can
//     be satisfied any more;
//   - each sweeper's RECORD latch (watchRecords) closes when the daemon writes
//     a WARN or ERROR record that sweeper answers for and its test did not
//     declare (ExpectWarnings): the test would fail at its cleanup sweep
//     anyway, and the record is the failure's cause.
//
// A closed latch cancels the context with its cause, so every wait ends at
// once and grantedclock.Explain names the daemon's own record or exit rather
// than "never satisfied within 5s". The bounds stay as the fallback for a
// daemon that hangs without writing anything.

// processFault is the process latch.
type processFault struct {
	ctx    context.Context
	cancel context.CancelCauseFunc

	mu sync.Mutex
	// asked reports that the harness signalled the process (Stop, Kill, a
	// goroutine dump): whatever exit follows is the one asked for.
	asked bool
	// failureExpected reports that the test means the process to fail
	// (Opts.ExpectEarlyExit, ExpectFailedExit).
	failureExpected bool
}

func newProcessFault() *processFault {
	ctx, cancel := context.WithCancelCause(context.Background())
	return &processFault{ctx: ctx, cancel: cancel}
}

// ask records that the harness is about to signal the process.
func (f *processFault) ask() {
	f.mu.Lock()
	f.asked = true
	f.mu.Unlock()
}

// excuses reports whether END is no fault: the harness asked for the exit,
// the test expected a failure, or the process left cleanly of its own accord
// (a drain, a handover, a DaemonStop).
func (f *processFault) excuses(end ProcessEnd) bool {
	f.mu.Lock()
	defer f.mu.Unlock()
	return f.asked || f.failureExpected || end.Clean()
}

// processEndFault answers the latch's cause for a watched end, or nil when
// the end is excused. err is the watch's own failure, end the process's end.
func (f *processFault) processEndFault(pid int, end ProcessEnd, err error, stderr string) error {
	switch {
	case errors.Is(err, errEndUnwatchable):
		f.mu.Lock()
		excused := f.asked || f.failureExpected
		f.mu.Unlock()
		if excused {
			return nil
		}
		return fmt.Errorf("the daemon (pid %d) had exited before its exit could be watched, unasked%s", pid, stderrTail(stderr))
	case err != nil:
		return fmt.Errorf("harness: watch the daemon (pid %d)'s exit: %w", pid, err)
	case f.excuses(end):
		return nil
	}
	return fmt.Errorf("the daemon (pid %d) exited unasked, %v%s", pid, end, stderrTail(stderr))
}

// stderrTailLines is how much of a faulted daemon's stderr its cause carries:
// a panic's first goroutine fits in it.
const stderrTailLines = 40

func stderrTail(stderr string) string {
	lines := strings.Split(strings.TrimRight(stderr, "\n"), "\n")
	if len(lines) == 1 && lines[0] == "" {
		return "; its stderr is empty"
	}
	if len(lines) > stderrTailLines {
		lines = lines[len(lines)-stderrTailLines:]
	}
	return "; its stderr ends:\n" + strings.Join(lines, "\n")
}

// watchProcessEnd closes d's process latch when the process ends unexcused.
// It runs until the process ends or t does, and answers a channel closed
// once it has stopped.
func (d *Daemon) watchProcessEnd(t testing.TB) <-chan struct{} {
	f, pid, stderrPath := d.fault, d.cmd.Process.Pid, d.stderrPath
	stop, cancel := context.WithCancel(context.Background())
	done := make(chan struct{})
	go func() {
		defer close(done)
		end, err := AwaitProcessEnd(stop, pid)
		if stop.Err() != nil {
			return
		}
		stderr, _ := os.ReadFile(stderrPath)
		if cause := f.processEndFault(pid, end, err, string(stderr)); cause != nil {
			t.Logf("harness: fault latch closed: %v", cause)
			f.cancel(cause)
		}
	}()
	t.Cleanup(func() {
		cancel()
		<-done
	})
	return done
}

// ExpectFailedExit declares that the test means this daemon to exit failing
// (a refused boot, a fatal record) on its own, so that exit fails no wait.
func (d *Daemon) ExpectFailedExit() {
	d.fault.mu.Lock()
	d.fault.failureExpected = true
	d.fault.mu.Unlock()
}

// watchRecords answers a child of PARENT that ends, with the record as its
// cause, once this sweeper's daemon writes a WARN or ERROR record that d
// answers for (sweptRecords) and that d's declarations do not license. It
// reads only the records appended since its last read, and stops when t does.
func (d *Daemon) watchRecords(t testing.TB, parent context.Context) context.Context {
	ctx, cancelCause := context.WithCancelCause(parent)
	stop := make(chan struct{})
	done := make(chan struct{})
	// THE LATCH READS AT EVERY CHANGE THE KERNEL REPORTS to the run log or a
	// workspace target, never on a timer: a record that fails the sweep closes
	// the latch the moment it is written. The watch is re-taken each round,
	// so a target made since is followed, and each is taken before its read.
	watched, endWatch := context.WithCancel(ctx)
	go func() {
		<-stop
		endWatch()
	}()
	logs := filepath.Join(d.StateDir, "logs")
	go func() {
		defer close(done)
		read := map[string]int{}
		for {
			targets, _ := filepath.Glob(filepath.Join(logs, "agent-repl-*-daemon-*.log"))
			changes, stopFollowing := kernelevent.Follow(watched, append([]string{filepath.Dir(logs), logs, d.RunLogPath()}, targets...)...)
			cause, err := d.recordFault(read)
			if err != nil {
				cause = fmt.Errorf("harness: read the daemon's logs for faults: %w", err)
			}
			if cause != nil {
				stopFollowing()
				t.Logf("harness: fault latch closed: %v", cause)
				cancelCause(cause)
				return
			}
			select {
			case <-watched.Done():
				stopFollowing()
				return
			case err := <-changes:
				stopFollowing()
				if err != nil && watched.Err() == nil {
					cause := fmt.Errorf("harness: watch the daemon's logs for faults: %w", err)
					t.Logf("harness: fault latch closed: %v", cause)
					cancelCause(cause)
					return
				}
			}
		}
	}()
	t.Cleanup(func() {
		close(stop)
		<-done
		cancelCause(context.Canceled)
	})
	return ctx
}

// recordFault reads every log the sweep reads, from where READ says each was
// last read, and answers the first new record that fails the sweep.
func (d *Daemon) recordFault(read map[string]int) (error, error) {
	if d.cmd == nil || d.cmd.Process == nil {
		return nil, nil
	}
	targets, err := filepath.Glob(filepath.Join(d.StateDir, "logs", "agent-repl-*-daemon-*.log"))
	if err != nil {
		return nil, err
	}
	pid := d.PID()
	for _, path := range append([]string{d.RunLogPath()}, targets...) {
		records, err := readLogRecords(d.t, path)
		if err != nil {
			return nil, err
		}
		from := read[path]
		if from > len(records) {
			// The file was replaced or truncated and read afresh.
			from = 0
		}
		read[path] = len(records)
		for _, r := range records[from:] {
			if r.PID != pid || !warningLevels[strings.ToLower(r.Level)] {
				continue
			}
			if d.failsSweep(r) {
				return fmt.Errorf("the daemon wrote %s %s: %s%s, which no ExpectWarnings declared", strings.ToUpper(r.Level), r.Operation, r.Message, contextSuffix(r)), nil
			}
		}
	}
	return nil, nil
}

// failsSweep reports whether r is a record d's sweep answers for and its
// declarations do not license: exactly what assertNoUnexpectedWarnings
// would fail on.
func (d *Daemon) failsSweep(r LogRecord) bool {
	d.mu.Lock()
	defer d.mu.Unlock()
	if d.sweepIsTheSubject {
		return false
	}
	_, claimed := d.claimed[r.WorkspaceDir]
	if d.view && !d.claims[r.WorkspaceDir] || !d.view && claimed {
		return false
	}
	return !d.expects(r)
}

// SweepIsTheSubject stands this sweeper's record latch aside, for a test
// whose subject is the warnings it reads back undeclared (UnexpectedWarnings,
// a golden's regeneration): its waits must not end on the very records it
// reads. The cleanup sweep still fails every record the test leaves
// undeclared.
func (d *Daemon) SweepIsTheSubject() {
	d.mu.Lock()
	d.sweepIsTheSubject = true
	d.mu.Unlock()
}
