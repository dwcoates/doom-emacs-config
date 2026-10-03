package rollout

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"slices"
	"strconv"
	"strings"
	"sync/atomic"
	"syscall"
	"testing"
	"time"
)

func TestReportJoiningAddrRoundTripsTheAddress(t *testing.T) {
	// Arrange
	state := t.TempDir()

	// Act
	if err := ReportJoiningAddr(state, "127.0.0.1:7788"); err != nil {
		t.Fatalf("ReportJoiningAddr: %v", err)
	}
	got, reported, err := ReadJoiningAddr(state)

	// Assert
	if err != nil {
		t.Fatalf("ReadJoiningAddr: %v", err)
	}
	if !reported || got != "127.0.0.1:7788" {
		t.Fatalf("address = %q reported = %v, want the address back", got, reported)
	}
}

func TestReportJoiningAddrRefusesABlankAddress(t *testing.T) {
	// Arrange
	state := t.TempDir()

	// Act
	err := ReportJoiningAddr(state, "   ")

	// Assert
	if err == nil {
		t.Fatalf("ReportJoiningAddr accepted a blank address")
	}
}

func TestReadJoiningAddrReportsAbsenceWhileTheSuccessorIsStillBinding(t *testing.T) {
	// Arrange
	state := t.TempDir()

	// Act
	_, reported, err := ReadJoiningAddr(state)

	// Assert
	if err != nil {
		t.Fatalf("ReadJoiningAddr: %v", err)
	}
	if reported {
		t.Fatalf("reported = true with no report written")
	}
}

func TestReadJoiningAddrTreatsAnEmptyReportAsAbsence(t *testing.T) {
	// Arrange
	state := t.TempDir()
	if err := os.WriteFile(JoiningAddrPath(state), []byte("\n"), 0o644); err != nil {
		t.Fatalf("write the report: %v", err)
	}

	// Act
	_, reported, err := ReadJoiningAddr(state)

	// Assert
	if err != nil {
		t.Fatalf("ReadJoiningAddr: %v", err)
	}
	if reported {
		t.Fatalf("reported = true for an empty report; a blank address would be dialed as a real one")
	}
}

func TestJoiningAddrPathNamesTheOneReportFile(t *testing.T) {
	// Arrange
	state := "/tmp/state"

	// Act
	got := JoiningAddrPath(state)

	// Assert
	if got != filepath.Join(state, JoiningAddrFile) {
		t.Fatalf("path = %q, want %q", got, filepath.Join(state, JoiningAddrFile))
	}
}

func TestSpawnRefusesWithNoDaemonBinary(t *testing.T) {
	// Arrange
	spawner := NewProcessSpawner("", t.TempDir(), nil)

	// Act
	_, err := spawner.Spawn(context.Background(), "127.0.0.1:7777")

	// Assert
	if err == nil {
		t.Fatalf("Spawn accepted an empty binary path")
	}
}

func TestSpawnRefusesWithNoIncumbentAddress(t *testing.T) {
	// Arrange
	spawner := NewProcessSpawner("/bin/true", t.TempDir(), nil)

	// Act
	_, err := spawner.Spawn(context.Background(), "")

	// Assert
	if err == nil {
		t.Fatalf("Spawn accepted a blank incumbent address; the successor is told, never left to infer")
	}
}

func TestSpawnAnswersTheAddressTheSuccessorReports(t *testing.T) {
	// Arrange
	state := t.TempDir()
	// A stand-in for the daemon binary that reports an address and exits: the
	// real one is not built in a unit test, and what is under test is the
	// report's round trip, not the daemon.
	script := filepath.Join(state, "successor.sh")
	body := "#!/bin/sh\nprintf '127.0.0.1:7788\\n' > " + JoiningAddrPath(state) + ".tmp\n" +
		"mv " + JoiningAddrPath(state) + ".tmp " + JoiningAddrPath(state) + "\n"
	if err := os.WriteFile(script, []byte(body), 0o755); err != nil {
		t.Fatalf("write the stand-in: %v", err)
	}
	spawner := NewProcessSpawner(script, state, nil)
	spawner.Poll = time.Millisecond
	spawner.Timeout = 10 * time.Second

	// Act
	got, err := spawner.Spawn(context.Background(), "127.0.0.1:7777")

	// Assert
	if err != nil {
		t.Fatalf("Spawn: %v", err)
	}
	if got.Address() != "127.0.0.1:7788" {
		t.Fatalf("address = %q, want the successor's report", got.Address())
	}
}

func TestSpawnLeavesTheSuccessorRunningWhenTheIncumbentsContextEnds(t *testing.T) {
	// Arrange: a stand-in successor that reports its address and then keeps
	// running, exactly as the real daemon does once its listener is bound.
	// The context is the INCUMBENT'S serving lifetime, which the handover
	// ends -- binding the child to it killed the successor Emacs had already
	// been handed.
	state := t.TempDir()
	alive := filepath.Join(state, "alive")
	pidFile := filepath.Join(state, "successor.pid")
	script := filepath.Join(state, "successor.sh")
	body := "#!/bin/sh\n" +
		"printf '%s' \"$$\" > " + pidFile + "\n" +
		"printf '127.0.0.1:7788\\n' > " + JoiningAddrPath(state) + ".tmp\n" +
		"mv " + JoiningAddrPath(state) + ".tmp " + JoiningAddrPath(state) + "\n" +
		"trap 'exit 0' TERM\n" +
		"i=0\n" +
		"while [ $i -lt 200 ]; do printf 'x' >> " + alive + "; i=$((i+1)); sleep 0.01; done\n"
	if err := os.WriteFile(script, []byte(body), 0o755); err != nil {
		t.Fatalf("write the stand-in: %v", err)
	}
	spawner := NewProcessSpawner(script, state, nil)
	spawner.Poll = time.Millisecond
	spawner.Timeout = 10 * time.Second
	ctx, cancel := context.WithCancel(context.Background())

	// Act: the handover completes, then the incumbent's lifetime ends.
	if _, err := spawner.Spawn(ctx, "127.0.0.1:7777"); err != nil {
		t.Fatalf("Spawn: %v", err)
	}
	reapSuccessor(t, pidFile)
	before := spawnAliveLen(t, alive)
	cancel()

	// Assert: the successor is still writing after the cancellation. Polling
	// for GROWTH is the liveness proof; a killed child's file never moves
	// again, so the wait ends on the first larger read rather than on a sleep.
	deadline := time.Now().Add(2 * time.Second)
	for {
		if spawnAliveLen(t, alive) > before {
			return
		}
		if time.Now().After(deadline) {
			t.Fatalf("the successor stopped writing after the incumbent's context was cancelled; "+
				"it must outlive the daemon that spawned it (size stayed %d)", before)
		}
		time.Sleep(time.Millisecond)
	}
}

// reapSuccessor kills the stand-in successor and waits for it to be gone
// before the TempDir cleanup runs (cleanups run last-registered first). A
// successor still appending to its liveness file races RemoveAll and fails
// the test with "directory not empty".
func reapSuccessor(t *testing.T, pidFile string) {
	t.Helper()
	raw, err := os.ReadFile(pidFile)
	if err != nil {
		t.Fatalf("read the successor's pid: %v", err)
	}
	pid, err := strconv.Atoi(strings.TrimSpace(string(raw)))
	if err != nil {
		t.Fatalf("parse the successor's pid %q: %v", raw, err)
	}
	t.Cleanup(func() {
		if err := syscall.Kill(pid, syscall.SIGKILL); err != nil && err != syscall.ESRCH {
			t.Errorf("kill the successor %d: %v", pid, err)
			return
		}
		// Spawn's own goroutine reaps the child; ESRCH is that reap.
		deadline := time.Now().Add(2 * time.Second)
		for syscall.Kill(pid, 0) == nil {
			if time.Now().After(deadline) {
				t.Errorf("the successor %d was not reaped after SIGKILL", pid)
				return
			}
			time.Sleep(time.Millisecond)
		}
	})
}

// spawnAliveLen reports how much the stand-in successor has written so far.
// An absent file is zero: the child may not have reached its first write.
func spawnAliveLen(t *testing.T, path string) int64 {
	t.Helper()
	info, err := os.Stat(path)
	if os.IsNotExist(err) {
		return 0
	}
	if err != nil {
		t.Fatalf("stat the liveness file: %v", err)
	}
	return info.Size()
}

func TestSpawnPutsTheSuccessorInItsOwnSession(t *testing.T) {
	// Arrange: a stand-in successor that reports its own pid alongside the
	// address. Emacs runs the incumbent on a pty it owns, so a successor left
	// in the incumbent's session takes the SIGHUP that closing the pty
	// delivers -- the session is the guarantee, not the exit's manners.
	state := t.TempDir()
	pidFile := filepath.Join(state, "successor.pid")
	script := filepath.Join(state, "successor.sh")
	body := "#!/bin/sh\n" +
		"printf '%s' \"$$\" > " + pidFile + "\n" +
		"printf '127.0.0.1:7788\\n' > " + JoiningAddrPath(state) + ".tmp\n" +
		"mv " + JoiningAddrPath(state) + ".tmp " + JoiningAddrPath(state) + "\n" +
		"sleep 5\n"
	if err := os.WriteFile(script, []byte(body), 0o755); err != nil {
		t.Fatalf("write the stand-in: %v", err)
	}
	spawner := NewProcessSpawner(script, state, nil)
	spawner.Poll = time.Millisecond
	spawner.Timeout = 10 * time.Second

	// Act
	if _, err := spawner.Spawn(context.Background(), "127.0.0.1:7777"); err != nil {
		t.Fatalf("Spawn: %v", err)
	}
	raw, err := os.ReadFile(pidFile)
	if err != nil {
		t.Fatalf("read the successor's pid: %v", err)
	}
	pid, err := strconv.Atoi(strings.TrimSpace(string(raw)))
	if err != nil {
		t.Fatalf("parse the successor's pid %q: %v", raw, err)
	}
	t.Cleanup(func() { _ = syscall.Kill(pid, syscall.SIGKILL) })
	child, err := syscall.Getpgid(pid)
	if err != nil {
		t.Fatalf("Getpgid(successor): %v", err)
	}
	self, err := syscall.Getpgid(os.Getpid())
	if err != nil {
		t.Fatalf("Getpgid(self): %v", err)
	}

	// Assert: a session leader's process group is its own pid, and it is not
	// the spawning process's.
	if child == self {
		t.Fatalf("the successor is in the incumbent's process group %d; it must lead its own session", child)
	}
	if child != pid {
		t.Fatalf("the successor's process group is %d, want its own pid %d (a session leader leads its own group)", child, pid)
	}
}

// spawnScript writes a successor stand-in that runs body and nothing else.
func spawnScript(t *testing.T, state, body string) string {
	t.Helper()
	script := filepath.Join(state, "successor.sh")
	if err := os.WriteFile(script, []byte("#!/bin/sh\n"+body), 0o755); err != nil {
		t.Fatalf("write the stand-in: %v", err)
	}
	return script
}

// reportingBody is the stand-in's report of 127.0.0.1:7788, written the way a
// successor writes it: to a temporary file renamed into place.
func reportingBody(state string) string {
	return "printf '127.0.0.1:7788\\n' > " + JoiningAddrPath(state) + ".tmp\n" +
		"mv " + JoiningAddrPath(state) + ".tmp " + JoiningAddrPath(state) + "\n"
}

func TestSpawnFailsAtTheDeadlineWhenALiveSuccessorNeverReports(t *testing.T) {
	// Arrange: a successor that stays up and never reports.
	state := t.TempDir()
	spawner := NewProcessSpawner(spawnScript(t, state, "exec sleep 60\n"), state, nil)
	spawner.Poll = time.Millisecond
	spawner.Timeout = 20 * time.Millisecond
	spawner.StopGrace = 50 * time.Millisecond

	// Act
	successor, err := spawner.Spawn(context.Background(), "127.0.0.1:7777")
	t.Cleanup(func() {
		if err := successor.Stop(context.Background()); err != nil {
			t.Errorf("stop the stand-in: %v", err)
		}
	})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "did not report an address within") {
		t.Fatalf("Spawn = %v, want the deadline", err)
	}
}

// TestSpawnNamesTheExitOfASuccessorThatDiesBeforeReporting is the 2026-09-30
// successor: handed exclusive flags, it exited at once, and the incumbent
// waited out its whole deadline and then blamed slowness.
func TestSpawnNamesTheExitOfASuccessorThatDiesBeforeReporting(t *testing.T) {
	// Arrange: a deadline no test run reaches, so only the exit can answer.
	state := t.TempDir()
	spawner := NewProcessSpawner(spawnScript(t, state, "exit 2\n"), state, nil)
	spawner.Poll = time.Hour
	spawner.Timeout = time.Hour

	// Act
	successor, err := spawner.Spawn(context.Background(), "127.0.0.1:7777")

	// Assert
	var exited *SuccessorExitedError
	if !errors.As(err, &exited) {
		t.Fatalf("Spawn = %v, want *SuccessorExitedError", err)
	}
	if exited.PID != successor.PID() || exited.Exit != "exit status 2" {
		t.Fatalf("exit = %+v, want pid %d and \"exit status 2\"", exited, successor.PID())
	}
}

func TestSpawnAnswersTheAddressASuccessorReportedBeforeItExited(t *testing.T) {
	// Arrange: the report lands, then the process ends, before any poll.
	state := t.TempDir()
	spawner := NewProcessSpawner(spawnScript(t, state, reportingBody(state)+"exit 3\n"), state, nil)
	spawner.Poll = time.Hour
	spawner.Timeout = time.Hour

	// Act
	successor, err := spawner.Spawn(context.Background(), "127.0.0.1:7777")

	// Assert: the address is the answer, and Ready is left to name the exit.
	if err != nil {
		t.Fatalf("Spawn = %v, want the reported address", err)
	}
	if successor.Address() != "127.0.0.1:7788" {
		t.Fatalf("address = %q, want 127.0.0.1:7788", successor.Address())
	}
}

func TestSpawnClearsAStaleReportFromAnEarlierHandover(t *testing.T) {
	// Arrange
	state := t.TempDir()
	if err := ReportJoiningAddr(state, "127.0.0.1:9999"); err != nil {
		t.Fatalf("ReportJoiningAddr: %v", err)
	}
	spawner := NewProcessSpawner("/usr/bin/true", state, nil)
	spawner.Poll = time.Millisecond
	spawner.Timeout = 20 * time.Millisecond

	// Act
	_, err := spawner.Spawn(context.Background(), "127.0.0.1:7777")
	_, reported, readErr := ReadJoiningAddr(state)

	// Assert
	if err == nil {
		t.Fatalf("Spawn answered the stale report from an earlier handover")
	}
	if readErr != nil {
		t.Fatalf("ReadJoiningAddr: %v", readErr)
	}
	if reported {
		t.Fatalf("the stale report survived; it would be dialed as this handover's successor")
	}
}

// TestTheSuccessorInheritsTheIncumbentsConfiguration covers what a successor
// IS: the same daemon, re-pointed.
func TestTheSuccessorInheritsTheIncumbentsConfiguration(t *testing.T) {
	tests := []struct {
		name   string
		config []string
		want   []string
	}{
		{name: "no configuration is just the joining flag", want: []string{JoiningFlag, "127.0.0.1:1"}},
		{name: "the configuration is carried through ahead of the role", config: []string{"--default-config-dir=/roots/default", "--prompts-dir=/prompts"}, want: []string{"--default-config-dir=/roots/default", "--prompts-dir=/prompts", JoiningFlag, "127.0.0.1:1"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act
			got := successorArgv(tc.config, "127.0.0.1:1")

			// Assert
			if !slices.Equal(got, tc.want) {
				t.Fatalf("argv = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestSpawnAnswersTheHandleWhenTheSuccessorNeverReports(t *testing.T) {
	// Arrange: a process that starts and exits without reporting.
	state := t.TempDir()
	spawner := NewProcessSpawner("/usr/bin/true", state, nil)
	spawner.Poll = time.Millisecond
	spawner.Timeout = 20 * time.Millisecond

	// Act
	successor, err := spawner.Spawn(context.Background(), "127.0.0.1:7777")

	// Assert
	if err == nil {
		t.Fatalf("Spawn succeeded with no address reported")
	}
	if successor == nil {
		t.Fatalf("Spawn answered no handle for a process it started; nothing would stop it")
	}
	if err := successor.Stop(context.Background()); err != nil {
		t.Fatalf("Stop: %v", err)
	}
}

func TestSpawnAnswersNoHandleWhenNothingStarted(t *testing.T) {
	// Arrange
	spawner := NewProcessSpawner(filepath.Join(t.TempDir(), "absent"), t.TempDir(), nil)

	// Act
	successor, err := spawner.Spawn(context.Background(), "127.0.0.1:7777")

	// Assert
	if err == nil {
		t.Fatalf("Spawn started a binary that does not exist")
	}
	if successor != nil {
		t.Fatalf("Spawn answered a handle for a process it never started")
	}
}

// TestStopReapsTheSuccessor covers the one proof Stop answers on: the reap.
// Each row is a stand-in successor that reports its address and keeps
// running, and the assertion is that its pid is gone once Stop returns --
// no wait follows, because Stop itself is the wait.
func TestStopReapsTheSuccessor(t *testing.T) {
	tests := []struct {
		name string
		// prelude runs before the stand-in reports and execs its sleep.
		prelude string
	}{
		{name: "a successor that exits on SIGTERM", prelude: ""},
		// SIG_IGN is inherited across exec, so the sleep itself ignores the
		// TERM and only the escalation's SIGKILL ends it.
		{name: "a successor that ignores SIGTERM is killed", prelude: "trap '' TERM\n"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			state := t.TempDir()
			script := filepath.Join(state, "successor.sh")
			body := "#!/bin/sh\n" + tc.prelude +
				"printf '127.0.0.1:7788\\n' > " + JoiningAddrPath(state) + ".tmp\n" +
				"mv " + JoiningAddrPath(state) + ".tmp " + JoiningAddrPath(state) + "\n" +
				"exec sleep 60\n"
			if err := os.WriteFile(script, []byte(body), 0o755); err != nil {
				t.Fatalf("write the stand-in: %v", err)
			}
			spawner := NewProcessSpawner(script, state, nil)
			spawner.Poll = time.Millisecond
			spawner.Timeout = 10 * time.Second
			spawner.StopGrace = 50 * time.Millisecond
			successor, err := spawner.Spawn(context.Background(), "127.0.0.1:7777")
			if err != nil {
				t.Fatalf("Spawn: %v", err)
			}
			pid := successor.(*processSuccessor).process.Pid

			// Act
			stopErr := successor.Stop(context.Background())

			// Assert
			if stopErr != nil {
				t.Fatalf("Stop: %v", stopErr)
			}
			if err := syscall.Kill(pid, 0); err != syscall.ESRCH {
				t.Fatalf("kill(%d, 0) = %v after Stop, want ESRCH: the successor is still there", pid, err)
			}
		})
	}
}

// spawnStandIn spawns a stand-in successor that reports its address and then
// runs tail, with probe as its health probe. A successor still running at the
// end of the test is stopped.
func spawnStandIn(t *testing.T, tail string, probe HealthProbe) Successor {
	t.Helper()
	state := t.TempDir()
	spawner := NewProcessSpawner(spawnScript(t, state, reportingBody(state)+tail), state, nil)
	spawner.Poll = time.Millisecond
	spawner.Timeout = 10 * time.Second
	spawner.StopGrace = 50 * time.Millisecond
	spawner.Probe = probe
	spawner.ProbeEvery = time.Millisecond
	successor, err := spawner.Spawn(context.Background(), "127.0.0.1:7777")
	if err != nil {
		t.Fatalf("Spawn: %v", err)
	}
	t.Cleanup(func() {
		if err := successor.Stop(context.Background()); err != nil {
			t.Errorf("stop the stand-in: %v", err)
		}
	})
	return successor
}

func TestReadyAnswersOnceTheSuccessorAnswersItsHealthProbe(t *testing.T) {
	// Arrange: the successor answers on the third probe, as one still
	// finishing its boot does.
	var probes atomic.Int32
	successor := spawnStandIn(t, "exec sleep 60\n", func(context.Context, string) error {
		if probes.Add(1) < 3 {
			return errors.New("connection refused")
		}
		return nil
	})

	// Act
	err := successor.Ready(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("Ready = %v, want nil once the probe answered", err)
	}
	if got := probes.Load(); got != 3 {
		t.Fatalf("probes = %d, want exactly the three it took to answer", got)
	}
}

// TestReadyNamesTheExitOfASuccessorThatDiesBeforeAnswering is the 2026-09-27
// successor: it reported its address and then exited on a state layout it
// could not read, without ever serving.
func TestReadyNamesTheExitOfASuccessorThatDiesBeforeAnswering(t *testing.T) {
	// Arrange
	successor := spawnStandIn(t, "exit 3\n", func(context.Context, string) error {
		return errors.New("connection refused")
	})

	// Act
	err := successor.Ready(context.Background())

	// Assert
	var exited *SuccessorExitedError
	if !errors.As(err, &exited) {
		t.Fatalf("Ready = %v, want *SuccessorExitedError", err)
	}
	if exited.PID != successor.PID() || exited.Exit != "exit status 3" {
		t.Fatalf("exit = %+v, want pid %d and \"exit status 3\"", exited, successor.PID())
	}
}

func TestReadyGivesUpAtItsBoundOnASuccessorThatNeverAnswers(t *testing.T) {
	// Arrange
	successor := spawnStandIn(t, "exec sleep 60\n", func(context.Context, string) error {
		return errors.New("connection refused")
	})
	ctx, cancel := context.WithTimeout(context.Background(), 50*time.Millisecond)
	defer cancel()

	// Act
	err := successor.Ready(ctx)

	// Assert
	if !errors.Is(err, context.DeadlineExceeded) {
		t.Fatalf("Ready = %v, want the bound's deadline", err)
	}
}

// TestSpawnReplacementStartsAnOrdinaryDaemonThatReplaces covers the restart's
// spawn: the replacement is the same binary, told it replaces, never joining.
// The stand-in writes its last argument into a file renamed into place and
// exits, so its exit is the synchronization (a FIFO read here could come back
// empty, or block forever, on macOS; see the argv test below).
func TestSpawnReplacementStartsAnOrdinaryDaemonThatReplaces(t *testing.T) {
	// Arrange
	state := t.TempDir()
	report := filepath.Join(state, "last")
	body := "for last; do :; done\nprintf '%s' \"$last\" > " + report + ".tmp\nmv " + report + ".tmp " + report + "\n"
	spawner := NewProcessSpawner(spawnScript(t, state, body), state, nil)

	// Act
	pid, err := spawner.SpawnReplacement(context.Background())

	// Assert
	if err != nil || pid <= 0 {
		t.Fatalf("SpawnReplacement = (%d, %v), want a started process", pid, err)
	}
	<-pidExited(t, pid)
	last, err := os.ReadFile(report)
	if err != nil {
		t.Fatalf("read the stand-in's report: %v", err)
	}
	if string(last) != "--"+ReplacingFlagName {
		t.Fatalf("the replacement's last argument = %q, want --%s", last, ReplacingFlagName)
	}
}

// TestEverySpawnStartsItsProcessOnTheConfigurationArgv pins that both spawns
// hand the process ProcessSpawner.Argv plus their own role, and nothing read
// from this process's own command line.
func TestEverySpawnStartsItsProcessOnTheConfigurationArgv(t *testing.T) {
	config := []string{"--default-config-dir=/roots/default", "--node=node"}
	tests := []struct {
		name string
		// spawn starts the stand-in and answers a channel closed when it has
		// exited.
		spawn func(s *ProcessSpawner) (<-chan struct{}, error)
		want  []string
	}{
		{
			name: "successor",
			spawn: func(s *ProcessSpawner) (<-chan struct{}, error) {
				successor, err := s.Spawn(context.Background(), "127.0.0.1:7777")
				if successor == nil {
					return nil, err
				}
				t.Cleanup(func() { _ = successor.Stop(context.Background()) })
				return successor.(*processSuccessor).exited, err
			},
			want: append(slices.Clone(config), JoiningFlag, "127.0.0.1:7777"),
		},
		{
			name: "replacement",
			spawn: func(s *ProcessSpawner) (<-chan struct{}, error) {
				pid, err := s.SpawnReplacement(context.Background())
				if err != nil {
					return nil, err
				}
				return pidExited(t, pid), nil
			},
			want: append(slices.Clone(config), "--"+ReplacingFlagName),
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the stand-in reports an address, writes its argv into a
			// file renamed into place, and exits. ITS EXIT IS THIS TEST'S
			// SYNCHRONIZATION: once the process is gone the file is whole.
			//
			// It used to report through a FIFO whose blocking read was the
			// synchronization, and on macOS that read came back EMPTY although
			// the stand-in's trace showed the write and a clean exit, or, with
			// no exit to watch, blocked in its open until the 10m timeout
			// (daemon unit suite, 2026-10-03).
			state := t.TempDir()
			argv := filepath.Join(state, "argv")
			body := reportingBody(state) + "printf '%s\\n' \"$@\" > " + argv + ".tmp\nmv " + argv + ".tmp " + argv + "\n"
			spawner := NewProcessSpawner(spawnScript(t, state, body), state, config)
			spawner.Poll = time.Millisecond

			// Act
			exited, err := tt.spawn(spawner)
			if err != nil {
				t.Fatalf("spawn: %v", err)
			}
			<-exited

			// Assert
			raw, err := os.ReadFile(argv)
			if err != nil {
				t.Fatalf("the stand-in exited without its argv: %v", err)
			}
			got := strings.Split(strings.TrimSuffix(string(raw), "\n"), "\n")
			if !slices.Equal(got, tt.want) {
				t.Fatalf("argv = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestSpawnReplacementRefusesWithNoDaemonBinary(t *testing.T) {
	// Arrange
	spawner := NewProcessSpawner("", t.TempDir(), nil)

	// Act
	_, err := spawner.SpawnReplacement(context.Background())

	// Assert
	if err == nil {
		t.Fatal("SpawnReplacement accepted an empty binary path")
	}
}

// pidExited answers a channel closed once PID, a process this test started
// and whose reap belongs to someone else, is gone. It polls the kernel on a
// ticker: a process that is not this goroutine's child announces nothing.
func pidExited(t *testing.T, pid int) <-chan struct{} {
	t.Helper()
	gone := make(chan struct{})
	stop := make(chan struct{})
	t.Cleanup(func() { close(stop) })
	go func() {
		ticker := time.NewTicker(5 * time.Millisecond)
		defer ticker.Stop()
		for {
			if err := syscall.Kill(pid, 0); err != nil {
				close(gone)
				return
			}
			select {
			case <-ticker.C:
			case <-stop:
				return
			}
		}
	}()
	return gone
}
