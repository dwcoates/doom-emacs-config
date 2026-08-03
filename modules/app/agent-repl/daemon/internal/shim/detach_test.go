package shim

import (
	"errors"
	"strconv"
	"strings"
	"syscall"
	"testing"
)

// testStop is the attribution every deliberate stop in these tests carries.
var testStop = Stop{Initiator: "shim_unit_test", Reason: "exercising the stop path"}

func TestSpawnConfiguresTheChildIntoItsOwnProcessGroup(t *testing.T) {
	// Arrange + Act
	p := spawnScript(t, `cat >/dev/null`)
	// Assert — the attribute, not its effect: this is the one setting that
	// decouples the shim's lifetime from the daemon's process group.
	if p.cmd.SysProcAttr == nil || !p.cmd.SysProcAttr.Setpgid {
		t.Fatalf("SysProcAttr = %+v, want Setpgid so a signal to the daemon's group cannot reach the shim", p.cmd.SysProcAttr)
	}
}

func TestSpawnDoesNotBindTheChildToADaemonScopedContext(t *testing.T) {
	// Arrange + Act
	p := spawnScript(t, `cat >/dev/null`)
	// Assert — a Cancel func would re-couple the shim to whatever context the
	// daemon built the command with, undoing the detachment above.
	if p.cmd.Cancel != nil {
		t.Fatal("shim command carries a Cancel func, which would kill the shim when a daemon-scoped context ends")
	}
}

func TestSpawnedShimLeadsItsOwnProcessGroup(t *testing.T) {
	// Arrange + Act — a real child, held alive by its stdin pipe.
	p := spawnScript(t, `cat >/dev/null`)
	pid := p.cmd.Process.Pid
	// Assert
	pgid, err := syscall.Getpgid(pid)
	if err != nil {
		t.Fatalf("Getpgid(%d): %v", pid, err)
	}
	if pgid != pid {
		t.Fatalf("shim pid=%d pgid=%d, want pgid == pid (its own group)", pid, pgid)
	}
}

func TestSpawnedShimIsNotInTheSpawningProcessGroup(t *testing.T) {
	// Arrange + Act
	p := spawnScript(t, `cat >/dev/null`)
	pgid, err := syscall.Getpgid(p.cmd.Process.Pid)
	if err != nil {
		t.Fatalf("Getpgid: %v", err)
	}
	// Assert — this is the production failure verbatim: the shim used to share
	// the daemon's group, so bouncing the daemon killed every shim with it.
	if pgid == syscall.Getpgrp() {
		t.Fatalf("shim pgid=%d equals the spawning process group; a signal to that group would kill the shim", pgid)
	}
}

func TestSpawnRecordsThePidAndProcessGroupItObserved(t *testing.T) {
	// Arrange + Act
	p := spawnScript(t, `cat >/dev/null`)
	// Assert — the accessors the daemon's spawn record is built from.
	if p.Pid() != p.cmd.Process.Pid || p.Pgid() != p.Pid() {
		t.Fatalf("Pid()=%d Pgid()=%d, want both equal to the child pid %d", p.Pid(), p.Pgid(), p.cmd.Process.Pid)
	}
}

func TestReportProcessGroupClassifiesEveryDetachmentOutcome(t *testing.T) {
	tests := []struct {
		name     string
		getpgid  func(int) (int, error)
		wantPgid int
		wantLog  string
	}{
		{
			name:     "detached",
			getpgid:  func(pid int) (int, error) { return pid, nil },
			wantPgid: 4321,
			wantLog:  "",
		},
		{
			name:     "unreadable group is announced, never assumed",
			getpgid:  func(int) (int, error) { return 0, errors.New("no such process") },
			wantPgid: 0,
			wantLog:  "UNVERIFIED",
		},
		{
			name:     "still in the daemon group is announced as still coupled",
			getpgid:  func(int) (int, error) { return 99, nil },
			wantPgid: 99,
			wantLog:  "STILL COUPLED",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			logger := &recordingLogger{}
			// Act
			got := reportProcessGroup(4321, logger, tc.getpgid)
			// Assert
			if got != tc.wantPgid {
				t.Errorf("pgid = %d, want %d", got, tc.wantPgid)
			}
			switch {
			case tc.wantLog == "":
				if len(logger.records) != 0 {
					t.Errorf("a healthy detachment logged %#v, want nothing", logger.records)
				}
			default:
				logger.record(t, 0, "normal", tc.wantLog)
			}
		})
	}
}

func TestStopValidateRejectsAnUnattributedStop(t *testing.T) {
	tests := []struct {
		name string
		stop Stop
		want string
	}{
		{name: "no initiator", stop: Stop{Reason: "r"}, want: "Initiator"},
		{name: "no reason", stop: Stop{Initiator: "i"}, want: "Reason"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange + Act
			err := tc.stop.Validate()
			// Assert
			if err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("validate() = %v, want an error naming %s", err, tc.want)
			}
		})
	}
}

func TestStopValidateAcceptsAFullyAttributedStop(t *testing.T) {
	// Arrange + Act + Assert
	if err := testStop.Validate(); err != nil {
		t.Fatalf("validate() = %v, want nil for a fully attributed stop", err)
	}
}

func TestTerminateRefusesAnUnattributedStop(t *testing.T) {
	// Arrange
	p := spawnScript(t, `cat >/dev/null`)
	// Act
	err := p.Terminate(Stop{})
	// Assert
	if err == nil || !strings.Contains(err.Error(), "refusing to terminate") {
		t.Fatalf("Terminate(unattributed) = %v, want a refusal", err)
	}
}

func TestKillRefusesAnUnattributedStop(t *testing.T) {
	// Arrange
	p := spawnScript(t, `cat >/dev/null`)
	// Act
	err := p.Kill(Stop{})
	// Assert
	if err == nil || !strings.Contains(err.Error(), "refusing to kill") {
		t.Fatalf("Kill(unattributed) = %v, want a refusal", err)
	}
}

func TestTerminateRefusalLeavesTheShimRunning(t *testing.T) {
	// Arrange
	p := spawnScript(t, `cat >/dev/null`)
	// Act
	if err := p.Terminate(Stop{}); err == nil {
		t.Fatal("Terminate(unattributed) should have been refused")
	}
	// Assert — the refusal is a refusal, not a signal sent and then complained about.
	if err := syscall.Kill(p.Pid(), 0); err != nil {
		t.Fatalf("shim pid %d is gone after a REFUSED terminate: %v", p.Pid(), err)
	}
}

func TestTerminateStopsAnAttributedShim(t *testing.T) {
	// Arrange
	p := spawnScript(t, `cat >/dev/null`)
	// Act
	if err := p.Terminate(testStop); err != nil {
		t.Fatalf("Terminate: %v", err)
	}
	// Assert — Wait reaping the child is the completion signal; no sleeping.
	if got := ExitDescription(p.Wait()); got != "killed by terminated" {
		t.Fatalf("exit = %q, want the SIGTERM we sent", got)
	}
}

func TestKillStopsAnAttributedShim(t *testing.T) {
	// Arrange
	p := spawnScript(t, `cat >/dev/null`)
	// Act
	if err := p.Kill(testStop); err != nil {
		t.Fatalf("Kill: %v", err)
	}
	// Assert
	if got := ExitDescription(p.Wait()); got != "killed by killed" {
		t.Fatalf("exit = %q, want the SIGKILL we sent", got)
	}
}

// TestTerminateSignalsTheShimPidAndNotItsProcessGroup is the counterpart to the
// detachment: now that the shim LEADS a group, a group-directed signal would
// reach everything the shim spawned. The stop path must aim at the one pid.
//
// The shim announces its own child's pid over the protocol channel — the same
// stdout decode path every other test uses — so the check needs no polling and
// no sleeping for synchronization.
func TestTerminateSignalsTheShimPidAndNotItsProcessGroup(t *testing.T) {
	// Arrange — a shim with a long-lived child in the shim's process group.
	p := spawnScript(t, `sleep 300 & printf '{"type":"ready","session_id":"%s","shim_version":"1","sdk_version":"2","permission_mode":"default"}\n' "$!"; wait`)
	evt := recvEvent(t, p)
	childPID, err := strconv.Atoi(evt.SessionID)
	if err != nil {
		t.Fatalf("child pid %q: %v", evt.SessionID, err)
	}
	t.Cleanup(func() { _ = syscall.Kill(childPID, syscall.SIGKILL) })

	// Act — stop the shim and reap it, so the signal has demonstrably landed.
	if err := p.Terminate(testStop); err != nil {
		t.Fatalf("Terminate: %v", err)
	}
	_ = p.Wait()

	// Assert — a group-directed SIGTERM would have taken the child with it.
	if err := syscall.Kill(childPID, 0); err != nil {
		t.Fatalf("the shim's child pid %d also died: the stop signalled the process GROUP, not the shim pid: %v", childPID, err)
	}
}
