package grantedclock

import (
	"os"
	"os/exec"
	"slices"
	"testing"
	"time"
	"unsafe"
)

func TestRusageInfoIsTheKernelsV4Layout(t *testing.T) {
	// Arrange: <sys/resource.h>'s rusage_info_v4 is a 16-byte uuid and 35
	// uint64s, ri_runnable_time the last of them.

	// Act
	size, at := unsafe.Sizeof(rusageInfo{}), unsafe.Offsetof(rusageInfo{}.RunnableTime)

	// Assert
	if size != 296 || at != 288 {
		t.Fatalf("rusageInfo is %d bytes with RunnableTime at %d, want 296 and 288", size, at)
	}
}

func TestMachDurationConvertsTicksAtTheTimebase(t *testing.T) {
	tests := []struct {
		name        string
		ticks, freq uint64
		want        time.Duration
	}{
		{name: "an apple silicon timebase ticks at 24MHz", ticks: 341649, freq: 24_000_000, want: 14235375 * time.Nanosecond},
		{name: "an intel timebase ticks in nanoseconds", ticks: 1500, freq: 1_000_000_000, want: 1500 * time.Nanosecond},
		{name: "a day of ticks does not overflow", ticks: 24 * 3600 * 24_000_000, freq: 24_000_000, want: 24 * time.Hour},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: tt.ticks and tt.freq.

			// Act
			got := machDuration(tt.ticks, tt.freq)

			// Assert
			if got != tt.want {
				t.Fatalf("machDuration(%d, %d) = %s, want %s", tt.ticks, tt.freq, got, tt.want)
			}
		})
	}
}

func TestChildPIDsListsAStartedChild(t *testing.T) {
	// Arrange
	cmd := exec.Command("/bin/sleep", "30")
	if err := cmd.Start(); err != nil {
		t.Fatalf("start a child: %v", err)
	}
	t.Cleanup(func() { _ = cmd.Process.Kill(); _ = cmd.Wait() })

	// Act
	kids, err := childPIDs(os.Getpid())

	// Assert
	if err != nil || !slices.Contains(kids, cmd.Process.Pid) {
		t.Fatalf("childPIDs() = %v, %v, want it to hold %d", kids, err, cmd.Process.Pid)
	}
}

func TestPIDRusageAnswersWhatItCouldRead(t *testing.T) {
	tests := []struct {
		name string
		pid  func(t *testing.T) int
		want usageState
	}{
		{name: "this process is read", pid: func(*testing.T) int { return os.Getpid() }, want: usageRead},
		{name: "a reaped process is gone", pid: func(t *testing.T) int {
			cmd := exec.Command("/usr/bin/true")
			if err := cmd.Run(); err != nil {
				t.Fatalf("run a child: %v", err)
			}
			return cmd.Process.Pid
		}, want: usageGone},
		{name: "a process running as root is forbidden", pid: func(*testing.T) int { return 1 }, want: usageForbidden},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			pid := tt.pid(t)

			// Act
			_, state, err := pidRusage(pid)

			// Assert
			if err != nil || state != tt.want {
				t.Fatalf("pidRusage(%d) = %v, %v, want %v", pid, state, err, tt.want)
			}
		})
	}
}

func TestProcessTreeStallReadsThisProcessTree(t *testing.T) {
	// Arrange
	stall, err := processTreeStall(os.Getpid())
	if err != nil {
		t.Fatalf("processTreeStall() = %v", err)
	}
	first, err := stall()
	if err != nil {
		t.Fatalf("first stall() = %v", err)
	}

	// Act
	second, err := stall()

	// Assert
	if err != nil || first != 0 || second < first {
		t.Fatalf("stall() = %s then %s, %v, want an origin of 0 and no decrease", first, second, err)
	}
}
