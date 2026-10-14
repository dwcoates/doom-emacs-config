package harness

import (
	"fmt"
	"strings"
	"testing"
	"time"
)

func TestLinuxThreadsHeld(t *testing.T) {
	cases := []struct {
		name            string
		threads         []linuxThread
		wantHalted      bool
		wantAwaitingCPU bool
		wantName        string
	}{
		{
			name:       "every thread stopped, traced or dead is halted",
			threads:    []linuxThread{{"1", "T"}, {"2", "t"}, {"3", "Z"}, {"4", "X"}, {"5", "x"}},
			wantHalted: true,
		},
		{
			name:       "a process with no thread left is halted",
			wantHalted: true,
		},
		{
			name:            "a runnable thread awaits the CPU",
			threads:         []linuxThread{{"1", "T"}, {"2", "R"}},
			wantAwaitingCPU: true,
			wantName:        "stopping (thread 2 in state R)",
		},
		{
			name:     "a sleeping thread does not await the CPU",
			threads:  []linuxThread{{"1", "T"}, {"2", "S"}},
			wantName: "stopping (thread 2 in state S)",
		},
		{
			name:     "a blocked thread beside a runnable one does not await the CPU",
			threads:  []linuxThread{{"1", "R"}, {"2", "D"}},
			wantName: "stopping (thread 1 in state R)",
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			halted, awaitingCPU, name := linuxThreadsHeld(tc.threads)

			// Assert
			if halted != tc.wantHalted || awaitingCPU != tc.wantAwaitingCPU || name != tc.wantName {
				t.Fatalf("linuxThreadsHeld = %v, %v, %q; want %v, %v, %q", halted, awaitingCPU, name, tc.wantHalted, tc.wantAwaitingCPU, tc.wantName)
			}
		})
	}
}

// stallRecorder records what a stall report wrote.
type stallRecorder struct {
	errors, logs []string
}

func (r *stallRecorder) Helper() {}
func (r *stallRecorder) Errorf(format string, args ...any) {
	r.errors = append(r.errors, fmt.Sprintf(format, args...))
}
func (r *stallRecorder) Logf(format string, args ...any) {
	r.logs = append(r.logs, fmt.Sprintf(format, args...))
}

func TestReportStrayStall(t *testing.T) {
	cases := []struct {
		name       string
		state      processState
		wantErrors int
		wantLogs   int
		wantText   string
	}{
		{
			name:     "a posted stop whose threads await the CPU is logged as starved",
			state:    processState{name: "stopping (1 thread(s) still running)", awaitingCPU: true},
			wantLogs: 1,
			wantText: "starved, not stuck",
		},
		{
			name:       "a stop not taken for any other reason fails the test",
			state:      processState{name: "sleeping"},
			wantErrors: 1,
			wantText:   "is still sleeping 30s after its SIGSTOP; still waiting",
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			var r stallRecorder

			// Act
			reportStrayStall(&r, stray{pid: 7, args: "node shim"}, 7, tc.state, 30*time.Second, "(ps err <nil>):\nPID")

			// Assert
			if len(r.errors) != tc.wantErrors || len(r.logs) != tc.wantLogs {
				t.Fatalf("errors %q, logs %q; want %d error(s), %d log(s)", r.errors, r.logs, tc.wantErrors, tc.wantLogs)
			}
			all := strings.Join(append(r.errors, r.logs...), "\n")
			for _, want := range []string{"stray 7 (node shim)", tc.wantText, "The kernel's view (ps err <nil>):\nPID"} {
				if !strings.Contains(all, want) {
					t.Fatalf("report %q lacks %q", all, want)
				}
			}
		})
	}
}
