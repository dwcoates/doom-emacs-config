package procmetrics

import (
	"context"
	"errors"
	"testing"
	"time"

	"agent-repld/internal/clock/clocktest"
	"agent-repld/internal/dlog"
	"agent-repld/internal/restarttiming"
)

var windowEpoch = time.Date(2026, 11, 20, 12, 0, 0, 0, time.UTC)

// newWindowsHarness is the collector harness with windows measuring through
// it, on a scripted clock, their series registered beside the collector's.
func newWindowsHarness(t *testing.T) (*harness, *Windows, *clocktest.Scripted) {
	t.Helper()
	h := newHarness(t, newWorld())
	clk := clocktest.New(windowEpoch)
	ws := NewWindows(h.collector, clk)
	for _, c := range ws.Collectors() {
		h.reg.MustRegister(c)
	}
	return h, ws, clk
}

// histogram answers the sum and count of NAME's histogram under labels.
func (h *harness) histogram(t *testing.T, name string, labels ...string) (sum float64, count uint64) {
	t.Helper()
	families, err := h.reg.Gather()
	if err != nil {
		t.Fatalf("Gather: %v", err)
	}
	for _, f := range families {
		if f.GetName() != name {
			continue
		}
		for _, m := range f.GetMetric() {
			if matches(m, labels) {
				hist := m.GetHistogram()
				return hist.GetSampleSum(), hist.GetSampleCount()
			}
		}
	}
	return 0, 0
}

// windowLabels are a daemon_boot window's labels for one stat.
func windowLabels(process, stat string) []string {
	return []string{"restart", "daemon_boot", "phase", "sessions_up", "process", process, "stat", stat}
}

func mustObserved(t *testing.T, h *harness, name string, want float64, labels ...string) {
	t.Helper()
	sum, count := h.histogram(t, name, labels...)
	if count != 1 || sum != want {
		t.Fatalf("%s%v = sum %v over %d, want one observation of %v", name, labels, sum, count, want)
	}
}

func TestAClosedWindowRecordsTheMeanAndPeakOfItsGroup(t *testing.T) {
	// Arrange: the daemon at 1 core, then 3 cores, its resident memory
	// 100, 300 and 200 at the three reads.
	h, ws, clk := newWindowsHarness(t)
	closeWindow := ws.OpenWindow(restarttiming.DaemonBoot, "sessions_up", time.Minute, restarttiming.Watched{Process: ProcessDaemon})
	ws.Sample()
	clk.Set(windowEpoch.Add(time.Second))
	h.reader.setUsage(testSelf, Usage{CPUSeconds: 11, ResidentBytes: 300})
	ws.Sample()
	clk.Set(windowEpoch.Add(2 * time.Second))
	h.reader.setUsage(testSelf, Usage{CPUSeconds: 14, ResidentBytes: 200})

	// Act
	closeWindow()
	open := ws.Sample()

	// Assert
	if open {
		t.Fatal("Sample answered a window still open after the only one closed")
	}
	mustObserved(t, h, WindowCPUName, 2, windowLabels(ProcessDaemon, statMean)...)
	mustObserved(t, h, WindowCPUName, 3, windowLabels(ProcessDaemon, statMax)...)
	mustObserved(t, h, WindowResidentName, 200, windowLabels(ProcessDaemon, statMean)...)
	mustObserved(t, h, WindowResidentName, 300, windowLabels(ProcessDaemon, statMax)...)
}

func TestAWindowOnOneWorkspacesShimLeavesAnotherWorkspacesShimOut(t *testing.T) {
	// Arrange: a second shim, for ws2.
	h, ws, clk := newWindowsHarness(t)
	h.reader.addProcess(Identity{PID: 31, PPID: testSelf, Name: "node", Responsible: testEmacs, StartUnixMicros: 5}, Usage{CPUSeconds: 1, ResidentBytes: 900})
	h.reader.mu.Lock()
	h.reader.argv[31] = []string{"node", "main.js", "--listen", "/state/sock/ws2.sock"}
	h.reader.mu.Unlock()
	closeWindow := ws.OpenWindow(restarttiming.SessionStart, "total", time.Minute, restarttiming.Watched{Process: ProcessShim, Workspace: "ws1"})
	ws.Sample()
	clk.Set(windowEpoch.Add(time.Second))

	// Act
	closeWindow()
	ws.Sample()

	// Assert: only ws1's shim, resident 50 at both reads.
	mustObserved(t, h, WindowResidentName, 50, "restart", "session_start", "phase", "total", "process", ProcessShim, "stat", statMax)
}

func TestAWindowPastItsBoundIsDroppedUnrecorded(t *testing.T) {
	// Arrange
	h, ws, clk := newWindowsHarness(t)
	ws.OpenWindow(restarttiming.DaemonBoot, "sessions_up", time.Second, restarttiming.Watched{Process: ProcessDaemon})
	ws.Sample()
	clk.Set(windowEpoch.Add(2 * time.Second))

	// Act
	open := ws.Sample()

	// Assert
	if open {
		t.Fatal("a window past its bound is still open")
	}
	if _, count := h.histogram(t, WindowResidentName, windowLabels(ProcessDaemon, statMax)...); count != 0 {
		t.Fatalf("a window past its bound was recorded %d times", count)
	}
	if _, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "info", Operation: Operation, Message: "a resource window stayed open past its bound; it is dropped unrecorded"}); !ok {
		t.Fatalf("records = %+v, want the bound named", h.log.Records())
	}
}

func TestWithNoWindowOpenNothingIsRead(t *testing.T) {
	// Arrange
	h, ws, _ := newWindowsHarness(t)
	h.reader.onUsage = map[int]func(*fakeReader){testSelf: func(*fakeReader) { t.Error("a process was read with no window open") }}

	// Act
	open := ws.Sample()

	// Assert
	if open {
		t.Fatal("Sample answered a window open with none opened")
	}
}

func TestAWindowAfterAnIdleSpellStartsFromABaseline(t *testing.T) {
	// Arrange: a first window reads the daemon at 10s of CPU; an hour later,
	// at 3610s, a second window opens.
	h, ws, clk := newWindowsHarness(t)
	first := ws.OpenWindow(restarttiming.ServiceRestart, "store", time.Minute, restarttiming.Watched{Process: ProcessDaemon})
	ws.Sample()
	first()
	ws.Sample()
	ws.Sample()
	clk.Set(windowEpoch.Add(time.Hour))
	h.reader.setUsage(testSelf, Usage{CPUSeconds: 3610, ResidentBytes: 100})
	second := ws.OpenWindow(restarttiming.DaemonBoot, "sessions_up", time.Minute, restarttiming.Watched{Process: ProcessDaemon})
	ws.Sample()
	clk.Set(windowEpoch.Add(time.Hour + time.Second))
	h.reader.setUsage(testSelf, Usage{CPUSeconds: 3610.5, ResidentBytes: 100})

	// Act
	second()
	ws.Sample()

	// Assert: half a core inside the window, never the hour's average.
	mustObserved(t, h, WindowCPUName, 0.5, windowLabels(ProcessDaemon, statMax)...)
}

func TestAFailedUsageReadInAWindowIsRecordedAtError(t *testing.T) {
	// Arrange
	h, ws, _ := newWindowsHarness(t)
	ws.OpenWindow(restarttiming.DaemonBoot, "sessions_up", time.Minute, restarttiming.Watched{Process: ProcessDaemon})
	ws.Sample()
	h.reader.mu.Lock()
	h.reader.usageErr[testSelf] = errors.New("proc_pid_rusage: operation not permitted")
	h.reader.mu.Unlock()

	// Act
	ws.Sample()

	// Assert
	failed, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "error", Operation: Operation, Message: "a process metrics read failed; what it measures is omitted until it succeeds"})
	if !ok || failed.Context["step"] != "window_usage" {
		t.Fatalf("records = %+v, want the window's failed read at ERROR", h.log.Records())
	}
}

func TestRunReadsAtOnceWhenAWindowOpensAndAgainEveryHalfSecond(t *testing.T) {
	// Arrange
	_, ws, clk := newWindowsHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	done := make(chan error, 1)
	go func() { done <- ws.Run(ctx) }()

	// Act
	ws.OpenWindow(restarttiming.DaemonBoot, "sessions_up", time.Minute, restarttiming.Watched{Process: ProcessDaemon})

	// Assert
	if got := <-clk.Asked; got != WindowSampleEvery {
		t.Fatalf("the next read waits %v, want %v", got, WindowSampleEvery)
	}
	cancel()
	if err := <-done; !errors.Is(err, context.Canceled) {
		t.Fatalf("Run = %v, want the context's end", err)
	}
}
