package frontend

import (
	"errors"
	"fmt"
	"strings"
	"sync"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// recordingLatency captures every sample the transport hands it, and can be
// asked to fail so the transport's error surfacing is exercised.
//
// It is mutex-guarded because the transport genuinely records from more than
// one goroutine: a lane records a completion while the ack-deadline watchdog
// may be recording an overdue observation for a different command.
type recordingLatency struct {
	mu      sync.Mutex
	samples []CommandLatencySample
	err     error
	// notify, when non-nil, receives every recorded sample. It lets a test
	// rendezvous with an emission instead of waiting out a clock.
	notify chan CommandLatencySample
}

func (r *recordingLatency) RecordCommandLatency(sample CommandLatencySample) error {
	r.mu.Lock()
	r.samples = append(r.samples, sample)
	err := r.err
	notify := r.notify
	r.mu.Unlock()
	if notify != nil {
		notify <- sample
	}
	return err
}

// all returns a snapshot of the recorded samples.
func (r *recordingLatency) all() []CommandLatencySample {
	r.mu.Lock()
	defer r.mu.Unlock()
	return append([]CommandLatencySample(nil), r.samples...)
}

// concurrentLogs captures log lines from every goroutine that logs. With
// commands dispatched on lane goroutines, the transport's Logf is genuinely
// called concurrently — by a lane running a command and by the read loop
// closing its lanes — so an unguarded slice would be capturing a race rather
// than the log.
type concurrentLogs struct {
	mu    sync.Mutex
	lines []string
}

func (r *concurrentLogs) logf(format string, args ...any) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.lines = append(r.lines, fmt.Sprintf(format, args...))
}

func (r *concurrentLogs) all() []string {
	r.mu.Lock()
	defer r.mu.Unlock()
	return append([]string(nil), r.lines...)
}

// runOneCommand drives readLoop over a single scripted command and returns the
// server once the loop has finished. Nothing here waits on a clock: the
// command stream is closed up front, so the loop returns when the script is
// exhausted and its lanes have drained — a rendezvous with the loop's own
// completion rather than with elapsed time.
func runOneCommand(t *testing.T, cfg Config, cmd *frontendv1.FrontendCommand) *Server {
	t.Helper()
	s := New(cfg)
	cl := newClient(defaultClientBuffer, nil, ClientKindHost)
	s.clients[cl] = struct{}{}
	c := newScriptedConn()
	c.cmds <- cmd
	close(c.cmds)
	s.readLoop(c, cl)
	return s
}

func TestReadLoopRecordsOneLatencySamplePerCommand(t *testing.T) {
	tests := []struct {
		name      string
		threshold time.Duration
		wantSlow  bool
	}{
		{
			name: "a threshold nothing real can beat leaves the sample fast",
			// An hour is unreachable by a scripted in-memory dispatch, so this
			// arm is a deterministic "under threshold" without a clock.
			threshold: time.Hour,
			wantSlow:  false,
		},
		{
			name: "a one-nanosecond threshold is crossed by any real dispatch",
			// Symmetrically deterministic: no measurable work completes in
			// under a nanosecond, so this arm is always over threshold.
			threshold: time.Nanosecond,
			wantSlow:  true,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			latency := &recordingLatency{}
			cfg := Config{
				Logf: testLogf(t), LogVerbosef: testLogf(t),
				State: staticState{snap: sampleSnapshot()}, Handler: &mockHandler{},
				CommandLatency: latency, AckWarnThreshold: tc.threshold,
			}

			// Act.
			runOneCommand(t, cfg, submitCmd("r-1", "/ws"))

			// Assert.
			if len(latency.samples) != 1 {
				t.Fatalf("latency samples = %d, want exactly one per completed command", len(latency.samples))
			}
			if got := latency.samples[0].Slow(); got != tc.wantSlow {
				t.Fatalf("sample.Slow() with threshold %s = %v, want %v (delivery=%s)",
					tc.threshold, got, tc.wantSlow, latency.samples[0].Delivery)
			}
		})
	}
}

func TestReadLoopSampleCarriesTheCommandIdentity(t *testing.T) {
	// Arrange.
	latency := &recordingLatency{}
	cfg := Config{
		Logf: testLogf(t), LogVerbosef: testLogf(t),
		State: staticState{snap: sampleSnapshot()}, Handler: &mockHandler{},
		CommandLatency: latency, AckWarnThreshold: time.Hour,
	}

	// Act.
	runOneCommand(t, cfg, submitCmd("r-7", "/ws"))

	// Assert.
	got := latency.samples[0]
	if got.Command != "submit_prompt" || got.RequestID != "r-7" || got.Workspace != "/ws" {
		t.Fatalf("sample identity = %+v, want submit_prompt/r-7//ws", got)
	}
}

func TestReadLoopSampleReportsTheQueueDepthAtReceipt(t *testing.T) {
	// Arrange.
	latency := &recordingLatency{}
	cfg := Config{
		Logf: testLogf(t), LogVerbosef: testLogf(t),
		State: staticState{snap: sampleSnapshot()}, Handler: &mockHandler{},
		CommandLatency: latency, AckWarnThreshold: time.Hour,
	}

	// Act. One connection dispatching alone must report a depth of exactly
	// one: itself, and nothing it waited behind.
	runOneCommand(t, cfg, submitCmd("r-1", "/ws"))

	// Assert.
	if got := latency.samples[0].QueueDepth; got != 1 {
		t.Fatalf("queue depth for a lone command = %d, want 1 (itself)", got)
	}
}

func TestReadLoopReleasesTheInflightGaugeAfterEachCommand(t *testing.T) {
	// Arrange.
	cfg := Config{
		Logf: testLogf(t), LogVerbosef: testLogf(t),
		State: staticState{snap: sampleSnapshot()}, Handler: &mockHandler{},
		CommandLatency: &recordingLatency{}, AckWarnThreshold: time.Hour,
	}

	// Act.
	s := runOneCommand(t, cfg, submitCmd("r-1", "/ws"))

	// Assert. A gauge that only climbs would report every later command as
	// queued behind work that had already finished.
	if got := s.inflight.Load(); got != 0 {
		t.Fatalf("in-flight gauge after a completed command = %d, want 0", got)
	}
}

func TestReadLoopSampleCarriesTheAckVerdict(t *testing.T) {
	// Arrange.
	latency := &recordingLatency{}
	cfg := Config{
		Logf: testLogf(t), LogVerbosef: testLogf(t),
		State: staticState{snap: sampleSnapshot()},
		// A refused command must still be timed, and distinguishable from a
		// slow success.
		Handler:        &mockHandler{err: errors.New("refused")},
		CommandLatency: latency, AckWarnThreshold: time.Hour,
	}

	// Act.
	runOneCommand(t, cfg, submitCmd("r-1", "/ws"))

	// Assert.
	if latency.samples[0].Ok {
		t.Fatal("sample.Ok = true for a refused command, want false")
	}
}

func TestReadLoopSurfacesARecorderFailure(t *testing.T) {
	// Arrange.
	logs := &concurrentLogs{}
	cfg := Config{
		Logf:        logs.logf,
		LogVerbosef: testLogf(t),
		State:       staticState{snap: sampleSnapshot()}, Handler: &mockHandler{},
		CommandLatency:   &recordingLatency{err: errors.New("workspace routing failure")},
		AckWarnThreshold: time.Hour,
	}

	// Act.
	runOneCommand(t, cfg, submitCmd("r-1", "/ws"))

	// Assert. A telemetry write that failed must never be silent.
	lines := logs.all()
	var found bool
	for _, line := range lines {
		if strings.Contains(line, "record command latency FAILED") && strings.Contains(line, "workspace routing failure") {
			found = true
		}
	}
	if !found {
		t.Fatalf("logs = %v, want a loud recorder-failure line", lines)
	}
}

func TestReadLoopWithoutARecorderStillCompletesTheCommand(t *testing.T) {
	// Arrange. An unwired recorder is a missing telemetry capability, never a
	// reason to stop serving commands.
	handler := &mockHandler{}
	cfg := Config{
		Logf: testLogf(t), LogVerbosef: testLogf(t),
		State: staticState{snap: sampleSnapshot()}, Handler: handler,
	}

	// Act.
	runOneCommand(t, cfg, submitCmd("r-1", "/ws"))

	// Assert.
	if handler.called != "submit_prompt" {
		t.Fatalf("handler.called = %q, want the command dispatched anyway", handler.called)
	}
}

func TestNewFallsBackToTheDefaultAckWarnThreshold(t *testing.T) {
	// Arrange, Act.
	s := New(Config{
		Logf: testLogf(t), LogVerbosef: testLogf(t),
		State: staticState{snap: sampleSnapshot()}, Handler: &mockHandler{},
		CommandLatency: &recordingLatency{},
	})

	// Assert.
	if s.ackWarn != DefaultAckWarnThreshold {
		t.Fatalf("ackWarn with no configured threshold = %s, want %s", s.ackWarn, DefaultAckWarnThreshold)
	}
}

func TestCommandFieldNameNamesTheSetOneofArm(t *testing.T) {
	tests := []struct {
		name string
		cmd  *frontendv1.FrontendCommand
		want string
	}{
		{
			name: "a submit prompt",
			cmd:  submitCmd("r", "/ws"),
			want: "submit_prompt",
		},
		{
			name: "an open workspace",
			cmd: &frontendv1.FrontendCommand{Command: &frontendv1.FrontendCommand_OpenWorkspace{
				OpenWorkspace: &frontendv1.OpenWorkspaceCmd{},
			}},
			want: "open_workspace",
		},
		{
			name: "a daemon health probe",
			cmd: &frontendv1.FrontendCommand{Command: &frontendv1.FrontendCommand_DaemonHealth{
				DaemonHealth: &frontendv1.DaemonHealthCmd{},
			}},
			want: "daemon_health",
		},
		{
			name: "an empty oneof is named, never blank",
			cmd:  &frontendv1.FrontendCommand{RequestId: "r"},
			want: "unset",
		},
		{
			name: "a nil command is named, never blank",
			cmd:  nil,
			want: "nil",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			got := CommandFieldName(tc.cmd)

			// Assert.
			if got != tc.want {
				t.Fatalf("CommandFieldName = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestAckWarnFromEnvReadsAnUnsetEnvironmentAsTheDefault(t *testing.T) {
	// Arrange.
	t.Setenv(EnvAckWarnMs, "")

	// Act.
	got, err := AckWarnFromEnv()

	// Assert.
	if err != nil || got != DefaultAckWarnThreshold {
		t.Fatalf("AckWarnFromEnv() = (%s, %v), want (%s, nil)", got, err, DefaultAckWarnThreshold)
	}
}

func TestAckWarnFromEnvReadsAnOverride(t *testing.T) {
	// Arrange.
	t.Setenv(EnvAckWarnMs, "750")

	// Act.
	got, err := AckWarnFromEnv()

	// Assert.
	if err != nil || got != 750*time.Millisecond {
		t.Fatalf("AckWarnFromEnv() = (%s, %v), want (750ms, nil)", got, err)
	}
}

func TestAckWarnFromEnvRefusesAMalformedValue(t *testing.T) {
	// Arrange.
	t.Setenv(EnvAckWarnMs, "2s")

	// Act.
	_, err := AckWarnFromEnv()

	// Assert.
	if err == nil {
		t.Fatal("AckWarnFromEnv() with a non-integer = nil error, want a loud refusal")
	}
}

func TestAckWarnFromEnvRefusesZeroRatherThanDefaulting(t *testing.T) {
	// Arrange.
	t.Setenv(EnvAckWarnMs, "0")

	// Act.
	_, err := AckWarnFromEnv()

	// Assert. Defaulting here would run a threshold the operator believes they
	// disabled.
	if err == nil {
		t.Fatal("AckWarnFromEnv() with zero = nil error, want a loud refusal")
	}
}
