package frontend

import (
	"errors"
	"fmt"
	"io"
	"strings"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// recordingLatency captures every sample the transport hands it, and can be
// asked to fail so the transport's error surfacing is exercised.
type recordingLatency struct {
	samples []CommandLatencySample
	err     error
}

func (r *recordingLatency) RecordCommandLatency(sample CommandLatencySample) error {
	r.samples = append(r.samples, sample)
	return r.err
}

// scriptedConn replays a fixed command list to readLoop and then reports EOF,
// which is the loop's ordinary termination. Nothing here waits on a clock: the
// loop returns when the script is exhausted, so a test rendezvouses with the
// loop's own completion rather than with elapsed time.
type scriptedConn struct {
	commands []*frontendv1.FrontendCommand
	written  [][]byte
}

func (c *scriptedConn) writeFrame(data []byte) error {
	c.written = append(c.written, data)
	return nil
}

func (c *scriptedConn) readCommand() (*frontendv1.FrontendCommand, error) {
	if len(c.commands) == 0 {
		return nil, io.EOF
	}
	next := c.commands[0]
	c.commands = c.commands[1:]
	return next, nil
}

func (c *scriptedConn) close() error { return nil }

func submitCommand(workspace, requestID string) *frontendv1.FrontendCommand {
	return &frontendv1.FrontendCommand{
		RequestId: requestID,
		Workspace: workspace,
		Command: &frontendv1.FrontendCommand_SubmitPrompt{
			SubmitPrompt: &frontendv1.SubmitPromptCmd{Text: "hi"},
		},
	}
}

// runOneCommand drives readLoop over a single scripted command and returns the
// samples the recorder collected.
func runOneCommand(t *testing.T, cfg Config, cmd *frontendv1.FrontendCommand) *Server {
	t.Helper()
	s := New(cfg)
	cl := newClient(4, nil, ClientKindHost)
	s.clients[cl] = struct{}{}
	s.readLoop(&scriptedConn{commands: []*frontendv1.FrontendCommand{cmd}}, cl)
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
			runOneCommand(t, cfg, submitCommand("/ws", "r-1"))

			// Assert.
			if len(latency.samples) != 1 {
				t.Fatalf("latency samples = %d, want exactly one per completed command", len(latency.samples))
			}
			if got := latency.samples[0].Slow(); got != tc.wantSlow {
				t.Fatalf("sample.Slow() with threshold %s = %v, want %v (ack=%s)",
					tc.threshold, got, tc.wantSlow, latency.samples[0].Ack)
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
	runOneCommand(t, cfg, submitCommand("/ws", "r-7"))

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
	runOneCommand(t, cfg, submitCommand("/ws", "r-1"))

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
	s := runOneCommand(t, cfg, submitCommand("/ws", "r-1"))

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
	runOneCommand(t, cfg, submitCommand("/ws", "r-1"))

	// Assert.
	if latency.samples[0].Ok {
		t.Fatal("sample.Ok = true for a refused command, want false")
	}
}

func TestReadLoopSurfacesARecorderFailure(t *testing.T) {
	// Arrange.
	var logs []string
	cfg := Config{
		Logf:        func(format string, args ...any) { logs = append(logs, fmt.Sprintf(format, args...)) },
		LogVerbosef: testLogf(t),
		State:       staticState{snap: sampleSnapshot()}, Handler: &mockHandler{},
		CommandLatency:   &recordingLatency{err: errors.New("workspace routing failure")},
		AckWarnThreshold: time.Hour,
	}

	// Act.
	runOneCommand(t, cfg, submitCommand("/ws", "r-1"))

	// Assert. A telemetry write that failed must never be silent.
	var found bool
	for _, line := range logs {
		if strings.Contains(line, "record command latency FAILED") && strings.Contains(line, "workspace routing failure") {
			found = true
		}
	}
	if !found {
		t.Fatalf("logs = %v, want a loud recorder-failure line", logs)
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
	runOneCommand(t, cfg, submitCommand("/ws", "r-1"))

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
			cmd:  submitCommand("/ws", "r"),
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
