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

// --- the delivery measurement -----------------------------------------------
//
// The alarm used to stop at the ack ENQUEUE, so it saw a 12ms command and
// stayed silent while that command's ack sat 15 seconds deep in the outbound
// queue. These pin the measurement to the socket write instead.

func TestCommandLatencySampleSlowJudgesTheDeliveryNotTheEnqueue(t *testing.T) {
	tests := []struct {
		name      string
		enqueue   time.Duration
		delivery  time.Duration
		threshold time.Duration
		wantSlow  bool
	}{
		{
			// The incident exactly: the daemon's own share was trivial and the
			// drain was the whole round trip. Judging the enqueue called this
			// fast while the client's deadline was expiring.
			name:      "a fast enqueue behind a slow drain is slow",
			enqueue:   12 * time.Millisecond,
			delivery:  15 * time.Second,
			threshold: 2 * time.Second,
			wantSlow:  true,
		},
		{
			name:      "a fast enqueue with a fast drain is not slow",
			enqueue:   12 * time.Millisecond,
			delivery:  20 * time.Millisecond,
			threshold: 2 * time.Second,
			wantSlow:  false,
		},
		{
			// The pre-existing case, still caught: a slow HANDLER shows up as a
			// delivery that is almost entirely its own enqueue.
			name:      "a slow handler is still slow",
			enqueue:   3 * time.Second,
			delivery:  3 * time.Second,
			threshold: 2 * time.Second,
			wantSlow:  true,
		},
		{
			name:      "no threshold configured never warns",
			enqueue:   time.Millisecond,
			delivery:  time.Hour,
			threshold: 0,
			wantSlow:  false,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			sample := CommandLatencySample{
				Enqueue: tc.enqueue, Delivery: tc.delivery,
				Delivered: true, Threshold: tc.threshold,
			}

			// Act.
			got := sample.Slow()

			// Assert.
			if got != tc.wantSlow {
				t.Fatalf("Slow() with enqueue=%s delivery=%s threshold=%s = %v, want %v",
					tc.enqueue, tc.delivery, tc.threshold, got, tc.wantSlow)
			}
		})
	}
}

// gatedConn holds every write until the test releases it, so a slow DRAIN can
// be arranged without a clock: the frame is queued, the writer is inside
// writeFrame, and nothing but the test can let it finish.
type gatedConn struct {
	entered chan []byte
	release chan error
	closed  chan struct{}
	once    sync.Once
}

func newGatedConn() *gatedConn {
	return &gatedConn{
		entered: make(chan []byte, 8),
		release: make(chan error, 8),
		closed:  make(chan struct{}),
	}
}

func (c *gatedConn) readCommand() (*frontendv1.FrontendCommand, error) {
	<-c.closed
	return nil, errors.New("frontend test: gated connection has no command script")
}

func (c *gatedConn) writeFrame(data []byte, _ func()) error {
	c.entered <- append([]byte(nil), data...)
	return <-c.release
}

func (c *gatedConn) close(closeCause) error {
	c.once.Do(func() { close(c.closed) })
	return nil
}

// awaitWrite rendezvouses with the writer entering writeFrame, so a test knows
// the writer is parked rather than assuming it.
func (c *gatedConn) awaitWrite(t *testing.T) {
	t.Helper()
	select {
	case <-c.entered:
	case <-time.After(ticketTestDeadline):
		t.Fatal("the writer never reached writeFrame before the failure deadline")
	}
}

func TestCommandLatencyIsRecordedOnlyOnceTheAckReachesTheSocket(t *testing.T) {
	// Arrange: a writer parked inside writeFrame on a bulk frame, which is the
	// backlog an ack used to queue behind.
	latency := &recordingLatency{notify: make(chan CommandLatencySample, 4)}
	s := newTicketServer(t, latency, time.Hour)
	cl := newClient(defaultClientBuffer, nil, ClientKindHost)
	c := newGatedConn()
	go s.writeLoop(c, cl)
	s.enqueue(cl, outFrame{data: []byte(`{"bulk":true}`)})
	c.awaitWrite(t)

	// Act: the command runs to completion and its ack is queued behind that
	// held write. processCommand has RETURNED, so the dispatch is done.
	ticket := s.newCommandTicket(cl, openCmd("held", "/ws/a"), time.Now(), s.inflight.Add(1))
	s.processCommand(ticket)

	// Assert: no record yet. The daemon is finished with the command, but the
	// client has not seen a byte of the answer, and the record that used to be
	// written here is what made a 15s wait look like a 12ms one.
	if samples := latency.all(); len(samples) != 0 {
		t.Fatalf("samples while the ack is still queued = %+v, want none until it is written", samples)
	}

	// Act: let the writer through.
	c.release <- nil
	c.awaitWrite(t)
	c.release <- nil

	// Assert.
	sample := awaitSample(t, latency.notify)
	if !sample.Delivered || sample.DeliveryError != "" {
		t.Fatalf("sample = %+v, want a delivered ack", sample)
	}
	if sample.Delivery < sample.Enqueue {
		t.Fatalf("delivery %s < enqueue %s, want the drain counted on top of the daemon's own share",
			sample.Delivery, sample.Enqueue)
	}
}

func TestCommandLatencyReportsAnAckThatNeverReachedTheClient(t *testing.T) {
	// Arrange: a command whose ack is queued for a connection that then dies.
	latency := &recordingLatency{notify: make(chan CommandLatencySample, 4)}
	s := newTicketServer(t, latency, time.Hour)
	cl := newClient(defaultClientBuffer, nil, ClientKindHost)
	ticket := s.newCommandTicket(cl, openCmd("lost", "/ws/a"), time.Now(), s.inflight.Add(1))
	s.processCommand(ticket)

	// Act: the connection goes before any writer drained it.
	s.disconnect(cl, causeServerShutdown)

	// Assert: the record still happens, and it says the client never saw the
	// ack rather than reporting a delivery that did not occur.
	sample := awaitSample(t, latency.notify)
	if sample.Delivered {
		t.Fatalf("sample = %+v, want the ack reported undelivered", sample)
	}
	if sample.DeliveryError == "" {
		t.Fatal("sample carries no delivery error, want the reason the ack never landed")
	}
}

func TestHostKindWidensTheDefaultAckThreshold(t *testing.T) {
	got := AckWarnThresholdFor("host", DefaultAckWarnThreshold)
	if got != HostAckWarnThreshold {
		t.Fatalf("host default threshold = %v, want %v", got, HostAckWarnThreshold)
	}
}

func TestGuiKindKeepsTheDefaultAckThreshold(t *testing.T) {
	got := AckWarnThresholdFor("gui", DefaultAckWarnThreshold)
	if got != DefaultAckWarnThreshold {
		t.Fatalf("gui default threshold = %v, want %v", got, DefaultAckWarnThreshold)
	}
}

func TestAnOperatorAckThresholdAppliesVerbatimToTheHost(t *testing.T) {
	operator := 1 * time.Second
	got := AckWarnThresholdFor("host", operator)
	if got != operator {
		t.Fatalf("host operator threshold = %v, want the operator's own %v", got, operator)
	}
}
