package frontend

import (
	"testing"
	"time"
)

// ticketTestDeadline bounds how long a test waits for an emission a correct
// implementation produces promptly. It is a FAILURE deadline, never a
// synchronization device: every rendezvous below is a channel.
const ticketTestDeadline = 5 * time.Second

// awaitSample receives one recorded sample or fails the test.
func awaitSample(t *testing.T, notify chan CommandLatencySample) CommandLatencySample {
	t.Helper()
	select {
	case sample := <-notify:
		return sample
	case <-time.After(ticketTestDeadline):
		t.Fatal("no latency sample was recorded before the failure deadline")
		return CommandLatencySample{}
	}
}

// newTicketServer builds a transport wired to a recording latency sink, with an
// ack deadline the test picks so the overdue branch is reachable without
// waiting out the production one.
func newTicketServer(t *testing.T, latency *recordingLatency, deadline time.Duration) *Server {
	t.Helper()
	return New(Config{
		Logf: testLogf(t), LogVerbosef: testLogf(t),
		State: staticState{snap: sampleSnapshot()}, Handler: &mockHandler{},
		CommandLatency: latency, AckWarnThreshold: time.Hour, AckDeadline: deadline,
	})
}

func TestNewFallsBackToTheClientAckDeadline(t *testing.T) {
	// Arrange, Act. An unset deadline must be the budget the Emacs client
	// itself enforces, not some transport-local guess.
	s := New(Config{
		Logf: testLogf(t), LogVerbosef: testLogf(t),
		State: staticState{snap: sampleSnapshot()}, Handler: &mockHandler{},
	})

	// Assert.
	if s.ackDeadline != CommandAckDeadline {
		t.Fatalf("ackDeadline = %s, want %s", s.ackDeadline, CommandAckDeadline)
	}
}

func TestTicketReleasesTheGaugeWhenTheDispatchPanics(t *testing.T) {
	// Arrange: a handler that panics is the exit the old un-deferred decrement
	// skipped, leaking a queue_depth for the rest of the daemon's life.
	latency := &recordingLatency{}
	h := newLaneHandler()
	h.open = func(string) error { panic("frontend test: the handler exploded") }
	s := New(Config{
		Logf: testLogf(t), LogVerbosef: testLogf(t),
		State: staticState{snap: sampleSnapshot()}, Handler: h,
		CommandLatency: latency, AckWarnThreshold: time.Hour,
	})
	cl := newClient(defaultClientBuffer, nil, ClientKindHost)
	ticket := s.newCommandTicket(cl, openCmd("boom", "/ws/a"), time.Now(), s.inflight.Add(1))

	// Act.
	recovered := func() (v any) {
		defer func() { v = recover() }()
		s.processCommand(ticket)
		return nil
	}()

	// Assert: the panic still propagates AND the gauge came back.
	if recovered == nil {
		t.Fatal("processCommand swallowed the handler's panic, want it to propagate")
	}
	if depth := s.inflight.Load(); depth != 0 {
		t.Fatalf("in-flight gauge after a panicking dispatch = %d, want 0", depth)
	}
}

func TestTicketRecordsTheCompletionWhenTheDispatchPanics(t *testing.T) {
	// Arrange.
	latency := &recordingLatency{}
	h := newLaneHandler()
	h.open = func(string) error { panic("frontend test: the handler exploded") }
	s := New(Config{
		Logf: testLogf(t), LogVerbosef: testLogf(t),
		State: staticState{snap: sampleSnapshot()}, Handler: h,
		CommandLatency: latency, AckWarnThreshold: time.Hour,
	})
	cl := newClient(defaultClientBuffer, nil, ClientKindHost)
	ticket := s.newCommandTicket(cl, openCmd("boom", "/ws/a"), time.Now(), s.inflight.Add(1))

	// Act.
	func() {
		defer func() { _ = recover() }()
		s.processCommand(ticket)
	}()

	// Assert: exactly one record, on the path that used to produce none.
	samples := latency.all()
	if len(samples) != 1 {
		t.Fatalf("latency samples after a panicking dispatch = %d, want exactly one", len(samples))
	}
	if samples[0].RequestID != "boom" || samples[0].Overdue {
		t.Fatalf("sample = %+v, want the completion record for request boom", samples[0])
	}
}

func TestTicketAnnouncesACommandStillInFlightPastTheAckDeadline(t *testing.T) {
	// Arrange: a bring-up that never returns until released. Its overdue
	// record must arrive WHILE it is still running.
	latency := &recordingLatency{notify: make(chan CommandLatencySample, 4)}
	s := newTicketServer(t, latency, time.Millisecond)
	cl := newClient(defaultClientBuffer, nil, ClientKindHost)
	ticket := s.newCommandTicket(cl, openCmd("wedged", "/ws/a"), time.Now(), s.inflight.Add(1))

	// Act.
	sample := awaitSample(t, latency.notify)

	// Assert.
	if !sample.Overdue {
		t.Fatalf("sample.Overdue = false, want the in-flight announcement (%+v)", sample)
	}
	if sample.RequestID != "wedged" || sample.Command != "open_workspace" {
		t.Fatalf("sample identity = %q/%q, want wedged/open_workspace", sample.RequestID, sample.Command)
	}
	ticket.finish(nil, 0)
}

func TestTicketKeepsAnOverdueCommandCountedInTheGauge(t *testing.T) {
	// Arrange: the announcement is not a completion — the command is still
	// running, so the depth it contributes is real and must stay.
	latency := &recordingLatency{notify: make(chan CommandLatencySample, 4)}
	s := newTicketServer(t, latency, time.Millisecond)
	cl := newClient(defaultClientBuffer, nil, ClientKindHost)
	ticket := s.newCommandTicket(cl, openCmd("wedged", "/ws/a"), time.Now(), s.inflight.Add(1))

	// Act.
	awaitSample(t, latency.notify)

	// Assert.
	if depth := s.inflight.Load(); depth != 1 {
		t.Fatalf("in-flight gauge while the command is still running = %d, want 1", depth)
	}
	ticket.finish(nil, 0)
}

func TestTicketStillRecordsTheCompletionOfAnOverdueCommand(t *testing.T) {
	// Arrange: a command that outlives its deadline still owes exactly one
	// completion record when it finally finishes.
	latency := &recordingLatency{notify: make(chan CommandLatencySample, 4)}
	s := newTicketServer(t, latency, time.Millisecond)
	cl := newClient(defaultClientBuffer, nil, ClientKindHost)
	ticket := s.newCommandTicket(cl, openCmd("wedged", "/ws/a"), time.Now(), s.inflight.Add(1))
	awaitSample(t, latency.notify)

	// Act.
	ticket.finish(nil, 7*time.Millisecond)

	// Assert.
	completion := awaitSample(t, latency.notify)
	if completion.Overdue {
		t.Fatalf("second sample = %+v, want the completion record", completion)
	}
	if depth := s.inflight.Load(); depth != 0 {
		t.Fatalf("in-flight gauge after completion = %d, want 0", depth)
	}
}

func TestTicketLeavesAPromptCommandUnannounced(t *testing.T) {
	// Arrange: a command that beat its deadline must produce no overdue record.
	// An unreachable deadline is deterministic without a clock.
	latency := &recordingLatency{}
	s := newTicketServer(t, latency, time.Hour)
	cl := newClient(defaultClientBuffer, nil, ClientKindHost)
	ticket := s.newCommandTicket(cl, openCmd("prompt", "/ws/a"), time.Now(), s.inflight.Add(1))

	// Act.
	ticket.finish(nil, time.Millisecond)

	// Assert.
	samples := latency.all()
	if len(samples) != 1 || samples[0].Overdue {
		t.Fatalf("samples = %+v, want exactly one completion and no announcement", samples)
	}
}

func TestTicketSettlesOnlyOnce(t *testing.T) {
	// Arrange: a second settle must neither double-decrement the gauge below
	// the commands genuinely in flight nor write a second completion record.
	latency := &recordingLatency{}
	s := newTicketServer(t, latency, time.Hour)
	cl := newClient(defaultClientBuffer, nil, ClientKindHost)
	ticket := s.newCommandTicket(cl, openCmd("once", "/ws/a"), time.Now(), s.inflight.Add(1))

	// Act.
	ticket.finish(nil, time.Millisecond)
	ticket.finish(nil, time.Millisecond)

	// Assert.
	if depth := s.inflight.Load(); depth != 0 {
		t.Fatalf("in-flight gauge after a repeated settle = %d, want 0", depth)
	}
	if samples := latency.all(); len(samples) != 1 {
		t.Fatalf("latency samples = %d, want exactly one completion per received command", len(samples))
	}
}

func TestTicketDoesNotAnnounceACommandThatAlreadySettled(t *testing.T) {
	// Arrange: a watchdog that fires after the command completed must stay
	// silent, so an announcement always names something genuinely running.
	latency := &recordingLatency{}
	s := newTicketServer(t, latency, time.Hour)
	cl := newClient(defaultClientBuffer, nil, ClientKindHost)
	ticket := s.newCommandTicket(cl, openCmd("done", "/ws/a"), time.Now(), s.inflight.Add(1))
	ticket.finish(nil, time.Millisecond)

	// Act: the fire the stopped timer would have made.
	ticket.reportOverdue()

	// Assert.
	samples := latency.all()
	if len(samples) != 1 || samples[0].Overdue {
		t.Fatalf("samples = %+v, want only the completion record", samples)
	}
}
