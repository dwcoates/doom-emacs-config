package frontend

import (
	"errors"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// drainAt builds a window over a fixed pair of instants, so every assertion
// about its edges is a statement about arithmetic rather than about how fast
// the test machine happened to run.
func drainAt(open time.Time, close *time.Time) *snapshotDrain {
	d := &snapshotDrain{}
	d.open(open)
	if close != nil {
		d.closeAt(*close)
	}
	return d
}

func TestSnapshotDrainOverlap(t *testing.T) {
	base := time.Unix(1700000000, 0)
	opened := base
	closed := base.Add(4 * time.Second)
	tests := []struct {
		name  string
		drain *snapshotDrain
		start time.Time
		end   time.Time
		want  bool
	}{
		{
			// The connection this sample rode never enqueued a snapshot, which
			// serveClient makes impossible. An unexplained slow ack must keep
			// warning rather than inherit an explanation nothing established.
			name:  "a drain that never opened explains nothing",
			drain: &snapshotDrain{},
			start: base,
			end:   base.Add(4 * time.Second),
			want:  false,
		},
		{
			name:  "a sample delivered while the backlog is still draining overlaps",
			drain: drainAt(opened, nil),
			start: base.Add(time.Second),
			end:   base.Add(5 * time.Second),
			want:  true,
		},
		{
			name:  "a sample straddling the moment the outbox ran dry overlaps",
			drain: drainAt(opened, &closed),
			start: base.Add(time.Second),
			end:   base.Add(9 * time.Second),
			want:  true,
		},
		{
			// The closing edge is exclusive: a command whose wait began at the
			// instant the backlog cleared was not waiting on the bring-up.
			name:  "a sample beginning exactly when the outbox ran dry is outside",
			drain: drainAt(opened, &closed),
			start: closed,
			end:   closed.Add(3 * time.Second),
			want:  false,
		},
		{
			name:  "a sample entirely after the drain is outside",
			drain: drainAt(opened, &closed),
			start: closed.Add(time.Second),
			end:   closed.Add(5 * time.Second),
			want:  false,
		},
		{
			name:  "a sample entirely before the drain opened is outside",
			drain: drainAt(opened, &closed),
			start: base.Add(-5 * time.Second),
			end:   base.Add(-time.Second),
			want:  false,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := tc.drain.overlaps(tc.start, tc.end)

			// Assert.
			if got != tc.want {
				t.Fatalf("overlaps(%v, %v) = %t, want %t", tc.start, tc.end, got, tc.want)
			}
		})
	}
}

// stormConn records, at the moment of every write, whether the connection's
// boot-drain window was still open. It is how a test observes the window from
// INSIDE the drain — while frames are still owed — without a clock and without
// a second goroutine.
type stormConn struct {
	cl        *client
	openWhile []bool
	wrote     [][]byte
}

func (c *stormConn) readCommand() (*frontendv1.FrontendCommand, error) {
	return nil, errors.New("frontend test: storm connection has no command script")
}

func (c *stormConn) writeFrame(data []byte, _ func()) error {
	now := time.Now()
	c.openWhile = append(c.openWhile, c.cl.drain.overlaps(now, now))
	c.wrote = append(c.wrote, append([]byte(nil), data...))
	return nil
}

func (c *stormConn) close() error { return nil }

// newBringUpStorm arranges a connection mid bring-up: the window is open and
// the connect snapshot plus the retained pushes that follow it — the roster,
// the catalogs, the views — are all queued behind it. That is the shape the
// 4343ms roster ack was measured in.
func newBringUpStorm(t *testing.T, latency *recordingLatency) (*Server, *client, *stormConn) {
	t.Helper()
	s := New(Config{
		Logf: testLogf(t), LogVerbosef: testLogf(t),
		State: staticState{snap: sampleSnapshot()}, Handler: &mockHandler{},
		CommandLatency: latency,
		// A one-nanosecond threshold makes every real dispatch slow without a
		// clock; an hour-long ack deadline keeps the overdue watchdog out of it.
		AckWarnThreshold: time.Nanosecond, AckDeadline: time.Hour,
	})
	cl := newClient(defaultClientBuffer, nil, ClientKindHost)
	cl.drain.open(time.Now())
	for _, data := range [][]byte{[]byte(`{"snapshot":true}`), []byte(`{"roster":true}`), []byte(`{"catalog":true}`)} {
		if res := enqueueLocked(cl, outFrame{data: data}); !res.queued {
			t.Fatalf("bring-up push = %#v, want queued", res)
		}
	}
	return s, cl, &stormConn{cl: cl}
}

func TestSnapshotDrainStaysOpenWhileTheBringUpStormIsStillGoingOut(t *testing.T) {
	// Arrange. The connect snapshot is only the FIRST frame of the storm. The
	// window that closed at its write completion left the roster ack behind it
	// unclassified, which is the bug this edge exists to fix.
	s, cl, c := newBringUpStorm(t, nil)

	// Act.
	if err := s.drainOutbox(c, cl); err != nil {
		t.Fatalf("drainOutbox = %v, want nil", err)
	}

	// Assert. Every frame of the storm, not just the snapshot, was written with
	// the window still open.
	if len(c.openWhile) != 3 {
		t.Fatalf("writes = %d, want the three queued bring-up frames", len(c.openWhile))
	}
	for i, open := range c.openWhile {
		if !open {
			t.Fatalf("window was closed during bring-up write %d, want it open until the outbox runs dry", i)
		}
	}
}

func TestSnapshotDrainClosesWhenTheOutboxFirstRunsDry(t *testing.T) {
	// Arrange.
	s, cl, c := newBringUpStorm(t, nil)

	// Act. One wake of the writer drains the whole backlog and finds the queue
	// exhausted, which is the closing edge — the transport's own seam, driven
	// synchronously rather than waited out.
	if err := s.drainOutbox(c, cl); err != nil {
		t.Fatalf("drainOutbox = %v, want nil", err)
	}

	// Assert.
	after := time.Now()
	if cl.drain.overlaps(after, after) {
		t.Fatal("overlaps = true after the outbox ran dry, want false")
	}
}

func TestSnapshotDrainKeepsTheFirstDryMomentWhenTheOutboxEmptiesAgain(t *testing.T) {
	// Arrange. Later quiet periods are ordinary running, not a second bring-up,
	// so the window must not slide forward onto them.
	s, cl, c := newBringUpStorm(t, nil)
	if err := s.drainOutbox(c, cl); err != nil {
		t.Fatalf("drainOutbox = %v, want nil", err)
	}
	closedAfterBringUp := time.Now()

	// Act.
	if res := enqueueLocked(cl, outFrame{data: []byte(`{"later":true}`)}); !res.queued {
		t.Fatalf("later push = %#v, want queued", res)
	}
	if err := s.drainOutbox(c, cl); err != nil {
		t.Fatalf("drainOutbox = %v, want nil", err)
	}

	// Assert. The later traffic was written OUTSIDE the window.
	if c.openWhile[len(c.openWhile)-1] {
		t.Fatal("window was open during a post-bring-up write, want it closed at the first dry moment")
	}
	if cl.drain.overlaps(closedAfterBringUp, time.Now()) {
		t.Fatal("overlaps = true for an interval after the first dry moment, want false")
	}
}

func TestSnapshotDrainStaysOpenWhenTheOutboxNeverRunsDry(t *testing.T) {
	// Arrange. A connection whose backlog never clears keeps its window open,
	// and that is honest rather than lenient: the storm never ended.
	_, cl, _ := newBringUpStorm(t, nil)

	// Act.
	now := time.Now()

	// Assert.
	if !cl.drain.overlaps(now, now) {
		t.Fatal("overlaps = false with the whole bring-up backlog still queued, want true")
	}
}

func TestCommandLatencySampleNamesTheBootDrainWindowForAnAckBehindTheStorm(t *testing.T) {
	// Arrange. This is the observed shape exactly: a command answered while the
	// connect snapshot AND the retained pushes behind it are still going out,
	// its ack delivered behind that traffic. The window that ended at the
	// snapshot's own write left this sample unclassified and warning.
	latency := &recordingLatency{}
	s, cl, c := newBringUpStorm(t, latency)

	// Act. The command completes and its ack joins the outbox behind the storm;
	// one wake of the writer then puts every byte on the socket, which is what
	// ends the client's wait and writes the sample.
	s.processCommand(s.newCommandTicket(cl, submitCmd("r-1", "/ws"), time.Now(), s.inflight.Add(1)))
	if err := s.drainOutbox(c, cl); err != nil {
		t.Fatalf("drainOutbox = %v, want nil", err)
	}

	// Assert.
	samples := latency.all()
	if len(samples) != 1 {
		t.Fatalf("samples = %d, want exactly one", len(samples))
	}
	if !samples[0].Slow() || !samples[0].Delivered {
		t.Fatalf("sample slow/delivered = %t/%t, want a delivered sample over threshold",
			samples[0].Slow(), samples[0].Delivered)
	}
	if samples[0].Decision != BootSnapshotDrainDecision {
		t.Fatalf("sample.Decision = %q, want %q", samples[0].Decision, BootSnapshotDrainDecision)
	}
}

func TestCommandLatencySampleCarriesNoDecisionOnceTheStormHasDrained(t *testing.T) {
	// Arrange. The bring-up backlog is fully written before this command is
	// received, so nothing structural explains a slow ack and the record must
	// warn exactly as it always did.
	latency := &recordingLatency{}
	s, cl, c := newBringUpStorm(t, latency)
	if err := s.drainOutbox(c, cl); err != nil {
		t.Fatalf("drainOutbox = %v, want nil", err)
	}

	// Act.
	s.processCommand(s.newCommandTicket(cl, submitCmd("r-1", "/ws"), time.Now(), s.inflight.Add(1)))
	if err := s.drainOutbox(c, cl); err != nil {
		t.Fatalf("drainOutbox = %v, want nil", err)
	}

	// Assert.
	samples := latency.all()
	if len(samples) != 1 {
		t.Fatalf("samples = %d, want exactly one", len(samples))
	}
	if !samples[0].Slow() {
		t.Fatalf("sample.Slow() = false against a one-nanosecond threshold, want true")
	}
	if samples[0].Decision != "" {
		t.Fatalf("sample.Decision = %q, want an unexplained sample to carry none", samples[0].Decision)
	}
}

func TestCommandLatencySampleCarriesNoDecisionWithoutAConnectSnapshot(t *testing.T) {
	// Arrange. A connection that never enqueued a snapshot cannot have been
	// draining one. serveClient makes this unreachable; the record warns rather
	// than inventing an explanation if it ever happens.
	latency := &recordingLatency{}
	cfg := Config{
		Logf: testLogf(t), LogVerbosef: testLogf(t),
		State: staticState{snap: sampleSnapshot()}, Handler: &mockHandler{},
		CommandLatency: latency, AckWarnThreshold: time.Nanosecond,
	}

	// Act.
	runOneCommand(t, cfg, submitCmd("r-1", "/ws"))

	// Assert.
	samples := latency.all()
	if len(samples) != 1 {
		t.Fatalf("samples = %d, want exactly one", len(samples))
	}
	if samples[0].Decision != "" {
		t.Fatalf("sample.Decision = %q, want none", samples[0].Decision)
	}
}
