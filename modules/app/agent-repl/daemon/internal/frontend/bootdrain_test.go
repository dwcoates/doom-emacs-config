package frontend

import (
	"testing"
	"time"
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
			name:  "a sample delivered while the snapshot is still draining overlaps",
			drain: drainAt(opened, nil),
			start: base.Add(time.Second),
			end:   base.Add(5 * time.Second),
			want:  true,
		},
		{
			name:  "a sample straddling the write completion overlaps",
			drain: drainAt(opened, &closed),
			start: base.Add(time.Second),
			end:   base.Add(9 * time.Second),
			want:  true,
		},
		{
			// The closing edge is exclusive: a command whose wait began at the
			// instant the snapshot finished was not waiting on that write.
			name:  "a sample beginning exactly at the write completion is outside",
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

func TestSnapshotDrainStaysOpenWhileTheSnapshotIsQueued(t *testing.T) {
	// Arrange. The frame is queued and nothing has drained it, which is exactly
	// the daemon's state while a 104-workspace snapshot waits on the writer.
	cl := newClient(defaultClientBuffer, nil, ClientKindHost)
	cl.drain.open(time.Now())
	if res := enqueueLocked(cl, outFrame{data: []byte("snapshot"), notify: cl.drain.snapshotDisposed}); !res.queued {
		t.Fatalf("connect snapshot push = %#v, want queued", res)
	}

	// Act.
	now := time.Now()

	// Assert.
	if !cl.drain.overlaps(now, now) {
		t.Fatal("overlaps = false while the connect snapshot is still queued, want true")
	}
}

func TestSnapshotDrainClosesOnTheSnapshotFrameDisposition(t *testing.T) {
	// Arrange.
	cl := newClient(defaultClientBuffer, nil, ClientKindHost)
	cl.drain.open(time.Now())
	if res := enqueueLocked(cl, outFrame{data: []byte("snapshot"), notify: cl.drain.snapshotDisposed}); !res.queued {
		t.Fatalf("connect snapshot push = %#v, want queued", res)
	}

	// Act. This is the writer's own sequence — pop, then announce the frame's
	// disposition — driven through the outbox seams rather than by waiting on a
	// clock, so the window's close is the write completion by construction.
	f, ok := cl.out.pop()
	if !ok {
		t.Fatal("pop = false, want the queued connect snapshot")
	}
	notifyFrame(f, nil)

	// Assert.
	after := time.Now()
	if cl.drain.overlaps(after, after) {
		t.Fatal("overlaps = true after the snapshot frame was written, want false")
	}
}

func TestSnapshotDrainClosesWhenTheSnapshotWillNeverBeWritten(t *testing.T) {
	// Arrange. A failed write ends the drain just as a successful one does: the
	// snapshot is not still draining, and the connection is being torn down.
	cl := newClient(defaultClientBuffer, nil, ClientKindHost)
	cl.drain.open(time.Now())
	if res := enqueueLocked(cl, outFrame{data: []byte("snapshot"), notify: cl.drain.snapshotDisposed}); !res.queued {
		t.Fatalf("connect snapshot push = %#v, want queued", res)
	}

	// Act.
	for _, f := range cl.out.close() {
		notifyFrame(f, errClientGone)
	}

	// Assert.
	after := time.Now()
	if cl.drain.overlaps(after, after) {
		t.Fatal("overlaps = true after the snapshot was stranded, want false")
	}
}

func TestCommandLatencySampleNamesTheBootDrainWindow(t *testing.T) {
	// Arrange. The connection is mid bring-up: its connect snapshot has been
	// enqueued and no write has completed, so every ack answered now is
	// delivered behind it.
	latency := &recordingLatency{}
	cfg := Config{
		Logf: testLogf(t), LogVerbosef: testLogf(t),
		State: staticState{snap: sampleSnapshot()}, Handler: &mockHandler{},
		CommandLatency: latency, AckWarnThreshold: time.Nanosecond,
	}
	cl := newClient(defaultClientBuffer, nil, ClientKindHost)
	cl.drain.open(time.Now())

	// Act.
	runOneCommandOnClient(t, cfg, submitCmd("r-1", "/ws"), cl)

	// Assert.
	samples := latency.all()
	if len(samples) != 1 {
		t.Fatalf("samples = %d, want exactly one", len(samples))
	}
	if !samples[0].Slow() {
		t.Fatalf("sample.Slow() = false against a one-nanosecond threshold, want true")
	}
	if samples[0].Decision != BootSnapshotDrainDecision {
		t.Fatalf("sample.Decision = %q, want %q", samples[0].Decision, BootSnapshotDrainDecision)
	}
}

func TestCommandLatencySampleCarriesNoDecisionAfterTheDrainClosed(t *testing.T) {
	// Arrange. The bring-up is over: the snapshot's bytes are on the socket
	// before this command is even received, so nothing structural explains a
	// slow ack and the record must warn exactly as it always did.
	latency := &recordingLatency{}
	cfg := Config{
		Logf: testLogf(t), LogVerbosef: testLogf(t),
		State: staticState{snap: sampleSnapshot()}, Handler: &mockHandler{},
		CommandLatency: latency, AckWarnThreshold: time.Nanosecond,
	}
	cl := newClient(defaultClientBuffer, nil, ClientKindHost)
	opened := time.Now()
	cl.drain.open(opened)
	cl.drain.closeAt(opened)

	// Act.
	runOneCommandOnClient(t, cfg, submitCmd("r-1", "/ws"), cl)

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
