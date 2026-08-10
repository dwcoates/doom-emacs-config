package frontend

import (
	"strings"
	"sync"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"google.golang.org/protobuf/encoding/protojson"
)

// resyncCmd builds one conversation resync for a workspace.
func resyncCmd(requestID, workspace string, fromSeq uint64) *frontendv1.FrontendCommand {
	return &frontendv1.FrontendCommand{
		RequestId: requestID, Workspace: workspace,
		Command: &frontendv1.FrontendCommand_Resync{
			Resync: &frontendv1.ResyncCmd{FromSeq: fromSeq, Fence: "f1"},
		},
	}
}

// coalesceRecorder collects what a set of lanes ran and what it superseded,
// with a gate the test opens when it wants the head command to finish. Holding
// the head is what makes an entry QUEUED rather than executing, so every
// ordering here is established by channels rather than by elapsed time.
type coalesceRecorder struct {
	mu         sync.Mutex
	ran        []string
	superseded []string
	logs       []string

	started chan struct{}
	release chan struct{}
	once    sync.Once
}

func newCoalesceRecorder() *coalesceRecorder {
	return &coalesceRecorder{started: make(chan struct{}), release: make(chan struct{})}
}

// run holds the FIRST command it is given until release, so everything
// submitted after it is provably still queued.
func (r *coalesceRecorder) run(t *commandTicket) {
	r.mu.Lock()
	r.ran = append(r.ran, t.cmd.GetRequestId())
	first := len(r.ran) == 1
	r.mu.Unlock()
	if first {
		r.once.Do(func() { close(r.started) })
		<-r.release
	}
}

func (r *coalesceRecorder) supersede(t *commandTicket) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.superseded = append(r.superseded, t.cmd.GetRequestId())
}

func (r *coalesceRecorder) debugf(format string, args ...any) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.logs = append(r.logs, sprintfLane(format, args...))
}

func (r *coalesceRecorder) snapshot() (ran, superseded, logs []string) {
	r.mu.Lock()
	defer r.mu.Unlock()
	return append([]string(nil), r.ran...), append([]string(nil), r.superseded...), append([]string(nil), r.logs...)
}

func TestCommandLanesCoalesceQueuedResyncs(t *testing.T) {
	tests := []struct {
		name string
		// queued is submitted while the lane's head command is held, so every
		// one of these entries is provably QUEUED and not executing.
		queued         []*frontendv1.FrontendCommand
		wantSuperseded []string
		wantRan        []string
	}{
		{
			name:           "an older queued resync is superseded by a newer one",
			queued:         []*frontendv1.FrontendCommand{resyncCmd("q1", "/ws/a", 1), resyncCmd("q2", "/ws/a", 2)},
			wantSuperseded: []string{"q1"},
			wantRan:        []string{"head", "q2"},
		},
		{
			name: "a resync flood collapses to the newest queued entry",
			queued: []*frontendv1.FrontendCommand{
				resyncCmd("q1", "/ws/a", 1), resyncCmd("q2", "/ws/a", 2),
				resyncCmd("q3", "/ws/a", 3), resyncCmd("q4", "/ws/a", 4),
			},
			wantSuperseded: []string{"q1", "q2", "q3"},
			wantRan:        []string{"head", "q4"},
		},
		{
			name:           "non-resync commands are never coalesced",
			queued:         []*frontendv1.FrontendCommand{openCmd("q1", "/ws/a"), submitCmd("q2", "/ws/a"), openCmd("q3", "/ws/a")},
			wantSuperseded: nil,
			wantRan:        []string{"head", "q1", "q2", "q3"},
		},
		{
			name:           "a resync never supersedes a non-resync queued beside it",
			queued:         []*frontendv1.FrontendCommand{submitCmd("q1", "/ws/a"), resyncCmd("q2", "/ws/a", 2), resyncCmd("q3", "/ws/a", 3)},
			wantSuperseded: []string{"q2"},
			wantRan:        []string{"head", "q1", "q3"},
		},
		{
			name:           "a resync on another workspace's lane is untouched",
			queued:         []*frontendv1.FrontendCommand{resyncCmd("q1", "/ws/b", 1), resyncCmd("q2", "/ws/a", 2)},
			wantSuperseded: nil,
			wantRan:        []string{"head", "q1", "q2"},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the head command occupies /ws/a's lane worker, so every
			// command submitted after it sits in the queue where coalescing
			// applies.
			rec := newCoalesceRecorder()
			s := newLaneServer(t, newLaneHandler())
			lanes := newCommandLanes(testLogf(t), rec.debugf, rec.run, rec.supersede)
			lanes.submit(s.newCommandTicket(nil, openCmd("head", "/ws/a"), time.Now(), 1))
			<-rec.started

			// Act.
			for i, cmd := range tc.queued {
				lanes.submit(s.newCommandTicket(nil, cmd, time.Now(), int64(i+2)))
			}
			close(rec.release)
			lanes.close()

			// Assert.
			ran, superseded, _ := rec.snapshot()
			if strings.Join(superseded, ",") != strings.Join(tc.wantSuperseded, ",") {
				t.Fatalf("superseded = %v, want %v", superseded, tc.wantSuperseded)
			}
			if len(ran) != len(tc.wantRan) {
				t.Fatalf("commands run = %v, want %v", ran, tc.wantRan)
			}
			// /ws/b runs on its own lane concurrently, so only the set is
			// promised across lanes; order within a lane is asserted by the
			// single-lane cases above.
			gotSet := map[string]bool{}
			for _, r := range ran {
				gotSet[r] = true
			}
			for _, want := range tc.wantRan {
				if !gotSet[want] {
					t.Fatalf("commands run = %v, want it to include %q", ran, want)
				}
			}
		})
	}
}

func TestCommandLanesNeverSupersedeAnExecutingResync(t *testing.T) {
	// Arrange: the EXECUTING command is itself a resync, held mid-run. next()
	// removed it from the queue before running it, so no later resync may
	// treat it as coalescible.
	rec := newCoalesceRecorder()
	s := newLaneServer(t, newLaneHandler())
	lanes := newCommandLanes(testLogf(t), rec.debugf, rec.run, rec.supersede)
	lanes.submit(s.newCommandTicket(nil, resyncCmd("executing", "/ws/a", 1), time.Now(), 1))
	<-rec.started

	// Act.
	lanes.submit(s.newCommandTicket(nil, resyncCmd("newer", "/ws/a", 2), time.Now(), 2))
	close(rec.release)
	lanes.close()

	// Assert.
	ran, superseded, _ := rec.snapshot()
	if len(superseded) != 0 {
		t.Fatalf("superseded = %v, want the executing resync left alone", superseded)
	}
	if strings.Join(ran, ",") != "executing,newer" {
		t.Fatalf("commands run = %v, want executing,newer", ran)
	}
}

func TestCommandLanesLogEveryCoalescingWithCounts(t *testing.T) {
	// Arrange.
	rec := newCoalesceRecorder()
	s := newLaneServer(t, newLaneHandler())
	lanes := newCommandLanes(testLogf(t), rec.debugf, rec.run, rec.supersede)
	lanes.submit(s.newCommandTicket(nil, openCmd("head", "/ws/a"), time.Now(), 1))
	<-rec.started

	// Act.
	lanes.submit(s.newCommandTicket(nil, resyncCmd("q1", "/ws/a", 1), time.Now(), 2))
	lanes.submit(s.newCommandTicket(nil, resyncCmd("q2", "/ws/a", 2), time.Now(), 3))
	close(rec.release)
	lanes.close()

	// Assert: one line per superseded entry, naming both identities and the
	// counts an operator reads a flood from.
	_, _, logs := rec.snapshot()
	if len(logs) != 1 {
		t.Fatalf("coalescing logs = %v, want exactly one", logs)
	}
	for _, want := range []string{
		`lane="/ws/a"`, `superseded_request_id="q1"`, `by_request_id="q2"`,
		"superseded=1", "queued_resyncs=1", "lane_depth=1",
	} {
		if !strings.Contains(logs[0], want) {
			t.Fatalf("coalescing log = %q, want it to carry %q", logs[0], want)
		}
	}
}

func TestCommandLaneBoundsQueuedResyncDepth(t *testing.T) {
	// Arrange: a queue holding more resyncs than the bound allows, which the
	// coalescing above cannot produce today. The bound is asserted against the
	// LANE so it survives any future narrowing of the coalescing predicate.
	s := newLaneServer(t, newLaneHandler())
	lane := &commandLane{key: "/ws/a", ready: make(chan struct{}, 1)}
	for _, rid := range []string{"r1", "r2", "r3"} {
		lane.queue = append(lane.queue, laneItem{
			ticket: s.newCommandTicket(nil, resyncCmd(rid, "/ws/a", 1), time.Now(), 1),
		})
	}

	// Act.
	trimmed := lane.trimQueuedResyncsLocked()

	// Assert: the OLDEST beyond the bound are handed back for their answer,
	// and the lane is left holding exactly the bound.
	if len(trimmed) != 1 || trimmed[0].ticket.cmd.GetRequestId() != "r1" {
		t.Fatalf("trimmed = %v, want the oldest queued resync r1", trimmed)
	}
	if got := lane.depthLocked().resyncs; got != maxQueuedResyncPerLane {
		t.Fatalf("queued resyncs = %d, want %d", got, maxQueuedResyncPerLane)
	}
}

func TestReadLoopAcksASupersededResyncWithoutARefusal(t *testing.T) {
	// Arrange: the ack shape is what keeps a coalesced entry from feeding the
	// flood — a nack reaches both clients' refusal sinks, and Emacs's ack
	// deadline fires on no ack at all.
	s := newLaneServer(t, newLaneHandler())
	ticket := s.newCommandTicket(newClient(defaultClientBuffer, nil, ClientKindHost), resyncCmd("old", "/ws/a", 1), time.Now(), 1)

	// Act.
	s.answerSuperseded(ticket)

	// Assert: an ok ack for the superseded request, carrying no classified
	// failure and naming the supersession in its note.
	frame := &frontendv1.FrontendFrame{}
	if err := protojson.Unmarshal(mustPop(t, ticket.cl), frame); err != nil {
		t.Fatalf("decode queued frame: %v", err)
	}
	ack := frame.GetCommandAck()
	if ack == nil {
		t.Fatal("queued frame is not a CommandAck, want the superseded answer")
	}
	if !ack.GetOk() {
		t.Fatalf("ack ok = false (error %q), want a success ack that opens no refusal card", ack.GetError())
	}
	if ack.GetRequestId() != "old" {
		t.Fatalf("ack request_id = %q, want the superseded command's own id", ack.GetRequestId())
	}
	if ack.GetFailure() != nil {
		t.Fatal("ack carries a classified failure, want none — a coalesced resync is not a refusal")
	}
	if !strings.Contains(ack.GetError(), "superseded") {
		t.Fatalf("ack note = %q, want it to name the supersession", ack.GetError())
	}
}

func TestSupersededResyncReleasesItsInFlightGauge(t *testing.T) {
	// Arrange: a coalesced command still owes the accounting every received
	// command owes, or its lost decrement inflates every later queue_depth for
	// the rest of the daemon's life.
	s := newLaneServer(t, newLaneHandler())
	cl := newClient(defaultClientBuffer, nil, ClientKindHost)
	depth := s.inflight.Add(1)
	ticket := s.newCommandTicket(cl, resyncCmd("old", "/ws/a", 1), time.Now(), depth)

	// Act.
	s.answerSuperseded(ticket)

	// Assert.
	if got := s.inflight.Load(); got != 0 {
		t.Fatalf("in-flight gauge = %d, want 0", got)
	}
}
