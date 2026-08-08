package frontend

import (
	"strings"
	"sync"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// --- frame classification ---------------------------------------------------

func TestCoalesceKeyClassifiesFrames(t *testing.T) {
	tests := []struct {
		name       string
		frame      *frontendv1.FrontendFrame
		wantKeyed  bool
		wantSubstr string
	}{
		{
			name:       "workspace state is absolute per workspace",
			frame:      WorkspaceStateFrame(&frontendv1.WorkspaceState{Workspace: "/w", SessionId: "s1"}),
			wantKeyed:  true,
			wantSubstr: "workspace_state",
		},
		{
			name:       "progress is absolute per workspace and session",
			frame:      ProgressViewFrame(&frontendv1.ProgressView{Workspace: "/w", Fence: "s1"}),
			wantKeyed:  true,
			wantSubstr: "progress",
		},
		{
			name:       "queue is absolute per workspace and session",
			frame:      QueueViewFrame(&frontendv1.QueueView{Workspace: "/w", Fence: "s1"}),
			wantKeyed:  true,
			wantSubstr: "queue",
		},
		{
			name: "heartbeat is absolute per running tool",
			frame: HeartbeatViewFrame(&frontendv1.HeartbeatView{
				Workspace: "/w", Fence: "s1",
				Progress: &corev1.HeartbeatProgress{ToolUseId: "t1"},
			}),
			wantKeyed:  true,
			wantSubstr: "t1",
		},
		{
			name: "typing delta grows a block and is never coalescable",
			frame: TypingDeltaFrame(&frontendv1.TypingDelta{
				Workspace: "/w", Fence: "s1",
				Delta: &corev1.ContentDelta{Uuid: "u1", Delta: &corev1.ContentDelta_Text{Text: "hi"}},
			}),
			wantKeyed: false,
		},
		{
			name:      "conversation delta appends items and is never coalescable",
			frame:     ConversationDeltaFrame(&frontendv1.ConversationDelta{Workspace: "/w", Fence: "s1"}),
			wantKeyed: false,
		},
		{
			name:      "session init is a one-shot catalog and is never coalescable",
			frame:     SessionInitViewFrame(&frontendv1.SessionInitView{Workspace: "/w", Fence: "s1"}),
			wantKeyed: false,
		},
		{
			name:      "host action is durable host work and is never coalescable",
			frame:     HostActionFrame(&frontendv1.HostAction{}),
			wantKeyed: false,
		},
		{
			name:      "state snapshot is the freshness lease and is never coalescable",
			frame:     SnapshotFrame(sampleSnapshot()),
			wantKeyed: false,
		},
		{
			name:       "workspace roster is absolute for the whole editor",
			frame:      WorkspaceRosterFrame(validRoster(1)),
			wantKeyed:  true,
			wantSubstr: "workspace_roster",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act.
			key := coalesceKey(tc.frame)

			// Assert.
			if tc.wantKeyed && key == "" {
				t.Fatalf("coalesceKey = %q, want a supersession key", key)
			}
			if !tc.wantKeyed && key != "" {
				t.Fatalf("coalesceKey = %q, want no supersession key", key)
			}
			if tc.wantSubstr != "" && !strings.Contains(key, tc.wantSubstr) {
				t.Fatalf("coalesceKey = %q, want it to name %q", key, tc.wantSubstr)
			}
		})
	}
}

// TestCoalesceKeyMergesEveryRoster covers the roster's GLOBAL key: unlike every
// other coalescable frame it has no discriminator, because there is exactly one
// roster for the whole editor and any newer one is the whole truth.
func TestCoalesceKeyMergesEveryRoster(t *testing.T) {
	// Arrange: two rosters that differ in revision, view and current workspace.
	older := WorkspaceRosterFrame(validRoster(1))
	newer := validRoster(2)
	newer.CurrentDir = "/elsewhere"

	// Act.
	a, b := coalesceKey(older), coalesceKey(WorkspaceRosterFrame(newer))

	// Assert.
	if a != b {
		t.Fatalf("roster keys = %q and %q, want one shared key", a, b)
	}
}

// TestOutboxCompactionKeepsOnlyTheNewestRoster covers roster supersession in
// the queue: a slow consumer's backlog collapses to the newest roster alone.
func TestOutboxCompactionKeepsOnlyTheNewestRoster(t *testing.T) {
	// Arrange: a full queue holding two rosters.
	o := newOutbox(2)
	o.push(outFrame{key: coalesceKey(WorkspaceRosterFrame(validRoster(1))), data: []byte("r1")})
	o.push(outFrame{key: coalesceKey(WorkspaceRosterFrame(validRoster(2))), data: []byte("r2")})

	// Act: a third roster forces a compaction.
	res := o.push(outFrame{key: coalesceKey(WorkspaceRosterFrame(validRoster(3))), data: []byte("r3")})

	// Assert: the superseded roster made room for the new one instead of the
	// connection being given up on. Compaction rewrites what is ALREADY queued,
	// so the survivor of that pass (r2) still precedes the frame that triggered
	// it (r3) — the roster the consumer ends on is the newest either way.
	if !res.queued {
		t.Fatal("push after compaction was refused, want the newest roster queued")
	}
	if res.compacted != 1 {
		t.Errorf("compacted = %d, want 1 superseded roster removed", res.compacted)
	}
	var drained []string
	for {
		data, ok := o.pop()
		if !ok {
			break
		}
		drained = append(drained, string(data))
	}
	if len(drained) != 2 || drained[0] != "r2" || drained[1] != "r3" {
		t.Fatalf("queue drained to %v, want the oldest roster dropped and [r2 r3] left", drained)
	}
}

func TestCoalesceKeySeparatesConcurrentTools(t *testing.T) {
	// Arrange: two heartbeats for the same session but different tools.
	first := HeartbeatViewFrame(&frontendv1.HeartbeatView{
		Workspace: "/w", Fence: "s1", Progress: &corev1.HeartbeatProgress{ToolUseId: "t1"},
	})
	second := HeartbeatViewFrame(&frontendv1.HeartbeatView{
		Workspace: "/w", Fence: "s1", Progress: &corev1.HeartbeatProgress{ToolUseId: "t2"},
	})

	// Act.
	a, b := coalesceKey(first), coalesceKey(second)

	// Assert: one running tool's liveness never supersedes another's.
	if a == b {
		t.Fatalf("heartbeat keys for distinct tools are both %q, want distinct keys", a)
	}
}

func TestCoalesceKeySeparatesWorkspaces(t *testing.T) {
	// Arrange: resolved state for two workspaces.
	first := WorkspaceStateFrame(&frontendv1.WorkspaceState{Workspace: "/a", SessionId: "s1"})
	second := WorkspaceStateFrame(&frontendv1.WorkspaceState{Workspace: "/b", SessionId: "s1"})

	// Act.
	a, b := coalesceKey(first), coalesceKey(second)

	// Assert.
	if a == b {
		t.Fatalf("workspace state keys for distinct workspaces are both %q, want distinct keys", a)
	}
}

// --- compaction -------------------------------------------------------------

// drain empties an outbox into the payload strings it held, oldest first.
func drain(o *outbox) []string {
	var out []string
	for {
		data, ok := o.pop()
		if !ok {
			return out
		}
		out = append(out, string(data))
	}
}

// fill pushes each payload, treating a "key:payload" spec as a supersedable
// frame and a bare payload as one nothing may replace.
func fill(t *testing.T, o *outbox, specs ...string) {
	t.Helper()
	for _, spec := range specs {
		f := outFrame{data: []byte(spec)}
		if key, _, ok := strings.Cut(spec, ":"); ok {
			f.key = key
		}
		if res := o.push(f); !res.queued {
			t.Fatalf("push %q refused while arranging the queue", spec)
		}
	}
}

func TestOutboxPushCoalescesSupersededFramesWhenFull(t *testing.T) {
	// Arrange: a full queue holding two revisions of the same progress view.
	o := newOutbox(3)
	fill(t, o, "progress:p1", "progress:p2", "conv1")

	// Act: a third progress revision arrives with no room left.
	res := o.push(outFrame{key: "progress", data: []byte("progress:p3")})

	// Assert: the stale revisions are gone and the newest one is queued.
	if !res.queued {
		t.Fatal("push = false, want the frame queued after compaction")
	}
	if res.compacted != 1 {
		t.Fatalf("compacted = %d, want the one superseded progress frame removed", res.compacted)
	}
	if got, want := drain(o), []string{"progress:p2", "conv1", "progress:p3"}; !equalStrings(got, want) {
		t.Fatalf("queue = %v, want %v", got, want)
	}
}

func TestOutboxCompactionNeverDropsIrreplaceableFrames(t *testing.T) {
	// Arrange: a full queue of frames nothing may replace.
	o := newOutbox(3)
	fill(t, o, "conv1", "conv2", "conv3")

	// Act: one more arrives.
	res := o.push(outFrame{data: []byte("conv4")})

	// Assert: nothing was removed and the push is refused rather than silently
	// dropping content.
	if res.queued {
		t.Fatal("push = true, want refusal when no queued frame is supersedable")
	}
	if res.compacted != 0 {
		t.Fatalf("compacted = %d, want 0 — append-semantic frames are never dropped", res.compacted)
	}
	if got, want := drain(o), []string{"conv1", "conv2", "conv3"}; !equalStrings(got, want) {
		t.Fatalf("queue = %v, want it untouched %v", got, want)
	}
}

func TestOutboxCompactionKeepsTheNewestFramesPosition(t *testing.T) {
	// Arrange: a supersedable frame straddling an irreplaceable one.
	o := newOutbox(3)
	fill(t, o, "ws:a1", "conv1", "ws:a2")

	// Act: fill beyond the bound to force compaction.
	if res := o.push(outFrame{data: []byte("conv2")}); !res.queued {
		t.Fatal("push = false, want the frame queued after compaction")
	}

	// Assert: the surviving state stays AFTER the conversation frame it
	// followed, so no state overtakes content the producer emitted first.
	if got, want := drain(o), []string{"conv1", "ws:a2", "conv2"}; !equalStrings(got, want) {
		t.Fatalf("queue = %v, want %v", got, want)
	}
}

func TestOutboxCompactionKeepsDistinctKeys(t *testing.T) {
	// Arrange: two workspaces' states plus a stale revision of one of them.
	o := newOutbox(3)
	fill(t, o, "ws-a:a1", "ws-b:b1", "ws-a:a2")

	// Act.
	if res := o.push(outFrame{data: []byte("conv1")}); !res.queued || res.compacted != 1 {
		t.Fatalf("push = (%v, %d), want (true, 1)", res.queued, res.compacted)
	}

	// Assert: only the superseded revision went; the other workspace stayed.
	if got, want := drain(o), []string{"ws-b:b1", "ws-a:a2", "conv1"}; !equalStrings(got, want) {
		t.Fatalf("queue = %v, want %v", got, want)
	}
}

func TestOutboxPushBelowBoundDoesNotCompact(t *testing.T) {
	// Arrange: a queue with room to spare holding two progress revisions.
	o := newOutbox(4)
	fill(t, o, "progress:p1", "progress:p2")

	// Act.
	res := o.push(outFrame{key: "progress", data: []byte("progress:p3")})

	// Assert: a consumer keeping up sees every frame, unchanged.
	if !res.queued || res.compacted != 0 {
		t.Fatalf("push = (%v, %d), want (true, 0)", res.queued, res.compacted)
	}
	if got, want := drain(o), []string{"progress:p1", "progress:p2", "progress:p3"}; !equalStrings(got, want) {
		t.Fatalf("queue = %v, want every frame kept %v", got, want)
	}
}

func TestOutboxPushSignalsTheWriter(t *testing.T) {
	// Arrange.
	o := newOutbox(2)

	// Act.
	if res := o.push(outFrame{data: []byte("a")}); !res.queued {
		t.Fatal("push = false, want the frame queued")
	}

	// Assert: the writer has a wakeup waiting.
	select {
	case <-o.ready:
	default:
		t.Fatal("push did not signal the writer")
	}
}

func TestOutboxConcurrentPushAndPopIsRaceFree(t *testing.T) {
	// Arrange: many producers of one supersedable key against one consumer.
	const producers, perProducer = 8, 200
	o := newOutbox(16)
	var wg sync.WaitGroup
	stop := make(chan struct{})
	consumed := make(chan int, 1)

	// Act.
	go func() {
		n := 0
		for {
			select {
			case <-stop:
				n += len(drain(o))
				consumed <- n
				return
			case <-o.ready:
				n += len(drain(o))
			}
		}
	}()
	wg.Add(producers)
	for p := 0; p < producers; p++ {
		go func() {
			defer wg.Done()
			for i := 0; i < perProducer; i++ {
				o.push(outFrame{key: "progress", data: []byte("p")})
			}
		}()
	}
	wg.Wait()
	close(stop)

	// Assert: the run completes with a consistent, bounded queue (the race
	// detector is the real assertion; this guards against a wedged outbox).
	if got := <-consumed; got == 0 {
		t.Fatal("consumer saw no frames at all")
	}
	if got := o.depth(); got != 0 {
		t.Fatalf("outbox depth = %d after draining, want 0", got)
	}
}

func equalStrings(a, b []string) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

// --- elasticity -------------------------------------------------------------

// fakeClock is a manually advanced time source, so a grace period can be
// crossed without a test ever sleeping.
type fakeClock struct {
	mu  sync.Mutex
	now time.Time
}

func newFakeClock() *fakeClock {
	return &fakeClock{now: time.Date(2026, 8, 7, 16, 28, 0, 0, time.UTC)}
}

func (c *fakeClock) Now() time.Time {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.now
}

func (c *fakeClock) advance(d time.Duration) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.now = c.now.Add(d)
}

// TestElasticOutboxAbsorbsPressureWhileTheConsumerDrains is the busy-then-
// drains case: a consumer blocked long enough to overrun its soft bound, which
// then catches up, must never be refused.
func TestElasticOutboxAbsorbsPressureWhileTheConsumerDrains(t *testing.T) {
	// Arrange: soft 2, hard 8, and a consumer blocked past the soft bound.
	clock := newFakeClock()
	o := newElasticOutbox(2, 8, time.Minute)
	o.now = clock.Now
	fill(t, o, "conv1", "conv2", "conv3")

	// Act: the consumer wakes, drains one frame, and more frames arrive well
	// past the grace period's wall-clock length.
	if _, ok := o.pop(); !ok {
		t.Fatal("pop = false, want the blocked consumer to drain a frame")
	}
	clock.advance(5 * time.Minute)
	res := o.push(outFrame{data: []byte("conv4")})

	// Assert: drain progress resets the stall mark, so the busy host is
	// absorbed rather than evicted.
	if !res.queued {
		t.Fatalf("push = %+v, want a draining consumer's frame queued", res)
	}
	if res.reason != "" {
		t.Fatalf("reason = %q, want no refusal reason", res.reason)
	}
}

// TestElasticOutboxRefusesAtTheHardCeiling covers the memory bound: no amount
// of drain progress buys a queue past its absolute ceiling.
func TestElasticOutboxRefusesAtTheHardCeiling(t *testing.T) {
	// Arrange: an irreplaceable backlog filled exactly to the hard ceiling.
	o := newElasticOutbox(2, 4, time.Hour)
	fill(t, o, "conv1", "conv2", "conv3", "conv4")

	// Act.
	res := o.push(outFrame{data: []byte("conv5")})

	// Assert.
	if res.queued {
		t.Fatal("push = true, want refusal at the hard ceiling")
	}
	if res.reason != overflowCeiling {
		t.Fatalf("reason = %q, want %q", res.reason, overflowCeiling)
	}
	if res.depth != 4 || res.soft != 2 || res.hard != 4 {
		t.Fatalf("stats = (depth %d, soft %d, hard %d), want (4, 2, 4)", res.depth, res.soft, res.hard)
	}
}

// TestElasticOutboxRefusesAConsumerThatDrainsNothingPastTheGrace covers the
// genuinely wedged consumer: still under its ceiling, but it has not taken a
// single frame for the whole grace period.
func TestElasticOutboxRefusesAConsumerThatDrainsNothingPastTheGrace(t *testing.T) {
	// Arrange: a queue over its soft bound whose consumer never pops.
	clock := newFakeClock()
	o := newElasticOutbox(2, 64, 30*time.Second)
	o.now = clock.Now
	fill(t, o, "conv1", "conv2", "conv3")

	// Act: time passes with no drain at all, then another frame arrives.
	clock.advance(31 * time.Second)
	res := o.push(outFrame{data: []byte("conv4")})

	// Assert.
	if res.queued {
		t.Fatal("push = true, want a wedged consumer refused")
	}
	if res.reason != overflowStalled {
		t.Fatalf("reason = %q, want %q", res.reason, overflowStalled)
	}
	if res.stalledFor != 31*time.Second {
		t.Fatalf("stalledFor = %s, want 31s", res.stalledFor)
	}
}

// TestElasticOutboxKeepsAcceptingBeforeTheGraceExpires covers the other side of
// that deadline: an undrained queue inside the grace window is still absorbed.
func TestElasticOutboxKeepsAcceptingBeforeTheGraceExpires(t *testing.T) {
	// Arrange.
	clock := newFakeClock()
	o := newElasticOutbox(2, 64, 30*time.Second)
	o.now = clock.Now
	fill(t, o, "conv1", "conv2", "conv3")

	// Act.
	clock.advance(29 * time.Second)
	res := o.push(outFrame{data: []byte("conv4")})

	// Assert.
	if !res.queued {
		t.Fatalf("push = %+v, want the frame absorbed inside the grace window", res)
	}
}

// TestElasticOutboxReportsEnteringPressureOnce covers the pressure telemetry:
// an episode is reported when it begins, not once per frame.
func TestElasticOutboxReportsEnteringPressureOnce(t *testing.T) {
	// Arrange: a queue filled exactly to its soft bound.
	o := newElasticOutbox(2, 8, time.Minute)
	fill(t, o, "conv1", "conv2")

	// Act.
	first := o.push(outFrame{data: []byte("conv3")})
	second := o.push(outFrame{data: []byte("conv4")})

	// Assert.
	if !first.entered || !first.overSoft {
		t.Fatalf("first push = %+v, want it to report entering the elastic region", first)
	}
	if second.entered {
		t.Fatalf("second push = %+v, want no second entry record for one episode", second)
	}
}

// TestElasticOutboxReEntersPressureAfterDraining covers the episode boundary: a
// queue that recovers below its soft bound reports the NEXT overrun as new.
func TestElasticOutboxReEntersPressureAfterDraining(t *testing.T) {
	// Arrange: one completed pressure episode, then a full drain.
	o := newElasticOutbox(2, 8, time.Minute)
	fill(t, o, "conv1", "conv2")
	o.push(outFrame{data: []byte("conv3")})
	drain(o)

	// Act.
	fill(t, o, "conv4", "conv5")
	res := o.push(outFrame{data: []byte("conv6")})

	// Assert.
	if !res.entered {
		t.Fatalf("push = %+v, want a fresh episode reported after recovery", res)
	}
}

// TestOutboxWithoutElasticityRefusesAtItsBound pins the non-host policy: a flat
// queue is exactly a soft==hard elastic one, so the first frame past the bound
// is refused at the ceiling with no grace consulted.
func TestOutboxWithoutElasticityRefusesAtItsBound(t *testing.T) {
	// Arrange.
	o := newOutbox(2)
	fill(t, o, "conv1", "conv2")

	// Act.
	res := o.push(outFrame{data: []byte("conv3")})

	// Assert.
	if res.queued {
		t.Fatal("push = true, want a flat queue refused at its bound")
	}
	if res.reason != overflowCeiling {
		t.Fatalf("reason = %q, want %q", res.reason, overflowCeiling)
	}
}

// TestNewElasticOutboxClampsAHardBoundBelowItsSoft guards the constructor: a
// ceiling under the soft bound would refuse frames below the queue's own stated
// capacity, so it is raised rather than silently honored.
func TestNewElasticOutboxClampsAHardBoundBelowItsSoft(t *testing.T) {
	// Arrange / Act.
	o := newElasticOutbox(8, 2, time.Minute)

	// Assert.
	if o.capacity() != 8 || o.ceiling() != 8 {
		t.Fatalf("bounds = (soft %d, hard %d), want (8, 8)", o.capacity(), o.ceiling())
	}
}
