package frontend

import (
	"strings"
	"sync"
	"testing"

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
			frame:      ProgressViewFrame(&frontendv1.ProgressView{Workspace: "/w", SessionId: "s1"}),
			wantKeyed:  true,
			wantSubstr: "progress",
		},
		{
			name:       "queue is absolute per workspace and session",
			frame:      QueueViewFrame(&frontendv1.QueueView{Workspace: "/w", SessionId: "s1"}),
			wantKeyed:  true,
			wantSubstr: "queue",
		},
		{
			name: "heartbeat is absolute per running tool",
			frame: HeartbeatViewFrame(&frontendv1.HeartbeatView{
				Workspace: "/w", SessionId: "s1",
				Progress: &corev1.HeartbeatProgress{ToolUseId: "t1"},
			}),
			wantKeyed:  true,
			wantSubstr: "t1",
		},
		{
			name: "typing delta grows a block and is never coalescable",
			frame: TypingDeltaFrame(&frontendv1.TypingDelta{
				Workspace: "/w", SessionId: "s1",
				Delta: &corev1.ContentDelta{Uuid: "u1", Delta: &corev1.ContentDelta_Text{Text: "hi"}},
			}),
			wantKeyed: false,
		},
		{
			name:      "conversation delta appends items and is never coalescable",
			frame:     ConversationDeltaFrame(&frontendv1.ConversationDelta{Workspace: "/w", SessionId: "s1"}),
			wantKeyed: false,
		},
		{
			name:      "session init is a one-shot catalog and is never coalescable",
			frame:     SessionInitViewFrame(&frontendv1.SessionInitView{Workspace: "/w", SessionId: "s1"}),
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

func TestCoalesceKeySeparatesConcurrentTools(t *testing.T) {
	// Arrange: two heartbeats for the same session but different tools.
	first := HeartbeatViewFrame(&frontendv1.HeartbeatView{
		Workspace: "/w", SessionId: "s1", Progress: &corev1.HeartbeatProgress{ToolUseId: "t1"},
	})
	second := HeartbeatViewFrame(&frontendv1.HeartbeatView{
		Workspace: "/w", SessionId: "s1", Progress: &corev1.HeartbeatProgress{ToolUseId: "t2"},
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
		if queued, _ := o.push(f); !queued {
			t.Fatalf("push %q refused while arranging the queue", spec)
		}
	}
}

func TestOutboxPushCoalescesSupersededFramesWhenFull(t *testing.T) {
	// Arrange: a full queue holding two revisions of the same progress view.
	o := newOutbox(3)
	fill(t, o, "progress:p1", "progress:p2", "conv1")

	// Act: a third progress revision arrives with no room left.
	queued, compacted := o.push(outFrame{key: "progress", data: []byte("progress:p3")})

	// Assert: the stale revisions are gone and the newest one is queued.
	if !queued {
		t.Fatal("push = false, want the frame queued after compaction")
	}
	if compacted != 1 {
		t.Fatalf("compacted = %d, want the one superseded progress frame removed", compacted)
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
	queued, compacted := o.push(outFrame{data: []byte("conv4")})

	// Assert: nothing was removed and the push is refused rather than silently
	// dropping content.
	if queued {
		t.Fatal("push = true, want refusal when no queued frame is supersedable")
	}
	if compacted != 0 {
		t.Fatalf("compacted = %d, want 0 — append-semantic frames are never dropped", compacted)
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
	if queued, _ := o.push(outFrame{data: []byte("conv2")}); !queued {
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
	if queued, compacted := o.push(outFrame{data: []byte("conv1")}); !queued || compacted != 1 {
		t.Fatalf("push = (%v, %d), want (true, 1)", queued, compacted)
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
	queued, compacted := o.push(outFrame{key: "progress", data: []byte("progress:p3")})

	// Assert: a consumer keeping up sees every frame, unchanged.
	if !queued || compacted != 0 {
		t.Fatalf("push = (%v, %d), want (true, 0)", queued, compacted)
	}
	if got, want := drain(o), []string{"progress:p1", "progress:p2", "progress:p3"}; !equalStrings(got, want) {
		t.Fatalf("queue = %v, want every frame kept %v", got, want)
	}
}

func TestOutboxPushSignalsTheWriter(t *testing.T) {
	// Arrange.
	o := newOutbox(2)

	// Act.
	if queued, _ := o.push(outFrame{data: []byte("a")}); !queued {
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
