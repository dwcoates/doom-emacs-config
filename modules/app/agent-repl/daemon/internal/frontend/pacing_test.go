package frontend

import (
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
	"google.golang.org/protobuf/encoding/protojson"
)

// registerPacingClient wires cl into s's fan-out set, which is what makes it a
// broadcast target.
func registerPacingClient(s *Server, cl *client) {
	s.mu.Lock()
	s.clients[cl] = struct{}{}
	s.mu.Unlock()
}

// popPacingFrame takes exactly one frame off cl's queue, waiting for the
// producer to put one there. It is the gated consumer's single step: no sleep,
// no poll — it parks on the queue's own wakeup, or on the teardown.
func popPacingFrame(cl *client) (*frontendv1.FrontendFrame, bool) {
	for {
		f, ok := cl.out.pop()
		if ok {
			frame := &frontendv1.FrontendFrame{}
			if err := protojson.Unmarshal(f.data, frame); err != nil {
				return nil, false
			}
			return frame, true
		}
		select {
		case <-cl.out.ready:
		case <-cl.done:
			return nil, false
		}
	}
}

func TestPacedReplayTenTimesTheBufferSurvivesASlowConsumer(t *testing.T) {
	// Arrange: a buffer of 8 and a replay of 80 append-semantic deltas —
	// nothing about them is coalescable, so compaction can free nothing and the
	// old non-blocking producer would have hit the ceiling on frame nine.
	const (
		buffer = 8
		replay = buffer * 10
	)
	s, _ := newTestServer(t, buffer)
	cl := newClient(s.bufSize, nil, ClientKindGUIStream)
	registerPacingClient(s, cl)
	produced := make(chan struct{})
	go func() {
		defer close(produced)
		for seq := 1; seq <= replay; seq++ {
			s.PushConversationDelta(&frontendv1.ConversationDelta{
				Workspace: "w1", Fence: "s1", ThroughSeq: uint64(seq),
			})
		}
	}()

	// Act: consume one frame at a time, strictly slower than the producer
	// composes them.
	var got []uint64
	for i := 0; i < replay; i++ {
		frame, ok := popPacingFrame(cl)
		if !ok {
			t.Fatalf("consumer lost the connection after %d of %d replay frames", i, replay)
		}
		got = append(got, frame.GetConversationDelta().GetThroughSeq())
	}
	<-produced

	// Assert: the whole replay arrived, in order, on a live connection.
	if s.clientCount() != 1 {
		t.Fatalf("client count = %d, want the slow-but-draining consumer kept", s.clientCount())
	}
	for i, seq := range got {
		if seq != uint64(i+1) {
			t.Fatalf("replay frame %d carried through_seq=%d, want %d", i, seq, i+1)
		}
	}
}

func TestPacingDisconnectsAConsumerMakingNoProgress(t *testing.T) {
	// Arrange: a client that will never pop a frame nor accept a byte.
	s, _ := newTestServer(t, 4)
	cl := newClient(s.bufSize, nil, ClientKindGUIStream)
	registerPacingClient(s, cl)

	// Act: push past the point where pacing engages.
	for seq := 1; seq <= 4; seq++ {
		s.PushConversationDelta(&frontendv1.ConversationDelta{
			Workspace: "w1", Fence: "s1", ThroughSeq: uint64(seq),
		})
	}

	// Assert: the stall verdict still fires, loudly and by name.
	if s.clientCount() != 0 {
		t.Fatalf("client count = %d, want the wedged consumer hard-disconnected", s.clientCount())
	}
	detail := cl.closeCause().String()
	if !strings.Contains(detail, "limit=stalled") || !strings.Contains(detail, "phase=pacing") {
		t.Fatalf("close cause = %q, want the pacing stall verdict", detail)
	}
}

func TestPacedReplayKeepsLivePushInterleavingOrder(t *testing.T) {
	// Arrange: a replay long enough to engage pacing, with one LIVE
	// WorkspaceState pushed mid-replay. The state frame goes out under the
	// delivery lock without pacing, so this is the interleaving of the two
	// paths, not of one.
	const (
		buffer   = 8
		replay   = 24
		liveAt   = 10
		liveAtMs = 4242
	)
	s, _ := newTestServer(t, buffer)
	cl := newClient(s.bufSize, nil, ClientKindHost)
	registerPacingClient(s, cl)
	produced := make(chan struct{})
	go func() {
		defer close(produced)
		for seq := 1; seq <= replay; seq++ {
			s.PushConversationDelta(&frontendv1.ConversationDelta{
				Workspace: "w1", Fence: "s1", ThroughSeq: uint64(seq),
			})
			if seq == liveAt {
				s.PushWorkspaceState(&frontendv1.WorkspaceState{
					Workspace: "w1", SessionId: "s1", AtMs: liveAtMs,
				})
			}
		}
	}()

	// Act: drain everything the producer emitted, one frame at a time.
	var order []string
	for i := 0; i < replay+1; i++ {
		frame, ok := popPacingFrame(cl)
		if !ok {
			t.Fatalf("consumer lost the connection after %d frames", i)
		}
		if ws := frame.GetWorkspaceState(); ws != nil {
			order = append(order, "state")
			continue
		}
		order = append(order, "delta")
	}
	<-produced

	// Assert: the live state sits exactly where it was pushed — pacing waits
	// before the push, so it can delay a frame but never move one.
	if len(order) != replay+1 {
		t.Fatalf("drained %d frames, want %d", len(order), replay+1)
	}
	for i, kind := range order {
		want := "delta"
		if i == liveAt {
			want = "state"
		}
		if kind != want {
			t.Fatalf("frame %d was %q, want %q — pacing reordered the stream", i, kind, want)
		}
	}
}

func TestAwaitRoomReturnsClosedForATornDownConnection(t *testing.T) {
	// Arrange: a queue whose connection died while a producer was waiting on it.
	o := newOutbox(4)
	o.push(outFrame{data: []byte("a")})
	o.push(outFrame{data: []byte("b")})
	o.push(outFrame{data: []byte("c")})
	o.close()

	// Act.
	verdict, res := o.awaitRoom(paceStallGrace)

	// Assert: a gone connection is not a slow one — no stall verdict, no second
	// teardown.
	if verdict != roomClosed {
		t.Fatalf("verdict = %v, want roomClosed", verdict)
	}
	if !res.closed {
		t.Fatal("result did not report the queue closed")
	}
}
