package login

import (
	"context"
	"strings"
	"testing"

	"claude-repld/internal/dlog"
)

// bareSession builds a session with no child attached. Every method under test
// here touches only the scrollback and the viewer set, never the pty.
func bareSession(t *testing.T) *session {
	t.Helper()
	return newSession("/roots/default", nil, nil, dlog.NewTestLogger(), func(string) {})
}

func TestBroadcastRetainsOutputForReplay(t *testing.T) {
	// Arrange.
	s := bareSession(t)

	// Act.
	s.broadcast([]byte("hello "))
	s.broadcast([]byte("world"))

	// Assert.
	if string(s.scroll) != "hello world" {
		t.Fatalf("scrollback = %q, want %q", s.scroll, "hello world")
	}
}

func TestBroadcastTrimsTheScrollbackFromTheFront(t *testing.T) {
	// Arrange: overflow drops the OLDEST bytes; the newest screen is the one a
	// late viewer needs.
	s := bareSession(t)
	s.broadcast([]byte(strings.Repeat("o", scrollbackCap)))

	// Act.
	s.broadcast([]byte("NEW"))

	// Assert.
	if len(s.scroll) != scrollbackCap {
		t.Fatalf("scrollback = %d bytes, want the cap %d", len(s.scroll), scrollbackCap)
	}
	if !strings.HasSuffix(string(s.scroll), "NEW") {
		t.Fatal("scrollback lost the newest bytes, want the oldest dropped instead")
	}
}

func TestAttachReplaysTheScrollbackFirst(t *testing.T) {
	// Arrange.
	s := bareSession(t)
	s.broadcast([]byte("the whole screen"))

	// Act.
	sub := s.attach()
	out := sub.start(context.Background(), func() {})

	// Assert.
	frame := <-out
	if string(frame.Bytes) != "the whole screen" {
		t.Fatalf("first frame = %q, want the replayed scrollback", frame.Bytes)
	}
}

func TestAttachToAnExitedSessionReplaysThenCloses(t *testing.T) {
	// Arrange: the final screen is exactly what the user needs to read.
	s := bareSession(t)
	s.broadcast([]byte("final screen"))
	s.mu.Lock()
	s.exited = true
	s.mu.Unlock()

	// Act.
	sub := s.attach()
	out := sub.start(context.Background(), func() {})

	// Assert.
	var frames []Output
	for frame := range out {
		frames = append(frames, frame)
	}
	if len(frames) != 2 || string(frames[0].Bytes) != "final screen" || !frames[1].Closed {
		t.Fatalf("frames = %+v, want the final screen then the terminal frame", frames)
	}
}

func TestAttachToAnExitedSessionRegistersNoViewer(t *testing.T) {
	// Arrange.
	s := bareSession(t)
	s.mu.Lock()
	s.exited = true
	s.mu.Unlock()

	// Act.
	s.attach()

	// Assert.
	s.mu.Lock()
	defer s.mu.Unlock()
	if len(s.subs) != 0 {
		t.Fatalf("viewers = %d, want none registered on an exited session", len(s.subs))
	}
}

func TestBroadcastReachesEveryAttachedViewer(t *testing.T) {
	// Arrange.
	s := bareSession(t)
	first := s.attach().start(context.Background(), func() {})
	second := s.attach().start(context.Background(), func() {})

	// Act.
	s.broadcast([]byte("chunk"))

	// Assert.
	if got := string((<-first).Bytes); got != "chunk" {
		t.Fatalf("first viewer = %q, want %q", got, "chunk")
	}
	if got := string((<-second).Bytes); got != "chunk" {
		t.Fatalf("second viewer = %q, want %q", got, "chunk")
	}
}

func TestDetachUnregistersTheViewer(t *testing.T) {
	// Arrange.
	s := bareSession(t)
	sub := s.attach()

	// Act.
	s.detach(sub)

	// Assert.
	s.mu.Lock()
	defer s.mu.Unlock()
	if len(s.subs) != 0 {
		t.Fatalf("viewers = %d, want none after a detach", len(s.subs))
	}
}

func TestResizeRefusesANonPositiveGeometry(t *testing.T) {
	tests := []struct {
		name string
		size Resize
	}{
		{name: "zero rows", size: Resize{Rows: 0, Cols: 80}},
		{name: "zero columns", size: Resize{Rows: 24, Cols: 0}},
		{name: "negative rows", size: Resize{Rows: -1, Cols: 80}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			s := bareSession(t)

			// Act.
			err := s.resize(tc.size)

			// Assert.
			if err == nil {
				t.Fatalf("resize(%+v) = nil error, want a refusal", tc.size)
			}
		})
	}
}
