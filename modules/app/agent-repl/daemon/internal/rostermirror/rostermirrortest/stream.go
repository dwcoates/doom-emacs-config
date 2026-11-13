// Package rostermirrortest is the incumbent's roster stream as a test drives
// it, shared by every suite that runs a roster mirror.
package rostermirrortest

import (
	"context"
	"sync"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

// Stream is a rostermirror.Stream the test pushes frames on. It ends when the
// test closes it (End) and when the context it was dialed with ends.
type Stream struct {
	// Ctx is the context the mirror dialed the stream with.
	Ctx context.Context
	// Frames carries each frame the test pushes.
	Frames chan *agentreplv1.WatchWorkspaceRosterResponse
	// Closed closes when the mirror closes the stream.
	Closed chan struct{}

	msg  *agentreplv1.WatchWorkspaceRosterResponse
	mu   sync.Mutex
	err  error
	once sync.Once
}

// New is a stream dialed with CTX.
func New(ctx context.Context) *Stream {
	return &Stream{
		Ctx:    ctx,
		Frames: make(chan *agentreplv1.WatchWorkspaceRosterResponse, 8),
		Closed: make(chan struct{}),
	}
}

// End ends the stream after the frames already pushed, with ERR (nil: a
// clean close).
func (s *Stream) End(err error) {
	s.mu.Lock()
	s.err = err
	s.mu.Unlock()
	close(s.Frames)
}

// Receive implements rostermirror.Stream.
func (s *Stream) Receive() bool {
	select {
	case f, ok := <-s.Frames:
		if !ok {
			return false
		}
		s.msg = f
		return true
	case <-s.Ctx.Done():
		s.mu.Lock()
		s.err = s.Ctx.Err()
		s.mu.Unlock()
		return false
	}
}

// Msg implements rostermirror.Stream.
func (s *Stream) Msg() *agentreplv1.WatchWorkspaceRosterResponse { return s.msg }

// Err implements rostermirror.Stream.
func (s *Stream) Err() error {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.err
}

// Close implements rostermirror.Stream.
func (s *Stream) Close() error {
	s.once.Do(func() { close(s.Closed) })
	return nil
}
