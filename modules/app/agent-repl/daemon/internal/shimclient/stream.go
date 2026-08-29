package shimclient

import (
	"context"
	"io"
	"sync"

	"connectrpc.com/connect"
)

// mappedStream is one Connect server stream, projected onto the type its
// consumer wants. Recv returns io.EOF ONLY when the producer ended the stream;
// a transport failure is that failure's error, and the CONSUMER decides
// whether an end without a terminal frame was legal.
type mappedStream[W any, T any] struct {
	procedure string
	stream    *connect.ServerStreamForClient[W]
	project   func(*W) (T, error)
	// cancel ends the underlying request. Closing a still-live server stream
	// must never block draining a producer that has not finished, so the
	// request is canceled BEFORE the connection is closed.
	cancel    context.CancelFunc
	closeOnce sync.Once
}

// Recv blocks for the next frame.
func (m *mappedStream[W, T]) Recv() (T, error) {
	var zero T
	if !m.stream.Receive() {
		if err := m.stream.Err(); err != nil {
			return zero, err
		}
		return zero, io.EOF
	}
	return m.project(m.stream.Msg())
}

// Close ends the stream from this side. Closing twice is harmless: a consumer
// that closes on its own and a supervisor tearing the link down both do it.
func (m *mappedStream[W, T]) Close() {
	m.closeOnce.Do(func() {
		m.cancel()
		_ = m.stream.Close()
	})
}
