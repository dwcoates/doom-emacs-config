package rostermirrortest

import (
	"context"
	"errors"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

func TestStream(t *testing.T) {
	reset := errors.New("stream reset")
	cases := []struct {
		name string
		// end ends the stream after one pushed frame.
		end        func(s *Stream, cancel context.CancelFunc)
		wantErr    error
		wantCancel bool
	}{
		{"a clean end", func(s *Stream, _ context.CancelFunc) { s.End(nil) }, nil, false},
		{"an end with an error", func(s *Stream, _ context.CancelFunc) { s.End(reset) }, reset, false},
		{"the dial's context ending", func(_ *Stream, cancel context.CancelFunc) { cancel() }, context.Canceled, true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			s := New(ctx)
			frame := &agentreplv1.WatchWorkspaceRosterResponse{}
			s.Frames <- frame

			// Act
			first := s.Receive()
			got := s.Msg()
			tc.end(s, cancel)
			second := s.Receive()

			// Assert
			if !first || got != frame {
				t.Fatalf("first Receive = %v, %v; want the pushed frame", first, got)
			}
			if second {
				t.Fatal("second Receive = true, want the stream ended")
			}
			if err := s.Err(); !errors.Is(err, tc.wantErr) {
				t.Fatalf("Err = %v, want %v", err, tc.wantErr)
			}
		})
	}
}

func TestCloseClosesOnceHoweverOftenCalled(t *testing.T) {
	// Arrange
	s := New(context.Background())

	// Act
	first, second := s.Close(), s.Close()

	// Assert
	if first != nil || second != nil {
		t.Fatalf("Close = %v, %v; want nil", first, second)
	}
	select {
	case <-s.Closed:
	default:
		t.Fatal("Closed is still open after Close")
	}
}
