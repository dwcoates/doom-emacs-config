package sockethold

import (
	"context"
	"errors"
	"io"
	"net"
	"os"
	"testing"
)

// acceptedUnread dials H's data socket through a listener taken from H and
// answers the client's end and the accepted end's file, never read.
func acceptedUnread(t *testing.T, h *holder) (net.Conn, *os.File) {
	t.Helper()
	ln, err := take(t, h.control, "data")
	if err != nil {
		t.Fatalf("Take: %v", err)
	}
	defer ln.Close()
	client, err := net.Dial("unix", h.data)
	if err != nil {
		t.Fatalf("dial the data socket: %v", err)
	}
	t.Cleanup(func() { client.Close() })
	conn, err := ln.Accept()
	if err != nil {
		t.Fatalf("accept: %v", err)
	}
	defer conn.Close()
	file, err := conn.(*net.UnixConn).File()
	if err != nil {
		t.Fatalf("the accepted connection's file: %v", err)
	}
	t.Cleanup(func() { file.Close() })
	return client, file
}

func handBack(t *testing.T, h *holder, file *os.File) error {
	t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), takeBound)
	defer cancel()
	return Return(ctx, h.control, "data", file)
}

func subscribe(t *testing.T, h *holder, name string) *Returned {
	t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), takeBound)
	defer cancel()
	r, err := Subscribe(ctx, h.control, name)
	if err != nil {
		t.Fatalf("Subscribe: %v", err)
	}
	t.Cleanup(func() { r.Close() })
	return r
}

// readsTheCall asserts that CONN reads what CLIENT wrote: the call reached
// the server that took the connection over, byte for byte.
func readsTheCall(t *testing.T, client, conn net.Conn) {
	t.Helper()
	if _, err := client.Write([]byte("the call")); err != nil {
		t.Fatalf("the client's write: %v", err)
	}
	buf := make([]byte, len("the call"))
	if _, err := io.ReadFull(conn, buf); err != nil || string(buf) != "the call" {
		t.Fatalf("read %q (%v), want the client's call", buf, err)
	}
}

func TestAHandedBackConnectionReachesTheNextAcceptStream(t *testing.T) {
	cases := []struct {
		name            string
		subscribeBefore bool
	}{
		{name: "a stream already open receives it", subscribeBefore: true},
		{name: "a stream opened later receives it from the queue"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := startHolder(t)
			client, file := acceptedUnread(t, h)
			var stream *Returned
			if tc.subscribeBefore {
				stream = subscribe(t, h, "data")
			}

			// Act
			err := handBack(t, h, file)

			// Assert
			if err != nil {
				t.Fatalf("Return: %v", err)
			}
			file.Close()
			if stream == nil {
				stream = subscribe(t, h, "data")
			}
			conn, err := stream.Next()
			if err != nil {
				t.Fatalf("Next: %v", err)
			}
			defer conn.Close()
			readsTheCall(t, client, conn)
		})
	}
}

func TestANewerAcceptStreamEndsTheOlder(t *testing.T) {
	// Arrange
	h := startHolder(t)
	older := subscribe(t, h, "data")

	// Act
	newer := subscribe(t, h, "data")

	// Assert: the older reads its end; a connection handed back goes to the newer.
	if _, err := older.Next(); !errors.Is(err, io.EOF) {
		t.Fatalf("the older stream's Next = %v, want io.EOF", err)
	}
	client, file := acceptedUnread(t, h)
	if err := handBack(t, h, file); err != nil {
		t.Fatalf("Return: %v", err)
	}
	conn, err := newer.Next()
	if err != nil {
		t.Fatalf("the newer stream's Next: %v", err)
	}
	defer conn.Close()
	readsTheCall(t, client, conn)
}

func TestAStoppedStreamReadsItsEnd(t *testing.T) {
	// Arrange
	h := startHolder(t)
	stream := subscribe(t, h, "data")

	// Act
	err := stream.Stop()

	// Assert
	if err != nil {
		t.Fatalf("Stop: %v", err)
	}
	if _, err := stream.Next(); !errors.Is(err, io.EOF) {
		t.Fatalf("Next after Stop = %v, want io.EOF", err)
	}
}

func TestAReturnOrStreamOfASocketNotHeldIsRefused(t *testing.T) {
	cases := []struct {
		name string
		ask  func(h *holder, file *os.File) error
	}{
		{name: "a return", ask: func(h *holder, file *os.File) error {
			return Return(context.Background(), h.control, "elsewhere", file)
		}},
		{name: "an accept stream", ask: func(h *holder, _ *os.File) error {
			r, err := Subscribe(context.Background(), h.control, "elsewhere")
			if err != nil {
				return err
			}
			defer r.Close()
			_, err = r.Next()
			return err
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := startHolder(t)
			_, file := acceptedUnread(t, h)

			// Act
			err := tc.ask(h, file)

			// Assert
			if !errors.Is(err, ErrRefused) {
				t.Fatalf("err = %v, want ErrRefused", err)
			}
		})
	}
}
