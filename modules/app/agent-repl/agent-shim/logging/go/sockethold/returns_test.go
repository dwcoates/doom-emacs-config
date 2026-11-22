package sockethold

import (
	"context"
	"errors"
	"io"
	"net"
	"os"
	"syscall"
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
	cases := []struct {
		name string
		// open opens the stream and stops it.
		open func(t *testing.T, h *holder) (*Returned, error)
	}{
		{name: "asked to stop", open: func(t *testing.T, h *holder) (*Returned, error) {
			r := subscribe(t, h, "data")
			return r, r.Stop()
		}},
		{name: "asked to stop in the request's own write", open: func(t *testing.T, h *holder) (*Returned, error) {
			ctx, cancel := context.WithTimeout(context.Background(), takeBound)
			defer cancel()
			uc, err := dial(ctx, h.control)
			if err != nil {
				t.Fatal(err)
			}
			r := &Returned{uc: uc, name: "data"}
			t.Cleanup(func() { r.Close() })
			_, err = uc.Write([]byte(VerbAccept.prefix() + "data\n" + string(StreamStop)))
			return r, err
		}},
		// A client of the build before asked by half-closing.
		{name: "half-closed", open: func(t *testing.T, h *holder) (*Returned, error) {
			r := subscribe(t, h, "data")
			return r, r.uc.CloseWrite()
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := startHolder(t)

			// Act
			stream, err := tc.open(t, h)

			// Assert
			if err != nil {
				t.Fatalf("stop: %v", err)
			}
			if _, err := stream.Next(); !errors.Is(err, io.EOF) {
				t.Fatalf("Next after the stop = %v, want io.EOF", err)
			}
		})
	}
}

func TestAStreamClientSendingAnUnknownByteIsRecordedAndEnded(t *testing.T) {
	// Arrange
	h := startHolder(t)
	stream := subscribe(t, h, "data")

	// Act
	if _, err := stream.uc.Write([]byte{'x'}); err != nil {
		t.Fatal(err)
	}

	// Assert: the stream ends, and its grant records the breach.
	if _, err := stream.Next(); !errors.Is(err, io.EOF) {
		t.Fatalf("Next after the breach = %v, want io.EOF", err)
	}
	stream.Close()
	g := <-h.grants
	if g.Verb != VerbAccept || g.Err == nil {
		t.Fatalf("grant = %+v, want the accept stream's breach", g)
	}
}

// tcpServerFile answers the server's end of a loopback TCP connection as a
// file, the connection itself closed: the file is its one descriptor.
func tcpServerFile(t *testing.T) *os.File {
	t.Helper()
	ln, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	defer ln.Close()
	client, err := net.Dial("tcp", ln.Addr().String())
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { client.Close() })
	conn, err := ln.Accept()
	if err != nil {
		t.Fatal(err)
	}
	defer conn.Close()
	file, err := conn.(*net.TCPConn).File()
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { file.Close() })
	return file
}

// unixPair answers both ends of a connected unix stream socket pair.
func unixPair(t *testing.T) (a, b *net.UnixConn) {
	t.Helper()
	fds, err := syscall.Socketpair(syscall.AF_UNIX, syscall.SOCK_STREAM, 0)
	if err != nil {
		t.Fatal(err)
	}
	conns := make([]*net.UnixConn, 2)
	for i, fd := range fds {
		f := os.NewFile(uintptr(fd), "pair")
		c, err := net.FileConn(f)
		f.Close()
		if err != nil {
			t.Fatal(err)
		}
		t.Cleanup(func() { c.Close() })
		conns[i] = c.(*net.UnixConn)
	}
	return conns[0], conns[1]
}

// isOpen reports whether FILE is still an open descriptor.
func isOpen(file *os.File) bool {
	_, err := file.Stat()
	return !errors.Is(err, os.ErrClosed)
}

func TestADeliveredConnectionStaysTheHoldersUntilReceived(t *testing.T) {
	// THE HOLDER'S COPY OUTLIVES THE FLIGHT: with every descriptor of a
	// socket in flight, darwin's collector of in-flight descriptors flushes
	// it whenever any unix socket closes, and its client is reset.
	cases := []struct {
		name     string
		after    func(s *stream) error
		wantOpen bool
	}{
		{name: "sent, it stays open", after: func(*stream) error { return nil }, wantOpen: true},
		{name: "told received, it is closed", after: (*stream).received},
		{name: "at the stream's end, it is closed", after: (*stream).release},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			holderEnd, _ := unixPair(t)
			s := &stream{conn: holderEnd}
			file := tcpServerFile(t)
			if !newReturns().deliver("data", s, file) {
				t.Fatal("deliver answered the stream ended")
			}

			// Act
			err := tc.after(s)

			// Assert
			if err != nil {
				t.Fatalf("after the delivery: %v", err)
			}
			if got := isOpen(file); got != tc.wantOpen {
				t.Fatalf("the holder's copy open = %v, want %v", got, tc.wantOpen)
			}
		})
	}
}

func TestAReceiptWithNothingSentIsABreach(t *testing.T) {
	// Arrange
	holderEnd, _ := unixPair(t)
	s := &stream{conn: holderEnd}

	// Act
	err := s.received()

	// Assert
	if !errors.Is(err, errNothingSent) {
		t.Fatalf("received = %v, want errNothingSent", err)
	}
}

func TestTheStreamsClientTellsTheHolder(t *testing.T) {
	cases := []struct {
		name string
		act  func(t *testing.T, r *Returned)
		want byte
	}{
		{name: "Next tells it received the connection", act: func(t *testing.T, r *Returned) {
			conn, err := r.Next()
			if err != nil {
				t.Fatalf("Next: %v", err)
			}
			conn.Close()
		}, want: StreamReceived},
		{name: "Stop asks it to stop", act: func(t *testing.T, r *Returned) {
			if err := r.Stop(); err != nil {
				t.Fatalf("Stop: %v", err)
			}
		}, want: StreamStop},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the test is the holder, and has sent one connection.
			holderEnd, clientEnd := unixPair(t)
			r := &Returned{uc: clientEnd, name: "data"}
			file := tcpServerFile(t)
			if _, _, err := holderEnd.WriteMsgUnix([]byte{StatusOK}, syscall.UnixRights(int(file.Fd())), nil); err != nil {
				t.Fatal(err)
			}

			// Act
			tc.act(t, r)

			// Assert
			got := make([]byte, 1)
			if _, err := io.ReadFull(holderEnd, got); err != nil || got[0] != tc.want {
				t.Fatalf("the holder read %q (%v), want %q", got, err, tc.want)
			}
		})
	}
}

func TestAFailedReceiptIsAnsweredByTheNextNext(t *testing.T) {
	// Arrange: the holder sent one connection and went away.
	holderEnd, clientEnd := unixPair(t)
	r := &Returned{uc: clientEnd, name: "data"}
	file := tcpServerFile(t)
	if _, _, err := holderEnd.WriteMsgUnix([]byte{StatusOK}, syscall.UnixRights(int(file.Fd())), nil); err != nil {
		t.Fatal(err)
	}
	holderEnd.Close()

	// Act
	conn, firstErr := r.Next()
	_, secondErr := r.Next()

	// Assert: the connection received is kept, and the receipt's failure is
	// answered next.
	if firstErr != nil {
		t.Fatalf("the first Next = %v, want the connection", firstErr)
	}
	conn.Close()
	if !errors.Is(secondErr, syscall.EPIPE) {
		t.Fatalf("the second Next = %v, want the receipt's failure", secondErr)
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

func TestTheProtocolsSpellingsArePinned(t *testing.T) {
	// The wire spellings are what every holder and client already speak: a
	// holder of one build answers a client of another, so none may move.
	cases := []struct {
		name string
		got  string
		want string
	}{
		{name: "take", got: string(VerbTake), want: "take"},
		{name: "probe", got: string(VerbProbe), want: "probe"},
		{name: "return", got: string(VerbReturn), want: "return"},
		{name: "accept", got: string(VerbAccept), want: "accept"},
		{name: "a return's request prefix", got: VerbReturn.prefix(), want: "return "},
		{name: "an accept's request prefix", got: VerbAccept.prefix(), want: "accept "},
		{name: "the granted status", got: string(StatusOK), want: "1"},
		{name: "the refused status", got: string(StatusRefused), want: "0"},
		{name: "a stream's receipt", got: string(StreamReceived), want: "r"},
		{name: "a stream's stop", got: string(StreamStop), want: "s"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Assert
			if tc.got != tc.want {
				t.Fatalf("%s = %q, want %q", tc.name, tc.got, tc.want)
			}
		})
	}
}

func TestStoppingAnEndedStreamIsNoFailure(t *testing.T) {
	// A STREAM ALREADY ENDED IS STOPPED: whether Next read the end first
	// never changes Stop's answer (2026-11-22: a stream the holder had
	// refused and closed failed the daemon's stop, and the boot claim was
	// never yielded).
	cases := []struct {
		name string
		// end ends the stream from the holder's side, and reads it with Next
		// when the case says so.
		end func(t *testing.T, holderEnd *net.UnixConn, r *Returned)
	}{
		{name: "after Next read the stream's EOF, nothing is sent", end: func(t *testing.T, holderEnd *net.UnixConn, r *Returned) {
			if err := holderEnd.CloseWrite(); err != nil {
				t.Fatal(err)
			}
			if _, err := r.Next(); !errors.Is(err, io.EOF) {
				t.Fatalf("Next = %v, want io.EOF", err)
			}
		}},
		{name: "after Next read a refusal, nothing is sent", end: func(t *testing.T, holderEnd *net.UnixConn, r *Returned) {
			if _, err := holderEnd.Write([]byte(string(StatusRefused) + "no such socket\n")); err != nil {
				t.Fatal(err)
			}
			if _, err := r.Next(); !errors.Is(err, ErrRefused) {
				t.Fatalf("Next = %v, want ErrRefused", err)
			}
		}},
		{name: "a holder end closed whose end Next never read", end: func(t *testing.T, holderEnd *net.UnixConn, _ *Returned) {
			if err := holderEnd.Close(); err != nil {
				t.Fatal(err)
			}
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			holderEnd, clientEnd := unixPair(t)
			r := &Returned{uc: clientEnd, name: "data"}
			tc.end(t, holderEnd, r)

			// Act
			err := r.Stop()

			// Assert
			if err != nil {
				t.Fatalf("Stop = %v, want nil: the stream had ended", err)
			}
			if !r.hasEnded() {
				t.Fatal("the stream is not recorded ended")
			}
			r.Close()
			if got, rerr := io.ReadAll(holderEnd); rerr == nil && len(got) != 0 {
				t.Fatalf("the holder read %q after the end, want nothing sent", got)
			}
		})
	}
}

func TestStoppingAStreamAnswersAWriteFailureThatIsNotTheHoldersEnd(t *testing.T) {
	// Arrange: the stream's own end is closed, so the write fails with no
	// sign of the holder.
	_, clientEnd := unixPair(t)
	r := &Returned{uc: clientEnd, name: "data"}
	clientEnd.Close()

	// Act
	err := r.Stop()

	// Assert
	if err == nil || !errors.Is(err, net.ErrClosed) {
		t.Fatalf("Stop = %v, want the write's failure", err)
	}
}

func TestPeerGoneNamesOnlyTheErrnosOfAnEndGone(t *testing.T) {
	cases := []struct {
		name string
		err  error
		want bool
	}{
		{name: "a broken pipe", err: &net.OpError{Op: "write", Err: os.NewSyscallError("write", syscall.EPIPE)}, want: true},
		{name: "not connected", err: &net.OpError{Op: "write", Err: os.NewSyscallError("write", syscall.ENOTCONN)}, want: true},
		{name: "a reset", err: &net.OpError{Op: "write", Err: os.NewSyscallError("write", syscall.ECONNRESET)}, want: true},
		{name: "a closed connection", err: net.ErrClosed},
		{name: "a bad descriptor", err: syscall.EBADF},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := peerGone(tc.err)

			// Assert
			if got != tc.want {
				t.Fatalf("peerGone(%v) = %v, want %v", tc.err, got, tc.want)
			}
		})
	}
}
