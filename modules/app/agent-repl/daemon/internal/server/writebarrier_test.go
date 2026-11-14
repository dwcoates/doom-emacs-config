package server

import (
	"bytes"
	"encoding/binary"
	"errors"
	"io"
	"net"
	"testing"
	"time"
)

// scriptedConn is a connection whose reads are a script and whose writes are
// kept, or fail with writeErr.
type scriptedConn struct {
	net.Conn
	reads    *bytes.Reader
	written  bytes.Buffer
	writeErr error
}

func (c *scriptedConn) Read(p []byte) (int, error) { return c.reads.Read(p) }

func (c *scriptedConn) Write(p []byte) (int, error) {
	if c.writeErr != nil {
		return 0, c.writeErr
	}
	return c.written.Write(p)
}

func (c *scriptedConn) Close() error         { return nil }
func (c *scriptedConn) RemoteAddr() net.Addr { return &net.UnixAddr{Name: "client", Net: "unix"} }

// frame is one h2 frame with a zero payload of length n.
func frame(kind, flags byte, stream uint32, n int) []byte {
	out := make([]byte, 9+n)
	out[0], out[1], out[2] = byte(n>>16), byte(n>>8), byte(n)
	out[3], out[4] = kind, flags
	binary.BigEndian.PutUint32(out[5:9], stream)
	return out
}

// h2Conn is a counted connection whose client sent the preface and then
// INCOMING, all of it already read.
func h2Conn(t *testing.T, b *WriteBarrier, incoming ...[]byte) *barrierConn {
	t.Helper()
	script := append([]byte{}, clientPreface...)
	for _, f := range incoming {
		script = append(script, f...)
	}
	c := (&barrierListener{barrier: b}).count(&scriptedConn{reads: bytes.NewReader(script)}).(*barrierConn)
	if _, err := io.ReadAll(c); err != nil {
		t.Fatalf("reading the script: %v", err)
	}
	return c
}

// write sends server frames on c.
func write(t *testing.T, c *barrierConn, frames ...[]byte) {
	t.Helper()
	if _, err := c.Write(bytes.Join(frames, nil)); err != nil {
		t.Fatalf("Write() = %v", err)
	}
}

func openCount(b *WriteBarrier) int {
	n, _ := b.Open()
	return n
}

func TestTheBarrierCountsEachStreamItsClientOpens(t *testing.T) {
	// Arrange.
	b := &WriteBarrier{}

	// Act.
	h2Conn(t, b, frame(frameHeaders, flagEndHeaders, 1, 5), frame(frameHeaders, flagEndHeaders, 3, 5))

	// Assert.
	if got := openCount(b); got != 2 {
		t.Fatalf("open = %d, want 2", got)
	}
}

func TestAClientsTrailersDoNotOpenItsStreamTwice(t *testing.T) {
	// Arrange.
	b := &WriteBarrier{}

	// Act.
	h2Conn(t, b, frame(frameHeaders, flagEndHeaders, 1, 5), frame(frameHeaders, flagEndHeaders|flagEndStream, 1, 2))

	// Assert.
	if got := openCount(b); got != 1 {
		t.Fatalf("open = %d, want 1", got)
	}
}

func TestAStreamEndsWithWhatTheServerWrote(t *testing.T) {
	tests := []struct {
		name   string
		frames [][]byte
		want   int
	}{
		{name: "DATA carrying END_STREAM ends it", frames: [][]byte{frame(frameData, flagEndStream, 1, 4)}, want: 0},
		{name: "DATA without END_STREAM leaves it open", frames: [][]byte{frame(frameData, 0, 1, 4)}, want: 1},
		{name: "trailers carrying END_STREAM end it", frames: [][]byte{frame(frameHeaders, flagEndStream|flagEndHeaders, 1, 3)}, want: 0},
		{name: "response headers alone leave it open", frames: [][]byte{frame(frameHeaders, flagEndHeaders, 1, 3)}, want: 1},
		{name: "an END_STREAM header block still being continued leaves it open", frames: [][]byte{frame(frameHeaders, flagEndStream, 1, 3)}, want: 1},
		{name: "the CONTINUATION completing an END_STREAM block ends it", frames: [][]byte{frame(frameHeaders, flagEndStream, 1, 3), frame(frameContinuation, flagEndHeaders, 1, 3)}, want: 0},
		{name: "a CONTINUATION of a block that does not end the stream leaves it open", frames: [][]byte{frame(frameHeaders, 0, 1, 3), frame(frameContinuation, flagEndHeaders, 1, 3)}, want: 1},
		{name: "RST_STREAM ends it", frames: [][]byte{frame(frameRSTStream, 0, 1, 4)}, want: 0},
		{name: "another stream's end leaves it open", frames: [][]byte{frame(frameData, flagEndStream, 3, 4)}, want: 1},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			b := &WriteBarrier{}
			c := h2Conn(t, b, frame(frameHeaders, flagEndHeaders, 1, 5))

			// Act.
			write(t, c, tt.frames...)

			// Assert.
			if got := openCount(b); got != tt.want {
				t.Fatalf("open = %d, want %d", got, tt.want)
			}
		})
	}
}

func TestAFrameSplitAcrossWritesIsStillRead(t *testing.T) {
	// Arrange.
	b := &WriteBarrier{}
	c := h2Conn(t, b, frame(frameHeaders, flagEndHeaders, 1, 5))
	whole := append(frame(frameData, 0, 1, 6), frame(frameData, flagEndStream, 1, 2)...)

	// Act: one byte at a time.
	for i := range whole {
		write(t, c, whole[i:i+1])
	}

	// Assert.
	if got := openCount(b); got != 0 {
		t.Fatalf("open = %d, want 0", got)
	}
}

func TestAClientsResetEndsItsStream(t *testing.T) {
	// Arrange.
	b := &WriteBarrier{}

	// Act.
	h2Conn(t, b, frame(frameHeaders, flagEndHeaders, 1, 5), frame(frameRSTStream, 0, 1, 4))

	// Assert.
	if got := openCount(b); got != 0 {
		t.Fatalf("open = %d, want 0", got)
	}
}

func TestAnHTTP1ConnectionIsNotCounted(t *testing.T) {
	// Arrange.
	b := &WriteBarrier{}
	c := (&barrierListener{barrier: b}).count(&scriptedConn{reads: bytes.NewReader([]byte("GET / HTTP/1.1\r\nHost: x\r\n\r\n"))}).(*barrierConn)

	// Act.
	_, _ = io.ReadAll(c)
	_, err := c.Write(frame(frameHeaders, flagEndHeaders, 1, 0))

	// Assert.
	if err != nil || openCount(b) != 0 || c.proto.Load() != protocolHTTP1 {
		t.Fatalf("(err %v, open %d, proto %d), want an uncounted HTTP/1.1 connection", err, openCount(b), c.proto.Load())
	}
}

func TestAFailedWriteOwesNothingOnItsConnection(t *testing.T) {
	// Arrange.
	b := &WriteBarrier{}
	c := h2Conn(t, b, frame(frameHeaders, flagEndHeaders, 1, 5))
	c.Conn.(*scriptedConn).writeErr = errors.New("broken pipe")

	// Act.
	_, err := c.Write(frame(frameData, 0, 1, 4))

	// Assert.
	if err == nil || openCount(b) != 0 {
		t.Fatalf("(err %v, open %d), want the write's error and nothing owed", err, openCount(b))
	}
}

func TestAClosedConnectionOwesNothing(t *testing.T) {
	// Arrange.
	b := &WriteBarrier{}
	c := h2Conn(t, b, frame(frameHeaders, flagEndHeaders, 1, 5))

	// Act.
	_ = c.Close()

	// Assert.
	if got := openCount(b); got != 0 {
		t.Fatalf("open = %d, want 0", got)
	}
}

func TestAwaitStreamsWrittenSettlesOnceEveryEndIsWritten(t *testing.T) {
	// Arrange.
	b := &WriteBarrier{}
	c := h2Conn(t, b, frame(frameHeaders, flagEndHeaders, 1, 5))

	// Act.
	before := b.AwaitStreamsWritten(time.Now())
	write(t, c, frame(frameData, flagEndStream, 1, 0))
	after := b.AwaitStreamsWritten(time.Now().Add(time.Minute))

	// Assert.
	if before != 1 || after != 0 {
		t.Fatalf("AwaitStreamsWritten = (%d before, %d after), want (1, 0)", before, after)
	}
}

func TestAwaitStreamsWrittenWakesOnTheEndItWaitsFor(t *testing.T) {
	// Arrange.
	b := &WriteBarrier{}
	c := h2Conn(t, b, frame(frameHeaders, flagEndHeaders, 1, 5))
	done := make(chan int, 1)
	go func() { done <- b.AwaitStreamsWritten(time.Now().Add(time.Minute)) }()

	// Act.
	write(t, c, frame(frameData, flagEndStream, 1, 0))

	// Assert.
	if left := <-done; left != 0 {
		t.Fatalf("AwaitStreamsWritten = %d, want 0", left)
	}
}

func TestAStreamOpenedOnAClosedConnectionIsNotCounted(t *testing.T) {
	// Arrange.
	b := &WriteBarrier{}
	c := h2Conn(t, b)
	_ = c.Close()

	// Act.
	b.opened(c, 5)

	// Assert.
	if got := openCount(b); got != 0 {
		t.Fatalf("open = %d, want 0", got)
	}
}

// goAwayFrame is the server's GOAWAY naming last as its last admitted stream.
func goAwayFrame(last uint32) []byte {
	out := frame(frameGoAway, 0, 0, 8)
	binary.BigEndian.PutUint32(out[9:13], last)
	return out
}

func TestAGoAwayDropsTheStreamsAboveItsLast(t *testing.T) {
	tests := []struct {
		name    string
		written [][]byte
		want    int
	}{
		{name: "in one write", written: [][]byte{goAwayFrame(1)}, want: 1},
		{name: "split inside its last-stream id", written: [][]byte{goAwayFrame(1)[:11], goAwayFrame(1)[11:]}, want: 1},
		{name: "naming every stream", written: [][]byte{goAwayFrame(3)}, want: 2},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: two streams read, the second of which the server will
			// not have admitted.
			var b WriteBarrier
			c := h2Conn(t, &b, frame(frameHeaders, flagEndHeaders, 1, 0), frame(frameHeaders, flagEndHeaders, 3, 0))

			// Act
			for _, w := range tt.written {
				write(t, c, w)
			}

			// Assert
			if got := openCount(&b); got != tt.want {
				t.Fatalf("open = %d, want %d", got, tt.want)
			}
		})
	}
}

func TestAStreamOpenedAfterTheGoAwayIsNotCounted(t *testing.T) {
	// Arrange
	var b WriteBarrier
	c := h2Conn(t, &b)
	write(t, c, goAwayFrame(1))

	// Act: the client opens a stream the server discards.
	c.readFrames(frame(frameHeaders, flagEndHeaders, 3, 0))

	// Assert
	if got := openCount(&b); got != 0 {
		t.Fatalf("open = %d, want 0: the server never admits a stream above its GOAWAY's last", got)
	}
}

func TestGoAwaySettlesOnceEveryConnectionWroteItsGoAway(t *testing.T) {
	// Arrange
	var b WriteBarrier
	c := h2Conn(t, &b)
	write(t, c, frame(0x4, 0, 0, 0)) // the server's SETTINGS: it is serving.

	// Act
	left := b.GoAway(func() { write(t, c, goAwayFrame(0)) }, time.Now().Add(time.Minute))

	// Assert
	if left != 0 {
		t.Fatalf("GoAway() = %d, want 0", left)
	}
}

func TestGoAwayAnswersTheConnectionsStillOwingIt(t *testing.T) {
	// Arrange
	var b WriteBarrier
	h2Conn(t, &b)

	// Act
	left := b.GoAway(func() {}, time.Now())

	// Assert
	if left != 1 || len(b.Owing()) != 1 {
		t.Fatalf("GoAway() = %d owing %v, want the one connection that never wrote its GOAWAY", left, b.Owing())
	}
}

func TestAConnectionOwesNoGoAway(t *testing.T) {
	tests := []struct {
		name   string
		settle func(t *testing.T, b *WriteBarrier)
	}{
		{name: "once it turned out to be HTTP/1.1", settle: func(t *testing.T, b *WriteBarrier) {
			c := (&barrierListener{barrier: b}).count(&scriptedConn{reads: bytes.NewReader([]byte("GET / HTTP/1.1\r\n\r\n"))})
			if _, err := io.ReadAll(c); err != nil {
				t.Fatalf("reading the script: %v", err)
			}
		}},
		{name: "once it closed", settle: func(t *testing.T, b *WriteBarrier) {
			if err := h2Conn(t, b).Close(); err != nil {
				t.Fatalf("Close() = %v", err)
			}
		}},
		{name: "once its write failed", settle: func(t *testing.T, b *WriteBarrier) {
			c := h2Conn(t, b)
			c.Conn.(*scriptedConn).writeErr = errors.New("broken pipe")
			if _, err := c.Write(frame(0x4, 0, 0, 0)); err == nil {
				t.Fatal("Write() = nil, want the socket's failure")
			}
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			var b WriteBarrier
			tt.settle(t, &b)

			// Act
			left := b.GoAway(func() {}, time.Now())

			// Assert
			if left != 0 {
				t.Fatalf("GoAway() = %d, want 0", left)
			}
		})
	}
}

func TestAConnectionsFirstWriteAfterTheGoAwaySendsItAgain(t *testing.T) {
	tests := []struct {
		name string
		// servingBefore is whether the connection wrote before GoAway.
		servingBefore bool
		want          int
	}{
		{name: "serving starts after the GoAway", servingBefore: false, want: 2},
		{name: "serving started before the GoAway", servingBefore: true, want: 1},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			var b WriteBarrier
			c := h2Conn(t, &b)
			if tt.servingBefore {
				write(t, c, frame(0x4, 0, 0, 0))
			}
			sends := 0
			b.GoAway(func() { sends++ }, time.Now())

			// Act: the server writes on the connection.
			write(t, c, frame(0x4, 0, 0, 0))

			// Assert
			if sends != tt.want {
				t.Fatalf("GOAWAY sends = %d, want %d", sends, tt.want)
			}
		})
	}
}
