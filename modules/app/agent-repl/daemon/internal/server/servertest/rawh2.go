// Package servertest drives the daemon's h2c serving at the frame level, for
// a test whose subject is what the frames say: a GOAWAY and the last stream
// it admits, a stream answered or never admitted. An http.Client hides
// exactly these: it retries a stream a GOAWAY refused and never shows the
// frame.
package servertest

import (
	"bytes"
	"errors"
	"net"
	"sync"
	"testing"

	"golang.org/x/net/http2"
	"golang.org/x/net/http2/hpack"
)

// Frame is one frame the server sent, as far as a test reads it, or the
// read's end.
type Frame struct {
	Type      http2.FrameType
	Stream    uint32
	EndStream bool
	// Ack is a SETTINGS or PING acknowledgement.
	Ack bool
	// LastStream is a GOAWAY's last admitted stream.
	LastStream uint32
	// Err ends the frames: the connection read failed or closed.
	Err error
}

// Conn is one prior-knowledge h2 connection a test writes frames on by hand.
type Conn struct {
	conn net.Conn
	mu   sync.Mutex
	fr   *http2.Framer
	// frames carries every frame the server sent, in order, ending with
	// one whose Err is set.
	frames chan Frame
	// stop ends the reader's sends once the test is over; done is closed
	// when the reader returned.
	stop, done chan struct{}
}

// Dial opens a prior-knowledge h2 connection to addr: the preface and an
// empty SETTINGS, every server SETTINGS acknowledged as it comes.
func Dial(t testing.TB, addr string) *Conn {
	t.Helper()
	nc, err := net.Dial("tcp", addr)
	if err != nil {
		t.Fatalf("dial %s: %v", addr, err)
	}
	c := &Conn{conn: nc, fr: http2.NewFramer(nc, nc), frames: make(chan Frame, 256), stop: make(chan struct{}), done: make(chan struct{})}
	t.Cleanup(func() {
		close(c.stop)
		_ = nc.Close()
		<-c.done
	})
	if _, err := nc.Write([]byte(http2.ClientPreface)); err != nil {
		t.Fatalf("write the client preface: %v", err)
	}
	c.write(t, func(fr *http2.Framer) error { return fr.WriteSettings() })
	// The test's cleanup closes the connection and waits for the reader.
	// inflight: joined: the cleanup above waits for it.
	go c.read()
	return c
}

func (c *Conn) write(t testing.TB, w func(*http2.Framer) error) {
	t.Helper()
	c.mu.Lock()
	defer c.mu.Unlock()
	if err := w(c.fr); err != nil {
		t.Fatalf("write a frame: %v", err)
	}
}

// send hands the test one frame, unless the test is over.
func (c *Conn) send(f Frame) bool {
	select {
	case c.frames <- f:
		return true
	case <-c.stop:
		return false
	}
}

func (c *Conn) read() {
	defer close(c.done)
	defer close(c.frames)
	for {
		f, err := c.fr.ReadFrame()
		if err != nil {
			c.send(Frame{Err: err})
			return
		}
		h := f.Header()
		out := Frame{Type: h.Type, Stream: h.StreamID, EndStream: h.Flags.Has(http2.FlagDataEndStream)}
		switch f := f.(type) {
		case *http2.SettingsFrame:
			out.Ack = f.IsAck()
			if !out.Ack {
				c.mu.Lock()
				err := c.fr.WriteSettingsAck()
				c.mu.Unlock()
				if err != nil {
					c.send(Frame{Err: err})
					return
				}
			}
		case *http2.PingFrame:
			out.Ack = f.IsAck()
		case *http2.GoAwayFrame:
			out.LastStream = f.LastStreamID
		}
		if !c.send(out) {
			return
		}
	}
}

// Request opens stream with a GET of path, its request complete.
func (c *Conn) Request(t testing.TB, stream uint32, path string) {
	t.Helper()
	var block bytes.Buffer
	enc := hpack.NewEncoder(&block)
	for _, f := range []hpack.HeaderField{
		{Name: ":method", Value: "GET"},
		{Name: ":scheme", Value: "http"},
		{Name: ":authority", Value: c.conn.RemoteAddr().String()},
		{Name: ":path", Value: path},
	} {
		if err := enc.WriteField(f); err != nil {
			t.Fatalf("encode %s: %v", f.Name, err)
		}
	}
	c.write(t, func(fr *http2.Framer) error {
		return fr.WriteHeaders(http2.HeadersFrameParam{StreamID: stream, BlockFragment: block.Bytes(), EndStream: true, EndHeaders: true})
	})
}

// Ping sends a PING; its acknowledgement proves the server read every frame
// sent before it.
func (c *Conn) Ping(t testing.TB, data [8]byte) {
	t.Helper()
	c.write(t, func(fr *http2.Framer) error { return fr.WritePing(false, data) })
}

// ErrNotSeen is Await's answer when the connection's frames ended first.
var ErrNotSeen = errors.New("servertest: the connection ended before the frame awaited")

// Await reads the server's frames until one matches, and answers it. Once
// the connection's frames end first it answers ErrNotSeen, the returned
// Frame carrying the read's own error.
func (c *Conn) Await(match func(Frame) bool) (Frame, error) {
	for f := range c.frames {
		if f.Err != nil {
			return f, ErrNotSeen
		}
		if match(f) {
			return f, nil
		}
	}
	return Frame{}, ErrNotSeen
}

// Ended matches the server's end of stream: END_STREAM or a RST_STREAM.
func Ended(stream uint32) func(Frame) bool {
	return func(f Frame) bool {
		return f.Stream == stream && (f.Type == http2.FrameRSTStream || (f.EndStream && (f.Type == http2.FrameData || f.Type == http2.FrameHeaders)))
	}
}

// GoAway matches the server's GOAWAY.
func GoAway(f Frame) bool { return f.Type == http2.FrameGoAway }

// PingAck matches a PING's acknowledgement.
func PingAck(f Frame) bool { return f.Type == http2.FramePing && f.Ack }
