package server

import (
	"bytes"
	"encoding/binary"
	"fmt"
	"net"
	"sort"
	"sync"
	"sync/atomic"
	"time"
)

// WriteBarrier knows, for every h2 stream a client opened on a counted
// connection, whether the server's END of that stream is on the socket yet,
// so an exit waits for each answer and each end frame to LEAVE rather than for
// its handler to return.
//
// WHY THE OBVIOUS SIGNALS DO NOT SAY THIS, which is the whole reason this type
// exists rather than one more `<-r.Context().Done()`.
//
// In `golang.org/x/net/http2`, `serverConn.runHandler` cancels the request's
// context in a DEFER that runs the instant the handler function returns, and
// only THEN calls `rw.handlerDone()` — the call that produces the answer's
// final frames at all:
//
//	defer func() {
//	    rw.rws.stream.cancelCtx()
//	    ...
//	    rw.handlerDone()
//	}()
//	handler(rw, req)
//
// So `r.Context().Done()` fires BEFORE the answer is even queued, let alone
// written. An exit gated on it ran over the answer, which is what
// `TestHostRequestedStopLeavesNoProcessBehind` read as `unexpected EOF` on an
// `UpdateShutdownSchedule{now}` the daemon had performed. Nor does any later
// signal help: `CloseNotify` fires once the END_STREAM frame is in the
// connection's `bufio.Writer`, and its FLUSH is a separate write the serve
// goroutine schedules afterwards. There is no exported "the bytes are on the
// socket" in the http2 server.
//
// THE FRAMES SAY IT EXACTLY, so the barrier reads them. Every frame on an h2
// connection begins with a nine-byte header naming its type, flags and
// stream. The barrier follows both directions of every counted connection:
//
//   - a HEADERS frame the CLIENT sends opens its stream (a repeat — trailers —
//     changes nothing);
//   - the stream's END is the server's DATA or HEADERS frame carrying
//     END_STREAM (its header block complete, through any CONTINUATION), or a
//     RST_STREAM from either side;
//   - a server frame counts only once the `Write` that carried it has
//     returned, which for a unix or loopback socket means the bytes are in the
//     kernel and survive this process exiting.
//
// Frames on one stream are written in order, so a stream whose end is on the
// socket has every frame before it there too: its answer, and every push a
// standing stream sent before it ended. Nothing here is a quiet window or a
// clock: the wait is the count's own zero, bounded by the exit's deadline.
//
// A CONNECTION THAT IS NOT h2 IS NOT COUNTED. Its first bytes are not the h2
// client preface, so it is HTTP/1.1, which `http.Server.Shutdown` already
// waits on exactly: such a connection turns idle only once its response is
// flushed. The h2c Upgrade route, the one way an HTTP/1.1 connection becomes
// h2 here, is refused (Serving.ServeHTTP), so no h2 stream is ever uncounted.
type WriteBarrier struct {
	mu sync.Mutex
	// open is, per counted connection, every stream its client opened whose
	// end is not yet on the socket.
	open map[*barrierConn]map[uint32]struct{}
	// total is the sum of open's sets.
	total int
	// drained is closed when total reaches zero, only while somebody waits.
	drained chan struct{}
}

// Listener wraps a listener so every connection it accepts is counted.
//
// The wrapper hides the concrete connection type, so net/http can no longer
// recognize a `*net.TCPConn` for its keep-alive tuning. That costs a loopback
// listener nothing this daemon depends on, and it is the price of seeing the
// frames at all.
func (b *WriteBarrier) Listener(inner net.Listener) net.Listener {
	return &barrierListener{Listener: inner, barrier: b}
}

// Open answers how many streams are open now, and how many on each
// connection that has any, as "addr=N" entries for a record.
func (b *WriteBarrier) Open() (total int, byConn []string) {
	b.mu.Lock()
	defer b.mu.Unlock()
	for c, streams := range b.open {
		if len(streams) > 0 {
			byConn = append(byConn, fmt.Sprintf("%s=%d", c.RemoteAddr(), len(streams)))
		}
	}
	sort.Strings(byConn)
	return b.total, byConn
}

// AwaitStreamsWritten waits until every stream any client opened on a counted
// connection has its end on the socket, or deadline passes, and answers how
// many had not.
func (b *WriteBarrier) AwaitStreamsWritten(deadline time.Time) int {
	b.mu.Lock()
	if b.total == 0 {
		b.mu.Unlock()
		return 0
	}
	if b.drained == nil {
		b.drained = make(chan struct{})
	}
	drained := b.drained
	b.mu.Unlock()
	timer := time.NewTimer(time.Until(deadline))
	defer timer.Stop()
	select {
	case <-drained:
		return 0
	case <-timer.C:
		b.mu.Lock()
		defer b.mu.Unlock()
		return b.total
	}
}

// opened records a stream its client opened.
func (b *WriteBarrier) opened(c *barrierConn, stream uint32) {
	b.mu.Lock()
	defer b.mu.Unlock()
	if c.gone {
		// The connection already closed: nothing can be written for it.
		return
	}
	if b.open == nil {
		b.open = map[*barrierConn]map[uint32]struct{}{}
	}
	streams := b.open[c]
	if streams == nil {
		streams = map[uint32]struct{}{}
		b.open[c] = streams
	}
	if _, ok := streams[stream]; ok {
		return
	}
	streams[stream] = struct{}{}
	b.total++
}

// ended records streams whose end is on the socket.
func (b *WriteBarrier) ended(c *barrierConn, streams ...uint32) {
	if len(streams) == 0 {
		return
	}
	b.mu.Lock()
	defer b.mu.Unlock()
	open := b.open[c]
	for _, stream := range streams {
		if _, ok := open[stream]; ok {
			delete(open, stream)
			b.total--
		}
	}
	b.wakeLocked()
}

// gone records a connection that closed or failed: nothing more can be
// written on it, so none of its streams is owed anything.
func (b *WriteBarrier) gone(c *barrierConn) {
	b.mu.Lock()
	defer b.mu.Unlock()
	c.gone = true
	b.total -= len(b.open[c])
	delete(b.open, c)
	b.wakeLocked()
}

// wakeLocked closes drained once nothing is open. b.mu is held.
func (b *WriteBarrier) wakeLocked() {
	if b.total < 0 {
		panic(fmt.Sprintf("server: the write barrier ended %d more stream(s) than were opened", -b.total))
	}
	if b.total == 0 && b.drained != nil {
		close(b.drained)
		b.drained = nil
	}
}

// barrierListener counts every connection it hands out.
type barrierListener struct {
	net.Listener
	barrier *WriteBarrier
}

func (l *barrierListener) Accept() (net.Conn, error) {
	conn, err := l.Listener.Accept()
	if err != nil {
		return nil, err
	}
	return l.count(conn), nil
}

// count wraps one connection. Separated from Accept so a suite can drive the
// counting over a plain pipe, with no listener in the way.
func (l *barrierListener) count(conn net.Conn) net.Conn {
	return &barrierConn{Conn: conn, barrier: l.barrier}
}

// clientPreface is what every prior-knowledge h2 connection opens with.
var clientPreface = []byte("PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n")

// The frame types and flags the barrier reads (RFC 9113 section 6).
const (
	frameData         = 0x0
	frameHeaders      = 0x1
	frameRSTStream    = 0x3
	frameContinuation = 0x9

	flagEndStream  = 0x1
	flagEndHeaders = 0x4
)

// protocol is what a counted connection turned out to carry.
type protocol = int32

const (
	// protocolUnknown is a connection whose first bytes have not all arrived.
	protocolUnknown protocol = iota
	// protocolH2 opened with the client preface: its frames are followed.
	protocolH2
	// protocolHTTP1 did not: net/http's own Shutdown waits on it.
	protocolHTTP1
)

// barrierConn is one counted connection.
type barrierConn struct {
	net.Conn
	barrier *WriteBarrier

	// The client's side is touched only by the one goroutine reading the
	// connection; proto is settled by it before the server writes any h2
	// frame, because the server writes none until it has read the preface.
	proto    atomic.Int32
	preface  int
	incoming frameScanner

	// writeMu serializes the server's side: one Write at a time is parsed.
	writeMu  sync.Mutex
	outgoing frameScanner
	// endingHeaders are the server's streams whose END_STREAM header block is
	// still being continued; each ends with its last CONTINUATION.
	endingHeaders map[uint32]bool

	// gone is guarded by barrier.mu.
	gone bool
}

func (c *barrierConn) Read(p []byte) (int, error) {
	n, err := c.Conn.Read(p)
	c.readFrames(p[:n])
	return n, err
}

// readFrames follows what the client sent.
func (c *barrierConn) readFrames(p []byte) {
	if c.proto.Load() == protocolUnknown {
		take := min(len(clientPreface)-c.preface, len(p))
		if !bytes.Equal(p[:take], clientPreface[c.preface:c.preface+take]) {
			c.proto.Store(protocolHTTP1)
			return
		}
		c.preface += take
		p = p[take:]
		if c.preface < len(clientPreface) {
			return
		}
		c.proto.Store(protocolH2)
	}
	if c.proto.Load() != protocolH2 {
		return
	}
	c.incoming.feed(p, func(h frameHeader) {
		switch h.kind {
		case frameHeaders:
			c.barrier.opened(c, h.stream)
		case frameRSTStream:
			c.barrier.ended(c, h.stream)
		}
	})
}

func (c *barrierConn) Write(p []byte) (int, error) {
	if c.proto.Load() != protocolH2 {
		return c.Conn.Write(p)
	}
	c.writeMu.Lock()
	defer c.writeMu.Unlock()
	var ends []uint32
	c.outgoing.feed(p, func(h frameHeader) {
		if stream, ok := c.endOf(h); ok {
			ends = append(ends, stream)
		}
	})
	n, err := c.Conn.Write(p)
	if err != nil {
		c.barrier.gone(c)
		return n, err
	}
	c.barrier.ended(c, ends...)
	return n, nil
}

// endOf answers the stream a server frame ends, if it ends one.
func (c *barrierConn) endOf(h frameHeader) (uint32, bool) {
	switch h.kind {
	case frameRSTStream:
		return h.stream, true
	case frameData:
		return h.stream, h.flags&flagEndStream != 0
	case frameHeaders:
		if h.flags&flagEndStream == 0 {
			return 0, false
		}
		if h.flags&flagEndHeaders != 0 {
			return h.stream, true
		}
		if c.endingHeaders == nil {
			c.endingHeaders = map[uint32]bool{}
		}
		c.endingHeaders[h.stream] = true
		return 0, false
	case frameContinuation:
		if h.flags&flagEndHeaders == 0 || !c.endingHeaders[h.stream] {
			return 0, false
		}
		delete(c.endingHeaders, h.stream)
		return h.stream, true
	}
	return 0, false
}

func (c *barrierConn) Close() error {
	c.barrier.gone(c)
	return c.Conn.Close()
}

// frameHeader is the part of an h2 frame header the barrier reads.
type frameHeader struct {
	kind   byte
	flags  byte
	stream uint32
}

// frameScanner finds the frame headers in one direction's byte stream, fed in
// whatever pieces the connection reads or writes it in.
type frameScanner struct {
	header [9]byte
	have   int
	// skip is how many payload bytes of the current frame are still to come.
	skip int
}

// feed scans p, calling found for every frame header it completes.
func (s *frameScanner) feed(p []byte, found func(frameHeader)) {
	for len(p) > 0 {
		if s.skip > 0 {
			n := min(s.skip, len(p))
			s.skip -= n
			p = p[n:]
			continue
		}
		n := copy(s.header[s.have:], p)
		s.have += n
		p = p[n:]
		if s.have < len(s.header) {
			return
		}
		s.have = 0
		s.skip = int(s.header[0])<<16 | int(s.header[1])<<8 | int(s.header[2])
		found(frameHeader{
			kind:   s.header[3],
			flags:  s.header[4],
			stream: binary.BigEndian.Uint32(s.header[5:9]) & 0x7fffffff,
		})
	}
}
