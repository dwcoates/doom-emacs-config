package server

import (
	"net"
	"sync"
	"time"
)

// WriteBarrier counts the bytes a listener's connections have actually written,
// so an exit can wait for an answer to LEAVE rather than for its handler to
// return.
//
// WHY THE OBVIOUS SIGNALS DO NOT SAY THIS, which is the whole reason this type
// exists rather than one more `<-r.Context().Done()`.
//
// The comment this replaces claimed "net/http cancels a request's context once
// its stream is closed, which is after the last frame of the answer was
// written". It does not. In `golang.org/x/net/http2`, `serverConn.runHandler`
// cancels the request's context in a DEFER that runs the instant the handler
// function returns, and only THEN calls `rw.handlerDone()` — the call that
// produces the answer's final frames at all:
//
//	defer func() {
//	    rw.rws.stream.cancelCtx()
//	    ...
//	    rw.handlerDone()
//	}()
//	handler(rw, req)
//
// So `r.Context().Done()` fires BEFORE the answer is even queued, let alone
// written. An exit gated on it still runs over the answer, which is what
// `TestHostRequestedStopLeavesNoProcessBehind` read as `unexpected EOF` on an
// `UpdateShutdownSchedule{now}` the daemon had performed — every run inside the
// e2e sandbox — and what the fake shim's stand-down read as `unexpected EOF`
// from a `KillSession` it had accepted, in about one integration run in ten.
//
// Nor does any later signal help. `CloseNotify` fires from `closeStream`, which
// `wroteFrame` reaches once the END_STREAM frame is in the connection's
// `bufio.Writer` — and the FLUSH of that writer is a separate frame the serve
// goroutine schedules afterwards. There is no exported "the bytes are on the
// socket" in the http2 server. The socket itself is the only place that knows.
//
// So the answer is taken where it is actually true: a `Write` on the connection
// has returned, which for a unix or loopback socket means the bytes are in the
// kernel and survive this process exiting. The same ruling the Node shim
// already carries for the same failure — see `cutWhenQuiet` in
// agent-shim/claude/shim/src/service/server.ts, "THE WAIT IS QUIESCENCE, NOT A
// DELAY".
type WriteBarrier struct {
	mu sync.Mutex
	// active is the number of Write calls currently in the kernel.
	active int
	// written counts completed writes. Monotonic, so a caller can name a
	// moment and ask whether the connection has spoken since.
	written uint64
}

// Listener wraps a listener so every connection it accepts is counted.
//
// The wrapper hides the concrete connection type, so net/http can no longer
// recognize a `*net.TCPConn` for its keep-alive tuning. That costs a loopback
// listener nothing this daemon depends on, and it is the price of seeing the
// writes at all.
func (b *WriteBarrier) Listener(inner net.Listener) net.Listener {
	return &barrierListener{Listener: inner, barrier: b}
}

// Mark names the present moment on the barrier's write counter.
//
// Taken BEFORE the answer exists, so "written past this mark" is evidence the
// answer's own frames reached the socket, rather than a report about somebody
// else's traffic that happened to be quiet.
func (b *WriteBarrier) Mark() uint64 {
	b.mu.Lock()
	defer b.mu.Unlock()
	return b.written
}

// barrierSettle is how long the barrier lets pass with no write completing
// before it calls the connection quiet.
//
// NOT A DELAY: the loop ends on the CONDITION, and the ordinary case spends two
// of these. What it has to cover is one `write(2)` of a few dozen bytes onto a
// unix or loopback socket by a goroutine that is already runnable — microseconds
// — so a millisecond is three orders of magnitude over the work, and the bound
// above it is what covers a connection that never stops.
const barrierSettle = time.Millisecond

// AwaitWrittenSince waits for the connections to have written past mark, and
// reports whether the answer can be said to have left.
//
// TWO WAYS TO SETTLE TRUE, and both are statements about the socket rather than
// concessions.
//
//   - QUIET is the ordinary one and the one the exit wants: something was
//     written past the mark and then nothing was, so the connection owes
//     nothing.
//   - STILL SPEAKING is the other, and it exists because a busy connection must
//     not be reported as a lost answer. One h2 connection multiplexes this
//     daemon's standing pushes alongside its unary calls, so a connection can
//     write without pause for the whole bound while a turn is running. It has
//     written past the mark; waiting for it to fall silent would turn an active
//     link into an ERROR record about an answer that did go out, and the e2e
//     warning sweep would fail an entirely healthy run with it.
//
// FALSE means one thing only: NOTHING was written since the mark. The answer
// was produced and the socket never took a byte of it. Every caller records
// that; nothing is swallowed.
func (b *WriteBarrier) AwaitWrittenSince(mark uint64, bound time.Duration) bool {
	deadline := time.Now().Add(bound)
	ticker := time.NewTicker(barrierSettle)
	defer ticker.Stop()
	previous := mark
	for {
		<-ticker.C
		b.mu.Lock()
		written, active := b.written, b.active
		b.mu.Unlock()
		if active == 0 && written > mark && written == previous {
			return true
		}
		previous = written
		if !time.Now().Before(deadline) {
			return written > mark
		}
	}
}

// AwaitQuiescent waits, bounded, for the connections to stop writing, and
// reports whether they did.
//
// IT IS THE HALF OF THE EXIT `AwaitWrittenSince` DOES NOT COVER. `Serving`'s
// counted gate skips `standingStreamPaths` on purpose — a Watch* handler does
// not return until its client goes away, so counting it would make every exit
// wait out its whole grace — but the daemon's LAST WORDS go out on exactly
// those streams. `DaemonShutdownAnnounced` is pushed onto every `WatchDaemon`
// stream and the drain then calls `Exit` on the next line, so the exit's own
// `AwaitQuiet` (which sees no in-flight unary call at all) returns at once and
// `Server.Shutdown` closes the streams over a push that has not reached the
// socket. That is the same failure the unary half of this file exists for, on
// the path that carries the announcement a client acts on.
//
// There is no mark here, and that is deliberate: the exit is not asking
// whether ONE answer left, it is asking whether the connections owe anything
// at all. So a barrier that has never been written to is quiescent, which is
// the correct answer for a daemon nothing is connected to.
//
// FALSE means the connections were STILL WRITING when the bound expired. That
// is not automatically a fault — one h2 connection multiplexes the standing
// pushes with everything else, and a link that is genuinely busy will not fall
// silent — so the caller states it rather than this type deciding it is an
// error.
func (b *WriteBarrier) AwaitQuiescent(bound time.Duration) bool {
	deadline := time.Now().Add(bound)
	ticker := time.NewTicker(barrierSettle)
	defer ticker.Stop()
	b.mu.Lock()
	previous := b.written
	b.mu.Unlock()
	for {
		<-ticker.C
		b.mu.Lock()
		written, active := b.written, b.active
		b.mu.Unlock()
		if active == 0 && written == previous {
			return true
		}
		previous = written
		if !time.Now().Before(deadline) {
			return false
		}
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

// barrierConn is one counted connection.
type barrierConn struct {
	net.Conn
	barrier *WriteBarrier
}

func (c *barrierConn) Write(p []byte) (int, error) {
	c.barrier.mu.Lock()
	c.barrier.active++
	c.barrier.mu.Unlock()
	n, err := c.Conn.Write(p)
	c.barrier.mu.Lock()
	c.barrier.active--
	c.barrier.written++
	c.barrier.mu.Unlock()
	return n, err
}
