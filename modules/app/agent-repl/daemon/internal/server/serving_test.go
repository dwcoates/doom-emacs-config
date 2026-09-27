package server

import (
	"context"
	"crypto/tls"
	"io"
	"net"
	"net/http"
	"net/http/httptest"
	"sync/atomic"
	"testing"
	"time"

	"golang.org/x/net/http2"
)

// TestAwaitQuietWaitsForACallStillBeingAnswered pins the whole point of the
// gate: an exit that fired while a unary call was in flight must not proceed
// until that call has been answered.
func TestAwaitQuietWaitsForACallStillBeingAnswered(t *testing.T) {
	// Arrange: a handler parked inside one unary call.
	entered := make(chan struct{})
	release := make(chan struct{})
	serving := H2C(http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
		close(entered)
		<-release
		w.WriteHeader(http.StatusOK)
	}), nil)
	httpServer := servedThroughTheGate(serving)
	defer httpServer.Close()
	answered := make(chan struct{})
	go func() {
		defer close(answered)
		resp, err := httpServer.Client().Get(httpServer.URL + "/agentrepl.v1.AgentRepl/SelectWorkspace")
		if err == nil {
			_ = resp.Body.Close()
		}
	}()
	<-entered

	// Act: the exit's wait, on a bound the parked call will outlast.
	quiet := make(chan int, 1)
	go func() { quiet <- serving.AwaitQuiet(time.Minute) }()

	// Assert: it does not settle while the call is being answered, and does
	// the moment it has been.
	select {
	case left := <-quiet:
		t.Fatalf("AwaitQuiet returned %d while a call was still being answered", left)
	case <-time.After(50 * time.Millisecond):
	}
	close(release)
	if left := <-quiet; left != 0 {
		t.Fatalf("AwaitQuiet = %d after the call was answered, want 0", left)
	}
	<-answered
}

// TestAwaitQuietReportsTheCallsItGaveUpOn pins the loud half: a bound that
// expires answers how many callers are about to read a cut connection.
func TestAwaitQuietReportsTheCallsItGaveUpOn(t *testing.T) {
	// Arrange
	entered := make(chan struct{})
	release := make(chan struct{})
	serving := H2C(http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
		close(entered)
		<-release
		w.WriteHeader(http.StatusOK)
	}), nil)
	httpServer := servedThroughTheGate(serving)
	// THE PARKED HANDLER IS RELEASED BEFORE THE SERVER IS CLOSED: httptest's
	// Close waits for every outstanding request, so the reverse order is a
	// deadlock rather than a teardown.
	defer httpServer.Close()
	defer close(release)
	go func() {
		resp, err := httpServer.Client().Get(httpServer.URL + "/agentrepl.v1.AgentRepl/SelectWorkspace")
		if err == nil {
			_ = resp.Body.Close()
		}
	}()
	<-entered

	// Act
	left := serving.AwaitQuiet(10 * time.Millisecond)

	// Assert
	if left != 1 {
		t.Fatalf("AwaitQuiet = %d after its bound expired on one parked call, want 1", left)
	}
}

// TestAwaitQuietDoesNotWaitForAStandingStream pins the exclusion. A Watch*
// handler returns only when its client goes away, so counting one would spend
// the exit's whole grace on every stop and bound nothing.
func TestAwaitQuietDoesNotWaitForAStandingStream(t *testing.T) {
	// Arrange
	entered := make(chan struct{})
	release := make(chan struct{})
	serving := H2C(http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
		close(entered)
		<-release
		w.WriteHeader(http.StatusOK)
	}), nil)
	httpServer := servedThroughTheGate(serving)
	defer httpServer.Close()
	defer close(release)
	go func() {
		resp, err := httpServer.Client().Get(httpServer.URL + "/agentrepl.v1.AgentRepl/WatchDaemon")
		if err == nil {
			_ = resp.Body.Close()
		}
	}()
	<-entered

	// Act, Assert
	if left := serving.AwaitQuiet(time.Minute); left != 0 {
		t.Fatalf("AwaitQuiet = %d while only a standing stream was open, want 0 at once", left)
	}
}

// TestStandingStreamPathsComeFromTheDescriptor pins that the exclusion set is
// derived rather than listed: every server-streaming verb in the service is in
// it, and no unary one is.
func TestStandingStreamPathsComeFromTheDescriptor(t *testing.T) {
	// Arrange, Act
	paths := serverStreamPaths()

	// Assert
	if !paths["/agentrepl.v1.AgentRepl/WatchDaemon"] {
		t.Fatalf("WatchDaemon is not in the standing-stream set: %v", paths)
	}
	if paths["/agentrepl.v1.AgentRepl/SelectWorkspace"] {
		t.Fatal("SelectWorkspace, a unary verb, is in the standing-stream set")
	}
}

// TestAwaitQuietWaitsForTheAnswerToLeaveNotForTheHandlerToReturn is the
// difference this gate has twice got wrong: the handler had returned, the exit
// proceeded, and the caller still read a cut connection because the response
// had not been written yet.
//
// IT DRIVES A REAL SOCKET, and it has to. The version this replaces asserted
// against `httptest.NewRecorder` and a hand-made request context, on the belief
// that the request context ends when the stream closes. It does not:
// `serverConn.runHandler` cancels it BEFORE the same defer produces the
// answer's frames, so that test passed over the very defect it was named for.
// Here the connection's own writes are held, which is the only statement of
// "the answer has not left" that is not a belief about http2's internals.
func TestAwaitQuietWaitsForTheAnswerToLeaveNotForTheHandlerToReturn(t *testing.T) {
	// Arrange: one h2c call whose response frames cannot reach the socket
	// until the test lets them.
	var held atomic.Bool
	release := make(chan struct{})
	returned := make(chan struct{})
	serving := H2C(http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
		_, _ = io.WriteString(w, "ok")
		held.Store(true)
		close(returned)
	}), nil)
	tcp, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatalf("listen: %v", err)
	}
	httpServer := &http.Server{Handler: serving}
	go func() {
		_ = httpServer.Serve(serving.Listener(&heldWriteListener{Listener: tcp, held: &held, release: release}))
	}()
	t.Cleanup(func() { _ = httpServer.Close() })
	client := &http.Client{Transport: &http2.Transport{
		AllowHTTP: true,
		DialTLSContext: func(ctx context.Context, network, addr string, _ *tls.Config) (net.Conn, error) {
			var d net.Dialer
			return d.DialContext(ctx, network, addr)
		},
	}}
	go func() {
		resp, err := client.Post("http://"+tcp.Addr().String()+"/agentrepl.v1.AgentRepl/SelectWorkspace", "application/proto", nil)
		if err == nil {
			_ = resp.Body.Close()
		}
	}()
	<-returned

	// Act, Assert: the gate holds while the answer is still unwritten, and
	// lets go once the socket has taken it.
	quiet := make(chan int, 1)
	go func() { quiet <- serving.AwaitQuiet(time.Minute) }()
	select {
	case left := <-quiet:
		t.Fatalf("AwaitQuiet returned %d with the answer's own frames not yet written to the socket", left)
	case <-time.After(50 * time.Millisecond):
	}
	close(release)
	if left := <-quiet; left != 0 {
		t.Fatalf("AwaitQuiet = %d once the answer was written, want 0", left)
	}
}

// heldWriteListener hands out connections whose writes park until the test
// releases them, standing in for a socket that has not taken the answer yet.
type heldWriteListener struct {
	net.Listener
	held    *atomic.Bool
	release chan struct{}
}

func (l *heldWriteListener) Accept() (net.Conn, error) {
	conn, err := l.Listener.Accept()
	if err != nil {
		return nil, err
	}
	return &heldWriteConn{Conn: conn, held: l.held, release: l.release}, nil
}

type heldWriteConn struct {
	net.Conn
	held    *atomic.Bool
	release chan struct{}
}

func (c *heldWriteConn) Write(p []byte) (int, error) {
	if c.held.Load() {
		<-c.release
	}
	return c.Conn.Write(p)
}

// servedThroughTheGate starts a test server the way run.go serves the real one:
// through the gate's OWN listener, so a call counts as answered only once its
// bytes have left the socket. A test that served the bare listener would be
// testing a gate the daemon does not run.
func servedThroughTheGate(serving *Serving) *httptest.Server {
	httpServer := httptest.NewUnstartedServer(serving)
	httpServer.Listener = serving.Listener(httpServer.Listener)
	httpServer.Start()
	return httpServer
}

// TestWriteBarrierIsNotQuietUntilTheAnswerHasBeenWritten pins the fact the
// whole barrier exists for: a handler that has returned has not yet spoken, and
// the barrier says so until the socket has taken its bytes.
func TestWriteBarrierIsNotQuietUntilTheAnswerHasBeenWritten(t *testing.T) {
	// Arrange: a barrier that has seen no write at all.
	barrier := &WriteBarrier{}
	mark := barrier.Mark()

	// Act.
	quiet := barrier.AwaitWrittenSince(mark, 20*time.Millisecond)

	// Assert.
	if quiet {
		t.Fatal("AwaitWrittenSince = true with nothing written since the mark; an exit would run over the answer")
	}
}

// TestWriteBarrierSettlesOnceTheConnectionHasSpokenAndStopped pins the other
// half: a write past the mark, followed by quiet, is the answer having left.
func TestWriteBarrierSettlesOnceTheConnectionHasSpokenAndStopped(t *testing.T) {
	// Arrange.
	barrier := &WriteBarrier{}
	inner, outer := net.Pipe()
	t.Cleanup(func() { _ = inner.Close(); _ = outer.Close() })
	drained := make(chan struct{})
	go func() {
		defer close(drained)
		_, _ = io.ReadFull(outer, make([]byte, 4))
	}()
	counted := (&barrierListener{barrier: barrier}).count(inner)
	mark := barrier.Mark()

	// Act.
	if _, err := counted.Write([]byte("done")); err != nil {
		t.Fatalf("Write() = %v, want the bytes to go out", err)
	}
	<-drained
	quiet := barrier.AwaitWrittenSince(mark, time.Second)

	// Assert.
	if !quiet {
		t.Fatal("AwaitWrittenSince = false after the connection wrote and went quiet")
	}
}

// TestWriteBarrierDoesNotCallABusyConnectionALostAnswer pins the second way the
// barrier settles true: a connection that never falls silent within the bound
// has still written past the mark, and reporting that as a lost answer would
// put an ERROR record against a healthy run.
func TestWriteBarrierDoesNotCallABusyConnectionALostAnswer(t *testing.T) {
	// Arrange: a connection that writes without pause for the whole bound.
	barrier := &WriteBarrier{}
	inner, outer := net.Pipe()
	t.Cleanup(func() { _ = inner.Close(); _ = outer.Close() })
	stop := make(chan struct{})
	go func() {
		buf := make([]byte, 1)
		for {
			if _, err := outer.Read(buf); err != nil {
				return
			}
		}
	}()
	counted := (&barrierListener{barrier: barrier}).count(inner)
	go func() {
		for {
			select {
			case <-stop:
				return
			default:
			}
			if _, err := counted.Write([]byte("x")); err != nil {
				return
			}
		}
	}()
	mark := barrier.Mark()

	// Act.
	settled := barrier.AwaitWrittenSince(mark, 20*time.Millisecond)
	close(stop)

	// Assert.
	if !settled {
		t.Fatal("AwaitWrittenSince = false on a connection that wrote past the mark throughout; a busy link is not a lost answer")
	}
}

// TestAwaitWritesQuietSettlesOnABarrierNothingHasWrittenTo covers the exit of a
// daemon nothing is connected to: it owes nothing, so it must not spend the
// bound proving that.
func TestAwaitWritesQuietSettlesOnABarrierNothingHasWrittenTo(t *testing.T) {
	// Arrange
	barrier := &WriteBarrier{}

	// Act
	quiet := barrier.AwaitQuiescent(20 * time.Millisecond)

	// Assert
	if !quiet {
		t.Fatal("AwaitQuiescent = false on a barrier no connection ever wrote to; the exit would wait out its bound for nothing")
	}
}

// TestAwaitWritesQuietWaitsForAPushStillOnItsWayOut is the whole reason the
// quiescent half exists: a push onto a STANDING stream is not a counted call,
// so nothing else in the exit is holding it.
func TestAwaitWritesQuietWaitsForAPushStillOnItsWayOut(t *testing.T) {
	// Arrange: a connection whose Write is blocked mid-flight, exactly as a
	// push whose bytes have not reached the socket is.
	barrier := &WriteBarrier{}
	inner, outer := net.Pipe()
	t.Cleanup(func() { _ = inner.Close(); _ = outer.Close() })
	counted := (&barrierListener{barrier: barrier}).count(inner)
	writing := make(chan struct{})
	go func() {
		close(writing)
		_, _ = counted.Write([]byte("push"))
	}()
	<-writing

	// Act: the reader never takes the bytes, so the write stays in flight.
	quiet := barrier.AwaitQuiescent(20 * time.Millisecond)

	// Assert
	if quiet {
		t.Fatal("AwaitQuiescent = true while a write was still in flight; the exit would close the stream over the push")
	}
}

// TestAwaitWritesQuietSettlesOnceThePushHasLeft is the other half: the bytes
// are on the socket, so the exit may close the streams.
func TestAwaitWritesQuietSettlesOnceThePushHasLeft(t *testing.T) {
	// Arrange
	barrier := &WriteBarrier{}
	inner, outer := net.Pipe()
	t.Cleanup(func() { _ = inner.Close(); _ = outer.Close() })
	drained := make(chan struct{})
	go func() {
		defer close(drained)
		_, _ = io.ReadFull(outer, make([]byte, 4))
	}()
	counted := (&barrierListener{barrier: barrier}).count(inner)
	if _, err := counted.Write([]byte("push")); err != nil {
		t.Fatalf("Write() = %v, want the bytes to go out", err)
	}
	<-drained

	// Act
	quiet := barrier.AwaitQuiescent(time.Second)

	// Assert
	if !quiet {
		t.Fatal("AwaitQuiescent = false after the connection wrote and went quiet")
	}
}

// standingStreamServing serves one standing stream whose handler flushes its
// headers and then holds until ended is closed, through a listener whose
// writes park once the handler has returned, until release is closed.
func standingStreamServing(t *testing.T, ended <-chan struct{}, held *atomic.Bool, release chan struct{}) (*Serving, string) {
	t.Helper()
	serving := H2C(http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
		w.WriteHeader(http.StatusOK)
		w.(http.Flusher).Flush()
		<-ended
		held.Store(true)
	}), nil)
	tcp, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatalf("listen: %v", err)
	}
	httpServer := &http.Server{Handler: serving}
	go func() {
		_ = httpServer.Serve(serving.Listener(&heldWriteListener{Listener: tcp, held: held, release: release}))
	}()
	t.Cleanup(func() { _ = httpServer.Close() })
	return serving, "http://" + tcp.Addr().String() + "/agentrepl.v1.AgentRepl/WatchDaemon"
}

// openStream opens the standing stream and answers once its headers arrived,
// so the stream is counted before the test acts.
func openStream(t *testing.T, url string) {
	t.Helper()
	opened := make(chan struct{})
	go func() {
		resp, err := h2cClient().Get(url)
		close(opened)
		if err == nil {
			_, _ = io.Copy(io.Discard, resp.Body)
			_ = resp.Body.Close()
		}
	}()
	<-opened
}

// TestAwaitStreamsEndedWaitsForTheEndFrameToLeave pins that the exit's stream
// wait is a statement about the SOCKET: a standing stream the exit ended has
// returned from its handler before its end frame is written, and the wait
// holds until the socket has taken it.
func TestAwaitStreamsEndedWaitsForTheEndFrameToLeave(t *testing.T) {
	// Arrange
	var held atomic.Bool
	ended, release := make(chan struct{}), make(chan struct{})
	serving, url := standingStreamServing(t, ended, &held, release)
	openStream(t, url)

	// Act
	serving.EndStreams(func() { close(ended) })
	gone := make(chan int, 1)
	go func() { gone <- serving.AwaitStreamsEnded(time.Minute) }()

	// Assert
	select {
	case left := <-gone:
		t.Fatalf("AwaitStreamsEnded returned %d with the end frame not yet written to the socket", left)
	case <-time.After(50 * time.Millisecond):
	}
	close(release)
	if left := <-gone; left != 0 {
		t.Fatalf("AwaitStreamsEnded = %d once the end frame was written, want 0", left)
	}
}

// TestAwaitStreamsEndedReportsTheStreamsStillOpen pins the loud half: a stream
// that did not end within the bound is answered, because its client will read
// a cut connection rather than an end frame.
func TestAwaitStreamsEndedReportsTheStreamsStillOpen(t *testing.T) {
	// Arrange
	var held atomic.Bool
	ended, release := make(chan struct{}), make(chan struct{})
	t.Cleanup(func() { close(ended); close(release) })
	serving, url := standingStreamServing(t, ended, &held, release)
	openStream(t, url)

	// Act
	serving.EndStreams(func() {})
	left := serving.AwaitStreamsEnded(20 * time.Millisecond)

	// Assert
	if left != 1 {
		t.Fatalf("AwaitStreamsEnded = %d, want the one stream that never ended", left)
	}
}

// TestAwaitStreamsEndedWithNoStreamSettlesAtOnce pins that a daemon nothing
// watches has no end frame to wait for.
func TestAwaitStreamsEndedWithNoStreamSettlesAtOnce(t *testing.T) {
	// Arrange
	serving := H2C(http.NotFoundHandler(), nil)

	// Act
	serving.EndStreams(func() {})
	left := serving.AwaitStreamsEnded(time.Minute)

	// Assert
	if left != 0 {
		t.Fatalf("AwaitStreamsEnded = %d with no stream open, want 0", left)
	}
}

// TestAStreamItsClientEndedIsNotHeldForAnEndFrame pins that only a stream the
// EXIT ended waits on the socket: one that ended without the exit is owed no
// wait, so it is uncounted the moment its handler returns, even while the
// connection's writes are parked.
func TestAStreamItsClientEndedIsNotHeldForAnEndFrame(t *testing.T) {
	// Arrange
	var held atomic.Bool
	ended, release := make(chan struct{}), make(chan struct{})
	t.Cleanup(func() { close(release) })
	serving, url := standingStreamServing(t, ended, &held, release)
	openStream(t, url)

	// Act
	close(ended)
	gone := make(chan int, 1)
	go func() { gone <- serving.AwaitStreamsEnded(time.Minute) }()

	// Assert
	select {
	case left := <-gone:
		if left != 0 {
			t.Fatalf("AwaitStreamsEnded = %d, want 0", left)
		}
	case <-time.After(time.Second):
		t.Fatal("a stream the exit did not end was held waiting on the socket")
	}
}
