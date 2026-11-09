package server

import (
	"agent-repld/internal/dlog"
	"agent-repld/internal/inflight"
	"io"
	"net"
	"net/http"
	"net/http/httptest"
	neturl "net/url"
	"reflect"
	"sync"
	"testing"
	"time"
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

	// Act: the exit's wait while the call is parked, then once it returned.
	parked := serving.AwaitQuiet(0)
	close(release)
	<-answered
	settled := serving.AwaitQuiet(time.Minute)

	// Assert.
	if parked != 1 || settled != 0 {
		t.Fatalf("AwaitQuiet = (%d parked, %d answered), want (1, 0)", parked, settled)
	}
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
	left := serving.AwaitQuiet(0)

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

// TestAwaitStreamsEndedWaitsForTheAnswerToLeave is the difference this gate
// has twice got wrong: the handler had returned, the exit proceeded, and the
// caller still read a cut connection because the response had not been
// written yet.
//
// IT DRIVES A REAL SOCKET, and it has to: the connection's own writes are
// held, which is the only statement of "the answer has not left" that is not
// a belief about http2's internals.
func TestAwaitStreamsEndedWaitsForTheAnswerToLeave(t *testing.T) {
	// Arrange: one h2c call whose response frames cannot reach the socket
	// until the test lets them.
	var held writeHold
	release := make(chan struct{})
	returned := make(chan struct{})
	serving := H2C(http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
		_, _ = io.WriteString(w, "ok")
		held.hold()
		close(returned)
	}), nil)
	url := serveHeld(t, serving, &held, release)
	answered := make(chan struct{})
	go func() {
		defer close(answered)
		resp, err := h2cClient().Post(url+"/agentrepl.v1.AgentRepl/SelectWorkspace", "application/proto", nil)
		if err == nil {
			_, _ = io.Copy(io.Discard, resp.Body)
			_ = resp.Body.Close()
		}
	}()
	<-returned
	serving.EndStreams(time.Now(), func() {})

	// Act: the wait with the answer parked, then once the socket took it.
	parked := serving.AwaitStreamsEnded(time.Now())
	close(release)
	<-answered
	written := serving.AwaitStreamsEnded(time.Now().Add(time.Minute))

	// Assert.
	if parked != 1 || written != 0 {
		t.Fatalf("AwaitStreamsEnded = (%d parked, %d written), want (1, 0)", parked, written)
	}
}

// TestAnAnswerNotWrittenByTheExitsDeadlineIsRecordedAtError pins the loud
// half: the record says how many streams were left and on which connection.
func TestAnAnswerNotWrittenByTheExitsDeadlineIsRecordedAtError(t *testing.T) {
	// Arrange.
	var held writeHold
	release := make(chan struct{})
	t.Cleanup(func() { close(release) })
	returned := make(chan struct{})
	log := dlog.NewTestLogger()
	serving := H2C(http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
		_, _ = io.WriteString(w, "ok")
		held.hold()
		close(returned)
	}), log)
	url := serveHeld(t, serving, &held, release)
	go func() {
		resp, err := h2cClient().Post(url+"/agentrepl.v1.AgentRepl/SelectWorkspace", "application/proto", nil)
		if err == nil {
			_ = resp.Body.Close()
		}
	}()
	<-returned
	deadline := time.Now()

	// Act.
	serving.AwaitStreamsEnded(deadline)

	// Assert.
	got := dlog.FilterRecords(log.Records(), dlog.RecordQuery{Operation: "daemon.server.streams_unwritten"})
	if len(got) != 1 || got[0].Level != dlog.LevelError || got[0].Context["open"] != 1 ||
		got[0].Context["deadline"] != deadline.Format(time.RFC3339Nano) {
		t.Fatalf("records = %+v, want one ERROR naming the one unwritten stream and the deadline", got)
	}
}

// TestAnH2cUpgradeIsRefused pins the one way an HTTP/1.1 connection could turn
// into uncounted h2 streams.
func TestAnH2cUpgradeIsRefused(t *testing.T) {
	// Arrange.
	reached := false
	log := dlog.NewTestLogger()
	serving := H2C(http.HandlerFunc(func(http.ResponseWriter, *http.Request) { reached = true }), log)
	req := httptest.NewRequest(http.MethodGet, "/agentrepl.v1.AgentRepl/SelectWorkspace", nil)
	req.Header.Set("Connection", "Upgrade, HTTP2-Settings")
	req.Header.Set("Upgrade", "h2c")
	req.Header.Set("HTTP2-Settings", "")
	rec := httptest.NewRecorder()

	// Act.
	serving.ServeHTTP(rec, req)

	// Assert.
	if rec.Code != http.StatusBadRequest || reached {
		t.Fatalf("upgrade answered %d (handler reached %t), want 400 and no handler", rec.Code, reached)
	}
}

// TestAnH2cUpgradeIsRecordedAtError pins the refusal's record.
func TestAnH2cUpgradeIsRecordedAtError(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	serving := H2C(http.NotFoundHandler(), log)
	req := httptest.NewRequest(http.MethodGet, "/agentrepl.v1.AgentRepl/SelectWorkspace", nil)
	req.Header.Set("Upgrade", "h2c")

	// Act.
	serving.ServeHTTP(httptest.NewRecorder(), req)

	// Assert.
	got := dlog.FilterRecords(log.Records(), dlog.RecordQuery{Operation: "daemon.server.h2c_upgrade_refused"})
	if len(got) != 1 || got[0].Level != dlog.LevelError || got[0].Context["path"] != "/agentrepl.v1.AgentRepl/SelectWorkspace" {
		t.Fatalf("records = %+v, want one ERROR naming the path", got)
	}
}

// TestAwaitPushesSentWaitsForAPushNotYetSent pins the exit's wait on the
// daemon's last words: a push counted on its way to a stream holds it.
func TestAwaitPushesSentWaitsForAPushNotYetSent(t *testing.T) {
	// Arrange.
	serving := H2C(http.NotFoundHandler(), dlog.NewTestLogger())
	serving.pushes = &inflight.Tracker{}
	sent := serving.pushes.Begin()

	// Act.
	pending := serving.AwaitPushesSent(time.Now())
	sent()
	settled := serving.AwaitPushesSent(time.Now().Add(time.Minute))

	// Assert.
	if pending || !settled {
		t.Fatalf("AwaitPushesSent = (%t pending, %t sent), want (false, true)", pending, settled)
	}
}

// TestAPushNotSentByTheExitsDeadlineIsRecordedAtWarn pins the record of a
// stream ended over a push.
func TestAPushNotSentByTheExitsDeadlineIsRecordedAtWarn(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	serving := H2C(http.NotFoundHandler(), log)
	serving.pushes = &inflight.Tracker{}
	defer serving.pushes.Begin()()

	// Act.
	serving.AwaitPushesSent(time.Now())

	// Assert.
	got := dlog.FilterRecords(log.Records(), dlog.RecordQuery{Operation: "daemon.server.await_pushes_sent"})
	if len(got) != 1 || got[0].Level != dlog.LevelWarn || got[0].Context["pending"] != 1 {
		t.Fatalf("records = %+v, want one WARN naming the one pending push", got)
	}
}

// writeHold is the switch that parks a connection's writes. Taking it waits
// out every write already passing through, so once hold returns no write that
// began before it can still complete: every write the socket takes from then
// on waits for the test's release.
type writeHold struct {
	mu   sync.RWMutex
	held bool
}

// hold parks every write from now on, once the writes in flight have ended.
func (h *writeHold) hold() {
	h.mu.Lock()
	h.held = true
	h.mu.Unlock()
}

// heldWriteListener hands out connections whose writes park until the test
// releases them, standing in for a socket that has not taken the answer yet.
type heldWriteListener struct {
	net.Listener
	held    *writeHold
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
	held    *writeHold
	release chan struct{}
}

func (c *heldWriteConn) Write(p []byte) (int, error) {
	c.held.mu.RLock()
	if c.held.held {
		c.held.mu.RUnlock()
		<-c.release
		return c.Conn.Write(p)
	}
	defer c.held.mu.RUnlock()
	return c.Conn.Write(p)
}

// serveHeld serves SERVING the way run.go serves the real one -- through the
// gate's own listener -- over a socket whose writes park on held, and answers
// its base URL.
func serveHeld(t *testing.T, serving *Serving, held *writeHold, release chan struct{}) string {
	t.Helper()
	tcp, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatalf("listen: %v", err)
	}
	httpServer := &http.Server{Handler: serving}
	go func() {
		_ = httpServer.Serve(serving.Listener(&heldWriteListener{Listener: tcp, held: held, release: release}))
	}()
	t.Cleanup(func() { _ = httpServer.Close() })
	return "http://" + tcp.Addr().String()
}

// servedThroughTheGate starts a test server the way run.go serves the real one:
// through the gate's OWN listener. A test that served the bare listener would
// be testing a gate the daemon does not run.
func servedThroughTheGate(serving *Serving) *httptest.Server {
	httpServer := httptest.NewUnstartedServer(serving)
	httpServer.Listener = serving.Listener(httpServer.Listener)
	httpServer.Start()
	return httpServer
}

// standingStreamServing serves one standing stream whose handler flushes its
// headers and then holds until ended is closed, through a listener whose
// writes park once the handler has returned, until release is closed.
func standingStreamServing(t *testing.T, ended <-chan struct{}, held *writeHold, release chan struct{}) (*Serving, string) {
	t.Helper()
	return standingStreamServingLogged(t, ended, held, release, nil)
}

// standingStreamServingLogged is standingStreamServing recording into log.
func standingStreamServingLogged(t *testing.T, ended <-chan struct{}, held *writeHold, release chan struct{}, log dlog.Logger) (*Serving, string) {
	t.Helper()
	serving := H2C(http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
		w.WriteHeader(http.StatusOK)
		w.(http.Flusher).Flush()
		<-ended
		held.hold()
	}), log)
	return serving, serveHeld(t, serving, held, release) + "/agentrepl.v1.AgentRepl/WatchDaemon"
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
	// Arrange.
	var held writeHold
	ended, release := make(chan struct{}), make(chan struct{})
	serving, url := standingStreamServing(t, ended, &held, release)
	openStream(t, url)
	serving.EndStreams(time.Now().Add(time.Minute), func() { close(ended) })
	if left := serving.awaitStreamHandlers(time.Now().Add(time.Minute)); left != 0 {
		t.Fatalf("the ended stream's handler never returned: %d left", left)
	}

	// Act: the wait with the end frame parked, then once the socket took it.
	parked := serving.AwaitStreamsEnded(time.Now())
	close(release)
	written := serving.AwaitStreamsEnded(time.Now().Add(time.Minute))

	// Assert.
	if parked != 1 || written != 0 {
		t.Fatalf("AwaitStreamsEnded = (%d parked, %d written), want (1, 0)", parked, written)
	}
}

// TestAwaitStreamsEndedReportsTheStreamsStillOpen pins the loud half: a stream
// that did not end by the deadline is answered, because its client will read
// a cut connection rather than an end frame.
func TestAwaitStreamsEndedReportsTheStreamsStillOpen(t *testing.T) {
	// Arrange.
	var held writeHold
	ended, release := make(chan struct{}), make(chan struct{})
	t.Cleanup(func() { close(ended); close(release) })
	serving, url := standingStreamServing(t, ended, &held, release)
	openStream(t, url)

	// Act.
	deadline := time.Now()
	serving.EndStreams(deadline, func() {})
	left := serving.AwaitStreamsEnded(deadline)

	// Assert.
	if left != 1 {
		t.Fatalf("AwaitStreamsEnded = %d, want the one stream that never ended", left)
	}
}

// TestAStreamStillOpenAtTheExitsDeadlineIsRecordedAtError pins the loud half
// of the exit's one deadline: the record names how many streams were left
// open and the deadline that passed.
func TestAStreamStillOpenAtTheExitsDeadlineIsRecordedAtError(t *testing.T) {
	// Arrange.
	var held writeHold
	ended, release := make(chan struct{}), make(chan struct{})
	t.Cleanup(func() { close(ended); close(release) })
	log := dlog.NewTestLogger()
	serving, url := standingStreamServingLogged(t, ended, &held, release, log)
	openStream(t, url)
	deadline := time.Now()

	// Act.
	serving.EndStreams(deadline, func() {})
	serving.AwaitStreamsEnded(deadline)

	// Assert.
	got := dlog.FilterRecords(log.Records(), dlog.RecordQuery{Operation: "daemon.server.await_streams_ended"})
	if len(got) != 1 || got[0].Level != dlog.LevelError || got[0].Context["open"] != 1 ||
		got[0].Context["deadline"] != deadline.Format(time.RFC3339Nano) {
		t.Fatalf("records = %+v, want one ERROR naming the one open stream and the deadline", got)
	}
}

// TestTheExitsStreamRecordsNameEachOpenStreamByItsPath pins what makes an
// overrun diagnosable: both the exit's start and its overrun name the
// standing streams still open, by procedure path.
func TestTheExitsStreamRecordsNameEachOpenStreamByItsPath(t *testing.T) {
	tests := []struct {
		name      string
		operation string
	}{
		{name: "the exit's start", operation: "daemon.server.end_streams"},
		{name: "the exit's overrun", operation: "daemon.server.await_streams_ended"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			var held writeHold
			ended, release := make(chan struct{}), make(chan struct{})
			t.Cleanup(func() { close(ended); close(release) })
			log := dlog.NewTestLogger()
			serving, streamURL := standingStreamServingLogged(t, ended, &held, release, log)
			openStream(t, streamURL)
			parsed, err := neturl.Parse(streamURL)
			if err != nil {
				t.Fatal(err)
			}
			deadline := time.Now()

			// Act.
			serving.EndStreams(deadline, func() {})
			serving.AwaitStreamsEnded(deadline)

			// Assert.
			want := []string{parsed.Path + "=1"}
			var got []any
			for _, r := range log.Records() {
				if r.Operation == tt.operation {
					got = append(got, r.Context["paths"])
				}
			}
			if len(got) != 1 || !reflect.DeepEqual(got[0], want) {
				t.Fatalf("%s paths = %v, want one record naming %v", tt.operation, got, want)
			}
		})
	}
}

// TestAnEndFrameWrittenBeforeTheDeadlineRecordsNothing pins the quiet half:
// an exit whose streams end within the deadline records no error.
func TestAnEndFrameWrittenBeforeTheDeadlineRecordsNothing(t *testing.T) {
	// Arrange.
	var held writeHold
	ended, release := make(chan struct{}), make(chan struct{})
	log := dlog.NewTestLogger()
	serving, url := standingStreamServingLogged(t, ended, &held, release, log)
	openStream(t, url)
	deadline := time.Now().Add(time.Minute)

	// Act.
	serving.EndStreams(deadline, func() { close(ended) })
	close(release)
	left := serving.AwaitStreamsEnded(deadline)

	// Assert.
	if left != 0 {
		t.Fatalf("AwaitStreamsEnded = %d, want 0", left)
	}
	for _, r := range log.Records() {
		if r.Level == dlog.LevelError {
			t.Fatalf("records = %+v, want no error", log.Records())
		}
	}
}

// TestAwaitStreamsEndedWithNoStreamSettlesAtOnce pins that a daemon nothing
// watches has no end frame to wait for.
func TestAwaitStreamsEndedWithNoStreamSettlesAtOnce(t *testing.T) {
	// Arrange.
	serving := H2C(http.NotFoundHandler(), nil)

	// Act.
	serving.EndStreams(time.Now(), func() {})
	left := serving.AwaitStreamsEnded(time.Now())

	// Assert.
	if left != 0 {
		t.Fatalf("AwaitStreamsEnded = %d with no stream open, want 0", left)
	}
}
