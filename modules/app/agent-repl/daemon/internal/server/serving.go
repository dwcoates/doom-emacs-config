package server

import (
	"context"
	"fmt"
	"net"
	"net/http"
	"sort"
	"strings"
	"sync"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"golang.org/x/net/http2"
	"golang.org/x/net/http2/h2c"

	"agent-repld/internal/dlog"
	"agent-repld/internal/inflight"
	"agent-repld/internal/publish"
)

// RequestGate is what an orderly exit waits on before it stops serving: the
// calls that are still being answered, and every stream's end on the socket.
type RequestGate interface {
	// AwaitQuiet waits, bounded, for every in-flight call's handler to return
	// and reports how many were still running when the bound expired.
	AwaitQuiet(bound time.Duration) int
	// Listener wraps the listener this handler serves, so every h2 stream
	// counts until its end is on the socket (WriteBarrier).
	Listener(inner net.Listener) net.Listener
	// AwaitPushesSent waits, up to deadline, until every push enqueued for a
	// standing stream has been sent on it (publish.StreamPushes), and reports
	// whether it was. It covers the daemon's last words: a push sent before
	// its stream ends is written before the stream's end frame.
	AwaitPushesSent(deadline time.Time) bool
	// EndStreams runs end, which ends every standing stream the daemon
	// serves.
	EndStreams(deadline time.Time, end func())
	// AwaitStreamsEnded waits, up to deadline, for every standing stream's
	// handler to return AND every h2 stream's end -- each answer's and each
	// end frame -- to be on the socket, and reports how many were not.
	AwaitStreamsEnded(deadline time.Time) int
}

// Serving is the daemon's one serving handler: h2c over the loopback listener,
// with the in-flight calls counted so an orderly exit can wait for them.
//
// THE COUNT EXISTS BECAUSE net/http's OWN GRACE CANNOT SEE THESE CALLS, and
// that is not a subtlety — it is the whole of the daemon's graceful shutdown
// being inoperative for the only transport its clients use. `h2c.NewHandler`
// serves a prior-knowledge h2c connection by HIJACKING it, and net/http's
// `Server.Shutdown` "does not attempt to close nor wait for hijacked
// connections": every h2 stream on that connection is invisible to it, so
// `Shutdown` returns at once and the process tears down over calls it is still
// answering.
//
// Measured, in the e2e sandbox: an `UpdateShutdownSchedule{now}` handler
// recorded "applied the shutdown schedule" and the serving lifetime had ended
// every open stream 211 microseconds later, so Emacs read `unexpected EOF`
// from a stop the daemon had in fact performed.
//
// THE STANDING STREAMS ARE NOT COUNTED AS CALLS. A `Watch*` handler returns
// only when its client goes away, so counting one in AwaitQuiet would make the
// exit spend its whole grace on every stop and would bound nothing. They are
// counted APART (streams), and the exit ENDS them itself -- EndStreams, then
// AwaitStreamsEnded -- so every client reads its stream's end frame rather
// than a connection cut under it.
//
// A HANDLER RETURNING IS NOT ITS ANSWER LEAVING. The counts here come down
// when handlers return; what the sockets have written is the barrier's
// (WriteBarrier), which the exit's last wait reads.
//
// Regression, 2026-09-27: the exit let `http.Server.Shutdown` "close" the
// standing streams, which on a hijacked h2c connection closes nothing, and the
// server's own close ran as a deferred call the process exit raced. Emacs read
// `WatchHostWorkspace: producer closed without an end frame` for every
// workspace still watched, and the same on its roster stream.
type Serving struct {
	handler http.Handler
	log     dlog.Logger
	// barrier knows which h2 streams' ends are on the socket, because a
	// handler returning is not its answer leaving. See WriteBarrier.
	barrier WriteBarrier
	// pushes counts the values on their way to a standing stream
	// (publish.StreamPushes in the daemon).
	pushes *inflight.Tracker

	mu       sync.Mutex
	inFlight int
	// quiet is closed when the count reaches zero. It exists only while
	// somebody is waiting, so an ordinary call pays one comparison.
	quiet chan struct{}
	// streams counts the standing streams whose handler has not returned.
	streams int
	// streamPaths counts the same streams by procedure path, so an exit that
	// leaves some open names which (`daemon.server.await_streams_ended`).
	streamPaths map[string]int
	// streamsGone is closed when streams reaches zero, only while somebody is
	// waiting.
	streamsGone chan struct{}
}

// Listener wraps the listener this handler serves, so every h2 stream counts
// until its end is on the socket and not merely produced.
func (s *Serving) Listener(inner net.Listener) net.Listener {
	return s.barrier.Listener(inner)
}

// H2C wraps the Connect handler so ONE loopback listener serves both HTTP/1.1
// and cleartext HTTP/2. The daemon binds one listener and serves the rpcs and
// the webapp assets on one origin, which is what makes the webview URL and the
// Connect endpoint the same host.
//
// The counting sits INSIDE the h2c handler, because outside it there is only
// the single hijacked connection to see and never the streams carried on it.
func H2C(h http.Handler, log dlog.Logger) *Serving {
	s := &Serving{log: log, pushes: publish.StreamPushes}
	s.handler = h2c.NewHandler(s.counted(h), &http2.Server{})
	return s
}

// ServeHTTP serves one request through the h2c handler.
//
// THE h2c UPGRADE IS REFUSED: it is the one way an HTTP/1.1 connection turns
// into h2 after its first bytes, which WriteBarrier reads to know a
// connection's protocol, so an upgraded connection's streams would be
// uncounted and an exit would cut them. Every client of this daemon dials h2
// with prior knowledge; one that asks to upgrade is refused loudly.
func (s *Serving) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	if strings.EqualFold(r.Header.Get("Upgrade"), "h2c") {
		if s.log != nil {
			s.log.Error("daemon.server.h2c_upgrade_refused",
				"a client asked to upgrade HTTP/1.1 to h2c, which this daemon refuses: dial h2 with prior knowledge",
				dlog.Context{"path": r.URL.Path, "remote": r.RemoteAddr, "user_agent": r.UserAgent()})
		}
		http.Error(w, "h2c upgrade is not served; dial h2 with prior knowledge", http.StatusBadRequest)
		return
	}
	s.handler.ServeHTTP(w, r)
}

// counted counts one call for as long as its handler runs: a standing stream
// apart from the other calls (streams), so the exit can end the one and wait
// for the other. What leaves the socket after a handler returns is the
// barrier's (WriteBarrier).
func (s *Serving) counted(inner http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if standingStreamPaths[r.URL.Path] {
			s.enterStream(r.URL.Path)
			defer s.leaveStream(r.URL.Path)
			inner.ServeHTTP(w, r)
			return
		}
		s.enter()
		defer s.leave()
		inner.ServeHTTP(w, r)
	})
}

func (s *Serving) enter() {
	s.mu.Lock()
	s.inFlight++
	s.mu.Unlock()
}

func (s *Serving) leave() {
	s.mu.Lock()
	s.inFlight--
	if s.inFlight == 0 && s.quiet != nil {
		close(s.quiet)
		s.quiet = nil
	}
	s.mu.Unlock()
}

// AwaitQuiet waits for the calls in flight to be answered, and says so loudly
// when the bound expires with some still running: those callers will read a
// cut connection rather than the answer this daemon produced.
func (s *Serving) AwaitQuiet(bound time.Duration) int {
	s.mu.Lock()
	if s.inFlight == 0 {
		s.mu.Unlock()
		return 0
	}
	if s.quiet == nil {
		s.quiet = make(chan struct{})
	}
	quiet := s.quiet
	s.mu.Unlock()

	timer := time.NewTimer(bound)
	defer timer.Stop()
	select {
	case <-quiet:
		return 0
	case <-timer.C:
		s.mu.Lock()
		left := s.inFlight
		s.mu.Unlock()
		if s.log != nil {
			s.log.Warn("daemon.server.await_quiet", "the exit's grace expired with calls still being answered; their callers will read a cut connection",
				dlog.Context{"in_flight": left, "bound_ms": bound.Milliseconds()})
		}
		return left
	}
}

func (s *Serving) enterStream(path string) {
	s.mu.Lock()
	s.streams++
	if s.streamPaths == nil {
		s.streamPaths = map[string]int{}
	}
	s.streamPaths[path]++
	s.mu.Unlock()
}

func (s *Serving) leaveStream(path string) {
	s.mu.Lock()
	s.streams--
	if s.streamPaths[path]--; s.streamPaths[path] == 0 {
		delete(s.streamPaths, path)
	}
	if s.streams == 0 && s.streamsGone != nil {
		close(s.streamsGone)
		s.streamsGone = nil
	}
	s.mu.Unlock()
}

// EndStreams runs end, which ends every standing stream, recording what was
// open.
func (s *Serving) EndStreams(deadline time.Time, end func()) {
	s.mu.Lock()
	open, paths := s.streams, s.openStreamPathsLocked()
	s.mu.Unlock()
	if s.log != nil {
		s.log.Info("daemon.server.end_streams", "ending every standing stream with its end frame", dlog.Context{"open": open, "paths": paths})
	}
	end()
}

// AwaitStreamsEnded waits for every standing stream's handler to return and
// then for every h2 stream's end to be on the socket, and says so loudly when
// the exit's deadline passes first: those clients will read a cut connection
// instead of their answer or end frame.
func (s *Serving) AwaitStreamsEnded(deadline time.Time) int {
	if left := s.awaitStreamHandlers(deadline); left > 0 {
		return left
	}
	left := s.barrier.AwaitStreamsWritten(deadline)
	if left > 0 && s.log != nil {
		_, byConn := s.barrier.Open()
		s.log.Error("daemon.server.streams_unwritten",
			"the exit's deadline passed with streams whose end never reached the socket; their clients will read a cut connection rather than an answer or an end frame",
			dlog.Context{"open": left, "by_connection": byConn, "deadline": deadline.Format(time.RFC3339Nano)})
	}
	return left
}

// awaitStreamHandlers waits for every standing stream's handler to return,
// and says so loudly when deadline passes first.
func (s *Serving) awaitStreamHandlers(deadline time.Time) int {
	s.mu.Lock()
	if s.streams == 0 {
		s.mu.Unlock()
		return 0
	}
	if s.streamsGone == nil {
		s.streamsGone = make(chan struct{})
	}
	gone := s.streamsGone
	s.mu.Unlock()

	timer := time.NewTimer(time.Until(deadline))
	defer timer.Stop()
	select {
	case <-gone:
		return 0
	case <-timer.C:
		s.mu.Lock()
		left, paths := s.streams, s.openStreamPathsLocked()
		s.mu.Unlock()
		if s.log != nil {
			s.log.Error("daemon.server.await_streams_ended", "the exit's deadline passed with standing streams still open; their clients will read a cut connection rather than an end frame",
				dlog.Context{"open": left, "paths": paths, "deadline": deadline.Format(time.RFC3339Nano)})
		}
		return left
	}
}

// openStreamPathsLocked answers the open standing streams as "path=count"
// entries, sorted, for a record. s.mu is held.
func (s *Serving) openStreamPathsLocked() []string {
	paths := make([]string, 0, len(s.streamPaths))
	for path, n := range s.streamPaths {
		paths = append(paths, fmt.Sprintf("%s=%d", path, n))
	}
	sort.Strings(paths)
	return paths
}

// AwaitPushesSent waits for every push enqueued for a standing stream to
// have been sent on it, so the exit does not end the streams over the
// daemon's own last push.
//
// THE COUNTED CALLS ARE NOT THE WHOLE OF WHAT IS OWED. `counted` skips
// standingStreamPaths deliberately, so `AwaitQuiet` reports zero in flight
// while a `DaemonShutdownAnnounced` the drain pushed a moment earlier is still
// on its way onto every WatchDaemon stream. Each push is counted from its
// enqueue until its stream sent it (publish.StreamPushes); a sent push is
// ahead of its stream's end frame, which AwaitStreamsEnded waits onto the
// socket.
func (s *Serving) AwaitPushesSent(deadline time.Time) bool {
	ctx, cancel := context.WithDeadline(context.Background(), deadline)
	defer cancel()
	err := s.pushes.AwaitIdle(ctx)
	if err == nil {
		return true
	}
	if s.log != nil {
		s.log.Warn("daemon.server.await_pushes_sent",
			"the exit's deadline passed with pushes not yet sent on their streams; the streams are ended over them",
			dlog.Context{"pending": s.pushes.InFlight(), "sites": s.pushes.InFlightSites(), "cause": err.Error()})
	}
	return false
}

// standingStreamPaths are the procedure paths whose handler does not return
// until its client goes away.
//
// DERIVED FROM THE SERVICE DESCRIPTOR, never listed by hand: a streaming verb
// added to the proto is excluded here on the day it lands, and a list that had
// drifted would make every orderly exit wait out its whole grace on a stream
// that was never going to end.
var standingStreamPaths = serverStreamPaths()

func serverStreamPaths() map[string]bool {
	out := map[string]bool{}
	services := agentreplv1.File_agentrepl_v1_service_proto.Services()
	for i := 0; i < services.Len(); i++ {
		service := services.Get(i)
		methods := service.Methods()
		for j := 0; j < methods.Len(); j++ {
			method := methods.Get(j)
			if !method.IsStreamingServer() && !method.IsStreamingClient() {
				continue
			}
			out["/"+string(service.FullName())+"/"+string(method.Name())] = true
		}
	}
	return out
}
