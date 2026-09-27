package server

import (
	"net"
	"net/http"
	"sync"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"golang.org/x/net/http2"
	"golang.org/x/net/http2/h2c"

	"claude-repld/internal/dlog"
)

// RequestGate is what an orderly exit waits on before it stops serving: the
// calls that are still being answered.
type RequestGate interface {
	// AwaitQuiet waits, bounded, for every in-flight call to be answered and
	// reports how many were still running when the bound expired.
	AwaitQuiet(bound time.Duration) int
	// Listener wraps the listener this handler serves, so a call counts as
	// answered only once its bytes have left the socket.
	Listener(inner net.Listener) net.Listener
	// AwaitWritesQuiet waits, bounded, for the connections to stop writing,
	// and reports whether they did. It covers what AwaitQuiet cannot: the
	// pushes on the STANDING streams, which are not counted calls.
	AwaitWritesQuiet(bound time.Duration) bool
	// EndStreams runs end, which ends every standing stream the daemon
	// serves, and from then on holds each one that ends until its END FRAME
	// is on the socket.
	EndStreams(end func())
	// AwaitStreamsEnded waits, bounded, for every standing stream to have
	// ended and written its end frame, and reports how many had not when the
	// bound expired.
	AwaitStreamsEnded(bound time.Duration) int
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
// Regression, 2026-09-27: the exit let `http.Server.Shutdown` "close" the
// standing streams, which on a hijacked h2c connection closes nothing, and the
// server's own close ran as a deferred call the process exit raced. Emacs read
// `WatchHostWorkspace: producer closed without an end frame` for every
// workspace still watched, and the same on its roster stream.
type Serving struct {
	handler http.Handler
	log     dlog.Logger
	// barrier counts what the sockets have actually written, because a
	// handler returning is not its answer leaving. See WriteBarrier.
	barrier WriteBarrier

	mu       sync.Mutex
	inFlight int
	// quiet is closed when the count reaches zero. It exists only while
	// somebody is waiting, so an ordinary call pays one comparison.
	quiet chan struct{}
	// streams counts the standing streams still open, or still writing the
	// end frame the exit ended them with.
	streams int
	// streamsGone is closed when streams reaches zero, only while somebody is
	// waiting.
	streamsGone chan struct{}
	// ending is set by EndStreams: from then on a standing stream that ends
	// was ended by the exit, and is counted until its end frame has left.
	ending bool
}

// Listener wraps the listener this handler serves, so the calls it counts are
// held open until their bytes are on the wire and not merely produced.
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
	s := &Serving{log: log}
	s.handler = h2c.NewHandler(s.counted(h), &http2.Server{})
	return s
}

// ServeHTTP serves one request through the h2c handler.
func (s *Serving) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	s.handler.ServeHTTP(w, r)
}

// counted holds one call open for as long as its answer is still owed.
//
// THE COUNT COMES DOWN WHEN THE ANSWER IS ON THE SOCKET, NOT WHEN THE HANDLER
// RETURNS, and the difference is the whole defect: a returning handler has
// produced its answer, and http2 writes it from the CONNECTION's own goroutine
// afterwards, so an exit that waited only for the return cut the answer off the
// wire — "applied the shutdown schedule" and "every open stream was ended" 369
// microseconds apart, with the caller reading `unexpected EOF`.
//
// THE REQUEST CONTEXT IS NOT THAT SIGNAL EITHER, though this gate used to say
// it was. `serverConn.runHandler` cancels it in a defer that runs BEFORE the
// same defer's `rw.handlerDone()` produces the answer's final frames, so a gate
// on `r.Context().Done()` is a gate on a moment at which the answer does not
// yet exist. It measured as `TestHostRequestedStopLeavesNoProcessBehind`
// failing every run in the e2e sandbox on a stop the daemon had in fact
// performed.
//
// So the call is held until the socket has written past the mark taken on the
// way in and then gone quiet, which is what WriteBarrier is for. The context's
// cancellation is still waited on first: it is an exact statement that the
// handler's own goroutine has reached its teardown, and that is what makes the
// barrier's "written since" question a question about THIS answer.
func (s *Serving) counted(inner http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if standingStreamPaths[r.URL.Path] {
			s.enterStream()
			defer s.leaveStreamOnceEnded(r)
			inner.ServeHTTP(w, r)
			return
		}
		s.enter()
		answered := r.Context()
		mark := s.barrier.Mark()
		path := r.URL.Path
		defer func() {
			if answered.Done() == nil {
				s.leave()
				return
			}
			go func() {
				<-answered.Done()
				if !s.barrier.AwaitWrittenSince(mark, answerWriteBound) && s.log != nil {
					s.log.Error("daemon.server.answer_unwritten",
						"an answer's own frames never reached the socket within the bound; its caller will read a cut connection rather than this answer",
						dlog.Context{"path": path, "bound_ms": answerWriteBound.Milliseconds()})
				}
				s.leave()
			}()
		}()
		inner.ServeHTTP(w, r)
	})
}

// answerWriteBound is how long ONE answer has to reach the socket after its
// handler's goroutine has finished with it.
//
// A LAST RESORT, not the mechanism: the work it covers is the http2 serve
// goroutine flushing a few dozen buffered bytes onto a loopback socket, and the
// barrier ends on quiescence rather than on this clock. It is deliberately well
// under the exit's own grace, so one stuck answer cannot spend the whole of it.
const answerWriteBound = 250 * time.Millisecond

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

func (s *Serving) enterStream() {
	s.mu.Lock()
	s.streams++
	s.mu.Unlock()
}

func (s *Serving) leaveStream() {
	s.mu.Lock()
	s.streams--
	if s.streams == 0 && s.streamsGone != nil {
		close(s.streamsGone)
		s.streamsGone = nil
	}
	s.mu.Unlock()
}

// leaveStreamOnceEnded uncounts a standing stream whose handler has returned.
//
// A STREAM ITS CLIENT ENDED LEAVES AT ONCE: nothing is owed to a client that
// went away, and its connection may write nothing more at all. A stream the
// EXIT ended is held until the socket has written past the moment its handler
// returned -- its end frame, which http2 produces after the handler returns
// (see WriteBarrier) -- so AwaitStreamsEnded means "every client has its end
// frame", not "every handler has returned".
func (s *Serving) leaveStreamOnceEnded(r *http.Request) {
	s.mu.Lock()
	ending := s.ending
	s.mu.Unlock()
	answered := r.Context()
	if !ending || answered.Done() == nil {
		s.leaveStream()
		return
	}
	mark := s.barrier.Mark()
	path := r.URL.Path
	go func() {
		<-answered.Done()
		if !s.barrier.AwaitWrittenSince(mark, answerWriteBound) && s.log != nil {
			s.log.Error("daemon.server.stream_end_unwritten",
				"a standing stream the exit ended never had its end frame reach the socket within the bound; its client will read a cut connection",
				dlog.Context{"path": path, "bound_ms": answerWriteBound.Milliseconds()})
		}
		s.leaveStream()
	}()
}

// EndStreams marks the exit as ending the standing streams, then runs end,
// which does it. The mark comes FIRST, so no stream end could be read as its
// client's own.
func (s *Serving) EndStreams(end func()) {
	s.mu.Lock()
	s.ending = true
	open := s.streams
	s.mu.Unlock()
	if s.log != nil {
		s.log.Info("daemon.server.end_streams", "ending every standing stream with its end frame", dlog.Context{"open": open})
	}
	end()
}

// AwaitStreamsEnded waits for every standing stream to have ended and written
// its end frame, and says so loudly when the bound expires with some still
// open: those clients will read a cut connection instead of an end frame.
func (s *Serving) AwaitStreamsEnded(bound time.Duration) int {
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

	timer := time.NewTimer(bound)
	defer timer.Stop()
	select {
	case <-gone:
		return 0
	case <-timer.C:
		s.mu.Lock()
		left := s.streams
		s.mu.Unlock()
		if s.log != nil {
			s.log.Error("daemon.server.await_streams_ended", "the exit's stream-end bound expired with standing streams still open; their clients will read a cut connection rather than an end frame",
				dlog.Context{"open": left, "bound_ms": bound.Milliseconds()})
		}
		return left
	}
}

// AwaitWritesQuiet waits for the connections to fall silent, so the exit does
// not close the standing streams over the daemon's own last push.
//
// THE COUNTED CALLS ARE NOT THE WHOLE OF WHAT IS OWED. `counted` skips
// standingStreamPaths deliberately, so `AwaitQuiet` reports zero in flight
// while a `DaemonShutdownAnnounced` the drain pushed a moment earlier is still
// on its way onto every WatchDaemon stream. See WriteBarrier.AwaitQuiescent.
func (s *Serving) AwaitWritesQuiet(bound time.Duration) bool {
	if s.barrier.AwaitQuiescent(bound) {
		return true
	}
	if s.log != nil {
		s.log.Warn("daemon.server.await_writes_quiet",
			"the connections were still writing when the exit's write-quiet bound expired; the standing streams are closed over whatever was still going out",
			dlog.Context{"bound_ms": bound.Milliseconds()})
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
