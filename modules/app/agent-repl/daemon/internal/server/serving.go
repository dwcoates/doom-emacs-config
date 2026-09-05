package server

import (
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
// THE STANDING STREAMS ARE NOT COUNTED. A `Watch*` handler returns only when
// its client goes away, so counting one would make the exit spend its whole
// grace on every stop and would bound nothing. They are ended by the shutdown
// that follows, which is what they are for.
type Serving struct {
	handler http.Handler
	log     dlog.Logger

	mu       sync.Mutex
	inFlight int
	// quiet is closed when the count reaches zero. It exists only while
	// somebody is waiting, so an ordinary call pays one comparison.
	quiet chan struct{}
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

func (s *Serving) counted(inner http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if standingStreamPaths[r.URL.Path] {
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
