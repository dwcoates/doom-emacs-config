package server

import (
	"context"
	"time"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/inflight"
)

// opAwaitIdle is the test barrier's record.
const opAwaitIdle = "daemon.server.await_idle"

// AwaitIdle is the test barrier: it answers once nothing the daemon counts
// (internal/inflight) is in flight, with every standing stream's frames
// written, so a test can assert at once that nothing followed what it did.
// A daemon no test harness started refuses it (Deps.TestBarrier).
func (s *server) AwaitIdle(
	ctx context.Context,
	req *connect.Request[agentreplv1.AwaitIdleRequest],
) (*connect.Response[agentreplv1.AwaitIdleResponse], error) {
	if !s.deps.TestBarrier {
		s.log.Info(opAwaitIdle, "refused the test barrier: no test harness started this daemon", nil)
		return connect.NewResponse(&agentreplv1.AwaitIdleResponse{Result: &agentreplv1.AwaitIdleResponse_Error{
			Error: &agentreplv1.AwaitIdleError{Cause: &agentreplv1.AwaitIdleError_NotTesting{NotTesting: &agentreplv1.AwaitIdleNotTesting{}}},
		}}), nil
	}
	tracker := s.deps.InFlight
	if tracker == nil {
		tracker = inflight.Default
	}
	if bound := req.Msg.GetBoundMs(); bound > 0 {
		var cancel context.CancelFunc
		ctx, cancel = context.WithTimeout(ctx, time.Duration(bound)*time.Millisecond)
		defer cancel()
	}
	if err := tracker.AwaitIdle(ctx); err != nil {
		inFlight, sites := tracker.InFlight(), tracker.InFlightSites()
		s.log.Info(opAwaitIdle, "the test barrier's caller gave up while work was still in flight", dlog.Context{
			"in_flight": inFlight, "sites": sites, "cause": err.Error(),
		})
		return connect.NewResponse(&agentreplv1.AwaitIdleResponse{Result: &agentreplv1.AwaitIdleResponse_Error{
			Error: &agentreplv1.AwaitIdleError{Cause: &agentreplv1.AwaitIdleError_StillBusy{
				StillBusy: &agentreplv1.AwaitIdleStillBusy{InFlight: uint64(max(inFlight, 0)), Sites: sites},
			}},
		}}), nil
	}
	streams := s.frames.written()
	s.log.Debug(opAwaitIdle, "nothing is in flight; answered the test barrier", dlog.Context{"streams": len(streams)})
	return connect.NewResponse(&agentreplv1.AwaitIdleResponse{Result: &agentreplv1.AwaitIdleResponse_Success{
		Success: &agentreplv1.AwaitIdleSuccess{Streams: streams},
	}}), nil
}
