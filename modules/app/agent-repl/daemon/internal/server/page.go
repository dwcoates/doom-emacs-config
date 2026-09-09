package server

import (
	"context"
	"fmt"
	"sync"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/internal/dlog"
)

// THE PAGE MUX: one stream per browser page, however many views it watches.
//
// WHY IT EXISTS IS A PROPERTY OF THE BROWSER, NOT OF THIS DAEMON. The daemon
// serves h2c, but no browser negotiates cleartext HTTP/2, so a webview talks
// HTTP/1.1 — which caps a page at about SIX connections per host. A
// server-streaming Connect call pins one for its whole life, and the webapp
// held six before its feed tail was even opened. The seventh did not fail: it
// QUEUED, forever, with no request on the wire, no error and no frame, so no
// row a workspace produced after the page loaded was ever drawn. Every expanded
// subagent bubble opens another feed tail, so the count grew with the
// conversation and no fixed budget could have contained it.
//
// SO THE COUNT IS THE GUARANTEE. A page opens `WatchPage` and nothing else;
// `SubscribePage` and `UnsubscribePage` are unary and hold no connection
// between calls. There is no second stream for a page to open, whatever it
// decides to watch, so exceeding the cap is not a thing the webapp can do
// wrong — it is a thing the protocol cannot express.
//
// A SUBSCRIPTION IS EXACTLY THE STREAM IT REPLACES. It runs the SAME handler
// body the dedicated rpc runs (`watchFooter`, `watchFeed`, …) against a sink
// that writes into this mux instead of into a `connect.ServerStream`. The
// subscription invariant, `WatchFeed`'s token pin, the refusals, the
// acceptance — all of it is the one implementation, not a second one kept in
// step by hand.
//
// THE WRITER IS SINGLE AND THE BACKPRESSURE IS THE SAME. `connect.ServerStream`
// is not safe for concurrent use, so every subscription's pump hands its push
// to the page's own writer goroutine over an UNBUFFERED channel: a pump blocks
// until its push has been taken, exactly as it blocked on `Send` before, so
// nothing is dropped and nothing is queued without bound. The cost is that one
// slow reader holds up the page's other subscriptions — which is what a shared
// HTTP/1.1 connection does anyway, and is why a page is the unit here rather
// than the daemon.

// errPageGone is what a sink reports when the page's stream has ended. It reads
// to a pump exactly as a dead `connect.ServerStream` does: the client went away.
var errPageGone = fmt.Errorf("server: the page's stream has ended")

// pageStream is one attached page: its outbound writer and its subscriptions.
type pageStream struct {
	id string
	// out carries pushes to the single writer goroutine. UNBUFFERED, so a
	// subscription's pump blocks until the writer has taken its push.
	out chan *agentreplv1.WatchPageResponse
	// ctx ends when the page's stream ends, which ends every subscription on it.
	ctx context.Context

	mu   sync.Mutex
	subs map[string]*pageSubscription
}

// pageSubscription is one watch running on a page.
type pageSubscription struct {
	cancel context.CancelFunc
	// unsubscribed marks an end the CLIENT asked for. Such a subscription
	// sends no `ended` frame: a client that asked for the end does not need to
	// be told about it.
	unsubscribed bool
}

// send hands one frame to the page's writer, or reports that the page is gone.
func (p *pageStream) send(msg *agentreplv1.WatchPageResponse) error {
	select {
	case p.out <- msg:
		return nil
	case <-p.ctx.Done():
		return errPageGone
	}
}

// pageSink is one subscription's `streamSink`. Every push it takes is wrapped
// in the arm its rpc owns and addressed to the subscription that asked for it.
type pageSink[R any] struct {
	page *pageStream
	sub  string
	// wrap puts one response into the `PageFrame` arm its rpc owns. It is a
	// function because the generated oneof wrappers are distinct types.
	wrap func(*R) *agentreplv1.PageFrame
}

func (s pageSink[R]) Send(msg *R) error {
	frame := s.wrap(msg)
	frame.Subscription = s.sub
	return s.page.send(&agentreplv1.WatchPageResponse{
		Frame: &agentreplv1.WatchPageResponse_Push{Push: frame},
	})
}

// attachPage registers a page and answers its stream. A SECOND stream for one
// page id is REFUSED rather than replacing the first: the first page's
// subscriptions would otherwise become addressable by a client that does not
// own them.
func (s *server) attachPage(id string, ctx context.Context) (*pageStream, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if _, held := s.pages[id]; held {
		return nil, fmt.Errorf("page %q already holds a stream on this daemon", id)
	}
	p := &pageStream{
		id:   id,
		out:  make(chan *agentreplv1.WatchPageResponse),
		ctx:  ctx,
		subs: make(map[string]*pageSubscription),
	}
	s.pages[id] = p
	return p, nil
}

// detachPage forgets a page whose stream has ended. Its subscriptions die with
// the stream's context, so nothing here has to cancel them one at a time.
func (s *server) detachPage(p *pageStream) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if held, ok := s.pages[p.id]; ok && held == p {
		delete(s.pages, p.id)
	}
}

// pageFor answers an attached page.
func (s *server) pageFor(id string) (*pageStream, bool) {
	s.mu.Lock()
	defer s.mu.Unlock()
	p, ok := s.pages[id]
	return p, ok
}

// WatchPage serves ONE page's whole standing surface.
//
// `PageAttached` is the FIRST frame and it is a LATCH: it is sent once the page
// is registered, so a client that has read it can subscribe and a client that
// has not cannot. Nothing buffers a subscription against a stream that may
// never arrive.
func (s *server) WatchPage(
	ctx context.Context,
	req *connect.Request[agentreplv1.WatchPageRequest],
	out *connect.ServerStream[agentreplv1.WatchPageResponse],
) error {
	const rpc = "WatchPage"
	if req.Msg.GetPage() == "" {
		return connect.NewError(connect.CodeInvalidArgument,
			fmt.Errorf("%s: page is required", rpc))
	}
	streamCtx, cancel := s.streamContext(ctx)
	defer cancel()

	page, err := s.attachPage(req.Msg.GetPage(), streamCtx)
	if err != nil {
		return TransportClosed(s.log, rpc, "page_already_attached", err.Error(), false)
	}
	defer s.detachPage(page)

	s.acceptStream(ctx, rpc)
	s.log.Debug(rpc, "attached a page's one standing stream",
		dlog.Context{"page": page.id})
	if err := out.Send(&agentreplv1.WatchPageResponse{
		Frame: &agentreplv1.WatchPageResponse_Attached{Attached: &agentreplv1.PageAttached{}},
	}); err != nil {
		s.log.Debug(rpc, "the page went away before it was told it was attached",
			dlog.Context{"page": page.id, "cause": err.Error()})
		return nil
	}

	for {
		select {
		case <-streamCtx.Done():
			s.log.Debug(rpc, "the page's stream ended on cancellation",
				dlog.Context{"page": page.id})
			return nil
		case msg := <-page.out:
			if err := out.Send(msg); err != nil {
				s.log.Debug(rpc, "the page's client went away",
					dlog.Context{"page": page.id, "cause": err.Error()})
				return nil
			}
		}
	}
}

// SubscribePage starts one subscription on an attached page.
//
// THE ANSWER MEANS THE SUBSCRIPTION EXISTS. It is withheld until the
// subscription's own handler body has accepted — the same moment the dedicated
// rpc flushes its response headers — so a view published after this call
// answers cannot be missed. A refusal comes back as the Connect error the
// dedicated rpc's open would have refused with, unchanged.
func (s *server) SubscribePage(
	ctx context.Context,
	req *connect.Request[agentreplv1.SubscribePageRequest],
) (*connect.Response[agentreplv1.SubscribePageResponse], error) {
	const rpc = "SubscribePage"
	switch {
	case req.Msg.GetPage() == "":
		return nil, connect.NewError(connect.CodeInvalidArgument,
			fmt.Errorf("%s: page is required", rpc))
	case req.Msg.GetSubscription() == "":
		return nil, connect.NewError(connect.CodeInvalidArgument,
			fmt.Errorf("%s: subscription is required", rpc))
	case req.Msg.GetRequest() == nil:
		return nil, connect.NewError(connect.CodeInvalidArgument,
			fmt.Errorf("%s: request names no watch to open", rpc))
	}

	page, attached := s.pageFor(req.Msg.GetPage())
	if !attached {
		return nil, TransportClosed(s.log, rpc, "page_not_attached",
			fmt.Sprintf("no page %q holds a stream on this daemon; WatchPage must be attached first",
				req.Msg.GetPage()), true)
	}

	watch, known := s.pageWatchFor(page, req.Msg)
	if !known {
		// An arm the schema grew and `pageWatchFor` did not. It is a defect in
		// this daemon, not a client error, and it is raised rather than
		// answered with a subscription that could never push anything.
		s.log.Error("daemon.server.page_subscription_unhandled",
			"a SubscribePage arm has no watch behind it; the subscription was refused",
			dlog.Context{"page": page.id, "subscription": req.Msg.GetSubscription(),
				"arm": fmt.Sprintf("%T", req.Msg.GetRequest())})
		return nil, connect.NewError(connect.CodeUnimplemented,
			fmt.Errorf("%s: arm %T has no watch behind it", rpc, req.Msg.GetRequest()))
	}

	subCtx, cancel := context.WithCancel(page.ctx)
	record := &pageSubscription{cancel: cancel}
	page.mu.Lock()
	if _, live := page.subs[req.Msg.GetSubscription()]; live {
		page.mu.Unlock()
		cancel()
		return nil, TransportClosed(s.log, rpc, "subscription_already_live",
			fmt.Sprintf("page %q already holds a subscription %q",
				req.Msg.GetPage(), req.Msg.GetSubscription()), false)
	}
	page.subs[req.Msg.GetSubscription()] = record
	page.mu.Unlock()

	// The acceptance latch: the body signals through the subscription's own
	// context when it has registered with its publisher, which is exactly what
	// `acceptStream` means for the dedicated rpc.
	accepted := make(chan struct{})
	var once sync.Once
	subCtx = withAcceptNotifier(subCtx, func() { once.Do(func() { close(accepted) }) })

	done := make(chan error, 1)
	go func() {
		done <- watch.Run(subCtx)
		s.endPageSubscription(page, req.Msg.GetSubscription())
	}()

	select {
	case <-accepted:
		s.log.Debug(rpc, "a page subscription was accepted", dlog.Context{
			"page": page.id, "subscription": req.Msg.GetSubscription(), "watch": watch.Name,
		})
		return connect.NewResponse(&agentreplv1.SubscribePageResponse{}), nil
	case err := <-done:
		// The body finished before it accepted: a refusal, or a validation
		// failure. It is this call's answer, exactly as it would have been the
		// dedicated rpc's.
		s.endPageSubscription(page, req.Msg.GetSubscription())
		if err == nil {
			err = TransportClosed(s.log, rpc, "watch_ended_before_acceptance",
				fmt.Sprintf("the %s subscription ended before it was accepted", watch.Name), false)
		}
		return nil, err
	case <-ctx.Done():
		cancel()
		return nil, connect.NewError(connect.CodeCanceled, ctx.Err())
	}
}

// UnsubscribePage ends one subscription and leaves the page's others alone.
//
// Unsubscribing one that does not exist is SUCCESS: the end is the state the
// caller asked for, and a subscription the daemon already ended is that state.
func (s *server) UnsubscribePage(
	ctx context.Context,
	req *connect.Request[agentreplv1.UnsubscribePageRequest],
) (*connect.Response[agentreplv1.UnsubscribePageResponse], error) {
	const rpc = "UnsubscribePage"
	switch {
	case req.Msg.GetPage() == "":
		return nil, connect.NewError(connect.CodeInvalidArgument,
			fmt.Errorf("%s: page is required", rpc))
	case req.Msg.GetSubscription() == "":
		return nil, connect.NewError(connect.CodeInvalidArgument,
			fmt.Errorf("%s: subscription is required", rpc))
	}
	answer := connect.NewResponse(&agentreplv1.UnsubscribePageResponse{})

	page, attached := s.pageFor(req.Msg.GetPage())
	if !attached {
		return answer, nil
	}
	page.mu.Lock()
	record, live := page.subs[req.Msg.GetSubscription()]
	if live {
		record.unsubscribed = true
	}
	page.mu.Unlock()
	if !live {
		return answer, nil
	}
	record.cancel()
	s.log.Debug(rpc, "a page subscription was ended by its client", dlog.Context{
		"page": page.id, "subscription": req.Msg.GetSubscription(),
	})
	return answer, nil
}

// endPageSubscription forgets a subscription whose body has returned and tells
// the page it is over — unless the client is the one who ended it.
func (s *server) endPageSubscription(page *pageStream, id string) {
	page.mu.Lock()
	record, live := page.subs[id]
	if live {
		delete(page.subs, id)
	}
	page.mu.Unlock()
	if !live {
		return
	}
	record.cancel()
	if record.unsubscribed {
		return
	}
	// The page's writer may already be gone; a failed send here means the page
	// itself ended, which is the strongest form of the same news.
	if err := page.send(&agentreplv1.WatchPageResponse{
		Frame: &agentreplv1.WatchPageResponse_Ended{
			Ended: &agentreplv1.PageSubscriptionEnded{Subscription: id},
		},
	}); err != nil {
		return
	}
	s.log.Debug("daemon.server.page_subscription_ended",
		"a page subscription ended on its own and its page was told",
		dlog.Context{"page": page.id, "subscription": id})
}

// pageWatch is what ONE arm of the subscription oneof resolves to: the rpc it
// stands for, and the body that serves it.
//
// THE ARM IS READ ONCE. Naming the watch and running it are the same question
// asked of the same oneof, and asking it in two switches is how the two come to
// disagree about an arm somebody added to only one of them.
type pageWatch struct {
	// Name is the rpc this subscription stands in for, for the log.
	Name string
	// Run serves it, writing into the page's mux.
	Run func(context.Context) error
}

// pageWatchFor resolves one subscription request onto the watch behind it.
//
// EVERY ARM CALLS THE SAME BODY THE DEDICATED RPC CALLS. There is no second
// implementation of any watch here, only a second destination for its pushes.
// An arm with nothing behind it answers false, which its caller raises.
func (s *server) pageWatchFor(
	page *pageStream,
	req *agentreplv1.SubscribePageRequest,
) (pageWatch, bool) {
	sub := req.GetSubscription()
	switch watch := req.GetRequest().(type) {
	case *agentreplv1.SubscribePageRequest_Roster:
		sink := pageSink[agentreplv1.WatchWorkspaceRosterResponse]{page: page, sub: sub,
			wrap: func(m *agentreplv1.WatchWorkspaceRosterResponse) *agentreplv1.PageFrame {
				return &agentreplv1.PageFrame{Payload: &agentreplv1.PageFrame_Roster{Roster: m}}
			}}
		return pageWatch{"WatchWorkspaceRoster", func(ctx context.Context) error {
			return s.watchWorkspaceRoster(ctx, watch.Roster, sink)
		}}, true
	case *agentreplv1.SubscribePageRequest_WebWorkspace:
		sink := pageSink[agentreplv1.WatchWebWorkspaceResponse]{page: page, sub: sub,
			wrap: func(m *agentreplv1.WatchWebWorkspaceResponse) *agentreplv1.PageFrame {
				return &agentreplv1.PageFrame{Payload: &agentreplv1.PageFrame_WebWorkspace{WebWorkspace: m}}
			}}
		return pageWatch{"WatchWebWorkspace", func(ctx context.Context) error {
			return s.watchWebWorkspace(ctx, watch.WebWorkspace, sink)
		}}, true
	case *agentreplv1.SubscribePageRequest_Daemon:
		sink := pageSink[agentreplv1.WatchDaemonResponse]{page: page, sub: sub,
			wrap: func(m *agentreplv1.WatchDaemonResponse) *agentreplv1.PageFrame {
				return &agentreplv1.PageFrame{Payload: &agentreplv1.PageFrame_Daemon{Daemon: m}}
			}}
		return pageWatch{"WatchDaemon", func(ctx context.Context) error {
			return s.watchDaemon(ctx, watch.Daemon, sink)
		}}, true
	case *agentreplv1.SubscribePageRequest_Topbar:
		sink := pageSink[agentreplv1.WatchTopbarResponse]{page: page, sub: sub,
			wrap: func(m *agentreplv1.WatchTopbarResponse) *agentreplv1.PageFrame {
				return &agentreplv1.PageFrame{Payload: &agentreplv1.PageFrame_Topbar{Topbar: m}}
			}}
		return pageWatch{"WatchTopbar", func(ctx context.Context) error {
			return s.watchTopbar(ctx, watch.Topbar, sink)
		}}, true
	case *agentreplv1.SubscribePageRequest_Footer:
		sink := pageSink[agentreplv1.WatchFooterResponse]{page: page, sub: sub,
			wrap: func(m *agentreplv1.WatchFooterResponse) *agentreplv1.PageFrame {
				return &agentreplv1.PageFrame{Payload: &agentreplv1.PageFrame_Footer{Footer: m}}
			}}
		return pageWatch{"WatchFooter", func(ctx context.Context) error {
			return s.watchFooter(ctx, watch.Footer, sink)
		}}, true
	case *agentreplv1.SubscribePageRequest_Holds:
		sink := pageSink[agentreplv1.WatchDaemonHoldsResponse]{page: page, sub: sub,
			wrap: func(m *agentreplv1.WatchDaemonHoldsResponse) *agentreplv1.PageFrame {
				return &agentreplv1.PageFrame{Payload: &agentreplv1.PageFrame_Holds{Holds: m}}
			}}
		return pageWatch{"WatchDaemonHolds", func(ctx context.Context) error {
			return s.watchDaemonHolds(ctx, watch.Holds, sink)
		}}, true
	case *agentreplv1.SubscribePageRequest_Feed:
		sink := pageSink[agentreplv1.WatchFeedResponse]{page: page, sub: sub,
			wrap: func(m *agentreplv1.WatchFeedResponse) *agentreplv1.PageFrame {
				return &agentreplv1.PageFrame{Payload: &agentreplv1.PageFrame_Feed{Feed: m}}
			}}
		return pageWatch{"WatchFeed", func(ctx context.Context) error {
			return s.watchFeed(ctx, watch.Feed, sink)
		}}, true
	case *agentreplv1.SubscribePageRequest_LoginTerminal:
		sink := pageSink[agentreplv1.LoginTerminalOutput]{page: page, sub: sub,
			wrap: func(m *agentreplv1.LoginTerminalOutput) *agentreplv1.PageFrame {
				return &agentreplv1.PageFrame{Payload: &agentreplv1.PageFrame_LoginTerminal{LoginTerminal: m}}
			}}
		return pageWatch{"WatchLoginTerminal", func(ctx context.Context) error {
			return s.watchLoginTerminal(ctx, watch.LoginTerminal, sink)
		}}, true
	default:
		return pageWatch{}, false
	}
}
