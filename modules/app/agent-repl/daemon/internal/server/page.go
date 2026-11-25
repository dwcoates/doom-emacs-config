package server

import (
	"context"
	"errors"
	"fmt"
	"sync"

	"connectrpc.com/connect"

	"agentrepl/logging/logsite"
	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"

	"agent-repld/internal/deploy"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/inflight"
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
	// sequence is this attachment's number (PageAttachment.sequence): a page
	// call is served only when it echoes it.
	sequence uint64
	// cancel ends the page's stream, which a newer attachment of the same
	// page id does to the one it replaces.
	cancel context.CancelFunc
	// out carries pushes to the single writer goroutine. UNBUFFERED, so a
	// subscription's pump blocks until the writer has taken its push.
	out chan *agentreplv1.WatchPageResponse
	// ctx ends when the page's stream ends, which ends every subscription on it.
	ctx context.Context
	// writerGone closes when the page's writer stops taking frames, which is
	// when a send can no longer reach the page. It is NOT ctx: a planned exit
	// ends ctx and the subscriptions with it, while the writer stays to carry
	// each subscription's last frame and then the page's own ending.
	writerGone chan struct{}
	// running counts the subscription bodies that have yet to send their last
	// frame, which a planned exit's drain waits out. Added to under mu, and
	// never once closing is set.
	running sync.WaitGroup

	mu   sync.Mutex
	subs map[string]*pageSubscription
	// closing is set when the planned exit's drain begins; a subscription
	// asked for after it is refused, since the page will carry nothing more.
	closing bool

	// handOff closes when the successor accepts on the shared address
	// (EndPageStreams): the page's stream then ends as a planned exit ends it,
	// so the page reattaches once, at the successor.
	handOff     chan struct{}
	handOffOnce sync.Once
}

// isClosing reports whether the page's planned ending has begun.
func (p *pageStream) isClosing() bool {
	p.mu.Lock()
	defer p.mu.Unlock()
	return p.closing
}

// pageSubscription is one watch running on a page. Every field is read and
// written under its page's mutex.
type pageSubscription struct {
	cancel context.CancelFunc
	// unsubscribed marks an end the CLIENT asked for, which is the `how` arm
	// its `ended` frame carries.
	unsubscribed bool
	// accepted marks a subscription whose body reached acceptance, which is
	// the moment `SubscribePage` answered that it EXISTS.
	//
	// NOTHING BEFORE THAT MOMENT IS ANNOUNCED ON THE PAGE. A body that ended
	// without accepting is a REFUSAL, and the refusal is `SubscribePage`'s own
	// Connect error — exactly as a refused dedicated open is. An `ended` frame
	// for it would name a subscription the client was told does not exist.
	accepted bool
}

// send hands one frame to the page's writer, or reports that the page is gone.
func (p *pageStream) send(msg *agentreplv1.WatchPageResponse) error {
	select {
	case p.out <- msg:
		return nil
	case <-p.writerGone:
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

// attachPage registers a page, issues its attachment, and answers its stream.
//
// A SECOND STREAM FOR ONE PAGE ID REPLACES THE FIRST. A page holds exactly
// one, so a second is the page reopening after an end this daemon has not
// finished unwinding (the client reopens the moment it reads the ending, and
// the first handler's detach has not run yet). The first stream is ended,
// which ends every subscription on it, and the second is issued its own
// sequence, so a call still echoing the first's is answered `page_gone`.
func (s *server) attachPage(id string, ctx context.Context, cancel context.CancelFunc) *pageStream {
	s.mu.Lock()
	defer s.mu.Unlock()
	if older, held := s.pages[id]; held {
		older.cancel()
		s.log.Info("WatchPage", "a page attached again while its earlier stream still stood here; the earlier stream is ended",
			dlog.Context{"page": id, "replaced_sequence": older.sequence})
	}
	s.pageSeq++
	p := &pageStream{
		id:         id,
		sequence:   s.pageSeq,
		cancel:     cancel,
		out:        make(chan *agentreplv1.WatchPageResponse),
		ctx:        ctx,
		writerGone: make(chan struct{}),
		subs:       make(map[string]*pageSubscription),
		handOff:    make(chan struct{}),
	}
	// A PAGE ATTACHING AFTER THE HAND-OFF (its request was taken before this
	// daemon stopped accepting, and reached here after EndPageStreams) is
	// handed off at once, so no page is left on a daemon that is not the one
	// accepting.
	if s.pagesHandedOff {
		p.handOffOnce.Do(func() { close(p.handOff) })
	}
	s.pages[id] = p
	// THE OPENS LEARN OF THE PAGE UNDER THE SAME LOCK, so the attach and the
	// detach of one page id reach them in the order the map changed.
	s.pagePresentLocked(id)
	return p
}

// detachPage forgets a page whose stream has ended. Its subscriptions die with
// the stream's context, so nothing here has to cancel them one at a time.
func (s *server) detachPage(p *pageStream) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if held, ok := s.pages[p.id]; ok && held == p {
		delete(s.pages, p.id)
	}
	// Every fact the page held (what it serves, what it drew) ends once
	// nothing holds the page here: a stream that was replaced leaves the
	// page present under its replacement.
	s.pageAbsentLocked(p.id)
}

// pagePresentLocked counts one more holder of page ID on this daemon, telling
// the opens of the page when it is the first. The caller holds mu.
func (s *server) pagePresentLocked(id string) {
	s.pagePresence[id]++
	if s.pagePresence[id] == 1 {
		s.deps.Opens.PageAttached(id)
	}
}

// pageAbsentLocked counts one holder of page ID gone, telling the opens the
// page is gone when it was the last. The caller holds mu.
func (s *server) pageAbsentLocked(id string) {
	count, held := s.pagePresence[id]
	if !held {
		panic(fmt.Sprintf("server: page %q was released with no holder counted", id))
	}
	if count > 1 {
		s.pagePresence[id] = count - 1
		return
	}
	delete(s.pagePresence, id)
	s.deps.Opens.PageDetached(id)
}

// holdRelayedPage holds page PAGE as connected to workspace WS on this daemon
// for a relayed subscription (RelayedPageHeader): the page's stream is on
// another daemon, which runs the subscription here because this daemon serves
// WS. ACCEPTED is called when the subscription is accepted, which is when the
// page's watch of the workspace exists, and RELEASE when its stream ends.
func (s *server) holdRelayedPage(page string, ws ids.WorkspaceID) (accepted func(), release func()) {
	var (
		mu   sync.Mutex
		held bool
	)
	accepted = func() {
		mu.Lock()
		defer mu.Unlock()
		if held {
			return
		}
		held = true
		s.mu.Lock()
		s.pagePresentLocked(page)
		s.mu.Unlock()
		s.deps.Opens.PageServes(page, ws)
		s.deps.Pages.Served(page, ws)
		s.log.Debug("daemon.server.relayed_page", "a page on another daemon is connected to a workspace this daemon serves",
			dlog.Context{"page": page, dlog.KeyWorkspaceID: string(ws)})
	}
	release = func() {
		mu.Lock()
		defer mu.Unlock()
		if !held {
			return
		}
		held = false
		s.mu.Lock()
		s.pageAbsentLocked(page)
		s.mu.Unlock()
	}
	return accepted, release
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
	if host := req.Msg.GetWebhost(); host != nil && host.GetWebappBuild() == "" {
		return connect.NewError(connect.CodeInvalidArgument,
			fmt.Errorf("%s: webhost.webapp_build is required", rpc))
	}
	streamCtx, cancel := s.streamContext(ctx)
	defer cancel()

	page := s.attachPage(req.Msg.GetPage(), streamCtx, cancel)
	defer s.detachPage(page)
	defer close(page.writerGone)
	// THE WEB VIEW HOST'S BUILD AND RELOAD GENERATION stand for as long as
	// its page stream does: a deploy reloads a host on an older webapp, and
	// judges a host still stale by its generation (deploy/webhost.go).
	if host := req.Msg.GetWebhost(); host != nil {
		defer s.holdWebhostPage(page.id, host)()
		// A HOST ATTACHING ON ANOTHER WEBAPP than the installed one is
		// reloaded onto it (deploy/webhost.go): a handover never reloads it.
		s.deps.Deploy.ReconcileWebhost(deploy.WebhostEdgeHostAttached)
	}

	// THE ATTACH STATES THE INSTALLED WEBAPP, so a page on another build
	// subscribes nothing it could not decode until it is reloaded onto it. A
	// daemon that cannot read its own webapp's build serves no page.
	webapp, err := s.deps.Deploy.InstalledWebapp()
	if err != nil {
		s.log.Error(rpc, "could not read the installed webapp's build; the page is not attached", dlog.WithCause(dlog.Context{"page": page.id}, err))
		return connect.NewError(connect.CodeInternal, fmt.Errorf("%s: read the installed webapp's build: %w", rpc, err))
	}
	s.acceptStream(ctx, rpc)
	s.log.Info(rpc, "attached a page's one standing stream",
		dlog.Context{"page": page.id, "webapp_build": webapp, "sequence": page.sequence})
	if err := out.Send(&agentreplv1.WatchPageResponse{
		Frame: &agentreplv1.WatchPageResponse_Attached{Attached: &agentreplv1.PageAttached{
			WebappBuild: webapp,
			Attachment:  &agentreplv1.PageAttachment{Instance: string(s.deps.Instance), Sequence: page.sequence},
		}},
	}); err != nil {
		s.log.Debug(rpc, "the page went away before it was told it was attached",
			dlog.Context{"page": page.id, "cause": err.Error()})
		return nil
	}

	for {
		select {
		case <-streamCtx.Done():
			// A PLANNED EXIT ENDS THE PAGE IN ORDER: every subscription's
			// last frame, then the page's own ending, so the page reopens on
			// the next daemon without reading a cut link. A client that left
			// is owed nothing.
			if s.life.Err() != nil && ctx.Err() == nil {
				s.drainPageForExit(rpc, page, ctx, out)
				return nil
			}
			s.log.Info(rpc, "the page's stream ended on cancellation",
				dlog.Context{"page": page.id})
			return nil
		case <-page.handOff:
			// THE SUCCESSOR ACCEPTS ON THE SHARED ADDRESS NOW, and this daemon
			// on it no more: the page ends here as a planned exit ends it, so
			// its one reattach can only reach the successor.
			s.drainPageForExit(rpc, page, ctx, out)
			s.log.Info(rpc, "the page's stream ended for the successor, which now accepts on the shared address",
				dlog.Context{"page": page.id})
			return nil
		case msg := <-page.out:
			if err := out.Send(msg); err != nil {
				s.log.Info(rpc, "the page's client went away",
					dlog.Context{"page": page.id, "cause": err.Error()})
				return nil
			}
		}
	}
}

// EndPageStreams ends every page stream this daemon holds, each as its
// planned exit ends it: every subscription's last frame, then the page's
// `ending`. It is called once the successor accepts on the shared address
// (rollout's yield), which is the handover's promotion: a page reopening
// after it reaches only the successor, so each page attaches once, there,
// however long this daemon goes on serving workspaces still busy here.
func (s *server) EndPageStreams() {
	s.mu.Lock()
	s.pagesHandedOff = true
	pages := make([]*pageStream, 0, len(s.pages))
	for _, p := range s.pages {
		pages = append(pages, p)
	}
	s.mu.Unlock()
	for _, p := range pages {
		p.handOffOnce.Do(func() { close(p.handOff) })
	}
	s.log.Info("daemon.server.page_streams_ended", "every page stream here ends for the successor, which now accepts on the shared address",
		dlog.Context{"pages": len(pages)})
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
	case req.Msg.GetAttachment().GetInstance() == "":
		return nil, connect.NewError(connect.CodeInvalidArgument,
			fmt.Errorf("%s: attachment is required", rpc))
	}

	page, gone := s.pageUnder(req.Msg.GetPage(), req.Msg.GetAttachment())
	if gone != "" {
		return s.pageGone(rpc, req.Msg, gone), nil
	}

	watch, known := s.pageWatchFor(page, req.Msg)
	if known {
		// THE VIEW COMES FROM THE DAEMON THAT SERVES IT. A workspace another
		// live daemon serves, or a feed tail another daemon pinned, is run as
		// a relay of the dedicated rpc on that daemon; everything else runs
		// here.
		target, elsewhere, err := s.relayTarget(ctx, watch)
		if err != nil {
			return nil, boundaryFailure(err)
		}
		if elsewhere {
			relay, here := watch.Relay, watch.Run
			// A TARGET THAT NO LONGER ACCEPTS CONNECTIONS SERVES NOTHING: the
			// view is run here, as it is for a daemon that no longer runs
			// (targetGone).
			watch.Run = func(ctx context.Context) error {
				if err := relay(ctx, target); !errors.Is(err, errTargetGone) {
					return err
				}
				return here(ctx)
			}
			s.log.Debug(rpc, "a page subscription is relayed from the daemon that serves it", dlog.Context{
				"page": page.id, "subscription": req.Msg.GetSubscription(), "watch": watch.Name, "target": target,
			})
		}
	}
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
	if page.closing {
		page.mu.Unlock()
		cancel()
		return s.pageGone(rpc, req.Msg, "the page is ending with this daemon's planned exit"), nil
	}
	page.subs[req.Msg.GetSubscription()] = record
	page.running.Add(1)
	page.mu.Unlock()

	// The acceptance latch: the body signals through the subscription's own
	// context when it has registered with its publisher, which is exactly what
	// `acceptStream` means for the dedicated rpc.
	accepted := make(chan struct{})
	var once sync.Once
	served, servesWorkspace := pageSubscriptionWorkspace(req.Msg)
	subCtx = withAcceptNotifier(subCtx, func() {
		once.Do(func() {
			page.mu.Lock()
			record.accepted = true
			page.mu.Unlock()
			// THE PAGE SERVES ITS WORKSPACE FROM ITS FIRST ACCEPTED
			// SUBSCRIPTION ON IT: the moment the page's watch of that
			// workspace exists, which is what an open's `page_connected`
			// means.
			if servesWorkspace {
				s.deps.Opens.PageServes(page.id, served)
				s.deps.Pages.Served(page.id, served)
			}
			close(accepted)
		})
	})

	// THE END IS STATED BEFORE THE ANSWER IS, which is what keeps the two from
	// racing: `endPageSubscription` runs before `done` carries the body's
	// error, so the branch below that reads it never has to end the
	// subscription a second time and the two can never disagree about which
	// frame the ending owes.
	done := make(chan error, 1)
	inflight.Standing(func() {
		defer page.running.Done()
		err := watch.Run(subCtx)
		s.endPageSubscription(page, req.Msg.GetSubscription(), err)
		done <- err
	})

	select {
	case <-accepted:
		s.log.Debug(rpc, "a page subscription was accepted", dlog.Context{
			"page": page.id, "subscription": req.Msg.GetSubscription(), "watch": watch.Name,
		})
		return connect.NewResponse(subscribedAnswer()), nil
	case err := <-done:
		// ACCEPTANCE WINS WHEN BOTH ARE READY, and they are ready together
		// whenever a body accepts and then finishes at once — a login terminal
		// whose pty is already gone does exactly that. Go picks a ready case at
		// random, so without this the same subscription would be answered
		// "accepted" or "refused" depending on a coin toss, while the page's own
		// stream had already announced the end of a subscription the client was
		// told never existed. The two must never disagree.
		select {
		case <-accepted:
			s.log.Debug(rpc, "a page subscription was accepted and ended at once", dlog.Context{
				"page": page.id, "subscription": req.Msg.GetSubscription(), "watch": watch.Name,
			})
			return connect.NewResponse(subscribedAnswer()), nil
		default:
		}
		// THE PAGE ENDED UNDER IT: the page's planned ending cancelled a
		// subscription that had not yet accepted, which is the attachment
		// being over, not a refusal of the watch.
		if page.isClosing() {
			return s.pageGone(rpc, req.Msg, "the page's stream ended while the subscription was being made"), nil
		}
		// The body finished before it accepted: a refusal, or a validation
		// failure. It is this call's answer, exactly as it would have been the
		// dedicated rpc's. The subscription is already ended and, never having
		// been accepted, announced nothing on the page.
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

// pageUnder answers the page PAGE attached here under ATTACHMENT, or why that
// attachment is over: the daemon that issued it is not this one (which a call
// reaches only when that daemon no longer runs: forward.go instanceTarget),
// the page holds no stream here, or it was issued a newer attachment.
func (s *server) pageUnder(id string, attachment *agentreplv1.PageAttachment) (*pageStream, string) {
	if attachment.GetInstance() != string(s.deps.Instance) {
		return nil, "the daemon that issued the attachment no longer runs"
	}
	page, attached := s.pageFor(id)
	if !attached {
		return nil, "the page holds no stream on this daemon"
	}
	if page.sequence != attachment.GetSequence() {
		return nil, "the page was issued a newer attachment"
	}
	return page, ""
}

// pageGone answers a subscribe whose attachment is over, and records it. It
// is a planned outcome, recorded at INFO: the page ends that attachment and
// subscribes again under its next one.
func (s *server) pageGone(rpc string, req *agentreplv1.SubscribePageRequest, why string) *connect.Response[agentreplv1.SubscribePageResponse] {
	s.log.Info("daemon.server.page_gone", "a page call carried an attachment that is over; it was answered page_gone", dlog.Context{
		"rpc": rpc, "page": req.GetPage(), "subscription": req.GetSubscription(),
		"instance": req.GetAttachment().GetInstance(), "sequence": req.GetAttachment().GetSequence(), "why": why,
	})
	return connect.NewResponse(&agentreplv1.SubscribePageResponse{
		Result: &agentreplv1.SubscribePageResponse_Success{Success: &agentreplv1.SubscribePageSuccess{
			Outcome: &agentreplv1.SubscribePageSuccess_PageGone{PageGone: &agentreplv1.SubscribePagePageGone{}},
		}},
	})
}

// subscribedAnswer is the answer of a subscription that exists.
func subscribedAnswer() *agentreplv1.SubscribePageResponse {
	return &agentreplv1.SubscribePageResponse{
		Result: &agentreplv1.SubscribePageResponse_Success{Success: &agentreplv1.SubscribePageSuccess{
			Outcome: &agentreplv1.SubscribePageSuccess_Subscribed{Subscribed: &agentreplv1.SubscribePageSubscribed{}},
		}},
	}
}

// UnsubscribePage ends one subscription and leaves the page's others alone.
//
// THE END IS STILL ANNOUNCED, carrying the `unsubscribed` arm. The ending is
// ONE fact on ONE wire whoever asked for it: suppressing the client's own would
// leave a page inferring, from silence, the difference between the end it asked
// for and an end that raced its request.
//
// Unsubscribing one that does not exist is SUCCESS: the end is the state the
// caller asked for, and a subscription the daemon already ended, or one made
// under an attachment that is itself over, is that state.
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
	case req.Msg.GetAttachment().GetInstance() == "":
		return nil, connect.NewError(connect.CodeInvalidArgument,
			fmt.Errorf("%s: attachment is required", rpc))
	}
	answer := connect.NewResponse(&agentreplv1.UnsubscribePageResponse{})

	page, gone := s.pageUnder(req.Msg.GetPage(), req.Msg.GetAttachment())
	if gone != "" {
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
// the page it is over, NAMING WHY.
//
// err is the body's own return: the Connect error a dedicated rpc would have
// ended its stream with, or nil for a source that simply finished.
func (s *server) endPageSubscription(page *pageStream, id string, err error) {
	// The flags are READ UNDER THE PAGE'S LOCK, in the same critical section
	// that removes the record. UnsubscribePage sets `unsubscribed` on a record
	// it found in this map under this lock, so reading it afterwards outside
	// the lock would be a race with the very client whose request decides
	// which arm this frame carries.
	page.mu.Lock()
	record, live := page.subs[id]
	var accepted, unsubscribed bool
	if live {
		accepted, unsubscribed = record.accepted, record.unsubscribed
		delete(page.subs, id)
	}
	page.mu.Unlock()
	if !live {
		return
	}
	record.cancel()
	if !accepted {
		// Never accepted, so the client was told the subscription does not
		// exist. Announcing its end would name an id it never held.
		return
	}
	// THE EXIT'S END IS THE PAGE'S ENDING, NOT AN `ended` FRAME: an ended
	// subscription that nobody unsubscribed reads on the page as a source
	// that finished, which it files as a failure. The page's `ending` frame,
	// sent once every subscription has finished, is this subscription's end.
	if !unsubscribed && (s.life.Err() != nil || page.isClosing()) {
		s.log.Debug("daemon.server.page_subscription_ended",
			"a page subscription ended with the daemon's planned exit; the page's ending frame is its end",
			dlog.Context{"page": page.id, "subscription": id})
		return
	}
	ended, arm := endedFrame(id, unsubscribed, err)
	// The page's writer may already be gone; a failed send here means the page
	// itself ended, which is the strongest form of the same news.
	if sendErr := page.send(&agentreplv1.WatchPageResponse{
		Frame: &agentreplv1.WatchPageResponse_Ended{Ended: ended},
	}); sendErr != nil {
		return
	}
	s.log.Debug("daemon.server.page_subscription_ended",
		"a page subscription ended and its page was told why",
		dlog.Context{"page": page.id, "subscription": id, "how": arm})
}

// endedFrame builds the ending's frame, naming its `how` arm and answering the
// arm's own name for the log alongside it.
//
// AN ARM IS ALWAYS SET. An `ended` frame with no `how` says a subscription
// stopped without saying whether anything went wrong, which is precisely the
// fact the arm exists to carry.
//
// THE CLIENT'S OWN REQUEST WINS OVER THE BODY'S RETURN. UnsubscribePage cancels
// the subscription's context and the body then reports whatever its source
// reports on cancellation; calling that a failure would turn every ordinary
// collapse of a subagent bubble into an error drawn on the page.
func endedFrame(id string, unsubscribed bool, err error) (*agentreplv1.PageSubscriptionEnded, string) {
	ended := &agentreplv1.PageSubscriptionEnded{Subscription: id}
	switch {
	case unsubscribed:
		ended.How = &agentreplv1.PageSubscriptionEnded_Unsubscribed{
			Unsubscribed: &agentreplv1.PageSubscriptionUnsubscribed{},
		}
		return ended, "unsubscribed"
	case errors.Is(err, errSubscriptionMoved):
		// THE SERVING DAEMON ENDED THE VIEW CLEANLY (relayWatch): nothing
		// failed and the view is still wanted, so the page subscribes again.
		ended.How = &agentreplv1.PageSubscriptionEnded_Moved{
			Moved: &agentreplv1.PageSubscriptionMoved{},
		}
		return ended, "moved"
	case err != nil:
		ended.How = &agentreplv1.PageSubscriptionEnded_Failed{Failed: failedAs(err)}
		return ended, "failed"
	default:
		ended.How = &agentreplv1.PageSubscriptionEnded_SourceEnded{
			SourceEnded: &agentreplv1.PageSubscriptionSourceEnded{},
		}
		return ended, "source_ended"
	}
}

// failedAs renders one watch body's error as the failure the page draws: the
// SAME Connect code and sentence the dedicated rpc's stream would have carried.
//
// An error that is not a Connect error at all is `internal`, which is exactly
// what connect-go itself would have stamped on it had it reached the wire as a
// dedicated stream's status.
func failedAs(err error) *agentreplv1.PageSubscriptionFailed {
	var coded *connect.Error
	if errors.As(err, &coded) {
		failed := &agentreplv1.PageSubscriptionFailed{
			Code: coded.Code().String(), Message: coded.Message(),
		}
		// A HANDOVER IS NOT A FAULT: the successor rides the error's metadata
		// (SuccessorHeader), and the page draws a move from it rather than an
		// outage.
		if successor := coded.Meta().Get(SuccessorHeader); successor != "" {
			failed.HandedAway = &agentreplv1.PageSubscriptionHandedAway{Address: successor}
		}
		// NEITHER IS A STREAM WITHHELD FROM A WORKSPACE THAT IS NOT VISIBLE
		// (WithheldHeader): the page files no failure for it.
		if coded.Meta().Get(WithheldHeader) != "" {
			failed.Withheld = &agentreplv1.PageSubscriptionWithheld{}
		}
		return failed
	}
	return &agentreplv1.PageSubscriptionFailed{
		Code: connect.CodeInternal.String(), Message: err.Error(),
	}
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
	// Workspace is the workspace whose serving daemon holds the view, empty
	// for a view of the daemon itself (the roster, the daemon stream).
	Workspace ids.WorkspaceID
	// Minter is the daemon instance that holds the view's pin: a feed tail's
	// token names it. Empty for every other watch.
	Minter string
	// Relay serves it from the daemon at TARGET instead of this one, writing
	// that daemon's pushes into the page's mux. Nil for a watch that is never
	// relayed.
	Relay func(ctx context.Context, target string) error
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
		return pageWatch{Name: "WatchWorkspaceRoster", Run: func(ctx context.Context) error {
			return serveThenEnd(s, "WatchWorkspaceRoster", sink, rosterEnding(), func() error { return s.watchWorkspaceRoster(ctx, watch.Roster, sink) })
		}}, true
	case *agentreplv1.SubscribePageRequest_WebWorkspace:
		sink := pageSink[agentreplv1.WatchWebWorkspaceResponse]{page: page, sub: sub,
			wrap: func(m *agentreplv1.WatchWebWorkspaceResponse) *agentreplv1.PageFrame {
				return &agentreplv1.PageFrame{Payload: &agentreplv1.PageFrame_WebWorkspace{WebWorkspace: m}}
			}}
		return pageWatch{Name: "WatchWebWorkspace", Run: func(ctx context.Context) error {
			return s.watchWebWorkspace(ctx, watch.WebWorkspace, sink)
		},
			Workspace: ids.WorkspaceID(watch.WebWorkspace.GetWorkspace().GetId()),
			Relay: func(ctx context.Context, target string) error {
				return relayWatch(ctx, s, "WatchWebWorkspace", agentreplv1connect.AgentReplWatchWebWorkspaceProcedure, target, page.id, watch.WebWorkspace, sink)
			}}, true
	case *agentreplv1.SubscribePageRequest_Daemon:
		sink := pageSink[agentreplv1.WatchDaemonResponse]{page: page, sub: sub,
			wrap: func(m *agentreplv1.WatchDaemonResponse) *agentreplv1.PageFrame {
				return &agentreplv1.PageFrame{Payload: &agentreplv1.PageFrame_Daemon{Daemon: m}}
			}}
		return pageWatch{Name: "WatchDaemon", Run: func(ctx context.Context) error {
			return serveThenEnd(s, "WatchDaemon", sink, daemonEnding(), func() error { return s.watchDaemon(ctx, watch.Daemon, sink) })
		}}, true
	case *agentreplv1.SubscribePageRequest_Topbar:
		sink := pageSink[agentreplv1.WatchTopbarResponse]{page: page, sub: sub,
			wrap: func(m *agentreplv1.WatchTopbarResponse) *agentreplv1.PageFrame {
				return &agentreplv1.PageFrame{Payload: &agentreplv1.PageFrame_Topbar{Topbar: m}}
			}}
		return pageWatch{Name: "WatchTopbar", Run: func(ctx context.Context) error {
			return s.watchTopbar(ctx, watch.Topbar, sink)
		},
			Workspace: ids.WorkspaceID(watch.Topbar.GetWorkspace().GetId()),
			Relay: func(ctx context.Context, target string) error {
				return relayWatch(ctx, s, "WatchTopbar", agentreplv1connect.AgentReplWatchTopbarProcedure, target, page.id, watch.Topbar, sink)
			}}, true
	case *agentreplv1.SubscribePageRequest_WorkspaceMentions:
		sink := pageSink[agentreplv1.WatchWorkspaceMentionsResponse]{page: page, sub: sub,
			wrap: func(m *agentreplv1.WatchWorkspaceMentionsResponse) *agentreplv1.PageFrame {
				return &agentreplv1.PageFrame{Payload: &agentreplv1.PageFrame_WorkspaceMentions{WorkspaceMentions: m}}
			}}
		return pageWatch{Name: "WatchWorkspaceMentions", Run: func(ctx context.Context) error {
			return serveThenEnd(s, "WatchWorkspaceMentions", sink, workspaceMentionsEnding(), func() error { return s.watchWorkspaceMentions(ctx, watch.WorkspaceMentions, sink) })
		},
			Workspace: ids.WorkspaceID(watch.WorkspaceMentions.GetViewer().GetId()),
			Relay: func(ctx context.Context, target string) error {
				return relayWatch(ctx, s, "WatchWorkspaceMentions", agentreplv1connect.AgentReplWatchWorkspaceMentionsProcedure, target, page.id, watch.WorkspaceMentions, sink)
			}}, true
	case *agentreplv1.SubscribePageRequest_Chat:
		sink := pageSink[agentreplv1.WatchChatResponse]{page: page, sub: sub,
			wrap: func(m *agentreplv1.WatchChatResponse) *agentreplv1.PageFrame {
				return &agentreplv1.PageFrame{Payload: &agentreplv1.PageFrame_Chat{Chat: m}}
			}}
		return pageWatch{Name: "WatchChat", Run: func(ctx context.Context) error {
			return s.watchChat(ctx, watch.Chat, sink)
		},
			Workspace: ids.WorkspaceID(watch.Chat.GetThread().GetId()),
			Relay: func(ctx context.Context, target string) error {
				return relayWatch(ctx, s, "WatchChat", agentreplv1connect.AgentReplWatchChatProcedure, target, page.id, watch.Chat, sink)
			}}, true
	case *agentreplv1.SubscribePageRequest_Footer:
		sink := pageSink[agentreplv1.WatchFooterResponse]{page: page, sub: sub,
			wrap: func(m *agentreplv1.WatchFooterResponse) *agentreplv1.PageFrame {
				return &agentreplv1.PageFrame{Payload: &agentreplv1.PageFrame_Footer{Footer: m}}
			}}
		return pageWatch{Name: "WatchFooter", Run: func(ctx context.Context) error {
			return s.watchFooter(ctx, watch.Footer, sink)
		},
			Workspace: ids.WorkspaceID(watch.Footer.GetWorkspace().GetId()),
			Relay: func(ctx context.Context, target string) error {
				return relayWatch(ctx, s, "WatchFooter", agentreplv1connect.AgentReplWatchFooterProcedure, target, page.id, watch.Footer, sink)
			}}, true
	case *agentreplv1.SubscribePageRequest_Feed:
		sink := pageSink[agentreplv1.WatchFeedResponse]{page: page, sub: sub,
			wrap: func(m *agentreplv1.WatchFeedResponse) *agentreplv1.PageFrame {
				return &agentreplv1.PageFrame{Payload: &agentreplv1.PageFrame_Feed{Feed: m}}
			}}
		return pageWatch{Name: "WatchFeed", Run: func(ctx context.Context) error {
			return s.watchFeed(ctx, watch.Feed, sink)
		},
			Minter: watch.Feed.GetWatch().GetInstance(),
			Relay: func(ctx context.Context, target string) error {
				return relayWatch(ctx, s, "WatchFeed", agentreplv1connect.AgentReplWatchFeedProcedure, target, page.id, watch.Feed, sink)
			}}, true
	case *agentreplv1.SubscribePageRequest_LoginTerminal:
		sink := pageSink[agentreplv1.LoginTerminalOutput]{page: page, sub: sub,
			wrap: func(m *agentreplv1.LoginTerminalOutput) *agentreplv1.PageFrame {
				return &agentreplv1.PageFrame{Payload: &agentreplv1.PageFrame_LoginTerminal{LoginTerminal: m}}
			}}
		return pageWatch{Name: "WatchLoginTerminal", Run: func(ctx context.Context) error {
			return s.watchLoginTerminal(ctx, watch.LoginTerminal, sink)
		},
			Workspace: ids.WorkspaceID(watch.LoginTerminal.GetWorkspace().GetId()),
			Relay: func(ctx context.Context, target string) error {
				return relayWatch(ctx, s, "WatchLoginTerminal", agentreplv1connect.AgentReplWatchLoginTerminalProcedure, target, page.id, watch.LoginTerminal, sink)
			}}, true
	default:
		return pageWatch{}, false
	}
}

// drainPageForExit ends a page in a planned exit: it keeps carrying frames
// until every subscription on the page has sent its last one (its kind's
// planned ending, where its rpc has one), then sends the page's own ending.
// A page whose client goes away meanwhile is owed nothing more.
func (s *server) drainPageForExit(rpc string, page *pageStream, ctx context.Context, out streamSink[agentreplv1.WatchPageResponse]) {
	clientGone := logsite.Msg("the page's client went away while its exit drained")
	// CLOSING IS SET BEFORE ANY SUBSCRIPTION ENDS, so each one's end reads it
	// and leaves its ending to the page's own (endPageSubscription). A
	// daemon's exit has already ended them with its lifetime; a hand-off to
	// the successor ends them here.
	page.mu.Lock()
	page.closing = true
	cancels := make([]context.CancelFunc, 0, len(page.subs))
	for _, sub := range page.subs {
		cancels = append(cancels, sub.cancel)
	}
	page.mu.Unlock()
	for _, cancel := range cancels {
		cancel()
	}
	drained := make(chan struct{})
	// inflight: joined: the function that starts it waits for it.
	go func() {
		page.running.Wait()
		close(drained)
	}()
	for {
		select {
		case msg := <-page.out:
			if err := out.Send(msg); err != nil {
				s.log.Log(dlog.LevelDebug, rpc, clientGone,
					dlog.Context{"page": page.id, "cause": err.Error()})
				return
			}
		case <-drained:
			sendStandingEnding(rpc, s.log, out, pageEnding())
			s.log.Debug(rpc, "the page's subscriptions sent their last frames; the page was told the daemon is standing down",
				dlog.Context{"page": page.id})
			return
		case <-ctx.Done():
			s.log.Log(dlog.LevelDebug, rpc, clientGone,
				dlog.Context{"page": page.id})
			return
		}
	}
}

// pageEnding is the page stream's planned ending frame.
func pageEnding() *agentreplv1.WatchPageResponse {
	return &agentreplv1.WatchPageResponse{
		Frame: &agentreplv1.WatchPageResponse_Ending{Ending: &agentreplv1.DaemonStreamEnding{}},
	}
}
