package server

import (
	"context"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/publish"
)

// THE SUBSCRIPTION INVARIANT, served once here for every standing stream:
//
//   - the LATEST published view is sent first when one exists;
//   - every later view follows in publication order, skipping none;
//   - a message is never partial and never empty — absence is the legal
//     not-yet state, and a stream with nothing published yet simply stays
//     quiet;
//   - the response headers are FLUSHED the moment the subscription is
//     registered, so acceptance is observable before the first view;
//   - the stream ends when the CLIENT cancels, or when the daemon closes.
//
// publish.Topic supplies the first two; acceptStream supplies the flush; the
// pump below supplies the last two.

// The view types the four resolver topics carry, aliased so the pump's wrap
// functions read as the view they send rather than as a package path.
type (
	frontendRoster = frontendv1.WorkspaceRoster
	frontendFooter = frontendv1.FooterView
	frontendTopbar = frontendv1.TopbarView
	frontendTray   = frontendv1.DaemonHoldTray
)

// streamSink is what a standing stream's pump writes its pushes to.
//
// IT IS AN INTERFACE BECAUSE A STREAM'S PUSHES NO LONGER HAVE ONE DESTINATION.
// A page multiplexes every standing subscription onto its single `WatchPage`
// stream (endpoint_watch_page.proto), so a `WatchFooter` body serves either the
// dedicated rpc's own `connect.ServerStream` or one page's mux — the same body,
// the same order, the same acceptance, writing somewhere else.
// `*connect.ServerStream[R]` satisfies it as it stands, so the dedicated rpcs
// are unchanged by it.
type streamSink[R any] interface {
	Send(*R) error
}

// streamContext ties a stream's lifetime to BOTH the client's cancellation and
// the daemon's Close, so nothing outlives the surface that serves it.
func (s *server) streamContext(ctx context.Context) (context.Context, context.CancelFunc) {
	streamCtx, cancel := context.WithCancel(ctx)
	go func() {
		select {
		case <-s.life.Done():
			cancel()
		case <-streamCtx.Done():
		}
	}()
	return streamCtx, cancel
}

// backgroundContext detaches work from the REQUEST's cancellation while still
// ending it with the daemon. context.WithoutCancel keeps the request's values
// (log scope and the like) but drops its cancellation, so a client deadline or
// disconnect can never cancel the work — the exact fix for a `git worktree add`
// that a cancelled request context used to kill mid-run. Cancellation is then
// re-supplied from the daemon's own lifetime alone, so the work still stops
// when the daemon closes and never outlives it. The caller MUST call the
// returned cancel, or the s.life watcher goroutine leaks.
func (s *server) backgroundContext(ctx context.Context) (context.Context, context.CancelFunc) {
	bgCtx, cancel := context.WithCancel(context.WithoutCancel(ctx))
	go func() {
		select {
		case <-s.life.Done():
			cancel()
		case <-bgCtx.Done():
		}
	}()
	return bgCtx, cancel
}

// serveTopic pumps one topic onto one server stream under the subscription
// invariant. It is a free function because Go methods take no type parameters.
func serveTopic[T comparable, R any](
	s *server,
	ctx context.Context,
	rpc string,
	log dlog.Logger,
	topic *publish.Topic[T],
	out streamSink[R],
	wrap func(T) *R,
) error {
	return serveTopicWith(s, ctx, rpc, log, topic, out, wrap, topicHooks[T]{})
}

// topicHooks are the optional edges one stream family can take on the pump.
// WatchDaemon is the only family that needs them: the stand-down announcement
// must be PROVEN onto every live stream before the orderly exit cancels
// serving, and proving it needs both the instant the subscription exists (so
// an announcer never waits on a stream that cannot yet receive) and the
// instant a view has actually been sent.
type topicHooks[T any] struct {
	// attached runs once the subscription exists; the func it returns runs
	// when the stream ends.
	attached func() func()
	// sent runs after each successful Send, with the view that was sent.
	sent func(T)
}

func serveTopicWith[T comparable, R any](
	s *server,
	ctx context.Context,
	rpc string,
	log dlog.Logger,
	topic *publish.Topic[T],
	out streamSink[R],
	wrap func(T) *R,
	hooks topicHooks[T],
) error {
	streamCtx, cancel := s.streamContext(ctx)
	defer cancel()

	views := topic.Subscribe(streamCtx)
	if hooks.attached != nil {
		detach := hooks.attached()
		defer detach()
	}
	s.acceptStream(ctx, rpc)
	log.Debug(rpc, "accepted a standing stream", nil)

	var zero T
	for {
		select {
		case <-streamCtx.Done():
			log.Debug(rpc, "the standing stream ended on cancellation", nil)
			return nil
		case view, ok := <-views:
			if !ok {
				log.Debug(rpc, "the standing stream's subscription closed", nil)
				return nil
			}
			if view == zero {
				// A resolver that published nothing at all would be publishing
				// a PARTIAL view, which the invariant forbids. It is raised
				// rather than forwarded.
				log.Error(rpc, "a publisher raised an empty view; it was not sent", nil)
				continue
			}
			if err := out.Send(wrap(view)); err != nil {
				log.Debug(rpc, "the standing stream's client went away",
					dlog.Context{"cause": err.Error()})
				return nil
			}
			if hooks.sent != nil {
				hooks.sent(view)
			}
		}
	}
}

// endStandingStream sends a standing stream's `DaemonStreamEnding` frame as
// its LAST frame when the daemon's own lifetime is what ended it -- and only
// then. The lifetime ends in `Close`, and `Close` runs on the planned exit
// alone (`serve` calls it through `Serving.EndStreams` on every stand-down: a
// handover, a restart, a drain, a signal, a state-root loss), so a client that
// reads this frame knows the clean end after it was planned. A stream its
// CLIENT ended is sent nothing: nobody is listening. An unplanned death never
// reaches here at all, which is exactly how the client tells the two apart.
//
// IT RUNS ONLY ON A DEDICATED RPC'S OWN STREAM, never inside a page's mux: a
// page subscription's sink ends with the page's stream at the same lifetime
// edge, so an ending sent there would race the page's own end, and the page
// stream has no planned-ending arm of its own for the frame to belong to.
func endStandingStream[R any](s *server, rpc string, log dlog.Logger, out streamSink[R], ending *R) {
	if s.life.Err() == nil {
		return
	}
	if err := out.Send(ending); err != nil {
		log.Debug(rpc, "the standing stream's client went away before its planned ending",
			dlog.Context{"cause": err.Error()})
		return
	}
	log.Debug(rpc, "sent the standing stream's planned ending", nil)
}

// refuseStream answers a refused stream open. A Watch* rpc has NO `<Rpc>Error`
// message — a refused open is a Connect error BEFORE any frame — and by ruling
// (landing 6) that is the SETTLED shape rather than an unlanded arm, so the
// refusal is recorded at INFO under "daemon.refusal.transport_closed" and never
// warned.
func refuseStream(log dlog.Logger, rpc string, r refusal) *connect.Error {
	return TransportClosed(log, rpc, r.Arm, r.Reason, r.NotFound)
}

// WatchWorkspaceRoster serves the ONE editor-global stream: the roster, whole,
// for Emacs and every webview alike.
func (s *server) WatchWorkspaceRoster(
	ctx context.Context,
	req *connect.Request[agentreplv1.WatchWorkspaceRosterRequest],
	out *connect.ServerStream[agentreplv1.WatchWorkspaceRosterResponse],
) error {
	const rpc = "WatchWorkspaceRoster"
	if err := s.watchWorkspaceRoster(ctx, req.Msg, out); err != nil {
		return err
	}
	endStandingStream(s, rpc, s.log, out, &agentreplv1.WatchWorkspaceRosterResponse{
		Push: &agentreplv1.WatchWorkspaceRosterResponse_Ending{Ending: &agentreplv1.DaemonStreamEnding{}},
	})
	return nil
}

// watchWorkspaceRoster is the body, written to whatever sink carries it: the
// dedicated rpc's own stream, or one page's mux.
func (s *server) watchWorkspaceRoster(
	ctx context.Context,
	_ *agentreplv1.WatchWorkspaceRosterRequest,
	out streamSink[agentreplv1.WatchWorkspaceRosterResponse],
) error {
	return serveTopic(s, ctx, "WatchWorkspaceRoster", s.log, s.deps.Sidebar.Topic(), out,
		func(roster *frontendRoster) *agentreplv1.WatchWorkspaceRosterResponse {
			return &agentreplv1.WatchWorkspaceRosterResponse{
				Push: &agentreplv1.WatchWorkspaceRosterResponse_Roster{Roster: roster},
			}
		})
}

// WatchFooter serves one workspace's footer view.
func (s *server) WatchFooter(
	ctx context.Context,
	req *connect.Request[agentreplv1.WatchFooterRequest],
	out *connect.ServerStream[agentreplv1.WatchFooterResponse],
) error {
	return s.watchFooter(ctx, req.Msg, out)
}

// watchFooter is the body, written to whatever sink carries it.
func (s *server) watchFooter(
	ctx context.Context,
	msg *agentreplv1.WatchFooterRequest,
	out streamSink[agentreplv1.WatchFooterResponse],
) error {
	const rpc = "WatchFooter"
	if err := validateWorkspaceRef("workspace", msg.GetWorkspace()); err != nil {
		return err
	}
	subject, r, err := s.resolveStreamRef(ctx, rpc, msg.GetWorkspace())
	if err != nil {
		return endStream(s.log, rpc, err)
	}
	if r != nil {
		return refuseStream(s.log, rpc, *r)
	}
	return serveTopic(s, ctx, rpc, subject.Log, s.deps.Footer.Topic(subject.Record.ID), out,
		func(view *frontendFooter) *agentreplv1.WatchFooterResponse {
			return &agentreplv1.WatchFooterResponse{Footer: view}
		})
}

// WatchTopbar serves one workspace's topbar view.
func (s *server) WatchTopbar(
	ctx context.Context,
	req *connect.Request[agentreplv1.WatchTopbarRequest],
	out *connect.ServerStream[agentreplv1.WatchTopbarResponse],
) error {
	return s.watchTopbar(ctx, req.Msg, out)
}

// watchTopbar is the body, written to whatever sink carries it.
func (s *server) watchTopbar(
	ctx context.Context,
	msg *agentreplv1.WatchTopbarRequest,
	out streamSink[agentreplv1.WatchTopbarResponse],
) error {
	const rpc = "WatchTopbar"
	if err := validateWorkspaceRef("workspace", msg.GetWorkspace()); err != nil {
		return err
	}
	subject, r, err := s.resolveStreamRef(ctx, rpc, msg.GetWorkspace())
	if err != nil {
		return endStream(s.log, rpc, err)
	}
	if r != nil {
		return refuseStream(s.log, rpc, *r)
	}
	return serveTopic(s, ctx, rpc, subject.Log, s.deps.Topbar.Topic(subject.Record.ID), out,
		func(view *frontendTopbar) *agentreplv1.WatchTopbarResponse {
			return &agentreplv1.WatchTopbarResponse{Topbar: view}
		})
}

// WatchDaemonHolds serves one workspace's daemon-hold tray.
func (s *server) WatchDaemonHolds(
	ctx context.Context,
	req *connect.Request[agentreplv1.WatchDaemonHoldsRequest],
	out *connect.ServerStream[agentreplv1.WatchDaemonHoldsResponse],
) error {
	return s.watchDaemonHolds(ctx, req.Msg, out)
}

// watchDaemonHolds is the body, written to whatever sink carries it.
func (s *server) watchDaemonHolds(
	ctx context.Context,
	msg *agentreplv1.WatchDaemonHoldsRequest,
	out streamSink[agentreplv1.WatchDaemonHoldsResponse],
) error {
	const rpc = "WatchDaemonHolds"
	if err := validateWorkspaceRef("workspace", msg.GetWorkspace()); err != nil {
		return err
	}
	subject, r, err := s.resolveStreamRef(ctx, rpc, msg.GetWorkspace())
	if err != nil {
		return endStream(s.log, rpc, err)
	}
	if r != nil {
		return refuseStream(s.log, rpc, *r)
	}
	return serveTopic(s, ctx, rpc, subject.Log, s.deps.Holds.Topic(subject.Record.ID), out,
		func(tray *frontendTray) *agentreplv1.WatchDaemonHoldsResponse {
			return &agentreplv1.WatchDaemonHoldsResponse{Tray: tray}
		})
}

// WatchHostWorkspace serves one workspace's HOST stream: the notifications,
// the transfer notice, the reload request and the editor link relay.
func (s *server) WatchHostWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.WatchHostWorkspaceRequest],
	out *connect.ServerStream[agentreplv1.WatchHostWorkspaceResponse],
) error {
	const rpc = "WatchHostWorkspace"
	if err := validateWorkspaceRef("workspace", req.Msg.GetWorkspace()); err != nil {
		return err
	}
	subject, r, err := s.resolveStreamRef(ctx, rpc, req.Msg.GetWorkspace())
	if err != nil {
		return endStream(s.log, rpc, err)
	}
	if r != nil {
		return refuseStream(s.log, rpc, *r)
	}
	s.holdParticipant(subject.Record.ID, true, +1)
	defer s.holdParticipant(subject.Record.ID, true, -1)

	// COMPOSE BEFORE SUBSCRIBING. The state topic replays its latest value to
	// a new subscriber, so publishing here is what gives every fresh
	// subscription its opening `host` push — including the first one, before
	// any session edge has ever fired.
	s.PublishHostWorkspace(ctx, subject.Record.ID)
	if err := s.serveHost(ctx, rpc, subject.Log, subject.Record.ID, out); err != nil {
		return err
	}
	endStandingStream(s, rpc, subject.Log, out, &agentreplv1.WatchHostWorkspaceResponse{
		Push: &agentreplv1.WatchHostWorkspaceResponse_Ending{Ending: &agentreplv1.DaemonStreamEnding{}},
	})
	return nil
}

// serveHost serves the host stream's TWO topics onto one wire: the `host`
// state and the four event arms. They are separate topics (see host.go) and
// this is the one place they are merged, in publication order per topic.
func (s *server) serveHost(
	ctx context.Context,
	rpc string,
	log dlog.Logger,
	ws ids.WorkspaceID,
	out streamSink[agentreplv1.WatchHostWorkspaceResponse],
) error {
	streamCtx, cancel := s.streamContext(ctx)
	defer cancel()

	states := s.hostStateTopic(ws).Subscribe(streamCtx)
	events := s.hostTopic(ws).Subscribe(streamCtx)
	// THE FEED'S SELECTION reaches Emacs from the one topic the webapp's root
	// feed watch reads, mapped to the kind of row alone (hostSelectionOf), so
	// Emacs and the webapp see one sequence of selections. The topic replays
	// the selection in force, so a late subscriber is handed it first.
	selections := s.selectionTopic(ws).Subscribe(streamCtx)
	s.acceptStream(ctx, rpc)
	log.Debug(rpc, "accepted a standing stream", nil)

	for {
		var push *agentreplv1.WatchHostWorkspaceResponse
		select {
		case <-streamCtx.Done():
			log.Debug(rpc, "the standing stream ended on cancellation", nil)
			return nil
		case view, ok := <-states:
			if !ok {
				log.Debug(rpc, "the standing stream's subscription closed", nil)
				return nil
			}
			if view == nil {
				log.Error(rpc, "the host composer raised an empty view; it was not sent", nil)
				continue
			}
			push = &agentreplv1.WatchHostWorkspaceResponse{
				Push: &agentreplv1.WatchHostWorkspaceResponse_Host{Host: view},
			}
		case event, ok := <-events:
			if !ok {
				log.Debug(rpc, "the standing stream's subscription closed", nil)
				return nil
			}
			if event == nil {
				log.Error(rpc, "a publisher raised an empty push; it was not sent", nil)
				continue
			}
			push = event
		case sel, ok := <-selections:
			if !ok {
				log.Debug(rpc, "the standing stream's subscription closed", nil)
				return nil
			}
			push = &agentreplv1.WatchHostWorkspaceResponse{
				Push: &agentreplv1.WatchHostWorkspaceResponse_Selection{Selection: hostSelectionOf(sel)},
			}
		}
		if err := out.Send(push); err != nil {
			log.Debug(rpc, "the standing stream's client went away",
				dlog.Context{"cause": err.Error()})
			return nil
		}
	}
}

// WatchWebWorkspace serves one workspace's WEB link stream. The web side never
// redials: the only push it carries is `transferred{address}`, and the reloaded
// page adopts through AdoptWebWorkspace rather than through this stream.
func (s *server) WatchWebWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.WatchWebWorkspaceRequest],
	out *connect.ServerStream[agentreplv1.WatchWebWorkspaceResponse],
) error {
	return s.watchWebWorkspace(ctx, req.Msg, out)
}

// watchWebWorkspace is the body, written to whatever sink carries it.
func (s *server) watchWebWorkspace(
	ctx context.Context,
	msg *agentreplv1.WatchWebWorkspaceRequest,
	out streamSink[agentreplv1.WatchWebWorkspaceResponse],
) error {
	const rpc = "WatchWebWorkspace"
	if err := validateWatchWebWorkspaceRequest(msg); err != nil {
		return err
	}
	subject, r, err := s.resolveStreamRef(ctx, rpc, msg.GetWorkspace())
	if err != nil {
		return endStream(s.log, rpc, err)
	}
	if r != nil {
		return refuseStream(s.log, rpc, *r)
	}
	s.holdParticipant(subject.Record.ID, false, +1)
	defer s.holdParticipant(subject.Record.ID, false, -1)
	// THE PAGE'S BUILD STANDS FOR AS LONG AS ITS STREAM DOES: a deploy tells
	// exactly the webviews on an older webapp to reload.
	defer s.holdWebBuild(subject.Record.ID, msg.GetWebappBuild())()
	subject.Log.Debug(rpc, "the webview reported its webapp build", dlog.Context{"webapp_build": msg.GetWebappBuild()})

	// COMPOSE BEFORE SUBSCRIBING, as the host stream does. The state topic
	// replays its latest value to a new subscriber, so publishing here is what
	// gives every fresh subscription its opening `session_identity` push --
	// including the first one, before any session edge has ever fired. The
	// page binds its log context from it, so a page that never got one would
	// forward every record of its life unattributed.
	s.publishWebSessionIdentity(ctx, subject.Log, subject.Record.ID)
	return s.serveWeb(ctx, rpc, subject.Log, subject.Record.ID, out)
}

// serveWeb serves the web stream's TWO topics onto one wire: the
// `session_identity` state and the `transferred` event. They are separate
// topics (see webidentity.go) and this is the one place they are merged, in
// publication order per topic.
func (s *server) serveWeb(
	ctx context.Context,
	rpc string,
	log dlog.Logger,
	ws ids.WorkspaceID,
	out streamSink[agentreplv1.WatchWebWorkspaceResponse],
) error {
	streamCtx, cancel := s.streamContext(ctx)
	defer cancel()

	states := s.webStateTopic(ws).Subscribe(streamCtx)
	events := s.webTopic(ws).Subscribe(streamCtx)
	s.acceptStream(ctx, rpc)
	log.Debug(rpc, "accepted a standing stream", nil)

	for {
		var push *agentreplv1.WatchWebWorkspaceResponse
		select {
		case <-streamCtx.Done():
			log.Debug(rpc, "the standing stream ended on cancellation", nil)
			return nil
		case identity, ok := <-states:
			if !ok {
				log.Debug(rpc, "the standing stream's subscription closed", nil)
				return nil
			}
			if identity == nil {
				log.Error(rpc, "the web identity composer raised an empty identity; it was not sent", nil)
				continue
			}
			push = &agentreplv1.WatchWebWorkspaceResponse{
				Push: &agentreplv1.WatchWebWorkspaceResponse_SessionIdentity{
					SessionIdentity: identity,
				},
			}
		case event, ok := <-events:
			if !ok {
				log.Debug(rpc, "the standing stream's subscription closed", nil)
				return nil
			}
			if event == nil {
				log.Error(rpc, "a publisher raised an empty push; it was not sent", nil)
				continue
			}
			push = event
		}
		if err := out.Send(push); err != nil {
			log.Debug(rpc, "the standing stream's client went away",
				dlog.Context{"cause": err.Error()})
			return nil
		}
	}
}

// WatchDaemon serves the DAEMON-LEVEL stream — daemon-scoped facts only. It is
// held by Emacs AND by every webview alike (ruling R3): the drain banner and
// the stand-down announcement are drawn by both.
func (s *server) WatchDaemon(
	ctx context.Context,
	req *connect.Request[agentreplv1.WatchDaemonRequest],
	out *connect.ServerStream[agentreplv1.WatchDaemonResponse],
) error {
	if err := s.watchDaemon(ctx, req.Msg, out); err != nil {
		return err
	}
	endStandingStream(s, "WatchDaemon", s.log, out, &agentreplv1.WatchDaemonResponse{
		Push: &agentreplv1.WatchDaemonResponse_Ending{Ending: &agentreplv1.DaemonStreamEnding{}},
	})
	return nil
}

// watchDaemon is the body, written to whatever sink carries it. It merges the
// daemon's TWO topics onto one wire, as serveWeb merges the web stream's state
// and event topics: daemonTopic carries state (the drain schedule, the
// stand-down announcement), daemonEventTopic carries events (workspace-mutation
// progress). They are merged here in publication order per topic. The
// announcement machinery — the daemonWatcher the orderly exit flushes onto —
// hangs off the STATE topic's shutdown push alone; an event never satisfies it.
func (s *server) watchDaemon(
	ctx context.Context,
	msg *agentreplv1.WatchDaemonRequest,
	out streamSink[agentreplv1.WatchDaemonResponse],
) error {
	if err := validateWatchDaemonRequest(msg); err != nil {
		return err
	}
	streamCtx, cancel := s.streamContext(ctx)
	defer cancel()

	states := s.daemonTopic.Subscribe(streamCtx)
	events := s.daemonEventTopic.Subscribe(streamCtx)

	w := &daemonWatcher{sent: make(chan struct{}), elisp: make(chan *agentreplv1.WatchDaemonResponse, 4)}
	// THE STANDING LOUD FAULTS ARE AN EMACS STREAM'S ALONE: a webview draws
	// them on the topbar and footer views it already holds. A nil channel is
	// never ready, so a webview's select never takes that case.
	var faults <-chan *agentreplv1.DaemonFaultsStanding
	if emacs := msg.GetEmacs(); emacs != nil {
		w.emacs, w.elispBuild = true, emacs.GetElispBuild()
		faults = s.deps.LoudFaults.Subscribe(streamCtx)
		// EMACS'S FOCUS LIVES AND DIES WITH THIS STREAM: attached from the
		// request, so the daemon knows it from the stream's first instant,
		// and released when the stream ends, after which Emacs reads as
		// unfocused.
		release := s.deps.Focus.Attach(isFocused(emacs.GetFocus()))
		defer release()
	}
	// The reported build is read into the record BEFORE the watcher is
	// shared: once registered, a deploy's reload may move it under s.mu.
	reported := w.elispBuild
	s.addDaemonWatcher(w)
	// A DEPARTING STREAM IS A SATISFIED ONE: the announcer waits for delivery
	// or for the stream to be gone, never for a client that has stopped
	// listening.
	defer s.removeDaemonWatcher(w)
	s.acceptStream(ctx, "WatchDaemon")
	s.log.Debug("WatchDaemon", "accepted a standing stream", dlog.Context{
		"stream": w.id, "emacs": w.emacs, "elisp_build": reported,
	})

	for {
		var push *agentreplv1.WatchDaemonResponse
		var fromState bool
		select {
		case <-streamCtx.Done():
			s.log.Debug("WatchDaemon", "the standing stream ended on cancellation", nil)
			return nil
		case state, ok := <-states:
			if !ok {
				s.log.Debug("WatchDaemon", "the standing stream's subscription closed", nil)
				return nil
			}
			if state == nil {
				s.log.Error("WatchDaemon", "a publisher raised an empty view; it was not sent", nil)
				continue
			}
			push, fromState = state, true
		case event, ok := <-events:
			if !ok {
				s.log.Debug("WatchDaemon", "the standing stream's subscription closed", nil)
				return nil
			}
			if event == nil {
				s.log.Error("WatchDaemon", "a publisher raised an empty push; it was not sent", nil)
				continue
			}
			push, fromState = event, false
		case addressed := <-w.elisp:
			push, fromState = addressed, false
		case standing, ok := <-faults:
			if !ok {
				s.log.Debug("WatchDaemon", "the standing stream's subscription closed", nil)
				return nil
			}
			if standing == nil {
				s.log.Error("WatchDaemon", "a publisher raised an empty fault set; it was not sent", nil)
				continue
			}
			push = &agentreplv1.WatchDaemonResponse{
				Push: &agentreplv1.WatchDaemonResponse_FaultsStanding{FaultsStanding: standing},
			}
			fromState = false
		}
		if err := out.Send(push); err != nil {
			s.log.Debug("WatchDaemon", "the standing stream's client went away",
				dlog.Context{"cause": err.Error()})
			return nil
		}
		// The stand-down announcement rides the STATE topic; only it satisfies
		// the announcer's latch, so a mutation-progress event can never make a
		// departing daemon think its stand-down reached this client.
		if fromState && push.GetShutdownAnnounced() != nil {
			w.done()
		}
	}
}

// MutationProgress pushes one workspace-mutation progress event onto every
// WatchDaemon stream, keyed on the op_id it carries. It rides the event topic,
// never the state topic, so it is not replayed to a late subscriber in place of
// the standing drain banner.
func (s *server) MutationProgress(progress *agentreplv1.WorkspaceMutationProgress) {
	s.daemonEventTopic.Publish(&agentreplv1.WatchDaemonResponse{
		Push: &agentreplv1.WatchDaemonResponse_MutationProgress{MutationProgress: progress},
	})
}

// hostStreamHeld reports whether a WatchHostWorkspace stream stands for the
// workspace right now: the held-prompt edit's editor probe.
func (s *server) hostStreamHeld(ws ids.WorkspaceID) bool {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.hostHeld[ws] > 0
}

// holdParticipant records that one of a workspace's two per-workspace streams
// is held, which is the fact rollout's adoption rendezvous terminates on AND
// two of the three hops of connectivity truth (daemon.md invariant 11). Every
// open and close edge states the pair to the two resolvers that draw
// connectivity, so a hop going down is published rather than waited for.
func (s *server) holdParticipant(ws ids.WorkspaceID, host bool, delta int) {
	s.mu.Lock()
	if host {
		s.hostHeld[ws] += delta
	} else {
		s.webHeld[ws] += delta
	}
	hostLive, webLive := s.hostHeld[ws] > 0, s.webHeld[ws] > 0
	s.mu.Unlock()

	// THE EDITOR'S HOST STREAM IS THE HELD-PROMPT EDIT'S SCOPE. The last one
	// closing retires the edit as a cancel would; the queue decides that
	// under its delivery lock, so a begin that raced this close either saw no
	// stream or is retired here.
	if host && delta < 0 && !hostLive {
		s.deps.Queue.EditorGone(ws)
	}

	s.log.Debug("daemon.server.participants", "a per-workspace stream edge moved the participant set",
		dlog.Context{"workspace": string(ws), "host_stream": hostLive, "web_stream": webLive})
	s.deps.Footer.SetParticipants(ws, hostLive, webLive)
	s.deps.Topbar.SetParticipants(ws, hostLive, webLive)
}
