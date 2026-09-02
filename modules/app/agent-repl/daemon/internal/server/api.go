// Package server is the Connect surface: handlers, publisher wiring, the
// static asset origin, and the transport.
//
// One loopback listener serves Connect (HTTP/1.1 and h2c, binary and JSON) and
// the webapp assets on ONE origin. Handlers validate through the base
// functions and DELEGATE; they hold no policy. An rpc naming a workspace this
// daemon does not own is REFUSED rather than served. See ARCHITECTURE.md
// "server".
package server

import (
	"context"
	"fmt"
	"net/http"
	"sync"

	"connectrpc.com/connect"
	"golang.org/x/net/http2"
	"golang.org/x/net/http2/h2c"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"

	"claude-repld/internal/commandfile"
	"claude-repld/internal/dlog"
	"claude-repld/internal/drain"
	"claude-repld/internal/feedid"
	"claude-repld/internal/health"
	"claude-repld/internal/ids"
	"claude-repld/internal/login"
	"claude-repld/internal/merge"
	"claude-repld/internal/prompthandler"
	"claude-repld/internal/promptqueue"
	"claude-repld/internal/publish"
	"claude-repld/internal/resolve/feed"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/resolve/holds"
	"claude-repld/internal/resolve/sidebar"
	"claude-repld/internal/resolve/topbar"
	"claude-repld/internal/rollout"
	"claude-repld/internal/workspace"
	"claude-repld/internal/wsm"
)

// Deps names every dependency the handler graph needs. It is a struct rather
// than a constructor argument list so a wave-2 agent adding a handler adds a
// field here and nowhere else.
type Deps struct {
	// Instance is this daemon process's identity, for serving ownership.
	Instance ids.InstanceID
	// DB is the durable state, for the ownership check and the registry
	// answers.
	DB wsm.DB

	// Prompts is SubmitPrompt's body.
	Prompts prompthandler.Handler
	// Queue backs UpdateHeldPrompt's three arms.
	Queue promptqueue.Queue
	// Verbs backs every workspace, task, answer and link verb.
	Verbs workspace.Verbs
	// Merge backs MergeWorkspace, UpdateMergeQueue and AnswerHeldOffer.
	Merge merge.Orchestrator
	// Drain backs UpdateShutdownSchedule.
	Drain drain.Controller
	// Rollout backs AdoptHostWorkspace and AdoptWebWorkspace.
	Rollout rollout.Controller
	// Health backs DaemonHealth and SessionHealth.
	Health health.Reporter
	// SessionFacts answers the LIVE half of the host view — the session
	// identity, the controller generation, whether the shim is attached and
	// the transcript's backfill. Only the party that spawns and supervises
	// shims can know them, and the server must not reach into it for them.
	SessionFacts SessionFacts
	// Login backs the four login verbs; WatchLoginTerminal is a SERVER stream
	// and SendLoginInput is unary.
	Login login.Manager
	// Commands is the command-file ingress, wired here so the two ingresses
	// share one dependency graph.
	Commands commandfile.Ingress

	// Ownership answers a workspace's SERVING STANDING, which is what every
	// per-workspace rpc refuses on before it delegates.
	//
	// SEAM ADDITION (recorded in the report): workspace.Verbs consults its own
	// Ownership internally, but the rpcs that do NOT route through the verbs —
	// the feed pages, the merge and drain verbs, health, login and every
	// per-workspace stream — need the same truth, and the alternative was for
	// each of them to invent an ownership rule of its own.
	Ownership workspace.Ownership
	// SuccessorAddress answers the address a transferred workspace moved to,
	// which is `transferring_away{address}`'s only field. It answers the empty
	// string when no handover is in flight.
	//
	// SEAM ADDITION (recorded in the report): workspace.Ownership reports the
	// STANDING but carries no address, and the arm has nowhere else to get one.
	SuccessorAddress func() string

	// Feed backs OpenFeed, WatchFeed and GetFeedPage.
	Feed feed.Resolver
	// Footer backs WatchFooter.
	Footer footer.Resolver
	// Topbar backs WatchTopbar.
	Topbar topbar.Resolver
	// Sidebar backs WatchWorkspaceRoster, the one editor-global stream.
	Sidebar sidebar.Resolver
	// Holds backs WatchDaemonHolds.
	Holds holds.Resolver

	// WebappDist is the webapp's dist directory, served on the same origin.
	// Its entry point is re-stat'd per request and answered with
	// Cache-Control: no-store; nothing else gets that header.
	WebappDist string
	// Log is the server's logger.
	Log dlog.Surfaces
}

// Server is the running Connect surface plus the host-push relay the workspace
// verbs use.
type Server interface {
	http.Handler
	// WorkspacePusher is the rollout's per-workspace relay: `transferred` on
	// the host stream and `transferred{address}` on the web stream.
	rollout.WorkspacePusher
	// ParticipantSource answers which of a workspace's two per-workspace
	// streams are held, which is what terminates the adoption rendezvous.
	rollout.ParticipantSource
	// Announcer publishes the daemon-scoped pushes onto every WatchDaemon
	// stream. It also satisfies rollout.Announcer, whose one method it shares.
	drain.Announcer

	// Relay is the host-push relay the workspace verbs use. It is a SEPARATE
	// value rather than an embedded interface because workspace.HostRelay's
	// OpenInEditor(ws, path, line) and the OpenInEditor RPC handler cannot both
	// be methods of one type; the relay wraps the same push topics.
	Relay() workspace.HostRelay

	// PublishHostWorkspace recomposes and publishes one workspace's host view.
	// It is exported because the SESSION EDGES that move the view — a shim
	// dying, a merge parking, a lease released — happen inside the fleet and
	// the merge orchestrator, not inside an rpc the server handles. Those
	// parties call it; the topic's proto.Equal dedupe means a caller never has
	// to decide whether its edge actually changed anything.
	PublishHostWorkspace(ctx context.Context, ws ids.WorkspaceID)

	// Daemon pushes a daemon-scoped fact onto every WatchDaemon stream — the
	// graceful rollout's stand-down announcement. It serves Emacs AND every
	// webview alike (R3).
	Daemon(push any)
	// Close ends every open stream.
	Close() error
}

// server is the running surface. It owns the per-workspace push topics, the
// daemon-level one, the feed watch-token memo, and the lifetime every open
// stream hangs off.
type server struct {
	deps Deps
	log  dlog.Logger

	mux http.Handler

	// life is cancelled by Close, which is what ends every open stream.
	life   context.Context
	cancel context.CancelFunc

	mu sync.Mutex
	// hostTopics is one EVENT topic per workspace's WatchHostWorkspace stream.
	hostTopics map[ids.WorkspaceID]*publish.Topic[*agentreplv1.WatchHostWorkspaceResponse]
	// hostStateTopics is one STATE topic per workspace, carrying the `host`
	// arm. It is separate from the event topic because a Topic replays exactly
	// its latest value: sharing one would hand a late subscriber whichever
	// event happened last instead of the state it subscribed for.
	hostStateTopics map[ids.WorkspaceID]*publish.Topic[*agentreplv1.HostWorkspace]
	// webTopics is one push topic per workspace's WatchWebWorkspace stream.
	webTopics map[ids.WorkspaceID]*publish.Topic[*agentreplv1.WatchWebWorkspaceResponse]
	// daemonTopic is the one daemon-level push topic, for Emacs and every
	// webview alike.
	daemonTopic publish.Topic[*agentreplv1.WatchDaemonResponse]
	// hostHeld and webHeld count the live holders of each workspace's two
	// streams, which is what ParticipantSource answers from.
	hostHeld map[ids.WorkspaceID]int
	webHeld  map[ids.WorkspaceID]int
	// watchTokens memoizes which workspace and feed each minted watch token
	// addresses. WatchFeedRequest carries ONLY the token while the feed
	// resolver's Tail takes the workspace and feed explicitly, so the one mint
	// site — OpenFeed — records the pair here.
	watchTokens map[string]tokenTarget
}

// tokenTarget is the workspace and feed one minted watch token addresses.
type tokenTarget struct {
	WS   ids.WorkspaceID
	Feed feedid.Feed
}

// New builds the handler graph. The returned handler serves Connect over
// HTTP/1.1 and h2c on one origin, with the webapp assets beneath it.
func New(deps Deps) (Server, error) {
	missing := func(what string) error { return fmt.Errorf("server: %s is required", what) }
	switch {
	case deps.DB == nil:
		return nil, missing("a state client")
	case deps.Prompts == nil:
		return nil, missing("a prompt handler")
	case deps.Queue == nil:
		return nil, missing("a prompt queue")
	case deps.Verbs == nil:
		return nil, missing("the workspace verbs")
	case deps.Merge == nil:
		return nil, missing("a merge orchestrator")
	case deps.Drain == nil:
		return nil, missing("a drain controller")
	case deps.Rollout == nil:
		return nil, missing("a rollout controller")
	case deps.Health == nil:
		return nil, missing("a health reporter")
	case deps.Login == nil:
		return nil, missing("a login manager")
	case deps.Ownership == nil:
		return nil, missing("an ownership source")
	case deps.SuccessorAddress == nil:
		return nil, missing("a successor address source")
	case deps.Feed == nil:
		return nil, missing("a feed resolver")
	case deps.Footer == nil:
		return nil, missing("a footer resolver")
	case deps.Topbar == nil:
		return nil, missing("a topbar resolver")
	case deps.Sidebar == nil:
		return nil, missing("a sidebar resolver")
	case deps.Holds == nil:
		return nil, missing("a holds resolver")
	case deps.WebappDist == "":
		return nil, missing("the webapp dist directory")
	case deps.Log == nil:
		return nil, missing("log surfaces")
	case deps.SessionFacts == nil:
		// The host view's live half is REQUIRED, like every other Deps field:
		// without it no workspace with a session can be served a host view at
		// all, and a daemon that cannot do that is not one to start.
		return nil, missing("a session-facts source")
	}

	life, cancel := context.WithCancel(context.Background())
	s := &server{
		deps:            deps,
		log:             deps.Log.Global(),
		life:            life,
		cancel:          cancel,
		hostTopics:      make(map[ids.WorkspaceID]*publish.Topic[*agentreplv1.WatchHostWorkspaceResponse]),
		hostStateTopics: make(map[ids.WorkspaceID]*publish.Topic[*agentreplv1.HostWorkspace]),
		webTopics:       make(map[ids.WorkspaceID]*publish.Topic[*agentreplv1.WatchWebWorkspaceResponse]),
		hostHeld:        make(map[ids.WorkspaceID]int),
		webHeld:         make(map[ids.WorkspaceID]int),
		watchTokens:     make(map[string]tokenTarget),
	}

	mux := http.NewServeMux()
	path, handler := agentreplv1connect.NewAgentReplHandler(s)
	mux.Handle(path, handler)
	// The Connect routes sit on their own longer prefix, so http.ServeMux
	// gives them precedence over the asset origin at "/".
	mux.Handle("/", s.assets())
	s.mux = withAcceptWriter(mux)
	return s, nil
}

// ServeHTTP serves the Connect routes and the asset origin on one mux.
func (s *server) ServeHTTP(w http.ResponseWriter, r *http.Request) { s.mux.ServeHTTP(w, r) }

// Close ends every open stream by cancelling the lifetime they hang off.
func (s *server) Close() error {
	s.cancel()
	s.log.Debug("daemon.server.close", "every open stream was ended", nil)
	return nil
}

// hostTopic answers a workspace's host push topic, creating it on first use.
func (s *server) hostTopic(ws ids.WorkspaceID) *publish.Topic[*agentreplv1.WatchHostWorkspaceResponse] {
	s.mu.Lock()
	defer s.mu.Unlock()
	t, ok := s.hostTopics[ws]
	if !ok {
		t = &publish.Topic[*agentreplv1.WatchHostWorkspaceResponse]{}
		s.hostTopics[ws] = t
	}
	return t
}

// webTopic answers a workspace's web push topic, creating it on first use.
func (s *server) webTopic(ws ids.WorkspaceID) *publish.Topic[*agentreplv1.WatchWebWorkspaceResponse] {
	s.mu.Lock()
	defer s.mu.Unlock()
	t, ok := s.webTopics[ws]
	if !ok {
		t = &publish.Topic[*agentreplv1.WatchWebWorkspaceResponse]{}
		s.webTopics[ws] = t
	}
	return t
}

// hostRelay is the server's workspace.HostRelay face. It exists as its own
// type because the relay's OpenInEditor and the OpenInEditor RPC handler would
// otherwise be two methods of the same name on *server.
type hostRelay struct{ s *server }

// Relay answers the host-push relay the workspace verbs push through.
func (s *server) Relay() workspace.HostRelay { return hostRelay{s: s} }

// OpenInEditor pushes the open_in_editor arm onto the workspace's host stream.
func (r hostRelay) OpenInEditor(ws ids.WorkspaceID, path string, line *uint32) {
	r.s.pushOpenInEditor(ws, path, line)
}

// ReloadWebapp pushes the reload_webapp arm onto the workspace's host stream.
func (r hostRelay) ReloadWebapp(ws ids.WorkspaceID) { r.s.ReloadWebapp(ws) }

// Notify pushes a host notification onto the workspace's host stream.
// PublishHostWorkspace republishes the workspace's host state. The lifetime is
// the SERVER's, not any caller's: the edges that call it are async (a shim
// dying, a lease released) and carry no request context of their own.
func (r hostRelay) PublishHostWorkspace(ws ids.WorkspaceID) {
	r.s.PublishHostWorkspace(r.s.life, ws)
}

func (r hostRelay) Notify(ws ids.WorkspaceID, text, kind, toolName string) {
	r.s.notify(ws, text, kind, toolName)
}

// pushOpenInEditor publishes the open_in_editor arm.
func (s *server) pushOpenInEditor(ws ids.WorkspaceID, path string, line *uint32) {
	s.log.Debug("daemon.server.open_in_editor", "relayed a link click to the host stream",
		dlog.Context{"workspace": string(ws), "path": path})
	s.hostTopic(ws).Publish(&agentreplv1.WatchHostWorkspaceResponse{
		Push: &agentreplv1.WatchHostWorkspaceResponse_OpenInEditor{
			OpenInEditor: &agentreplv1.HostOpenInEditor{Path: path, Line: line},
		},
	})
}

// ReloadWebapp pushes the reload_webapp arm onto the workspace's host stream.
func (s *server) ReloadWebapp(ws ids.WorkspaceID) {
	s.log.Debug("daemon.server.reload_webapp", "asked the host to reload the webview",
		dlog.Context{"workspace": string(ws)})
	s.hostTopic(ws).Publish(&agentreplv1.WatchHostWorkspaceResponse{
		Push: &agentreplv1.WatchHostWorkspaceResponse_ReloadWebapp{
			ReloadWebapp: &agentreplv1.HostWorkspaceReloadWebapp{},
		},
	})
}

// PushReloadWebapp is the rollout's spelling of ReloadWebapp.
func (s *server) PushReloadWebapp(ws ids.WorkspaceID) { s.ReloadWebapp(ws) }

// notify publishes a host notification.
func (s *server) notify(ws ids.WorkspaceID, text string, kind string, toolName string) {
	s.log.Debug("daemon.server.notify", "relayed a host notification",
		dlog.Context{"workspace": string(ws), "kind": kind})
	s.hostTopic(ws).Publish(&agentreplv1.WatchHostWorkspaceResponse{
		Push: &agentreplv1.WatchHostWorkspaceResponse_Notification{
			Notification: &agentreplv1.HostWorkspaceNotification{
				Text: text,
				Kind: s.notificationKind(kind, toolName),
			},
		},
	})
}

// notificationKind renders the verbs' notification kind name as the typed arm.
// An unrecognized name is never guessed at silently: it raises agent_addressed
// and is logged at ERROR, because a dropped notification is a lost one.
func (s *server) notificationKind(kind, toolName string) *agentreplv1.HostNotificationKind {
	switch kind {
	case "permission_requested":
		return &agentreplv1.HostNotificationKind{
			Kind: &agentreplv1.HostNotificationKind_PermissionRequested{
				PermissionRequested: &agentreplv1.HostNotificationPermissionRequested{ToolName: toolName},
			},
		}
	case "question_asked":
		return &agentreplv1.HostNotificationKind{
			Kind: &agentreplv1.HostNotificationKind_QuestionAsked{
				QuestionAsked: &agentreplv1.HostNotificationQuestionAsked{Header: toolName},
			},
		}
	case "agent_addressed":
	default:
		s.log.Error("daemon.server.notify", "a host notification named an unknown kind",
			dlog.Context{"kind": kind})
	}
	return &agentreplv1.HostNotificationKind{
		Kind: &agentreplv1.HostNotificationKind_AgentAddressed{
			AgentAddressed: &agentreplv1.HostNotificationAgentAddressed{},
		},
	}
}

// PushTransferred pushes `transferred` on the host stream and
// `transferred{address}` on the web stream.
func (s *server) PushTransferred(ws ids.WorkspaceID, successorAddress string) {
	s.log.Info("daemon.server.transferred", "announced the workspace's transfer",
		dlog.Context{"workspace": string(ws), "successor": successorAddress})
	s.hostTopic(ws).Publish(&agentreplv1.WatchHostWorkspaceResponse{
		Push: &agentreplv1.WatchHostWorkspaceResponse_Transferred{
			Transferred: &agentreplv1.HostWorkspaceTransferred{},
		},
	})
	s.webTopic(ws).Publish(&agentreplv1.WatchWebWorkspaceResponse{
		Push: &agentreplv1.WatchWebWorkspaceResponse_Transferred{
			Transferred: &agentreplv1.WebWorkspaceTransferred{Address: successorAddress},
		},
	})
}

// Participants reports which of a workspace's two per-workspace streams are
// held right now.
func (s *server) Participants(ws ids.WorkspaceID) rollout.Participants {
	s.mu.Lock()
	defer s.mu.Unlock()
	return rollout.Participants{Host: s.hostHeld[ws] > 0, Web: s.webHeld[ws] > 0}
}

// Daemon pushes a daemon-scoped fact onto every WatchDaemon stream. The push is
// typed `any` at the seam because three producers raise it; an unrecognized
// value is an invariant violation and is raised loudly rather than dropped.
func (s *server) Daemon(push any) {
	switch p := push.(type) {
	case *agentreplv1.WatchDaemonResponse:
		s.daemonTopic.Publish(p)
	case *agentreplv1.DaemonShutdownAnnounced:
		s.ShutdownAnnounced(p)
	case *agentreplv1.DaemonDrainScheduled:
		s.DrainScheduled(p)
	case *agentreplv1.DaemonDrainCancelled:
		s.DrainCancelled(p)
	default:
		s.log.Error("daemon.server.daemon_push", "a daemon push of an unknown shape was raised",
			dlog.Context{"type": fmt.Sprintf("%T", push)})
	}
}

// DrainScheduled publishes the standing drain schedule.
func (s *server) DrainScheduled(push *agentreplv1.DaemonDrainScheduled) {
	s.log.Debug("daemon.server.drain_scheduled", "published the drain schedule", nil)
	s.daemonTopic.Publish(&agentreplv1.WatchDaemonResponse{
		Push: &agentreplv1.WatchDaemonResponse_DrainScheduled{DrainScheduled: push},
	})
}

// DrainCancelled publishes the schedule's cancellation.
func (s *server) DrainCancelled(push *agentreplv1.DaemonDrainCancelled) {
	s.log.Debug("daemon.server.drain_cancelled", "published the drain cancellation", nil)
	s.daemonTopic.Publish(&agentreplv1.WatchDaemonResponse{
		Push: &agentreplv1.WatchDaemonResponse_DrainCancelled{DrainCancelled: push},
	})
}

// ShutdownAnnounced publishes the stand-down announcement.
func (s *server) ShutdownAnnounced(push *agentreplv1.DaemonShutdownAnnounced) {
	s.log.Info("daemon.server.shutdown_announced", "published the stand-down announcement", nil)
	s.daemonTopic.Publish(&agentreplv1.WatchDaemonResponse{
		Push: &agentreplv1.WatchDaemonResponse_ShutdownAnnounced{ShutdownAnnounced: push},
	})
}

// UnlandedArm is the standard refusal for a state whose typed error arm does
// not exist in the contract yet. It answers a Connect error —
// CodeFailedPrecondition for a state refusal, CodeNotFound for an unknown id —
// whose message is exactly "intended arm: <RpcName>Error.<arm_name>: <reason>",
// and logs the intended arm at WARN with operation
// "daemon.refusal.unlanded_arm". Every call site is recorded in
// daemon/ERROR-ARMS.md.
func UnlandedArm(log dlog.Logger, rpc, arm, reason string, notFound bool) *connect.Error {
	message := fmt.Sprintf("intended arm: %sError.%s: %s", rpc, arm, reason)
	code := connect.CodeFailedPrecondition
	if notFound {
		code = connect.CodeNotFound
	}
	if log != nil {
		log.Warn(opUnlandedArm, message, dlog.Context{
			"rpc": rpc, "arm": arm, "reason": reason, "not_found": notFound,
		})
	}
	return connect.NewError(code, fmt.Errorf("%s", message))
}

// opUnlandedArm is the operation every unlanded-arm refusal is logged under, so
// the ledger in daemon/ERROR-ARMS.md reconciles against the log.
const opUnlandedArm = "daemon.refusal.unlanded_arm"

// TransportClosed answers a refused open of a STANDING STREAM. A Watch* rpc has
// no `<Rpc>Error` message at all: the refusal IS the closed transport, which is
// the settled shape rather than a gap, so it is NOT an unlanded arm. It answers
// a Connect error — CodeNotFound for an unknown id, CodeFailedPrecondition
// otherwise — whose message names the cause without the "intended arm:"
// spelling, and records the refusal at INFO under
// "daemon.refusal.transport_closed" with structured `rpc` and `cause`.
func TransportClosed(log dlog.Logger, rpc, cause, reason string, notFound bool) *connect.Error {
	message := fmt.Sprintf("%s closed the stream: %s: %s", rpc, cause, reason)
	code := connect.CodeFailedPrecondition
	if notFound {
		code = connect.CodeNotFound
	}
	if log != nil {
		log.Info(opTransportClosed, message, dlog.Context{
			"rpc": rpc, "cause": cause, "reason": reason, "not_found": notFound,
		})
	}
	return connect.NewError(code, fmt.Errorf("%s", message))
}

// opTransportClosed is the operation every refused stream open is recorded
// under. ERROR-ARMS.md's transport-closed section reconciles against it.
const opTransportClosed = "daemon.refusal.transport_closed"

// H2C wraps the Connect handler so ONE loopback listener serves both HTTP/1.1
// and cleartext HTTP/2. The daemon binds one listener and serves the rpcs and
// the webapp assets on one origin, which is what makes the webview URL and the
// Connect endpoint the same host.
func H2C(h http.Handler) http.Handler {
	return h2c.NewHandler(h, &http2.Server{})
}
