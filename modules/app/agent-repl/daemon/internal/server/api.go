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
	"strconv"
	"sync"
	"time"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/commandfile"
	"claude-repld/internal/deploy"
	"claude-repld/internal/desktopnotify"
	"claude-repld/internal/dlog"
	"claude-repld/internal/drain"
	"claude-repld/internal/feedid"
	"claude-repld/internal/health"
	"claude-repld/internal/ids"
	"claude-repld/internal/imageorigin"
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
	// Deploy backs the Deploy rpc: the daemon's own build-and-roll-out.
	Deploy Deployer
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
	// LoudFaults is the daemon's standing loud faults, the `faults_standing`
	// state every Emacs WatchDaemon stream subscribes to (a webview's never
	// does: its topbar and footer views carry the same faults).
	LoudFaults *publish.Topic[*agentreplv1.DaemonFaultsStanding]
	// Focus is Emacs's desktop focus: an Emacs WatchDaemon stream attaches it
	// for the stream's lifetime, and ReportEditorFocus moves it.
	Focus *desktopnotify.Focus

	// WebappDist is the webapp's dist directory, served on the same origin.
	// Its entry point is re-stat'd per request and answered with
	// Cache-Control: no-store; nothing else gets that header.
	WebappDist string
	// ImageOrigin serves the images a drawn feed refers to, mounted on
	// imageorigin.Route beneath the same origin as the webapp and the Connect
	// routes -- which is what lets a bubble's `<img src="/feed-images/...">`
	// load without a second host. It is REQUIRED like every other dependency:
	// a daemon that cannot draw an attached image is not one to start.
	ImageOrigin http.Handler
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
	// Clients are the connected clients' reported builds and the reload pushes
	// a deploy addresses to the stale ones (deploy.go).
	deploy.Clients

	// Relay is the host-push relay the workspace verbs use. It is a SEPARATE
	// value rather than an embedded interface because workspace.HostRelay's
	// OpenInEditor(ws, path, line) and the OpenInEditor RPC handler cannot both
	// be methods of one type; the relay wraps the same push topics.
	Relay() workspace.HostRelay

	// NotificationClicked pushes notification_clicked onto a workspace's host
	// stream. It is the desktop notifier's ClickSink.
	NotificationClicked(ws ids.WorkspaceID)

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

	// SeedFeedTextScale publishes the persisted feed text zoom onto the feed
	// watch topic. Prime calls it at boot, after the surface is bound and
	// before anything is served, so a feed opened before the first nudge shows
	// the zoom the user last chose. It is the feed-zoom sibling of the drain
	// controller's Republish.
	SeedFeedTextScale(scale float64)
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
	// closeOnce makes Close idempotent: the orderly exit ends the streams
	// through it while it still serves, and the teardown's deferred Close
	// then finds nothing left to do.
	closeOnce sync.Once

	// registry orders every registry read a request's RESOLUTION makes
	// against Close: a read holds it shared, Close takes it exclusively and
	// sets registryClosed before cancelling life. The daemon closes this
	// surface before it closes the state client, so a read either completes
	// before Close returns or is never made -- see readRegistry.
	registry       sync.RWMutex
	registryClosed bool

	mu sync.Mutex
	// hostTopics is one EVENT topic per workspace's WatchHostWorkspace stream.
	hostTopics map[ids.WorkspaceID]*publish.Topic[*agentreplv1.WatchHostWorkspaceResponse]
	// hostStateTopics is one STATE topic per workspace, carrying the `host`
	// arm. It is separate from the event topic because a Topic replays exactly
	// its latest value: sharing one would hand a late subscriber whichever
	// event happened last instead of the state it subscribed for.
	hostStateTopics map[ids.WorkspaceID]*publish.Topic[*agentreplv1.HostWorkspace]
	// webTopics is one EVENT topic per workspace's WatchWebWorkspace stream.
	webTopics map[ids.WorkspaceID]*publish.Topic[*agentreplv1.WatchWebWorkspaceResponse]
	// webStateTopics is one STATE topic per workspace, carrying the
	// `session_identity` arm. It is separate from the event topic for the same
	// reason hostStateTopics is separate from hostTopics: a Topic replays
	// exactly its latest value, so a subscriber arriving after `transferred`
	// would be handed the identity instead of the transfer notice, and never
	// learn the workspace had moved.
	webStateTopics map[ids.WorkspaceID]*publish.Topic[*agentreplv1.WebWorkspaceSessionIdentity]
	// daemonTopic is the one daemon-level push topic, for Emacs and every
	// webview alike. It carries STATE — the standing drain schedule and the
	// stand-down announcement — which publish.Topic replays to a late
	// subscriber so it draws the current banner the instant it attaches.
	daemonTopic publish.Topic[*agentreplv1.WatchDaemonResponse]
	// daemonEventTopic carries daemon-level EVENTS — workspace-mutation
	// progress — merged onto the same WatchDaemon wire. It is SEPARATE from
	// daemonTopic for the reason webTopics is separate from webStateTopics: a
	// Topic replays exactly its latest value, so an event sharing the state
	// topic would be replayed to a late subscriber IN PLACE OF the standing
	// drain banner, and a create's stale progress would masquerade as daemon
	// state. A late subscriber may still replay this topic's last event, which
	// carries an op_id it never issued and therefore ignores.
	daemonEventTopic publish.Topic[*agentreplv1.WatchDaemonResponse]
	// hostHeld and webHeld count the live holders of each workspace's two
	// streams, which is what ParticipantSource answers from.
	hostHeld map[ids.WorkspaceID]int
	webHeld  map[ids.WorkspaceID]int
	// daemonWatchers are the live WatchDaemon streams whose subscription
	// already exists. The stand-down announcement is flushed onto every one of
	// them BEFORE the orderly exit cancels serving, so a client learns the
	// daemon is standing down from the announcement rather than from the
	// socket going away underneath it.
	daemonWatchers map[*daemonWatcher]struct{}
	// daemonWatcherSeq names each WatchDaemon stream, so a deploy's elisp
	// reload is addressed to exactly the Emacs streams it judged stale.
	daemonWatcherSeq uint64
	// webBuilds is the webapp build every open web stream reported, per
	// workspace and per stream: what a deploy compares the fresh build to.
	webBuilds map[ids.WorkspaceID]map[string]string
	// webStreamSeq names each web stream's entry in webBuilds.
	webStreamSeq uint64
	// watchTokens memoizes which workspace and feed each minted watch token
	// addresses. WatchFeedRequest carries ONLY the token while the feed
	// resolver's Tail takes the workspace and feed explicitly, so the one mint
	// site — OpenFeed — records the pair here.
	watchTokens map[string]tokenTarget
	// pages are the attached browser pages, by the id each minted for itself.
	// A page holds ONE stream and multiplexes every standing subscription onto
	// it, because HTTP/1.1 caps a browser at about six connections per host and
	// a server-streaming call pins one for its whole life (page.go).
	pages map[string]*pageStream
	// hostIdentityAwaited records when a workspace's host view was FIRST
	// withheld because its session record carries no host identity yet. Right
	// after Emacs subscribes to WatchHostWorkspace, the shim may not have
	// described the session, so the identity is legitimately absent for a
	// moment: the first withholding per workspace is a STARTUP TRANSIENT logged
	// at DEBUG, and only a withholding that persists past
	// hostIdentityDescribeBound escalates to the ERROR that names a genuine
	// mint defect. A successful compose clears the entry.
	hostIdentityAwaited map[ids.WorkspaceID]time.Time
	// now is the clock, injected in tests so the transient-versus-defect
	// escalation can be exercised without waiting real seconds.
	now func() time.Time

	// selections is each workspace's feed selection (select_feed_row.go): a
	// selected response or prompt, ABSENT for none. The daemon is its only
	// holder: Emacs and the webapp move it (SelectFeedRow) and are pushed it,
	// and a sent prompt and a rollback read it here. Guarded by mu.
	selections map[ids.WorkspaceID]*frontendv1.FeedSelection
	// selectionTopics is one FeedSelection push topic per workspace, subscribed
	// by the ROOT feed's WatchFeed so a selection change reaches every open
	// webview. A publish.Topic replays its latest value, so a webview that
	// attaches mid-selection is handed the current selection at once.
	selectionTopics map[ids.WorkspaceID]*publish.Topic[*frontendv1.FeedSelection]

	// feedTextScaleMu guards the feed text zoom below. It is SEPARATE from mu so
	// an AdjustFeedTextScale that persists to the state store never blocks a
	// feed watch, a host push, or any other rpc that takes mu.
	feedTextScaleMu sync.Mutex
	// feedTextScale is the single daemon-global feed text zoom multiplier — one
	// value for every feed, not per-workspace, because "the feed text size" is
	// one setting. Seeded from the state store at boot (SeedFeedTextScale) and
	// moved by AdjustFeedTextScale, which persists each change. Guarded by
	// feedTextScaleMu.
	feedTextScale float64
	// feedTextScaleTopic is the one global FeedTextScale push topic. EVERY open
	// feed's watch subscribes to it — root and expanded sub-feeds alike, since
	// all of them draw feed text — so a zoom change reaches every open webview.
	// A publish.Topic replays its latest value, so a feed opened mid-session is
	// handed the current zoom the instant it attaches.
	feedTextScaleTopic publish.Topic[*frontendv1.FeedTextScale]
}

// hostIdentityDescribeBound is how long a session record may carry no host
// identity after WatchHostWorkspace subscribes before the missing identity
// stops being a startup transient and becomes a defect. It mirrors the boot
// adoption window within which the shim adopts a workspace and describes its
// session; past it, an unminted identity is the real defect the ERROR names.
const hostIdentityDescribeBound = 10 * time.Second

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
	case deps.Deploy == nil:
		return nil, missing("a deployer")
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
	case deps.LoudFaults == nil:
		return nil, missing("the standing loud faults")
	case deps.Focus == nil:
		return nil, missing("Emacs's focus")
	case deps.WebappDist == "":
		return nil, missing("the webapp dist directory")
	case deps.ImageOrigin == nil:
		return nil, missing("an image origin")
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
		deps:                deps,
		log:                 deps.Log.Global(),
		life:                life,
		cancel:              cancel,
		hostTopics:          make(map[ids.WorkspaceID]*publish.Topic[*agentreplv1.WatchHostWorkspaceResponse]),
		hostStateTopics:     make(map[ids.WorkspaceID]*publish.Topic[*agentreplv1.HostWorkspace]),
		webStateTopics:      make(map[ids.WorkspaceID]*publish.Topic[*agentreplv1.WebWorkspaceSessionIdentity]),
		webTopics:           make(map[ids.WorkspaceID]*publish.Topic[*agentreplv1.WatchWebWorkspaceResponse]),
		daemonWatchers:      make(map[*daemonWatcher]struct{}),
		webBuilds:           make(map[ids.WorkspaceID]map[string]string),
		hostHeld:            make(map[ids.WorkspaceID]int),
		webHeld:             make(map[ids.WorkspaceID]int),
		watchTokens:         make(map[string]tokenTarget),
		pages:               make(map[string]*pageStream),
		hostIdentityAwaited: make(map[ids.WorkspaceID]time.Time),
		now:                 time.Now,
		selections:          make(map[ids.WorkspaceID]*frontendv1.FeedSelection),
		selectionTopics:     make(map[ids.WorkspaceID]*publish.Topic[*frontendv1.FeedSelection]),
		// The zoom starts at the persistence default; Prime seeds the stored
		// value onto the topic before anything is served.
		feedTextScale: wsm.DefaultFeedTextScale,
	}

	mux := http.NewServeMux()
	path, handler := agentreplv1connect.NewAgentReplHandler(&requestLoggingServer{server: s})
	mux.Handle(path, handler)
	// The Connect routes and the image origin sit on their own longer
	// prefixes, so http.ServeMux gives them precedence over the asset origin
	// at "/".
	mux.Handle(imageorigin.Route, deps.ImageOrigin)
	mux.Handle("/", s.assets())
	s.mux = withAcceptWriter(mux)
	return s, nil
}

// ServeHTTP serves the Connect routes and the asset origin on one mux.
func (s *server) ServeHTTP(w http.ResponseWriter, r *http.Request) { s.mux.ServeHTTP(w, r) }

// Close ends every open stream by cancelling the lifetime they hang off.
func (s *server) Close() error {
	s.closeOnce.Do(func() {
		// THE REGISTRY GATE CLOSES FIRST: every resolution read already
		// admitted finishes, and none is admitted after, so no request of this
		// surface reads a state client the exit is about to close.
		s.registry.Lock()
		s.registryClosed = true
		s.registry.Unlock()
		s.cancel()
		s.log.Info("daemon.server.close", "every open stream was ended", nil)
	})
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

// webStateTopic answers a workspace's web STATE topic, minting it on first use
// exactly as webTopic mints the event one.
func (s *server) webStateTopic(
	ws ids.WorkspaceID,
) *publish.Topic[*agentreplv1.WebWorkspaceSessionIdentity] {
	s.mu.Lock()
	defer s.mu.Unlock()
	t, ok := s.webStateTopics[ws]
	if !ok {
		t = &publish.Topic[*agentreplv1.WebWorkspaceSessionIdentity]{}
		s.webStateTopics[ws] = t
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

// PublishHostWorkspace republishes the workspace's host state. The lifetime is
// the SERVER's, not any caller's: the edges that call it are async (a shim
// dying, a lease released) and carry no request context of their own.
func (r hostRelay) PublishHostWorkspace(ws ids.WorkspaceID) {
	r.s.PublishHostWorkspace(r.s.life, ws)
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

// NotificationClicked pushes notification_clicked onto the workspace's host
// stream: the user clicked the workspace's desktop banner, and Emacs raises its
// frame and selects the tab. It is the desktop notifier's ClickSink.
func (s *server) NotificationClicked(ws ids.WorkspaceID) {
	s.log.Info("daemon.server.notification_clicked", "relayed a desktop banner click to the host stream",
		dlog.Context{"workspace": string(ws)})
	s.hostTopic(ws).Publish(&agentreplv1.WatchHostWorkspaceResponse{
		Push: &agentreplv1.WatchHostWorkspaceResponse_NotificationClicked{
			NotificationClicked: &agentreplv1.HostWorkspaceNotificationClicked{},
		},
	})
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

// ShutdownAnnounced publishes the stand-down announcement AND BLOCKS until
// every WatchDaemon stream that was live when it was published has actually
// sent it, or has ended.
//
// The wait is the point. Publishing is asynchronous — publish.Topic hands each
// subscriber an unbounded queue drained by its own goroutine — and every
// caller of this method exits the process immediately afterwards by cancelling
// the serving lifetime. Without the wait, the cancellation races the delivery
// and the announcement is simply lost: the client sees its stream end with no
// error and no reason, which is exactly the "the daemon vanished" state the
// announcement exists to prevent.
func (s *server) ShutdownAnnounced(push *agentreplv1.DaemonShutdownAnnounced) {
	waiting := s.snapshotDaemonWatchers()
	s.daemonTopic.Publish(&agentreplv1.WatchDaemonResponse{
		Push: &agentreplv1.WatchDaemonResponse_ShutdownAnnounced{ShutdownAnnounced: push},
	})
	undelivered := s.awaitAnnouncement(waiting)
	if undelivered > 0 {
		// NOT SWALLOWED, and not waited on forever either: a client wedged
		// mid-write would otherwise hold the daemon's shutdown open
		// indefinitely, so the failure to reach it is recorded loudly and the
		// stand-down proceeds.
		s.log.Warn("daemon.server.shutdown_announced", "the stand-down announcement did not reach every client before the exit",
			dlog.Context{"undelivered": undelivered, "bound_ms": announcementFlush.Milliseconds()})
	}
	s.log.Info("daemon.server.shutdown_announced", "published the stand-down announcement",
		dlog.Context{"clients": len(waiting)})
}

// announcementFlush bounds the stand-down announcement's delivery wait. It is
// not a poll cadence and no healthy path ever rides it: a live stream takes
// the announcement in microseconds, and a stream that has gone away satisfies
// the wait at once. It exists purely so a wedged client cannot hold the
// orderly exit open forever.
const announcementFlush = 2 * time.Second

// daemonWatcher is one live WatchDaemon stream, as the announcer sees it: a
// latch closed the moment that stream has sent a stand-down announcement, or
// has ended without one.
type daemonWatcher struct {
	once sync.Once
	sent chan struct{}
	// id names the stream for an addressed push.
	id string
	// emacs reports that the client is Emacs, which states elispBuild.
	emacs      bool
	elispBuild string
	// elisp carries the pushes addressed to this stream alone.
	elisp chan *agentreplv1.WatchDaemonResponse
}

func (w *daemonWatcher) done() { w.once.Do(func() { close(w.sent) }) }

func (s *server) addDaemonWatcher(w *daemonWatcher) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.daemonWatcherSeq++
	w.id = "watch-daemon-" + strconv.FormatUint(s.daemonWatcherSeq, 10)
	s.daemonWatchers[w] = struct{}{}
}

func (s *server) removeDaemonWatcher(w *daemonWatcher) {
	s.mu.Lock()
	delete(s.daemonWatchers, w)
	s.mu.Unlock()
	w.done()
}

// snapshotDaemonWatchers lists the streams an announcement must reach. It is
// taken BEFORE the publish: a stream that opens afterwards subscribes to the
// topic's latest value and therefore receives the announcement anyway, and
// waiting on one that had not subscribed yet would be waiting on a stream the
// publish never reached.
func (s *server) snapshotDaemonWatchers() []*daemonWatcher {
	s.mu.Lock()
	defer s.mu.Unlock()
	out := make([]*daemonWatcher, 0, len(s.daemonWatchers))
	for w := range s.daemonWatchers {
		out = append(out, w)
	}
	return out
}

// awaitAnnouncement waits for every snapshotted stream to have sent the
// announcement or ended, and reports how many did neither inside the bound.
func (s *server) awaitAnnouncement(waiting []*daemonWatcher) int {
	if len(waiting) == 0 {
		return 0
	}
	// ONE DEADLINE FOR THE WHOLE SET, not one per stream: the bound is how
	// long the exit is willing to be held, and a per-stream timer would
	// multiply it by the number of clients.
	deadline, cancel := context.WithTimeout(context.Background(), announcementFlush)
	defer cancel()
	undelivered := 0
	for _, w := range waiting {
		select {
		case <-w.sent:
		case <-deadline.Done():
			undelivered++
		}
	}
	return undelivered
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
