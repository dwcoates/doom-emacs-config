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
	"net/http"

	"connectrpc.com/connect"
	"golang.org/x/net/http2"
	"golang.org/x/net/http2/h2c"

	"claude-repld/internal/commandfile"
	"claude-repld/internal/dlog"
	"claude-repld/internal/drain"
	"claude-repld/internal/health"
	"claude-repld/internal/ids"
	"claude-repld/internal/login"
	"claude-repld/internal/merge"
	"claude-repld/internal/notimpl"
	"claude-repld/internal/prompthandler"
	"claude-repld/internal/promptqueue"
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
	// Login backs the four login verbs; WatchLoginTerminal is a SERVER stream
	// and SendLoginInput is unary.
	Login login.Manager
	// Commands is the command-file ingress, wired here so the two ingresses
	// share one dependency graph.
	Commands commandfile.Ingress

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
	workspace.HostRelay

	// Daemon pushes a daemon-scoped fact onto every WatchDaemon stream — the
	// graceful rollout's stand-down announcement. It serves Emacs AND every
	// webview alike (R3).
	Daemon(push any)
	// Close ends every open stream.
	Close() error
}

// New builds the handler graph. The returned handler serves Connect over
// HTTP/1.1 and h2c on one origin, with the webapp assets beneath it.
func New(deps Deps) (Server, error) {
	return nil, notimpl.Err
}

// UnlandedArm is the standard refusal for a state whose typed error arm does
// not exist in the contract yet. It answers a Connect error —
// CodeFailedPrecondition for a state refusal, CodeNotFound for an unknown id —
// whose message is exactly "intended arm: <RpcName>Error.<arm_name>: <reason>",
// and logs the intended arm at WARN with operation
// "daemon.refusal.unlanded_arm". Every call site is recorded in
// daemon/ERROR-ARMS.md.
func UnlandedArm(log dlog.Logger, rpc, arm, reason string, notFound bool) *connect.Error {
	return connect.NewError(connect.CodeInternal, notimpl.Err)
}

// H2C wraps the Connect handler so ONE loopback listener serves both HTTP/1.1
// and cleartext HTTP/2. The daemon binds one listener and serves the rpcs and
// the webapp assets on one origin, which is what makes the webview URL and the
// Connect endpoint the same host.
func H2C(h http.Handler) http.Handler {
	return h2c.NewHandler(h, &http2.Server{})
}
