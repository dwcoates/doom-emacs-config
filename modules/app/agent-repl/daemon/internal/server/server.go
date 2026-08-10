// Package server exposes the daemon's HTTP surface: session CRUD plus the
// per-session WebSocket stream endpoint (/sessions/{id}/stream).
//
// After the agent-shim consumption cutover the daemon no longer owns a
// Layer-2 stdio streaming hub. Each session's UDS shim is consumed through
// the per-session controller (internal/sessioncontroller) and rendered onto the
// frontend.v1 surface (internal/frontend) plus the session-state manager
// (internal/ssm). The registry is the source of truth for which sessions
// exist; the session controller owns the live shims. Several routes here exist ONLY to
// keep the still-Layer-2 Emacs client working until it finishes its own
// full-UDS migration; those are marked SUPERSEDED (S7).
package server

import (
	"context"
	"crypto/rand"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"time"

	"github.com/gorilla/websocket"
	"google.golang.org/protobuf/encoding/protojson"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/account"
	"claude-repld/internal/addsupport"
	"claude-repld/internal/dlog"
	"claude-repld/internal/errclass"
	"claude-repld/internal/externalbrowser"
	"claude-repld/internal/frontend"
	"claude-repld/internal/keepalive"
	"claude-repld/internal/login"
	"claude-repld/internal/protocol"
	"claude-repld/internal/registry"
	"claude-repld/internal/session"
	"claude-repld/internal/sessioncontroller"
	"claude-repld/internal/ssm"
	"claude-repld/internal/workspacecmd"
)

// CreateOpts is the POST /sessions request body.
type CreateOpts struct {
	CWD            string `json:"cwd,omitempty"`
	Model          string `json:"model,omitempty"`
	PermissionMode string `json:"permission_mode,omitempty"`
	Resume         string `json:"resume,omitempty"`
	Fake           bool   `json:"fake,omitempty"`
	// ConfigDir is the CLAUDE_CONFIG_DIR the session's CLI runs under —
	// i.e. WHICH ACCOUNT it uses. Emacs computes it per workspace
	// (agent-repl--compute-config-dir: ~/.claude-chesscom under
	// $MULTI_REPO_ROOT, ~/.claude elsewhere) and sends it here, because
	// one shared daemon serves every workspace and its own environment
	// therefore cannot encode a per-workspace account.
	//
	// Empty means "the daemon's own default", NOT "no config dir".
	ConfigDir string `json:"config_dir,omitempty"`
	// AllowUngated is the caller's DELIBERATE consent to create a session
	// with no permission gate (protocol.UngatedPermissionMode). CreateSession
	// refuses such a create without it; see the CreateSessionCmd.allow_ungated
	// field comment for why the consent is create-time only.
	AllowUngated bool `json:"allow_ungated,omitempty"`
	// The REWIND LINEAGE, carried only on the one spawn that follows a
	// transcript rewind and never persisted. It tells the shim that the
	// conversation it is resuming is a truncated copy, which vendor session it
	// was truncated from, and which turns went — facts the shim cannot derive,
	// because it never sees a transcript path and cannot rebuild its own query.
	//
	// The three travel together or not at all: a lineage naming a predecessor
	// with no dropped turns describes a rewind that discarded nothing, which is
	// not a rewind. The shim rejects an empty dropped list outright, so the
	// argv renderer refuses to emit a partial set rather than producing one the
	// shim will reject on startup.
	// RewindDroppedTurns is the comma-separated turn_id list, in submission
	// order. It is a STRING rather than a slice deliberately: CreateOpts is
	// compared with == on the create-establish path, so a slice field would
	// make the whole struct incomparable — and the comma-separated form is
	// exactly what the argv carries, so nothing is lost by holding it that way.
	RewoundFrom        string `json:"rewound_from,omitempty"`
	RewindRetainedLeaf string `json:"rewind_retained_leaf,omitempty"`
	RewindDroppedTurns string `json:"rewind_dropped_turns,omitempty"`
	// ConfigDirOverride carries an account SELECTION into the create — the one
	// a human made in the webapp, inherited by a workspace created from one
	// that carries it. Empty means no selection, and then the account is
	// resolved from this workspace's own prior selection or from its path
	// (AccountResolver). It is persisted onto the new record so the selection
	// keeps travelling to that workspace's own children.
	ConfigDirOverride string `json:"config_dir_override,omitempty"`
	// ResumeDaemonResolved marks a Resume the DAEMON chose from its own records
	// (RESUME_MODE_CONTINUE) rather than one a caller NAMED.
	//
	// The two are different promises and the resume ladder must not treat them
	// alike. A caller that names a uuid has made a continuity commitment: if
	// that conversation is unavailable, the honest answer is to fail, because
	// anything else lands the caller somewhere it did not ask for. A uuid the
	// daemon resolved carries no such commitment — the caller asked for "this
	// workspace's conversation", and when the only candidate turns out to be a
	// bring-up handshake nothing was ever said in, continuing with none is the
	// answer to the question actually asked.
	//
	// NOT PERSISTED and not part of the session's identity: it describes how
	// THIS create was phrased, and CreateOpts is compared with == on the
	// establish path, so it must stay a comparable scalar.
	ResumeDaemonResolved bool `json:"resume_daemon_resolved,omitempty"`
}

// RewindLineageComplete reports whether the three rewind fields describe a real
// rewind. It is the ONE definition of completeness, shared by the argv renderer
// and by the arming path, so the two cannot disagree about what a usable
// lineage is.
func (o CreateOpts) RewindLineageComplete() bool {
	return o.RewoundFrom != "" && o.RewindRetainedLeaf != "" && o.RewindDroppedTurns != ""
}

// ShimArgv assembles the node argv that launches the shim for one
// session. forceFake forces --fake regardless of opts.Fake (the
// daemon-wide -fake flag). Shared by cmd/claude-repld and the e2e
// harness so the two spawn paths cannot drift.
func ShimArgv(node, script, sessionID string, forceFake bool, opts CreateOpts) []string {
	argv := []string{node, script, "--session-id", sessionID}
	if forceFake || opts.Fake {
		argv = append(argv, "--fake")
	}
	if opts.PermissionMode != "" {
		argv = append(argv, "--permission-mode", opts.PermissionMode)
	}
	if opts.CWD != "" {
		argv = append(argv, "--cwd", opts.CWD)
	}
	if model := registry.NormalizeModel(opts.Model); model != "" {
		argv = append(argv, "--model", model)
	}
	if opts.Resume != "" {
		argv = append(argv, "--resume", opts.Resume)
	}
	// THE REWIND LINEAGE ARGV IS A FROZEN CONTRACT with the shim, which reads
	// these three flags and emits the durable SessionRewound from them. It is
	// emitted ALL OR NOTHING: the shim rejects an empty dropped-turn list, so a
	// partial set rendered here would be a spawn that fails at startup instead
	// of a rewind that merely went unrecorded.
	if opts.RewindLineageComplete() {
		argv = append(argv,
			"--rewound-from", opts.RewoundFrom,
			"--rewind-retained-leaf", opts.RewindRetainedLeaf,
			"--rewind-dropped-turns", opts.RewindDroppedTurns)
	}
	return argv
}

// ShimEnv assembles the KEY=VALUE overlay every shim spawn adds to the
// inherited environment. The SDK's claude subprocess inherits it from
// the shim, so this is the ONLY channel by which per-session account
// selection reaches the CLI. Shared by cmd/claude-repld and the e2e
// harness for the same reason ShimArgv is.
//
// AGENT_REPL_OWNED marks the CLI as module-launched for the hook scripts.
// CLAUDE_CONFIG_DIR is set only when the session carries one; an empty
// ConfigDir deliberately leaves the daemon's own value (or its absence)
// inherited rather than exporting an empty override, which the CLI would
// read as a config root literally named "".
// AGENT_REPL_DAEMON_ADDR carries THIS daemon's -addr down to the CLI so a
// session's tools can reach the daemon's HTTP surface; empty leaves it unset.
func ShimEnv(opts CreateOpts, daemonAddr string) []string {
	env := []string{"AGENT_REPL_OWNED=1"}
	if opts.ConfigDir != "" {
		env = append(env, "CLAUDE_CONFIG_DIR="+opts.ConfigDir)
	}
	if daemonAddr != "" {
		env = append(env, "AGENT_REPL_DAEMON_ADDR="+daemonAddr)
	}
	return env
}

// SessionTokenUsageSource reads normalized live responses whose terminal turn
// accounting committed and historical file-plane responses that cannot prove
// a root turn or stream timing. The server aggregates that durable ledger into
// every SessionView rather than retaining a second cumulative counter.
type SessionTokenUsageSource interface {
	List(sessionID string) ([]*frontendv1.TokenUtilization, error)
}

// Server routes daemon HTTP traffic.
type Server struct {
	daemonVersion string
	bootID        string
	// binaryMTime is the Unix mtime (seconds) of the executable this daemon
	// was LAUNCHED from, captured at boot and served on GET /sessions. Emacs
	// compares it against the on-disk binary's mtime to tell a running daemon
	// apart from a freshly rebuilt binary. Zero means the boot-time stat failed.
	binaryMTime int64
	forceFake   bool
	logf        func(format string, args ...any)
	now         func() time.Time
	upgrader    websocket.Upgrader
	// widgetAssetsDir is the embeddable-widget dist the daemon serves at
	// /widget-assets/ (empty = the capability is off); GET /capabilities
	// reports it so a client detects the capability authoritatively.
	widgetAssetsDir string
	// daemonAddr is this daemon's own listen address (-addr), exported to
	// every session as AGENT_REPL_DAEMON_ADDR.
	daemonAddr string

	// controller consumes each session's UDS shim and backs prompt/interrupt/
	// permission plus /status, /commands, and /tasks introspection.
	controller *sessioncontroller.Manager
	// ssm resolves per-workspace render state (turn-active, live tasks) the
	// list and idle-sweep read.
	ssm *ssm.Manager
	// frontend fans frontend.v1 frames to the per-session /stream WebSocket.
	frontend      *frontend.Server
	registry      *registry.Registry
	modelCatalogs *SessionModelCatalogs
	tokenUsage    SessionTokenUsageSource
	// workspaceViews is the resolved-view publisher; see Config.WorkspaceViews.
	workspaceViews *WorkspaceViews
	// logins owns the interactive Claude login terminals, at most one per
	// account; nil makes the login routes report the capability unconfigured.
	logins *login.Manager
	// accounts is the canonical account roster; empty makes GET /accounts
	// report the capability unconfigured.
	accounts []Account

	// idleTimeout is how long a session may go without a turn before the
	// sweeper hibernates its shim (frees the ~500MB node+CLI pair). Zero
	// disables hibernation.
	idleTimeout time.Duration
	// idleSweepTicks drives the sweeper; tests inject a channel so the sweep
	// runs on demand rather than on a wall clock.
	idleSweepTicks <-chan time.Time
	// keepAlive is the cache keep-alive policy the sweeper evaluates on every
	// tick, beside its own idle gate. It rides the SWEEPER rather than a timer
	// of its own because the sweeper is already the daemon's only hibernation
	// initiator and already computes wall-clock-now minus a durable timestamp;
	// a second scheduler would be a second answer to "how long has this
	// workspace been quiet".
	keepAlive keepalive.Config
	// stopped is closed by ShutdownAll, ending the sweeper goroutine.
	stopped     chan struct{}
	sweeperDone chan struct{}
	idleSweep   func()
	stopOnce    sync.Once

	// viewsMu and viewsClosed own the SessionView push's access to DAEMON-OWNED
	// DURABLE STATE, on exactly the reasoning that already ends the idle
	// sweeper's lifetime inside ShutdownAll.
	//
	// A push is not daemon-initiated: it is driven by whatever the SHIMS say —
	// a model catalog, a persisted accounting, a backfill verdict — through the
	// registrar's PushView hook, from goroutines the server does not own and
	// cannot join. Those goroutines outlive the request that started them, so
	// during teardown one of them would reach the token ledger after its owner
	// had closed the database, and the read's own no-fallback contract turned
	// that into a panic ("sql: database is closed").
	//
	// The read lock is what makes the ordering structural rather than likely:
	// ShutdownAll takes the WRITE lock, so it cannot return while a push is
	// mid-read, and no push begun afterwards can pass the closed flag. Once
	// ShutdownAll returns, closing the stores is safe by construction.
	viewsMu     sync.RWMutex
	viewsClosed bool

	// openExternalURL backs POST /open-external: it hands a hyperlink to the
	// pinned external browser profile. Always non-nil (New defaults it).
	openExternalURL func(url string) error

	mu sync.Mutex
}

// Config assembles a Server.
type Config struct {
	DaemonVersion string
	// BinaryMTime is the Unix mtime (seconds) of the executable the daemon
	// was launched from, stat'd once at boot. Zero disables staleness
	// reporting: GET /sessions still carries the field.
	BinaryMTime int64
	Logf        func(format string, args ...any)
	// Now is the clock used to stamp registry records (defaults to
	// time.Now); injected for tests.
	Now func() time.Time
	// ForceFake mirrors the daemon-wide -fake flag: every session runs the
	// offline scripted SDK. The resume viability gate skips fake sessions.
	ForceFake bool
	// Controller consumes each session's UDS shim (prompt/interrupt/permission,
	// plus /status, /commands, /tasks introspection). Required in production.
	Controller *sessioncontroller.Manager
	// AgentShim is the frontend surface this server publishes through, WHOLE.
	//
	// IT IS ONE FIELD BECAUSE IT IS ONE THING. The state machine, the frame
	// fan-out and the resolved-view publisher are built together by
	// WireAgentShim and are only meaningful together: the topbar the publisher
	// resolves is fenced off the state machine's state and delivered by the
	// fan-out. They used to be three fields — SSM, Frontend, WorkspaceViews —
	// which meant a caller could hand over two and forget the third, and every
	// harness in the tree did exactly that: server.New got Frontend and SSM but
	// no WorkspaceViews, so PublishTokenBreakdown's only call site sat behind a
	// guard that could never open and the breakdown menu could never arrive.
	// Nothing said so, because a nil publisher published nothing.
	//
	// Taking the shim itself makes the omission unrepresentable rather than
	// merely refused: there is no way to supply the fan-out without also
	// supplying the publisher that pushes through it.
	//
	// Nil is a server with no frontend surface at all — a focused harness
	// testing the HTTP routes alone — which is a coherent whole rather than a
	// half-wired one.
	AgentShim *AgentShim
	// Registry persists session records across daemon restarts. Required: it
	// is the source of truth for which sessions exist.
	Registry      *registry.Registry
	ModelCatalogs *SessionModelCatalogs
	TokenUsage    SessionTokenUsageSource
	// Logins owns the interactive Claude login terminals; nil disables the
	// login routes.
	Logins *login.Manager
	// WidgetAssetsDir is the embeddable-widget dist served at /widget-assets/
	// (the -widget-assets flag); empty means the chess-widget capability is
	// off. Surfaced on GET /capabilities.
	WidgetAssetsDir string
	// DaemonAddr is the daemon's own listen address (-addr), exported to each
	// session as AGENT_REPL_DAEMON_ADDR.
	DaemonAddr string
	// Accounts is the canonical account roster (the -accounts flag). Empty
	// disables GET /accounts.
	Accounts []Account
	// KeepAlive is the cache keep-alive policy. The zero value takes
	// keepalive.DefaultConfig: a zero TTL would read every session as already
	// cache-expired and hibernate the whole fleet on the first tick.
	KeepAlive keepalive.Config
	// IdleTimeout is how long a session may go without a turn before the
	// sweeper hibernates its shim. Zero disables hibernation.
	IdleTimeout time.Duration
	// IdleSweepTicks overrides the sweeper's clock. Nil mints a real ticker;
	// tests inject a channel so a sweep runs on demand.
	IdleSweepTicks <-chan time.Time
	// OpenExternalURL hands a hyperlink to the external browser for POST
	// /open-external. Nil takes the production opener (externalbrowser);
	// tests inject one so a route test cannot launch a browser.
	OpenExternalURL func(url string) error
}

// Account is one canonical config root the daemon can name and switch
// sessions onto. The set is closed by configuration (the -accounts flag),
// never discovered.
type Account struct {
	// Label is the human name for the root ("personal", "work").
	Label string `json:"label"`
	// ConfigDir is the CLAUDE_CONFIG_DIR selecting the root, "" meaning the
	// CLI's own default (~/.claude).
	ConfigDir string `json:"config_dir"`
}

// New builds a Server.
func New(cfg Config) *Server {
	if cfg.Logf == nil {
		panic("server: Config.Logf is required")
	}
	if cfg.ModelCatalogs == nil {
		panic("server: Config.ModelCatalogs is required")
	}
	logf := cfg.Logf
	now := cfg.Now
	if now == nil {
		now = time.Now
	}
	openExternalURL := cfg.OpenExternalURL
	if openExternalURL == nil {
		openExternalURL = func(url string) error {
			return externalbrowser.Open(url, externalbrowser.Exec)
		}
	}
	s := &Server{
		daemonVersion:   cfg.DaemonVersion,
		bootID:          newBootID(),
		binaryMTime:     cfg.BinaryMTime,
		forceFake:       cfg.ForceFake,
		logf:            logf,
		now:             now,
		widgetAssetsDir: cfg.WidgetAssetsDir,
		daemonAddr:      cfg.DaemonAddr,
		controller:      cfg.Controller,
		logins:          cfg.Logins,
		accounts:        cfg.Accounts,
		registry:        cfg.Registry,
		modelCatalogs:   cfg.ModelCatalogs,
		tokenUsage:      cfg.TokenUsage,
		idleTimeout:     cfg.IdleTimeout,
		idleSweepTicks:  cfg.IdleSweepTicks,
		keepAlive:       cfg.KeepAlive,
		openExternalURL: openExternalURL,
		stopped:         make(chan struct{}),
		sweeperDone:     make(chan struct{}),
		upgrader: websocket.Upgrader{
			// The daemon is a local-loopback developer tool; the Emacs
			// xwidget origin is file-/app-scoped, so origin checks are
			// permissive by design.
			CheckOrigin: func(*http.Request) bool { return true },
		},
	}
	// THE THREE HALVES OF THE FRONTEND SURFACE ARE TAKEN TOGETHER OR NOT AT
	// ALL. Deriving them here rather than accepting them separately is what
	// makes "the publisher is wired whenever the fan-out is" true by
	// construction: there is no assignment a caller can omit.
	if cfg.AgentShim != nil {
		s.ssm = cfg.AgentShim.SSM
		s.frontend = cfg.AgentShim.Server
		s.workspaceViews = cfg.AgentShim.WorkspaceViews
	}
	s.idleSweep = s.sweepIdle
	if s.idleTimeout > 0 || s.idleSweepTicks != nil {
		go func() {
			defer close(s.sweeperDone)
			s.runIdleSweeper()
		}()
	} else {
		close(s.sweeperDone)
	}
	return s
}

// updateRegistry applies fn to id's registry record. Only sessions the
// create path registered carry a record, so a missing one is unexpected
// and logged, never silently dropped.
func (s *Server) updateRegistry(id, what string, fn func(*registry.Record)) {
	found, err := s.registry.Update(id, fn)
	if err != nil {
		s.logf("session %s: registry write (%s) FAILED — the session may not survive a daemon restart: %v", id, what, err)
		return
	}
	if !found {
		s.logf("session %s: registry write (%s) found no record — the session was never registered", id, what)
	}
}

// Handler returns the daemon's HTTP mux. Routes the webapp consumes over
// HTTP/WS are unmarked; routes only the still-Layer-2 Emacs client drives
// are marked SUPERSEDED (S7): they die when elisp completes its full-UDS
// migration.
func (s *Server) Handler() http.Handler {
	mux := http.NewServeMux()
	for _, rt := range s.routes() {
		mux.HandleFunc(rt.pattern, rt.handler)
	}
	return mux
}

// route is one registered HTTP pattern. Handler() registers the table, and
// TestEveryRouteIsCoveredByAnAPIPrefix walks it to prove every pattern is
// actually reachable through APIPrefixes.
type route struct {
	pattern string
	handler http.HandlerFunc
}

// APIPrefixes are the URL prefixes the daemon's API handler must be mounted at
// on the process-level mux. They matter because the webapp SPA is mounted at
// "/": a route whose prefix is MISSING here does not 404 from this mux at all —
// it silently falls through to the static file server, which answers the
// frontend's API call with a 404 page. That is exactly how POST
// /workspace-command was unreachable while a stale "/workspaces/" prefix (whose
// route had long since been deleted) sat in its place.
//
// This list is the single source of truth: cmd/claude-repld mounts from it, and
// the routes() table is asserted against it.
var APIPrefixes = []string{
	"/sessions",
	"/sessions/",
	"/accounts",
	"/capabilities",
	"/workspace-command",
	"/workspace-stream",
	"/open-external",
}

// routes is the daemon's full HTTP surface.
//
// Deleted routes and where their capability went, so the absences stay legible:
//   - POST /sessions (D-phase): both frontends create over the createSession
//     FrontendCommand, correlating the new id off the pushed SessionView. The
//     create CORE (s.CreateSession) is untouched.
//   - DELETE /sessions/{id}, POST /sessions/{id}/message, POST
//     /sessions/{id}/interrupt (S7): driven as frontend.v1 commands
//     (deleteSession/submitPrompt/interrupt). Their cores survive.
//   - GET/POST /sessions/{id}/commands{,/refresh} and .../status{,/refresh}
//     (D-phase): both frontends re-source the SDK system:init from the pushed
//     SessionInitView frame; the refresh halves were already loud no-ops
//     because the UDS shim has no re-init control.
//   - POST /sessions/{id}/queue/{queueId}/{run-now,cancel} (S9): the
//     daemon-owned queue plane is dead server-side.
//   - POST /shutdown (D-phase): Emacs bounces an adopted daemon over the
//     shutdown FrontendCommand.
func (s *Server) routes() []route {
	return []route{
		{"GET /sessions", s.handleListSessions},
		{"GET /sessions/{id}/stream", s.handleStream},
		{"GET /sessions/{id}/tasks/{taskId}/output", s.handleTaskOutput},
		{"GET /sessions/{id}/account", s.handleAccount},
		{"POST /sessions/{id}/account", s.handleAccountSwitch},
		{"GET /accounts", s.handleAccounts},
		{"GET /capabilities", s.handleCapabilities},
		{"POST /sessions/{id}/login", s.handleLogin},
		{"GET /sessions/{id}/login/terminal", s.handleLoginTerminal},
		{"DELETE /sessions/{id}/login", s.handleLoginClose},
		{"GET /sessions/{id}/chess-game", s.handleChessGameFile},
		{"POST /sessions/{id}/add-support", s.handleAddSupport},
		{"POST /workspace-command", s.handleWorkspaceCommand},
		{"GET /workspace-stream", s.handleWorkspaceStream},
		{"POST /open-external", s.handleOpenExternal},
	}
}

// handleOpenExternal opens one hyperlink in the pinned external browser
// profile. The webapp cancels an anchor click before WebKit can navigate the
// xwidget and posts the URL here, so this route is the ONLY thing standing
// between a clicked link and nothing happening at all — a failure is answered
// with a status, never absorbed.
func (s *Server) handleOpenExternal(w http.ResponseWriter, r *http.Request) {
	var body struct {
		URL string `json:"url"`
	}
	if err := json.NewDecoder(r.Body).Decode(&body); err != nil {
		httpError(w, http.StatusBadRequest, fmt.Sprintf("invalid request body: %v", err))
		return
	}
	if err := externalbrowser.Validate(body.URL); err != nil {
		s.logf("open-external: refused url=%q: %v", body.URL, err)
		httpError(w, http.StatusBadRequest, err.Error())
		return
	}
	if err := s.openExternalURL(body.URL); err != nil {
		s.logf("open-external: opening url=%q in profile %q failed: %v",
			body.URL, externalbrowser.ProfileDirectory, err)
		httpError(w, http.StatusInternalServerError, err.Error())
		return
	}
	s.logf("open-external: opened url=%q in profile %q",
		body.URL, externalbrowser.ProfileDirectory)
	writeJSON(w, s.logf, map[string]string{"opened": body.URL})
}

// ---------------------------------------------------------------------------
// Registry-sourced session resolution
//
// There is no live-session map anymore: the registry is the source of truth
// for records and the session controller owns the live shims. A session id maps to its
// workspace via rec.CWD.
// ---------------------------------------------------------------------------

// workspaceForSession returns the workspace (cwd) driving id, reading a
// NON-terminal registry record. A terminal or unknown id reports ok=false.
func (s *Server) workspaceForSession(id string) (string, bool) {
	rec, ok := s.registry.Get(id)
	if !ok || rec.Terminal {
		return "", false
	}
	return rec.CWD, true
}

// sessionCWD reports the working directory a session runs in, from its
// registry record (terminal or not). Reading a game file must not depend on a
// live shim.
func (s *Server) sessionCWD(id string) (string, bool) {
	rec, ok := s.registry.Get(id)
	if !ok {
		return "", false
	}
	return rec.CWD, true
}

// sessionDirs reports a session's working directory and its CLAUDE_CONFIG_DIR
// from its registry record. An empty ConfigDir names the CLI's own default
// root rather than "unknown".
func (s *Server) sessionDirs(id string) (cwd, configDir string, known bool) {
	rec, ok := s.registry.Get(id)
	if !ok {
		return "", "", false
	}
	return rec.CWD, rec.ConfigDir, true
}

// sessionConfigDir returns id's CLAUDE_CONFIG_DIR — the ACCOUNT it runs as —
// and whether the daemon knows id at all. An empty config dir is a real
// answer (the CLI's own default root), which is why the bool carries the
// "unknown" case rather than the string.
func (s *Server) sessionConfigDir(id string) (string, bool) {
	rec, ok := s.registry.Get(id)
	if !ok {
		return "", false
	}
	return rec.ConfigDir, true
}

// known reports whether id resolves as a non-terminal registry record.
func (s *Server) known(id string) bool {
	rec, ok := s.registry.Get(id)
	return ok && !rec.Terminal
}

// chessGameDirParts is the fixed worktree-relative directory the
// /show-chess-game skill writes game payload files into.
var chessGameDirParts = []string{".claude", "emacs", "cee-web-widget"}

// handleAddSupport asks Emacs to open a workspace that builds graphical
// support for a slash command the CLI refused in this environment. The daemon
// detects nothing here and generates nothing: a 202 means "asked", never
// "created".
func (s *Server) handleAddSupport(w http.ResponseWriter, r *http.Request) {
	id := r.PathValue("id")
	cwd, configDir, known := s.sessionDirs(id)
	if !known {
		httpError(w, http.StatusNotFound, "no such session")
		return
	}
	// git_root is mandatory downstream and Emacs refuses a create without it,
	// so a session with no cwd cannot be served rather than guessed at.
	if cwd == "" {
		httpError(w, http.StatusConflict, "session has no working directory to open a workspace against")
		return
	}
	var body struct {
		Command string `json:"command"`
	}
	if err := json.NewDecoder(r.Body).Decode(&body); err != nil {
		httpError(w, http.StatusBadRequest, fmt.Sprintf("invalid request body: %v", err))
		return
	}
	if err := addsupport.ValidateCommand(body.Command); err != nil {
		httpError(w, http.StatusBadRequest, err.Error())
		return
	}
	dir, err := workspacecmd.Dir()
	if err != nil {
		s.logf("session %s: resolve workspace-commands dir: %v", id, err)
		httpError(w, http.StatusInternalServerError, "resolving the workspace-commands directory failed")
		return
	}
	// The brief is read from prompts/ per request. A missing or malformed prompt
	// file is a 500 rather than a workspace opened around an empty brief, which
	// would burn a worktree, a branch, and a session on nothing.
	brief, err := addsupport.Prompt(body.Command, configDir)
	if err != nil {
		s.logf("session %s: compose add-support brief for /%s: %v", id, body.Command, err)
		httpError(w, http.StatusInternalServerError, "composing the add-support prompt failed")
		return
	}
	cmd := workspacecmd.NewCreate(
		addsupport.WorkspaceName(body.Command),
		cwd,
		brief,
	)
	path, err := workspacecmd.Emit(dir, []workspacecmd.Entry{cmd})
	if err != nil {
		s.logf("session %s: emit add-support workspace command for /%s: %v", id, body.Command, err)
		httpError(w, http.StatusInternalServerError, "emitting the workspace command failed")
		return
	}
	s.logf("session %s: asked Emacs to open workspace %q for unsupported /%s (%s)",
		id, cmd.Name, body.Command, path)
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusAccepted)
	writeJSON(w, s.logf, map[string]string{"workspace": cmd.Name})
}

// handleWorkspaceCommand drops webapp sidebar actions onto the
// workspace-commands channel Emacs watches. Writing the file is the entire
// request: the ack means "asked", never "done". The body is a JSON array
// honored whole or not at all.
func (s *Server) handleWorkspaceCommand(w http.ResponseWriter, r *http.Request) {
	var raw []json.RawMessage
	if err := json.NewDecoder(r.Body).Decode(&raw); err != nil {
		httpError(w, http.StatusBadRequest, fmt.Sprintf("invalid request body: %v", err))
		return
	}
	if len(raw) == 0 {
		httpError(w, http.StatusBadRequest, "body must be a non-empty JSON array of entries")
		return
	}
	entries := make([]workspacecmd.Entry, len(raw))
	for i, msg := range raw {
		entry, err := decodeWorkspaceCommand(msg)
		if err != nil {
			httpError(w, http.StatusBadRequest, fmt.Sprintf("entry %d: %v", i, err))
			return
		}
		entries[i] = entry
	}
	dir, err := workspacecmd.Dir()
	if err != nil {
		s.logf("workspace-command: resolve workspace-commands dir: %v", err)
		httpError(w, http.StatusInternalServerError,
			fmt.Sprintf("resolving the workspace-commands directory failed: %v", err))
		return
	}
	path, err := workspacecmd.Emit(dir, entries)
	if err != nil {
		s.logf("workspace-command: emit %d entries: %v", len(entries), err)
		httpError(w, http.StatusInternalServerError,
			fmt.Sprintf("emitting the workspace commands failed: %v", err))
		return
	}
	s.logf("workspace-command: asked Emacs to apply %d entries (%s)", len(entries), path)
	w.Header().Set("Content-Type", "application/json")
	writeJSON(w, s.logf, map[string]int{"emitted": len(entries)})
}

// decodeWorkspaceCommand decodes one POST /workspace-command array entry by
// its "type" tag. Only the sidebar's own gestures are accepted.
func decodeWorkspaceCommand(raw json.RawMessage) (workspacecmd.Entry, error) {
	var head struct {
		Type string `json:"type"`
	}
	if err := json.Unmarshal(raw, &head); err != nil {
		return nil, fmt.Errorf("invalid entry: %v", err)
	}
	var entry workspacecmd.Entry
	switch head.Type {
	case "switch":
		var e workspacecmd.Switch
		if err := json.Unmarshal(raw, &e); err != nil {
			return nil, fmt.Errorf("invalid switch entry: %v", err)
		}
		entry = e
	case "fold":
		var e workspacecmd.Fold
		if err := json.Unmarshal(raw, &e); err != nil {
			return nil, fmt.Errorf("invalid fold entry: %v", err)
		}
		entry = e
	case "set-view":
		var e workspacecmd.SetView
		if err := json.Unmarshal(raw, &e); err != nil {
			return nil, fmt.Errorf("invalid set-view entry: %v", err)
		}
		entry = e
	case "task-create":
		var e workspacecmd.TaskCreate
		if err := json.Unmarshal(raw, &e); err != nil {
			return nil, fmt.Errorf("invalid task-create entry: %v", err)
		}
		entry = e
	case "task-toggle-done":
		var e workspacecmd.TaskToggleDone
		if err := json.Unmarshal(raw, &e); err != nil {
			return nil, fmt.Errorf("invalid task-toggle-done entry: %v", err)
		}
		entry = e
	case "task-open":
		var e workspacecmd.TaskOpen
		if err := json.Unmarshal(raw, &e); err != nil {
			return nil, fmt.Errorf("invalid task-open entry: %v", err)
		}
		entry = e
	case "task-add-workspace":
		var e workspacecmd.TaskAddWorkspace
		if err := json.Unmarshal(raw, &e); err != nil {
			return nil, fmt.Errorf("invalid task-add-workspace entry: %v", err)
		}
		entry = e
	default:
		return nil, fmt.Errorf("unsupported type %q", head.Type)
	}
	if err := entry.Validate(); err != nil {
		return nil, err
	}
	return entry, nil
}

// handleChessGameFile serves a chess-game payload file the session's agent
// wrote under its worktree. The path is caller-supplied, so it is validated
// down to "a chess-game-* file directly inside THIS session's
// <cwd>/.claude/emacs/cee-web-widget/" before any read happens.
func (s *Server) handleChessGameFile(w http.ResponseWriter, r *http.Request) {
	id := r.PathValue("id")
	cwd, known := s.sessionCWD(id)
	if !known {
		httpError(w, http.StatusNotFound, "no such session")
		return
	}
	if cwd == "" {
		httpError(w, http.StatusNotFound, "session has no working directory")
		return
	}
	raw := r.URL.Query().Get("path")
	if raw == "" {
		httpError(w, http.StatusBadRequest, "path is required")
		return
	}
	path, err := chessGamePath(cwd, raw)
	if err != nil {
		httpError(w, http.StatusForbidden, err.Error())
		return
	}
	data, err := os.ReadFile(path)
	if err != nil {
		if os.IsNotExist(err) {
			httpError(w, http.StatusNotFound, "game file not found")
			return
		}
		s.logf("session %s: read chess-game file %s: %v", id, path, err)
		httpError(w, http.StatusInternalServerError, "reading game file failed")
		return
	}
	w.Header().Set("Content-Type", "text/plain; charset=utf-8")
	if _, err := w.Write(data); err != nil {
		s.logf("session %s: write chess-game response: %v", id, err)
	}
}

// chessGamePath validates a marker-carried path against the session's
// worktree: after cleaning, the file must sit DIRECTLY inside
// <cwd>/.claude/emacs/cee-web-widget/ and be named chess-game-*.
func chessGamePath(cwd, raw string) (string, error) {
	clean := filepath.Clean(raw)
	dir := filepath.Join(append([]string{cwd}, chessGameDirParts...)...)
	if filepath.Dir(clean) != dir {
		return "", fmt.Errorf("path is outside the session's %s directory", filepath.Join(chessGameDirParts...))
	}
	if !strings.HasPrefix(filepath.Base(clean), "chess-game-") {
		return "", fmt.Errorf("game file name must start with chess-game-")
	}
	return clean, nil
}

// loginAccount resolves the account a login route is being asked about,
// answering the 404 / 503 cases shared by all three of them.
func (s *Server) loginAccount(w http.ResponseWriter, id string) (string, bool) {
	configDir, known := s.sessionConfigDir(id)
	if !known {
		httpError(w, http.StatusNotFound, "no such session")
		return "", false
	}
	if s.logins == nil {
		httpError(w, http.StatusServiceUnavailable, "login is not configured")
		return "", false
	}
	return configDir, true
}

// handleLogin opens the interactive Claude login for the account this session
// runs as, on a pty the daemon owns. Idempotent: a second click joins the
// terminal already open.
func (s *Server) handleLogin(w http.ResponseWriter, r *http.Request) {
	id := r.PathValue("id")
	configDir, ok := s.loginAccount(w, id)
	if !ok {
		return
	}
	sess, err := s.logins.Open(configDir)
	if err != nil {
		httpError(w, http.StatusInternalServerError, fmt.Sprintf("session %s: opening the login terminal: %v", id, err))
		return
	}
	s.logf("session %s: login terminal open for account %q", id, sess.Account())
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusAccepted)
	writeJSON(w, s.logf, map[string]string{
		"account":      sess.Account(),
		"terminal_url": fmt.Sprintf("/sessions/%s/login/terminal", id),
	})
}

// handleLoginTerminal streams the login terminal to a viewer and feeds its
// keystrokes back. Nothing here parses the terminal.
func (s *Server) handleLoginTerminal(w http.ResponseWriter, r *http.Request) {
	id := r.PathValue("id")
	configDir, ok := s.loginAccount(w, id)
	if !ok {
		return
	}
	sess := s.logins.Get(configDir)
	if sess == nil {
		httpError(w, http.StatusConflict, "no login is running for this session's account (POST the login first)")
		return
	}
	conn, err := s.upgrader.Upgrade(w, r, nil)
	if err != nil {
		s.logf("server: login websocket upgrade: %v", err)
		return
	}

	client := login.NewClient()
	sess.Attach(client)

	// Writer: terminal → socket.
	go func() {
		defer func() {
			if err := conn.Close(); err != nil {
				s.logf("server: login websocket close: %v", err)
			}
		}()
		for chunk := range client.Out {
			if err := conn.WriteMessage(websocket.BinaryMessage, chunk); err != nil {
				s.logf("server: login websocket write: %v", err)
				sess.Detach(client)
				for range client.Out { //nolint:revive
				}
				return
			}
		}
	}()

	// Reader: socket → child.
	for {
		kind, data, err := conn.ReadMessage()
		if err != nil {
			sess.Detach(client)
			return
		}
		switch kind {
		case websocket.BinaryMessage:
			// The login session owns the canonical PTY write error record.
			_ = sess.Write(data)
		case websocket.TextMessage:
			var ctl struct {
				Resize *struct {
					Rows uint16 `json:"rows"`
					Cols uint16 `json:"cols"`
				} `json:"resize"`
			}
			if err := json.Unmarshal(data, &ctl); err != nil {
				s.logf("server: login control frame %q: %v", data, err)
				continue
			}
			if ctl.Resize != nil {
				// The login session owns the canonical PTY resize error record.
				_ = sess.Resize(ctl.Resize.Rows, ctl.Resize.Cols)
			}
		}
	}
}

// handleLoginClose ends the login terminal for this session's account.
// Closing one that is not running is a success.
func (s *Server) handleLoginClose(w http.ResponseWriter, r *http.Request) {
	id := r.PathValue("id")
	configDir, ok := s.loginAccount(w, id)
	if !ok {
		return
	}
	if err := s.logins.Close(configDir); err != nil {
		httpError(w, http.StatusInternalServerError, fmt.Sprintf("session %s: closing the login terminal: %v", id, err))
		return
	}
	w.WriteHeader(http.StatusNoContent)
}

// handleTaskOutput serves a bounded, session-scoped tail of a detached task's
// output file. The task's output path comes off the session controller's rebuilt
// TaskEntry and is re-validated for confinement before any read. The response
// carries the next byte cursor, whether the task completed, and a live elapsed
// the frozen SDK heartbeat no longer feeds.
func (s *Server) handleTaskOutput(w http.ResponseWriter, r *http.Request) {
	id := r.PathValue("id")
	cwd, configDir, known := s.sessionDirs(id)
	if !known {
		httpError(w, http.StatusNotFound, "no such session")
		return
	}
	entry, ok := s.controller.TaskEntry(cwd, r.PathValue("taskId"))
	if !ok {
		httpError(w, http.StatusNotFound, "no such task")
		return
	}
	path := entry.GetOutputPath()
	root := session.ClaudeConfigDir(configDir)
	if path == "" || (!allowedTaskOutputPath(path) && !allowedJournalPath(path, root)) {
		httpError(w, http.StatusNotFound, "no such task")
		return
	}
	var offset int64
	if q := r.URL.Query().Get("offset"); q != "" {
		n, err := strconv.ParseInt(q, 10, 64)
		if err != nil || n < 0 {
			httpError(w, http.StatusBadRequest, "invalid offset")
			return
		}
		offset = n
	}
	text, next, err := readTailChunk(path, offset)
	if err != nil {
		s.httpFail(w, r, http.StatusInternalServerError, "session %s: read task output %s: %v", id, path, err)
		return
	}
	done := entry.GetEndedAtMs() != 0
	var elapsedMs int64
	if start := entry.GetStartedAtMs(); start != 0 {
		if end := entry.GetEndedAtMs(); end != 0 {
			elapsedMs = end - start
		} else {
			elapsedMs = s.now().UnixMilli() - start
		}
	}
	w.Header().Set("Content-Type", "application/json")
	writeJSON(w, s.logf, map[string]any{
		"text":       text,
		"offset":     next,
		"done":       done,
		"elapsed_ms": elapsedMs,
	})
}

func (s *Server) handleAccount(w http.ResponseWriter, r *http.Request) {
	id := r.PathValue("id")
	configDir, known := s.sessionConfigDir(id)
	if !known {
		httpError(w, http.StatusNotFound, "no such session")
		return
	}
	identity, err := account.Read(configDir)
	if err != nil {
		s.httpFail(w, r, http.StatusInternalServerError, "session %s: read account identity: %v", id, err)
		return
	}
	w.Header().Set("Content-Type", "application/json")
	writeJSON(w, s.logf, identity)
}

// accountStatus is one roster entry as GET /accounts reports it.
type accountStatus struct {
	Account
	// Email is the root's logged-in account, "" when logged out.
	Email string `json:"email"`
	// Error carries a per-root identity read failure (corrupt .claude.json),
	// surfaced in-band so one broken root cannot hide the healthy ones.
	Error string `json:"error,omitempty"`
}

// handleAccounts reports the canonical account roster with each root's live
// identity. An empty roster is the capability being unconfigured.
func (s *Server) handleAccounts(w http.ResponseWriter, _ *http.Request) {
	if len(s.accounts) == 0 {
		httpError(w, http.StatusServiceUnavailable, "accounts are not configured")
		return
	}
	roster := make([]accountStatus, 0, len(s.accounts))
	for _, acct := range s.accounts {
		entry := accountStatus{Account: acct}
		identity, err := account.Read(acct.ConfigDir)
		if err != nil {
			entry.Error = err.Error()
		} else {
			entry.Email = identity.Email
		}
		roster = append(roster, entry)
	}
	w.Header().Set("Content-Type", "application/json")
	writeJSON(w, s.logf, map[string][]accountStatus{"accounts": roster})
}

// handleCapabilities reports which optional daemon capabilities are wired.
func (s *Server) handleCapabilities(w http.ResponseWriter, _ *http.Request) {
	dir := s.widgetAssetsDir
	bundlePresent := false
	if dir != "" {
		if info, err := os.Stat(filepath.Join(dir, "chess-widget.js")); err == nil && !info.IsDir() {
			bundlePresent = true
		}
	}
	w.Header().Set("Content-Type", "application/json")
	writeJSON(w, s.logf, map[string]any{
		"widget_assets":         dir != "",
		"widget_assets_dir":     dir,
		"widget_bundle_present": bundlePresent,
	})
}

// migrateTranscript copies the conversation's durable transcript into the
// target root so a --resume there finds it. An already-present dst wins.
func migrateTranscript(src, dst string) error {
	if _, err := os.Stat(dst); err == nil {
		return nil
	}
	in, err := os.Open(src) //nolint:gosec // path is daemon-derived, not user input
	if err != nil {
		return fmt.Errorf("open source transcript: %w", err)
	}
	defer func() { _ = in.Close() }()
	if err := os.MkdirAll(filepath.Dir(dst), 0o755); err != nil {
		return fmt.Errorf("create target project dir: %w", err)
	}
	out, err := os.Create(dst) //nolint:gosec // path is daemon-derived, not user input
	if err != nil {
		return fmt.Errorf("create target transcript: %w", err)
	}
	if _, err := io.Copy(out, in); err != nil {
		_ = out.Close()
		return fmt.Errorf("copy transcript: %w", err)
	}
	if err := out.Close(); err != nil {
		return fmt.Errorf("close target transcript: %w", err)
	}
	return nil
}

// handleAccountSwitch moves a session onto another canonical account root:
// migrate the transcript so --resume finds it, stop the old shim, persist the
// new root, and bring the shim back up under the target CLAUDE_CONFIG_DIR with
// the same s_ id. The target must be on the -accounts roster.
func (s *Server) handleAccountSwitch(w http.ResponseWriter, r *http.Request) {
	id := r.PathValue("id")
	if len(s.accounts) == 0 {
		httpError(w, http.StatusServiceUnavailable, "accounts are not configured")
		return
	}
	if s.registry == nil {
		httpError(w, http.StatusServiceUnavailable, "account switching requires the session registry")
		return
	}
	var body struct {
		ConfigDir string `json:"config_dir"`
	}
	if err := json.NewDecoder(r.Body).Decode(&body); err != nil {
		httpError(w, http.StatusBadRequest, fmt.Sprintf("invalid request body: %v", err))
		return
	}
	target, ok := s.rosterEntry(body.ConfigDir)
	if !ok {
		httpError(w, http.StatusBadRequest, fmt.Sprintf("config_dir %q is not on the account roster", body.ConfigDir))
		return
	}
	rec, ok := s.registry.Get(id)
	if !ok {
		httpError(w, http.StatusNotFound, "no such session")
		return
	}
	if rec.Terminal {
		httpError(w, http.StatusConflict, "session has ended")
		return
	}
	if rec.ConfigDir == target.ConfigDir {
		s.respondSwitched(w, http.StatusOK, false, target)
		return
	}
	// A turn in flight cannot be interrupted by a shim restart cleanly.
	if st, found, _ := s.ssm.Current(rec.CWD); found && st.GetTurnActive() {
		httpError(w, http.StatusConflict, "a turn is in flight; retry when it settles")
		return
	}

	csid := rec.ClaudeSessionID
	if csid != "" {
		src := session.TranscriptPath(session.ClaudeConfigDir(rec.ConfigDir), rec.CWD, csid)
		dst := session.TranscriptPath(session.ClaudeConfigDir(target.ConfigDir), rec.CWD, csid)
		if err := migrateTranscript(src, dst); err != nil {
			s.httpFail(w, r, http.StatusConflict, "session %s (cwd %s): transcript migration: %v", id, rec.CWD, err)
			return
		}
	}

	// Stop the exact process before the root changes. Account switching is an
	// intentional replacement, not hibernation: it may land while the original
	// generation is entering service, and it never publishes HIBERNATED.
	if err := s.controller.StopSessionForReplacement(rec.CWD, id); err != nil {
		s.httpFail(w, r, http.StatusConflict, "session %s (cwd %s): account-switch shim stop: %v", id, rec.CWD, err)
		return
	}

	// Persist the new root (and freshest claude_session_id) BEFORE the
	// relaunch: if the bring-up fails, the record still rehydrates under the
	// target root on the next access instead of the old one.
	//
	// THE SELECTION IS RECORDED SEPARATELY from the account it resolves to.
	// ConfigDir alone cannot say whether a human chose it, and every later
	// bring-up needs that distinction: without it, the next create would
	// recompute the account from the workspace path and quietly undo this
	// switch. The override is what makes the choice stick, and what a child
	// workspace inherits.
	//
	// An explicit selection of the DEFAULT account is stored as that root's
	// absolute path, never as "", so "nobody chose" stays distinguishable from
	// "the default was chosen".
	override := target.ConfigDir
	if override == "" {
		override = session.DefaultClaudeConfigDir()
	}
	s.updateRegistry(id, "account switch", func(rec *registry.Record) {
		rec.ConfigDir = target.ConfigDir
		rec.ConfigDirOverride = override
		if csid != "" {
			rec.ClaudeSessionID = csid
		}
	})

	// Bring the shim back up under the same id: Ensure re-locates the
	// just-updated record and spawns fresh under the target root.
	if err := s.controller.Ensure(rec.CWD); err != nil {
		s.httpFail(w, r, http.StatusInternalServerError, "session %s (cwd %s): relaunch under %q: %v", id, rec.CWD, target.ConfigDir, err)
		return
	}
	s.logf("session %s: switched to account %q (%s), resume %s", id, target.Label, target.ConfigDir, csid)
	// Push the updated SessionView so every frontend reflects the new account
	// (SessionView.config_dir now carries the switched-to root, S8). The webapp
	// owns account switching; Emacs merely renders this pushed state.
	s.pushSessionView(id)
	s.respondSwitched(w, http.StatusAccepted, true, target)
}

// rosterEntry resolves dir against the canonical account roster.
func (s *Server) rosterEntry(dir string) (Account, bool) {
	for _, acct := range s.accounts {
		if acct.ConfigDir == dir {
			return acct, true
		}
	}
	return Account{}, false
}

// respondSwitched reports a switch outcome with the target root's live
// identity.
func (s *Server) respondSwitched(w http.ResponseWriter, status int, switched bool, target Account) {
	entry := accountStatus{Account: target}
	identity, err := account.Read(target.ConfigDir)
	if err != nil {
		entry.Error = err.Error()
	} else {
		entry.Email = identity.Email
	}
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(status)
	writeJSON(w, s.logf, map[string]any{"switched": switched, "account": entry})
}

// handleSendMessage and handleInterrupt were removed in S7: Emacs submits
// prompts and interrupts over the frontend.v1 UDS commands
// (submitPrompt/interrupt, keyed by workspace), and the webapp drives both over
// its /stream WebSocket — neither HTTP route had a remaining caller. The turn
// still flows through the same per-session controller (SubmitPrompt/Interrupt); only
// the HTTP entry points are gone.

// frontendProtocolVersion is the agentshim.frontend.v1 protocol version the
// daemon reports in DaemonView. It is distinct from protocol.Layer2Version (the
// Layer-2 wire version GET /sessions still reports): the frontend surface has
// its own version line, and Emacs keys its UDS version-mismatch warnings on it.
const frontendProtocolVersion = "1"

// ResumeTranscriptMissingError reports a create rejected by the resume-viability
// gate: the --resume target has no transcript in this daemon's config dir.
// Callers map it to their transport — HTTP a 422 with the structured body, UDS a
// loud CommandAck error — rather than silently downgrading to a fresh session.
type ResumeTranscriptMissingError struct {
	ResumeID          string
	CWD               string
	ConfigDir         string
	ResolvedConfigDir string
	TranscriptPath    string
	SearchedPaths     []string
}

func (e *ResumeTranscriptMissingError) Error() string {
	return fmt.Sprintf("resume target %s has no transcript in this daemon's config dir (searched %s); refusing to start a fresh conversation",
		e.ResumeID, strings.Join(e.SearchedPaths, ", "))
}

// validateResumeTarget is the sole resume-viability gate. Every path that
// names a Claude conversation delegates here before it can launch a shim.
// A non-empty resume target is a continuity commitment: it either names a
// readable transcript under the session's recorded config root and cwd, or
// the operation fails without starting a different conversation.
func validateResumeTarget(opts CreateOpts, skip bool) *ResumeTranscriptMissingError {
	if opts.Resume == "" || skip {
		return nil
	}
	resolvedConfigDir := session.ClaudeConfigDir(opts.ConfigDir)
	path, ok := session.TranscriptExists(opts.ConfigDir, opts.CWD, opts.Resume)
	if ok {
		return nil
	}
	return &ResumeTranscriptMissingError{
		ResumeID:          opts.Resume,
		CWD:               opts.CWD,
		ConfigDir:         opts.ConfigDir,
		ResolvedConfigDir: resolvedConfigDir,
		TranscriptPath:    path,
		SearchedPaths:     []string{path},
	}
}

// logResumeContinuityFailure emits the one ownership-point diagnostic for a
// failed exact resume. Propagation layers return the typed error unchanged and
// never log it again.
func logResumeContinuityFailure(logf func(string, ...any), operation, sessionID string, opts CreateOpts, missing *ResumeTranscriptMissingError) {
	dlog.Tag(dlog.Logf(logf),
		"event", "resume_continuity_failure",
		"operation", operation,
		"decision", "hard_fail",
		"reason", "transcript_missing_or_unreadable",
		"agent_repl_session_id", sessionID,
		"claude_session_id", missing.ResumeID,
		"cwd", missing.CWD,
		"config_dir", missing.ConfigDir,
		"resolved_config_dir", missing.ResolvedConfigDir,
		"transcript_path", missing.TranscriptPath,
		"fake", opts.Fake,
	)("Claude resume rejected because its transcript is unavailable")
}

// InvalidCreateError reports a malformed create request (currently only an
// invalid permission mode). HTTP maps it to 400; UDS surfaces it as a loud ack.
type InvalidCreateError struct{ msg string }

func (e *InvalidCreateError) Error() string { return e.msg }

// errSessionNotFound reports a delete/lookup for an id with no registry record.
var errSessionNotFound = errors.New("no such session")

// CreateSession is the shared create-session core behind both POST /sessions
// (webapp) and the createSession UDS command (Emacs): validate, apply the
// resume-viability gate, supersede workspace/transcript conflicts, register the record,
// and bring up the shim. It returns the new session id and pushes a SessionView
// so every connected frontend learns the workspace->session binding without
// polling GET /sessions. Typed errors (*InvalidCreateError,
// *ResumeTranscriptMissingError) let callers map to their transport; any other
// error is an internal bring-up failure surfaced loudly. A failure after the
// registry write returns the durable session id with the error so the caller
// can identify the failed operation precisely.
//
// errclass.ErrSessionHibernated is the ONE returned error that is not a failed
// create: the record is registered, durably asleep, and its SessionView has
// been pushed. It is returned rather than swallowed because the caller must be
// able to tell a session that is up from one that is asleep — the command
// boundary acks it (createestablish.go), exactly as WorkspaceOpener.establish
// and the boot sweep already read the same sentinel.
func (s *Server) CreateSession(_ context.Context, opts CreateOpts) (string, error) {
	requestedModel := opts.Model
	opts.Model = registry.NormalizeModel(opts.Model)
	if opts.Model != requestedModel {
		s.logf("session create normalized model marker %q to empty before validation, persistence, and spawn (cwd=%s)",
			requestedModel, opts.CWD)
	}
	if opts.PermissionMode != "" && !protocol.ValidPermissionMode(opts.PermissionMode) {
		return "", &InvalidCreateError{msg: fmt.Sprintf("invalid permission_mode %q", opts.PermissionMode)}
	}
	// The ungated-create consent gate. A mode that shadows canUseTool in the
	// fail-open direction leaves this session with no permission gate at all
	// (protocol.UngatedPermissionMode), and it is one string away from every
	// ordinary create — so it takes a caller who SAID SO, not a caller who
	// happened to pass it. Refused loudly rather than downgraded to a gated
	// mode: silently running under a posture nobody asked for is how a caller
	// ends up believing a gate exists.
	if protocol.UngatedPermissionMode(opts.PermissionMode) && !opts.AllowUngated {
		s.logf("session create REJECTED: permission_mode %q runs with NO permission gate (the SDK auto-approves every tool before canUseTool) and the create did not set allow_ungated",
			opts.PermissionMode)
		return "", &InvalidCreateError{msg: fmt.Sprintf(
			"permission_mode %q creates a session with NO permission gate; set allow_ungated to confirm that is intended",
			opts.PermissionMode)}
	}
	resumeLabel := opts.Resume
	if resumeLabel == "" {
		resumeLabel = "fresh"
	}
	// permission_mode rides in the tag set because it is the session's whole
	// safety posture, and an ungated create is announced in the message itself
	// so the log carries a durable record of every gate-less session and the
	// consent that admitted it.
	dlog.Tag(s.logf, "cwd", opts.CWD, "model", opts.Model, "config_dir", opts.ConfigDir,
		"permission_mode", opts.PermissionMode)(
		"session create requested (resume=%s)%s", resumeLabel,
		protocol.UngatedNote("this session", opts.PermissionMode, opts.AllowUngated))
	// Resume viability gate: the CLI hard-exits when asked to --resume a
	// session id with no transcript in this daemon's config dir. Silently
	// downgrading to a FRESH conversation buries a genuinely lost session, so
	// HARD-FAIL the create before bringing anything up. Fake sessions skip the
	// gate — the scripted SDK has no transcripts by design.
	if missing := validateResumeTarget(opts, opts.Fake || s.forceFake); missing != nil {
		// THE RESTORE RUNG. A create naming a conversation whose transcript is
		// gone is not automatically a failed create: the workspace keeps its
		// own backups beside the work (transcriptbackup.go), and putting one
		// back is what turns this into an ordinary resume. Only when there is
		// nothing to put back does the gate's refusal stand.
		restored, restoreErr := attemptTranscriptRestore(s.logf, "session_create", "", opts)
		if restoreErr != nil {
			return "", restoreErr
		}
		if restored {
			missing = validateResumeTarget(opts, opts.Fake || s.forceFake)
		}
		// THE HANDSHAKE RUNG, below the restore and above the refusal — the same
		// rung, in the same order, as the respawn path's (sessioncontrollers.go).
		// A target with no transcript in a workspace that has never run a turn is
		// the uuid the vendor minted at system:init and nothing was ever said in;
		// refusing it destroys nothing and merely leaves the workspace
		// permanently unstartable, because every later create resolves the same
		// dead uuid and fails the same way.
		if missing != nil && waiveHandshakeOnlyResume(s.registry, opts) {
			s.logf("server: session create: resume viability gate WAIVED resume=%q reason=handshake_only_no_turn_ever_ran cwd=%q config_dir=%q — no conversation at this workspace has ever run a turn, so this uuid is a bring-up handshake rather than something to lose; the spawn's own gate decides whether a fresh conversation may start",
				opts.Resume, opts.CWD, opts.ConfigDir)
			opts.Resume = ""
			missing = nil
		}
		if missing != nil {
			logResumeContinuityFailure(s.logf, "session_create", "", opts, missing)
			return "", missing
		}
	}
	// A workspace takes exactly one live session and a transcript exactly one
	// writer, and this create is the newest claim on both, so any older
	// session still holding either stands down BEFORE a second CLI exists
	// (see supersede.go). After the viability gate so a create about to be
	// rejected never tears down a healthy session.
	//
	// A stand-down whose shim SURVIVED its stop ends the create HERE. Minting
	// the replacement past a live writer of the same transcript is the
	// double-writer condition the supersede exists to remove, so no session id
	// is allocated, no record is written and no shim is spawned.
	if err := s.supersedeCreateConflicts(opts); err != nil {
		s.logf("session create REFUSED: a session it supersedes is still alive and holding its transcript: %v", err)
		return "", err
	}
	id := newSessionID()
	// Register BEFORE bring-up: the session controller's SessionLocator resolves a
	// workspace to a session by reading the registry, so the record MUST exist
	// for Ensure to find and drive THIS session — for fake sessions too (unlike
	// the old L2 hub, which kept fake sessions only in an in-memory map). A fake
	// record carries an empty claude_session_id, so it never rehydrates into a
	// doomed --resume; it is simply the session controller's handle on a transient session.
	if s.registry != nil {
		if err := s.registry.Put(registry.Record{
			SessionID:      id,
			CWD:            opts.CWD,
			Model:          opts.Model,
			PermissionMode: opts.PermissionMode,
			ConfigDir:      opts.ConfigDir,
			// The SELECTION rides onto the record so it outlives this session
			// and keeps travelling to this workspace's children. A resolved
			// account is not a selection and is deliberately not copied here.
			ConfigDirOverride: opts.ConfigDirOverride,
			ClaudeSessionID:   opts.Resume,
			CreatedAt:         s.now().UTC().Format(time.RFC3339),
		}); err != nil {
			s.logf("session %s: registry write on create FAILED — the session will not survive a daemon restart: %v", id, err)
		}
	}
	// Eager, reattach-first bring-up: the session controller's SessionLocator resolves the
	// workspace to the newest non-terminal record — the one just Put — so
	// Ensure brings up THIS session. A workspace-less session (no cwd) has no
	// workspace to drive, so it is registered but not brought up.
	if opts.CWD != "" {
		if err := s.controller.Ensure(opts.CWD); err != nil {
			// ONE bring-up outcome is not a failure of this create: the revival
			// gate. The bring-up evaluates the keep-alive policy before it
			// spawns, so a record already past the thresholds is hibernated
			// instead of started (sessioncontroller.hibernateIfStale) — the
			// session EXISTS, it is durably asleep, and the SessionView the
			// gate is rendered from is exactly what the caller is waiting for.
			// The same reading as WorkspaceOpener.establish and the boot sweep.
			//
			// The SessionView is pushed here rather than left to the
			// transition's own repush so the create's workspace->session
			// binding reaches every frontend on the SAME rule as a create that
			// spawned: this path owns the binding push, and a caller that
			// waited for it must not depend on which layer happened to write
			// the record last.
			if errors.Is(err, errclass.ErrSessionHibernated) {
				dlog.Tag(s.logf, "cwd", opts.CWD)(
					"session %s: created HIBERNATED — the record was found past the keep-alive policy's threshold at bring-up, so no shim was spawned and the revival gate stands; the hibernated SessionView is pushed and the create is NOT a resume failure: %v",
					id, err)
				s.pushSessionView(id)
				return id, err
			}
			// The registry record already owns id. Returning it with the failure
			// lets the command boundary attach the exact failed resume identity;
			// discarding it would make a durable session anonymous in its own
			// continuity diagnostic.
			return id, fmt.Errorf("session %s (cwd %s): bring up shim: %w", id, opts.CWD, err)
		}
	}
	dlog.Tag(s.logf, "cwd", opts.CWD)("session %s: created", id)
	// Deliver the workspace->session binding proactively: SessionView is not a
	// controller push (only snapshots carry it otherwise), so the UDS/webapp clients
	// would not learn the new session's id until their next resync without this.
	s.pushSessionView(id)
	return id, nil
}

// DeleteSession is the shared core behind DELETE /sessions/{id} (its HTTP route
// is superseded) and the deleteSession UDS command: mark the record terminal so
// its id stops resolving, best-effort stop the live shim, and push the terminal
// SessionView. A missing record returns errSessionNotFound.
func (s *Server) DeleteSession(id string) error {
	rec, ok := s.registry.Get(id)
	if !ok {
		return errSessionNotFound
	}
	wasTerminal := rec.Terminal
	if !rec.Terminal {
		s.updateRegistry(id, "terminal transition", func(r *registry.Record) {
			r.Terminal = true
			r.DeathReason = errclass.DeathReasonDeleted
		})
		rec, _ = s.registry.Get(id)
	}
	// Always retry this exact session's stop. A terminal transition can race a
	// controller eviction while the spawner still owns its process handle; the
	// session-id-scoped stop guarantees a replacement on the same cwd is untouched.
	if rec.CWD != "" {
		if err := s.controller.StopSession(rec.CWD, id); err != nil {
			s.logf("session %s: delete exact shim stop FAILED (ws %s terminal_before=%v): %v",
				id, rec.CWD, wasTerminal, err)
		} else {
			s.logf("session %s: delete shim stop complete (ws %s terminal_before=%v)",
				id, rec.CWD, wasTerminal)
		}
	}
	// THE TURN CLAIM DIES WITH THE SESSION. A delete landing mid-turn stops the
	// shim, so the TurnEnded that would have superseded the workspace's
	// `thinking` row is never produced by anyone. Left standing it holds the
	// workspace THINKING forever and suppresses the readiness of the next
	// session to drive it — which is exactly what survived a
	// delete-and-recreate as "readiness suppressed (turn in flight)".
	if rec.CWD != "" && s.ssm != nil {
		if err := s.ssm.InvalidateTurnClaim(rec.CWD, id, "session_deleted"); err != nil {
			s.logf("session %s: releasing the workspace's turn claim FAILED (ws %s) — the workspace may stay THINKING until another session supersedes it: %v",
				id, rec.CWD, err)
		}
	}
	// Idempotent delete also repairs stale client rosters. Supersede or another
	// client may have made the record terminal before this caller observed it.
	s.pushSessionView(id)
	s.logf("session %s: delete terminal SessionView pushed (ws %s terminal_before=%v death_reason=%q)",
		id, rec.CWD, wasTerminal, rec.DeathReason)
	return nil
}

// DaemonView is the daemon-identity frame frontends key boot detection and
// version-mismatch warnings on. ProtocolVersion is the frontend.v1 version
// ("1"); DaemonBinaryMtimeMs is the boot-captured binary mtime in milliseconds
// (binaryMTime is stored in seconds). A zero mtime means the boot-time stat
// failed, carried honestly rather than fabricated.
func (s *Server) DaemonView() *frontendv1.DaemonView {
	return &frontendv1.DaemonView{
		BootId:              s.bootID,
		ProtocolVersion:     frontendProtocolVersion,
		DaemonBinaryMtimeMs: s.binaryMTime * 1000,
		DaemonVersion:       s.daemonVersion,
	}
}

// SessionViewFromRecord builds a SessionView from a registry record plus the
// live pending-permission ids. It is the SINGLE shaping shared by the connect
// snapshot (cmd/claude-repld registrySessions) and the create/delete pushes, so
// the two cannot drift. Rehydratable is not listed session state post-cutover
// (controller-internal shim lifecycle) and stays false. Hibernated no longer
// does: hibernation is now a DURABLE record fact with a typed account beside
// it, so both are shaped from the record here.
//
// logf carries the classifier's loud default for a persisted death reason
// outside the known set. A record written by an earlier build may hold an
// arbitrary string, and passing one through silently is precisely what the
// backfillState precedent below exists to avoid.
// The registry-less shapers below pass a nil registry, which withholds
// nothing: see deathForView. They exist for callers that shape a record in
// isolation, where "does this workspace have a claiming successor" is not a
// question that can be asked.
func SessionViewFromRecord(logf dlog.Logf, rec registry.Record, pendingPermissions []string, shimAttached bool) *frontendv1.SessionView {
	return SessionViewFromRecordWithModels(logf, rec, pendingPermissions, shimAttached, nil)
}

// SessionViewFromRecordWithModels is the canonical SessionView shaper with a
// query-published model menu. The menu does not select a model; rec.Model is
// still the one authoritative current selection.
func SessionViewFromRecordWithModels(logf dlog.Logf, rec registry.Record, pendingPermissions []string, shimAttached bool, modelOptions []*frontendv1.ModelOption) *frontendv1.SessionView {
	return SessionViewFromRecordWithModelsAndUsage(logf, nil, rec, pendingPermissions, shimAttached, modelOptions, nil)
}

// SessionViewFromRecordWithModelsAndUsage is the complete canonical SessionView
// shaper, including the durable completed-response aggregate.
//
// `usage` IS NOT SHAPED ONTO THE SessionView, and that is deliberate rather
// than an oversight. It reaches this function because this is where the durable
// read happens, and it LANDS on the TokenBreakdownView — the resolved menu that
// is the aggregate's only rendering surface (internal/frontend/tokenbreakdown.go,
// published from pushSessionView). The parameter stayed on this signature
// through the period when the breakdown did not exist yet, carrying a fact with
// nowhere to put it; the fact now has somewhere, and the parameter is what
// forces every caller of this shaper to have read the aggregate the breakdown
// beside it is resolved from.
//
// reg is the registry the record was read from, and it is read for exactly one
// question: whether a superseded predecessor's workspace still has a successor
// claiming it, in which case the handover is in flight and its death card is
// withheld rather than presented as an open failure (supersedepresent.go). A
// nil reg withholds nothing.
func SessionViewFromRecordWithModelsAndUsage(logf dlog.Logf, reg *registry.Registry, rec registry.Record, pendingPermissions []string, shimAttached bool, modelOptions []*frontendv1.ModelOption, usage *frontendv1.SessionTokenUtilization) *frontendv1.SessionView {
	return &frontendv1.SessionView{
		Workspace:       rec.CWD,
		SessionId:       rec.SessionID,
		Model:           rec.Model,
		PermissionMode:  rec.PermissionMode,
		ClaudeSessionId: rec.ClaudeSessionID,
		Cwd:             rec.CWD,
		Terminal:        rec.Terminal,
		// The one NON-DURABLE fact on this message: whether THIS daemon holds a
		// live session controller for the workspace. See the field's proto comment — a
		// frontend that answers "is this workspace already up?" from the durable
		// fields alone says yes about a workspace a restarted daemon has never
		// brought up, which is how an unwired workspace stopped bootstrapping on
		// a perspective switch.
		ShimAttached: shimAttached,
		// The TYPED death (F4), superseding the free-string death_reason
		// (RETIRED, step 11): it had two producers and zero readers because a
		// frontend could not tell what class of failure the string described;
		// this is the same fact classified once so the dead-state card can
		// render it like every other failure.
		// Derived through deathForView, not errclass.Death directly, so the ONE
		// case where a recorded death must not be presented — a supersede whose
		// successor is still claiming the workspace — is decided in one place
		// for the snapshot and the pushes alike.
		Death:              deathForView(logf, reg, rec),
		PendingPermissions: int64(len(pendingPermissions)),
		// The CLAUDE_CONFIG_DIR the session's shim runs against — the ACCOUNT it
		// runs as (S8). Empty names the CLI's own default root. Carried on every
		// SessionView push (this is the single shaping) so account switching is
		// webapp-initiated, daemon-executed, and reflected in pushed state.
		ConfigDir: rec.ConfigDir,
		// The never-blue backfill signal (F2), mapped off the durable record.
		Backfill:     backfillState(rec.BackfillState),
		ModelOptions: modelOptions,
		// THE HIBERNATION PAIR, from the durable record and from nowhere else.
		// `hibernated` is the compatibility projection of `hibernation`, so
		// both are shaped from the SAME field here rather than one being a live
		// guess beside the other's durable fact — which is how the bool came to
		// be hard-coded false while a session really was asleep.
		Hibernated:  rec.Hibernated,
		Hibernation: hibernationDetail(logf, rec.SessionID, rec.Hibernation),
	}
}

// hibernationDetail maps the record's durable hibernation account onto the
// wire message. Nil for a session that is not hibernated — the field is
// defined as "present iff hibernated", so an empty-but-present detail would
// claim a sleep that is not happening.
//
// AN UNRECOGNIZED CAUSE IS LOUD AND STILL REPORTED. The registry refuses to
// WRITE one, so reaching here means a record written by a binary that knows a
// cause this one does not. Dropping the whole detail would hide the sleep
// itself and leave the frontend rendering a live session that has no shim;
// carrying the timestamp with no cause arm reports exactly what is known.
func hibernationDetail(logf dlog.Logf, sessionID string, h registry.HibernationDetail) *frontendv1.HibernationDetail {
	if h.Cause == "" {
		return nil
	}
	detail := &frontendv1.HibernationDetail{SinceMs: h.SinceMs}
	switch h.Cause {
	case registry.HibernationCauseIdleCutoff:
		detail.Cause = &frontendv1.HibernationDetail_IdleCutoff{
			IdleCutoff: &frontendv1.HibernationIdleCutoff{CutoffMs: h.CutoffMs},
		}
	case registry.HibernationCauseForced:
		detail.Cause = &frontendv1.HibernationDetail_Forced{Forced: &frontendv1.HibernationForced{}}
	case registry.HibernationCauseCacheExpired:
		detail.Cause = &frontendv1.HibernationDetail_CacheExpired{
			CacheExpired: &frontendv1.HibernationCacheExpired{ElapsedMs: h.ElapsedMs, TtlMs: h.TTLMs},
		}
	default:
		if logf != nil {
			logf("server: session %s: hibernation cause %q is not one this binary understands; the sleep is reported with its timestamp and no cause arm rather than hidden",
				sessionID, h.Cause)
		}
	}
	return detail
}

func sessionTokenUtilization(logf dlog.Logf, source SessionTokenUsageSource, sessionID string) *frontendv1.SessionTokenUtilization {
	if source == nil {
		return nil
	}
	records, err := source.List(sessionID)
	if err != nil {
		if logf != nil {
			logf("server: session token utilization read FAILED session=%q operation=list-completed-responses error=%v", sessionID, err)
		}
		panic(fmt.Sprintf("server: list completed token utilization for session %q: %v", sessionID, err))
	}
	if err := frontend.ValidateTokenUtilizationAggregation(records); err != nil {
		// Validate before shaping SessionView so a corrupt durable record cannot
		// escape inside a partially constructed snapshot or session-view frame.
		if logf != nil {
			logf("server: SessionView token utilization aggregation REFUSED source_plane=durable-store requested_session_id=%q error=%v", sessionID, err)
		}
		panic(fmt.Sprintf("server: SessionView token utilization aggregation for session %q: %v", sessionID, err))
	}
	return frontend.AggregateTokenUtilization(records)
}

// backfillState maps the registry record's stored token onto the wire enum. An
// unrecognized token is UNSPECIFIED and loud in the sense that it reads as
// "nothing to backfill" rather than silently as DONE — the safe direction,
// since UNSPECIFIED makes the switch-ensure retry rather than skip.
func backfillState(s string) frontendv1.BackfillState {
	switch s {
	case sessioncontroller.BackfillPending:
		return frontendv1.BackfillState_BACKFILL_STATE_PENDING
	case sessioncontroller.BackfillDone:
		return frontendv1.BackfillState_BACKFILL_STATE_DONE
	case sessioncontroller.BackfillFailed:
		return frontendv1.BackfillState_BACKFILL_STATE_FAILED
	default:
		return frontendv1.BackfillState_BACKFILL_STATE_UNSPECIFIED
	}
}

// pushSessionView pushes id's current SessionView to every connected frontend.
// Nil-safe against a Server built without a registry or frontend server (unit
// harnesses): the push is a best-effort delivery, not a precondition.
// RepushSessionView re-pushes a session's SessionView to every connected
// frontend. Exported for the late-bound registrar hook (F2), which writes a
// record field and needs the change delivered without waiting for whatever
// unrelated event would next push one.
func (s *Server) RepushSessionView(id string) { s.pushSessionView(id) }

// closeViewPushes ends the SessionView push's access to daemon-owned durable
// state, joining whichever push is in flight.
//
// Taking the WRITE lock is the join: it cannot be acquired while any push holds
// the read side, so once this returns there is no reader left and no new one
// can start. That is what lets the caller close the state database next without
// racing a shim-driven push it has no handle on.
func (s *Server) closeViewPushes(cause sessioncontroller.StopCause) {
	s.viewsMu.Lock()
	already := s.viewsClosed
	s.viewsClosed = true
	s.viewsMu.Unlock()
	if !already {
		s.logf("server: SessionView pushes CLOSED before daemon teardown initiator=%s", cause)
	}
}

func (s *Server) pushSessionView(id string) {
	// Held for the WHOLE push: the durable reads are not only the token ledger
	// read at the end — the registry and the SSM are on the same database, and
	// releasing early would put the rest of the push back in the race.
	s.viewsMu.RLock()
	defer s.viewsMu.RUnlock()
	if s.viewsClosed {
		// Reported, never swallowed: a view that could not be delivered is a
		// frontend left one revision stale, and during teardown that is the
		// correct outcome — but it is still a delivery that did not happen.
		s.logf("server: session %s: SessionView push DECLINED — daemon teardown has closed durable-state access; the frontend keeps its last delivered revision", id)
		return
	}
	if s.registry == nil || s.frontend == nil {
		return
	}
	rec, ok := s.registry.Get(id)
	if !ok {
		s.logf("session %s: pushSessionView found no record — cannot deliver the workspace binding", id)
		return
	}
	var pending []string
	live := false
	if !rec.Terminal && rec.CWD != "" && s.controller != nil {
		pending = s.controller.PendingPermissions(rec.CWD)
		live = s.controller.Live(rec.CWD)
	}
	if s.modelCatalogs == nil {
		panic(fmt.Sprintf("server: session %s: pushSessionView requires ModelCatalogs", id))
	}
	modelOptions := s.modelCatalogs.Get(id)
	usage := sessionTokenUtilization(s.logf, s.tokenUsage, id)
	s.frontend.PushSessionView(SessionViewFromRecordWithModelsAndUsage(s.logf, s.registry, rec, pending, live, modelOptions, usage))
	// THE TOKEN-BREAKDOWN MENU is resolved from the SAME aggregate, here,
	// because this is where the durable read already happens: resolving it on
	// its own trigger would put a second durable read of the token ledger on a
	// second clock, and the two could then disagree about what the session
	// spent. The fence comes off the workspace's current state and is carried,
	// never composed.
	//
	// THE REVIVAL GATE RIDES THE SAME PUSH. Its facts come off the session
	// RECORD, and this is the funnel every record mutation ends in — the
	// registry's hibernation write re-pushes the session view as its last step.
	// A hibernation flip that records no SSM state transition would otherwise
	// leave the gate stale until something unrelated moved the state.
	s.publishSessionDerivedViews(rec.CWD, id, usage)
}

// publishSessionDerivedViews hands the resolved-view publisher the workspace's
// fence, its token aggregate and its session identity — the breakdown menu and
// the revival gate, resolved off ONE read of the workspace's current state.
//
// A workspace with no current SSM state has no fence, and both views are
// withheld rather than published unfenced — an unfenced push cannot be told
// from a stale one, which is the whole job of the token. The withholding is
// recorded; it is never silent.
func (s *Server) publishSessionDerivedViews(workspace, sessionID string, usage *frontendv1.SessionTokenUtilization) {
	// NEVER SILENT. This guard used to return with no record at all, which made
	// an unwired frontend surface indistinguishable from a workspace that
	// simply had nothing to publish — and since this function holds the only
	// call site of PublishTokenBreakdown, an unwired publisher meant the
	// breakdown menu could never arrive and nothing anywhere said why. The
	// wiring itself is now taken whole (see Config.AgentShim), so the first two
	// arms are a server with no frontend surface; the third is a session with
	// no workspace to key its views on.
	switch {
	case s.workspaceViews == nil || s.ssm == nil:
		s.logf("server: token breakdown and revival gate NOT PUBLISHED ws=%q session=%q — this server was built with no frontend surface (Config.AgentShim), so there is no publisher to resolve them through and no state machine to fence them with",
			workspace, sessionID)
		return
	case workspace == "":
		s.logf("server: token breakdown and revival gate NOT PUBLISHED session=%q — the session names no workspace, which is the only routing key either view has",
			sessionID)
		return
	}
	state, found, err := s.ssm.Current(workspace)
	if err != nil {
		s.logf("server: token breakdown and revival gate NOT PUBLISHED ws=%q session=%q — the workspace's current state could not be read for its fence: %v", workspace, sessionID, err)
		return
	}
	if !found {
		s.logf("server: token breakdown and revival gate NOT PUBLISHED ws=%q session=%q — the workspace has no resolved state yet, so there is no fence to stamp them with", workspace, sessionID)
		return
	}
	s.workspaceViews.PublishTokenBreakdown(workspace, state.GetFence(), usage)
	s.workspaceViews.PublishSession(workspace, state.GetFence(), sessionID)
}

func (s *Server) handleListSessions(w http.ResponseWriter, _ *http.Request) {
	// SUPERSEDED (S7): the Emacs poller consumes the full list; the webapp
	// probe only reads presence. It is built entirely off the registry (the
	// source of truth for records) plus the session controller/SSM for live fields.
	type entry struct {
		SessionID string `json:"session_id"`
		Terminal  bool   `json:"terminal"`
		CWD       string `json:"cwd,omitempty"`
		Model     string `json:"model,omitempty"`
		// ClaudeSessionID is the durable CLI session uuid (resume target).
		ClaudeSessionID string `json:"claude_session_id,omitempty"`
		// DeathReason classifies a terminal session's end; absent while alive.
		DeathReason string `json:"death_reason,omitempty"`
		// TurnActive reports whether a user turn is in flight (from the SSM).
		TurnActive bool `json:"turn_active"`
		// PendingPermissions lists unresolved permission request ids.
		PendingPermissions []string `json:"pending_permissions,omitempty"`
		// Rehydratable is retained for wire compatibility; the concept is gone
		// under the cutover (the registry is always the source of truth).
		Rehydratable bool `json:"rehydratable,omitempty"`
		// Hibernated is retained for wire compatibility; hibernation is now a
		// controller-internal shim lifecycle detail, not a listed session state.
		Hibernated bool `json:"hibernated,omitempty"`
	}
	records := s.registry.All()
	list := make([]entry, 0, len(records))
	for _, rec := range records {
		e := entry{
			SessionID:       rec.SessionID,
			Terminal:        rec.Terminal,
			CWD:             rec.CWD,
			Model:           rec.Model,
			ClaudeSessionID: rec.ClaudeSessionID,
			DeathReason:     rec.DeathReason,
		}
		if !rec.Terminal && rec.CWD != "" {
			if st, found, _ := s.ssm.Current(rec.CWD); found {
				e.TurnActive = st.GetTurnActive()
			}
			e.PendingPermissions = s.controller.PendingPermissions(rec.CWD)
		}
		list = append(list, e)
	}
	w.Header().Set("Content-Type", "application/json")
	writeJSON(w, s.logf, map[string]any{
		"sessions":            list,
		"boot_id":             s.bootID,
		"protocol_version":    protocol.Layer2Version,
		"daemon_binary_mtime": s.binaryMTime,
	})
}

func (s *Server) handleStream(w http.ResponseWriter, r *http.Request) {
	id := r.PathValue("id")
	cwd, ok := s.workspaceForSession(id)
	if !ok {
		httpError(w, http.StatusNotFound, "no such session")
		return
	}
	// ServeWSScoped upgrades the socket itself and serves frontend.v1 frames
	// scoped to this session/workspace. Inbound is command-strict (S9): the
	// webapp sends FrontendCommand protojson frames, routed through the SAME
	// handler as the Emacs UDS surface. The translator only stamps the scoped
	// workspace when a command omits it (the URL already scopes the connection).
	//
	// This is the endpoint the rendering webview holds open. It receives every
	// state the moment the resolver produces one, exactly as Emacs does: no
	// frontend's render pace gates another's delivery.
	s.frontend.ServeWSScoped(w, r, frontend.Scope{SessionID: id, Workspace: cwd},
		frontend.ClientKindGUIStream, s.frontendCommandTranslator(cwd))
}

// handleWorkspaceStream is the WORKSPACE-ADDRESSED half of the rendering
// webview's transport: GET /workspace-stream?workspace=<URL-encoded absolute
// directory path>. It serves the same frames handleStream does, scoped by the
// workspace the URL names instead of by a session id, so a viewer attaches to a
// workspace without holding any session identity of its own.
//
// Every way the query can fail to name a servable workspace ends here as a
// typed frontend.ScopeRefusal carrying its own status, and the socket is never
// upgraded: an unscoped connection would receive every workspace's frames.
func (s *Server) handleWorkspaceStream(w http.ResponseWriter, r *http.Request) {
	scope, err := frontend.WorkspaceScopeFromQuery(r.URL.RawQuery, s.workspaceKnown)
	if err != nil {
		var refusal *frontend.ScopeRefusal
		if !errors.As(err, &refusal) {
			// Unreachable by construction, and reported rather than assumed
			// away: an unclassified failure here still must not upgrade.
			s.httpFail(w, r, http.StatusInternalServerError,
				"workspace-stream: unclassified scope failure: %v", err)
			return
		}
		s.httpFail(w, r, refusal.HTTPStatus(),
			"workspace-stream: reason=%s workspace=%q: %v", refusal.Reason, refusal.Workspace, err)
		return
	}
	s.frontend.ServeWSScoped(w, r, scope,
		frontend.ClientKindGUIStream, s.frontendCommandTranslator(scope.Workspace))
}

// workspaceKnown reports whether the daemon holds state for workspace: a
// resolved render state in the SSM, or a non-terminal registry record rooted
// there. The union is deliberate — a workspace whose session is still being
// established has a record before the SSM resolves anything for it, and one
// whose session ended still has render state — so a viewer attaching around
// either edge is admitted rather than refused for a workspace that exists.
//
// A lookup failure is returned as an error, never as "not known": the two are
// different answers and the caller reports them differently.
func (s *Server) workspaceKnown(workspace string) (bool, error) {
	if s.ssm == nil {
		return false, fmt.Errorf("server: workspace %q: no SSM is wired", workspace)
	}
	_, found, err := s.ssm.Current(workspace)
	if err != nil {
		return false, fmt.Errorf("server: workspace %q: resolve render state: %w", workspace, err)
	}
	if found {
		return true, nil
	}
	for _, rec := range s.registry.All() {
		if !rec.Terminal && rec.CWD == workspace {
			return true, nil
		}
	}
	return false, nil
}

// frontendCommandTranslator decodes an inbound FrontendCommand protojson frame
// on the per-session /stream WebSocket (command-strict, S9). It stamps the
// scoped workspace only when the command omits it, so the webapp can send a
// bare-workspace command on a session-scoped stream and still route correctly.
// A malformed frame surfaces an error (the frontend read loop logs it and
// continues) rather than being silently dropped.
func (s *Server) frontendCommandTranslator(workspace string) frontend.CommandTranslator {
	return func(raw []byte) (*frontendv1.FrontendCommand, bool, error) {
		cmd := &frontendv1.FrontendCommand{}
		if err := protojson.Unmarshal(raw, cmd); err != nil {
			return nil, false, fmt.Errorf("server: stream: decode FrontendCommand: %w", err)
		}
		if cmd.GetWorkspace() == "" {
			cmd.Workspace = workspace
		}
		return cmd, true, nil
	}
}

// runIdleSweeper periodically hibernates the shims of sessions that have gone
// idle, freeing their CLI process pairs.
//
// IT IS ONE OF THREE PLACES THAT INITIATE THE ONE TRANSITION, and the only one
// that runs on a schedule. The other two ask the same question of the same
// policy at the moments a session is actually used, because a schedule cannot
// answer for the window before its first tick:
//
//   - the prompt gate (sessioncontroller.guardHibernation) evaluates a
//     not-yet-hibernated record before accepting a prompt, so a stale session
//     meets the revival gate instead of forwarding a cold full-context turn;
//   - the bring-up (sessioncontroller.bringUpTracked) evaluates it before
//     spawning, so a stale record sleeps without a shim ever being started.
//
// All three compute nothing themselves: they read keepalive.Config and route
// through sessioncontroller.HibernateWithCause, which owns the claim, the fresh
// elapsed re-check, the durable write and the SessionView push. This one still
// exists because it is the only route that reaches a session NOBODY asks for.
func (s *Server) runIdleSweeper() {
	ticks := s.idleSweepTicks
	if ticks == nil {
		interval := s.idleTimeout / 4
		if interval <= 0 {
			interval = s.idleTimeout
		}
		// THE TICK MUST FIT INSIDE THE PING WINDOW. That window is exactly one
		// leeway wide, so a sweep slower than it could step straight over the
		// only moment a ping is both due and still useful — and the session
		// would silently fall through to the cache-expired branch every time.
		// The keep-alive config tightens the interval and never loosens it.
		interval = s.keepAliveConfig().SweepInterval(interval)
		ticker := time.NewTicker(interval)
		defer ticker.Stop()
		ticks = ticker.C
	}
	for {
		select {
		case <-s.stopped:
			return
		case _, ok := <-ticks:
			if !ok {
				return
			}
			// A tick and shutdown can become ready together. Shutdown wins: a
			// sweeper that starts after its owner begins teardown could touch the
			// registry or token ledger after their owners have begun closing.
			select {
			case <-s.stopped:
				return
			default:
			}
			s.idleSweep()
		}
	}
}

// sweepIdle hibernates every session that has actually gone idle, best-effort.
//
// A workspace whose shim is already stopped returns a "no live session" error
// from the transition, which is expected and skipped, not a failure.
//
// EVERY TEARDOWN THIS SWEEP TAKES GOES THROUGH THE ONE TRANSITION. It used to
// call Hibernate directly for the sessions the keep-alive policy declined,
// which stopped the shim WITHOUT writing a hibernation record — a
// stopped-but-awake durable state whose next prompt silently brought the
// session back up rather than meeting the revival gate. With an idle timeout
// shorter than the ping window that was the ordinary case, not a corner.
func (s *Server) sweepIdle() {
	nowMs := s.now().UnixMilli()
	// THE PING'S FAILURE BOUND IS EVALUATED BEFORE THE POLICY THAT SUBMITS ONE,
	// and the order is the point. A ping whose end was lost holds a claim that
	// declines every later ping, parks real prompts behind it, and reads as a
	// live turn to the hibernation lease this very sweep is about to ask for —
	// so a sweep that submitted first would spend the whole walk being refused by
	// a turn that finished hours ago. See keepalivedeadline.go.
	if s.controller != nil {
		s.controller.SweepOverdueKeepAlivePings()
	}
	for _, rec := range s.registry.All() {
		// THE SWEEP ABANDONS ITS REMAINDER ONCE SHUTDOWN HAS BEGUN, and this is
		// what bounds ShutdownAll's join on sweeperDone. A sweep is a serial walk
		// of the whole registry, and every session it reaches costs a full
		// hibernation — the shim's interrupt ack plus its SIGTERM exit wait — so a
		// sweep that started one tick before a stop held the teardown for the sum
		// of the fleet's exit waits before the drain itself had begun. Measured on
		// 2026-08-08 that join plus a serial drain took ~45s against a 30s stop
		// grace, and the daemon was SIGKILLed mid-drain.
		//
		// ABANDONING IS SAFE AND WAITING IS NOT OPTIONAL. Every session this sweep
		// would still have reached is hibernated by the shutdown drain immediately
		// after, so nothing is left un-torn-down; and the join itself stays,
		// because it is what makes a post-close read of the registry, SSM or token
		// ledger structurally impossible rather than merely unlikely.
		select {
		case <-s.stopped:
			s.logf("server: idle sweep ABANDONED at ws=%q — daemon teardown has begun, and the shutdown drain hibernates every remaining session itself; finishing the walk would only delay the teardown by the fleet's shim exit waits",
				rec.CWD)
			return
		default:
		}
		if rec.Terminal || rec.CWD == "" {
			continue
		}
		// STAMPED BEFORE THE POLICY IS ASKED. A record from before the
		// keep-alive existed carries no last-turn-end, and the policy's "every
		// unknown answers none" rule would leave it outside the loop forever.
		rec = s.stampLegacyTurnEnd(rec)
		// THE KEEP-ALIVE POLICY IS EVALUATED FIRST, and it can take the whole
		// decision for this session. Its hibernate branches are strictly more
		// specific than the idle sweep's — they carry the cutoff or the TTL
		// that tripped — so letting the generic sweep reach a session the
		// policy has already spoken about would replace a stated reason with an
		// unstated one.
		if s.applyKeepAlivePolicy(rec, nowMs) {
			continue
		}
		idleMs, sweepable := s.sweepable(rec.SessionID, rec.CWD, nowMs)
		if !sweepable {
			continue
		}
		// THE IDLE TIMEOUT IS AN IDLE CUTOFF. It is the same fact the policy's
		// own cutoff branch records — "pinging stops and the session sleeps" —
		// measured against this daemon's own -idle-timeout rather than the
		// policy's, so the account carries the threshold that actually tripped.
		detail := registry.HibernationDetail{
			Cause:     registry.HibernationCauseIdleCutoff,
			SinceMs:   nowMs,
			CutoffMs:  int64(s.idleTimeout / time.Millisecond),
			ElapsedMs: idleMs,
		}
		if err := s.controller.HibernateWithCause(rec.CWD, detail); err != nil {
			// Expected for an already-hibernated / never-brought-up workspace, and
			// now also for one that started working between sweepable's read and
			// this call. That race is exactly why the settled check is inside
			// hibernate() as well as here: this gate can go stale, and the one
			// inside the teardown cannot.
			switch {
			case errors.Is(err, sessioncontroller.ErrNotSettled):
				s.logf("session %s: idle sweep HELD after the gate (ws %s): the workspace started working between the check and the teardown: %v",
					rec.SessionID, rec.CWD, err)
			case errors.Is(err, sessioncontroller.ErrHibernationNoLongerIdle):
				s.logf("session %s: idle sweep REFUSED as stale (ws %s): %v", rec.SessionID, rec.CWD, err)
			case errors.Is(err, sessioncontroller.ErrAlreadyHibernated),
				errors.Is(err, sessioncontroller.ErrHibernationInFlight):
				// Another cause won the single-transition claim. Expected.
			default:
				s.logf("session %s: idle sweep skipped (ws %s): %v", rec.SessionID, rec.CWD, err)
			}
		}
	}
}

// stampLegacyTurnEnd gives a record with no last-turn-end one, taken from the
// workspace's own dated state history, and returns the record as it now stands.
//
// PRE-BRANCH RECORDS WOULD OTHERWISE LIVE OUTSIDE THE POLICY FOREVER. Every
// keep-alive decision is a time-since check against LastTurnEndMs, and "every
// unknown answers none" means a zero one is never pinged and never hibernated
// by the policy — so a session created before this feature shipped would be
// governed only by the sweep's own threshold for the rest of its life.
//
// THE INSTANT COMES FROM DURABLE STATE, NEVER FROM now(). Stamping the moment
// of observation would claim the session had just finished a turn, resetting
// its idleness on every daemon boot and making a long-quiet legacy session look
// permanently fresh. The SSM's last activity is the truest dated fact the
// daemon holds about a session whose turn ends it never saw.
func (s *Server) stampLegacyTurnEnd(rec registry.Record) registry.Record {
	if rec.LastTurnEndMs > 0 || s.registry == nil {
		return rec
	}
	atMs, ok := s.legacyTurnEnds().StampLegacyTurnEnd(rec.SessionID, rec.CWD)
	if !ok {
		return rec
	}
	rec.LastTurnEndMs = atMs
	rec.LastTurnEndBackfilled = true
	return rec
}

// legacyTurnEnds is the sweeper's view of the shared stamping rule. The session
// controller is wired with the same object (main.go), so the acceptance-time
// staleness check and this sweep measure a legacy session identically.
func (s *Server) legacyTurnEnds() LegacyTurnEndStamps {
	return LegacyTurnEndStamps{Reg: s.registry, Activity: s.ssm, Logf: s.logf}
}

// WorkspaceActivity is the one dated fact the legacy stamp is taken from: when
// the workspace's durable state history last moved. Stated as an interface so
// the stamping rule does not depend on the whole SSM.
type WorkspaceActivity interface {
	LastActivityMs(workspace string) (int64, bool, error)
}

// LegacyTurnEndStamps is THE legacy last-turn-end stamping rule, held as one
// object because it has TWO callers now: the idle sweeper, and the session
// controller's staleness check at prompt acceptance and bring-up. A second
// expression of the rule would be a second answer to "when was this session
// last active", and the two routes would then disagree about which sessions are
// stale — the one thing this whole gate exists to make unambiguous.
type LegacyTurnEndStamps struct {
	Reg      *registry.Registry
	Activity WorkspaceActivity
	Logf     func(string, ...any)
}

// StampLegacyTurnEnd stamps sessionID's missing last-turn-end from workspace's
// dated state history and reports the instant. A false return means there is no
// dated fact to stamp from, or the stamp could not be made durable — never a
// guessed instant, and never now().
func (s LegacyTurnEndStamps) StampLegacyTurnEnd(sessionID, workspace string) (int64, bool) {
	logf := s.Logf
	if logf == nil {
		logf = func(string, ...any) {}
	}
	if s.Reg == nil || s.Activity == nil {
		logf("session %s: legacy last_turn_end_ms stamp SKIPPED (ws %s): no registry or no activity source is wired, so the session stays outside the keep-alive policy",
			sessionID, workspace)
		return 0, false
	}
	atMs, dated, err := s.Activity.LastActivityMs(workspace)
	if err != nil {
		logf("session %s: legacy last_turn_end_ms stamp read FAILED (ws %s): %v — the session stays outside the keep-alive policy this tick",
			sessionID, workspace, err)
		return 0, false
	}
	if !dated || atMs <= 0 {
		return 0, false
	}
	found, err := s.Reg.Update(sessionID, func(r *registry.Record) {
		if r.LastTurnEndMs == 0 {
			r.LastTurnEndMs = atMs
			// MARKED AS BACKFILLED, because that is what it is. The instant is
			// a true dated fact about the WORKSPACE and the keep-alive policy
			// is right to measure from it; it is not evidence that a turn ran
			// under this record's conversation, and the resume ladder must be
			// able to tell the two apart. See Record.LastTurnEndBackfilled for
			// the workspace this conflation made permanently unstartable.
			r.LastTurnEndBackfilled = true
		}
	})
	if err != nil {
		logf("session %s: legacy last_turn_end_ms stamp write FAILED (ws %s) at_ms=%d: %v — the session stays outside the keep-alive policy",
			sessionID, workspace, atMs, err)
		return 0, false
	}
	if !found {
		logf("session %s: legacy last_turn_end_ms stamp found no record (ws %s)", sessionID, workspace)
		return 0, false
	}
	logf("session %s: legacy record STAMPED with last_turn_end_ms=%d from its dated state history (ws %s) — it enters the cache keep-alive policy from here rather than living outside it",
		sessionID, atMs, workspace)
	return atMs, true
}

// keepAliveConfig is the resolved policy, defaulting a zero Config rather than
// running one whose zero TTL reads every session as already cache-expired.
func (s *Server) keepAliveConfig() keepalive.Config {
	if s.keepAlive.CacheTTL <= 0 {
		return keepalive.DefaultConfig()
	}
	return s.keepAlive
}

// applyKeepAlivePolicy takes the cache keep-alive decision for one session and
// reports whether it OWNS this session's outcome for this tick.
//
// IT IS A TIME-SINCE CHECK AND NOTHING ELSE. The only input is the durably
// persisted last-turn-end instant, compared against wall-clock now. There is no
// timer to miss, no state to drift, and no difference between "the daemon was
// running the whole time" and "the laptop slept through the window" — the
// second case simply produces a larger elapsed and therefore a different, and
// correct, decision.
//
// A HIBERNATED SESSION IS ALREADY OUT. It has no live controller to ping and no
// second sleep to take, so it is claimed here and left alone: letting it fall
// through to the idle sweep would mean re-hibernating a sleeping session on
// every tick forever.
func (s *Server) applyKeepAlivePolicy(rec registry.Record, nowMs int64) (owned bool) {
	if s.controller == nil {
		return false
	}
	if rec.Hibernated {
		return true
	}
	cfg := s.keepAliveConfig()
	decision := cfg.Evaluate(nowMs, rec.LastTurnEndMs)
	switch decision.Action {
	case keepalive.ActionPing:
		// The submit RE-CHECKS eligibility under the manager mutex; this tick's
		// reading of the registry is already stale by the time it can be acted
		// on, and only the mutex can make the check and the submit one act.
		if _, err := s.controller.SubmitKeepAlivePing(context.Background(), rec.CWD); err != nil {
			if errors.Is(err, sessioncontroller.ErrKeepAliveNotEligible) {
				// The overwhelmingly common outcome: no live controller, a turn
				// in flight, prompts queued. Not a failure.
				return true
			}
			s.logf("session %s: cache keep-alive ping FAILED (ws %s) elapsed_ms=%d remaining_ms=%d: %v",
				rec.SessionID, rec.CWD, decision.ElapsedMs, decision.RemainingMs, err)
		}
		return true
	case keepalive.ActionWarmCompact:
		// COMPACT WHILE THE CACHE IS STILL ALIVE. The whole-conversation read a
		// compaction costs is served from the prompt cache here and would be
		// re-ingested at full price once the cache dies — which is exactly what
		// a compact-first revival used to pay, measured at 1.5 million uncached
		// input tokens for one session.
		//
		// The submit RE-CHECKS eligibility under the manager mutex, and it is
		// where the size floor and the once-per-cache-window anchor live; this
		// tick's reading of the registry is already stale by the time it can be
		// acted on. The anchor it is given is the SAME durable instant this
		// decision was taken against, so the whole span of ticks the arm is due
		// across produces exactly one attempt.
		if _, err := s.controller.SubmitWarmCompaction(context.Background(), rec.CWD, rec.LastTurnEndMs); err != nil {
			if errors.Is(err, sessioncontroller.ErrWarmCompactNotEligible) {
				// The overwhelmingly common outcome: the attempt for this cache
				// window has already been made, or the conversation is too small
				// to be worth compacting. Not a failure, and the submit logged
				// which it was.
				return true
			}
			// A FAILED WARM COMPACTION CHANGES NOTHING ELSE ABOUT THE LIFECYCLE.
			// The keep-alive arms still fire for this session on later ticks and
			// it still hibernates on time; all that is lost is the cheap
			// compaction, and the revival that eventually pays for the expensive
			// one is watched by the cold-read alarm.
			s.logf("session %s: warm compaction FAILED (ws %s) elapsed_ms=%d remaining_ms=%d: %v — the session keeps today's behavior for this cache window: keep-alive pings, then hibernation, then a compact-first revival that pays the full-context read",
				rec.SessionID, rec.CWD, decision.ElapsedMs, decision.RemainingMs, err)
		}
		return true
	case keepalive.ActionAwaitExpiry:
		// THE RETRY FLOOR, and there is deliberately no submit in this arm.
		// The policy decided it, so no reading of this switch can ping inside
		// the floor; the line is the canonical record of having entered it.
		s.logf("session %s: cache keep-alive will NOT be submitted (ws %s) elapsed_ms=%d remaining_ms=%d floor_ms=%d: the remaining margin before the cache expires is inside the retry floor, so an attempt would more likely pay a full re-ingest than save one; the cache is left to expire and the policy's cache-expired branch will report it",
			rec.SessionID, rec.CWD, decision.ElapsedMs, decision.RemainingMs, decision.FloorMs)
		return true
	case keepalive.ActionHibernate:
		detail := registry.HibernationDetail{
			Cause:   decision.Cause,
			SinceMs: nowMs,
		}
		switch decision.Cause {
		case keepalive.CauseIdleCutoff:
			detail.CutoffMs = int64(cfg.IdleCutoff / time.Millisecond)
			s.logf("session %s: hibernating on the IDLE CUTOFF (ws %s): quiet for %s, cutoff %s — the keep-alive loop reached its configured maximum, so pinging stops and the session sleeps in the same transition",
				rec.SessionID, rec.CWD, (time.Duration(decision.ElapsedMs) * time.Millisecond).Round(time.Second), cfg.IdleCutoff)
		case keepalive.CauseCacheExpired:
			detail.ElapsedMs = decision.ElapsedMs
			detail.TTLMs = int64(cfg.CacheTTL / time.Millisecond)
			s.logf("session %s: hibernating because the PROMPT CACHE EXPIRED before a ping could fire (ws %s): quiet for %s against a %s TTL — a laptop sleep or daemon downtime carried the session past the window, and pinging a cold cache would pay a full context re-ingest for nobody, so the discovery IS the hibernation",
				rec.SessionID, rec.CWD, (time.Duration(decision.ElapsedMs) * time.Millisecond).Round(time.Second), cfg.CacheTTL)
		}
		if err := s.controller.HibernateWithCause(rec.CWD, detail); err != nil {
			switch {
			case errors.Is(err, sessioncontroller.ErrNotSettled):
				s.logf("session %s: keep-alive hibernation HELD (ws %s cause %s): the workspace started working between the check and the teardown: %v",
					rec.SessionID, rec.CWD, decision.Cause, err)
			case errors.Is(err, sessioncontroller.ErrHibernationNoLongerIdle):
				// The session worked between this sweep's snapshot and the
				// claim's fresh read. Expected, and the refusal is the point:
				// the decision was taken on a reading that has since moved.
				s.logf("session %s: keep-alive hibernation REFUSED as stale (ws %s cause %s): %v",
					rec.SessionID, rec.CWD, decision.Cause, err)
			case errors.Is(err, sessioncontroller.ErrAlreadyHibernated),
				errors.Is(err, sessioncontroller.ErrHibernationInFlight):
				// Another cause won the single-transition claim. Expected.
			default:
				s.logf("session %s: keep-alive hibernation skipped (ws %s cause %s): %v",
					rec.SessionID, rec.CWD, decision.Cause, err)
			}
		}
		return true
	default:
		return false
	}
}

// sweepable reports whether a workspace has been quiet long enough to hibernate.
//
// TWO GATES, AND THE SECOND ONE IS THE POINT. `!turn_active` alone says only
// that no turn is running THIS INSTANT, which every workspace satisfies the
// moment its turn ends — so a sweeper gated on it hibernated healthy sessions
// within one tick of them finishing work, roughly every seven minutes in
// practice. The elapsed-idle gate is what makes the configured window mean
// anything: the shim survives until the workspace has genuinely been left alone
// for idleTimeout.
//
// EVERY UNKNOWN ANSWERS NO. A workspace with no resolved state, or none the log
// can date, is a workspace this sweeper knows nothing about, and reaping on
// absent evidence is precisely how a bring-up in flight got hibernated before
// its first event landed. Only a positive, dated measurement licenses a
// teardown.
// It reports the MEASURED idleness alongside the verdict, so the hibernation
// account records the figure this gate acted on rather than one re-derived from
// a clock that has since moved.
func (s *Server) sweepable(sessionID, workspace string, nowMs int64) (idleMs int64, ok bool) {
	st, found, err := s.ssm.Current(workspace)
	if err != nil {
		s.logf("session %s: idle sweep state read (ws %s): %v", sessionID, workspace, err)
		return 0, false
	}
	if !found {
		s.logf("session %s: idle sweep HELD (ws %s): no resolved state, and an unknown workspace is not a quiet one",
			sessionID, workspace)
		return 0, false
	}
	if st.GetTurnActive() {
		return 0, false
	}
	atMs, dated, err := s.ssm.LastActivityMs(workspace)
	if err != nil {
		s.logf("session %s: idle sweep activity read (ws %s): %v", sessionID, workspace, err)
		return 0, false
	}
	if !dated {
		s.logf("session %s: idle sweep HELD (ws %s): no state history to date the workspace by",
			sessionID, workspace)
		return 0, false
	}
	idle := time.Duration(nowMs-atMs) * time.Millisecond
	if idle < s.idleTimeout {
		return 0, false
	}
	s.logf("session %s: idle sweep hibernating (ws %s): quiet for %s, threshold %s",
		sessionID, workspace, idle.Round(time.Second), s.idleTimeout)
	return int64(idle / time.Millisecond), true
}

// ShutdownAll ends the daemon's session work (daemon teardown). The registry
// records stay non-terminal so they rehydrate on the next boot; main also
// calls controller.Close().
//
// SHIMS SURVIVE BY DEFAULT, and that is the whole point of the parameter.
//
// This used to SIGTERM every live shim unconditionally, which threw away
// exactly the thing the transport inversion was built to preserve: a shim
// outlives its daemon, redials the one well-known daemon socket forever with
// backoff, and is auto-PARKED by the next daemon's listener before anything
// asks for it. Killing them on an orderly bounce meant every restart paid a
// full cold bring-up per workspace — and killed mid-conversation CLI processes
// — to save nothing. Preserved shims cost the next boot a reattach, which is
// what EnsureShim already prefers.
//
// stopShims restores the old behavior for the one caller that needs it: a
// deploy whose shim BUNDLE changed, where a survivor would keep running the
// previous build. It is belt-and-braces beside the version-driven stale-shim
// refresh, never the only guard.
//
// Nothing here marks a record terminal in either mode: a stopped shim's
// session is merely unwired, not dead.
// THE CAUSE IS THE CALLER'S, because two different callers reach this: an
// ordinary daemon shutdown, and the execution phase of a scheduled drain. They
// are the same teardown and NOT the same event — one is somebody stopping the
// daemon, the other is a bounce a deploy scheduled and waited on — so the
// decision lines and the stop records both name which, from the same table the
// stop itself is rendered from.
func (s *Server) ShutdownAll(stopShims bool, cause sessioncontroller.StopCause) {
	s.stopOnce.Do(func() { close(s.stopped) })
	// The idle sweeper reads durable registry, SSM, and token-usage state. Its
	// lifetime therefore ends before any daemon owner may close those stores.
	// Waiting here makes a post-close read structurally impossible rather than
	// merely less likely during process or test teardown.
	<-s.sweeperDone
	s.logf("server: idle sweeper STOPPED before daemon teardown initiator=%s", cause)
	// The shim-driven SessionView pushes end for the sweeper's reason: they read
	// the same durable state, from goroutines this server does not own. It is a
	// DEFER rather than a statement here so the stop-shims pass below still
	// delivers the views its hibernations produce — what the caller needs is
	// only that no push survives this call.
	defer s.closeViewPushes(cause)
	if !stopShims {
		s.logf("server: SHIM STOP DECLINED initiator=%s scope=all_sessions reason=stop_shims_false — every session shim is PRESERVED; survivors redial the daemon shim socket and park until the next daemon claims them", cause)
		return
	}
	s.logf("server: SHIM STOP DECIDED initiator=%s scope=all_sessions reason=stop_shims_true — the caller asked for the shim bundle to be replaced, so every non-terminal session's shim is stopped on the way out", cause)
	if err := s.hibernateAllForShutdown(cause); err != nil {
		// NOTHING HERE IS SWALLOWED. Each session's failure is already logged
		// against its own identity by the worker; this is the joined account, so
		// a drain that lost sessions cannot look like one that lost none.
		s.logf("server: shutdown drain FAILURES initiator=%s: %v", cause, err)
	}
}

// shutdownHibernateWorkers bounds the fan-out of the shutdown drain.
//
// THE DRAIN IS WAIT-BOUND, NOT CPU-BOUND, and the bound is sized from that.
// One session's hibernation is almost entirely time spent waiting on other
// processes: the teardown prologue asks the shim to interrupt its turn and
// waits up to interruptDrainTimeout for the ack, and ShimSpawner.StopShim then
// SIGTERMs the shim and waits on the workspace's kernel session lock for up to
// stopTermGrace (10s), escalating to SIGKILL plus stopKillGrace (2s). Serially,
// a single unresponsive shim spends the whole daemon's stop grace on its own,
// and the ~90-record fleets seen in production overran the 30s grace and were
// SIGKILLed mid-drain — which is what skips lease release and merge
// reconstruction.
//
// Sixteen is generous on purpose. The workers hold no shared CPU-bound
// resource; they block on pipes, signals and lock waits, so the marginal cost
// of a worker is one goroutine and the marginal benefit is one more shim
// exiting concurrently. It is bounded rather than unbounded because each worker
// does touch the serialized durable stores (the SSM's single mutex, the
// registry's write transaction), and an unbounded fan-out over a ~90-session
// fleet would queue ~90 goroutines on those locks for no gain over a pool that
// keeps them saturated.
const shutdownHibernateWorkers = 16

// hibernateAllForShutdown hibernates every non-terminal session CONCURRENTLY
// and returns the joined failure of the ones that could not be hibernated.
//
// THE ORDER OF SESSIONS CARRIES NO MEANING, which is what makes the fan-out
// legitimate. Each hibernation is keyed by its own workspace: the SSM's
// hibernation lease is per-workspace under the manager's own mutex, the session
// controller manager's byWS eviction is under its own mutex, and the registry's
// writes go through one serialized write transaction. Two workspaces therefore
// never contend for anything but those locks, and no session's teardown reads
// state another session's teardown writes.
func (s *Server) hibernateAllForShutdown(cause sessioncontroller.StopCause) error {
	var victims []registry.Record
	for _, rec := range s.registry.All() {
		if rec.Terminal || rec.CWD == "" {
			continue
		}
		victims = append(victims, rec)
	}
	workers := shutdownHibernateWorkers
	if len(victims) < workers {
		workers = len(victims)
	}
	started := time.Now()
	s.logf("server: shutdown drain ENTRY initiator=%s sessions=%d workers=%d — every non-terminal session's shim is stopped concurrently, because a serial drain is bounded by the sum of the fleet's shim exit waits and overran the daemon's stop grace",
		cause, len(victims), workers)
	if workers == 0 {
		s.logf("server: shutdown drain COMPLETE initiator=%s sessions=0 failures=0 elapsed=%s — no non-terminal session had a workspace to drain", cause, time.Since(started).Round(time.Millisecond))
		return nil
	}

	jobs := make(chan registry.Record)
	errsCh := make(chan error, len(victims))
	var wg sync.WaitGroup
	for range workers {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for rec := range jobs {
				if err := s.hibernateOneForShutdown(rec, cause); err != nil {
					errsCh <- err
				}
			}
		}()
	}
	for _, rec := range victims {
		jobs <- rec
	}
	close(jobs)
	wg.Wait()
	close(errsCh)

	var errs []error
	for err := range errsCh {
		errs = append(errs, err)
	}
	s.logf("server: shutdown drain COMPLETE initiator=%s sessions=%d workers=%d failures=%d elapsed=%s",
		cause, len(victims), workers, len(errs), time.Since(started).Round(time.Millisecond))
	return errors.Join(errs...)
}

// hibernateOneForShutdown is one worker's share of the drain: exactly the
// per-session work the serial loop did, with the same lines against the same
// identities, so parallelizing the drain changed what runs concurrently and
// nothing about what each session's teardown reports.
func (s *Server) hibernateOneForShutdown(rec registry.Record, cause sessioncontroller.StopCause) error {
	s.logf("server: SHIM STOP ISSUED initiator=%s session=%s ws=%q reason=stop_shims_true", cause, rec.SessionID, rec.CWD)
	err := s.controller.Hibernate(rec.CWD, cause)
	if err == nil {
		return nil
	}
	// A MID-TURN WORKSPACE IS NOW REFUSED here too, because the settled
	// check lives inside the shared teardown rather than in each caller.
	// That is the right trade for this caller as well: a shim left running
	// on the previous bundle is a stale binary, while a shim SIGTERMed
	// mid-turn is lost work, and the version-driven stale-shim refresh
	// already bounces the survivor the moment it reconnects.
	if errors.Is(err, sessioncontroller.ErrNotSettled) {
		s.logf("server: shutdown stop-shims mode PRESERVING the shim for session %s (ws %s): it has not settled, and the stale-shim refresh will bounce it after the turn: %v",
			rec.SessionID, rec.CWD, err)
	} else {
		s.logf("server: shutdown stop shim (ws %s): %v", rec.CWD, err)
	}
	return fmt.Errorf("server: shutdown stop shim for session %s (ws %q): %w", rec.SessionID, rec.CWD, err)
}

func httpError(w http.ResponseWriter, status int, msg string) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(status)
	_ = json.NewEncoder(w).Encode(map[string]string{"error": msg})
}

// httpFail is httpError plus a log line: every server-fault response otherwise
// vanishes from the daemon log.
func (s *Server) httpFail(w http.ResponseWriter, r *http.Request, status int, format string, args ...any) {
	msg := fmt.Sprintf(format, args...)
	s.logf("server: %s %s -> %d: %s", r.Method, r.URL.Path, status, msg)
	httpError(w, status, msg)
}

func writeJSON(w http.ResponseWriter, logf func(string, ...any), v any) {
	if err := json.NewEncoder(w).Encode(v); err != nil {
		logf("server: encode response: %v", err)
	}
}

func newSessionID() string {
	return "s_" + randomHex()
}

// newBootID mints the daemon instance identity: stable for the life of this
// process, different after every restart.
func newBootID() string {
	return "b_" + randomHex()
}

func randomHex() string {
	var b [8]byte
	if _, err := rand.Read(b[:]); err != nil {
		panic(fmt.Sprintf("server: crypto/rand failed: %v", err))
	}
	return hex.EncodeToString(b[:])
}
