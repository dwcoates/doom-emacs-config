// Package server exposes the daemon's HTTP surface: session CRUD plus
// the per-session WebSocket stream endpoint (/sessions/{id}/stream)
// speaking Layer 2 of shared/protocol.md.
package server

import (
	"crypto/rand"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"log"
	"net/http"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"sync/atomic"
	"time"

	"github.com/gorilla/websocket"

	"claude-repld/internal/account"
	"claude-repld/internal/addsupport"
	"claude-repld/internal/login"
	"claude-repld/internal/protocol"
	"claude-repld/internal/registry"
	"claude-repld/internal/session"
	"claude-repld/internal/sidebaraction"
	"claude-repld/internal/workspacecmd"
	"claude-repld/internal/workspaces"
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
}

// SpawnFunc launches a shim for a new session; injected so tests can
// substitute an in-memory shim.
type SpawnFunc func(sessionID string, opts CreateOpts) (session.ShimHandle, error)

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
	if opts.Model != "" {
		argv = append(argv, "--model", opts.Model)
	}
	if opts.Resume != "" {
		argv = append(argv, "--resume", opts.Resume)
	}
	return argv
}

// ShimEnv assembles the KEY=VALUE overlay every shim spawn adds to the
// inherited environment. The SDK's claude subprocess inherits it from
// the shim, so this is the ONLY channel by which per-session account
// selection reaches the CLI. Shared by cmd/claude-repld and the e2e
// harness for the same reason ShimArgv is: two spawn paths that assemble
// the environment separately would drift.
//
// AGENT_REPL_OWNED marks the CLI as module-launched for the hook scripts.
// CLAUDE_CONFIG_DIR is set only when the session carries one; an empty
// ConfigDir deliberately leaves the daemon's own value (or its absence)
// inherited rather than exporting an empty override, which the CLI would
// read as a config root literally named "".
func ShimEnv(opts CreateOpts) []string {
	env := []string{"AGENT_REPL_OWNED=1"}
	if opts.ConfigDir != "" {
		env = append(env, "CLAUDE_CONFIG_DIR="+opts.ConfigDir)
	}
	return env
}

// Remediator dispatches the headless analyst for a session that has
// vanished from the daemon. Start reports whether this call is the one
// that launched it (a repeat for an already-dispatched id is a no-op).
type Remediator interface {
	Start(sessionID string) (bool, error)
}

// Server routes daemon HTTP traffic.
type Server struct {
	daemonVersion string
	bootID        string
	// binaryMTime is the Unix mtime (seconds) of the executable this daemon
	// was LAUNCHED from, captured at boot and served on GET /sessions. Emacs
	// compares it against the on-disk binary's mtime to tell a running daemon
	// apart from a freshly rebuilt binary and bounce only when genuinely
	// stale. Zero means the boot-time stat failed (staleness never asserted).
	binaryMTime int64
	retention   int
	forceFake     bool
	spawn         SpawnFunc
	logf          func(format string, args ...any)
	now           func() time.Time
	upgrader      websocket.Upgrader
	// classifyQueue / classifierModel configure the §2.13 in-flight-queue
	// classifier every real session inherits.
	classifyQueue   bool
	classifierModel string

	sentinel   session.SentinelSink
	remediator Remediator
	registry   *registry.Registry
	// workspaces relays the Emacs drawer's sidebar snapshot stream
	// (shared/protocol.md, "Workspace sidebar stream"): Emacs POSTs the
	// latest view-model and connected sidebars receive it verbatim.
	workspaces *workspaces.Hub
	// logins owns the interactive Claude login terminals, at most one per
	// account; nil makes the login routes report the capability as
	// unconfigured.
	logins *login.Manager
	// accounts is the canonical account roster; empty makes GET /accounts
	// report the capability as unconfigured.
	accounts []Account
	// draining marks a daemon-wide teardown (ShutdownAll): the session
	// deaths it causes are NOT conversation deaths, so the registrar
	// must leave their records non-terminal for the next boot to
	// rehydrate.
	draining atomic.Bool

	// idleTimeout is how long a session may go without real activity before
	// the sweeper hibernates it (frees its CLI process pair). Zero disables
	// hibernation entirely.
	idleTimeout time.Duration
	// idleSweepTicks drives the sweeper; tests inject a channel so the
	// sweep runs on demand rather than on a wall clock.
	idleSweepTicks <-chan time.Time
	// stopped is closed by ShutdownAll, ending the sweeper goroutine.
	stopped  chan struct{}
	stopOnce sync.Once

	mu       sync.Mutex
	sessions map[string]*session.Session
	// dormant holds rehydratable registry records from a previous daemon
	// process, keyed by their ORIGINAL s_ id: the id keeps resolving
	// across the restart, and the first ATTACH materializes it as a
	// hibernated session (history, no processes); only a real act spawns
	// its shim. A restart never fans out N shims eagerly.
	dormant map[string]registry.Record
	// switching marks sessions mid account-switch: their shim shutdown is
	// a planned respawn, not a conversation death, so the registrar must
	// leave their records non-terminal — the per-session analogue of the
	// daemon-wide draining flag.
	switching map[string]struct{}
}

// Config assembles a Server.
type Config struct {
	DaemonVersion string
	// BinaryMTime is the Unix mtime (seconds) of the executable the daemon
	// was launched from, stat'd once at boot (see main.launchedBinaryMTime).
	// Zero disables staleness reporting: GET /sessions still carries the
	// field, and a zero value tells Emacs not to assert staleness on a guess.
	BinaryMTime int64
	Retention   int
	Spawn         SpawnFunc
	Logf          func(format string, args ...any)
	// Now is the clock used to stamp registry records (defaults to
	// time.Now); injected for tests.
	Now func() time.Time
	// ForceFake mirrors the daemon-wide -fake flag: every session runs
	// the offline scripted SDK. The resume viability gate skips fake
	// sessions (they have no on-disk transcripts by design).
	ForceFake bool
	// Sentinel receives agent-state side-channel notifications for every
	// session; nil disables sentinel writes.
	Sentinel session.SentinelSink
	// Remediator dispatches the "session gone" analyst; nil makes
	// POST /remediation report the capability as unconfigured.
	Remediator Remediator
	// Registry persists session records across daemon restarts; nil
	// disables persistence (tests, embedded use).
	Registry *registry.Registry
	// Logins owns the interactive Claude login terminals; nil disables the
	// login routes.
	Logins *login.Manager
	// ClassifyQueue enables the §2.13 in-flight-queue classifier on every
	// real session. False leaves the queue active but every busy-submitted
	// message defaults to an immediate wait with no subprocess spawned.
	ClassifyQueue bool
	// ClassifierModel is the model the queue classifier pins; empty takes
	// the session package's haiku default.
	ClassifierModel string
	// Accounts is the canonical account roster (the -accounts flag):
	// every root a session may run as, in menu order. Empty disables
	// GET /accounts.
	Accounts []Account
	// IdleTimeout is how long a session may go without real activity (a
	// shim event, an acting client command) before the sweeper hibernates
	// it: the ~500MB node+CLI pair is freed while the conversation stays
	// listed and fully replayable, and the next act respawns it with
	// --resume. Zero disables hibernation.
	//
	// Attaching, replaying, and listing are NOT activity. A workspace the
	// user merely switches to still goes idle, which is the point: people
	// open workspaces far more often than they prompt them.
	IdleTimeout time.Duration
	// IdleSweepTicks overrides the sweeper's clock. Nil mints a real
	// ticker; tests inject a channel so a sweep runs on demand.
	IdleSweepTicks <-chan time.Time
}

// Account is one canonical config root the daemon can name and switch
// sessions onto. The set is closed by configuration (the -accounts
// flag), never discovered: WHICH roots exist is the operator's policy,
// and the daemon only reports and applies it.
type Account struct {
	// Label is the human name for the root ("personal", "work").
	Label string `json:"label"`
	// ConfigDir is the CLAUDE_CONFIG_DIR selecting the root, "" meaning
	// the CLI's own default (~/.claude) — the same convention every
	// session record uses, so identity comparison is plain equality.
	ConfigDir string `json:"config_dir"`
}

// New builds a Server.
func New(cfg Config) *Server {
	logf := cfg.Logf
	if logf == nil {
		logf = log.Printf
	}
	now := cfg.Now
	if now == nil {
		now = time.Now
	}
	s := &Server{
		daemonVersion:   cfg.DaemonVersion,
		bootID:          newBootID(),
		binaryMTime:     cfg.BinaryMTime,
		retention:       cfg.Retention,
		forceFake:       cfg.ForceFake,
		spawn:           cfg.Spawn,
		logf:            logf,
		now:             now,
		classifyQueue:   cfg.ClassifyQueue,
		classifierModel: cfg.ClassifierModel,
		sentinel:        cfg.Sentinel,
		logins:          cfg.Logins,
		accounts:        cfg.Accounts,
		remediator:      cfg.Remediator,
		registry:        cfg.Registry,
		idleTimeout:     cfg.IdleTimeout,
		idleSweepTicks:  cfg.IdleSweepTicks,
		stopped:         make(chan struct{}),
		upgrader: websocket.Upgrader{
			// The daemon is a local-loopback developer tool; the Emacs
			// xwidget origin is file-/app-scoped, so origin checks are
			// permissive by design.
			CheckOrigin: func(*http.Request) bool { return true },
		},
		workspaces: workspaces.NewHub(),
		sessions:   map[string]*session.Session{},
		dormant:    map[string]registry.Record{},
		switching:  map[string]struct{}{},
	}
	// A -fake daemon must leave the registry untouched: fake sessions
	// have no transcripts, so the prune below would destroy every REAL
	// record just because the daemon happened to boot offline.
	if s.registry != nil && !s.forceFake {
		s.loadDormant()
	} else if s.registry != nil {
		s.logf("server: -fake daemon: session registry left untouched (no rehydration)")
	}
	if s.idleTimeout > 0 || s.idleSweepTicks != nil {
		go s.runIdleSweeper()
	}
	return s
}

// loadDormant registers every rehydratable registry record under its
// original s_ id and prunes the ones no boot could ever revive: records
// that never learned their claude_session_id (no resume target exists)
// and records whose transcript is gone (the same viability gate the
// create path applies — a --resume against a missing transcript
// hard-kills the CLI). Runs at construction, before the HTTP surface is
// up, so no locking is needed.
func (s *Server) loadDormant() {
	kept, pruned := 0, 0
	prune := func(rec registry.Record, why string) {
		s.logf("registry: pruning session %s (%s)", rec.SessionID, why)
		if err := s.registry.Delete(rec.SessionID); err != nil {
			s.logf("registry: prune %s FAILED: %v", rec.SessionID, err)
			return
		}
		pruned++
	}
	for _, rec := range s.registry.All() {
		if rec.Terminal {
			continue
		}
		if rec.ClaudeSessionID == "" {
			prune(rec, "no claude_session_id ever arrived; it cannot be resumed")
			continue
		}
		path := session.TranscriptPath(session.ClaudeConfigDir(rec.ConfigDir), rec.CWD, rec.ClaudeSessionID)
		if _, err := os.Stat(path); err != nil {
			prune(rec, fmt.Sprintf("transcript %s missing: %v", path, err))
			continue
		}
		s.dormant[rec.SessionID] = rec
		kept++
	}
	if kept > 0 || pruned > 0 {
		s.logf("server: session registry: %d rehydratable session(s), %d pruned", kept, pruned)
	}
}

// registrar bridges session durable-state transitions into the
// persistent registry. A separate type keeps the session.Registrar
// methods off the Server API.
type registrar struct{ s *Server }

func (r registrar) ClaudeSessionIDChanged(sessionID, claudeSessionID string) {
	r.s.updateRegistry(sessionID, "claude_session_id", func(rec *registry.Record) {
		rec.ClaudeSessionID = claudeSessionID
	})
}

func (r registrar) ModelChanged(sessionID, model string) {
	r.s.updateRegistry(sessionID, "model", func(rec *registry.Record) {
		rec.Model = model
	})
}

func (r registrar) SessionTerminal(sessionID, deathReason string) {
	// A daemon-wide drain is not a conversation death: the registry
	// exists precisely so these sessions rehydrate on the next boot, so
	// the drain must leave their records non-terminal. An account switch
	// is the per-session analogue — its shim shutdown is a planned
	// respawn under a new root, so the record must stay non-terminal for
	// the relaunch to carry the conversation over.
	if r.s.draining.Load() || r.s.isSwitching(sessionID) {
		return
	}
	r.s.updateRegistry(sessionID, "terminal transition", func(rec *registry.Record) {
		rec.Terminal = true
		rec.DeathReason = deathReason
	})
}

// updateRegistry applies fn to id's registry record. Only sessions the
// create path registered carry a registrar, so a missing record is
// unexpected and logged, never silently dropped.
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

// Handler returns the daemon's HTTP mux.
func (s *Server) Handler() http.Handler {
	mux := http.NewServeMux()
	mux.HandleFunc("POST /sessions", s.handleCreateSession)
	mux.HandleFunc("GET /sessions", s.handleListSessions)
	mux.HandleFunc("GET /sessions/{id}/stream", s.handleStream)
	mux.HandleFunc("DELETE /sessions/{id}", s.handleDeleteSession)
	mux.HandleFunc("POST /sessions/{id}/message", s.handleSendMessage)
	mux.HandleFunc("POST /sessions/{id}/interrupt", s.handleInterrupt)
	mux.HandleFunc("POST /sessions/{id}/permission", s.handlePermissionDecision)
	mux.HandleFunc("GET /sessions/{id}/commands", s.handleCommands)
	mux.HandleFunc("GET /sessions/{id}/tasks/{taskId}/output", s.handleTaskOutput)
	mux.HandleFunc("POST /sessions/{id}/commands/refresh", s.handleRefreshCommands)
	mux.HandleFunc("POST /sessions/{id}/queue/{queueId}/run-now", s.handleQueueRunNow)
	mux.HandleFunc("POST /sessions/{id}/queue/{queueId}/cancel", s.handleQueueCancel)
	mux.HandleFunc("GET /sessions/{id}/account", s.handleAccount)
	mux.HandleFunc("POST /sessions/{id}/account", s.handleAccountSwitch)
	mux.HandleFunc("GET /accounts", s.handleAccounts)
	mux.HandleFunc("POST /sessions/{id}/login", s.handleLogin)
	mux.HandleFunc("GET /sessions/{id}/login/terminal", s.handleLoginTerminal)
	mux.HandleFunc("DELETE /sessions/{id}/login", s.handleLoginClose)
	mux.HandleFunc("GET /sessions/{id}/chess-game", s.handleChessGameFile)
	mux.HandleFunc("POST /sessions/{id}/add-support", s.handleAddSupport)
	mux.HandleFunc("POST /remediation", s.handleRemediate)
	mux.HandleFunc("POST /workspaces/status", s.handleWorkspaceStatusIngest)
	mux.HandleFunc("GET /workspaces/status", s.handleWorkspaceStatus)
	mux.HandleFunc("GET /workspaces/stream", s.handleWorkspaceStream)
	mux.HandleFunc("POST /workspaces/action", s.handleWorkspaceAction)
	return mux
}

// chessGameDirParts is the fixed worktree-relative directory the
// /show-chess-game skill writes game payload files into. The route below
// serves ONLY files directly inside it.
var chessGameDirParts = []string{".claude", "emacs", "cee-web-widget"}

// sessionCWD reports the working directory a session runs in, live or
// dormant, without rehydrating a dormant record (reading a game file
// must not spawn a shim).
func (s *Server) sessionCWD(id string) (string, bool) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if sess := s.sessions[id]; sess != nil {
		return sess.Info().CWD, true
	}
	if rec, ok := s.dormant[id]; ok {
		return rec.CWD, true
	}
	return "", false
}

// sessionDirs reports a session's working directory and its
// CLAUDE_CONFIG_DIR, resolving a live session ahead of a dormant record
// exactly as sessionCWD does. An empty ConfigDir names the CLI's own
// default root rather than "unknown".
func (s *Server) sessionDirs(id string) (cwd, configDir string, known bool) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if sess := s.sessions[id]; sess != nil {
		info := sess.Info()
		return info.CWD, info.ConfigDir, true
	}
	if rec, ok := s.dormant[id]; ok {
		return rec.CWD, rec.ConfigDir, true
	}
	return "", "", false
}

// handleAddSupport asks Emacs to open a workspace that builds graphical
// support for a slash command the CLI refused in this environment.
//
// The daemon detects nothing here and generates nothing: the webapp spots
// the refusal and the user presses the button, and workspace generation
// belongs entirely to Emacs. All this does is compose the request and drop
// it on the channel Emacs already watches, so a 202 means "asked", never
// "created" — the daemon never learns the outcome and must not imply it.
func (s *Server) handleAddSupport(w http.ResponseWriter, r *http.Request) {
	id := r.PathValue("id")
	cwd, configDir, known := s.sessionDirs(id)
	if !known {
		httpError(w, http.StatusNotFound, "no such session")
		return
	}
	// git_root is mandatory downstream and Emacs refuses a create without
	// it, so a session with no cwd cannot be served rather than guessed at.
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
	cmd := workspacecmd.NewCreate(
		addsupport.WorkspaceName(body.Command),
		cwd,
		addsupport.Prompt(body.Command, configDir),
	)
	path, err := workspacecmd.Emit(dir, []workspacecmd.Create{cmd})
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

// handleChessGameFile serves a chess-game payload file that the session's
// agent wrote under its worktree (the marker channel of the chess-board
// widget: the response text carries only the file path, and the webapp
// fetches the payload here). The path is caller-supplied, so it is
// validated down to "a chess-game-* file directly inside THIS session's
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
// answering the 404 / 503 cases shared by all three of them. The bool
// reports whether the caller should carry on.
//
// The session is resolved WITHOUT rehydrating a dormant record: the login
// is wanted precisely when the account's credentials have expired and
// sessions are dying, so spawning a shim just to read back a config dir
// already on the record would launch a doomed CLI for no reason.
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

// handleLogin opens the interactive Claude login for the account this
// session runs as, on a pty the daemon owns. The webapp then attaches to
// GET /sessions/{id}/login/terminal and renders it.
//
// Idempotent: a second click, or a second workspace on the same account,
// joins the terminal already open rather than racing a second OAuth flow.
//
// Note there is no "this session has no account" error. An empty config
// dir is a REAL account — the CLI's own default root — so every known
// session has one.
func (s *Server) handleLogin(w http.ResponseWriter, r *http.Request) {
	id := r.PathValue("id")
	configDir, ok := s.loginAccount(w, id)
	if !ok {
		return
	}
	sess, err := s.logins.Open(configDir)
	if err != nil {
		httpError(w, http.StatusInternalServerError, fmt.Sprintf("opening the login terminal: %v", err))
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
// keystrokes back.
//
// Nothing here parses the terminal. Binary frames are raw pty bytes in both
// directions; text frames are the one control message, a geometry report.
// The login is a full-screen TUI gated behind stateful prompts before it
// ever reaches OAuth, so a human reads it — the daemon only carries it.
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

	// Writer: terminal → socket. Owns the socket's write side and its
	// closure. The channel closing means the login ended, which the viewer
	// learns from the socket closing under it.
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
				// Drain so Detach's close(Out) is safe.
				for range client.Out { //nolint:revive
				}
				return
			}
		}
	}()

	// Reader: socket → child. Runs on the handler goroutine.
	for {
		kind, data, err := conn.ReadMessage()
		if err != nil {
			sess.Detach(client)
			return
		}
		switch kind {
		case websocket.BinaryMessage:
			if err := sess.Write(data); err != nil {
				s.logf("server: login keystroke: %v", err)
			}
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
				if err := sess.Resize(ctl.Resize.Rows, ctl.Resize.Cols); err != nil {
					s.logf("server: login resize: %v", err)
				}
			}
		}
	}
}

// handleLoginClose ends the login terminal for this session's account.
// Closing one that is not running is a success: the caller wanted it gone
// and it is.
func (s *Server) handleLoginClose(w http.ResponseWriter, r *http.Request) {
	id := r.PathValue("id")
	configDir, ok := s.loginAccount(w, id)
	if !ok {
		return
	}
	if err := s.logins.Close(configDir); err != nil {
		httpError(w, http.StatusInternalServerError, fmt.Sprintf("closing the login terminal: %v", err))
		return
	}
	w.WriteHeader(http.StatusNoContent)
}

// handleAccount names the Claude account this session runs as, for the
// topbar. Resolved without rehydrating, for the same reason the login is:
// which account a session belongs to matters most when it is not running.
//
// A logged-out account is a 200 with an empty email, not an error — the
// topbar renders that state rather than reporting a failure.
// handleCommands answers the session's slash-command menu, which the Emacs
// input panel completes against.
//
// An unresolved menu answers `[]` with a 200 rather than an error: the list
// lands asynchronously off the SDK's init handshake, so a client asking in
// the moments before it does is early, not broken. It is serialized as an
// empty array rather than JSON null so the reader never has to tell the two
// apart.
func (s *Server) handleCommands(w http.ResponseWriter, r *http.Request) {
	// Read-only introspection, and pollable: it serves the cached menu
	// (which survives hibernation) and must never resurrect a session.
	sess, err := s.resolveForAttach(r.PathValue("id"))
	if err != nil {
		httpError(w, http.StatusInternalServerError, err.Error())
		return
	}
	if sess == nil {
		httpError(w, http.StatusNotFound, "no such session")
		return
	}
	commands := sess.Commands()
	if commands == nil {
		commands = []protocol.SlashCommand{}
	}
	w.Header().Set("Content-Type", "application/json")
	writeJSON(w, s.logf, map[string]any{"commands": commands})
}

// handleTaskOutput serves a bounded, session-scoped tail of a detached
// task's output file (§ watcher-bubble expansion). Read-only and pollable
// exactly like handleCommands: it resolves for ATTACH, so a webapp polling
// an expanded watcher fold never resurrects a hibernated session, and it
// serves only a path the session recorded for itself (see Session.TaskOutput).
//
// The `offset` query param is the byte cursor the client advances across
// polls; the response carries the next cursor, whether the task has
// completed, and a live elapsed the frozen SDK heartbeat no longer feeds.
func (s *Server) handleTaskOutput(w http.ResponseWriter, r *http.Request) {
	sess, err := s.resolveForAttach(r.PathValue("id"))
	if err != nil {
		httpError(w, http.StatusInternalServerError, err.Error())
		return
	}
	if sess == nil {
		httpError(w, http.StatusNotFound, "no such session")
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
	text, next, done, elapsedMs, ok := sess.TaskOutput(r.PathValue("taskId"), offset)
	if !ok {
		httpError(w, http.StatusNotFound, "no such task")
		return
	}
	w.Header().Set("Content-Type", "application/json")
	writeJSON(w, s.logf, map[string]any{
		"text":       text,
		"offset":     next,
		"done":       done,
		"elapsed_ms": elapsedMs,
	})
}

// handleRefreshCommands asks the shim to re-resolve the menu, and returns
// immediately without waiting for it.
//
// The refresh is asynchronous by design. It costs the shim a process spawn
// (the SDK memoizes the list against its init handshake, so re-resolving
// means performing a fresh one), and its caller is a filesystem watcher
// reacting to a skill being edited — nobody is blocked on the answer, so
// making them wait for it would buy nothing. The fresh list lands on the
// cache when the shim's `commands` event arrives, and the next GET sees it.
func (s *Server) handleRefreshCommands(w http.ResponseWriter, r *http.Request) {
	// ATTACH, not act: the caller is a filesystem watcher reacting to a
	// skill edit, so reviving here would resurrect every hibernated
	// session on any skill change. A hibernated session needs no refresh
	// now — it re-resolves the menu from a fresh init handshake when it is
	// next revived — so the refresh is a no-op for it.
	sess, err := s.resolveForAttach(r.PathValue("id"))
	if err != nil {
		httpError(w, http.StatusInternalServerError, err.Error())
		return
	}
	if sess == nil {
		httpError(w, http.StatusNotFound, "no such session")
		return
	}
	if sess.Hibernated() {
		w.WriteHeader(http.StatusAccepted)
		return
	}
	if err := sess.InjectCommand(map[string]any{
		"type":       "refresh-commands",
		"request_id": newRequestID(),
	}); err != nil {
		httpError(w, http.StatusConflict, err.Error())
		return
	}
	w.WriteHeader(http.StatusAccepted)
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
		httpError(w, http.StatusInternalServerError, err.Error())
		return
	}
	w.Header().Set("Content-Type", "application/json")
	writeJSON(w, s.logf, identity)
}

// accountStatus is one roster entry as GET /accounts reports it: the
// configured root plus who it is currently logged in as.
type accountStatus struct {
	Account
	// Email is the root's logged-in account, "" when logged out.
	Email string `json:"email"`
	// Error carries a per-root identity read failure (corrupt
	// .claude.json). Surfaced in-band so one broken root cannot hide the
	// healthy ones, and never blanked into a fake "logged out".
	Error string `json:"error,omitempty"`
}

// handleAccounts reports the canonical account roster with each root's
// live identity, for the topbar's account menu. The roster is
// configuration (the -accounts flag), so an empty one is the capability
// being unconfigured, not an empty success.
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

// switchDrainTimeout bounds how long an account switch waits for the old
// shim to drain before giving up. A package var so tests can shorten it.
var switchDrainTimeout = 10 * time.Second

// isSwitching reports whether id's shim shutdown is a planned
// account-switch respawn rather than a conversation death.
func (s *Server) isSwitching(id string) bool {
	s.mu.Lock()
	defer s.mu.Unlock()
	_, ok := s.switching[id]
	return ok
}

// setSwitching marks or clears id's account-switch window.
func (s *Server) setSwitching(id string, on bool) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if on {
		s.switching[id] = struct{}{}
	} else {
		delete(s.switching, id)
	}
}

// migrateTranscript copies the conversation's durable transcript into
// the target root so a --resume there finds it. An already-present dst
// wins — switching BACK to a root the conversation has lived under must
// not overwrite the newer history there with the stale copy.
func migrateTranscript(src, dst string) error {
	if _, err := os.Stat(dst); err == nil {
		return nil
	}
	in, err := os.Open(src) //nolint:gosec // path is daemon-derived, not user input
	if err != nil {
		return fmt.Errorf("open source transcript: %w", err)
	}
	defer func() {
		// Read-side close: the copy's success is decided by out's write
		// and close below, so this error is log-free but non-fatal.
		_ = in.Close()
	}()
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

// handleAccountSwitch moves a session onto another canonical account
// root: migrate the transcript so --resume finds it, stop the old shim
// NON-terminally, and relaunch under the target CLAUDE_CONFIG_DIR with
// the same s_ id — the id every frontend holds keeps resolving, exactly
// as it does across a daemon restart.
//
// The target must be on the -accounts roster: which roots exist is
// operator policy, and a typo'd free-form dir would strand the session
// on an unauthenticated root.
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

	sess := s.lookup(id)
	if sess != nil && sess.Info().TurnActive {
		httpError(w, http.StatusConflict, "a turn is in flight; retry when it settles")
		return
	}

	// The live translator's claude_session_id is fresher than the
	// registry's (a --resume mints a new id that write-through may still
	// be chasing), and the transcript to migrate is the CURRENT one.
	csid := rec.ClaudeSessionID
	if sess != nil {
		if live := sess.Info().ClaudeSessionID; live != "" {
			csid = live
		}
	}
	if csid != "" {
		src := session.TranscriptPath(session.ClaudeConfigDir(rec.ConfigDir), rec.CWD, csid)
		dst := session.TranscriptPath(session.ClaudeConfigDir(target.ConfigDir), rec.CWD, csid)
		if err := migrateTranscript(src, dst); err != nil {
			httpError(w, http.StatusConflict, fmt.Sprintf("transcript migration: %v", err))
			return
		}
	}

	// Stop the old shim inside the switching window, so its terminal
	// transition registers as a planned respawn rather than a
	// conversation death.
	if sess != nil {
		s.setSwitching(id, true)
		defer s.setSwitching(id, false)
		if err := sess.Shutdown("account switch"); err != nil {
			httpError(w, http.StatusInternalServerError, fmt.Sprintf("shutdown for account switch: %v", err))
			return
		}
		select {
		case <-sess.Done():
		case <-time.After(switchDrainTimeout):
			httpError(w, http.StatusGatewayTimeout, "old shim did not drain in time; the session is unchanged")
			return
		}
	}

	// Persist the new root (and the freshest claude_session_id) BEFORE
	// the relaunch: if the spawn fails, the record still rehydrates
	// under the target root on the next access instead of the old one.
	s.updateRegistry(id, "account switch", func(rec *registry.Record) {
		rec.ConfigDir = target.ConfigDir
		if csid != "" {
			rec.ClaudeSessionID = csid
		}
	})

	// Relaunch under the same id. A session that never reported a
	// claude_session_id has no transcript to carry, so it starts fresh —
	// there is no history to lose.
	opts := CreateOpts{
		CWD:            rec.CWD,
		Model:          rec.Model,
		PermissionMode: rec.PermissionMode,
		ConfigDir:      target.ConfigDir,
		Resume:         csid,
	}
	fresh, err := s.launchSession(id, opts, true)
	if err != nil {
		httpError(w, http.StatusInternalServerError, fmt.Sprintf("relaunch under %q: %v", target.ConfigDir, err))
		return
	}
	s.mu.Lock()
	delete(s.dormant, id)
	s.sessions[id] = fresh
	s.mu.Unlock()
	go fresh.Run()
	s.logf("session %s: switched to account %q (%s), resume %s", id, target.Label, target.ConfigDir, csid)
	// Poke Emacs over the sentinel side channel: its per-workspace
	// config-dir override must follow the switch, or its own next
	// create/reattach would put the session back on the computed default.
	if s.sentinel != nil {
		s.sentinel.AccountChanged(rec.CWD, csid)
	}
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
// identity, so the caller can render the account it now runs as.
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

// sessionConfigDir returns id's CLAUDE_CONFIG_DIR — the ACCOUNT it runs as
// — and whether the daemon knows id at all. Consults live sessions and
// dormant records alike, and unlike resolve it never rehydrates.
//
// An empty config dir is a real answer, not a missing one: it names the
// CLI's own default root, which is why the bool carries the "unknown"
// case rather than the string.
func (s *Server) sessionConfigDir(id string) (string, bool) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if sess := s.sessions[id]; sess != nil {
		return sess.Info().ConfigDir, true
	}
	if rec, ok := s.dormant[id]; ok {
		return rec.ConfigDir, true
	}
	return "", false
}

// handleRemediate dispatches the "session gone" analyst. The frontend
// calls it the moment its existence probe reports the daemon no longer
// knows the session it is holding — the id is by definition absent from
// the session map, so this route is daemon-scoped rather than
// session-scoped.
func (s *Server) handleRemediate(w http.ResponseWriter, r *http.Request) {
	var body struct {
		SessionID string `json:"session_id"`
	}
	if err := json.NewDecoder(r.Body).Decode(&body); err != nil {
		httpError(w, http.StatusBadRequest, fmt.Sprintf("invalid request body: %v", err))
		return
	}
	if body.SessionID == "" {
		httpError(w, http.StatusBadRequest, "session_id must be non-empty")
		return
	}
	// A session the daemon still serves — live or dormant-rehydratable —
	// is not gone, so a remediation request naming one is a frontend
	// bug: refuse rather than burn an analyst on a healthy session.
	if s.known(body.SessionID) {
		httpError(w, http.StatusConflict, "session is alive; nothing to remediate")
		return
	}
	if s.remediator == nil {
		httpError(w, http.StatusServiceUnavailable, "remediation is not configured")
		return
	}
	started, err := s.remediator.Start(body.SessionID)
	if err != nil {
		httpError(w, http.StatusInternalServerError, err.Error())
		return
	}
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusAccepted)
	writeJSON(w, s.logf, map[string]bool{"started": started})
}

// handleSendMessage injects a user turn over HTTP — the send path for
// clients that hold no WebSocket (the Emacs input buffer). The turn
// flows through the exact same pipeline as a WS-submitted user-message,
// so connected tabs still get the user-turn broadcast and replay
// retention sees it.
func (s *Server) handleSendMessage(w http.ResponseWriter, r *http.Request) {
	// An ACT: this is the prompt, so it is exactly what earns a CLI back.
	sess, err := s.resolveForAct(r.PathValue("id"))
	if err != nil {
		httpError(w, http.StatusInternalServerError, err.Error())
		return
	}
	if sess == nil {
		httpError(w, http.StatusNotFound, "no such session")
		return
	}
	var body struct {
		Content   string `json:"content"`
		RequestID string `json:"request_id"`
	}
	if err := json.NewDecoder(r.Body).Decode(&body); err != nil {
		httpError(w, http.StatusBadRequest, fmt.Sprintf("invalid request body: %v", err))
		return
	}
	if strings.TrimSpace(body.Content) == "" {
		httpError(w, http.StatusBadRequest, "content must be non-empty")
		return
	}
	if body.RequestID == "" {
		body.RequestID = newRequestID()
	}
	if err := sess.InjectCommand(map[string]any{
		"type":       "user-message",
		"request_id": body.RequestID,
		"content":    body.Content,
	}); err != nil {
		httpError(w, http.StatusConflict, err.Error())
		return
	}
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusAccepted)
	writeJSON(w, s.logf, map[string]string{"request_id": body.RequestID})
}

// handleInterrupt aborts the in-flight turn over HTTP (the Emacs-side
// C-c C-k path). Same pipeline as a WS interrupt: pending permission
// prompts cancel and the turn's result arrives as "aborted".
//
// An optional `retract_request_id` body field asks, additionally, to withdraw
// that turn's prompt bubble when the agent never answered it — the undo half
// of Emacs's C-c C-k, which lands the prompt back in its input buffer. The
// reply's `retracted` says whether that happened, so the caller only restores
// a prompt the feed actually gave up. A body is optional entirely: a bare
// interrupt keeps its original no-body contract.
func (s *Server) handleInterrupt(w http.ResponseWriter, r *http.Request) {
	// An ACT: it reaches the CLI, so the CLI has to exist.
	sess, err := s.resolveForAct(r.PathValue("id"))
	if err != nil {
		httpError(w, http.StatusInternalServerError, err.Error())
		return
	}
	if sess == nil {
		httpError(w, http.StatusNotFound, "no such session")
		return
	}
	var body struct {
		RetractRequestID string `json:"retract_request_id"`
	}
	// EOF is the no-body interrupt, which is a valid call rather than a
	// malformed one; any other decode failure is the client's bug to hear.
	if err := json.NewDecoder(r.Body).Decode(&body); err != nil && !errors.Is(err, io.EOF) {
		httpError(w, http.StatusBadRequest, fmt.Sprintf("invalid request body: %v", err))
		return
	}
	if err := sess.InjectCommand(map[string]any{
		"type":       "interrupt",
		"request_id": newRequestID(),
	}); err != nil {
		httpError(w, http.StatusConflict, err.Error())
		return
	}
	// Strictly after the interrupt: retracting a turn still streaming toward
	// the client would drop the prompt out from under a live answer.
	retracted := sess.RetractActiveTurn(body.RetractRequestID)
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusAccepted)
	writeJSON(w, s.logf, map[string]bool{"retracted": retracted})
}

// handlePermissionDecision resolves a pending permission prompt over
// HTTP — the send path for clients that hold no WebSocket (the sidebar's
// one-click approve/deny buttons). The decision flows through the exact
// same pipeline as a WS-submitted permission-decision, so connected tabs
// still get the permission-resolved broadcast and the shim's own gating
// still owns unknown or already-resolved request ids (surfaced as a 409,
// like every other rejected injection).
func (s *Server) handlePermissionDecision(w http.ResponseWriter, r *http.Request) {
	// An ACT: permission-decision must reach a live CLI
	// (protocol.CommandActs), so a hibernated session revives exactly as
	// it would for the same frame arriving over the WS path.
	sess, err := s.resolveForAct(r.PathValue("id"))
	if err != nil {
		httpError(w, http.StatusInternalServerError, err.Error())
		return
	}
	if sess == nil {
		httpError(w, http.StatusNotFound, "no such session")
		return
	}
	var body struct {
		RequestID string `json:"request_id"`
		Behavior  string `json:"behavior"`
	}
	if err := json.NewDecoder(r.Body).Decode(&body); err != nil {
		httpError(w, http.StatusBadRequest, fmt.Sprintf("invalid request body: %v", err))
		return
	}
	if body.RequestID == "" {
		httpError(w, http.StatusBadRequest, "request_id must be non-empty")
		return
	}
	if body.Behavior != "allow" && body.Behavior != "deny" {
		httpError(w, http.StatusBadRequest, fmt.Sprintf("behavior must be allow|deny, got %q", body.Behavior))
		return
	}
	decision := map[string]any{"behavior": body.Behavior}
	if body.Behavior == "deny" {
		// The wire contract requires every deny to carry a message
		// (protocol.DecodeCommand enforces it), but this route's body has
		// none, so the daemon supplies the sidebar's — mirroring the
		// webapp's own "denied from webapp".
		decision["message"] = "denied from sidebar"
	}
	if err := sess.InjectCommand(map[string]any{
		"type":       "permission-decision",
		"request_id": body.RequestID,
		"decision":   decision,
	}); err != nil {
		httpError(w, http.StatusConflict, err.Error())
		return
	}
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusAccepted)
	writeJSON(w, s.logf, map[string]string{"request_id": body.RequestID})
}

// handleQueueRunNow escalates a queued message over HTTP (§2.13): the
// Emacs-host equivalent of the webapp's Run-now control. Same pipeline as
// a WS queue-run-now: the item is promoted and the running turn preempted.
func (s *Server) handleQueueRunNow(w http.ResponseWriter, r *http.Request) {
	s.queueOverride(w, r, "queue-run-now")
}

// handleQueueCancel drops a queued message over HTTP (§2.13): the
// Emacs-host equivalent of the webapp's Cancel control.
func (s *Server) handleQueueCancel(w http.ResponseWriter, r *http.Request) {
	s.queueOverride(w, r, "queue-cancel")
}

// queueOverride injects a §2.13 queue override (queue-run-now /
// queue-cancel) for the named item through the shared client-command
// pipeline. A stale queue_id is a no-op ack inside the session, so a
// success here means "dispatched", not "the item still existed".
func (s *Server) queueOverride(w http.ResponseWriter, r *http.Request, cmdType string) {
	// An explicit user act (the Emacs Cancel / run-now control) that runs
	// through the client-command pipeline, so it needs a live shim: revive
	// a hibernated session rather than reject the override.
	sess, err := s.resolveForAct(r.PathValue("id"))
	if err != nil {
		httpError(w, http.StatusInternalServerError, err.Error())
		return
	}
	if sess == nil {
		httpError(w, http.StatusNotFound, "no such session")
		return
	}
	queueID := r.PathValue("queueId")
	if queueID == "" {
		httpError(w, http.StatusBadRequest, "queue id must be non-empty")
		return
	}
	if err := sess.InjectCommand(map[string]any{
		"type":       cmdType,
		"request_id": newRequestID(),
		"queue_id":   queueID,
	}); err != nil {
		httpError(w, http.StatusConflict, err.Error())
		return
	}
	w.WriteHeader(http.StatusAccepted)
}

func (s *Server) handleCreateSession(w http.ResponseWriter, r *http.Request) {
	var opts CreateOpts
	if r.Body != nil {
		if err := json.NewDecoder(r.Body).Decode(&opts); err != nil && err.Error() != "EOF" {
			httpError(w, http.StatusBadRequest, fmt.Sprintf("invalid request body: %v", err))
			return
		}
	}
	if opts.PermissionMode != "" && !protocol.ValidPermissionMode(opts.PermissionMode) {
		httpError(w, http.StatusBadRequest, fmt.Sprintf("invalid permission_mode %q", opts.PermissionMode))
		return
	}
	// Resume viability gate: the CLI hard-exits (fatal_error) when asked
	// to --resume a session id with no transcript in this daemon's
	// config dir — e.g. an id minted inside the Docker sandbox, under
	// another CLAUDE_CONFIG_DIR, or one whose transcript was deleted.
	// Silently downgrading to a FRESH conversation (the old behavior)
	// buries a genuinely lost session, so instead HARD-FAIL the create
	// before spawning anything: the response carries a machine-detectable
	// code plus the resume id and every path stat'd, so the Emacs client
	// opens an investigation workspace for the lost session and surfaces
	// the loss loudly rather than either running a doomed --resume or
	// papering over it with a blank conversation. Fake sessions skip the
	// gate — the scripted SDK has no transcripts by design.
	if opts.Resume != "" && !opts.Fake && !s.forceFake {
		path := session.TranscriptPath(session.ClaudeConfigDir(opts.ConfigDir), opts.CWD, opts.Resume)
		if _, statErr := os.Stat(path); statErr != nil {
			s.logf("session create REJECTED: resume target %s has no transcript at %s — hard-failing so the client opens an investigation workspace instead of a doomed --resume or a silent fresh start: %v",
				opts.Resume, path, statErr)
			writeResumeTranscriptMissing(w, opts.Resume, []string{path})
			return
		}
	}
	// A transcript takes exactly one writer, and this create is the newest
	// claim on it, so any older session still holding it stands down BEFORE
	// a second CLI exists (see supersede.go). Placed after the viability
	// gate so a create that is about to be rejected never tears down a
	// healthy session on its way out.
	s.supersedeResumeConflicts(opts)
	id := newSessionID()
	// Register BEFORE launch: transcript seeding fires the registrar's
	// claude_session_id write-through, which updates this record. Fake
	// sessions are never registered — they have no durable transcript,
	// so a record could only ever rehydrate into a doomed --resume.
	registrable := s.registry != nil && !opts.Fake && !s.forceFake
	if registrable {
		if err := s.registry.Put(registry.Record{
			SessionID:       id,
			CWD:             opts.CWD,
			Model:           opts.Model,
			PermissionMode:  opts.PermissionMode,
			ConfigDir:       opts.ConfigDir,
			ClaudeSessionID: opts.Resume,
			CreatedAt:       s.now().UTC().Format(time.RFC3339),
		}); err != nil {
			s.logf("session %s: registry write on create FAILED — the session will not survive a daemon restart: %v", id, err)
		}
	}
	sess, err := s.launchSession(id, opts, registrable)
	if err != nil {
		httpError(w, http.StatusInternalServerError, fmt.Sprintf("spawn shim: %v", err))
		return
	}
	s.mu.Lock()
	s.sessions[id] = sess
	s.mu.Unlock()
	go sess.Run()

	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusCreated)
	writeJSON(w, s.logf, map[string]string{
		"session_id": id,
		"stream_url": fmt.Sprintf("/sessions/%s/stream", id),
	})
}

// launchSession spawns the shim for id and assembles its session hub,
// seeding the replay ring from the resumed transcript when opts.Resume
// is set. Shared by the create path and the restart-rehydration path so
// the two cannot drift. The caller registers the session in the map and
// starts Run.
func (s *Server) launchSession(id string, opts CreateOpts, registrable bool) (*session.Session, error) {
	shim, err := s.spawn(id, opts)
	if err != nil {
		return nil, err
	}
	sess := session.New(s.sessionConfig(id, opts, registrable, shim))
	s.seedFromTranscript(sess, id, opts)
	return sess, nil
}

// buildHibernated assembles a session hub with NO shim: full replayable
// history seeded from the transcript, and zero processes.
//
// This is what a dormant record becomes when someone merely LOOKS at its
// workspace. Attaching to it replays the whole conversation from the ring
// without spawning a CLI, which is the entire point: switching to a
// workspace has to stay free, or the savings evaporate the moment a user
// clicks through their worktrees. The CLI arrives only on the first act,
// via reviveLocked.
func (s *Server) buildHibernated(id string, opts CreateOpts, registrable bool) *session.Session {
	sess := session.New(s.sessionConfig(id, opts, registrable, nil))
	s.seedFromTranscript(sess, id, opts)
	return sess
}

// sessionConfig assembles the session hub's config. Shared by the live
// path (launchSession) and the no-shim path (buildHibernated) so the two
// cannot drift; a nil shim yields a session born hibernated.
func (s *Server) sessionConfig(id string, opts CreateOpts, registrable bool, shim session.ShimHandle) session.Config {
	cfg := session.Config{
		ID:              id,
		DaemonVersion:   s.daemonVersion,
		BootID:          s.bootID,
		Shim:            shim,
		CWD:             opts.CWD,
		Model:           opts.Model,
		PermissionMode:  opts.PermissionMode,
		ConfigDir:       opts.ConfigDir,
		Retention:       s.retention,
		Logf:            s.logf,
		Sentinel:        s.sentinel,
		ClassifyQueue:   s.classifyQueue,
		ClassifierModel: s.classifierModel,
		// Share the server's clock so a session's idle stamp and the
		// sweeper's idle check read the same time — identical to time.Now
		// in production, injectable together under test.
		Now: s.now,
	}
	// A fake session's CLI is a scripted stand-in that writes no
	// transcript, so there is nothing to reconcile its model against; a
	// reconciler here could only log, twice a minute, that the file it
	// wants does not exist.
	if opts.Fake || s.forceFake {
		cfg.ModelReconcileInterval = -1
		// The queue classifier spawns a REAL claude; a fake/offline session
		// must never reach the network, so it enqueues busy messages as
		// immediate waits instead of classifying them.
		cfg.ClassifyQueue = false
	}
	if registrable {
		cfg.Registrar = registrar{s}
	}
	return cfg
}

// seedFromTranscript fills the replay ring from the durable transcript
// BEFORE Run: the CLI re-emits no history on --resume, so without this
// every rebind (daemon restart, frontend switch, hibernation attach)
// would show a blank conversation.
func (s *Server) seedFromTranscript(sess *session.Session, id string, opts CreateOpts) {
	if opts.Resume == "" {
		return
	}
	path := session.TranscriptPath(session.ClaudeConfigDir(opts.ConfigDir), opts.CWD, opts.Resume)
	if err := sess.SeedFromTranscript(path, opts.Resume); err != nil {
		s.logf("session %s: transcript replay seed from %s failed (history will not render): %v", id, path, err)
		return
	}
	s.logf("session %s: replay seeded from %s", id, path)
}

func (s *Server) handleListSessions(w http.ResponseWriter, _ *http.Request) {
	s.mu.Lock()
	type entry struct {
		SessionID string `json:"session_id"`
		Terminal  bool   `json:"terminal"`
		CWD       string `json:"cwd,omitempty"`
		Model     string `json:"model,omitempty"`
		// ClaudeSessionID is the durable CLI session uuid (resume
		// target); empty until the SDK's system:init has arrived.
		ClaudeSessionID string `json:"claude_session_id,omitempty"`
		// DeathReason classifies a terminal session's end (closed
		// reason or "shim_died"); absent while alive.
		DeathReason string `json:"death_reason,omitempty"`
		// TurnActive reports whether a user turn is in flight.
		TurnActive bool `json:"turn_active"`
		// TurnPreview is the tail (≤ 200 chars) of the active turn's
		// streamed assistant text, "" when no turn is active — the
		// sidebar roster's live-turn preview. Always present: the empty
		// string is a real answer ("nothing to preview"), not a gap.
		TurnPreview string `json:"turn_preview"`
		// TotalCostUSD is the cumulative cost of the session's completed
		// turns, accumulated from result frames. Live sessions only; a
		// dormant record reports 0.
		TotalCostUSD float64 `json:"total_cost_usd"`
		// PendingPermissions lists unresolved permission request ids.
		PendingPermissions []string `json:"pending_permissions,omitempty"`
		// Queue is the in-flight message queue snapshot (§2.13),
		// front-to-back; absent when empty.
		Queue []protocol.QueuedItem `json:"queue,omitempty"`
		// Rehydratable marks a cold session carried over from a previous
		// daemon process: its id resolves, but the shim spawns (with
		// --resume) only on first real access.
		Rehydratable bool `json:"rehydratable,omitempty"`
		// Hibernated marks a session whose CLI has been freed for idleness
		// while its conversation stays fully replayable. It is NOT
		// terminal: it lists, it answers replay, and it revives on the
		// next act. Frontends must keep treating it as a live session, so
		// terminal stays false — this field is observability only.
		Hibernated bool `json:"hibernated,omitempty"`
	}
	list := make([]entry, 0, len(s.sessions)+len(s.dormant))
	for id, sess := range s.sessions {
		info := sess.Info()
		list = append(list, entry{
			SessionID:          id,
			Terminal:           info.Terminal,
			CWD:                info.CWD,
			Model:              info.Model,
			ClaudeSessionID:    info.ClaudeSessionID,
			DeathReason:        info.DeathReason,
			TurnActive:         info.TurnActive,
			TurnPreview:        info.TurnPreview,
			TotalCostUSD:       info.TotalCostUSD,
			PendingPermissions: info.PendingPermissions,
			Queue:              info.Queue,
			Hibernated:         info.Hibernated,
		})
	}
	for id, rec := range s.dormant {
		list = append(list, entry{
			SessionID:       id,
			CWD:             rec.CWD,
			Model:           rec.Model,
			ClaudeSessionID: rec.ClaudeSessionID,
			Rehydratable:    true,
		})
	}
	s.mu.Unlock()
	w.Header().Set("Content-Type", "application/json")
	writeJSON(w, s.logf, map[string]any{
		"sessions": list,
		// Instance identity + wire version: clients watch boot_id to
		// detect a daemon bounce and protocol_version to detect skew.
		"boot_id":          s.bootID,
		"protocol_version": protocol.Layer2Version,
		// Launched-binary mtime: Emacs compares it against the on-disk
		// binary to bounce the daemon only when the build genuinely moved
		// ahead of the running process (staleness bounce on Emacs open).
		"daemon_binary_mtime": s.binaryMTime,
	})
}

func (s *Server) handleDeleteSession(w http.ResponseWriter, r *http.Request) {
	id := r.PathValue("id")
	// Deleting a dormant session needs no shim: drop the record so the
	// id stops resolving and never rehydrates again.
	s.mu.Lock()
	if _, ok := s.dormant[id]; ok {
		delete(s.dormant, id)
		s.mu.Unlock()
		if err := s.registry.Delete(id); err != nil {
			httpError(w, http.StatusInternalServerError, fmt.Sprintf("registry delete: %v", err))
			return
		}
		w.WriteHeader(http.StatusNoContent)
		return
	}
	s.mu.Unlock()
	sess := s.lookup(id)
	if sess == nil {
		httpError(w, http.StatusNotFound, "no such session")
		return
	}
	if err := sess.Shutdown("DELETE /sessions"); err != nil {
		httpError(w, http.StatusInternalServerError, fmt.Sprintf("shutdown: %v", err))
		return
	}
	w.WriteHeader(http.StatusNoContent)
}

func (s *Server) handleStream(w http.ResponseWriter, r *http.Request) {
	// ATTACH, not act: opening the socket is what a frontend does the
	// instant a workspace appears on screen. It must stay free — a
	// hibernated session serves its whole history from the ring, and only
	// a command arriving on this socket (below) can spawn a CLI.
	sess, err := s.resolveForAttach(r.PathValue("id"))
	if err != nil {
		httpError(w, http.StatusInternalServerError, err.Error())
		return
	}
	if sess == nil {
		httpError(w, http.StatusNotFound, "no such session")
		return
	}
	conn, err := s.upgrader.Upgrade(w, r, nil)
	if err != nil {
		s.logf("server: websocket upgrade: %v", err)
		return
	}

	// Replay-sized: the buffer must absorb a full-ring replay burst
	// (transcript-seeded sessions replay thousands of frames on attach).
	client := sess.NewReplayClient()
	sess.Attach(client)

	// Writer: session frame queue → socket. Owns the socket's write side
	// and its closure.
	go func() {
		defer func() {
			if err := conn.Close(); err != nil {
				s.logf("server: websocket close: %v", err)
			}
		}()
		for data := range client.Send {
			if err := conn.WriteMessage(websocket.TextMessage, data); err != nil {
				s.logf("server: websocket write: %v", err)
				sess.Detach(client)
				// Drain remaining frames so Detach's close(Send) is safe.
				for range client.Send { //nolint:revive
				}
				return
			}
		}
	}()

	// Reader: socket → session commands. Runs on the handler goroutine.
	for {
		_, data, err := conn.ReadMessage()
		if err != nil {
			sess.Detach(client)
			return
		}
		// An acting frame (the webapp's own composer POSTs its prompt over
		// this socket) revives a hibernated session before it is handled —
		// this is the second act-path, alongside HTTP send/interrupt. A
		// replay-request is NOT acting, so a client that merely reconnects
		// and rebuilds history never resurrects the CLI.
		if protocol.CommandActs(data) && sess.Hibernated() {
			if _, err := s.resolveForAct(sess.ID); err != nil {
				s.logf("server: revive on client frame failed: %v", err)
			}
		}
		if err := sess.HandleClientFrame(client, data); err != nil {
			s.logf("server: client frame rejected: %v", err)
		}
	}
}

// maxSnapshotBytes caps a POST /workspaces/status body. A drawer
// view-model is a few KB; 8 MiB is generous headroom that still keeps a
// runaway client from ballooning daemon memory, since the hub retains
// the whole body as the latest snapshot.
const maxSnapshotBytes = 8 << 20

// handleWorkspaceStatusIngest accepts the Emacs drawer's latest sidebar
// snapshot. Validation is shape-only — valid JSON whose top-level type
// tags it as a workspace-snapshot — because the daemon relays the
// view-model without interpreting it (Emacs owns every fact in it).
func (s *Server) handleWorkspaceStatusIngest(w http.ResponseWriter, r *http.Request) {
	body, err := io.ReadAll(http.MaxBytesReader(w, r.Body, maxSnapshotBytes))
	if err != nil {
		var tooLarge *http.MaxBytesError
		if errors.As(err, &tooLarge) {
			httpError(w, http.StatusRequestEntityTooLarge,
				fmt.Sprintf("snapshot exceeds the %d-byte cap", int64(maxSnapshotBytes)))
			return
		}
		httpError(w, http.StatusBadRequest, fmt.Sprintf("read request body: %v", err))
		return
	}
	var probe struct {
		Type string `json:"type"`
	}
	if err := json.Unmarshal(body, &probe); err != nil {
		httpError(w, http.StatusBadRequest, fmt.Sprintf("invalid snapshot JSON: %v", err))
		return
	}
	if probe.Type != "workspace-snapshot" {
		httpError(w, http.StatusBadRequest,
			fmt.Sprintf(`snapshot type must be "workspace-snapshot", got %q`, probe.Type))
		return
	}
	s.workspaces.SetLatest(body)
	w.WriteHeader(http.StatusNoContent)
}

// handleWorkspaceStatus serves the latest sidebar snapshot one-shot: the
// poll/bootstrap path for clients that hold no stream socket.
func (s *Server) handleWorkspaceStatus(w http.ResponseWriter, _ *http.Request) {
	data := s.workspaces.Latest()
	if data == nil {
		httpError(w, http.StatusNotFound, "no workspace snapshot has been ingested")
		return
	}
	w.Header().Set("Content-Type", "application/json")
	if _, err := w.Write(data); err != nil {
		s.logf("server: write workspace snapshot: %v", err)
	}
}

// handleWorkspaceStream streams sidebar snapshots over a WebSocket: the
// latest one immediately on attach (when one exists), then every newly
// ingested snapshot as its own text message. Inbound messages are read
// and discarded by contract — the read loop exists only to observe the
// close.
func (s *Server) handleWorkspaceStream(w http.ResponseWriter, r *http.Request) {
	conn, err := s.upgrader.Upgrade(w, r, nil)
	if err != nil {
		s.logf("server: websocket upgrade: %v", err)
		return
	}

	// Attach primes the client with the latest snapshot, so a fresh
	// sidebar renders without waiting for the next Emacs push.
	client := workspaces.NewClient()
	s.workspaces.Attach(client)

	// Writer: hub snapshot queue → socket. Owns the socket's write side
	// and its closure.
	go func() {
		defer func() {
			if err := conn.Close(); err != nil {
				s.logf("server: websocket close: %v", err)
			}
		}()
		for data := range client.Send {
			if err := conn.WriteMessage(websocket.TextMessage, data); err != nil {
				s.logf("server: websocket write: %v", err)
				s.workspaces.Detach(client)
				// Drain remaining snapshots so Detach's close(Send) is safe.
				for range client.Send { //nolint:revive
				}
				return
			}
		}
	}()

	// Reader: runs on the handler goroutine, solely to detect the close.
	for {
		if _, _, err := conn.ReadMessage(); err != nil {
			s.workspaces.Detach(client)
			return
		}
	}
}

// handleWorkspaceAction accepts one sidebar action and relays it to
// Emacs as a sidebar-action file (see internal/sidebaraction). The 202
// carries the minted id so the frontend can match the outcome Emacs
// reports through the snapshot's last_action_result; whether the action
// is known, permitted, or successful is entirely Emacs's business.
func (s *Server) handleWorkspaceAction(w http.ResponseWriter, r *http.Request) {
	var body struct {
		Action    string         `json:"action"`
		Targets   []string       `json:"targets"`
		Args      map[string]any `json:"args"`
		Confirmed bool           `json:"confirmed"`
	}
	if err := json.NewDecoder(r.Body).Decode(&body); err != nil {
		httpError(w, http.StatusBadRequest, fmt.Sprintf("invalid request body: %v", err))
		return
	}
	if body.Action == "" {
		httpError(w, http.StatusBadRequest, "action must be non-empty")
		return
	}
	dir, err := sidebaraction.Dir()
	if err != nil {
		s.logf("server: resolve sidebar-actions dir: %v", err)
		httpError(w, http.StatusInternalServerError, err.Error())
		return
	}
	id, err := sidebaraction.NewID()
	if err != nil {
		httpError(w, http.StatusInternalServerError, err.Error())
		return
	}
	path, err := sidebaraction.Emit(dir, sidebaraction.Action{
		ID:        id,
		Action:    body.Action,
		Targets:   body.Targets,
		Args:      body.Args,
		Confirmed: body.Confirmed,
		TS:        s.now().Unix(),
	})
	if err != nil {
		s.logf("server: emit sidebar action %q: %v", body.Action, err)
		httpError(w, http.StatusInternalServerError, err.Error())
		return
	}
	s.logf("server: asked Emacs to run sidebar action %q (%s)", body.Action, path)
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusAccepted)
	writeJSON(w, s.logf, map[string]string{"id": id})
}

func (s *Server) lookup(id string) *session.Session {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.sessions[id]
}

// known reports whether id resolves at all — as a live session or as a
// dormant rehydratable record.
func (s *Server) known(id string) bool {
	s.mu.Lock()
	defer s.mu.Unlock()
	if _, live := s.sessions[id]; live {
		return true
	}
	_, ok := s.dormant[id]
	return ok
}

// resolveForAttach returns a session to OBSERVE, and never spawns a
// process to do it.
//
// This is the read path — the WebSocket attach a frontend opens the
// moment a workspace comes on screen. Users switch to workspaces
// constantly without prompting them, so if merely looking cost a CLI,
// hibernation would reclaim nothing in practice. A dormant record
// therefore materializes as a HIBERNATED session: history replayed from
// the transcript, zero processes. The CLI arrives only when the user
// actually acts (resolveForAct).
//
// (nil, nil) means the id is unknown (404).
func (s *Server) resolveForAttach(id string) (*session.Session, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if sess := s.sessions[id]; sess != nil {
		return sess, nil
	}
	rec, ok := s.dormant[id]
	if !ok {
		return nil, nil
	}
	return s.materializeLocked(rec)
}

// resolveForAct returns a session with a LIVE shim, spawning one if the
// session is hibernated (or dormant from a previous daemon).
//
// This is the write path — a message, an interrupt, a UI command. It is
// the ONLY thing that brings a CLI back, which is what makes idle
// eviction pay: the process is spent on work, never on attention.
//
// Holding s.mu across the spawn is deliberate: two tabs racing the first
// act must resolve to exactly ONE shim, and the spawn is a rare, short,
// local exec.
func (s *Server) resolveForAct(id string) (*session.Session, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	sess := s.sessions[id]
	if sess == nil {
		rec, ok := s.dormant[id]
		if !ok {
			return nil, nil
		}
		var err error
		if sess, err = s.materializeLocked(rec); err != nil || sess == nil {
			return sess, err
		}
	}
	if !sess.Hibernated() {
		return sess, nil
	}
	if err := s.reviveLocked(sess); err != nil {
		return nil, err
	}
	return sess, nil
}

// reviveLocked spawns a fresh shim for a hibernated session and hands it
// over. The session keeps its ring, seq watermark, translator, and
// attached clients across the gap, so nothing is re-seeded and no client
// observes anything but a pause.
func (s *Server) reviveLocked(sess *session.Session) error {
	info := sess.Info()
	// The resume target is the basis of recovery. Hibernate refuses to
	// suspend a session without one, so its absence here is not a
	// recoverable condition — it is a broken invariant, and papering over
	// it would silently start a FRESH conversation in place of the user's.
	if info.ClaudeSessionID == "" {
		return fmt.Errorf("session %s: revive: hibernated with no claude_session_id to resume from", sess.ID)
	}
	shim, err := s.spawn(sess.ID, CreateOpts{
		CWD:            info.CWD,
		Model:          info.Model,
		PermissionMode: string(info.PermissionMode),
		ConfigDir:      info.ConfigDir,
		Resume:         info.ClaudeSessionID,
	})
	if err != nil {
		return fmt.Errorf("session %s: revive: spawn shim: %w", sess.ID, err)
	}
	return sess.Revive(shim)
}

// materializeLocked revives one dormant record under its ORIGINAL s_ id
// as a HIBERNATED session — the whole point of the registry: the id a
// frontend held before the daemon restart keeps resolving after it. The
// transcript is re-statted (same viability gate as the create path)
// because it may have vanished since boot; a record that fails the gate
// is pruned and reports unknown, which routes the client to its own
// rebind path.
//
// No shim is spawned here. A daemon restart followed by a user clicking
// through their workspaces must not fan out N CLIs; each one waits for a
// real act.
func (s *Server) materializeLocked(rec registry.Record) (*session.Session, error) {
	path := session.TranscriptPath(session.ClaudeConfigDir(rec.ConfigDir), rec.CWD, rec.ClaudeSessionID)
	if _, err := os.Stat(path); err != nil {
		s.logf("session %s: rehydration target %s lost its transcript at %s — pruning: %v",
			rec.SessionID, rec.ClaudeSessionID, path, err)
		delete(s.dormant, rec.SessionID)
		if delErr := s.registry.Delete(rec.SessionID); delErr != nil {
			s.logf("registry: prune %s FAILED: %v", rec.SessionID, delErr)
		}
		return nil, nil
	}
	sess := s.buildHibernated(rec.SessionID, CreateOpts{
		CWD:            rec.CWD,
		Model:          rec.Model,
		PermissionMode: rec.PermissionMode,
		ConfigDir:      rec.ConfigDir,
		Resume:         rec.ClaudeSessionID,
	}, true)
	delete(s.dormant, rec.SessionID)
	s.sessions[rec.SessionID] = sess
	s.logf("session %s: materialized hibernated from registry (resume %s) — no CLI until first act",
		rec.SessionID, rec.ClaudeSessionID)
	return sess, nil
}

// runIdleSweeper periodically hibernates sessions that have gone idle,
// freeing their CLI process pairs. It is the ONLY thing that initiates
// hibernation; everything else (attach, list, replay) is deliberately
// free of side effects on process lifetime.
func (s *Server) runIdleSweeper() {
	ticks := s.idleSweepTicks
	if ticks == nil {
		// Sweep at a quarter of the timeout so a session hibernates within
		// ~25% of idleTimeout of crossing it, never a whole timeout late.
		interval := s.idleTimeout / 4
		if interval <= 0 {
			interval = s.idleTimeout
		}
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
			s.sweepIdle()
		}
	}
}

// sweepIdle hibernates every live session idle past the timeout. Refusals
// (busy, no resume target yet) are expected and simply skipped — the next
// sweep re-checks them.
func (s *Server) sweepIdle() {
	now := s.now()
	s.mu.Lock()
	var candidates []*session.Session
	for _, sess := range s.sessions {
		if sess.Hibernated() || sess.Terminal() {
			continue
		}
		if sess.IdleFor(now) >= s.idleTimeout {
			candidates = append(candidates, sess)
		}
	}
	s.mu.Unlock()
	// Hibernate OUTSIDE the server lock: Hibernate sends the shim a
	// shutdown command and may spawn a grace-timer goroutine, none of
	// which needs s.mu, and holding it would serialize every session's
	// teardown behind one lock.
	for _, sess := range candidates {
		if err := sess.Hibernate("idle timeout"); err != nil {
			// Not a failure: a busy or not-yet-initialized session refuses,
			// and the next sweep tries again. Logged at debug volume only.
			s.logf("session %s: idle sweep skipped: %v", sess.ID, err)
		}
	}
}

// ShutdownAll asks every live session to drain (daemon teardown).
func (s *Server) ShutdownAll() {
	// Stop the idle sweeper first: a hibernate racing the drain would
	// churn a shim we are about to ask to shut down anyway.
	s.stopOnce.Do(func() { close(s.stopped) })
	// Flag the drain FIRST: the terminal transitions it triggers must
	// not mark registry records terminal, or nothing would rehydrate on
	// the next boot and every routine daemon restart would strand its
	// frontends ("session gone").
	s.draining.Store(true)
	s.mu.Lock()
	sessions := make([]*session.Session, 0, len(s.sessions))
	for _, sess := range s.sessions {
		sessions = append(sessions, sess)
	}
	s.mu.Unlock()
	for _, sess := range sessions {
		if err := sess.Shutdown("daemon shutting down"); err != nil {
			s.logf("server: shutdown session %s: %v", sess.ID, err)
		}
	}
}

func httpError(w http.ResponseWriter, status int, msg string) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(status)
	// Encoding a map[string]string cannot fail; ignore is unreachable.
	_ = json.NewEncoder(w).Encode(map[string]string{"error": msg})
}

// writeResumeTranscriptMissing hard-fails a create whose --resume target
// has no transcript in this daemon's config dir. No session is spawned:
// the body carries a machine-detectable code plus the resume id and
// every path stat'd so the Emacs client can open an investigation
// workspace for the lost session instead of silently starting fresh.
// Non-recoverable by construction — there is no session to recover.
func writeResumeTranscriptMissing(w http.ResponseWriter, resumeID string, searchedPaths []string) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusUnprocessableEntity)
	// Encoding strings and a []string cannot fail; ignore is unreachable.
	_ = json.NewEncoder(w).Encode(map[string]any{
		"code":           "resume_transcript_missing",
		"resume_id":      resumeID,
		"searched_paths": searchedPaths,
		"error": fmt.Sprintf(
			"resume target %s has no transcript in this daemon's config dir (searched %s); refusing to start a fresh conversation — the client will open an investigation workspace",
			resumeID, strings.Join(searchedPaths, ", ")),
	})
}

func writeJSON(w http.ResponseWriter, logf func(string, ...any), v any) {
	if err := json.NewEncoder(w).Encode(v); err != nil {
		logf("server: encode response: %v", err)
	}
}

func newSessionID() string {
	return "s_" + randomHex()
}

// newBootID mints the daemon instance identity: stable for the life of
// this process, different after every restart.
func newBootID() string {
	return "b_" + randomHex()
}

// newRequestID mints correlation ids for daemon-originated commands
// (the HTTP message/interrupt injection paths, where no WS client
// supplied one).
func newRequestID() string {
	return "r_" + randomHex()
}

func randomHex() string {
	var b [8]byte
	if _, err := rand.Read(b[:]); err != nil {
		panic(fmt.Sprintf("server: crypto/rand failed: %v", err))
	}
	return hex.EncodeToString(b[:])
}
