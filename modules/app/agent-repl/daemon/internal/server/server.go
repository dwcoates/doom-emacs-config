// Package server exposes the daemon's HTTP surface: session CRUD plus the
// per-session WebSocket stream endpoint (/sessions/{id}/stream).
//
// After the agent-shim consumption cutover the daemon no longer owns a
// Layer-2 stdio streaming hub. Each session's UDS shim is consumed through
// the per-session driver (internal/sessiondrv) and rendered onto the
// frontend.v1 surface (internal/frontend) plus the session-state manager
// (internal/ssm). The registry is the source of truth for which sessions
// exist; the driver owns the live shims. Several routes here exist ONLY to
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
	"log"
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
	"claude-repld/internal/frontend"
	"claude-repld/internal/login"
	"claude-repld/internal/protocol"
	"claude-repld/internal/registry"
	"claude-repld/internal/session"
	"claude-repld/internal/sessiondrv"
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

// Remediator dispatches the headless analyst for a session that has
// vanished from the daemon. Start reports whether this call is the one
// that launched it (a repeat for an already-dispatched id is a no-op).
type Remediator interface {
	Start(sessionID string) (bool, error)
}

// SentinelSink receives the account-switch side-channel poke Emacs needs so
// its per-workspace config-dir override follows a daemon-driven switch. The
// concrete *sentinel.Writer satisfies it; the daemon uses only this one
// method now that the L2 broadcast-tap sentinel writes are gone.
type SentinelSink interface {
	AccountChanged(cwd, sid string)
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

	// driver consumes each session's UDS shim and backs prompt/interrupt/
	// permission plus /status, /commands, and /tasks introspection.
	driver *sessiondrv.Manager
	// ssm resolves per-workspace render state (turn-active, live tasks) the
	// list and idle-sweep read.
	ssm *ssm.Manager
	// frontend fans frontend.v1 frames to the per-session /stream WebSocket.
	frontend *frontend.Server
	// sentinel receives the account-switch poke; nil disables it.
	sentinel SentinelSink

	remediator Remediator
	registry   *registry.Registry
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
	// stopped is closed by ShutdownAll, ending the sweeper goroutine.
	stopped  chan struct{}
	stopOnce sync.Once

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
	// Driver consumes each session's UDS shim (prompt/interrupt/permission,
	// plus /status, /commands, /tasks introspection). Required in production.
	Driver *sessiondrv.Manager
	// SSM resolves per-workspace render state (turn-active, live tasks).
	// Required in production.
	SSM *ssm.Manager
	// Frontend fans frontend.v1 frames to the per-session /stream WebSocket.
	// Required in production.
	Frontend *frontend.Server
	// Sentinel receives the account-switch side-channel poke; nil disables it.
	Sentinel SentinelSink
	// Remediator dispatches the "session gone" analyst; nil makes
	// POST /remediation report the capability unconfigured.
	Remediator Remediator
	// Registry persists session records across daemon restarts. Required: it
	// is the source of truth for which sessions exist.
	Registry *registry.Registry
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
	// IdleTimeout is how long a session may go without a turn before the
	// sweeper hibernates its shim. Zero disables hibernation.
	IdleTimeout time.Duration
	// IdleSweepTicks overrides the sweeper's clock. Nil mints a real ticker;
	// tests inject a channel so a sweep runs on demand.
	IdleSweepTicks <-chan time.Time
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
		forceFake:       cfg.ForceFake,
		logf:            logf,
		now:             now,
		widgetAssetsDir: cfg.WidgetAssetsDir,
		daemonAddr:      cfg.DaemonAddr,
		driver:          cfg.Driver,
		ssm:             cfg.SSM,
		frontend:        cfg.Frontend,
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
	}
	if s.idleTimeout > 0 || s.idleSweepTicks != nil {
		go s.runIdleSweeper()
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
	mux.HandleFunc("POST /sessions", s.handleCreateSession)
	mux.HandleFunc("GET /sessions", s.handleListSessions)
	mux.HandleFunc("GET /sessions/{id}/stream", s.handleStream)
	// DELETE /sessions/{id}, POST /sessions/{id}/message, and
	// POST /sessions/{id}/interrupt were removed in S7: Emacs drives them over
	// the frontend.v1 UDS commands (deleteSession/submitPrompt/interrupt) and
	// the webapp never used them (it submits/interrupts over its /stream WS).
	// Their cores survive: s.DeleteSession backs the deleteSession command, and
	// the driver's SubmitPrompt/Interrupt back the prompt/interrupt commands.
	mux.HandleFunc("GET /sessions/{id}/tasks/{taskId}/output", s.handleTaskOutput)
	// The slash-command routes (GET /sessions/{id}/commands, POST
	// /sessions/{id}/commands/refresh) and the status routes (GET
	// /sessions/{id}/status, POST /sessions/{id}/status/refresh) were DELETED in
	// the D-phase census: both frontends re-source the SDK system:init from the
	// pushed SessionInitView frame, and the two refresh routes had already
	// degraded to loud no-ops (the UDS shim has no re-init control).
	// The queue-control routes (POST /sessions/{id}/queue/{queueId}/run-now and
	// /cancel) were DELETED in S9: the daemon-owned queue plane is dead
	// server-side, and both routes were already loud no-ops. Frontends drive the
	// turn directly over submitPrompt/interrupt.
	mux.HandleFunc("GET /sessions/{id}/account", s.handleAccount)
	mux.HandleFunc("POST /sessions/{id}/account", s.handleAccountSwitch)
	mux.HandleFunc("GET /accounts", s.handleAccounts)
	mux.HandleFunc("GET /capabilities", s.handleCapabilities)
	mux.HandleFunc("POST /sessions/{id}/login", s.handleLogin)
	mux.HandleFunc("GET /sessions/{id}/login/terminal", s.handleLoginTerminal)
	mux.HandleFunc("DELETE /sessions/{id}/login", s.handleLoginClose)
	mux.HandleFunc("GET /sessions/{id}/chess-game", s.handleChessGameFile)
	mux.HandleFunc("POST /sessions/{id}/add-support", s.handleAddSupport)
	mux.HandleFunc("POST /remediation", s.handleRemediate) // SUPERSEDED (S7): dies when elisp completes its full-UDS migration
	mux.HandleFunc("POST /workspace-command", s.handleWorkspaceCommand)
	// POST /shutdown was DELETED in the D-phase census: Emacs bounces an
	// adopted daemon over the shutdown FrontendCommand (frontend-uds.el) and no
	// other surface ever called it. The graceful-teardown func it drove survives
	// on the command handler.
	return mux
}

// ---------------------------------------------------------------------------
// Registry-sourced session resolution
//
// There is no live-session map anymore: the registry is the source of truth
// for records and the driver owns the live shims. A session id maps to its
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
	cmd := workspacecmd.NewCreate(
		addsupport.WorkspaceName(body.Command),
		cwd,
		addsupport.Prompt(body.Command, configDir),
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
		s.httpFail(w, r, http.StatusInternalServerError, "session %s: opening the login terminal: %v", id, err)
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
// Closing one that is not running is a success.
func (s *Server) handleLoginClose(w http.ResponseWriter, r *http.Request) {
	id := r.PathValue("id")
	configDir, ok := s.loginAccount(w, id)
	if !ok {
		return
	}
	if err := s.logins.Close(configDir); err != nil {
		s.httpFail(w, r, http.StatusInternalServerError, "session %s: closing the login terminal: %v", id, err)
		return
	}
	w.WriteHeader(http.StatusNoContent)
}

// handleTaskOutput serves a bounded, session-scoped tail of a detached task's
// output file. The task's output path comes off the driver's rebuilt
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
	entry, ok := s.driver.TaskEntry(cwd, r.PathValue("taskId"))
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

	// Stop the old shim before the root changes. A workspace with no live
	// shim is an expected no-op, not a failure.
	if err := s.driver.Hibernate(rec.CWD); err != nil {
		s.logf("session %s: account-switch shim stop (ws %s): %v (expected when no live shim)", id, rec.CWD, err)
	}

	// Persist the new root (and freshest claude_session_id) BEFORE the
	// relaunch: if the bring-up fails, the record still rehydrates under the
	// target root on the next access instead of the old one.
	s.updateRegistry(id, "account switch", func(rec *registry.Record) {
		rec.ConfigDir = target.ConfigDir
		if csid != "" {
			rec.ClaudeSessionID = csid
		}
	})

	// Bring the shim back up under the same id: Ensure re-locates the
	// just-updated record and spawns fresh under the target root.
	if err := s.driver.Ensure(rec.CWD); err != nil {
		s.httpFail(w, r, http.StatusInternalServerError, "session %s (cwd %s): relaunch under %q: %v", id, rec.CWD, target.ConfigDir, err)
		return
	}
	s.logf("session %s: switched to account %q (%s), resume %s", id, target.Label, target.ConfigDir, csid)
	// Push the updated SessionView so every frontend reflects the new account
	// (SessionView.config_dir now carries the switched-to root, S8). The webapp
	// owns account switching; Emacs merely renders this pushed state.
	s.pushSessionView(id)
	// Poke Emacs over the sentinel side channel: its per-workspace config-dir
	// override must follow the switch, or its own next create/reattach would
	// put the session back on the computed default.
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

// handleRemediate dispatches the "session gone" analyst. SUPERSEDED (S7).
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
	// A session the daemon still serves (a non-terminal record) is not gone,
	// so a remediation request naming one is a frontend bug.
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
		s.httpFail(w, r, http.StatusInternalServerError, "session %s: remediation start: %v", body.SessionID, err)
		return
	}
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusAccepted)
	writeJSON(w, s.logf, map[string]bool{"started": started})
}

// handleSendMessage and handleInterrupt were removed in S7: Emacs submits
// prompts and interrupts over the frontend.v1 UDS commands
// (submitPrompt/interrupt, keyed by workspace), and the webapp drives both over
// its /stream WebSocket — neither HTTP route had a remaining caller. The turn
// still flows through the same per-session driver (SubmitPrompt/Interrupt); only
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
	ResumeID      string
	SearchedPaths []string
}

func (e *ResumeTranscriptMissingError) Error() string {
	return fmt.Sprintf("resume target %s has no transcript in this daemon's config dir (searched %s); refusing to start a fresh conversation",
		e.ResumeID, strings.Join(e.SearchedPaths, ", "))
}

// InvalidCreateError reports a malformed create request (currently only an
// invalid permission mode). HTTP maps it to 400; UDS surfaces it as a loud ack.
type InvalidCreateError struct{ msg string }

func (e *InvalidCreateError) Error() string { return e.msg }

// errSessionNotFound reports a delete/lookup for an id with no registry record.
var errSessionNotFound = errors.New("no such session")

// CreateSession is the shared create-session core behind both POST /sessions
// (webapp) and the createSession UDS command (Emacs): validate, apply the
// resume-viability gate, supersede transcript conflicts, register the record,
// and bring up the shim. It returns the new session id and pushes a SessionView
// so every connected frontend learns the workspace->session binding without
// polling GET /sessions. Typed errors (*InvalidCreateError,
// *ResumeTranscriptMissingError) let callers map to their transport; any other
// error is an internal bring-up failure surfaced loudly.
func (s *Server) CreateSession(_ context.Context, opts CreateOpts) (string, error) {
	if opts.PermissionMode != "" && !protocol.ValidPermissionMode(opts.PermissionMode) {
		return "", &InvalidCreateError{msg: fmt.Sprintf("invalid permission_mode %q", opts.PermissionMode)}
	}
	resumeLabel := opts.Resume
	if resumeLabel == "" {
		resumeLabel = "fresh"
	}
	dlog.Tag(s.logf, "cwd", opts.CWD, "model", opts.Model, "config_dir", opts.ConfigDir)(
		"session create requested (resume=%s)", resumeLabel)
	// Resume viability gate: the CLI hard-exits when asked to --resume a
	// session id with no transcript in this daemon's config dir. Silently
	// downgrading to a FRESH conversation buries a genuinely lost session, so
	// HARD-FAIL the create before bringing anything up. Fake sessions skip the
	// gate — the scripted SDK has no transcripts by design.
	if opts.Resume != "" && !opts.Fake && !s.forceFake {
		path := session.TranscriptPath(session.ClaudeConfigDir(opts.ConfigDir), opts.CWD, opts.Resume)
		if _, statErr := os.Stat(path); statErr != nil {
			s.logf("session create REJECTED: resume target %s has no transcript at %s — hard-failing so the client opens an investigation workspace: %v",
				opts.Resume, path, statErr)
			return "", &ResumeTranscriptMissingError{ResumeID: opts.Resume, SearchedPaths: []string{path}}
		}
	}
	// A transcript takes exactly one writer, and this create is the newest
	// claim on it, so any older session still holding it stands down BEFORE a
	// second CLI exists (see supersede.go). After the viability gate so a
	// create about to be rejected never tears down a healthy session.
	s.supersedeResumeConflicts(opts)
	id := newSessionID()
	// Register BEFORE bring-up: the driver's SessionLocator resolves a
	// workspace to a session by reading the registry, so the record MUST exist
	// for Ensure to find and drive THIS session — for fake sessions too (unlike
	// the old L2 hub, which kept fake sessions only in an in-memory map). A fake
	// record carries an empty claude_session_id, so it never rehydrates into a
	// doomed --resume; it is simply the driver's handle on a transient session.
	if s.registry != nil {
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
	// Eager, reattach-first bring-up: the driver's SessionLocator resolves the
	// workspace to the newest non-terminal record — the one just Put — so
	// Ensure brings up THIS session. A workspace-less session (no cwd) has no
	// workspace to drive, so it is registered but not brought up.
	if opts.CWD != "" {
		if err := s.driver.Ensure(opts.CWD); err != nil {
			return "", fmt.Errorf("session %s (cwd %s): bring up shim: %w", id, opts.CWD, err)
		}
	}
	dlog.Tag(s.logf, "cwd", opts.CWD)("session %s: created", id)
	// Deliver the workspace->session binding proactively: SessionView is not a
	// driver push (only snapshots carry it otherwise), so the UDS/webapp clients
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
	if !rec.Terminal {
		s.updateRegistry(id, "terminal transition", func(r *registry.Record) {
			r.Terminal = true
			r.DeathReason = "delete session"
		})
		// Best-effort stop of the live shim. A workspace with no live shim is
		// an expected no-op, not a failure.
		if rec.CWD != "" {
			if err := s.driver.Hibernate(rec.CWD); err != nil {
				s.logf("session %s: delete shim stop (ws %s): %v (expected when no live shim)", id, rec.CWD, err)
			}
		}
		// Push the terminal SessionView so frontends reap the workspace binding
		// (the orphan/reattach sweep re-keys on it) instead of polling.
		s.pushSessionView(id)
	}
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
// the two cannot drift. Rehydratable/Hibernated are not listed session state
// post-cutover (driver-internal shim lifecycle) and stay false.
func SessionViewFromRecord(rec registry.Record, pendingPermissions []string) *frontendv1.SessionView {
	return &frontendv1.SessionView{
		Workspace:          rec.CWD,
		SessionId:          rec.SessionID,
		Model:              rec.Model,
		PermissionMode:     rec.PermissionMode,
		ClaudeSessionId:    rec.ClaudeSessionID,
		Cwd:                rec.CWD,
		Terminal:           rec.Terminal,
		DeathReason:        rec.DeathReason,
		PendingPermissions: int64(len(pendingPermissions)),
		// The CLAUDE_CONFIG_DIR the session's shim runs against — the ACCOUNT it
		// runs as (S8). Empty names the CLI's own default root. Carried on every
		// SessionView push (this is the single shaping) so account switching is
		// webapp-initiated, daemon-executed, and reflected in pushed state.
		ConfigDir: rec.ConfigDir,
	}
}

// pushSessionView pushes id's current SessionView to every connected frontend.
// Nil-safe against a Server built without a registry or frontend server (unit
// harnesses): the push is a best-effort delivery, not a precondition.
func (s *Server) pushSessionView(id string) {
	if s.registry == nil || s.frontend == nil {
		return
	}
	rec, ok := s.registry.Get(id)
	if !ok {
		s.logf("session %s: pushSessionView found no record — cannot deliver the workspace binding", id)
		return
	}
	var pending []string
	if !rec.Terminal && rec.CWD != "" && s.driver != nil {
		pending = s.driver.PendingPermissions(rec.CWD)
	}
	s.frontend.PushSessionView(SessionViewFromRecord(rec, pending))
}

func (s *Server) handleCreateSession(w http.ResponseWriter, r *http.Request) {
	var opts CreateOpts
	if r.Body != nil {
		if err := json.NewDecoder(r.Body).Decode(&opts); err != nil && err.Error() != "EOF" {
			httpError(w, http.StatusBadRequest, fmt.Sprintf("invalid request body: %v", err))
			return
		}
	}
	id, err := s.CreateSession(r.Context(), opts)
	if err != nil {
		var invalid *InvalidCreateError
		var missing *ResumeTranscriptMissingError
		switch {
		case errors.As(err, &invalid):
			httpError(w, http.StatusBadRequest, invalid.Error())
		case errors.As(err, &missing):
			writeResumeTranscriptMissing(w, missing.ResumeID, missing.SearchedPaths)
		default:
			s.httpFail(w, r, http.StatusInternalServerError, "%v", err)
		}
		return
	}
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusCreated)
	writeJSON(w, s.logf, map[string]string{
		"session_id": id,
		"stream_url": fmt.Sprintf("/sessions/%s/stream", id),
	})
}

func (s *Server) handleListSessions(w http.ResponseWriter, _ *http.Request) {
	// SUPERSEDED (S7): the Emacs poller consumes the full list; the webapp
	// probe only reads presence. It is built entirely off the registry (the
	// source of truth for records) plus the driver/SSM for live fields.
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
		// driver-internal shim lifecycle detail, not a listed session state.
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
			e.PendingPermissions = s.driver.PendingPermissions(rec.CWD)
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
	s.frontend.ServeWSScoped(w, r, frontend.Scope{SessionID: id, Workspace: cwd}, s.frontendCommandTranslator(cwd))
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
// idle, freeing their CLI process pairs. It is the ONLY thing that initiates
// hibernation.
func (s *Server) runIdleSweeper() {
	ticks := s.idleSweepTicks
	if ticks == nil {
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

// sweepIdle stops the shim of every non-turn-active session best-effort.
//
// SIMPLIFICATION (post-cutover): the daemon no longer tracks a per-session
// lastActive stamp — the driver owns liveness and the shim outlives a
// hibernation via reattach on the next act. Without a lastActive signal the
// sweeper cannot gate on a real idle duration, so it gates ONLY on
// !turn_active (never hibernate a session mid-turn) and calls Hibernate
// best-effort. A workspace whose shim is already stopped returns a "no live
// session" error, which is expected and skipped, not a failure.
func (s *Server) sweepIdle() {
	for _, rec := range s.registry.All() {
		if rec.Terminal || rec.CWD == "" {
			continue
		}
		if st, found, err := s.ssm.Current(rec.CWD); err != nil {
			s.logf("session %s: idle sweep state read (ws %s): %v", rec.SessionID, rec.CWD, err)
			continue
		} else if found && st.GetTurnActive() {
			continue // never hibernate a turn-active session
		}
		if err := s.driver.Hibernate(rec.CWD); err != nil {
			// Expected for an already-hibernated / never-brought-up workspace.
			s.logf("session %s: idle sweep skipped (ws %s): %v", rec.SessionID, rec.CWD, err)
		}
	}
}

// ShutdownAll stops every live shim (daemon teardown). The registry records
// stay non-terminal so they rehydrate on the next boot; main also calls
// driver.Close().
func (s *Server) ShutdownAll() {
	s.stopOnce.Do(func() { close(s.stopped) })
	for _, rec := range s.registry.All() {
		if rec.Terminal || rec.CWD == "" {
			continue
		}
		if err := s.driver.Hibernate(rec.CWD); err != nil {
			s.logf("server: shutdown stop shim (ws %s): %v", rec.CWD, err)
		}
	}
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

// writeResumeTranscriptMissing hard-fails a create whose --resume target has
// no transcript in this daemon's config dir. No session is brought up: the
// body carries a machine-detectable code plus the resume id and every path
// stat'd so the Emacs client can open an investigation workspace.
func writeResumeTranscriptMissing(w http.ResponseWriter, resumeID string, searchedPaths []string) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusUnprocessableEntity)
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
