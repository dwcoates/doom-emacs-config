// Package server exposes the daemon's HTTP surface: session CRUD plus
// the per-session WebSocket stream endpoint (/sessions/{id}/stream)
// speaking Layer 2 of shared/protocol.md.
package server

import (
	"crypto/rand"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"os"
	"strings"
	"sync"
	"sync/atomic"
	"time"

	"github.com/gorilla/websocket"

	"claude-repld/internal/account"
	"claude-repld/internal/login"
	"claude-repld/internal/protocol"
	"claude-repld/internal/registry"
	"claude-repld/internal/session"
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
	retention     int
	forceFake     bool
	spawn         SpawnFunc
	logf          func(format string, args ...any)
	now           func() time.Time
	upgrader      websocket.Upgrader

	sentinel   session.SentinelSink
	remediator Remediator
	registry   *registry.Registry
	// logins owns the interactive Claude login terminals, at most one per
	// account; nil makes the login routes report the capability as
	// unconfigured.
	logins *login.Manager
	// draining marks a daemon-wide teardown (ShutdownAll): the session
	// deaths it causes are NOT conversation deaths, so the registrar
	// must leave their records non-terminal for the next boot to
	// rehydrate.
	draining atomic.Bool

	mu       sync.Mutex
	sessions map[string]*session.Session
	// dormant holds rehydratable registry records from a previous daemon
	// process, keyed by their ORIGINAL s_ id: the id keeps resolving
	// across the restart, and the first real access (stream / message /
	// interrupt) spawns a shim with --resume. A restart never fans out N
	// shims eagerly.
	dormant map[string]registry.Record
}

// Config assembles a Server.
type Config struct {
	DaemonVersion string
	Retention     int
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
		daemonVersion: cfg.DaemonVersion,
		bootID:        newBootID(),
		retention:     cfg.Retention,
		forceFake:     cfg.ForceFake,
		spawn:         cfg.Spawn,
		logf:          logf,
		now:           now,
		sentinel:      cfg.Sentinel,
		logins:        cfg.Logins,
		remediator:    cfg.Remediator,
		registry:      cfg.Registry,
		upgrader: websocket.Upgrader{
			// The daemon is a local-loopback developer tool; the Emacs
			// xwidget origin is file-/app-scoped, so origin checks are
			// permissive by design.
			CheckOrigin: func(*http.Request) bool { return true },
		},
		sessions: map[string]*session.Session{},
		dormant:  map[string]registry.Record{},
	}
	// A -fake daemon must leave the registry untouched: fake sessions
	// have no transcripts, so the prune below would destroy every REAL
	// record just because the daemon happened to boot offline.
	if s.registry != nil && !s.forceFake {
		s.loadDormant()
	} else if s.registry != nil {
		s.logf("server: -fake daemon: session registry left untouched (no rehydration)")
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
	// the drain must leave their records non-terminal.
	if r.s.draining.Load() {
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
	mux.HandleFunc("GET /sessions/{id}/commands", s.handleCommands)
	mux.HandleFunc("POST /sessions/{id}/commands/refresh", s.handleRefreshCommands)
	mux.HandleFunc("GET /sessions/{id}/account", s.handleAccount)
	mux.HandleFunc("POST /sessions/{id}/login", s.handleLogin)
	mux.HandleFunc("GET /sessions/{id}/login/terminal", s.handleLoginTerminal)
	mux.HandleFunc("DELETE /sessions/{id}/login", s.handleLoginClose)
	mux.HandleFunc("POST /remediation", s.handleRemediate)
	return mux
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
	sess, err := s.resolve(r.PathValue("id"))
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
	sess, err := s.resolve(r.PathValue("id"))
	if err != nil {
		httpError(w, http.StatusInternalServerError, err.Error())
		return
	}
	if sess == nil {
		httpError(w, http.StatusNotFound, "no such session")
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
	sess, err := s.resolve(r.PathValue("id"))
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
func (s *Server) handleInterrupt(w http.ResponseWriter, r *http.Request) {
	sess, err := s.resolve(r.PathValue("id"))
	if err != nil {
		httpError(w, http.StatusInternalServerError, err.Error())
		return
	}
	if sess == nil {
		httpError(w, http.StatusNotFound, "no such session")
		return
	}
	if err := sess.InjectCommand(map[string]any{
		"type":       "interrupt",
		"request_id": newRequestID(),
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
	id := newSessionID()
	// Resume viability gate: the CLI hard-exits (fatal_error) when asked
	// to --resume a session id with no transcript in this daemon's
	// config dir — e.g. an id minted inside the Docker sandbox or under
	// another CLAUDE_CONFIG_DIR. Spawning anyway yields a session that
	// dies within seconds and a client-side death loop (every send
	// recreates another doomed session). Start FRESH instead and tell
	// the webapp why in-band; nothing about this is silent.
	var droppedResume string
	var droppedPath string
	if opts.Resume != "" && !opts.Fake && !s.forceFake {
		path := session.TranscriptPath(session.ClaudeConfigDir(opts.ConfigDir), opts.CWD, opts.Resume)
		if _, statErr := os.Stat(path); statErr != nil {
			s.logf("session %s: resume target %s has no transcript at %s — starting fresh instead of a doomed --resume: %v",
				id, opts.Resume, path, statErr)
			droppedResume = opts.Resume
			droppedPath = path
			opts.Resume = ""
		}
	}
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
	if droppedResume != "" {
		sess.NoteResumeUnavailable(droppedResume, droppedPath)
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
	cfg := session.Config{
		ID:            id,
		DaemonVersion: s.daemonVersion,
		BootID:        s.bootID,
		Shim:          shim,
		CWD:           opts.CWD,
		Model:         opts.Model,
		ConfigDir:     opts.ConfigDir,
		Retention:     s.retention,
		Logf:          s.logf,
		Sentinel:      s.sentinel,
	}
	// A fake session's CLI is a scripted stand-in that writes no
	// transcript, so there is nothing to reconcile its model against; a
	// reconciler here could only log, twice a minute, that the file it
	// wants does not exist.
	if opts.Fake || s.forceFake {
		cfg.ModelReconcileInterval = -1
	}
	if registrable {
		cfg.Registrar = registrar{s}
	}
	sess := session.New(cfg)
	// Resumed sessions seed their replay ring from the durable
	// transcript BEFORE Run: the CLI re-emits no history on --resume,
	// so without this every rebind (daemon restart, frontend switch)
	// attaches to a blank conversation.
	if opts.Resume != "" {
		path := session.TranscriptPath(session.ClaudeConfigDir(opts.ConfigDir), opts.CWD, opts.Resume)
		if err := sess.SeedFromTranscript(path, opts.Resume); err != nil {
			s.logf("session %s: transcript replay seed from %s failed (history will not render): %v", id, path, err)
		} else {
			s.logf("session %s: replay seeded from %s", id, path)
		}
	}
	return sess, nil
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
		// PendingPermissions lists unresolved permission request ids.
		PendingPermissions []string `json:"pending_permissions,omitempty"`
		// Rehydratable marks a cold session carried over from a previous
		// daemon process: its id resolves, but the shim spawns (with
		// --resume) only on first real access.
		Rehydratable bool `json:"rehydratable,omitempty"`
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
			PendingPermissions: info.PendingPermissions,
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
	sess, err := s.resolve(r.PathValue("id"))
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
		if err := sess.HandleClientFrame(client, data); err != nil {
			s.logf("server: client frame rejected: %v", err)
		}
	}
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

// resolve returns the live session for id, rehydrating a dormant record
// into a running --resume session on first access. (nil, nil) means the
// id is unknown (404). Holding s.mu across the rehydration spawn is
// deliberate: two tabs racing the first access must resolve to exactly
// ONE shim, and the spawn is a rare, short, local exec.
func (s *Server) resolve(id string) (*session.Session, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if sess := s.sessions[id]; sess != nil {
		return sess, nil
	}
	rec, ok := s.dormant[id]
	if !ok {
		return nil, nil
	}
	return s.rehydrateLocked(rec)
}

// rehydrateLocked revives one dormant record under its ORIGINAL s_ id —
// the whole point of the registry: the id a frontend held before the
// daemon restart keeps resolving after it. The transcript is re-statted
// (same viability gate as the create path) because it may have vanished
// since boot; a record that fails the gate is pruned and reports
// unknown, which routes the client to its own rebind path.
func (s *Server) rehydrateLocked(rec registry.Record) (*session.Session, error) {
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
	opts := CreateOpts{
		CWD:            rec.CWD,
		Model:          rec.Model,
		PermissionMode: rec.PermissionMode,
		ConfigDir:      rec.ConfigDir,
		Resume:         rec.ClaudeSessionID,
	}
	sess, err := s.launchSession(rec.SessionID, opts, true)
	if err != nil {
		return nil, fmt.Errorf("rehydrate session %s: spawn shim: %w", rec.SessionID, err)
	}
	delete(s.dormant, rec.SessionID)
	s.sessions[rec.SessionID] = sess
	go sess.Run()
	s.logf("session %s: rehydrated on first access (resume %s)", rec.SessionID, rec.ClaudeSessionID)
	return sess, nil
}

// ShutdownAll asks every live session to drain (daemon teardown).
func (s *Server) ShutdownAll() {
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
