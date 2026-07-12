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

	"github.com/gorilla/websocket"

	"claude-repld/internal/protocol"
	"claude-repld/internal/session"
)

// CreateOpts is the POST /sessions request body.
type CreateOpts struct {
	CWD            string `json:"cwd,omitempty"`
	Model          string `json:"model,omitempty"`
	PermissionMode string `json:"permission_mode,omitempty"`
	Resume         string `json:"resume,omitempty"`
	Fake           bool   `json:"fake,omitempty"`
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

// Remediator dispatches the headless analyst for a session that has
// vanished from the daemon. Start reports whether this call is the one
// that launched it (a repeat for an already-dispatched id is a no-op).
type Remediator interface {
	Start(sessionID string) (bool, error)
}

// Server routes daemon HTTP traffic.
type Server struct {
	daemonVersion string
	retention     int
	forceFake     bool
	spawn         SpawnFunc
	logf          func(format string, args ...any)
	upgrader      websocket.Upgrader

	sentinel   session.SentinelSink
	remediator Remediator

	mu       sync.Mutex
	sessions map[string]*session.Session
}

// Config assembles a Server.
type Config struct {
	DaemonVersion string
	Retention     int
	Spawn         SpawnFunc
	Logf          func(format string, args ...any)
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
}

// New builds a Server.
func New(cfg Config) *Server {
	logf := cfg.Logf
	if logf == nil {
		logf = log.Printf
	}
	return &Server{
		daemonVersion: cfg.DaemonVersion,
		retention:     cfg.Retention,
		forceFake:     cfg.ForceFake,
		spawn:         cfg.Spawn,
		logf:          logf,
		sentinel:      cfg.Sentinel,
		remediator:    cfg.Remediator,
		upgrader: websocket.Upgrader{
			// The daemon is a local-loopback developer tool; the Emacs
			// xwidget origin is file-/app-scoped, so origin checks are
			// permissive by design.
			CheckOrigin: func(*http.Request) bool { return true },
		},
		sessions: map[string]*session.Session{},
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
	mux.HandleFunc("POST /remediation", s.handleRemediate)
	return mux
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
	// A session the daemon still serves is not gone, so a remediation
	// request naming one is a frontend bug: refuse rather than burn an
	// analyst on a healthy session.
	if s.lookup(body.SessionID) != nil {
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
	sess := s.lookup(r.PathValue("id"))
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
	sess := s.lookup(r.PathValue("id"))
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
		path := session.TranscriptPath(session.DefaultClaudeConfigDir(), opts.CWD, opts.Resume)
		if _, statErr := os.Stat(path); statErr != nil {
			s.logf("session %s: resume target %s has no transcript at %s — starting fresh instead of a doomed --resume: %v",
				id, opts.Resume, path, statErr)
			droppedResume = opts.Resume
			droppedPath = path
			opts.Resume = ""
		}
	}
	shim, err := s.spawn(id, opts)
	if err != nil {
		httpError(w, http.StatusInternalServerError, fmt.Sprintf("spawn shim: %v", err))
		return
	}
	sess := session.New(session.Config{
		ID:            id,
		DaemonVersion: s.daemonVersion,
		Shim:          shim,
		CWD:           opts.CWD,
		Model:         opts.Model,
		Retention:     s.retention,
		Logf:          s.logf,
		Sentinel:      s.sentinel,
	})
	// Resumed sessions seed their replay ring from the durable
	// transcript BEFORE Run: the CLI re-emits no history on --resume,
	// so without this every rebind (daemon restart, frontend switch)
	// attaches to a blank conversation.
	if opts.Resume != "" {
		path := session.TranscriptPath(session.DefaultClaudeConfigDir(), opts.CWD, opts.Resume)
		if err := sess.SeedFromTranscript(path, opts.Resume); err != nil {
			s.logf("session %s: transcript replay seed from %s failed (history will not render): %v", id, path, err)
		} else {
			s.logf("session %s: replay seeded from %s", id, path)
		}
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
	}
	list := make([]entry, 0, len(s.sessions))
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
	s.mu.Unlock()
	w.Header().Set("Content-Type", "application/json")
	writeJSON(w, s.logf, map[string]any{"sessions": list})
}

func (s *Server) handleDeleteSession(w http.ResponseWriter, r *http.Request) {
	sess := s.lookup(r.PathValue("id"))
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
	sess := s.lookup(r.PathValue("id"))
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

// ShutdownAll asks every live session to drain (daemon teardown).
func (s *Server) ShutdownAll() {
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
