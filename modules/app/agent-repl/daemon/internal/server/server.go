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

// Server routes daemon HTTP traffic.
type Server struct {
	daemonVersion string
	retention     int
	spawn         SpawnFunc
	logf          func(format string, args ...any)
	upgrader      websocket.Upgrader

	mu       sync.Mutex
	sessions map[string]*session.Session
}

// Config assembles a Server.
type Config struct {
	DaemonVersion string
	Retention     int
	Spawn         SpawnFunc
	Logf          func(format string, args ...any)
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
		spawn:         cfg.Spawn,
		logf:          logf,
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
	return mux
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
	shim, err := s.spawn(id, opts)
	if err != nil {
		httpError(w, http.StatusInternalServerError, fmt.Sprintf("spawn shim: %v", err))
		return
	}
	sess := session.New(session.Config{
		ID:            id,
		DaemonVersion: s.daemonVersion,
		Shim:          shim,
		Retention:     s.retention,
		Logf:          s.logf,
	})
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
	}
	list := make([]entry, 0, len(s.sessions))
	for id, sess := range s.sessions {
		list = append(list, entry{SessionID: id, Terminal: sess.Terminal()})
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

	client := session.NewClient()
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
	var b [8]byte
	if _, err := rand.Read(b[:]); err != nil {
		panic(fmt.Sprintf("server: crypto/rand failed: %v", err))
	}
	return "s_" + hex.EncodeToString(b[:])
}
