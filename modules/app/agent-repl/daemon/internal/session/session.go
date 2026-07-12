package session

import (
	"crypto/rand"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"log"
	"sync"
	"time"

	"claude-repld/internal/protocol"
)

// ShimHandle is the slice of the shim supervisor the session needs;
// satisfied by *shim.Proc and by test fakes.
type ShimHandle interface {
	Events() <-chan *protocol.L1Event
	SendRaw(line []byte) error
	Send(cmd any) error
}

// Client is one attached WebSocket consumer. The server owns the socket;
// the session owns the outbound frame queue.
type Client struct {
	// Send carries stamped, marshaled frames. Closed by the session on
	// detach or when the client is dropped for falling behind.
	Send chan []byte
}

// NewClient returns a client with the standard outbound buffer.
func NewClient() *Client {
	return &Client{Send: make(chan []byte, 256)}
}

type retained struct {
	seq  int64
	data []byte
}

// Session glues one shim process to any number of Layer-2 clients:
// it translates Layer-1 events, stamps the §2.1 envelope (monotonic seq,
// ISO-8601 ts), retains the last N frames for §2.10 replay, and fans
// frames out to attached clients.
type Session struct {
	ID            string
	DaemonVersion string

	shim       ShimHandle
	translator *Translator
	retention  int
	now        func() time.Time
	logf       func(format string, args ...any)
	sentinel   SentinelSink

	mu       sync.Mutex
	seq      int64
	ring     []retained
	clients  map[*Client]struct{}
	terminal bool

	done chan struct{}
}

// SentinelSink receives agent-state notifications for the Emacs
// sentinel-file side channel (see internal/sentinel and the
// "Agent-state sentinels" section of shared/protocol.md). Optional: a
// nil sink disables the side channel (tests, embedded use).
type SentinelSink interface {
	PermissionRequested(cwd, sid, reqID string)
	PermissionResolved(cwd, sid, reqID string)
	SessionDead(cwd, sid string)
}

// Config carries session construction parameters.
type Config struct {
	ID            string
	DaemonVersion string
	Shim          ShimHandle
	// CWD and Model are the CreateOpts-requested values; they seed the
	// translator's hello mirror so introspection works before the SDK's
	// system:init overwrites them with authoritative values.
	CWD   string
	Model string
	// Retention is the §2.10 replay window in frames (defaults to 4096).
	Retention int
	// Now is the frame-timestamp clock (defaults to time.Now).
	Now func() time.Time
	// Logf receives supervision noise (defaults to log.Printf).
	Logf func(format string, args ...any)
	// Sentinel receives agent-state side-channel notifications; nil
	// disables the sentinel writes.
	Sentinel SentinelSink
}

// New assembles a session; call Run to start consuming shim events.
func New(cfg Config) *Session {
	if cfg.Retention <= 0 {
		cfg.Retention = 4096
	}
	if cfg.Now == nil {
		cfg.Now = time.Now
	}
	if cfg.Logf == nil {
		cfg.Logf = log.Printf
	}
	translator := NewTranslator()
	translator.CWD = cfg.CWD
	translator.Model = cfg.Model
	return &Session{
		ID:            cfg.ID,
		DaemonVersion: cfg.DaemonVersion,
		shim:          cfg.Shim,
		translator:    translator,
		retention:     cfg.Retention,
		now:           cfg.Now,
		logf:          cfg.Logf,
		sentinel:      cfg.Sentinel,
		clients:       map[*Client]struct{}{},
		done:          make(chan struct{}),
	}
}

// Info is a point-in-time introspection snapshot of a session.
type Info struct {
	CWD             string
	Model           string
	ClaudeSessionID string
	Terminal        bool
}

// Info returns the session's current introspection snapshot: the
// requested-then-authoritative cwd/model mirror, the durable CLI session
// id (empty until system:init), and terminality.
func (s *Session) Info() Info {
	s.mu.Lock()
	defer s.mu.Unlock()
	return Info{
		CWD:             s.translator.CWD,
		Model:           s.translator.Model,
		ClaudeSessionID: s.translator.ClaudeSessionID,
		Terminal:        s.terminal,
	}
}

// Run consumes the shim event stream until it closes. Blocks; run it on
// its own goroutine. All translation happens here, so the translator
// needs no locking of its own.
func (s *Session) Run() {
	for evt := range s.shim.Events() {
		s.mu.Lock()
		frames := s.translator.OnEvent(evt)
		s.broadcastLocked(frames)
		if evt.Type == "closed" {
			s.terminal = true
		}
		s.mu.Unlock()
	}
	s.mu.Lock()
	if !s.terminal {
		// Shim stdout closed without a `closed` event: hard death.
		// Cancel pending permission prompts FIRST (§2.7 "cancel" on shim
		// death) so no tab is left with a live prompt for a dead shim,
		// then surface the error frame.
		s.terminal = true
		s.broadcastLocked(s.translator.OnShimDeath())
		s.broadcastLocked([]protocol.L2Frame{&protocol.ErrorFrame{
			Envelope:    protocol.Envelope{Type: "error"},
			Code:        "shim_died",
			Message:     "shim exited without a closed event",
			Recoverable: false,
		}})
	}
	for c := range s.clients {
		close(c.Send)
		delete(s.clients, c)
	}
	s.mu.Unlock()
	close(s.done)
}

// Done is closed once the shim event stream has drained and the session
// is terminal.
func (s *Session) Done() <-chan struct{} { return s.done }

// Terminal reports whether the session has ended.
func (s *Session) Terminal() bool {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.terminal
}

// Attach registers a client and sends it the §2.2 hello frame. The hello
// reuses the current seq watermark without consuming one and is not
// retained: it is connection-scoped, not part of session history.
func (s *Session) Attach(c *Client) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.clients[c] = struct{}{}
	c.Send <- s.helloLocked()
}

// Detach unregisters a client; its Send channel is closed.
func (s *Session) Detach(c *Client) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if _, ok := s.clients[c]; ok {
		delete(s.clients, c)
		close(c.Send)
	}
}

func (s *Session) helloLocked() []byte {
	var resumeFrom int64
	if len(s.ring) > 0 {
		resumeFrom = s.ring[0].seq
	}
	hello := &protocol.HelloFrame{
		Envelope: protocol.Envelope{
			Type:      "hello",
			Seq:       s.seq,
			TS:        s.timestamp(),
			SessionID: s.ID,
		},
		DaemonVersion:   s.DaemonVersion,
		ResumeFromSeq:   resumeFrom,
		PermissionMode:  s.translator.PermissionMode,
		Model:           s.translator.Model,
		CWD:             s.translator.CWD,
		ClaudeSessionID: s.translator.ClaudeSessionID,
	}
	data, err := json.Marshal(hello)
	if err != nil {
		panic(fmt.Sprintf("session: marshal hello: %v", err)) // static shape, cannot fail
	}
	return data
}

// HandleClientFrame processes one webapp→daemon NDJSON frame from c.
func (s *Session) HandleClientFrame(c *Client, raw []byte) error {
	cmd, err := protocol.DecodeCommand(raw)
	if err != nil {
		return err
	}
	if cmd == nil {
		return nil // unknown type: ignored for forward compatibility
	}

	s.mu.Lock()
	defer s.mu.Unlock()
	// replay-request works on TERMINAL sessions too: a client attaching
	// after the shim ended must still be able to rebuild the retained
	// history instead of staring at an empty feed.
	if cmd.Type == "replay-request" {
		s.replayLocked(c, cmd.FromSeq)
		return nil
	}
	if s.terminal {
		return fmt.Errorf("session %s: command %s on terminal session", s.ID, cmd.Type)
	}

	switch cmd.Type {
	case "user-message":
		s.broadcastLocked([]protocol.L2Frame{s.translator.OnUserMessageCmd(cmd)})
		return s.shim.SendRaw(ndjson(raw))
	case "permission-decision":
		frame, pending := s.translator.OnPermissionDecisionCmd(cmd)
		if !pending {
			return fmt.Errorf("session %s: permission-decision for unknown request_id %s", s.ID, cmd.RequestID)
		}
		s.broadcastLocked([]protocol.L2Frame{frame})
		return s.shim.SendRaw(ndjson(raw))
	case "interrupt":
		s.broadcastLocked(s.translator.OnInterruptCmd())
		return s.shim.SendRaw(ndjson(raw))
	case "set-permission-mode":
		s.translator.OnSetPermissionModeCmd(cmd)
		return s.shim.SendRaw(ndjson(raw))
	}
	// `shutdown` is deliberately NOT forwarded: the §2 preamble limits
	// webapp→daemon traffic to the four UI commands plus replay-request;
	// session teardown is daemon-owned via DELETE /sessions/{id}
	// (Session.Shutdown). Note the mechanism differs from a truly
	// unknown type: DecodeCommand KNOWS shutdown (it is a Layer-1
	// command) and decodes it, so it reaches this switch and falls
	// through — it is deliberately dropped here, not filtered upstream.
	return nil
}

// InjectCommand runs a daemon-originated client command (HTTP send /
// interrupt paths, where the submitter holds no WebSocket) through the
// exact same pipeline as a WS frame, so broadcasts, retention, and
// translator state behave identically. CMD must marshal to a §2
// client-command shape.
//
// The nil Client is safe by construction: of HandleClientFrame's
// branches, only replay-request touches the client, and the injected
// commands here are never replay-requests.
func (s *Session) InjectCommand(cmd map[string]any) error {
	raw, err := json.Marshal(cmd)
	if err != nil {
		return fmt.Errorf("session %s: marshal injected command: %w", s.ID, err)
	}
	return s.HandleClientFrame(nil, raw)
}

// Shutdown asks the shim to drain and exit (used by DELETE /sessions/{id}
// and daemon teardown).
func (s *Session) Shutdown(reason string) error {
	s.mu.Lock()
	if s.terminal {
		s.mu.Unlock()
		return nil
	}
	s.mu.Unlock()
	return s.shim.Send(protocol.NewShutdownCmd(newRequestID(), reason))
}

// replayLocked implements §2.10: re-send retained frames with their
// original seq/ts, or a fresh hello when from_seq has been evicted.
func (s *Session) replayLocked(c *Client, fromSeq int64) {
	if len(s.ring) == 0 || fromSeq < s.ring[0].seq {
		s.sendToClientLocked(c, s.helloLocked())
		return
	}
	for _, r := range s.ring {
		if r.seq >= fromSeq {
			if !s.sendToClientLocked(c, r.data) {
				return
			}
		}
	}
}

// broadcastLocked stamps, retains and fans out frames. Callers hold s.mu.
//
// It is also the single choke point every authoritative frame passes
// through exactly once (replay re-sends retained bytes and never
// re-enters here), which makes it the tap for the Emacs sentinel side
// channel: permission lifecycle and shim death map to sentinel writes.
// The sink only enqueues onto a buffered channel (I/O happens on the
// writer's own goroutine), so the tap does not hold s.mu across disk.
func (s *Session) broadcastLocked(frames []protocol.L2Frame) {
	for _, frame := range frames {
		if s.sentinel != nil {
			switch f := frame.(type) {
			case *protocol.PermissionRequestFrame:
				s.sentinel.PermissionRequested(s.translator.CWD, s.translator.ClaudeSessionID, f.RequestID)
			case *protocol.PermissionResolvedFrame:
				s.sentinel.PermissionResolved(s.translator.CWD, s.translator.ClaudeSessionID, f.RequestID)
			case *protocol.ErrorFrame:
				if f.Code == "shim_died" {
					s.sentinel.SessionDead(s.translator.CWD, s.translator.ClaudeSessionID)
				}
			}
		}
		s.seq++
		env := frame.Env()
		env.Seq = s.seq
		env.TS = s.timestamp()
		env.SessionID = s.ID
		data, err := json.Marshal(frame)
		if err != nil {
			// A frame we built that cannot marshal is a daemon bug; skip
			// the frame but keep the seq gap visible to clients so they
			// know something was lost.
			s.logf("session %s: marshal %s frame: %v", s.ID, env.Type, err)
			continue
		}
		s.ring = append(s.ring, retained{seq: s.seq, data: data})
		if len(s.ring) > s.retention {
			s.ring = s.ring[len(s.ring)-s.retention:]
		}
		for c := range s.clients {
			s.sendToClientLocked(c, data)
		}
	}
}

// sendToClientLocked queues data on c, dropping the client if its buffer
// is full (it can reconnect and replay). Reports whether the client is
// still attached.
//
// The membership check is load-bearing, not defensive: Run's shim-death
// drain closes every attached client's Send channel under the lock, but
// the server's per-connection reader goroutine can still deliver a
// replay-request (which the terminal carve-out honors) in the window
// before the writer tears the socket down. Sending on the closed
// channel would panic; a client no longer in s.clients has by invariant
// already had its channel closed, so it is skipped.
func (s *Session) sendToClientLocked(c *Client, data []byte) bool {
	if _, attached := s.clients[c]; !attached {
		return false
	}
	select {
	case c.Send <- data:
		return true
	default:
		s.logf("session %s: dropping client that fell behind", s.ID)
		delete(s.clients, c)
		close(c.Send)
		return false
	}
}

func (s *Session) timestamp() string {
	return s.now().UTC().Format("2006-01-02T15:04:05.000Z")
}

func ndjson(raw []byte) []byte {
	if len(raw) > 0 && raw[len(raw)-1] == '\n' {
		return raw
	}
	return append(append([]byte{}, raw...), '\n')
}

func newRequestID() string {
	var b [16]byte
	if _, err := rand.Read(b[:]); err != nil {
		panic(fmt.Sprintf("session: crypto/rand failed: %v", err))
	}
	return "req_" + hex.EncodeToString(b[:])
}
