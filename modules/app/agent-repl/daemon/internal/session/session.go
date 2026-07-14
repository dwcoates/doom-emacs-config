package session

import (
	"crypto/rand"
	"encoding/hex"
	"encoding/json"
	"errors"
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
	// Kill terminates the shim process without waiting for it to drain.
	// Hibernation's escalation path: a shim that ignores the cooperative
	// shutdown command must not be able to pin its ~500MB CLI forever.
	Kill() error
}

// Hibernation refusal reasons. A session that cannot hibernate is a
// normal, expected condition (it is busy, or it has no durable resume
// target yet), never an invariant violation — the sweeper skips it and
// re-checks on its next pass.
var (
	// ErrNoResumeTarget: system:init has not reported the CLI's session
	// uuid, so nothing could bring this session back. Hibernating it
	// would DESTROY it, not save it.
	ErrNoResumeTarget = errors.New("session has no claude_session_id to resume from")
	// ErrTurnActive: a user turn is in flight; killing the CLI now would
	// lose the response it is mid-way through producing.
	ErrTurnActive = errors.New("session has a turn in flight")
	// ErrPermissionPending: an unresolved permission request lives only
	// in the translator's memory and is absent from the transcript, so a
	// revived session could never answer it.
	ErrPermissionPending = errors.New("session has pending permission requests")
	// ErrNotHibernatable: the session is already terminal, already
	// hibernated, or hibernating.
	ErrNotHibernatable = errors.New("session is not in a hibernatable state")
)

// hibernateGrace is how long a hibernating shim is given to honor the
// cooperative shutdown command before it is killed outright.
const hibernateGrace = 5 * time.Second

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

// NewReplayClient returns a client whose outbound buffer can absorb
// this session's ENTIRE retained ring in one burst. replayLocked
// enqueues the full ring on the handler goroutine far faster than the
// socket writer drains it, so a fixed 256-frame buffer drops any
// client replaying a ring bigger than that — which transcript seeding
// (§2.10) makes the norm, not the edge case. The slack beyond
// retention covers live frames broadcast while the replay burst is
// still flushing.
func (s *Session) NewReplayClient() *Client {
	s.mu.Lock()
	defer s.mu.Unlock()
	return &Client{Send: make(chan []byte, s.retention+256)}
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
	// BootID is the owning daemon instance's boot identity (see
	// protocol.HelloFrame.BootID); stamped into every hello.
	BootID string

	shim       ShimHandle
	translator *Translator
	retention  int
	now        func() time.Time
	logf       func(format string, args ...any)
	sentinel   SentinelSink
	registrar  Registrar
	// configDir is the account this session's CLI runs as. Immutable for
	// the session's life, so it needs no lock.
	configDir string
	// Model-drift reconciler clock (§2.12); immutable for the session's life.
	reconcileInterval time.Duration
	reconcileTicks    <-chan time.Time
	// hibernateGrace bounds the wait for a cooperative shutdown before the
	// shim is killed; immutable for the session's life.
	hibernateGrace time.Duration

	mu       sync.Mutex
	seq      int64
	ring     []retained
	clients  map[*Client]struct{}
	terminal bool
	// deathReason classifies HOW a terminal session ended: the shim's
	// closed reason (shutdown / sdk_end / fatal_error), or "shim_died"
	// for a hard death without a closed event. Empty while alive.
	deathReason string
	// registrarClaudeID / registrarTerminal track what the registrar has
	// already been told, so each transition is reported exactly once.
	registrarClaudeID string
	registrarTerminal bool

	// hibernating marks a shim teardown that is a SUSPENSION, not a death.
	// Run consults it on the way out: set, it keeps the ring, the
	// translator, the attached clients, and the non-terminal registry
	// record, so the session survives as a warm cache with no processes.
	// Clear, and Run's exit is the ordinary terminal path.
	hibernating bool
	// hibernated marks the settled suspended state: no shim, but full
	// history. An attach REPLAYS from here without spawning anything;
	// only an act (a message, an interrupt, a UI command) revives it.
	// This is the whole point — viewing a workspace must stay free.
	hibernated bool
	// lastActive stamps the last REAL activity: a shim event, or a client
	// command that acts. Attaching, detaching, replaying, and listing all
	// deliberately do NOT stamp it — a workspace the user merely looks at
	// must still go idle, or the sweeper would never reclaim a session
	// whose webview stays mounted in the background forever.
	lastActive time.Time
	// runDone is closed by each Run on its way out (hibernation OR death),
	// bounding the per-run reconciler goroutine to the shim it checks. A
	// revived session gets a fresh one, so a Run/Hibernate/Run cycle
	// cannot leave two reconcilers behind. Distinct from done, which is
	// closed ONLY on the true terminal transition.
	runDone chan struct{}
	// hibernateDone is set by Hibernate and closed by Run when it settles
	// into the suspended state. It lets a caller (today: tests) observe
	// the transition without racing on the hibernating→hibernated flip,
	// which happens on Run's goroutine after the shim's stream closes.
	hibernateDone chan struct{}

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

// Registrar receives the durable-state transitions the persistent
// session registry records: the arrival (or change) of the CLI session
// uuid, and the session's terminal transition. Optional: a nil
// registrar disables the notifications (tests, embedded use).
// Implementations are called under the session lock and must not call
// back into the session.
type Registrar interface {
	ClaudeSessionIDChanged(sessionID, claudeSessionID string)
	SessionTerminal(sessionID, deathReason string)
}

// Config carries session construction parameters.
type Config struct {
	ID            string
	DaemonVersion string
	// BootID is the daemon instance identity minted at startup.
	BootID string
	Shim   ShimHandle
	// CWD and Model are the CreateOpts-requested values; they seed the
	// translator's hello mirror so introspection works before the SDK's
	// system:init overwrites them with authoritative values.
	CWD   string
	Model string
	// PermissionMode seeds the translator's mode mirror the same way, so a
	// session materialized from a registry record (a cross-restart resume)
	// reports and REVIVES under its persisted mode instead of the default.
	// Empty or invalid leaves the translator's default in place; the SDK's
	// system:init later reports the authoritative value.
	PermissionMode string
	// Retention is the §2.10 replay window in frames (defaults to 4096).
	Retention int
	// Now is the frame-timestamp clock (defaults to time.Now).
	Now func() time.Time
	// Logf receives supervision noise (defaults to log.Printf).
	Logf func(format string, args ...any)
	// Sentinel receives agent-state side-channel notifications; nil
	// disables the sentinel writes.
	Sentinel SentinelSink
	// Registrar receives durable-state transitions for the persistent
	// session registry; nil disables the notifications.
	Registrar Registrar
	// ConfigDir is the CLAUDE_CONFIG_DIR this session's CLI runs under —
	// i.e. WHICH ACCOUNT it is. Carried so the account is answerable for a
	// LIVE session, not just a persisted one: the topbar names it, and the
	// login opens against it.
	//
	// Empty is a real answer, not a missing one — it names the CLI's own
	// default root.
	ConfigDir string
	// ModelReconcileInterval is how often the session re-derives its model
	// from the transcript (§2.12). Zero takes the 30s default; NEGATIVE
	// disables the check, which is what a fake session wants — it has no
	// transcript, so a reconciler could only ever log that it cannot find
	// one.
	ModelReconcileInterval time.Duration
	// ModelReconcileTicks overrides the reconciler's clock. Nil mints a
	// real ticker; tests inject a channel so the check runs on demand
	// rather than on a wall clock.
	ModelReconcileTicks <-chan time.Time
	// HibernateGrace is how long a hibernating shim is given to honor the
	// cooperative shutdown before it is killed. Zero takes the default
	// (hibernateGrace); tests shrink it to exercise the escalation path.
	HibernateGrace time.Duration
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
	if cfg.ModelReconcileInterval == 0 {
		cfg.ModelReconcileInterval = DefaultModelReconcileInterval
	}
	if cfg.HibernateGrace <= 0 {
		cfg.HibernateGrace = hibernateGrace
	}
	translator := NewTranslator()
	translator.CWD = cfg.CWD
	translator.Model = cfg.Model
	if cfg.PermissionMode != "" && protocol.ValidPermissionMode(cfg.PermissionMode) {
		translator.PermissionMode = protocol.PermissionMode(cfg.PermissionMode)
	}
	return &Session{
		ID:            cfg.ID,
		DaemonVersion: cfg.DaemonVersion,
		BootID:        cfg.BootID,
		shim:          cfg.Shim,
		// A nil shim is a session born HIBERNATED: a dormant registry
		// record materialized to serve an attach (history from the
		// transcript) without paying for a CLI nobody has prompted yet.
		// Revive gives it a shim when someone finally acts.
		hibernated:        cfg.Shim == nil,
		translator:        translator,
		retention:         cfg.Retention,
		now:               cfg.Now,
		logf:              cfg.Logf,
		sentinel:          cfg.Sentinel,
		registrar:         cfg.Registrar,
		configDir:         cfg.ConfigDir,
		reconcileInterval: cfg.ModelReconcileInterval,
		reconcileTicks:    cfg.ModelReconcileTicks,
		hibernateGrace:    cfg.HibernateGrace,
		clients:           map[*Client]struct{}{},
		lastActive:        cfg.Now(),
		done:              make(chan struct{}),
	}
}

// Info is a point-in-time introspection snapshot of a session.
type Info struct {
	CWD             string
	Model           string
	ClaudeSessionID string
	// ConfigDir is the session's CLAUDE_CONFIG_DIR (the account it runs
	// as). Empty names the CLI's own default root.
	ConfigDir string
	Terminal  bool
	// DeathReason classifies a terminal session's end (closed reason or
	// "shim_died"); empty while alive.
	DeathReason string
	// TurnActive reports whether a user turn is in flight.
	TurnActive bool
	// PendingPermissions lists unresolved permission request ids, sorted.
	PendingPermissions []string
	// Hibernated reports that the CLI process pair has been freed while
	// the conversation stays fully replayable. NOT a kind of terminal:
	// the session still answers, still lists, and revives on the next act.
	Hibernated bool
	// PermissionMode is the session's current mode. Carried so a revive
	// can respawn the CLI under the mode the session actually holds now,
	// rather than the one it was created with.
	PermissionMode protocol.PermissionMode
}

// Info returns the session's current introspection snapshot: the
// requested-then-authoritative cwd/model mirror, the durable CLI session
// id (empty until system:init), terminality, and the reconcile fields
// (death reason, turn activity, pending permissions) that let a
// level-triggered poller re-derive agent state from daemon truth.
func (s *Session) Info() Info {
	s.mu.Lock()
	defer s.mu.Unlock()
	return Info{
		CWD:                s.translator.CWD,
		Model:              s.translator.Model,
		ClaudeSessionID:    s.translator.ClaudeSessionID,
		ConfigDir:          s.configDir,
		Terminal:           s.terminal,
		DeathReason:        s.deathReason,
		TurnActive:         s.translator.TurnActive(),
		PendingPermissions: s.translator.PendingPermissionIDs(),
		Hibernated:         s.hibernated,
		PermissionMode:     s.translator.PermissionMode,
	}
}

// Run consumes the shim event stream until it closes. Blocks; run it on
// its own goroutine. All translation happens here, so the translator
// needs no locking of its own.
func (s *Session) Run() {
	s.mu.Lock()
	shim := s.shim
	if shim == nil {
		// Run with no shim is a caller bug: a hibernated session has
		// nothing to consume. Revive installs the shim, THEN starts Run.
		s.mu.Unlock()
		panic(fmt.Sprintf("session %s: Run with no shim", s.ID))
	}
	runDone := make(chan struct{})
	s.runDone = runDone
	s.mu.Unlock()

	// The model mirror's safety net (§2.12). Bounded by runDone, not by
	// s.done: a hibernating session's Run returns WITHOUT ending the
	// session, and the reconciler must die with the shim it checks or a
	// revive would leave a second one running behind it.
	go s.runModelReconciler(runDone, s.reconcileTicks, s.reconcileInterval)
	for evt := range shim.Events() {
		s.mu.Lock()
		// A cooperative hibernation makes the shim emit its own `closed`
		// (reason "shutdown") on the way out. That is the EXPECTED drain,
		// not a death: swallow it whole so the session stays non-terminal,
		// the registry record keeps rehydrating, and no `closed` frame
		// reaches an attached client to report a session that is merely
		// suspended as gone. Run then exits the loop on the stream close
		// below and takes the hibernation branch.
		if evt.Type == "closed" && s.hibernating {
			s.mu.Unlock()
			continue
		}
		s.lastActive = s.now()
		frames := s.translator.OnEvent(evt)
		s.broadcastLocked(frames)
		if evt.Type == "closed" {
			s.terminal = true
			s.deathReason = evt.Reason
		}
		s.notifyRegistrarLocked()
		s.mu.Unlock()
	}

	s.mu.Lock()
	if s.hibernating {
		// SUSPENSION, not death. Everything that makes this session
		// answerable survives: the ring (so an attach still replays the
		// whole conversation), the translator (so tool metadata and the
		// model mirror hold), the attached clients (so no socket churns
		// and no frontend sees a "session gone"), and the non-terminal
		// registry record (so the Emacs reattach sweep leaves it alone).
		// The ONLY thing freed is the CLI process pair.
		s.hibernating = false
		s.hibernated = true
		s.shim = nil
		close(runDone)
		if s.hibernateDone != nil {
			close(s.hibernateDone)
		}
		nRing, nClients := len(s.ring), len(s.clients)
		s.mu.Unlock()
		s.logf("session %s: hibernated (idle) — CLI freed, %d frames retained, %d clients still attached",
			s.ID, nRing, nClients)
		return
	}
	if !s.terminal {
		// Shim stdout closed without a `closed` event: hard death.
		// Cancel pending permission prompts FIRST (§2.7 "cancel" on shim
		// death) so no tab is left with a live prompt for a dead shim,
		// then surface the error frame.
		s.terminal = true
		s.deathReason = "shim_died"
		s.broadcastLocked(s.translator.OnShimDeath())
		s.broadcastLocked([]protocol.L2Frame{&protocol.ErrorFrame{
			Envelope:    protocol.Envelope{Type: "error"},
			Code:        "shim_died",
			Message:     "shim exited without a closed event",
			Recoverable: false,
		}})
	}
	s.notifyRegistrarLocked()
	for c := range s.clients {
		close(c.Send)
		delete(s.clients, c)
	}
	close(runDone)
	s.mu.Unlock()
	close(s.done)
}

// Hibernate suspends an IDLE session: it asks the shim to drain and exit,
// freeing the ~500MB node+CLI process pair, while the session itself
// stays live in the server's map as a warm, fully-replayable cache.
//
// It is deliberately NOT a teardown. The session stays non-terminal and
// stays listed, because both frontends treat "absent or terminal" as
// "recreate it": the Emacs reattach sweep would POST a new session within
// 15s, and the webapp's exists-probe would declare the session gone.
// Hibernation has to be invisible to them, and it is.
//
// Returns a non-nil error when the session must NOT be suspended, which
// the sweeper treats as "skip and re-check next pass", never as a
// failure. Refusal is how a busy session protects itself.
func (s *Session) Hibernate(reason string) error {
	s.mu.Lock()
	if s.terminal || s.hibernated || s.hibernating || s.shim == nil {
		s.mu.Unlock()
		return ErrNotHibernatable
	}
	// The resume target is the whole basis of recovery: with no CLI
	// session uuid there is no --resume to come back through, so
	// suspending would be destroying. system:init has simply not landed
	// yet; the next sweep will find it.
	if s.translator.ClaudeSessionID == "" {
		s.mu.Unlock()
		return ErrNoResumeTarget
	}
	// A turn in flight is unfinished work that lives only in the CLI.
	if s.translator.TurnActive() {
		s.mu.Unlock()
		return ErrTurnActive
	}
	// A pending permission request exists ONLY in the translator's memory
	// and never reaches the transcript, so a revived CLI could not be
	// answered about it. Wait for the human.
	if len(s.translator.PendingPermissionIDs()) > 0 {
		s.mu.Unlock()
		return ErrPermissionPending
	}
	s.hibernating = true
	s.hibernateDone = make(chan struct{})
	shim := s.shim
	runDone := s.runDone
	s.mu.Unlock()

	// Cooperative first: `shutdown` lets the CLI flush its transcript,
	// which is the very thing a revive resumes from. A truncated
	// transcript would be an unrecoverable session, so the graceful path
	// is not a nicety here — it is the correctness path.
	if err := shim.Send(protocol.NewShutdownCmd(newRequestID(), reason)); err != nil {
		// It cannot be asked nicely, so take it down anyway rather than
		// leak the process pair. The error is surfaced, not swallowed.
		s.logf("session %s: hibernate shutdown command failed, killing shim: %v", s.ID, err)
		if killErr := shim.Kill(); killErr != nil {
			return fmt.Errorf("session %s: hibernate: shutdown failed (%w) and kill failed: %w", s.ID, err, killErr)
		}
		return nil
	}
	// Escalation: a shim that ignores shutdown must not pin its CLI's
	// half-gigabyte forever. Wait for Run to observe the event stream
	// close (runDone), and kill if it does not.
	go func() {
		select {
		case <-runDone:
		case <-time.After(s.hibernateGrace):
			s.logf("session %s: shim ignored hibernate shutdown after %s — killing", s.ID, s.hibernateGrace)
			if err := shim.Kill(); err != nil {
				s.logf("session %s: hibernate kill FAILED, the CLI process pair is leaked: %v", s.ID, err)
			}
		}
	}()
	return nil
}

// Revive re-arms a hibernated session with a freshly spawned shim (the
// server spawns it with --resume, pointed at the transcript the CLI has
// been writing all along).
//
// Nothing is re-seeded: the ring, the seq watermark, the translator, and
// the attached clients all survived hibernation, so history is already
// in memory and the clients never knew it went away. Re-seeding here
// would duplicate the whole conversation.
func (s *Session) Revive(shim ShimHandle) error {
	s.mu.Lock()
	if s.terminal {
		s.mu.Unlock()
		return fmt.Errorf("session %s: cannot revive a terminal session", s.ID)
	}
	if !s.hibernated {
		s.mu.Unlock()
		return fmt.Errorf("session %s: cannot revive a session that is not hibernated", s.ID)
	}
	s.shim = shim
	s.hibernated = false
	s.lastActive = s.now()
	s.mu.Unlock()
	s.logf("session %s: revived on demand", s.ID)
	go s.Run()
	return nil
}

// Hibernated reports whether the session is suspended (no shim, but full
// retained history). An attach to one of these must NOT revive it.
func (s *Session) Hibernated() bool {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.hibernated
}

// HibernateDone returns the channel that closes when the pending
// Hibernate settles into the suspended state (the shim's stream has
// closed and Run has freed it). Nil before any Hibernate call. A
// supervisor uses it to know a suspension has fully landed before, say,
// reviving on a racing act.
func (s *Session) HibernateDone() <-chan struct{} {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.hibernateDone
}

// IdleFor reports how long the session has gone without real activity
// (shim events, acting client commands). Attaching and replaying do not
// count, so a workspace that is merely on screen still goes idle.
func (s *Session) IdleFor(now time.Time) time.Duration {
	s.mu.Lock()
	defer s.mu.Unlock()
	return now.Sub(s.lastActive)
}

// notifyRegistrarLocked reports durable-state transitions (the CLI
// session uuid arriving or changing, the terminal transition) to the
// registrar, each exactly once per value. Runs under s.mu; the
// registrar only touches its own state (the write-through registry),
// so no lock cycle is possible. Disk I/O under the session lock is
// deliberate and cheap: each session transitions at most a handful of
// times over its whole life.
func (s *Session) notifyRegistrarLocked() {
	if s.registrar == nil {
		return
	}
	if id := s.translator.ClaudeSessionID; id != "" && id != s.registrarClaudeID {
		s.registrarClaudeID = id
		s.registrar.ClaudeSessionIDChanged(s.ID, id)
	}
	if s.terminal && !s.registrarTerminal {
		s.registrarTerminal = true
		s.registrar.SessionTerminal(s.ID, s.deathReason)
	}
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
		BootID:          s.BootID,
		ProtocolVersion: protocol.Layer2Version,
		ResumeFromSeq:   resumeFrom,
		PermissionMode:  s.translator.PermissionMode,
		Model:           s.translator.Model,
		Models:          s.translator.Models,
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
	// history instead of staring at an empty feed. It works on HIBERNATED
	// sessions for the same reason, and it must never revive one — replay
	// is served wholly from the ring, and answering it is exactly the
	// "look at a workspace for free" case hibernation exists to protect.
	if cmd.Type == "replay-request" {
		s.replayLocked(c, cmd.FromSeq)
		return nil
	}
	if s.terminal {
		return fmt.Errorf("session %s: command %s on terminal session", s.ID, cmd.Type)
	}
	// Every command below ACTS, so it needs a live CLI. The server revives
	// the session before handing the frame over (Server.resolveForAct); a
	// hibernated session reaching here means that contract was broken, and
	// the command would otherwise nil-panic on s.shim.
	if s.hibernated || s.shim == nil {
		return fmt.Errorf("session %s: command %s on a hibernated session: it must be revived first", s.ID, cmd.Type)
	}
	// This is real activity, so it defers hibernation.
	s.lastActive = s.now()

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
	case "set-model":
		s.translator.OnSetModelCmd(cmd)
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
	// A hibernated session has no shim to ask. There is no Run in flight
	// to observe a death either, so the terminal transition has to be made
	// here: mark it, tell the registrar (so the record stops rehydrating),
	// and release the clients — the same end state Run would have reached.
	if s.hibernated || s.shim == nil {
		s.terminal = true
		s.deathReason = reason
		s.notifyRegistrarLocked()
		for c := range s.clients {
			close(c.Send)
			delete(s.clients, c)
		}
		s.mu.Unlock()
		close(s.done)
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
		if env.TS == "" {
			// Transcript-replayed frames arrive pre-stamped with the
			// original event's time (§2.1); only live frames are stamped here.
			env.TS = s.timestamp()
		}
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
