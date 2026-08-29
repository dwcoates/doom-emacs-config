// Package shimclient is the only module that dials shim.v1, and it is also the
// shim PROCESS SUPERVISOR.
//
// It is a leaf: it knows no other daemon package (beyond ids and dlog) and
// carries no policy. It spawns with process-group discipline, kills with stop
// attribution, reaps with exit decoding, keeps the stderr ring buffer as
// failure evidence, and correlates spawn death against connect so a dead
// process ends bring-up immediately with exit and stderr rather than a
// timeout. It redials FOREVER with backoff when a still-running shim's
// connection breaks; retry-vs-give-up is decided by EVIDENCE, never by a
// count. See docs/overhaul/daemon.md decision 2 and ARCHITECTURE.md
// "shimclient".
package shimclient

import (
	"context"
	"errors"
	"os"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
)

// Spec is everything a spawn needs. It reproduces the common spawn contract in
// ARCHITECTURE.md verbatim; nothing else assembles a shim command line.
type Spec struct {
	// WorkspaceID identifies the workspace the shim serves.
	WorkspaceID ids.WorkspaceID
	// WorkspaceDir is the spawned process's cwd.
	WorkspaceDir string
	// UDSPath is the --listen socket.
	UDSPath string
	// StoreSocket is the --store-socket path.
	StoreSocket string
	// ConfigDir is CLAUDE_CONFIG_DIR: the account root.
	ConfigDir string
	// ShimBuildSHA is SHIM_BUILD_SHA.
	ShimBuildSHA string
	// NodeBin is the node binary.
	NodeBin string
	// MainJS is agent-shim/claude/shim/dist/main.js.
	MainJS string
	// Fake adds --fake.
	Fake bool
	// LogSink is the already-open shim log sink passed as fd 3. It is NEVER a
	// pipe to the daemon's stderr.
	LogSink *os.File
	// ForbidVendor sets AGENT_REPL_FORBID_VENDOR_CALLS=1 (tests).
	ForbidVendor bool
	// StateDir is AGENT_REPL_STATE_DIR for the child: the ONE state root the
	// daemon, Emacs and the skills must all resolve. Empty inherits this
	// daemon's own, which is correct only when the daemon took its own from
	// the environment rather than from -state-dir.
	StateDir string
	// SessionID is AGENT_REPL_SESSION_ID, the host session identity, for LOG
	// CORRELATION only; session facts still travel exclusively in
	// StartSession. Empty omits it.
	SessionID string
}

// Supervisor brings shim processes up and adopts surviving ones.
type Supervisor interface {
	// Spawn starts a shim, dials it, and returns once WatchSession is
	// connected and the first pushed diagnostics arm says healthy. A dead
	// process ends bring-up at once with its exit decoding and stderr ring,
	// never a timeout.
	Spawn(ctx context.Context, spec Spec) (Client, error)
	// Adopt dials a shim that is already running — a crash boot's surviving
	// process, or a handover's transferred one — and supervises it without
	// spawning. In-flight work survives; a surviving shim is never
	// killed-and-restarted. The workspace dir is required because every
	// record an adopted client writes is workspace-bound (a global write would
	// be the invariant violation) and because the adopted-death witness is
	// keyed on the workspace's kernel lock.
	Adopt(ctx context.Context, ws ids.WorkspaceID, workspaceDir, udsPath string) (Client, error)
}

// Client is one shim connection. Every verb is mutex-guarded by the internal
// occupancy guard; lease POLICY is the caller's, not the client's. The
// workflow verbs (GetWorkflow, WatchWorkflow, StopWorkflow) are deliberately
// not exposed: workflow is kicked, and no watch is ever opened for one.
type Client interface {
	// StartSession starts or resumes the session. Session facts travel only
	// here.
	StartSession(ctx context.Context, req *shimv1.StartSessionRequest) (*shimv1.StartSessionResponse, error)
	// WatchSession opens the session update stream. Its first healthy
	// diagnostics push is the readiness signal.
	WatchSession(ctx context.Context) (Stream[*conversationv1.SessionUpdate], error)
	// SetSessionModel switches the session's model; the cold arm is an answer.
	SetSessionModel(ctx context.Context, req *shimv1.SetSessionModelRequest) (*shimv1.SetSessionModelResponse, error)
	// SetSessionPermissionMode switches the session's permission mode.
	SetSessionPermissionMode(ctx context.Context, req *shimv1.SetSessionPermissionModeRequest) (*shimv1.SetSessionPermissionModeResponse, error)
	// Hibernate stands the session down for the idle sweep. It is NOT used by
	// the relaunch engine, which kills gracefully instead.
	Hibernate(ctx context.Context, req *shimv1.HibernateRequest) (*shimv1.HibernateResponse, error)
	// KillSession ends the session, gracefully unless forced.
	KillSession(ctx context.Context, req *shimv1.KillSessionRequest) (*shimv1.KillSessionResponse, error)
	// StartTurn opens a turn with the daemon's minted TurnId and the required
	// origin, which the shim persists onto the AgentPrompt.
	StartTurn(ctx context.Context, req *shimv1.StartTurnRequest) (*shimv1.StartTurnResponse, error)
	// WatchAgent opens one agent's frame stream, opening with a catch-up page.
	WatchAgent(ctx context.Context, req *shimv1.WatchAgentRequest) (Stream[*shimv1.WatchAgentResponse], error)
	// UpdateAgent delivers an answer, a consent, a prompt or a stop to an
	// agent.
	UpdateAgent(ctx context.Context, req *shimv1.UpdateAgentRequest) (*shimv1.UpdateAgentResponse, error)
	// KillTurn interrupts the open turn, forced when the confirm challenge was
	// answered.
	KillTurn(ctx context.Context, req *shimv1.KillTurnRequest) (*shimv1.KillTurnResponse, error)
	// WatchBash opens one detached shell's stream.
	WatchBash(ctx context.Context, work *conversationv1.DetachedWorkId) (Stream[*conversationv1.AgentBash], error)
	// StopBash stops one detached shell.
	StopBash(ctx context.Context, req *shimv1.StopBashRequest) (*shimv1.StopBashResponse, error)
	// DetachForeground detaches a running foreground unit.
	DetachForeground(ctx context.Context, req *shimv1.DetachForegroundRequest) (*shimv1.DetachForegroundResponse, error)
	// ReadHistory pages an agent's history without opening a watch.
	ReadHistory(ctx context.Context, req *shimv1.ReadHistoryRequest) (*shimv1.ReadHistoryResponse, error)

	// Occupy takes the in-memory occupancy guard that backs WSM's lease row,
	// returning the release function. It refuses while another holder has it.
	Occupy(holder string) (release func(), err error)

	// Exited yields exactly one ExitInfo when the process is gone, then
	// closes. It carries the exit decoding and the stderr ring as evidence.
	Exited() <-chan ExitInfo
	// Kill stops the process, recording who asked and why.
	Kill(attr KillAttribution) error
	// Detach stops supervising while LEAVING THE PROCESS RUNNING — the
	// handover's per-workspace transfer.
	Detach()
	// Connectivity yields every link state change: dialing, connected,
	// redialing, dead. Redial is forever while the process lives.
	Connectivity() <-chan LinkState
	// PID is the supervised process's pid.
	PID() int
}

// Stream is one server stream. Recv returns io.EOF only on a producer-side
// end; the CONSUMER decides whether that is a transport failure, because only
// the consumer knows whether the stream should still be open.
type Stream[T any] interface {
	// Recv blocks for the next frame.
	Recv() (T, error)
	// Close ends the stream from this side.
	Close()
}

// LinkState is the daemon-to-shim hop of connectivity truth.
type LinkState int

// The link states. They are the keys of render-colors.json's
// topbar_connectivity table.
const (
	// LinkDialing is a connection being established for the first time.
	LinkDialing LinkState = iota
	// LinkConnected is a serving connection.
	LinkConnected
	// LinkRedialing is a broken connection to a still-running process, being
	// retried forever with backoff.
	LinkRedialing
	// LinkDead is a connection whose process is gone. Redial stops here,
	// because the evidence — not a retry count — decided.
	LinkDead
)

// ExitInfo is a dead shim's decoded exit plus the evidence kept for it.
type ExitInfo struct {
	// PID is the process that exited.
	PID int
	// Code is the exit status, when it exited normally.
	Code int
	// Signal is the signal that killed it, empty when none did.
	Signal string
	// Stderr is the captured stderr ring buffer: the failure evidence.
	Stderr string
	// Attribution is the kill this daemon asked for, nil when the process died
	// on its own. This is how a supervised kill is told from a crash.
	Attribution *KillAttribution
}

// KillAttribution records who asked for a kill and why, so a supervised stop
// is never misread as a crash.
type KillAttribution struct {
	// Actor names the asking component ("workspace.kill", "rollout.relaunch",
	// "drain").
	Actor string
	// Reason is the human-readable cause.
	Reason string
	// Force reports whether the kill skipped the graceful path.
	Force bool
}

// DefaultPageSize is the opening page budget every paged request carries. The
// shim REFUSES page_size == 0, so no request is ever sent with a zero-as-
// default; a caller with its own budget states it instead.
const DefaultPageSize uint32 = 50

// Option adjusts the supervisor. Every option exists because a peer must be
// able to state a policy the client itself must not own.
type Option func(*supervisor)

// WithBackoff replaces the redial schedule. Tests make it instant; nothing
// about the client's behavior depends on the delays.
func WithBackoff(initial, max time.Duration, factor float64) Option {
	return func(s *supervisor) { s.back = backoff{Initial: initial, Max: max, Factor: factor} }
}

// WithKillGrace replaces how long a SIGTERMed shim has before the SIGKILL.
func WithKillGrace(grace time.Duration) Option {
	return func(s *supervisor) { s.grace = grace }
}

// WithLockProbe supplies the ADOPTED-DEATH WITNESS: given a workspace dir, it
// answers whether that workspace's kernel lock reads FREE. sessionlock is
// shimclient's PEER, not its dependency, so the probe is injected by the
// component that owns both (boot, rollout). Without it an adopted shim's
// broken link is redialed forever — which is correct, because nothing has
// witnessed a death.
func WithLockProbe(probe func(workspaceDir string) (free bool, err error)) Option {
	return func(s *supervisor) { s.lockProbe = probe }
}

// NewSupervisor builds the supervisor. It is the daemon's only one.
func NewSupervisor(log dlog.Surfaces, opts ...Option) (Supervisor, error) {
	if log == nil {
		return nil, errors.New("shimclient: log surfaces are required")
	}
	s := &supervisor{surfaces: log, back: defaultBackoff, grace: defaultKillGrace}
	for _, opt := range opts {
		opt(s)
	}
	return s, nil
}
