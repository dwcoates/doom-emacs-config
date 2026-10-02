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
	// Fake adds --fake. The spawn is ALSO forced fake when ForbidVendor is
	// set or when this daemon's own contracts forbid vendor calls: under the
	// guard a real-vendor shim is not something the daemon may start, and
	// forcing the fake is how a guarded run still creates and forks
	// workspaces instead of being refused (see `fakeMode`).
	Fake bool
	// LogSink is the already-open shim log sink passed as fd 3. It is NEVER a
	// pipe to the daemon's stderr.
	LogSink *os.File
	// ForbidVendor sets AGENT_REPL_FORBID_VENDOR_CALLS=1 (tests). It also
	// forces the spawn into fake mode, because a child told the vendor is
	// forbidden must not be asked to reach it.
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
	// Spawned is called with the child's pid the INSTANT the fork returns,
	// synchronously, before the bring-up this Spawn then blocks on.
	//
	// IT IS THE ONLY MOMENT THAT WILL DO. Spawn does not return at the fork:
	// it returns once the shim has bound its socket, dialed and pushed its
	// first diagnostics -- ~110ms of Node startup on a healthy box -- and a
	// daemon killed inside that window leaves a shim that holds no lock, is
	// bound to nothing and is recorded nowhere. Its successor then reads "lock
	// free, socket absent", spawns a second shim onto the one session socket,
	// and that shim refuses the bind and dies. The callback is where the pid
	// is made DURABLE (wsm.SetSpawnedShimPID) so the successor can tell a
	// starting shim from no shim at all.
	//
	// It must not block on anything but a local write: the supervisor calls it
	// on the spawn's own goroutine, between the fork and the bring-up.
	Spawned func(pid int)
}

// Supervisor brings shim processes up and adopts surviving ones.
type Supervisor interface {
	// Spawn starts a shim, dials it, and returns once WatchSession is
	// connected and the shim has pushed its first diagnostics arm. An
	// UNHEALTHY arm is an ANSWER and completes the bring-up; its faults reach
	// the workspace health path through the watcher the caller then attaches.
	// A dead process ends bring-up at once with its exit decoding and stderr
	// ring, never a timeout.
	Spawn(ctx context.Context, spec Spec) (Client, error)
	// Adopt dials a shim that is already running — a crash boot's surviving
	// process, or a handover's transferred one — and supervises it without
	// spawning. In-flight work survives; a surviving shim is never
	// killed-and-restarted. The workspace dir is required because every
	// record an adopted client writes is workspace-bound (a global write would
	// be the invariant violation) and because the adopted-death witness is
	// keyed on the workspace's kernel lock.
	Adopt(ctx context.Context, ws ids.WorkspaceID, workspaceDir, udsPath string) (Client, error)
	// StandDownEverySpawn force-kills every process this supervisor STARTED
	// and still owns, and returns every kill that failed joined together.
	//
	// It exists for the one caller that cannot go through the workspace fleet:
	// the immediate shutdown. A shim enters the fleet's session map only once
	// bring-up has returned healthy and StartSession has answered, so a spawn
	// still inside that window is known to NOBODY but this supervisor, and the
	// fleet's own stand-down walk steps straight past it. A BOUNCE is not this
	// case and must not call this: its shims are handed to a successor through
	// Client.Detach, which takes them out of the registry this sweeps.
	StandDownEverySpawn(ctx context.Context, reason string) error
	// BeginStandDown latches the stand-down without sweeping anything, and
	// answers whether this call was the one that latched it. The immediate
	// shutdown calls it FIRST, so every client this supervisor handed out
	// reads an ordered departure as one from the moment the shutdown begins
	// rather than from the moment its sweep is reached.
	BeginStandDown() bool
	// SpawnedFor answers whether this supervisor still owns a shim it
	// STARTED for that workspace, with its pid. The bring-up's adoption path
	// reads it before it adopts: a shim this daemon spawned and still
	// supervises must never become a SECOND client of the same process.
	SpawnedFor(ws ids.WorkspaceID) (int, bool)
	// StandingDown answers the latch. A bring-up reads it before it spawns:
	// a daemon that is leaving must not start a process nothing will be left
	// to stop.
	StandingDown() bool
}

// Client is one shim connection. Every verb is mutex-guarded by the internal
// occupancy guard; lease POLICY is the caller's, not the client's. The
// workflow verbs (GetWorkflow, WatchWorkflow, StopWorkflow) are deliberately
// not exposed: workflow is kicked, and no watch is ever opened for one.
type Client interface {
	// StartSession starts or resumes the session. Session facts travel only
	// here.
	StartSession(ctx context.Context, req *shimv1.StartSessionRequest) (*shimv1.StartSessionResponse, error)
	// WatchSession opens the session frame stream. Its first diagnostics push
	// is the readiness signal, healthy or not. The FRAME is handed on whole:
	// a frame is either a SessionUpdate or the landing-7 re-announcement of
	// the session's own SessionStarted, and the consumer tells them apart.
	WatchSession(ctx context.Context) (Stream[*shimv1.WatchSessionResponse], error)
	// SetSessionModel switches the session's model; the cold arm is an answer.
	SetSessionModel(ctx context.Context, req *shimv1.SetSessionModelRequest) (*shimv1.SetSessionModelResponse, error)
	// SetSessionEffort switches the session's reasoning effort from the next
	// turn on; it resolves once the current turn has ended.
	SetSessionEffort(ctx context.Context, req *shimv1.SetSessionEffortRequest) (*shimv1.SetSessionEffortResponse, error)
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
	// RollBackSession rewinds the main agent's vendor conversation to just
	// before one of its prompts.
	RollBackSession(ctx context.Context, req *shimv1.RollBackSessionRequest) (*shimv1.RollBackSessionResponse, error)
	// WatchBash opens one detached shell's stream.
	WatchBash(ctx context.Context, work *conversationv1.DetachedWorkId) (Stream[*conversationv1.AgentBash], error)
	// StopBash stops one detached shell.
	StopBash(ctx context.Context, req *shimv1.StopBashRequest) (*shimv1.StopBashResponse, error)
	// DetachForeground detaches a running foreground unit.
	DetachForeground(ctx context.Context, req *shimv1.DetachForegroundRequest) (*shimv1.DetachForegroundResponse, error)
	// ReadHistory pages an agent's history without opening a watch.
	ReadHistory(ctx context.Context, req *shimv1.ReadHistoryRequest) (*shimv1.ReadHistoryResponse, error)
	// ReadTranscripts lists every vendor conversation filed under the shim's
	// own working directory, read from the transcripts' own lines. It starts
	// no query and makes no model call.
	ReadTranscripts(ctx context.Context, req *shimv1.ReadTranscriptsRequest) (*shimv1.ReadTranscriptsResponse, error)
	// GatherTitleDigest reads the transcript for the material the daemon
	// synthesizes a workspace title from when the vendor wrote no ai-title.
	GatherTitleDigest(ctx context.Context, req *shimv1.GatherTitleDigestRequest) (*shimv1.GatherTitleDigestResponse, error)

	// Occupy takes the in-memory occupancy guard that backs WSM's lease row,
	// returning the release function. It refuses while another holder has it.
	Occupy(holder string) (release func(), err error)

	// Exited yields exactly one ExitInfo when the process is gone, then
	// closes. It carries the exit decoding and the stderr ring as evidence.
	Exited() <-chan ExitInfo
	// Reaped answers the decoded exit WITHOUT consuming Exited, whose channel
	// carries exactly one value and is therefore owned by a single waiter. A
	// second party that needs the exit code as EVIDENCE — the watcher raising
	// the session's shim_died fault — reads it here instead of racing that
	// waiter for the value.
	Reaped() (ExitInfo, bool)
	// StandingDown answers whether a KillSession has been asked of this shim.
	//
	// IT IS THE ONE PLACE THE DAEMON'S OWN TEARDOWN IS RECORDED, and every
	// route to ending a session -- `Fleet.KillSession`, the verbs' kill and
	// nuke, the rollout's stand-down, the drain's sweep -- reaches the shim
	// through `KillSession` above, which latches it before the verb goes. A
	// consumer that reads it therefore cannot be bypassed by a caller that
	// forgot to announce the teardown out of band; a caller that forgot is
	// exactly how a deliberate stand-down came to be recorded as a transport
	// fault.
	//
	// The latch is one-way: a shim asked to end its session is never asked to
	// un-end it, and a revival is a new process with a new client.
	StandingDown() bool
	// StandDown ARMS the latch for a teardown this daemon is ordering, and
	// answers whether it was armed.
	//
	// It exists because the ask does not always reach the shim: a
	// `KillSession` that never answers is escalated to a process stop by the
	// daemon itself, and the escalation is the ordered teardown. Arming it
	// here, BEFORE the escalation, is what lets the exit watcher, the redialer
	// and the adopted-death witness read that departure as ordinary rather
	// than as a shim that died on its own.
	//
	// A DETACHED CLIENT ARMS NOTHING and answers false: that process belongs
	// to the successor daemon, so this daemon is ordering no teardown of it.
	// The latch is one-way, so arming an already-armed client is a no-op.
	StandDown() bool
	// Kill stops the process, recording who asked and why. ctx bounds the
	// call's WAITS -- the SIGTERM grace and the wait for the exit decode --
	// and never the reap itself, which runs on the client's own goroutine. A
	// ctx that ends inside the grace ESCALATES to the SIGKILL rather than
	// leaving a signalled process standing, and says so in its error.
	Kill(ctx context.Context, attr KillAttribution) error
	// Detach stops supervising while LEAVING THE PROCESS RUNNING — the
	// handover's per-workspace transfer.
	Detach()
	// Connectivity yields every link state change: dialing, connected,
	// redialing, dead. Redial is forever while the process lives.
	Connectivity() <-chan LinkState
	// Connections counts the links established so far. It is advanced BEFORE
	// the LinkConnected that announces each one is published, so a consumer
	// that recorded the count when it opened its streams can tell, at the
	// moment those streams break, whether the link has already come back
	// since -- which a LinkConnected it consumed earlier cannot tell it.
	Connections() uint64
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
	// Inferred reports that NO wait status was ever observed and the departure
	// was CONCLUDED from evidence — a refused socket over a free workspace
	// lock, or a socket already gone when the daemon went to stop it.
	//
	// It exists because an adopted shim is not this daemon's child, so there is
	// no exit to decode and Code carries the sentinel -1 rather than a status.
	// Read as a status, that sentinel says "signalled death" about a shim this
	// daemon asked to leave and that left exactly as asked. Nothing else may
	// set it: an inferred departure NOBODY asked for is still a death, and
	// still loud.
	Inferred bool
	// At is when this client concluded the process was gone: the wait status
	// was read, or the departure was inferred. Nothing the process wrote can
	// be later than it, which is what lets a bring-up account for the last
	// writer of the process's transcript.
	At time.Time
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

// Option adjusts the supervisor. Every option exists because a peer must be
// able to state a policy the client itself must not own.
type Option func(*supervisor)

// WithBackoff replaces the redial schedule. Tests make it instant; nothing
// about the client's behavior depends on the delays.
func WithBackoff(initial, max time.Duration, factor float64) Option {
	return func(s *supervisor) { s.back = backoff{Initial: initial, Max: max, Factor: factor} }
}

// WithKillGrace replaces how long a SIGTERMed shim has before the SIGKILL. A
// caller that shortens it must shorten its own bound with it: see
// GracefulKillBound.
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
	s := &supervisor{surfaces: log, back: defaultBackoff, grace: DefaultKillGrace}
	for _, opt := range opts {
		opt(s)
	}
	return s, nil
}
