// frontendcmd.go binds the daemon's frontend.v1 command surface: the
// FrontendCommand handler that routes each inbound command to the module that
// owns it (design §14.2 point 1, §5.4), and the SSM-backed StateProvider the
// frontend server snapshots on every (re)connect.
//
// The handler dispatches through NARROW injected interfaces (PromptRouter,
// MergeRunner, WorkspaceLifecycle) rather than reaching into the modules
// directly, so the routing is unit-testable and the concrete bindings (the
// per-session shimclient, the merge Engine, the Emacs workspace-command
// channel) are assembled by WireAgentShim / main.go. Every path surfaces its
// failure as a CommandAck error via the returned error — never a silent drop.
package server

import (
	"context"
	"fmt"
	"path/filepath"
	"sync"
	"sync/atomic"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/frontend"
	"claude-repld/internal/ssm"

	"google.golang.org/protobuf/encoding/protojson"
	"google.golang.org/protobuf/types/known/structpb"
)

// PromptRouter forwards conversation control to a workspace's session shim
// (the per-session shimclient). A workspace with no live shim connection must
// return a loud error, never a silent no-op — the frontend renders the failed
// CommandAck.
type PromptRouter interface {
	// SubmitPrompt carries the COMMAND'S OWN request id through to the driver:
	// it is what the daemon's immediate prompt receipt is keyed on, and what
	// the durable transcript line is later stamped with, so a frontend
	// reconciles the two onto one bubble.
	SubmitPrompt(ctx context.Context, workspace, requestID, text, permissionMode string) error
	Interrupt(ctx context.Context, workspace string) error
	AnswerPermission(ctx context.Context, workspace, permissionRequestID string, allow bool, denyMessage string, updatedInput *structpb.Struct) error
}

// SessionHealthRouter proves a named session's existing daemon-to-shim path.
// It is intentionally separate from PromptRouter: a health probe must never
// lazily create or revive a shim just because a frontend asked whether one is
// ready.
type SessionHealthRouter interface {
	Health(ctx context.Context, workspace, sessionID, requestID string) (*corev1.HealthStatus, error)
}

// DaemonHealthChecker reports whether every daemon-global boot dependency is
// ready.  The HTTP /healthz route and frontend command use the same checker,
// so startup cannot have two competing definitions of operational.
type DaemonHealthChecker interface {
	DaemonHealth() (healthy bool, reason string)
}

// MergeRunner runs (or resumes) a workspace merge. It owns the workspace ->
// (source dir, source branch, target dir) resolution the merge.Engine needs,
// so the handler stays free of that policy.
type MergeRunner interface {
	// Merge starts a cherry-pick merge for the workspace.
	Merge(ctx context.Context, workspace string) error
	// Resume continues a human-resolved conflict (the
	// conflict_resolved_continue handoff, §9.3).
	Resume(ctx context.Context, workspace string) error
}

// WorkspaceLifecycle closes/opens a workspace. Bound to the Emacs
// workspace-command channel (workspacecmd) at stitch.
type WorkspaceLifecycle interface {
	Close(ctx context.Context, workspace string) error
	Open(ctx context.Context, workspace string) error
}

// WorkspaceCreationBridge is the server-side seam for daemon-owned workspace
// creation.  It deliberately speaks only frontend.v1 messages: the concrete
// adapter may compose workspace/create.Manager, its durable store, and the
// inbox without making this transport/server package import any of them.
//
// A nil bridge is an explicit unsupported capability: the command handler
// returns a loud Nack rather than claiming a workspace or host action exists.
type WorkspaceCreationBridge interface {
	MarkWorkspaceMaterialized(ctx context.Context, jobID string) error
	CompleteHostAction(ctx context.Context, actionID string, ok bool, failure string) error
	SnapshotHostWork() WorkspaceHostWorkSnapshot
	SubscribeWorkspaceAvailable() (<-chan *frontendv1.WorkspaceAvailable, func())
	SubscribeHostActions() (<-chan *frontendv1.HostAction, func())
}

// WorkspaceHostWorkSnapshot is the durable host-only subset a reconnecting
// Emacs receives.  Its proto representation lets frontend.Server enforce its
// ClientKind boundary without knowing how the creation store persists jobs.
type WorkspaceHostWorkSnapshot struct {
	WorkspaceAvailable []*frontendv1.WorkspaceAvailable
	HostActions        []*frontendv1.HostAction
}

// Resyncer replays a workspace's retained conversation deltas from the
// requested seq INCLUSIVE (design §5.4), the conversation-delta half of a
// frontend resync the StateSnapshot re-send does not cover. The implementation
// raises that start to the newest clear or compaction when there is one, so a
// frontend is never served history one of those already discarded. Satisfied by
// *sessiondrv.Manager.
type Resyncer interface {
	Resync(workspace string, fromSeq uint64) error
}

// SessionCreateDeleter is the daemon-core session-lifecycle surface behind the
// createSession/deleteSession UDS commands (the same core POST /sessions and
// DELETE /sessions/{id} use). *Server satisfies it, but it is constructed AFTER
// WireAgentShim, so main injects the late-bound *SessionCommandBinding.
type SessionCreateDeleter interface {
	CreateSession(ctx context.Context, opts CreateOpts) (string, error)
	DeleteSession(sessionID string) error
}

// DaemonViewSource supplies the daemon-identity frame for the connect snapshot
// (boot id, protocol version, binary mtime, version). *Server satisfies it via
// the same late-bound binding.
type DaemonViewSource interface {
	DaemonView() *frontendv1.DaemonView
}

// SessionCommands is the combined daemon-core surface the frontend command
// handler and snapshot provider need. *SessionCommandBinding satisfies it via
// its late-bound *Server target.
type SessionCommands interface {
	SessionCreateDeleter
	DaemonViewSource
}

// SessionCommandBinding is the late-bound bridge from the frontend command
// handler and snapshot provider to the daemon core. The *Server that satisfies
// SessionCommands is constructed AFTER WireAgentShim (it needs the
// frontend.Server WireAgentShim builds), so main injects this holder and calls
// SetTarget once the Server exists — the same late-bind shape as PushForwarder.
type SessionCommandBinding struct {
	Logf   func(string, ...any)
	target atomic.Pointer[Server]
}

var _ SessionCommands = (*SessionCommandBinding)(nil)

// SetTarget binds the *Server the holder delegates to. Called once by main,
// after New, before any frontend client can connect.
func (b *SessionCommandBinding) SetTarget(s *Server) { b.target.Store(s) }

func (b *SessionCommandBinding) logMiss(what string) {
	if b.Logf != nil {
		b.Logf("server: session-command binding %s before SetTarget — daemon core not yet wired", what)
	}
}

// CreateSession delegates to the bound Server, or fails loudly when the binding
// has no target yet (a construction-order bug, never a normal runtime state).
func (b *SessionCommandBinding) CreateSession(ctx context.Context, opts CreateOpts) (string, error) {
	s := b.target.Load()
	if s == nil {
		b.logMiss("CreateSession")
		return "", fmt.Errorf("server: session-create binding not wired")
	}
	return s.CreateSession(ctx, opts)
}

// DeleteSession delegates to the bound Server, failing loudly when unbound.
func (b *SessionCommandBinding) DeleteSession(sessionID string) error {
	s := b.target.Load()
	if s == nil {
		b.logMiss("DeleteSession")
		return fmt.Errorf("server: session-delete binding not wired")
	}
	return s.DeleteSession(sessionID)
}

// DaemonView delegates to the bound Server. An unbound binding logs the miss and
// returns nil (a snapshot with no daemon block); in production the binding is
// always set before any client connects.
func (b *SessionCommandBinding) DaemonView() *frontendv1.DaemonView {
	s := b.target.Load()
	if s == nil {
		b.logMiss("DaemonView")
		return nil
	}
	return s.DaemonView()
}

// commandHandler implements frontend.CommandHandler by routing each command to
// the owning module. Every dependency is required; a nil one is a construction
// error (surfaced by newCommandHandler) rather than a nil-deref at dispatch.
type commandHandler struct {
	prompts   PromptRouter
	merges    MergeRunner
	lifecycle WorkspaceLifecycle
	// resyncer replays conversation deltas on a resync; nil-safe (Resync then
	// documents the snapshot-only behavior rather than swallowing).
	resyncer Resyncer
	// sessions backs the createSession/deleteSession commands. Required.
	sessions SessionCreateDeleter
	// shutdown begins the daemon's graceful teardown (the same func POST
	// /shutdown drives). Nil makes the shutdown command a loud failing ack (the
	// capability is unconfigured), never a silent no-op.
	shutdown func()
	// queues backs the queue force/accept/cancel commands (E4). Nil makes each
	// of them a loud failing ack rather than a silent no-op, same as shutdown.
	queues QueueController
	// painters records frontend paint attestations onto the SSM's paint
	// axis. Nil makes a paint-ack a loud failing ack rather than a silent
	// no-op: an attestation the daemon quietly dropped would leave the
	// workspace blue with nothing to explain why.
	painters PaintAcker
	// workspaceCreation owns durable create jobs and the file-inbox actions.
	// Nil is a loud unsupported capability, never a success-shaped no-op.
	workspaceCreation WorkspaceCreationBridge
	health            SessionHealthRouter
	daemonHealth      DaemonHealthChecker
	// turns and liveTasks are the interrupt confirm gate's two facts (I1),
	// each read from the authority that already owns it rather than
	// re-derived here. Both required for the interrupt command; an unwired one
	// makes it a loud failing ack instead of an unchallenged stop.
	turns     TurnStateSource
	liveTasks LiveTaskSource
	logf      func(string, ...any)
	// establishMu guards establishing, the in-flight create-plus-establish
	// round per workspace cwd. This is the NARROWEST point the coalescing can
	// sit at: the create core is shared with POST /sessions, and the frontend
	// command is the only caller whose contract is "acked means established".
	// See createestablish.go.
	establishMu  sync.Mutex
	establishing map[string]*sessionEstablishment
	// establishTimeout bounds one create-plus-establish round. Zero means
	// createEstablishTimeout.
	establishTimeout time.Duration
}

// TurnStateSource reports whether a workspace's session has a turn IN FLIGHT,
// as observed off the shim's own TurnStarted/TurnEnded stream. Satisfied by
// *sessiondrv.Manager, which is where that observation already lives.
type TurnStateSource interface {
	TurnActive(workspace string) (bool, error)
}

// LiveTaskSource reports a workspace's live subagent-task count, and whether
// the workspace is known at all. Satisfied by *progress.Manager, which already
// adopts the count off the SSM's WorkspaceState and carries it to
// ProgressView — so the gate and the footer answer with the same number.
type LiveTaskSource interface {
	LiveTasks(workspace string) (int64, bool)
}

// CommandHandlerConfig collects the independently optional capabilities used
// by focused unit harnesses. Production WireAgentShim supplies every field.
type CommandHandlerConfig struct {
	WorkspaceCreation WorkspaceCreationBridge
	Health            HealthConfig
	Interrupt         InterruptGateConfig
	// EstablishTimeout bounds one createSession establishment round. Zero takes
	// createEstablishTimeout, which is the only value production uses; it is
	// injectable so a harness can prove the DEADLINE nack without waiting out a
	// bound sized for a loaded machine.
	EstablishTimeout time.Duration
}

// InterruptGateConfig supplies the interrupt confirm gate's two facts. Both
// are required for the interrupt command to run at all; a harness that omits
// them gets a loud failing ack rather than a stop that skipped the gate.
type InterruptGateConfig struct {
	Turns     TurnStateSource
	LiveTasks LiveTaskSource
}

type HealthConfig struct {
	Router SessionHealthRouter
	Daemon DaemonHealthChecker
}

// PaintAcker records a frontend's attestation that it painted a workspace's
// conversation through a seq. Satisfied by *ssm.Manager.
type PaintAcker interface {
	ApplyPaintAck(workspace string, throughSeq uint64) error
}

var _ frontend.CommandHandler = (*commandHandler)(nil)

func (h *commandHandler) WorkspaceMaterialized(ctx context.Context, _ string, requestID string, cmd *frontendv1.WorkspaceMaterializedCmd) error {
	h.logf("frontend cmd: workspace_materialized request_id=%s job_id=%s", requestID, cmd.GetJobId())
	if h.workspaceCreation == nil {
		return fmt.Errorf("server: workspace_materialized is unavailable: workspace creation manager is not wired")
	}
	return h.workspaceCreation.MarkWorkspaceMaterialized(ctx, cmd.GetJobId())
}

func (h *commandHandler) HostActionCompleted(ctx context.Context, _ string, requestID string, cmd *frontendv1.HostActionCompletedCmd) error {
	h.logf("frontend cmd: host_action_completed request_id=%s action_id=%s ok=%t error=%s", requestID, cmd.GetActionId(), cmd.GetOk(), cmd.GetError())
	if h.workspaceCreation == nil {
		return fmt.Errorf("server: host_action_completed is unavailable: workspace creation manager is not wired")
	}
	return h.workspaceCreation.CompleteHostAction(ctx, cmd.GetActionId(), cmd.GetOk(), cmd.GetError())
}

// newCommandHandler validates its dependencies and returns the handler. The
// resyncer is optional (nil-safe) and shutdown is optional (an unconfigured
// shutdown fails the command loudly); the three routers and the
// session-lifecycle binding are required.
func newCommandHandler(prompts PromptRouter, merges MergeRunner, lifecycle WorkspaceLifecycle, resyncer Resyncer, sessions SessionCreateDeleter, shutdown func(), queues QueueController, painters PaintAcker, logf func(string, ...any), configs ...CommandHandlerConfig) (*commandHandler, error) {
	switch {
	case prompts == nil:
		return nil, fmt.Errorf("server: frontend command handler needs a PromptRouter")
	case merges == nil:
		return nil, fmt.Errorf("server: frontend command handler needs a MergeRunner")
	case lifecycle == nil:
		return nil, fmt.Errorf("server: frontend command handler needs a WorkspaceLifecycle")
	case sessions == nil:
		return nil, fmt.Errorf("server: frontend command handler needs a SessionCreateDeleter")
	}
	if logf == nil {
		logf = func(string, ...any) {}
	}
	if len(configs) > 1 {
		return nil, fmt.Errorf("server: frontend command handler received %d configs; expected at most one", len(configs))
	}
	var config CommandHandlerConfig
	if len(configs) == 1 {
		config = configs[0]
	}
	return &commandHandler{
		prompts: prompts, merges: merges, lifecycle: lifecycle, resyncer: resyncer,
		sessions: sessions, shutdown: shutdown, queues: queues, painters: painters,
		workspaceCreation: config.WorkspaceCreation,
		health:            config.Health.Router, daemonHealth: config.Health.Daemon,
		turns: config.Interrupt.Turns, liveTasks: config.Interrupt.LiveTasks, logf: logf,
		establishTimeout: config.EstablishTimeout,
	}, nil
}

// DaemonHealth returns the daemon-global readiness assertion.  A false answer
// is a completed check, not a command failure: callers wait for this correlated
// view and retry only when the daemon reports what remains unavailable.
func (h *commandHandler) DaemonHealth(_ context.Context, _ string, requestID string, _ *frontendv1.DaemonHealthCmd) (*frontendv1.DaemonHealthView, error) {
	if requestID == "" {
		return nil, fmt.Errorf("frontend cmd: daemon_health requires a request_id")
	}
	if h.daemonHealth == nil {
		return nil, fmt.Errorf("frontend cmd: daemon_health request_id=%s: no daemon health checker wired", requestID)
	}
	healthy, reason := h.daemonHealth.DaemonHealth()
	h.logf("frontend cmd: daemon_health request_id=%s healthy=%v reason=%q", requestID, healthy, reason)
	return &frontendv1.DaemonHealthView{RequestId: requestID, Healthy: healthy, Reason: reason}, nil
}

// SessionHealth returns a correlated assertion for precisely the session
// carried by the command.  Any missing driver, mismatched session, transport
// failure, or unhealthy shim is encoded as healthy=false so Emacs has one
// honest result type to wait on during restore.
func (h *commandHandler) SessionHealth(ctx context.Context, workspace, requestID string, cmd *frontendv1.SessionHealthCmd) (*frontendv1.SessionHealthView, error) {
	view := &frontendv1.SessionHealthView{RequestId: requestID, Workspace: workspace, SessionId: cmd.GetSessionId()}
	if requestID == "" {
		return nil, fmt.Errorf("frontend cmd: session_health requires a request_id")
	}
	if err := checkWorkspaceKey("session_health", workspace); err != nil {
		view.Reason = err.Error()
		h.logf("frontend cmd: session_health request_id=%s healthy=false reason=%q", requestID, view.Reason)
		return view, nil
	}
	if view.GetSessionId() == "" {
		view.Reason = "frontend cmd: session_health requires a session_id"
		h.logf("frontend cmd: session_health ws=%s request_id=%s healthy=false reason=%q", workspace, requestID, view.Reason)
		return view, nil
	}
	if h.health == nil {
		view.Reason = "frontend cmd: session health router is not wired"
		h.logf("frontend cmd: session_health ws=%s session=%s request_id=%s healthy=false reason=%q", workspace, view.GetSessionId(), requestID, view.Reason)
		return view, nil
	}
	actual, err := h.health.Health(ctx, workspace, view.GetSessionId(), requestID)
	if err != nil {
		view.Reason = err.Error()
		h.logf("frontend cmd: session_health ws=%s session=%s request_id=%s healthy=false reason=%q", workspace, view.GetSessionId(), requestID, view.Reason)
		return view, nil
	}
	if actual.GetRequestId() != requestID {
		view.Reason = fmt.Sprintf("frontend cmd: session health response request_id mismatch got=%q want=%q", actual.GetRequestId(), requestID)
		h.logf("frontend cmd: session_health ws=%s session=%s request_id=%s healthy=false reason=%q", workspace, view.GetSessionId(), requestID, view.Reason)
		return view, nil
	}
	view.Healthy = actual.GetHealthy()
	view.Reason = actual.GetReason()
	h.logf("frontend cmd: session_health ws=%s session=%s request_id=%s healthy=%v component=%s reason=%q", workspace, view.GetSessionId(), requestID, view.GetHealthy(), actual.GetComponent(), view.GetReason())
	return view, nil
}

// checkWorkspaceKey rejects a prompt-plane workspace key that is not an
// absolute path.
//
// Every session-routed command is keyed by the session's CWD: SessionLocator
// matches records on CWD == workspace and sessiondrv maps live drivers under
// that same string. A frontend that sends a DISPLAY NAME instead ("doom" rather
// than "/Users/…/.config/doom") therefore matches nothing — and without this
// check the miss surfaces as `workspace "doom" has no live session to drive`,
// indistinguishable from a genuinely dead session. That ambiguity is what let
// the 2026-07-25 name-keyed regression read as a session-startup failure
// instead of the wire-contract violation it was. Naming the real defect costs
// one check.
func checkWorkspaceKey(command, workspace string) error {
	if !filepath.IsAbs(workspace) {
		return fmt.Errorf("server: %s workspace key %q is not an absolute path — session-routed commands must be keyed by the session cwd, not a display name", command, workspace)
	}
	return nil
}

func (h *commandHandler) SubmitPrompt(ctx context.Context, workspace, requestID string, cmd *frontendv1.SubmitPromptCmd) error {
	h.logf("frontend cmd: submit_prompt ws=%s request_id=%s", workspace, requestID)
	if err := checkWorkspaceKey("submit_prompt", workspace); err != nil {
		return err
	}
	return h.prompts.SubmitPrompt(ctx, workspace, requestID, cmd.GetText(), cmd.GetPermissionMode())
}

// Interrupt stops the workspace's turn, subject to THE CONFIRM GATE.
//
// Interrupting a LIVE TURN never asks: the user can see the turn running and
// asked for it to stop. What deserves a second keystroke is the other case —
// no turn in flight, but subagent tasks still working — because there the
// visible thing the user meant to stop is already over and the thing they
// would actually stop is work they may not have in mind at all.
//
// The challenge is returned as a TYPED error rather than as a refusal: it is
// not a failure, and the ack it becomes carries neither `failure` nor `error`
// (see frontend.InterruptConfirmRequired). NOTHING is delivered to the shim on
// that path; the client asks the user and resends with confirm_agents.
//
// No turn and no live tasks DELIVERS anyway. The shim answers
// ALREADY_COMPLETE atomically and remains the sole authority on the verdict —
// the daemon's view of "no turn" is an observation, not a ruling, and refusing
// on it would let a stale observation swallow a stop.
func (h *commandHandler) Interrupt(ctx context.Context, workspace, requestID string, cmd *frontendv1.InterruptCmd) error {
	h.logf("frontend cmd: interrupt ws=%s request_id=%s confirm_agents=%v", workspace, requestID, cmd.GetConfirmAgents())
	if err := checkWorkspaceKey("interrupt", workspace); err != nil {
		return err
	}
	if !cmd.GetConfirmAgents() {
		challenge, err := h.interruptChallenge(workspace, requestID)
		if err != nil {
			return err
		}
		if challenge != nil {
			return challenge
		}
	}
	return h.prompts.Interrupt(ctx, workspace)
}

// interruptChallenge decides whether an unconfirmed interrupt must be
// challenged, returning the challenge or nil to deliver.
//
// Both facts come from the daemon's EXISTING authorities and neither is
// re-derived here: the turn boundary from the driver that observes it off the
// shim's own stream, and the live-task count from the resolver that already
// carries it to ProgressView. An unwired source is a construction bug and
// fails loudly rather than silently skipping the gate — a skipped gate would
// stop working subagents with no question asked, which is precisely what it
// exists to prevent.
func (h *commandHandler) interruptChallenge(workspace, requestID string) (error, error) {
	if h.turns == nil || h.liveTasks == nil {
		return nil, fmt.Errorf("server: interrupt confirm gate is not wired (turn source=%t live-task source=%t); refusing to stop workspace %q without the check the contract requires",
			h.turns != nil, h.liveTasks != nil, workspace)
	}
	active, err := h.turns.TurnActive(workspace)
	if err != nil {
		return nil, err
	}
	if active {
		return nil, nil
	}
	tasks, known := h.liveTasks.LiveTasks(workspace)
	if !known {
		// Nothing has ever reported this workspace's task count, so there is no
		// live work on record to ask about. Loud, because the alternative
		// reading — silently treating an unknown as zero — is how a gate stops
		// engaging without anyone noticing.
		h.logf("frontend cmd: interrupt ws=%s request_id=%s has no progress view; no live subagent work on record, delivering unchallenged", workspace, requestID)
		return nil, nil
	}
	if tasks <= 0 {
		return nil, nil
	}
	return &frontend.InterruptConfirmRequired{LiveTasks: tasks}, nil
}

func (h *commandHandler) AnswerPermission(ctx context.Context, workspace, requestID string, cmd *frontendv1.PermissionAnswerCmd) error {
	h.logf("frontend cmd: answer_permission ws=%s request_id=%s permission_request_id=%s allow=%v",
		workspace, requestID, cmd.GetPermissionRequestId(), cmd.GetAllow())
	if err := checkWorkspaceKey("answer_permission", workspace); err != nil {
		return err
	}
	return h.prompts.AnswerPermission(ctx, workspace, cmd.GetPermissionRequestId(), cmd.GetAllow(), cmd.GetDenyMessage(), cmd.GetUpdatedInput())
}

// MergeWorkspace runs a merge, or resumes one on the conflict_resolved_continue
// handoff (§9.3).
func (h *commandHandler) MergeWorkspace(ctx context.Context, workspace, requestID string, cmd *frontendv1.MergeWorkspaceCmd) error {
	if cmd.GetConflictResolvedContinue() {
		h.logf("frontend cmd: merge_workspace RESUME ws=%s request_id=%s", workspace, requestID)
		return h.merges.Resume(ctx, workspace)
	}
	h.logf("frontend cmd: merge_workspace ws=%s request_id=%s handler=%s", workspace, requestID, cmd.GetHandler())
	return h.merges.Merge(ctx, workspace)
}

func (h *commandHandler) CloseWorkspace(ctx context.Context, workspace, requestID string, _ *frontendv1.CloseWorkspaceCmd) error {
	h.logf("frontend cmd: close_workspace ws=%s request_id=%s", workspace, requestID)
	return h.lifecycle.Close(ctx, workspace)
}

func (h *commandHandler) OpenWorkspace(ctx context.Context, workspace, requestID string, _ *frontendv1.OpenWorkspaceCmd) error {
	h.logf("frontend cmd: open_workspace ws=%s request_id=%s", workspace, requestID)
	return h.lifecycle.Open(ctx, workspace)
}

// Resync drives the conversation-delta replay half of a frontend resync (the
// frontend server independently re-sends the StateSnapshot). It routes to the
// per-session driver's retained-ring replay from the requested seq, inclusive
// and floored at the newest clear or compaction.
// A nil resyncer (no driver wired) leaves this a documented no-op — the
// snapshot half is honest and sufficient for state — rather than a swallow.
// PaintAck records that a frontend PAINTED this workspace's conversation
// through the carried seq, which is the half of green's promise the daemon
// cannot observe for itself: it cannot distinguish a webview that drew the
// history from one that received it and drew nothing.
//
// Emacs never sends this — it renders no conversation, so it has nothing to
// attest to. The webapp sends it from the completion of a render that
// actually drew.
func (h *commandHandler) PaintAck(_ context.Context, workspace, requestID string, cmd *frontendv1.PaintAckCmd) error {
	if workspace == "" {
		return fmt.Errorf("frontend cmd: paint_ack request_id=%s carries no workspace", requestID)
	}
	if h.painters == nil {
		return fmt.Errorf("frontend cmd: paint_ack ws=%s request_id=%s: no paint recorder wired", workspace, requestID)
	}
	// An outcome is required. "I drew this" and "I cannot draw this" are
	// opposite claims about the same generation, and only the first may green a
	// workspace — so an ack that names neither is refused rather than read as
	// whichever one the zero value happens to be.
	if cmd.GetOutcome() == frontendv1.PaintOutcome_PAINT_OUTCOME_UNSPECIFIED {
		return fmt.Errorf("frontend cmd: paint_ack ws=%s request_id=%s names no paint outcome", workspace, requestID)
	}
	h.logf("frontend cmd: paint_ack ws=%s request_id=%s through_seq=%d generation=%d outcome=%s",
		workspace, requestID, cmd.GetThroughSeq(), cmd.GetStateGeneration(), cmd.GetOutcome())
	// A SUSPENDED ack settles the state's DELIVERY (the frontend server does
	// that on the read loop) but attests no paint: the webview holds the state
	// and cannot draw it, and greening a workspace on that would be exactly the
	// lie the attestation exists to prevent.
	if !frontend.AttestsPaint(cmd) {
		return nil
	}
	return h.painters.ApplyPaintAck(workspace, cmd.GetThroughSeq())
}

func (h *commandHandler) Resync(_ context.Context, workspace, requestID string, cmd *frontendv1.ResyncCmd) error {
	if h.resyncer == nil {
		h.logf("frontend cmd: resync ws=%s request_id=%s from_seq=%d (snapshot re-sent by server; no driver wired for conversation replay)",
			workspace, requestID, cmd.GetFromSeq())
		return nil
	}
	h.logf("frontend cmd: resync ws=%s request_id=%s from_seq=%d", workspace, requestID, cmd.GetFromSeq())
	return h.resyncer.Resync(workspace, cmd.GetFromSeq())
}

// CreateSession lives in createestablish.go: the command is the daemon's
// establishment gate, and its ack condition is the whole reason that file
// exists.

// DeleteSession marks the command's session terminal and stops its shim.
func (h *commandHandler) DeleteSession(_ context.Context, workspace, requestID string, cmd *frontendv1.DeleteSessionCmd) error {
	h.logf("frontend cmd: delete_session ws=%s request_id=%s session=%s", workspace, requestID, cmd.GetSessionId())
	return h.sessions.DeleteSession(cmd.GetSessionId())
}

// Shutdown begins the daemon's graceful teardown — the same func POST /shutdown
// drives — asynchronously, so the ok CommandAck is delivered before the process
// exits. An unconfigured shutdown is a loud failing ack (the capability is
// absent), never a silent no-op.
func (h *commandHandler) Shutdown(_ context.Context, workspace, requestID string, _ *frontendv1.ShutdownCmd) error {
	h.logf("frontend cmd: shutdown ws=%s request_id=%s", workspace, requestID)
	if h.shutdown == nil {
		return fmt.Errorf("server: shutdown not supported by this daemon")
	}
	go h.shutdown()
	return nil
}

// clientLogMessageCap bounds ONE mirrored client-log line's message text. The
// frontend already caps how MANY lines it forwards per window; this caps how
// big each one may be, so neither dimension of a pathological log loop can run
// away with the daemon's disk.
const clientLogMessageCap = 4000

// clientLogContextCap bounds the rendered structured context of one line.
const clientLogContextCap = 2000

// ClientLog mirrors a frontend-side diagnostic line into the daemon's own log.
//
// The line is EVIDENCE, never a control signal: this records it and touches no
// daemon state. It is tagged `frontend client-log` so a line that originated in
// a webview can never be misread as one the daemon produced itself — the whole
// point is to make an invisible webview's failures greppable next to the daemon
// events around them.
//
// A log with no message carries no evidence and is a caller bug, so it fails
// loudly rather than writing a blank line. An UNSPECIFIED level, by contrast,
// is recorded AS unspecified: the level is metadata, and discarding real
// evidence over missing metadata would defeat the purpose.
// QueueController is the queue half of the frontend command surface (E4).
// Satisfied by *sessiondrv.Manager. Every method reports an unknown entry id
// as an error: the user asked for something specific, and pretending to have
// done it would be worse than saying it is gone.
type QueueController interface {
	ForceQueueEntry(workspace, entryID string) error
	AcceptQueueEntry(workspace, entryID string) error
	CancelQueueEntry(workspace, entryID string) error
}

// ForceQueueEntry runs the interject sequence for a held prompt, user-initiated.
func (h *commandHandler) ForceQueueEntry(_ context.Context, workspace, requestID string, cmd *frontendv1.QueueForceCmd) error {
	h.logf("frontend cmd: queue_force ws=%s request_id=%s entry=%s", workspace, requestID, cmd.GetEntryId())
	if h.queues == nil {
		return fmt.Errorf("server: prompt queue not supported by this daemon")
	}
	return h.queues.ForceQueueEntry(workspace, cmd.GetEntryId())
}

// AcceptQueueEntry confirms a held prompt's classification (view state only).
func (h *commandHandler) AcceptQueueEntry(_ context.Context, workspace, requestID string, cmd *frontendv1.QueueAcceptCmd) error {
	h.logf("frontend cmd: queue_accept ws=%s request_id=%s entry=%s", workspace, requestID, cmd.GetEntryId())
	if h.queues == nil {
		return fmt.Errorf("server: prompt queue not supported by this daemon")
	}
	return h.queues.AcceptQueueEntry(workspace, cmd.GetEntryId())
}

// CancelQueueEntry drops a held prompt; it is never delivered.
func (h *commandHandler) CancelQueueEntry(_ context.Context, workspace, requestID string, cmd *frontendv1.QueueCancelCmd) error {
	h.logf("frontend cmd: queue_cancel ws=%s request_id=%s entry=%s", workspace, requestID, cmd.GetEntryId())
	if h.queues == nil {
		return fmt.Errorf("server: prompt queue not supported by this daemon")
	}
	return h.queues.CancelQueueEntry(workspace, cmd.GetEntryId())
}

func (h *commandHandler) ClientLog(_ context.Context, workspace, requestID string, cmd *frontendv1.ClientLogCmd) error {
	if cmd.GetMessage() == "" {
		return fmt.Errorf("server: client log carries no message (ws=%q request_id=%q)", workspace, requestID)
	}
	h.logf("frontend client-log: level=%s ws=%s request_id=%s message=%s%s",
		clientLogLevelName(cmd.GetLevel()), workspace, requestID,
		dlog.Clamp(cmd.GetMessage(), clientLogMessageCap),
		clientLogContextSuffix(cmd.GetContext()))
	return nil
}

// clientLogLevelName renders a level for the log. An unrecognized or unset
// level is rendered honestly rather than defaulted to info, so a frontend that
// forgets to set one is visible in the log instead of masquerading as info.
func clientLogLevelName(l frontendv1.ClientLogLevel) string {
	switch l {
	case frontendv1.ClientLogLevel_CLIENT_LOG_LEVEL_INFO:
		return "info"
	case frontendv1.ClientLogLevel_CLIENT_LOG_LEVEL_WARN:
		return "warn"
	case frontendv1.ClientLogLevel_CLIENT_LOG_LEVEL_ERROR:
		return "error"
	default:
		return fmt.Sprintf("unspecified(%d)", int32(l))
	}
}

// clientLogContextSuffix renders the optional structured context as compact
// JSON. An absent context contributes nothing; a context that cannot be
// rendered is reported inline rather than dropped, since the failure is itself
// diagnostic information about the reporting frontend.
func clientLogContextSuffix(ctx *structpb.Struct) string {
	if ctx == nil || len(ctx.GetFields()) == 0 {
		return ""
	}
	raw, err := protojson.Marshal(ctx)
	if err != nil {
		return fmt.Sprintf(" context=<unrenderable: %v>", err)
	}
	return " context=" + dlog.Clamp(string(raw), clientLogContextCap)
}

// ssmSnapshotProvider implements frontend.StateProvider from the SSM's
// resolved per-workspace state plus per-session metadata from the registry
// (model/slug/title where the daemon has them, design §14.2 point 1).
type ssmSnapshotProvider struct {
	ssm      *ssm.Manager
	sessions SessionMetaSource
	// inits supplies the retained SessionInitView of every live session (S9),
	// so a (re)connecting frontend sources its slash-command/tools/model menus
	// from the snapshot. Nil-safe: a nil source leaves snapshot.inits empty.
	inits SessionInitSource
	// catalogs supplies every live session's complete detached-task roster, so
	// reconnect restores or clears the webapp's roster before later deltas.
	catalogs TaskCatalogSource
	// queues supplies each live session's held-prompt queue (E4). Nil-safe: a
	// nil source leaves snapshot.queues empty.
	queues QueueSource
	// daemon supplies the DaemonView (boot id / protocol version / binary
	// mtime / version) carried on every connect snapshot. Nil-safe: a nil
	// source leaves snapshot.daemon unset rather than nil-derefing.
	daemon DaemonViewSource
	// progress supplies each workspace's resolved ProgressView (F1), so a
	// (re)connecting frontend's footer is populated before the next change
	// pushes. Nil-safe: a nil source leaves snapshot.progress empty.
	progress ProgressSource
	// workspaceCreation supplies retained daemon-owned work for the Emacs host.
	// frontend.Server removes these fields for all GUI client kinds; retaining
	// them here makes a reconnecting host drain the durable queue before relying
	// on best-effort live publications.
	workspaceCreation WorkspaceCreationBridge
	// logf records the complete snapshot shape at the reconnect boundary. It is
	// optional only for focused unit construction outside WireAgentShim.
	logf dlog.Logf
}

var _ frontend.StateProvider = (*ssmSnapshotProvider)(nil)

// SessionMetaSource supplies the SessionView metadata the SSM does not carry
// (model, slug, title). Bound to the session registry / live session map at
// stitch. Returning an empty slice is valid (no sessions yet).
type SessionMetaSource interface {
	SessionViews() []*frontendv1.SessionView
}

// SessionInitSource supplies the retained SystemInit of every live session as
// SessionInitViews (S9), for the connect snapshot's inits. Satisfied by
// *sessiondrv.Manager. Returning an empty slice is valid (no inits yet).
type SessionInitSource interface {
	SessionInits() []*frontendv1.SessionInitView
}

// TaskCatalogSource supplies the authoritative detached-task roster for every
// live session on connect/resync. Empty per-session catalogs are significant:
// they clear stale frontend roster state.
type TaskCatalogSource interface {
	TaskCatalogs() []*frontendv1.TaskCatalog
}

// QueueSource supplies every live session's held-prompt queue (E4) for the
// connect/resync snapshot. Satisfied by *sessiondrv.Manager. A session with an
// empty queue still contributes its empty view, so a reconnecting frontend is
// TOLD the queue is empty rather than left to assume it.
type QueueSource interface {
	QueueViews() []*frontendv1.QueueView
}

// ProgressSource supplies every workspace's resolved ProgressView (F1) for the
// connect/resync snapshot, so a frontend's progress footer starts populated
// rather than blank until the next change. Satisfied by *progress.Manager.
type ProgressSource interface {
	Snapshot() []*frontendv1.ProgressView
}

// QueueBackend is the daemon's whole prompt-queue surface: the command half
// and the snapshot half. Satisfied by *sessiondrv.Manager, which owns both.
type QueueBackend interface {
	QueueController
	QueueSource
}

// Snapshot assembles a StateSnapshot from the SSM's workspace states and the
// session metadata source. A failed SSM read yields the sessions-only snapshot
// with the failure loud-logged by the SSM; it never blocks the connect.
func (p *ssmSnapshotProvider) Snapshot() *frontendv1.StateSnapshot {
	snap := &frontendv1.StateSnapshot{}
	if p.ssm != nil {
		if states, err := p.ssm.Snapshot(); err == nil {
			snap.Workspaces = states
		}
	}
	if p.sessions != nil {
		snap.Sessions = p.sessions.SessionViews()
	}
	if p.inits != nil {
		snap.Inits = p.inits.SessionInits()
	}
	if p.catalogs != nil {
		snap.Catalogs = p.catalogs.TaskCatalogs()
	}
	if p.queues != nil {
		snap.Queues = p.queues.QueueViews()
	}
	if p.daemon != nil {
		snap.Daemon = p.daemon.DaemonView()
	}
	if p.progress != nil {
		snap.Progress = p.progress.Snapshot()
	}
	if p.workspaceCreation == nil {
		panic("server: snapshot provider requires workspace creation bridge")
	}
	hostWork := p.workspaceCreation.SnapshotHostWork()
	snap.WorkspaceAvailable = hostWork.WorkspaceAvailable
	snap.HostActions = hostWork.HostActions
	if p.logf != nil {
		taskCount := 0
		for _, catalog := range snap.GetCatalogs() {
			taskCount += len(catalog.GetTasks())
		}
		p.logf("frontend: connect snapshot workspaces=%d sessions=%d catalogs=%d tasks=%d inits=%d queues=%d progress=%d workspace_available=%d host_actions=%d daemon=%t",
			len(snap.GetWorkspaces()), len(snap.GetSessions()), len(snap.GetCatalogs()), taskCount,
			len(snap.GetInits()), len(snap.GetQueues()), len(snap.GetProgress()), len(snap.GetWorkspaceAvailable()), len(snap.GetHostActions()), snap.GetDaemon() != nil)
	}
	return snap
}
