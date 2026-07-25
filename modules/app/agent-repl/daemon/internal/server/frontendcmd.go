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
	"sync/atomic"

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
	SubmitPrompt(ctx context.Context, workspace, text, permissionMode string) error
	Interrupt(ctx context.Context, workspace string, hard bool) error
	AnswerPermission(ctx context.Context, workspace, permissionRequestID string, allow bool, denyMessage string, updatedInput *structpb.Struct) error
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

// Resyncer replays a workspace's retained conversation deltas from an exclusive
// seq (design §5.4), the conversation-delta half of a frontend resync the
// StateSnapshot re-send does not cover. Satisfied by *sessiondrv.Manager.
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
	logf   func(string, ...any)
}

var _ frontend.CommandHandler = (*commandHandler)(nil)

// newCommandHandler validates its dependencies and returns the handler. The
// resyncer is optional (nil-safe) and shutdown is optional (an unconfigured
// shutdown fails the command loudly); the three routers and the
// session-lifecycle binding are required.
func newCommandHandler(prompts PromptRouter, merges MergeRunner, lifecycle WorkspaceLifecycle, resyncer Resyncer, sessions SessionCreateDeleter, shutdown func(), queues QueueController, logf func(string, ...any)) (*commandHandler, error) {
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
	return &commandHandler{prompts: prompts, merges: merges, lifecycle: lifecycle, resyncer: resyncer, sessions: sessions, shutdown: shutdown, queues: queues, logf: logf}, nil
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
	return h.prompts.SubmitPrompt(ctx, workspace, cmd.GetText(), cmd.GetPermissionMode())
}

func (h *commandHandler) Interrupt(ctx context.Context, workspace, requestID string, cmd *frontendv1.InterruptCmd) error {
	h.logf("frontend cmd: interrupt ws=%s request_id=%s hard=%v", workspace, requestID, cmd.GetHard())
	if err := checkWorkspaceKey("interrupt", workspace); err != nil {
		return err
	}
	return h.prompts.Interrupt(ctx, workspace, cmd.GetHard())
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
// per-session driver's retained-ring replay from the requested exclusive seq.
// A nil resyncer (no driver wired) leaves this a documented no-op — the
// snapshot half is honest and sufficient for state — rather than a swallow.
func (h *commandHandler) Resync(_ context.Context, workspace, requestID string, cmd *frontendv1.ResyncCmd) error {
	if h.resyncer == nil {
		h.logf("frontend cmd: resync ws=%s request_id=%s from_seq=%d (snapshot re-sent by server; no driver wired for conversation replay)",
			workspace, requestID, cmd.GetFromSeq())
		return nil
	}
	h.logf("frontend cmd: resync ws=%s request_id=%s from_seq=%d", workspace, requestID, cmd.GetFromSeq())
	return h.resyncer.Resync(workspace, cmd.GetFromSeq())
}

// CreateSession runs the shared create core for the command's cwd and reports
// its outcome via the CommandAck (the resulting session identity reaches the
// frontend as the SessionView the create core pushes). A typed create failure
// (invalid mode / resume-missing) or a bring-up error surfaces loudly.
func (h *commandHandler) CreateSession(ctx context.Context, workspace, requestID string, cmd *frontendv1.CreateSessionCmd) error {
	h.logf("frontend cmd: create_session ws=%s request_id=%s model=%s config_dir=%s resume=%q fake=%v",
		workspace, requestID, cmd.GetModel(), cmd.GetConfigDir(), cmd.GetResumeClaudeSessionId(), cmd.GetFake())
	id, err := h.sessions.CreateSession(ctx, CreateOpts{
		CWD:            cmd.GetCwd(),
		Model:          cmd.GetModel(),
		PermissionMode: cmd.GetPermissionMode(),
		ConfigDir:      cmd.GetConfigDir(),
		Resume:         cmd.GetResumeClaudeSessionId(),
		Fake:           cmd.GetFake(),
	})
	if err != nil {
		return err
	}
	h.logf("frontend cmd: create_session ws=%s request_id=%s -> session=%s", workspace, requestID, id)
	return nil
}

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
	// queues supplies each live session's held-prompt queue (E4). Nil-safe: a
	// nil source leaves snapshot.queues empty.
	queues QueueSource
	// daemon supplies the DaemonView (boot id / protocol version / binary
	// mtime / version) carried on every connect snapshot. Nil-safe: a nil
	// source leaves snapshot.daemon unset rather than nil-derefing.
	daemon DaemonViewSource
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

// QueueSource supplies every live session's held-prompt queue (E4) for the
// connect/resync snapshot. Satisfied by *sessiondrv.Manager. A session with an
// empty queue still contributes its empty view, so a reconnecting frontend is
// TOLD the queue is empty rather than left to assume it.
type QueueSource interface {
	QueueViews() []*frontendv1.QueueView
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
	if p.queues != nil {
		snap.Queues = p.queues.QueueViews()
	}
	if p.daemon != nil {
		snap.Daemon = p.daemon.DaemonView()
	}
	return snap
}
