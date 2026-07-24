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

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/frontend"
	"claude-repld/internal/ssm"

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
	logf     func(string, ...any)
}

var _ frontend.CommandHandler = (*commandHandler)(nil)

// newCommandHandler validates its dependencies and returns the handler. The
// resyncer is optional (nil-safe); the three routers are required.
func newCommandHandler(prompts PromptRouter, merges MergeRunner, lifecycle WorkspaceLifecycle, resyncer Resyncer, logf func(string, ...any)) (*commandHandler, error) {
	switch {
	case prompts == nil:
		return nil, fmt.Errorf("server: frontend command handler needs a PromptRouter")
	case merges == nil:
		return nil, fmt.Errorf("server: frontend command handler needs a MergeRunner")
	case lifecycle == nil:
		return nil, fmt.Errorf("server: frontend command handler needs a WorkspaceLifecycle")
	}
	if logf == nil {
		logf = func(string, ...any) {}
	}
	return &commandHandler{prompts: prompts, merges: merges, lifecycle: lifecycle, resyncer: resyncer, logf: logf}, nil
}

func (h *commandHandler) SubmitPrompt(ctx context.Context, workspace, requestID string, cmd *frontendv1.SubmitPromptCmd) error {
	h.logf("frontend cmd: submit_prompt ws=%s request_id=%s", workspace, requestID)
	return h.prompts.SubmitPrompt(ctx, workspace, cmd.GetText(), cmd.GetPermissionMode())
}

func (h *commandHandler) Interrupt(ctx context.Context, workspace, requestID string, cmd *frontendv1.InterruptCmd) error {
	h.logf("frontend cmd: interrupt ws=%s request_id=%s hard=%v", workspace, requestID, cmd.GetHard())
	return h.prompts.Interrupt(ctx, workspace, cmd.GetHard())
}

func (h *commandHandler) AnswerPermission(ctx context.Context, workspace, requestID string, cmd *frontendv1.PermissionAnswerCmd) error {
	h.logf("frontend cmd: answer_permission ws=%s request_id=%s permission_request_id=%s allow=%v",
		workspace, requestID, cmd.GetPermissionRequestId(), cmd.GetAllow())
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

// ssmSnapshotProvider implements frontend.StateProvider from the SSM's
// resolved per-workspace state plus per-session metadata from the registry
// (model/slug/title where the daemon has them, design §14.2 point 1).
type ssmSnapshotProvider struct {
	ssm      *ssm.Manager
	sessions SessionMetaSource
}

var _ frontend.StateProvider = (*ssmSnapshotProvider)(nil)

// SessionMetaSource supplies the SessionView metadata the SSM does not carry
// (model, slug, title). Bound to the session registry / live session map at
// stitch. Returning an empty slice is valid (no sessions yet).
type SessionMetaSource interface {
	SessionViews() []*frontendv1.SessionView
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
	return snap
}
