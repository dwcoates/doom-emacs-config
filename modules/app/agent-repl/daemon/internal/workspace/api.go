// Package workspace is the daemon's workspace verbs.
//
// Each verb delegates to wsm, gitclient, shimclient, the prompt queue and the
// resolvers; the policy lives here and nowhere else. Every verb keys its
// WorkspaceRef on `id` and REFUSES a ref whose `dir` disagrees with the
// registry. See ARCHITECTURE.md "workspace".
package workspace

import (
	"context"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/internal/account"
	"claude-repld/internal/dlog"
	"claude-repld/internal/drain"
	"claude-repld/internal/feedid"
	"claude-repld/internal/gitclient"
	"claude-repld/internal/health"
	"claude-repld/internal/ids"
	"claude-repld/internal/merge"
	"claude-repld/internal/notimpl"
	"claude-repld/internal/promptqueue"
	"claude-repld/internal/resolve/feed"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/resolve/holds"
	"claude-repld/internal/resolve/sidebar"
	"claude-repld/internal/resolve/topbar"
	"claude-repld/internal/rollout"
	"claude-repld/internal/wsm"
)

// CreateSpec is a creation request, both forms.
type CreateSpec struct {
	// RepoDir is the repository the workspace is cut from.
	RepoDir string
	// InitialPrompt is the prompt the workspace starts with; the slug is
	// derived from it by the naming rule. Empty for a workspace created
	// without one.
	InitialPrompt string
	// OneShot marks the one-shot form: created, prompted, merged, closed.
	OneShot bool
	// BaseRef is the base to cut from, empty for the repository's default
	// branch.
	BaseRef string
	// ForkFrom, when set, is the parent workspace whose transcript the daemon
	// PORTS into the child's config root before StartSession(resume).
	ForkFrom *ids.WorkspaceID
	// ConsentedUngatedMode is the permission mode the user consented to at
	// creation. An ungated mode with no consent recorded is REFUSED.
	ConsentedUngatedMode string
	// MergeActions are the configured before/after merge actions.
	MergeActions wsm.MergeActions
}

// InterruptTarget names what an Interrupt aims at. Exactly one is set.
type InterruptTarget struct {
	// Turn interrupts the running turn: KillTurn{force: confirm}, with the
	// confirm_required challenge raised first when detached agents are live.
	Turn bool
	// Detached interrupts one detached bubble, addressed by its FeedId, which
	// decodes to either UpdateAgent.stop or StopBash.
	Detached *feedid.Ref
	// AllAgents interrupts fan-wide.
	AllAgents bool
}

// Verbs is the whole verb surface. Each method is one rpc's body.
type Verbs interface {
	// Register records a workspace Emacs announced. Idempotent by normalized
	// dir.
	Register(ctx context.Context, dir string, facts wsm.RegisterFacts) (wsm.Workspace, error)
	// Create materializes a new workspace: slug from the initial prompt by the
	// naming rule, branch, worktree, layout facts recorded, and REGISTRATION
	// ONLY AFTER MATERIALIZATION.
	Create(ctx context.Context, spec CreateSpec) (wsm.Workspace, error)
	// RequestCommandSupport composes the add-support brief from
	// prompts/add-support-slash-command.md — LOUD when the brief is absent —
	// and creates a support workspace through the ordinary standard form with
	// that brief as its initial prompt.
	RequestCommandSupport(ctx context.Context, ws ids.WorkspaceID, command string) (wsm.Workspace, error)
	// Open spawns a registered-but-closed workspace's session (spawn on mount
	// semantics).
	Open(ctx context.Context, ws ids.WorkspaceID) error
	// Close tears down a workspace's editor state. It REQUIRES QUIET: no turn
	// in flight, no live work, no held prompts, no queued merge. The refusal
	// manifests in the footer, not only in the answer.
	Close(ctx context.Context, ws ids.WorkspaceID) error
	// Kill is the big red button: forced session death, never blocks, data
	// survives.
	Kill(ctx context.Context, ws ids.WorkspaceID) error
	// Nuke destroys data: kill if live, then delete the worktree and the
	// branch, then forget the record. A nuked workspace LEAVES the roster.
	Nuke(ctx context.Context, ws ids.WorkspaceID) error
	// Restart bounces the workspace's shim by delegating to
	// rollout.RelaunchShim. force sends KillSession{force:true} first. It owns
	// the reload_webapp push when the webapp changed too.
	Restart(ctx context.Context, ws ids.WorkspaceID, force bool) error
	// Select records the user's switch to this workspace and clears its
	// attention marker. Idempotent.
	Select(ctx context.Context, ws ids.WorkspaceID) error
	// SetPriority sets or clears the roster priority.
	SetPriority(ctx context.Context, ws ids.WorkspaceID, p *wsm.Priority) error
	// CreateTask records a new task.
	CreateTask(ctx context.Context, title string) (wsm.Task, error)
	// UpdateTask retitles, completes or reopens a task.
	UpdateTask(ctx context.Context, id ids.TaskID, change wsm.TaskChange) error
	// AssignTask assigns a workspace to a task, or unassigns it.
	AssignTask(ctx context.Context, ws ids.WorkspaceID, task *ids.TaskID) error
	// AnswerColdGate resolves a standing cold gate: pay, clear, or
	// compact{model, scope}.
	AnswerColdGate(ctx context.Context, ws ids.WorkspaceID, answer *frontendv1.FeedColdGateResolved) error
	// SetModel switches the session's model through the QUEUE's session-act
	// path, so it cannot overtake a queued prompt.
	SetModel(ctx context.Context, ws ids.WorkspaceID, model string) error
	// SetPermissionMode switches the permission mode through the same path,
	// validating the mode against exactly what the topbar's picker served. An
	// ungated mode needs the consent recorded at creation.
	SetPermissionMode(ctx context.Context, ws ids.WorkspaceID, mode string) error
	// Interrupt stops what the target names.
	Interrupt(ctx context.Context, ws ids.WorkspaceID, target InterruptTarget, confirm bool) error
	// AnswerPermission delivers the permission card's verdict.
	AnswerPermission(ctx context.Context, ws ids.WorkspaceID, answer *conversationv1.AgentAnswer) error
	// AnswerQuestion delivers the question card's answer, echoing the served
	// values.
	AnswerQuestion(ctx context.Context, ws ids.WorkspaceID, answer *conversationv1.AgentAnswer) error
	// OpenExternal opens a clicked link in the pinned external browser.
	OpenExternal(ctx context.Context, ws ids.WorkspaceID, url string) error
	// OpenInEditor RELAYS a web link click onto the workspace's host stream
	// verbatim. The daemon validates the workspace and opens nothing itself;
	// there is no ack and no command loop.
	OpenInEditor(ctx context.Context, ws ids.WorkspaceID, path string, line *uint32) error
	// Resolve turns a client's echoed WorkspaceRef into a workspace, keying on
	// `id` and REFUSING a ref whose `dir` disagrees with the registry.
	Resolve(ctx context.Context, ref *workspacev1.WorkspaceRef) (wsm.Workspace, error)
}

// HostRelay is the slice of the server the verbs use to push onto a
// workspace's host stream. It is an interface so workspace does not import
// server (which sits above it).
type HostRelay interface {
	// OpenInEditor pushes the open_in_editor arm to the workspace's host
	// stream.
	OpenInEditor(ws ids.WorkspaceID, path string, line *uint32)
	// ReloadWebapp pushes the reload_webapp arm.
	ReloadWebapp(ws ids.WorkspaceID)
	// Notify pushes a host notification.
	Notify(ws ids.WorkspaceID, text string, kind string, toolName string)
}

// Deps are the verbs' collaborators.
type Deps struct {
	DB       wsm.DB
	Git      gitclient.Git
	Accounts account.Resolver
	Queue    promptqueue.Queue
	Merge    merge.Orchestrator
	Rollout  rollout.Controller
	Drain    drain.Controller
	Health   health.Reporter
	Feed     feed.Resolver
	Footer   footer.Resolver
	Topbar   topbar.Resolver
	Sidebar  sidebar.Resolver
	Holds    holds.Resolver
	Host     HostRelay
	// Sessions brings sessions up and down; injected so the verbs do not own
	// the shim fleet.
	Sessions Sessions
	// PromptsDir is where RequestCommandSupport reads its brief at use time.
	PromptsDir string
	Log        dlog.Surfaces
}

// Sessions is the slice of the session fleet the verbs drive.
type Sessions interface {
	// Start brings a workspace's session up, spawning and starting it.
	Start(ctx context.Context, ws ids.WorkspaceID) error
	// Stop ends a workspace's session, forced or graceful.
	Stop(ctx context.Context, ws ids.WorkspaceID, force bool) error
	// Live reports whether the workspace currently has a live session.
	Live(ws ids.WorkspaceID) bool
}

// New builds the verbs.
func New(deps Deps) (Verbs, error) {
	return nil, notimpl.Err
}
