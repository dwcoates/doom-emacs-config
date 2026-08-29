// Package workspace is the daemon's workspace verbs.
//
// Each verb delegates to wsm, gitclient, shimclient, the prompt queue and the
// resolvers; the policy lives here and nowhere else. Every verb keys its
// WorkspaceRef on `id` and REFUSES a ref whose `dir` disagrees with the
// registry. See ARCHITECTURE.md "workspace".
package workspace

import (
	"context"
	"fmt"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/internal/account"
	"claude-repld/internal/dlog"
	"claude-repld/internal/drain"
	"claude-repld/internal/externalbrowser"
	"claude-repld/internal/feedid"
	"claude-repld/internal/gitclient"
	"claude-repld/internal/health"
	"claude-repld/internal/ids"
	"claude-repld/internal/merge"
	"claude-repld/internal/promptqueue"
	"claude-repld/internal/prompts"
	"claude-repld/internal/resolve/feed"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/resolve/holds"
	"claude-repld/internal/resolve/sidebar"
	"claude-repld/internal/resolve/topbar"
	"claude-repld/internal/rollout"
	"claude-repld/internal/sessionwatcher"
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
	// Name is the user-supplied workspace name. Empty derives the slug from
	// the initial prompt by the naming rule.
	Name string
	// Model is the model the session starts under, empty for the vendor's
	// default.
	Model string
	// PermissionMode is the permission mode the session starts under, empty
	// for the vendor's default.
	PermissionMode string
	// Priority is the roster priority recorded at creation, nil when unset.
	Priority *wsm.Priority
	// Finish is the one-shot form's finish action. It is REQUIRED when OneShot
	// is set and refused otherwise.
	Finish *OneShotFinish
}

// OneShotFinish is how a one-shot workspace ends: exactly one arm is set. The
// finish is recorded in the creation job BEFORE materialization, because the
// turn that concludes with the success marker is what acts on it and that turn
// may outlive this daemon.
type OneShotFinish struct {
	// SelfMerge enqueues the workspace's own merge when the session's turn
	// concludes with the success marker.
	SelfMerge bool
	// OpenPr submits the create-pr follow-up as a post-prompt instead of
	// merging.
	OpenPr *OneShotOpenPr
}

// OneShotOpenPr is the open-pr finish's configuration, as the create-pr
// invocation spells it.
type OneShotOpenPr struct {
	// SelfCertified passes the self-certification flag to the pr command.
	SelfCertified bool
	// AddToMergeQueue asks the pr command to add the pr to the merge queue.
	AddToMergeQueue bool
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
	// compact{model, scope}. The scope travels beside the answer because
	// FeedColdGateResolvedCompact carries only the model, while the shim's
	// SessionColdCompact remediation requires both — and a scope is never
	// defaulted.
	AnswerColdGate(ctx context.Context, ws ids.WorkspaceID, answer *frontendv1.FeedColdGateResolved, scope conversationv1.SessionCompactScope) error
	// SetModel switches the session's model through the QUEUE's session-act
	// path, so it cannot overtake a queued prompt.
	SetModel(ctx context.Context, ws ids.WorkspaceID, model string) error
	// SetPermissionMode switches the permission mode through the same path,
	// validating the mode against exactly what the topbar's picker served. An
	// ungated mode needs the consent recorded at creation.
	SetPermissionMode(ctx context.Context, ws ids.WorkspaceID, mode string) error
	// Interrupt stops what the target names and ANSWERS with what it stopped:
	// the interrupted turn, the number of detached items stopped, or "nothing
	// was running", which is a success and not a failure.
	Interrupt(ctx context.Context, ws ids.WorkspaceID, target InterruptTarget, confirm bool) (InterruptOutcome, error)
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
	// Notify raises one host notification: it relays the TYPED notification
	// onto the workspace's host stream and sets the roster's attention marker,
	// which SelectWorkspace clears. It is the session watcher's LifecycleSink
	// notification hook, wired by the server.
	Notify(ctx context.Context, ws ids.WorkspaceID, note sessionwatcher.HostNotification) error
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
	// Browser opens a clicked link in the pinned external browser.
	Browser externalbrowser.Opener
	// PromptsDir is where RequestCommandSupport reads its brief at use time.
	PromptsDir string
	Log        dlog.Surfaces

	// Shim resolves the narrow slice of a workspace's shim client the verbs
	// drive directly: the interrupt verbs, the permission and question
	// answers, and the forced session kill. Injected so the verbs do not own
	// the shim fleet.
	Shim ShimFunc
	// Freeness answers what is running in a workspace, which is what the close
	// verb's quiet requirement and the interrupt verb's confirm challenge are
	// judged from.
	Freeness FreenessFunc
	// Ownership answers whether this daemon still serves a workspace, so a
	// verb refuses an unowned workspace mid-handover rather than acting on it.
	Ownership Ownership
	// Cards answers what the daemon SERVED for a permission ask, a question
	// batch or a cold gate. Every answer verb echoes against it: an answer
	// that does not match what was served is refused, never forwarded.
	Cards Cards
	// LoadPrompt reads one brief from the prompts directory at use time. It is
	// a function so the read is faked in tests; nil means prompts.Load.
	LoadPrompt PromptLoader
	// SplicePrompt substitutes a brief's placeholders. It is a function for the
	// same reason LoadPrompt is; nil means the brief's own Splice.
	SplicePrompt PromptSplicer
	// Now supplies the instants the verbs stamp. nil means time.Now.
	Now func() time.Time
	// EvictLogSink drops one workspace's durable log sink when the workspace
	// closes, releasing the shared descriptor. It is a function because
	// dlog.Surfaces does not expose eviction yet; nil leaves the sink open.
	EvictLogSink func(dir string) error
}

// PromptLoader reads one brief by name from a prompts directory at use time.
type PromptLoader func(dir, name string) (prompts.Prompt, error)

// PromptSplicer substitutes values into a loaded brief's placeholders. A value
// for an unknown placeholder, or a placeholder with no value, is an error: a
// brief is never sent with a hole in it.
type PromptSplicer func(prompt prompts.Prompt, values map[string]string) (string, error)

// ShimFunc resolves a workspace's live shim surface, reporting false when the
// workspace has no live session.
type ShimFunc func(ws ids.WorkspaceID) (Shim, bool)

// Shim is the slice of the shim client the verbs drive directly. Keeping it
// narrow is what lets every verb be tested against a fake instead of a whole
// shim process.
type Shim interface {
	// KillTurn interrupts the open turn, forced when the confirm challenge was
	// answered.
	KillTurn(ctx context.Context, turn ids.TurnID, force bool) error
	// StopAgent sends UpdateAgent.stop to one detached subagent.
	StopAgent(ctx context.Context, agent *conversationv1.AgentId) error
	// StopBash stops one detached shell.
	StopBash(ctx context.Context, work *conversationv1.DetachedWorkId) error
	// Answer delivers a permission verdict or a question answer to one agent
	// through UpdateAgent.answer.
	Answer(ctx context.Context, agent *conversationv1.AgentId, answer *conversationv1.AgentAnswer) error
	// KillSession ends the session, forced when the caller says so.
	KillSession(ctx context.Context, force bool) error
	// StartSession re-opens the session, which is what an answered cold gate
	// does: it resumes carrying the chosen remediation.
	StartSession(ctx context.Context, resume ColdResume) error
}

// ColdResume is the resume a cold-gate answer re-opens with: the conversation
// to resume and the remediation the user chose.
type ColdResume struct {
	// VendorSessionID is the conversation being resumed.
	VendorSessionID string
	// Remediation is the answered gate's choice.
	Remediation *conversationv1.SessionColdRemediation
}

// FreenessFunc answers what is running in one workspace. The bool is false
// when the workspace has no live session, in which case nothing is running.
type FreenessFunc func(ws ids.WorkspaceID) (Running, bool)

// Running is what a workspace currently has in flight, as the session watcher
// answers it.
type Running struct {
	// Turn is the open turn, nil when none is.
	Turn *ids.TurnID
	// LiveWork is the detached work still live.
	LiveWork sessionwatcher.LiveWorkSet
}

// Standing is whether this daemon still serves a workspace.
type Standing int

// The standings. Anything but StandingOwned refuses every per-workspace verb:
// acting on a workspace this daemon does not serve would race the daemon that
// does.
const (
	// StandingOwned is a workspace this daemon serves.
	StandingOwned Standing = iota
	// StandingTransferringAway is a workspace handed to a successor.
	StandingTransferringAway
	// StandingNotYetAdopted is a workspace a joining daemon has not adopted.
	StandingNotYetAdopted
)

// Ownership answers a workspace's serving standing. It is an interface so the
// rollout controller owns the truth and the verbs only consult it.
type Ownership interface {
	// Standing reports whether this daemon serves the workspace.
	Standing(ctx context.Context, ws ids.WorkspaceID) (Standing, error)
}

// Cards is what the daemon SERVED for the asks the answer verbs echo against.
// The feed resolver is its owner; the verbs only read it.
type Cards interface {
	// Permission answers the served permission ask, false when nothing with
	// that id is standing.
	Permission(ws ids.WorkspaceID, id *conversationv1.AgentPermissionId) (ServedPermission, bool)
	// Question answers the served question batch, false when nothing with that
	// id is standing.
	Question(ws ids.WorkspaceID, id *conversationv1.AgentQuestionId) (ServedQuestion, bool)
	// ColdGate answers the standing cold gate's served menu, false when no
	// gate stands.
	ColdGate(ws ids.WorkspaceID) (ServedColdGate, bool)
	// PermissionModes answers EXACTLY the switchable set the topbar's picker
	// served, false when the workspace has served no picker. SetPermissionMode
	// validates against it, because the daemon accepts only what it offered.
	PermissionModes(ws ids.WorkspaceID) ([]string, bool)
}

// ServedPermission is one served permission ask: who asked, and whether a
// standing grant was OFFERED. allow_standing without an offer is refused.
type ServedPermission struct {
	// Agent is the agent that asked, which is where the answer is delivered.
	Agent *conversationv1.AgentId
	// StandingFor is the standing grant the ask offered, nil when none was.
	StandingFor *conversationv1.AgentPermissionStanding
}

// ServedQuestion is one served question batch: who asked, and exactly what was
// offered, which is what an answer is validated against.
type ServedQuestion struct {
	// Agent is the agent that asked.
	Agent *conversationv1.AgentId
	// Batch is the batch exactly as served.
	Batch *conversationv1.AgentQuestionBatch
}

// ServedColdGate is the standing gate's served menu. A remediation naming a
// model or scope outside it is refused.
type ServedColdGate struct {
	// VendorSessionID is the conversation the gate parked.
	VendorSessionID string
	// Models are the models the compact menu served.
	Models []*conversationv1.AgentModel
	// Scopes are the compaction scopes the menu served.
	Scopes []conversationv1.SessionCompactScope
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

// New builds the verbs. Every collaborator a verb reaches is required: a verb
// that silently skipped a missing collaborator would answer success for work
// it never did.
func New(deps Deps) (Verbs, error) {
	missing := func(what string) error { return fmt.Errorf("workspace: %s is required", what) }
	switch {
	case deps.DB == nil:
		return nil, missing("a state client")
	case deps.Git == nil:
		return nil, missing("a git client")
	case deps.Accounts == nil:
		return nil, missing("an account resolver")
	case deps.Queue == nil:
		return nil, missing("a prompt queue")
	case deps.Merge == nil:
		return nil, missing("a merge orchestrator")
	case deps.Rollout == nil:
		return nil, missing("a rollout controller")
	case deps.Feed == nil:
		return nil, missing("a feed resolver")
	case deps.Footer == nil:
		return nil, missing("a footer resolver")
	case deps.Topbar == nil:
		return nil, missing("a topbar resolver")
	case deps.Sidebar == nil:
		return nil, missing("a sidebar resolver")
	case deps.Holds == nil:
		return nil, missing("a holds resolver")
	case deps.Host == nil:
		return nil, missing("a host relay")
	case deps.Sessions == nil:
		return nil, missing("a session fleet")
	case deps.Shim == nil:
		return nil, missing("a shim resolver")
	case deps.Freeness == nil:
		return nil, missing("a freeness probe")
	case deps.Ownership == nil:
		return nil, missing("an ownership probe")
	case deps.Cards == nil:
		return nil, missing("a served-card store")
	case deps.PromptsDir == "":
		return nil, missing("a prompts directory")
	case deps.Log == nil:
		return nil, missing("log surfaces")
	}
	load := deps.LoadPrompt
	if load == nil {
		load = prompts.Load
	}
	splice := deps.SplicePrompt
	if splice == nil {
		splice = func(prompt prompts.Prompt, values map[string]string) (string, error) {
			return prompt.Splice(values)
		}
	}
	now := deps.Now
	if now == nil {
		now = time.Now
	}
	return &verbs{deps: deps, load: load, splice: splice, now: now}, nil
}
