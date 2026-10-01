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

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	shimv1 "agentrepl/proto/shim/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/internal/account"
	"claude-repld/internal/dlog"
	"claude-repld/internal/drain"
	"claude-repld/internal/externalbrowser"
	"claude-repld/internal/feedid"
	"claude-repld/internal/gitclient"
	"claude-repld/internal/headless"
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
	// Parent, when set, is the workspace this create was SPAWNED FROM: the
	// child's merge target is the parent's worktree, and the roster nests the
	// child under it. It is recorded on the workspace at creation, so the
	// nesting is a stated fact rather than one derived from the branch
	// lineage.
	Parent *ids.WorkspaceID
	// ForkFrom, when set, is the parent workspace whose transcript the daemon
	// PORTS into the child's config root before StartSession(resume). A fork
	// is always FROM the spawning parent, so a spec that sets it and leaves
	// Parent unset names the same workspace for both.
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
	// Progress receives the create's stage transitions as they happen, for a
	// caller relaying them to a client. Nil for the legacy synchronous create,
	// which emits nothing; the verb never assumes it is set.
	Progress CreateProgress
}

// CreateStage is one stage a Create passes through between acceptance and its
// terminal outcome. It is the verb's own vocabulary, proto-free: the caller
// maps it onto whatever channel carries progress.
type CreateStage int

const (
	// CreateStageDerivingName: the daemon is minting the workspace's name with
	// a headless naming call. Reported only when the create supplied no name.
	CreateStageDerivingName CreateStage = iota
	// CreateStageCreatingWorktree: the daemon is materializing the git
	// worktree (`git worktree add`), the step a cancelled request context used
	// to kill mid-run.
	CreateStageCreatingWorktree
	// CreateStageStartingSession: the daemon is starting the workspace's
	// session — registering the workspace, copying a fork's transcript,
	// spawning the shim and bringing the vendor session up, then submitting
	// the initial prompt when there is one. Reported by EVERY create that gets
	// past the worktree, and ended by the create's terminal outcome.
	CreateStageStartingSession
)

// CreateProgress receives a Create's stage transitions in order. The terminal
// outcome is NOT reported here: it is the verb's own return value, which the
// caller maps. A create with no reporter leaves this nil.
type CreateProgress interface {
	Stage(CreateStage)
}

// OpenStage is one stage an Open passes through while its rpc is in flight.
// Like CreateStage it is the verb's own vocabulary, proto-free: the caller maps
// it onto whatever channel carries progress.
//
// THE TERMINAL OUTCOME IS NOT A STAGE. An open is answered synchronously by its
// own rpc, so success and every refusal already reach the caller there; what
// the answer cannot carry is the wait inside it, which is what these are.
type OpenStage int

const (
	// OpenStageCheckingWorktree: the daemon is confirming the workspace's
	// directory is still on disk. An open whose directory is gone is refused
	// at this stage.
	OpenStageCheckingWorktree OpenStage = iota
	// OpenStageStartingSession: the daemon is bringing the session up —
	// spawning the shim and resuming the vendor conversation. THE SLOW STAGE,
	// and the reason this vocabulary exists.
	OpenStageStartingSession
	// OpenStageReviving: the daemon is lifting a hibernation park. Reported
	// only for a workspace that was actually parked.
	OpenStageReviving
	// OpenStageClearingClosed: the daemon is clearing the closed flag, which
	// is what puts the row back among the open ones. Reported only for a
	// workspace that was actually closed.
	OpenStageClearingClosed
	// OpenStageCheckingBuild: the daemon is checking the shim against the
	// deployed build and bouncing it when stale.
	OpenStageCheckingBuild
)

// OpenProgress receives an Open's stage transitions in order. The terminal
// outcome is NOT reported here: it is the verb's own return value, which the
// caller maps. An open with no reporter leaves this nil.
type OpenProgress interface {
	Stage(OpenStage)
}

// BindStage is one stage a BindSession passes through while its rpc is in
// flight. Like OpenStage it is the verb's own vocabulary, proto-free: the
// caller maps it onto whatever channel carries progress.
//
// THE TERMINAL OUTCOME IS NOT A STAGE. A bind is answered synchronously by its
// own rpc, so success and every refusal already reach the caller there; what
// the answer cannot carry is the wait inside it, which is what these are.
type BindStage int

const (
	// BindStageReadingTranscripts: the daemon is asking the workspace's shim
	// for a fresh listing, which is what the choice is validated against.
	BindStageReadingTranscripts BindStage = iota
	// BindStageStoppingSession: the daemon is ending the current session. A
	// bind is a session swap, and this is its first half.
	BindStageStoppingSession
	// BindStageRecordingBinding: the daemon is writing the chosen conversation
	// onto the workspace's session record, which is what makes the choice
	// survive a restart.
	BindStageRecordingBinding
	// BindStageStartingSession: the daemon is bringing the session up on the
	// bound conversation through the ordinary resume. THE SLOW STAGE.
	BindStageStartingSession
)

// BindProgress receives a BindSession's stage transitions in order. The
// terminal outcome is NOT reported here: it is the verb's own return value,
// which the caller maps. A bind with no reporter leaves this nil.
type BindProgress interface {
	Stage(BindStage)
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
	// RegisterRepository records a repository resolved from ANY path inside it
	// (a file or a directory) through git's main worktree, AND registers that
	// main worktree as an open workspace through the same registration
	// RegisterWorkspace runs (owner ruling, 2026-09-14): a repository with no
	// workspace is not selectable, because `SPC p p' completes over live
	// workspaces. Idempotent on both halves; the answer's two bools report
	// which of them the registry already held, each an ANSWER rather than a
	// refusal.
	RegisterRepository(ctx context.Context, path string) (RegisteredRepository, error)
	// PublishRegistry publishes the roster's durable half once, from what the
	// registry holds right now. The boot spine calls it before anything is
	// served: the roster is otherwise published only as a side effect of a
	// verb, and a daemon nobody has asked anything of yet would leave the one
	// editor-global stream with nothing to deliver — including the EMPTY
	// roster, which is a roster.
	PublishRegistry(ctx context.Context) error
	// BindViews binds every registered workspace whose directory exists on
	// every resolver that logs per workspace, and writes NOTHING. The boot
	// calls it FIRST, before any reconciliation step can raise or close a
	// fault: those reach the footer, and a footer record for an unbound
	// workspace is an invariant violation. PublishRegistry binds again, later,
	// once it has closed the rows whose directory is gone.
	BindViews(ctx context.Context) error
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
	// semantics). progress receives the open's stages as it reaches them, for
	// a caller relaying them to a client; nil reports nothing.
	Open(ctx context.Context, ws ids.WorkspaceID, progress OpenProgress) error
	// Close tears down a workspace's editor state. It REQUIRES QUIET: no turn
	// in flight, no live work, no held prompts, no queued merge. The refusal
	// manifests in the footer, not only in the answer.
	Close(ctx context.Context, ws ids.WorkspaceID) error
	// Kill is the big red button: forced session death, never blocks, data
	// survives. It is BeginKill and its Teardown, run in one call.
	Kill(ctx context.Context, ws ids.WorkspaceID) error
	// BeginKill is a kill's fast half: every refusal, the queued merge
	// dropped, the workspace marked closed. Its Teardown kills the session, and
	// a caller that answered its client in between runs it detached.
	BeginKill(ctx context.Context, ws ids.WorkspaceID) (Teardown, error)
	// Nuke destroys data: kill if live, then delete the worktree and the
	// branch, then forget the record. A nuked workspace LEAVES the roster. It
	// is BeginNuke and its Teardown, run in one call.
	Nuke(ctx context.Context, ws ids.WorkspaceID) error
	// BeginNuke is a nuke's fast half, as BeginKill is a kill's.
	BeginNuke(ctx context.Context, ws ids.WorkspaceID) (Teardown, error)
	// Forget removes a CLOSED workspace's registry record, and its repository
	// record when no other workspace references that repository. It destroys
	// no files: the directory survives and re-registering it mints a fresh
	// record. It refuses an open workspace, a workspace that is not quiet, and
	// a workspace others were spawned from.
	Forget(ctx context.Context, ws ids.WorkspaceID) error
	// Restart bounces the workspace's shim by delegating to
	// rollout.RelaunchShim. force sends KillSession{force:true} first. It owns
	// the reload_webapp push when the webapp changed too.
	Restart(ctx context.Context, ws ids.WorkspaceID, force bool) error
	// Select records the user's switch to this workspace and clears its
	// attention marker. Idempotent.
	Select(ctx context.Context, ws ids.WorkspaceID) error
	// MarkViewed records that the user has SEEN this workspace: its roster row
	// goes PARTIAL until its status changes, if and only if the row is DONE
	// (the roster drops the report on any other status). Idempotent, and it
	// touches no durable record — the mode is a view fact, not a workspace
	// fact.
	MarkViewed(ctx context.Context, ws ids.WorkspaceID) error
	// SetPriority sets or clears the roster priority.
	SetPriority(ctx context.Context, ws ids.WorkspaceID, p *wsm.Priority) error
	// FoldRepository records whether a repository's roster section is
	// collapsed and republishes the roster. An unknown repository is refused
	// with ArmUnknownRepository.
	FoldRepository(ctx context.Context, repo ids.RepoID, folded bool) error
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
	// SelectAccount makes the workspace's session spend as the named account
	// root: the choice is recorded on the session row and the session is then
	// bounced through the restart verb's own engine, which is what carries the
	// vendor transcript into the new root. It answers whether that root holds
	// a login, so the caller can open its login flow; a logged-out root is
	// honored, never refused.
	SelectAccount(ctx context.Context, ws ids.WorkspaceID, configDir string) (bool, error)
	// SetPermissionMode switches the permission mode through the same path,
	// validating the mode against exactly what the topbar's picker served. An
	// ungated mode needs the consent recorded at creation.
	SetPermissionMode(ctx context.Context, ws ids.WorkspaceID, mode string) error
	// Interrupt stops what the target names and ANSWERS with what it stopped:
	// the interrupted turn, the number of detached items stopped, or "nothing
	// was running", which is a success and not a failure.
	Interrupt(ctx context.Context, ws ids.WorkspaceID, target InterruptTarget, confirm bool) (InterruptOutcome, error)
	// RollBack performs a confirmed rollback (rollback.go): the vendor
	// conversation, the held prompts and the feed return to just before a
	// prompt, as one operation the prompt queue owns.
	RollBack(ctx context.Context, ws ids.WorkspaceID, req RollbackRequest) (RollbackResult, error)
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
	// OpenDaemonFileInEditor relays an open of a file the DAEMON holds for the
	// workspace -- outside its worktree, a merge's test log -- onto the host
	// stream. The caller has already resolved the path from a token the
	// daemon served, which is what stands in for the worktree containment
	// check.
	OpenDaemonFileInEditor(ctx context.Context, ws ids.WorkspaceID, path string) error
	// Notify raises one host notification: it raises the workspace's desktop
	// banner (the daemon's own, decided on Emacs's focus) and sets the roster's
	// attention marker, which SelectWorkspace and AsksSettled clear. It is the
	// session watcher's LifecycleSink notification hook, wired by the server.
	Notify(ctx context.Context, ws ids.WorkspaceID, note sessionwatcher.HostNotification) error
	// AsksSettled clears the roster's attention marker when the last ask that
	// raised it settles. It is the session watcher's LifecycleSink
	// asks-settled hook, and Notify's counterpart: an answered ask is a SEEN
	// notification, whether or not the workspace was ever selected.
	AsksSettled(ctx context.Context, ws ids.WorkspaceID) error
	// ListTranscripts answers every vendor conversation filed under the
	// workspace's own directory, so a person can choose which one it runs.
	// The shim reads them; the daemon adds the one fact it alone holds —
	// which OTHER workspace already holds a conversation.
	ListTranscripts(ctx context.Context, ws ids.WorkspaceID) ([]*agentreplv1.WorkspaceTranscript, error)
	// BindSession points the workspace at a different conversation in its own
	// directory: validated against a fresh listing, refused while a turn is in
	// flight, then the session is stopped, the binding recorded, and a new
	// session started through the ORDINARY resume path so a cold conversation
	// parks at its cold gate. progress receives the bind's stages; nil reports
	// nothing.
	BindSession(ctx context.Context, ws ids.WorkspaceID, vendorSessionID string, progress BindProgress) error
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
	// PublishHostWorkspace recomposes and republishes the workspace's host
	// STATE. Every edge that can move it calls this: the edges the server
	// cannot see for itself -- a shim attaching or dying, a lease taken,
	// parked or released, a restart -- happen in the fleet and the merge
	// orchestrator. The topic dedupes, so a caller never has to decide
	// whether its edge actually changed the view.
	PublishHostWorkspace(ws ids.WorkspaceID)
}

// Banners raises a workspace's desktop banner (desktopnotify.Notifier.Raise):
// titled by the workspace's name, TEXT below it, posted only while Emacs is
// not focused.
type Banners interface {
	Raise(ws ids.WorkspaceID, kind, text string)
}

// Deps are the verbs' collaborators.
type Deps struct {
	// Instance is THIS DAEMON's identity. Registration claims serving
	// ownership of every workspace under it, which is what a handover hands
	// over: without a claim the outgoing daemon serves nothing as far as the
	// durable record is concerned, and transfers nothing.
	Instance ids.InstanceID
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
	// Banners raises the desktop banner an agent notification earns.
	Banners Banners
	// Sessions brings sessions up and down; injected so the verbs do not own
	// the shim fleet.
	Sessions Sessions
	// Browser opens a clicked link in the pinned external browser.
	Browser externalbrowser.Opener
	// Headless is the daemon's own one-shot vendor run. A create that supplies
	// no name asks it for one; nothing else in this package uses it. nil is a
	// build with no naming call at all, which REFUSES an unnamed create rather
	// than inventing a name.
	Headless headless.Runner
	// PromptsDir is where RequestCommandSupport reads its brief at use time.
	// It is also the daemon's own prompt CORPUS, which is the one-shot policy
	// of exactly one repository: the one the daemon's checkout lives in.
	PromptsDir string
	// CheckoutRoot is the agent-repl module checkout the daemon was deployed
	// from. It is what decides whether a repository's one-shot policy is the
	// corpus or the repository's own `.agent-repl/prompts`.
	CheckoutRoot string
	// Policy probes a repository's policy directory. nil means the real
	// filesystem (prompts.OnDisk).
	Policy prompts.Files
	Log    dlog.Surfaces

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
	// answered. commandedBy is HOW the person commanded the stop, relayed as
	// KillTurnRequest.commanded_by; nil states none.
	KillTurn(ctx context.Context, turn ids.TurnID, force bool, commandedBy *conversationv1.AgentInterruptedByUser) error
	// RollBackSession rewinds the vendor conversation to just before the first
	// of TURNS, answering the paths a files restore changed back; a refusal is
	// a *ShimRefusal naming its arm, the vendor's words as its detail.
	RollBackSession(ctx context.Context, turns []ids.TurnID, restoreFiles bool) ([]string, error)
	// StopAgent sends UpdateAgent.stop to one detached subagent.
	StopAgent(ctx context.Context, agent *conversationv1.AgentId) error
	// StopBash stops one detached shell.
	StopBash(ctx context.Context, work *conversationv1.DetachedWorkId) error
	// Answer delivers a permission verdict or a question answer to one agent
	// through UpdateAgent.answer.
	Answer(ctx context.Context, agent *conversationv1.AgentId, answer *conversationv1.AgentAnswer) error
	// KillSession ends the session, forced when the caller says so.
	KillSession(ctx context.Context, force bool) error
	// ReadTranscripts answers every vendor conversation filed under the
	// workspace's own directory, as the shim read them. The WHOLE response
	// travels, because its failure arms carry evidence no arm name holds.
	ReadTranscripts(ctx context.Context) (*shimv1.ReadTranscriptsResponse, error)
	// StandDown arms the shim client's stand-down latch for a teardown THIS
	// DAEMON is ordering, answering whether it was armed.
	//
	// Every verb here that ends a session asks the shim first and stops the
	// process unconditionally afterwards, so the escalation — the stop after a
	// kill that did not answer — is itself part of the ordered teardown. The
	// latch has to be armed BEFORE it, or the exit and the redial that follow
	// read a departure this daemon ordered as one that happened to it.
	StandDown() bool
}

// ColdResume is the resume a cold-gate answer re-opens with: the conversation
// to resume and the remediation the user chose. It is what Sessions.ResumeCold
// takes, because the re-open is a SESSION BRING-UP and not a bare shim call:
// the same watcher, facts and host view a cold start installs are owed here.
type ColdResume struct {
	// VendorSessionID is the conversation being resumed.
	VendorSessionID string
	// Remediation is the answered gate's choice.
	Remediation *conversationv1.SessionColdRemediation
	// OnPhase is called for every compaction phase the SHIM relays while the
	// remediated re-open runs, in order, on the caller's behalf.
	//
	// WHY A CALLBACK AND NOT A SINK. The compaction the gate's `compact`
	// remediation spends happens INSIDE StartSession, before any session
	// watcher exists to carry its frames, so the fleet opens a watch of its
	// own for the duration and hands the phases back to the verb that is
	// drawing them. The verb owns the surface; the fleet owns the stream.
	// Nil is legal and means nobody is watching.
	OnPhase func(*conversationv1.SessionCompactionProgress)
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
	// TakeColdGate spends the gate standing for the conversation
	// VENDORSESSIONID, answering whether this caller took it. The check and
	// the removal are one step, so of two answers racing for one gate exactly
	// one spends it; a gate left behind would re-open the session again on
	// every replayed click.
	TakeColdGate(ws ids.WorkspaceID, vendorSessionID string) bool
	// EndColdGate retires a TAKEN gate once its remediation brought the
	// session back, so a prompt is no longer refused by the gate's name.
	EndColdGate(ws ids.WorkspaceID, vendorSessionID string)
	// ReraiseColdGate stands the gate for VENDORSESSIONID again from the cold
	// facts it was first raised with, answering false when those are gone.
	ReraiseColdGate(ws ids.WorkspaceID, vendorSessionID string) bool
	// PermissionModes answers EXACTLY the switchable set the topbar's picker
	// served, false when the workspace has served no picker. SetPermissionMode
	// validates against it, because the daemon accepts only what it offered.
	PermissionModes(ws ids.WorkspaceID) ([]string, bool)
	// Models answers exactly the model catalog the topbar's selector served,
	// reporting false when none has been served. SetModel validates against
	// it: the daemon accepts only the tokens it offered.
	Models(ws ids.WorkspaceID) ([]string, bool)
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
	// Detail is the gate's own account of what was refused cold — the SAME
	// sentence the footer's cold-gate line carries, composed once at the raise
	// so the strip, the gate card and a prompt's `cold_gate` refusal cannot
	// give three accounts of one gate.
	Detail string
	// Compact is the compact menu the gate served, nil when the session's
	// account is not offered compaction; a compact answer to such a gate is
	// refused.
	Compact *ServedColdGateCompact
}

// ServedColdGateCompact is the compact menu a standing gate served.
type ServedColdGateCompact struct {
	// Models are the models the compact menu served.
	Models []*conversationv1.AgentModel
	// Scopes are the compaction scopes the menu served.
	Scopes []conversationv1.SessionCompactScope
}

// Sessions is the slice of the session fleet the verbs drive.
type Sessions interface {
	// Start brings a workspace's session up, spawning and starting it.
	Start(ctx context.Context, ws ids.WorkspaceID) error
	// StartRebound brings a workspace's session up after a
	// BindWorkspaceSession has pointed it at a DIFFERENT conversation, and
	// says so on the resume. It is the ONE start that does: the shim's book
	// for the workspace is the conversation's identity, and only a bind may
	// move it. BindSession is its only caller; everything else calls Start and
	// stays a plain resume.
	StartRebound(ctx context.Context, ws ids.WorkspaceID) error
	// Stop ends a workspace's session, forced or graceful.
	Stop(ctx context.Context, ws ids.WorkspaceID, force bool) error
	// StartDetached brings a workspace's session up OFF the caller's
	// goroutine, reporting the outcome to `done` when it settles. It is what
	// a caller uses when the start is occasioned by its answer rather than
	// contained in it; see Fleet.StartDetached for why the register uses it.
	StartDetached(ws ids.WorkspaceID, done func(error))
	// Live reports whether the workspace currently has a live session.
	Live(ws ids.WorkspaceID) bool
	// ResumeCold re-opens a session parked behind a standing cold gate,
	// carrying the chosen remediation, and completes the SAME bring-up a cold
	// start does: the session facts recorded, the session watcher installed,
	// and the host view republished as live. An answered gate that stopped at
	// the shim call left the workspace with no watcher at all, so the very
	// next prompt was refused `no_session`.
	ResumeCold(ctx context.Context, ws ids.WorkspaceID, resume ColdResume) error
	// ResumeColdDetached is ResumeCold OFF the caller's goroutine, reporting
	// the outcome to `done` with the context it ran under. It is what an
	// answered cold gate uses: the answer is acknowledged at once and the
	// remediation it starts runs after (see Fleet.ResumeColdDetached).
	ResumeColdDetached(ws ids.WorkspaceID, resume ColdResume, done func(context.Context, error))
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
	case deps.Health == nil:
		return nil, missing("a health reporter")
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
	case deps.Banners == nil:
		return nil, missing("a desktop notifier")
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
	case deps.CheckoutRoot == "":
		return nil, missing("a checkout root")
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
	if deps.Policy == nil {
		deps.Policy = prompts.OnDisk{}
	}
	return &verbs{deps: deps, load: load, splice: splice, now: now}, nil
}
