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
	"path/filepath"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/account"
	"agent-repld/internal/claudesettings"
	"agent-repld/internal/creategate"
	"agent-repld/internal/dlog"
	"agent-repld/internal/drain"
	"agent-repld/internal/externalbrowser"
	"agent-repld/internal/feedid"
	"agent-repld/internal/gitclient"
	"agent-repld/internal/headless"
	"agent-repld/internal/health"
	"agent-repld/internal/ids"
	"agent-repld/internal/merge"
	"agent-repld/internal/promptqueue"
	"agent-repld/internal/prompts"
	"agent-repld/internal/resolve/feed"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/resolve/topbar"
	"agent-repld/internal/rollout"
	"agent-repld/internal/sessionwatcher"
	"agent-repld/internal/wsm"
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
	// FromOrigin cuts a primary workspace from origin's primary branch,
	// fetched as the create runs, rather than from the local primary branch
	// (agentrepl.v1.WorkspaceTopologyPrimary.from_origin). Never set with a
	// Parent: only a primary workspace is cut from the primary branch.
	FromOrigin bool
	// ContextWorkspace, set only on a one-shot, is the registered workspace
	// the commission is about: the one-shot's first message opens with the
	// one-shot context note naming it as the workspace the cited issue
	// corresponds to. It is no parent and no fork source; nothing of it is
	// carried into the one-shot but its identity.
	ContextWorkspace *ids.WorkspaceID
	// BaseRef is the base a MERGE WORKSPACE is cut from (its branch), set only
	// with MergeOf. Every other workspace is cut from its source's tip: the
	// parent's branch for a child, the repository's default branch otherwise
	// (agentrepl.v1.WorkspaceTopology), so no request names a base.
	BaseRef string
	// Parent, when set, is the workspace this create's branch is cut from and
	// lands into: its OWNER (agentrepl.v1.WorkspaceTopologyChild). The
	// child's merge target is the parent's worktree, and the roster nests the
	// child under it. Nil cuts from and lands on the primary branch.
	Parent *ids.WorkspaceID
	// ForkFrom, when set, is the workspace whose conversation is forked into
	// this one (agentrepl.v1.WorkspaceContextOrigin.fork). It is INDEPENDENT
	// of Parent: a child may fork its owner, another workspace, or nothing,
	// and a top-level workspace may fork any workspace. When it is not the
	// branch source (forkedElsewhere), the first message opens with the note
	// naming it.
	ForkFrom *ids.WorkspaceID
	// Requester is the workspace whose agent asked for the create, nil when
	// the user asked (agentrepl.v1.WorkspaceRequester). Recorded on the
	// workspace; it is never told about its own act.
	Requester *ids.WorkspaceID
	// OwnerNotice is the requester's notice to the owner of a child that does
	// not carry the owner's context (OwnerLands): set exactly then, delivered
	// with the daemon's addendum.
	OwnerNotice string
	// ConsentedUngatedMode is the permission mode the user consented to at
	// creation. An ungated mode with no consent recorded is REFUSED.
	ConsentedUngatedMode string
	// DeployHooks are the deploy hooks the creation supplies, run by every deploy
	// of the workspace in place of its repository's for the same stage.
	DeployHooks wsm.DeployHooks
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
	// A child with none inherits its parent's.
	Priority *wsm.Priority
	// Progress receives the create's stage transitions as they happen, for a
	// caller relaying them to a client. Nil for the legacy synchronous create,
	// which emits nothing; the verb never assumes it is set.
	Progress CreateProgress
	// MergeOf, when set, makes this create a MERGE WORKSPACE: a hidden fork of
	// MergeOf.Requester made to carry that workspace's async merge. The spec
	// must then fork from the requester (ForkFrom) and name nothing else the
	// daemon derives here: the name is MergeName, which the request reserved;
	// the merge target, the branch it lands on and the merge actions are the
	// ones the merge queue decided at hand-off (MergeOf.TargetDir,
	// .ParentBranch, .Actions), read off nothing else, so a requester with no
	// creation job (the main checkout) is forked from like any other and the
	// merge workspace runs only the actions its request named; and the roster
	// nests it under the requester once it is revealed.
	MergeOf *wsm.MergeOrigin
	// MergeName is a merge workspace's name as its requesting agent chose it,
	// unprefixed, set exactly when MergeOf is. It was checked and reserved when
	// the request was recorded, so it is taken as it stands.
	MergeName string
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
	// directory is still on disk. An open whose directory is gone goes on to
	// OpenStageRestoringWorktree when its branch survives, and is refused at
	// this stage when there is nothing to restore it from.
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
	// OpenStageRestoringWorktree: the workspace's directory was gone and its
	// branch still exists, so the daemon is checking the branch out at the
	// recorded directory again. Reported only by such an open, right after
	// OpenStageCheckingWorktree.
	OpenStageRestoringWorktree
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
	// RollBack, on a Turn stop, rolls the running turn's prompt back instead
	// of killing the turn, when no detached agent is live: the rollback's own
	// interrupt stops the turn and no interrupted ending is drawn. The caller
	// sets it only for a prompt nothing has answered yet. A refused rollback
	// stops the turn by the ordinary kill. nil: the turn is only stopped.
	RollBack *RollbackRequest
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
	// EnsureMainWorktrees registers, born closed, the main worktree of every
	// repository the registry holds without one, whose directory is on disk.
	// The boot calls it once its gone repositories are retired: a repository
	// is known exactly as long as its main worktree is a registered workspace.
	EnsureMainWorktrees(ctx context.Context) error
	// RestoreMissingWorktree recreates a workspace's deleted worktree from its
	// recorded branch, as an open of it does, for the boot's missing-directory
	// step. It answers true when it restored the tree, false with a nil error
	// when there is nothing to restore from, and an error when git could not
	// act (already recorded at ERROR).
	RestoreMissingWorktree(ctx context.Context, ws wsm.Workspace) (bool, error)
	// Create materializes a new workspace: slug from the initial prompt by the
	// naming rule, branch, worktree, layout facts recorded, and REGISTRATION
	// ONLY AFTER MATERIALIZATION.
	Create(ctx context.Context, spec CreateSpec) (wsm.Workspace, error)
	// CreateMergeWorkspace creates the hidden MERGE WORKSPACE an async merge
	// runs in: a fork of REQUESTER's conversation named NAME, in a worktree of
	// its own on BASE as it stands now, recorded as forked for ORIGIN's
	// request.
	CreateMergeWorkspace(ctx context.Context, requester wsm.Workspace, origin wsm.MergeOrigin, name, base string) (wsm.Workspace, error)
	// StartMergeWorkspace brings up merge workspace WS's session when none is
	// live, making the fork it still owes: what a hand-off that finds the
	// merge workspace a restart left behind needs before its pick can run.
	StartMergeWorkspace(ctx context.Context, ws ids.WorkspaceID) error
	// RevealMergeWorkspace draws a hidden merge workspace in the tab bar and
	// the sidebar from now on, answering whether this call revealed it.
	RevealMergeWorkspace(ctx context.Context, ws ids.WorkspaceID) (bool, error)
	// ClaimMergeName checks an async merge's name and claims it until the
	// request carrying it is recorded (merge.Names).
	ClaimMergeName(ctx context.Context, repoDir, name string) (branch string, release func(), taken bool, err error)
	// RequestCommandSupport composes the add-support brief from
	// prompts/add-support-slash-command.md — LOUD when the brief is absent —
	// and creates a support workspace through the ordinary standard form with
	// that brief as its initial prompt.
	RequestCommandSupport(ctx context.Context, ws ids.WorkspaceID, command string) (wsm.Workspace, error)
	// Open spawns a registered-but-closed workspace's session (spawn on mount
	// semantics). progress receives the open's stages as it reaches them, for
	// a caller relaying them to a client; nil reports nothing.
	Open(ctx context.Context, ws ids.WorkspaceID, progress OpenProgress) error
	// OpenToRevertMerge reopens a MERGED workspace as Open does and queues the
	// merge-revert brief into its session (OpenWorkspaceRequest.revert_merge).
	// A workspace whose merge has not landed is refused ArmNotMerged, and a
	// brief that cannot be composed ArmBriefMissing, both before anything is
	// opened.
	OpenToRevertMerge(ctx context.Context, ws ids.WorkspaceID, progress OpenProgress) error
	// Close tears down a workspace's editor state. It REQUIRES QUIET: no turn
	// in flight, no live work, no held prompts, no queued merge. The refusal
	// manifests in the footer, not only in the answer. BY is who asked for it,
	// which the close record keeps.
	Close(ctx context.Context, ws ids.WorkspaceID, by wsm.CloseCause) error
	// SetOneShot marks a workspace a one-shot, or unmarks it, as the asker
	// (nil for the user) asks, answering the completion directive a marked
	// workspace follows (standings.go).
	SetOneShot(ctx context.Context, ws ids.WorkspaceID, asker *ids.WorkspaceID, oneShot bool) (string, error)
	// SetCorrespondence marks a workspace awaiting another, or clears the mark
	// with nil, as the asker (nil for the user) asks (standings.go).
	SetCorrespondence(ctx context.Context, ws ids.WorkspaceID, asker *ids.WorkspaceID, awaited *ids.WorkspaceID) error
	// CloseSelf is the workspace's own agent closing it: the asking turn is
	// never a blocker, and a workspace whose turn still runs is marked to
	// close when that turn ends, answering true (close.go). A workspace with
	// no turn running closes now, answering false.
	CloseSelf(ctx context.Context, ws ids.WorkspaceID) (bool, error)
	// CarryOutSelfClose carries out the close a workspace asked of itself,
	// when one is marked (SelfCloses calls it at each turn's end).
	CarryOutSelfClose(ctx context.Context, ws ids.WorkspaceID) error
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
	// RetireLanded retires a requester whose detached merge landed: its
	// session stopped, its worktree removed, the workspace closed and stamped
	// merged. It answers why it kept the workspace instead (the RetireKept
	// reasons), empty when it retired it.
	RetireLanded(ctx context.Context, ws ids.WorkspaceID) (string, error)
	// Forget removes a CLOSED workspace's registry record, and its repository
	// record when no other workspace references that repository. It destroys
	// no files: the directory survives and re-registering it mints a fresh
	// record. It refuses an open workspace, a workspace that is not quiet, and
	// a workspace others were spawned from.
	Forget(ctx context.Context, ws ids.WorkspaceID) error
	// Restart bounces the workspace's shim IMMEDIATELY, always: it ends any
	// vendor-start retry run, force-ends the running turn with a bounded call,
	// asks the rollout for a FORCED bounce, and pushes the reload_webapp once
	// the bounce is done. A workspace with no session is restarted by the
	// bounce bringing one up.
	Restart(ctx context.Context, ws ids.WorkspaceID) error
	// Status answers the workspace's status as an agent reads it, open or
	// closed (agentrepl.v1.WorkspaceStatus). An unregistered workspace is
	// refused as unknown_workspace.
	Status(ctx context.Context, ws ids.WorkspaceID) (*agentreplv1.WorkspaceStatus, error)
	// Select records the user's switch to this workspace and clears its
	// attention marker. Idempotent. HISTORY says whether the switch stamps
	// the selection instant or, as a step of a recency walk, leaves it.
	Select(ctx context.Context, ws ids.WorkspaceID, history SelectionHistory) error
	// FollowStoredSelection brings the roster's selection in line with the one
	// WSM stores, when ANOTHER daemon sharing the state file stamped it (a
	// handover's predecessor and successor each serve a roster, and a select
	// lands on whichever owns the workspace). It pushes one roster when the
	// two disagree and nothing when they agree, under the same lock Select
	// takes, so it can never interleave with a select's stamp and push.
	FollowStoredSelection(ctx context.Context) error
	// MarkViewed records that the user has SEEN this workspace: its roster row
	// goes PARTIAL until its status changes, if and only if the row is DONE
	// (the roster drops the report on any other status). Idempotent, and it
	// touches no durable record — the mode is a view fact, not a workspace
	// fact.
	MarkViewed(ctx context.Context, ws ids.WorkspaceID) error
	// SetPriority sets or clears the roster priority badge.
	SetPriority(ctx context.Context, ws ids.WorkspaceID, p *wsm.Priority) error
	// SetPrincipalLink records the workspace's principal link, which the
	// topbar's summary opens, or clears it with nil.
	SetPrincipalLink(ctx context.Context, ws ids.WorkspaceID, link *string) error
	// SetHidden moves the workspace into the roster's hidden section, or back
	// out of it, and republishes the roster.
	SetHidden(ctx context.Context, ws ids.WorkspaceID, hidden bool) error
	// SetParent retargets the workspace's merge to parent's checkout (nil:
	// the repository's main checkout), records parent as its roster parent,
	// and republishes the roster. It tells the workspace's session NOTHING:
	// internal/retarget, its only caller, pairs it with the notice.
	SetParent(ctx context.Context, ws ids.WorkspaceID, parent *ids.WorkspaceID) (wsm.ParentChange, error)
	// RestoreParent undoes change unless a later retarget superseded it.
	RestoreParent(ctx context.Context, change wsm.ParentChange) error
	// Swap exchanges the places of workspaces ws and target in the registry
	// order, every other workspace keeping its own.
	Swap(ctx context.Context, ws, target ids.WorkspaceID) error
	// Move puts workspace ws onto target's tab in the arrangement's walk,
	// the tabs between shifting over; the two must be siblings there.
	Move(ctx context.Context, ws, target ids.WorkspaceID) error
	// FoldRepository records whether a repository's roster section is
	// collapsed and republishes the roster. An unknown repository is refused
	// with ArmUnknownRepository.
	FoldRepository(ctx context.Context, repo ids.RepoID, folded bool) error
	// FoldSubsection records whether a subsection is collapsed, republishes
	// the roster, and on a collapse stands its open workspaces down.
	FoldSubsection(ctx context.Context, sub ids.SubsectionID, folded bool) error
	// FoldMergedSection records whether the recently-merged band is collapsed
	// and republishes the roster.
	FoldMergedSection(ctx context.Context, folded bool) error
	// FoldHiddenSection records whether the hidden-workspaces section is
	// collapsed and republishes the roster.
	FoldHiddenSection(ctx context.Context, folded bool) error
	// FoldOpenSection records whether the open-workspaces section is
	// collapsed and republishes the roster.
	FoldOpenSection(ctx context.Context, folded bool) error
	// CreateSubsection adds a named subsection at the bottom of a
	// repository's section and republishes the roster (arrange.go).
	CreateSubsection(ctx context.Context, repo ids.RepoID, name string) (wsm.Subsection, error)
	// RenameSubsection relabels one subsection and republishes the roster.
	RenameSubsection(ctx context.Context, id ids.SubsectionID, name string) error
	// DeleteSubsection removes one subsection, its workspaces returning to
	// none, and republishes the roster.
	DeleteSubsection(ctx context.Context, id ids.SubsectionID) error
	// MoveSubsection moves one subsection among its repository's entries
	// (before a subsection, before a root in no subsection, or last) and
	// republishes the roster.
	MoveSubsection(ctx context.Context, id ids.SubsectionID, to wsm.Anchor) error
	// MoveRepository moves one repository before another (nil: last) and
	// republishes the roster.
	MoveRepository(ctx context.Context, id ids.RepoID, before *ids.RepoID) error
	// PlaceWorkspaces puts root workspaces, with their families, into a
	// subsection of their repository (nil: none) as one block at an anchor
	// (before a root there, before a subsection when into none, or last), and
	// republishes the roster.
	PlaceWorkspaces(ctx context.Context, ws []ids.WorkspaceID, into *ids.SubsectionID, at wsm.Anchor) error
	// SidebarArrangement reads the repository grouping's arrangement, for an
	// agent organizing it (GetSidebarArrangement).
	SidebarArrangement(ctx context.Context) ([]sidebar.ArrangedRepository, error)
	// CreateTask records a new task.
	CreateTask(ctx context.Context, title string) (wsm.Task, error)
	// UpdateTask retitles, completes or reopens a task.
	UpdateTask(ctx context.Context, id ids.TaskID, change wsm.TaskChange) error
	// AssignTask assigns a workspace to a task, or unassigns it.
	AssignTask(ctx context.Context, ws ids.WorkspaceID, task *ids.TaskID) error
	// AnswerColdGate resolves a standing cold gate with the request's choice:
	// pay, clear, compact{model, scope} or compact_and_resume{model, scope}.
	// The choice also decides the prompts held at the gate (gatefate.go in the
	// prompt queue), settled before the session is brought back.
	AnswerColdGate(ctx context.Context, ws ids.WorkspaceID, answer *agentreplv1.AnswerColdGateRequest) error
	// SetModel switches the session's model through the QUEUE's session-act
	// path, so it cannot overtake a queued prompt.
	SetModel(ctx context.Context, ws ids.WorkspaceID, model string) error
	// SetEffort switches the session's reasoning effort to a level the
	// topbar's selector served. It does NOT ride the queue's session-act path:
	// an effort change never puts a row in front of the reader, and the shim
	// itself waits for the running turn to end.
	SetEffort(ctx context.Context, ws ids.WorkspaceID, level conversationv1.AgentEffortLevel) error
	// RetryVendorError asks the workspace's shim for its scheduled
	// vendor-error retry now (RET in an empty composer) and answers the
	// shim's judgment whole.
	RetryVendorError(ctx context.Context, ws ids.WorkspaceID) (*shimv1.RetryVendorErrorNowResponse, error)
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
	// UpdateQuestionDraft applies one click to a standing question card's
	// daemon-owned draft and, when the click answered the batch's last
	// question, delivers the batch (askdraft.go).
	UpdateQuestionDraft(ctx context.Context, ws ids.WorkspaceID, ask string, edit feed.QuestionEdit) error
	// AnswerStandingAsk takes a user's typed send as the answer to the oldest
	// ask standing on the addressed feed (target nil = the root): the active
	// question of a batch takes it as its typed answer, a permission ask is
	// denied with it as the reason. It answers false, having done nothing,
	// when no ask stands there.
	AnswerStandingAsk(ctx context.Context, ws ids.WorkspaceID, target *feedid.Ref, text string) (bool, error)
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
	// OpenCommitLogInEditor relays a click on a landed merge's commit onto the
	// host stream verbatim: Emacs opens the branch's log in the repository
	// with point on the commit. The workspace is checked as every verb checks
	// it, and the repository dir must be absolute.
	OpenCommitLogInEditor(ctx context.Context, ws ids.WorkspaceID, target *agentreplv1.OpenInEditorCommitLog) error
	// OpenFeedLink resolves a non-web link clicked in a prompt or response
	// bubble (OpenInEditorFeedLink's order) and relays the file it names
	// exactly as OpenInEditor relays a workspace file. A link that names no
	// existing file is answered with the `link_unresolved` refusal; when
	// REPORT is set (OpenInEditorFeedLink.on_unresolved's `report` arm) it
	// also raises the transient `unknown_file` footer line and answers the
	// composed follow-up question, for the caller to submit. Unset is the
	// `web_fallback` arm: the refusal alone, silently.
	OpenFeedLink(ctx context.Context, ws ids.WorkspaceID, href string, report bool) (*UnresolvedLink, error)
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
	// OpenCommitLog pushes the open_commit_log arm to the workspace's host
	// stream.
	OpenCommitLog(ws ids.WorkspaceID, target *agentreplv1.OpenInEditorCommitLog)
	// ReloadWebapp pushes the reload_webapp arm.
	ReloadWebapp(ws ids.WorkspaceID)
	// PublishHostWorkspace recomposes and republishes the workspace's host
	// STATE. Every edge that can move it calls this: the edges the server
	// cannot see for itself -- a shim attaching or dying, a lease taken,
	// parked or released, a restart -- happen in the fleet and the merge
	// orchestrator. The topic dedupes, so a caller never has to decide
	// whether its edge actually changed the view.
	PublishHostWorkspace(ws ids.WorkspaceID)
	// ReturnFeedToTail ends the workspace's feed reply selection with
	// `return_to_tail`, because the user just switched to the workspace: a
	// switch lands on the newest row with the follow re-armed (owner ruling,
	// 2026-10-02), and a standing selection holds the follow off. Nothing
	// selected is nothing to end.
	ReturnFeedToTail(ws ids.WorkspaceID)
}

// Banners raises a workspace's desktop banner (desktopnotify.Notifier.Raise):
// titled by the workspace's name, TEXT below it, posted only while Emacs is
// not focused.
type Banners interface {
	Raise(ws ids.WorkspaceID, kind, text string)
}

// DerivedReporter tells the workspaces a new workspace was derived from -- its
// owner and its context source -- that it was created
// (internal/workspacenotice); its landing and its close reach them as
// lifecycle edges (internal/wsedges). It never blocks.
type DerivedReporter interface {
	ReportCreated(ws wsm.Workspace, ownerNotice string)
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
	// TurnStops is the queue's part in a turn stop (the interrupt verb): it
	// withdraws the accepted turn a stop lands on while the session is still
	// coming up, and orders a kill behind its turn's start.
	TurnStops promptqueue.TurnStops
	Merge     merge.Orchestrator
	Rollout   rollout.Controller
	Drain     drain.Controller
	Health    health.Reporter
	// Derived tells a created, landed or closed workspace's sources about it.
	Derived DerivedReporter
	Feed    feed.Resolver
	Footer  footer.Resolver
	Topbar  topbar.Resolver
	Sidebar sidebar.Resolver
	Host    HostRelay
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
	// HomeDir is the user's home directory, absolute: a feed link's leading
	// `~` expands to it (resolveFeedLink).
	HomeDir string
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
	// StandDowns stands down the session of a workspace whose repository
	// section the user collapsed (drain.Controller.StandDownWithheld).
	StandDowns StandDowns
	// CacheLapses holds a hidden workspace's keep-alives and stands its
	// session down once its cache lapses (cachelapse.Keeper).
	CacheLapses CacheLapses
	// Summaries installs a workspace's persisted summary into the topbar when
	// the workspace is registered (titlesynth.Synthesizer.Restore).
	Summaries Summaries
	// Cards answers what the daemon SERVED for a permission ask, a question
	// batch or a cold gate. Every answer verb echoes against it: an answer
	// that does not match what was served is refused, never forwarded.
	Cards Cards
	// ReadEffortSettings reads a config root's persisted effort level. It is a
	// function so the read is faked in tests; nil means
	// claudesettings.ReadEffort.
	ReadEffortSettings func(configDir string) (claudesettings.Effort, error)
	// LoadPrompt reads one brief from the prompts directory at use time. It is
	// a function so the read is faked in tests; nil means prompts.Load.
	LoadPrompt PromptLoader
	// SplicePrompt substitutes a brief's placeholders. It is a function for the
	// same reason LoadPrompt is; nil means the brief's own Splice.
	SplicePrompt PromptSplicer
	// Now supplies the instants the verbs stamp. nil means time.Now.
	Now func() time.Time
	// RestartStopBound bounds the restart's forced end of the running turn.
	// Zero means DefaultRestartStopBound, where the sizing is stated; it is a
	// field so the overrun is exercised without waiting the real bound out.
	RestartStopBound time.Duration
	// Creates is the create gate every create holds a ticket from for its
	// whole run (creategate). Required.
	Creates *creategate.Gate
	// WorktreeBound bounds a create's worktree step. Zero means
	// DefaultWorktreeBound, where the sizing is stated; it is a field so the
	// overrun is exercised without waiting the real bound out.
	WorktreeBound time.Duration
}

// CacheLapses is the hidden workspaces' cache keeper (cachelapse.Keeper),
// told of every hide and show once it is recorded.
type CacheLapses interface {
	VisibilityChanged(ws ids.WorkspaceID)
}

// Summaries restores a workspace's persisted summary (titlesynth.Synthesizer):
// registration is the one edge every workspace passes before any session
// trigger, so a restarted daemon draws the summary at once.
type Summaries interface {
	Restore(ctx context.Context, ws ids.WorkspaceID) error
}

// StandDowns stands a withheld workspace's session down as the idle sweep
// does, never under a turn. drain.Controller is the production one.
type StandDowns interface {
	StandDownWithheld(ws ids.WorkspaceID)
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
	// EditQuestionDraft applies one edit to a standing question batch's
	// draft, where the feed resolver holds it (feed.Resolver.EditQuestionDraft).
	EditQuestionDraft(ws ids.WorkspaceID, ask string, edit feed.QuestionEdit) (feed.QuestionDraftOutcome, error)
	// ReleaseQuestionDelivery frees a claimed batch whose delivery failed.
	ReleaseQuestionDelivery(ws ids.WorkspaceID, ask string)
	// StandingAsk answers the oldest ask standing on the addressed feed.
	StandingAsk(ws ids.WorkspaceID, target *feedid.Ref) (feed.StandingAsk, bool)
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
	// EffortLevels answers exactly the levels the topbar's effort selector
	// served for the selected model, false when it served none. SetEffort
	// validates against it.
	EffortLevels(ws ids.WorkspaceID) ([]conversationv1.AgentEffortLevel, bool)
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
	// Live reports whether the workspace has nothing for a start to do: a
	// session runs on its shim, or a cold gate parks it.
	Live(ws ids.WorkspaceID) bool
	// Held reports whether a shim is held for the workspace, a session on it
	// or not: what a teardown has to stand down.
	Held(ws ids.WorkspaceID) bool
	// SetEffort asks the workspace's shim for an effort level and, once it
	// confirms, states the level in effect to the topbar.
	SetEffort(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, level conversationv1.AgentEffortLevel) error
	// RetryVendorError asks the workspace's shim for its scheduled
	// vendor-error retry now.
	RetryVendorError(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID) (*shimv1.RetryVendorErrorNowResponse, error)
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
	// CancelVendorStart ends the vendor-start retry run in flight for the
	// workspace, if one is, and returns once the start it ran in has left. It
	// reports whether a run was ended. A restart calls it first so its own
	// bring-up begins a fresh run with the whole window.
	CancelVendorStart(ctx context.Context, ws ids.WorkspaceID) bool
}

// New builds the verbs. Every collaborator a verb reaches is required: a verb
// that silently skipped a missing collaborator would answer success for work
// it never did.
func New(deps Deps) (Verbs, error) {
	missing := func(what string) error { return fmt.Errorf("workspace: %s is required", what) }
	switch {
	case deps.DB == nil:
		return nil, missing("a state client")
	case deps.Derived == nil:
		return nil, missing("a derived-workspace reporter")
	case deps.Git == nil:
		return nil, missing("a git client")
	case deps.Creates == nil:
		return nil, missing("the create gate")
	case deps.Accounts == nil:
		return nil, missing("an account resolver")
	case deps.Queue == nil:
		return nil, missing("a prompt queue")
	case deps.TurnStops == nil:
		return nil, missing("a turn-stop queue")
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
	case deps.StandDowns == nil:
		return nil, missing("a withheld workspace's stand-down")
	case deps.CacheLapses == nil:
		return nil, missing("the hidden workspaces' cache keeper")
	case deps.Summaries == nil:
		return nil, missing("a summary restorer")
	case deps.Cards == nil:
		return nil, missing("a served-card store")
	case deps.PromptsDir == "":
		return nil, missing("a prompts directory")
	case deps.CheckoutRoot == "":
		return nil, missing("a checkout root")
	case !filepath.IsAbs(deps.HomeDir):
		return nil, missing("an absolute home directory")
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
	if deps.ReadEffortSettings == nil {
		deps.ReadEffortSettings = claudesettings.ReadEffort
	}
	stopBound := deps.RestartStopBound
	if stopBound <= 0 {
		stopBound = DefaultRestartStopBound
	}
	worktreeBound := deps.WorktreeBound
	if worktreeBound <= 0 {
		worktreeBound = DefaultWorktreeBound
	}
	return &verbs{deps: deps, load: load, splice: splice, now: now, restartStopBound: stopBound, worktreeBound: worktreeBound}, nil
}

// SelectionHistory says what a select does to the durable selection history
// (wsm.Workspace.LastSelectedAt, the roster row's last-selected instant).
type SelectionHistory int

const (
	// RecordSelection stamps the selection instant: the user chose the
	// workspace.
	RecordSelection SelectionHistory = iota
	// KeepSelectionHistory leaves the instant standing: the switch is a step
	// of Emacs's walk through the selection history (agentrepl.v1
	// SelectWorkspaceRequest.keep_selection_history), which must not reorder
	// the history it walks.
	KeepSelectionHistory
)

// String names the history for the log.
func (h SelectionHistory) String() string {
	switch h {
	case RecordSelection:
		return "record"
	case KeepSelectionHistory:
		return "keep"
	}
	panic(fmt.Sprintf("workspace: unknown SelectionHistory %d", int(h)))
}
