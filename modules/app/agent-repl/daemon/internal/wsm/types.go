// Package wsm is the daemon's state client: the sole owner of the durable
// fact inventory and of the occupancy lease's policy metadata.
//
// It holds no orchestration logic. Peers call it; it calls only the database.
// See docs/overhaul/daemon.md decision 3 "WSM, THE STATE CLIENT" and
// ARCHITECTURE.md "wsm (internal/wsm)".
package wsm

import (
	"fmt"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	repoconfigv1 "agentrepl/proto/repoconfig/v1"

	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"

	"google.golang.org/protobuf/proto"
)

// The identity newtypes are aliases of internal/ids, the leaf that holds one
// spelling of each. Aliasing rather than redeclaring is what lets feedid —
// which sits below wsm and may not import it — name the same types.
type (
	// WorkspaceID is the daemon-minted, opaque workspace identity.
	WorkspaceID = ids.WorkspaceID
	// RepoID is the daemon-minted repository identity.
	RepoID = ids.RepoID
	// InstanceID identifies one daemon process.
	InstanceID = ids.InstanceID
	// LeaseID identifies one acquisition of a workspace's occupancy lease.
	LeaseID = ids.LeaseID
	// TurnID is the daemon-minted turn identity.
	TurnID = ids.TurnID
	// TaskID identifies one user task.
	TaskID = ids.TaskID
	// SubsectionID identifies one repository subsection.
	SubsectionID = ids.SubsectionID
	// FaultID identifies one recorded fault.
	FaultID = ids.FaultID
)

// Workspace is one registered workspace's durable record.
type Workspace struct {
	ID WorkspaceID
	// Kind is what the workspace is: a repository's worktree, or a zettel
	// chat thread (threads.go).
	Kind WorkspaceKind
	// Repo is the repository the worktree belongs to.
	Repo RepoID
	// Dir is the normalized worktree directory.
	Dir string
	// Name is the workspace's display name (the creation slug, or the
	// directory's base name for a registered one).
	Name string
	// Branch is the worktree's branch.
	Branch string
	// ParentBranch is the base the branch was cut from.
	ParentBranch string
	// Parent is the workspace this one was SPAWNED FROM, recorded at creation
	// when the creation request named one, and nil otherwise. It is the
	// roster's nesting fact stated directly rather than derived: a workspace
	// registered by Emacs (never created through the daemon) carries none, and
	// the roster falls back to the branch lineage for it.
	Parent *WorkspaceID
	// OwnerLands marks a child whose context was not forked from its owner
	// (Parent): only the owner, or the user, lands it. Recorded at creation;
	// never set without Parent.
	OwnerLands bool
	// ContextFrom is the workspace whose conversation was forked into this
	// one at its creation, nil for a fresh context or a registered workspace.
	ContextFrom *WorkspaceID
	// RequestedBy is the workspace whose agent asked for this one's creation,
	// nil when the user asked or the workspace was registered.
	RequestedBy *WorkspaceID
	// CloseWhenTurnEnds is when the workspace asked to close itself, set while
	// it waits for that asking turn to end (selfclose.go); nil otherwise.
	CloseWhenTurnEnds *time.Time
	// Awaiting is the workspace this one awaits correspondence from
	// (standings.go), nil while it awaits none.
	Awaiting *WorkspaceID
	// PrincipalLink is the url that best stands for the workspace's task
	// (principallink.go), which the topbar's summary opens; nil while it has
	// none.
	PrincipalLink *string
	// Closed reports whether the workspace's editor state has been torn down.
	Closed bool
	// Closure is when the workspace was closed and by whom (MarkClosed); nil
	// while it is open, and on a closed row whose close predates the record.
	Closure *Closure
	// Attention marks the roster's attention badge; set on a host
	// notification, cleared on SelectWorkspace.
	Attention bool
	// Priority is the roster's priority badge, nil when unset. It also picks
	// where the workspace lands when it is created or its priority is set
	// (see place.go).
	Priority *Priority
	// Hidden reports whether the user moved the workspace into the roster's
	// hidden section (agentrepl.v1.SetWorkspaceHidden): off every grouping and
	// off the editor's tab bar, its session and tab untouched.
	Hidden bool
	// Current reports whether the workspace is the user's selection
	// (SetCurrent): the one they are looking at. At most one workspace is.
	Current bool
	// Place is the workspace's position in the registry order, which the
	// sidebar and the tab bar both follow. Unique, and at least 1.
	Place int64
	// Task is the task this workspace is assigned to, nil when unassigned.
	Task *TaskID
	// Subsection is the subsection of its repository's section the
	// workspace sits in (arrange.go), nil when it sits in none.
	Subsection *SubsectionID
	// LastSelectedAt is when the user last selected the workspace.
	LastSelectedAt *time.Time
	// HiddenAt is when the user moved the workspace into the hidden section,
	// set exactly while Hidden is (SetHidden).
	HiddenAt *time.Time
	// CacheLapse is a HIDDEN workspace's projected cache lapse, as its shim
	// last stated it (SetCacheLapse); nil when none is known. Never set on a
	// workspace that is not hidden.
	CacheLapse *CacheLapse
	// LastActivityAt is when the workspace LAST DID REAL WORK: the most recent
	// turn start or turn close, stamped at the genuine activity edge inside the
	// turn writes (PutTurn/CloseTurn), never at compose or select time. Nil
	// when the workspace has never taken a turn.
	LastActivityAt *time.Time
	// LastTurnEndedAt is when the workspace's most recent turn CLOSED, of any
	// close kind: derived on read from the turns' closed_at, never stored on
	// the row. Nil when no turn of the workspace has closed yet, including
	// while its first turn is still running. It is the roster when-column's
	// source — distinct from LastSelectedAt, which times VIEWING recency, and
	// from LastActivityAt, which a turn START also moves — so the column
	// counts the idle time since work last finished and stays stable across
	// mere selection, navigation, and the start of the next turn.
	LastTurnEndedAt *time.Time
	// StatusChangedAt is when the roster's status arm for the workspace last
	// moved (SetStatusChangedAt); nil when no change has been recorded.
	StatusChangedAt *time.Time
	// MergedAt is when the workspace's merge landed, nil when it has not.
	MergedAt *time.Time
	// CommitCount is how many commits the workspace's branch holds beyond its
	// merge-base with ParentBranch: recounted when a turn closes, and fixed
	// at the count the merge brought in once MergedAt is set. Nil when it has
	// never been counted.
	CommitCount *uint32
	// CreatedAt is when the record was minted.
	CreatedAt time.Time
	// SpawnedShimPID is the pid of a shim a daemon SPAWNED for this workspace,
	// recorded at the instant the fork returned and cleared when that spawn is
	// stood down. Nil means no daemon has a spawn outstanding here.
	//
	// IT IS NOT Session.ShimPID. That one names the shim SERVING A SESSION and
	// is written once a session exists; this one exists precisely so the
	// window BEFORE a session -- between the fork and the shim's first bound
	// socket -- has a durable trace. A successor daemon that finds the
	// workspace lock free and the socket absent reads this pid before
	// concluding no shim survives.
	SpawnedShimPID *int
	// Result is the roster's last turn result: how the last turn ended and
	// whether the user has seen it. Nil when none stands (no turn has ended
	// since the last prompt). It is what a daemon that did not see the turn
	// end -- a successor after a handover, a restart -- draws the row from
	// until a turn of its own supersedes it.
	Result *TurnResult
	// MergeOf is set on a MERGE WORKSPACE: one the daemon forked from another
	// workspace to carry that workspace's detached merge. Nil for every other
	// workspace.
	MergeOf *MergeOrigin
	// MergeRevealed reports that a MERGE WORKSPACE has been revealed: it
	// needed the user (its merge failed or was abandoned, its agent asked
	// something, the user interrupted it) or the user selected it. An
	// unrevealed, unmerged merge workspace is drawn in no tab bar and no
	// sidebar section. Once set it stays set. Always false for every other
	// workspace.
	MergeRevealed bool
	// Landed is set on a REQUESTER whose detached merge landed: the commit of
	// its own branch that landed, and when (see landed.go). Nil when none has.
	Landed *Landing
	// OneShot reports that the workspace was CREATED as a one-shot. It is
	// READ-ONLY here: the fact is the creation job's (CreationJob.OneShot),
	// read through the shared select list, so the record and the job can never
	// disagree. A workspace with no creation job (one Emacs registered) is not
	// a one-shot.
	OneShot bool
	// MergePending reports that a merge queue entry stands for the workspace
	// in ANY state: recorded while its requesting turn runs, waiting in line,
	// or admitted. A DETACHED merge counts too: once handed off, its entry is
	// the merge workspace's (the one whose MergeOf names this workspace), and
	// it still stands for this workspace's merge. READ-ONLY, from the merge queue in the same read, so a
	// reader that has not seen the merge orchestrator's live facts yet (the
	// roster at boot, which publishes before merges are recovered) still
	// knows a merge stands.
	MergePending bool
}

// TurnResult is the roster's durable read of a workspace's last turn result.
type TurnResult struct {
	// End is the turn-end arm the last turn resolved to.
	End TurnResultEnd
	// Read reports that the user has seen the result.
	Read bool
}

// TurnResultEnd is a turn-end arm, spelled as the roster's status arm.
type TurnResultEnd string

// The turn-end arms a result can stand on. A closed set: a stored value
// outside it is a corrupt row.
const (
	TurnResultDone        TurnResultEnd = "done"
	TurnResultInterrupted TurnResultEnd = "interrupted"
	TurnResultFailed      TurnResultEnd = "turn_failed"
)

// valid reports whether an end is one of the declared arms.
func (e TurnResultEnd) valid() bool {
	switch e {
	case TurnResultDone, TurnResultInterrupted, TurnResultFailed:
		return true
	}
	return false
}

// Repository is one repository's durable record.
type Repository struct {
	ID RepoID
	// Dir is the repository's canonicalized MAIN WORKTREE (symlinks resolved),
	// which is what workspace.v1's RepositoryRef.dir means and what a
	// top-level workspace's merge targets. It is not the common dir: two
	// worktrees of one repository still map to one repository, because they
	// share one main worktree.
	Dir string
	// Name is the repository's display name.
	Name string
	// DefaultBranch is the repository's default branch.
	DefaultBranch string
	// Folded reports that the repository's roster section is collapsed: its
	// rows hidden in the sidebar and its workspaces' tabs off the Emacs bar.
	Folded bool
	// Place is the repository's position among the repositories, which the
	// repository grouping and the tab bar both follow (arrange.go). Unique,
	// and at least 1.
	Place int64
	// Kind is what the row is: a git repository, or the zettel home that holds
	// every chat thread (threads.go).
	Kind RepositoryKind
}

// Subsection is one user-named subsection of a repository's sidebar section
// (arrange.go).
type Subsection struct {
	ID SubsectionID
	// Repo is the repository whose section the subsection sits in.
	Repo RepoID
	// Name is the subsection's drawn label: never blank.
	Name string
	// Place is the subsection's position among its repository's
	// subsections. Unique within the repository, and at least 1.
	Place int64
	// Slot is where the subsection stands among its repository's roots in no
	// subsection (walk.go): just before the first whose registry place is at
	// least Slot, or after every one when nil.
	Slot *int64
	// Folded is whether the subsection is collapsed: its rows hidden in the
	// sidebar and its workspaces' tabs folded off the tab bar
	// (SetSubsectionFolded).
	Folded bool
}

// ForgetReport is what a Forget removed BESIDES the workspace's own rows.
//
// It exists because the repository record's removal is CONDITIONAL — the last
// workspace registered under a repository takes the repository with it, and any
// other workspace keeps it — and a caller that wants to say what it forgot
// cannot re-read a row that is already gone.
type ForgetReport struct {
	// Repository is the repository whose record went with the workspace,
	// empty when another workspace still references it.
	Repository RepoID
	// RepositoryDir is that repository's directory, empty for the same
	// reason. It is carried so the caller can NAME the path that stopped
	// being registered without a second lookup.
	RepositoryDir string
}

// RetireReport is what a RetireRepository removed.
type RetireReport struct {
	// RepositoryDir is the retired repository's directory, carried so the
	// caller can name the path that stopped being registered.
	RepositoryDir string
	// Workspaces are the workspace records that went with it, in id order.
	Workspaces []WorkspaceID
}

// RegisterFacts are the facts Emacs supplies when it announces a workspace.
// Registration is idempotent by normalized dir; the daemon mints the ids.
type RegisterFacts struct {
	// Name is the display name, empty to let the daemon derive one.
	Name string
	// Branch is the worktree's current branch.
	Branch string
	// ParentBranch is the base the branch was cut from, empty when unknown.
	ParentBranch string
	// Parent is the workspace this one was spawned from, nil when the
	// announcement names none. Only the creation path supplies it; a bare
	// registration has no parent workspace to state.
	Parent *WorkspaceID
	// RepoDir is the repository's normalized main-worktree directory.
	RepoDir string
	// DefaultBranch is the repository's default branch, as the announcing
	// caller read it off git. It is recorded on FIRST SIGHT of the repository
	// and refreshed on every later announcement, so a repository whose default
	// branch is renamed does not keep answering with the old one. Empty leaves
	// whatever is recorded alone, because "not looked up" is not "no default
	// branch".
	DefaultBranch string
	// MergeOf marks a MERGE WORKSPACE at its creation, nil for every other
	// workspace. Only the creation path supplies it.
	MergeOf *MergeOrigin
	// OwnerLands, ContextFrom and RequestedBy are Workspace's derivation
	// facts, supplied only by the creation path.
	OwnerLands  bool
	ContextFrom *WorkspaceID
	RequestedBy *WorkspaceID
	// Priority is the priority a minted workspace is created with, nil for
	// none. It is recorded in the insert itself because it decides the place
	// the workspace lands on. An announcement of an already-registered
	// workspace ignores it.
	Priority *Priority
	// Closure, when set, makes a MINTED workspace born closed with this close
	// record: it exists without being open. Only the daemon's own
	// main-worktree guarantee supplies it (a repository's main worktree is
	// registered whenever the repository is known, and is not opened by
	// that). An announcement of an already-registered workspace ignores it.
	Closure *Closure
}

// Priority is the roster's ordering priority, mirroring
// agentrepl.v1.WorkspacePriority's arms.
type Priority int

// The priority values, highest first.
const (
	PriorityP05 Priority = iota
	PriorityP1
	PriorityP2
	PriorityP3
)

// CreationJob is a workspace's lifecycle record from before any session
// exists: where it is being materialized, and what the merge orchestrator must
// read back later. Merge geometry is REFUSED rather than guessed when absent.
type CreationJob struct {
	Workspace WorkspaceID
	// Layout is the merge geometry recorded at creation.
	Layout MergeLayout
	// Hooks are the deploy hooks the workspace's creation recorded, run by
	// every deploy of it in place of its repository's hooks for the same
	// stage.
	Hooks DeployHooks
	// BaseRef is the resolved base the branch was cut from.
	BaseRef string
	// Materialized reports whether the worktree exists on disk. Registration
	// happens only after materialization.
	Materialized bool
	// OneShot marks a one-shot workspace (created, prompted, merged, closed).
	OneShot bool
	// InitialPrompt is the prompt the workspace was created with, empty when
	// created without one.
	InitialPrompt string
	// ConsentedUngatedMode records the permission mode the user consented to at
	// creation, empty when none was.
	ConsentedUngatedMode string
	// CreatedAt is when the job was recorded.
	CreatedAt time.Time
}

// MergeLayout is a workspace's merge geometry: recorded at creation, never
// inferred later.
type MergeLayout struct {
	// SourceBranch is the branch that merges.
	SourceBranch string
	// SourceDir is the worktree that branch lives in.
	SourceDir string
	// TargetDir is the checkout the merge lands in.
	TargetDir string
	// Origin names where the geometry came from (the creating verb).
	Origin string
}

// DeployHooks are the hooks a request or a creation supplies
// (agentrepl.v1.CreateWorkspaceDeployHooks): each stage given here REPLACES the
// repository's own hooks for that stage (repoconfig.v1.DeployConfig), and a
// stage left empty runs the repository's. Each hook is a script or a prompt,
// checked by deployconfig.CheckHook at the boundary that built it.
type DeployHooks struct {
	// PreDeploy runs before the local merge, in order.
	PreDeploy []*repoconfigv1.DeployHook
	// PostDeploy runs after the deploy stage, in order.
	PostDeploy []*repoconfigv1.DeployHook
}

// Empty reports whether no stage is given.
func (h DeployHooks) Empty() bool { return len(h.PreDeploy) == 0 && len(h.PostDeploy) == 0 }

// Equal reports whether o holds equal hooks, stage by stage and in order.
func (h DeployHooks) Equal(o DeployHooks) bool {
	return equalHooks(h.PreDeploy, o.PreDeploy) && equalHooks(h.PostDeploy, o.PostDeploy)
}

// equalHooks reports whether two hook lists are equal hook for hook.
func equalHooks(a, b []*repoconfigv1.DeployHook) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if !proto.Equal(a[i], b[i]) {
			return false
		}
	}
	return true
}

// Session is a workspace's session binding plus the facts that outlive one
// shim process.
type Session struct {
	Workspace WorkspaceID
	// HostSessionID is the DAEMON-minted session identity the host stream
	// echoes and Emacs correlates against. Sessions rotate under one
	// workspace; this is what distinguishes them, and it is not the vendor's
	// id (a fork mints a fresh vendor id for the same host session, and a
	// fresh conversation on one workspace is a new host session).
	HostSessionID string
	// VendorSessionID is the vendor's session identity, for resume.
	VendorSessionID string
	// ConfigDir is the account root the session was spawned under.
	ConfigDir string
	// SelectedConfigDir is the account root the USER CHOSE for this workspace
	// (SelectAccount), empty when nobody has chosen one and the path routing
	// decides. It is a DIFFERENT FACT from ConfigDir, which records where the
	// session actually came up: with one field the two answers collide, and a
	// bring-up cannot tell a root that merely happens to be recorded from a
	// root somebody asked for — so a re-route would either always beat a
	// choice or never take effect at all.
	SelectedConfigDir string
	// Model is the last-writer-wins model fact, in shim order.
	Model string
	// PermissionMode is the session's current permission mode.
	PermissionMode string
	// StartedAt is when the session was first started.
	StartedAt time.Time
	// LastEngagementAt is the idle sweep's input.
	LastEngagementAt time.Time
	// ShimPID is the pid of the shim process serving this session, nil when no
	// shim is up. It exists because the ROLLOUT's stand-down INTENT MANIFEST
	// names a pid per session, and the incoming daemon reconciles that pid
	// against the kernel lock the shim holds; nothing else in the daemon reads
	// it, and it is cleared whenever a session stands down.
	ShimPID *int
	// Terminal is the session's death, nil while it lives. A deleted session
	// REFUSES resurrection.
	Terminal *SessionTerminal
}

// TerminalHibernated is the session terminal the idle sweep records. It is
// REHYDRATABLE — unlike "deleted", which refuses resurrection — because the
// next mount of the workspace revives the session from the compacted
// transcript the Hibernate directive left behind.
//
// IT LIVES HERE BECAUSE IT IS READ FROM THREE PLACES, not only written from
// one: the sweep writes it, the boot's bring-up reads it to leave a
// hibernated workspace asleep, and the open and select verbs read it to know
// a workspace they must revive. One authority, on the type that carries it.
const TerminalHibernated = "hibernated"

// Hibernated reports whether this session was stood down by the idle sweep.
// The terminal is cleared by the next PutSession, so a revived session stops
// answering yes the moment its bring-up records its facts.
func (s Session) Hibernated() bool {
	return s.Terminal != nil && s.Terminal.Kind == TerminalHibernated
}

// SessionTerminal is a session's death with its cause.
type SessionTerminal struct {
	// Kind names the terminal ("deleted", "killed", "shim_died", "superseded").
	Kind string
	// Detail is the cause, kept as evidence.
	Detail string
	// At is when the session died.
	At time.Time
}

// Lease is the workspace's occupancy lease row: the POLICY metadata behind the
// kernel lock that does the arbitration. The lock decides, the row describes.
type Lease struct {
	ID        LeaseID
	Workspace WorkspaceID
	// Holder is the peer that holds it.
	Holder LeaseHolder
	// Policy is what the holder projects onto new submissions.
	Policy LeasePolicy
	// AcquiredAt is when it was taken.
	AcquiredAt time.Time
}

// LeaseHolder is the peer holding a workspace's occupancy lease.
type LeaseHolder int

// The lease holders.
const (
	// HolderMerge is the merge orchestrator.
	HolderMerge LeaseHolder = iota
	// HolderRestart is a shim relaunch (restart-pending).
	HolderRestart
	// HolderDrain is the shutdown drain.
	HolderDrain
	// HolderHibernate is the idle sweep's hibernation.
	HolderHibernate
)

// LeasePolicy is what a lease projects onto new prompt submissions.
type LeasePolicy int

// The lease policies.
const (
	// PolicyRefuse errors new submissions. Nothing writes it since
	// 2026-10-01, when the merge lease began to hold; a merge lease a build
	// before then wrote may still carry it, and it still refuses.
	PolicyRefuse LeasePolicy = iota
	// PolicyHold parks new submissions until release (restart, drain,
	// merge).
	PolicyHold
	// PolicyParked is RETIRED: nothing parks (owner ruling, 2026-09-29) and
	// nothing writes it. A lease row a build before 2026-09-30 wrote may
	// still carry it, so it stays decodable; it reads as a merge lease that
	// refuses, and the boot's merge recovery releases every merge lease.
	PolicyParked
)

// OutputAddress is where a turn's output lands: the feed resolver draws every
// row of a turn recorded with it (Turn.Address) at it. A lease holder stands
// one for the turns it starts itself; every other turn has none and draws on
// the root feed.
type OutputAddress struct {
	// Feed is the feed rows land in.
	Feed feedid.Feed
	// Parent is the row they nest under, nil for top-level rows.
	Parent *feedid.Ref
}

// HeldPrompt is one parked submission. WSM is the ONE durable hold store.
//
// Two ORTHOGONAL facts describe a hold and each has its own column. The
// CLASSIFICATION is the judge's verdict on the prompt (five arms, one column);
// the HOLD KIND is a daemon-side condition unrelated to any verdict (three
// arms, a separate nullable column). Neither is derivable from the other, so
// neither is stored as a projection of the other.
type HeldPrompt struct {
	Workspace WorkspaceID
	// Turn is the minted turn the prompt will run as when released.
	Turn TurnID
	// Said is the WHOLE submission as the user composed it — text and any
	// attached images — kept as the serialized proto blob. A text-only record
	// would silently drop a pasted image, so there is no text column.
	Said *conversationv1.UserSaid
	// Origin is the prompt's origin, as conversation.v1.PromptOrigin names it.
	Origin string
	// SentBy is the workspace whose agent sent the prompt, set exactly when
	// Origin is PROMPT_ORIGIN_AGENT_SENT (sentby.go).
	SentBy *WorkspaceID
	// Target, when set, is the bubble composer's addressed row.
	Target *feedid.Ref
	// Hold is the daemon-side condition holding the prompt, nil when no
	// condition does.
	Hold *HoldKind
	// ScheduleID is the drain schedule a HoldShutdown is waiting on, empty for
	// every other hold kind.
	ScheduleID string
	// Classification is the classifier's verdict, nil until judged.
	Classification *Classification
	// Tombstone is why it was retired, nil while it stands.
	Tombstone *Tombstone
	// QueuedAt is when it was submitted.
	QueuedAt time.Time
	// Delivery is how the prompt asked to be delivered. It is DURABLE because
	// every later decision about a standing hold -- an edit's re-judgement, a
	// restart's restore, a successor's adoption -- must honor it: a deferred
	// prompt is never classified and never interjected, whichever daemon holds
	// it and however often the hold is judged again.
	Delivery Delivery
	// Act, when set, makes the entry a held SESSION ACT rather than a prompt:
	// a model or permission-mode change queued behind the work ahead of it,
	// in order with the prompts around it, and never classified. Said is
	// then what the act is shown as. A context cut is not an Act: it is a
	// prompt whose text is the command, delivered as the cut it is.
	Act *HeldAct
	// Coalesced records that later prompts were folded into this one while it
	// was still queued, so the tray can say so.
	Coalesced bool
}

// HeldAct is a session act held in the queue.
type HeldAct struct {
	// Kind is ActModel or ActPermissionMode.
	Kind string
	// Value is the model or the permission mode the act sets.
	Value string
}

// The held act kinds.
const (
	ActModel          = "model"
	ActPermissionMode = "permission_mode"
)

// Delivery is how a held prompt asked to be delivered: agentrepl.v1's
// SubmitPromptDelivery, as the queue stores it.
type Delivery int

// The deliveries.
const (
	// DeliveryOrdinary is the ordinary delivery: classified against the
	// running turn, which may interject it.
	DeliveryOrdinary Delivery = iota
	// DeliveryDeferred runs as its own turn after the running one: never
	// classified, never interjected.
	DeliveryDeferred
)

// HoldKind is a DAEMON-SIDE condition holding a prompt, independent of any
// classification verdict.
type HoldKind int

// The hold kinds.
const (
	// HoldShutdown holds for the shutdown drain's lease.
	HoldShutdown HoldKind = iota
	// HoldReconnect holds while the session is not up: coming up, being
	// retried, or down until a restart. A failed bring-up never drops it.
	// ITS STORED VALUE IS 1 AND MUST STAY SO: held_prompts.hold_kind persists
	// the integer, and the rename from session_starting kept it.
	HoldReconnect
	// HoldBuildRefresh holds across a build-staleness bounce.
	HoldBuildRefresh
	// HoldMerge holds while a merge of the workspace drives its session. It
	// is bound to the merge it waits on (bindMergeHold): recording one keeps
	// the merge's requester open once it lands.
	HoldMerge
	// HoldColdGate holds while the workspace's session is parked at its cold
	// gate: the prompt waits for the user's answer to the gate, which a prompt
	// never gives. Only a compact-and-resume answer delivers it: clear
	// discards it, and pay or compact move it to HoldColdGateAnswered.
	HoldColdGate
	// HoldColdGateAnswered holds a prompt that was held at a cold gate the
	// user answered with pay or compact: those answers send nothing, so the
	// prompt waits for the user to send it (a release) or cancel it. Nothing
	// else delivers it -- not the session coming up, not a turn's end.
	HoldColdGateAnswered
)

// ClassificationArm is the classifier's verdict on a held prompt: ONE column
// with five arms, mirroring frontend.v1's held-prompt verdict oneof. Interject
// and failure are arms of this one fact, never separate booleans that could
// disagree with it.
type ClassificationArm int

// The classification arms.
const (
	// ArmClassifying is awaiting the classifier's verdict.
	ArmClassifying ClassificationArm = iota
	// ArmInterject interrupts the running turn.
	ArmInterject
	// ArmHoldForTurnEnd waits for the running turn to end.
	ArmHoldForTurnEnd
	// ArmUninterruptibleTurn waits because the running turn refuses interrupts;
	// Command names the session command that made it uninterruptible.
	ArmUninterruptibleTurn
	// ArmClassificationError is the verdict after the classifier failed. It
	// has NO PRODUCER: the prompt queue resolves every failure to
	// ArmHoldForTurnEnd. It is kept, with its tray projection, because
	// removing error coverage needs the owner's sign-off.
	ArmClassificationError
	// ArmAfterToolCall reaches the running turn at its next tool boundary:
	// nothing is interrupted, and the prompt is folded into the turn after
	// the call in flight returns.
	ArmAfterToolCall
)

// Classification is the classifier's verdict on a held prompt.
type Classification struct {
	// Arm is the verdict.
	Arm ClassificationArm
	// Reason is the judge's stated reason, kept as evidence.
	Reason string
	// Command is the recognized session command that made the running turn
	// uninterruptible. It is set only on ArmUninterruptibleTurn and is
	// UNSPECIFIED otherwise.
	Command conversationv1.SessionCommand
	// At is when the verdict landed.
	At time.Time
}

// Tombstone is why a held prompt was retired.
type Tombstone struct {
	// Kind names the retirement ("delivered", "dropped", "session_deleted").
	Kind string
	// At is when it was retired.
	At time.Time
}

// Coalescence is one held prompt folded into another standing in the same
// queue: INTO keeps its place and identity and takes SAID, and FROM is retired.
type Coalescence struct {
	// Into is the hold that takes the merged content and keeps its place.
	Into TurnID
	// From is the hold folded into it, retired by the same transaction.
	From TurnID
	// Said is INTO's whole content after the merge.
	Said *conversationv1.UserSaid
	// Retired is FROM's tombstone.
	Retired Tombstone
	// DiscardVerdict clears INTO's verdict with the merge, as an edit's
	// replacement does, because it was about the words the merge replaced.
	// False keeps it.
	DiscardVerdict bool
}

// Turn is a turn's durable record: the origin that survives a restart, the
// displaced capture a merge takes, and the idempotency claim.
type Turn struct {
	ID        TurnID
	Workspace WorkspaceID
	// Text is the submission's full text.
	Text string
	// Origin is the prompt's origin.
	Origin string
	// SentBy is the workspace whose agent sent the turn's prompt, set exactly
	// when Origin is PROMPT_ORIGIN_AGENT_SENT (sentby.go).
	SentBy *WorkspaceID
	// Address is where the turn's output lands, nil for the root feed.
	Address *OutputAddress
	// Mirror is where the turn is ALSO drawn, in another workspace's feed: an
	// async merge workspace's merge turn, mirrored into the bubble's agentic
	// tab in the requester's feed. Nil for every other turn.
	Mirror *MirrorAddress
	// Displaced marks a turn captured by a lease holder for exactly-once
	// resubmission at release.
	Displaced bool
	// StartedAt is when the turn was opened.
	StartedAt time.Time
	// ClosedAt is when it closed, nil while it runs.
	ClosedAt *time.Time
	// Close is how it ended, nil while it runs.
	Close *TurnClose
}

// TurnClose is how a turn ended.
type TurnClose int

// The turn closes.
const (
	// CloseCompleted is an ordinary agent-side completion.
	CloseCompleted TurnClose = iota
	// CloseFailed is an agent-side failure.
	CloseFailed
	// CloseKilled is a user interrupt.
	CloseKilled
	// CloseOrphaned is a close written for a turn that had no terminal when the
	// daemon reconciled.
	CloseOrphaned
	// CloseAgentDied is a turn the agent process (its shim) cut by dying on its
	// own: nobody ordered the death, and no terminal will ever arrive for it.
	CloseAgentDied
	// CloseFolded is a prompt the vendor folded into the running turn at a tool
	// boundary (conversation.v1.AgentPrompt.folded_into): it was delivered and
	// answered, but as part of that turn, so its own turn never ran and no
	// ending of its own is drawn.
	CloseFolded
	// CloseWithdrawn is a prompt sent to join the running turn that the vendor
	// gave back before taking it in, because an ask opened first
	// (conversation.v1.SessionJoinWithdrawn): its turn never ran, no ending of
	// its own is drawn, and the prompt is held again under a new turn.
	CloseWithdrawn
)

// Failed reports whether the close is the turn failing: its own failure, or
// the agent process dying under it.
func (c TurnClose) Failed() bool { return c == CloseFailed || c == CloseAgentDied }

// RecordedClose is a turn's durable close: how, and when.
type RecordedClose struct {
	How TurnClose
	At  time.Time
}

// DisplacedClaim is what ClaimDisplacedTurn took.
type DisplacedClaim struct {
	// Claimed is true for the one caller that took the displaced mark.
	Claimed bool
	// Closed is true when the claim also closed the turn, which was still open
	// (as CloseOrphaned: no terminal was ever seen for it).
	Closed bool
}

// ClaimStanding is what an idempotency claim found standing on its key.
type ClaimStanding int

// The claim standings.
const (
	// ClaimMinted is a first claim: the key is now bound to the offered turn,
	// unaccepted until the queue takes the submission.
	ClaimMinted ClaimStanding = iota
	// ClaimAccepted is a key whose submission the queue ACCEPTED (delivered or
	// durably held). Only this standing refuses a retry as a duplicate.
	ClaimAccepted
	// ClaimRedriven is a key whose earlier submission never reached
	// acceptance: its handler blocked, errored, was cancelled, or its process
	// died mid-submit. The retry is driven under the turn the claim was
	// ALREADY bound to, never the offered one, so a shim that did accept that
	// turn answers the repeat as a no-op.
	ClaimRedriven
)

// String renders a claim standing for a log record.
func (c ClaimStanding) String() string {
	switch c {
	case ClaimMinted:
		return "minted"
	case ClaimAccepted:
		return "accepted"
	case ClaimRedriven:
		return "redriven"
	default:
		return fmt.Sprintf("claim_standing(%d)", int(c))
	}
}

// IdempotencyClaim is what ClaimIdempotencyKey found and did.
type IdempotencyClaim struct {
	// Standing says which of the three answers this is.
	Standing ClaimStanding
	// Turn is the turn the key is bound to once the claim commits: the offered
	// turn for ClaimMinted, the first submission's turn for ClaimRedriven and
	// ClaimAccepted.
	Turn TurnID
	// Evidence names the durable record that made an unstamped claim
	// ClaimAccepted in this transaction (EvidenceHeld, EvidenceTerminal);
	// empty for a claim that was already stamped, and for the other standings.
	Evidence string
	// Reopened reports that a ClaimRedriven cleared the turn's orphaned or
	// agent-died close, so the retry runs it as an open turn again.
	Reopened bool
}

// The durable evidence that makes an unstamped claim accepted.
const (
	// EvidenceHeld is a held_prompts row under the claimed turn.
	EvidenceHeld = "held_prompt"
	// EvidenceTerminal is the claimed turn's row closed by a vendor terminal.
	EvidenceTerminal = "turn_terminal"
)

// OrphanReport is what CloseOrphans closed, in one transaction.
type OrphanReport struct {
	// Turns are the turns closed as orphans.
	Turns []TurnID
	// At is the instant stamped on every close.
	At time.Time
}

// Task is one user task.
type Task struct {
	ID TaskID
	// Title is the task's text.
	Title string
	// Done reports whether it is complete.
	Done bool
	// CreatedAt is when it was created.
	CreatedAt time.Time
}

// TaskChange is one mutation of a task; a nil field is left alone.
type TaskChange struct {
	// Title retitles the task when set.
	Title *string
	// Done marks it done or reopens it when set.
	Done *bool
}

// TabInterval is one merge round's tab on the merge bubble's sub-feed:
// append-only, round-numbered.
type TabInterval struct {
	// Round is the tab's round number, from one.
	Round int
	// Kind names the tab ("pre_prompt", "merge", "conflicts", "tests", "fixes",
	// "rollout", "post_prompt").
	Kind string
	// StartedAt is when the round opened.
	StartedAt time.Time
	// EndedAt is when it closed, nil while it runs.
	EndedAt *time.Time
	// Outcome is the round's result, empty while it runs.
	Outcome string
}

// MergeLedgerEntry is one workspace's merge-lease ledger row: the lease and
// every tab interval recorded under it.
type MergeLedgerEntry struct {
	Workspace WorkspaceID
	Lease     LeaseID
	// Intervals are the rounds recorded under that lease, in order.
	Intervals []TabInterval
	// OpenedAt is when the ledger was opened.
	OpenedAt time.Time
}

// FaultScope narrows an OpenFaults query.
type FaultScope struct {
	// Workspace, when set, restricts to that workspace's faults.
	Workspace *WorkspaceID
	// Kind, when set, restricts to that fault kind.
	Kind string
}

// FaultMatch names a fault by what it was raised ABOUT rather than by its id:
// one workspace, one kind, and evidence fields that must all be present with
// exactly these values. It is how a caller asks whether a verdict it is about
// to raise was already recorded — by this process or by any before it.
type FaultMatch struct {
	// Workspace is the faulting workspace.
	Workspace WorkspaceID
	// Kind names the fault.
	Kind string
	// Evidence holds the fields that identify the occurrence, keyed as
	// Fault.Evidence is. Every pair must match; fields not named are free.
	Evidence map[string]string
}

// Fault is one recorded fault, open until it is explicitly closed. The
// resolved-at instant is persisted, so a fault's history survives a restart.
type Fault struct {
	ID FaultID
	// Workspace is the faulting workspace, nil for a daemon-scoped fault.
	Workspace *WorkspaceID
	// Kind names the fault. It is the arm name of the typed DaemonFault /
	// SessionFault kind oneof the health reporter answers with, so the record
	// and the wire cannot disagree about which fault this is.
	Kind string
	// Detail is the human-readable evidence.
	Detail string
	// Evidence carries the typed kind's OWN fields — a shim exit code, a
	// stderr tail, a resume cause — keyed by the proto field name. It exists
	// so the reporter fills the typed arm from a record rather than parsing
	// them back out of Detail, which is prose and is allowed to change.
	Evidence map[string]string
	// OpenedAt is when it was raised.
	OpenedAt time.Time
	// ResolvedAt is when it was closed, nil while open.
	ResolvedAt *time.Time
}

// DrainSchedule is the deploy tooling's drain-and-exit control: at most one is
// in force at a time.
type DrainSchedule struct {
	// Reason is why the daemon is draining, from agentrepl.v1.DrainReason.
	Reason string
	// Deadline is when the daemon exits regardless of outstanding work.
	Deadline time.Time
	// SetAt is when the schedule was put in force.
	SetAt time.Time
}

// RepoKey identifies one repository's merge queue: the TARGET repository's
// canonical common dir. It is a path rather than a RepoID because the queue is
// keyed by what a merge lands in, which the orchestrator knows before it knows
// any workspace's registered repository.
type RepoKey string

// MergeQueueState is where one queue entry stands.
type MergeQueueState int

// The merge queue states.
const (
	// MergeQueued is waiting for its repository's turn.
	MergeQueued MergeQueueState = iota
	// MergeAdmitted is the entry the orchestrator is running now.
	MergeAdmitted
	// MergeRequested is a merge RECORDED but not yet in line: the turn that
	// asked for it has not ended. It is durable so a daemon exit before that
	// turn's end does not lose the request, and it is in nobody's line and on
	// no client until it is queued.
	MergeRequested
)

// MergeMove is which way a one-place move takes a waiting merge in its queue
// (agentrepl.v1.UpdateMergeQueueRequest's move_up and move_down arms).
type MergeMove int

// The one-place moves. The zero value is no move, so a move left unset is
// refused rather than read as one of them.
const (
	// MergeMoveUp moves a waiting merge one place toward the front.
	MergeMoveUp MergeMove = iota + 1
	// MergeMoveDown moves a waiting merge one place toward the back.
	MergeMoveDown
)

// String names the move in log records.
func (m MergeMove) String() string {
	switch m {
	case MergeMoveUp:
		return "up"
	case MergeMoveDown:
		return "down"
	}
	return fmt.Sprintf("MergeMove(%d)", int(m))
}

// valid reports whether the move is one of the declared moves.
func (m MergeMove) valid() bool {
	return m == MergeMoveUp || m == MergeMoveDown
}

// MergeSourceKind is WHAT a merge lands: the requesting workspace's own
// branch, another workspace's, or a branch that is no workspace.
type MergeSourceKind int

// The merge sources. The zero value is the requester's own branch, which is
// what every row a build before sources existed recorded.
const (
	// MergeSourceOwnBranch is the requesting workspace's own branch.
	MergeSourceOwnBranch MergeSourceKind = iota
	// MergeSourceWorkspace is another open workspace's branch.
	MergeSourceWorkspace
	// MergeSourceBranch is a branch that is no workspace.
	MergeSourceBranch
	// The fourth value (3), the requester's own branch already merged
	// upstream, is RETIRED with the merged-upstream finish: finishing always
	// runs the repository's deploy. A row carrying it fails validation.
)

// ClosesRequester reports whether a landed merge of this source closes the
// workspace that asked for it, which is exactly when keeping that workspace
// open means anything.
func (k MergeSourceKind) ClosesRequester() bool {
	return k == MergeSourceOwnBranch
}

// MergeSource is one merge's source, stored with its queue entry so a restart
// runs the merge that was asked for.
type MergeSource struct {
	// Kind is the source's arm.
	Kind MergeSourceKind
	// KeepOpen keeps the requester open once its own branch lands. Only the
	// own-branch source carries it.
	KeepOpen bool
	// Workspace is the other workspace, for MergeSourceWorkspace only.
	Workspace WorkspaceID
	// Branch is the branch's name: the named branch for MergeSourceBranch, and
	// the branch checked out in the workspace's worktree when the request was
	// made for every other arm (empty on a request an earlier build recorded).
	Branch string
	// Detach is set on a request that runs ASYNC, in a merge workspace forked
	// from the requester (agentrepl.v1.MergeWorkspaceAsync): the request's own
	// identity, which the merge workspace records as its MergeOrigin.Request so
	// a recovery finds the one workspace forked for THIS request. Empty for an
	// in-place merge.
	Detach DetachedMergeID
	// DetachMaker is the workspace a DETACHED request's merge workspace forks:
	// the one the merged work was done in (a subagent's spawning workspace,
	// or the workspace that cut the branch). Set iff Detach is.
	DetachMaker WorkspaceID
	// DetachTargetMaker lands a DETACHED request into its maker's branch
	// instead of on the primary branch. Only a detached request carries it.
	DetachTargetMaker bool
	// DetachName is a detached request's merge workspace name, as the
	// requesting agent supplied it (unprefixed): RESERVED from the moment the
	// request is recorded until the merge workspace exists under it. Set iff
	// Detach is.
	DetachName string
	// Pick is the requesting agent's instruction for preparing an async
	// merge's branch before it joins the queue
	// (agentrepl.v1.MergeWorkspaceAsync.instruction), empty when the branch
	// lands as it is. It rides the requester's detached request, then
	// the merge workspace's own request, which carries it until the pick has
	// run; PickStartedAt says whether it has begun.
	Pick string
	// PickStartedAt is when the merge workspace's pick turn began, nil while
	// it has not. Only a merge workspace's request with a Pick carries it.
	PickStartedAt *time.Time
	// PickTurn is the merge workspace's pick turn, set with PickStartedAt.
	PickTurn TurnID
	// CompactAfter compacts the requester's conversation once a DETACHED
	// merge lands (agentrepl.v1.MergeWorkspaceAsync.compact_after).
	// Only a detached request carries it: the in-place run has no way to ask.
	CompactAfter bool
	// CloseOnLanding is the workspace a DETACHED request closes once it lands
	// (agentrepl.v1.MergeWorkspaceAsync.one_shot), empty when it closes none.
	// Never the requester.
	CloseOnLanding WorkspaceID
	// CleanupWorktree removes, once a DETACHED request lands, the merge
	// workspace's worktree and CleanupDir
	// (agentrepl.v1.MergeWorkspaceAsync.cleanup_worktree).
	CleanupWorktree bool
	// CleanupDir is the worktree that had the request's branch checked out
	// when the request was accepted, removed once it lands; empty when none
	// qualified. Only a request with CleanupWorktree carries one.
	CleanupDir string
	// Front puts the merge at the FRONT of its queue when it is put in line
	// (QueueMerge): ahead of every merge still waiting, never ahead of the one
	// already running. Any arm may ask for it; a hotfix that must land before
	// the work already waiting is what it is for.
	Front bool
	// Behind puts the merge DIRECTLY BEHIND the named merge (by its queue
	// workspace) when it is put in line, instead of at the back or the front;
	// "" places it by Front. It takes its neighbor's front mark, so a front
	// merge never ends up behind an ordinary one: behind the running merge it
	// is the first in line, marked front; behind a waiting merge it takes
	// that merge's mark. A merge that has left the line by then left its
	// place at the head of the waiting line, which is where this merge goes.
	// Only a REQUEST carries it: the ask is spent once the merge is placed.
	Behind WorkspaceID
	// Version is how the merge moves agent-repl's version once it lands
	// (agentrepl.v1.MergeWorkspaceRequest.version_increment), nil when it
	// moves none. A detached request's hand-off carries it onto the merge
	// workspace's own request.
	Version *VersionIncrement
	// Hooks are the deploy hooks a DETACHED request names
	// (agentrepl.v1.CreateWorkspaceAsyncMerge.deploy_hooks), run by its merge
	// workspace and never by the requester; empty when it names none. Only a
	// detached request carries them: the hand-off moves them onto the merge
	// workspace's origin (MergeOrigin.Hooks), and from there into its creation
	// job, so the merge workspace's own request row carries none.
	Hooks DeployHooks
}

// DetachedMergeID is one detached merge request's identity, minted when the
// request is recorded.
type DetachedMergeID string

// MergeOrigin is what a MERGE WORKSPACE was forked for: the workspace whose
// detached merge it carries, and that request's identity.
type MergeOrigin struct {
	// Requester is the workspace that asked for the detached merge. The roster
	// nests the merge workspace under it, and the merge's handback is
	// delivered to its session.
	Requester WorkspaceID
	// Request is the detached request the merge workspace was forked for.
	Request DetachedMergeID
	// Maker is the workspace whose conversation the merge workspace forks:
	// the one the merged work was done in (a subagent's spawning workspace,
	// or the workspace that cut the branch). TRANSPORT-ONLY, like the landing
	// fields below: the create records it as the merge workspace's
	// ContextFrom.
	Maker WorkspaceID
	// ForkHead is the commit the merge workspace was cut at: the requester's
	// branch tip when the merge was handed off, which is exactly what the
	// merge lands. Empty only on a merge workspace forked before layout 41
	// recorded it.
	ForkHead string
	// CompactAfter is the detached request's ask that the requester's
	// conversation be compacted once this merge workspace's branch lands
	// (MergeSource.CompactAfter, carried over when the workspace is forked).
	CompactAfter bool
	// CloseOnLanding is the workspace this merge workspace's landing closes
	// (MergeSource.CloseOnLanding, carried over when the workspace is forked).
	CloseOnLanding WorkspaceID
	// CleanupWorktree removes this merge workspace's worktree and CleanupDir
	// once its branch lands (MergeSource.CleanupWorktree).
	CleanupWorktree bool
	// CleanupDir is the worktree the landing removes besides this merge
	// workspace's own (MergeSource.CleanupDir).
	CleanupDir string

	// THE LANDING THE QUEUE DECIDED. The merge queue decides where an async
	// merge lands ONCE, at hand-off (merge.orchestrator.handOffDetached), and
	// the merge workspace's create lands by exactly these, reading nothing off
	// the requester. They are TRANSPORT-ONLY: the create records TargetDir and
	// Hooks in the merge workspace's own creation job and ParentBranch on
	// its registry row, which is where they live durably, so the stored origin
	// (workspaces.merge_*) does not carry them and a read of one answers them
	// empty.

	// TargetDir is the checkout the merge workspace's branch lands in: the
	// requester's creation-job target, or its repository's main worktree when
	// the requester is the main checkout.
	TargetDir string
	// ParentBranch is the branch the merge workspace lands on: the
	// requester's own parent branch.
	ParentBranch string
	// Hooks are the deploy hooks the async request named (MergeSource.Hooks),
	// and nothing else: a merge workspace never inherits its requester's
	// hooks.
	Hooks DeployHooks
}

// MergeQueueEntry is one workspace's place in its repository's merge queue. The
// queue is DURABLE so a restart re-enqueues exactly what was waiting, in the
// order it was waiting in.
type MergeQueueEntry struct {
	// Repo is the queue's repository key.
	Repo RepoKey
	// Workspace is the workspace whose merge is queued.
	Workspace WorkspaceID
	// Position is the entry's one-based place in the queue's order.
	Position int
	// State is where the entry stands.
	State MergeQueueState
	// Source is what the merge lands.
	Source MergeSource
	// EnqueuedAt is when it joined the queue.
	EnqueuedAt time.Time
	// Ledger is the merge bubble's ledger identity, written when the merge
	// was put in line; empty while it is only requested, and on a row an
	// earlier build queued.
	Ledger LeaseID
}
