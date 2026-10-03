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

	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
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
	// FaultID identifies one recorded fault.
	FaultID = ids.FaultID
)

// Workspace is one registered workspace's durable record.
type Workspace struct {
	ID WorkspaceID
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
	// Closed reports whether the workspace's editor state has been torn down.
	Closed bool
	// Attention marks the roster's attention badge; set on a host
	// notification, cleared on SelectWorkspace.
	Attention bool
	// Priority is the roster's ordering priority, nil when unset.
	Priority *Priority
	// Task is the task this workspace is assigned to, nil when unassigned.
	Task *TaskID
	// LastSelectedAt is when the user last selected the workspace.
	LastSelectedAt *time.Time
	// LastActivityAt is when the workspace LAST DID REAL WORK: the most recent
	// turn start or turn close, stamped at the genuine activity edge inside the
	// turn writes (PutTurn/CloseTurn), never at compose or select time. Nil
	// when the workspace has never taken a turn. It is the roster when-column's
	// source — distinct from LastSelectedAt, which times VIEWING recency and
	// must never drive that column — so the column stays stable across mere
	// selection and navigation.
	LastActivityAt *time.Time
	// MergedAt is when the workspace's merge landed, nil when it has not.
	MergedAt *time.Time
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
	// RepoDir is the repository's canonicalized common dir.
	RepoDir string
	// DefaultBranch is the repository's default branch, as the announcing
	// caller read it off git. It is recorded on FIRST SIGHT of the repository
	// and refreshed on every later announcement, so a repository whose default
	// branch is renamed does not keep answering with the old one. Empty leaves
	// whatever is recorded alone, because "not looked up" is not "no default
	// branch".
	DefaultBranch string
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
	// Actions are the configured before/after merge actions.
	Actions MergeActions
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

// MergeActions are the configured prompts run in the workspace before and
// after its merge.
type MergeActions struct {
	// Before is the PROMPT TEXT run in the source workspace before the merge.
	// CreateWorkspaceMergeActions carries the words themselves
	// (conversation.v1.UserSaid), not the name of a file in the prompts
	// directory, so this is what is submitted verbatim.
	Before []string
	// After is the prompt text run after the merge lands, on the same terms.
	After []string
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
	// Accepted records that the user accepted the tray's offer to let the
	// prompt wait for the turn's end (UpdateHeldPrompt.accept), which is legal
	// only on a hold_for_turn_end verdict.
	Accepted bool
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
	// DiscardVerdict clears INTO's verdict and acceptance with the merge, as
	// an edit's replacement does, because both were about the words the merge
	// replaced. False keeps them.
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
	// Address is where the turn's output lands, nil for the root feed.
	Address *OutputAddress
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

// Task is one user task the roster's task view renders.
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

// MergeSourceKind is WHAT a merge lands (agentrepl.v1.MergeWorkspaceSource's
// arm): the requesting workspace's own branch, another workspace's, a branch
// that is no workspace, or the requester's own branch already merged
// upstream.
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
	// MergeSourceMergedUpstream is the requester's own branch, already merged
	// upstream.
	MergeSourceMergedUpstream
)

// ClosesRequester reports whether a landed merge of this source closes the
// workspace that asked for it, which is exactly when keeping that workspace
// open means anything.
func (k MergeSourceKind) ClosesRequester() bool {
	return k == MergeSourceOwnBranch || k == MergeSourceMergedUpstream
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
}
