package wsm

import (
	"context"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	// The daemon's SQLite driver. Registered here because wsm is the sole
	// owner of the database handle; nothing else opens it.
	_ "modernc.org/sqlite"
)

// DB is the daemon's durable state. One handle, one writer: the
// implementation opens a single *sql.DB with SetMaxOpenConns(1), the DSN
// carrying _pragma=busy_timeout(5000)&_pragma=journal_mode(WAL)&
// _txlock=immediate. A layout version table gates the open; a newer layout
// refuses to open. Every load is all-or-nothing — a corrupt row fails the
// whole read, and nothing is loaded.
type DB interface {
	// Close releases the handle.
	Close() error
	// ReadOnly reports whether this handle was opened read-only.
	ReadOnly() bool
	// Promote turns a READ-ONLY handle into a writing one, in place. It exists
	// for the handover's successor, which opens read-only because the
	// incumbent is still the sole writer and becomes a writer at its first
	// adoption. Promoting a handle that already writes is success.
	Promote(ctx context.Context) error

	// RegisterWorkspace records a workspace, idempotent by normalized dir,
	// minting a WorkspaceID and, when the workspace is its repository's main
	// worktree, a RepoID on first sight. A linked worktree whose repository
	// is not yet known is refused with ErrMainWorktreeUnregistered. The bool
	// reports whether the record was created.
	RegisterWorkspace(ctx context.Context, dir string, facts RegisterFacts) (Workspace, bool, error)
	// RegisterThread records a new zettel chat thread at dir, inside the zettel
	// home (dir's parent), which it mints on first sight. Not idempotent: a
	// directory already registered is ErrThreadNameTaken.
	RegisterThread(ctx context.Context, dir, name string) (Workspace, error)
	// Workspace loads one workspace by id.
	Workspace(ctx context.Context, id WorkspaceID) (Workspace, error)
	// WorkspaceByDir loads one workspace by its normalized worktree directory.
	WorkspaceByDir(ctx context.Context, dir string) (Workspace, error)
	// ListWorkspaces loads every workspace, all-or-nothing.
	ListWorkspaces(ctx context.Context) ([]Workspace, error)
	// RefuseTemporary answers the *tempdirs.InsideError RegisterWorkspace
	// would refuse dir with, or nil: the SAME check, for a
	// verb that must refuse a temporary directory before it registers
	// anything (CreateWorkspace's repository). A dir that cannot be judged is
	// an ordinary error.
	RefuseTemporary(dir string) error
	// ListRepositories loads every GIT repository, all-or-nothing. The zettel
	// home, which holds the chat threads (threads.go), is no repository to
	// any repository feature -- the roster's sections, the reaper, the
	// divergence sweep, the boot's retirement, the merge queue -- and is
	// never listed.
	ListRepositories(ctx context.Context) ([]Repository, error)
	// SetRepositoryFolded records whether a repository's roster section is
	// collapsed; an unknown repository is ErrNotFound.
	SetRepositoryFolded(ctx context.Context, id RepoID, folded bool) error
	// SetSubsectionFolded records whether a subsection is collapsed; an
	// unknown subsection is UnknownSubsectionError (subsectionfolds.go).
	SetSubsectionFolded(ctx context.Context, id SubsectionID, folded bool) error
	// MarkClosed records that a workspace's editor state is torn down, when,
	// and by whom; the first close's record stands (see the store's doc).
	MarkClosed(ctx context.Context, id WorkspaceID, closure Closure) error
	// RequireOpenParent answers ParentNotOpenError when the parent is hidden,
	// closed or closing, nil when it is open (family.go).
	RequireOpenParent(ctx context.Context, parent WorkspaceID) error
	// RequireNoOpenChildren answers HasOpenChildrenError for the first of ids
	// with an open descendant, nil when none has (family.go).
	RequireNoOpenChildren(ctx context.Context, ids ...WorkspaceID) error
	// Reopen records that a workspace's editor state stands again, clearing
	// its close record and, for a merged workspace, the merge's settlement,
	// and records SERVING as the daemon serving it.
	Reopen(ctx context.Context, id WorkspaceID, serving InstanceID) error
	// SetCurrent records the user's selection of a workspace at an instant,
	// expanding its repository's section and subsection in the same write
	// (setCurrent).
	SetCurrent(ctx context.Context, id WorkspaceID, at time.Time) error
	// SetCurrentKeepingLastSelected records a workspace as current and leaves
	// its last-selected instant as it stands: a step of a walk through the
	// selection history, which must not reorder the history it walks. It
	// expands the folds holding the workspace as SetCurrent does.
	SetCurrentKeepingLastSelected(ctx context.Context, id WorkspaceID) error
	// Current reports the currently selected workspace, nil when none is.
	Current(ctx context.Context) (*WorkspaceID, error)
	// DataVersion answers SQLite's `PRAGMA data_version` on this handle's one
	// connection: a number that changes exactly when ANOTHER connection --
	// another daemon sharing the file during a handover -- commits, and never
	// for this handle's own writes. Compare two answers for equality only;
	// the number itself means nothing, and a promotion (which swaps the
	// connection) may change it with no peer commit at all.
	DataVersion(ctx context.Context) (int64, error)
	// SetPriority sets or clears a workspace's priority; setting one moves the
	// workspace to second-to-last within that priority.
	SetPriority(ctx context.Context, id WorkspaceID, p *Priority) error
	// SetHidden records whether a workspace sits in the roster's hidden
	// section; an unknown workspace is ErrNotFound.
	SetHidden(ctx context.Context, id WorkspaceID, hidden bool, at time.Time) error
	// SetCacheLapse records a hidden workspace's projected cache lapse, and
	// answers whether it was recorded: a workspace not hidden takes none.
	SetCacheLapse(ctx context.Context, id WorkspaceID, lapse CacheLapse) (bool, error)
	// RevealMergeWorkspace records that a merge workspace has been revealed,
	// answering whether this call revealed it; a workspace that is no merge
	// workspace is refused, an unknown one is ErrNotFound.
	RevealMergeWorkspace(ctx context.Context, id WorkspaceID) (bool, error)
	// SetParent retargets a workspace: its recorded parent (nil: top-level)
	// and its merge target directory, in one transaction. A parent the
	// registry does not hold, of another repository, or beneath the
	// workspace, and a workspace with no creation job, are refused with a
	// ParentRefusedError; an unknown workspace is ErrNotFound.
	SetParent(ctx context.Context, id WorkspaceID, parent *WorkspaceID) (ParentChange, error)
	// RestoreParent puts a retarget's prior target back while the workspace
	// still stands as the retarget left it; stale reports a retarget that
	// has since been superseded, which is left alone.
	RestoreParent(ctx context.Context, change ParentChange) (stale bool, err error)
	// SwapPlaces exchanges the places of workspaces ws and target in the
	// registry order, every other workspace keeping its own; an unknown id is
	// ErrNotFound and nothing is written.
	SwapPlaces(ctx context.Context, ws, target WorkspaceID) error
	// MoveWorkspace moves ws onto target's tab in the arrangement's walk,
	// the two being siblings there (arrange.go).
	MoveWorkspace(ctx context.Context, ws, target WorkspaceID) error
	// ListSubsections loads every subsection, by repository and then in each
	// repository's stored order (arrange.go).
	ListSubsections(ctx context.Context) ([]Subsection, error)
	// ReadArrangement reads the workspaces, repositories and subsections as
	// ONE snapshot (arrange.go).
	ReadArrangement(ctx context.Context) (Arrangement, error)
	// CreateSubsection records a named subsection after REPO's others.
	CreateSubsection(ctx context.Context, repo RepoID, name string) (Subsection, error)
	// CreateAgentSubsection records a named subsection a workspace's agent
	// created, with its first members placed in it, in one write; it is
	// removed by the write that leaves it with no open workspace.
	CreateAgentSubsection(ctx context.Context, creator WorkspaceID, name string, members []WorkspaceID) (Subsection, error)
	// RenameSubsection relabels one subsection.
	RenameSubsection(ctx context.Context, id SubsectionID, name string) error
	// DeleteSubsection removes one subsection, its workspaces returning to
	// none.
	DeleteSubsection(ctx context.Context, id SubsectionID) error
	// MoveSubsection moves one subsection among its repository's entries:
	// before another subsection, before a root in no subsection, or last.
	MoveSubsection(ctx context.Context, id SubsectionID, to Anchor) error
	// MoveRepository moves one repository before another (nil: last).
	MoveRepository(ctx context.Context, id RepoID, before *RepoID) error
	// PlaceWorkspaces puts root workspaces, with their families, into a
	// subsection of their repository (nil: none) as one block at an anchor
	// there: before a root, before a subsection (into none only), or last;
	// clearing or keeping the families' one-shot marks as MARKS says.
	PlaceWorkspaces(ctx context.Context, ws []WorkspaceID, into *SubsectionID, at Anchor, marks OneShotMarks) error
	// SetAttention sets or clears the roster's attention marker.
	SetAttention(ctx context.Context, id WorkspaceID, on bool) error
	// SetResult records, or clears with nil, the roster's last turn result.
	SetResult(ctx context.Context, id WorkspaceID, result *TurnResult) error
	// SetStatusChangedAt advances the instant the roster's status arm for the
	// workspace last moved; it never moves backward (see statuschanged.go).
	SetStatusChangedAt(ctx context.Context, id WorkspaceID, at time.Time) error
	// SetMergedAt records that the workspace's merge landed and, when commits
	// is set, the commit count it brought in (see commitcount.go).
	SetMergedAt(ctx context.Context, id WorkspaceID, at time.Time, commits *uint32) error
	// SetLanded records, or clears with nil, a requester's record of its
	// detached merge landing (see landed.go).
	SetLanded(ctx context.Context, id WorkspaceID, landing *Landing) error
	// SetLiveCommitCount records a live workspace's branch commit count. It
	// answers false, writing nothing, once the workspace has merged.
	SetLiveCommitCount(ctx context.Context, id WorkspaceID, count uint32) (bool, error)
	// Forget deletes a workspace's every record — the nuke's durable half and
	// the whole of the forget verb — and, when the workspace was the last one
	// registered under its repository, that repository's record too. A main
	// worktree's workspace is refused with ErrMainWorktreeInUse while other
	// workspaces are registered under its repository. The
	// report names the repository that went, empty when one stayed.
	Forget(ctx context.Context, id WorkspaceID) (ForgetReport, error)
	// RetireRepository deletes a repository's record together with every
	// workspace registered under it, in one transaction. It refuses with
	// ErrRepositoryInUse while any of those workspaces is open or still holds
	// live state (an undelivered held prompt, a lease, a merge-queue entry);
	// an unknown repository is ErrNotFound.
	RetireRepository(ctx context.Context, id RepoID) (RetireReport, error)

	// PutCreationJob records a workspace's merge geometry, configured actions
	// and materialization state.
	PutCreationJob(ctx context.Context, job CreationJob) error
	// CreationJob loads one workspace's creation job; the bool reports
	// existence. Merge refuses rather than guessing when it is absent.
	CreationJob(ctx context.Context, id WorkspaceID) (CreationJob, bool, error)

	// PutSession records a workspace's session binding and spawn identity.
	PutSession(ctx context.Context, s Session) error
	// Session loads one workspace's session; the bool reports existence.
	Session(ctx context.Context, id WorkspaceID) (Session, bool, error)
	// Sessions loads every workspace's session in one read, ordered by
	// workspace.
	Sessions(ctx context.Context) ([]Session, error)
	// SetSessionTerminal records a session's death with its cause. A deleted
	// session refuses resurrection.
	SetSessionTerminal(ctx context.Context, id WorkspaceID, t SessionTerminal) error
	// SetVendorSessionID records the vendor session id a later resume names
	// (the id in force after a rotation), answering the one it replaced.
	SetVendorSessionID(ctx context.Context, id WorkspaceID, vendorSessionID string) (string, error)
	// SetSessionModel records the model the user chose for a workspace's
	// session, the one every later start names. An empty model is refused.
	SetSessionModel(ctx context.Context, id WorkspaceID, model string) error
	// ClearSessionTerminal retires a workspace's terminal session record, so
	// a workspace whose shim is live carries none. A workspace with no
	// session row has nothing to retire and is not a refusal; a deleted
	// session is never resurrected.
	ClearSessionTerminal(ctx context.Context, id WorkspaceID) error
	// TouchEngagement records last engagement — the idle sweep's input.
	TouchEngagement(ctx context.Context, id WorkspaceID, at time.Time) error
	// SetShimPID records, or clears with nil, the pid of the shim process
	// serving a workspace's session. The rollout's intent manifest names it.
	SetShimPID(ctx context.Context, id WorkspaceID, pid *int) error
	// SetSpawnedShimPID records, or clears with nil, the pid of a shim a
	// daemon SPAWNED for this workspace, written at the instant the fork
	// returns. A successor daemon that finds the workspace lock free and the
	// socket absent reads it before concluding no shim survives.
	SetSpawnedShimPID(ctx context.Context, id WorkspaceID, pid *int) error

	// AcquireLease takes the workspace's occupancy lease for holder under
	// policy, refusing when it is already held.
	AcquireLease(ctx context.Context, id WorkspaceID, holder LeaseHolder, policy LeasePolicy) (Lease, error)
	// AcquireLeaseAs acquires it under an identity the CALLER minted. The
	// merge's lease id is also its LEDGER identity -- the merge bubble is
	// addressed by it and drawn from the moment the merge is QUEUED, before
	// any occupancy is taken -- so it cannot be minted at acquisition.
	AcquireLeaseAs(ctx context.Context, id WorkspaceID, lease LeaseID, holder LeaseHolder, policy LeasePolicy) (Lease, error)
	// ReleaseLease releases one acquisition.
	ReleaseLease(ctx context.Context, leaseID LeaseID) error
	// Lease loads a workspace's current lease; the bool reports whether one is
	// held.
	Lease(ctx context.Context, id WorkspaceID) (Lease, bool, error)
	// ForeignLeases lists every held lease THIS HANDLE did not acquire. A
	// daemon holds one handle for its whole life, so on an incumbent's boot
	// each of them is an orphan whose owning process is gone.
	ForeignLeases(ctx context.Context) ([]Lease, error)
	// SetLeasePolicy changes what a held lease projects onto new submissions
	// (a merge moving from refusing to parked).
	SetLeasePolicy(ctx context.Context, leaseID LeaseID, p LeasePolicy) error

	// PutHeldPrompt records a parked submission. WSM is the one durable hold
	// store.
	PutHeldPrompt(ctx context.Context, h HeldPrompt) error
	// UpdateHeldPromptClassification records the classifier's verdict.
	UpdateHeldPromptClassification(ctx context.Context, turn TurnID, c Classification) error
	// UpdateHeldPromptHold changes or clears the daemon-side condition holding a
	// prompt. scheduleID is the drain schedule a HoldShutdown waits on and is
	// required for that arm, empty for every other kind.
	UpdateHeldPromptHold(ctx context.Context, turn TurnID, h *HoldKind, scheduleID string) error
	// TombstoneHeldPrompt retires a held prompt with its reason.
	TombstoneHeldPrompt(ctx context.Context, turn TurnID, why Tombstone) error
	// TombstoneHeldPrompts retires several held prompts with one reason, all
	// or nothing.
	TombstoneHeldPrompts(ctx context.Context, turns []TurnID, why Tombstone) error
	// ReplaceHeldPromptSaid replaces a STANDING hold's content (an edit's
	// commit) and discards its verdict: the classification and the acceptance
	// are cleared in the same transaction, so the new content is never read
	// beside the old content's verdict. Its queue position is unchanged.
	ReplaceHeldPromptSaid(ctx context.Context, turn TurnID, said *conversationv1.UserSaid) error
	// CoalesceHeldPrompts folds one standing hold into another standing in the
	// same queue, in ONE transaction: the merged content (marked coalesced, and
	// with its verdict discarded when the coalescence says so) and the folded
	// hold's tombstone land together or not at all.
	CoalesceHeldPrompts(ctx context.Context, c Coalescence) error
	// HeldPromptByTurn loads ONE hold by its turn, retired or not, reporting
	// false when no hold was ever recorded under the turn. It is what tells an
	// unknown turn from a delivered or a dropped one.
	HeldPromptByTurn(ctx context.Context, turn TurnID) (HeldPrompt, bool, error)
	// HeldPrompts loads one workspace's standing holds, all-or-nothing.
	HeldPrompts(ctx context.Context, id WorkspaceID) ([]HeldPrompt, error)
	// AllHeldPrompts loads every standing hold for the boot restore,
	// all-or-nothing: a corrupt row fails the read and nothing is loaded.
	AllHeldPrompts(ctx context.Context) ([]HeldPrompt, error)

	// PutTurn records a turn's durable origin and address. It NEVER writes a
	// close: a record carrying one is refused, and an existing row's close is
	// kept, because a turn closes only through the prompt queue's door.
	PutTurn(ctx context.Context, t Turn) error
	// CloseTurn stamps a turn's close. Only the prompt queue's door calls it.
	CloseTurn(ctx context.Context, turn TurnID, at time.Time, how TurnClose) error
	// TurnCloses answers the recorded close of each named turn that has one;
	// an open or unknown turn is absent. It is what a feed replay draws a
	// turn's ending from when the turn's own terminal was never stored.
	TurnCloses(ctx context.Context, id WorkspaceID, turns []TurnID) (map[TurnID]RecordedClose, error)
	// TurnAddresses answers the output address each named turn of this
	// workspace was RECORDED with: where its rows landed when it ran. A recorded
	// turn whose output went to the root feed answers a nil address; a turn the
	// workspace never recorded is absent. A feed replay places each turn's rows
	// by it, so a replayed turn is drawn where it was drawn live.
	TurnAddresses(ctx context.Context, id WorkspaceID, turns []TurnID) (map[TurnID]*OutputAddress, error)
	// TurnMirrors answers the mirror address each named turn of this
	// workspace was recorded with, for the turns that have one: where else its
	// rows were drawn (an async merge workspace's merge turn, in the
	// requester's bubble). A feed replay mirrors each such turn by it.
	TurnMirrors(ctx context.Context, id WorkspaceID, turns []TurnID) (map[TurnID]*MirrorAddress, error)
	// TurnSenders answers the workspace whose agent sent each named turn's
	// prompt, for the turns that have one (sentby.go). A feed replay heads each
	// such turn's prompt by it.
	TurnSenders(ctx context.Context, id WorkspaceID, turns []TurnID) (map[TurnID]WorkspaceID, error)
	// RecordedTurns answers which of the named turns this workspace recorded,
	// open or closed: the durable statement of which turns are the
	// workspace's OWN work. A fork's feed reads it to tell the conversation it
	// inherited from the turns it ran itself.
	RecordedTurns(ctx context.Context, id WorkspaceID, turns []TurnID) (map[TurnID]bool, error)
	// OpenTurns loads a workspace's turns that have no terminal.
	OpenTurns(ctx context.Context, id WorkspaceID) ([]Turn, error)
	// HasTurns reports whether a workspace has EVER recorded a turn, open or
	// closed. It is an existence question, not a listing: the classifier asks
	// it to tell a conversation that was never engaged from one that was, and
	// a listing would make that answer cost the whole table.
	HasTurns(ctx context.Context, id WorkspaceID) (bool, error)
	// RecentConversationPrompts answers the most recent `limit` prompts of a
	// workspace's conversation, oldest first: its own turns and, when those
	// fall short, the turns it inherited through its fork lineage.
	RecentConversationPrompts(ctx context.Context, id WorkspaceID, limit int) ([]ConversationPrompt, error)
	// BindSession records a workspace's session bound to a different
	// conversation and, in the same transaction, forgets the fork the
	// workspace was: its lineage and any fork it still owed.
	BindSession(ctx context.Context, sess Session) error
	// PutPendingFork records that a workspace is to be forked from its parent
	// at its first bring-up.
	PutPendingFork(ctx context.Context, fork PendingFork) error
	// PendingFork answers the fork a workspace still owes, if any.
	PendingFork(ctx context.Context, id WorkspaceID) (PendingFork, bool, error)
	// ResolveFork records a made fork in one transaction: the session's vendor
	// id, the lineage, and the pending fork's retirement.
	ResolveFork(ctx context.Context, vendorSessionID string, lineage ForkLineage) error
	// Lineage answers where a workspace's inherited past is read from, nearest
	// generation first; empty for a workspace never forked.
	Lineage(ctx context.Context, id WorkspaceID) ([]ForkLineage, error)
	// AllDisplacedTurns loads every turn still marked displaced, across every
	// workspace and REGARDLESS of whether the turn is still open: a merge
	// displaces a turn by ending it, so the record a boot has to put back is
	// normally a closed one. It is the boot recovery's whole input.
	AllDisplacedTurns(ctx context.Context) ([]Turn, error)
	// ClaimDisplacedTurn takes exclusive ownership of a displaced turn: it
	// clears the mark and, for a turn still open, stamps its close as
	// CloseOrphaned, in ONE transaction. It reports whether THIS caller took
	// the record, and whether the claim closed the turn —
	// false means somebody else already did, and the caller must not put the
	// turn back. It is what makes the resubmission exactly-once with two
	// possible owners (the merge's own release and the boot recovery).
	ClaimDisplacedTurn(ctx context.Context, turn TurnID, at time.Time) (DisplacedClaim, error)
	// OweResumption records that a stand-down this daemon ordered cut turn on
	// ws, so the next session that serves ws resumes it (owedresumptions.go).
	OweResumption(ctx context.Context, ws WorkspaceID, turn TurnID, at time.Time) error
	// ClaimResumptions takes every resumption ws is owed and answers how many,
	// exactly once across every boot and session-up edge.
	ClaimResumptions(ctx context.Context, ws WorkspaceID) (int, error)
	// OwedTurns answers the turns ws is owed a resumption for, unclaimed.
	OwedTurns(ctx context.Context, ws WorkspaceID) ([]TurnID, error)
	// OweHandBack records a landed async merge's report as owed until the
	// build that carries the landing is running (owedhandbacks.go).
	OweHandBack(ctx context.Context, owed OwedHandBack) error
	// CoverOwedHandBacks marks every owed report covered by the deploy
	// committing now, and answers how many it newly covered.
	CoverOwedHandBacks(ctx context.Context) (int, error)
	// OwedHandBacks answers the owed reports, oldest first: only the covered
	// ones when coveredOnly.
	OwedHandBacks(ctx context.Context, coveredOnly bool) ([]OwedHandBack, error)
	// PayHandBack deletes mergeWS's owed report once it was submitted, and
	// answers whether one was there.
	PayHandBack(ctx context.Context, mergeWS WorkspaceID) (bool, error)
	// ClaimIdempotencyKey binds a client's key to a turn. A key whose
	// submission the queue ACCEPTED answers ClaimAccepted with that turn and
	// binds nothing; a key claimed for a submission that never reached
	// acceptance answers ClaimRedriven with the turn it was ALREADY bound to
	// (reopening an orphaned or agent-died close on it), so the retry is
	// delivered under the same turn id rather than refused.
	ClaimIdempotencyKey(ctx context.Context, id WorkspaceID, key string, turn TurnID) (IdempotencyClaim, error)
	// AcceptIdempotencyKey records that the queue accepted the submission the
	// key is bound to under turn. It refuses a key not bound to that turn, or
	// already accepted.
	AcceptIdempotencyKey(ctx context.Context, id WorkspaceID, key string, turn TurnID) error

	// CloseOrphans closes every turn without a terminal in one transaction and
	// reports what it closed.
	CloseOrphans(ctx context.Context, id WorkspaceID, at time.Time) (OrphanReport, error)

	// CreateTask records a new user task.
	CreateTask(ctx context.Context, title string) (Task, error)
	// UpdateTask applies one mutation to a task.
	UpdateTask(ctx context.Context, id TaskID, change TaskChange) error
	// Tasks loads every task, all-or-nothing.
	Tasks(ctx context.Context) ([]Task, error)
	// SetMergedSectionFolded records whether the recently-merged band is
	// collapsed.
	SetMergedSectionFolded(ctx context.Context, folded bool) error
	// SetHiddenSectionFolded records whether the hidden-workspaces section is
	// collapsed.
	SetHiddenSectionFolded(ctx context.Context, folded bool) error
	// SetOpenSectionFolded records whether the open-workspaces section is
	// collapsed.
	SetOpenSectionFolded(ctx context.Context, folded bool) error
	// SidebarView loads the sidebar's view state that belongs to no row, or
	// DefaultSidebarView when nobody has changed it.
	SidebarView(ctx context.Context) (SidebarView, error)
	// SetAccountUsage records an account root's last usage evidence,
	// replacing what the root had.
	SetAccountUsage(ctx context.Context, usage AccountUsage) error
	// AccountUsages loads every account root's last usage evidence,
	// all-or-nothing.
	AccountUsages(ctx context.Context) ([]AccountUsage, error)
	// PutRevealSpeedWindow replaces the stored block-speed window of one
	// model and block kind.
	PutRevealSpeedWindow(ctx context.Context, w RevealSpeedWindow) error
	// RevealSpeedWindows loads every stored block-speed window.
	RevealSpeedWindows(ctx context.Context) ([]RevealSpeedWindow, error)
	// AssignWorkspaceTask assigns a workspace to a task, or unassigns it when
	// task is nil.
	AssignWorkspaceTask(ctx context.Context, id WorkspaceID, task *TaskID) error

	// OpenMergeLedger opens a workspace's ledger for one merge lease.
	OpenMergeLedger(ctx context.Context, id WorkspaceID, lease LeaseID) error
	// RecordTabInterval appends one round's tab interval to a lease's ledger.
	RecordTabInterval(ctx context.Context, lease LeaseID, interval TabInterval) error
	// MergeLedger loads a workspace's ledger entries, all-or-nothing.
	MergeLedger(ctx context.Context, id WorkspaceID) ([]MergeLedgerEntry, error)
	// PutMergeProgress records an admitted merge's progress, replacing the
	// workspace's record.
	PutMergeProgress(ctx context.Context, p MergeProgress) error
	// MergeProgressOf loads a workspace's merge progress record; the bool is
	// false when none stands.
	MergeProgressOf(ctx context.Context, id WorkspaceID) (MergeProgress, bool, error)
	// SweepOrphanedMergeState releases merge leases, drops progress records
	// and closes open ledger rounds that no queue entry names any more
	// (mergeorphans.go).
	SweepOrphanedMergeState(ctx context.Context, at time.Time) (OrphanedMergeState, error)
	// DropMergeProgress deletes one merge lease's progress record and
	// reports whether one stood.
	DropMergeProgress(ctx context.Context, lease LeaseID) (bool, error)
	// PutWorkspaceSummary records a workspace's synthesized summary and the
	// digest it was distilled from, replacing any it had (summaries.go).
	PutWorkspaceSummary(ctx context.Context, id WorkspaceID, sum WorkspaceSummary) error
	// ResetWorkspaceSummaryDigest forgets the digest a workspace's summary was
	// distilled from, keeping the title.
	ResetWorkspaceSummaryDigest(ctx context.Context, id WorkspaceID) error
	// DropWorkspaceSummary deletes a workspace's summary.
	DropWorkspaceSummary(ctx context.Context, id WorkspaceID) error
	// WorkspaceSummaryOf loads a workspace's summary; the bool is false when
	// none stands.
	WorkspaceSummaryOf(ctx context.Context, id WorkspaceID) (WorkspaceSummary, bool, error)
	// ListWorkspaceSummaries loads every stored summary by workspace; a
	// workspace with none is absent.
	ListWorkspaceSummaries(ctx context.Context) (map[WorkspaceID]WorkspaceSummary, error)
	// PutColdReadPayment records the cold read a workspace's user paid for,
	// replacing any it had (coldreadpayments.go).
	PutColdReadPayment(ctx context.Context, id WorkspaceID, p ColdReadPayment) error
	// DropColdReadPayment deletes a workspace's cold read payment.
	DropColdReadPayment(ctx context.Context, id WorkspaceID) error
	// ColdReadPaymentOf loads a workspace's cold read payment; the bool is
	// false when none stands.
	ColdReadPaymentOf(ctx context.Context, id WorkspaceID) (ColdReadPayment, bool, error)
	// AdoptMergeLease takes ownership of the merge lease a previous process
	// left held, for the merge this process resumes under it.
	AdoptMergeLease(ctx context.Context, id WorkspaceID, lease LeaseID) (Lease, error)

	// RequestMerge RECORDS a workspace's merge request in its target
	// repository's durable queue, in the requested state (in nobody's line),
	// refusing a workspace already requested, queued or admitted there.
	RequestMerge(ctx context.Context, repo RepoKey, id WorkspaceID, source MergeSource, at time.Time) error
	// RequestOneShotMerge records RequestMerge's request for the requester's
	// own branch, in place, and marks the requester a one-shot on its creation
	// job, in one transaction. Any other source is refused, and so is a
	// requester with no creation job.
	RequestOneShotMerge(ctx context.Context, repo RepoKey, id WorkspaceID, source MergeSource, at time.Time) error
	// MergeWorkspaceFor loads the merge workspace forked for one detached
	// request; false when none is registered for it yet.
	MergeWorkspaceFor(ctx context.Context, request DetachedMergeID) (Workspace, bool, error)
	// HandOffDetachedMerge moves a detached request from the requester's queue
	// row onto the merge workspace forked for it, in one transaction.
	HandOffDetachedMerge(ctx context.Context, repo RepoKey, requester WorkspaceID, request DetachedMergeID, mergeWS WorkspaceID, branch, pick string, at time.Time) error
	// ReservedMergeNames answers every merge workspace name a standing async
	// request has reserved, mapped to the workspace that reserved it.
	ReservedMergeNames(ctx context.Context) (map[string]WorkspaceID, error)
	// StartMergePick records that a merge workspace's pick began as a turn,
	// with its bubble's ledger identity, before the turn is submitted.
	StartMergePick(ctx context.Context, repo RepoKey, id WorkspaceID, ledger LeaseID, turn TurnID, at time.Time) error
	// ClearMergePick records that a queued merge workspace's begun pick
	// succeeded: its instruction leaves the row, so its next admission
	// admits the merge itself.
	ClearMergePick(ctx context.Context, repo RepoKey, id WorkspaceID, at time.Time) error
	// RecordMergeWorkspaceRemoved records that the inactivity sweep is
	// removing a merge workspace, before its records are deleted.
	RecordMergeWorkspaceRemoved(ctx context.Context, ws WorkspaceID, requester WorkspaceID, at time.Time) error
	// MergeWorkspaceRemovedAt answers when the sweep removed a merge
	// workspace; false when no removal is recorded for it.
	MergeWorkspaceRemovedAt(ctx context.Context, ws WorkspaceID) (time.Time, bool, error)
	// QueueMerge moves a requested merge into line, at the back, with its
	// bubble's ledger identity, and returns its one-based place among the
	// entries in line.
	QueueMerge(ctx context.Context, repo RepoKey, id WorkspaceID, ledger LeaseID) (int, error)
	// YieldMergeHead records a race to land the head lost: winner takes the
	// head's place at the front and loser waits right behind it.
	YieldMergeHead(ctx context.Context, repo RepoKey, loser, winner WorkspaceID) error
	// MoveMerge moves a waiting merge one place up or down its queue, past
	// the nearest waiting merge on that side, answering whether it moved.
	MoveMerge(ctx context.Context, repo RepoKey, id WorkspaceID, move MergeMove) (bool, error)
	// MoveMergeTo moves a waiting merge directly ahead of or behind another
	// waiting merge on its queue, answering whether it moved.
	MoveMergeTo(ctx context.Context, repo RepoKey, id WorkspaceID, anchor WorkspaceID, side MergeSide) (bool, error)
	// PrioritizeMerge moves a waiting merge to the front of the merges
	// waiting in its queue, so it runs next, answering whether it moved.
	PrioritizeMerge(ctx context.Context, repo RepoKey, id WorkspaceID) (bool, error)
	// AdmitMerge marks the entry the orchestrator is running now.
	AdmitMerge(ctx context.Context, repo RepoKey, id WorkspaceID) error
	// ReturnAdmittedMerge puts an admitted merge with no run back in line at
	// its own place, waiting, under its ledger identity.
	ReturnAdmittedMerge(ctx context.Context, repo RepoKey, id WorkspaceID, ledger LeaseID) error
	// RemoveMergeQueueEntry drops one entry with the cause it was dropped for.
	RemoveMergeQueueEntry(ctx context.Context, repo RepoKey, id WorkspaceID, cause string) error
	// DropAdmittedMerge drops an admitted merge with no run: its entry, its
	// merge lease and that lease's progress record, in one transaction.
	DropAdmittedMerge(ctx context.Context, repo RepoKey, id WorkspaceID, lease LeaseID, cause string) error
	// MergeQueue loads one repository's queue in order, all-or-nothing.
	MergeQueue(ctx context.Context, repo RepoKey) ([]MergeQueueEntry, error)
	// AllMergeQueues loads every repository's queue for the boot re-enqueue,
	// all-or-nothing.
	AllMergeQueues(ctx context.Context) (map[RepoKey][]MergeQueueEntry, error)
	// SetMergeQueuePaused pauses one repository's queue with its record, or
	// (a nil pause) resumes it.
	SetMergeQueuePaused(ctx context.Context, repo RepoKey, pause *MergeQueuePause) error
	// MergeQueuePaused reports whether one repository's queue is paused.
	MergeQueuePaused(ctx context.Context, repo RepoKey) (bool, error)
	// MergeQueuePause answers one repository's standing pause, nil while its
	// queue runs.
	MergeQueuePause(ctx context.Context, repo RepoKey) (*MergeQueuePause, error)
	// SetDeployHold holds one repository's deploy stage from at, or (held
	// false) ends the hold and clears its skip count.
	SetDeployHold(ctx context.Context, repo RepoKey, held bool, at time.Time) error
	// RecordDeploySkip counts one deploy that skipped its deploy hook while
	// the repository was held.
	RecordDeploySkip(ctx context.Context, repo RepoKey) error
	// DeployHoldOf reports one repository's deploy hold.
	DeployHoldOf(ctx context.Context, repo RepoKey) (DeployHold, error)
	// SetMergeRacingDisabled disables the merge queue's racing daemon-wide
	// from at, or (disabled false) enables it again.
	SetMergeRacingDisabled(ctx context.Context, disabled bool, at time.Time) error
	// MergeRacing reports the daemon-wide racing switch.
	MergeRacing(ctx context.Context) (RacingSwitch, error)
	// SubscribeMergeQueue records a merge queue subscription, replacing the
	// detail of the one its subscriber holds in the same scope, and answers
	// whether it replaced one. A closed subscriber is ErrSubscriberClosed.
	SubscribeMergeQueue(ctx context.Context, sub MergeQueueSubscription) (bool, error)
	// UnsubscribeMergeQueue ends a subscriber's subscription in one scope,
	// answering whether one stood there.
	UnsubscribeMergeQueue(ctx context.Context, subscriber WorkspaceID, repo RepoKey) (bool, error)
	// MergeQueueSubscriptions loads every merge queue subscription.
	MergeQueueSubscriptions(ctx context.Context) ([]MergeQueueSubscription, error)
	// UntoldMergeNotice answers a subscriber's untold notice of one queue
	// (mergenotices.go), and whether one is recorded.
	UntoldMergeNotice(ctx context.Context, subscriber WorkspaceID, repo RepoKey) (UntoldMergeNotice, bool, error)
	// RecordUntoldMergeNotice records a subscriber's untold notice of one
	// queue, replacing the one recorded before.
	RecordUntoldMergeNotice(ctx context.Context, n UntoldMergeNotice) error
	// UntoldWorkspaceNotice answers a subscriber's untold notice about one
	// followed workspace (workspacenotices.go), and whether one is recorded.
	UntoldWorkspaceNotice(ctx context.Context, subscriber, followed WorkspaceID) (UntoldWorkspaceNotice, bool, error)
	// RecordUntoldWorkspaceNotice records a subscriber's untold notice about
	// one followed workspace, replacing the one recorded before.
	RecordUntoldWorkspaceNotice(ctx context.Context, n UntoldWorkspaceNotice) error
	// SetLastKeepalive records when a workspace's keep-alive was last sent
	// (lastkeepalive.go).
	SetLastKeepalive(ctx context.Context, id WorkspaceID, at time.Time) error
	// MarkCloseWhenTurnEnds marks a workspace to close when its running turn
	// ends (selfclose.go).
	MarkCloseWhenTurnEnds(ctx context.Context, id WorkspaceID, at time.Time) error
	// TakeCloseWhenTurnEnds clears a workspace's self-close mark in one
	// statement, answering whether one stood.
	TakeCloseWhenTurnEnds(ctx context.Context, id WorkspaceID) (bool, error)
	// SetAwaiting marks a workspace awaiting correspondence from another, or
	// clears the mark with nil (standings.go).
	SetAwaiting(ctx context.Context, id WorkspaceID, awaited *WorkspaceID) error
	// SetPrincipalLink records a workspace's principal link, or clears it
	// with nil (principallink.go).
	SetPrincipalLink(ctx context.Context, id WorkspaceID, link *string) error
	// SetName records a workspace's new name, leaving its branch and
	// directory as they are (rename.go). An empty name is refused.
	SetName(ctx context.Context, id WorkspaceID, name string) error
	// SetOneShot marks a workspace's creation a one-shot or unmarks it; a
	// workspace with no creation job is ErrNotFound (standings.go).
	SetOneShot(ctx context.Context, id WorkspaceID, oneShot bool) error
	// SubscribeWorkspace records a workspace subscription, replacing the
	// events of one its subscriber holds to the same workspace, and answers
	// whether it replaced one (workspacesubscriptions.go).
	SubscribeWorkspace(ctx context.Context, sub WorkspaceSubscription) (bool, error)
	// UnsubscribeWorkspace ends a subscriber's subscription to a workspace,
	// answering whether one stood.
	UnsubscribeWorkspace(ctx context.Context, subscriber, followed WorkspaceID) (bool, error)
	// WorkspaceSubscribers loads every subscription following a workspace.
	WorkspaceSubscribers(ctx context.Context, followed WorkspaceID) ([]WorkspaceSubscription, error)

	// OpenFault records a fault and returns its id.
	OpenFault(ctx context.Context, f Fault) (FaultID, error)
	// CloseFault stamps a fault's persisted resolved-at.
	CloseFault(ctx context.Context, id FaultID, at time.Time) error
	// OpenFaults loads the open faults matching scope, all-or-nothing.
	OpenFaults(ctx context.Context, scope FaultScope) ([]Fault, error)
	// Fault loads one fault by id, open or resolved — the read that proves a
	// closing edge was persisted rather than reopening on the next boot.
	Fault(ctx context.Context, id FaultID) (Fault, error)
	// FaultRecorded reports whether any fault matching m was ever recorded,
	// open or resolved — the read that keeps a verdict replayed from history
	// from being raised afresh on every boot.
	FaultRecorded(ctx context.Context, m FaultMatch) (bool, error)

	// PutDrainSchedule puts a drain schedule in force, replacing any current
	// one.
	PutDrainSchedule(ctx context.Context, s DrainSchedule) error
	// ClearDrainSchedule cancels the schedule in force.
	ClearDrainSchedule(ctx context.Context) error
	// DrainSchedule loads the schedule in force, nil when none is.
	DrainSchedule(ctx context.Context) (*DrainSchedule, error)

	// PutDurableFeedRow records (or replaces) one daemon-synthesized feed row a
	// new daemon must draw again.
	PutDurableFeedRow(ctx context.Context, row DurableFeedRow) error
	// DurableFeedRows loads every durable feed row of one workspace.
	DurableFeedRows(ctx context.Context, id WorkspaceID) ([]DurableFeedRow, error)
	// ClearDurableFeedRows drops every durable feed row of one workspace.
	ClearDurableFeedRows(ctx context.Context, id WorkspaceID) error

	// PutFeedTextScale persists the single daemon-global feed text zoom.
	PutFeedTextScale(ctx context.Context, scale float64) error
	// FeedTextScale loads the persisted feed text zoom, or DefaultFeedTextScale
	// when none is set.
	FeedTextScale(ctx context.Context) (float64, error)

	// PutAccountDefaultModel records the model new sessions on one account
	// root start under when nobody names one.
	PutAccountDefaultModel(ctx context.Context, configDir, model string) error
	// AccountDefaultModel loads the default chosen for one account root,
	// false when nobody chose one.
	AccountDefaultModel(ctx context.Context, configDir string) (string, bool, error)

	// RecordRolledBackTurns records that turns were rolled back, all or
	// nothing; the feed never draws them again.
	RecordRolledBackTurns(ctx context.Context, id WorkspaceID, turns []TurnID) error
	// RolledBackTurns loads every rolled-back turn of a workspace.
	RolledBackTurns(ctx context.Context, id WorkspaceID) ([]TurnID, error)
	// RecordSignedInTurns records that a sign-in answered the account failure
	// each turn ended on, all or nothing; the feed never draws that failure
	// again.
	RecordSignedInTurns(ctx context.Context, id WorkspaceID, turns []TurnID) error
	// SignedInTurns loads every turn of a workspace whose account failure a
	// sign-in answered.
	SignedInTurns(ctx context.Context, id WorkspaceID) ([]TurnID, error)
	// RecordReconnectedTurns records that the network coming back answered
	// the connection failure each of a workspace's turns ended on, all or
	// nothing.
	RecordReconnectedTurns(ctx context.Context, id WorkspaceID, turns []TurnID) error
	// ReconnectedTurns loads every turn of a workspace whose connection
	// failure the network coming back answered.
	ReconnectedTurns(ctx context.Context, id WorkspaceID) ([]TurnID, error)
	// NewsDigestState loads the daily news digest's durable state: the
	// cadence's origin, the next digest's baseline, the newest digest minted
	// and whether it stands, and every source's snapshot.
	NewsDigestState(ctx context.Context) (NewsDigestState, error)
	// RecordNewsDigestRun records one finished news digest run, whole.
	RecordNewsDigestRun(ctx context.Context, run NewsDigestRun) error
	// NewsDigestRisksSince loads every kept digest item marked as a regression
	// risk whose run ended at or after since, oldest first.
	NewsDigestRisksSince(ctx context.Context, since time.Time) ([]NewsDigestRisk, error)
	// DismissNewsDigest takes the standing digest down when id names the
	// newest digest minted (true, also when already down); any other id is
	// false and changes nothing.
	DismissNewsDigest(ctx context.Context, id string) (bool, error)
	// RestandNewsDigest stands the newest digest minted again, from the
	// overlay kept beside it, when it is down; it answers whether it stood it.
	RestandNewsDigest(ctx context.Context, id string) (bool, error)
	// NoteEditorInstance records the Emacs process identity a WatchDaemon
	// carried, answering true when it differs from the last one recorded (a
	// full Emacs restart) and false for the same one (a reconnect).
	NoteEditorInstance(ctx context.Context, instance string, at time.Time) (bool, error)
	// AgentReplSession loads agent-repl's session, reporting false when none
	// has begun.
	AgentReplSession(ctx context.Context) (AgentReplSession, bool, error)
	// PutAgentReplSession replaces agent-repl's session whole.
	PutAgentReplSession(ctx context.Context, session AgentReplSession) error

	// Turn answers one recorded turn whole; ErrNotFound for a turn the
	// workspace never recorded.
	Turn(ctx context.Context, id WorkspaceID, turn TurnID) (Turn, error)

	// ClaimServing records this daemon instance as the workspace's serving
	// owner — the handover's per-workspace transfer.
	ClaimServing(ctx context.Context, id WorkspaceID, daemon InstanceID) error
	// ClaimUnownedServing claims serving ownership only when no other
	// instance holds it, answering whether the claim stood and, when it did
	// not, who holds the workspace. It is the arbitration between an
	// incumbent reclaiming a workspace and the successor adopting it.
	ClaimUnownedServing(ctx context.Context, id WorkspaceID, daemon InstanceID) (bool, InstanceID, error)
	// Serving reports which daemon instance serves a workspace, nil when none
	// does.
	Serving(ctx context.Context, id WorkspaceID) (*InstanceID, error)
	// ReleaseServing gives up serving ownership, refusing when this instance
	// does not hold it.
	ReleaseServing(ctx context.Context, id WorkspaceID, daemon InstanceID) error
}
