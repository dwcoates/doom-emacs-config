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
	// minting a WorkspaceID and a RepoID on first sight. The bool reports
	// whether the record was created.
	RegisterWorkspace(ctx context.Context, dir string, facts RegisterFacts) (Workspace, bool, error)
	// Workspace loads one workspace by id.
	Workspace(ctx context.Context, id WorkspaceID) (Workspace, error)
	// WorkspaceByDir loads one workspace by its normalized worktree directory.
	WorkspaceByDir(ctx context.Context, dir string) (Workspace, error)
	// ListWorkspaces loads every workspace, all-or-nothing.
	ListWorkspaces(ctx context.Context) ([]Workspace, error)
	// RegisterRepository records a repository ON ITS OWN, with no workspace,
	// idempotent by normalized main-worktree dir. It mints the RepoID on first
	// sight; the bool reports whether the record was created. It is the same
	// mint RegisterWorkspace performs through ensureRepo, reachable without a
	// workspace to hang it on.
	RegisterRepository(ctx context.Context, dir, defaultBranch string) (Repository, bool, error)
	// RefuseTemporary answers the *tempdirs.InsideError RegisterWorkspace and
	// RegisterRepository would refuse dir with, or nil: the SAME check, for a
	// verb that must refuse a temporary directory before it registers
	// anything (CreateWorkspace's repository). A dir that cannot be judged is
	// an ordinary error.
	RefuseTemporary(dir string) error
	// ListRepositories loads every repository, all-or-nothing.
	ListRepositories(ctx context.Context) ([]Repository, error)
	// SetRepositoryFolded records whether a repository's roster section is
	// collapsed; an unknown repository is ErrNotFound.
	SetRepositoryFolded(ctx context.Context, id RepoID, folded bool) error
	// SetClosed records whether a workspace's editor state is torn down.
	SetClosed(ctx context.Context, id WorkspaceID, closed bool) error
	// SetCurrent records the user's selection of a workspace at an instant.
	SetCurrent(ctx context.Context, id WorkspaceID, at time.Time) error
	// Current reports the currently selected workspace, nil when none is.
	Current(ctx context.Context) (*WorkspaceID, error)
	// SetPriority sets or clears a workspace's roster priority.
	SetPriority(ctx context.Context, id WorkspaceID, p *Priority) error
	// SetAttention sets or clears the roster's attention marker.
	SetAttention(ctx context.Context, id WorkspaceID, on bool) error
	// SetResult records, or clears with nil, the roster's last turn result.
	SetResult(ctx context.Context, id WorkspaceID, result *TurnResult) error
	// SetMergedAt records that the workspace's merge landed.
	SetMergedAt(ctx context.Context, id WorkspaceID, at time.Time) error
	// Forget deletes a workspace's every record — the nuke's durable half and
	// the whole of the forget verb — and, when the workspace was the last one
	// registered under its repository, that repository's record too. The
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
	// SetSessionTerminal records a session's death with its cause. A deleted
	// session refuses resurrection.
	SetSessionTerminal(ctx context.Context, id WorkspaceID, t SessionTerminal) error
	// SetVendorSessionID records the vendor session id a later resume names
	// (the id in force after a rotation), answering the one it replaced.
	SetVendorSessionID(ctx context.Context, id WorkspaceID, vendorSessionID string) (string, error)
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
	// SetHeldPromptAccepted records the user's acceptance of the tray's offer to
	// let the prompt wait for the turn's end. Legal ONLY on a hold_for_turn_end
	// verdict.
	SetHeldPromptAccepted(ctx context.Context, turn TurnID) error
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
	// PutPortedPrompts writes a fork's whole ported conversation in one
	// transaction: the parent's prompt rows, re-minted under the child's own
	// turn identities. All or nothing.
	PutPortedPrompts(ctx context.Context, id WorkspaceID, rows []PortedPrompt) error
	// PortedPrompts loads one workspace's ported conversation, oldest first.
	PortedPrompts(ctx context.Context, id WorkspaceID) ([]PortedPrompt, error)
	// ConversationPrompts is what a FORK of this workspace inherits: what this
	// workspace itself inherited followed by every prompt of its own, in one
	// order with contiguous ordinals.
	ConversationPrompts(ctx context.Context, id WorkspaceID) ([]PortedPrompt, error)
	// RecentConversationPrompts is a BOUNDED ConversationPrompts: the same rows,
	// truncated to the most recent `limit`, oldest first, read with a SQL LIMIT
	// on each table rather than a whole-history load. It is for a reader that
	// only ever needs the newest rows, such as the fork-naming digest; it never
	// changes what a fork inherits, which still reads ConversationPrompts.
	RecentConversationPrompts(ctx context.Context, id WorkspaceID, limit int) ([]PortedPrompt, error)
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
	// SetTaskFolded records whether a task's roster section is collapsed; an
	// unknown task is ErrNotFound.
	SetTaskFolded(ctx context.Context, id TaskID, folded bool) error
	// SetMergedSectionFolded records whether the recently-merged band is
	// collapsed.
	SetMergedSectionFolded(ctx context.Context, folded bool) error
	// SetGrouping records which grouping every page shows.
	SetGrouping(ctx context.Context, grouping Grouping) error
	// SidebarView loads the sidebar's view state that belongs to no row, or
	// DefaultSidebarView when nobody has changed it.
	SidebarView(ctx context.Context) (SidebarView, error)
	// SetAccountUsage records an account root's last usage evidence,
	// replacing what the root had.
	SetAccountUsage(ctx context.Context, usage AccountUsage) error
	// AccountUsages loads every account root's last usage evidence,
	// all-or-nothing.
	AccountUsages(ctx context.Context) ([]AccountUsage, error)
	// AssignWorkspaceTask assigns a workspace to a task, or unassigns it when
	// task is nil.
	AssignWorkspaceTask(ctx context.Context, id WorkspaceID, task *TaskID) error

	// OpenMergeLedger opens a workspace's ledger for one merge lease.
	OpenMergeLedger(ctx context.Context, id WorkspaceID, lease LeaseID) error
	// RecordTabInterval appends one round's tab interval to a lease's ledger.
	RecordTabInterval(ctx context.Context, lease LeaseID, interval TabInterval) error
	// MergeLedger loads a workspace's ledger entries, all-or-nothing.
	MergeLedger(ctx context.Context, id WorkspaceID) ([]MergeLedgerEntry, error)

	// RequestMerge RECORDS a workspace's merge request in its target
	// repository's durable queue, in the requested state (in nobody's line),
	// refusing a workspace already requested, queued or admitted there.
	RequestMerge(ctx context.Context, repo RepoKey, id WorkspaceID, source MergeSource, at time.Time) error
	// QueueMerge moves a requested merge into line, at the back, and returns
	// its one-based place among the entries in line.
	QueueMerge(ctx context.Context, repo RepoKey, id WorkspaceID) (int, error)
	// AdmitMerge marks the entry the orchestrator is running now.
	AdmitMerge(ctx context.Context, repo RepoKey, id WorkspaceID) error
	// RemoveMergeQueueEntry drops one entry with the cause it was dropped for.
	RemoveMergeQueueEntry(ctx context.Context, repo RepoKey, id WorkspaceID, cause string) error
	// MergeQueue loads one repository's queue in order, all-or-nothing.
	MergeQueue(ctx context.Context, repo RepoKey) ([]MergeQueueEntry, error)
	// AllMergeQueues loads every repository's queue for the boot re-enqueue,
	// all-or-nothing.
	AllMergeQueues(ctx context.Context) (map[RepoKey][]MergeQueueEntry, error)
	// SetMergeQueuePaused pauses or resumes one repository's queue.
	SetMergeQueuePaused(ctx context.Context, repo RepoKey, paused bool) error
	// MergeQueuePaused reports whether one repository's queue is paused.
	MergeQueuePaused(ctx context.Context, repo RepoKey) (bool, error)

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

	// RecordRolledBackTurns records that turns were rolled back, all or
	// nothing; the feed never draws them again.
	RecordRolledBackTurns(ctx context.Context, id WorkspaceID, turns []TurnID) error
	// RolledBackTurns loads every rolled-back turn of a workspace.
	RolledBackTurns(ctx context.Context, id WorkspaceID) ([]TurnID, error)
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

	// TurnStartedAt answers when a recorded turn was opened; ErrNotFound for a
	// turn the workspace never recorded.
	TurnStartedAt(ctx context.Context, id WorkspaceID, turn TurnID) (time.Time, error)

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
