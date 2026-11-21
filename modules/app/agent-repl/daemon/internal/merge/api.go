// Package merge is the deploy queue (still spelled "merge" in code and on the
// wire): a per-repo queue, the stages a repository's deploy config names, tabs,
// the lease, the test gate, the briefs and the ledger.
//
// A DEPLOY RUNS IN THE WORKSPACE THAT ASKED FOR IT (owner, 2026-09-30). No
// workspace is ever created for one: the requesting workspace's feed carries
// the bubble, its footer and roster row carry the status, and its own session
// runs the prompt hooks, the conflict resolution and the test fixing. What it
// deploys is the request's SOURCE: its own branch, another workspace's, or a
// branch that is no workspace.
//
// WHAT A DEPLOY DOES IS THE REPOSITORY'S. Its `.agent-repl/lifecycle.yaml`
// (repoconfig.v1.LifecycleConfig, read at every use by lifecycleconfig) names three
// stages: pre-deploy hooks and, optionally, the built-in local merge (rebase
// with conflict resolution, tests with fixing, the non-fast-forward merge
// commit and its version tag); the deploy hook, which a repository's deploy
// hold skips; and the post-deploy hooks. Every hook is a script, run from the
// repository's main checkout root, or a prompt, run by the deploying
// workspace's own session. Any failure FAILS the deploy and hands the
// workspace back to investigate; nothing parks. The displaced user turn is
// captured durably and resubmitted EXACTLY ONCE at lease release. See
// docs/protobuf-design/deploy-queue.md.
package merge

import (
	"context"
	"fmt"
	"strings"
	"time"

	"agent-repld/internal/desktopnotify"
	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/gitclient"
	"agent-repld/internal/ids"
	"agent-repld/internal/lifecycleconfig"
	"agent-repld/internal/paint"
	"agent-repld/internal/promptqueue"
	"agent-repld/internal/prompts"
	"agent-repld/internal/resolve/feed"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/scriptrunner"
	"agent-repld/internal/testsclient"
	"agent-repld/internal/wsm"
)

// MergeFacts is what the footer and the sidebar read back. It aliases the
// footer's spelling so the two surfaces cannot disagree about a merge.
type MergeFacts = footer.MergeFacts

// The brief names the orchestrator reads from the prompts directory at use
// time. A missing brief is LOUD: the round refuses rather than sending an
// empty prompt.
const (
	// BriefConflictResolve is the conflict-resolution agent's brief.
	BriefConflictResolve = "merge-conflict-resolve"
	// BriefTestFailureResolve is the test-failure-resolution agent's brief.
	BriefTestFailureResolve = "merge-test-failure-resolve"
	// BriefFixCommit asks a fixing turn that ended with uncommitted changes
	// to commit them before the merge goes on.
	BriefFixCommit = "merge-fix-commit"
	// BriefRebaseCommit asks the session of a merge whose rebase met
	// uncommitted changes in its worktree to commit them before the rebase
	// starts again.
	BriefRebaseCommit = "merge-rebase-commit"
	// BriefHookPrompt is what a deploy stage's prompt hook submits: the
	// configured prompt, and the step that stores a pull request the turn
	// opens as the workspace's owned link.
	BriefHookPrompt = "merge-hook-prompt"
)

// MaxCommitPrompts bounds how many times a fixing turn that ended with
// uncommitted changes is prompted to commit them. A worktree still dirty after
// the last prompt fails the merge: the agent was asked and did not, and the
// merge never rebases over work it would lose.
const MaxCommitPrompts = 2

// QueueOfferAnswer is the user's answer to the merge-queue question
// (agentrepl.v1.AnswerMergeQueueOfferRequest's decision arms).
type QueueOfferAnswer int

// The answers. The zero value is no answer, so an answer left unset is never
// read as one of them.
const (
	// QueueOfferEvict takes the merge out of the queue.
	QueueOfferEvict QueueOfferAnswer = iota + 1
	// QueueOfferPrioritize moves the merge to the front of the merges waiting.
	QueueOfferPrioritize
	// QueueOfferCancel leaves the queue as it is.
	QueueOfferCancel
)

// String names the answer in log records.
func (a QueueOfferAnswer) String() string {
	switch a {
	case QueueOfferEvict:
		return "evict"
	case QueueOfferPrioritize:
		return "prioritize"
	case QueueOfferCancel:
		return "cancel"
	}
	return fmt.Sprintf("QueueOfferAnswer(%d)", int(a))
}

// Orchestrator is the merge queue's whole surface.
type Orchestrator interface {
	// RecordDirectLanding records a landing land-direct.sh made on a
	// repository's main branch, classified by the daemon's own state, and
	// answers the path recorded (landings.go).
	RecordDirectLanding(ctx context.Context, report DirectLanding) (wsm.LandingPath, error)
	// Landings answers the scoped repository's landings in [since, until],
	// the queue's apart from every other (landings.go).
	Landings(ctx context.Context, scope RepositoryScope, since, until time.Time) (LandingsWindow, error)
	// HoldTargetForSync holds the target of the repository holding DIR for
	// a main sync (synchold.go): held only while no merge of the repository
	// is in line or running, and admitting none until the answered release
	// is called.
	HoldTargetForSync(ctx context.Context, dir string) (release func(), held bool, err error)
	// Enqueue records a merge request. It REFUSES pre-state: no layout facts
	// recorded at creation, a deleted session, a source workspace or branch
	// that is not there, or a workspace already requested, queued or merging.
	// The request is DURABLE from here, and it is put in line -- and reported
	// to any client -- only once the turn that asked for it has ended (a
	// merge the USER asked for over an rpc has no requesting turn and is put
	// in line at once). It answers what the request reserved.
	Enqueue(ctx context.Context, req Request) (Enqueued, error)
	// Pause stops the queue from starting new merges; an in-flight merge runs
	// on. A nil scope is the daemon-wide switch (UpdateMergeQueuePause with an
	// UNSET repository); a scope names ONE repository's queue.
	// BY is who asked, recorded on the pause (frontend.v1.RosterMergeQueuePaused).
	Pause(ctx context.Context, scope *RepositoryScope, by Pauser) error
	// HardStop pauses as Pause does, scoped the same, and stops the work the
	// queues' merges are doing now: the turn of each merge suspended at a
	// fixing attempt or a conflict resolution, and of each waiting merge's
	// background rebase, is interrupted (UpdateMergeQueuePause.hard).
	HardStop(ctx context.Context, scope *RepositoryScope, by Pauser) error
	// Unpause resumes starting merges, with the same scoping as Pause.
	Unpause(ctx context.Context, scope *RepositoryScope) error
	// ResumeOwnPause resumes the one queue SCOPE names only when the pause
	// standing on it is workspace WS's own AND was made to land a change,
	// answering whether it did: the queue bypass holder lets its own landing
	// pause through at its turn's end, and never a pause the owner asked for,
	// the user's, the host's, or another workspace's.
	ResumeOwnPause(ctx context.Context, scope RepositoryScope, ws ids.WorkspaceID) (bool, error)
	// Status answers each registered repository's queue as it stands, its
	// started merges' steps filled in: the one repository a scope names (an
	// unknown one refused as unknown_repository), or every registered one for
	// a nil scope.
	Status(ctx context.Context, scope *RepositoryScope) ([]QueueStatus, error)
	// HoldDeploys holds the deploy stage of the repository SCOPE names,
	// durably: its deploys keep running their pre- and
	// post-deploy stages and skip their deploy hook. A hold of a held
	// repository is refused (already_held), and of an unregistered one
	// (unknown_repository).
	HoldDeploys(ctx context.Context, scope RepositoryScope) error
	// ReleaseDeploys ends the hold of the repository SCOPE names, and runs its deploy hook once when a
	// deploy skipped it and the hook is a script. A release of a repository
	// that is not held is refused (not_held).
	ReleaseDeploys(ctx context.Context, scope RepositoryScope) (DeployRelease, error)
	// RollOutLanding is a deploy hook's ask for the rollout of what its deploy
	// landed: registered on the running deploy LEDGER names, fired at its
	// teardown, and answered true; with no ledger, started at once and
	// answered false. A ledger naming no running deploy is refused
	// (unknown_deploy).
	RollOutLanding(ctx context.Context, ledger ids.LeaseID) (bool, error)
	// Evict removes a workspace's merge from the queue. A merge that is
	// already RUNNING is ABANDONED: it is ended through the one release path,
	// which gives back its lease, its queue entry, its ledger interval and its
	// repository's slot, and the call returns once that is done.
	Evict(ctx context.Context, ws ids.WorkspaceID) error
	// GiveUp ends WS's merge at its own agent's word, from a repair turn the
	// queue gave it, saying REASON (giveup.go).
	GiveUp(ctx context.Context, ws ids.WorkspaceID, reason string) error
	// Requeue puts a merge that left its queue without landing back in line
	// as itself: the same workspace, branch and request (offqueue.go).
	Requeue(ctx context.Context, req Requeue) (Requeued, error)
	// Discard ends a request standing off its queue without landing it;
	// CLOSE closes an async merge's merge workspace (offqueue.go).
	Discard(ctx context.Context, ws ids.WorkspaceID, close func(context.Context) error) (Discarded, error)
	// SetGithubDeployments installs, WHOLE, every workspace whose owned pull
	// request stands in a GitHub merge queue (githubdeploy.go): each is drawn
	// as deploying, and at its GitHub place in its repository's merge queue.
	// The pull-request watcher tells it after every poll.
	SetGithubDeployments(ctx context.Context, deployments []GithubDeployment)
	// Subscribe records SUBSCRIBER's subscription to the queue of the
	// repository SCOPE names (every repository's for nil), standing in for
	// the one it holds in that scope, and answers whether it replaced one. A
	// closed subscriber is refused (ArmSubscriberClosed), as is an unknown
	// repository.
	Subscribe(ctx context.Context, subscriber ids.WorkspaceID, scope *RepositoryScope) (bool, error)
	// LeaseChanged is told each lease change of a workspace: a queue front
	// that waited for that workspace's lease is offered to its queue again.
	LeaseChanged(ws ids.WorkspaceID)
	// TakeRequestingTurn reports whether turn of ws is the turn that asked
	// for a merge of ws, and forgets it. The merge queue's own banners
	// announce that merge, so the turn's end raises none.
	TakeRequestingTurn(ws ids.WorkspaceID, turn ids.TurnID) bool
	// Relabel is told WS was renamed: every queue WS stands in is
	// republished, so its entry's label and the line naming the merge ahead
	// read the new name. A workspace on no queue is no change.
	Relabel(ctx context.Context, ws ids.WorkspaceID) error
	// Unsubscribe ends SUBSCRIBER's subscription in SCOPE, and only there. A
	// subscriber holding none there is refused (ArmNotSubscribed).
	Unsubscribe(ctx context.Context, subscriber ids.WorkspaceID, scope *RepositoryScope) error
	// Move moves a workspace's waiting merge one place up or down its queue,
	// past the nearest waiting merge on that side. A move that moves nothing
	// -- no waiting merge on that side, or a merge not waiting (running, or
	// only requested) -- is refused with ArmMoveBlocked, naming why.
	Move(ctx context.Context, ws ids.WorkspaceID, move wsm.MergeMove) error
	// MoveTo moves WS's waiting merge directly to SIDE of ANCHOR's waiting
	// merge on the same queue (UpdateMergeQueueMoveTo), answering whether it
	// moved. A merge or an anchor not waiting is left where it is; an anchor
	// with no merge on WS's queue is the no_such_queued_merge refusal.
	MoveTo(ctx context.Context, ws, anchor ids.WorkspaceID, side wsm.MergeSide) (bool, error)
	// Place moves WS's waiting merge to PLACE among the merges waiting on its
	// queue -- front, back, behind a merge in line named as the queue lists
	// it, or a waiting index -- in one store transaction
	// (UpdateMergeQueueMove), answering whether it moved. A workspace with no
	// waiting merge is no_such_queued_merge, a name in no merge in line
	// placement_behind_not_in_line, and an index past the last waiting place
	// index_out_of_range; a refused move moves nothing.
	Place(ctx context.Context, ws ids.WorkspaceID, place MovePlace) (bool, error)
	// AnswerQueueOffer answers the merge-queue question an interrupt raised:
	// evict the merge (a running one is abandoned, as Evict does), move it to
	// the front of the merges waiting in its queue, or leave the queue as it
	// is. Every answer retires the question; with none standing it is refused
	// (no_offer_standing).
	AnswerQueueOffer(ctx context.Context, ws ids.WorkspaceID, answer QueueOfferAnswer) error
	// OnInterrupt raises the merge-queue question in the PINNED bubble of a
	// workspace whose sync merge is queued, answering whether it did. It
	// changes nothing on the queue.
	OnInterrupt(ctx context.Context, ws ids.WorkspaceID) bool
	// OnWorkspaceClosed abandons a workspace's merge when the workspace
	// itself is torn down (killed or nuked), recording the close as the
	// abandon cause: a request or a waiting merge leaves its queue, and a
	// running one is ended through the one release path. It is a no-op for a
	// workspace with no merge.
	OnWorkspaceClosed(ctx context.Context, ws ids.WorkspaceID)
	// ColdGateRaised is told WS's session parked at its cold gate: its merge
	// keeps its place in line and is skipped until the gate is passed, and a
	// merge being worked on is put back at its place (coldgate.go). It
	// returns at once; the edges of every gate are applied in the order they
	// were told.
	ColdGateRaised(ws ids.WorkspaceID)
	// ColdGateRetired is told WS's recorded cold gate was retired (a session
	// came up): its merge resumes at its place in line.
	ColdGateRetired(ws ids.WorkspaceID)
	// OnHidden is told WS was hidden: a merge standing at its cold gate
	// leaves its queue (CauseHiddenAtColdGate). It returns at once.
	OnHidden(ws ids.WorkspaceID)
	// RevealMergeWorkspace draws a hidden async merge workspace in the tab bar
	// and the sidebar from now on, naming CAUSE in its record, and restates the
	// requester's bubble head with its switch. It is a no-op for a workspace
	// that is no merge workspace or is already revealed.
	RevealMergeWorkspace(ctx context.Context, ws ids.WorkspaceID, cause string) error
	// WorkspaceLinksChanged draws again the heads of WS's merges, and of the
	// async merges WS asked for, once WS's links changed: a merge's target
	// opens the pull request the workspace owns
	// (frontend.v1.FeedMergeBranchLineTarget.pull_request), which a finish
	// hook stores mid-merge.
	WorkspaceLinksChanged(ws ids.WorkspaceID)
	// InterruptTab stops the turn an AGENTIC merge tab of REQUESTER's feed
	// draws (agentrepl.v1.InterruptRequest.merge_tab): the merge workspace's
	// turn for an async merge, which also reveals that workspace, or the
	// requester's own for a sync merge. It answers whether a turn was running.
	// A ref that names no live agentic tab of a merge drawn in REQUESTER's
	// feed is refused (not_merge_tab).
	InterruptTab(ctx context.Context, requester ids.WorkspaceID, tab feedid.Ref) (bool, error)
	// Facts reports a workspace's merge facts for the footer and the roster;
	// the bool is false when the workspace has no merge.
	Facts(ws ids.WorkspaceID) (MergeFacts, bool)
	// RetireConcluded retires a CONCLUDED merge's standing state (failed or
	// merged) once the workspace has moved on -- the queue accepted a
	// submission of its own. A merge that is queued or running is untouched.
	RetireConcluded(ctx context.Context, ws ids.WorkspaceID)
	// TestLogPath resolves the test log a merge bubble named by token, for
	// that workspace. A token that names no log this daemon holds for the
	// workspace is REFUSED (unknown_merge_test_log).
	TestLogPath(ctx context.Context, ws ids.WorkspaceID, token string) (string, error)
	// Drain stops admitting merges, SUSPENDS every merge still mid-step at a
	// stopping point -- its git command in flight waited out
	// (MergeGitStopBound), no git started after, its turn and test-gate waits
	// cut -- and waits, WITHIN A BOUND (TerminalDrainBound), for every run
	// that has already reached its TERMINAL to finish its durable stamps and
	// its teardown. The daemon's orderly exit calls it with the state client
	// STILL OPEN: without it a SIGTERM landing mid-terminal closed the store
	// under those writes, and merged_at, closed and the lease release were
	// lost to failed transactions.
	Drain(ctx context.Context)
	// OweMediations records that the deploy that started AT owes a check in
	// every repository that checks its deploys (mediation.go).
	OweMediations(ctx context.Context, at time.Time) error
	// ReleaseUnchangedDeploy releases, unchecked, the check the deploy that
	// began AT owes in every repository that checks its deploys, when that
	// deploy changed nothing and the check began its own chain.
	ReleaseUnchangedDeploy(ctx context.Context, at time.Time) error
	// NoteDeployEnding records how the deploy that began at AT ended on
	// every check it owes, for the intake's brief (mediation.go).
	NoteDeployEnding(ctx context.Context, at time.Time, ending wsm.DeployEnding) error
	// FixerTurnEnded settles the check a fixer is dispatched for when a
	// turn of WS, that fixer, ended (fixerwatch.go).
	FixerTurnEnded(ctx context.Context, ws wsm.WorkspaceID)
	// MediatorTurnSpent meters the tokens a turn of WS spent when WS is a
	// deploy mediator, by the phase of its run the turn ended in
	// (mediatormetrics.go).
	MediatorTurnSpent(ctx context.Context, ws wsm.WorkspaceID, spend footer.TurnSpent)
	// Mediate runs an intake round for every check owed, and takes up every
	// remediation finish a restart cut: a deploy
	// finished, or a successor took over.
	Mediate(ctx context.Context)
	// DecideMediation is the chain's intake WS's decision on the deploy its
	// chain owes a check of (mediation.go).
	DecideMediation(ctx context.Context, ws ids.WorkspaceID, d Decision) error
	// IgnoreRecords records KIND as a kind of log record the deploy checks
	// leave out, on the word of deploy mediator WS; UnignoreRecords
	// withdraws the kind of OPERATION whose message holds MESSAGECONTAINS.
	// A workspace that is no repository's deploy mediator is refused
	// (ArmNotMediator).
	IgnoreRecords(ctx context.Context, ws ids.WorkspaceID, kind wsm.IgnoredRecordKind) error
	UnignoreRecords(ctx context.Context, ws ids.WorkspaceID, operation, messageContains string) error
	// ReleaseEscalation releases the hold a repository's escalation puts on
	// its queue (remediationfinish.go).
	ReleaseEscalation(ctx context.Context, scope RepositoryScope) error
	// LandsDirectly answers whether WS's finish lands directly
	// (FinishRemediation): a deploy remediation a fix is dispatched to, or a
	// deploy escalation workspace the daemon made.
	LandsDirectly(ctx context.Context, ws ids.WorkspaceID) (bool, error)
	// FinishRemediation runs remediation workspace WS's finish: the full
	// gate, the direct landing and the deploy, each step recorded before it
	// starts (remediationfinish.go).
	FinishRemediation(ctx context.Context, ws ids.WorkspaceID, version *wsm.VersionIncrement) error
	// FollowStanding holds every queue still while this daemon may not
	// advance it (a handover in flight, or a successor not up yet), and
	// takes the queues on again when it may, until ctx ends.
	FollowStanding(ctx context.Context) error
	// Recover RESUMES every in-flight merge at boot from its progress record
	// (the step it stood on, under the same lease and in the same bubble),
	// runs again from the queue an admitted merge that never took a step,
	// re-arms every request still waiting for its turn to end, and re-enqueues
	// every merge that was queued but not started, in the order it was
	// waiting in. It never silently abandons one.
	Recover(ctx context.Context) error
	// SuspendForTransfer brings a workspace's merge to a stopping point for a
	// handover's transfer and stops this daemon admitting any merge of it;
	// the successor resumes it (AdoptWorkspace).
	SuspendForTransfer(ctx context.Context, ws ids.WorkspaceID) error
	// AdoptWorkspace takes every merge of a workspace this daemon now serves
	// from its durable rows, resuming an admitted one at its recorded step.
	AdoptWorkspace(ctx context.Context, ws ids.WorkspaceID) error
	// ReturnPausedMerges puts every merge of WS a handover paused back at
	// its place in line, dropping its merge lease and progress: WS waits on
	// this daemon for its work to end, and its merge must not hold the
	// queue the successor serves meanwhile.
	ReturnPausedMerges(ctx context.Context, ws ids.WorkspaceID) error
	// ReviewQueues offers every repository with a merge queue to this
	// daemon's pump, judged from its durable rows (rollout.MergeMover).
	ReviewQueues(ctx context.Context) error
	// TakeTestRunsOff takes every gate's run off the test service and holds
	// every gate off it until the release it answers is called: the daemon's
	// own restart of the service (deploy.Restarter) runs between the two and
	// releases the gates once the service answers again, or with why it does
	// not. A gate it held runs again from its start, spending no fixing
	// attempt and pausing nothing (testservicehold.go).
	TakeTestRunsOff(ctx context.Context) (func(answered error), error)
	// SuspendForRestart brings a workspace's merge to a stopping point for a
	// RestartWorkspace, exactly as a transfer does, and stops this daemon
	// admitting any merge of it until ResumeAfterRestart. A merge that is
	// still waiting for the workspace to fall free is suspended the same way.
	SuspendForRestart(ctx context.Context, ws ids.WorkspaceID) error
	// ResumeAfterRestart takes the workspace's merges back from their durable
	// rows once the restart's bounce has finished, whatever its outcome: a
	// merge that was waiting waits again on the fresh session.
	ResumeAfterRestart(ctx context.Context, ws ids.WorkspaceID) error
}

// Request is one merge request: the REQUESTING workspace, which the merge
// runs in, and the SOURCE, which is what it lands.
type Request struct {
	// Workspace is the requester.
	Workspace ids.WorkspaceID
	// Source is what the requester merges.
	Source wsm.MergeSource
	// By says who asked, which decides whether the admission may displace the
	// requester's turn in flight (see Requester).
	By Requester
	// Asker is the workspace whose agent asked, set exactly when By is
	// RequestedByAgent (agentrepl.v1.WorkspaceRequesterWorkspace). An
	// in-place merge lands only on the ask of the workspace itself, of its
	// owner, of the workspace it lands into, or of the user (checkAsker).
	Asker ids.WorkspaceID
	// Detached runs a named branch's merge ASYNC, in a hidden MERGE WORKSPACE
	// forked from the requester instead of in the requester
	// (agentrepl.v1.MergeWorkspaceAsync); nil runs it SYNC, in place. It is
	// legal on the branch source alone.
	Detached *DetachedRun
	// OneShot ANNOUNCES the requester a one-shot: its own branch merges in
	// place once the requesting turn ends and the requester closes once it
	// lands, and the request marks the requester a one-shot on its creation
	// job (wsm.CreationJob.OneShot), the fact every surface draws a one-shot
	// from. It is legal on an in-place own-branch source that does not keep
	// the requester open, alone.
	OneShot bool
	// PlaceBehind names the merge, as the queue lists it (QueueMember.Name),
	// this one is put in line directly behind (wsm.MergeSource.Behind); ""
	// places it at the back or, with Source.Front, at the front. The named
	// merge must stand in the requester's queue when it is asked.
	PlaceBehind string
}

// Enqueued is what a recorded request reserved.
type Enqueued struct {
	// MergeBranch is the branch an ASYNC request's merge workspace will be
	// created under, reserved from now until it exists
	// (agentrepl.v1.CreateWorkspaceAsyncMergeAccepted.merge_workspace_branch).
	// Empty for an in-place merge.
	MergeBranch string
}

// DetachedRun is what an async request says about its merge workspace.
type DetachedRun struct {
	// Name is the merge workspace's name as the requesting agent chose it,
	// unprefixed. It is checked and reserved when the request is recorded.
	Name string
	// Pick is the requesting agent's instruction for preparing the branch
	// before it joins the queue (agentrepl.v1.MergeWorkspaceAsync.instruction),
	// empty when the branch lands as it is.
	Pick string
	// CompactAfter compacts the requester's conversation once the merge
	// lands (agentrepl.v1.MergeWorkspaceAsync.compact_after).
	CompactAfter bool
	// CloseOnLanding is another workspace the merge closes once it lands
	// (agentrepl.v1.MergeWorkspaceAsync.one_shot), empty when it closes none.
	CloseOnLanding ids.WorkspaceID
	// CleanupWorktree removes, once the merge lands, the worktree that had
	// the branch checked out when the request was accepted
	// (agentrepl.v1.MergeWorkspaceAsync.cleanup_worktree).
	CleanupWorktree bool
	// Hooks are the deploy hooks the merge workspace runs in place of its
	// repository's (agentrepl.v1.CreateWorkspaceAsyncMerge.deploy_hooks), and
	// it never inherits its requester's. Empty when the request names none.
	Hooks wsm.DeployHooks
	// Maker is the workspace the merged work was done in: the merge workspace
	// forks its conversation, and it is the one target besides the primary
	// branch the branch may land in (agentrepl.v1.CreateWorkspaceAsyncMerge).
	Maker ids.WorkspaceID
	// IntoMaker lands the branch into the maker's branch instead of on the
	// primary branch.
	IntoMaker bool
}

// NewDetachedRun is the async run the rpc reads off a request: the merge
// workspace's name and the instruction with their surrounding space trimmed,
// and the landing asks, deploy hooks, maker and target as given.
func NewDetachedRun(name, instruction string, compactAfter bool, closeOnLanding ids.WorkspaceID, cleanupWorktree bool, hooks wsm.DeployHooks, maker ids.WorkspaceID, intoMaker bool) *DetachedRun {
	return &DetachedRun{Name: strings.TrimSpace(name), Pick: strings.TrimSpace(instruction), CompactAfter: compactAfter,
		CloseOnLanding: closeOnLanding, CleanupWorktree: cleanupWorktree, Hooks: hooks, Maker: maker, IntoMaker: intoMaker}
}

// DetachedMerges forks the merge workspace a detached request runs in, and
// hands a detached merge's outcome back to the workspace that asked for it.
// The merge package never creates a workspace or submits to another
// workspace's session itself: this seam is the one way it does either.
type DetachedMerges interface {
	// Fork answers the merge workspace forked for ORIGIN's request, creating
	// it when none is registered yet: a fork of the requester's conversation
	// named NAME (the name the request reserved), its own worktree on BASE as
	// it stands now — the requester's branch for a whole-branch merge, the
	// target branch for a pick — landing by ORIGIN's TargetDir and
	// ParentBranch and running exactly ORIGIN's Hooks, which the hand-off
	// decided, and recording ORIGIN (whose Requester is REQUESTER's id) as
	// what it was forked for. It is idempotent per request, so a hand-off a restart cut
	// short finds the workspace already forked.
	Fork(ctx context.Context, requester wsm.Workspace, origin wsm.MergeOrigin, name, base string) (wsm.Workspace, error)
	// HandBack reports how a detached merge ended to the conversation of the
	// workspace that asked for it.
	HandBack(ctx context.Context, requester ids.WorkspaceID, mergeWS wsm.Workspace, outcome DetachedOutcome) error
	// Reveal draws a hidden merge workspace in the tab bar and the sidebar
	// from now on, and answers whether this call revealed it (false when it
	// already was).
	Reveal(ctx context.Context, mergeWS ids.WorkspaceID) (bool, error)
}

// Names is the slice of workspace naming an async request needs: its merge
// workspace's name is checked and reserved when the request is recorded, so
// the fork can never collide.
type Names interface {
	// ClaimMergeName resolves NAME (unprefixed) to the branch a merge workspace
	// of the repository at REPODIR would be created under, and claims it until
	// RELEASE is called: no other name allocation hands it out meanwhile. The
	// caller releases once the request carrying the name is recorded (which
	// reserves it durably) or refused. A name that is already a workspace, a
	// branch, an occupied worktree directory, a reservation or a claim answers
	// TAKEN with no claim.
	ClaimMergeName(ctx context.Context, repoDir, name string) (branch string, release func(), taken bool, err error)
	// MergeBranch resolves NAME (unprefixed) to the name its merge workspace
	// would stand under, claiming nothing: the name a merge off its queue is
	// matched against.
	MergeBranch(name string) string
}

// Banners raises the merge queue's desktop banners
// (desktopnotify.MergeBanners). A MERGE RAISES EXACTLY TWO: Queued as it is
// put in line, and Resolved as it leaves the queue for good. Nothing between
// -- admission, a repair turn, the test gate, a detached merge's handback --
// raises one.
type Banners interface {
	Queued(ws ids.WorkspaceID, label string)
	Resolved(ws ids.WorkspaceID, end desktopnotify.MergeEnd, summary string)
	// Mediated posts the notification of an escalated deploy, whose click
	// opens escalation workspace WS.
	Mediated(ws ids.WorkspaceID, summary string)
}

// DetachedOutcome is how a detached merge ended, for its handback. THE KIND IS
// THE END; Summary is its one-line account, and Branch is the requester's
// branch the merge was asked for.
type DetachedOutcome struct {
	Kind    DetachedOutcomeKind
	Summary string
	Branch  string
	// MergedInto is the branch a landing merged the source into, empty for
	// every end that merged nothing (a branch already on its target among
	// them). The resolved banner says it in place of Summary.
	MergedInto string
	// InPlace marks the end of a SYNC merge, which ran in the workspace it is
	// handed back to rather than in a merge workspace.
	InPlace bool
	// Deploy is what became of a landing's deploy: the report of a landing
	// the deploy was told of is owed until that deploy has run.
	Deploy LandingDeploy
	// Cause is why an abandoned merge left the queue, empty for every other
	// end and for a detached request whose merge workspace could not be
	// forked.
	Cause AbandonCause
}

// Released reports whether the outcome is a merge the user or the operator
// RELEASED themselves (an evict, a dequeue), queued or running, in place or
// detached. NOBODY IS TOLD OF IT (2026-11-17): whoever released it already
// knows, and its bubble already shows the abandoned terminal, so it hands no
// report back to its requester and raises no resolved banner.
func (o DetachedOutcome) Released() bool {
	return o.Kind == DetachedAbandoned && o.Cause.releases()
}

// LandingDeploy names what a deploy did about the runtime's rollout.
type LandingDeploy int

const (
	// LandingNotDeployed is a deploy whose end rolls nothing out: it failed,
	// or its deploy hook asked for no rollout.
	LandingNotDeployed LandingDeploy = iota
	// LandingDeployOwed is a deploy whose deploy hook asked for the rollout
	// of its landing (RollOutLanding): its report waits until the build that
	// carries it runs.
	LandingDeployOwed
	// LandingRolloutsHeld is a deploy whose repository's deploy stage is
	// held, so its deploy hook was skipped.
	LandingRolloutsHeld
	// LandingHoldUnread is a deploy whose repository's hold could not be
	// read, so its deploy hook was not run.
	LandingHoldUnread
)

// DetachedOutcomeKind names how a detached merge ended.
type DetachedOutcomeKind int

const (
	// DetachedLanded is a merge whose branch landed on its target.
	DetachedLanded DetachedOutcomeKind = iota
	// DetachedFailed is a merge that ran and gave up; the merge workspace is
	// left open where it stopped.
	DetachedFailed
	// DetachedAbandoned is a merge taken off the queue before it ran, or a
	// detached request whose merge workspace could not be forked.
	DetachedAbandoned
)

// mergeEnd is the kind's resolved banner end. A kind outside the three is a
// defect in the caller, and panics.
func (k DetachedOutcomeKind) mergeEnd() desktopnotify.MergeEnd {
	switch k {
	case DetachedLanded:
		return desktopnotify.MergeLanded
	case DetachedFailed:
		return desktopnotify.MergeFailed
	case DetachedAbandoned:
		return desktopnotify.MergeAbandoned
	}
	panic(fmt.Sprintf("merge: no banner end for %s", k))
}

// bannerLine is the outcome's line on its resolved desktop banner: where a
// landing merged the source, and Summary for every other end.
func (o DetachedOutcome) bannerLine() string {
	if o.MergedInto != "" {
		return "Merged into " + o.MergedInto
	}
	return o.Summary
}

// String names the kind for the log record.
func (k DetachedOutcomeKind) String() string {
	switch k {
	case DetachedLanded:
		return "landed"
	case DetachedFailed:
		return "failed"
	case DetachedAbandoned:
		return "abandoned"
	}
	return fmt.Sprintf("DetachedOutcomeKind(%d)", int(k))
}

// Requester names who asked for a merge.
//
// IT DECIDES WHEN THE MERGE IS PUT IN LINE AND WHETHER ITS ADMISSION MAY
// DISPLACE THE WORKSPACE'S TURN. A merge an AGENT asks for (the command-file
// ingress, which is what a turn writes to) is put in line only once the
// requesting workspace's turn in flight has ended -- nothing about it reaches
// any client before that (owner, 2026-09-29) -- and it never displaces
// anything: ending the turn in flight would kill the very turn that asked
// (2026-09-28, prompt-bubble-height: "the turn was interrupted"). A merge the
// USER asks for (MergeWorkspace, from Emacs or the webapp) has no requesting
// turn: it is put in line at once, and its admission takes the session away
// from the turn in flight, which is captured, ended unforced and resubmitted
// at the lease's release.
type Requester int

const (
	// RequestedByAgent is a merge an agent's turn asked for over the agent
	// API (agentrepl.v1.WorkspaceRequesterWorkspace). It is the zero value on purpose: a merge whose
	// requester is not known (one the boot recovery put back) never
	// displaces anything either.
	RequestedByAgent Requester = iota
	// RequestedByUser is a merge the user asked for
	// (agentrepl.v1.WorkspaceRequesterUser).
	RequestedByUser
)

// Pauser is who asked for a pause: the user, or the agent of the workspace
// Asker names.
type Pauser struct {
	By Requester
	// Asker is the asking workspace; empty for the user.
	Asker ids.WorkspaceID
	// ToLand marks a pause made to land a change
	// (agentrepl.v1.UpdateMergeQueuePause.to_land), the only pause the queue
	// bypass holder's end-of-turn let-through lifts (ResumeOwnPause).
	ToLand bool
}

// String names a requester for the log record.
func (r Requester) String() string {
	if r == RequestedByUser {
		return "user"
	}
	return "agent"
}

// RepositoryScope names WHICH repository's merge queue a pause or a resume
// addresses. It is the daemon-side spelling of the optional
// workspace.v1.RepositoryRef the request carries, so merge never imports the
// wire types: nil means every repository (the unset ref), and a value is
// resolved against the registry, which is what makes an unknown ref a refusal
// rather than a pause of a queue nobody has.
type RepositoryScope struct {
	// ID is the daemon-minted repository id the ref carried, empty when the
	// ref named only a dir.
	ID ids.RepoID
	// Dir is the repository's common dir the ref carried, empty when the ref
	// named only an id.
	Dir string
}

// Deps are the orchestrator's collaborators.
type Deps struct {
	// DB holds the creation job's layout facts, the lease, the durable per-repo
	// queue and the ledger.
	DB wsm.DB
	// Git is the merge itself.
	Git gitclient.Git
	// Queue delivers the briefs and the resubmitted displaced turn.
	Queue promptqueue.Queue
	// TurnInFlight answers the workspace's turn in flight, false when none is.
	// An agent's merge request is put in line once THAT turn has ended.
	TurnInFlight func(ws ids.WorkspaceID) (ids.TurnID, bool)
	// Feed carries the merge bubble's tabs.
	Feed feed.Resolver
	// Detached forks a detached request's merge workspace and hands its
	// outcome back to the requester.
	Detached DetachedMerges
	// QueueEvents is told every change of every queue this daemon makes, with
	// the queue as it stands after it: the merge queue subscriptions' notices
	// (internal/mergenotice).
	QueueEvents QueueObserver
	// Names checks and claims an async request's merge workspace name.
	Names Names
	// Footer and Sidebar receive the merge facts.
	Footer  footer.Resolver
	Sidebar sidebar.Resolver
	// Banners raises the merge queue's only two desktop banners: one as a
	// merge is put in line, one as it leaves the queue for good.
	Banners Banners
	// Asks are the permission asks and question batches open in each
	// workspace's session: a conflict-resolution or fixing tab whose agent
	// has one open is drawn waiting on the user.
	Asks *Asks
	// Prompts reads the briefs at use time. The field holds the directory,
	// not a cached brief, because a brief is never cached across a use.
	PromptsDir string
	// DeployConfig reads a repository's `.agent-repl/lifecycle.yaml`, at every
	// deploy. nil means the real filesystem (lifecycleconfig.OnDisk).
	DeployConfig lifecycleconfig.Files
	// Builtins is what the stages' built-in defaults need: where the
	// emoji-ack post-deploy script lives.
	Builtins lifecycleconfig.Builtins
	// SelfAddress is the address this daemon serves on: a deploy's script
	// hooks are told it (EnvDeployDaemon) so they call back the daemon that
	// runs their deploy.
	SelfAddress string
	// HookRunner runs a deploy's script hooks. Injected so a hook is
	// exercised against a scripted runner: nothing in a test spawns the
	// repository's real scripts.
	HookRunner HookScriptRunner
	// Briefs loads and splices one brief by name at USE time. It is injected
	// rather than called directly so the orchestrator's tests fake a brief
	// without a prompts directory on disk; the production value is BriefsFrom.
	Briefs BriefLoader
	// StateDir is the state root, whose merge-logs/ subdirectory archives every
	// test-gate run's output, and whose merge-trees/ and merge-worktrees/ hold
	// the queue's own scratch trees and the worktrees it makes for a branch
	// that has none.
	StateDir string
	// Home is the user's home directory, which a test log's drawn label
	// shortens to ~. Empty means os.UserHomeDir.
	Home string
	// TestCommand answers the local merge's test gate FOR ONE TREE, run
	// with the selected suites, no flake re-run, output archived. The gate
	// runs in the worktree checked out on the rebased branch, and a
	// repository's test entrypoint tests the tree it lives in, so the command is
	// resolved per tree from the deploy config's script path (TestCommandFor;
	// AGENT_REPL_TEST_ALL_SCRIPT overrides the script). Its LAST element is the
	// script, which the gate checks is there before it runs; `--suites <a,b>`
	// is appended. WORKSPACE is the requesting workspace's name, which the
	// run names itself by in the RAM disk seat it holds.
	TestCommand func(tree, script, workspace string) []string
	// TestMetricsRoot is the metrics root every gate's test runner pushes its
	// tests' times under (gateArgv); empty when the daemon records no metrics.
	TestMetricsRoot string
	// TestRunner runs a remediation's full-suite gate. Injected so the gate is
	// exercised against a scripted script rather than the repository's real
	// suite: GIT IS NEVER CALLED DURING TESTING and neither is the real roster.
	TestRunner ScriptRunner
	// Tests is the test service every queue gate runs on, so a daemon deploy
	// never cuts one (testsclient): the gate starts its run there, records the
	// run's id in its progress, and a resumed merge watches the same run.
	Tests GateService
	// GateEnv is added to the environment of every gate's run on the test
	// service ("KEY=value"): the daemon's own PATH, which names the
	// toolchains a gate builds and tests with. The service runs under its
	// service manager's bare environment, and a gate without it could not
	// start (2026-11-17 13:07: "go is not on PATH", and the queue paused).
	GateEnv []string
	// Painter turns the gate's ANSI output into paint spans. The client never
	// parses an escape.
	Painter paint.Painter
	// StartSession brings a workspace's session up when none is live, under
	// the lease, because a merge prompt needs one (revival-is-implicit). It
	// answers at once for a live session.
	StartSession StartSessionFunc
	// StopSession ends and reaps a merged workspace's session before its
	// worktree is removed. The process must lose its working directory only
	// after it has stopped writing through it.
	StopSession StopSessionFunc
	// HoldStarts holds the workspace's session starts and relaunches off
	// until the answered release is called (workspace.Fleet.HoldStarts). The
	// close of a landed workspace holds it across stopping the session and
	// removing the worktree, so no relaunch in flight installs a shim into a
	// directory about to vanish: one already running finishes first and is
	// stopped, and one after it finds the workspace closed and drops.
	HoldStarts func(ws ids.WorkspaceID) (release func())
	// PublishRegistry republishes the roster's DURABLE half. A landed merge
	// stamps merged_at and closes the workspace in the registry, and the
	// roster's `recently_merged` section is composed from exactly those
	// facts: without a republish the row stays where it was until some other
	// verb happens to refresh the registry. Nil means no roster is wired.
	PublishRegistry func(context.Context) error
	// PublishHost recomposes and republishes one workspace's HOST view. The
	// composer gate on it is a function of the merge's own state -- merging or
	// not -- and the server cannot see a lease taken or released. Nil means no
	// host surface is wired yet.
	PublishHost func(ids.WorkspaceID)
	// Occupy takes the shim client's in-memory occupancy guard that backs the
	// WSM lease row. The lock arbitrates; the row describes.
	Occupy OccupancyFunc
	// AwaitTurnEnd blocks until one submitted turn ends, reporting how. The
	// orchestrator continues a phase only on a real turn end, never a timer.
	AwaitTurnEnd TurnWaiter
	// InterruptTurn stops a workspace's running turn, as the user's turn
	// interrupt does, answering whether one was running. A merge-tab
	// interrupt reaches the session through it.
	InterruptTurn InterruptTurnFunc
	// CaptureDisplaced durably captures the user turn a merge displaces, at
	// admission, so it is resubmitted EXACTLY ONCE at lease release even across
	// a daemon bounce. It reports false when nothing was in flight.
	CaptureDisplaced DisplacedCapture
	// Freeness answers whether the workspace is free — no turn in flight and
	// no live detached work — and waits for it to become so. An admitted merge
	// waits on it before it drives the session: the displaced turn was ended
	// UNFORCED, so its background agents, shells and monitors run on, and the
	// merge neither races them for the conversation nor stops them. Detached
	// work ends only by its own per-task stop or a forced kill the user asked
	// for.
	Freeness Freeness
	// AtColdGate reports whether the workspace's session is parked at its
	// cold gate. A requeue of such a workspace's merge goes in line skipped
	// (wsm.SetMergeColdGated), and its gate is answered by a compaction at
	// once (CompactColdGate).
	AtColdGate func(ws ids.WorkspaceID) bool
	// CompactColdGate answers the cold gate standing on the workspace by a
	// compaction of its whole conversation with the gate's default
	// summarizer, keeping the prompts held at the gate. The session it
	// brings up retires the gate, which resumes the workspace's merge
	// (ColdGateRetired). A workspace with no gate standing is left as it is.
	CompactColdGate func(ctx context.Context, ws ids.WorkspaceID) error
	// PauseAfterCapture is a TEST-ONLY seam: when set, a run blocks in it
	// immediately after the displaced turn was captured, which is the one
	// window a test cannot otherwise reach (the merge's own next submission,
	// or a clean run's finish, closes it instantly). PRODUCTION LEAVES IT NIL
	// — nothing in the daemon's own graph builds one unless the test-only
	// knob names a rendezvous file.
	PauseAfterCapture AdmissionPause
	// PauseInTerminal is a TEST-ONLY seam: when set, a run blocks in it at the
	// TOP of its terminal — the terminal work already registered with the
	// shutdown drain, and not one durable stamp written yet. It is what lets a
	// suite land a deliberate stop exactly in the window the drain covers.
	// Nil in production, where a terminal runs straight through.
	PauseInTerminal AdmissionPause
	// Rollout is the deploy, told of a landing ONLY after lease release and
	// terminal publication.
	Rollout Trigger
	// Standing is whether this daemon may advance its queues across a
	// handover, and which workspaces it serves (rollout.Controller).
	Standing Standing
	// DeployChecks makes, reopens and closes the deploy checks' workspaces
	// (workspace.Verbs). REQUIRED.
	DeployChecks DeployCheckWorkspaces
	// ReadLogs reads the warnings and errors every system logged in a span,
	// for the intake's facts (intakefacts.go). REQUIRED.
	ReadLogs LogReader
	// Now is the clock. Injected so a ledger interval and a terminal stamp are
	// assertable without a real one.
	Now func() time.Time
	// Meter records the merge queue's metrics (runtimemetrics.Surface): how
	// long each merge stage took, and the deploy mediations. REQUIRED.
	Meter Meter
	// Log is the orchestrator's logger.
	Log dlog.Surfaces
}

// DeployCheckWorkspaces is what the deploy checks need of the workspaces
// (workspace.Verbs, bound after the queue is built).
type DeployCheckWorkspaces interface {
	// CreateDeployMediator makes a repository's deploy mediator, in sight,
	// unprompted and in remote control.
	CreateDeployMediator(ctx context.Context, repoDir, name string) (wsm.Workspace, error)
	// SetDeployMediatorEffort picks the deploy mediator's effort, which its
	// session comes up at, and switches a live session to it without waiting
	// on its turn; a shim absent, stood down or unreachable is no failure.
	SetDeployMediatorEffort(ctx context.Context, ws ids.WorkspaceID) error
	// Reopen opens a closed remediation workspace again, its worktree cut
	// again on its branch at the main branch's tip.
	Reopen(ctx context.Context, ws ids.WorkspaceID) error
	// CloseSelf closes a deploy check workspace, at its running turn's end
	// when one runs, answering whether it waits for that end.
	CloseSelf(ctx context.Context, ws ids.WorkspaceID) (bool, error)
	// Resume shows a hidden deploy check workspace again: the mediator
	// stands in sight for good, so any hide of it is undone.
	Resume(ctx context.Context, ws ids.WorkspaceID) error
	// Refresh brings the standing fixer's worktree to its parent's tip
	// before a new brief.
	Refresh(ctx context.Context, ws ids.WorkspaceID) error
	// Compact compacts a standing deploy check's conversation by Haiku now,
	// answering whether it had one to compact.
	Compact(ctx context.Context, ws ids.WorkspaceID) (bool, error)
	// ContextTokens answers the tokens of context a deploy check workspace's
	// conversation holds now, 0 when it states none.
	ContextTokens(ws ids.WorkspaceID) (int64, error)
}

// Meter records the merge queue's metrics. Each call is an atomic add and
// never blocks the merge.
type Meter interface {
	// ObserveMergeStep records how long a run spent on one step before it
	// left it, by the step's footer.MergeStep spelling.
	ObserveMergeStep(step string, took time.Duration)
	// ObserveMergeSpan records one span of a merge (SpanPick, SpanQueued,
	// SpanRunning, SpanTotal) by how it ended.
	ObserveMergeSpan(span, outcome string, took time.Duration)
	// ObserveMediationSpan records one span of a deploy mediation
	// (MediationSpanRollout, MediationSpanAssessment, MediationSpanTotal) by
	// how it ended (mediation.go's outcomes).
	ObserveMediationSpan(span, outcome string, took time.Duration)
	// ObserveMediationChain records a chain of mediations the queue advanced
	// past, by how its last ended: how many remediations were deployed in a
	// row, and how long the queue was held from the chain's first deploy.
	ObserveMediationChain(outcome string, redeploys int, took time.Duration)
	// SetRemediationsInARow is the chain owed now: how many remediations
	// were deployed in a row since the queue last advanced, 0 when none is
	// owed.
	SetRemediationsInARow(n int)
	// ObserveRemediation records one remediation, from its dispatch to the
	// check that ended it, by how it ended (RemediationLanded,
	// RemediationEscalated, RemediationRedispatched).
	ObserveRemediation(outcome string, took time.Duration)
	// ObserveRemediationGateRounds records how many of a remediation
	// finish's gates failed before it landed or was given up, by how it
	// ended (RemediationGateLanded, RemediationGateGivenUp).
	ObserveRemediationGateRounds(outcome string, n int)
	// ObserveMergeRound records one round of a merge's tab (a rebase, a
	// conflict resolution, a test gate, a fixing attempt) by its ledger
	// outcome.
	ObserveMergeRound(kind, outcome string, took time.Duration)
	// ObserveMergeRounds records how many rounds of one repair tab kind
	// (RoundCountKinds) a concluded merge took, by how the merge ended.
	ObserveMergeRounds(kind, outcome string, n int)
	// CountQueueChange records one change to a queue's line, by its
	// QueueChangeKind spelling and, for a merge leaving, its cause.
	CountQueueChange(change, cause string)
	// ObserveMergeHead records one stay of a merge at its queue's head, from
	// taking the head to leaving it, by how it left (the HeadExit values, or
	// an AbandonCause's spelling for a merge abandoned at the head).
	ObserveMergeHead(exit string, took time.Duration)
	// ObserveMediatorTurnTokens records the tokens of one KIND
	// (MediatorTokenKinds) a deploy mediator's turn spent, by the phase of
	// its run the turn ended in (the MediatorPhase constants).
	ObserveMediatorTurnTokens(phase, kind string, tokens uint64)
	// ObserveMediatorCompaction records one compaction of a deploy
	// mediator, by how it ended (the MediatorCompaction constants).
	ObserveMediatorCompaction(outcome string, took time.Duration)
	// CountMediatorRun records one deploy mediator run, by how it ended (the
	// MediatorRun constants).
	CountMediatorRun(outcome string)
}

// How a merge leaves its queue's head, the merge head metric's `exit`. A
// merge abandoned at the head leaves by its AbandonCause's spelling.
const (
	HeadExitLanded                   = OutcomeLanded
	HeadExitAlreadyOn                = OutcomeAlreadyOn
	HeadExitFailed                   = OutcomeFailed
	HeadExitNeededFixes              = "needed_fixes"
	HeadExitNeededConflictResolution = "needed_conflict_resolution"
	HeadExitTransferred              = "transferred"
	HeadExitPaused                   = "paused"
	HeadExitRestarted                = "restarted"
	HeadExitHandover                 = "handover"
	HeadExitDaemonExit               = "daemon_exit"
)

// RoundCountKinds are the tab kinds a concluded merge's round counts are
// recorded for, every one of them, 0 for a kind the merge never opened: how
// many rebases, conflict resolutions, test gates and fixing attempts it
// took.
var RoundCountKinds = []string{TabRebasing, TabConflicts, TabTests, TabFixes}

// The merge spans a Meter records.
const (
	// SpanPick is the branch's preparation from the request's instruction.
	SpanPick = "pick"
	// SpanQueued is the time from enqueueing until the run was admitted.
	SpanQueued = "queued"
	// SpanRunning is the time from admission until the terminal.
	SpanRunning = "running"
	// SpanTotal is the time from enqueueing until the terminal.
	SpanTotal = "total"
)

// The outcomes a merge span ends in.
const (
	OutcomeLanded    = "landed"
	OutcomeAlreadyOn = "already_on"
	OutcomeFailed    = "failed"
	OutcomeOK        = "ok"
)

// BriefLoader loads one brief by name and splices its values, at use time. It
// composes rather than returning the parsed brief because a brief is never
// worth having half-composed: the two steps fail for the same reason and the
// caller reacts the same way to either.
type BriefLoader func(name string, values map[string]string) (string, error)

// BriefsFrom is the production BriefLoader: it reads dir at EVERY use, so
// editing a brief takes effect without a daemon bounce, and a missing file or a
// placeholder the values do not cover is LOUD rather than a hole in a prompt.
func BriefsFrom(dir string) BriefLoader {
	return func(name string, values map[string]string) (string, error) {
		brief, err := LoadBrief(dir, name)
		if err != nil {
			return "", err
		}
		return brief.Splice(values)
	}
}

// ScriptRunner runs one command in a directory and reports its combined output
// and exit code. An error means the run could not be CLASSIFIED (the script
// could not be spawned); a suite that ran and failed is a non-zero code and a
// nil error, because a failing suite is an answer.
type ScriptRunner interface {
	// RunLinesMeasured executes argv in dir and answers the combined stdout
	// and stderr with the process's exit code and what the run cost, handing
	// each line to onLine as it is written, so the gate follows its suites
	// live and the governor learns a gate's processor cost.
	RunLinesMeasured(ctx context.Context, dir string, argv []string, onLine func(string)) (scriptrunner.Run, error)
}

// GateService is the slice of the test service a queue gate drives
// (testsclient.Client).
type GateService interface {
	Start(ctx context.Context, spec testsclient.Spec) (string, error)
	Watch(ctx context.Context, run string, from uint64, onLine func(number uint64, text string)) (testsclient.End, error)
	Stop(ctx context.Context, run string) error
}

// HookScriptRunner runs one script hook: argv in dir with the options' added
// environment and timeout, handing each output line to onLine as it is
// written (scriptrunner.Runner).
type HookScriptRunner interface {
	RunLinesWith(ctx context.Context, dir string, argv []string, opts scriptrunner.Options, onLine func(string)) (scriptrunner.Run, error)
}

// StartSessionFunc starts a workspace's session under the merge lease, for a
// workspace that has none but has a configured prompt to run.
type StartSessionFunc func(ctx context.Context, ws ids.WorkspaceID) error

// StopSessionFunc ends and reaps one workspace session. Force is true when a
// landed merge is retiring the worktree the process runs inside.
type StopSessionFunc func(ctx context.Context, ws ids.WorkspaceID, force bool) error

// OccupancyFunc takes the shim client's occupancy guard for a workspace,
// returning the release. It reports false when the workspace has no live shim,
// which is the sessionless merge's legal answer rather than a failure.
type OccupancyFunc func(ws ids.WorkspaceID, holder string) (release func(), ok bool, err error)

// InterruptTurnFunc stops one workspace's running turn and reports whether a
// turn was running.
type InterruptTurnFunc func(ctx context.Context, ws ids.WorkspaceID) (bool, error)

// TurnWaiter blocks until one turn ends and reports how it ended.
type TurnWaiter func(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID) (wsm.TurnClose, error)

// Displaced is the turn a merge took the session away from: its id, and the
// text it carried, kept so the resubmission does not depend on the turn's
// record still being open when the lease is released.
type Displaced struct {
	Turn ids.TurnID
	Text string
}

// Freeness is the slice of the session fleet a merge waits on. It is the same
// freeness the rollout's relaunch waits on, answered by the session watcher's
// stream edges: nothing polls and nothing sleeps.
type Freeness interface {
	// Free reports whether the workspace is free right now. A workspace with no
	// live session is free.
	Free(ws ids.WorkspaceID) bool
	// AwaitFree blocks until the workspace is free, or until ctx ends.
	AwaitFree(ctx context.Context, ws ids.WorkspaceID) error
	// Blockers names what keeps the workspace from being free right now --
	// its turn in flight ("turn:<id>") and each live detached item
	// (sessionwatcher.LiveWorkSet.Names) -- empty when it is free.
	Blockers(ws ids.WorkspaceID) []string
}

// AdmissionPause blocks a merge run at admission. It exists for the test seam
// PauseAfterCapture and has no production implementation; a nil pause is the
// production value and is never called.
type AdmissionPause func(ctx context.Context, ws ids.WorkspaceID)

// DisplacedCapture durably records the turn a merge displaced. The bool is
// false when no turn was in flight, which is not a failure.
type DisplacedCapture func(ctx context.Context, ws ids.WorkspaceID) (Displaced, bool, error)

// Trigger is the slice of the daemon's deploy merge uses: the self-reload,
// told what landed. It is a narrow interface so merge imports neither the
// deploy nor the rollout.
type Trigger interface {
	// Landed reports ONE COMPLETE CHANGE landed on the daemon's own checkout —
	// however many commits it carries — and the deploy runs ONCE for it, off
	// the caller: a merge never waits on a build.
	Landed(ctx context.Context, landed []gitclient.Commit)
}

// LoadBrief reads one brief by name from dir at use time. It exists so every
// call site loads a brief the same way and fails the same way when one is
// missing.
func LoadBrief(dir, name string) (prompts.Prompt, error) {
	return prompts.Load(dir, name)
}

// Standing is the part of the rollout controller the queue reads: whether
// this daemon's queues may advance, the edge on which that answer changes,
// and whether it serves a workspace now.
type Standing interface {
	ServesQueue() bool
	QueueChanged() <-chan struct{}
	ServesWorkspace(ws ids.WorkspaceID) bool
}
