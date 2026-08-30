// Package merge is the merge orchestrator: a per-repo queue, two methods,
// tabs, the lease, the test gate, the briefs and the ledger.
//
// The two methods are keyed by gitclient.SameRepo(target, daemonCheckout). The
// Emacs repo runs pre-prompt, no-ff merge, conflicts, tests, fixes, a rollout
// bounce and post-prompt; EVERY OTHER repo runs pre-prompt, merge,
// post-prompt. Briefs are read from prompts/ AT USE TIME. The displaced user
// turn is captured durably and resubmitted EXACTLY ONCE at lease release. See
// ARCHITECTURE.md "merge".
package merge

import (
	"context"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/gitclient"
	"claude-repld/internal/ids"
	"claude-repld/internal/paint"
	"claude-repld/internal/promptqueue"
	"claude-repld/internal/prompts"
	"claude-repld/internal/resolve/feed"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/resolve/holds"
	"claude-repld/internal/resolve/sidebar"
	"claude-repld/internal/wsm"
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
)

// EscalationFile is the file the test-fix agent writes in the merge target to
// end the fixes loop without a passing suite, and EscalationMarker is the
// exact first line that makes it one. THE DAEMON SUBSTITUTES BOTH INTO THE
// BRIEF: a user-edited brief must not be able to drift into instructing the
// agent to write a record nothing reads.
const (
	// EscalationFile is the marker file's target-relative path.
	EscalationFile = ".agent-repl-merge-escalation"
	// EscalationMarker is the marker file's required first line.
	EscalationMarker = "MERGE ESCALATION: ARCHITECTURAL CHANGE REQUIRED"
)

// Orchestrator is the merge queue's whole surface.
type Orchestrator interface {
	// Enqueue queues a workspace's merge. It REFUSES pre-state: no layout
	// facts recorded at creation, a deleted session, or a workspace already
	// queued or merging.
	Enqueue(ctx context.Context, ws ids.WorkspaceID) error
	// Pause stops the queue from starting new merges; an in-flight merge runs
	// on.
	Pause(ctx context.Context) error
	// Unpause resumes starting merges.
	Unpause(ctx context.Context) error
	// Evict removes a queued workspace from the queue.
	Evict(ctx context.Context, ws ids.WorkspaceID) error
	// AnswerDequeue answers the tray's dequeue offer: keep the merge queued,
	// or take it out.
	AnswerDequeue(ctx context.Context, ws ids.WorkspaceID, keep bool) error
	// OnInterrupt raises the dequeue offer when the user interrupts a
	// workspace that is queued.
	OnInterrupt(ctx context.Context, ws ids.WorkspaceID)
	// RouteParked delivers a submission that arrived while this workspace's
	// merge lease stands PARKED. It is the queue's one ingress into the
	// orchestrator: the queue recognizes the parked lease policy, never merge
	// as a concept, and hands the submission here rather than starting a turn
	// of the session's own. THE LEASE STATE IS THE RECOGNITION — no classifier
	// and no content inspection happens on this path.
	RouteParked(ctx context.Context, ws ids.WorkspaceID, said *conversationv1.UserSaid) error
	// Facts reports a workspace's merge facts for the footer and the roster;
	// the bool is false when the workspace has no merge.
	Facts(ws ids.WorkspaceID) (MergeFacts, bool)
	// Recover resumes or LOUDLY FAILS every in-flight merge at boot, and
	// re-enqueues every merge that was queued but not started, in the order it
	// was waiting in. It never silently abandons one.
	Recover(ctx context.Context) error
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
	// Feed carries the merge bubble's tabs.
	Feed feed.Resolver
	// Footer and Sidebar receive the merge facts.
	Footer  footer.Resolver
	Sidebar sidebar.Resolver
	// Holds carries the dequeue offer.
	Holds holds.Resolver
	// Prompts reads the briefs at use time. The field holds the directory,
	// not a cached brief, because a brief is never cached across a use.
	PromptsDir string
	// Briefs loads one brief by name at USE time. It is injected rather than
	// called directly so the orchestrator's tests fake a brief without a
	// prompts directory on disk; the production value is BriefsFrom.
	Briefs BriefLoader
	// SelfRepoDir is the daemon's OWN checkout. SameRepo against it is what
	// selects the Emacs-repo method; AGENT_REPL_SELF_REPO_DIR overrides it in
	// tests, and the self-reload trigger STAYS ON under that override.
	SelfRepoDir string
	// StateDir is the state root, whose merge-logs/ subdirectory archives every
	// test-gate run's output.
	StateDir string
	// TestCommand is the Emacs-repo method's test gate, run with the selected
	// suites, no flake re-run, output archived. The first element is the script
	// (AGENT_REPL_TEST_ALL_SCRIPT overrides it); `--suites <a,b>` is appended.
	TestCommand []string
	// TestRunner runs the test gate. Injected so the gate is exercised against
	// a scripted script rather than the repository's real suite: GIT IS NEVER
	// CALLED DURING TESTING and neither is the real roster.
	TestRunner ScriptRunner
	// Painter turns the gate's ANSI output into paint spans. The client never
	// parses an escape.
	Painter paint.Painter
	// StartSession starts a session for a workspace that has none, under the
	// lease, because a configured prompt needs one (revival-is-implicit).
	StartSession StartSessionFunc
	// CommitMerge concludes a merge whose index the resolution agent finished
	// staging, and commits a staged test fix as a follow-up commit.
	//
	// IT IS OWED TO gitclient.Git. MergeNoFF creates its own commit when the
	// merge applies cleanly, but a CONFLICTED merge is left in progress with
	// nothing to conclude it, and the git leaf exposes no commit verb — so the
	// conflicts tab and the fixes loop have no way to land the work the agent
	// staged. The seam belongs there as `Commit(ctx, dir, message) (Commit,
	// error)`; it lives here until the leaf grows it.
	CommitMerge CommitFunc
	// Occupy takes the shim client's in-memory occupancy guard that backs the
	// WSM lease row. The lock arbitrates; the row describes.
	Occupy OccupancyFunc
	// AwaitTurnEnd blocks until one submitted turn ends, reporting how. The
	// orchestrator continues a phase only on a real turn end, never a timer.
	AwaitTurnEnd TurnWaiter
	// CaptureDisplaced durably captures the user turn a merge displaces, at
	// admission, so it is resubmitted EXACTLY ONCE at lease release even across
	// a daemon bounce. It reports false when nothing was in flight.
	CaptureDisplaced DisplacedCapture
	// ParkedRoute delivers a parked submission to the resolution agent as
	// guidance, landing it in the parked tab.
	ParkedRoute ParkedRouter
	// Rollout is triggered ONLY after lease release and terminal publication.
	Rollout Trigger
	// Now is the clock. Injected so a ledger interval and a terminal stamp are
	// assertable without a real one.
	Now func() time.Time
	// Log is the orchestrator's logger.
	Log dlog.Surfaces
}

// BriefLoader loads one brief by name, at use time.
type BriefLoader func(name string) (prompts.Prompt, error)

// BriefsFrom is the production BriefLoader: it reads dir at every use, so
// editing a brief takes effect without a daemon bounce.
func BriefsFrom(dir string) BriefLoader {
	return func(name string) (prompts.Prompt, error) { return LoadBrief(dir, name) }
}

// ScriptRunner runs one command in a directory and reports its combined output
// and exit code. An error means the run could not be CLASSIFIED (the script
// could not be spawned); a suite that ran and failed is a non-zero code and a
// nil error, because a failing suite is an answer.
type ScriptRunner interface {
	// Run executes argv in dir and returns the combined stdout and stderr with
	// the process's exit code.
	Run(ctx context.Context, dir string, argv []string) (output string, exitCode int, err error)
}

// StartSessionFunc starts a workspace's session under the merge lease, for a
// workspace that has none but has a configured prompt to run.
type StartSessionFunc func(ctx context.Context, ws ids.WorkspaceID) error

// CommitFunc commits whatever is staged in a directory, answering with the
// commit it produced. See Deps.CommitMerge for why it is not on the git leaf
// yet.
type CommitFunc func(ctx context.Context, dir, message string) (gitclient.Commit, error)

// OccupancyFunc takes the shim client's occupancy guard for a workspace,
// returning the release. It reports false when the workspace has no live shim,
// which is the sessionless merge's legal answer rather than a failure.
type OccupancyFunc func(ws ids.WorkspaceID, holder string) (release func(), ok bool, err error)

// TurnWaiter blocks until one turn ends and reports how it ended.
type TurnWaiter func(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID) (wsm.TurnClose, error)

// DisplacedCapture durably records the turn a merge displaced. The bool is
// false when no turn was in flight, which is not a failure.
type DisplacedCapture func(ctx context.Context, ws ids.WorkspaceID) (ids.TurnID, bool, error)

// ParkedRouter delivers one parked submission to the resolution agent as
// guidance, addressed at the parked tab. It answers with the turn the guidance
// runs as, so the orchestrator resumes on that turn's real end rather than a
// timer.
type ParkedRouter func(ctx context.Context, ws ids.WorkspaceID, said *conversationv1.UserSaid) (ids.TurnID, error)

// Trigger is the slice of the rollout controller merge uses: the self-reload
// trigger, invoked with what landed. It is a narrow interface so merge does
// not import the rollout controller (the two never import each other).
type Trigger interface {
	// Trigger classifies the landed commits by subsystem and runs the
	// deploy-and-bounce sequence.
	Trigger(ctx context.Context, landed []gitclient.Commit) error
}

// LoadBrief reads one brief by name from dir at use time. It exists so every
// call site loads a brief the same way and fails the same way when one is
// missing.
func LoadBrief(dir, name string) (prompts.Prompt, error) {
	return prompts.Load(dir, name)
}
