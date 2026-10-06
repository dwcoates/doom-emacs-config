// Package merge is the merge orchestrator: a per-repo queue, two methods,
// tabs, the lease, the test gate, the briefs and the ledger.
//
// A MERGE RUNS IN THE WORKSPACE THAT ASKED FOR IT (owner, 2026-09-30). No
// workspace is ever created for a merge: the requesting workspace's feed
// carries the bubble, its footer and roster row carry the status, and its own
// session does the conflict resolution and the test fixing. What it merges is
// the request's SOURCE: its own branch, another workspace's, a branch that is
// no workspace, or its own branch already merged upstream.
//
// The two methods are keyed by gitclient.SameRepo(target, daemonCheckout). The
// Emacs repo runs pre-prompt, rebase (with conflict resolution), tests (with
// fixing), the non-fast-forward merge commit and post-prompt; EVERY OTHER repo
// runs pre-prompt and post-prompt. A branch already merged upstream runs
// updating main and post-prompt. Nothing parks: a merge that gives up FAILS
// and hands the workspace back. Briefs are read from prompts/ AT USE TIME. The
// displaced user turn is captured durably and resubmitted EXACTLY ONCE at
// lease release. See docs/protobuf-design/merge-landing.md.
package merge

import (
	"context"
	"time"

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

// EscalationFile is the file the test-fix agent writes in the worktree it
// fixes to end the fixing attempts without a passing suite, and EscalationMarker is the
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
	// Enqueue records a merge request. It REFUSES pre-state: no layout facts
	// recorded at creation, a deleted session, a source workspace or branch
	// that is not there, or a workspace already requested, queued or merging.
	// The request is DURABLE from here, and it is put in line -- and reported
	// to any client -- only once the turn that asked for it has ended (a
	// merge the USER asked for over an rpc has no requesting turn and is put
	// in line at once).
	Enqueue(ctx context.Context, req Request) error
	// Pause stops the queue from starting new merges; an in-flight merge runs
	// on. A nil scope is the daemon-wide switch (UpdateMergeQueuePause with an
	// UNSET repository); a scope names ONE repository's queue.
	Pause(ctx context.Context, scope *RepositoryScope) error
	// Unpause resumes starting merges, with the same scoping as Pause.
	Unpause(ctx context.Context, scope *RepositoryScope) error
	// Evict removes a workspace's merge from the queue. A merge that is
	// already RUNNING is ABANDONED: it is ended through the one release path,
	// which gives back its lease, its queue entry, its ledger interval and its
	// repository's slot, and the call returns once that is done.
	Evict(ctx context.Context, ws ids.WorkspaceID) error
	// AnswerDequeue answers the tray's dequeue offer: keep the merge queued,
	// or take it out. Taking out a running merge abandons it, as Evict does.
	AnswerDequeue(ctx context.Context, ws ids.WorkspaceID, keep bool) error
	// OnInterrupt raises the dequeue offer when the user interrupts a
	// workspace that is queued.
	OnInterrupt(ctx context.Context, ws ids.WorkspaceID)
	// OnWorkspaceClosed abandons a workspace's merge when the workspace
	// itself is torn down (killed or nuked), recording the close as the
	// abandon cause: a request or a waiting merge leaves its queue, and a
	// running one is ended through the one release path. It is a no-op for a
	// workspace with no merge.
	OnWorkspaceClosed(ctx context.Context, ws ids.WorkspaceID)
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
	// Recover RESUMES every in-flight merge at boot from its progress record
	// (the step it stood on, under the same lease and in the same bubble),
	// runs again from the queue an admitted merge that never took a step,
	// re-arms every request still waiting for its turn to end, and re-enqueues
	// every merge that was queued but not started, in the order it was
	// waiting in. It never silently abandons one.
	Recover(ctx context.Context) error
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
	// RequestedByAgent is a merge an agent's turn asked for through the
	// command-file ingress. It is the zero value on purpose: a merge whose
	// requester is not known (one the boot recovery put back) never
	// displaces anything either.
	RequestedByAgent Requester = iota
	// RequestedByUser is a merge the user asked for over an rpc.
	RequestedByUser
)

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
	// Footer and Sidebar receive the merge facts.
	Footer  footer.Resolver
	Sidebar sidebar.Resolver
	// Holds carries the dequeue offer.
	Holds holds.Resolver
	// Prompts reads the briefs at use time. The field holds the directory,
	// not a cached brief, because a brief is never cached across a use.
	PromptsDir string
	// CheckoutRoot is the agent-repl module checkout the daemon was deployed
	// from. It decides whether a repository's merge policy is the daemon's
	// corpus or the repository's own `.agent-repl/prompts`.
	CheckoutRoot string
	// Policy probes a repository's policy directory for the `merge-before`
	// and `merge-after` briefs it may state. nil means the real filesystem
	// (prompts.OnDisk).
	Policy prompts.Files
	// Briefs loads and splices one brief by name at USE time. It is injected
	// rather than called directly so the orchestrator's tests fake a brief
	// without a prompts directory on disk; the production value is BriefsFrom.
	Briefs BriefLoader
	// SelfRepoDir is the daemon's OWN checkout. SameRepo against it is what
	// selects the Emacs-repo method; AGENT_REPL_SELF_REPO_DIR overrides it in
	// tests, and the self-reload trigger STAYS ON under that override.
	SelfRepoDir string
	// StateDir is the state root, whose merge-logs/ subdirectory archives every
	// test-gate run's output, and whose merge-trees/ and merge-worktrees/ hold
	// the queue's own scratch trees and the worktrees it makes for a branch
	// that has none.
	StateDir string
	// Home is the user's home directory, which a test log's drawn label
	// shortens to ~. Empty means os.UserHomeDir.
	Home string
	// TestCommand answers the Emacs-repo method's test gate FOR ONE TREE, run
	// with the selected suites, no flake re-run, output archived. The gate
	// runs in the worktree checked out on the rebased branch, and a
	// repository's test entrypoint tests the tree it lives in, so the command is
	// resolved per tree (TestCommandFor;
	// AGENT_REPL_TEST_ALL_SCRIPT overrides the script). Its LAST element is the
	// script, which the gate checks is there before it runs; `--suites <a,b>`
	// is appended.
	TestCommand func(tree string) []string
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
	// StopSession ends and reaps a merged workspace's session before its
	// worktree is removed. The process must lose its working directory only
	// after it has stopped writing through it.
	StopSession StopSessionFunc
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
	// Now is the clock. Injected so a ledger interval and a terminal stamp are
	// assertable without a real one.
	Now func() time.Time
	// Log is the orchestrator's logger.
	Log dlog.Surfaces
}

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
	// RunLines executes argv in dir and returns the combined stdout and stderr
	// with the process's exit code, handing each line to onLine as it is
	// written, so the gate follows its suites live.
	RunLines(ctx context.Context, dir string, argv []string, onLine func(string)) (output string, exitCode int, err error)
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
