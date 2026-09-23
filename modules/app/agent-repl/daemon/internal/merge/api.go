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
	// on. A nil scope is the daemon-wide switch (UpdateMergeQueuePause with an
	// UNSET repository); a scope names ONE repository's queue.
	Pause(ctx context.Context, scope *RepositoryScope) error
	// Unpause resumes starting merges, with the same scoping as Pause.
	Unpause(ctx context.Context, scope *RepositoryScope) error
	// Evict removes a queued workspace from the queue.
	Evict(ctx context.Context, ws ids.WorkspaceID) error
	// AnswerDequeue answers the tray's dequeue offer: keep the merge queued,
	// or take it out.
	AnswerDequeue(ctx context.Context, ws ids.WorkspaceID, keep bool) error
	// OnInterrupt raises the dequeue offer when the user interrupts a
	// workspace that is queued.
	OnInterrupt(ctx context.Context, ws ids.WorkspaceID)
	// OnWorkspaceClosed abandons a workspace's WAITING merge when the
	// workspace itself is torn down (killed or nuked), recording the close as
	// the abandon cause. It is a no-op for a workspace with no waiting merge,
	// and never touches a merge already in flight.
	OnWorkspaceClosed(ctx context.Context, ws ids.WorkspaceID)
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
	// Drain stops admitting merges and waits, WITHIN A BOUND
	// (TerminalDrainBound), for every run that has already reached its
	// TERMINAL to finish its durable stamps and its teardown. The daemon's
	// orderly exit calls it with the state client STILL OPEN: without it a
	// SIGTERM landing mid-terminal closed the store under those writes, and
	// merged_at, closed and the lease release were lost to failed
	// transactions. A merge still in a long phase is announced at INFO and
	// left to the boot recovery, never waited for.
	Drain(ctx context.Context)
	// Recover resumes or LOUDLY FAILS every in-flight merge at boot, and
	// re-enqueues every merge that was queued but not started, in the order it
	// was waiting in. It never silently abandons one.
	Recover(ctx context.Context) error
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
	// composer gate on it is a function of the merge's own state -- merging,
	// parked, or neither -- and the server cannot see a lease taken, parked
	// or released. Nil means no host surface is wired yet.
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
	// Run executes argv in dir and returns the combined stdout and stderr with
	// the process's exit code.
	Run(ctx context.Context, dir string, argv []string) (output string, exitCode int, err error)
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
