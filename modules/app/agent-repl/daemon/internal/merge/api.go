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

	"claude-repld/internal/dlog"
	"claude-repld/internal/gitclient"
	"claude-repld/internal/ids"
	"claude-repld/internal/notimpl"
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
	// Facts reports a workspace's merge facts for the footer and the roster;
	// the bool is false when the workspace has no merge.
	Facts(ws ids.WorkspaceID) (MergeFacts, bool)
	// Recover resumes or LOUDLY FAILS every in-flight merge at boot. It never
	// silently abandons one.
	Recover(ctx context.Context) error
}

// Deps are the orchestrator's collaborators.
type Deps struct {
	// DB holds the creation job's layout facts, the lease and the ledger.
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
	// SelfRepoDir is the daemon's OWN checkout. SameRepo against it is what
	// selects the Emacs-repo method; `-self-repo` overrides it in tests.
	SelfRepoDir string
	// TestCommand is the Emacs-repo method's test gate, run with the selected
	// suites, no flake re-run, output archived.
	TestCommand []string
	// Rollout is triggered ONLY after lease release and terminal publication.
	Rollout Trigger
	// Log is the orchestrator's logger.
	Log dlog.Surfaces
}

// Trigger is the slice of the rollout controller merge uses: the self-reload
// trigger, invoked with what landed. It is a narrow interface so merge does
// not import the rollout controller (the two never import each other).
type Trigger interface {
	// Trigger classifies the landed commits by subsystem and runs the
	// deploy-and-bounce sequence.
	Trigger(ctx context.Context, landed []gitclient.Commit) error
}

// New builds the orchestrator.
func New(deps Deps) (Orchestrator, error) {
	return nil, notimpl.Err
}

// LoadBrief reads one brief by name from dir at use time. It exists so every
// call site loads a brief the same way and fails the same way when one is
// missing.
func LoadBrief(dir, name string) (prompts.Prompt, error) {
	return prompts.Prompt{}, notimpl.Err
}
