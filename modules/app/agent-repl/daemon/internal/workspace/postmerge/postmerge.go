// Package postmerge implements merge.PostMergeHook: the child-to-parent
// handoffs that run once a workspace's merge has fully landed.
//
// Two behaviors live here, and both used to be Emacs's:
//
//   - THE PHONE-HOME. A workspace spawned from another workspace merges into
//     its PARENT's worktree, not into the repository's main checkout. When that
//     merge lands, the parent's agent session is told its child merged and what
//     the child was. Emacs used to do this with a headless `claude -p` fired
//     from its merge handler, which meant it simply did not happen when no
//     editor was watching the merge.
//   - THE POSTPROCESSING PROMPT. A workspace can be CREATED with a
//     postprocessing_prompt, a task meant to run in the parent once the child's
//     work is merged in. The Emacs handoff that fired it is gone, and until
//     this package nothing consumed the field at all.
//
// BOTH ARE COURTESY NUDGES, not the merge's correctness. The commits are on the
// target worktree before this package is ever called, and every state a
// frontend renders is already published. So a parent with no live session is a
// LOUD SKIP rather than an error: there is nothing to nudge, and spawning a
// session for a workspace nobody has open — just to tell it something it can
// read out of state — would be worse than saying so in the log.
//
// PROVENANCE. Both prompts go through the ORDINARY user prompt path
// (ParentSession.SubmitPrompt), never the merge-lease submit. The merge lease
// is held over the CHILD's session and is already released by the time this
// runs; the parent is under no lease at all, so its prompts are exactly what
// they look like — prompts submitted to a session a user may also be using.
package postmerge

import (
	"context"
	"crypto/rand"
	"encoding/hex"
	"fmt"

	"claude-repld/internal/dlog"
	"claude-repld/internal/workspace/merge"
)

// ParentSession is the notifier's outbound port onto the PARENT workspace's
// agent session.
//
// Live and SubmitPrompt are on ONE interface deliberately. They must be
// answered by the same controller fleet: checking liveness against one fleet
// and submitting to another would let a prompt lazily spawn the very session
// the liveness check said was absent, which is the one thing this package
// promises not to do.
//
// *sessioncontroller.Manager satisfies it.
type ParentSession interface {
	// Live reports whether a live session controller already exists for
	// workspace. It must CREATE NOTHING.
	Live(workspace string) bool
	// SubmitPrompt submits text to workspace's session on the ordinary user
	// prompt path.
	SubmitPrompt(ctx context.Context, workspace, requestID, text, permissionMode string) error
}

// WorktreeProbe answers the one git question the handoff turns on: is this
// merge's TARGET worktree itself a linked worktree?
//
// A child that merged into the repository's main checkout has no parent
// workspace to phone home to — the trunk is nobody's session. A child that
// merged into a linked worktree merged into ANOTHER WORKSPACE, and that
// workspace is the parent.
type WorktreeProbe interface {
	IsLinkedWorktree(ctx context.Context, dir string) (bool, error)
}

// PostprocessingSource resolves the postprocessing prompt a workspace was
// CREATED with, keyed by that workspace's worktree path.
//
// An absent record is not an error: most workspaces carry no postprocessing
// prompt, and a workspace created before the daemon owned creation has no job
// record at all. It reports ("", nil) for those and an error only when the
// records themselves could not be read.
type PostprocessingSource interface {
	PostprocessingPrompt(worktreePath string) (string, error)
}

// Config constructs a postmerge.Notifier. Every field is required: a notifier
// missing any one of them cannot perform the handoff it exists to perform, and
// a partially-wired one would silently drop exactly what the Emacs strip
// already dropped once.
type Config struct {
	// Logf receives the notifier's canonical logging.
	Logf dlog.Logf
	// Parents resolves and prompts the parent workspace's session.
	Parents ParentSession
	// Worktrees answers whether the merge target is a linked worktree.
	Worktrees WorktreeProbe
	// Postprocessing resolves the merged workspace's creation-time
	// postprocessing prompt.
	Postprocessing PostprocessingSource
}

// Notifier is the merge.PostMergeHook implementation.
type Notifier struct {
	logf           dlog.Logf
	parents        ParentSession
	worktrees      WorktreeProbe
	postprocessing PostprocessingSource
}

var _ merge.PostMergeHook = (*Notifier)(nil)

// New validates cfg and returns the notifier. Any missing dependency is a hard
// construction error.
func New(cfg Config) (*Notifier, error) {
	switch {
	case cfg.Logf == nil:
		return nil, fmt.Errorf("postmerge: Notifier Logf is required")
	case cfg.Parents == nil:
		return nil, fmt.Errorf("postmerge: Notifier Parents is required")
	case cfg.Worktrees == nil:
		return nil, fmt.Errorf("postmerge: Notifier Worktrees is required")
	case cfg.Postprocessing == nil:
		return nil, fmt.Errorf("postmerge: Notifier Postprocessing is required")
	}
	return &Notifier{
		logf:           cfg.Logf,
		parents:        cfg.Parents,
		worktrees:      cfg.Worktrees,
		postprocessing: cfg.Postprocessing,
	}, nil
}

// AfterMerged implements merge.PostMergeHook.
//
// THE PARENT WORKSPACE IS req.TargetDir. The daemon's workspace key IS the
// session's working directory, and a merge's target directory is the worktree
// the cherry-pick landed in — so when that worktree is a linked worktree, its
// path is verbatim the key of the workspace whose session owns it. Both strings
// come from the SAME frontend geometry on the merge command, so there is no
// second spelling to reconcile.
//
// Ordering is phone-home FIRST, postprocessing second: the postprocessing task
// only makes sense to an agent that has just been told what merged.
func (n *Notifier) AfterMerged(ctx context.Context, req merge.Request) error {
	// The request is NOT re-validated here. merge.Coordinator.Enqueue refuses
	// an invalid merge.Request before it ever reaches a queue, so by the time a
	// merge is MERGED its request is valid by construction — re-checking would
	// be defensive code against an invariant the merge subsystem owns.
	linked, err := n.worktrees.IsLinkedWorktree(ctx, req.TargetDir)
	if err != nil {
		n.logf("postmerge: worktree probe FAILED {ws=%s name=%s target=%s}: %v — the merge stands, but neither the phone-home nor the postprocessing prompt could be routed",
			req.Workspace, req.Name, req.TargetDir, err)
		return fmt.Errorf("postmerge: probe merge target %s for %q: %w", req.TargetDir, req.Name, err)
	}
	if !linked {
		n.logf("postmerge: NO PARENT to notify {ws=%s name=%s target=%s} — the merge landed on the repository's MAIN worktree, which is no workspace's session",
			req.Workspace, req.Name, req.TargetDir)
		return nil
	}

	parent := req.TargetDir
	if !n.parents.Live(parent) {
		n.logf("postmerge: parent has NO LIVE SESSION, handoff SKIPPED {child_ws=%s child_name=%s parent_ws=%s} — the parent can read the merge out of its own state when it comes up; this nudge is a courtesy, not the record",
			req.Workspace, req.Name, parent)
		return nil
	}

	if err := n.submit(ctx, parent, "phone_home", PhoneHomePrompt(req)); err != nil {
		n.logf("postmerge: phone-home submit FAILED {child_ws=%s child_name=%s parent_ws=%s}: %v — the merge stands and the postprocessing prompt is NOT sent, because a parent that refused the notification has no context for the task that follows it",
			req.Workspace, req.Name, parent, err)
		return fmt.Errorf("postmerge: phone home to %s for %q: %w", parent, req.Name, err)
	}
	n.logf("postmerge: phone-home DELIVERED {child_ws=%s child_name=%s parent_ws=%s branch=%s}", req.Workspace, req.Name, parent, req.SourceBranch)

	prompt, err := n.postprocessing.PostprocessingPrompt(req.SourceDir)
	if err != nil {
		n.logf("postmerge: postprocessing lookup FAILED {child_ws=%s child_name=%s child_dir=%s}: %v — the phone-home landed; only the postprocessing task did not",
			req.Workspace, req.Name, req.SourceDir, err)
		return fmt.Errorf("postmerge: resolve postprocessing prompt for %q: %w", req.Name, err)
	}
	if prompt == "" {
		n.logf("postmerge: no postprocessing prompt recorded {child_ws=%s child_name=%s child_dir=%s}", req.Workspace, req.Name, req.SourceDir)
		return nil
	}
	if err := n.submit(ctx, parent, "postprocessing", PostprocessingTaskPrompt(req, prompt)); err != nil {
		n.logf("postmerge: postprocessing submit FAILED {child_ws=%s child_name=%s parent_ws=%s}: %v — the merge and the phone-home both stand",
			req.Workspace, req.Name, parent, err)
		return fmt.Errorf("postmerge: submit postprocessing prompt to %s for %q: %w", parent, req.Name, err)
	}
	n.logf("postmerge: postprocessing prompt DELIVERED {child_ws=%s child_name=%s parent_ws=%s}", req.Workspace, req.Name, parent)
	return nil
}

// submit sends one post-merge prompt to the parent on the ordinary user path.
//
// permissionMode is empty on purpose: these prompts inherit the parent
// session's own configured mode, exactly as a prompt the user typed would.
// Choosing a mode here would silently widen (or narrow) what an agent may do
// based on nothing but who triggered its turn.
func (n *Notifier) submit(ctx context.Context, parent, kind, text string) error {
	return n.parents.SubmitPrompt(ctx, parent, newRequestID(kind), text, "")
}

// PhoneHomePrompt is the notification the PARENT's agent receives when a child
// workspace merges into its worktree.
//
// The text lives here rather than in the session controller for the same reason
// merge.ConflictResolution.Prompt lives in the merge package: what a merged
// child means for the parent's tree is post-merge knowledge, not prompt-routing
// knowledge.
func PhoneHomePrompt(req merge.Request) string {
	return fmt.Sprintf(`The child workspace %q has MERGED into the worktree you are working in (%s).

Its commits were cherry-picked from branch %s, so your working tree now carries that work. The child's own worktree was %s.

This is a notification, not a task. Take the merged work into account if it bears on what you are doing, and otherwise carry on with what you were doing.`,
		req.Name, req.TargetDir, req.SourceBranch, req.SourceDir)
}

// PostprocessingTaskPrompt frames a workspace's creation-time
// postprocessing_prompt for the parent session that runs it.
//
// The provenance header is not decoration: the parent never asked for this
// turn, so a bare task body would arrive with no indication of which child
// caused it or that the work it refers to has already landed.
func PostprocessingTaskPrompt(req merge.Request, prompt string) string {
	return fmt.Sprintf(`Post-merge task, requested when the workspace %q was created and now due because that workspace has merged into %s (from branch %s).

%s`, req.Name, req.TargetDir, req.SourceBranch, prompt)
}

// newRequestID mints the id one post-merge prompt is submitted under. A
// crypto/rand failure is not a condition to paper over with a weaker id: a
// colliding request id would reconcile one prompt's receipt against another's
// transcript line.
func newRequestID(kind string) string {
	var b [12]byte
	if _, err := rand.Read(b[:]); err != nil {
		panic(fmt.Sprintf("postmerge: crypto/rand failed minting a %s request id: %v", kind, err))
	}
	return "postmerge_" + kind + "_" + hex.EncodeToString(b[:])
}
