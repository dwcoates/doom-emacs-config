package merge

import (
	"context"
	"fmt"

	"claude-repld/internal/dlog"
	"claude-repld/internal/prompts"
)

// This file is merge.Coordinator's outbound port onto the AGENT that owns the
// work whose rebase broke the target's test suite. It is the sibling of
// merge.ConflictResolver (conflictresolver.go) and follows the same shape for
// the same reasons: the party best placed to fix a suite the merge just broke
// is the session that wrote the commits, and the coordinator is already holding
// that session's lease.
//
// The two ports are deliberately NOT one port with a mode flag. What the agent
// is told to do differs in the one respect that matters: a conflict resolution
// must stop before committing (the daemon resumes a still-paused cherry-pick),
// while a test fix must NOT stop before staging (the daemon commits the staged
// fix as a follow-up commit). Collapsing them would make the prompt text
// conditional on a flag, which is exactly where the instruction and the
// machinery it describes drift apart.

// testFailureTailPromptBytes bounds how much of the failing suite's output goes
// into the resolution prompt. The tail is already clamped by the suite runner;
// this is the second, tighter clamp for the prompt specifically.
const testFailureTailPromptBytes = 4000

// TestFailureResolution is the fact set merge.Coordinator hands the resolver
// when a cherry-picked commit leaves the target's test suite failing.
type TestFailureResolution struct {
	// Workspace is the daemon's WORKSPACE KEY (Request.Workspace) — the session
	// whose shim is asked to fix the suite, and the key the merge lease is held
	// on.
	Workspace string
	// RequestID is the id the resolution prompt is submitted under, minted per
	// attempt.
	RequestID string
	// FailingCommit is the short SHA of the commit whose cherry-pick left the
	// suite failing.
	FailingCommit string
	// SourceBranch is the branch that commit is being replayed from.
	SourceBranch string
	// TargetDir is the worktree the agent fixes IN, which is the temporary
	// REBASE WORKTREE rather than the merge target: the failing commit was
	// replayed there and the target carries nothing of this merge yet. The name
	// is kept because the prompt's {{target_dir}} placeholder is user-editable
	// text this field fills.
	TargetDir string
	// FailureTail is the clamped tail of the failing suite's output.
	FailureTail string
}

func (r TestFailureResolution) validate() error {
	switch {
	case r.Workspace == "":
		return fmt.Errorf("merge: test failure resolution Workspace is required")
	case r.RequestID == "":
		return fmt.Errorf("merge: test failure resolution RequestID is required")
	case r.FailingCommit == "":
		return fmt.Errorf("merge: test failure resolution FailingCommit is required")
	case r.SourceBranch == "":
		return fmt.Errorf("merge: test failure resolution SourceBranch is required")
	case r.TargetDir == "":
		return fmt.Errorf("merge: test failure resolution TargetDir is required")
	case r.FailureTail == "":
		return fmt.Errorf("merge: test failure resolution FailureTail is required")
	}
	return nil
}

// TestFailurePromptFile is the prompt file this resolution's brief is read
// from.
//
// THE TEXT MOVED TO prompts/ so it can be customized without a rebuild, and
// this constant still binds it to the machinery, for the same reason
// merge.ConflictPromptFile does: what the agent may do with a half-landed merge
// is merge-subsystem knowledge. The daemon commits the staged fix itself as a
// FOLLOW-UP commit, so an agent that committed, amended, or reset would either
// duplicate that commit or rewrite the very SHA the already-incorporated probe
// keys the rest of the replay on.
const TestFailurePromptFile = "merge-test-failure-resolve.md"

// Prompt is the instruction the resolving agent receives, read from
// prompts/TestFailurePromptFile at EVERY use.
//
// An error means no prompt could be composed and the caller fails the fix
// attempt. It never degrades to a partial prompt: the failing output is the
// whole reason the agent can act at all, so a brief missing it would spend the
// single permitted attempt on a guess.
func (r TestFailureResolution) Prompt() (string, error) {
	return prompts.Render(TestFailurePromptFile, map[string]string{
		"failing_commit": r.FailingCommit,
		"source_branch":  r.SourceBranch,
		"target_dir":     r.TargetDir,
		"failure_tail":   dlog.Clamp(r.FailureTail, testFailureTailPromptBytes),
	})
}

// TestFailureResolver drives the merging workspace's OWN shim to fix a test
// suite the merge broke.
//
// The implementation is a thin adapter over the session controller's
// merge-lease submit path — the same one merge.ConflictResolver reaches — so
// the fix necessarily runs on the session the lease was taken over.
type TestFailureResolver interface {
	// Resolve submits the fix prompt and returns ONCE THE RESOLUTION TURN HAS
	// COMPLETED. merge.Coordinator commits the staged fix and re-runs the suite
	// the instant Resolve returns nil, so returning early would test a tree the
	// agent is still editing.
	//
	// An error means no fix happened. The coordinator does not retry: it fails
	// the merge, and the target — which this merge has not touched — stays
	// exactly as it was.
	//
	// IT MAY FAIL FAST, and the coordinator must not read a quick error as a
	// mistake. The turn has to START before the long wait for it to finish
	// means anything: a prompt that produced no turn — because the workspace is
	// busy with a turn of the user's own, or because the shim never began one —
	// fails within a short bind bound rather than holding the merge (and the
	// workspace's shim lease) for the whole of a window sized for an agent that
	// is actually working. The cause names which of those it was.
	Resolve(ctx context.Context, res TestFailureResolution) error
}

// newTestFixRequestID mints the id one test-fix attempt is submitted under. It
// shares newResolutionRequestID's rationale (a colliding id would reconcile one
// turn's receipt against another's transcript line) and differs only in prefix,
// so the two kinds of merge-driven turn are told apart in the durable record.
func newTestFixRequestID() string {
	return "merge_testfix_" + randomIDSuffix()
}
