package merge

import (
	"context"
	"crypto/rand"
	"encoding/hex"
	"fmt"

	"claude-repld/internal/prompts"
)

// This file is merge.Coordinator's outbound port onto the AGENT that owns the
// conflicted work.
//
// A conflicted cherry-pick used to sit parked until a human resolved it by
// hand. But the party best placed to resolve it is not the daemon and not
// (immediately) a person: it is the workspace's OWN agent session, which holds
// the context of the very commits that conflicted. So the coordinator drives
// that session, once, and falls back to the human path only when the shim
// attempt does not finish the job.
//
// The port is an interface rather than a direct call because the implementation
// lives in the session controller, and the merge subsystem must not import it:
// merge is the subsystem the session controller's lease serves, not the other
// way round.

// ConflictResolution is the fact set merge.Coordinator hands the resolver when
// a cherry-pick parks. It carries everything the resolving turn needs to know
// what to fix and where, plus the request id its prompt is submitted under.
type ConflictResolution struct {
	// Workspace is the daemon's WORKSPACE KEY (Request.Workspace) — the session
	// whose shim is asked to resolve, and the key the merge lease is held on.
	Workspace string
	// RequestID is the id the resolution prompt is submitted under. It is what
	// the prompt receipt and the durable transcript line reconcile on, so it is
	// minted per attempt rather than reused across merges.
	RequestID string
	// ConflictCommit is the short SHA of the commit whose cherry-pick
	// conflicted (CHERRY_PICK_HEAD), as merge.Driver reported it.
	ConflictCommit string
	// SourceBranch is the branch the conflicted commit is being replayed from.
	SourceBranch string
	// TargetDir is the worktree holding the paused cherry-pick — the tree the
	// agent resolves in.
	TargetDir string
}

func (r ConflictResolution) validate() error {
	switch {
	case r.Workspace == "":
		return fmt.Errorf("merge: conflict resolution Workspace is required")
	case r.RequestID == "":
		return fmt.Errorf("merge: conflict resolution RequestID is required")
	case r.ConflictCommit == "":
		return fmt.Errorf("merge: conflict resolution ConflictCommit is required")
	case r.SourceBranch == "":
		return fmt.Errorf("merge: conflict resolution SourceBranch is required")
	case r.TargetDir == "":
		return fmt.Errorf("merge: conflict resolution TargetDir is required")
	}
	return nil
}

// ConflictPromptFile is the prompt file this resolution's brief is read from.
//
// THE TEXT MOVED TO prompts/ so it can be customized without a rebuild, and
// this constant is what still binds the instruction to the machinery it
// describes. What the agent may and may not do with a paused cherry-pick is
// merge-subsystem knowledge: the coordinator resumes the pick itself
// (picker.Resume runs `git add -u` + `git cherry-pick --continue`), so an agent
// that continues or commits on its own would leave the coordinator resuming a
// pick that is no longer paused — merge.Driver.Resume fails loudly on exactly
// that. The file's header names this call site so whoever edits the text can
// find the machinery behind it.
const ConflictPromptFile = "merge-conflict-resolve.md"

// Prompt is the instruction the resolving agent receives, read from
// prompts/ConflictPromptFile at EVERY use so an edit takes effect on the next
// conflict without a rebuild or a daemon restart.
//
// An error means no prompt could be composed — the file is missing, unreadable,
// or its placeholders no longer match this call site. The caller fails the
// resolution rather than sending an approximation: an agent pointed at the
// wrong worktree, or handed a literal "{{target_dir}}", would corrupt the very
// merge it was summoned to repair.
func (r ConflictResolution) Prompt() (string, error) {
	return prompts.Render(ConflictPromptFile, map[string]string{
		"conflict_commit": r.ConflictCommit,
		"source_branch":   r.SourceBranch,
		"target_dir":      r.TargetDir,
	})
}

// ConflictResolver drives the ORIGINAL workspace's own shim to resolve a merge
// conflict merge.Coordinator has parked.
//
// The implementation is a thin adapter over the session controller's
// merge-lease submit path; the coordinator already holds that lease when it
// calls, which is what makes the submit admissible at all.
type ConflictResolver interface {
	// Resolve submits the resolution prompt and returns ONCE THE RESOLUTION
	// TURN HAS COMPLETED. That is the whole contract: the coordinator resumes
	// the cherry-pick the instant Resolve returns nil, and resuming while the
	// agent is still editing would continue a half-staged tree.
	//
	// An error means no resolution happened — the lease was not held, no live
	// session took the prompt, the submit was refused, or the turn never
	// finished. The coordinator leaves the conflict parked for a human; it
	// never treats a failed Resolve as a merged outcome.
	Resolve(ctx context.Context, res ConflictResolution) error
}

// newResolutionRequestID mints the id one resolution attempt is submitted
// under. crypto/rand failing is not a condition to paper over with a weaker id:
// a colliding request id would reconcile one turn's receipt against another's
// transcript line.
func newResolutionRequestID() string {
	return "merge_resolve_" + randomIDSuffix()
}

// randomIDSuffix is the shared entropy behind every merge-driven request id
// (conflict resolution and test-fix alike). crypto/rand failing is not a
// condition to paper over with a weaker id, so it panics rather than degrade.
func randomIDSuffix() string {
	var b [12]byte
	if _, err := rand.Read(b[:]); err != nil {
		panic(fmt.Sprintf("merge: crypto/rand failed minting a merge request id: %v", err))
	}
	return hex.EncodeToString(b[:])
}
