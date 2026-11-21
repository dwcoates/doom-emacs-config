package main

import (
	"errors"
	"fmt"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

// THE SENTENCES EVERY WORKSPACE VERB'S REFUSAL SHARES. Every verb that
// addresses a workspace can be refused for the same four reasons: the id is
// unknown, the echoed dir disagrees, or a restarting daemon has handed the
// workspace on or not yet taken it over. Each describer says them through
// these, so a caller reads one wording for one cause whichever verb it ran.
// REQUESTED names what the request named ("the prompt", "the request"), and
// AGAIN is what to do once the restart allows ("send the prompt again").

// sayNoWorkspace is the unknown_workspace sentence.
func sayNoWorkspace(ws string) string { return fmt.Sprintf("the daemon has no workspace %s", ws) }

// sayLivesElsewhere is the workspace_ref_mismatch sentence.
func sayLivesElsewhere(ws, registryDir, requested string) string {
	return fmt.Sprintf("%s lives at %s, not at the directory %s named", ws, registryDir, requested)
}

// sayHandedOver is the transferring_away sentence.
func sayHandedOver(ws, successor, again string) string {
	return fmt.Sprintf("the daemon is restarting and handed %s to its successor at %s; %s", ws, successor, again)
}

// sayNotYetAdopted is the not_yet_adopted sentence.
func sayNotYetAdopted(ws, again string) string {
	return fmt.Sprintf("the daemon is restarting and has not taken %s over yet; %s once the restart finishes", ws, again)
}

// sayBehindNotInLine is the placement_behind_not_in_line sentence of every
// verb that places WS's merge behind another: BEHIND names no merge in line on
// its queue.
func sayBehindNotInLine(ws string, e *agentreplv1.MergeQueuePlacementBehindNotInLine) string {
	return fmt.Sprintf("the merge %s that %s was to go behind is not in its queue", e.GetBehind(), ws)
}

// errUndescribedArm is a refusal arm, or a fact inside one, that the command
// has no sentence for: a new arm landed without one, a bug the unit tests
// catch.
var errUndescribedArm = errors.New("the daemon refused with an arm this command has no sentence for")

// undescribed is errUndescribedArm for VERB's refusal E.
func undescribed(verb string, e any) error {
	return fmt.Errorf("%w: %s: %v", errUndescribedArm, verb, e)
}

// sayOpenChildren is the has_open_children sentence of every verb but finish,
// AGAIN being what to do once they are gone ("close it again").
func sayOpenChildren(ws string, e *agentreplv1.WorkspaceHasOpenChildren, again string) string {
	return fmt.Sprintf("%s has %d open children and %d open workspaces landing into it, hidden ones included; close or merge each, then %s",
		ws, len(e.Children), len(e.LandingInto), again)
}

// sayParentNotOpen is the parent_not_open sentence, AGAIN being what to do
// once the parent is open ("open it again").
func sayParentNotOpen(e *agentreplv1.WorkspaceParentNotOpen, again string) (string, error) {
	parent := e.GetParent().GetDir()
	switch e.GetState().(type) {
	case *agentreplv1.WorkspaceParentNotOpen_Hidden:
		return fmt.Sprintf("the parent %s is hidden; show it, then %s", parent, again), nil
	case *agentreplv1.WorkspaceParentNotOpen_Closed:
		return fmt.Sprintf("the parent %s is closed; reopen it, then %s", parent, again), nil
	case *agentreplv1.WorkspaceParentNotOpen_Closing:
		return fmt.Sprintf("the parent %s closes once its queued merge lands, so it takes no open child", parent), nil
	}
	return "", undescribed("parent_not_open", e)
}

// sayInvalidName is the invalid_name sentence, one per rule the name broke:
// its last component is no slug, a component before it is no prefix, or it
// lies in the daemon's own `agent-repl` branch namespace. A create and a
// rename refuse a name by the same message, so both say it here.
func sayInvalidName(n *agentreplv1.CreateWorkspaceInvalidName) (string, error) {
	switch n.GetRule().(type) {
	case *agentreplv1.CreateWorkspaceInvalidName_Slug:
		return fmt.Sprintf("the name %q breaks the naming rule: its last component %q is not a slug", n.GetName(), n.GetComponent()), nil
	case *agentreplv1.CreateWorkspaceInvalidName_Prefix:
		return fmt.Sprintf("the name %q breaks the naming rule: its component %q before the last is not a prefix", n.GetName(), n.GetComponent()), nil
	case *agentreplv1.CreateWorkspaceInvalidName_Reserved:
		return fmt.Sprintf("the name %q lies in the daemon's own branch namespace %q, which no workspace's name may use", n.GetName(), n.GetComponent()), nil
	}
	return "", undescribed("invalid_name", n)
}

// sayVersionUnexpected says why a merge that carries a version increment
// takes none.
func sayVersionUnexpected(e *agentreplv1.MergeWorkspaceVersionIncrementUnexpected) (string, error) {
	switch e.GetReason().(type) {
	case *agentreplv1.MergeWorkspaceVersionIncrementUnexpected_UnversionedRepository:
		return "its repository's deploy config versions nothing", nil
	case *agentreplv1.MergeWorkspaceVersionIncrementUnexpected_NotDefaultBranch:
		return "it lands into a parent workspace's checkout, not the main branch", nil
	}
	return "", undescribed("version_increment_unexpected", e)
}

// sayGithubFailure says why GitHub did not answer a read or a post.
func sayGithubFailure(f *agentreplv1.PullRequestGitHubFailure) (string, error) {
	switch c := f.GetCause().(type) {
	case *agentreplv1.PullRequestGitHubFailure_Unauthorized:
		return "GitHub refused the gh login's credentials; log in again with `gh auth login`", nil
	case *agentreplv1.PullRequestGitHubFailure_NotFound:
		return "GitHub answered not found: it was deleted, renamed, or never readable to the gh login", nil
	case *agentreplv1.PullRequestGitHubFailure_RateLimited:
		if at := c.RateLimited.GetResetsAt(); at != nil {
			return fmt.Sprintf("GitHub's rate limit is spent until %s", at.AsTime().UTC().Format(time.RFC3339)), nil
		}
		return "GitHub's rate limit is spent until it resets", nil
	case *agentreplv1.PullRequestGitHubFailure_Unreachable:
		return fmt.Sprintf("GitHub could not be reached, or answered a server error (%s)", c.Unreachable.GetDetail()), nil
	case *agentreplv1.PullRequestGitHubFailure_Refused:
		return fmt.Sprintf("GitHub refused it with %d (%s)", c.Refused.GetHttpStatus(), c.Refused.GetMessage()), nil
	}
	return "", undescribed("GitHub failure", f)
}

// saySubscriberClosed is the subscriber_closed sentence every subscription
// verb shares.
func saySubscriberClosed(subscriber string) string {
	return fmt.Sprintf("%s is closed: it runs no agent to tell, and a subscription taken while closed would never end", subscriber)
}

// sayNotOnGithub is the not_on_github sentence every GitHub subscription verb
// shares: SUBSCRIBER's repository has ORIGIN, which is not on github.com.
func sayNotOnGithub(subscriber, origin string) string {
	return fmt.Sprintf("%s's repository is not on github.com (its origin is %s)", subscriber, origin)
}
