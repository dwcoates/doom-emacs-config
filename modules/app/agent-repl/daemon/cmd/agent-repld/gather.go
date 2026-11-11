package main

import (
	"errors"
	"fmt"
	"strings"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/resolve/feed"
)

// workspaceGather asks every workspace -workspaces names the same question on
// the calling workspace's behalf (agentrepl.v1.GatherWorkspaces). The answers
// come back to the calling workspace as notices, never on this command's
// output: the first update once every target answered or the deadline
// passed, then each later answer.
func workspaceGather(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	workspaces := fs.String("workspaces", "", "the targets, by workspace name, comma-separated")
	prompt := fs.String("prompt", "", "the prompt every target is sent")
	asUser := fs.Bool("as-user", false, "the user explicitly asked for this gather: lifts the fan-out cap")
	rest, ok := c.parse(fs, args)
	if !ok {
		return exitFailure
	}
	names := gatherTargetNames(*workspaces)
	if len(rest) != 0 || len(names) == 0 || strings.TrimSpace(*prompt) == "" {
		return c.usageFailed("the command names its targets with -workspaces and its prompt with -prompt, and nothing positional")
	}
	if !c.dial(*stateDir) {
		return exitFailure
	}
	// THE CALLER IS THE CALLING WORKSPACE: the targets' feeds head the prompt
	// with it, and every notice goes to it.
	caller, ok := c.calling()
	if !ok {
		return exitFailure
	}
	key, err := c.deps.mintKey("gather")
	if err != nil {
		c.failed("%v", err)
		return exitFailure
	}
	resp, err := c.client.GatherWorkspaces(c.ctx, connect.NewRequest(&agentreplv1.GatherWorkspacesRequest{
		Caller: caller, TargetNames: names, Said: feed.SaidText(*prompt), AsUser: *asUser, IdempotencyKey: key,
	}))
	code := answerDescribed(c, resp, err, func(msg *agentreplv1.GatherWorkspacesResponse) (string, error) {
		return describeGatherRefusal(msg.GetError())
	})
	if code == exitSuccess && err == nil && resp != nil {
		// A GATHER MADE WITH SOME TARGETS REFUSED still succeeds; each refusal
		// is said, so the caller knows who will never answer.
		for _, r := range resp.Msg.GetSuccess().GetRefused() {
			line, err := describeGatherTargetRefusal(r)
			if err != nil {
				c.failed("%v", err)
				return exitFailure
			}
			c.noted("%s", line)
		}
	}
	return code
}

// gatherTargetNames splits -workspaces into its names, dropping blanks.
func gatherTargetNames(flag string) []string {
	var out []string
	for _, name := range strings.Split(flag, ",") {
		if name = strings.TrimSpace(name); name != "" {
			out = append(out, name)
		}
	}
	return out
}

// workspaceGatherStatus prints one gather's answers and outstanding targets
// (agentrepl.v1.GetGatherStatus).
func workspaceGatherStatus(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	rest, ok := c.parse(fs, args)
	if !ok {
		return exitFailure
	}
	if len(rest) != 1 || strings.TrimSpace(rest[0]) == "" {
		return c.usageFailed("the command names the gather, once")
	}
	if !c.dial(*stateDir) {
		return exitFailure
	}
	resp, err := c.client.GetGatherStatus(c.ctx, connect.NewRequest(&agentreplv1.GetGatherStatusRequest{Gather: &agentreplv1.GatherRef{Id: rest[0]}}))
	return answerDescribed(c, resp, err, func(msg *agentreplv1.GetGatherStatusResponse) (string, error) {
		return describeGatherStatusRefusal(rest[0], msg.GetError())
	})
}

// errUndescribedGatherRefusal is a gather refusal arm this command has no
// sentence for: a new arm landed without one, a bug the unit tests catch.
var errUndescribedGatherRefusal = errors.New("the daemon refused the gather with an arm this command has no sentence for")

// describeGatherRefusal says in one plain sentence why no gather was made.
func describeGatherRefusal(e *agentreplv1.GatherWorkspacesError) (string, error) {
	switch cause := e.GetCause().(type) {
	case *agentreplv1.GatherWorkspacesError_UnknownCaller:
		return "this workspace is unknown to the daemon, so it cannot gather", nil
	case *agentreplv1.GatherWorkspacesError_CallerClosed:
		return "this workspace is closed, so no answer could reach it", nil
	case *agentreplv1.GatherWorkspacesError_NoneAccepted:
		lines := make([]string, 0, len(cause.NoneAccepted.GetRefused()))
		for _, r := range cause.NoneAccepted.GetRefused() {
			line, err := describeGatherTargetRefusal(r)
			if err != nil {
				return "", err
			}
			lines = append(lines, line)
		}
		return "no target took the prompt, so no gather was made: " + strings.Join(lines, "; "), nil
	case nil:
		return "", fmt.Errorf("%w: the error names no cause", errUndescribedGatherRefusal)
	default:
		return "", fmt.Errorf("%w: %T", errUndescribedGatherRefusal, cause)
	}
}

// describeGatherTargetRefusal says in one plain sentence why one target was
// left out of the gather. A target the submission refused is said as
// `workspace prompt` says it.
func describeGatherTargetRefusal(r *agentreplv1.GatherRefusedTarget) (string, error) {
	name := r.GetName()
	switch cause := r.GetCause().GetCause().(type) {
	case *agentreplv1.GatherRefusal_UnknownWorkspace:
		return sayNoWorkspace(name), nil
	case *agentreplv1.GatherRefusal_Closed:
		return fmt.Sprintf("%s is closed, so it is not gathered from", name), nil
	case *agentreplv1.GatherRefusal_IsCaller:
		return fmt.Sprintf("%s is this workspace, which never gathers from itself", name), nil
	case *agentreplv1.GatherRefusal_NeedsInput:
		return fmt.Sprintf("%s has a question or permission waiting on the user, so it is not gathered from", name), nil
	case *agentreplv1.GatherRefusal_Merging:
		return fmt.Sprintf("%s is being merged, so it is not gathered from", name), nil
	case *agentreplv1.GatherRefusal_AmbiguousName:
		dirs := make([]string, 0, len(cause.AmbiguousName.GetWorkspaces()))
		for _, w := range cause.AmbiguousName.GetWorkspaces() {
			dirs = append(dirs, w.GetDir())
		}
		return fmt.Sprintf("several open workspaces are named %s (%s), so it is not gathered from", name, strings.Join(dirs, ", ")), nil
	case *agentreplv1.GatherRefusal_PromptRefused:
		return describePromptRefusal(name, cause.PromptRefused)
	case nil:
		return "", fmt.Errorf("%w: target %s names no cause", errUndescribedGatherRefusal, name)
	default:
		return "", fmt.Errorf("%w: %T", errUndescribedGatherRefusal, cause)
	}
}

// describeGatherStatusRefusal says in one plain sentence why GATHER could not
// be read.
func describeGatherStatusRefusal(gather string, e *agentreplv1.GetGatherStatusError) (string, error) {
	switch cause := e.GetCause().(type) {
	case *agentreplv1.GetGatherStatusError_UnknownGather:
		return fmt.Sprintf("the daemon has no gather %s", gather), nil
	case nil:
		return "", fmt.Errorf("%w: the error names no cause", errUndescribedGatherRefusal)
	default:
		return "", fmt.Errorf("%w: %T", errUndescribedGatherRefusal, cause)
	}
}
