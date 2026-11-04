package main

import (
	"errors"
	"fmt"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

// errUndescribedRefusal is a SubmitPromptError whose arm this command has no
// sentence for: a new arm landed without one, a bug the unit tests catch.
var errUndescribedRefusal = errors.New("the daemon refused the prompt with an arm this command has no sentence for")

// describePromptRefusal says in one plain sentence why WS, as the caller
// named it, did not take the prompt. Every SubmitPromptError arm has its own
// sentence, so a caller tells a cold gate from a missing session from the
// fan-out cap without reading the protocol.
func describePromptRefusal(ws string, e *agentreplv1.SubmitPromptError) (string, error) {
	switch r := e.GetReason().(type) {
	case *agentreplv1.SubmitPromptError_Merging:
		return fmt.Sprintf("%s is being merged; send the prompt again once the merge resolves", ws), nil
	case *agentreplv1.SubmitPromptError_UnknownWorkspace:
		return sayNoWorkspace(ws), nil
	case *agentreplv1.SubmitPromptError_WorkspaceRefMismatch:
		return sayLivesElsewhere(ws, r.WorkspaceRefMismatch.GetRegistryDir(), "the prompt"), nil
	case *agentreplv1.SubmitPromptError_TransferringAway:
		return sayHandedOver(ws, r.TransferringAway.GetAddress(), "send the prompt again"), nil
	case *agentreplv1.SubmitPromptError_NotYetAdopted:
		return sayNotYetAdopted(ws, "send the prompt again"), nil
	case *agentreplv1.SubmitPromptError_FeedNotInWorkspace:
		return fmt.Sprintf("the prompt named a feed that is not %s's", ws), nil
	case *agentreplv1.SubmitPromptError_FeedUndecodable:
		return "the prompt named a feed the daemon cannot read", nil
	case *agentreplv1.SubmitPromptError_NoSession:
		return fmt.Sprintf("%s has no live session to deliver to", ws), nil
	case *agentreplv1.SubmitPromptError_DuplicateSubmission:
		return fmt.Sprintf("%s already took this prompt, so it was not sent twice", ws), nil
	case *agentreplv1.SubmitPromptError_BubbleRefused:
		b := r.BubbleRefused
		switch b.GetKind().(type) {
		case *agentreplv1.SubmitPromptBubbleRefused_NotDeliverable:
			return fmt.Sprintf("the subagent the prompt addressed in %s cannot be prompted directly (%s)", ws, b.GetDetail()), nil
		case *agentreplv1.SubmitPromptBubbleRefused_AgentBusy:
			return fmt.Sprintf("the subagent the prompt addressed in %s is already running a turn (%s)", ws, b.GetDetail()), nil
		}
		return "", fmt.Errorf("%w: bubble_refused names no kind (%s)", errUndescribedRefusal, b.GetDetail())
	case *agentreplv1.SubmitPromptError_ColdGate:
		return fmt.Sprintf("%s is parked at its cold gate (%s); the user must answer the gate before it can take a prompt", ws, r.ColdGate.GetDetail()), nil
	case *agentreplv1.SubmitPromptError_ModelNotInCatalog:
		return fmt.Sprintf("%s's session has no such model, so nothing was sent (%s)", ws, r.ModelNotInCatalog.GetDetail()), nil
	case *agentreplv1.SubmitPromptError_ModelRefused:
		return fmt.Sprintf("the vendor refused %s's model change, so nothing was sent (%s)", ws, r.ModelRefused.GetDetail()), nil
	case *agentreplv1.SubmitPromptError_AskNotStanding:
		return fmt.Sprintf("the question this answer was for in %s was already settled, so nothing was sent", ws), nil
	case *agentreplv1.SubmitPromptError_SlashCommandFromAgent:
		return fmt.Sprintf("an agent may not send %s a slash command (%s); only the user runs those", ws, r.SlashCommandFromAgent.GetCommand()), nil
	case *agentreplv1.SubmitPromptError_UnknownSender:
		return fmt.Sprintf("the sending workspace is unknown to the daemon or is %s itself, and a workspace cannot prompt itself", ws), nil
	case *agentreplv1.SubmitPromptError_ThreadAwaitingFirstAnswer:
		return fmt.Sprintf("%s's chat thread is still answering its first question; ask again once that answer lands", ws), nil
	case *agentreplv1.SubmitPromptError_FanOutRefused:
		f := r.FanOutRefused
		return fmt.Sprintf("this workspace already prompted %d unrelated workspaces in the last %s; send with -as-user only when the user asked for this send",
			f.GetCap(), windowPhrase(f.GetWindowMinutes())), nil
	case nil:
		return "", fmt.Errorf("%w: the error names no reason", errUndescribedRefusal)
	default:
		return "", fmt.Errorf("%w: %T", errUndescribedRefusal, r)
	}
}

// windowPhrase says a trailing window of MINUTES as a reader would: "hour",
// "2 hours", "45 minutes".
func windowPhrase(minutes uint32) string {
	switch {
	case minutes == 60:
		return "hour"
	case minutes > 0 && minutes%60 == 0:
		return fmt.Sprintf("%d hours", minutes/60)
	case minutes == 1:
		return "minute"
	default:
		return fmt.Sprintf("%d minutes", minutes)
	}
}
