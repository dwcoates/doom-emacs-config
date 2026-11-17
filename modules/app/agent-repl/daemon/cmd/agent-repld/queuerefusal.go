package main

import (
	"fmt"
	"strings"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

// THE MERGE-QUEUE VERBS' REFUSALS: status, requeue, discard, every update of
// a queue (dequeue, moves, pause, resume, mediation) and the queue
// subscriptions. Each describer says every arm of its verb's error in one
// plain sentence.

// noRepositoryFlag is the unknown_repository sentence of a verb whose
// -repository named it.
const noRepositoryFlag = "the daemon has no repository by the one -repository named"

// describeQueueStatusRefusal says in one plain sentence why the queues were
// not read. Every GetMergeQueueStatusError arm has its own sentence.
func describeQueueStatusRefusal(e *agentreplv1.GetMergeQueueStatusError) (string, error) {
	switch e.GetCause().(type) {
	case *agentreplv1.GetMergeQueueStatusError_UnknownRepository:
		return noRepositoryFlag, nil
	}
	return "", undescribed("reading the merge queues", e)
}

// describeRequeueRefusal says in one plain sentence why the merge of WS was
// not put back in line. Every RequeueMergeError arm has its own sentence.
func describeRequeueRefusal(ws string, e *agentreplv1.RequeueMergeError) (string, error) {
	switch r := e.GetCause().(type) {
	case *agentreplv1.RequeueMergeError_UnknownWorkspace:
		return sayNoWorkspace(ws), nil
	case *agentreplv1.RequeueMergeError_WorkspaceRefMismatch:
		return sayLivesElsewhere(ws, r.WorkspaceRefMismatch.GetRegistryDir(), "the request"), nil
	case *agentreplv1.RequeueMergeError_UnknownMerge:
		return fmt.Sprintf("%s holds no merge request: none off its queue, none in line, and it never landed one", ws), nil
	case *agentreplv1.RequeueMergeError_StillQueued:
		return fmt.Sprintf("%s's merge stands in line already, so there is nothing to put back", ws), nil
	case *agentreplv1.RequeueMergeError_BranchGone:
		return fmt.Sprintf("the branch %s that %s's merge lands no longer exists, so the merge could never run; end it with `agent-repld merge-queue discard %s`", r.BranchGone.GetBranch(), ws, ws), nil
	case *agentreplv1.RequeueMergeError_AlreadyLanded:
		return fmt.Sprintf("%s's merge already landed, which ended its request and closed its workspace", ws), nil
	case *agentreplv1.RequeueMergeError_VersionIncrementRequired:
		return fmt.Sprintf("%s's request lost its values when it was adopted and lands on a versioned main branch; requeue it with -version-part minor or patch and -changelog-line TEXT", ws), nil
	case *agentreplv1.RequeueMergeError_VersionIncrementUnexpected:
		why, err := sayVersionUnexpected(r.VersionIncrementUnexpected)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("%s's merge takes no version increment, as %s; requeue it without -version-part or -changelog-line", ws, why), nil
	case *agentreplv1.RequeueMergeError_VersionAlreadyRecorded:
		return fmt.Sprintf("%s's request recorded its own version increment; requeue it without -version-part or -changelog-line", ws), nil
	case *agentreplv1.RequeueMergeError_PlacementBehindNotInLine:
		return fmt.Sprintf("the merge %s that %s was to go behind is not in its queue", r.PlacementBehindNotInLine.GetBehind(), ws), nil
	case *agentreplv1.RequeueMergeError_WorkspaceClosed:
		return fmt.Sprintf("%s was closed since it was named, and a merge request never outlives its workspace's close", ws), nil
	case *agentreplv1.RequeueMergeError_NotInCatalog:
		return fmt.Sprintf("-model names a model not in the catalog %s's login last served, or the login has served none", ws), nil
	}
	return "", undescribed("requeueing "+ws, e)
}

// describeDiscardRefusal says in one plain sentence why the merge of WS was
// not discarded. Every DiscardMergeError arm has its own sentence.
func describeDiscardRefusal(ws string, e *agentreplv1.DiscardMergeError) (string, error) {
	switch r := e.GetCause().(type) {
	case *agentreplv1.DiscardMergeError_UnknownWorkspace:
		return sayNoWorkspace(ws), nil
	case *agentreplv1.DiscardMergeError_WorkspaceRefMismatch:
		return sayLivesElsewhere(ws, r.WorkspaceRefMismatch.GetRegistryDir(), "the request"), nil
	case *agentreplv1.DiscardMergeError_UnknownMerge:
		return fmt.Sprintf("no request of %s's merge stands off its queue", ws), nil
	case *agentreplv1.DiscardMergeError_StillQueued:
		return fmt.Sprintf("%s's merge stands in line; take it off with `agent-repld merge-queue dequeue %s` first", ws, ws), nil
	}
	return "", undescribed("discarding "+ws, e)
}

// describeUpdateQueueRefusal says in one plain sentence why a queue was not
// changed. WS is the workspace the update names: the merge it moves or
// dequeues, or the requester for a pause, a resume or a mediation. Every
// UpdateMergeQueueError arm has its own sentence.
func describeUpdateQueueRefusal(ws string, e *agentreplv1.UpdateMergeQueueError) (string, error) {
	switch r := e.GetCause().(type) {
	case *agentreplv1.UpdateMergeQueueError_UnknownWorkspace:
		return sayNoWorkspace(ws), nil
	case *agentreplv1.UpdateMergeQueueError_WorkspaceRefMismatch:
		return sayLivesElsewhere(ws, r.WorkspaceRefMismatch.GetRegistryDir(), "the request"), nil
	case *agentreplv1.UpdateMergeQueueError_TransferringAway:
		return sayHandedOver(ws, r.TransferringAway.GetAddress(), "ask again"), nil
	case *agentreplv1.UpdateMergeQueueError_NotYetAdopted:
		return sayNotYetAdopted(ws, "ask again"), nil
	case *agentreplv1.UpdateMergeQueueError_AlreadyPaused:
		return "the queue is already paused", nil
	case *agentreplv1.UpdateMergeQueueError_NotPaused:
		return "the queue is not paused", nil
	case *agentreplv1.UpdateMergeQueueError_NoSuchQueuedMerge:
		return fmt.Sprintf("no merge of %s is queued", ws), nil
	case *agentreplv1.UpdateMergeQueueError_UnknownRepository:
		return noRepositoryFlag, nil
	case *agentreplv1.UpdateMergeQueueError_NotMediator:
		return fmt.Sprintf("%s is no repository's deploy mediator, so it makes no mediation decision", ws), nil
	case *agentreplv1.UpdateMergeQueueError_NoMediationOwed:
		return "the mediator's repository owes no mediation now", nil
	case *agentreplv1.UpdateMergeQueueError_NoRemediationToRestore:
		return "the chain has no remediation workspace to restore yet; dispatch a fresh one instead", nil
	case *agentreplv1.UpdateMergeQueueError_RemediationBoundReached:
		return fmt.Sprintf("the chain has deployed %d remediations in a row, its repository's bound, so only an escalation is accepted now",
			r.RemediationBoundReached.GetRemediationsBeforeEscalation()), nil
	case *agentreplv1.UpdateMergeQueueError_ProblemsUnowned:
		return fmt.Sprintf("the check's window holds problems no one owns, so it cannot be approved; dispatch or escalate them instead: %s",
			strings.Join(r.ProblemsUnowned.GetGroups(), "; ")), nil
	case *agentreplv1.UpdateMergeQueueError_ProblemsRecurring:
		return fmt.Sprintf("the check's window holds problems that came back after more than two of their fixes deployed, so only an escalation is accepted: %s",
			strings.Join(r.ProblemsRecurring.GetGroups(), "; ")), nil
	}
	return "", undescribed("updating the merge queue", e)
}

// describeQueueSubscribeRefusal says in one plain sentence why SUBSCRIBER was
// not subscribed to the merge queues. Every SubscribeMergeQueueError arm has
// its own sentence.
func describeQueueSubscribeRefusal(subscriber string, e *agentreplv1.SubscribeMergeQueueError) (string, error) {
	switch e.GetCause().(type) {
	case *agentreplv1.SubscribeMergeQueueError_UnknownSubscriber:
		return sayNoWorkspace(subscriber), nil
	case *agentreplv1.SubscribeMergeQueueError_UnknownRepository:
		return noRepositoryFlag, nil
	case *agentreplv1.SubscribeMergeQueueError_SubscriberClosed:
		return saySubscriberClosed(subscriber), nil
	}
	return "", undescribed("subscribing "+subscriber+" to the merge queues", e)
}

// describeQueueUnsubscribeRefusal says in one plain sentence why SUBSCRIBER
// was not unsubscribed from the merge queues. Every
// UnsubscribeMergeQueueError arm has its own sentence.
func describeQueueUnsubscribeRefusal(subscriber string, e *agentreplv1.UnsubscribeMergeQueueError) (string, error) {
	switch e.GetCause().(type) {
	case *agentreplv1.UnsubscribeMergeQueueError_UnknownSubscriber:
		return sayNoWorkspace(subscriber), nil
	case *agentreplv1.UnsubscribeMergeQueueError_UnknownRepository:
		return noRepositoryFlag, nil
	}
	return "", undescribed("unsubscribing "+subscriber+" from the merge queues", e)
}
