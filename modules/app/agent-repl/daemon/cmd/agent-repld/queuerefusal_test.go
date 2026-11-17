package main

import (
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"google.golang.org/protobuf/proto"
)

// queueRefusals is each merge-queue verb's describer with its cases.
var queueRefusals = func() []describedRefusal {
	status := &agentreplv1.GetMergeQueueStatusError{}
	requeue := &agentreplv1.RequeueMergeError{}
	discard := &agentreplv1.DiscardMergeError{}
	update := &agentreplv1.UpdateMergeQueueError{}
	subscribe := &agentreplv1.SubscribeMergeQueueError{}
	unsubscribe := &agentreplv1.UnsubscribeMergeQueueError{}
	return []describedRefusal{
		{name: "status", empty: status, oneof: "cause",
			cases: []refusalCase{{name: "unknown_repository", err: armed(status, "unknown_repository", nil), want: "the daemon has no repository by the one -repository named"}},
			describe: func(e proto.Message) (string, error) {
				return describeQueueStatusRefusal(e.(*agentreplv1.GetMergeQueueStatusError))
			}},
		{name: "requeue", empty: requeue, oneof: "cause",
			cases: append(sharedWorkspaceCases(requeue, "", "unknown_workspace", "workspace_ref_mismatch"),
				refusalCase{name: "unknown_merge", err: armed(requeue, "unknown_merge", nil),
					want: "ws holds no merge request: none off its queue, none in line, and it never landed one"},
				refusalCase{name: "still_queued", err: armed(requeue, "still_queued", nil), want: "ws's merge stands in line already, so there is nothing to put back"},
				refusalCase{name: "branch_gone", err: armedWith(requeue, "branch_gone", map[string]any{"branch": "b"}),
					want: "the branch b that ws's merge lands no longer exists, so the merge could never run; end it with `agent-repld merge-queue discard ws`"},
				refusalCase{name: "already_landed", err: armed(requeue, "already_landed", nil), want: "ws's merge already landed, which ended its request and closed its workspace"},
				refusalCase{name: "version_increment_required", err: armed(requeue, "version_increment_required", nil),
					want: "ws's request lost its values when it was adopted and lands on a versioned main branch; requeue it with -version-part minor or patch and -changelog-line TEXT"},
				refusalCase{name: "version_increment_unexpected/unversioned_repository", err: armed(requeue, "version_increment_unexpected", &agentreplv1.MergeWorkspaceVersionIncrementUnexpected{
					Reason: &agentreplv1.MergeWorkspaceVersionIncrementUnexpected_UnversionedRepository{UnversionedRepository: &agentreplv1.MergeWorkspaceVersionIncrementUnexpectedUnversionedRepository{}}}),
					want: "ws's merge takes no version increment, as its repository's deploy config versions nothing; requeue it without -version-part or -changelog-line"},
				refusalCase{name: "version_increment_unexpected/not_default_branch", err: armed(requeue, "version_increment_unexpected", &agentreplv1.MergeWorkspaceVersionIncrementUnexpected{
					Reason: &agentreplv1.MergeWorkspaceVersionIncrementUnexpected_NotDefaultBranch{NotDefaultBranch: &agentreplv1.MergeWorkspaceVersionIncrementUnexpectedNotDefaultBranch{}}}),
					want: "ws's merge takes no version increment, as it lands into a parent workspace's checkout, not the main branch; requeue it without -version-part or -changelog-line"},
				refusalCase{name: "version_already_recorded", err: armed(requeue, "version_already_recorded", nil),
					want: "ws's request recorded its own version increment; requeue it without -version-part or -changelog-line"},
				refusalCase{name: "placement_behind_not_in_line", err: armedWith(requeue, "placement_behind_not_in_line", map[string]any{"behind": "other"}),
					want: "the merge other that ws was to go behind is not in its queue"},
				refusalCase{name: "workspace_closed", err: armed(requeue, "workspace_closed", nil),
					want: "ws was closed since it was named, and a merge request never outlives its workspace's close"},
				refusalCase{name: "not_in_catalog", err: armed(requeue, "not_in_catalog", nil),
					want: "-model names a model not in the catalog ws's login last served, or the login has served none"}),
			describe: func(e proto.Message) (string, error) {
				return describeRequeueRefusal("ws", e.(*agentreplv1.RequeueMergeError))
			}},
		{name: "discard", empty: discard, oneof: "cause",
			cases: append(sharedWorkspaceCases(discard, "", "unknown_workspace", "workspace_ref_mismatch"),
				refusalCase{name: "unknown_merge", err: armed(discard, "unknown_merge", nil), want: "no request of ws's merge stands off its queue"},
				refusalCase{name: "still_queued", err: armed(discard, "still_queued", nil),
					want: "ws's merge stands in line; take it off with `agent-repld merge-queue dequeue ws` first"}),
			describe: func(e proto.Message) (string, error) {
				return describeDiscardRefusal("ws", e.(*agentreplv1.DiscardMergeError))
			}},
		{name: "update", empty: update, oneof: "cause",
			cases: append(sharedWorkspaceCases(update, "ask again", theFourWorkspaceArms...),
				refusalCase{name: "already_paused", err: armed(update, "already_paused", nil), want: "the queue is already paused"},
				refusalCase{name: "not_paused", err: armed(update, "not_paused", nil), want: "the queue is not paused"},
				refusalCase{name: "no_such_queued_merge", err: armed(update, "no_such_queued_merge", nil), want: "no merge of ws is queued"},
				refusalCase{name: "unknown_repository", err: armed(update, "unknown_repository", nil), want: "the daemon has no repository by the one -repository named"},
				refusalCase{name: "not_mediator", err: armed(update, "not_mediator", nil), want: "ws is no repository's deploy mediator, so it makes no mediation decision"},
				refusalCase{name: "no_mediation_owed", err: armed(update, "no_mediation_owed", nil), want: "the mediator's repository owes no mediation now"},
				refusalCase{name: "no_remediation_to_restore", err: armed(update, "no_remediation_to_restore", nil),
					want: "the chain has no remediation workspace to restore yet; dispatch a fresh one instead"},
				refusalCase{name: "remediation_bound_reached",
					err:  armed(update, "remediation_bound_reached", &agentreplv1.UpdateMergeQueueRemediationBoundReached{RemediationsBeforeEscalation: 3}),
					want: "the chain has deployed 3 remediations in a row, its repository's bound, so only an escalation is accepted now"},
				refusalCase{name: "problems_unowned",
					err:  armed(update, "problems_unowned", &agentreplv1.UpdateMergeQueueProblemsUnowned{Groups: []string{"a: b (new)", "c: d (unowned)"}}),
					want: "the check's window holds problems no one owns, so it cannot be approved; dispatch or escalate them instead: a: b (new); c: d (unowned)"},
				refusalCase{name: "problems_recurring",
					err:  armed(update, "problems_recurring", &agentreplv1.UpdateMergeQueueProblemsRecurring{Groups: []string{"a: b (3 fixes)"}}),
					want: "the check's window holds problems that came back after more than two of their fixes deployed, so only an escalation is accepted: a: b (3 fixes)"}),
			describe: func(e proto.Message) (string, error) {
				return describeUpdateQueueRefusal("ws", e.(*agentreplv1.UpdateMergeQueueError))
			}},
		{name: "subscribe", empty: subscribe, oneof: "cause",
			cases: []refusalCase{
				{name: "unknown_subscriber", err: armed(subscribe, "unknown_subscriber", nil), want: "the daemon has no workspace ws"},
				{name: "unknown_repository", err: armed(subscribe, "unknown_repository", nil), want: "the daemon has no repository by the one -repository named"},
				{name: "subscriber_closed", err: armed(subscribe, "subscriber_closed", nil),
					want: "ws is closed: it runs no agent to tell, and a subscription taken while closed would never end"},
			},
			describe: func(e proto.Message) (string, error) {
				return describeQueueSubscribeRefusal("ws", e.(*agentreplv1.SubscribeMergeQueueError))
			}},
		{name: "unsubscribe", empty: unsubscribe, oneof: "cause",
			cases: []refusalCase{
				{name: "unknown_subscriber", err: armed(unsubscribe, "unknown_subscriber", nil), want: "the daemon has no workspace ws"},
				{name: "unknown_repository", err: armed(unsubscribe, "unknown_repository", nil), want: "the daemon has no repository by the one -repository named"},
			},
			describe: func(e proto.Message) (string, error) {
				return describeQueueUnsubscribeRefusal("ws", e.(*agentreplv1.UnsubscribeMergeQueueError))
			}},
	}
}()

func TestDescribeQueueRefusals(t *testing.T) {
	testDescribedRefusals(t, queueRefusals)
}
