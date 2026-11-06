package prwatch

import (
	"fmt"
	"strconv"

	"google.golang.org/protobuf/types/known/timestamppb"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

// prQueueSnapshot is a pull request's way through its merge queue, as one
// read found it.
type prQueueSnapshot struct {
	// Events is its latest merge-queue timeline events, oldest first.
	Events []remoteQueueEvent
	// Entry is its queue entry; nil when it is not in the queue.
	Entry *remoteEntryFacts
	// Position is its place, set while the queue's read holds it.
	Position *position
	// GroupChecks is the check runs of Entry's merge group commit; empty
	// while the queue has made none.
	GroupChecks []remoteCheck
}

// diffChecks answers the check runs RUNS on HEAD finished that PREVCHECKS,
// told for PREVHEAD, did not tell, and the head and checks after them. A new
// head starts its checks afresh; of several runs of one name the latest
// counts, and an unfinished one is told once it finishes.
func diffChecks(prevHead string, prevChecks map[string]string, head string, runs []remoteCheck) (string, map[string]string, []*agentreplv1.PullRequestActivityCheckFinished) {
	next := map[string]string{}
	if head == prevHead {
		next = copyMap(prevChecks)
	}
	latest := map[string]remoteCheck{}
	for _, c := range runs {
		if cur, ok := latest[c.Name]; !ok || c.ID > cur.ID {
			latest[c.Name] = c
		}
	}
	var out []*agentreplv1.PullRequestActivityCheckFinished
	for _, name := range sortedNames(latest) {
		c := latest[name]
		if !c.Completed {
			continue
		}
		value := strconv.FormatUint(c.ID, 10) + ":" + c.Conclusion
		if next[name] == value {
			continue
		}
		next[name] = value
		out = append(out, &agentreplv1.PullRequestActivityCheckFinished{Name: c.Name, Conclusion: conclusionProto(c.Conclusion)})
	}
	return head, next, out
}

// diffQueue answers what Q holds that PREV did not tell: the timed joinings
// and removals, then the move, the entry's state change and the merge
// group's finished checks, and the queue fields of the snapshot after them.
//
// A REMOVAL READ IN THE SAME READ AS THE MERGE IS NOT TOLD: the merge, told
// as the state change, is how the pull request left the queue.
func diffQueue(q prQueueSnapshot, merged bool, prev told) (events map[string]string, place, groupHead string, groupChecks map[string]string, timed []timedActivity, after []*agentreplv1.PullRequestActivity) {
	events = map[string]string{}
	entered := false
	for i, ev := range q.Events {
		kind := "added"
		if ev.Removed {
			kind = "removed"
		}
		events[ev.ID] = kind
		if _, known := prev.QueueEvents[ev.ID]; known {
			continue
		}
		switch {
		case !ev.Removed:
			entered = true
			in := &agentreplv1.PullRequestActivityMergeQueueEntered{Actor: actorProto(ev.Actor), EnteredAt: timestamppb.New(ev.At)}
			// Only the latest joining is where the pull request now stands.
			if i == len(q.Events)-1 && q.Position != nil {
				in.Position = positionProto(*q.Position)
			}
			timed = append(timed, timedActivity{ev.At, &agentreplv1.PullRequestActivity{Activity: &agentreplv1.PullRequestActivity_MergeQueueEntered{MergeQueueEntered: in}}})
		case !merged:
			timed = append(timed, timedActivity{ev.At, &agentreplv1.PullRequestActivity{Activity: &agentreplv1.PullRequestActivity_MergeQueueRemoved{MergeQueueRemoved: &agentreplv1.PullRequestActivityMergeQueueRemoved{
				Actor: actorProto(ev.Actor), Reason: ev.Reason, RemovedAt: timestamppb.New(ev.At)}}}})
		}
	}

	if q.Entry != nil && q.Position != nil {
		place = placeOf(*q.Position, q.Entry.State)
	}
	if was, ok := parsePlace(prev.Queue); ok && place != "" && !entered {
		now, _ := parsePlace(place)
		if now.pos.Ahead != was.pos.Ahead {
			after = append(after, &agentreplv1.PullRequestActivity{Activity: &agentreplv1.PullRequestActivity_MergeQueueMoved{MergeQueueMoved: &agentreplv1.PullRequestActivityMergeQueueMoved{
				From: positionProto(was.pos), To: positionProto(now.pos)}}})
		}
		if now.state != was.state {
			after = append(after, &agentreplv1.PullRequestActivity{Activity: &agentreplv1.PullRequestActivity_MergeQueueEntryStateChanged{MergeQueueEntryStateChanged: &agentreplv1.PullRequestActivityMergeQueueEntryStateChanged{
				State: entryStateProto(now.state)}}})
		}
	}

	groupHead, groupChecks = prev.GroupHead, prev.GroupChecks
	if q.Entry != nil && q.Entry.GroupHead != "" {
		var finished []*agentreplv1.PullRequestActivityCheckFinished
		groupHead, groupChecks, finished = diffChecks(prev.GroupHead, prev.GroupChecks, q.Entry.GroupHead, q.GroupChecks)
		for _, c := range finished {
			after = append(after, &agentreplv1.PullRequestActivity{Activity: &agentreplv1.PullRequestActivity_MergeGroupCheckFinished{MergeGroupCheckFinished: c}})
		}
	}
	return events, place, groupHead, groupChecks, timed, after
}

// place is a pull request's place in the queue, as told spells it.
type place struct {
	pos   position
	state string
}

// placeOf spells a place `ahead/total/state`.
func placeOf(p position, state string) string {
	return fmt.Sprintf("%d/%d/%s", p.Ahead, p.Total, state)
}

// parsePlace reads placeOf's spelling; false for an empty or unreadable one.
func parsePlace(text string) (place, bool) {
	var (
		p     place
		state string
	)
	if _, err := fmt.Sscanf(text, "%d/%d/%s", &p.pos.Ahead, &p.pos.Total, &state); err != nil {
		return place{}, false
	}
	p.state = state
	return p, true
}

// positionProto is a place as the contract spells it.
func positionProto(p position) *agentreplv1.PullRequestMergeQueuePosition {
	return &agentreplv1.PullRequestMergeQueuePosition{EntriesAhead: p.Ahead, EntriesTotal: p.Total}
}

// entryStateProto is GitHub's MergeQueueEntryState as the contract spells
// it; a state this schema does not name leaves the oneof unassigned.
func entryStateProto(state string) *agentreplv1.PullRequestMergeQueueEntryState {
	s := &agentreplv1.PullRequestMergeQueueEntryState{}
	switch state {
	case "QUEUED":
		s.State = &agentreplv1.PullRequestMergeQueueEntryState_Queued{Queued: &agentreplv1.PullRequestMergeQueueEntryStateQueued{}}
	case "AWAITING_CHECKS":
		s.State = &agentreplv1.PullRequestMergeQueueEntryState_AwaitingChecks{AwaitingChecks: &agentreplv1.PullRequestMergeQueueEntryStateAwaitingChecks{}}
	case "MERGEABLE":
		s.State = &agentreplv1.PullRequestMergeQueueEntryState_Mergeable{Mergeable: &agentreplv1.PullRequestMergeQueueEntryStateMergeable{}}
	case "UNMERGEABLE":
		s.State = &agentreplv1.PullRequestMergeQueueEntryState_Unmergeable{Unmergeable: &agentreplv1.PullRequestMergeQueueEntryStateUnmergeable{}}
	case "LOCKED":
		s.State = &agentreplv1.PullRequestMergeQueueEntryState_Locked{Locked: &agentreplv1.PullRequestMergeQueueEntryStateLocked{}}
	}
	return s
}

// actorProto is a timeline event's actor; nil when GitHub names none.
func actorProto(login string) *agentreplv1.PullRequestActivityAuthor {
	if login == "" {
		return nil
	}
	return &agentreplv1.PullRequestActivityAuthor{Login: login}
}
