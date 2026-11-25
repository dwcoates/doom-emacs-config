package sidebar

import (
	"fmt"
	"slices"

	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
	"agentrepl/protohelpers/rosterwalk"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/repodivergence"
	"agent-repld/internal/wsm"
)

// MergedSectionLabel is the Recently Merged section's heading (owner
// request, 2026-11-01; it was "Deploys", which drew the queues too). It has
// one author for the same reason every other drawn string does.
const MergedSectionLabel = "Recently Merged"

// MergeQueueSectionLabel is the Merge Queue section's heading (owner
// request, 2026-11-01).
const MergeQueueSectionLabel = "Merge Queue"

// HiddenSectionLabel is the hidden-workspaces section's heading. The section
// is drawn as the BACKLOG (owner request, 2026-10-26).
const HiddenSectionLabel = "Backlog"

// OpenSectionLabel is the open-workspaces section's heading.
const OpenSectionLabel = "Open"

// repositoryView resolves the Open section from the Open tree (openTree):
// ONE SECTION PER REPOSITORY in the arrangement's order, its body its walk's
// entries (its rows in no subsection and its subsections, interleaved as
// arranged), each open row carrying its tab
// numbered in this very walk (tabNumberer), so the sidebar's numbers and the
// tab bar's are one fact.
//
// A repository with no rows still draws a section, and a subsection with no
// rows still draws: both are registered, and a section that vanished when its
// last workspace left would make the grouping flicker (and leave an empty
// subsection nowhere to drag a workspace into).
func (r *resolver) repositoryView(repos []openRepo, orphans map[ids.RepoID][]wsm.Workspace, rc rowContext, log dlog.Logger) *frontendv1.RosterRepositoryView {
	current := r.selectedWorkspace()
	tabs := tabsOf(repos)
	filed := wsm.SectionRepos(r.state.reg.Workspaces)
	out := &frontendv1.RosterRepositoryView{}
	numbers := &tabNumberer{}
	shown := &frontendv1.RosterOpenCount{}
	for _, placed := range repos {
		repoLog := log.With(dlog.Context{"repo_id": string(placed.repo.ID)})
		section := &frontendv1.RosterRepoSection{
			Key:        repoKey(placed.repo),
			Divergence: repoDivergence(r.state.divergence[placed.repo.ID]),
		}
		count := &frontendv1.RosterOpenCount{}
		for _, region := range placed.regions {
			regionLog := repoLog
			if region.subsection != nil {
				regionLog = repoLog.With(dlog.Context{"subsection": string(region.subsection.ID)})
			}
			rows := r.openRows(region.rows, region.folded(placed.repo), numbers, rc, regionLog)
			regionCount := openCount(rows.GetRows())
			addOpenCount(count, regionCount)
			if region.subsection == nil {
				section.Entries = append(section.Entries, rosterwalk.RowEntries(rows.GetRows())...)
				continue
			}
			sub := &frontendv1.RosterSubsection{
				Kind: &frontendv1.RosterSubsection_Named{Named: &frontendv1.RosterSubsectionNamed{
					Key: &frontendv1.RosterSubsectionKey{SubsectionId: string(region.subsection.ID)},
				}},
				Header:   openHeader(region.subsection.Name, regionCount),
				OpenRows: rows,
			}
			setSubsectionFold(sub, region.subsection.Folded, holdsCurrent(r.state.reg, tabs, current, wsm.SubsectionMembers(region.subsection.ID)))
			section.Entries = append(section.Entries, &frontendv1.RosterRepoEntry{Entry: &frontendv1.RosterRepoEntry_Subsection{Subsection: sub}})
		}
		addOpenCount(shown, count)
		section.Header = openHeader(placed.repo.Name, count)
		setRepoFold(section, placed.repo.Folded, holdsCurrent(r.state.reg, tabs, current, wsm.RepositoryMembers(filed, placed.repo.ID)))
		out.Sections = append(out.Sections, section)
	}
	r.assertRepositoryInvariant(orphans, "daemon.sidebar.repository_view", "the repo grouping", log)
	out.OpenCount = shown
	return out
}

// openCount counts ROWS, a rows region's top-level open rows, as every count
// in the Open section does (frontendv1.RosterOpenCount): every row of their
// walk, nested family rows included, and the ones among them no fold hides,
// which are exactly the rows whose tab this walk numbered. A row without a
// tab arm is a resolver that skipped its numbering, and panics.
func openCount(rows []*frontendv1.RosterOpenRow) *frontendv1.RosterOpenCount {
	count := &frontendv1.RosterOpenCount{}
	for _, row := range rosterwalk.FlattenOpen(rows) {
		count.Workspaces++
		switch tab := row.GetTab().GetTab().(type) {
		case *frontendv1.RosterOpenRowTab_Numbered:
			count.Unfolded++
		case *frontendv1.RosterOpenRowTab_Folded:
		default:
			panic(fmt.Sprintf("sidebar: open row %s carries no tab arm (%T)", row.GetRow().GetWorkspace().GetWorkspace().GetId(), tab))
		}
	}
	return count
}

// addOpenCount adds PART, a region's or a repository's count, into TOTAL.
func addOpenCount(total, part *frontendv1.RosterOpenCount) {
	total.Workspaces += part.GetWorkspaces()
	total.Unfolded += part.GetUnfolded()
}

// openHeader composes a repository section's or a subsection's header: its
// label and its count, which its caller reads off the very rows the part
// carries (openCount), so it can never disagree with them, and no client
// counts rows for itself.
func openHeader(label string, count *frontendv1.RosterOpenCount) *frontendv1.RosterOpenHeader {
	return &frontendv1.RosterOpenHeader{
		Label: &frontendv1.RosterLabel{Text: label},
		Count: count,
	}
}

// repoQueue is one repository's merge queue as the merge queue section draws
// it: the merges in line front first, and the pause.
type repoQueue struct {
	line        []QueueEntry
	remediating []QueueEntry
	pause       *wsm.MergeQueuePause
	deployCheck *DeployCheckHold
	// github are the repository's workspaces standing in a GitHub merge
	// queue, in queue order.
	github []GithubQueueEntry
	// offQueue are the repository's merges off their queue, oldest departure
	// first.
	offQueue []OffQueueEntry
	// mediation is the deploy mediation the queue waits on, nil for none.
	mediation *QueueMediation
	// deploying is the landed merge still deploying, nil for none.
	deploying *QueueEntry
}

// listed reports whether the merge queue section lists Q: its line holds a
// merge, or a merge stands out of line (owner request, 2026-10-27: a
// repository leaves the section as soon as its line empties and nothing
// stands out of line). A paused queue holding nothing is not listed either.
func (q repoQueue) listed() bool {
	return len(q.line) > 0 || len(q.remediating) > 0 || len(q.github) > 0 || len(q.offQueue) > 0 || q.mediation != nil || q.deploying != nil
}

// drawMergeQueue composes one repository's merge queue: the merge at the
// front of the line as its head, every merge behind the front as waiting,
// and the pause. EVERY MERGE IS CREDITED AS THE ROSTER CREDITS IT
// (QueueEntry.Credited), so a hidden merge workspace is never selected here,
// and an async merge is named by its own name (queueSubject).
// Q is listed (repoQueue.listed), so its line holds a merge or a merge
// stands out of line: the head is drawn only while the line holds one. The
// merges out of line are drawn as the queue's two repair slots and the
// merges awaiting each, under the head.
func (r *resolver) drawMergeQueue(q repoQueue, log dlog.Logger) *frontendv1.RosterRepoMergeQueue {
	const op = "daemon.sidebar.merge_queue"
	out := &frontendv1.RosterRepoMergeQueue{Paused: drawMergeQueuePause(q.pause, q.deployCheck)}
	subject := func(entry QueueEntry) (*frontendv1.RosterMergeQueueSubject, bool) {
		credited, ok := r.state.record(entry.Credited)
		if !ok {
			log.Error(op, "a merge in the queue is credited to a workspace the registry does not carry, so it is not drawn", dlog.Context{
				"workspace_id": string(entry.Workspace), "credited": string(entry.Credited),
				"invariant_violation": "a merge queue entry is credited to a registered workspace"})
			return nil, false
		}
		if entry.Async != (entry.AsyncName != "") {
			log.Error(op, "a merge in the queue is async without its own name, or named without being async, so it is not drawn", dlog.Context{
				"workspace_id": string(entry.Workspace), "credited": string(entry.Credited), "async": entry.Async, "async_name": entry.AsyncName,
				"invariant_violation": "a merge queue entry carries its own name iff it is async"})
			return nil, false
		}
		return queueSubject(*credited, entry), true
	}
	if m := q.mediation; m != nil {
		out.Mediation = drawMediation(*m, r.state.record, r.state.turnInFlight(m.Mediator), log)
	}
	for _, entry := range q.remediating {
		sub, ok := subject(entry)
		if !ok {
			continue
		}
		drawRepair(out, entry, sub, log)
	}
	for _, entry := range q.github {
		sub, ok := subject(QueueEntry{Workspace: entry.Workspace, Credited: entry.Workspace})
		if !ok {
			continue
		}
		out.Github = append(out.Github, &frontendv1.RosterMergeQueueGithubEntry{
			Subject:    sub,
			Place:      &frontendv1.RosterMergeQueueGithubPlace{Place: uint32(entry.Place), Total: uint32(entry.Total)},
			QueuedAtMs: entry.QueuedAt.UnixMilli(),
		})
	}
	for _, entry := range q.offQueue {
		own, ok := r.state.record(entry.Workspace)
		if !ok {
			log.Error(op, "a merge off its queue names a workspace the registry does not carry, so it is not drawn", dlog.Context{
				"workspace_id": string(entry.Workspace), "invariant_violation": "a merge off its queue references a registered workspace"})
			continue
		}
		sub, ok := subject(QueueEntry{Workspace: entry.Workspace, Credited: entry.Credited, Async: entry.Async, AsyncName: asyncName(*own, entry.Async)})
		if !ok {
			continue
		}
		out.OffQueue = append(out.OffQueue, &frontendv1.RosterMergeQueueOffQueue{
			Subject: sub,
			Cause:   &frontendv1.RosterMergeQueueOffQueueCause{Text: entry.Cause},
			Requeue: &frontendv1.RosterMergeQueueOffQueueRequeue{Merge: own.Ref()},
			Discard: &frontendv1.RosterMergeQueueOffQueueDiscard{Merge: own.Ref()},
		})
	}
	// THE LANDED MERGE STILL DEPLOYING IS THE HEAD until its deploy's check
	// ends, and every merge in line waits behind it (MergeQueue.Deploying).
	// It holds the queue's head from its landing's teardown, so a front
	// admitted beside it is a broken hold, drawn behind it; the broken hold
	// is recorded once, when the queue is taken (SetMergeQueue).
	line := q.line
	if d := q.deploying; d != nil {
		line = append([]QueueEntry{*d}, line...)
	}
	if len(line) == 0 {
		return out
	}
	front := line[0]
	if sub, ok := subject(front); ok {
		head := &frontendv1.RosterMergeQueueHead{Subject: sub, Stages: front.Stages}
		if front.Admitted {
			head.Standing = &frontendv1.RosterMergeQueueHead_Merging{Merging: &frontendv1.RosterMergeQueueHeadMerging{}}
		} else {
			head.Standing = &frontendv1.RosterMergeQueueHead_Waiting{Waiting: &frontendv1.RosterMergeQueueHeadWaiting{}}
		}
		if clock, ok := drawHeadClock(front, log); ok {
			head.Clock = clock
			out.Head = head
		}
	}
	for _, entry := range line[1:] {
		sub, ok := subject(entry)
		if !ok {
			continue
		}
		clock, ok := drawWaitingClock(entry, log)
		if !ok {
			continue
		}
		out.Waiting = append(out.Waiting, &frontendv1.RosterMergeQueueWaiting{Subject: sub, Clock: clock})
	}
	return out
}

// drawRepair draws ENTRY, a merge out of line drawn as SUB, at the repair
// slot it holds or awaits. The fixing-tests slot holds one merge (wsm's
// one-holder index): a second holder is a defect, recorded at ERROR and not
// drawn, and so is a merge out of line naming no repair slot. Every merge
// resolving conflicts is drawn, in order (owner ruling, 2026-11-14).
func drawRepair(out *frontendv1.RosterRepoMergeQueue, entry QueueEntry, sub *frontendv1.RosterMergeQueueSubject, log dlog.Logger) {
	const op = "daemon.sidebar.merge_queue"
	second := func(holder *frontendv1.RosterMergeQueueSubject) {
		log.Error(op, "a second merge holds a repair slot of the queue, so it is not drawn", dlog.Context{
			"workspace_id": string(entry.Workspace), "repair": entry.Repair.String(), "holder": holder.GetName(),
			"invariant_violation": "a queue's repair slot has at most one holder"})
	}
	switch entry.Repair {
	case RepairFixingTests:
		if out.FixingTests != nil {
			second(out.FixingTests.Subject)
			return
		}
		clock, ok := drawActiveClock(entry, log)
		if !ok {
			return
		}
		out.FixingTests = &frontendv1.RosterMergeQueueFixingTests{Subject: sub, Clock: clock}
	case RepairResolvingConflicts:
		clock, ok := drawActiveClock(entry, log)
		if !ok {
			return
		}
		out.ResolvingConflicts = append(out.ResolvingConflicts, &frontendv1.RosterMergeQueueResolvingConflicts{Subject: sub, Clock: clock})
	case RepairAwaitingFixingTests:
		clock, ok := drawWaitingClock(entry, log)
		if !ok {
			return
		}
		out.AwaitingFixingTests = append(out.AwaitingFixingTests, &frontendv1.RosterMergeQueueAwaitingFixingTests{Subject: sub, Clock: clock})
	default:
		log.Error(op, "a merge out of line names no repair slot, so it is not drawn", dlog.Context{
			"workspace_id": string(entry.Workspace), "repair": entry.Repair.String(),
			"invariant_violation": "a merge out of line holds or awaits a repair slot"})
	}
}

// drawActiveClock draws ENTRY's clock as an active one: the clock of a line
// whose merge is always at work (fixing tests, resolving conflicts). A clock
// in any other phase there is a defect, recorded at ERROR, and the line is
// not drawn.
func drawActiveClock(entry QueueEntry, log dlog.Logger) (*frontendv1.RosterMergeQueueActiveClock, bool) {
	if entry.Clock.Phase != wsm.MergeClockActive {
		clockDefect(entry, wsm.MergeClockActive.String(), log)
		return nil, false
	}
	return &frontendv1.RosterMergeQueueActiveClock{ActiveSinceMs: entry.Clock.Since.UnixMilli()}, true
}

// drawWaitingClock draws ENTRY's clock as a waiting one: the clock of a line
// whose merge always waits (in line behind the head, awaiting the
// fixing-tests slot). A clock in any other phase there is a defect, recorded
// at ERROR, and the line is not drawn.
func drawWaitingClock(entry QueueEntry, log dlog.Logger) (*frontendv1.RosterMergeQueueWaitingClock, bool) {
	if entry.Clock.Phase != wsm.MergeClockWaiting {
		clockDefect(entry, wsm.MergeClockWaiting.String(), log)
		return nil, false
	}
	return &frontendv1.RosterMergeQueueWaitingClock{
		WaitingSinceMs: entry.Clock.Since.UnixMilli(),
		WaitedBeforeMs: entry.Clock.Waited.Milliseconds(),
	}, true
}

// drawHeadClock draws the head ENTRY's clock in the stretch it is in. A head
// whose clock counts nothing is a defect, recorded at ERROR, and the head is
// not drawn.
func drawHeadClock(entry QueueEntry, log dlog.Logger) (*frontendv1.RosterMergeQueueHeadClock, bool) {
	switch entry.Clock.Phase {
	case wsm.MergeClockActive:
		active, _ := drawActiveClock(entry, log)
		return &frontendv1.RosterMergeQueueHeadClock{Stretch: &frontendv1.RosterMergeQueueHeadClock_Active{Active: active}}, true
	case wsm.MergeClockWaiting:
		waiting, _ := drawWaitingClock(entry, log)
		return &frontendv1.RosterMergeQueueHeadClock{Stretch: &frontendv1.RosterMergeQueueHeadClock_Waiting{Waiting: waiting}}, true
	}
	clockDefect(entry, "active or waiting", log)
	return nil, false
}

// clockDefect records that ENTRY's line is not drawn because its clock is in
// a phase its line cannot carry: WANT names the phases it can.
func clockDefect(entry QueueEntry, want string, log dlog.Logger) {
	log.Error("daemon.sidebar.merge_queue", "a merge's clock is in a phase its line cannot carry, so it is not drawn", dlog.Context{
		"workspace_id": string(entry.Workspace), "credited": string(entry.Credited), "repair": entry.Repair.String(),
		"clock_phase": entry.Clock.Phase.String(), "want_phase": want, "admitted": entry.Admitted,
		"invariant_violation": "a merge queue line's clock is in the phase its standing gives it"})
}

// drawMergeQueuePause renders a queue's standing pause as the roster draws
// it, else its deploy check's hold (DEPLOYCHECK), nil while the queue runs.
// A REAL PAUSE IS DRAWN OVER THE HOLD: the hold's reason stays on the
// mediation's label beside the repository's name. A pause recorded before its
// record was kept carries neither its instant nor its pauser.
func drawMergeQueuePause(pause *wsm.MergeQueuePause, deployCheck *DeployCheckHold) *frontendv1.RosterMergeQueuePaused {
	if pause == nil {
		return drawDeployCheckHold(deployCheck)
	}
	out := &frontendv1.RosterMergeQueuePaused{Hard: pause.Hard}
	if !pause.At.IsZero() {
		at := pause.At.UnixMilli()
		out.PausedAtMs = &at
	}
	switch pause.By.Kind {
	case wsm.MergeQueuePauserUser:
		out.By = &frontendv1.RosterMergeQueuePauser{Asker: &frontendv1.RosterMergeQueuePauser_User{User: &frontendv1.RosterMergeQueuePauserUser{}}}
	case wsm.MergeQueuePauserWorkspace:
		out.By = &frontendv1.RosterMergeQueuePauser{Asker: &frontendv1.RosterMergeQueuePauser_Workspace{
			Workspace: &frontendv1.RosterMergeQueuePauserWorkspace{Name: pause.By.Name}}}
	case wsm.MergeQueuePauserHost:
		out.By = &frontendv1.RosterMergeQueuePauser{Asker: &frontendv1.RosterMergeQueuePauser_Host{
			Host: &frontendv1.RosterMergeQueuePauserHost{Cause: pause.By.Cause}}}
	}
	return out
}

// drawDeployCheckHold renders a queue's deploy check hold H as its pause, nil
// for none. It is never a hard stop: the hold suspends merges at their step.
func drawDeployCheckHold(h *DeployCheckHold) *frontendv1.RosterMergeQueuePaused {
	if h == nil {
		return nil
	}
	out := &frontendv1.RosterMergeQueuePaused{By: &frontendv1.RosterMergeQueuePauser{
		Asker: &frontendv1.RosterMergeQueuePauser_DeployCheck{DeployCheck: &frontendv1.RosterMergeQueuePauserDeployCheck{}}}}
	if !h.Since.IsZero() {
		at := h.Since.UnixMilli()
		out.PausedAtMs = &at
	}
	return out
}

// drawMediation is the label of deploy check M beside the repository's name,
// with the workspace a click selects: the chain's intake while the deploy
// rolls out (none for a chain's first deploy) or is assessed, and the
// remediation workspace while it is fixed. A workspace at work that the
// registry does not carry is a broken roster: it is recorded, and the label
// is drawn unclickable while rolling out, and not at all otherwise, since
// those standings require their target.
//
// AN ESCALATED CHECK WHOSE MEDIATOR IS MID-TURN IS DRAWN AS FIXING (owner
// ruling, 2026-11-18): WORKING reports the mediator's turn in flight, so the
// label reads that the mediator is at work on what the user settled, and
// reads that the deploy needs the user again once the turn ends with no
// finish. The label follows the same turn the mediator's own row draws.
func drawMediation(m QueueMediation, record func(ids.WorkspaceID) (*wsm.Workspace, bool), working bool, log dlog.Logger) *frontendv1.RosterMergeQueueMediation {
	const op = "daemon.sidebar.merge_queue"
	target := func(ws ids.WorkspaceID) *frontendv1.RosterMergeQueueMediationTarget {
		if ws == "" {
			return nil
		}
		rec, ok := record(ws)
		if !ok {
			log.Error(op, "the deploy mediator is one the registry does not carry, so the label selects nothing", dlog.Context{
				"workspace_id": string(ws), "repository_id": string(m.Repo), "stage": int(m.Stage),
				"invariant_violation": "a deploy mediator is registered"})
			return nil
		}
		return &frontendv1.RosterMergeQueueMediationTarget{Workspace: rec.Ref()}
	}
	stage := m.Stage
	if stage == MediationEscalated && working && m.Mediator != "" {
		stage = MediationFixing
	}
	switch stage {
	case MediationWaiting:
		return &frontendv1.RosterMergeQueueMediation{Standing: &frontendv1.RosterMergeQueueMediation_Waiting{
			Waiting: &frontendv1.RosterMergeQueueMediationWaiting{Target: target(m.Mediator)}}}
	case MediationChecking:
		if t := target(m.Mediator); t != nil {
			return &frontendv1.RosterMergeQueueMediation{Standing: &frontendv1.RosterMergeQueueMediation_Checking{
				Checking: &frontendv1.RosterMergeQueueMediationChecking{Target: t}}}
		}
	case MediationFixing:
		if t := target(m.Mediator); t != nil {
			return &frontendv1.RosterMergeQueueMediation{Standing: &frontendv1.RosterMergeQueueMediation_Fixing{
				Fixing: &frontendv1.RosterMergeQueueMediationFixing{Target: t}}}
		}
	case MediationTesting:
		if t := target(m.Mediator); t != nil {
			return &frontendv1.RosterMergeQueueMediation{Standing: &frontendv1.RosterMergeQueueMediation_Testing{
				Testing: &frontendv1.RosterMergeQueueMediationTesting{Target: t}}}
		}
	case MediationEscalated:
		return &frontendv1.RosterMergeQueueMediation{Standing: &frontendv1.RosterMergeQueueMediation_Escalated{
			Escalated: &frontendv1.RosterMergeQueueMediationEscalated{Target: target(m.Mediator)}}}
	default:
		panic(fmt.Sprintf("sidebar: a deploy check stands at no stage (%+v)", m))
	}
	return nil
}

// queueSubject is the subject a merge in the queue is drawn as: the name it is
// drawn under, the workspace it is credited to, where a click on it goes, and
// how it runs. A sync merge is named by the workspace it is credited to and an
// async merge by its own name (QueueEntry.AsyncName), so several async merges
// one workspace asked for are drawn apart; a click on either selects the
// credited workspace and reveals the merge's own bubble on its root feed. The
// merge itself is named by its queue entry's workspace, which every ask about
// it addresses.
func queueSubject(credited wsm.Workspace, entry QueueEntry) *frontendv1.RosterMergeQueueSubject {
	mode := &frontendv1.RosterMergeQueueMode{Mode: &frontendv1.RosterMergeQueueMode_Sync{Sync: &frontendv1.RosterMergeQueueModeSync{}}}
	name := credited.Name
	if entry.Async {
		mode.Mode = &frontendv1.RosterMergeQueueMode_Async{Async: &frontendv1.RosterMergeQueueModeAsync{
			Requester: &frontendv1.RosterMergeQueueModeAsyncRequester{Name: credited.Name}}}
		name = entry.AsyncName
	}
	return &frontendv1.RosterMergeQueueSubject{Workspace: credited.Ref(), Name: name, Reveal: entry.Reveal, Mode: mode,
		Merge: &workspacev1.WorkspaceRef{Id: string(entry.Workspace)}}
}

// asyncName is the name an async merge off its queue is drawn under: its
// merge workspace's own, and none for a workspace's own merge.
func asyncName(own wsm.Workspace, async bool) string {
	if !async {
		return ""
	}
	return own.Name
}

// repoQueues answers every repository's merge queue, keyed by the repository
// of the workspace each merge runs in: a queue is keyed by its repository's
// git common dir, which only the merge orchestrator resolves, and every merge
// on one queue is a workspace of that queue's repository.
//
// TWO QUEUES ON ONE REPOSITORY, and a merge naming a workspace the registry
// does not carry, are INVARIANT VIOLATIONS (one queue per repository; a queue
// entry references a registered workspace), recorded loudly here as
// assertRepositoryInvariant records its own. Nothing is dropped to hide the
// first: both queues' merges are drawn, in queue-key order. A merge with no
// record cannot be drawn and is named in the record. A FAILED merge whose
// workspace has since left the registry (closed and forgotten) is placed by
// the workspace it is credited to.
func (r *resolver) repoQueues(log dlog.Logger) map[ids.RepoID]repoQueue {
	const op = "daemon.sidebar.queue_lines"
	keys := make([]wsm.RepoKey, 0, len(r.state.queues))
	for key := range r.state.queues {
		keys = append(keys, key)
	}
	slices.Sort(keys)
	out := map[ids.RepoID]repoQueue{}
	queueOf := map[ids.RepoID]wsm.RepoKey{}
	claim := func(key wsm.RepoKey, repo ids.RepoID, ws ids.WorkspaceID) {
		if first, seen := queueOf[repo]; seen && first != key {
			log.Error(op, "two merge queues hold merges of one repository; both are drawn", dlog.Context{
				"repo_id": string(repo), "first_queue": string(first), "second_queue": string(key), "workspace_id": string(ws),
				"invariant_violation": "one merge queue per repository"})
			return
		}
		queueOf[repo] = key
	}
	for _, key := range keys {
		q := r.state.queues[key]
		for _, entry := range q.line {
			ws, ok := r.state.record(entry.Workspace)
			if !ok {
				log.Error(op, "a merge in line names a workspace the registry does not carry, so its queue row is not drawn", dlog.Context{
					"repo": string(key), "workspace_id": string(entry.Workspace),
					"invariant_violation": "a merge queue entry references a registered workspace"})
				continue
			}
			claim(key, ws.Repo, entry.Workspace)
			rq := out[ws.Repo]
			rq.line = append(rq.line, entry)
			rq.pause, rq.deployCheck = q.pause, q.deployCheck
			out[ws.Repo] = rq
		}
		for _, entry := range q.remediating {
			ws, ok := r.state.record(entry.Workspace)
			if !ok {
				log.Error(op, "a merge remediating out of line names a workspace the registry does not carry, so it is not drawn", dlog.Context{
					"repo": string(key), "workspace_id": string(entry.Workspace),
					"invariant_violation": "a merge queue entry references a registered workspace"})
				continue
			}
			claim(key, ws.Repo, entry.Workspace)
			rq := out[ws.Repo]
			rq.remediating = append(rq.remediating, entry)
			rq.pause, rq.deployCheck = q.pause, q.deployCheck
			out[ws.Repo] = rq
		}
		if d := q.deploying; d != nil {
			ws, ok := r.state.record(d.Workspace)
			if !ok {
				log.Error(op, "the landed merge still deploying names a workspace the registry does not carry, so it is not drawn", dlog.Context{
					"repo": string(key), "workspace_id": string(d.Workspace),
					"invariant_violation": "a merge queue entry references a registered workspace"})
			} else {
				claim(key, ws.Repo, d.Workspace)
				rq := out[ws.Repo]
				rq.deploying = d
				rq.pause, rq.deployCheck = q.pause, q.deployCheck
				out[ws.Repo] = rq
			}
		}
		// THE MEDIATION NAMES THE REPOSITORY whose queue it holds: the
		// mediator is no workspace the sidebar draws.
		if m := q.mediation; m != nil {
			claim(key, m.Repo, "")
			rq := out[m.Repo]
			rq.mediation = m
			rq.pause, rq.deployCheck = q.pause, q.deployCheck
			out[m.Repo] = rq
		}
	}
	for _, entry := range r.state.github {
		ws, ok := r.state.record(entry.Workspace)
		if !ok {
			log.Error(op, "a workspace standing in a GitHub merge queue is not in the registry, so it is not drawn", dlog.Context{
				"workspace_id":        string(entry.Workspace),
				"invariant_violation": "a pull request's owner is a registered workspace"})
			continue
		}
		rq := out[ws.Repo]
		rq.github = append(rq.github, entry)
		out[ws.Repo] = rq
	}
	for _, entry := range r.state.offQueue {
		ws, ok := r.state.record(entry.Workspace)
		if !ok {
			log.Error(op, "a merge off its queue names a workspace the registry does not carry, so it is not drawn", dlog.Context{
				"workspace_id": string(entry.Workspace), "invariant_violation": "a merge off its queue references a registered workspace"})
			continue
		}
		// A CLOSE ENDS THE REQUEST in the close's own store write
		// (wsm.markClosedInTx), and the roster learns of the close from the
		// registry before the orchestrator's next set arrives, so a closed
		// workspace's merge is already gone.
		if ws.Closed {
			continue
		}
		rq := out[ws.Repo]
		rq.offQueue = append(rq.offQueue, entry)
		out[ws.Repo] = rq
	}
	return out
}

// openRows composes one region's open rows, depth-first, each taking the next
// tab of NUMBERS before its family does, so a parent's number precedes its
// children's.
func (r *resolver) openRows(nodes []openNode, folded bool, numbers *tabNumberer, rc rowContext, log dlog.Logger) *frontendv1.RosterOpenRows {
	out := &frontendv1.RosterOpenRows{}
	for _, node := range nodes {
		out.Rows = append(out.Rows, r.openRow(node, folded, numbers, rc, log))
	}
	return out
}

// openRow composes one open row, its tab, and its open family below it: its
// own-repository children, then one repository block per other repository,
// each headed by one divider naming its repository
// (frontend.v1.RosterOpenRow.repository_blocks). Tabs are numbered in that
// same order, so a block's rows take their tabs after the own-repository
// children's.
func (r *resolver) openRow(node openNode, folded bool, numbers *tabNumberer, rc rowContext, log dlog.Logger) *frontendv1.RosterOpenRow {
	// A VISIBLE ROW'S TAB IS NUMBERED, whatever folds cover it, and one a fold
	// covers is drawn inside the fold (wsm.Visibilities): the bar and the
	// sidebar draw exactly the workspaces the editor holds.
	drawn := r.state.visibility[node.ws.ID].Visible()
	out := &frontendv1.RosterOpenRow{
		Row: r.row(node.ws, rc, log), Tab: numbers.tab(!drawn), FoldCover: foldCoverOf(folded, drawn),
		RecencyGutter: r.recencyGutter(node.ws.ID),
	}
	// THE CHILDREN FOLD COVERS EVERY CHILD, the own-repository ones and every
	// repository block's rows alike (frontend.v1.RosterOpenRow.children_fold).
	if len(node.children) > 0 || len(node.blocks) > 0 {
		out.ChildrenFold = childrenFold(node.ws.ChildrenFolded,
			wsm.DescendantMembers(r.state.reg.Workspaces, node.ws.ID).HoldsCurrent(r.selectedWorkspace()))
	}
	kin := childrenFolded(node, folded)
	for _, child := range node.children {
		out.Children = append(out.Children, r.openRow(child, kin, numbers, rc, log))
	}
	for _, block := range node.blocks {
		repo := r.blockRepository(node.ws, block.repo, log)
		composed := &frontendv1.RosterOpenRowRepositoryBlock{
			Repository: repoKey(repo).Repository,
			Divider:    &frontendv1.RosterOpenRowRepositoryDivider{Name: repo.Name, MainWorkspace: r.mainWorkspace(repo)},
		}
		for _, child := range block.members {
			composed.Rows = append(composed.Rows, r.openRow(child, kin, numbers, rc, log))
		}
		out.RepositoryBlocks = append(out.RepositoryBlocks, composed)
	}
	return out
}

// mainWorkspace answers REPO's main workspace (wsm.IsMainWorktree) as the
// ref a click on a block divider's name selects, nil when the registry holds
// none for it or it is closed.
func (r *resolver) mainWorkspace(repo wsm.Repository) *workspacev1.WorkspaceRef {
	for _, ws := range r.state.reg.Workspaces {
		if ws.Repo == repo.ID && wsm.IsMainWorktree(ws.Dir, repo.Dir) && !ws.Closed {
			return ws.Ref()
		}
	}
	return nil
}

// blockRepository answers repository ID of a block under PARENT. A block
// whose repository the registry does not carry is an invariant violation
// (assertRepositoryInvariant) and fails hard.
func (r *resolver) blockRepository(parent wsm.Workspace, id ids.RepoID, log dlog.Logger) wsm.Repository {
	if repo, ok := r.repositoryByID(id); ok {
		return repo
	}
	log.Error("daemon.sidebar.repository_block", "a repository block names a repository the registry does not carry",
		dlog.Context{"parent_id": string(parent.ID), "repo_id": string(id), "invariant_violation": "workspace.Repo names no registered repository"})
	panic(fmt.Sprintf("sidebar: a block under workspace %s names repository %s, which the registry does not carry", parent.ID, id))
}

// repositoryOf returns the registered repository WS belongs to, and whether
// the registry carries it. The one lookup every row-level repository fact
// (the divider, RosterRow.repository) goes through; each caller decides how
// a repository the registry does not carry fails.
func (r *resolver) repositoryOf(ws wsm.Workspace) (wsm.Repository, bool) {
	return r.repositoryByID(ws.Repo)
}

// repositoryByID returns the registered repository ID, and whether the
// registry carries it: repositoryOf's lookup, for a repository named by id
// rather than by a workspace (a repository block's).
func (r *resolver) repositoryByID(id ids.RepoID) (wsm.Repository, bool) {
	for _, repo := range r.state.reg.Repositories {
		if repo.ID == id {
			return repo, true
		}
	}
	return wsm.Repository{}, false
}

// repoKey is a repository's join key, as its section and its merged group
// both carry it.
func repoKey(repo wsm.Repository) *frontendv1.RosterRepoKey {
	return &frontendv1.RosterRepoKey{
		Repository: &workspacev1.RepositoryRef{Id: string(repo.ID), Dir: repo.Dir}}
}

// holdsCurrent reports whether a collapse of the section MEMBERS names must be
// refused: it holds the current workspace AND no other drawn tab remains to
// land the user on (LandingTarget). A collapse of a section holding the
// current workspace is otherwise allowed, and the daemon's fold verb moves the
// selection to that landing, asking LandingTarget the same question by the
// same membership (wsm.FoldMembers).
func holdsCurrent(reg Registry, tabs []Tab, current *wsm.Workspace, members wsm.FoldMembers) bool {
	if !members.HoldsCurrent(current) {
		return false
	}
	_, _, lands := LandingTarget(reg, tabs, *current, members)
	return !lands
}

// setRepoFold states the repository's fold on its section: the ONE arm the
// sidebar and the Emacs tab bar both draw from. An expanded section says
// whether a collapse may be asked: never while it HOLDS_CURRENT, the current
// workspace with nowhere else to land (holdsCurrent), which the daemon's fold
// verb refuses too.
func setRepoFold(section *frontendv1.RosterRepoSection, folded, holdsCurrent bool) {
	if folded {
		section.Fold = &frontendv1.RosterRepoSection_Collapsed{Collapsed: &frontendv1.RosterRepoSectionCollapsed{}}
		return
	}
	expanded := &frontendv1.RosterRepoSectionExpanded{
		Collapse: &frontendv1.RosterRepoSectionExpanded_Collapsible{Collapsible: &frontendv1.RosterRepoSectionCollapsible{}}}
	if holdsCurrent {
		expanded.Collapse = &frontendv1.RosterRepoSectionExpanded_HoldsCurrent{HoldsCurrent: &frontendv1.RosterRepoSectionHoldsCurrent{}}
	}
	section.Fold = &frontendv1.RosterRepoSection_Expanded{Expanded: expanded}
}

// setSubsectionFold sets a subsection's fold arm: its OWN fold, which the
// sidebar draws. Its tabs are folded by its repository's collapse as well
// (openRegion.folded).
// An expanded subsection says whether a collapse may be asked, as a
// repository's section does (setRepoFold).
func setSubsectionFold(sub *frontendv1.RosterSubsection, folded, holdsCurrent bool) {
	if folded {
		sub.Fold = &frontendv1.RosterSubsection_Collapsed{Collapsed: &frontendv1.RosterSubsectionCollapsed{}}
		return
	}
	expanded := &frontendv1.RosterSubsectionExpanded{
		Collapse: &frontendv1.RosterSubsectionExpanded_Collapsible{Collapsible: &frontendv1.RosterSubsectionCollapsible{}}}
	if holdsCurrent {
		expanded.Collapse = &frontendv1.RosterSubsectionExpanded_HoldsCurrent{HoldsCurrent: &frontendv1.RosterSubsectionHoldsCurrent{}}
	}
	sub.Fold = &frontendv1.RosterSubsection_Expanded{Expanded: expanded}
}

// childrenFold states a row's children's fold: the ONE arm the sidebar and the
// Emacs tab bar both draw from. Expanded children say whether a collapse may
// be asked, as a subsection's do (setSubsectionFold).
func childrenFold(folded, holdsCurrent bool) *frontendv1.RosterOpenRowChildrenFold {
	if folded {
		return &frontendv1.RosterOpenRowChildrenFold{Fold: &frontendv1.RosterOpenRowChildrenFold_Collapsed{
			Collapsed: &frontendv1.RosterOpenRowChildrenCollapsed{}}}
	}
	expanded := &frontendv1.RosterOpenRowChildrenExpanded{
		Collapse: &frontendv1.RosterOpenRowChildrenExpanded_Collapsible{Collapsible: &frontendv1.RosterOpenRowChildrenCollapsible{}}}
	if holdsCurrent {
		expanded.Collapse = &frontendv1.RosterOpenRowChildrenExpanded_HoldsCurrent{HoldsCurrent: &frontendv1.RosterOpenRowChildrenHoldsCurrent{}}
	}
	return &frontendv1.RosterOpenRowChildrenFold{Fold: &frontendv1.RosterOpenRowChildrenFold_Expanded{Expanded: expanded}}
}

// repoDivergence projects a repository's measure onto its section's label
// hover: no measure yet is pending. Every Measure is one of the three arms.
func repoDivergence(measure repodivergence.Measure) *frontendv1.RosterRepoDivergence {
	out := &frontendv1.RosterRepoDivergence{}
	switch m := measure.(type) {
	case nil:
		out.State = &frontendv1.RosterRepoDivergence_Pending{Pending: &frontendv1.RosterRepoDivergencePending{}}
	case repodivergence.Counted:
		out.State = &frontendv1.RosterRepoDivergence_Counted{Counted: &frontendv1.RosterRepoDivergenceCounted{
			Branch: m.Branch, Upstream: m.Upstream, Ahead: uint32(m.Ahead), Behind: uint32(m.Behind)}}
	case repodivergence.NoUpstream:
		out.State = &frontendv1.RosterRepoDivergence_NoUpstream{NoUpstream: &frontendv1.RosterRepoDivergenceNoUpstream{Branch: m.Branch}}
	case repodivergence.Failed:
		out.State = &frontendv1.RosterRepoDivergence_Failed{Failed: &frontendv1.RosterRepoDivergenceFailed{Reason: m.Reason}}
	default:
		panic(fmt.Sprintf("sidebar: repository divergence measure %T has no arm", measure))
	}
	return out
}

// assertRepositoryInvariant records a workspace whose `Repo` names no
// registered repository. IT IS UNREACHABLE, and the assertion is what says so
// out loud rather than letting the roster quietly draw one workspace fewer.
//
// A workspace whose repository is unregistered is an INVARIANT VIOLATION and
// must be impossible (owner ruling, 2026-09-13). Three layers hold it and the
// roster is none of them: `workspaces.repo_id` is `NOT NULL REFERENCES
// repositories(id)` and every state-store handle carries
// `_pragma=foreign_keys(1)`, so SQLite refuses the row; `RegisterWorkspace`
// mints the repository row through `ensureRepo` FIRST, in the workspace
// insert's own transaction; and `Create` refuses a repository the registry does
// not hold before anything is built. The state store's open reports a row that
// was already there when it boots.
//
// So reaching here is a defect in one of those, not a state to render, and it
// is recorded the way this resolver records every other assertion it cannot
// serve (see assertArm): loudly, once, naming the remedy.
func (r *resolver) assertRepositoryInvariant(byRepo map[ids.RepoID][]wsm.Workspace, operation, leftOutOf string, log dlog.Logger) {
	for repo, orphans := range byRepo {
		log.Error(operation,
			"workspaces name a repository the registry does not carry and were left out of {left_out_of}",
			dlog.Context{
				"left_out_of":         leftOutOf,
				"repo_id":             string(repo),
				"workspaces":          len(orphans),
				"invariant_violation": "workspace.Repo names no registered repository",
				"remediation":         "re-register the workspace's directory, which mints its repository row, or forget the workspace",
			})
	}
}

// mergeQueueSection resolves the Merge Queue section: ONE GROUP PER
// REPOSITORY whose queue is active (repoQueue.listed), in the repository
// grouping's order (owner request, 2026-11-01). A group follows no
// repository fold: it folds by itself on the page.
func (r *resolver) mergeQueueSection(log dlog.Logger) *frontendv1.RosterMergeQueueSection {
	queues := r.repoQueues(log)
	section := &frontendv1.RosterMergeQueueSection{
		Header: &frontendv1.RosterSectionHeader{Label: &frontendv1.RosterLabel{Text: MergeQueueSectionLabel}},
	}
	for _, repo := range sortRepositories(r.state.reg.Repositories) {
		q, queued := queues[repo.ID]
		delete(queues, repo.ID)
		if !queued || !q.listed() {
			continue
		}
		section.Groups = append(section.Groups, &frontendv1.RosterMergeQueueRepoGroup{
			Key:    repoKey(repo),
			Header: repoGroupHeader(repo),
			Queue:  r.drawMergeQueue(q, log.With(dlog.Context{"repo_id": string(repo.ID)})),
		})
	}
	for repo := range queues {
		log.Error("daemon.sidebar.merge_queue_section", "a merge queue holds merges of a repository the registry does not carry, so it is not drawn", dlog.Context{
			"repo_id": string(repo), "invariant_violation": "a merge queue's workspaces name a registered repository"})
	}
	setMergeQueueFold(section, r.state.reg.View.MergeQueueFolded)
	setMergeQueueHeight(section, r.state.reg.View.MergeQueueHeight)
	return section
}

// repoGroupHeader is the header a repository's group draws in the Merge
// Queue and Recently Merged sections alike: the repository's name.
func repoGroupHeader(repo wsm.Repository) *frontendv1.RosterMergedRepoGroupHeader {
	return &frontendv1.RosterMergedRepoGroupHeader{Label: &frontendv1.RosterLabel{Text: repo.Name}}
}

// hiddenSection resolves the hidden-workspaces section: the open workspaces
// the user hid (SetWorkspaceBacklogged), which have NO TAB. Its rows nest by
// lineage among the hidden workspaces alone, so a hidden family keeps its
// shape (a workspace with open children cannot be hidden, so a hidden row's
// family is hidden too); its top-level rows go in HIDE ORDER, the latest hide
// first (sortHiddenNewestFirst, owner request 2026-10-13), and its count is
// the rows it draws.
func (r *resolver) hiddenSection(hidden []wsm.Workspace, rc rowContext, log dlog.Logger) *frontendv1.RosterBacklogSection {
	log = log.With(dlog.Context{"section": "hidden"})
	tree := nest(hidden, log)
	rows := &frontendv1.RosterBacklogRows{}
	for _, ws := range sortHiddenNewestFirst(tree.roots) {
		rows.Rows = append(rows.Rows, r.hiddenRow(ws, tree, rc, log))
	}
	section := &frontendv1.RosterBacklogSection{
		Header:      sectionHeader(HiddenSectionLabel, len(rosterwalk.FlattenHidden(rows.GetRows()))),
		BacklogRows: rows,
	}
	setHiddenFold(section, r.state.reg.View.HiddenFolded)
	setHiddenHeight(section, r.state.reg.View.HiddenHeight)
	return section
}

// hiddenRow composes one hidden row and its hidden family below it.
func (r *resolver) hiddenRow(ws wsm.Workspace, tree forest, rc rowContext, log dlog.Logger) *frontendv1.RosterBacklogRow {
	out := &frontendv1.RosterBacklogRow{Row: r.row(ws, rc, log)}
	for _, child := range tree.children[ws.ID] {
		out.Children = append(out.Children, r.hiddenRow(child, tree, rc, log))
	}
	return out
}

// closedRows resolves the closed list: the closed workspaces that never
// merged, in roster order, which CLOSED is already in (registryIndex). No sidebar section draws it; the editor offers them
// again and routes their records by it.
func (r *resolver) closedRows(closed []wsm.Workspace, rc rowContext, log dlog.Logger) *frontendv1.RosterClosedRows {
	built := r.stats.rowsBuilt
	rows := make([]*frontendv1.RosterRow, 0, len(closed))
	for _, ws := range closed {
		rows = append(rows, r.row(ws, rc, log))
	}
	// A CLOSED LIST OF THE SAME WORKSPACES, NONE OF WHOSE ROWS WAS BUILT
	// AGAIN, IS THE LAST PUSH'S OWN: every kept row is the one the last full
	// render drew, which that push carries or equals. It is answered as that
	// push's message, which the push compares by pointer rather than walking
	// every closed row (resolvePush).
	if r.last != nil && r.stats.rowsBuilt == built && sameWorkspaces(r.last.GetClosed().GetRows(), closed) {
		return r.last.Closed
	}
	out := &frontendv1.RosterClosedRows{Rows: make([]*frontendv1.RosterClosedRow, 0, len(rows))}
	for _, row := range rows {
		out.Rows = append(out.Rows, &frontendv1.RosterClosedRow{Row: row})
	}
	return out
}

// sameWorkspaces reports whether ROWS are CLOSED's rows, in order.
func sameWorkspaces(rows []*frontendv1.RosterClosedRow, closed []wsm.Workspace) bool {
	if len(rows) != len(closed) {
		return false
	}
	for i, row := range rows {
		if row.GetRow().GetWorkspace().GetWorkspace().GetId() != string(closed[i].ID) {
			return false
		}
	}
	return true
}

// setHiddenFold states the hidden section's fold. The fold is the daemon's so
// every page draws the same one (UpdateSidebarView); it folds sidebar rows
// only, never a tab.
func setHiddenFold(section *frontendv1.RosterBacklogSection, folded bool) {
	if folded {
		section.Fold = &frontendv1.RosterBacklogSection_Collapsed{Collapsed: &frontendv1.RosterBacklogSectionCollapsed{}}
		return
	}
	section.Fold = &frontendv1.RosterBacklogSection_Expanded{Expanded: &frontendv1.RosterBacklogSectionExpanded{}}
}

// openSection resolves the open-workspaces section: its heading and its fold.
// The groupings it boxes stay on the roster, each carrying its own count
// (openCount). The fold is the daemon's so every page draws the same one
// (UpdateSidebarView); it folds sidebar rows only, never a tab.
func (r *resolver) openSection() *frontendv1.RosterOpenSection {
	section := &frontendv1.RosterOpenSection{Label: &frontendv1.RosterLabel{Text: OpenSectionLabel}}
	if r.state.reg.View.OpenFolded {
		section.Fold = &frontendv1.RosterOpenSection_Collapsed{Collapsed: &frontendv1.RosterOpenSectionCollapsed{}}
	} else {
		section.Fold = &frontendv1.RosterOpenSection_Expanded{Expanded: &frontendv1.RosterOpenSectionExpanded{}}
	}
	return section
}

// setMergedFold states the recently-merged band's fold. The fold is the
// daemon's so every page draws the same one (UpdateSidebarView).
func setMergedFold(section *frontendv1.RosterMergedSection, folded bool) {
	if folded {
		section.Fold = &frontendv1.RosterMergedSection_Collapsed{Collapsed: &frontendv1.RosterMergedSectionCollapsed{}}
		return
	}
	section.Fold = &frontendv1.RosterMergedSection_Expanded{Expanded: &frontendv1.RosterMergedSectionExpanded{}}
}

// setMergeQueueFold states the Merge Queue section's fold. The fold is the
// daemon's so every page draws the same one (UpdateSidebarView).
func setMergeQueueFold(section *frontendv1.RosterMergeQueueSection, folded bool) {
	if folded {
		section.Fold = &frontendv1.RosterMergeQueueSection_Collapsed{Collapsed: &frontendv1.RosterMergeQueueSectionCollapsed{}}
		return
	}
	section.Fold = &frontendv1.RosterMergeQueueSection_Expanded{Expanded: &frontendv1.RosterMergeQueueSectionExpanded{}}
}

// setMergeQueueHeight states the Merge Queue section's height. The height is
// the daemon's so every page draws the same one (UpdateSidebarView).
func setMergeQueueHeight(section *frontendv1.RosterMergeQueueSection, height wsm.SectionHeight) {
	if lines, sized := height.Lines(); sized {
		section.Height = &frontendv1.RosterMergeQueueSection_Sized{Sized: &frontendv1.RosterMergeQueueSectionSized{Lines: lines}}
		return
	}
	section.Height = &frontendv1.RosterMergeQueueSection_Fitted{Fitted: &frontendv1.RosterMergeQueueSectionFitted{}}
}

// setMergedHeight states the Recently Merged section's height. The height is
// the daemon's so every page draws the same one (UpdateSidebarView).
func setMergedHeight(section *frontendv1.RosterMergedSection, height wsm.SectionHeight) {
	if lines, sized := height.Lines(); sized {
		section.Height = &frontendv1.RosterMergedSection_Sized{Sized: &frontendv1.RosterMergedSectionSized{Lines: lines}}
		return
	}
	section.Height = &frontendv1.RosterMergedSection_Fitted{Fitted: &frontendv1.RosterMergedSectionFitted{}}
}

// setHiddenHeight states the hidden section's height. The height is the
// daemon's so every page draws the same one (UpdateSidebarView).
func setHiddenHeight(section *frontendv1.RosterBacklogSection, height wsm.SectionHeight) {
	if lines, sized := height.Lines(); sized {
		section.Height = &frontendv1.RosterBacklogSection_Sized{Sized: &frontendv1.RosterBacklogSectionSized{Lines: lines}}
		return
	}
	section.Height = &frontendv1.RosterBacklogSection_Fitted{Fitted: &frontendv1.RosterBacklogSectionFitted{}}
}

// sectionHeader composes a header outside the Open section (the Backlog's):
// its label and the count a FOLDED section draws beside it, which its caller
// reads off the very rows the section carries, so it can never disagree with
// them, and no client counts rows for itself.
func sectionHeader(label string, count int) *frontendv1.RosterSectionHeader {
	return &frontendv1.RosterSectionHeader{
		Label: &frontendv1.RosterLabel{Text: label},
		Count: &frontendv1.RosterSectionCount{Workspaces: uint32(count)},
	}
}

// foldCoverOf states an open row's standing under the folds: uncovered when
// no fold covers it (FOLDED false), else surfaced when it is drawn anyway
// (DRAWN, its visibility) and hidden when it is not. It is THE ONE spelling of
// the arm, so the sidebar never re-derives coverage from the folds it draws.
func foldCoverOf(folded, drawn bool) *frontendv1.RosterOpenRowFoldCover {
	switch {
	case !folded:
		return &frontendv1.RosterOpenRowFoldCover{Cover: &frontendv1.RosterOpenRowFoldCover_Uncovered{Uncovered: &frontendv1.RosterOpenRowFoldCoverUncovered{}}}
	case drawn:
		return &frontendv1.RosterOpenRowFoldCover{Cover: &frontendv1.RosterOpenRowFoldCover_Surfaced{Surfaced: &frontendv1.RosterOpenRowFoldCoverSurfaced{}}}
	default:
		return &frontendv1.RosterOpenRowFoldCover{Cover: &frontendv1.RosterOpenRowFoldCover_Hidden{Hidden: &frontendv1.RosterOpenRowFoldCoverHidden{}}}
	}
}
