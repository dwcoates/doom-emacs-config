package merge

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"strconv"

	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/gitclient"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/wsm"
)

// This file is the bubble's two PARENT tabs (frontend.v1.FeedMergeTab): the
// ONE rebasing tab, whose subtabs are the commits each replay replays, and the
// ONE tests tab, whose subtabs are the runs of the suites, each with the
// fixing attempt that followed it as a sub-subtab.
//
// THE VIEW IS THE DAEMON'S OWN BOOKKEEPING OF WHAT IT DREW, kept apart from the
// ledger's rounds. A round is a ledger interval (a conflict resolution is a
// `conflicts` round, a fixing attempt a `fixes` round, and the rebase goes on
// in a new `rebasing` round after each), and the footer and the resume read
// those rounds unchanged. The view is what the bubble draws: one rebasing tab
// across every round and replay, each commit's subtab across its conflict
// resolution, one tests tab across every run. Each view is recorded in the
// merge's progress record, so a resumed run draws on from where it stood.

// The row ids of the nested tabs. A subtab's row is keyed by what it is (a
// commit of a replay, a run), and its parent is the tab it is drawn in.
const (
	rowRebaseCommit   = "rebase_commit"
	rowTestsRun       = "tests_run"
	rowTestsRunFixing = "tests_run_fixing"
)

// tabKey names one tab row of the bubble's sub-feed: its row id, its secondary
// key, and, for a subtab, the tab row it is drawn in. A placement turns it into
// the row's ref for its own workspace.
type tabKey struct {
	id     string
	sub    string
	parent *tabKey
}

// roundTabKey is the key of a top-level tab round: its kind and its round.
func roundTabKey(kind string, round int) tabKey {
	return tabKey{id: kind, sub: strconv.Itoa(round)}
}

// rebasingKey is the ONE rebasing tab's key.
func rebasingKey() tabKey { return roundTabKey(TabRebasing, 1) }

// testsKey is the ONE tests tab's key.
func testsKey() tabKey { return roundTabKey(TabTests, 1) }

// commitKey is the subtab of commit i (0-based) of replay `replay`.
func commitKey(replay, i int) tabKey {
	parent := rebasingKey()
	return tabKey{id: rowRebaseCommit, sub: fmt.Sprintf("%d.%d", replay, i), parent: &parent}
}

// runKey is the subtab of run n (1-based): the n-th tests round.
func runKey(n int) tabKey {
	parent := testsKey()
	return tabKey{id: rowTestsRun, sub: strconv.Itoa(n), parent: &parent}
}

// fixingKey is the sub-subtab of the fixing attempt that followed run n.
func fixingKey(n int) tabKey {
	parent := runKey(n)
	return tabKey{id: rowTestsRunFixing, sub: strconv.Itoa(n), parent: &parent}
}

// span is one drawn unit's badge as the view records it: when it began, and,
// once it settled, when and how. A zero span is a unit not begun.
type span struct {
	StartedMS int64  `json:"started_ms"`
	EndedMS   int64  `json:"ended_ms,omitempty"`
	Failure   string `json:"failure,omitempty"`
}

// begun reports whether the unit has started.
func (s span) begun() bool { return s.StartedMS != 0 }

// settled reports whether the unit has ended.
func (s span) settled() bool { return s.EndedMS != 0 }

// state is the span as the shared badge state, with waiting set for a live
// unit whose agent has an ask open.
func (s span) state(waiting bool) tabState {
	if s.settled() {
		return tabState{startedMS: s.StartedMS, settled: true, endedMS: s.EndedMS, failure: s.Failure}
	}
	return tabState{startedMS: s.StartedMS, waiting: waiting}
}

// relive stands a parent tab live again from nowMS when it had settled (a
// merge that started over), and from nowMS when it had never begun; a tab
// already live keeps its start.
func (s *span) relive(nowMS int64) {
	if !s.begun() || s.settled() {
		*s = span{StartedMS: nowMS}
	}
}

// settle ends the span at nowMS, failed with failure when it is set.
func (s *span) settle(nowMS int64, failure string) {
	s.EndedMS, s.Failure = nowMS, failure
}

// rebaseView is the rebasing tab and the subtabs of its CURRENT replay.
type rebaseView struct {
	Span span `json:"span"`
	// Lines is the rebase-wide narration, across every replay.
	Lines []rebaseLine `json:"lines,omitempty"`
	// Replay is the current replay's number, 1-based; zero before the first.
	Replay int `json:"replay"`
	// Commits are the current replay's commits, in replay order.
	Commits []commitView `json:"commits,omitempty"`
}

// rebaseLine is one line of the rebase-wide narration: a sentence drawn
// verbatim, or one replay beginning, drawn from its parts. Exactly one is set.
type rebaseLine struct {
	Text      string         `json:"text,omitempty"`
	Replaying *replayingLine `json:"replaying,omitempty"`
}

// textLine is a narration sentence, drawn verbatim.
func textLine(text string) rebaseLine { return rebaseLine{Text: text} }

// UnmarshalJSON reads a line recorded as an object, and a line recorded as a
// bare string: the shape every line had before replays were recorded from
// their parts, which a merge in flight across the upgrade still carries.
func (l *rebaseLine) UnmarshalJSON(data []byte) error {
	var text string
	if err := json.Unmarshal(data, &text); err == nil {
		*l = textLine(text)
		return nil
	}
	type plain rebaseLine
	var out plain
	if err := json.Unmarshal(data, &out); err != nil {
		return fmt.Errorf("merge: a rebase narration line is neither a string nor a line object: %w", err)
	}
	*l = rebaseLine(out)
	return nil
}

// replayingLine is one replay beginning: Commits commits of Branch replayed
// onto TargetBranch, checked out in TargetDir, at Tip.
type replayingLine struct {
	Commits      int    `json:"commits"`
	Branch       string `json:"branch"`
	TargetBranch string `json:"target_branch"`
	TargetDir    string `json:"target_dir"`
	Tip          string `json:"tip"`
	// TargetWorkspace is the workspace registered on TargetDir, nil when the
	// checkout is no workspace's.
	TargetWorkspace *workspaceRefDoc `json:"target_workspace,omitempty"`
}

// replaying is the narration of a replay of commits of branch beginning onto
// targetBranch at tip, naming the workspace registered on the target's
// checkout when there is one.
func (r *run) replaying(ctx context.Context, commits int, branch, targetBranch, tip string) (rebaseLine, error) {
	dir := r.subject.targetDir
	line := &replayingLine{Commits: commits, Branch: branch, TargetBranch: targetBranch, TargetDir: dir, Tip: tip}
	record, err := r.o.deps.DB.WorkspaceByDir(ctx, dir)
	switch {
	case errors.Is(err, wsm.ErrNotFound):
		// The target's checkout is no workspace: the target is drawn as text.
	case err != nil:
		return rebaseLine{}, fmt.Errorf("merge: reading the workspace registered on the target's checkout %s: %w", dir, err)
	default:
		line.TargetWorkspace = &workspaceRefDoc{ID: record.ID, Dir: record.Dir}
	}
	return rebaseLine{Replaying: line}, nil
}

// workspaceRefDoc is a workspace as a replay line names it: its id and its
// registered directory.
type workspaceRefDoc struct {
	ID  ids.WorkspaceID `json:"id"`
	Dir string          `json:"dir"`
}

// drawn is the line as the rebasing tab draws it.
func (l rebaseLine) drawn() *frontendv1.FeedMergeRebaseLine {
	if l.Replaying == nil {
		return &frontendv1.FeedMergeRebaseLine{Line: &frontendv1.FeedMergeRebaseLine_Text{Text: l.Text}}
	}
	p := l.Replaying
	target := &frontendv1.FeedMergeRebaseTarget{Branch: p.TargetBranch, RepositoryDir: p.TargetDir}
	if p.TargetWorkspace != nil {
		target.Workspace = &workspacev1.WorkspaceRef{Id: string(p.TargetWorkspace.ID), Dir: p.TargetWorkspace.Dir}
	}
	return &frontendv1.FeedMergeRebaseLine{Line: &frontendv1.FeedMergeRebaseLine_Replaying{Replaying: &frontendv1.FeedMergeRebaseReplaying{
		Commits: uint32(p.Commits), Branch: p.Branch, Target: target, Tip: p.Tip, TipShortSha: short(p.Tip),
	}}}
}

// commitView is one commit's subtab.
type commitView struct {
	SHA     string `json:"sha"`
	Subject string `json:"subject"`
	// Span is zero while the replay has not reached the commit.
	Span     span          `json:"span"`
	Lines    []string      `json:"lines,omitempty"`
	Conflict *conflictView `json:"conflict,omitempty"`
}

// conflictView is a commit's conflict resolution, once the session has it.
type conflictView struct {
	Span  span     `json:"span"`
	Files []string `json:"files,omitempty"`
}

// testsView is the tests tab. Its runs are the tests rounds, and each run's
// row is drawn by the run itself.
type testsView struct {
	Span span `json:"span"`
}

// clone answers a deep copy, so a recorded view never shares slices with the
// live one.
func (v *rebaseView) clone() *rebaseView {
	if v == nil {
		return nil
	}
	out := *v
	out.Lines = nil
	for _, line := range v.Lines {
		if line.Replaying != nil {
			replaying := *line.Replaying
			if replaying.TargetWorkspace != nil {
				ref := *replaying.TargetWorkspace
				replaying.TargetWorkspace = &ref
			}
			line.Replaying = &replaying
		}
		out.Lines = append(out.Lines, line)
	}
	out.Commits = make([]commitView, len(v.Commits))
	for i, c := range v.Commits {
		c.Lines = append([]string(nil), c.Lines...)
		if c.Conflict != nil {
			conflict := *c.Conflict
			conflict.Files = append([]string(nil), c.Conflict.Files...)
			c.Conflict = &conflict
		}
		out.Commits[i] = c
	}
	return &out
}

// clone answers a copy of the tests view.
func (v *testsView) clone() *testsView {
	if v == nil {
		return nil
	}
	out := *v
	return &out
}

// progress answers how many of the current replay's commits are on the tip.
func (v *rebaseView) progress() (replayed, total int) {
	for _, c := range v.Commits {
		if c.Span.settled() && c.Span.Failure == "" {
			replayed++
		}
	}
	return replayed, len(v.Commits)
}

// beginReplay starts a new replay of commits at nowMS: every commit pending,
// the tab live again.
func (v *rebaseView) beginReplay(commits []gitclient.Commit, nowMS int64) {
	v.Span.relive(nowMS)
	v.Replay++
	v.Commits = make([]commitView, len(commits))
	for i, c := range commits {
		v.Commits[i] = commitView{SHA: c.SHA, Subject: c.Subject}
	}
}

// reachCommit stands every commit before i on the tip and commit i live from
// nowMS, so the view agrees with how far git has got. A commit already in that
// state is left as it stands.
func (v *rebaseView) reachCommit(i int, nowMS int64) {
	for j := range v.Commits {
		c := &v.Commits[j]
		switch {
		case j < i && !c.Span.settled():
			if !c.Span.begun() {
				c.Span.StartedMS = nowMS
			}
			c.Span.settle(nowMS, "")
		case j == i && !c.Span.begun():
			c.Span = span{StartedMS: nowMS}
		}
	}
}

// rebasingTab builds the rebasing tab's kind arm at WAITING.
func (v *rebaseView) rebasingTab(waiting bool) *frontendv1.FeedMergeTab {
	replayed, total := v.progress()
	inner := &frontendv1.FeedMergeTabRebasing{
		Progress: &frontendv1.FeedMergeRebaseProgress{Replayed: uint32(replayed), Total: uint32(total)},
		Lines:    drawnLines(v.Lines),
	}
	st := v.Span.state(waiting)
	if w := st.waitingOnUser(); w != nil {
		inner.State = &frontendv1.FeedMergeTabRebasing_WaitingOnUser{WaitingOnUser: w}
	} else if live, settled := st.badge(); live != nil {
		inner.State = &frontendv1.FeedMergeTabRebasing_Live{Live: live}
	} else {
		inner.State = &frontendv1.FeedMergeTabRebasing_Settled{Settled: settled}
	}
	return &frontendv1.FeedMergeTab{
		Label: tabLabel(TabRebasing, 1),
		Kind:  &frontendv1.FeedMergeTab_Rebasing{Rebasing: inner},
	}
}

// commitTab builds commit i's subtab at WAITING.
func (v *rebaseView) commitTab(i int, waiting bool) *frontendv1.FeedMergeTab {
	c := v.Commits[i]
	inner := &frontendv1.FeedMergeTabRebaseCommit{Lines: textLines(c.Lines)}
	switch {
	case !c.Span.begun():
		inner.State = &frontendv1.FeedMergeTabRebaseCommit_Pending{Pending: &frontendv1.FeedMergeRebaseCommitPending{}}
	default:
		st := c.Span.state(waiting)
		if w := st.waitingOnUser(); w != nil {
			inner.State = &frontendv1.FeedMergeTabRebaseCommit_WaitingOnUser{WaitingOnUser: w}
		} else if live, settled := st.badge(); live != nil {
			inner.State = &frontendv1.FeedMergeTabRebaseCommit_Live{Live: live}
		} else {
			inner.State = &frontendv1.FeedMergeTabRebaseCommit_Settled{Settled: settled}
		}
	}
	if c.Conflict != nil {
		conflict := &frontendv1.FeedMergeRebaseCommitConflict{}
		for _, path := range c.Conflict.Files {
			conflict.Files = append(conflict.Files, &frontendv1.FeedMergeConflictFile{Path: path})
		}
		st := c.Conflict.Span.state(waiting)
		if w := st.waitingOnUser(); w != nil {
			conflict.State = &frontendv1.FeedMergeRebaseCommitConflict_WaitingOnUser{WaitingOnUser: w}
		} else if live, settled := st.badge(); live != nil {
			conflict.State = &frontendv1.FeedMergeRebaseCommitConflict_Live{Live: live}
		} else {
			conflict.State = &frontendv1.FeedMergeRebaseCommitConflict_Settled{Settled: settled}
		}
		inner.Conflict = conflict
	}
	return &frontendv1.FeedMergeTab{
		Label: &frontendv1.FeedMergeTabLabel{
			Words: &frontendv1.FeedMergeTabLabel_Commit{Commit: &frontendv1.FeedMergeRebaseCommitIdentity{ShortSha: short(c.SHA), Subject: c.Subject}},
			Round: uint32(v.Replay),
		},
		Kind: &frontendv1.FeedMergeTab_RebaseCommit{RebaseCommit: inner},
	}
}

// textLines wraps narration sentences as drawn line elements.
func textLines(lines []string) []*frontendv1.FeedMergeRebaseLine {
	out := make([]*frontendv1.FeedMergeRebaseLine, 0, len(lines))
	for _, line := range lines {
		out = append(out, textLine(line).drawn())
	}
	return out
}

// drawnLines wraps the rebase-wide narration as drawn line elements.
func drawnLines(lines []rebaseLine) []*frontendv1.FeedMergeRebaseLine {
	out := make([]*frontendv1.FeedMergeRebaseLine, 0, len(lines))
	for _, line := range lines {
		out = append(out, line.drawn())
	}
	return out
}

// testsTab builds the tests tab's kind arm at WAITING.
func (v *testsView) testsTab(waiting bool) *frontendv1.FeedMergeTab {
	inner := &frontendv1.FeedMergeTabTests{}
	st := v.Span.state(waiting)
	if w := st.waitingOnUser(); w != nil {
		inner.State = &frontendv1.FeedMergeTabTests_WaitingOnUser{WaitingOnUser: w}
	} else if live, settled := st.badge(); live != nil {
		inner.State = &frontendv1.FeedMergeTabTests_Live{Live: live}
	} else {
		inner.State = &frontendv1.FeedMergeTabTests_Settled{Settled: settled}
	}
	return &frontendv1.FeedMergeTab{
		Label: tabLabel(TabTests, 1),
		Kind:  &frontendv1.FeedMergeTab_Tests{Tests: inner},
	}
}

// runTab builds run n's subtab: the run's own badge and its output.
func runTab(n int, st tabState, suites []*frontendv1.FeedMergeTestSuite, log *frontendv1.FeedMergeTestLog) *frontendv1.FeedMergeTab {
	inner := &frontendv1.FeedMergeTabTestsRun{Output: &frontendv1.FeedMergeTestsRunOutput{Suites: suites, Log: log}}
	if !st.settled && st.turnHolder != "" {
		inner.State = &frontendv1.FeedMergeTabTestsRun_WaitingForTurn{WaitingForTurn: &frontendv1.FeedMergeTabTestsRunWaitingForTurn{
			StartedAtMs: st.startedMS, Holder: &frontendv1.FeedMergeTestTurnHolder{WorkspaceName: st.turnHolder}}}
	} else if live, settled := st.badge(); live != nil {
		inner.State = &frontendv1.FeedMergeTabTestsRun_Live{Live: live}
	} else {
		inner.State = &frontendv1.FeedMergeTabTestsRun_Settled{Settled: settled}
	}
	return &frontendv1.FeedMergeTab{
		Label: textLabel(runLabel(n), 1),
		Kind:  &frontendv1.FeedMergeTab_TestsRun{TestsRun: inner},
	}
}

// runLabel is run n's drawn label.
func runLabel(n int) string { return fmt.Sprintf("run %d", n) }

// fixingTab builds a fixing attempt's sub-subtab, carrying the attempt and
// the ONE bound.
func fixingTab(st tabState, attempt int) *frontendv1.FeedMergeTab {
	inner := &frontendv1.FeedMergeTabTestsRunFixing{Attempt: &frontendv1.FeedMergeFixAttempt{Attempt: uint32(attempt), MaxAttempts: MaxFixAttempts}}
	if waiting := st.waitingOnUser(); waiting != nil {
		inner.State = &frontendv1.FeedMergeTabTestsRunFixing_WaitingOnUser{WaitingOnUser: waiting}
	} else if live, settled := st.badge(); live != nil {
		inner.State = &frontendv1.FeedMergeTabTestsRunFixing_Live{Live: live}
	} else {
		inner.State = &frontendv1.FeedMergeTabTestsRunFixing_Settled{Settled: settled}
	}
	return &frontendv1.FeedMergeTab{
		Label: textLabel("fixing", 1),
		Kind:  &frontendv1.FeedMergeTab_TestsRunFixing{TestsRunFixing: inner},
	}
}

// publishRebasing publishes the rebasing tab at WAITING.
func (r *run) publishRebasing(waiting bool) {
	r.publish(rebasingKey(), r.rebasing.rebasingTab(waiting))
}

// publishCommit publishes commit i's subtab at WAITING.
func (r *run) publishCommit(i int, waiting bool) {
	r.publish(commitKey(r.rebasing.Replay, i), r.rebasing.commitTab(i, waiting))
}

// publishReplay publishes the rebasing tab and then every subtab of its
// current replay: a parent row before the rows drawn in it.
func (r *run) publishReplay() {
	r.publishRebasing(false)
	for i := range r.rebasing.Commits {
		r.publishCommit(i, false)
	}
}

// resumeReplay stands the rebasing view where a resumed rebase finds git: on
// replay p of its commits, with every commit before p.done on the tip and
// commit p.done live. A record that drew no replay of these commits is given
// one, so a resumed rebase always draws the commits it replays.
func (r *run) resumeReplay(p *replay) {
	if len(r.rebasing.Commits) != len(p.commits) {
		r.rebasing.beginReplay(p.commits, p.round.started.UnixMilli())
	}
	r.rebasing.Span.relive(p.round.started.UnixMilli())
	if p.done < len(p.commits) {
		r.rebasing.reachCommit(p.done, r.o.nowMS())
	}
}

// commitAt answers the index of the commit the replay stands on.
func (p *replay) commitAt() int {
	return min(p.done, len(p.commits)-1)
}

// settleConflictAttempt ends the commit's current resolution attempt with
// the line the merge tries it again with: a retry after a resolution that did
// not resolve, or a re-prompt after a restart cut it short. The commit and the
// rebasing tab stay live; the next attempt opens a new section.
func (r *run) settleConflictAttempt(p *replay, line string) {
	i := p.commitAt()
	c := &r.rebasing.Commits[i]
	c.Conflict.Span.settle(r.o.nowMS(), line)
	c.Lines = append(c.Lines, line)
	r.publishCommit(i, false)
}

// drawConflict stands commit i's conflict resolution live, fresh or resumed:
// the session's turns draw in the commit's subtab, which (with the rebasing
// tab) an open ask draws waiting on the user.
func (r *run) drawConflict(ctx context.Context, round tabRound, i int, files int) {
	commit := r.rebasing.Commits[i]
	r.drawLive(ctx, round, footer.StepConflictResolution, conflictLine(commit.Subject, files), nil,
		commitKey(r.rebasing.Replay, i), func(waiting bool) {
			r.publishRebasing(waiting)
			r.publishCommit(i, waiting)
		})
}

// openTesting stands the tests tab live for the run of tests round ROUND,
// from that run's start when the tab had settled or never begun.
func (r *run) openTesting(round tabRound) {
	r.testing.Span.relive(round.started.UnixMilli())
	r.publishTesting(false)
}

// publishTesting publishes the tests tab at WAITING.
func (r *run) publishTesting(waiting bool) {
	r.publish(testsKey(), r.testing.testsTab(waiting))
}

// settleTesting settles the tests tab: the gate passed, or it gave up with
// failure.
func (r *run) settleTesting(failure string) {
	r.testing.Span.settle(r.o.nowMS(), failure)
	r.publishTesting(false)
}

// endRun ends the run of tests round ROUND, and the tests tab with it: the
// run's subtab and the tab settle with LINE (empty for a pass), the run
// drawing SUITES and LOG, and the round's ledger interval closes as CLOSED.
// Every way a run ends with the gate settles through it, so the subtab, the
// tab and the ledger never disagree about how a run ended.
func (r *run) endRun(ctx context.Context, round tabRound, line string, suites []*frontendv1.FeedMergeTestSuite, log *frontendv1.FeedMergeTestLog, closed string) {
	r.publishRun(round, round.settled(r.o.nowMS(), line), suites, log)
	r.settleTesting(line)
	r.closeTab(ctx, round, closed)
}

// publishRun publishes the run of tests round ROUND at ST.
func (r *run) publishRun(round tabRound, st tabState, suites []*frontendv1.FeedMergeTestSuite, log *frontendv1.FeedMergeTestLog) {
	r.publish(runKey(round.n), runTab(round.n, st, suites, log))
}

// testsRound is the number of the latest tests round: the run a fixing
// attempt follows.
func (r *run) testsRound() int {
	r.mu.Lock()
	defer r.mu.Unlock()
	return r.rounds[TabTests]
}

// drawFixing stands fixing attempt `attempt` live, fresh or resumed, as the
// sub-subtab of run n: the session's turns draw in it, and an open ask draws
// it and the tests tab waiting on the user.
func (r *run) drawFixing(ctx context.Context, round tabRound, n, attempt int, suites []string) {
	r.drawLive(ctx, round, footer.StepFixing, fixingLine(suites), func(f *footer.MergeFacts) {
		f.Attempt, f.MaxAttempts = attempt, MaxFixAttempts
	}, fixingKey(n), func(waiting bool) {
		st := round.live()
		st.waiting = waiting
		r.publishTesting(waiting)
		r.publish(fixingKey(n), fixingTab(st, attempt))
	})
}
