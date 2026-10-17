package merge

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"reflect"
	"regexp"
	"slices"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
	"google.golang.org/protobuf/proto"

	"agent-repld/internal/dlog"
	"agent-repld/internal/gitclient"
	"agent-repld/internal/resolve/footer"
)

func TestSpanRelive(t *testing.T) {
	tests := []struct {
		name string
		span span
		want span
	}{
		{name: "a tab never begun begins now", span: span{}, want: span{StartedMS: 50}},
		{name: "a settled tab begins again now", span: span{StartedMS: 10, EndedMS: 20, Failure: "x"}, want: span{StartedMS: 50}},
		{name: "a live tab keeps its start", span: span{StartedMS: 10}, want: span{StartedMS: 10}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s := tt.span

			// Act
			s.relive(50)

			// Assert
			if s != tt.want {
				t.Fatalf("span = %+v, want %+v", s, tt.want)
			}
		})
	}
}

func TestBeginReplayStandsEveryCommitPendingInANewReplay(t *testing.T) {
	// Arrange: a view that finished one replay.
	v := rebaseView{Span: span{StartedMS: 1, EndedMS: 2}, Replay: 1, Commits: []commitView{{SHA: "old", Span: span{StartedMS: 1, EndedMS: 2}}}}

	// Act
	v.beginReplay(commits(2), 30)

	// Assert
	if v.Replay != 2 || v.Span != (span{StartedMS: 30}) {
		t.Fatalf("view = %+v, want replay 2 live from 30", v)
	}
	if len(v.Commits) != 2 || v.Commits[0].Span.begun() || v.Commits[1].Span.begun() {
		t.Fatalf("commits = %+v, want two pending", v.Commits)
	}
}

func TestReachCommit(t *testing.T) {
	tests := []struct {
		name  string
		start []span
		i     int
		want  []span
	}{
		{
			name:  "the first commit goes live",
			start: []span{{}, {}},
			i:     0,
			want:  []span{{StartedMS: 9}, {}},
		},
		{
			name:  "an earlier unsettled commit is settled on the tip",
			start: []span{{StartedMS: 3}, {}},
			i:     1,
			want:  []span{{StartedMS: 3, EndedMS: 9}, {StartedMS: 9}},
		},
		{
			name:  "an earlier settled commit keeps its own account",
			start: []span{{StartedMS: 3, EndedMS: 4, Failure: "x"}, {}},
			i:     1,
			want:  []span{{StartedMS: 3, EndedMS: 4, Failure: "x"}, {StartedMS: 9}},
		},
		{
			name:  "a live commit keeps its start",
			start: []span{{StartedMS: 5}},
			i:     0,
			want:  []span{{StartedMS: 5}},
		},
		{
			name:  "reaching past the last commit settles them all",
			start: []span{{}, {StartedMS: 5}},
			i:     2,
			want:  []span{{StartedMS: 9, EndedMS: 9}, {StartedMS: 5, EndedMS: 9}},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			v := rebaseView{}
			for _, s := range tt.start {
				v.Commits = append(v.Commits, commitView{Span: s})
			}

			// Act
			v.reachCommit(tt.i, 9)

			// Assert
			var got []span
			for _, c := range v.Commits {
				got = append(got, c.Span)
			}
			if !slices.Equal(got, tt.want) {
				t.Fatalf("spans = %+v, want %+v", got, tt.want)
			}
		})
	}
}

func TestProgressCountsOnlyCommitsOnTheTip(t *testing.T) {
	// Arrange: one on the tip, one failed, one live, one pending.
	v := rebaseView{Commits: []commitView{
		{Span: span{StartedMS: 1, EndedMS: 2}},
		{Span: span{StartedMS: 1, EndedMS: 2, Failure: "x"}},
		{Span: span{StartedMS: 1}},
		{},
	}}

	// Act
	replayed, total := v.progress()

	// Assert
	if replayed != 1 || total != 4 {
		t.Fatalf("progress = %d/%d, want 1/4", replayed, total)
	}
}

func TestCommitTabState(t *testing.T) {
	tests := []struct {
		name    string
		span    span
		waiting bool
		want    string
	}{
		{name: "not reached", span: span{}, want: "pending"},
		{name: "replaying", span: span{StartedMS: 1}, want: "live"},
		{name: "its agent asks", span: span{StartedMS: 1}, waiting: true, want: "waiting"},
		{name: "on the tip", span: span{StartedMS: 1, EndedMS: 2}, want: "settled"},
		{name: "a settled commit is never drawn waiting", span: span{StartedMS: 1, EndedMS: 2}, waiting: true, want: "settled"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			v := rebaseView{Replay: 1, Commits: []commitView{{SHA: "a1b2c3d4e5f6", Subject: "fix", Span: tt.span}}}

			// Act
			commit := v.commitTab(0, tt.waiting).GetRebaseCommit()

			// Assert
			got := ""
			switch commit.GetState().(type) {
			case *frontendv1.FeedMergeTabRebaseCommit_Pending:
				got = "pending"
			case *frontendv1.FeedMergeTabRebaseCommit_Live:
				got = "live"
			case *frontendv1.FeedMergeTabRebaseCommit_WaitingOnUser:
				got = "waiting"
			case *frontendv1.FeedMergeTabRebaseCommit_Settled:
				got = "settled"
			}
			if got != tt.want {
				t.Fatalf("state = %s, want %s", got, tt.want)
			}
		})
	}
}

func TestCommitTabNamesTheCommitAndItsReplay(t *testing.T) {
	// Arrange
	v := rebaseView{Replay: 2, Commits: []commitView{{SHA: "a1b2c3d4e5f6a7b8", Subject: "fix the loop", Lines: []string{"replayed 1/1"}}}}

	// Act
	tab := v.commitTab(0, false)

	// Assert
	label := tab.GetLabel()
	if label.GetCommit().GetShortSha() != "a1b2c3d4e5f6" || label.GetCommit().GetSubject() != "fix the loop" || label.GetRound() != 2 {
		t.Fatalf("label = %+v, want the short hash and the subject as parts, at replay 2", label)
	}
	commit := tab.GetRebaseCommit()
	if len(commit.GetLines()) != 1 || commit.GetLines()[0].GetText() != "replayed 1/1" {
		t.Fatalf("lines = %+v", commit.GetLines())
	}
}

func TestCommitTabCarriesItsConflictOnlyOnceHandedToTheSession(t *testing.T) {
	tests := []struct {
		name     string
		conflict *conflictView
		want     bool
	}{
		{name: "a clean commit has no section", conflict: nil, want: false},
		{name: "a handed-over conflict has one", conflict: &conflictView{Span: span{StartedMS: 1}, Files: []string{"a.go", "b.go"}}, want: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			v := rebaseView{Replay: 1, Commits: []commitView{{SHA: "a1b2c3d", Span: span{StartedMS: 1}, Conflict: tt.conflict}}}

			// Act
			conflict := v.commitTab(0, false).GetRebaseCommit().GetConflict()

			// Assert
			if (conflict != nil) != tt.want {
				t.Fatalf("conflict = %+v, want present %v", conflict, tt.want)
			}
			if tt.want && (len(conflict.GetFiles()) != 2 || conflict.GetFiles()[1].GetPath() != "b.go" || conflict.GetLive() == nil) {
				t.Fatalf("conflict = %+v, want both files, live", conflict)
			}
		})
	}
}

func TestTheConflictSectionIsDrawnWaitingWithItsCommit(t *testing.T) {
	// Arrange
	v := rebaseView{Replay: 1, Commits: []commitView{{SHA: "a1b2c3d", Span: span{StartedMS: 1}, Conflict: &conflictView{Span: span{StartedMS: 2}}}}}

	// Act
	commit := v.commitTab(0, true).GetRebaseCommit()

	// Assert
	if commit.GetWaitingOnUser().GetStartedAtMs() != 1 || commit.GetConflict().GetWaitingOnUser().GetStartedAtMs() != 2 {
		t.Fatalf("commit = %+v, want it and its section waiting, each from its own start", commit)
	}
}

func TestTheParentTabsAreDrawnWaitingWhileLive(t *testing.T) {
	// Arrange
	rebasing := rebaseView{Span: span{StartedMS: 7}}
	gate := testsView{Span: span{StartedMS: 8}}

	// Act
	r := rebasing.rebasingTab(true).GetRebasing()
	tt := gate.testsTab(true).GetTests()

	// Assert
	if r.GetWaitingOnUser().GetStartedAtMs() != 7 || tt.GetWaitingOnUser().GetStartedAtMs() != 8 {
		t.Fatalf("rebasing %+v, tests %+v; want both waiting from their starts", r, tt)
	}
}

func TestTheParentTabsCarryTheirOwnLabels(t *testing.T) {
	// Arrange
	rebasing, tests := rebaseView{Span: span{StartedMS: 1}}, testsView{Span: span{StartedMS: 1}}

	// Act
	rl, tl := rebasing.rebasingTab(false).GetLabel(), tests.testsTab(false).GetLabel()

	// Assert
	if rl.GetText() != "rebasing" || rl.GetRound() != 1 || tl.GetText() != "tests" || tl.GetRound() != 1 {
		t.Fatalf("labels = %+v, %+v; want the one rebasing and the one tests tab", rl, tl)
	}
}

func TestRunTabCarriesItsOutputAndNumber(t *testing.T) {
	// Arrange
	suites := []*frontendv1.FeedMergeTestSuite{{Name: "daemon"}}
	log := &frontendv1.FeedMergeTestLog{Label: &frontendv1.FeedMergeTestLogLabel{Text: "~/x.log"}}

	// Act
	tab := runTab(3, tabState{startedMS: 1}, suites, log)

	// Assert
	if tab.GetLabel().GetText() != "run 3" || tab.GetLabel().GetRound() != 1 {
		t.Fatalf("label = %+v, want run 3", tab.GetLabel())
	}
	out := tab.GetTestsRun().GetOutput()
	if len(out.GetSuites()) != 1 || out.GetLog().GetLabel().GetText() != "~/x.log" || tab.GetTestsRun().GetLive() == nil {
		t.Fatalf("run = %+v, want its suites and log, live", tab.GetTestsRun())
	}
}

func TestFixingTabState(t *testing.T) {
	tests := []struct {
		name  string
		state tabState
		want  string
	}{
		{name: "live", state: tabState{startedMS: 1}, want: "live"},
		{name: "waiting", state: tabState{startedMS: 1, waiting: true}, want: "waiting"},
		{name: "settled", state: tabState{startedMS: 1, settled: true, endedMS: 2}, want: "settled"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			fixing := fixingTab(tt.state, 2).GetTestsRunFixing()

			// Assert
			got := ""
			switch fixing.GetState().(type) {
			case *frontendv1.FeedMergeTabTestsRunFixing_Live:
				got = "live"
			case *frontendv1.FeedMergeTabTestsRunFixing_WaitingOnUser:
				got = "waiting"
			case *frontendv1.FeedMergeTabTestsRunFixing_Settled:
				got = "settled"
			}
			if got != tt.want || fixing.GetAttempt().GetAttempt() != 2 || fixing.GetAttempt().GetMaxAttempts() != MaxFixAttempts {
				t.Fatalf("fixing = %+v, want %s at attempt 2", fixing, tt.want)
			}
		})
	}
}

func TestTheSubtabKeysNestUnderTheirTabs(t *testing.T) {
	tests := []struct {
		name   string
		key    tabKey
		parent tabKey
	}{
		{name: "a commit under the rebasing tab", key: commitKey(2, 3), parent: rebasingKey()},
		{name: "a run under the tests tab", key: runKey(4), parent: testsKey()},
		{name: "a fixing attempt under its run", key: fixingKey(4), parent: runKey(4)},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act / Assert
			if tt.key.parent == nil || tt.key.parent.id != tt.parent.id || tt.key.parent.sub != tt.parent.sub {
				t.Fatalf("parent = %+v, want %+v", tt.key.parent, tt.parent)
			}
		})
	}
}

func TestEverySubtabKeyIsDistinct(t *testing.T) {
	// Arrange
	keys := []tabKey{commitKey(1, 0), commitKey(1, 1), commitKey(2, 0), runKey(1), runKey(2), fixingKey(1), rebasingKey(), testsKey()}

	// Act
	seen := map[[2]string]bool{}
	for _, k := range keys {
		seen[[2]string{k.id, k.sub}] = true
	}

	// Assert
	if len(seen) != len(keys) {
		t.Fatalf("%d distinct keys of %d, want every one distinct", len(seen), len(keys))
	}
}

func TestTabRowParentsASubtabAndNotATopLevelTab(t *testing.T) {
	// Act
	top := tabRow(theWorkspace, "lease-1", rebasingKey(), &frontendv1.FeedMergeTab{})
	sub := tabRow(theWorkspace, "lease-1", commitKey(1, 0), &frontendv1.FeedMergeTab{})

	// Assert
	if top.GetParent() != nil {
		t.Fatalf("top-level parent = %+v, want none", top.GetParent())
	}
	if sub.GetParent().GetRow().GetValue() != top.GetId().GetValue() {
		t.Fatalf("subtab parent = %+v, want the rebasing tab %+v", sub.GetParent(), top.GetId())
	}
}

func TestFrontTabLabelNamesTheTopLevelTab(t *testing.T) {
	tests := []struct {
		round tabRound
		want  string
		n     uint32
	}{
		{round: tabRound{kind: TabConflicts, n: 2}, want: "rebasing", n: 1},
		{round: tabRound{kind: TabRebasing, n: 3}, want: "rebasing", n: 1},
		{round: tabRound{kind: TabFixes, n: 2}, want: "tests", n: 1},
		{round: tabRound{kind: TabTests, n: 2}, want: "tests", n: 1},
		{round: tabRound{kind: TabCommitting, n: 2}, want: "committing", n: 2},
	}
	for _, tt := range tests {
		t.Run(tt.round.kind, func(t *testing.T) {
			// Act
			label := frontTabLabel(tt.round)

			// Assert
			if label.GetText() != tt.want || label.GetRound() != tt.n {
				t.Fatalf("label = %+v, want %s (%d)", label, tt.want, tt.n)
			}
		})
	}
}

func TestRebaseViewCloneSharesNothing(t *testing.T) {
	// Arrange
	v := rebaseView{
		Lines:   []rebaseLine{textLine("a"), {Replaying: &replayingLine{Branch: "b1", TargetWorkspace: &workspaceRefDoc{ID: "ws-t"}}}},
		Commits: []commitView{{Lines: []string{"b"}, Conflict: &conflictView{Files: []string{"c"}}}},
	}

	// Act
	c := v.clone()
	c.Lines[0], c.Commits[0].Lines[0], c.Commits[0].Conflict.Files[0] = textLine("x"), "y", "z"
	c.Lines[1].Replaying.Branch, c.Lines[1].Replaying.TargetWorkspace.ID = "b2", "ws-other"
	c.Commits[0].Conflict.Span.StartedMS = 5

	// Assert
	if v.Lines[0].Text != "a" || v.Commits[0].Lines[0] != "b" || v.Commits[0].Conflict.Files[0] != "c" || v.Commits[0].Conflict.Span.StartedMS != 0 {
		t.Fatalf("original = %+v, want it untouched by its clone", v)
	}
	if v.Lines[1].Replaying.Branch != "b1" || v.Lines[1].Replaying.TargetWorkspace.ID != "ws-t" {
		t.Fatalf("original replay line = %+v, want it untouched by its clone", v.Lines[1].Replaying)
	}
}

func TestRebaseLineReadsEveryRecordedShape(t *testing.T) {
	tests := []struct {
		name    string
		data    string
		want    rebaseLine
		wantErr bool
	}{
		{name: "a bare string, as lines were recorded before their parts", data: `"already on master; nothing to merge"`, want: textLine("already on master; nothing to merge")},
		{name: "a text line object", data: `{"text":"failed: boom"}`, want: textLine("failed: boom")},
		{
			name: "a replay line object",
			data: `{"replaying":{"commits":2,"branch":"ABC/fix","target_branch":"master","target_dir":"/t","tip":"4f2a1c","target_workspace":{"id":"ws-t","dir":"/t"}}}`,
			want: rebaseLine{Replaying: &replayingLine{Commits: 2, Branch: "ABC/fix", TargetBranch: "master", TargetDir: "/t", Tip: "4f2a1c", TargetWorkspace: &workspaceRefDoc{ID: "ws-t", Dir: "/t"}}},
		},
		{name: "neither", data: `7`, wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			var got rebaseLine

			// Act
			err := json.Unmarshal([]byte(tt.data), &got)

			// Assert
			if tt.wantErr {
				if err == nil || !strings.Contains(err.Error(), "neither a string nor a line object") {
					t.Fatalf("err = %v, want the line refused by name", err)
				}
				return
			}
			if err != nil {
				t.Fatalf("unmarshal: %v", err)
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("line = %+v, want %+v", got, tt.want)
			}
		})
	}
}

func TestRebaseLineDrawsItsArm(t *testing.T) {
	replay := func(ws *workspaceRefDoc) rebaseLine {
		return rebaseLine{Replaying: &replayingLine{Commits: 5, Branch: "ABC/fix", TargetBranch: "master", TargetDir: "/repo", Tip: "4f2a1c0123456789", TargetWorkspace: ws}}
	}
	tests := []struct {
		name string
		line rebaseLine
		want *frontendv1.FeedMergeRebaseLine
	}{
		{
			name: "a sentence is drawn verbatim",
			line: textLine("ABC/fix is already on master's tip; nothing to replay"),
			want: &frontendv1.FeedMergeRebaseLine{Line: &frontendv1.FeedMergeRebaseLine_Text{Text: "ABC/fix is already on master's tip; nothing to replay"}},
		},
		{
			name: "a replay onto a workspace's checkout names that workspace",
			line: replay(&workspaceRefDoc{ID: "ws-t", Dir: "/repo"}),
			want: &frontendv1.FeedMergeRebaseLine{Line: &frontendv1.FeedMergeRebaseLine_Replaying{Replaying: &frontendv1.FeedMergeRebaseReplaying{
				Commits: 5, Branch: "ABC/fix", Tip: "4f2a1c0123456789", TipShortSha: "4f2a1c012345",
				Target: &frontendv1.FeedMergeRebaseTarget{Branch: "master", RepositoryDir: "/repo", Workspace: &workspacev1.WorkspaceRef{Id: "ws-t", Dir: "/repo"}},
			}}},
		},
		{
			name: "a replay onto a checkout no workspace is names none",
			line: replay(nil),
			want: &frontendv1.FeedMergeRebaseLine{Line: &frontendv1.FeedMergeRebaseLine_Replaying{Replaying: &frontendv1.FeedMergeRebaseReplaying{
				Commits: 5, Branch: "ABC/fix", Tip: "4f2a1c0123456789", TipShortSha: "4f2a1c012345",
				Target: &frontendv1.FeedMergeRebaseTarget{Branch: "master", RepositoryDir: "/repo"},
			}}},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := tt.line.drawn()

			// Assert
			if !proto.Equal(got, tt.want) {
				t.Fatalf("drawn = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestTheProgressRecordCarriesTheViewsAcrossARestore(t *testing.T) {
	// Arrange: a run that drew a replay and a tests tab.
	h := newHarness(t)
	r := ledgeredRun(t, h)
	r.rebasing.beginReplay([]gitclient.Commit{{SHA: "a1b2c3d", Subject: "fix"}}, 10)
	r.rebasing.Commits[0].Lines = []string{"conflicted in 1 file(s)"}
	r.testing.Span = span{StartedMS: 20}

	// Act
	if err := r.checkpoint(context.Background(), TabRebasing, nil); err != nil {
		t.Fatalf("checkpoint: %v", err)
	}
	restored := ledgeredRun(t, h)
	restored.restoreFrom(r.prog)

	// Assert
	if restored.rebasing.Replay != 1 || len(restored.rebasing.Commits) != 1 || restored.rebasing.Commits[0].Lines[0] != "conflicted in 1 file(s)" {
		t.Fatalf("rebasing = %+v, want the replay as drawn", restored.rebasing)
	}
	if restored.testing.Span != (span{StartedMS: 20}) {
		t.Fatalf("testing = %+v, want the tests tab as drawn", restored.testing)
	}
}

func TestAResumedRebaseDrawsTheCommitsItReplays(t *testing.T) {
	// Arrange: a record from before the view existed, two commits on the tip.
	h := newHarness(t)
	r := ledgeredRun(t, h)
	round := r.openTab(context.Background(), TabRebasing)
	p := &replay{round: round, commits: commits(3), done: 2}

	// Act
	r.resumeReplay(p)

	// Assert
	if r.rebasing.Replay != 1 || len(r.rebasing.Commits) != 3 {
		t.Fatalf("rebasing = %+v, want one replay of the three commits", r.rebasing)
	}
	if !r.rebasing.Commits[1].Span.settled() || r.rebasing.Commits[2].Span.settled() || !r.rebasing.Commits[2].Span.begun() {
		t.Fatalf("commits = %+v, want two on the tip and the third live", r.rebasing.Commits)
	}
}

func TestAMergeThatStartsOverReplaysUnderTheSameRebasingTab(t *testing.T) {
	// Arrange: the target moves before the first attempt commits, and the
	// rebase onto it gives a new tree, so the gate runs again.
	h := newHarness(t)
	landing(h, 1)
	h.gatePasses("daemon")
	h.gatePasses("daemon")
	h.git.refSeqs["HEAD"] = []string{"old", "new", "new", "new"}
	h.git.treeSeq = []string{"passed-tree", "new-tree"}

	// Act
	admitted(t, h)

	// Assert: one rebasing tab, and the second replay's commit drawn as round 2.
	if rounds := h.feed.roundsOfKind(TabRebasing); !slices.Equal(rounds, []int{1}) {
		t.Fatalf("rebasing rounds = %v, want the one rebasing tab", rounds)
	}
	if rounds := h.feed.roundsOfKind(rowRebaseCommit); !slices.Equal(rounds, []int{1, 2}) {
		t.Fatalf("commit subtab rounds = %v, want replay 1 and replay 2", rounds)
	}
	if rounds := h.feed.roundsOfKind(TabTests); !slices.Equal(rounds, []int{1}) {
		t.Fatalf("tests rounds = %v, want the one tests tab", rounds)
	}
	if runs := h.feed.labelsOfKind(rowTestsRun); !slices.Equal(runs, []string{"run 1", "run 2"}) {
		t.Fatalf("runs = %v, want both attempts' runs", runs)
	}
}

func TestFixesExhaustedFailsTheMergeRecordsItAndSettlesTheTestsTab(t *testing.T) {
	// Arrange
	h := newHarness(t)
	r := ledgeredRun(t, h)
	r.testing.Span = span{StartedMS: 1}

	// Act
	out := r.fixesExhausted(context.Background(), "/a/log", dlog.Context{"speculative": true})

	// Assert
	want := fmt.Sprintf("the tests still fail after %d fixing attempts; the whole run is archived at /a/log", MaxFixAttempts)
	if out.failed != want || out.area != footer.FailedTests {
		t.Fatalf("outcome = %+v, want the tests failure %q", out, want)
	}
	record, found := h.recordFor("info", "daemon.merge.fixes")
	if !found || record.Context["archive"] != "/a/log" || record.Context["speculative"] != true {
		t.Fatalf("record = %+v, want the archive and the extra field", record)
	}
	if !r.testing.Span.settled() || r.testing.Span.Failure != want {
		t.Fatalf("tests tab = %+v, want it settled failed with the summary", r.testing.Span)
	}
}

// Both gates give up through fixesExhausted; a hand-rolled copy of its
// summary anywhere else would drift from it.
func TestEveryExhaustedGateGivesUpThroughFixesExhausted(t *testing.T) {
	// Arrange
	sources := []string{"process.go", "chain.go"}

	// Act
	calls, copies := 0, 0
	for _, name := range sources {
		body, err := os.ReadFile(name)
		if err != nil {
			t.Fatalf("reading %s: %v", name, err)
		}
		calls += strings.Count(string(body), "r.fixesExhausted(")
		copies += strings.Count(string(body), "fixing attempts; the whole run is archived")
	}

	// Assert: two callers, and the summary spelled once, in the helper.
	if calls != 2 || copies != 1 {
		t.Fatalf("fixesExhausted calls = %d, summary spellings = %d; want 2 and 1", calls, copies)
	}
}

func TestEndRunSettlesTheRunTheTestsTabAndTheRoundAsOne(t *testing.T) {
	// Arrange.
	tests := []struct {
		name, line, closed string
		failed             bool
	}{
		{name: "a pass", line: "", closed: "succeeded"},
		{name: "a cut", line: "the merge ahead moved; the tests run again on its new tip", closed: "restarted", failed: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			h := newHarness(t)
			r := ledgeredRun(t, h)
			round := r.openTab(context.Background(), TabTests)
			r.openTesting(round)

			// Act.
			r.endRun(context.Background(), round, tt.line, nil, nil, tt.closed)

			// Assert.
			run := h.feed.lastTabOfKind(rowTestsRun).GetTestsRun().GetSettled()
			tests := h.feed.lastTabOfKind(TabTests).GetTests().GetSettled()
			if run == nil || tests == nil {
				t.Fatalf("run settled %v, tests tab settled %v; want both settled", run, tests)
			}
			if got := run.GetFailed() != nil; got != tt.failed || (tests.GetFailed() != nil) != tt.failed {
				t.Fatalf("run failed %v, tests tab failed %v; want both %v", got, tests.GetFailed() != nil, tt.failed)
			}
			intervals := ledgerIntervals(h)
			if last := intervals[len(intervals)-1]; last.Kind != TabTests || last.Outcome != tt.closed {
				t.Fatalf("the round closed as %+v, want tests %s", last, tt.closed)
			}
		})
	}
}

// handSettled is a run's settled draw followed by its tests tab settling.
var handSettled = regexp.MustCompile(`publishRun\([^\n]*\.settled\([^\n]*\n\s*(g\.)?r\.settleTesting\(`)

func TestEveryRunEndingSettlesThroughEndRun(t *testing.T) {
	// Arrange.
	for _, file := range []string{"gate.go", "chain.go", "process.go"} {
		body, err := os.ReadFile(file)
		if err != nil {
			t.Fatalf("reading %s: %v", file, err)
		}

		// Act / Assert: a run settled together with its tests tab never
		// stands outside endRun. (A failed run alone, which leaves the tab
		// live for the fixing attempt after it, is not that shape.)
		if handSettled.Match(body) {
			t.Fatalf("%s settles a run and its tests tab by hand instead of through endRun", file)
		}
	}
}
