package retarget

import (
	"context"
	"errors"
	"os"
	"regexp"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
	"agent-repld/internal/merge"
	"agent-repld/internal/prompthandler"
	"agent-repld/internal/promptqueue"
	"agent-repld/internal/wsm"
)

// fakeVerbs records the retarget and the undo asked of it.
type fakeVerbs struct {
	change     wsm.ParentChange
	setErr     error
	set        []*ids.WorkspaceID
	restoreErr error
	restored   []wsm.ParentChange
	// calls is the order the two verbs were called in, shared with the
	// prompts fake so a test can assert the sequence.
	calls *[]string
}

func (v *fakeVerbs) SetParent(_ context.Context, _ ids.WorkspaceID, parent *ids.WorkspaceID) (wsm.ParentChange, error) {
	*v.calls = append(*v.calls, "set")
	v.set = append(v.set, parent)
	return v.change, v.setErr
}

func (v *fakeVerbs) RestoreParent(_ context.Context, change wsm.ParentChange) error {
	*v.calls = append(*v.calls, "restore")
	v.restored = append(v.restored, change)
	return v.restoreErr
}

// submission is one Submit call.
type submission struct {
	ws       ids.WorkspaceID
	text     string
	key      string
	origin   conversationv1.PromptOrigin
	delivery wsm.Delivery
	target   *feedid.Ref
}

// fakePrompts records each submission.
type fakePrompts struct {
	err   error
	got   []submission
	calls *[]string
}

func (p *fakePrompts) Submit(_ context.Context, ws ids.WorkspaceID, said *conversationv1.UserSaid, key string, origin conversationv1.PromptOrigin, _ *ids.WorkspaceID, delivery wsm.Delivery, target *feedid.Ref) (prompthandler.Outcome, error) {
	*p.calls = append(*p.calls, "submit")
	text := said.GetContent().GetBlocks()[0].GetText().GetText()
	p.got = append(p.got, submission{ws: ws, text: text, key: key, origin: origin, delivery: delivery, target: target})
	return prompthandler.Outcome{Turn: "t-1"}, p.err
}

type fixture struct {
	verbs   *fakeVerbs
	prompts *fakePrompts
	logs    *dlog.TestSurfaces
	briefs  merge.BriefLoader
	keyErr  error
	calls   []string
	svc     *Service
}

// toParent is a change from the main checkout to workspace p1.
func toParent() wsm.ParentChange {
	parent := ids.WorkspaceID("p1")
	return wsm.ParentChange{
		Workspace: "w1",
		Before:    wsm.MergeTarget{Dir: "/repo", DefaultBranch: "master"},
		After:     wsm.MergeTarget{Parent: &parent, ParentName: "parent-ws", Dir: "/wt/parent-ws", DefaultBranch: "master"},
	}
}

func newFixture(t *testing.T) *fixture {
	t.Helper()
	f := &fixture{logs: dlog.NewTestSurfaces()}
	f.verbs = &fakeVerbs{change: toParent(), calls: &f.calls}
	f.prompts = &fakePrompts{calls: &f.calls}
	f.briefs = merge.BriefsFrom("../../../prompts")
	f.svc = New(Deps{
		Verbs: f.verbs, Prompts: f.prompts, Log: f.logs,
		Briefs: func(name string, values map[string]string) (string, error) { return f.briefs(name, values) },
		NewKey: func() (string, error) { return "key-1", f.keyErr },
	})
	return f
}

func (f *fixture) loggedAt(level string) bool {
	for _, r := range f.logs.Records() {
		if r.Level == level && r.Operation == op {
			return true
		}
	}
	return false
}

func TestRetargetRecordsTheRetargetThenQueuesTheNotice(t *testing.T) {
	// Arrange
	f := newFixture(t)
	parent := ids.WorkspaceID("p1")

	// Act
	err := f.svc.Retarget(context.Background(), "w1", &parent)

	// Assert
	if err != nil {
		t.Fatalf("Retarget: %v", err)
	}
	if strings.Join(f.calls, ",") != "set,submit" {
		t.Fatalf("calls = %v, want the retarget recorded before its notice", f.calls)
	}
	if len(f.verbs.set) != 1 || f.verbs.set[0] == nil || *f.verbs.set[0] != parent {
		t.Fatalf("retargets = %v, want one naming p1", f.verbs.set)
	}
	got := f.prompts.got[0]
	if got.ws != "w1" || got.origin != Origin || got.delivery != wsm.DeliveryDeferred || got.target != nil || got.key != "key-1" {
		t.Fatalf("submission = %+v, want a deferred root-feed notice of the retarget origin into w1", got)
	}
	if !f.loggedAt(dlog.LevelInfo) {
		t.Fatalf("records = %+v, want the retarget recorded at info", f.logs.Records())
	}
}

func TestRetargetNoticeNamesTheNewAndPreviousTargets(t *testing.T) {
	tests := []struct {
		name   string
		change func() wsm.ParentChange
		want   []string
	}{
		{name: "to a parent workspace", change: toParent, want: []string{
			"lands in the workspace `parent-ws`, whose checkout is `/wt/parent-ws`",
			"used to land in the repository's main checkout at `/repo`, on its default branch `master`",
			"`agent-repld workspace finished here -placement back`",
		}},
		{name: "to top-level", change: func() wsm.ParentChange {
			c := toParent()
			c.Before, c.After = c.After, c.Before
			return c
		}, want: []string{
			"lands in the repository's main checkout at `/repo`, on its default branch `master`",
			"used to land in the workspace `parent-ws`",
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.verbs.change = tt.change()

			// Act
			if err := f.svc.Retarget(context.Background(), "w1", nil); err != nil {
				t.Fatalf("Retarget: %v", err)
			}

			// Assert
			text := f.prompts.got[0].text
			for _, want := range tt.want {
				if !strings.Contains(text, want) {
					t.Fatalf("notice %q lacks %q", text, want)
				}
			}
		})
	}
}

func TestRetargetPassesARefusalThroughAndSendsNoNotice(t *testing.T) {
	// Arrange
	f := newFixture(t)
	refusal := errors.New("intended arm: SetWorkspaceParentError.cycle: ...")
	f.verbs.setErr = refusal

	// Act
	err := f.svc.Retarget(context.Background(), "w1", nil)

	// Assert
	if !errors.Is(err, refusal) {
		t.Fatalf("err = %v, want the verb's refusal unchanged", err)
	}
	if len(f.prompts.got) != 0 || len(f.verbs.restored) != 0 {
		t.Fatalf("submissions = %d, undos = %d; want neither for a refused retarget", len(f.prompts.got), len(f.verbs.restored))
	}
}

func TestRetargetUndoesTheRetargetWhenTheNoticeCannotBeQueued(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(f *fixture)
	}{
		{name: "the submission fails", arrange: func(f *fixture) { f.prompts.err = errors.New("queue closed") }},
		{name: "the brief cannot be composed", arrange: func(f *fixture) {
			f.briefs = func(string, map[string]string) (string, error) { return "", os.ErrNotExist }
		}},
		{name: "the key cannot be minted", arrange: func(f *fixture) { f.keyErr = errors.New("no entropy") }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			tt.arrange(f)

			// Act
			err := f.svc.Retarget(context.Background(), "w1", nil)

			// Assert
			if err == nil {
				t.Fatal("a retarget whose notice was not queued was answered as success")
			}
			if len(f.verbs.restored) != 1 || f.verbs.restored[0].After.Dir != toParent().After.Dir {
				t.Fatalf("undos = %+v, want the retarget undone", f.verbs.restored)
			}
			if !f.loggedAt(dlog.LevelError) {
				t.Fatalf("records = %+v, want the failed notice recorded at error", f.logs.Records())
			}
		})
	}
}

func TestRetargetAtAColdGateIsTheGatesRefusalRecordedAtInfo(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.prompts.err = &promptqueue.ColdGateRefusal{Detail: "resumed cold"}

	// Act
	err := f.svc.Retarget(context.Background(), "w1", nil)

	// Assert
	var coldGate *promptqueue.ColdGateRefusal
	if !errors.As(err, &coldGate) {
		t.Fatalf("err = %v, want the cold gate's refusal", err)
	}
	if len(f.verbs.restored) != 1 {
		t.Fatalf("undos = %d, want the retarget undone", len(f.verbs.restored))
	}
	if f.loggedAt(dlog.LevelError) || !f.loggedAt(dlog.LevelInfo) {
		t.Fatalf("records = %+v, want the gate's answer at info and no error", f.logs.Records())
	}
}

func TestRetargetReportsAFailedUndoBesideTheFailedNotice(t *testing.T) {
	// Arrange
	f := newFixture(t)
	notice := errors.New("queue closed")
	undo := errors.New("disk I/O error")
	f.prompts.err = notice
	f.verbs.restoreErr = undo

	// Act
	err := f.svc.Retarget(context.Background(), "w1", nil)

	// Assert
	if !errors.Is(err, notice) || !errors.Is(err, undo) {
		t.Fatalf("err = %v, want both the notice's and the undo's failures", err)
	}
}

func TestRandomKeyIsTheSharedDaemonKeyShape(t *testing.T) {
	// Act
	key, err := randomKey()

	// Assert
	if err != nil {
		t.Fatalf("randomKey: %v", err)
	}
	if !regexp.MustCompile(`^workspace-retargeted-[0-9a-f]{32}$`).MatchString(key) {
		t.Fatalf("key = %q, want prompthandler.MintKey's shape under the retarget prefix", key)
	}
}
