package mergeworkspace

import (
	"context"
	"errors"
	"os"
	"regexp"
	"strings"
	"sync"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
	"agent-repld/internal/merge"
	"agent-repld/internal/prompthandler"
	"agent-repld/internal/tempdirs/tempdirstest"
	"agent-repld/internal/wsm"
)

// fakeWorkspaces records the verbs this package calls.
type fakeWorkspaces struct {
	db        wsm.DB
	t         *testing.T
	createErr error
	nukeErr   error
	// onNuke, when set, runs as Nuke is called, before nukeErr is answered.
	onNuke  func()
	created []string
	nuked   []ids.WorkspaceID
}

func (f *fakeWorkspaces) CreateMergeWorkspace(ctx context.Context, requester wsm.Workspace, origin wsm.MergeOrigin, branch string) (wsm.Workspace, error) {
	f.created = append(f.created, string(requester.ID)+":"+branch)
	if f.createErr != nil {
		return wsm.Workspace{}, f.createErr
	}
	return register(f.t, f.db, "sample-merge", &origin), nil
}

// Nuke records the nuke and, as a real nuke does, removes the workspace from
// the registry, so a later sweep cannot find it again.
func (f *fakeWorkspaces) Nuke(ctx context.Context, ws ids.WorkspaceID) error {
	if f.onNuke != nil {
		f.onNuke()
	}
	if f.nukeErr != nil {
		return f.nukeErr
	}
	f.nuked = append(f.nuked, ws)
	// Errorf, not Fatalf: Run calls this off the test goroutine.
	if _, err := f.db.Forget(ctx, ws); err != nil {
		f.t.Errorf("Forget %s: %v", ws, err)
		return err
	}
	return nil
}

// submitted is one handback Submit was handed.
type submitted struct {
	ws       ids.WorkspaceID
	text     string
	origin   conversationv1.PromptOrigin
	delivery wsm.Delivery
}

// fakePrompts records every submission.
type fakePrompts struct {
	mu   sync.Mutex
	err  error
	sent []submitted
}

func (f *fakePrompts) Submit(_ context.Context, ws ids.WorkspaceID, said *conversationv1.UserSaid, _ string, origin conversationv1.PromptOrigin, delivery wsm.Delivery, _ *feedid.Ref) (prompthandler.Outcome, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.err != nil {
		return prompthandler.Outcome{}, f.err
	}
	f.sent = append(f.sent, submitted{ws: ws, text: said.GetContent().GetBlocks()[0].GetText().GetText(), origin: origin, delivery: delivery})
	return prompthandler.Outcome{Turn: "turn-handback"}, nil
}

// marked is one MarkMergeWorkspaceRemoved call.
type marked struct {
	ws, mergeWS ids.WorkspaceID
	at          int64
}

// fakeFeed records every removal restatement.
type fakeFeed struct {
	marks []marked
}

func (f *fakeFeed) MarkMergeWorkspaceRemoved(ws, mergeWS ids.WorkspaceID, at int64) int {
	f.marks = append(f.marks, marked{ws: ws, mergeWS: mergeWS, at: at})
	return 1
}

// fixture is one service over an in-memory registry.
type fixture struct {
	svc        *Service
	db         wsm.DB
	workspaces *fakeWorkspaces
	prompts    *fakePrompts
	feed       *fakeFeed
	logs       *dlog.TestSurfaces
	now        time.Time
	briefErr   error
}

func newFixture(t *testing.T) *fixture {
	t.Helper()
	db, err := wsm.OpenInMemory(context.Background(), wsm.WithTemporaryGuard(tempdirstest.Guard(t)))
	if err != nil {
		t.Fatalf("OpenInMemory: %v", err)
	}
	t.Cleanup(func() { db.Close() })
	f := &fixture{db: db, prompts: &fakePrompts{}, feed: &fakeFeed{}, logs: dlog.NewTestSurfaces(), now: time.Now()}
	f.workspaces = &fakeWorkspaces{db: db, t: t}
	f.svc = New(Deps{
		DB: db, Workspaces: f.workspaces, Prompts: f.prompts, Feed: f.feed, Log: f.logs,
		Briefs: func(name string, values map[string]string) (string, error) {
			if f.briefErr != nil {
				return "", f.briefErr
			}
			return values["label"] + "\n\n" + values["report"], nil
		},
		Now: func() time.Time { return f.now },
	})
	return f
}

// register registers one workspace, a merge workspace when origin is set.
func register(t *testing.T, db wsm.DB, name string, origin *wsm.MergeOrigin) wsm.Workspace {
	t.Helper()
	dir := t.TempDir()
	facts := wsm.RegisterFacts{Name: name, Branch: name, ParentBranch: "master", RepoDir: dir, MergeOf: origin}
	if origin != nil {
		facts.Parent = &origin.Requester
	}
	ws, _, err := db.RegisterWorkspace(context.Background(), dir, facts)
	if err != nil {
		t.Fatalf("RegisterWorkspace: %v", err)
	}
	return ws
}

// loggedAt reports whether a record of level and operation was written.
func (f *fixture) loggedAt(level string) bool {
	for _, r := range f.logs.Records() {
		if r.Level == level && r.Operation == op {
			return true
		}
	}
	return false
}

func TestForkCreatesTheMergeWorkspace(t *testing.T) {
	// Arrange
	f := newFixture(t)
	requester := register(t, f.db, "sample", nil)

	// Act
	ws, err := f.svc.Fork(context.Background(), requester, "request-1", "sample")

	// Assert
	if err != nil || ws.MergeOf == nil || ws.MergeOf.Request != "request-1" {
		t.Fatalf("Fork = %+v, %v; want the merge workspace forked for the request", ws, err)
	}
	if len(f.workspaces.created) != 1 || f.workspaces.created[0] != string(requester.ID)+":sample" {
		t.Fatalf("created = %v, want one of the requester's branch", f.workspaces.created)
	}
}

func TestForkFindsTheWorkspaceARestartLeftForked(t *testing.T) {
	// Arrange
	f := newFixture(t)
	requester := register(t, f.db, "sample", nil)
	forked := register(t, f.db, "sample-merge", &wsm.MergeOrigin{Requester: requester.ID, Request: "request-1"})

	// Act
	ws, err := f.svc.Fork(context.Background(), requester, "request-1", "sample")

	// Assert
	if err != nil || ws.ID != forked.ID || len(f.workspaces.created) != 0 {
		t.Fatalf("Fork = %v, %v with %v created; want the existing workspace and no new one", ws.ID, err, f.workspaces.created)
	}
}

func TestForkAnswersTheCreatesFailure(t *testing.T) {
	// Arrange
	f := newFixture(t)
	requester := register(t, f.db, "sample", nil)
	f.workspaces.createErr = errors.New("no worktree")

	// Act
	_, err := f.svc.Fork(context.Background(), requester, "request-1", "sample")

	// Assert
	if !errors.Is(err, f.workspaces.createErr) {
		t.Fatalf("Fork = %v, want the create's failure", err)
	}
}

func TestHandBackSubmitsADeferredReportLedByItsLabel(t *testing.T) {
	mergeWS := wsm.Workspace{ID: "ws-merge", Name: "sample-merge", Dir: "/w/sample-merge"}
	tests := []struct {
		name      string
		outcome   merge.DetachedOutcome
		wantLabel string
		wantText  string
	}{
		{name: "landed", outcome: merge.DetachedOutcome{Kind: merge.DetachedLanded, Summary: "it landed as commit abc", Branch: "sample"},
			wantLabel: "merge of sample landed", wantText: "is closed now"},
		{name: "failed", outcome: merge.DetachedOutcome{Kind: merge.DetachedFailed, Summary: "tests failed", Branch: "sample"},
			wantLabel: "merge of sample failed", wantText: "/w/sample-merge"},
		{name: "abandoned", outcome: merge.DetachedOutcome{Kind: merge.DetachedAbandoned, Summary: "the fork failed", Branch: "sample"},
			wantLabel: "merge of sample did not run", wantText: "the fork failed"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)

			// Act
			err := f.svc.HandBack(context.Background(), "ws-requester", mergeWS, tt.outcome)

			// Assert
			if err != nil || len(f.prompts.sent) != 1 {
				t.Fatalf("HandBack = %v with %d submissions, want one", err, len(f.prompts.sent))
			}
			sent := f.prompts.sent[0]
			if sent.ws != "ws-requester" || sent.origin != conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_HANDBACK || sent.delivery != wsm.DeliveryDeferred {
				t.Fatalf("submission = %+v, want a deferred merge handback to the requester", sent)
			}
			if first, _, _ := strings.Cut(sent.text, "\n"); first != tt.wantLabel || !strings.Contains(sent.text, tt.wantText) {
				t.Fatalf("text = %q, want it led by %q and naming %q", sent.text, tt.wantLabel, tt.wantText)
			}
		})
	}
}

func TestHandBackFailsLoudlyWithNothingSubmitted(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(f *fixture)
		outcome merge.DetachedOutcome
	}{
		{name: "the brief", arrange: func(f *fixture) { f.briefErr = errors.New("no brief") },
			outcome: merge.DetachedOutcome{Kind: merge.DetachedLanded}},
		{name: "the submission", arrange: func(f *fixture) { f.prompts.err = errors.New("refused") },
			outcome: merge.DetachedOutcome{Kind: merge.DetachedLanded}},
		{name: "an undeclared outcome", arrange: func(*fixture) {},
			outcome: merge.DetachedOutcome{Kind: merge.DetachedOutcomeKind(99)}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			tt.arrange(f)

			// Act
			err := f.svc.HandBack(context.Background(), "ws-requester", wsm.Workspace{}, tt.outcome)

			// Assert
			if err == nil || len(f.prompts.sent) != 0 {
				t.Fatalf("HandBack = %v with %d submissions, want an error and none", err, len(f.prompts.sent))
			}
			if !f.loggedAt(dlog.LevelError) {
				t.Fatal("the failed handback was not recorded at ERROR")
			}
		})
	}
}

func TestSweepRemovesAMergeWorkspaceIdleFor15Days(t *testing.T) {
	// Arrange
	f := newFixture(t)
	requester := register(t, f.db, "sample", nil)
	idle := register(t, f.db, "sample-merge", &wsm.MergeOrigin{Requester: requester.ID, Request: "request-1"})
	f.now = time.Now().Add(IdleLimit + time.Hour)

	// Act
	removed := f.svc.Sweep(context.Background())

	// Assert: recorded, destroyed, and its requester's switches restated.
	if removed != 1 || len(f.workspaces.nuked) != 1 || f.workspaces.nuked[0] != idle.ID {
		t.Fatalf("removed %d, nuked %v; want the idle merge workspace", removed, f.workspaces.nuked)
	}
	at, found, err := f.db.MergeWorkspaceRemovedAt(context.Background(), idle.ID)
	if err != nil || !found {
		t.Fatalf("removal record = %v, %v; want it recorded", found, err)
	}
	if len(f.feed.marks) != 1 || f.feed.marks[0] != (marked{ws: requester.ID, mergeWS: idle.ID, at: at.UnixMilli()}) {
		t.Fatalf("marks = %+v, want the requester's switches restated at the recorded instant", f.feed.marks)
	}
}

func TestSweepLeavesWhatIsNotAnIdleMergeWorkspace(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, f *fixture)
	}{
		{name: "an ordinary workspace", arrange: func(t *testing.T, f *fixture) {
			register(t, f.db, "plain", nil)
			f.now = time.Now().Add(IdleLimit + time.Hour)
		}},
		{name: "a merge workspace used recently", arrange: func(t *testing.T, f *fixture) {
			requester := register(t, f.db, "sample", nil)
			register(t, f.db, "sample-merge", &wsm.MergeOrigin{Requester: requester.ID, Request: "request-1"})
			f.now = time.Now().Add(IdleLimit - time.Hour)
		}},
		{name: "a merge workspace whose merge is queued", arrange: func(t *testing.T, f *fixture) {
			requester := register(t, f.db, "sample", nil)
			ws := register(t, f.db, "sample-merge", &wsm.MergeOrigin{Requester: requester.ID, Request: "request-1"})
			if err := f.db.RequestMerge(context.Background(), wsm.RepoKey(t.TempDir()), ws.ID, wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch}, time.Now()); err != nil {
				t.Fatalf("RequestMerge: %v", err)
			}
			f.now = time.Now().Add(IdleLimit + time.Hour)
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			tt.arrange(t, f)

			// Act
			removed := f.svc.Sweep(context.Background())

			// Assert
			if removed != 0 || len(f.workspaces.nuked) != 0 || len(f.feed.marks) != 0 {
				t.Fatalf("removed %d, nuked %v, marks %v; want nothing touched", removed, f.workspaces.nuked, f.feed.marks)
			}
		})
	}
}

func TestSweepLeavesAMergeWorkspaceItCouldNotDestroyForTheNextSweep(t *testing.T) {
	// Arrange
	f := newFixture(t)
	requester := register(t, f.db, "sample", nil)
	idle := register(t, f.db, "sample-merge", &wsm.MergeOrigin{Requester: requester.ID, Request: "request-1"})
	f.now = time.Now().Add(IdleLimit + time.Hour)
	f.workspaces.nukeErr = errors.New("git refused")

	// Act
	removed := f.svc.Sweep(context.Background())

	// Assert: not counted, no switch restated, the removal still recorded for
	// the retry, and the failure warned.
	if removed != 0 || len(f.feed.marks) != 0 {
		t.Fatalf("removed %d, marks %v; want nothing removed", removed, f.feed.marks)
	}
	if _, found, _ := f.db.MergeWorkspaceRemovedAt(context.Background(), idle.ID); !found {
		t.Fatal("the removal was not recorded ahead of the retry")
	}
	if !f.loggedAt(dlog.LevelWarn) {
		t.Fatal("the failed destroy was not recorded")
	}
}

func TestSweepRestatesNothingForARequesterThatIsGone(t *testing.T) {
	// Arrange: the requester was forgotten; its merge workspace stays.
	f := newFixture(t)
	requester := register(t, f.db, "sample", nil)
	register(t, f.db, "sample-merge", &wsm.MergeOrigin{Requester: requester.ID, Request: "request-1"})
	if _, err := f.db.Forget(context.Background(), requester.ID); err != nil {
		t.Fatalf("Forget: %v", err)
	}
	f.now = time.Now().Add(IdleLimit + time.Hour)

	// Act
	removed := f.svc.Sweep(context.Background())

	// Assert
	if removed != 1 || len(f.feed.marks) != 0 {
		t.Fatalf("removed %d, marks %v; want it removed with no feed to restate", removed, f.feed.marks)
	}
}

func TestSweepCutShortByTheStandDownRecordsItAtInfo(t *testing.T) {
	// Arrange
	f := newFixture(t)
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act
	removed := f.svc.Sweep(ctx)

	// Assert
	if removed != 0 {
		t.Fatalf("removed %d, want nothing", removed)
	}
	if f.loggedAt(dlog.LevelError) || f.loggedAt(dlog.LevelWarn) {
		t.Fatalf("a stand-down was recorded as a failure: %v", f.logs.Records())
	}
	for _, r := range f.logs.Records() {
		if r.Level == dlog.LevelInfo && r.Operation == op && r.Message == "the daemon stood down mid-sweep" &&
			r.Context["left_undone"] == "could not list the workspaces; no merge workspace is swept" && r.Context["cause"] != nil {
			return
		}
	}
	t.Fatalf("the stand-down was not recorded with what it left undone: %v", f.logs.Records())
}

func TestSweepWhoseDestroyTheStandDownCutShortRecordsItAtInfo(t *testing.T) {
	// Arrange
	f := newFixture(t)
	requester := register(t, f.db, "sample", nil)
	register(t, f.db, "sample-merge", &wsm.MergeOrigin{Requester: requester.ID, Request: "request-1"})
	f.now = time.Now().Add(IdleLimit + time.Hour)
	ctx, cancel := context.WithCancel(context.Background())
	f.workspaces.onNuke = cancel
	f.workspaces.nukeErr = context.Canceled

	// Act
	removed := f.svc.Sweep(ctx)

	// Assert
	if removed != 0 {
		t.Fatalf("removed %d, want nothing", removed)
	}
	if f.loggedAt(dlog.LevelWarn) {
		t.Fatalf("a destroy the stand-down cut short was warned: %v", f.logs.Records())
	}
	for _, r := range f.logs.Records() {
		if r.Level == dlog.LevelInfo && r.Message == "the daemon stood down mid-sweep" &&
			r.Context["left_undone"] == "an idle merge workspace could not be destroyed; the next sweep tries again" &&
			r.Context["merge_workspace"] != nil {
			return
		}
	}
	t.Fatalf("the cut-short destroy was not recorded with its workspace: %v", f.logs.Records())
}

func TestSweepFailed(t *testing.T) {
	ended, cancel := context.WithCancel(context.Background())
	cancel()
	cases := []struct {
		name      string
		ctx       context.Context
		wantLevel string
		wantMsg   string
	}{
		{"a failure under a live lifetime is recorded at its level", context.Background(), dlog.LevelError, "the step failed"},
		{"a failure under an ended lifetime is a stand-down", ended, dlog.LevelInfo, "the daemon stood down mid-sweep"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			logs := dlog.NewTestSurfaces()
			log := logs.Global()

			// Act
			sweepFailed(tc.ctx, log, log.Error, "the step failed", dlog.Context{"merge_workspace": "m1"}, errors.New("boom"))

			// Assert
			records := logs.Records()
			if len(records) != 1 {
				t.Fatalf("records = %v, want one", records)
			}
			r := records[0]
			if r.Level != tc.wantLevel || r.Message != tc.wantMsg || r.Context["cause"] != "boom" || r.Context["merge_workspace"] != "m1" {
				t.Fatalf("record = %+v, want %s %q with the cause and fields", r, tc.wantLevel, tc.wantMsg)
			}
		})
	}
}

// TestSweepSitesShareSweepFailed fails a sweep step that records its failure
// by hand instead of through sweepFailed, which is what keeps a stand-down
// from reading as a failure.
func TestSweepSitesShareSweepFailed(t *testing.T) {
	// Arrange
	src, err := os.ReadFile("mergeworkspace.go")
	if err != nil {
		t.Fatalf("read: %v", err)
	}
	text := string(src)
	start := strings.Index(text, "func (s *Service) Sweep(")
	end := strings.Index(text, "func (s *Service) inAnyQueue(")
	if start < 0 || end < start {
		t.Fatal("the sweep's functions were not found")
	}
	sweep := text[start:end]

	// Act
	byHand := strings.Count(sweep, "log.Error(op") + strings.Count(sweep, "log.Warn(op")
	shared := strings.Count(sweep, "sweepFailed(")

	// Assert
	if byHand != 0 || shared == 0 {
		t.Fatalf("the sweep records %d failures by hand and %d through sweepFailed", byHand, shared)
	}
}

func TestRunSweepsAtOnceAndOnEveryTick(t *testing.T) {
	// Arrange
	f := newFixture(t)
	requester := register(t, f.db, "sample", nil)
	register(t, f.db, "sample-merge", &wsm.MergeOrigin{Requester: requester.ID, Request: "request-1"})
	f.now = time.Now().Add(IdleLimit + time.Hour)
	tick := make(chan time.Time)
	ctx, cancel := context.WithCancel(context.Background())
	done := make(chan struct{})

	// Act: the first sweep removes the workspace; a tick sweeps again.
	go func() { f.svc.Run(ctx, tick); close(done) }()
	tick <- time.Now()
	cancel()
	<-done

	// Assert: removed once, and the second sweep found nothing to do.
	if len(f.workspaces.nuked) != 1 {
		t.Fatalf("nuked %v, want the idle workspace removed once", f.workspaces.nuked)
	}
}

func TestHandbackKeyIsTheSharedDaemonKeyShape(t *testing.T) {
	// Act
	key, err := handbackKey()

	// Assert
	if err != nil {
		t.Fatalf("handbackKey: %v", err)
	}
	if !regexp.MustCompile(`^merge-handback-[0-9a-f]{32}$`).MatchString(key) {
		t.Fatalf("key = %q, want prompthandler.MintKey's shape under the merge-handback prefix", key)
	}
}

func TestSweepStoppedByItsContextIsNoFault(t *testing.T) {
	// Arrange: the daemon stopping mid-sweep.
	f := newFixture(t)
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act
	removed := f.svc.Sweep(ctx)

	// Assert
	if removed != 0 || len(f.workspaces.nuked) != 0 {
		t.Fatalf("removed %d, nuked %v; want nothing swept", removed, f.workspaces.nuked)
	}
	if f.loggedAt("error") || !f.loggedAt("info") {
		t.Fatal("a sweep its context ended was not recorded at INFO alone")
	}
}

func TestSweepThatCannotReadTheRegistryIsAnError(t *testing.T) {
	// Arrange: a state database that is gone.
	f := newFixture(t)
	f.db.Close()

	// Act
	removed := f.svc.Sweep(context.Background())

	// Assert
	if removed != 0 || len(f.workspaces.nuked) != 0 {
		t.Fatalf("removed %d, nuked %v; want nothing swept", removed, f.workspaces.nuked)
	}
	var cause any
	for _, r := range f.logs.Records() {
		if r.Level == "error" && r.Operation == op {
			cause = r.Context["cause"]
		}
	}
	if cause == nil {
		t.Fatal("an unreadable registry was not recorded at ERROR with its cause")
	}
}
