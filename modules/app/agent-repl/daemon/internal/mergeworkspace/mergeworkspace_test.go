package mergeworkspace

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"sync"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/flock"
	"agent-repld/internal/ids"
	"agent-repld/internal/merge"
	"agent-repld/internal/prompthandler"
	"agent-repld/internal/sourcescan"
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
	// revealErr fails every reveal; revealed is each reveal's answer.
	revealErr error
	revealed  bool
	reveals   []ids.WorkspaceID
	// retireKept and retireErr are RetireLanded's answer; onRetire, when set,
	// runs as it is called. retired records every requester it was asked for.
	retireKept string
	retireErr  error
	onRetire   func()
	retired    []ids.WorkspaceID
	// startErr fails every merge workspace start; started records each one.
	startErr error
	started  []ids.WorkspaceID
}

func (f *fakeWorkspaces) StartMergeWorkspace(_ context.Context, ws ids.WorkspaceID) error {
	if f.startErr != nil {
		return f.startErr
	}
	f.started = append(f.started, ws)
	return nil
}

func (f *fakeWorkspaces) RetireLanded(_ context.Context, ws ids.WorkspaceID) (string, error) {
	if f.onRetire != nil {
		f.onRetire()
	}
	f.retired = append(f.retired, ws)
	return f.retireKept, f.retireErr
}

func (f *fakeWorkspaces) CreateMergeWorkspace(ctx context.Context, requester wsm.Workspace, origin wsm.MergeOrigin, name, base string) (wsm.Workspace, error) {
	f.created = append(f.created, string(requester.ID)+":"+name+":"+base)
	if f.createErr != nil {
		return wsm.Workspace{}, f.createErr
	}
	return register(f.t, f.db, "sample-merge", &origin), nil
}

func (f *fakeWorkspaces) RevealMergeWorkspace(_ context.Context, ws ids.WorkspaceID) (bool, error) {
	if f.revealErr != nil {
		return false, f.revealErr
	}
	f.reveals = append(f.reveals, ws)
	return f.revealed, nil
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

// fakePrompts records every submission. err fails every submission;
// errOnText fails only the submission whose text is exactly it.
type fakePrompts struct {
	mu        sync.Mutex
	err       error
	errOnText string
	sent      []submitted
	// keys are the idempotency keys of the submissions in sent.
	keys []string
}

func (f *fakePrompts) Submit(_ context.Context, ws ids.WorkspaceID, said *conversationv1.UserSaid, key string, origin conversationv1.PromptOrigin, _ *ids.WorkspaceID, delivery wsm.Delivery, _ *feedid.Ref) (prompthandler.Outcome, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	text := said.GetContent().GetBlocks()[0].GetText().GetText()
	if f.err != nil {
		return prompthandler.Outcome{}, f.err
	}
	if f.errOnText != "" && text == f.errOnText {
		return prompthandler.Outcome{}, errors.New("refused " + text)
	}
	f.sent = append(f.sent, submitted{ws: ws, text: text, origin: origin, delivery: delivery})
	f.keys = append(f.keys, key)
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
	// keyErrOn fails MintKey for the one key prefix it names.
	keyErrOn string
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
		MintKey: func(prefix string) (string, error) {
			if prefix == f.keyErrOn {
				return "", errors.New("no randomness")
			}
			return prompthandler.MintKey(prefix)
		},
		Now:          func() time.Time { return f.now },
		WorktreeLock: filepath.Join(t.TempDir(), "worktree-reap.lock"),
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
	ws, err := f.svc.Fork(context.Background(), requester, wsm.MergeOrigin{Requester: requester.ID, Request: "request-1"}, "reconnect-fix", "sample")

	// Assert
	if err != nil || ws.MergeOf == nil || ws.MergeOf.Request != "request-1" {
		t.Fatalf("Fork = %+v, %v; want the merge workspace forked for the request", ws, err)
	}
	if len(f.workspaces.created) != 1 || f.workspaces.created[0] != string(requester.ID)+":reconnect-fix:sample" {
		t.Fatalf("created = %v, want one named reconnect-fix on the requester's branch", f.workspaces.created)
	}
}

func TestForkRecordsTheCompactAfterAskOnTheMergeWorkspace(t *testing.T) {
	// Arrange
	f := newFixture(t)
	requester := register(t, f.db, "sample", nil)
	origin := wsm.MergeOrigin{Requester: requester.ID, Request: "request-1", CompactAfter: true}

	// Act
	ws, err := f.svc.Fork(context.Background(), requester, origin, "reconnect-fix", "sample")

	// Assert
	if err != nil || ws.MergeOf == nil || !ws.MergeOf.CompactAfter {
		t.Fatalf("Fork = %+v, %v; want the merge workspace to carry the compact-after ask", ws.MergeOf, err)
	}
}

func TestForkRefusesAnOriginNamingAnotherRequester(t *testing.T) {
	// Arrange
	f := newFixture(t)
	requester := register(t, f.db, "sample", nil)
	origin := wsm.MergeOrigin{Requester: "ws-someone-else", Request: "request-1"}

	// Act
	_, err := f.svc.Fork(context.Background(), requester, origin, "reconnect-fix", "sample")

	// Assert
	if err == nil || len(f.workspaces.created) != 0 {
		t.Fatalf("Fork = %v with %v created; want a refusal and nothing created", err, f.workspaces.created)
	}
	if !f.loggedAt(dlog.LevelError) {
		t.Fatal("the contradictory fork was not recorded at ERROR")
	}
}

func TestForkFindsTheWorkspaceARestartLeftForked(t *testing.T) {
	// Arrange
	f := newFixture(t)
	requester := register(t, f.db, "sample", nil)
	forked := register(t, f.db, "sample-merge", &wsm.MergeOrigin{Requester: requester.ID, Request: "request-1"})

	// Act
	ws, err := f.svc.Fork(context.Background(), requester, wsm.MergeOrigin{Requester: requester.ID, Request: "request-1"}, "reconnect-fix", "sample")

	// Assert
	if err != nil || ws.ID != forked.ID || len(f.workspaces.created) != 0 {
		t.Fatalf("Fork = %v, %v with %v created; want the existing workspace and no new one", ws.ID, err, f.workspaces.created)
	}
}

// TestForkStartsTheSessionOfTheWorkspaceARestartLeftForked: the restart may
// have cut the create after registration and before its session came up, and
// the boot brings no hidden workspace up, so the pick that follows the
// hand-off would otherwise be submitted to no session.
func TestForkStartsTheSessionOfTheWorkspaceARestartLeftForked(t *testing.T) {
	// Arrange
	f := newFixture(t)
	requester := register(t, f.db, "sample", nil)
	forked := register(t, f.db, "sample-merge", &wsm.MergeOrigin{Requester: requester.ID, Request: "request-1"})

	// Act
	_, err := f.svc.Fork(context.Background(), requester, wsm.MergeOrigin{Requester: requester.ID, Request: "request-1"}, "reconnect-fix", "sample")

	// Assert
	if err != nil || len(f.workspaces.started) != 1 || f.workspaces.started[0] != forked.ID {
		t.Fatalf("Fork = %v with %v started; want the found workspace's session started", err, f.workspaces.started)
	}
}

func TestForkAnswersTheFoundWorkspacesStartFailure(t *testing.T) {
	// Arrange
	f := newFixture(t)
	requester := register(t, f.db, "sample", nil)
	register(t, f.db, "sample-merge", &wsm.MergeOrigin{Requester: requester.ID, Request: "request-1"})
	f.workspaces.startErr = errors.New("the shim would not spawn")

	// Act
	_, err := f.svc.Fork(context.Background(), requester, wsm.MergeOrigin{Requester: requester.ID, Request: "request-1"}, "reconnect-fix", "sample")

	// Assert
	if !errors.Is(err, f.workspaces.startErr) {
		t.Fatalf("Fork = %v, want the start's failure", err)
	}
}

func TestForkStartsNoSessionOfAWorkspaceItCreates(t *testing.T) {
	// Arrange: the create starts the session itself.
	f := newFixture(t)
	requester := register(t, f.db, "sample", nil)

	// Act
	_, err := f.svc.Fork(context.Background(), requester, wsm.MergeOrigin{Requester: requester.ID, Request: "request-1"}, "reconnect-fix", "sample")

	// Assert
	if err != nil || len(f.workspaces.started) != 0 {
		t.Fatalf("Fork = %v with %v started; want the create alone to start it", err, f.workspaces.started)
	}
}

func TestForkAnswersTheCreatesFailure(t *testing.T) {
	// Arrange
	f := newFixture(t)
	requester := register(t, f.db, "sample", nil)
	f.workspaces.createErr = errors.New("no worktree")

	// Act
	_, err := f.svc.Fork(context.Background(), requester, wsm.MergeOrigin{Requester: requester.ID, Request: "request-1"}, "reconnect-fix", "sample")

	// Assert
	if !errors.Is(err, f.workspaces.createErr) {
		t.Fatalf("Fork = %v, want the create's failure", err)
	}
}

func TestRevealRevealsThroughTheWorkspaceVerb(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspaces.revealed = true

	// Act
	revealed, err := f.svc.Reveal(context.Background(), "ws-merge")

	// Assert
	if err != nil || !revealed || len(f.workspaces.reveals) != 1 || f.workspaces.reveals[0] != "ws-merge" {
		t.Fatalf("Reveal = %v, %v, reveals %v; want the verb's answer", revealed, err, f.workspaces.reveals)
	}
}

func TestRevealAnswersTheVerbsFailure(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspaces.revealErr = errors.New("the registry could not be written")

	// Act
	_, err := f.svc.Reveal(context.Background(), "ws-merge")

	// Assert
	if !errors.Is(err, f.workspaces.revealErr) {
		t.Fatalf("Reveal = %v, want the verb's failure", err)
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
			wantLabel: "merge of sample landed", wantText: "Recently Merged"},
		{name: "failed", outcome: merge.DetachedOutcome{Kind: merge.DetachedFailed, Summary: "tests failed", Branch: "sample"},
			wantLabel: "merge of sample failed", wantText: "now shown in the tab bar and the sidebar, at /w/sample-merge"},
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

func TestHandBackOfAnInPlaceMergeReportsItToTheWorkspaceItRanIn(t *testing.T) {
	ws := wsm.Workspace{ID: "ws-requester", Name: "sample", Dir: "/w/sample"}
	tests := []struct {
		name      string
		outcome   merge.DetachedOutcome
		wantLabel string
		wantText  string
	}{
		{name: "failed", outcome: merge.DetachedOutcome{Kind: merge.DetachedFailed, Summary: "tests failed", Branch: "sample", InPlace: true},
			wantLabel: "one-shot merge of sample failed", wantText: "It ran in place, in this workspace. It failed: tests failed. This workspace stays open"},
		{name: "abandoned", outcome: merge.DetachedOutcome{Kind: merge.DetachedAbandoned, Summary: "the queue closed", Branch: "sample", InPlace: true},
			wantLabel: "one-shot merge of sample did not run", wantText: "It did not run: the queue closed. This workspace stays open."},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)

			// Act
			err := f.svc.HandBack(context.Background(), "ws-requester", ws, tt.outcome)

			// Assert
			if err != nil || len(f.prompts.sent) != 1 {
				t.Fatalf("HandBack = %v with %d submissions, want one", err, len(f.prompts.sent))
			}
			sent := f.prompts.sent[0]
			if sent.ws != "ws-requester" || sent.origin != conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_HANDBACK {
				t.Fatalf("submission = %+v, want a merge handback to the workspace", sent)
			}
			if first, _, _ := strings.Cut(sent.text, "\n"); first != tt.wantLabel || !strings.Contains(sent.text, tt.wantText) {
				t.Fatalf("text = %q, want it led by %q and naming %q", sent.text, tt.wantLabel, tt.wantText)
			}
			if strings.Contains(sent.text, "merge workspace") {
				t.Fatalf("text = %q, want no merge workspace named for an in-place merge", sent.text)
			}
		})
	}
}

func TestHandBackOfAnInPlaceLandingIsRefusedAsUndeclared(t *testing.T) {
	// Arrange
	f := newFixture(t)

	// Act
	err := f.svc.HandBack(context.Background(), "ws-requester", wsm.Workspace{ID: "ws-requester"},
		merge.DetachedOutcome{Kind: merge.DetachedLanded, InPlace: true, Branch: "sample"})

	// Assert: a landed one-shot closed its workspace, so handing it back is a
	// defect in the caller, refused with nothing submitted.
	if err == nil || len(f.prompts.sent) != 0 {
		t.Fatalf("HandBack = %v with %d submissions, want refused with none", err, len(f.prompts.sent))
	}
}

// compactingMergeWS is a merge workspace whose requester asked to be
// compacted once it lands.
var compactingMergeWS = wsm.Workspace{ID: "ws-merge", Name: "sample-merge", Dir: "/w/sample-merge",
	MergeOf: &wsm.MergeOrigin{Requester: "ws-requester", Request: "request-1", CompactAfter: true}}

func TestHandBackOfALandingThatAskedToCompactSubmitsTheCompactionAfterTheHandback(t *testing.T) {
	// Arrange
	f := newFixture(t)
	out := merge.DetachedOutcome{Kind: merge.DetachedLanded, Summary: "it landed as commit abc", Branch: "sample"}

	// Act
	err := f.svc.HandBack(context.Background(), "ws-requester", compactingMergeWS, out)

	// Assert: the handback first, then the requester's /compact, both down the
	// one prompt path, the compaction under its own origin.
	if err != nil || len(f.prompts.sent) != 2 {
		t.Fatalf("HandBack = %v with %d submissions, want the handback and the compaction", err, len(f.prompts.sent))
	}
	if f.prompts.sent[0].origin != conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_HANDBACK {
		t.Fatalf("first submission = %+v, want the handback", f.prompts.sent[0])
	}
	want := submitted{ws: "ws-requester", text: CompactCommand, origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_COMPACT_AFTER, delivery: wsm.DeliveryDeferred}
	if f.prompts.sent[1] != want {
		t.Fatalf("second submission = %+v, want %+v", f.prompts.sent[1], want)
	}
	if _, ok := f.recordWith(dlog.LevelInfo, "submitted the compaction a landed detached merge asked for to its requester"); !ok {
		t.Fatal("the compaction's submission was not recorded at INFO")
	}
}

func TestHandBackCompactsNothingUnlessALandingAskedForIt(t *testing.T) {
	noAsk := wsm.Workspace{ID: "ws-merge", Name: "sample-merge", Dir: "/w/sample-merge",
		MergeOf: &wsm.MergeOrigin{Requester: "ws-requester", Request: "request-1"}}
	tests := []struct {
		name    string
		mergeWS wsm.Workspace
		kind    merge.DetachedOutcomeKind
	}{
		{name: "landed without the ask", mergeWS: noAsk, kind: merge.DetachedLanded},
		{name: "failed with the ask", mergeWS: compactingMergeWS, kind: merge.DetachedFailed},
		{name: "abandoned with the ask", mergeWS: compactingMergeWS, kind: merge.DetachedAbandoned},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)

			// Act
			err := f.svc.HandBack(context.Background(), "ws-requester", tt.mergeWS, merge.DetachedOutcome{Kind: tt.kind, Summary: "s", Branch: "sample"})

			// Assert
			if err != nil || len(f.prompts.sent) != 1 || f.prompts.sent[0].origin != conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_HANDBACK {
				t.Fatalf("HandBack = %v with %+v submitted, want the handback alone", err, f.prompts.sent)
			}
		})
	}
}

func TestHandBackOfALandingWhoseCompactionFailsIsAnErrorAfterTheHandback(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(f *fixture)
		message string
	}{
		{name: "the key", arrange: func(f *fixture) { f.keyErrOn = "merge-compact-after" },
			message: "could not mint the idempotency key of a landed merge's compaction; its requester is not compacted"},
		{name: "the submission", arrange: func(f *fixture) { f.prompts.errOnText = CompactCommand },
			message: "could not submit the compaction a landed merge asked for; its requester is not compacted"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			tt.arrange(f)
			out := merge.DetachedOutcome{Kind: merge.DetachedLanded, Summary: "it landed as commit abc", Branch: "sample"}

			// Act
			err := f.svc.HandBack(context.Background(), "ws-requester", compactingMergeWS, out)

			// Assert: the handback stands, the compaction does not, and the
			// failure is recorded at ERROR with whose compaction it was.
			if err == nil || len(f.prompts.sent) != 1 || f.prompts.sent[0].origin != conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_HANDBACK {
				t.Fatalf("HandBack = %v with %+v submitted, want an error after the handback alone", err, f.prompts.sent)
			}
			rec, ok := f.recordWith(dlog.LevelError, tt.message)
			if !ok || rec.Context["requester"] != "ws-requester" || rec.Context["merge_workspace"] != "ws-merge" || rec.Context["cause"] == nil {
				t.Fatalf("ERROR record = %+v, %v; want one naming the requester, the merge workspace and the cause", rec, ok)
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
		{name: "the key", arrange: func(f *fixture) { f.keyErrOn = "merge-handback" },
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
	removed := f.svc.Sweep(context.Background()).Removed

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
			removed := f.svc.Sweep(context.Background()).Removed

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
	removed := f.svc.Sweep(context.Background()).Removed

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
	removed := f.svc.Sweep(context.Background()).Removed

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
	removed := f.svc.Sweep(ctx).Removed

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
	removed := f.svc.Sweep(ctx).Removed

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

func TestEverySubmissionCarriesAKeyOfTheSharedDaemonShape(t *testing.T) {
	// Arrange
	f := newFixture(t)
	out := merge.DetachedOutcome{Kind: merge.DetachedLanded, Summary: "it landed as commit abc", Branch: "sample"}

	// Act
	err := f.svc.HandBack(context.Background(), "ws-requester", compactingMergeWS, out)

	// Assert: the handback and the compaction each carry their own key, of
	// prompthandler.MintKey's shape under their own prefix.
	if err != nil || len(f.prompts.keys) != 2 {
		t.Fatalf("HandBack = %v with keys %v, want two", err, f.prompts.keys)
	}
	for i, prefix := range []string{"merge-handback", "merge-compact-after"} {
		if !regexp.MustCompile(`^` + prefix + `-[0-9a-f]{32}$`).MatchString(f.prompts.keys[i]) {
			t.Fatalf("key %d = %q, want prompthandler.MintKey's shape under the %s prefix", i, f.prompts.keys[i], prefix)
		}
	}
}

func TestSweepStoppedByItsContextIsNoFault(t *testing.T) {
	// Arrange: the daemon stopping mid-sweep.
	f := newFixture(t)
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act
	removed := f.svc.Sweep(ctx).Removed

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
	removed := f.svc.Sweep(context.Background()).Removed

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

// landedRequester registers a requester and stamps it landed at LANDEDAT.
func landedRequester(t *testing.T, f *fixture, landedAt time.Time) wsm.Workspace {
	t.Helper()
	ws := register(t, f.db, "sample", nil)
	if err := f.db.SetLanded(context.Background(), ws.ID, &wsm.Landing{Head: "fork-head", At: landedAt}); err != nil {
		t.Fatalf("SetLanded: %v", err)
	}
	return ws
}

// recordWith answers the first record of this package at LEVEL whose message
// is MESSAGE, false when none was written.
func (f *fixture) recordWith(level, message string) (dlog.Record, bool) {
	for _, r := range f.logs.Records() {
		if r.Level == level && r.Operation == op && r.Message == message {
			return r, true
		}
	}
	return dlog.Record{}, false
}

func TestHandBackOfALandedMergeStampsTheRequesterWithTheForkHead(t *testing.T) {
	// Arrange
	f := newFixture(t)
	requester := register(t, f.db, "sample", nil)
	mergeWS := register(t, f.db, "sample-merge", &wsm.MergeOrigin{Requester: requester.ID, Request: "request-1", ForkHead: "fork-head"})

	// Act
	err := f.svc.HandBack(context.Background(), requester.ID, mergeWS, merge.DetachedOutcome{Kind: merge.DetachedLanded, Branch: "sample"})

	// Assert
	if err != nil {
		t.Fatalf("HandBack: %v", err)
	}
	loaded, err := f.db.Workspace(context.Background(), requester.ID)
	if err != nil || loaded.Landed == nil || loaded.Landed.Head != "fork-head" || !loaded.Landed.At.Equal(f.now) {
		t.Fatalf("Landed = %+v, %v; want the fork head at the handback's instant", loaded.Landed, err)
	}
	if r, ok := f.recordWith(dlog.LevelInfo, "stamped the requester with the commit its detached merge landed"); !ok || r.Context["requester"] != string(requester.ID) || r.Context["landed_head"] != "fork-head" {
		t.Fatalf("no INFO stamp record naming the requester and head: %+v", f.logs.Records())
	}
}

func TestHandBackStampsNoLandingWhenTheMergeDidNotLand(t *testing.T) {
	tests := []struct {
		name string
		kind merge.DetachedOutcomeKind
	}{
		{name: "failed", kind: merge.DetachedFailed},
		{name: "abandoned", kind: merge.DetachedAbandoned},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			requester := register(t, f.db, "sample", nil)
			mergeWS := register(t, f.db, "sample-merge", &wsm.MergeOrigin{Requester: requester.ID, Request: "request-1", ForkHead: "fork-head"})

			// Act
			if err := f.svc.HandBack(context.Background(), requester.ID, mergeWS, merge.DetachedOutcome{Kind: tt.kind, Branch: "sample"}); err != nil {
				t.Fatalf("HandBack: %v", err)
			}

			// Assert
			loaded, err := f.db.Workspace(context.Background(), requester.ID)
			if err != nil || loaded.Landed != nil {
				t.Fatalf("Landed = %+v, %v; want none", loaded.Landed, err)
			}
		})
	}
}

func TestHandBackOfAMergeWorkspaceWithNoForkHeadStampsNothing(t *testing.T) {
	// Arrange: forked before fork heads were recorded.
	f := newFixture(t)
	requester := register(t, f.db, "sample", nil)
	mergeWS := register(t, f.db, "sample-merge", &wsm.MergeOrigin{Requester: requester.ID, Request: "request-1"})

	// Act
	err := f.svc.HandBack(context.Background(), requester.ID, mergeWS, merge.DetachedOutcome{Kind: merge.DetachedLanded, Branch: "sample"})

	// Assert
	loaded, readErr := f.db.Workspace(context.Background(), requester.ID)
	if err != nil || readErr != nil || loaded.Landed != nil || len(f.prompts.sent) != 1 {
		t.Fatalf("HandBack = %v, Landed = %+v, %v with %d submissions; want the handback alone", err, loaded.Landed, readErr, len(f.prompts.sent))
	}
	if _, ok := f.recordWith(dlog.LevelInfo, "the merge workspace was forked before fork heads were recorded; its requester is not stamped landed and is never retired"); !ok {
		t.Fatalf("no INFO record saying why nothing was stamped: %+v", f.logs.Records())
	}
}

func TestHandBackWhoseStampFailsRecordsItAndStillHandsBack(t *testing.T) {
	// Arrange: a requester the registry does not hold.
	f := newFixture(t)
	mergeWS := wsm.Workspace{ID: "ws-merge", Name: "sample-merge", MergeOf: &wsm.MergeOrigin{Requester: "ws-gone", Request: "request-1", ForkHead: "fork-head"}}

	// Act
	err := f.svc.HandBack(context.Background(), "ws-gone", mergeWS, merge.DetachedOutcome{Kind: merge.DetachedLanded, Branch: "sample"})

	// Assert
	if err != nil || len(f.prompts.sent) != 1 {
		t.Fatalf("HandBack = %v with %d submissions, want the handback made", err, len(f.prompts.sent))
	}
	r, ok := f.recordWith(dlog.LevelError, "could not stamp the requester with its landed merge; it is never retired")
	if !ok || r.Context["requester"] != "ws-gone" || r.Context["landed_head"] != "fork-head" || r.Context["cause"] == nil {
		t.Fatalf("no ERROR stamp record naming the requester, head and cause: %+v", f.logs.Records())
	}
}

func TestSweepRetiresARequesterIdleForADaySinceItsLanding(t *testing.T) {
	// Arrange
	f := newFixture(t)
	landedAt := time.Now()
	requester := landedRequester(t, f, landedAt)
	f.now = landedAt.Add(LandedIdleLimit + time.Hour)

	// Act
	report := f.svc.Sweep(context.Background())

	// Assert
	if report.Retired != 1 || len(f.workspaces.retired) != 1 || f.workspaces.retired[0] != requester.ID {
		t.Fatalf("report %+v, retired %v; want the idle requester retired", report, f.workspaces.retired)
	}
	if r, ok := f.recordWith(dlog.LevelInfo, "retired a requester idle for a day since its detached merge landed"); !ok || r.Context["requester"] != string(requester.ID) {
		t.Fatalf("no INFO retirement record naming the requester: %+v", f.logs.Records())
	}
}

func TestSweepLeavesWhatIsNotAnIdleLandedRequester(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, f *fixture)
	}{
		{name: "no landing", arrange: func(t *testing.T, f *fixture) {
			register(t, f.db, "plain", nil)
			f.now = time.Now().Add(LandedIdleLimit + time.Hour)
		}},
		{name: "a landing within the day", arrange: func(t *testing.T, f *fixture) {
			landedRequester(t, f, time.Now())
			f.now = time.Now().Add(LandedIdleLimit - time.Hour)
		}},
		{name: "a selection within the day", arrange: func(t *testing.T, f *fixture) {
			ws := landedRequester(t, f, time.Now())
			f.now = time.Now().Add(LandedIdleLimit + time.Hour)
			if err := f.db.SetCurrent(context.Background(), ws.ID, f.now.Add(-time.Hour)); err != nil {
				t.Fatalf("SetCurrent: %v", err)
			}
		}},
		{name: "already merged", arrange: func(t *testing.T, f *fixture) {
			ws := landedRequester(t, f, time.Now())
			if err := f.db.SetMergedAt(context.Background(), ws.ID, time.Now(), nil); err != nil {
				t.Fatalf("SetMergedAt: %v", err)
			}
			f.now = time.Now().Add(LandedIdleLimit + time.Hour)
		}},
		{name: "a queued merge", arrange: func(t *testing.T, f *fixture) {
			ws := landedRequester(t, f, time.Now())
			if err := f.db.RequestMerge(context.Background(), wsm.RepoKey(t.TempDir()), ws.ID, wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch}, time.Now()); err != nil {
				t.Fatalf("RequestMerge: %v", err)
			}
			f.now = time.Now().Add(LandedIdleLimit + time.Hour)
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			tt.arrange(t, f)

			// Act
			report := f.svc.Sweep(context.Background())

			// Assert
			if report.Retired != 0 || len(f.workspaces.retired) != 0 {
				t.Fatalf("report %+v, retired %v; want nothing retired", report, f.workspaces.retired)
			}
		})
	}
}

func TestSweepCountsNoRetirementForARequesterTheVerbKept(t *testing.T) {
	// Arrange
	f := newFixture(t)
	landedRequester(t, f, time.Now())
	f.now = time.Now().Add(LandedIdleLimit + time.Hour)
	f.workspaces.retireKept = "dirty"

	// Act
	report := f.svc.Sweep(context.Background())

	// Assert
	if report.Retired != 0 || len(f.workspaces.retired) != 1 {
		t.Fatalf("report %+v, asked %v; want one ask and no retirement", report, f.workspaces.retired)
	}
	if r, ok := f.recordWith(dlog.LevelDebug, "kept an idle landed requester"); !ok || r.Context["kept"] != "dirty" {
		t.Fatalf("no DEBUG keep record with its reason: %+v", f.logs.Records())
	}
}

func TestSweepLeavesARequesterTheVerbFailedOnForTheNextSweep(t *testing.T) {
	// Arrange
	f := newFixture(t)
	landedRequester(t, f, time.Now())
	f.now = time.Now().Add(LandedIdleLimit + time.Hour)
	f.workspaces.retireErr = errors.New("fatal: contains modified or untracked files")

	// Act
	first := f.svc.Sweep(context.Background())
	f.workspaces.retireErr = nil
	second := f.svc.Sweep(context.Background())

	// Assert: the verb records its own failure, so the sweep adds no ERROR.
	if first.Retired != 0 || second.Retired != 1 || len(f.workspaces.retired) != 2 {
		t.Fatalf("reports %+v then %+v with asks %v; want a retry that retires", first, second, f.workspaces.retired)
	}
	if f.loggedAt(dlog.LevelError) {
		t.Fatalf("the sweep re-recorded the verb's failure at ERROR: %+v", f.logs.Records())
	}
}

func TestSweepRetiresNothingWhileTheReaperHoldsTheWorktreeLock(t *testing.T) {
	// Arrange
	f := newFixture(t)
	landedRequester(t, f, time.Now())
	f.now = time.Now().Add(LandedIdleLimit + time.Hour)
	held, ok, err := flock.TryExclusive(f.svc.deps.WorktreeLock)
	if err != nil || !ok {
		t.Fatalf("TryExclusive = %v, %v", ok, err)
	}
	defer held.Release()

	// Act
	report := f.svc.Sweep(context.Background())

	// Assert
	if report.Retired != 0 || len(f.workspaces.retired) != 0 {
		t.Fatalf("report %+v, retired %v; want nothing retired under the reaper's lock", report, f.workspaces.retired)
	}
	if _, ok := f.recordWith(dlog.LevelInfo, "the landed-worktree reaper holds the worktree lock; the idle landed requesters are retired by the next sweep"); !ok {
		t.Fatalf("no INFO record of the held lock: %+v", f.logs.Records())
	}
}

func TestSweepThatCannotTakeTheWorktreeLockIsAnError(t *testing.T) {
	// Arrange: the lock's directory is a file.
	f := newFixture(t)
	landedRequester(t, f, time.Now())
	f.now = time.Now().Add(LandedIdleLimit + time.Hour)
	file := filepath.Join(t.TempDir(), "not-a-dir")
	if err := os.WriteFile(file, nil, 0o644); err != nil {
		t.Fatalf("WriteFile: %v", err)
	}
	f.svc.deps.WorktreeLock = filepath.Join(file, "worktree-reap.lock")

	// Act
	report := f.svc.Sweep(context.Background())

	// Assert
	if report.Retired != 0 || len(f.workspaces.retired) != 0 {
		t.Fatalf("report %+v, retired %v; want nothing retired", report, f.workspaces.retired)
	}
	r, ok := f.recordWith(dlog.LevelError, "could not take the worktree lock; no idle landed requester is retired")
	if !ok || r.Context["lock"] != f.svc.deps.WorktreeLock || r.Context["cause"] == nil {
		t.Fatalf("no ERROR lock record with its path and cause: %+v", f.logs.Records())
	}
}

func TestSweepWhoseRetirementTheStandDownCutShortRecordsItAtInfo(t *testing.T) {
	// Arrange: the daemon stands down while the verb runs.
	f := newFixture(t)
	landedRequester(t, f, time.Now())
	f.now = time.Now().Add(LandedIdleLimit + time.Hour)
	ctx, cancel := context.WithCancel(context.Background())
	f.workspaces.onRetire = cancel
	f.workspaces.retireErr = context.Canceled

	// Act
	report := f.svc.Sweep(ctx)

	// Assert
	if report.Retired != 0 {
		t.Fatalf("report %+v, want nothing retired", report)
	}
	r, ok := f.recordWith(dlog.LevelInfo, "the daemon stood down mid-sweep")
	if !ok || r.Context["left_undone"] != "an idle landed requester was not retired; the next sweep tries again" || r.Context["requester"] == nil {
		t.Fatalf("the cut-short retirement was not recorded with its requester: %+v", f.logs.Records())
	}
}

// ---- a landing's report waits for its deploy ----

// failingDB fails the owed-report calls it names, and passes everything else
// to the real store.
type failingDB struct {
	wsm.DB
	oweErr, coverErr, owedErr, payErr error
}

func (d failingDB) OweHandBack(ctx context.Context, owed wsm.OwedHandBack) error {
	if d.oweErr != nil {
		return d.oweErr
	}
	return d.DB.OweHandBack(ctx, owed)
}

func (d failingDB) CoverOwedHandBacks(ctx context.Context) (int, error) {
	if d.coverErr != nil {
		return 0, d.coverErr
	}
	return d.DB.CoverOwedHandBacks(ctx)
}

func (d failingDB) OwedHandBacks(ctx context.Context, coveredOnly bool) ([]wsm.OwedHandBack, error) {
	if d.owedErr != nil {
		return nil, d.owedErr
	}
	return d.DB.OwedHandBacks(ctx, coveredOnly)
}

func (d failingDB) PayHandBack(ctx context.Context, mergeWS wsm.WorkspaceID) (bool, error) {
	if d.payErr != nil {
		return false, d.payErr
	}
	return d.DB.PayHandBack(ctx, mergeWS)
}

// withDB rebuilds the fixture's service over db.
func (f *fixture) withDB(db wsm.DB) {
	deps := f.svc.deps
	deps.DB = db
	f.svc = New(deps)
}

// owedLanding is a landing whose deploy was told.
var owedLanding = merge.DetachedOutcome{Kind: merge.DetachedLanded, Summary: "it landed as commit abc", Branch: "sample", Deploy: merge.LandingDeployOwed}

// owe hands back an owed landing from merge workspace mergeWS.
func (f *fixture) owe(t *testing.T, mergeWS wsm.Workspace) {
	t.Helper()
	if err := f.svc.HandBack(context.Background(), "ws-requester", mergeWS, owedLanding); err != nil {
		t.Fatalf("HandBack: %v", err)
	}
}

func TestHandBackOfALandingOwedItsDeployTellsNobodyYet(t *testing.T) {
	// Arrange
	f := newFixture(t)
	mergeWS := wsm.Workspace{ID: "ws-merge", Name: "sample-merge"}

	// Act
	err := f.svc.HandBack(context.Background(), "ws-requester", mergeWS, owedLanding)

	// Assert
	if err != nil || len(f.prompts.sent) != 0 {
		t.Fatalf("HandBack = %v with %d submission(s), want the report owed and nothing sent", err, len(f.prompts.sent))
	}
	owed, err := f.db.OwedHandBacks(context.Background(), false)
	if err != nil || len(owed) != 1 || owed[0].Requester != "ws-requester" || owed[0].MergeName != "sample-merge" || owed[0].Summary != owedLanding.Summary || owed[0].Covered {
		t.Fatalf("owed = %+v, %v; want the one uncovered report", owed, err)
	}
	if _, ok := f.recordWith(dlog.LevelInfo, "a landed merge's report is owed until the deploy that carries it has run"); !ok {
		t.Fatal("the owed report was not recorded at INFO")
	}
}

func TestHandBackOfALandingThatCannotBeOwedIsAnErrorWithNothingSent(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.withDB(failingDB{DB: f.db, oweErr: errors.New("disk full")})

	// Act
	err := f.svc.HandBack(context.Background(), "ws-requester", wsm.Workspace{ID: "ws-merge", Name: "sample-merge"}, owedLanding)

	// Assert
	if err == nil || len(f.prompts.sent) != 0 {
		t.Fatalf("HandBack = %v with %d submission(s), want an error and nothing sent", err, len(f.prompts.sent))
	}
	if r, ok := f.recordWith(dlog.LevelError, "could not record a landed merge's report as owed until its deploy; its requester is not told"); !ok || r.Context["cause"] != "disk full" {
		t.Fatalf("the failure was not recorded at ERROR with its cause: %+v", f.logs.Records())
	}
}

func TestALandingNothingDeploysIsReportedAtOnceSayingWhy(t *testing.T) {
	tests := []struct {
		name   string
		deploy merge.LandingDeploy
		want   string
	}{
		{name: "nothing to deploy", deploy: merge.LandingNotDeployed, want: "read the merge's own conversation."},
		{name: "rollouts held", deploy: merge.LandingRolloutsHeld, want: "The rollouts are held, so nothing deployed it: the running build does not carry it yet."},
		{name: "hold unread", deploy: merge.LandingHoldUnread, want: "The rollout hold could not be read, so its deploy was not started: the running build does not carry it yet."},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			out := owedLanding
			out.Deploy = tt.deploy

			// Act
			err := f.svc.HandBack(context.Background(), "ws-requester", wsm.Workspace{ID: "ws-merge", Name: "sample-merge"}, out)

			// Assert
			if err != nil || len(f.prompts.sent) != 1 || !strings.HasSuffix(f.prompts.sent[0].text, tt.want) {
				t.Fatalf("HandBack = %v, sent %+v; want one report naming %q", err, f.prompts.sent, tt.want)
			}
		})
	}
}

func TestCoverOwedCoversTheReportsOwedBeforeTheDeploy(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.owe(t, wsm.Workspace{ID: "ws-merge", Name: "sample-merge"})

	// Act
	err := f.svc.CoverOwed(context.Background())

	// Assert
	owed, readErr := f.db.OwedHandBacks(context.Background(), true)
	if err != nil || readErr != nil || len(owed) != 1 {
		t.Fatalf("CoverOwed = %v; covered %+v, %v; want the report covered", err, owed, readErr)
	}
	if r, ok := f.recordWith(dlog.LevelInfo, "the deploy covers the landing reports owed before it"); !ok || r.Context["covered"] != 1 {
		t.Fatalf("the cover was not recorded at INFO with its count: %+v", f.logs.Records())
	}
}

func TestCoverOwedThatFailsIsAnErrorRecordedWithItsCause(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.withDB(failingDB{DB: f.db, coverErr: errors.New("locked")})

	// Act
	err := f.svc.CoverOwed(context.Background())

	// Assert
	if err == nil {
		t.Fatal("CoverOwed = nil, want the failure")
	}
	if r, ok := f.recordWith(dlog.LevelError, "could not mark the owed landing reports covered by the deploy; they wait for the next one"); !ok || r.Context["cause"] != "locked" {
		t.Fatalf("the failure was not recorded at ERROR with its cause: %+v", f.logs.Records())
	}
}

func TestPayOwedHandsBackOnlyTheCoveredReportsAfterADeploy(t *testing.T) {
	// Arrange: one report covered by the deploy, one owed after it committed.
	f := newFixture(t)
	f.owe(t, wsm.Workspace{ID: "ws-merge-a", Name: "a-merge"})
	if err := f.svc.CoverOwed(context.Background()); err != nil {
		t.Fatalf("CoverOwed: %v", err)
	}
	f.owe(t, wsm.Workspace{ID: "ws-merge-b", Name: "b-merge"})

	// Act
	f.svc.PayOwed(context.Background(), true, nil)

	// Assert
	if len(f.prompts.sent) != 1 || !strings.Contains(f.prompts.sent[0].text, "`a-merge`") ||
		!strings.Contains(f.prompts.sent[0].text, "Its deploy has finished: this report waited until the build that carries it was running") {
		t.Fatalf("sent = %+v, want only the covered report, saying its deploy finished", f.prompts.sent)
	}
	left, err := f.db.OwedHandBacks(context.Background(), false)
	if err != nil || len(left) != 1 || left[0].MergeWorkspace != "ws-merge-b" {
		t.Fatalf("left owed = %+v, %v; want only the uncovered report", left, err)
	}
}

func TestPayOwedOfEveryReportHandsBackTheUncoveredToo(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.owe(t, wsm.Workspace{ID: "ws-merge", Name: "sample-merge"})

	// Act
	f.svc.PayOwed(context.Background(), false, nil)

	// Assert
	left, err := f.db.OwedHandBacks(context.Background(), false)
	if len(f.prompts.sent) != 1 || err != nil || len(left) != 0 {
		t.Fatalf("sent %d, left %+v, %v; want the report paid", len(f.prompts.sent), left, err)
	}
}

func TestPayOwedSubmitsUnderTheKeyMintedWhenTheReportWasOwed(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.owe(t, wsm.Workspace{ID: "ws-merge", Name: "sample-merge"})
	owed, err := f.db.OwedHandBacks(context.Background(), false)
	if err != nil || len(owed) != 1 {
		t.Fatalf("owed = %+v, %v", owed, err)
	}

	// Act
	f.svc.PayOwed(context.Background(), false, nil)

	// Assert
	if len(f.prompts.keys) != 1 || f.prompts.keys[0] != owed[0].IdempotencyKey {
		t.Fatalf("keys = %v, want the owed key %q", f.prompts.keys, owed[0].IdempotencyKey)
	}
}

func TestPayOwedAfterAFailedDeploySaysTheRunningBuildDoesNotCarryIt(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.owe(t, wsm.Workspace{ID: "ws-merge", Name: "sample-merge"})
	if err := f.svc.CoverOwed(context.Background()); err != nil {
		t.Fatalf("CoverOwed: %v", err)
	}

	// Act
	f.svc.PayOwed(context.Background(), true, errors.New("the build failed"))

	// Assert
	if len(f.prompts.sent) != 1 || !strings.Contains(f.prompts.sent[0].text, "Its deploy failed (the build failed): master holds it, but the running build does not carry it.") {
		t.Fatalf("sent = %+v, want the report naming the failed deploy", f.prompts.sent)
	}
}

func TestPayOwedOfALandingThatAskedToCompactSubmitsTheCompactionAfter(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.owe(t, compactingMergeWS)

	// Act
	f.svc.PayOwed(context.Background(), false, nil)

	// Assert
	if len(f.prompts.sent) != 2 || f.prompts.sent[0].origin != conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_HANDBACK ||
		f.prompts.sent[1].origin != conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_COMPACT_AFTER {
		t.Fatalf("sent = %+v, want the report and then the compaction", f.prompts.sent)
	}
}

func TestPayOwedWhoseSubmissionFailsLeavesThatReportOwedAndPaysTheRest(t *testing.T) {
	// Arrange: the first report's submission is refused, the second's taken.
	f := newFixture(t)
	f.owe(t, wsm.Workspace{ID: "ws-merge-a", Name: "a-merge"})
	f.now = f.now.Add(time.Second)
	f.owe(t, wsm.Workspace{ID: "ws-merge-b", Name: "b-merge"})
	label, report := landedText("sample", "a-merge", owedLanding.Summary, deployedNote(nil))
	f.prompts.errOnText = label + "\n\n" + report

	// Act
	f.svc.PayOwed(context.Background(), false, nil)

	// Assert
	left, err := f.db.OwedHandBacks(context.Background(), false)
	if err != nil || len(left) != 1 || left[0].MergeWorkspace != "ws-merge-a" || len(f.prompts.sent) != 1 {
		t.Fatalf("left owed %+v, %v; sent %+v; want the refused report still owed and the other paid", left, err, f.prompts.sent)
	}
	if _, ok := f.recordWith(dlog.LevelError, "could not submit a detached merge's handback to its requester"); !ok {
		t.Fatal("the refused submission was not recorded at ERROR")
	}
}

func TestPayOwedThatCannotReadTheReportsRecordsItAndSendsNothing(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.withDB(failingDB{DB: f.db, owedErr: errors.New("unreadable")})

	// Act
	f.svc.PayOwed(context.Background(), true, nil)

	// Assert
	if len(f.prompts.sent) != 0 {
		t.Fatalf("sent = %+v, want nothing", f.prompts.sent)
	}
	if r, ok := f.recordWith(dlog.LevelError, "could not read the owed landing reports; they stay owed"); !ok || r.Context["cause"] != "unreadable" {
		t.Fatalf("the failed read was not recorded at ERROR with its cause: %+v", f.logs.Records())
	}
}

func TestPayOwedThatCannotMarkAReportPaidRecordsIt(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.owe(t, wsm.Workspace{ID: "ws-merge", Name: "sample-merge"})
	f.withDB(failingDB{DB: f.db, payErr: errors.New("locked")})

	// Act
	f.svc.PayOwed(context.Background(), false, nil)

	// Assert: the report went, and the unpaid mark is said at ERROR.
	if len(f.prompts.sent) != 1 {
		t.Fatalf("sent = %+v, want the report", f.prompts.sent)
	}
	if r, ok := f.recordWith(dlog.LevelError, "handed a landed merge's report back but could not mark it paid; the next payment hands it back again under the same key"); !ok || r.Context["cause"] != "locked" {
		t.Fatalf("the unpaid mark was not recorded at ERROR with its cause: %+v", f.logs.Records())
	}
}

func TestPayOwedCutShortByAStandDownSaysSoAtInfo(t *testing.T) {
	// Arrange: the read fails because the daemon's lifetime ended.
	f := newFixture(t)
	f.withDB(failingDB{DB: f.db, owedErr: context.Canceled})
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act
	f.svc.PayOwed(ctx, true, nil)

	// Assert
	if r, ok := f.recordWith(dlog.LevelInfo, "the daemon stood down mid-payment"); !ok || r.Context["left_undone"] != "could not read the owed landing reports; they stay owed" {
		t.Fatalf("records = %+v, want the cut-short payment at INFO with what it left undone", f.logs.Records())
	}
	if _, ok := f.recordWith(dlog.LevelError, "could not read the owed landing reports; they stay owed"); ok {
		t.Fatal("a stand-down was recorded as a failed read at ERROR")
	}
}

func TestAHandBackCutShortByAStandDownSaysSoAtInfo(t *testing.T) {
	// Arrange: the submission fails because the daemon's lifetime ended.
	f := newFixture(t)
	f.prompts.err = context.Canceled
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act
	err := f.svc.HandBack(ctx, "ws-requester", wsm.Workspace{ID: "ws-merge", Name: "sample-merge"},
		merge.DetachedOutcome{Kind: merge.DetachedFailed, Summary: "tests failed", Branch: "sample"})

	// Assert
	if err == nil {
		t.Fatal("HandBack = nil, want the cut-short submission's error")
	}
	if _, ok := f.recordWith(dlog.LevelInfo, "the daemon stood down mid-handback"); !ok {
		t.Fatalf("records = %+v, want the cut-short handback at INFO", f.logs.Records())
	}
}

// TestTheCompactionIsSpelledOnlyBySessionCommand pins that the merge
// workspace service spells no /compact by hand: sessioncommand.Literal reads
// it off the schema.
func TestTheCompactionIsSpelledOnlyBySessionCommand(t *testing.T) {
	// Act
	n := sourcescan.Count(t, `"/compact"`)

	// Assert
	if n != 0 {
		t.Errorf("the service's sources spell /compact by hand %d times", n)
	}
}
