package postmerge

import (
	"context"
	"errors"
	"strings"
	"sync"
	"testing"

	"claude-repld/internal/workspace/merge"
)

// --- fakes --------------------------------------------------------------

// submission is one prompt the notifier handed the parent session.
type submission struct {
	workspace      string
	requestID      string
	text           string
	permissionMode string
}

// fakeParents stands in for postmerge.ParentSession. It records every liveness
// question and every submit in ORDER, which is what lets a test assert the
// phone-home precedes the postprocessing prompt without inspecting timing.
type fakeParents struct {
	mu sync.Mutex
	// live names the workspaces that have a live session. Anything absent is
	// not live.
	live map[string]bool
	// asked records each workspace Live was called for.
	asked []string
	// got records each submit, in order.
	got []submission
	// errs is consumed one per submit; a nil entry (or an exhausted list)
	// means the submit succeeded.
	errs []error
}

func newFakeParents(live ...string) *fakeParents {
	set := map[string]bool{}
	for _, ws := range live {
		set[ws] = true
	}
	return &fakeParents{live: set}
}

func (p *fakeParents) Live(workspace string) bool {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.asked = append(p.asked, workspace)
	return p.live[workspace]
}

func (p *fakeParents) SubmitPrompt(_ context.Context, workspace, requestID, text, permissionMode string) error {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.got = append(p.got, submission{workspace, requestID, text, permissionMode})
	if len(p.errs) == 0 {
		return nil
	}
	err := p.errs[0]
	p.errs = p.errs[1:]
	return err
}

func (p *fakeParents) submissions() []submission {
	p.mu.Lock()
	defer p.mu.Unlock()
	out := make([]submission, len(p.got))
	copy(out, p.got)
	return out
}

// fakeProbe stands in for postmerge.WorktreeProbe.
type fakeProbe struct {
	linked map[string]bool
	err    error
	// probed records each directory the notifier asked about.
	probed []string
}

func (p *fakeProbe) IsLinkedWorktree(_ context.Context, dir string) (bool, error) {
	p.probed = append(p.probed, dir)
	if p.err != nil {
		return false, p.err
	}
	return p.linked[dir], nil
}

// fakePostprocessing stands in for postmerge.PostprocessingSource.
type fakePostprocessing struct {
	prompts map[string]string
	err     error
	// asked records each worktree path the notifier looked up.
	asked []string
}

func (s *fakePostprocessing) PostprocessingPrompt(worktreePath string) (string, error) {
	s.asked = append(s.asked, worktreePath)
	if s.err != nil {
		return "", s.err
	}
	return s.prompts[worktreePath], nil
}

const (
	childWS   = "/ws/child"
	childName = "child-one"
	parentWS  = "/ws/parent"
	trunkDir  = "/repo"
)

func childRequest() merge.Request {
	return merge.Request{
		Workspace:    childWS,
		Name:         childName,
		SourceBranch: "DWC/child-one",
		SourceDir:    childWS,
		TargetDir:    parentWS,
	}
}

// notifierHarness bundles a notifier with the fakes behind it.
type notifierHarness struct {
	notifier       *Notifier
	parents        *fakeParents
	probe          *fakeProbe
	postprocessing *fakePostprocessing
	logs           []string
}

type harnessOpts struct {
	parents        *fakeParents
	probe          *fakeProbe
	postprocessing *fakePostprocessing
}

func newHarness(t *testing.T, opts harnessOpts) *notifierHarness {
	t.Helper()
	h := &notifierHarness{
		parents:        opts.parents,
		probe:          opts.probe,
		postprocessing: opts.postprocessing,
	}
	if h.parents == nil {
		h.parents = newFakeParents(parentWS)
	}
	if h.probe == nil {
		h.probe = &fakeProbe{linked: map[string]bool{parentWS: true}}
	}
	if h.postprocessing == nil {
		h.postprocessing = &fakePostprocessing{}
	}
	n, err := New(Config{
		Logf: func(format string, args ...any) {
			h.logs = append(h.logs, format)
			t.Logf(format, args...)
		},
		Parents:        h.parents,
		Worktrees:      h.probe,
		Postprocessing: h.postprocessing,
	})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	h.notifier = n
	return h
}

// logged reports whether any canonical log record contains want.
func (h *notifierHarness) logged(want string) bool {
	for _, line := range h.logs {
		if strings.Contains(line, want) {
			return true
		}
	}
	return false
}

// --- construction -------------------------------------------------------

func TestNewRequiresEveryDependency(t *testing.T) {
	complete := func() Config {
		return Config{
			Logf:           func(string, ...any) {},
			Parents:        newFakeParents(),
			Worktrees:      &fakeProbe{},
			Postprocessing: &fakePostprocessing{},
		}
	}
	tests := []struct {
		name    string
		mutate  func(*Config)
		wantErr bool
	}{
		{name: "complete", mutate: func(*Config) {}, wantErr: false},
		{name: "no logger", mutate: func(c *Config) { c.Logf = nil }, wantErr: true},
		{name: "no parents", mutate: func(c *Config) { c.Parents = nil }, wantErr: true},
		{name: "no worktree probe", mutate: func(c *Config) { c.Worktrees = nil }, wantErr: true},
		{name: "no postprocessing source", mutate: func(c *Config) { c.Postprocessing = nil }, wantErr: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			cfg := complete()
			tc.mutate(&cfg)

			// Act.
			n, err := New(cfg)

			// Assert.
			if tc.wantErr {
				if err == nil {
					t.Fatalf("New() error = nil, want error")
				}
				return
			}
			if err != nil || n == nil {
				t.Fatalf("New() = %v, %v, want a notifier", n, err)
			}
		})
	}
}

// --- the phone-home -----------------------------------------------------

func TestMainWorktreeTargetSendsNoPhoneHome(t *testing.T) {
	// Arrange — the child merged into the repository's main checkout.
	req := childRequest()
	req.TargetDir = trunkDir
	h := newHarness(t, harnessOpts{probe: &fakeProbe{linked: map[string]bool{}}})

	// Act.
	err := h.notifier.AfterMerged(context.Background(), req)

	// Assert — nothing prompted, and the reason is in the canonical log.
	if err != nil {
		t.Fatalf("AfterMerged() error = %v", err)
	}
	if got := h.parents.submissions(); len(got) != 0 {
		t.Fatalf("submissions = %v, want none for a main-worktree target", got)
	}
	if !h.logged("NO PARENT to notify") {
		t.Fatalf("logs = %v, want the main-worktree skip recorded", h.logs)
	}
}

func TestMainWorktreeTargetNeverLooksUpAPostprocessingPrompt(t *testing.T) {
	// Arrange — the trunk is nobody's session, so the handoff stops before it
	// would reach any creation record at all.
	req := childRequest()
	req.TargetDir = trunkDir
	h := newHarness(t, harnessOpts{probe: &fakeProbe{linked: map[string]bool{}}})

	// Act.
	if err := h.notifier.AfterMerged(context.Background(), req); err != nil {
		t.Fatalf("AfterMerged() error = %v", err)
	}

	// Assert.
	if got := h.postprocessing.asked; len(got) != 0 {
		t.Fatalf("postprocessing lookups = %v, want none", got)
	}
}

func TestLinkedWorktreeTargetPhonesTheParentHome(t *testing.T) {
	// Arrange — a live parent workspace holding the target worktree.
	h := newHarness(t, harnessOpts{})

	// Act.
	err := h.notifier.AfterMerged(context.Background(), childRequest())

	// Assert — exactly one prompt, on the parent, naming the merged child.
	if err != nil {
		t.Fatalf("AfterMerged() error = %v", err)
	}
	got := h.parents.submissions()
	if len(got) != 1 {
		t.Fatalf("submissions = %v, want exactly the phone-home", got)
	}
	if got[0].workspace != parentWS {
		t.Fatalf("phone-home workspace = %q, want the parent %q", got[0].workspace, parentWS)
	}
	if !strings.Contains(got[0].text, childName) {
		t.Fatalf("phone-home text = %q, want it to name the merged child %q", got[0].text, childName)
	}
}

func TestPhoneHomeCarriesItsOwnRequestID(t *testing.T) {
	// Arrange.
	h := newHarness(t, harnessOpts{})

	// Act.
	if err := h.notifier.AfterMerged(context.Background(), childRequest()); err != nil {
		t.Fatalf("AfterMerged() error = %v", err)
	}

	// Assert — a minted, post-merge-scoped id, never an empty one.
	got := h.parents.submissions()
	if len(got) != 1 {
		t.Fatalf("submissions = %v, want one", got)
	}
	if !strings.HasPrefix(got[0].requestID, "postmerge_phone_home_") {
		t.Fatalf("phone-home request id = %q, want a minted postmerge id", got[0].requestID)
	}
}

func TestPhoneHomeInheritsTheParentsPermissionMode(t *testing.T) {
	// Arrange — the notifier must not choose a permission mode of its own.
	h := newHarness(t, harnessOpts{})

	// Act.
	if err := h.notifier.AfterMerged(context.Background(), childRequest()); err != nil {
		t.Fatalf("AfterMerged() error = %v", err)
	}

	// Assert.
	got := h.parents.submissions()
	if len(got) != 1 || got[0].permissionMode != "" {
		t.Fatalf("phone-home permission mode = %q, want the session's own (empty)", got[0].permissionMode)
	}
}

func TestParentWithNoLiveSessionIsALoudSkip(t *testing.T) {
	// Arrange — the parent workspace exists but nobody has it open.
	h := newHarness(t, harnessOpts{parents: newFakeParents()})

	// Act.
	err := h.notifier.AfterMerged(context.Background(), childRequest())

	// Assert — no prompt, no error, and the skip is on the record.
	if err != nil {
		t.Fatalf("AfterMerged() error = %v, want the skip to be non-fatal", err)
	}
	if got := h.parents.submissions(); len(got) != 0 {
		t.Fatalf("submissions = %v, want none against a dead parent", got)
	}
	if !h.logged("NO LIVE SESSION") {
		t.Fatalf("logs = %v, want the skip recorded loudly", h.logs)
	}
}

func TestParentLivenessIsAskedAboutTheTargetWorktree(t *testing.T) {
	// Arrange — the parent workspace key IS the merge's target directory.
	h := newHarness(t, harnessOpts{})

	// Act.
	if err := h.notifier.AfterMerged(context.Background(), childRequest()); err != nil {
		t.Fatalf("AfterMerged() error = %v", err)
	}

	// Assert.
	if len(h.parents.asked) != 1 || h.parents.asked[0] != parentWS {
		t.Fatalf("liveness asked about %v, want exactly the target worktree %q", h.parents.asked, parentWS)
	}
}

func TestWorktreeProbeFailureIsSurfaced(t *testing.T) {
	// Arrange — git could not answer what kind of worktree the target is.
	boom := errors.New("git exploded")
	h := newHarness(t, harnessOpts{probe: &fakeProbe{err: boom}})

	// Act.
	err := h.notifier.AfterMerged(context.Background(), childRequest())

	// Assert — surfaced, not swallowed, and nothing prompted on a guess.
	if !errors.Is(err, boom) {
		t.Fatalf("AfterMerged() error = %v, want the probe failure surfaced", err)
	}
	if got := h.parents.submissions(); len(got) != 0 {
		t.Fatalf("submissions = %v, want none when the target is unknown", got)
	}
	if !h.logged("worktree probe FAILED") {
		t.Fatalf("logs = %v, want the probe failure recorded", h.logs)
	}
}

func TestPhoneHomeSubmitFailureIsSurfaced(t *testing.T) {
	// Arrange — the parent's session refused the prompt.
	boom := errors.New("session refused")
	parents := newFakeParents(parentWS)
	parents.errs = []error{boom}
	h := newHarness(t, harnessOpts{parents: parents})

	// Act.
	err := h.notifier.AfterMerged(context.Background(), childRequest())

	// Assert.
	if !errors.Is(err, boom) {
		t.Fatalf("AfterMerged() error = %v, want the submit failure surfaced", err)
	}
	if !h.logged("phone-home submit FAILED") {
		t.Fatalf("logs = %v, want the submit failure recorded", h.logs)
	}
}

func TestPhoneHomeFailureIsTheOnlySubmissionAttempted(t *testing.T) {
	// Arrange — a refused phone-home plus a postprocessing prompt on record.
	parents := newFakeParents(parentWS)
	parents.errs = []error{errors.New("session refused")}
	h := newHarness(t, harnessOpts{
		parents:        parents,
		postprocessing: &fakePostprocessing{prompts: map[string]string{childWS: "tidy up"}},
	})

	// Act.
	if err := h.notifier.AfterMerged(context.Background(), childRequest()); err == nil {
		t.Fatalf("AfterMerged() error = nil, want the refused phone-home surfaced")
	}

	// Assert — only the phone-home was attempted.
	if got := h.parents.submissions(); len(got) != 1 {
		t.Fatalf("submissions = %v, want only the failed phone-home", got)
	}
}

// --- the postprocessing prompt ------------------------------------------
//
// The prompt is RESOLVED here and DELIVERED elsewhere. It is the merge run's
// after-action -- a turn in the MERGED WORKSPACE'S OWN session, run under the
// merge lease while the run publishes `merge_after_action` -- so this notifier
// reports it (AfterAction) and the parent handoff never submits it. Delivering
// it from both places ran one user-requested task twice per merge into a linked
// worktree.

func TestAfterMergedNeverDeliversThePostprocessingPrompt(t *testing.T) {
	// Arrange -- the merged child was created with a postprocessing prompt.
	h := newHarness(t, harnessOpts{
		postprocessing: &fakePostprocessing{prompts: map[string]string{childWS: "run the release checklist"}},
	})

	// Act.
	err := h.notifier.AfterMerged(context.Background(), childRequest())

	// Assert -- the phone-home alone; the task itself ran in the child.
	if err != nil {
		t.Fatalf("AfterMerged() error = %v", err)
	}
	got := h.parents.submissions()
	if len(got) != 1 {
		t.Fatalf("submissions = %v, want ONLY the phone-home: the postprocessing task is the merge run's after-action and already ran in the child's own session", got)
	}
	if !strings.Contains(got[0].text, "MERGED into the worktree") {
		t.Fatalf("submission = %q, want the phone-home", got[0].text)
	}
}

func TestAfterActionReportsTheRecordedPostprocessingPrompt(t *testing.T) {
	// Arrange.
	h := newHarness(t, harnessOpts{
		postprocessing: &fakePostprocessing{prompts: map[string]string{childWS: "run the release checklist"}},
	})

	// Act.
	got, err := h.notifier.AfterAction(childRequest())

	// Assert.
	if err != nil {
		t.Fatalf("AfterAction() error = %v", err)
	}
	if got != "run the release checklist" {
		t.Fatalf("AfterAction() = %q, want the recorded prompt", got)
	}
}

func TestAfterActionIsLookedUpByTheChildsWorktree(t *testing.T) {
	// Arrange -- the record is keyed by the MERGED workspace's own worktree.
	h := newHarness(t, harnessOpts{
		postprocessing: &fakePostprocessing{prompts: map[string]string{childWS: "tidy up"}},
	})

	// Act.
	if _, err := h.notifier.AfterAction(childRequest()); err != nil {
		t.Fatalf("AfterAction() error = %v", err)
	}

	// Assert.
	if len(h.postprocessing.asked) != 1 || h.postprocessing.asked[0] != childWS {
		t.Fatalf("postprocessing lookups = %v, want exactly the child worktree %q", h.postprocessing.asked, childWS)
	}
}

func TestAfterActionReportsNoneForAWorkspaceCreatedWithoutOne(t *testing.T) {
	// Arrange -- the ordinary case.
	h := newHarness(t, harnessOpts{postprocessing: &fakePostprocessing{}})

	// Act.
	got, err := h.notifier.AfterAction(childRequest())

	// Assert.
	if err != nil {
		t.Fatalf("AfterAction() error = %v", err)
	}
	if got != "" {
		t.Fatalf("AfterAction() = %q, want none", got)
	}
}

func TestAfterActionLookupFailureIsSurfaced(t *testing.T) {
	// Arrange -- the creation records could not be read.
	boom := errors.New("store unreadable")
	h := newHarness(t, harnessOpts{postprocessing: &fakePostprocessing{err: boom}})

	// Act.
	_, err := h.notifier.AfterAction(childRequest())

	// Assert -- surfaced, and never collapsed into "there was none".
	if !errors.Is(err, boom) {
		t.Fatalf("AfterAction() error = %v, want the lookup failure surfaced", err)
	}
	if !h.logged("after-action prompt lookup FAILED") {
		t.Fatalf("logs = %v, want the lookup failure recorded", h.logs)
	}
}
