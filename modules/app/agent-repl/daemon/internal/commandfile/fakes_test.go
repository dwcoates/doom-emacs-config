package commandfile

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
	"claude-repld/internal/merge"
	"claude-repld/internal/prompthandler"
	"claude-repld/internal/workspace"
	"claude-repld/internal/wsm"
)

// errFake is what a fake answers when a test arranged a failure without caring
// which one.
var errFake = errors.New("commandfile test: arranged failure")

// fixedNow is the instant a file's age is judged against.
var fixedNow = time.Date(2026, 8, 29, 12, 0, 0, 0, time.UTC)

// verbCall is one recorded verb invocation, so a test asserts that an entry
// reached THE SAME internal path the equivalent rpc would.
type verbCall struct {
	Verb string
	WS   ids.WorkspaceID
	Spec workspace.CreateSpec
	Task ids.TaskID
	Text string
}

// fakeVerbs records every verb the ingress drove.
type fakeVerbs struct {
	workspace.Verbs

	calls   []verbCall
	byID    map[ids.WorkspaceID]wsm.Workspace
	err     error
	created wsm.Workspace
}

func newFakeVerbs() *fakeVerbs {
	return &fakeVerbs{
		byID:    map[ids.WorkspaceID]wsm.Workspace{},
		created: wsm.Workspace{ID: "created-1"},
	}
}

func (v *fakeVerbs) Resolve(_ context.Context, ref *workspacev1.WorkspaceRef) (wsm.Workspace, error) {
	ws, ok := v.byID[ids.WorkspaceID(ref.GetId())]
	if !ok {
		return wsm.Workspace{}, errors.New("no such workspace")
	}
	if ref.GetDir() != "" && ref.GetDir() != ws.Dir {
		return wsm.Workspace{}, errors.New("the echoed dir disagrees with the registry")
	}
	return ws, nil
}

func (v *fakeVerbs) Create(_ context.Context, spec workspace.CreateSpec) (wsm.Workspace, error) {
	if v.err != nil {
		return wsm.Workspace{}, v.err
	}
	v.calls = append(v.calls, verbCall{Verb: "create", Spec: spec})
	return v.created, nil
}

func (v *fakeVerbs) Close(_ context.Context, ws ids.WorkspaceID) error {
	v.calls = append(v.calls, verbCall{Verb: "close", WS: ws})
	return v.err
}

func (v *fakeVerbs) Forget(_ context.Context, ws ids.WorkspaceID) error {
	v.calls = append(v.calls, verbCall{Verb: "forget", WS: ws})
	return v.err
}

func (v *fakeVerbs) Open(_ context.Context, ws ids.WorkspaceID, _ workspace.OpenProgress) error {
	v.calls = append(v.calls, verbCall{Verb: "open", WS: ws})
	return v.err
}

func (v *fakeVerbs) Select(_ context.Context, ws ids.WorkspaceID) error {
	v.calls = append(v.calls, verbCall{Verb: "select", WS: ws})
	return v.err
}

func (v *fakeVerbs) CreateTask(_ context.Context, title string) (wsm.Task, error) {
	if v.err != nil {
		return wsm.Task{}, v.err
	}
	v.calls = append(v.calls, verbCall{Verb: "create_task", Text: title})
	return wsm.Task{ID: "task-1", Title: title}, nil
}

func (v *fakeVerbs) UpdateTask(_ context.Context, id ids.TaskID, change wsm.TaskChange) error {
	call := verbCall{Verb: "update_task", Task: id}
	if change.Done != nil && *change.Done {
		call.Text = "done"
	}
	v.calls = append(v.calls, call)
	return v.err
}

func (v *fakeVerbs) AssignTask(_ context.Context, ws ids.WorkspaceID, task *ids.TaskID) error {
	call := verbCall{Verb: "assign_task", WS: ws}
	if task != nil {
		call.Task = *task
	}
	v.calls = append(v.calls, call)
	return v.err
}

// fakeMerge is a merge.Orchestrator.
type fakeMerge struct {
	merge.Orchestrator

	enqueued []ids.WorkspaceID
	// by records who each enqueue said asked for the merge.
	by  []merge.Requester
	err error
}

func (m *fakeMerge) Enqueue(_ context.Context, ws ids.WorkspaceID, by merge.Requester) error {
	if m.err != nil {
		return m.err
	}
	m.enqueued = append(m.enqueued, ws)
	m.by = append(m.by, by)
	return nil
}

// submitted is one recorded prompt submission.
type submitted struct {
	WS     ids.WorkspaceID
	Text   string
	Key    string
	Origin conversationv1.PromptOrigin
}

// fakePrompts is SubmitPrompt's own body, which is what a command-file prompt
// must reach.
type fakePrompts struct {
	prompthandler.Handler

	submissions []submitted
	err         error
}

func (p *fakePrompts) Submit(_ context.Context, ws ids.WorkspaceID, said *conversationv1.UserSaid, key string, origin conversationv1.PromptOrigin, _ wsm.Delivery, _ *feedid.Ref) (prompthandler.Outcome, error) {
	if p.err != nil {
		return prompthandler.Outcome{}, p.err
	}
	p.submissions = append(p.submissions, submitted{
		WS: ws, Text: said.GetContent().GetBlocks()[0].GetText().GetText(), Key: key, Origin: origin,
	})
	return prompthandler.Outcome{}, nil
}

// fakeDB resolves an entry that names only a directory.
type fakeDB struct {
	wsm.DB

	byDir map[string]wsm.Workspace
}

func (d *fakeDB) WorkspaceByDir(_ context.Context, dir string) (wsm.Workspace, error) {
	ws, ok := d.byDir[dir]
	if !ok {
		return wsm.Workspace{}, errors.New("no workspace at that dir")
	}
	return ws, nil
}

// fakeSurfaces is a dlog.Surfaces backed by one capturing logger.
type fakeSurfaces struct{ logger *dlog.TestLogger }

func newFakeSurfaces() *fakeSurfaces { return &fakeSurfaces{logger: dlog.NewTestLogger()} }

func (s *fakeSurfaces) Global() dlog.Logger { return s.logger }

func (s *fakeSurfaces) Workspace(dir string) (dlog.Logger, error) {
	return s.logger.With(dlog.Context{"dir": dir}), nil
}

// WorkspaceOrCentral implements dlog.Surfaces. This double always resolves.
func (s *fakeSurfaces) WorkspaceOrCentral(dir string) dlog.Logger {
	return s.logger.With(dlog.Context{"dir": dir})
}

func (s *fakeSurfaces) ShimSink(string) (dlog.Borrowed, error) { return nil, errFake }

// BindWorkspaceIDs implements dlog.Surfaces. This double answers its own
// workspace ids, so there is no lookup to install.
func (s *fakeSurfaces) BindWorkspaceIDs(dlog.WorkspaceIDLookup) {}

func (s *fakeSurfaces) ShimRollRequests() <-chan dlog.ShimRollRequest { return nil }

func (s *fakeSurfaces) DetachDir(string) error { return nil }

func (s *fakeSurfaces) AttachDir(string) error { return nil }

func (s *fakeSurfaces) ClientLog(string, dlog.ClientRecord) error { return errFake }

func (s *fakeSurfaces) Close() error { return nil }

// fixture is one arranged ingress plus every fake behind it.
type fixture struct {
	ingress Ingress
	dir     string
	verbs   *fakeVerbs
	merge   *fakeMerge
	prompts *fakePrompts
	db      *fakeDB
	log     *fakeSurfaces
	now     time.Time
	// serves is what the ingress's serving answer says.
	serves bool
}

// newFixture arranges an ingress over a fresh temp directory.
func newFixture(t *testing.T) *fixture {
	t.Helper()
	f := &fixture{
		dir:     t.TempDir(),
		verbs:   newFakeVerbs(),
		merge:   &fakeMerge{},
		prompts: &fakePrompts{},
		db:      &fakeDB{byDir: map[string]wsm.Workspace{}},
		log:     newFakeSurfaces(),
		now:     fixedNow,
		serves:  true,
	}
	ingress, err := New(Deps{
		Dir: f.dir, Verbs: f.verbs, DB: f.db, Merge: f.merge, Prompts: f.prompts, Log: f.log,
		Home:     fixtureHome,
		Serves:   func() bool { return f.serves },
		Interval: 10 * time.Millisecond,
		Now:      func() time.Time { return f.now },
	})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	f.ingress = ingress
	return f
}

// fixtureHome is the home directory every fixture's ingress expands `~` to.
const fixtureHome = "/Users/fixture"

// workspace records one workspace both fakes can resolve.
func (f *fixture) workspace(id ids.WorkspaceID, dir string) wsm.Workspace {
	ws := wsm.Workspace{ID: id, Dir: dir}
	f.verbs.byID[id] = ws
	f.db.byDir[dir] = ws
	return ws
}

// write drops one command file into the ingress directory and back-dates it so
// the settling window has already passed.
func (f *fixture) write(t *testing.T, name, body string) string {
	t.Helper()
	path := filepath.Join(f.dir, name)
	if err := os.WriteFile(path, []byte(body), 0o644); err != nil {
		t.Fatalf("write %s: %v", name, err)
	}
	old := f.now.Add(-time.Hour)
	if err := os.Chtimes(path, old, old); err != nil {
		t.Fatalf("chtimes %s: %v", name, err)
	}
	return path
}

// entries lists the file names under one of the ingress's side directories.
func entries(t *testing.T, dir string) []string {
	t.Helper()
	items, err := os.ReadDir(dir)
	if err != nil {
		if os.IsNotExist(err) {
			return nil
		}
		t.Fatalf("read %s: %v", dir, err)
	}
	var out []string
	for _, item := range items {
		out = append(out, item.Name())
	}
	return out
}

// verbNames renders the recorded calls for an assertion message.
func verbNames(calls []verbCall) []string {
	out := make([]string, 0, len(calls))
	for _, call := range calls {
		out = append(out, call.Verb)
	}
	return out
}

// Evict satisfies dlog.Surfaces for the merged seam (the bootinfra agent added it).
func (s *fakeSurfaces) Evict(_ string) error { return nil }
