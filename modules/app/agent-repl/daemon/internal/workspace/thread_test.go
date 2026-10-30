package workspace

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// THE THREAD VERB: a chat thread named after its first question, a directory
// under the zettel home, registered, its session up.

// threadRegistry stands in for wsm's RegisterThread on the fixture's fake DB.
type threadRegistry struct {
	*fakeDB
	err        error
	registered []string
}

func (r *threadRegistry) RegisterThread(_ context.Context, dir, name string) (wsm.Workspace, error) {
	if r.err != nil {
		return wsm.Workspace{}, r.err
	}
	r.registered = append(r.registered, name)
	// wsm records the normalized directory, as it does every workspace's.
	normalized, err := normalizeDir(dir)
	if err != nil {
		return wsm.Workspace{}, err
	}
	ws := wsm.Workspace{ID: wsm.NewWorkspaceID(), Kind: wsm.WorkspaceKindThread, Dir: normalized, Name: name}
	r.with(ws)
	return ws, nil
}

// longQuestion needs abbreviating, so it registers its thread under the
// placeholder.
const longQuestion = "why is the sky blue in the day and red at sunset?"

// newThreadFixture is the verbs fixture whose state client registers threads.
func newThreadFixture(t *testing.T) (*fixture, *threadRegistry) {
	t.Helper()
	f := newFixture(t)
	registry := &threadRegistry{fakeDB: f.db}
	f.verbs.(*verbs).deps.DB = registry
	return f, registry
}

// opened waits for the boot's thread opens and answers every thread opened.
func (f *fixture) opened() []ids.WorkspaceID {
	f.verbs.(*verbs).threadOpens.Wait()
	f.threads.mu.Lock()
	defer f.threads.mu.Unlock()
	return append([]ids.WorkspaceID(nil), f.threads.opened...)
}

func TestCreateThreadMakesTheThreadAtOnceUnderThePlaceholder(t *testing.T) {
	// Arrange
	f, registry := newThreadFixture(t)

	// Act
	record, err := f.verbs.CreateThread(context.Background(), longQuestion)

	// Assert: made at once, before any abbreviation, in a token directory.
	if err != nil {
		t.Fatalf("CreateThread: %v", err)
	}
	threads, err := normalizeDir(filepath.Join(f.home, "zettel", "threads"))
	if err != nil {
		t.Fatalf("normalize: %v", err)
	}
	if record.Name != ThreadTitlePlaceholder || !record.IsThread() || filepath.Dir(record.Dir) != threads ||
		!strings.HasPrefix(filepath.Base(record.Dir), "t-") {
		t.Fatalf("record = %+v, want a thread named %q in a t- directory under %s", record, ThreadTitlePlaceholder, threads)
	}
	if info, err := os.Stat(record.Dir); err != nil || !info.IsDir() {
		t.Fatalf("the thread's directory was not made: %v", err)
	}
	if len(registry.registered) != 1 || len(f.db.putSessions) != 1 || f.db.putSessions[0].Workspace != record.ID {
		t.Fatalf("registered %v, sessions %+v", registry.registered, f.db.putSessions)
	}
	if len(f.fleet.started) != 1 || f.fleet.started[0] != record.ID {
		t.Fatalf("started = %v, want the thread's session", f.fleet.started)
	}
	if len(f.headless.calls) != 0 {
		t.Fatalf("headless calls = %+v, want none: the chat service abbreviates", f.headless.calls)
	}
}

func TestCreateThreadNamesAThreadWhoseQuestionIsItsOwnAbbreviation(t *testing.T) {
	// Arrange
	f, _ := newThreadFixture(t)

	// Act
	record, err := f.verbs.CreateThread(context.Background(), "  why is the   sky blue?")

	// Assert
	if err != nil || record.Name != "why is the sky blue?" {
		t.Fatalf("record = %+v, err = %v; want the question as its name", record, err)
	}
}

func TestTwoThreadsOfOneQuestionShareItsName(t *testing.T) {
	// Arrange
	f, _ := newThreadFixture(t)
	first, err := f.verbs.CreateThread(context.Background(), "why?")
	if err != nil {
		t.Fatal(err)
	}

	// Act
	second, err := f.verbs.CreateThread(context.Background(), "why?")

	// Assert
	if err != nil || first.Name != "why?" || second.Name != "why?" {
		t.Fatalf("names = %q, %q, err = %v; want both the question, unsuffixed", first.Name, second.Name, err)
	}
}

func TestCreateThreadOpensTheThreadInTheChatService(t *testing.T) {
	cases := []struct {
		name    string
		openErr error
	}{
		{name: "it opens"},
		{name: "it will not open yet", openErr: errors.New("zettel is down")},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f, _ := newThreadFixture(t)
			f.threads.openErr = tc.openErr

			// Act
			record, err := f.verbs.CreateThread(context.Background(), longQuestion)

			// Assert: the thread is made either way, and the open asked for.
			if err != nil || !slices.Equal(f.opened(), []ids.WorkspaceID{record.ID}) {
				t.Fatalf("err = %v, opened = %v; want the new thread opened", err, f.opened())
			}
			_, recorded := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "info", Operation: opCreateThread, Message: "the chat service could not open the new thread yet; it is named once it opens"})
			if recorded != (tc.openErr != nil) {
				t.Fatalf("recorded = %v, want the unopened thread noted only when it would not open", recorded)
			}
		})
	}
}

func TestNameThreadNamesTheThreadAndRepublishes(t *testing.T) {
	// Arrange
	f, _ := newThreadFixture(t)
	f.db.with(wsm.Workspace{ID: "thr", Kind: wsm.WorkspaceKindThread, Dir: "/z/t-a", Name: ThreadTitlePlaceholder})

	// Act
	err := f.verbs.NameThread(context.Background(), "thr", "Why the sky is blue")

	// Assert
	if err != nil || f.db.workspaces["thr"].Name != "Why the sky is blue" || !slices.Contains(f.topbarNames, "Why the sky is blue") {
		t.Fatalf("err = %v, name = %q, topbar names = %v; want the name recorded and published", err, f.db.workspaces["thr"].Name, f.topbarNames)
	}
}

func TestNameThreadLeavesAThreadAlreadySoNamed(t *testing.T) {
	// Arrange
	f, _ := newThreadFixture(t)
	f.db.with(wsm.Workspace{ID: "thr", Kind: wsm.WorkspaceKindThread, Dir: "/z/t-a", Name: "Why"})
	f.db.setNameErr = errors.New("a rename was attempted")

	// Act
	err := f.verbs.NameThread(context.Background(), "thr", "Why")

	// Assert
	if err != nil || len(f.topbarNames) != 0 {
		t.Fatalf("err = %v, topbar names = %v; want nothing renamed or published", err, f.topbarNames)
	}
}

func TestNameThreadFailures(t *testing.T) {
	cases := []struct {
		name    string
		arrange func(f *fixture)
		message string
	}{
		{name: "the thread cannot be read", arrange: func(f *fixture) { f.db.workspaceErr = errors.New("registry down") }, message: "could not read the thread to name it"},
		{name: "the name cannot be recorded", arrange: func(f *fixture) { f.db.setNameErr = errors.New("registry down") }, message: "could not record the thread's name"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f, _ := newThreadFixture(t)
			f.db.with(wsm.Workspace{ID: "thr", Kind: wsm.WorkspaceKindThread, Dir: "/z/t-a", Name: ThreadTitlePlaceholder})
			tc.arrange(f)

			// Act
			err := f.verbs.NameThread(context.Background(), "thr", "Why")

			// Assert
			if err == nil || !strings.Contains(err.Error(), "registry down") || len(f.topbarNames) != 0 {
				t.Fatalf("err = %v, topbar names = %v; want the failure answered and nothing published", err, f.topbarNames)
			}
			r, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "error", Operation: opThreadName, Message: tc.message})
			if !ok || r.Context["name"] != "Why" || r.Context["cause"] != "registry down" {
				t.Fatalf("record = %+v, found %v; want the failure with the name and cause", r, ok)
			}
		})
	}
}

func TestNameThreadPanicsOnANonThread(t *testing.T) {
	// Arrange
	f, _ := newThreadFixture(t)
	f.db.with(wsm.Workspace{ID: "ws", Kind: wsm.WorkspaceKindRepository, Dir: "/r/ws", Name: "fix-flaky"})
	defer func() {
		if recover() == nil {
			t.Fatal("NameThread named a repository workspace")
		}
	}()

	// Act
	_ = f.verbs.NameThread(context.Background(), "ws", "Why")
}

func TestOpenThreadsOpensEveryThread(t *testing.T) {
	// Arrange: two threads and an agent workspace.
	f, _ := newThreadFixture(t)
	f.db.with(wsm.Workspace{ID: "a", Kind: wsm.WorkspaceKindThread, Dir: "/z/t-a", Name: ThreadTitlePlaceholder})
	f.db.with(wsm.Workspace{ID: "b", Kind: wsm.WorkspaceKindThread, Dir: "/z/t-b", Name: "Rust lifetimes"})
	f.db.with(wsm.Workspace{ID: "ws", Kind: wsm.WorkspaceKindRepository, Dir: "/r/ws", Name: "fix-flaky"})

	// Act
	err := f.verbs.OpenThreads(context.Background())

	// Assert
	got := f.opened()
	slices.Sort(got)
	if err != nil || !slices.Equal(got, []ids.WorkspaceID{"a", "b"}) {
		t.Fatalf("err = %v, opened = %v; want both threads and nothing else", err, got)
	}
}

func TestOpenThreadsFailsOnAnUnreadableRegistry(t *testing.T) {
	// Arrange
	f, _ := newThreadFixture(t)
	f.db.listWorkspacesErr = errors.New("registry down")

	// Act
	err := f.verbs.OpenThreads(context.Background())

	// Assert
	if err == nil || len(f.opened()) != 0 {
		t.Fatalf("err = %v, opened = %v; want the failure and no opens", err, f.opened())
	}
	if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "error", Operation: opThreadName}); !ok {
		t.Fatalf("no error record of the unreadable registry")
	}
}

func TestCreateThreadAdmitsTheThreadAsEveryWorkspaceIs(t *testing.T) {
	// Arrange
	f, _ := newThreadFixture(t)

	// Act
	record, err := f.verbs.CreateThread(context.Background(), "why is the sky blue?")

	// Assert: bound, primed and claimed, as a registration admits a workspace.
	if err != nil {
		t.Fatalf("CreateThread: %v", err)
	}
	if f.footer.dirs[record.ID] != record.Dir {
		t.Fatalf("footer dirs = %v, want the thread's directory bound", f.footer.dirs)
	}
	if !slices.Contains(f.footer.primed, record.ID) {
		t.Fatalf("primed = %v, want the thread's footer primed", f.footer.primed)
	}
}

func TestEveryRegistrationAdmitsThroughAdmit(t *testing.T) {
	// Arrange / Act: the one admission, and every site that admits a row.
	sites := 0
	for _, file := range []string{"register.go", "thread.go"} {
		src, err := os.ReadFile(file)
		if err != nil {
			t.Fatal(err)
		}
		sites += strings.Count(string(src), "v.admit(ctx, log, record, ")
	}

	// Assert
	if sites != 2 {
		t.Fatalf("admit is called at %d sites, want registerRow's and CreateThread's", sites)
	}
}

func TestCreateThreadRefusals(t *testing.T) {
	cases := []struct {
		name     string
		question string
		arrange  func(f *fixture)
	}{
		{name: "a blank question", question: "  "},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f, registry := newThreadFixture(t)
			if tc.arrange != nil {
				tc.arrange(f)
			}

			// Act
			_, err := f.verbs.CreateThread(context.Background(), tc.question)

			// Assert
			refused, ok := AsRefusal(err)
			if !ok || refused.Arm != ArmCreationFailed {
				t.Fatalf("err = %v, want the creation_failed refusal", err)
			}
			if len(registry.registered) != 0 || len(f.fleet.started) != 0 {
				t.Fatalf("a refused thread was made: %v, %v", registry.registered, f.fleet.started)
			}
		})
	}
}

func TestCreateThreadFailures(t *testing.T) {
	cases := []struct {
		name    string
		arrange func(f *fixture, r *threadRegistry)
		message string
	}{
		{name: "registration fails", arrange: func(_ *fixture, r *threadRegistry) { r.err = errors.New("db gone") },
			message: "could not register the thread"},
		{name: "the session does not come up", arrange: func(f *fixture, _ *threadRegistry) { f.fleet.startErr = errors.New("no shim") },
			message: "the thread's session did not come up"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f, registry := newThreadFixture(t)
			tc.arrange(f, registry)

			// Act
			_, err := f.verbs.CreateThread(context.Background(), "why?")

			// Assert
			if err == nil {
				t.Fatal("CreateThread answered no error")
			}
			if _, ok := AsRefusal(err); ok {
				t.Fatalf("err = %v, want a failure, not a refusal", err)
			}
			if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "error", Message: tc.message}); !ok {
				t.Fatalf("no error record %q in %+v", tc.message, f.log.logger.Records())
			}
		})
	}
}

func TestChatSystemPromptReadsTheCorpusBrief(t *testing.T) {
	cases := []struct {
		name    string
		dir     func(t *testing.T) string
		wantErr bool
	}{
		{name: "the shipped corpus", dir: func(*testing.T) string { return shippedPromptsDir }},
		{name: "a corpus without the brief", dir: func(t *testing.T) string { return t.TempDir() }, wantErr: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			read := ChatSystemPrompt(tc.dir(t))

			// Act
			text, err := read()

			// Assert
			if (err != nil) != tc.wantErr {
				t.Fatalf("err = %v, want error %v", err, tc.wantErr)
			}
			if !tc.wantErr && text == "" {
				t.Fatal("the shipped chat system prompt is empty")
			}
		})
	}
}

func TestAGoneThreadDirectoryIsMadeAgainWithNoGit(t *testing.T) {
	// Arrange
	f := newFixture(t)
	dir := filepath.Join(f.home, "zettel", "threads", "why")
	ws := wsm.Workspace{ID: "t1", Kind: wsm.WorkspaceKindThread, Dir: dir, Name: "why"}
	f.db.with(ws)
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID}

	// Act
	restored, err := f.verbs.(*verbs).RestoreMissingWorktree(context.Background(), opOpen, ws)

	// Assert
	if !restored || err != nil {
		t.Fatalf("RestoreMissingWorktree = (%v, %v), want the thread restored", restored, err)
	}
	if info, statErr := os.Stat(dir); statErr != nil || !info.IsDir() {
		t.Fatalf("the thread's directory was not made again: %v", statErr)
	}
	if len(f.git.gitCalls) != 0 || len(f.git.restored) != 0 {
		t.Fatalf("git calls = %v, want none for a thread", f.git.gitCalls)
	}
}

// heldThread is a thread CreateThread made on F, its row then closed when
// CLOSED.
func heldThread(t *testing.T, f *fixture, closed bool) wsm.Workspace {
	t.Helper()
	record, err := f.verbs.CreateThread(context.Background(), "why is the sky blue?")
	if err != nil {
		t.Fatalf("CreateThread: %v", err)
	}
	if closed {
		record.Closed = true
		f.db.with(record)
	}
	return record
}

func TestReRegisteringAHeldThreadAnswersTheThread(t *testing.T) {
	// Arrange: Emacs re-announces the thread's directory at a handover.
	f, _ := newThreadFixture(t)
	thread := heldThread(t, f, false)

	// Act
	again, err := f.verbs.Register(context.Background(), thread.Dir, wsm.RegisterFacts{})

	// Assert: the thread itself, and no git registration of its directory.
	if err != nil || again.ID != thread.ID || !again.IsThread() {
		t.Fatalf("Register = %+v, %v, want thread %s", again, err, thread.ID)
	}
	if len(f.db.registered) != 0 {
		t.Fatalf("registrations = %+v, want none: a thread is no git worktree", f.db.registered)
	}
}

func TestReRegisteringAClosedThreadReopensIt(t *testing.T) {
	// Arrange
	f, _ := newThreadFixture(t)
	thread := heldThread(t, f, true)

	// Act
	again, err := f.verbs.Register(context.Background(), thread.Dir, wsm.RegisterFacts{})

	// Assert
	if err != nil || again.Closed {
		t.Fatalf("Register = %+v, %v, want the thread re-opened", again, err)
	}
}

func TestAnUnreadableRegistryFailsTheAnnouncementAndIsRecorded(t *testing.T) {
	// Arrange
	f, _ := newThreadFixture(t)
	thread := heldThread(t, f, false)
	f.db.workspaceByDirErr = errors.New("sql: database is locked")

	// Act
	_, err := f.verbs.Register(context.Background(), thread.Dir, wsm.RegisterFacts{})

	// Assert
	if !errors.Is(err, f.db.workspaceByDirErr) {
		t.Fatalf("Register = %v, want the lookup's failure", err)
	}
	rec, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "error", Operation: opRegister})
	if !ok || rec.Context["dir"] != thread.Dir {
		t.Fatalf("ERROR record = %+v, want one naming the announced directory", rec)
	}
}
