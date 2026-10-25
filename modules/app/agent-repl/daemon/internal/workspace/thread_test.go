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
	"agent-repld/internal/headless"
	"agent-repld/internal/ids"
	"agent-repld/internal/prompts"
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

// FixtureThreadTitle is the title the fake model answers a thread's question with.
const FixtureThreadTitle = "Why the Sky Is Blue"

// newThreadFixture is the verbs fixture whose state client registers threads,
// and whose model answers a title.
func newThreadFixture(t *testing.T) (*fixture, *threadRegistry) {
	t.Helper()
	f := newFixture(t)
	registry := &threadRegistry{fakeDB: f.db}
	f.verbs.(*verbs).deps.DB = registry
	f.headless.answers = []headlessAnswer{{text: FixtureThreadTitle}}
	f.briefs[BriefThreadTitle] = prompts.Prompt{Name: BriefThreadTitle, Body: "title: {{question}}", Placeholders: []string{"question"}}
	return f, registry
}

// titled waits for every title in flight.
func (f *fixture) titled() { f.verbs.(*verbs).threadTitles.Wait() }

func TestCreateThreadMakesTheThreadAtOnceUnderThePlaceholder(t *testing.T) {
	// Arrange
	f, registry := newThreadFixture(t)

	// Act
	record, err := f.verbs.CreateThread(context.Background(), "why is the sky blue?")
	f.titled()

	// Assert: made at once, before any title, in a token directory.
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
}

func TestATitledThreadIsRenamedToItsTitle(t *testing.T) {
	// Arrange
	f, _ := newThreadFixture(t)

	// Act
	record, err := f.verbs.CreateThread(context.Background(), "why is the sky blue?")
	f.titled()

	// Assert
	if err != nil || f.db.workspaces[record.ID].Name != FixtureThreadTitle {
		t.Fatalf("name = %q, err = %v; want %q", f.db.workspaces[record.ID].Name, err, FixtureThreadTitle)
	}
	if call := f.headless.calls[0]; !strings.Contains(call.Prompt, "why is the sky blue?") || call.Model != headless.ModelHaiku {
		t.Fatalf("title call = %+v, want haiku asked about the question", call)
	}
}

func TestAThreadThatCannotBeTitledIsUntitledAndRecorded(t *testing.T) {
	cases := []struct {
		name    string
		answers []headlessAnswer
	}{
		{name: "the call fails", answers: []headlessAnswer{{err: &headless.Error{Cause: headless.CauseExitStatus, Detail: "exit status 3"}}}},
		{name: "every answer is a slug", answers: []headlessAnswer{{text: "sky-color"}}},
		{name: "the brief is missing", answers: nil},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f, _ := newThreadFixture(t)
			f.headless.answers = tc.answers
			if tc.answers == nil {
				delete(f.briefs, BriefThreadTitle)
			}

			// Act
			record, err := f.verbs.CreateThread(context.Background(), "why?")
			f.titled()

			// Assert
			if err != nil || f.db.workspaces[record.ID].Name != UntitledThread {
				t.Fatalf("name = %q, err = %v; want %q", f.db.workspaces[record.ID].Name, err, UntitledThread)
			}
			if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "error", Operation: opThreadTitle}); !ok {
				t.Fatalf("no error record of the failed title in %+v", f.log.logger.Records())
			}
		})
	}
}

func TestTwoThreadsOfOneTitleTakeASuffix(t *testing.T) {
	// Arrange
	f, _ := newThreadFixture(t)
	first, err := f.verbs.CreateThread(context.Background(), "why?")
	f.titled()
	if err != nil {
		t.Fatal(err)
	}

	// Act
	second, err := f.verbs.CreateThread(context.Background(), "why, again?")
	f.titled()

	// Assert
	names := []string{f.db.workspaces[first.ID].Name, f.db.workspaces[second.ID].Name}
	if err != nil || !slices.Equal(names, []string{FixtureThreadTitle, FixtureThreadTitle + " (2)"}) {
		t.Fatalf("names = %v, err = %v", names, err)
	}
}

func TestAThreadMadeWhileAnotherIsUntitledTakesTheNextPlaceholder(t *testing.T) {
	// Arrange: a thread still under the placeholder.
	f, _ := newThreadFixture(t)
	f.db.with(wsm.Workspace{ID: "held", Kind: wsm.WorkspaceKindThread, Dir: "/z/t-held", Name: ThreadTitlePlaceholder})

	// Act
	record, err := f.verbs.CreateThread(context.Background(), "why?")
	f.titled()

	// Assert
	if err != nil || record.Name != ThreadTitlePlaceholder+" (2)" {
		t.Fatalf("record = %+v, err = %v; want the next placeholder", record, err)
	}
}

func TestValidateThreadTitle(t *testing.T) {
	cases := []struct {
		answer, want string
		ok           bool
	}{
		{answer: "Fusion in the Sun", want: "Fusion in the Sun", ok: true},
		{answer: "  \"Fusion in the Sun.\"  ", want: "Fusion in the Sun", ok: true},
		{answer: "Sun  Fusion", want: "Sun Fusion", ok: true},
		{answer: "sun-fusion", ok: false},
		{answer: "One Two Three Four Five Six", ok: false},
		{answer: "Two\nLines", ok: false},
		{answer: "Supercalifragilistic Expialidocious Antidisestablishmentarian", ok: false},
	}
	for _, tc := range cases {
		t.Run(tc.answer, func(t *testing.T) {
			got, err := ValidateThreadTitle(tc.answer)
			if (err == nil) != tc.ok || got != tc.want {
				t.Fatalf("ValidateThreadTitle(%q) = %q, %v; want %q, ok %v", tc.answer, got, err, tc.want, tc.ok)
			}
		})
	}
}

func TestResumeThreadTitlesTitlesWhatAStoppedDaemonLeftUntitled(t *testing.T) {
	// Arrange: one thread left under the placeholder, one titled, one agent workspace.
	f, _ := newThreadFixture(t)
	f.db.with(wsm.Workspace{ID: "untitled", Kind: wsm.WorkspaceKindThread, Dir: "/z/t-a", Name: ThreadTitlePlaceholder})
	f.db.with(wsm.Workspace{ID: "done", Kind: wsm.WorkspaceKindThread, Dir: "/z/t-b", Name: "Rust Lifetime Basics"})
	f.db.firstQuestions = map[ids.WorkspaceID]string{"untitled": "why is the sky blue?"}

	// Act
	err := f.verbs.(*verbs).ResumeThreadTitles(context.Background())
	f.titled()

	// Assert
	if err != nil || f.db.workspaces["untitled"].Name != FixtureThreadTitle || f.db.workspaces["done"].Name != "Rust Lifetime Basics" {
		t.Fatalf("names = %q, %q; err = %v", f.db.workspaces["untitled"].Name, f.db.workspaces["done"].Name, err)
	}
	if len(f.headless.calls) != 1 || !strings.Contains(f.headless.calls[0].Prompt, "why is the sky blue?") {
		t.Fatalf("title calls = %+v, want one, of the untitled thread's question", f.headless.calls)
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
		{name: "the shipped corpus", dir: func(*testing.T) string { return filepath.Join("..", "..", "..", "prompts") }},
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
