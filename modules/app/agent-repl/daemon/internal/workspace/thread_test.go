package workspace

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/headless"
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
	ws := wsm.Workspace{ID: wsm.NewWorkspaceID(), Kind: wsm.WorkspaceKindThread, Dir: dir, Name: name}
	r.with(ws)
	return ws, nil
}

// newThreadFixture is the verbs fixture whose state client registers threads.
func newThreadFixture(t *testing.T) (*fixture, *threadRegistry) {
	t.Helper()
	f := newFixture(t)
	registry := &threadRegistry{fakeDB: f.db}
	f.verbs.(*verbs).deps.DB = registry
	return f, registry
}

func TestCreateThreadMakesARegisteredThreadWithItsSessionUp(t *testing.T) {
	// Arrange
	f, registry := newThreadFixture(t)

	// Act
	record, err := f.verbs.CreateThread(context.Background(), "why is the sky blue?")

	// Assert
	if err != nil {
		t.Fatalf("CreateThread: %v", err)
	}
	wantDir := filepath.Join(f.home, "zettel", "threads", FixtureMintedName)
	if record.Dir != wantDir || record.Name != FixtureMintedName || !record.IsThread() {
		t.Fatalf("record = %+v, want thread %q at %q", record, FixtureMintedName, wantDir)
	}
	if info, err := os.Stat(wantDir); err != nil || !info.IsDir() {
		t.Fatalf("the thread's directory was not made: %v", err)
	}
	if len(registry.registered) != 1 || len(f.db.putSessions) != 1 || f.db.putSessions[0].Workspace != record.ID {
		t.Fatalf("registered %v, sessions %+v", registry.registered, f.db.putSessions)
	}
	if len(f.fleet.started) != 1 || f.fleet.started[0] != record.ID {
		t.Fatalf("started = %v, want the thread's session", f.fleet.started)
	}
}

func TestCreateThreadTakesTheNextSuffixWhenTheDirectoryIsHeld(t *testing.T) {
	// Arrange
	f, _ := newThreadFixture(t)
	if err := os.MkdirAll(filepath.Join(f.home, "zettel", "threads", FixtureMintedName), 0o755); err != nil {
		t.Fatal(err)
	}

	// Act
	record, err := f.verbs.CreateThread(context.Background(), "why?")

	// Assert
	if err != nil || record.Name != FixtureMintedName+"-2" {
		t.Fatalf("record = %+v, err = %v, want %s-2", record, err, FixtureMintedName)
	}
}

func TestCreateThreadRefusals(t *testing.T) {
	cases := []struct {
		name     string
		question string
		arrange  func(f *fixture)
	}{
		{name: "a blank question", question: "  "},
		{name: "a naming call that fails", question: "why?", arrange: func(f *fixture) {
			failed := headlessAnswer{err: &headless.Error{Cause: headless.CauseExitStatus, Detail: "exit status 3"}}
			f.headless.answers = []headlessAnswer{failed, failed, failed}
		}},
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
