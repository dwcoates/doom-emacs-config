package subagentmaker

import (
	"context"
	"errors"
	"io/fs"
	"testing"

	"agent-repld/internal/account"
	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

// fixture is a registry of workspaces, each with a session and a sidecar dir,
// and the subagent files on disk.
type fixture struct {
	workspaces    []wsm.Workspace
	sessions      map[wsm.WorkspaceID]wsm.Session
	sidecars      map[string]string // workspace dir -> sidecar dir
	files         map[string]bool
	registryErr   error
	sessionErr    error
	transcriptErr error
	statErr       error
	log           *dlog.TestLogger
}

func newFixture() *fixture {
	return &fixture{
		sessions: map[wsm.WorkspaceID]wsm.Session{},
		sidecars: map[string]string{},
		files:    map[string]bool{},
		log:      dlog.NewTestLogger(),
	}
}

// add registers a workspace of REPO with a session whose sidecar is SIDECAR,
// holding the subagents named in AGENTS.
func (f *fixture) add(id wsm.WorkspaceID, repo wsm.RepoID, sidecar string, agents ...string) {
	dir := "/wt/" + string(id)
	f.workspaces = append(f.workspaces, wsm.Workspace{ID: id, Repo: repo, Dir: dir})
	f.sessions[id] = wsm.Session{Workspace: id, VendorSessionID: "vendor-" + string(id)}
	f.sidecars[dir] = sidecar
	for _, agent := range agents {
		f.files[TranscriptFile(sidecar, agent)] = true
	}
}

func (f *fixture) finder() *Finder {
	return New(Deps{
		Workspaces: func(context.Context) ([]wsm.Workspace, error) { return f.workspaces, f.registryErr },
		Session: func(_ context.Context, id wsm.WorkspaceID) (wsm.Session, bool, error) {
			if f.sessionErr != nil {
				return wsm.Session{}, false, f.sessionErr
			}
			s, ok := f.sessions[id]
			return s, ok, nil
		},
		Transcript: func(_ context.Context, dir, _ string) (account.Transcript, error) {
			if f.transcriptErr != nil {
				return account.Transcript{}, f.transcriptErr
			}
			sidecar, ok := f.sidecars[dir]
			if !ok {
				return account.Transcript{}, &account.NotFoundError{}
			}
			return account.Transcript{Path: sidecar + ".jsonl", SidecarDir: sidecar}, nil
		},
		Stat: func(path string) (fs.FileInfo, error) {
			if f.statErr != nil {
				return nil, f.statErr
			}
			if f.files[path] {
				return nil, nil
			}
			return nil, fs.ErrNotExist
		},
		Log: f.log,
	})
}

func TestFindAnswersTheWorkspaceWhoseSessionFilesTheSubagent(t *testing.T) {
	// Arrange
	f := newFixture()
	f.add("ws-other", "repo-1", "/side/other")
	f.add("ws-spawner", "repo-1", "/side/spawner", "a6e95de7")
	f.add("ws-elsewhere", "repo-2", "/side/elsewhere", "a6e95de7")

	// Act
	got, err := f.finder().Find(context.Background(), "repo-1", "a6e95de7")

	// Assert
	if err != nil || got.ID != "ws-spawner" {
		t.Fatalf("Find = %q, %v; want ws-spawner", got.ID, err)
	}
}

func TestFindAnswersNotFoundWhenNoSessionOfTheRepositoryFilesIt(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(f *fixture)
	}{
		{name: "nobody spawned it", arrange: func(f *fixture) { f.add("ws-a", "repo-1", "/side/a") }},
		{name: "only another repository's workspace did", arrange: func(f *fixture) { f.add("ws-b", "repo-2", "/side/b", "agent-x") }},
		{name: "the spawner's transcript is not on disk", arrange: func(f *fixture) {
			f.add("ws-c", "repo-1", "/side/c", "agent-x")
			delete(f.sidecars, "/wt/ws-c")
		}},
		{name: "the spawner has no session", arrange: func(f *fixture) {
			f.add("ws-d", "repo-1", "/side/d", "agent-x")
			delete(f.sessions, "ws-d")
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture()
			tt.arrange(f)

			// Act
			_, err := f.finder().Find(context.Background(), "repo-1", "agent-x")

			// Assert
			if !errors.Is(err, ErrNotFound) {
				t.Fatalf("Find = %v, want ErrNotFound", err)
			}
		})
	}
}

func TestFindFailsHard(t *testing.T) {
	tests := []struct {
		name    string
		agentID string
		arrange func(f *fixture)
		message string
	}{
		{name: "a subagent filed under two sessions", agentID: "agent-x", arrange: func(f *fixture) {
			f.add("ws-a", "repo-1", "/side/a", "agent-x")
			f.add("ws-b", "repo-1", "/side/b", "agent-x")
		}, message: "a subagent is filed under more than one session"},
		{name: "an id that names a path", agentID: "../escape", arrange: func(*fixture) {}, message: "refused a subagent id that cannot name a transcript file"},
		{name: "an unreadable registry", agentID: "agent-x", arrange: func(f *fixture) { f.registryErr = errors.New("db gone") },
			message: "could not read the registry to find a subagent's maker"},
		{name: "an unreadable session", agentID: "agent-x", arrange: func(f *fixture) {
			f.add("ws-a", "repo-1", "/side/a")
			f.sessionErr = errors.New("db gone")
		}, message: "could not read a workspace's session to find a subagent's maker"},
		{name: "an unlocatable transcript", agentID: "agent-x", arrange: func(f *fixture) {
			f.add("ws-a", "repo-1", "/side/a")
			f.transcriptErr = errors.New("permission denied")
		}, message: "could not locate a workspace's transcript to find a subagent's maker"},
		{name: "an unreadable disk", agentID: "agent-x", arrange: func(f *fixture) {
			f.add("ws-a", "repo-1", "/side/a")
			f.statErr = errors.New("io error")
		}, message: "could not read whether a session holds the subagent"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture()
			tt.arrange(f)

			// Act
			_, err := f.finder().Find(context.Background(), "repo-1", tt.agentID)

			// Assert
			if err == nil || errors.Is(err, ErrNotFound) {
				t.Fatalf("Find = %v, want a hard failure", err)
			}
			record, ok := dlog.FindRecord(f.log.Records(), dlog.RecordQuery{Level: "error", Operation: op, Message: tt.message})
			if !ok {
				t.Fatalf("no error record %q: %+v", tt.message, f.log.Records())
			}
			if record.Context["agent_id"] != tt.agentID {
				t.Errorf("record context = %+v, want the agent id", record.Context)
			}
		})
	}
}
