package workspace

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"agent-repld/internal/dlog"
	"agent-repld/internal/prompts"
	"agent-repld/internal/wsm"
)

// A ZETTEL CHAT THREAD is a workspace with no repository, branch or worktree:
// a directory under the zettel home and a session that answers questions. It
// shares every session verb with an agent workspace, and is left out of every
// repository one.

// BriefChatSystem is the corpus brief a chat thread's session runs under.
const BriefChatSystem = "chat-thread-system"

// opCreateThread is CreateThread's log operation.
const opCreateThread = "daemon.workspace.create_thread"

// ArmCreationFailed is CreateThread's one refusal arm.
const ArmCreationFailed = "creation_failed"

// ThreadsDir is where every chat thread's directory lives: ~/zettel/threads,
// fixed (owner ruling, 2026-10-24).
func ThreadsDir(home string) string { return filepath.Join(home, "zettel", "threads") }

// ChatSystemPrompt reads the chat system prompt from the corpus DIR, at use
// time: what FleetDeps.ChatSystemPrompt is in production.
func ChatSystemPrompt(dir string) func() (string, error) {
	return func() (string, error) {
		brief, err := prompts.Load(dir, BriefChatSystem)
		if err != nil {
			return "", err
		}
		return brief.Splice(nil)
	}
}

// CreateThread makes a new chat thread AT ONCE and brings its session up;
// the thread is titled after its first question afterwards (startTitling).
// The question itself is NOT submitted here: the caller submits it through
// the prompt handler, the one submission path, so it reaches the session
// exactly as every later question does.
//
// A THREAD IS SWITCHED TO BEFORE IT HAS A TITLE (owner ruling, 2026-10-25):
// it is registered under ThreadTitlePlaceholder, which every surface draws
// until the title arrives and renames it. Its DIRECTORY is a fresh token,
// never its name, and never moves: the live session works in it and the
// vendor keeps its transcript under its path, so a rename that moved it
// would strand both.
func (v *verbs) CreateThread(ctx context.Context, question string) (wsm.Workspace, error) {
	threads := ThreadsDir(v.deps.HomeDir)
	global := v.deps.Log.Global().With(dlog.Context{"threads_dir": threads})
	if strings.TrimSpace(question) == "" {
		return wsm.Workspace{}, refuse(global, "CreateThread", ArmCreationFailed, "a thread starts from a question, and this one is blank", false)
	}

	dir := filepath.Join(threads, newThreadToken())
	if err := os.MkdirAll(dir, 0o755); err != nil {
		global.Error(opCreateThread, "could not make the thread's directory", dlog.Context{"dir": dir, "cause": err.Error()})
		return wsm.Workspace{}, fmt.Errorf("create thread at %q: %w", dir, err)
	}
	// THE PLACEHOLDER IS CLAIMED AND REGISTERED under one naming lock, so
	// two threads created together take "Loading…" and "Loading… (2)".
	v.names.mu.Lock()
	name, err := v.freeThreadNameLocked(ctx, ThreadTitlePlaceholder, "")
	var record wsm.Workspace
	if err == nil {
		record, err = v.deps.DB.RegisterThread(ctx, dir, name)
	}
	v.names.mu.Unlock()
	if err != nil {
		global.Error(opCreateThread, "could not register the thread", dlog.Context{"dir": dir, "cause": err.Error()})
		return wsm.Workspace{}, fmt.Errorf("create thread at %q: register: %w", dir, err)
	}
	log, err := v.deps.Log.Workspace(record.Dir)
	if err != nil {
		global.Error(opCreateThread, "could not resolve the thread's log sink", dlog.Context{"dir": record.Dir, "cause": err.Error()})
		return wsm.Workspace{}, fmt.Errorf("create thread %q: resolve log sink: %w", name, err)
	}
	log = log.With(dlog.Context{"workspace": string(record.ID)})
	// A THREAD IS ADMITTED AS EVERY WORKSPACE IS (admit): its directory bound
	// on the resolvers, its naming published, its serving claimed.
	if err := v.admit(ctx, log, record, true); err != nil {
		return wsm.Workspace{}, fmt.Errorf("create thread %q: %w", name, err)
	}

	// THE SPAWN FACTS, with the host identity minted with the row, exactly as
	// Create records them (create.go).
	session := wsm.Session{
		Workspace:      record.ID,
		HostSessionID:  wsm.NewHostSessionID(),
		ConfigDir:      v.deps.Accounts.ConfigDirFor(record.Dir),
		PermissionMode: mintedPermissionMode(""),
		StartedAt:      v.now(),
	}
	if err := v.deps.DB.PutSession(ctx, session); err != nil {
		log.Error(opCreateThread, "could not record the thread's spawn facts", dlog.Context{"cause": err.Error()})
		return wsm.Workspace{}, fmt.Errorf("create thread %q: record the spawn facts: %w", name, err)
	}
	if err := v.deps.Sessions.Start(ctx, record.ID); err != nil {
		log.Error(opCreateThread, "the thread's session did not come up", dlog.Context{"cause": err.Error()})
		return wsm.Workspace{}, fmt.Errorf("create thread %q: start the session: %w", name, err)
	}
	log.Info(opCreateThread, "created the thread", dlog.Context{"name": name, "dir": record.Dir})
	v.republishRegistry(ctx, log, opCreateThread)
	v.startTitling(ctx, log, record, question)
	return record, nil
}

// newThreadToken is a fresh name for a thread's directory: random, so no two
// threads, and no title, can ever claim the same one.
func newThreadToken() string {
	return "t-" + string(wsm.NewWorkspaceID())
}
