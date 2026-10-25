package workspace

import (
	"context"
	"errors"
	"fmt"
	"io/fs"
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

// CreateThread makes a new chat thread named after its first question and
// brings its session up. The question itself is NOT submitted here: the
// caller submits it through the prompt handler, the one submission path, so
// it reaches the session exactly as every later question does.
func (v *verbs) CreateThread(ctx context.Context, question string) (wsm.Workspace, error) {
	threads := ThreadsDir(v.deps.HomeDir)
	global := v.deps.Log.Global().With(dlog.Context{"threads_dir": threads})
	if strings.TrimSpace(question) == "" {
		return wsm.Workspace{}, refuse(global, "CreateThread", ArmCreationFailed, "a thread starts from a question, and this one is blank", false)
	}

	slug, err := v.mintName(ctx, global, threads, question, "")
	if err != nil {
		var failure *namingFailure
		if errors.As(err, &failure) {
			return wsm.Workspace{}, refuse(global, "CreateThread", ArmCreationFailed,
				fmt.Sprintf("the thread could not be named: %s", failure.Detail), false)
		}
		return wsm.Workspace{}, err
	}
	// A THREAD'S NAME IS ITS DIRECTORY'S, so a directory already there holds
	// the name as surely as a registered workspace does.
	name, release, err := v.freeNameAgainst(ctx, global, slug, func(candidate string) (bool, error) {
		_, err := os.Stat(filepath.Join(threads, candidate))
		switch {
		case err == nil:
			return true, nil
		case errors.Is(err, fs.ErrNotExist):
			return false, nil
		}
		return false, fmt.Errorf("probe the thread directory %q: %w", candidate, err)
	})
	if err != nil {
		global.Error(opCreateThread, "could not find a free name for the thread", dlog.Context{"slug": slug, "cause": err.Error()})
		return wsm.Workspace{}, fmt.Errorf("create thread %q: %w", slug, err)
	}
	defer release()

	dir := filepath.Join(threads, name)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		global.Error(opCreateThread, "could not make the thread's directory", dlog.Context{"dir": dir, "cause": err.Error()})
		return wsm.Workspace{}, fmt.Errorf("create thread %q: %w", name, err)
	}
	record, err := v.deps.DB.RegisterThread(ctx, dir, name)
	if err != nil {
		global.Error(opCreateThread, "could not register the thread", dlog.Context{"dir": dir, "cause": err.Error()})
		return wsm.Workspace{}, fmt.Errorf("create thread %q: register: %w", name, err)
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
	return record, nil
}
