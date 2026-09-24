package workspace

import (
	"context"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/shimclient"
)

// HISTORY IS REPLAYED ONLY WHEN A WORKSPACE IS OPENED OR A TRANSCRIPT IS
// SELECTED, AND ONLY THE FIRST PAGE (owner rule, 2026-09-23). See
// sessionwatcher/opening.go for why a first page served onto a feed that
// already holds it is not harmless.
//
// The fleet is where the rule is decided, because the fleet is the one party
// that knows whether its views already hold a workspace's conversation: they do
// exactly when this process has watched it before and no bind has reset it
// since. So every watcher the fleet starts goes through startWatcher, and its
// Opening comes from openingFor:
//
//   - a transcript SELECTED since the last watcher -> TranscriptSelected;
//   - a workspace this process has never watched, or whose session is coming
//     up FRESH (a new conversation, a new book) -> WorkspaceOpened;
//   - anything else (a restart, a revival, a rollout's relaunch, a cold gate's
//     re-open) resumes from the pointers of the watcher it replaces.
//
// The feed keeps a workspace's rows across every bring-up but a bind
// (resolve/feed ResetWorkspace), which is what makes the resume correct: the
// rows a successor would replay are already standing.

// openingFor decides how the next watcher for ws begins. It is the ONLY
// production caller of sessionwatcher.WorkspaceOpened and
// sessionwatcher.TranscriptSelected, and TestReplayOpeningsHaveOneCallerEach
// holds it to that.
func (f *Fleet) openingFor(ws ids.WorkspaceID) sessionwatcher.Opening {
	f.mu.Lock()
	selected := f.selected[ws]
	prior, watched := f.watched[ws]
	f.mu.Unlock()
	switch {
	case selected:
		return sessionwatcher.TranscriptSelected()
	case watched:
		// OFF THE FLEET'S LOCK: the watcher takes its own, and it calls the
		// fleet's sinks while holding it.
		return sessionwatcher.ResumeFrom(prior.Pointers())
	default:
		return sessionwatcher.WorkspaceOpened()
	}
}

// noteTranscriptSelected records that the workspace was just pointed at a
// DIFFERENT conversation: the previous watcher's pointers name another book,
// and the next watcher replays the selected transcript's first page.
func (f *Fleet) noteTranscriptSelected(ws ids.WorkspaceID) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.selected[ws] = true
	delete(f.watched, ws)
}

// forgetPointers drops the previous watcher's pointers because the session
// coming up is a NEW conversation: a fresh start writes a new book, and a
// pointer names a line of the book it was served from, so resuming from it
// would be a stale pointer the store refuses. The new book holds nothing the
// views have drawn, so its first page replays nothing.
func (f *Fleet) forgetPointers(ws ids.WorkspaceID) {
	f.mu.Lock()
	defer f.mu.Unlock()
	delete(f.watched, ws)
}

// startWatcher opens a workspace's watch fleet with the opening openingFor
// decides, and remembers the watcher so its successor can resume from it. It
// is the ONE way the fleet starts a watcher. The caller states the session's
// facts and what an adoption found open; the opening is always decided here.
//
// Its context is DETACHED from the caller's: the watch fleet outlives the verb
// that brought the session up, and every stream it opens -- now and on every
// redial -- is opened against this context.
func (f *Fleet) startWatcher(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, client shimclient.Client, session sessionwatcher.Session) (sessionwatcher.Watcher, error) {
	opening := f.openingFor(ws)
	session.Opening = opening
	watcher, err := f.watch(context.WithoutCancel(ctx), ws, client, session, f.deps.Sinks, log)
	if err != nil {
		return nil, err
	}
	// THE SELECTION IS SPENT ONLY BY A WATCHER THAT STARTED: a failed start
	// still owes the selected transcript its first page on the next attempt.
	f.mu.Lock()
	delete(f.selected, ws)
	f.watched[ws] = watcher
	f.mu.Unlock()
	log.Info(opBringUp, "opened the session's watches", dlog.Context{
		"opening": opening.String(), "replays_history": opening.Replays(),
	})
	return watcher, nil
}
