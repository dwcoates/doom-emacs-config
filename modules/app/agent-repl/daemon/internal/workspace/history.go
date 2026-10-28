package workspace

import (
	"context"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/feed"
	"agent-repld/internal/sessionwatcher"
)

// THE FEED'S HISTORY SOURCE (feed.HistorySource). History is loaded only on a
// reader's request (feed paging on demand): the feed asks the fleet for one
// store page of one agent, the fleet reads it from the store's read-only face,
// and hands it to the session watcher's views before the feed draws it, so a
// loaded page names the main agent and reconciles the footer exactly as a
// watch's opening page used to.
//
// THE SOURCE IS THE STORE, NEVER A SHIM (owner ruling, 2026-10-27: a shim runs
// only when a session must). The workspace's book -- the store agent its
// conversation is filed under, which its shim names on every StartSession
// answer (recordBook) -- is read from the store whether or not any shim is up:
// a parked workspace, one at its cold gate, one whose shim is starting or gone
// all show their conversation, and none is started to show it. The daemon's
// relationship with the store is read-only (storereader).
//
// A WORKSPACE WITH NO RECORDED BOOK takes the book its shim persisted
// (shimidentity), records it, and is read from the store like any other: one
// whose shim has not answered a start since the book was first recorded, or
// whose record could not be written. One whose shim persisted none has no
// conversation yet, and its newest page is empty, at its floor.

// StoreHistory is the store's read-only face as the fleet reads it
// (storereader.Reader).
type StoreHistory interface {
	// Newest reads BOOK's newest page; STATED is a recorded book, which the
	// store serves empty when nothing is written under it yet.
	Newest(ctx context.Context, book string, stated bool) (*conversationv1.HistoryPage, error)
	// Before reads the page of BOOK placed strictly before AFTER's line.
	Before(ctx context.Context, book string, after *conversationv1.HistoryPointer) (*conversationv1.HistoryPage, error)
	// Through reads BOOK as it stood at THROUGHMS.
	Through(ctx context.Context, book string, throughMs int64) (*conversationv1.HistoryPage, error)
}

// ReadHistory reads TARGET's newest page (AFTER nil) or the page before AFTER,
// TARGET nil being the workspace's own book.
//
// A FRESH START'S NEWEST PAGE IS EMPTY until its session starts: the book on
// record names the conversation the fresh one replaces, and the new session's
// answer records its own.
func (f *Fleet) ReadHistory(ctx context.Context, ws ids.WorkspaceID, target *conversationv1.AgentId, after *conversationv1.HistoryPointer) (*conversationv1.HistoryPage, error) {
	if after == nil && target == nil && f.freshStartPending(ws) {
		f.deps.Log.Global().Debug("daemon.workspace.history_fresh",
			"the workspace's fresh session has not started; its newest page is empty",
			dlog.Context{"workspace": string(ws)})
		return emptyHistory(), nil
	}
	book, stated, recorded, err := f.historyBook(ctx, ws, target)
	if err != nil {
		return nil, err
	}
	if !recorded {
		book, recorded, err = f.adoptPersistedBook(ctx, ws)
		if err != nil {
			return nil, err
		}
		if !recorded {
			return emptyHistory(), nil
		}
		stated = true
	}
	var page *conversationv1.HistoryPage
	if after != nil {
		page, err = f.deps.StoreHistory.Before(ctx, book, after)
	} else {
		page, err = f.deps.StoreHistory.Newest(ctx, book, stated)
	}
	if err != nil {
		return nil, fmt.Errorf("read the history of book %q from the store: %w", book, err)
	}
	f.notePage(ws, target, page, after == nil)
	return page, nil
}

// emptyHistory is the newest page of a conversation that holds nothing yet:
// no entries, at its floor.
func emptyHistory() *conversationv1.HistoryPage {
	return &conversationv1.HistoryPage{Boundary: &conversationv1.HistoryPage_Floor{Floor: &conversationv1.HistoryFloor{}}}
}

// adoptPersistedBook answers the book WS's shim persisted, for a workspace
// with no recorded book, and records it. FOUND false is a workspace whose
// shim persisted none: it has no conversation yet. A record that cannot be
// written is an ERROR and the read goes on with the book: the next read
// adopts it again.
func (f *Fleet) adoptPersistedBook(ctx context.Context, ws ids.WorkspaceID) (string, bool, error) {
	log := f.logFor(ctx, ws)
	record, err := f.deps.DB.Workspace(ctx, ws)
	if err != nil {
		log.Error("daemon.workspace.history_book", "could not read the workspace's record to find the book its shim persisted; its history is not read",
			dlog.Context{"cause": err.Error()})
		return "", false, fmt.Errorf("read the record of %q: %w", ws, err)
	}
	book, found, err := f.deps.PersistedBook(record.Dir)
	if err != nil {
		log.Error("daemon.workspace.history_book", "could not read the book the workspace's shim persisted; its history is not read",
			dlog.Context{"cause": err.Error()})
		return "", false, fmt.Errorf("read the book %q's shim persisted: %w", ws, err)
	}
	if !found {
		log.Debug("daemon.workspace.history_book", "the workspace has no recorded book and its shim persisted none; it has no conversation yet", nil)
		return "", false, nil
	}
	if err := f.deps.DB.SetWorkspaceBook(ctx, ws, book); err != nil {
		log.Error("daemon.workspace.history_book", "could not record the book the workspace's shim persisted; it is read with it, and the next read adopts it again",
			dlog.Context{"book": book, "cause": err.Error()})
		return book, true, nil
	}
	log.Info("daemon.workspace.history_book", "recorded the book the workspace's shim persisted", dlog.Context{"book": book})
	return book, true, nil
}

// historyBook answers the book a read of TARGET reads: TARGET itself, or the
// workspace's recorded book when TARGET is nil, and whether it is STATED (the
// workspace's own recorded book, which a shim named). RECORDED is false for a
// workspace's own book when none is on record.
func (f *Fleet) historyBook(ctx context.Context, ws ids.WorkspaceID, target *conversationv1.AgentId) (book string, stated, recorded bool, err error) {
	own, has, err := f.deps.DB.WorkspaceBook(ctx, ws)
	if err != nil {
		f.deps.Log.Global().Error("daemon.workspace.history_book",
			"could not read the workspace's recorded book; its history is not read",
			dlog.Context{"workspace": string(ws), "cause": err.Error()})
		return "", false, false, fmt.Errorf("read the recorded book of %q: %w", ws, err)
	}
	if target != nil {
		return target.GetValue(), has && own == target.GetValue(), true, nil
	}
	return own, has, has, nil
}

// freshStartPending reports a fresh start whose session has not started.
func (f *Fleet) freshStartPending(ws ids.WorkspaceID) bool {
	f.mu.RLock()
	defer f.mu.RUnlock()
	session, held := f.sessions[ws]
	return held && session.freshStart
}

// notePage hands a loaded page to the workspace's views: its session watcher's
// when one is up, the views directly otherwise.
func (f *Fleet) notePage(ws ids.WorkspaceID, target *conversationv1.AgentId, page *conversationv1.HistoryPage, newest bool) {
	if watcher, watched := f.sessionWatcher(ws); watched {
		watcher.NoteHistoryLoaded(target, page, newest)
		return
	}
	f.noteHistoryWithoutWatcher(ws, target, page)
}

// ReadInherited reads one page of BOOK — a generation of a fork's inherited
// past — from the store: from THROUGHMS when AFTER is nil, the page before
// AFTER otherwise. Unlike ReadHistory it tells the session's views nothing:
// the page is the parent's past, and naming its agent as WS's main agent, or
// reconciling WS's footer from it, would state a fact of the parent's session
// as the fork's.
func (f *Fleet) ReadInherited(ctx context.Context, ws ids.WorkspaceID, book *conversationv1.AgentId, after *conversationv1.HistoryPointer, throughMs int64) (*conversationv1.HistoryPage, error) {
	var page *conversationv1.HistoryPage
	var err error
	if after != nil {
		page, err = f.deps.StoreHistory.Before(ctx, book.GetValue(), after)
	} else {
		page, err = f.deps.StoreHistory.Through(ctx, book.GetValue(), throughMs)
	}
	if err != nil {
		return nil, fmt.Errorf("read %s's inherited book %q from the store: %w", ws, book.GetValue(), err)
	}
	return page, nil
}

// readEndedInStandDown answers a history read that a stand-down THIS DAEMON
// ORDERED cut short (an idle park, a bounce) as no source, recorded at INFO.
// It is the same condition as a workspace with no shim at all, met while the
// read was in flight rather than before it: the shim it was reading from is
// going away on purpose, so the feed serves what it holds and loads the page
// when the next shim comes up, exactly as it does for a hibernated workspace.
func (f *Fleet) readEndedInStandDown(ws ids.WorkspaceID, err error) error {
	f.deps.Log.Global().Info("daemon.workspace.history_stood_down",
		"a history read ended in a stand-down this daemon ordered; the reader waits for the next shim",
		dlog.Context{"workspace": string(ws), "cause": err.Error()})
	return fmt.Errorf("%w: %w", feed.ErrNoHistorySource, err)
}

// noteHistoryWithoutWatcher states for the views what a loaded page states when
// no session watcher is up to take it (sessionwatcher.NoteHistoryLoaded): the
// main agent a root page names, so the feed places its rows on the root, and
// the page itself for the footer. A watcher that opens later names the same
// agent and is told nothing twice: its watch opens on what is written after.
func (f *Fleet) noteHistoryWithoutWatcher(ws ids.WorkspaceID, target *conversationv1.AgentId, page *conversationv1.HistoryPage) {
	agent := target
	if target == nil {
		agent = sessionwatcher.PageAgent(page)
		if agent != nil {
			f.deps.Feed.OnMainAgent(ws, agent)
			f.deps.Footer.OnMainAgent(ws, agent)
		}
	}
	f.deps.Log.Global().Info("daemon.workspace.history_without_session",
		"a history page was read with no session watcher up; the views were told its main agent",
		dlog.Context{"workspace": string(ws), "agent_id": agent.GetValue(), "entries": len(page.GetEntries()), "targeted": target != nil})
	f.deps.Footer.OnHistoryPage(ws, agent, page)
}
