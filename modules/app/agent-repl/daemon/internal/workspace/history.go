package workspace

import (
	"context"
	"errors"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/feed"
	"agent-repld/internal/sessionwatcher"
	"agent-repld/internal/shimclient"
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
// A WORKSPACE WITH NO RECORDED BOOK is read through its shim as before: one
// whose shim has not answered a start since the book was first recorded.
// Every such read is recorded at INFO, so the fallback's retirement is
// evidenced once every workspace has started under a build that records it.

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

// historyReader is the one shim call a read through a shim needs.
type historyReader interface {
	ReadHistory(ctx context.Context, req *shimv1.ReadHistoryRequest) (*shimv1.ReadHistoryResponse, error)
}

// ReadHistory reads TARGET's newest page (AFTER nil) or the page before AFTER,
// TARGET nil being the workspace's own book.
//
// A FRESH START'S BOOK IS NO SOURCE until its session starts: the book on
// record names the conversation the fresh one replaces, and the new session's
// answer records its own.
func (f *Fleet) ReadHistory(ctx context.Context, ws ids.WorkspaceID, target *conversationv1.AgentId, after *conversationv1.HistoryPointer) (*conversationv1.HistoryPage, error) {
	if f.freshStartPending(ws) {
		return nil, fmt.Errorf("%w: the workspace's fresh session has not started, and its recorded book names the conversation it replaces", feed.ErrNoHistorySource)
	}
	book, stated, recorded, err := f.historyBook(ctx, ws, target)
	if err != nil {
		return nil, err
	}
	if !recorded {
		f.deps.Log.Global().Info("daemon.workspace.history_through_shim",
			"the workspace has no recorded book yet; its history is read through its shim until a start records it",
			dlog.Context{"workspace": string(ws)})
		return f.readHistoryThroughShim(ctx, ws, target, after)
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

// readHistoryThroughShim reads through the workspace's shim, for a workspace
// with no recorded book.
//
// THE SHIM SERVES THE WORKSPACE'S PERSISTED BOOK whether or not a session was
// started on it, so any shim client the fleet can reach reads history: a
// started session's, one parked at its cold gate, one whose start is being
// answered or retried, a failed start's. Only a workspace with no shim at all
// answers feed.ErrNoHistorySource.
func (f *Fleet) readHistoryThroughShim(ctx context.Context, ws ids.WorkspaceID, target *conversationv1.AgentId, after *conversationv1.HistoryPointer) (*conversationv1.HistoryPage, error) {
	client, ok := f.historyClient(ws)
	if !ok {
		f.restoreParked(ws)
		return nil, feed.ErrNoHistorySource
	}
	page, err := readHistory(ctx, client, target, after)
	_, watched := f.sessionWatcher(ws)
	if errors.Is(err, shimclient.ErrStandDownOrdered) {
		return nil, f.readEndedInStandDown(ws, err)
	}
	if err != nil {
		if !watched && target == nil && isUnknownAgent(err) {
			// NO BOOK YET IS NO SOURCE YET: a shim with no session settled
			// serves the workspace's persisted book, and a workspace that
			// never ran one has none. The reader is served what the feed
			// holds, and its newest page is loaded when the session's watch
			// opens (the feed's kick), exactly as for a workspace with no shim.
			f.deps.Log.Global().Info("daemon.workspace.history_no_book",
				"the workspace's shim holds no book yet and no session is up; the reader waits for the session",
				dlog.Context{"workspace": string(ws), "cause": err.Error()})
			return nil, fmt.Errorf("%w: %w", feed.ErrNoHistorySource, err)
		}
		return nil, err
	}
	f.notePage(ws, target, page, after == nil)
	return page, nil
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

// historyClient answers the shim client history is read through: the one the
// fleet holds, whatever the vendor session is doing. The fleet holds every shim
// from its spawn, so a start being answered or retried, a failed start's shim
// and a cold gate's all serve the workspace's book.
//
// A FRESH START'S SHIM IS NO SOURCE until its session starts: the book it
// persisted names the conversation the fresh one replaces.
func (f *Fleet) historyClient(ws ids.WorkspaceID) (historyReader, bool) {
	client, ok := f.Client(ws)
	if !ok {
		return nil, false
	}
	f.mu.RLock()
	defer f.mu.RUnlock()
	if session, held := f.sessions[ws]; held && session.freshStart {
		return nil, false
	}
	return client, true
}

// isUnknownAgent reports a ReadHistory the shim refused unknown_agent.
func isUnknownAgent(err error) bool {
	var refusal *ShimRefusal
	return errors.As(err, &refusal) && refusal.Arm == "unknown_agent"
}

// readHistory makes the one ReadHistory call and unwraps its outcome.
func readHistory(ctx context.Context, client historyReader, target *conversationv1.AgentId, after *conversationv1.HistoryPointer) (*conversationv1.HistoryPage, error) {
	req := &shimv1.ReadHistoryRequest{Target: target}
	if after != nil {
		req.Position = &shimv1.ReadHistoryRequest_After{After: after}
	} else {
		req.Position = &shimv1.ReadHistoryRequest_First{First: &shimv1.ReadHistoryFirst{}}
	}
	return readHistoryPage(ctx, client, req)
}

// readHistoryPage makes one ReadHistory call and unwraps its outcome: the one
// unwrapping ReadHistory's own reads and a fork's inherited reads share.
func readHistoryPage(ctx context.Context, client historyReader, req *shimv1.ReadHistoryRequest) (*conversationv1.HistoryPage, error) {
	response, err := client.ReadHistory(ctx, req)
	if err != nil {
		return nil, err
	}
	if failure := response.GetFailure(); failure != nil {
		return nil, &ShimRefusal{Verb: "ReadHistory", Arm: readHistoryArm(failure), Detail: failure.GetDetail()}
	}
	success := response.GetSuccess()
	if success == nil || success.GetPage() == nil {
		return nil, fmt.Errorf("shim ReadHistory answered neither a page nor a failure for agent %q", req.GetTarget().GetValue())
	}
	return success.GetPage(), nil
}

// readHistoryArm names a ReadHistory refusal's arm.
func readHistoryArm(failure *shimv1.ReadHistoryFailure) string {
	switch {
	case failure.GetUnknownAgent() != nil:
		return "unknown_agent"
	case failure.GetStalePointer() != nil:
		return "stale_pointer"
	case failure.GetStoreUnavailable() != nil:
		return "store_unavailable"
	default:
		return ArmShimUnspecified
	}
}

// restoreParked asks for WS's session back when the keep-alive cycle parked it:
// a reader wants history only a shim serves. The keeper judges whether it is
// parked and brings it back a few at a time; the reader waits for the session
// exactly as for a workspace with no shim.
func (f *Fleet) restoreParked(ws ids.WorkspaceID) {
	if f.deps.RestoreParked != nil {
		f.deps.RestoreParked(ws)
	}
}
