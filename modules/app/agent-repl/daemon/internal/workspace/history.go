package workspace

import (
	"context"
	"fmt"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/feed"
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/shimclient"
)

// THE FEED'S HISTORY SOURCE (feed.HistorySource). History is loaded only on a
// reader's request (feed paging on demand): the feed asks the fleet for one
// store page of one agent, the fleet reads it from the workspace's shim, and
// hands it to the session watcher's views before the feed draws it, so a
// loaded page names the main agent and reconciles the footer exactly as a
// watch's opening page used to.

// historyReader is the one shim call ReadHistory needs.
type historyReader interface {
	ReadHistory(ctx context.Context, req *shimv1.ReadHistoryRequest) (*shimv1.ReadHistoryResponse, error)
}

// ReadHistory reads TARGET's newest page (AFTER nil) or the page before AFTER
// from the workspace's shim.
//
// THE SOURCE IS THE SHIM, NEVER THE VENDOR SESSION (owner principle,
// 2026-10-02: the vendor and its state never gate agent-repl's own functions).
// The shim serves the workspace's persisted book whether or not a session was
// started on it, so any shim client the fleet can reach reads history: the one
// it holds (a started session, a session parked at its cold gate) or the one a
// running vendor start has not handed over yet (startingClients). Only a
// workspace with no shim at all answers feed.ErrNoHistorySource.
func (f *Fleet) ReadHistory(ctx context.Context, ws ids.WorkspaceID, target *conversationv1.AgentId, after *conversationv1.HistoryPointer) (*conversationv1.HistoryPage, error) {
	client, ok := f.historyClient(ws)
	if !ok {
		return nil, feed.ErrNoHistorySource
	}
	page, err := readHistory(ctx, client, target, after)
	if err != nil {
		return nil, err
	}
	if watcher, ok := f.sessionWatcher(ws); ok {
		watcher.NoteHistoryLoaded(target, page, after == nil)
		return page, nil
	}
	f.noteHistoryWithoutWatcher(ws, target, page)
	return page, nil
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
		"a history page was read from the workspace's shim with no session watcher up; the views were told its main agent",
		dlog.Context{"workspace": string(ws), "agent_id": agent.GetValue(), "entries": len(page.GetEntries()), "targeted": target != nil})
	f.deps.Footer.OnHistoryPage(ws, agent, page)
}

// historyClient answers the shim client history is read through: the held
// one, else the one a running vendor start holds.
func (f *Fleet) historyClient(ws ids.WorkspaceID) (historyReader, bool) {
	if client, ok := f.Client(ws); ok {
		return client, true
	}
	f.mu.RLock()
	defer f.mu.RUnlock()
	client, ok := f.startingClients[ws]
	if !ok {
		return nil, false
	}
	if _, reaped := client.Reaped(); reaped {
		return nil, false
	}
	return client, true
}

// beginHistoryClient makes a running vendor start's CLIENT the workspace's
// history source until the answer: the fleet holds no client while
// StartSession is answered or retried, and the shim reads the workspace's book
// all the same. The feed is told a source is up, so a reader that opened
// before it has the newest page pushed. The returned func withdraws it.
func (f *Fleet) beginHistoryClient(ws ids.WorkspaceID, client shimclient.Client) func() {
	f.mu.Lock()
	f.startingClients[ws] = client
	f.mu.Unlock()
	f.deps.Feed.SourceUp(ws)
	return func() {
		f.mu.Lock()
		defer f.mu.Unlock()
		if f.startingClients[ws] == client {
			delete(f.startingClients, ws)
		}
	}
}

// keepNewestPageBound bounds the one newest-page read a failed start makes
// before its shim is stopped. The shim answers it off the store in
// milliseconds; it is sized like the feed's own kick bound's half, because the
// failed start's caller is waiting on it. An overrun is ERROR and the start's
// own error is returned unchanged.
const keepNewestPageBound = 5 * time.Second

// keepNewestPage secures the root feed's newest page through a FAILED start's
// client before that shim is stopped: no other shim will be up to read it from,
// and a reader that opens next is owed the conversation, not only the rows the
// daemon made itself. A failure is recorded and never changes the start's own
// error.
func (f *Fleet) keepNewestPage(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID) {
	if ctx.Err() != nil {
		log.Info(opBringUp, "the conversation's newest page was not secured: the bring-up's context ended with the failed start",
			dlog.Context{"cause": ctx.Err().Error()})
		return
	}
	readCtx, cancel := context.WithTimeout(ctx, keepNewestPageBound)
	defer cancel()
	if err := f.deps.Feed.KeepNewestPage(readCtx, ws); err != nil {
		log.Error(opBringUp, "the conversation's newest page could not be secured before the failed start's shim was stopped",
			dlog.Context{"cause": err.Error()})
	}
}

// readHistory makes the one ReadHistory call and unwraps its outcome.
func readHistory(ctx context.Context, client historyReader, target *conversationv1.AgentId, after *conversationv1.HistoryPointer) (*conversationv1.HistoryPage, error) {
	req := &shimv1.ReadHistoryRequest{Target: target}
	if after != nil {
		req.Position = &shimv1.ReadHistoryRequest_After{After: after}
	} else {
		req.Position = &shimv1.ReadHistoryRequest_First{First: &shimv1.ReadHistoryFirst{}}
	}
	response, err := client.ReadHistory(ctx, req)
	if err != nil {
		return nil, err
	}
	if failure := response.GetFailure(); failure != nil {
		return nil, &ShimRefusal{Verb: "ReadHistory", Arm: readHistoryArm(failure), Detail: failure.GetDetail()}
	}
	success := response.GetSuccess()
	if success == nil || success.GetPage() == nil {
		return nil, fmt.Errorf("shim ReadHistory answered neither a page nor a failure for agent %q", target.GetValue())
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
