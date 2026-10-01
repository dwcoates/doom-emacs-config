package promptqueue

import (
	"context"
	"path/filepath"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/feed"
	"claude-repld/internal/wsm"
)

// THE ONE-DOOR INVARIANT: every turn that opens gets exactly one ending row in
// the feed, whichever path closes it, live and on replay. The table walks EVERY
// close path through the real queue, the real durable store and the real feed
// resolver; each path's feed arrangement is what the watcher tells the feed
// before the queue hears of the end.

// doorWorld is one close path's world: a real store with one workspace and one
// open turn, a real feed that saw the turn opened, and the queue over both.
type doorWorld struct {
	h    *harness
	db   wsm.DB
	feed feed.Resolver
	ws   ids.WorkspaceID
	turn ids.TurnID
}

// doorFeed builds a real feed resolver reading its replayed closes from DB.
func doorFeed(t *testing.T, db wsm.DB) feed.Resolver {
	t.Helper()
	resolver, err := feed.New(feed.Deps{
		Log:          dlog.NewTestSurfaces(),
		WorkspaceDir: func(ids.WorkspaceID) (string, error) { return t.TempDir(), nil },
		ResolveImage: func(*conversationv1.ImageBlock) (string, string, error) { return "", "", nil },
		TurnCloses: func(ctx context.Context, ws ids.WorkspaceID, turns []ids.TurnID) (map[ids.TurnID]wsm.RecordedClose, error) {
			return db.TurnCloses(ctx, ws, turns)
		},
	})
	if err != nil {
		t.Fatalf("feed.New: %v", err)
	}
	return resolver
}

func doorAgent() *conversationv1.AgentId { return &conversationv1.AgentId{Value: "agent-main"} }

func newDoorWorld(t *testing.T) *doorWorld {
	t.Helper()
	ctx := context.Background()
	db, err := wsm.Open(ctx, filepath.Join(t.TempDir(), "wsm.db"))
	if err != nil {
		t.Fatalf("wsm.Open: %v", err)
	}
	t.Cleanup(func() { db.Close() })
	dir := t.TempDir()
	record, _, err := db.RegisterWorkspace(ctx, dir, wsm.RegisterFacts{Name: "door", Branch: "feature", ParentBranch: "master", RepoDir: dir})
	if err != nil {
		t.Fatalf("RegisterWorkspace: %v", err)
	}
	turn := wsm.NewTurnID()
	if err := db.PutTurn(ctx, wsm.Turn{ID: turn, Workspace: record.ID, Origin: "webapp", StartedAt: time.UnixMilli(1_700_000_000_000)}); err != nil {
		t.Fatalf("PutTurn: %v", err)
	}
	h := newHarness(t)
	resolver := doorFeed(t, db)
	deps := h.q.deps
	deps.DB = db
	deps.Feed = resolver
	q, err := newQueue(deps)
	if err != nil {
		t.Fatalf("newQueue: %v", err)
	}
	h.q = q
	resolver.OnMainAgent(record.ID, doorAgent())
	resolver.OnTurnOpened(record.ID, turn)
	return &doorWorld{h: h, db: db, feed: resolver, ws: record.ID, turn: turn}
}

// endings counts the turn's turn_ended rows on the root feed of RESOLVER.
func (w *doorWorld) endings(t *testing.T, resolver feed.Resolver) int {
	t.Helper()
	page, _, err := resolver.OpenPage(context.Background(), w.ws, feedid.Feed{Root: true}, feed.ReaderID("reader"))
	if err != nil {
		t.Fatalf("OpenPage: %v", err)
	}
	success, ok := page.GetResult().(*frontendv1.FeedPage_Success)
	if !ok {
		t.Fatalf("page = %T, want a success", page.GetResult())
	}
	n := 0
	for _, row := range success.Success.GetRows() {
		if row.GetTurnEnded() != nil && row.GetTurn().GetValue() == string(w.turn) {
			n++
		}
	}
	return n
}

// terminal routes the turn's own vendor terminal to the feed, as the watcher
// does before the queue hears of the end.
func (w *doorWorld) terminal(success *conversationv1.AgentSuccess, failure *conversationv1.AgentFailure) {
	turn := w.turn
	w.feed.OnAgentTerminal(w.ws, doorAgent(), &turn, success, failure, nil)
}

// replayPage is the turn's page as a fresh daemon replays it: its prompt, and
// the terminal the store carries when the path had one.
func (w *doorWorld) replayPage(stored *conversationv1.AgentFrame) *conversationv1.HistoryPage {
	entries := []*conversationv1.HistoryEntryAt{}
	if stored != nil {
		stored.AgentId = doorAgent()
		entries = append(entries, &conversationv1.HistoryEntryAt{
			At:    &conversationv1.HistoryPointer{Value: "b"},
			Turn:  &conversationv1.TurnId{Value: string(w.turn)},
			Entry: &conversationv1.HistoryEntry{Entry: &conversationv1.HistoryEntry_AgentFrame{AgentFrame: stored}},
		})
	}
	entries = append(entries, &conversationv1.HistoryEntryAt{
		At: &conversationv1.HistoryPointer{Value: "a"},
		Entry: &conversationv1.HistoryEntry{Entry: &conversationv1.HistoryEntry_UserPrompt{UserPrompt: &conversationv1.AgentPrompt{
			Id:     &conversationv1.TurnId{Value: string(w.turn)},
			Agent:  doorAgent(),
			Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
			Said: &conversationv1.UserSaid{Content: &conversationv1.UserContent{Blocks: []*conversationv1.UserContentBlock{{
				Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: "hello"}},
			}}}},
		}}},
	})
	return &conversationv1.HistoryPage{Entries: entries, Boundary: &conversationv1.HistoryPage_Floor{Floor: &conversationv1.HistoryFloor{}}}
}

var (
	completed   = &conversationv1.AgentSuccess{Outcome: &conversationv1.AgentSuccess_Completed{Completed: &conversationv1.AgentCompleted{}}}
	interrupted = &conversationv1.AgentSuccess{Outcome: &conversationv1.AgentSuccess_Interrupted{Interrupted: &conversationv1.AgentInterrupted{}}}
	apiFailed   = &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ApiRequestFailed{ApiRequestFailed: &conversationv1.ApiRequestFailed{
		Kind: &conversationv1.ApiRequestFailed_Internal{Internal: &conversationv1.ApiInternal{}},
	}}}
	eof        = &conversationv1.SessionQueryDied{Cause: &conversationv1.SessionQueryDied_UnexpectedEof{UnexpectedEof: &conversationv1.SessionQueryUnexpectedEof{}}}
	queryDeath = &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_QueryDied{QueryDied: eof}}
)

// closePath is one way a turn's row closes: what the watcher tells the feed
// first (live), how the queue is told, and what the store carries for the turn
// when a fresh daemon replays it.
type closePath struct {
	name   string
	live   func(t *testing.T, w *doorWorld)
	stored *conversationv1.AgentFrame
}

// closePaths is EVERY close path. A new one belongs here.
var closePaths = []closePath{
	{
		name: "the vendor terminal concludes the turn",
		live: func(t *testing.T, w *doorWorld) {
			w.terminal(completed, nil)
			w.h.q.OnTurnEnded(w.ws, w.turn, wsm.CloseCompleted)
		},
		stored: &conversationv1.AgentFrame{Result: &conversationv1.AgentFrame_Success{Success: completed}},
	},
	{
		name: "the vendor terminal fails the turn",
		live: func(t *testing.T, w *doorWorld) {
			w.terminal(nil, apiFailed)
			w.h.q.OnTurnEnded(w.ws, w.turn, wsm.CloseFailed)
		},
		stored: &conversationv1.AgentFrame{Result: &conversationv1.AgentFrame_Failure{Failure: apiFailed}},
	},
	{
		name: "an interrupt stops the turn",
		live: func(t *testing.T, w *doorWorld) {
			w.terminal(interrupted, nil)
			w.h.q.OnTurnEnded(w.ws, w.turn, wsm.CloseKilled)
		},
		stored: &conversationv1.AgentFrame{Result: &conversationv1.AgentFrame_Success{Success: interrupted}},
	},
	{
		name: "the query dies under the turn",
		live: func(t *testing.T, w *doorWorld) {
			w.feed.OnSessionUpdate(w.ws, &conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_QueryDied{QueryDied: eof}})
			w.h.q.OnTurnEnded(w.ws, w.turn, wsm.CloseFailed)
		},
		stored: &conversationv1.AgentFrame{Result: &conversationv1.AgentFrame_Failure{Failure: queryDeath}},
	},
	{
		name: "the agent process dies on its own under the turn",
		live: func(t *testing.T, w *doorWorld) {
			w.h.q.OnTurnEnded(w.ws, w.turn, wsm.CloseAgentDied)
		},
	},
	{
		name: "an adoption finds the turn ended while no daemon watched",
		live: func(t *testing.T, w *doorWorld) {
			w.h.q.OnTurnsEndedUnobserved(w.ws, []ids.TurnID{w.turn})
		},
	},
	{
		name: "the boot's hold restore closes a sessionless workspace's turn",
		live: func(t *testing.T, w *doorWorld) {
			w.h.noSession = true
			if err := w.h.q.RestoreHolds(context.Background()); err != nil {
				t.Fatalf("RestoreHolds: %v", err)
			}
		},
	},
	{
		name: "a boot or a teardown closes the workspace's orphans",
		live: func(t *testing.T, w *doorWorld) {
			if _, err := w.h.q.CloseOrphans(context.Background(), w.ws, time.UnixMilli(1_700_000_100_000)); err != nil {
				t.Fatalf("CloseOrphans: %v", err)
			}
		},
	},
	{
		name: "a handover's or a merge's displaced turn is claimed still open",
		live: func(t *testing.T, w *doorWorld) {
			ctx := context.Background()
			if err := w.db.PutTurn(ctx, wsm.Turn{ID: w.turn, Workspace: w.ws, Origin: "webapp", Displaced: true, StartedAt: time.UnixMilli(1_700_000_000_000)}); err != nil {
				t.Fatalf("PutTurn: %v", err)
			}
			if claimed, err := w.h.q.ClaimDisplacedTurn(ctx, w.ws, w.turn); err != nil || !claimed {
				t.Fatalf("ClaimDisplacedTurn = (%v, %v), want (true, nil)", claimed, err)
			}
		},
	},
}

func TestEveryClosePathDrawsExactlyOneEndingLive(t *testing.T) {
	for _, path := range closePaths {
		t.Run(path.name, func(t *testing.T) {
			// Arrange
			w := newDoorWorld(t)

			// Act
			path.live(t, w)

			// Assert
			if n := w.endings(t, w.feed); n != 1 {
				t.Fatalf("live ending rows = %d, want exactly 1", n)
			}
		})
	}
}

func TestEveryClosePathDrawsExactlyOneEndingOnReplay(t *testing.T) {
	for _, path := range closePaths {
		t.Run(path.name, func(t *testing.T) {
			// Arrange: the path ran under one daemon.
			w := newDoorWorld(t)
			path.live(t, w)
			fresh := doorFeed(t, w.db)
			fresh.OnMainAgent(w.ws, doorAgent())

			// Act: a fresh daemon replays the turn.
			fresh.OnHistoryPage(w.ws, doorAgent(), w.replayPage(path.stored))

			// Assert
			if n := w.endings(t, fresh); n != 1 {
				t.Fatalf("replayed ending rows = %d, want exactly 1", n)
			}
		})
	}
}

func TestEveryClosePathClosesTheDurableRow(t *testing.T) {
	for _, path := range closePaths {
		t.Run(path.name, func(t *testing.T) {
			// Arrange
			w := newDoorWorld(t)

			// Act
			path.live(t, w)

			// Assert
			open, err := w.db.OpenTurns(context.Background(), w.ws)
			if err != nil {
				t.Fatalf("OpenTurns: %v", err)
			}
			if len(open) != 0 {
				t.Fatalf("open turns = %+v, want the row closed", open)
			}
		})
	}
}
