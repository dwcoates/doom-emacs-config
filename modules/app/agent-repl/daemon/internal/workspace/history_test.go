package workspace

import (
	"context"
	"errors"
	"fmt"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/feed"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/storereader"
	"agent-repld/internal/wsm"
)

func pageResponse() *shimv1.ReadHistoryResponse {
	return &shimv1.ReadHistoryResponse{Result: &shimv1.ReadHistoryResponse_Success{Success: &shimv1.ReadHistorySuccess{
		Page: &conversationv1.HistoryPage{Boundary: &conversationv1.HistoryPage_Floor{Floor: &conversationv1.HistoryFloor{}}},
	}}}
}

// ReadHistory answers the scripted page and counts the read.
func (c *fakeClient) ReadHistory(_ context.Context, req *shimv1.ReadHistoryRequest) (*shimv1.ReadHistoryResponse, error) {
	c.historyReads++
	c.historyRequests = append(c.historyRequests, req)
	if c.historyErr != nil {
		return nil, c.historyErr
	}
	if c.history != nil {
		return c.history, nil
	}
	return pageResponse(), nil
}

// promptPageResponse is a root page of one prompt the main agent was sent.
func promptPageResponse(agent string) *shimv1.ReadHistoryResponse {
	return &shimv1.ReadHistoryResponse{Result: &shimv1.ReadHistoryResponse_Success{Success: &shimv1.ReadHistorySuccess{
		Page: &conversationv1.HistoryPage{
			Entries: []*conversationv1.HistoryEntryAt{{
				At:    &conversationv1.HistoryPointer{Value: "p-0"},
				Entry: &conversationv1.HistoryEntry{Entry: &conversationv1.HistoryEntry_UserPrompt{UserPrompt: &conversationv1.AgentPrompt{Agent: &conversationv1.AgentId{Value: agent}}}},
			}},
			Boundary: &conversationv1.HistoryPage_Floor{Floor: &conversationv1.HistoryFloor{}},
		},
	}}}
}

// resumable records W1 with a session to resume, so its bring-up is a RESUME.
func resumable(f *fleetFixture) ids.WorkspaceID {
	ws := f.workspace("w1")
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, VendorSessionID: "vendor-1"}
	return ws.ID
}

// heldWithoutSession brings W1 up to a shim held with no session and no
// watcher: a start the vendor rejected, which leaves its shim held for the
// next start to reuse.
func heldWithoutSession(t *testing.T, f *fleetFixture) ids.WorkspaceID {
	t.Helper()
	ws := resumable(f)
	f.client.response = vendorRefusal(rejectedVendorStart(), "invalid api key")
	_ = f.fleet.Start(context.Background(), ws)
	if !f.fleet.Held(ws) {
		t.Fatal("the rejected start left no shim held")
	}
	return ws
}

// parkAtColdGate brings W1 up to a standing cold gate: its shim is stood down,
// so no shim, session or watcher is up.
func parkAtColdGate(t *testing.T, f *fleetFixture) ids.WorkspaceID {
	t.Helper()
	ws := resumable(f)
	f.client.response = coldResponse()
	if err := f.fleet.Start(context.Background(), ws); err != nil {
		t.Fatalf("Start: %v", err)
	}
	return ws
}

// assertEmptyHistory requires PAGE to be the newest page of a conversation
// that holds nothing: no entries, at its floor.
func assertEmptyHistory(t *testing.T, page *conversationv1.HistoryPage, err error) {
	t.Helper()
	if err != nil || page == nil || len(page.GetEntries()) != 0 || page.GetFloor() == nil {
		t.Fatalf("ReadHistory = %v, %v; want an empty page at its floor", page, err)
	}
}

func TestFleetReadHistoryOfAWorkspaceWithNoConversationIsEmpty(t *testing.T) {
	// Arrange: no book recorded, and its shim persisted none.
	f := newFleetFixture(t)
	ws := f.workspace("w1")

	// Act.
	page, err := f.fleet.ReadHistory(context.Background(), ws.ID, nil, nil)

	// Assert.
	assertEmptyHistory(t, page, err)
	if len(f.store.asked) != 0 || len(f.supervisor.spawns) != 0 {
		t.Fatalf("store asked %v, spawns %d; want neither", f.store.asked, len(f.supervisor.spawns))
	}
}

func TestFleetReadHistoryOfAColdGatedWorkspaceReadsTheStoreAndSpawnsNothing(t *testing.T) {
	// Arrange: the gate's shim is stood down, and its book is on record.
	f := newFleetFixture(t)
	ws := resumable(f)
	f.client.response = withBook(coldResponse(), "book-1")
	if err := f.fleet.Start(context.Background(), ws); err != nil {
		t.Fatalf("Start: %v", err)
	}
	f.store.page = storePromptPage("book-1")
	spawned := len(f.supervisor.spawns)

	// Act.
	page, err := f.fleet.ReadHistory(context.Background(), ws, nil, nil)

	// Assert.
	if err != nil || page == nil || f.client.historyReads != 0 || len(f.supervisor.spawns) != spawned {
		t.Fatalf("ReadHistory = %v, %v after %d shim reads and %d spawns; want the store's page and no shim", page, err, f.client.historyReads, len(f.supervisor.spawns)-spawned)
	}
}

func TestFleetReadHistoryWithoutAWatcherNamesTheMainAgentForTheFeed(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := recordedBook(f, "main-agent")

	// Act.
	if _, err := f.fleet.ReadHistory(context.Background(), ws, nil, nil); err != nil {
		t.Fatalf("ReadHistory: %v", err)
	}

	// Assert.
	if len(f.feed.mainAgents) != 1 || f.feed.mainAgents[0] != "main-agent" {
		t.Fatalf("feed main agents = %v, want the page's main agent", f.feed.mainAgents)
	}
}

func TestFleetReadHistoryWithoutAWatcherNamesTheMainAgentForTheFooter(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := recordedBook(f, "main-agent")

	// Act.
	if _, err := f.fleet.ReadHistory(context.Background(), ws, nil, nil); err != nil {
		t.Fatalf("ReadHistory: %v", err)
	}

	// Assert.
	if len(f.footer.mainAgents) != 1 || f.footer.mainAgents[0] != "main-agent" {
		t.Fatalf("footer main agents = %v, want the page's main agent", f.footer.mainAgents)
	}
}

func TestFleetReadHistoryWithoutAWatcherHandsTheFooterThePage(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := recordedBook(f, "main-agent")

	// Act.
	if _, err := f.fleet.ReadHistory(context.Background(), ws, nil, nil); err != nil {
		t.Fatalf("ReadHistory: %v", err)
	}

	// Assert.
	if len(f.footer.historyPages) != 1 || f.footer.historyPages[0] != 1 {
		t.Fatalf("footer pages = %v, want the one-entry page", f.footer.historyPages)
	}
}

func TestFleetReadHistoryOfASubagentNamesNoMainAgent(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := recordedBook(f, "main-agent")
	f.store.page = storePromptPage("sub-agent")

	// Act.
	if _, err := f.fleet.ReadHistory(context.Background(), ws, &conversationv1.AgentId{Value: "sub-agent"}, nil); err != nil {
		t.Fatalf("ReadHistory: %v", err)
	}

	// Assert.
	if len(f.feed.mainAgents) != 0 {
		t.Fatalf("feed main agents = %v, want none named from a subagent's book", f.feed.mainAgents)
	}
}

func TestFleetReadHistoryWithAWatcherNamesNothingItself(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}
	f.db.books[ws.ID] = "main-agent"
	f.store.page = storePromptPage("main-agent")

	// Act.
	if _, err := f.fleet.ReadHistory(context.Background(), ws.ID, nil, nil); err != nil {
		t.Fatalf("ReadHistory: %v", err)
	}

	// Assert: the watcher takes the page up (NoteHistoryLoaded).
	if len(f.feed.mainAgents) != 0 || len(f.footer.historyPages) != 0 {
		t.Fatalf("feed named %v, footer pages %v; want the watcher to take the page", f.feed.mainAgents, f.footer.historyPages)
	}
}

func TestFleetReadHistoryReadsAStartBeingRetried(t *testing.T) {
	// Arrange: the first StartSession is refused retryable; the read is made
	// while the run waits to retry.
	f := newFleetFixture(t)
	ws := resumable(f)
	f.db.books[ws] = "book-1"
	f.store.page = storePromptPage("book-1")
	f.client.responses = []*shimv1.StartSessionResponse{vendorRefusal(retryableVendorStart(), "overloaded")}
	var readErr error
	f.retryAfter = func(time.Duration) <-chan time.Time {
		_, readErr = f.fleet.ReadHistory(context.Background(), ws, nil, nil)
		fired := make(chan time.Time, 1)
		fired <- f.now
		return fired
	}

	// Act.
	if err := f.fleet.Start(context.Background(), ws); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	if readErr != nil || len(f.store.asked) != 1 || f.client.historyReads != 0 {
		t.Fatalf("read during the retry = %v after store reads %v and %d shim reads, want the store's page", readErr, f.store.asked, f.client.historyReads)
	}
}

func TestASpawnedShimTellsTheFeedASourceIsUpBeforeItsStartAnswers(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := resumable(f)
	var sourcedAtStart []ids.WorkspaceID
	f.client.onStart = func() { sourcedAtStart = append([]ids.WorkspaceID(nil), f.feed.sourcesUp...) }

	// Act.
	if err := f.fleet.Start(context.Background(), ws); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	if len(sourcedAtStart) != 1 || sourcedAtStart[0] != ws {
		t.Fatalf("sources up while the start ran = %v, want %q", sourcedAtStart, ws)
	}
}

func TestAHeldClientTellsTheFeedASourceIsUp(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)

	// Act.
	ws := parkAtColdGate(t, f)

	// Assert: once, when the shim was held at its spawn; the park restates it.
	if len(f.feed.sourcesUp) != 1 || f.feed.sourcesUp[0] != ws {
		t.Fatalf("sources up = %v, want the held shim's once", f.feed.sourcesUp)
	}
}

func TestAFailedFreshStartsNewestPageIsEmpty(t *testing.T) {
	// Arrange: the held shim's persisted book names the conversation the
	// fresh one replaces.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.client.response = vendorRefusal(rejectedVendorStart(), "invalid api key")
	_ = f.fleet.Start(context.Background(), ws.ID)

	// Act.
	page, err := f.fleet.ReadHistory(context.Background(), ws.ID, nil, nil)

	// Assert.
	assertEmptyHistory(t, page, err)
	if !f.fleet.Held(ws.ID) || len(f.store.asked) != 0 {
		t.Fatalf("held = %t, store asked %v; want the fresh start's shim held and no store read", f.fleet.Held(ws.ID), f.store.asked)
	}
}

func TestFleetReadHistoryOfAnUnknownSubagentBeforeASessionIsARefusal(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := parkAtColdGate(t, f)
	f.store.err = &storereader.Refusal{Arm: storereader.ArmUnknownAgent, Detail: "no such book"}

	// Act.
	_, err := f.fleet.ReadHistory(context.Background(), ws, &conversationv1.AgentId{Value: "sub-agent"}, nil)

	// Assert.
	if errors.Is(err, feed.ErrNoHistorySource) || !storereader.IsUnknownAgent(err) {
		t.Fatalf("err = %v, want the store's unknown_agent refusal", err)
	}
	if len(f.store.asked) != 1 || f.store.asked[0] != "newest sub-agent stated=false" {
		t.Fatalf("store asked %v, want the subagent's book, named", f.store.asked)
	}
}

func TestAFreshStartsNewestPageIsEmptyWhileItRuns(t *testing.T) {
	// Arrange: a workspace that never ran comes up fresh; the read is made
	// while its start waits to retry.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.client.responses = []*shimv1.StartSessionResponse{vendorRefusal(retryableVendorStart(), "overloaded")}
	var (
		page    *conversationv1.HistoryPage
		readErr error
	)
	f.retryAfter = func(time.Duration) <-chan time.Time {
		page, readErr = f.fleet.ReadHistory(context.Background(), ws.ID, nil, nil)
		fired := make(chan time.Time, 1)
		fired <- f.now
		return fired
	}

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	assertEmptyHistory(t, page, readErr)
}

func TestAStartedSessionsHoldTellsTheFeedNoSource(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := resumable(f)

	// Act.
	if err := f.fleet.Start(context.Background(), ws); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert: only the running start's source; its watch kicks the rest.
	if len(f.feed.sourcesUp) != 1 {
		t.Fatalf("sources up = %v, want the running start's alone", f.feed.sourcesUp)
	}
}

func TestAFreshStartTellsTheFeedItsBookIsNew(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	if len(f.feed.freshBooks) != 1 || f.feed.freshBooks[0] != ws.ID {
		t.Fatalf("fresh books = %v, want %q", f.feed.freshBooks, ws.ID)
	}
}

func TestAResumeTellsTheFeedNoNewBook(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := resumable(f)

	// Act.
	if err := f.fleet.Start(context.Background(), ws); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	if len(f.feed.freshBooks) != 0 {
		t.Fatalf("fresh books = %v, want none for a resume", f.feed.freshBooks)
	}
}

// standDownErr is a history read a stand-down this daemon ordered cut short,
// shaped as the shim client wraps it.
var standDownErr = fmt.Errorf("%w: unavailable: use of closed network connection", shimclient.ErrStandDownOrdered)

func TestFleetReadInheritedSurfacesAFailedStoreRead(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := parkAtColdGate(t, f)
	f.store.err = storereader.ErrUnanswered

	// Act.
	_, err := f.fleet.ReadInherited(context.Background(), ws, &conversationv1.AgentId{Value: "parent-book"}, nil, 1)

	// Assert.
	if !errors.Is(err, storereader.ErrUnanswered) || errors.Is(err, feed.ErrNoHistorySource) {
		t.Fatalf("err = %v, want the store's failure surfaced", err)
	}
}

// storePromptPage is a root page of one prompt AGENT was sent, as the store
// serves it.
func storePromptPage(agent string) *conversationv1.HistoryPage {
	return promptPageResponse(agent).GetSuccess().GetPage()
}

// recordedBook records W1 with BOOK on record and the store serving a root
// page of one prompt BOOK's agent was sent; no shim is up.
func recordedBook(f *fleetFixture, book string) ids.WorkspaceID {
	ws := f.workspace("w1")
	f.db.books[ws.ID] = book
	f.store.page = storePromptPage(book)
	return ws.ID
}

// persistedBook records W1 with no book on record and BOOK persisted by its
// shim, the store serving a root page of one prompt BOOK's agent was sent.
func persistedBook(f *fleetFixture, book string) ids.WorkspaceID {
	ws := f.workspace("w1")
	f.persisted = map[string]string{ws.Dir: book}
	f.store.page = storePromptPage(book)
	return ws.ID
}

func TestFleetReadHistoryOfAnUnrecordedBookReadsTheBookItsShimPersisted(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := persistedBook(f, "book-p")

	// Act.
	page, err := f.fleet.ReadHistory(context.Background(), ws, nil, nil)

	// Assert.
	if err != nil || len(page.GetEntries()) != 1 {
		t.Fatalf("ReadHistory = %v, %v; want the store's page", page, err)
	}
	if len(f.store.asked) != 1 || f.store.asked[0] != "newest book-p stated=true" {
		t.Fatalf("store asked %v, want the persisted book as stated", f.store.asked)
	}
}

func TestFleetReadHistoryRecordsTheBookItsShimPersisted(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := persistedBook(f, "book-p")

	// Act.
	if _, err := f.fleet.ReadHistory(context.Background(), ws, nil, nil); err != nil {
		t.Fatalf("ReadHistory: %v", err)
	}

	// Assert.
	if f.db.books[ws] != "book-p" {
		t.Fatalf("recorded book = %q, want the persisted one", f.db.books[ws])
	}
}

func TestFleetReadHistoryWhosePersistedBookCannotBeRecordedStillReadsIt(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := persistedBook(f, "book-p")
	f.db.setBookErr = errFake

	// Act.
	_, err := f.fleet.ReadHistory(context.Background(), ws, nil, nil)

	// Assert.
	if err != nil || len(f.store.asked) != 1 {
		t.Fatalf("ReadHistory = %v after store reads %v, want the persisted book read", err, f.store.asked)
	}
	if got := recordsAt(f.log, "daemon.workspace.history_book", "error"); len(got) != 1 {
		t.Fatalf("history_book records = %v, want the failed record at ERROR", got)
	}
}

func TestFleetReadHistoryWhosePersistedBookCannotBeReadIsAFailure(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.persistedErr = errFake

	// Act.
	_, err := f.fleet.ReadHistory(context.Background(), ws.ID, nil, nil)

	// Assert.
	if !errors.Is(err, errFake) || len(f.store.asked) != 0 {
		t.Fatalf("err = %v after store reads %v, want the failed read surfaced and no store read", err, f.store.asked)
	}
	if got := recordsAt(f.log, "daemon.workspace.history_book", "error"); len(got) != 1 {
		t.Fatalf("history_book records = %v, want one ERROR", got)
	}
}

func TestFleetReadHistoryOfARecordedBookReadsTheStore(t *testing.T) {
	cases := []struct {
		name   string
		target *conversationv1.AgentId
		after  *conversationv1.HistoryPointer
		want   string
	}{
		{name: "the newest page reads the recorded book as stated", want: "newest book-1 stated=true"},
		{name: "a page below a pointer reads before it", after: &conversationv1.HistoryPointer{Value: "p-3"}, want: "before book-1 p-3"},
		{name: "a read naming the recorded book reads it as stated", target: &conversationv1.AgentId{Value: "book-1"}, want: "newest book-1 stated=true"},
		{name: "a read naming another book reads it as named", target: &conversationv1.AgentId{Value: "sub-1"}, want: "newest sub-1 stated=false"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: a workspace no shim serves, with its book on record.
			f := newFleetFixture(t)
			ws := f.workspace("w1")
			f.db.books[ws.ID] = "book-1"
			f.store.page = storePromptPage("book-1")

			// Act.
			page, err := f.fleet.ReadHistory(context.Background(), ws.ID, tc.target, tc.after)

			// Assert.
			if err != nil || page == nil {
				t.Fatalf("ReadHistory = %v, %v; want the store's page", page, err)
			}
			if len(f.store.asked) != 1 || f.store.asked[0] != tc.want {
				t.Fatalf("store asked %v, want %q", f.store.asked, tc.want)
			}
			if f.client.historyReads != 0 || len(f.supervisor.spawns) != 0 {
				t.Fatalf("shim reads %d, spawns %d; want no shim involved", f.client.historyReads, len(f.supervisor.spawns))
			}
		})
	}
}

func TestFleetReadHistoryOfARecordedBookNamesTheMainAgentForTheFeed(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.books[ws.ID] = "book-1"
	f.store.page = storePromptPage("book-1")

	// Act.
	if _, err := f.fleet.ReadHistory(context.Background(), ws.ID, nil, nil); err != nil {
		t.Fatalf("ReadHistory: %v", err)
	}

	// Assert.
	if len(f.feed.mainAgents) != 1 || f.feed.mainAgents[0] != "book-1" {
		t.Fatalf("feed main agents = %v, want the page's main agent", f.feed.mainAgents)
	}
}

func TestFleetReadHistoryOfARecordedBookIsNotReadDuringAFreshStart(t *testing.T) {
	// Arrange: the recorded book names the conversation the fresh one replaces.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.books[ws.ID] = "book-old"
	f.client.responses = []*shimv1.StartSessionResponse{vendorRefusal(retryableVendorStart(), "overloaded")}
	var (
		page    *conversationv1.HistoryPage
		readErr error
	)
	f.retryAfter = func(time.Duration) <-chan time.Time {
		page, readErr = f.fleet.ReadHistory(context.Background(), ws.ID, nil, nil)
		fired := make(chan time.Time, 1)
		fired <- f.now
		return fired
	}

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	assertEmptyHistory(t, page, readErr)
	if len(f.store.asked) != 0 {
		t.Fatalf("store asked %v during a fresh start, want none", f.store.asked)
	}
}

func TestFleetReadHistoryWhoseBookCannotBeReadIsAFailure(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.bookErr = errFake

	// Act.
	_, err := f.fleet.ReadHistory(context.Background(), ws.ID, nil, nil)

	// Assert.
	if !errors.Is(err, errFake) || errors.Is(err, feed.ErrNoHistorySource) {
		t.Fatalf("err = %v, want the failed read surfaced", err)
	}
	if got := recordsAt(f.log, "daemon.workspace.history_book", "error"); len(got) != 1 {
		t.Fatalf("history_book records = %v, want one ERROR", got)
	}
}

func TestFleetReadHistoryOfARecordedBookSurfacesAFailedStoreRead(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.books[ws.ID] = "book-1"
	f.store.err = storereader.ErrUnanswered

	// Act.
	_, err := f.fleet.ReadHistory(context.Background(), ws.ID, nil, nil)

	// Assert.
	if !errors.Is(err, storereader.ErrUnanswered) || errors.Is(err, feed.ErrNoHistorySource) {
		t.Fatalf("err = %v, want the store's failure surfaced", err)
	}
}
