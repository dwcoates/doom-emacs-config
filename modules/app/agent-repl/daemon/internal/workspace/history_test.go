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

// fakeHistoryReader answers one scripted ReadHistory and records the request.
type fakeHistoryReader struct {
	req  *shimv1.ReadHistoryRequest
	resp *shimv1.ReadHistoryResponse
	err  error
}

func (f *fakeHistoryReader) ReadHistory(_ context.Context, req *shimv1.ReadHistoryRequest) (*shimv1.ReadHistoryResponse, error) {
	f.req = req
	return f.resp, f.err
}

func pageResponse() *shimv1.ReadHistoryResponse {
	return &shimv1.ReadHistoryResponse{Result: &shimv1.ReadHistoryResponse_Success{Success: &shimv1.ReadHistorySuccess{
		Page: &conversationv1.HistoryPage{Boundary: &conversationv1.HistoryPage_Floor{Floor: &conversationv1.HistoryFloor{}}},
	}}}
}

func TestReadHistoryAsksForThePositionItWasGiven(t *testing.T) {
	tests := []struct {
		name      string
		after     *conversationv1.HistoryPointer
		wantFirst bool
		wantAfter string
	}{
		{name: "no pointer reads the newest page", wantFirst: true},
		{name: "a pointer reads the page before it", after: &conversationv1.HistoryPointer{Value: "p-7"}, wantAfter: "p-7"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			client := &fakeHistoryReader{resp: pageResponse()}

			// Act.
			if _, err := readHistory(context.Background(), client, nil, tt.after); err != nil {
				t.Fatalf("readHistory: %v", err)
			}

			// Assert.
			if got := client.req.GetFirst() != nil; got != tt.wantFirst {
				t.Fatalf("first = %v, want %v", got, tt.wantFirst)
			}
			if got := client.req.GetAfter().GetValue(); got != tt.wantAfter {
				t.Fatalf("after = %q, want %q", got, tt.wantAfter)
			}
		})
	}
}

func TestReadHistoryRefusalIsATypedShimRefusal(t *testing.T) {
	// Arrange.
	client := &fakeHistoryReader{resp: &shimv1.ReadHistoryResponse{Result: &shimv1.ReadHistoryResponse_Failure{Failure: &shimv1.ReadHistoryFailure{
		Detail: "store down",
		Kind:   &shimv1.ReadHistoryFailure_StoreUnavailable{StoreUnavailable: &shimv1.ReadHistoryStoreUnavailable{}},
	}}}}

	// Act.
	_, err := readHistory(context.Background(), client, nil, nil)

	// Assert.
	var refusal *ShimRefusal
	if !errors.As(err, &refusal) || refusal.Arm != "store_unavailable" {
		t.Fatalf("err = %v, want a store_unavailable ShimRefusal", err)
	}
}

func TestReadHistoryTransportErrorPassesThrough(t *testing.T) {
	// Arrange.
	broken := errors.New("link severed")
	client := &fakeHistoryReader{err: broken}

	// Act.
	_, err := readHistory(context.Background(), client, nil, nil)

	// Assert.
	if !errors.Is(err, broken) {
		t.Fatalf("err = %v, want the transport error", err)
	}
}

func TestReadHistoryAnswerWithNoArmIsAnError(t *testing.T) {
	// Arrange.
	client := &fakeHistoryReader{resp: &shimv1.ReadHistoryResponse{}}

	// Act.
	_, err := readHistory(context.Background(), client, nil, nil)

	// Assert.
	if err == nil {
		t.Fatal("readHistory accepted an answer with no arm")
	}
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

// parkAtColdGate brings W1 up to a standing cold gate: the client is held and
// no session, so no watcher, is up.
func parkAtColdGate(t *testing.T, f *fleetFixture) ids.WorkspaceID {
	t.Helper()
	ws := resumable(f)
	f.client.response = coldResponse()
	if err := f.fleet.Start(context.Background(), ws); err != nil {
		t.Fatalf("Start: %v", err)
	}
	return ws
}

func TestFleetReadHistoryWithNoShimHasNoSource(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")

	// Act.
	_, err := f.fleet.ReadHistory(context.Background(), ws.ID, nil, nil)

	// Assert.
	if !errors.Is(err, feed.ErrNoHistorySource) {
		t.Fatalf("err = %v, want ErrNoHistorySource", err)
	}
}

func TestFleetReadHistoryWithNoShimAsksForAParkedSessionBack(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")

	// Act.
	_, _ = f.fleet.ReadHistory(context.Background(), ws.ID, nil, nil)

	// Assert.
	if len(f.restoreAsked) != 1 || f.restoreAsked[0] != ws.ID {
		t.Fatalf("restores asked = %v, want %v", f.restoreAsked, ws.ID)
	}
}

func TestFleetReadHistoryReadsAColdGatedWorkspacesShim(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := parkAtColdGate(t, f)

	// Act.
	page, err := f.fleet.ReadHistory(context.Background(), ws, nil, nil)

	// Assert.
	if err != nil || page == nil || f.client.historyReads != 1 {
		t.Fatalf("ReadHistory = %v, %v after %d reads; want the shim's page", page, err, f.client.historyReads)
	}
}

func TestFleetReadHistoryWithoutAWatcherNamesTheMainAgentForTheFeed(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := parkAtColdGate(t, f)
	f.client.history = promptPageResponse("main-agent")

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
	ws := parkAtColdGate(t, f)
	f.client.history = promptPageResponse("main-agent")

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
	ws := parkAtColdGate(t, f)
	f.client.history = promptPageResponse("main-agent")

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
	ws := parkAtColdGate(t, f)
	f.client.history = promptPageResponse("sub-agent")

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
	f.client.history = promptPageResponse("main-agent")

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
	if readErr != nil || f.client.historyReads != 1 {
		t.Fatalf("read during the retry = %v after %d reads, want the shim's page", readErr, f.client.historyReads)
	}
}

func TestFleetReadHistoryOfAReapedHeldShimHasNoSource(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.fleet.sessions[ws.ID] = &live{client: &fakeClient{reaped: true}}

	// Act.
	_, err := f.fleet.ReadHistory(context.Background(), ws.ID, nil, nil)

	// Assert.
	if !errors.Is(err, feed.ErrNoHistorySource) {
		t.Fatalf("err = %v, want ErrNoHistorySource", err)
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

func TestAFailedStartIsAHistorySourceThroughItsHeldShim(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := resumable(f)
	f.client.response = vendorRefusal(rejectedVendorStart(), "invalid api key")
	_ = f.fleet.Start(context.Background(), ws)

	// Act.
	_, err := f.fleet.ReadHistory(context.Background(), ws, nil, nil)

	// Assert.
	if err != nil || f.client.historyReads != 1 || f.client.stoodDown {
		t.Fatalf("read = %v after %d reads (stood down %t), want the held shim's page", err, f.client.historyReads, f.client.stoodDown)
	}
}

func TestAFailedFreshStartIsNoHistorySource(t *testing.T) {
	// Arrange: the held shim's persisted book names the conversation the
	// fresh one replaces.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.client.response = vendorRefusal(rejectedVendorStart(), "invalid api key")
	_ = f.fleet.Start(context.Background(), ws.ID)

	// Act.
	_, err := f.fleet.ReadHistory(context.Background(), ws.ID, nil, nil)

	// Assert.
	if !errors.Is(err, feed.ErrNoHistorySource) || !f.fleet.Held(ws.ID) {
		t.Fatalf("read = %v, held = %t; want no source from a held fresh start's shim", err, f.fleet.Held(ws.ID))
	}
}

// unknownAgentResponse is the shim's refusal of a book it does not hold.
func unknownAgentResponse() *shimv1.ReadHistoryResponse {
	return &shimv1.ReadHistoryResponse{Result: &shimv1.ReadHistoryResponse_Failure{Failure: &shimv1.ReadHistoryFailure{
		Detail: "no session has been started on this shim and the workspace holds no persisted main agent",
		Kind:   &shimv1.ReadHistoryFailure_UnknownAgent{UnknownAgent: &shimv1.ReadHistoryUnknownAgent{}},
	}}}
}

func TestFleetReadHistoryOfNoBookBeforeASessionIsNoSource(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := parkAtColdGate(t, f)
	f.client.history = unknownAgentResponse()

	// Act.
	_, err := f.fleet.ReadHistory(context.Background(), ws, nil, nil)

	// Assert.
	if !errors.Is(err, feed.ErrNoHistorySource) {
		t.Fatalf("err = %v, want ErrNoHistorySource", err)
	}
}

func TestFleetReadHistoryOfNoBookBeforeASessionIsRecorded(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := parkAtColdGate(t, f)
	f.client.history = unknownAgentResponse()

	// Act.
	_, _ = f.fleet.ReadHistory(context.Background(), ws, nil, nil)

	// Assert.
	if got := recordsAt(f.log, "daemon.workspace.history_no_book", "info"); len(got) != 1 {
		t.Fatalf("history_no_book records = %v, want one INFO", got)
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

func TestFleetReadHistoryOfNoBookWithASessionUpIsARefusal(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}
	f.client.history = unknownAgentResponse()

	// Act.
	_, err := f.fleet.ReadHistory(context.Background(), ws.ID, nil, nil)

	// Assert.
	var refusal *ShimRefusal
	if errors.Is(err, feed.ErrNoHistorySource) || !errors.As(err, &refusal) || refusal.Arm != "unknown_agent" {
		t.Fatalf("err = %v, want the unknown_agent refusal", err)
	}
}

func TestAFreshStartIsNoHistorySourceWhileItRuns(t *testing.T) {
	// Arrange: a workspace that never ran comes up fresh; the read is made
	// while its start waits to retry.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.client.responses = []*shimv1.StartSessionResponse{vendorRefusal(retryableVendorStart(), "overloaded")}
	var readErr error
	f.retryAfter = func(time.Duration) <-chan time.Time {
		_, readErr = f.fleet.ReadHistory(context.Background(), ws.ID, nil, nil)
		fired := make(chan time.Time, 1)
		fired <- f.now
		return fired
	}

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	if !errors.Is(readErr, feed.ErrNoHistorySource) {
		t.Fatalf("read during a fresh start = %v, want ErrNoHistorySource", readErr)
	}
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

func TestFleetReadHistoryCutShortByAnOrderedStandDownIsNoSource(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := parkAtColdGate(t, f)
	f.client.historyErr = standDownErr

	// Act.
	_, err := f.fleet.ReadHistory(context.Background(), ws, nil, nil)

	// Assert.
	if !errors.Is(err, feed.ErrNoHistorySource) || !errors.Is(err, shimclient.ErrStandDownOrdered) {
		t.Fatalf("err = %v, want ErrNoHistorySource carrying the stand-down", err)
	}
}

func TestFleetReadHistoryCutShortByAnOrderedStandDownIsRecordedAtInfo(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := parkAtColdGate(t, f)
	f.client.historyErr = standDownErr

	// Act.
	_, _ = f.fleet.ReadHistory(context.Background(), ws, nil, nil)

	// Assert.
	if got := recordsAt(f.log, "daemon.workspace.history_stood_down", "info"); len(got) != 1 {
		t.Fatalf("history_stood_down records = %v, want one INFO", got)
	}
}

func TestFleetReadHistoryOfAnyOtherFailureStaysAFailure(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := parkAtColdGate(t, f)
	f.client.historyErr = errors.New("unavailable: connection reset")

	// Act.
	_, err := f.fleet.ReadHistory(context.Background(), ws, nil, nil)

	// Assert.
	if err == nil || errors.Is(err, feed.ErrNoHistorySource) {
		t.Fatalf("err = %v, want the failure itself, not no-source", err)
	}
}

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
			if f.client.historyReads != 0 || len(f.restoreAsked) != 0 || len(f.supervisor.spawns) != 0 {
				t.Fatalf("shim reads %d, restores asked %v, spawns %d; want no shim involved", f.client.historyReads, f.restoreAsked, len(f.supervisor.spawns))
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

func TestFleetReadHistoryOfARecordedBookIsNoSourceDuringAFreshStart(t *testing.T) {
	// Arrange: the recorded book names the conversation the fresh one replaces.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.books[ws.ID] = "book-old"
	f.client.responses = []*shimv1.StartSessionResponse{vendorRefusal(retryableVendorStart(), "overloaded")}
	var readErr error
	f.retryAfter = func(time.Duration) <-chan time.Time {
		_, readErr = f.fleet.ReadHistory(context.Background(), ws.ID, nil, nil)
		fired := make(chan time.Time, 1)
		fired <- f.now
		return fired
	}

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	if !errors.Is(readErr, feed.ErrNoHistorySource) || len(f.store.asked) != 0 {
		t.Fatalf("read during a fresh start = %v after store reads %v, want ErrNoHistorySource and none", readErr, f.store.asked)
	}
}

func TestFleetReadHistoryOfAnUnrecordedBookIsReadThroughItsShim(t *testing.T) {
	// Arrange: the cold refusal named no book, so none is on record.
	f := newFleetFixture(t)
	ws := parkAtColdGate(t, f)

	// Act.
	page, err := f.fleet.ReadHistory(context.Background(), ws, nil, nil)

	// Assert.
	if err != nil || page == nil || f.client.historyReads != 1 || len(f.store.asked) != 0 {
		t.Fatalf("ReadHistory = %v, %v after %d shim reads and store reads %v; want the shim's page", page, err, f.client.historyReads, f.store.asked)
	}
	if got := recordsAt(f.log, "daemon.workspace.history_through_shim", "info"); len(got) != 1 {
		t.Fatalf("history_through_shim records = %v, want one INFO", got)
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
