package workspace

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/feed"
	"claude-repld/internal/wsm"
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
func (c *fakeClient) ReadHistory(context.Context, *shimv1.ReadHistoryRequest) (*shimv1.ReadHistoryResponse, error) {
	c.historyReads++
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

// parkAtColdGate brings W1 up to a standing cold gate: the client is held and
// no session, so no watcher, is up.
func parkAtColdGate(t *testing.T, f *fleetFixture) ids.WorkspaceID {
	t.Helper()
	ws := f.workspace("w1")
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, VendorSessionID: "vendor-1"}
	f.client.response = coldResponse()
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}
	return ws.ID
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
	if readErr != nil || f.client.historyReads != 1 {
		t.Fatalf("read during the retry = %v after %d reads, want the shim's page", readErr, f.client.historyReads)
	}
}

func TestFleetReadHistoryOfAReapedStartingClientHasNoSource(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	end := f.fleet.beginHistoryClient(ws.ID, &fakeClient{reaped: true})
	defer end()

	// Act.
	_, err := f.fleet.ReadHistory(context.Background(), ws.ID, nil, nil)

	// Assert.
	if !errors.Is(err, feed.ErrNoHistorySource) {
		t.Fatalf("err = %v, want ErrNoHistorySource", err)
	}
}

func TestFleetReadHistoryOfAWithdrawnStartingClientHasNoSource(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.fleet.beginHistoryClient(ws.ID, &fakeClient{})()

	// Act.
	_, err := f.fleet.ReadHistory(context.Background(), ws.ID, nil, nil)

	// Assert.
	if !errors.Is(err, feed.ErrNoHistorySource) {
		t.Fatalf("err = %v, want ErrNoHistorySource", err)
	}
}

func TestBeginHistoryClientTellsTheFeedASourceIsUp(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")

	// Act.
	end := f.fleet.beginHistoryClient(ws.ID, &fakeClient{})
	defer end()

	// Assert.
	if len(f.feed.sourcesUp) != 1 || f.feed.sourcesUp[0] != ws.ID {
		t.Fatalf("sources up = %v, want %q", f.feed.sourcesUp, ws.ID)
	}
}

func TestAWithdrawalOfAReplacedStartingClientKeepsTheNewOne(t *testing.T) {
	// Arrange: a second start's client replaced the first's.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	endFirst := f.fleet.beginHistoryClient(ws.ID, &fakeClient{})
	endSecond := f.fleet.beginHistoryClient(ws.ID, &fakeClient{})
	defer endSecond()

	// Act.
	endFirst()

	// Assert.
	if _, ok := f.fleet.historyClient(ws.ID); !ok {
		t.Fatal("the first start's withdrawal took the second start's client")
	}
}

func TestAHeldClientTellsTheFeedASourceIsUp(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)

	// Act.
	ws := parkAtColdGate(t, f)

	// Assert: once for the running start, once for the held client.
	if len(f.feed.sourcesUp) != 2 || f.feed.sourcesUp[1] != ws {
		t.Fatalf("sources up = %v, want the running start's and the held client's", f.feed.sourcesUp)
	}
}

func TestAFailedStartKeepsTheNewestPageWhileItsShimServes(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.client.response = vendorRefusal(rejectedVendorStart(), "invalid api key")
	var stoppedAtKeep, sourcedAtKeep bool
	f.feed.onKeep = func(id ids.WorkspaceID) {
		stoppedAtKeep = f.client.stoodDown
		_, sourcedAtKeep = f.fleet.historyClient(id)
	}

	// Act.
	_ = f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if len(f.feed.kept) != 1 || stoppedAtKeep || !sourcedAtKeep {
		t.Fatalf("kept %v (stopped %v, sourced %v), want one keep through the live client", f.feed.kept, stoppedAtKeep, sourcedAtKeep)
	}
}

func TestAFailedStartIsNoHistorySourceOnceItsShimIsStopped(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.client.response = vendorRefusal(rejectedVendorStart(), "invalid api key")

	// Act.
	_ = f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if _, ok := f.fleet.historyClient(ws.ID); ok {
		t.Fatal("a failed start's stopped shim is still a history source")
	}
}

func TestAFailedKeepIsAnErrorAndTheStartsOwnErrorStands(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.client.response = vendorRefusal(rejectedVendorStart(), "invalid api key")
	f.feed.keptErr = errors.New("store unreachable")

	// Act.
	err := f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if err == nil || strings.Contains(err.Error(), "store unreachable") || len(recordsAt(f.log, opBringUp, "error")) == 0 {
		t.Fatalf("Start = %v with errors %v; want the start's refusal and the keep's ERROR", err, recordsAt(f.log, opBringUp, "error"))
	}
}

func TestAFailedStartWhoseContextEndedKeepsNothing(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	ctx, cancel := context.WithCancel(context.Background())
	f.client.response = vendorRefusal(rejectedVendorStart(), "invalid api key")
	f.client.entered = make(chan struct{})
	go func() {
		<-f.client.entered
		cancel()
	}()
	f.client.startHold = make(chan struct{})

	// Act.
	_ = f.fleet.Start(ctx, ws.ID)

	// Assert.
	if len(f.feed.kept) != 0 {
		t.Fatalf("kept %v, want nothing read on an ended context", f.feed.kept)
	}
}
