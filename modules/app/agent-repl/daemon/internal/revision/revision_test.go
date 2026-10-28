package revision

import (
	"context"
	"errors"
	"strings"
	"sync"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/prompts"
	"agent-repld/internal/resolve/feed"
	"agent-repld/internal/shimclient"
)

// repoPromptsDir is the checkout's own briefs.
const repoPromptsDir = "../../../prompts"

const ws = ids.WorkspaceID("ws-1")

// fakeFeed is the feed's side of a revision: one response per row, and every
// state a revision set.
type fakeFeed struct {
	mu        sync.Mutex
	responses map[string]feed.RevisableResponse
	states    []*frontendv1.FeedResponseRevision
}

func (f *fakeFeed) RevisableFinalResponse(_ ids.WorkspaceID, row *frontendv1.FeedId) (feed.RevisableResponse, bool) {
	f.mu.Lock()
	defer f.mu.Unlock()
	response, ok := f.responses[row.GetValue()]
	return response, ok
}

func (f *fakeFeed) SetResponseRevision(_ ids.WorkspaceID, _ *frontendv1.FeedId, state *frontendv1.FeedResponseRevision) bool {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.states = append(f.states, state)
	return true
}

// revise sets the markdown ROW's response says, as a landed revision does.
func (f *fakeFeed) revise(row, markdown string) {
	f.mu.Lock()
	defer f.mu.Unlock()
	response := f.responses[row]
	response.Markdown = markdown
	f.responses[row] = response
}

// lastState is the last revision state set.
func (f *fakeFeed) lastState() *frontendv1.FeedResponseRevision {
	f.mu.Lock()
	defer f.mu.Unlock()
	if len(f.states) == 0 {
		return nil
	}
	return f.states[len(f.states)-1]
}

// fakeShim answers ReviseResponse through ANSWER, recording every request.
type fakeShim struct {
	shimclient.Client
	mu       sync.Mutex
	requests []*shimv1.ReviseResponseRequest
	answer   func(*shimv1.ReviseResponseRequest) (*shimv1.ReviseResponseResponse, error)
}

func (s *fakeShim) ReviseResponse(_ context.Context, req *shimv1.ReviseResponseRequest) (*shimv1.ReviseResponseResponse, error) {
	s.mu.Lock()
	s.requests = append(s.requests, req)
	s.mu.Unlock()
	return s.answer(req)
}

func (s *fakeShim) asked() []*shimv1.ReviseResponseRequest {
	s.mu.Lock()
	defer s.mu.Unlock()
	return append([]*shimv1.ReviseResponseRequest(nil), s.requests...)
}

// fakeShims is the fleet: SHIM, or no live session when it is nil.
type fakeShims struct{ shim *fakeShim }

func (f fakeShims) Client(ids.WorkspaceID) (shimclient.Client, bool) {
	if f.shim == nil {
		return nil, false
	}
	return f.shim, true
}

func succeeded(*shimv1.ReviseResponseRequest) (*shimv1.ReviseResponseResponse, error) {
	return &shimv1.ReviseResponseResponse{Result: &shimv1.ReviseResponseResponse_Success{Success: &shimv1.ReviseResponseSuccess{Model: "claude-sonnet-5"}}}, nil
}

// harness is one Reviser over the fakes, with a signal per finished revision.
type harness struct {
	reviser  *Reviser
	feed     *fakeFeed
	shim     *fakeShim
	log      *dlog.TestLogger
	finished chan struct{}
}

func newHarness(t *testing.T, shim *fakeShim, promptsDir string) *harness {
	t.Helper()
	f := &fakeFeed{responses: map[string]feed.RevisableResponse{"row-1": {Unit: "msg_1:0", Markdown: "1 🔎 the original"}}}
	log := dlog.NewTestLogger()
	h := &harness{feed: f, shim: shim, log: log, finished: make(chan struct{}, 8)}
	shims := fakeShims{shim: shim}
	h.reviser = New(Deps{Shims: shims, Feed: f, PromptsDir: promptsDir, Life: context.Background(), Log: log})
	h.reviser.done = func() { h.finished <- struct{}{} }
	return h
}

func ask(words, text string) Ask {
	return Ask{
		Row:   &frontendv1.FeedId{Value: "row-1"},
		Words: words,
		Said: &conversationv1.UserSaid{Content: &conversationv1.UserContent{Blocks: []*conversationv1.UserContentBlock{
			{Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: text}}},
		}}},
	}
}

// reviseAndWait asks for one revision and waits for it to finish.
func (h *harness) reviseAndWait(t *testing.T, a Ask) {
	t.Helper()
	if err := h.reviser.Revise(ws, a); err != nil {
		t.Fatalf("Revise() = %v", err)
	}
	<-h.finished
}

func TestReviseRefusesARowThatIsNoFinalResponseMarkingNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeShim{answer: succeeded}, repoPromptsDir)
	a := ask("word", "what does this mean")
	a.Row = &frontendv1.FeedId{Value: "no-such-row"}

	// Act.
	err := h.reviser.Revise(ws, a)

	// Assert.
	if !errors.Is(err, ErrNotRevisable) {
		t.Fatalf("Revise() = %v, want ErrNotRevisable", err)
	}
	if len(h.feed.states) != 0 {
		t.Fatalf("a refused revision set states %v", h.feed.states)
	}
}

func TestReviseMarksTheResponsePendingBeforeItReturns(t *testing.T) {
	// Arrange.
	release := make(chan struct{})
	shim := &fakeShim{answer: func(req *shimv1.ReviseResponseRequest) (*shimv1.ReviseResponseResponse, error) {
		<-release
		return succeeded(req)
	}}
	h := newHarness(t, shim, repoPromptsDir)

	// Act.
	err := h.reviser.Revise(ws, ask("word", "what does this mean"))

	// Assert.
	if err != nil || h.feed.lastState().GetPending() == nil {
		t.Fatalf("Revise() = %v, last state %v, want pending", err, h.feed.lastState())
	}
	close(release)
	<-h.finished
}

func TestARevisionAsksTheShimForTheResponsesUnit(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeShim{answer: succeeded}, repoPromptsDir)

	// Act.
	h.reviseAndWait(t, ask("word", "what does this mean"))

	// Assert.
	if got := h.shim.asked()[0].GetResponse().GetValue(); got != "msg_1:0" {
		t.Fatalf("the shim was asked to revise %q, want msg_1:0", got)
	}
}

func TestARevisionsInstructionIsTheBriefAheadOfWhatTheUserSaid(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeShim{answer: succeeded}, repoPromptsDir)

	// Act.
	h.reviseAndWait(t, ask("entropy", "what does this mean"))

	// Assert.
	blocks := h.shim.asked()[0].GetInstruction().GetContent().GetBlocks()
	if len(blocks) != 2 {
		t.Fatalf("instruction blocks = %d, want the brief and the user's one", len(blocks))
	}
	brief := blocks[0].GetText().GetText()
	if !strings.Contains(brief, "1 🔎 the original") || !strings.Contains(brief, "entropy") {
		t.Fatalf("the brief carries neither the response nor the words: %q", brief)
	}
	if got := blocks[1].GetText().GetText(); got != "what does this mean" {
		t.Fatalf("the user's block reads %q", got)
	}
}

func TestASucceededRevisionLeavesThePendingStateToThePageLine(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeShim{answer: succeeded}, repoPromptsDir)

	// Act.
	h.reviseAndWait(t, ask("word", "what does this mean"))

	// Assert.
	if h.feed.lastState().GetPending() == nil {
		t.Fatalf("the last state is %v; the page line, not the call, ends the pending state", h.feed.lastState())
	}
}

func TestAFailedRevisionMarksTheResponseFailedAndRecordsTheCause(t *testing.T) {
	refused := func(cause *shimv1.ReviseResponseError) func(*shimv1.ReviseResponseRequest) (*shimv1.ReviseResponseResponse, error) {
		return func(*shimv1.ReviseResponseRequest) (*shimv1.ReviseResponseResponse, error) {
			return &shimv1.ReviseResponseResponse{Result: &shimv1.ReviseResponseResponse_Error{Error: cause}}, nil
		}
	}
	tests := []struct {
		name   string
		shim   *fakeShim
		prompt string
		text   string
		arm    string
	}{
		{"no session", &fakeShim{answer: refused(&shimv1.ReviseResponseError{Cause: &shimv1.ReviseResponseError_NoSession{NoSession: &shimv1.ReviseResponseNoSession{}}})}, repoPromptsDir,
			"Couldn't revise: the workspace has no running session", "no_session"},
		{"no fork point", &fakeShim{answer: refused(&shimv1.ReviseResponseError{Cause: &shimv1.ReviseResponseError_NoForkPoint{NoForkPoint: &shimv1.ReviseResponseNoForkPoint{Detail: "a turn is in flight"}}})}, repoPromptsDir,
			"Couldn't revise: the conversation could not be forked: a turn is in flight", "no_fork_point"},
		{"vendor failed", &fakeShim{answer: refused(&shimv1.ReviseResponseError{Cause: &shimv1.ReviseResponseError_VendorFailed{VendorFailed: &shimv1.ReviseResponseVendorFailed{Detail: "overloaded"}}})}, repoPromptsDir,
			"Couldn't revise: overloaded", "vendor_failed"},
		{"empty answer", &fakeShim{answer: refused(&shimv1.ReviseResponseError{Cause: &shimv1.ReviseResponseError_EmptyAnswer{EmptyAnswer: &shimv1.ReviseResponseEmptyAnswer{}}})}, repoPromptsDir,
			"Couldn't revise: the model answered with nothing", "empty_answer"},
		{"transport", &fakeShim{answer: func(*shimv1.ReviseResponseRequest) (*shimv1.ReviseResponseResponse, error) {
			return nil, errors.New("connection refused")
		}}, repoPromptsDir, "Couldn't revise: the session could not be asked: connection refused", "transport"},
		{"no live shim", nil, repoPromptsDir, "Couldn't revise: the workspace has no running session", "no_shim"},
		{"no brief", &fakeShim{answer: succeeded}, "/nonexistent-prompts-dir", "", "brief"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t, tt.shim, tt.prompt)

			// Act.
			h.reviseAndWait(t, ask("word", "what does this mean"))

			// Assert.
			failed := h.feed.lastState().GetFailed()
			if failed == nil || (tt.text != "" && failed.GetText() != tt.text) {
				t.Fatalf("last state = %v, want failed %q", h.feed.lastState(), tt.text)
			}
			record, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "error", Operation: opRun})
			if !ok || record.Context["arm"] != tt.arm || record.Context["row"] != "row-1" || record.Context["workspace"] != string(ws) {
				t.Fatalf("error record = %+v (%v), want arm %q naming the row and workspace", record.Context, ok, tt.arm)
			}
		})
	}
}

func TestARevisionOfAResponseThatLeftTheFeedIsMarkedFailed(t *testing.T) {
	// Arrange: the response is gone by the time the lane reaches it.
	release := make(chan struct{})
	first := true
	shim := &fakeShim{}
	h := newHarness(t, shim, repoPromptsDir)
	shim.answer = func(req *shimv1.ReviseResponseRequest) (*shimv1.ReviseResponseResponse, error) {
		if first {
			first = false
			<-release
		}
		return succeeded(req)
	}
	if err := h.reviser.Revise(ws, ask("word", "first")); err != nil {
		t.Fatalf("Revise() = %v", err)
	}
	if err := h.reviser.Revise(ws, ask("word", "second")); err != nil {
		t.Fatalf("Revise() = %v", err)
	}

	// Act.
	h.feed.mu.Lock()
	delete(h.feed.responses, "row-1")
	h.feed.mu.Unlock()
	close(release)
	<-h.finished
	<-h.finished

	// Assert.
	if got := h.feed.lastState().GetFailed().GetText(); got != "Couldn't revise: the response is no longer a final response of this workspace" {
		t.Fatalf("last state = %v", h.feed.lastState())
	}
}

func TestAWorkspacesRevisionsRunInOrderEachOnTheResponseAsItStands(t *testing.T) {
	// Arrange: the first revision lands while the second waits behind it.
	release := make(chan struct{})
	shim := &fakeShim{}
	h := newHarness(t, shim, repoPromptsDir)
	shim.answer = func(req *shimv1.ReviseResponseRequest) (*shimv1.ReviseResponseResponse, error) {
		if len(shim.asked()) == 1 {
			<-release
			h.feed.revise("row-1", "1 🔎 the first revision")
		}
		return succeeded(req)
	}
	if err := h.reviser.Revise(ws, ask("word", "first")); err != nil {
		t.Fatalf("Revise() = %v", err)
	}
	if err := h.reviser.Revise(ws, ask("word", "second")); err != nil {
		t.Fatalf("Revise() = %v", err)
	}

	// Act.
	close(release)
	<-h.finished
	<-h.finished

	// Assert.
	asked := shim.asked()
	if len(asked) != 2 {
		t.Fatalf("the shim was asked %d times, want 2", len(asked))
	}
	if got := asked[1].GetInstruction().GetContent().GetBlocks()[1].GetText().GetText(); got != "second" {
		t.Fatalf("the second revision asked %q, want the second prompt", got)
	}
	if brief := asked[1].GetInstruction().GetContent().GetBlocks()[0].GetText().GetText(); !strings.Contains(brief, "the first revision") {
		t.Fatalf("the second revision revised %q, not the first's result", brief)
	}
}

func TestTheRevisionBriefDeclaresTheResponseAndTheWords(t *testing.T) {
	// Arrange.
	brief, err := prompts.Load(repoPromptsDir, Brief)
	if err != nil {
		t.Fatalf("load the brief: %v", err)
	}

	// Act.
	_, err = brief.Splice(map[string]string{"response": "r", "words": "w"})

	// Assert.
	if err != nil {
		t.Fatalf("the brief does not splice exactly the response and the words: %v", err)
	}
}
