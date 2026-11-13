package revision

import (
	"context"
	"errors"
	"strings"
	"sync"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/prompts"
	"agent-repld/internal/resolve/feed"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/wsm"
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

// fakeStatus records every updating fact a lane set.
type fakeStatus struct {
	mu  sync.Mutex
	set []bool
}

func (f *fakeStatus) SetUpdating(_ ids.WorkspaceID, updating bool) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.set = append(f.set, updating)
}

func (f *fakeStatus) facts() []bool {
	f.mu.Lock()
	defer f.mu.Unlock()
	return append([]bool(nil), f.set...)
}

// fakeShim answers ReviseResponse through ANSWER, recording every request.
type fakeShim struct {
	shimclient.Client
	mu       sync.Mutex
	requests []*shimv1.ReviseResponseRequest
	answer   func(*shimv1.ReviseResponseRequest) (*shimv1.ReviseResponseResponse, error)
	// hangs makes the call answer only when its context ends, as a shim that
	// never answers does.
	hangs bool
	// answerCtx, when set, answers in place of answer, seeing the call's
	// context.
	answerCtx func(context.Context, *shimv1.ReviseResponseRequest) (*shimv1.ReviseResponseResponse, error)
}

func (s *fakeShim) ReviseResponse(ctx context.Context, req *shimv1.ReviseResponseRequest) (*shimv1.ReviseResponseResponse, error) {
	s.mu.Lock()
	s.requests = append(s.requests, req)
	s.mu.Unlock()
	if s.hangs {
		<-ctx.Done()
		return nil, ctx.Err()
	}
	if s.answerCtx != nil {
		return s.answerCtx(ctx, req)
	}
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

// revisedWords is the prose every successful fake revision answers.
const revisedWords = "1 🔎 the revised words"

func succeeded(*shimv1.ReviseResponseRequest) (*shimv1.ReviseResponseResponse, error) {
	return &shimv1.ReviseResponseResponse{Result: &shimv1.ReviseResponseResponse_Success{Success: &shimv1.ReviseResponseSuccess{
		Model: "claude-sonnet-5", Prose: &conversationv1.AgentResponseProse{Markdown: revisedWords},
	}}}, nil
}

// fakeOwed records every revision owed to the agent, failing with ERR.
type fakeOwed struct {
	mu   sync.Mutex
	owed []wsm.OwedRevision
	err  error
}

func (f *fakeOwed) OweRevision(_ context.Context, _ wsm.WorkspaceID, owed wsm.OwedRevision) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.err != nil {
		return f.err
	}
	f.owed = append(f.owed, owed)
	return nil
}

func (f *fakeOwed) recorded() []wsm.OwedRevision {
	f.mu.Lock()
	defer f.mu.Unlock()
	return append([]wsm.OwedRevision(nil), f.owed...)
}

// revisedAt is the instant every fake revision lands at.
var revisedAt = time.Unix(1_800_000_000, 0)

// harness is one Reviser over the fakes, with a signal per finished revision.
type harness struct {
	reviser  *Reviser
	feed     *fakeFeed
	status   *fakeStatus
	shim     *fakeShim
	owed     *fakeOwed
	log      *dlog.TestLogger
	finished chan struct{}
}

func newHarness(t *testing.T, shim *fakeShim, promptsDir string) *harness {
	t.Helper()
	f := &fakeFeed{responses: map[string]feed.RevisableResponse{"row-1": {Unit: "msg_1:0", Markdown: "1 🔎 the original"}}}
	log := dlog.NewTestLogger()
	h := &harness{feed: f, status: &fakeStatus{}, shim: shim, owed: &fakeOwed{}, log: log, finished: make(chan struct{}, 8)}
	shims := fakeShims{shim: shim}
	h.reviser = New(Deps{Shims: shims, Feed: f, Status: h.status, Owed: h.owed, Bound: Bound, Now: func() time.Time { return revisedAt },
		PromptsDir: promptsDir, Life: context.Background(), Log: log})
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
	if facts := h.status.facts(); len(facts) != 0 {
		t.Fatalf("a refused revision set updating %v", facts)
	}
}

func TestReviseRaisesUpdatingBeforeItReturns(t *testing.T) {
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
	if facts := h.status.facts(); err != nil || len(facts) != 1 || !facts[0] {
		t.Fatalf("Revise() = %v, updating facts %v, want [true]", err, facts)
	}
	close(release)
	<-h.finished
}

func TestALaneLowersUpdatingOnceItsLastRevisionEnds(t *testing.T) {
	// Arrange: the second revision queues behind the first.
	release := make(chan struct{})
	shim := &fakeShim{}
	shim.answer = func(req *shimv1.ReviseResponseRequest) (*shimv1.ReviseResponseResponse, error) {
		if len(shim.asked()) == 1 {
			<-release
		}
		return succeeded(req)
	}
	h := newHarness(t, shim, repoPromptsDir)
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

	// Assert: one raise for the lane, one lower when it drained.
	if facts := h.status.facts(); len(facts) != 2 || !facts[0] || facts[1] {
		t.Fatalf("updating facts %v, want [true false]", facts)
	}
}

func TestAFailedRevisionLowersUpdating(t *testing.T) {
	// Arrange: the workspace has no live session.
	h := newHarness(t, nil, repoPromptsDir)

	// Act.
	h.reviseAndWait(t, ask("word", "what does this mean"))

	// Assert.
	if facts := h.status.facts(); len(facts) != 2 || !facts[0] || facts[1] {
		t.Fatalf("updating facts %v, want [true false]", facts)
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
		{"no prose", &fakeShim{answer: func(*shimv1.ReviseResponseRequest) (*shimv1.ReviseResponseResponse, error) {
			return &shimv1.ReviseResponseResponse{Result: &shimv1.ReviseResponseResponse_Success{Success: &shimv1.ReviseResponseSuccess{Model: "m"}}}, nil
		}}, repoPromptsDir, "Couldn't revise: the session answered a written revision with no words", "no_prose"},
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

func TestTheRevisionBriefDiscouragesToolUse(t *testing.T) {
	// Arrange.
	brief, err := prompts.Load(repoPromptsDir, Brief)
	if err != nil {
		t.Fatalf("load the brief: %v", err)
	}

	// Act.
	text, err := brief.Splice(map[string]string{"response": "r", "words": "w"})

	// Assert.
	if err != nil {
		t.Fatalf("splice the brief: %v", err)
	}
	for _, want := range []string{"without using any tools", "strong last resort"} {
		if !strings.Contains(text, want) {
			t.Fatalf("the brief lacks %q:\n%s", want, text)
		}
	}
}

func TestARevisionTheSessionNeverAnswersTimesOutAtItsBound(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeShim{hangs: true}, repoPromptsDir)
	h.reviser.deps.Bound = time.Millisecond

	// Act.
	h.reviseAndWait(t, ask("word", "what does this mean"))

	// Assert.
	failed := h.feed.lastState().GetFailed()
	if failed == nil || !strings.Contains(failed.GetText(), "did not answer within 1ms") {
		t.Fatalf("last state = %v, want failed naming the bound", h.feed.lastState())
	}
	record, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "error", Operation: opRun})
	if !ok || record.Context["arm"] != "timed_out" {
		t.Fatalf("error record = %+v (%v), want arm timed_out", record.Context, ok)
	}
}

func TestALandedRevisionIsOwedToTheAgent(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeShim{answer: succeeded}, repoPromptsDir)

	// Act.
	h.reviseAndWait(t, ask("word", "what does this mean"))

	// Assert.
	want := []wsm.OwedRevision{{Unit: "msg_1:0", Original: "1 🔎 the original", Revised: revisedWords, OwedAtNs: revisedAt.UnixNano()}}
	if got := h.owed.recorded(); len(got) != 1 || got[0] != want[0] {
		t.Fatalf("owed = %+v, want %+v", got, want)
	}
}

func TestAFailedRevisionOwesTheAgentNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeShim{answer: func(*shimv1.ReviseResponseRequest) (*shimv1.ReviseResponseResponse, error) {
		return nil, errors.New("connection refused")
	}}, repoPromptsDir)

	// Act.
	h.reviseAndWait(t, ask("word", "what does this mean"))

	// Assert.
	if got := h.owed.recorded(); len(got) != 0 {
		t.Fatalf("owed = %+v, want nothing", got)
	}
}

func TestARevisionThatCannotBeOwedIsRecordedAtError(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeShim{answer: succeeded}, repoPromptsDir)
	h.owed.err = errors.New("disk full")

	// Act.
	h.reviseAndWait(t, ask("word", "what does this mean"))

	// Assert.
	record, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "error", Operation: opRun,
		Message: "the revision landed but could not be recorded for the agent; its next prompt will not say what the response now says"})
	if !ok || record.Context["cause"] != "disk full" || record.Context["unit"] != "msg_1:0" {
		t.Fatalf("records = %+v, want the failed record naming its cause and unit", h.log.Records())
	}
	if h.feed.lastState().GetFailed() != nil {
		t.Fatalf("last state = %v, want the landed revision left standing", h.feed.lastState())
	}
}

func TestNewRefusesAReviserWithNoBound(t *testing.T) {
	// Arrange.
	defer func() {
		// Assert.
		if recover() == nil {
			t.Fatal("New built a Reviser with no bound")
		}
	}()

	// Act.
	New(Deps{Owed: &fakeOwed{}, Now: time.Now})
}

// untilCancelled is a shim whose call answers once its context is cancelled,
// as a Connect call does, signalling STARTED when each call begins.
func untilCancelled(started chan<- struct{}) *fakeShim {
	return &fakeShim{answerCtx: func(ctx context.Context, _ *shimv1.ReviseResponseRequest) (*shimv1.ReviseResponseResponse, error) {
		started <- struct{}{}
		<-ctx.Done()
		return nil, ctx.Err()
	}}
}

// untilReleased is a shim whose first call answers success once RELEASE is
// closed, whatever its context; later calls answer at once.
func untilReleased(started chan<- struct{}, release <-chan struct{}) *fakeShim {
	shim := &fakeShim{}
	shim.answerCtx = func(_ context.Context, req *shimv1.ReviseResponseRequest) (*shimv1.ReviseResponseResponse, error) {
		if len(shim.asked()) == 1 {
			started <- struct{}{}
			<-release
		}
		return succeeded(req)
	}
	return shim
}

func TestStopWithNoRevisionStopsNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeShim{answer: succeeded}, repoPromptsDir)

	// Act.
	stopped := h.reviser.Stop(ws)

	// Assert.
	if stopped != 0 || len(h.feed.states) != 0 || len(h.status.facts()) != 0 {
		t.Fatalf("Stop() = %d, states %v, updating %v, want nothing touched", stopped, h.feed.states, h.status.facts())
	}
}

func TestStopCancelsTheRunningRevisionAndClearsItsState(t *testing.T) {
	// Arrange.
	started := make(chan struct{}, 1)
	h := newHarness(t, untilCancelled(started), repoPromptsDir)
	if err := h.reviser.Revise(ws, ask("word", "what does this mean")); err != nil {
		t.Fatalf("Revise() = %v", err)
	}
	<-started

	// Act.
	stopped := h.reviser.Stop(ws)
	<-h.finished

	// Assert.
	if stopped != 1 {
		t.Fatalf("Stop() = %d, want 1", stopped)
	}
	h.feed.mu.Lock()
	states := append([]*frontendv1.FeedResponseRevision(nil), h.feed.states...)
	h.feed.mu.Unlock()
	if len(states) != 2 || states[0].GetPending() == nil || states[1] != nil {
		t.Fatalf("states = %v, want pending then cleared", states)
	}
}

func TestAStoppedRevisionRecordsNoFailure(t *testing.T) {
	// Arrange.
	started := make(chan struct{}, 1)
	h := newHarness(t, untilCancelled(started), repoPromptsDir)
	if err := h.reviser.Revise(ws, ask("word", "what does this mean")); err != nil {
		t.Fatalf("Revise() = %v", err)
	}
	<-started

	// Act.
	h.reviser.Stop(ws)
	<-h.finished

	// Assert.
	if record, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "error", Operation: opRun}); ok {
		t.Fatalf("a stopped revision logged an error %+v", record.Context)
	}
	if _, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "info", Operation: opRun}); !ok {
		t.Fatalf("a stopped revision's end was not recorded")
	}
}

func TestStopRecordsHowManyItStopped(t *testing.T) {
	// Arrange.
	started := make(chan struct{}, 1)
	h := newHarness(t, untilCancelled(started), repoPromptsDir)
	if err := h.reviser.Revise(ws, ask("word", "what does this mean")); err != nil {
		t.Fatalf("Revise() = %v", err)
	}
	<-started

	// Act.
	h.reviser.Stop(ws)
	<-h.finished

	// Assert.
	record, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "info", Operation: opStop})
	if !ok || record.Context["stopped"] != 1 || record.Context["workspace"] != string(ws) {
		t.Fatalf("stop record = %+v (%v), want stopped 1 for the workspace", record.Context, ok)
	}
}

func TestStopLowersUpdatingOnceTheStoppedCallReturns(t *testing.T) {
	// Arrange.
	started := make(chan struct{}, 1)
	h := newHarness(t, untilCancelled(started), repoPromptsDir)
	if err := h.reviser.Revise(ws, ask("word", "what does this mean")); err != nil {
		t.Fatalf("Revise() = %v", err)
	}
	<-started

	// Act.
	h.reviser.Stop(ws)
	<-h.finished

	// Assert.
	if facts := h.status.facts(); len(facts) != 2 || !facts[0] || facts[1] {
		t.Fatalf("updating facts %v, want [true false]", facts)
	}
}

func TestStopDropsTheRevisionsQueuedBehindTheRunningOne(t *testing.T) {
	// Arrange.
	started := make(chan struct{}, 1)
	h := newHarness(t, untilCancelled(started), repoPromptsDir)
	for _, text := range []string{"first", "second"} {
		if err := h.reviser.Revise(ws, ask("word", text)); err != nil {
			t.Fatalf("Revise() = %v", err)
		}
	}
	<-started

	// Act.
	stopped := h.reviser.Stop(ws)
	<-h.finished

	// Assert.
	if stopped != 2 {
		t.Fatalf("Stop() = %d, want 2", stopped)
	}
	if asked := h.shim.asked(); len(asked) != 1 {
		t.Fatalf("the shim was asked %d times, want only the running revision", len(asked))
	}
	if facts := h.status.facts(); len(facts) != 2 || facts[1] {
		t.Fatalf("updating facts %v, want the lane ended", facts)
	}
}

func TestASecondStopBeforeTheStoppedCallReturnsStopsNothing(t *testing.T) {
	// Arrange.
	started := make(chan struct{}, 1)
	release := make(chan struct{})
	h := newHarness(t, untilReleased(started, release), repoPromptsDir)
	if err := h.reviser.Revise(ws, ask("word", "what does this mean")); err != nil {
		t.Fatalf("Revise() = %v", err)
	}
	<-started
	h.reviser.Stop(ws)

	// Act.
	again := h.reviser.Stop(ws)

	// Assert.
	if again != 0 {
		t.Fatalf("the second Stop() = %d, want 0 so the next press reaches the turn", again)
	}
	close(release)
	<-h.finished
}

func TestARevisionAskedAfterAStopRuns(t *testing.T) {
	// Arrange: the stopped call has not returned when the next is asked.
	started := make(chan struct{}, 1)
	release := make(chan struct{})
	h := newHarness(t, untilReleased(started, release), repoPromptsDir)
	if err := h.reviser.Revise(ws, ask("word", "first")); err != nil {
		t.Fatalf("Revise() = %v", err)
	}
	<-started
	h.reviser.Stop(ws)

	// Act.
	if err := h.reviser.Revise(ws, ask("word", "second")); err != nil {
		t.Fatalf("Revise() = %v", err)
	}
	close(release)
	<-h.finished
	<-h.finished

	// Assert.
	asked := h.shim.asked()
	if len(asked) != 2 || asked[1].GetInstruction().GetContent().GetBlocks()[1].GetText().GetText() != "second" {
		t.Fatalf("the shim was asked %d times, want the second revision run after the stop", len(asked))
	}
	if h.feed.lastState().GetPending() == nil {
		t.Fatalf("last state = %v, want the second revision still pending its page line", h.feed.lastState())
	}
}
