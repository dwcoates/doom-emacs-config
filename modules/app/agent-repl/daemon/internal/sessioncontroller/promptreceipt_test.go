package sessioncontroller

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"sync"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/shimclient"
	"claude-repld/internal/statedb"

	"google.golang.org/protobuf/types/known/anypb"
)

// NO SUBMITTED PROMPT MAY SILENTLY VANISH.
//
// The receipt bubble the daemon pushes at submit used to live only in memory,
// so a prompt accepted and not yet carried by the vendor's durable transcript
// disappeared with the daemon that accepted it. These tests pin the three
// claims that close that: the record is written BEFORE the user can see the
// bubble, it is retired the moment the conversation itself carries the prompt,
// and a durable replay serves whatever is left over.

// --- the fake ledger --------------------------------------------------------

// fakeReceiptStore is an in-memory PromptReceiptStore that records the order of
// its own writes, so a test can assert what happened BEFORE what.
type fakeReceiptStore struct {
	mu sync.Mutex
	// rows is the outstanding ledger, in insertion order.
	rows []statedb.PromptReceipt
	// calls is one entry per mutation, in order ("record:r-1", "retire:r-1",
	// "retire-ws:/ws").
	calls []string
	// recordErr fails every Record — the unwritable state store.
	recordErr error
	// outstandingErr fails every read — the unreadable receipt table.
	outstandingErr error
}

func newFakeReceiptStore() *fakeReceiptStore { return &fakeReceiptStore{} }

func (f *fakeReceiptStore) Record(r statedb.PromptReceipt) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.calls = append(f.calls, "record:"+r.RequestID)
	if f.recordErr != nil {
		return f.recordErr
	}
	for i := range f.rows {
		if f.rows[i].RequestID == r.RequestID {
			f.rows[i] = r
			return nil
		}
	}
	f.rows = append(f.rows, r)
	return nil
}

func (f *fakeReceiptStore) Retire(requestID string) (bool, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.calls = append(f.calls, "retire:"+requestID)
	for i := range f.rows {
		if f.rows[i].RequestID == requestID {
			f.rows = append(f.rows[:i], f.rows[i+1:]...)
			return true, nil
		}
	}
	return false, nil
}

func (f *fakeReceiptStore) RetireWorkspace(workspace string, throughMs int64) (int, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.calls = append(f.calls, "retire-ws:"+workspace)
	kept := make([]statedb.PromptReceipt, 0, len(f.rows))
	n := 0
	for _, r := range f.rows {
		if r.Workspace == workspace && r.AcceptedAtMs <= throughMs {
			n++
			continue
		}
		kept = append(kept, r)
	}
	f.rows = kept
	return n, nil
}

func (f *fakeReceiptStore) Outstanding(workspace string) ([]statedb.PromptReceipt, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.outstandingErr != nil {
		return nil, f.outstandingErr
	}
	var out []statedb.PromptReceipt
	for _, r := range f.rows {
		if r.Workspace == workspace {
			out = append(out, r)
		}
	}
	return out, nil
}

func (f *fakeReceiptStore) seed(rows ...statedb.PromptReceipt) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.rows = append(f.rows, rows...)
}

func (f *fakeReceiptStore) callLog() []string {
	f.mu.Lock()
	defer f.mu.Unlock()
	return append([]string(nil), f.calls...)
}

func (f *fakeReceiptStore) outstandingIDs(workspace string) []string {
	rows, err := f.Outstanding(workspace)
	if err != nil {
		return nil
	}
	var out []string
	for _, r := range rows {
		out = append(out, r.RequestID)
	}
	return out
}

// --- the submit path --------------------------------------------------------

// orderingPusher is a Pusher that appends to a SHARED trace, so the receipt
// push and the durable record land on one timeline.
type orderingPusher struct {
	fakePusher
	mu    *sync.Mutex
	trace *[]string
}

func (p *orderingPusher) PushAsyncBubbleDelta(*frontendv1.AsyncBubbleDelta) {}
func (p *orderingPusher) PushConversationDelta(cd *frontendv1.ConversationDelta) {
	p.mu.Lock()
	*p.trace = append(*p.trace, "push:"+cd.GetItems()[0].GetRequestId())
	p.mu.Unlock()
	p.fakePusher.PushConversationDelta(cd)
}

// receiptTracingStore is a fakeReceiptStore whose writes land on the SAME
// shared trace the pusher above writes to.
type receiptTracingStore struct {
	fakeReceiptStore
	mu    *sync.Mutex
	trace *[]string
}

func (s *receiptTracingStore) Record(r statedb.PromptReceipt) error {
	s.mu.Lock()
	*s.trace = append(*s.trace, "record:"+r.RequestID)
	s.mu.Unlock()
	return s.fakeReceiptStore.Record(r)
}

// submitHarness is a Manager that submits through a fake shim, with the
// durable receipt ledger and the frontend push sharing one ordered trace.
type submitHarness struct {
	m          *Manager
	receipts   *receiptTracingStore
	lastClient func() *fakeClient
	traceMu    *sync.Mutex
	trace      *[]string
}

func newSubmitHarness(t *testing.T) *submitHarness {
	t.Helper()
	var (
		traceMu sync.Mutex
		trace   []string
		mu      sync.Mutex
		last    *fakeClient
	)
	receipts := &receiptTracingStore{mu: &traceMu, trace: &trace}
	m, err := New(Config{
		Push:              &orderingPusher{mu: &traceMu, trace: &trace},
		SSM:               &fakeApplier{},
		Spawner:           &fakeSpawner{},
		Locator:           fakeLocator{m: map[string]string{"ws": "s1"}},
		SeqStore:          &fakeSeqStore{seq: map[string]uint64{}},
		ClearCompactStore: newFakeClearCompactStore(),
		TurnAccountings:   emptyTurnAccountingStore{},
		PromptReceipts:    receipts,
		ProtocolVersion:   "1",
		Source:            stubSource{},
		FileDiagnostics:   fakeFileDiagnosticPersister{},
		newClient: func(cfg shimclient.Config) sessionClient {
			fc := &fakeClient{cfg: cfg}
			mu.Lock()
			last = fc
			mu.Unlock()
			return fc
		},
	})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	t.Cleanup(m.Close)
	return &submitHarness{
		m:          m,
		receipts:   receipts,
		lastClient: func() *fakeClient { mu.Lock(); defer mu.Unlock(); return last },
		traceMu:    &traceMu,
		trace:      &trace,
	}
}

func (h *submitHarness) traced() []string {
	h.traceMu.Lock()
	defer h.traceMu.Unlock()
	return append([]string(nil), *h.trace...)
}

func TestAnAcceptedPromptIsRecordedBeforeItsReceiptIsPushed(t *testing.T) {
	// Arrange — a receipt the user has seen must never be unrecoverable, so
	// the durable write cannot come after the bubble.
	h := newSubmitHarness(t)

	// Act.
	if err := h.m.SubmitPrompt(context.Background(), "ws", "r-1", "the prompt", "default", testPromptOrigin); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}

	// Assert.
	got := h.traced()
	if len(got) != 2 || got[0] != "record:r-1" || got[1] != "push:r-1" {
		t.Fatalf("trace = %v, want the durable record strictly before the pushed receipt", got)
	}
}

func TestAnAcceptedPromptIsRecordedWithTheTextTheUserTyped(t *testing.T) {
	// Arrange.
	h := newSubmitHarness(t)

	// Act.
	if err := h.m.SubmitPrompt(context.Background(), "ws", "r-1", "the prompt", "default", testPromptOrigin); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}

	// Assert.
	rows, err := h.receipts.Outstanding("ws")
	if err != nil {
		t.Fatalf("Outstanding: %v", err)
	}
	if len(rows) != 1 || rows[0].Text != "the prompt" || rows[0].RequestID != "r-1" {
		t.Fatalf("recorded receipts = %+v, want the submitted prompt under its request id", rows)
	}
}

func TestTheRecordedInstantIsTheOneTheReceiptBubbleCarries(t *testing.T) {
	// Arrange — a replayed receipt is stamped from the RECORD's instant, so a
	// disagreement here would give the replayed bubble a different provenance
	// verdict than the one the user saw.
	h := newSubmitHarness(t)

	// Act.
	if err := h.m.SubmitPrompt(context.Background(), "ws", "r-1", "the prompt", "default", testPromptOrigin); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}

	// Assert.
	rows, err := h.receipts.Outstanding("ws")
	if err != nil {
		t.Fatalf("Outstanding: %v", err)
	}
	pusher := h.m.cfg.Push.(*orderingPusher)
	pusher.fakePusher.mu.Lock()
	defer pusher.fakePusher.mu.Unlock()
	if len(pusher.fakePusher.convo) != 1 {
		t.Fatalf("pushed %d deltas, want the one receipt", len(pusher.fakePusher.convo))
	}
	if got := pusher.fakePusher.convo[0].GetItems()[0].GetTsMs(); got != rows[0].AcceptedAtMs {
		t.Fatalf("receipt ts_ms = %d, recorded accepted_at_ms = %d; they must be the same instant", got, rows[0].AcceptedAtMs)
	}
}

func TestAnUnwritableReceiptLedgerFailsTheSubmit(t *testing.T) {
	// Arrange — carrying on would hand a shim a prompt the daemon has no
	// record of, which is the loss this mechanism exists to end.
	h := newSubmitHarness(t)
	h.receipts.recordErr = errors.New("disk I/O error")

	// Act.
	err := h.m.SubmitPrompt(context.Background(), "ws", "r-1", "the prompt", "default", testPromptOrigin)

	// Assert.
	if err == nil {
		t.Fatal("a submit whose durable receipt could not be written reported success")
	}
	if !strings.Contains(err.Error(), "disk I/O error") {
		t.Fatalf("submit error = %v, want it to carry the ledger's cause", err)
	}
}

func TestAnUnwritableReceiptLedgerWithholdsThePromptFromTheShim(t *testing.T) {
	// Arrange — the record is part of the acceptance, so its failure must
	// leave the session exactly as it was found.
	h := newSubmitHarness(t)
	h.receipts.recordErr = errors.New("disk I/O error")

	// Act.
	_ = h.m.SubmitPrompt(context.Background(), "ws", "r-1", "the prompt", "default", testPromptOrigin)

	// Assert.
	fc := h.lastClient()
	if fc != nil && len(fc.promptTexts()) != 0 {
		t.Fatalf("prompts forwarded to the shim = %v, want none", fc.promptTexts())
	}
}

func TestASubmitWithNoRequestIdIsRejectedBeforeReceiptMutation(t *testing.T) {
	h := newSubmitHarness(t)

	if err := h.m.SubmitPrompt(context.Background(), "ws", "", "the prompt", "default", testPromptOrigin); err == nil {
		t.Fatal("SubmitPrompt accepted an empty request id")
	}

	if got := h.receipts.callLog(); len(got) != 0 {
		t.Fatalf("ledger calls = %v, want none for rejected submit", got)
	}
}

// --- retirement -------------------------------------------------------------

// receiptConsumer is a bare consumer wired to a receipt ledger — the object
// attributeUserTurn runs on.
func receiptConsumer(t *testing.T, receipts PromptReceiptStore) *consumer {
	t.Helper()
	cons := newConsumer("ws", "s1", &fakePusher{}, &fakeApplier{}, nil, newFakeClearCompactStore(), emptyTurnAccountingStore{}, t.Logf, nil, nil, nil, nil, nil)
	cons.receipts = receipts
	return cons
}

// userDelta is a translated conversation delta carrying one user prompt.
func userDelta(uuid, requestID, text string) *frontendv1.ConversationDelta {
	return &frontendv1.ConversationDelta{
		Workspace: "ws",
		Items: []*frontendv1.ConversationItem{{
			Uuid:      uuid,
			RequestId: requestID,
			Item: &frontendv1.ConversationItem_UserMessage{UserMessage: &datav1.ApiUserMessage{
				Content: &datav1.ApiUserMessage_ContentString{ContentString: text},
			}},
		}},
	}
}

func TestADurableLineNamingItsRequestRetiresTheReceipt(t *testing.T) {
	// Arrange — the transcript now carries the prompt at its real seq, so the
	// daemon-local record is redundant.
	receipts := newFakeReceiptStore()
	receipts.seed(statedb.PromptReceipt{RequestID: "r-1", Workspace: "ws", Text: "the prompt", AcceptedAtMs: 1_000})
	cons := receiptConsumer(t, receipts)
	cons.pushUserEcho("r-1", "the prompt", 1_000)

	// Act.
	cons.attributeUserTurn(userDelta("u-1", "r-1", "the prompt"))

	// Assert.
	if got := receipts.outstandingIDs("ws"); len(got) != 0 {
		t.Fatalf("outstanding receipts = %v, want the superseded one retired", got)
	}
}

func TestADurableLineWithNoRequestIdRetiresTheOldestReceipt(t *testing.T) {
	// Arrange — a transcript UserLine carries no request id, so the oldest
	// outstanding submit is the one it answers.
	receipts := newFakeReceiptStore()
	receipts.seed(statedb.PromptReceipt{RequestID: "r-1", Workspace: "ws", Text: "the prompt", AcceptedAtMs: 1_000})
	cons := receiptConsumer(t, receipts)
	cons.pushUserEcho("r-1", "the prompt", 1_000)

	// Act.
	cons.attributeUserTurn(userDelta("u-1", "", "the prompt"))

	// Assert.
	if got := receipts.outstandingIDs("ws"); len(got) != 0 {
		t.Fatalf("outstanding receipts = %v, want the attributed one retired", got)
	}
}

func TestASecondDurableLineForARetiredReceiptChangesNothing(t *testing.T) {
	// Arrange — retirement runs from several places and any may run second,
	// so it must be idempotent rather than an anomaly.
	receipts := newFakeReceiptStore()
	receipts.seed(statedb.PromptReceipt{RequestID: "r-1", Workspace: "ws", Text: "the prompt", AcceptedAtMs: 1_000})
	cons := receiptConsumer(t, receipts)
	cons.pushUserEcho("r-1", "the prompt", 1_000)
	cons.attributeUserTurn(userDelta("u-1", "r-1", "the prompt"))

	// Act.
	cons.attributeUserTurn(userDelta("u-1", "r-1", "the prompt"))

	// Assert — the second delivery claims no in-memory echo, so it reaches the
	// ledger not at all, and the ledger stays empty either way.
	if got := receipts.outstandingIDs("ws"); len(got) != 0 {
		t.Fatalf("outstanding receipts = %v, want none after a repeated durable line", got)
	}
}

func TestAContextCutRetiresTheWorkspacesReceipts(t *testing.T) {
	// Arrange — a clear discards the history below it, and a receipt for a
	// prompt from below that line would replay pre-cut text above the floor.
	receipts := newFakeReceiptStore()
	receipts.seed(statedb.PromptReceipt{RequestID: "r-1", Workspace: "ws", Text: "the prompt", AcceptedAtMs: 1_000})
	cons := receiptConsumer(t, receipts)
	cons.pushUserEcho("r-1", "the prompt", 1_000)

	// Act.
	cons.Consume(&corev1.Event{
		SessionId: "vendor-uuid", Seq: 9, ProducedAtMs: 9_000,
		Payload: &corev1.Event_ContextCleared{ContextCleared: &corev1.ContextCleared{}},
	})

	// Assert.
	if got := receipts.outstandingIDs("ws"); len(got) != 0 {
		t.Fatalf("outstanding receipts = %v, want the cut to have swept them", got)
	}
}

func TestARetirementFailureIsLoggedRatherThanStoppingTheConversation(t *testing.T) {
	// Arrange — a bookkeeping row that will not delete is not a reason to stop
	// delivering the conversation.
	var mu sync.Mutex
	var logged []string
	receipts := &erroringRetireStore{fakeReceiptStore: *newFakeReceiptStore(), err: errors.New("database is locked")}
	cons := newConsumer("ws", "s1", &fakePusher{}, &fakeApplier{}, nil, newFakeClearCompactStore(), emptyTurnAccountingStore{}, func(f string, a ...any) {
		mu.Lock()
		logged = append(logged, fmt.Sprintf(f, a...))
		mu.Unlock()
	}, nil, nil, nil, nil, nil)
	cons.receipts = receipts
	cons.pushUserEcho("r-1", "the prompt", 1_000)

	// Act.
	cons.attributeUserTurn(userDelta("u-1", "r-1", "the prompt"))

	// Assert.
	mu.Lock()
	defer mu.Unlock()
	for _, l := range logged {
		if strings.Contains(l, "durable prompt receipt retirement FAILED") && strings.Contains(l, "database is locked") {
			return
		}
	}
	t.Fatalf("no loud retirement-failure line; lines=%v", logged)
}

// erroringRetireStore fails every Retire.
type erroringRetireStore struct {
	fakeReceiptStore
	err error
}

func (s *erroringRetireStore) Retire(string) (bool, error) { return false, s.err }

// --- the durable replay -----------------------------------------------------

// durableUserEvent is a stored user prompt at seq, stamped at tsMs and
// optionally naming the request it answers.
func durableUserEvent(t *testing.T, seq uint64, uuid, requestID, text string, tsMs int64) *corev1.Event {
	t.Helper()
	a, err := anypb.New(&datav1.ClaudeStreamMessage{
		Msg: &datav1.ClaudeStreamMessage_User{User: &datav1.UserMessage{
			Uuid: uuid,
			Message: &datav1.ApiUserMessage{
				Content: &datav1.ApiUserMessage_ContentString{ContentString: text},
			},
		}},
	})
	if err != nil {
		t.Fatalf("anypb.New: %v", err)
	}
	return &corev1.Event{
		SessionId: "vendor-uuid", Seq: seq, ProducedAtMs: tsMs, RequestId: requestID,
		Payload: &corev1.Event_Vendor{Vendor: a},
	}
}

// receiptItems returns every pushed prompt-receipt item, in push order.
func (h *durableHarness) receiptItems() []*frontendv1.ConversationItem {
	h.push.mu.Lock()
	defer h.push.mu.Unlock()
	var out []*frontendv1.ConversationItem
	for _, cd := range h.push.convo {
		for _, it := range cd.GetItems() {
			if strings.HasPrefix(it.GetUuid(), "prompt-echo:") {
				out = append(out, it)
			}
		}
	}
	return out
}

func TestADurableReplayServesAnUnretiredReceiptExactlyOnce(t *testing.T) {
	// Arrange — the daemon died between accepting the prompt and the vendor
	// making it durable, so the store has no copy of it at all.
	history := &durableHistorySpy{events: []*corev1.Event{durableAssistantEvent(t, 1, "u1", 1_000)}}
	h := newDurableHarness(t, history)
	h.receipts.seed(statedb.PromptReceipt{RequestID: "r-1", Workspace: "ws", Text: "the lost prompt", AcceptedAtMs: 2_000})

	// Act.
	if err := h.m.Resync("ws", 0); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert.
	got := h.receiptItems()
	if len(got) != 1 {
		t.Fatalf("served %d receipt bubbles, want exactly 1", len(got))
	}
	if got[0].GetRequestId() != "r-1" || got[0].GetUserMessage().GetContentString() != "the lost prompt" {
		t.Fatalf("served receipt = %+v, want the accepted prompt under its request id", got[0])
	}
}

func TestADurableReplayServesAReceiptAfterTheStoresOwnEvents(t *testing.T) {
	// Arrange — the prompt is the most recent thing that happened to the
	// workspace, so its bubble belongs at the bottom of the feed.
	history := &durableHistorySpy{events: []*corev1.Event{durableAssistantEvent(t, 1, "u1", 1_000)}}
	h := newDurableHarness(t, history)
	h.receipts.seed(statedb.PromptReceipt{RequestID: "r-1", Workspace: "ws", Text: "the lost prompt", AcceptedAtMs: 2_000})

	// Act.
	if err := h.m.Resync("ws", 0); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert.
	h.push.mu.Lock()
	defer h.push.mu.Unlock()
	last := h.push.convo[len(h.push.convo)-1]
	if got := last.GetItems()[0].GetUuid(); got != "prompt-echo:r-1" {
		t.Fatalf("last pushed item uuid = %q, want the receipt", got)
	}
}

func TestAServedReceiptCarriesTheProvenanceOfItsAcceptInstant(t *testing.T) {
	// Arrange — the prompt was accepted inside a merge window that has since
	// closed, and the durable ledger still says so.
	history := &durableHistorySpy{}
	h := newDurableHarness(t, history)
	h.applier.mergeWindows = map[string][][2]int64{"ws": {{1_000, 3_000}}}
	h.receipts.seed(statedb.PromptReceipt{RequestID: "r-1", Workspace: "ws", Text: "the merge's prompt", AcceptedAtMs: 2_000})

	// Act.
	if err := h.m.Resync("ws", 0); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert.
	got := h.receiptItems()
	if len(got) != 1 || got[0].GetSource() != frontendv1.ConversationSource_CONVERSATION_SOURCE_MERGE {
		t.Fatalf("served receipt source = %+v, want CONVERSATION_SOURCE_MERGE from the lease ledger", got)
	}
}

func TestAServedReceiptOutsideEveryMergeWindowIsTheUsers(t *testing.T) {
	// Arrange — the same path must not rewrite a user's prompt as a merge's.
	history := &durableHistorySpy{}
	h := newDurableHarness(t, history)
	h.applier.mergeWindows = map[string][][2]int64{"ws": {{1_000, 3_000}}}
	h.receipts.seed(statedb.PromptReceipt{RequestID: "r-1", Workspace: "ws", Text: "the user's prompt", AcceptedAtMs: 9_000})

	// Act.
	if err := h.m.Resync("ws", 0); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert.
	got := h.receiptItems()
	if len(got) != 1 || got[0].GetSource() != frontendv1.ConversationSource_CONVERSATION_SOURCE_USER {
		t.Fatalf("served receipt source = %+v, want CONVERSATION_SOURCE_USER", got)
	}
}

func TestADurableReplaySuppressesAReceiptTheStoreAlreadyNamed(t *testing.T) {
	// Arrange — the store's own copy of the prompt NAMES the request, which
	// settles that the conversation already carries it.
	history := &durableHistorySpy{events: []*corev1.Event{
		durableUserEvent(t, 1, "u1", "r-1", "the prompt", 3_000),
	}}
	h := newDurableHarness(t, history)
	h.receipts.seed(statedb.PromptReceipt{RequestID: "r-1", Workspace: "ws", Text: "the prompt", AcceptedAtMs: 2_000})

	// Act.
	if err := h.m.Resync("ws", 0); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert.
	if got := h.receiptItems(); len(got) != 0 {
		t.Fatalf("served %d receipt bubbles, want none beside the store's own copy of the prompt", len(got))
	}
}

func TestADurableReplayRetiresAReceiptTheStoreAlreadyCarries(t *testing.T) {
	// Arrange — this is the retirement the daemon that accepted the prompt
	// never lived to make.
	history := &durableHistorySpy{events: []*corev1.Event{
		durableUserEvent(t, 1, "u1", "r-1", "the prompt", 3_000),
	}}
	h := newDurableHarness(t, history)
	h.receipts.seed(statedb.PromptReceipt{RequestID: "r-1", Workspace: "ws", Text: "the prompt", AcceptedAtMs: 2_000})

	// Act.
	if err := h.m.Resync("ws", 0); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert.
	if got := h.receipts.outstandingIDs("ws"); len(got) != 0 {
		t.Fatalf("outstanding receipts = %v, want the replay to have retired the redundant one", got)
	}
}

func TestADurableReplaySuppressesAReceiptTheStoreCarriesWithoutARequestId(t *testing.T) {
	// Arrange — a transcript UserLine carries no request id, so text at or
	// after the accept instant is the only thing relating the two.
	history := &durableHistorySpy{events: []*corev1.Event{
		durableUserEvent(t, 1, "u1", "", "the prompt", 3_000),
	}}
	h := newDurableHarness(t, history)
	h.receipts.seed(statedb.PromptReceipt{RequestID: "r-1", Workspace: "ws", Text: "the prompt", AcceptedAtMs: 2_000})

	// Act.
	if err := h.m.Resync("ws", 0); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert.
	if got := h.receiptItems(); len(got) != 0 {
		t.Fatalf("served %d receipt bubbles, want none: the store already drew this prompt", len(got))
	}
}

func TestAnIdenticalPromptFromBeforeTheAcceptDoesNotSuppressTheReceipt(t *testing.T) {
	// Arrange — the user typed the same text earlier in the conversation. That
	// older turn is a different submit, and it must not discard the evidence of
	// the one still outstanding.
	history := &durableHistorySpy{events: []*corev1.Event{
		durableUserEvent(t, 1, "u1", "", "continue", 1_000),
	}}
	h := newDurableHarness(t, history)
	h.receipts.seed(statedb.PromptReceipt{RequestID: "r-1", Workspace: "ws", Text: "continue", AcceptedAtMs: 5_000})

	// Act.
	if err := h.m.Resync("ws", 0); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert.
	if got := h.receiptItems(); len(got) != 1 {
		t.Fatalf("served %d receipt bubbles, want 1: an older identical prompt is a different submit", len(got))
	}
}

func TestAStaleReceiptIsStillServedAndSaidToBeStale(t *testing.T) {
	// Arrange — an ancient receipt is an anomaly worth naming, never evidence
	// worth discarding.
	history := &durableHistorySpy{}
	h := newDurableHarness(t, history)
	h.receipts.seed(statedb.PromptReceipt{RequestID: "r-1", Workspace: "ws", Text: "the ancient prompt", AcceptedAtMs: 1})

	// Act.
	if err := h.m.Resync("ws", 0); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert.
	if got := h.receiptItems(); len(got) != 1 {
		t.Fatalf("served %d receipt bubbles, want the stale one served anyway", len(got))
	}
	for _, l := range h.logLines() {
		if strings.Contains(l, "durable prompt receipt STALE") && strings.Contains(l, "r-1") {
			return
		}
	}
	t.Fatalf("no stale-receipt line; lines=%v", h.logLines())
}

func TestAServedReceiptIsLoggedWithItsWorkspaceRequestAndAge(t *testing.T) {
	// Arrange — a receipt served on replay is the only record of its prompt,
	// so it is findable in the shared log.
	history := &durableHistorySpy{}
	h := newDurableHarness(t, history)
	h.receipts.seed(statedb.PromptReceipt{RequestID: "r-1", Workspace: "ws", Text: "the lost prompt", AcceptedAtMs: 2_000})

	// Act.
	if err := h.m.Resync("ws", 0); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert.
	for _, l := range h.logLines() {
		if strings.Contains(l, "durable prompt receipt SERVED") &&
			strings.Contains(l, `ws="ws"`) && strings.Contains(l, "request_id=r-1") && strings.Contains(l, "age_ms=") {
			return
		}
	}
	t.Fatalf("no served-receipt line carrying workspace, request id and age; lines=%v", h.logLines())
}

func TestAnUnreadableReceiptLedgerFailsTheResync(t *testing.T) {
	// Arrange — a frontend cannot tell "no receipts" from "the receipts could
	// not be read", so the daemon must say which one it means.
	history := &durableHistorySpy{}
	h := newDurableHarness(t, history)
	h.receipts.outstandingErr = errors.New("no such table: prompt_receipt")

	// Act.
	err := h.m.Resync("ws", 0)

	// Assert.
	if err == nil {
		t.Fatal("an unreadable receipt ledger served a silently receipt-free resync")
	}
	if !strings.Contains(err.Error(), "no such table: prompt_receipt") {
		t.Fatalf("resync error = %v, want it to carry the ledger's cause", err)
	}
}

func TestAnUnreadableReceiptLedgerIsLoggedWithItsCause(t *testing.T) {
	// Arrange.
	history := &durableHistorySpy{}
	h := newDurableHarness(t, history)
	h.receipts.outstandingErr = errors.New("no such table: prompt_receipt")

	// Act.
	_ = h.m.Resync("ws", 0)

	// Assert.
	for _, l := range h.logLines() {
		if strings.Contains(l, "durable prompt receipts UNREADABLE") && strings.Contains(l, "no such table") {
			return
		}
	}
	t.Fatalf("no loud unreadable-ledger line; lines=%v", h.logLines())
}

func TestALiveWorkspaceServesItsReceiptsFromTheRetainedRingAlone(t *testing.T) {
	// Arrange — a live session controller still holds the receipts in memory
	// and replays them itself (consumer.resync), so reading the ledger too
	// would draw every outstanding bubble twice.
	history := &durableHistorySpy{}
	h := newDurableHarness(t, history)
	if err := h.m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	d, err := h.m.existing("ws")
	if err != nil {
		t.Fatalf("existing: %v", err)
	}
	d.consumer.pushUserEcho("r-1", "the prompt", 2_000)
	h.receipts.seed(statedb.PromptReceipt{RequestID: "r-1", Workspace: "ws", Text: "the prompt", AcceptedAtMs: 2_000})

	// Act.
	if err := h.m.Resync("ws", 0); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert — one bubble from the submit itself plus one from the ring's
	// replay, and none from the ledger.
	if got := h.receiptItems(); len(got) != 2 {
		t.Fatalf("receipt bubbles = %d, want 2 (the live push and the ring replay)", len(got))
	}
}
