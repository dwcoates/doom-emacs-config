package sessioncontroller

import (
	"context"
	"sync"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/shimclient"
	"claude-repld/internal/statedb"
)

// THE PROMPT_RECEIPT TABLE'S ONE REMAINING ROW KIND.
//
// The receipt this table was named for is gone: a prompt renders when it
// round-trips through the SDK, so nothing daemon-local stands in for it and
// there is no second identity to reconcile. What is left is the interrupted-turn
// resumption, which was never a prompt and never rendered (turnresumption.go).

// --- the fake ledger --------------------------------------------------------

// fakeReceiptStore is an in-memory PromptReceiptStore that records the order of
// its own writes, so a test can assert what happened BEFORE what.
type fakeReceiptStore struct {
	mu sync.Mutex
	// calls is one entry per mutation, in order ("record-resumption:rd-1",
	// "claim-resumption:rd-1").
	calls []string
	// resumptions is the owed-resumption ledger, in insertion order.
	resumptions []statedb.PendingResumption
	// resumptionRecordErr fails every RecordPendingResumption — a teardown
	// that cannot record what it owes.
	resumptionRecordErr error
	// resumptionsErr fails every resumption read.
	resumptionsErr error
	// resumptionClaimErr fails every ClaimResumptionForDelivery — the store
	// that cannot durably record which re-drive took a turn.
	resumptionClaimErr error
	// resumptionDischargeErr fails every DischargeResumption.
	resumptionDischargeErr error
	// resumptionSweepErr fails every DischargeResumptionsThrough — the context
	// cut whose sweep cannot be written.
	resumptionSweepErr error
}

func newFakeReceiptStore() *fakeReceiptStore { return &fakeReceiptStore{} }

func (f *fakeReceiptStore) RecordPendingResumption(r statedb.PendingResumption) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.calls = append(f.calls, "record-resumption:"+r.RequestID)
	if f.resumptionRecordErr != nil {
		return f.resumptionRecordErr
	}
	r.State = statedb.ResumptionPending
	for i := range f.resumptions {
		if f.resumptions[i].RequestID == r.RequestID {
			// A CLAIMED ROW IS NEVER UN-CLAIMED, exactly as the real store's
			// conflict update refuses to: handing a claimed re-drive back as
			// owed is the duplicate the claim exists to prevent.
			if f.resumptions[i].State == statedb.ResumptionDelivering {
				return nil
			}
			f.resumptions[i] = r
			return nil
		}
	}
	f.resumptions = append(f.resumptions, r)
	return nil
}

func (f *fakeReceiptStore) PendingResumptions(workspace string) ([]statedb.PendingResumption, error) {
	return f.resumptionsInStates(workspace, statedb.ResumptionPending)
}

func (f *fakeReceiptStore) UndischargedResumptions(workspace string) ([]statedb.PendingResumption, error) {
	return f.resumptionsInStates(workspace, statedb.ResumptionPending, statedb.ResumptionDelivering)
}

func (f *fakeReceiptStore) resumptionsInStates(workspace string, states ...statedb.PromptReceiptResumption) ([]statedb.PendingResumption, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.resumptionsErr != nil {
		return nil, f.resumptionsErr
	}
	wanted := map[statedb.PromptReceiptResumption]bool{}
	for _, st := range states {
		wanted[st] = true
	}
	var out []statedb.PendingResumption
	for _, r := range f.resumptions {
		if r.Workspace == workspace && wanted[r.State] {
			out = append(out, r)
		}
	}
	return out, nil
}

// ClaimResumptionForDelivery mirrors the real store's conditional update: only
// a still-pending row can be taken, and it can be taken once.
func (f *fakeReceiptStore) ClaimResumptionForDelivery(requestID string, atMs int64) (bool, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.calls = append(f.calls, "claim-resumption:"+requestID)
	if f.resumptionClaimErr != nil {
		return false, f.resumptionClaimErr
	}
	for i := range f.resumptions {
		if f.resumptions[i].RequestID != requestID || f.resumptions[i].State != statedb.ResumptionPending {
			continue
		}
		f.resumptions[i].State = statedb.ResumptionDelivering
		f.resumptions[i].DeliveryStartedAtMs = atMs
		return true, nil
	}
	return false, nil
}

func (f *fakeReceiptStore) DischargeResumption(requestID string) (bool, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.calls = append(f.calls, "discharge-resumption:"+requestID)
	if f.resumptionDischargeErr != nil {
		return false, f.resumptionDischargeErr
	}
	for i := range f.resumptions {
		if f.resumptions[i].RequestID == requestID {
			f.resumptions = append(f.resumptions[:i], f.resumptions[i+1:]...)
			return true, nil
		}
	}
	return false, nil
}

// DischargeResumptionsThrough mirrors the real store's context-cut sweep: every
// resumption at or below the cut goes, claimed or not.
func (f *fakeReceiptStore) DischargeResumptionsThrough(workspace string, throughMs int64) (int, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.calls = append(f.calls, "discharge-resumptions-through:"+workspace)
	if f.resumptionSweepErr != nil {
		return 0, f.resumptionSweepErr
	}
	kept := make([]statedb.PendingResumption, 0, len(f.resumptions))
	n := 0
	for _, r := range f.resumptions {
		if r.Workspace == workspace && r.InterruptedAtMs <= throughMs {
			n++
			continue
		}
		kept = append(kept, r)
	}
	f.resumptions = kept
	return n, nil
}

func (f *fakeReceiptStore) owedResumptions(workspace string) []statedb.PendingResumption {
	rows, err := f.PendingResumptions(workspace)
	if err != nil {
		return nil
	}
	return rows
}

func (f *fakeReceiptStore) callLog() []string {
	f.mu.Lock()
	defer f.mu.Unlock()
	return append([]string(nil), f.calls...)
}

// --- the submit path --------------------------------------------------------

// orderingPusher is a Pusher that appends to a SHARED trace, so every pushed
// conversation delta lands on one ordered timeline.
type orderingPusher struct {
	fakePusher
	mu    *sync.Mutex
	trace *[]string
}

func (p *orderingPusher) PushDetachedWorkDelta(*frontendv1.DetachedWorkDelta) {}
func (p *orderingPusher) PushConversationDelta(cd *frontendv1.ConversationDelta) {
	p.mu.Lock()
	*p.trace = append(*p.trace, "push:"+cd.GetMessages()[0].GetRequestId())
	p.mu.Unlock()
	p.fakePusher.PushConversationDelta(cd)
}

// submitHarness is a Manager that submits through a fake shim, with the
// resumption ledger and the frontend push sharing one ordered trace.
type submitHarness struct {
	m          *Manager
	receipts   *fakeReceiptStore
	lastClient func() *fakeClient
	traceMu    *sync.Mutex
	trace      *[]string
	// log captures the manager's own record, for the assertions that pin a
	// decision's canonical line rather than only its effect.
	log *logCapture
}

func newSubmitHarness(t *testing.T) *submitHarness {
	t.Helper()
	return newSubmitHarnessWith(t, nil)
}

// newSubmitHarnessWith is newSubmitHarness with a hook run on each fake shim
// client as it is built, BEFORE any prompt can reach it.
//
// The refusal it exists for has to be armed ahead of the very first submit:
// the client is created by the bring-up that submit performs, so a test that
// waited for `lastClient` to exist could only ever arm the SECOND prompt.
func newSubmitHarnessWith(t *testing.T, prepare func(*fakeClient)) *submitHarness {
	t.Helper()
	var (
		traceMu sync.Mutex
		trace   []string
		mu      sync.Mutex
		last    *fakeClient
	)
	receipts := newFakeReceiptStore()
	cl := &logCapture{}
	m, err := New(Config{
		Logf:              cl.logf,
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
			if prepare != nil {
				prepare(fc)
			}
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
		log:        cl,
	}
}

// wireSession brings the harness's session up and JOINS the owed-resumption
// drive that wiring launches, so a row seeded afterwards cannot be raced by it.
//
// IT IS NOT A CONVENIENCE. noteWired is the level-trigger for the resumption
// driver, so a submit against an UNWIRED session brings the session up and,
// with it, a goroutine that concurrently claims whatever the test seeded — a
// test asserting on the owed set is then racing the daemon doing its job. The
// bring-up is driven by a real submit, which returns only once awaitDriveable
// has run noteWired, and the count is taken before the goroutine starts; so by
// the time Wait returns, the drive has read an EMPTY owed set and exited, and
// nothing re-arms it until another bring-up. The overlap is unrepresentable
// rather than merely ordered.
func (h *submitHarness) wireSession(t *testing.T) {
	t.Helper()
	if _, err := h.m.submitPromptAs(context.Background(), "ws", "wire-warmup", "respond with only '.'", "",
		"keep-alive", testPromptOrigin, submitterKeepAlive, leavesParkedPermissions); err != nil {
		t.Fatalf("wiring submit: %v", err)
	}
	h.m.resumptionDrives.Wait()
}

func (h *submitHarness) traced() []string {
	h.traceMu.Lock()
	defer h.traceMu.Unlock()
	return append([]string(nil), *h.trace...)
}

// receiptConsumer is a bare consumer wired to the prompt_receipt ledger — the
// object the resumption discharge runs on.
func receiptConsumer(t *testing.T, receipts PromptReceiptStore) *consumer {
	t.Helper()
	cons := newConsumer("ws", "s1", &fakePusher{}, &fakeApplier{}, nil, newFakeClearCompactStore(), emptyTurnAccountingStore{}, t.Logf, nil, nil, nil, nil, nil)
	cons.receipts = receipts
	return cons
}
