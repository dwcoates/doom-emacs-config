// validation_test.go — SUBJECT 11: the validation invariant on the wire.
//
// An unset non-optional field is ILLEGAL. Every rpc carrying one is answered
// with its typed failure AT ONCE — and the point of "at once" is observable:
// the request never reaches the database, so no storage operation is logged
// for it. Connect-level errors are reserved for transport and malformed
// protobuf; a request the store REFUSES answers HTTP 200 with the failure arm.
package integration

import (
	"testing"

	connect "connectrpc.com/connect"

	storev1 "agentrepl/proto/store/v1"
)

// TestWriteBatchRefusesMalformedEntries covers every required field of the
// write envelope: one edge per case, all refused before any storage happens.
func TestWriteBatchRefusesMalformedEntries(t *testing.T) {
	tests := []struct {
		name  string
		entry func(*producer) *storev1.StoreEntry
	}{
		{
			name: "missing write_id",
			entry: func(p *producer) *storev1.StoreEntry {
				return p.agentEntry("", "u-valid", frameLine(agentID("main"), responseFrame("main", "act-1", "x")))
			},
		},
		{
			name: "missing upsert_key",
			entry: func(p *producer) *storev1.StoreEntry {
				return p.agentEntry("w-valid", "", frameLine(agentID("main"), responseFrame("main", "act-1", "x")))
			},
		},
		{
			name: "missing plane arm",
			entry: func(p *producer) *storev1.StoreEntry {
				e := p.agentEntry("w-valid", "u-valid", frameLine(agentID("main"), responseFrame("main", "act-1", "x")))
				e.Plane = &storev1.Plane{}
				return e
			},
		},
		{
			name: "missing plane message",
			entry: func(p *producer) *storev1.StoreEntry {
				e := p.agentEntry("w-valid", "u-valid", frameLine(agentID("main"), responseFrame("main", "act-1", "x")))
				e.Plane = nil
				return e
			},
		},
		{
			name: "missing entry arm",
			entry: func(p *producer) *storev1.StoreEntry {
				return &storev1.StoreEntry{Plane: p.plane(), WriteId: "w-valid", UpsertKey: "u-valid"}
			},
		},
		{
			name: "missing agent_info arm",
			entry: func(p *producer) *storev1.StoreEntry {
				return p.agentEntry("w-valid", "u-valid", &storev1.StoreAgentUpdate{TopLevel: agentID("main")})
			},
		},
		{
			name: "page line naming no book",
			entry: func(p *producer) *storev1.StoreEntry {
				return p.agentEntry("w-valid", "u-valid", &storev1.StoreAgentUpdate{
					AgentInfo: &storev1.StoreAgentUpdate_ServeableFrame{
						ServeableFrame: &storev1.StorePageLine{
							PageAgentId: agentID(""),
							AgentItem:   frameItem(responseFrame("main", "act-1", "x")),
						},
					},
				})
			},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			store := startStore(t, storeOptions{})
			ctx, cancel := callContext(t)
			defer cancel()
			shim := streamProducer(store.client())
			mark := store.logMark()

			// Act.
			detail := shim.writeExpectingFailure(ctx, t, nil, tc.entry(shim))

			// Assert.
			if detail == "" {
				t.Errorf("the refusal carried no detail")
			}
			records := store.logRecordsAfter(mark)
			assertNoDatabaseTouch(t, records)
			if len(recordsWithContextKey(records, "rpc")) == 0 {
				t.Errorf("the refusal logged no record carrying the rpc correlation key")
			}
		})
	}
}

// TestWriteBatchRefusesAnEmptyBatch: a write of nothing is a producer defect,
// not a no-op to absorb quietly.
func TestWriteBatchRefusesAnEmptyBatch(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	shim := streamProducer(store.client())
	mark := store.logMark()

	// Act.
	detail := shim.writeExpectingFailure(ctx, t, nil)

	// Assert.
	if detail == "" {
		t.Errorf("the empty-batch refusal carried no detail")
	}
	assertNoDatabaseTouch(t, store.logRecordsAfter(mark))
}

// TestOpenAgentSessionRefusesUnsetRequiredFields.
func TestOpenAgentSessionRefusesUnsetRequiredFields(t *testing.T) {
	tests := []struct {
		name string
		req  *storev1.OpenAgentSessionRequest
	}{
		{name: "missing agent", req: &storev1.OpenAgentSessionRequest{PageSize: 10}},
		{name: "empty agent value", req: &storev1.OpenAgentSessionRequest{Agent: agentID(""), PageSize: 10}},
		{name: "zero page_size", req: &storev1.OpenAgentSessionRequest{Agent: agentID("main"), PageSize: 0}},
		{
			name: "empty known_through value",
			req: &storev1.OpenAgentSessionRequest{
				Agent:        agentID("main"),
				PageSize:     10,
				KnownThrough: &storev1.StoreItemPointer{Value: ""},
			},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			store := startStore(t, storeOptions{})
			ctx, cancel := callContext(t)
			defer cancel()
			mark := store.logMark()

			// Act.
			detail := openSessionExpectingFailure(ctx, t, store.client(), tc.req)

			// Assert.
			if detail == "" {
				t.Errorf("the refusal carried no detail")
			}
			assertNoDatabaseTouch(t, store.logRecordsAfter(mark))
		})
	}
}

// TestReadAgentPageRefusesUnsetRequiredFields.
func TestReadAgentPageRefusesUnsetRequiredFields(t *testing.T) {
	tests := []struct {
		name string
		req  *storev1.ReadAgentPageRequest
	}{
		{
			name: "missing book",
			req:  &storev1.ReadAgentPageRequest{PageSize: 10, After: &storev1.StoreItemPointer{Value: "p"}},
		},
		{
			name: "empty book value",
			req:  &storev1.ReadAgentPageRequest{Book: agentID(""), PageSize: 10, After: &storev1.StoreItemPointer{Value: "p"}},
		},
		{
			name: "zero page_size",
			req:  &storev1.ReadAgentPageRequest{Book: agentID("main"), PageSize: 0, After: &storev1.StoreItemPointer{Value: "p"}},
		},
		{
			name: "missing after pointer",
			req:  &storev1.ReadAgentPageRequest{Book: agentID("main"), PageSize: 10},
		},
		{
			name: "empty after pointer value",
			req:  &storev1.ReadAgentPageRequest{Book: agentID("main"), PageSize: 10, After: &storev1.StoreItemPointer{Value: ""}},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			store := startStore(t, storeOptions{})
			ctx, cancel := callContext(t)
			defer cancel()
			mark := store.logMark()

			// Act.
			detail := readPageExpectingFailure(ctx, t, store.client(), tc.req)

			// Assert.
			if detail == "" {
				t.Errorf("the refusal carried no detail")
			}
			assertNoDatabaseTouch(t, store.logRecordsAfter(mark))
		})
	}
}

// TestGetWorkflowRefusesAnUnsetHandle: the not-implemented answer is still an
// answer, so an unset handle must be refused on its own terms first.
func TestGetWorkflowRefusesAnUnsetHandle(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	mark := store.logMark()

	// Act.
	resp, err := store.client().GetWorkflow(ctx, connect.NewRequest(&storev1.GetWorkflowRequest{}))
	if err != nil {
		t.Fatalf("GetWorkflow answered a transport error where a typed failure was owed: %v", err)
	}

	// Assert.
	if resp.Msg.GetFailure() == nil {
		t.Fatalf("GetWorkflow accepted a request with no handle: %v", resp.Msg)
	}
	assertNoDatabaseTouch(t, store.logRecordsAfter(mark))
}

// TestWatchAgentSessionRefusesAnUnsetToken: there is no failure arm on this
// rpc by design, so the refusal is the transport's.
func TestWatchAgentSessionRefusesAnUnsetToken(t *testing.T) {
	tests := []struct {
		name  string
		token *storev1.AgentSessionToken
	}{
		{name: "missing token", token: nil},
		{name: "empty token value", token: &storev1.AgentSessionToken{Value: ""}},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			store := startStore(t, storeOptions{})
			ctx, cancel := callContext(t)
			defer cancel()

			// Act.
			stream := watchStream(ctx, t, store.client(), tc.token)
			defer stream.Close()

			// Assert.
			assertWatchRefused(t, stream)
		})
	}
}
