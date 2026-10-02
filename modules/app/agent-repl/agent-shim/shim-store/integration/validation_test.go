// validation_test.go — SUBJECT 11: the validation invariant on the wire.
//
// An unset non-optional field is ILLEGAL. Every rpc carrying one is answered
// with its typed failure AT ONCE — and the point of "at once" is observable:
// the request never reaches the database, so no storage operation is logged
// for it. Connect-level errors are reserved for transport and malformed
// protobuf; a request the store REFUSES answers HTTP 200 with the failure arm.
package integration

import (
	"agentrepl/shim-store/internal/testclose"
	"testing"

	connect "connectrpc.com/connect"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
)

// TestWriteBatchRefusesMalformedEntries covers every required field of the
// write envelope: one edge per case, all refused before any storage happens.
func TestWriteBatchRefusesMalformedEntries(t *testing.T) {
	tests := []struct {
		name      string
		entry     func(*producer) *storev1.StoreEntry
		wantField string
		wantSite  string
	}{
		{
			name:      "missing write_id",
			wantField: "entries[0].write_id",
			wantSite:  "entry_write_id_empty",
			entry: func(p *producer) *storev1.StoreEntry {
				return p.agentEntry("", "u-valid", frameLine(agentID("main"), responseFrame("main", "act-1", "x")))
			},
		},
		{
			name:      "missing upsert_key",
			wantField: "entries[0].upsert_key",
			wantSite:  "entry_upsert_key_empty",
			entry: func(p *producer) *storev1.StoreEntry {
				return p.agentEntry("w-valid", "", frameLine(agentID("main"), responseFrame("main", "act-1", "x")))
			},
		},
		{
			name:      "missing plane arm",
			wantField: "entries[0].plane",
			wantSite:  "entry_plane_unset",
			entry: func(p *producer) *storev1.StoreEntry {
				e := p.agentEntry("w-valid", "u-valid", frameLine(agentID("main"), responseFrame("main", "act-1", "x")))
				e.Plane = &storev1.Plane{}
				return e
			},
		},
		{
			name:      "missing plane message",
			wantField: "entries[0].plane",
			wantSite:  "entry_plane_unset",
			entry: func(p *producer) *storev1.StoreEntry {
				e := p.agentEntry("w-valid", "u-valid", frameLine(agentID("main"), responseFrame("main", "act-1", "x")))
				e.Plane = nil
				return e
			},
		},
		{
			name:      "missing entry arm",
			wantField: "entries[0].entry",
			wantSite:  "entry_arm_unset",
			entry: func(p *producer) *storev1.StoreEntry {
				return &storev1.StoreEntry{Plane: p.plane(), ConversionVersion: p.conversionVersion(), WriteId: "w-valid", UpsertKey: "u-valid"}
			},
		},
		{
			name:      "missing agent_info arm",
			wantField: "entries[0].agent_update",
			wantSite:  "store_refused_request",
			entry: func(p *producer) *storev1.StoreEntry {
				return p.agentEntry("w-valid", "u-valid", &storev1.StoreAgentUpdate{TopLevel: agentID("main")})
			},
		},
		{
			name:      "page line naming no book",
			wantField: "entries[0].agent_update.serveable_frame.page_agent_id",
			wantSite:  "store_refused_request",
			entry: func(p *producer) *storev1.StoreEntry {
				return p.agentEntry("w-valid", "u-valid", &storev1.StoreAgentUpdate{
					AgentInfo: &storev1.StoreAgentUpdate_ServeableFrame{
						ServeableFrame: &storev1.StorePageLine{
							Book:      &storev1.StorePageLine_PageAgentId{PageAgentId: agentID("")},
							AgentItem: frameItem(responseFrame("main", "act-1", "x")),
						},
					},
				})
			},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			store := startStore(t, storeOptions{verbose: true})
			ctx, cancel := callContext(t)
			defer cancel()
			requestID := newRequestID(t)
			shim := streamProducer(store.client()).correlated(requestID)
			mark := store.logMark()

			// Act.
			failure := shim.writeExpectingFailure(ctx, t, nil, tc.entry(shim))

			// Assert.
			assertWriteInvalidRequest(t, failure, tc.wantField)
			records := store.logRecordsAfter(mark)
			assertNoDatabaseTouch(t, records, requestID)
			if len(recordsWithContextKey(records, "rpc")) == 0 {
				t.Errorf("the refusal logged no record carrying the rpc correlation key")
			}
			assertRefusalKeys(t, assertExactlyOneNormalRecord(t, records, "a malformed entry"), tc.wantSite, "invalid_request")
		})
	}
}

// TestWriteBatchRefusesAMalformedRequestEnvelope covers the refusals ABOVE the
// entries: the producer string, the batch itself, and the cursor advance riding
// it. Each names the field the store blames, and each is refused before any
// storage happens.
func TestWriteBatchRefusesAMalformedRequestEnvelope(t *testing.T) {
	tests := []struct {
		name      string
		request   func(*producer) *storev1.WriteBatchRequest
		wantField string
		wantSite  string
	}{
		{
			name:      "empty producer",
			wantField: "producer",
			wantSite:  "producer_empty",
			request: func(p *producer) *storev1.WriteBatchRequest {
				return &storev1.WriteBatchRequest{WriteClass: p.writeClass(), Batch: &storev1.EntryBatch{Entries: []*storev1.StoreEntry{
					p.agentEntry("w-valid", "u-valid", frameLine(agentID("main"), responseFrame("main", "act-1", "x"))),
				}}}
			},
		},
		{
			name:      "no write class",
			wantField: "write_class",
			wantSite:  "write_class_unset",
			request: func(p *producer) *storev1.WriteBatchRequest {
				return &storev1.WriteBatchRequest{Producer: p.name, Batch: &storev1.EntryBatch{Entries: []*storev1.StoreEntry{
					p.agentEntry("w-valid", "u-valid", frameLine(agentID("main"), responseFrame("main", "act-1", "x"))),
				}}}
			},
		},
		{
			name:      "missing batch",
			wantField: "batch",
			wantSite:  "batch_missing",
			request: func(p *producer) *storev1.WriteBatchRequest {
				return &storev1.WriteBatchRequest{Producer: p.name, WriteClass: p.writeClass()}
			},
		},
		{
			name:      "cursor advance naming no file",
			wantField: "cursor_advance.file_id",
			wantSite:  "cursor_file_id_empty",
			request: func(p *producer) *storev1.WriteBatchRequest {
				return &storev1.WriteBatchRequest{Producer: p.name, WriteClass: p.writeClass(), Batch: &storev1.EntryBatch{
					CursorAdvance: cursorState("", "/transcripts/a.jsonl", 4096, nil),
				}}
			},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			store := startStore(t, storeOptions{verbose: true})
			ctx, cancel := callContext(t)
			defer cancel()
			requestID := newRequestID(t)
			sidecar := fileProducer(store.client()).correlated(requestID)
			mark := store.logMark()
			call := connect.NewRequest(tc.request(sidecar))
			call.Header().Set(requestIDHeader, requestID)

			// Act.
			resp, err := store.client().WriteBatch(ctx, call)
			if err != nil {
				t.Fatalf("WriteBatch answered a transport error where a typed failure was owed: %v", err)
			}

			// Assert.
			failure := resp.Msg.GetFailure()
			if failure == nil {
				t.Fatalf("WriteBatch accepted a request it owed a typed failure for: %v", resp.Msg)
			}
			assertWriteInvalidRequest(t, failure, tc.wantField)
			records := store.logRecordsAfter(mark)
			assertNoDatabaseTouch(t, records, requestID)
			assertRefusalKeys(t, assertExactlyOneNormalRecord(t, records, "a malformed write request"), tc.wantSite, "invalid_request")
		})
	}
}

// TestWriteBatchRefusesAnEmptyBatch: a write of nothing is a producer defect,
// not a no-op to absorb quietly.
func TestWriteBatchRefusesAnEmptyBatch(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{verbose: true})
	ctx, cancel := callContext(t)
	defer cancel()
	requestID := newRequestID(t)
	shim := streamProducer(store.client()).correlated(requestID)
	mark := store.logMark()

	// Act.
	failure := shim.writeExpectingFailure(ctx, t, nil)

	// Assert.
	assertWriteInvalidRequest(t, failure, "batch")
	assertNoDatabaseTouch(t, store.logRecordsAfter(mark), requestID)
}

// TestOpenAgentSessionRefusesUnsetRequiredFields.
func TestOpenAgentSessionRefusesUnsetRequiredFields(t *testing.T) {
	tests := []struct {
		name      string
		req       *storev1.OpenAgentSessionRequest
		wantField string
	}{
		{name: "missing agent", req: &storev1.OpenAgentSessionRequest{}, wantField: "agent"},
		{name: "empty agent value", req: &storev1.OpenAgentSessionRequest{Agent: agentID("")}, wantField: "agent"},
		{
			name: "empty known_through value",
			req: &storev1.OpenAgentSessionRequest{
				Agent:   agentID("main"),
				Opening: &storev1.OpenAgentSessionRequest_KnownThrough{KnownThrough: &storev1.StoreItemPointer{Value: ""}},
			},
			wantField: "known_through",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			store := startStore(t, storeOptions{verbose: true})
			ctx, cancel := callContext(t)
			defer cancel()
			requestID := newRequestID(t)
			mark := store.logMark()

			// Act.
			failure := openSessionExpectingFailure(ctx, t, store.client(), tc.req, requestID)

			// Assert.
			assertOpenInvalidRequest(t, failure, tc.wantField)
			assertNoDatabaseTouch(t, store.logRecordsAfter(mark), requestID)
		})
	}
}

// TestReadAgentPageRefusesUnsetRequiredFields.
func TestReadAgentPageRefusesUnsetRequiredFields(t *testing.T) {
	tests := []struct {
		name      string
		req       *storev1.ReadAgentPageRequest
		wantField string
	}{
		{
			name:      "missing book",
			req:       &storev1.ReadAgentPageRequest{Position: &storev1.ReadAgentPageRequest_After{After: &storev1.StoreItemPointer{Value: "p"}}},
			wantField: "book",
		},
		{
			name:      "empty book value",
			req:       &storev1.ReadAgentPageRequest{Book: agentID(""), Position: &storev1.ReadAgentPageRequest_After{After: &storev1.StoreItemPointer{Value: "p"}}},
			wantField: "book",
		},
		{
			name:      "unset position",
			req:       &storev1.ReadAgentPageRequest{Book: agentID("main")},
			wantField: "position",
		},
		{
			name: "non-positive through bound",
			req: &storev1.ReadAgentPageRequest{Book: agentID("main"),
				Position: &storev1.ReadAgentPageRequest_Through{Through: &conversationv1.ConversationThrough{AtMs: 0}}},
			wantField: "through.at_ms",
		},
		{
			name:      "empty after pointer value",
			req:       &storev1.ReadAgentPageRequest{Book: agentID("main"), Position: &storev1.ReadAgentPageRequest_After{After: &storev1.StoreItemPointer{Value: ""}}},
			wantField: "after",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			store := startStore(t, storeOptions{verbose: true})
			ctx, cancel := callContext(t)
			defer cancel()
			requestID := newRequestID(t)
			mark := store.logMark()

			// Act.
			failure := readPageExpectingFailure(ctx, t, store.client(), tc.req, requestID)

			// Assert.
			assertReadInvalidRequest(t, failure, tc.wantField)
			assertNoDatabaseTouch(t, store.logRecordsAfter(mark), requestID)
		})
	}
}

// TestGetWorkflowRefusesAnUnsetHandle: the not-implemented answer is still an
// answer, so an unset handle must be refused on its own terms first.
func TestGetWorkflowRefusesAnUnsetHandle(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{verbose: true})
	ctx, cancel := callContext(t)
	defer cancel()
	requestID := newRequestID(t)
	mark := store.logMark()
	call := connect.NewRequest(&storev1.GetWorkflowRequest{})
	call.Header().Set(requestIDHeader, requestID)

	// Act.
	resp, err := store.client().GetWorkflow(ctx, call)
	if err != nil {
		t.Fatalf("GetWorkflow answered a transport error where a typed failure was owed: %v", err)
	}

	// Assert: `not_implemented` is the ONE honest arm this wave — nothing routes
	// into the workflow table, so `unknown_run` would be an invention.
	failure := resp.Msg.GetFailure()
	if failure == nil {
		t.Fatalf("GetWorkflow accepted a request with no handle: %v", resp.Msg)
	}
	if failure.GetNotImplemented() == nil {
		t.Fatalf("GetWorkflow failure kind = %v, want not_implemented", failure.GetKind())
	}
	assertNoDatabaseTouch(t, store.logRecordsAfter(mark), requestID)
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
			defer testclose.OrFail(t, stream)

			// Assert.
			assertWatchRefused(t, stream)
		})
	}
}

// TestGetSidecarCursorsRefusesAnEmptyFileID: absence is spelled with optional
// PRESENCE, so a present-but-empty file_id is a sentinel standing in for
// absence — the one thing it may not be.
func TestGetSidecarCursorsRefusesAnEmptyFileID(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{verbose: true})
	ctx, cancel := callContext(t)
	defer cancel()
	requestID := newRequestID(t)
	mark := store.logMark()
	empty := ""

	// Act.
	failure := cursorsExpectingFailure(ctx, t, store.client(), &empty, requestID)

	// Assert.
	assertCursorsInvalidRequest(t, failure, "file_id")
	assertNoDatabaseTouch(t, store.logRecordsAfter(mark), requestID)
}

// TestWatchBashRunRefusesAnEmptyRunAtTheTransport: this rpc has no failure arm,
// so a malformed ADDRESS closes at the transport — and with CodeInvalidArgument
// rather than CodeNotFound, because the caller must fix the request rather than
// conclude the run does not exist.
func TestWatchBashRunRefusesAnEmptyRunAtTheTransport(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()

	// Act.
	stream := watchBashRun(ctx, t, store.client(), "")
	defer testclose.OrFail(t, stream)

	// Assert.
	err := awaitBashRunEnd(t, stream)
	if code := connect.CodeOf(err); code != connect.CodeInvalidArgument {
		t.Fatalf("an empty run ended with Connect code %v, want %v (error: %v)", code, connect.CodeInvalidArgument, err)
	}
}

// TestAnAcceptedRequestDoesLeaveAStatementRecordCarryingItsId is the POSITIVE
// CONTROL for every assertNoDatabaseTouch above.
//
// Without it those assertions could pass while proving nothing — which is
// exactly what happened before: they looked for any record carrying a
// `statement` family, and the only record that carried one was the slow-query
// warning, so their success meant "nothing was slow", not "nothing ran". This
// subject fails if the store ever stops leaving that mark, which is what keeps
// the negative assertions honest.
func TestAnAcceptedRequestDoesLeaveAStatementRecordCarryingItsId(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{verbose: true})
	ctx, cancel := callContext(t)
	defer cancel()
	requestID := newRequestID(t)
	shim := streamProducer(store.client()).correlated(requestID)
	mark := store.logMark()

	// Act.
	shim.write(ctx, t, shim.agentEntry("w-touch", "u-touch",
		frameLine(agentID("main"), responseFrame("main", "act-1", "L1"))))

	// Assert.
	found := false
	for _, rec := range store.logRecordsAfter(mark) {
		if rec.RequestID == requestID && rec.Context["statement"] != nil {
			found = true
		}
	}
	if !found {
		t.Fatal("an accepted write left no statement record carrying its request id; every no-database-touch assertion is vacuous")
	}
}
