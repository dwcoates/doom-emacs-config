package convert

import (
	"errors"
	"io"
	"testing"

	agentshimv1 "agentrepl/proto/agentshim/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
)

// testConverter builds a Converter whose log goes nowhere, so a suite asserting
// conversion is not also asserting log formatting.
//
// The diagnostic sink is installed rather than left nil because the logger
// refuses to emit a session-scoped record without one — a guard that exists so a
// diagnostic about a session can never be silently dropped, and one this suite
// must satisfy rather than route around.
func testConverter(t *testing.T) *Converter {
	t.Helper()
	log := logging.New(io.Discard, io.Discard).With(logging.Context{Component: "test"})
	log.SetDiagnosticSink(func(logging.Diagnostic) {})
	return New(log)
}

func testAttribution() Attribution {
	return Attribution{SessionID: "sess-1", Path: "/transcripts/sess-1.jsonl", Offset: 100, ProducedAtMs: 1700000000000}
}

// single asserts a conversion produced exactly one record and returns it.
func single(t *testing.T, entries []*agentshimv1.Entry) *agentshimv1.Entry {
	t.Helper()
	if len(entries) != 1 {
		t.Fatalf("entries = %d, want exactly 1", len(entries))
	}
	return entries[0]
}

// ---------------------------------------------------------------------------
// the ingestion mandate
// ---------------------------------------------------------------------------

// Every JSON object on disk must end up in the store as a protobuf shape. The
// converter is where that mandate is either kept or broken, so it is asserted
// per line SHAPE rather than only for the shapes that convert cleanly.
func TestLineAlwaysProducesARecord(t *testing.T) {
	tests := []struct {
		name   string
		record map[string]any
	}{
		{name: "user prompt", record: map[string]any{"type": "user", "uuid": "u1", "message": map[string]any{"content": "hello"}}},
		{name: "assistant response", record: map[string]any{"type": "assistant", "uuid": "a1", "message": map[string]any{"id": "m1", "content": []any{}}}},
		{name: "system subtype", record: map[string]any{"type": "system", "uuid": "s1", "subtype": "turn_duration"}},
		{name: "attachment", record: map[string]any{"type": "attachment", "uuid": "x1", "attachment": map[string]any{"type": "date_change"}}},
		{name: "flat metadata line", record: map[string]any{"type": "ai-title", "aiTitle": "a name"}},
		{name: "unmodeled top-level type", record: map[string]any{"type": "something-new", "field": "value"}},
		{name: "no type discriminator at all", record: map[string]any{"field": "value"}},
		{name: "system with no subtype", record: map[string]any{"type": "system", "uuid": "s2"}},
		{name: "attachment with no attachment object", record: map[string]any{"type": "attachment", "uuid": "x2"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			c := testConverter(t)

			// Act.
			entries := c.Line(tc.record, testAttribution(), nil)

			// Assert.
			if len(entries) == 0 {
				t.Fatal("Line produced no record; the line would never reach the store")
			}
		})
	}
}

// A record that cannot be placed is stored WHOLE with no external half, which is
// what makes "an unconvertible record cannot reach a page" a fact about the
// record's shape rather than a rule a query has to remember to apply.
func TestUnconvertedRecordsHaveNoExternalHalf(t *testing.T) {
	tests := []struct {
		name   string
		record map[string]any
	}{
		{name: "unmodeled top-level type", record: map[string]any{"type": "something-new"}},
		{name: "no type discriminator", record: map[string]any{"field": "value"}},
		{name: "flat metadata line", record: map[string]any{"type": "pr-link", "prNumber": float64(7)}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			c := testConverter(t)

			// Act.
			entry := single(t, c.Line(tc.record, testAttribution(), nil))

			// Assert.
			if entry.GetExternal() != nil {
				t.Fatal("unconverted record carries an external half, so it has a path to the daemon")
			}
			if entry.GetInternal().GetUnconverted() == nil {
				t.Fatal("unconverted record carries no unconverted arm, so nothing says why it was not placed")
			}
		})
	}
}

// The three unconverted arms mean three different things, and picking the wrong
// one misdirects whoever follows up: vendor_specific asks for a converter,
// unknown asks for a model, unparsed asks for an investigation.
func TestUnconvertedArmMatchesTheReason(t *testing.T) {
	tests := []struct {
		name   string
		record map[string]any
		want   string
	}{
		{name: "understood but not carried", record: map[string]any{"type": "ai-title"}, want: "vendor_specific"},
		{name: "parsed but not modeled", record: map[string]any{"type": "brand-new-line"}, want: "unknown"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			c := testConverter(t)

			// Act.
			entry := single(t, c.Line(tc.record, testAttribution(), nil))

			// Assert.
			var got string
			switch entry.GetInternal().GetUnconverted().(type) {
			case *agentshimv1.InternalEntry_VendorSpecific:
				got = "vendor_specific"
			case *agentshimv1.InternalEntry_Unknown:
				got = "unknown"
			case *agentshimv1.InternalEntry_Unparsed:
				got = "unparsed"
			}
			if got != tc.want {
				t.Fatalf("unconverted arm = %q, want %q", got, tc.want)
			}
		})
	}
}

// ---------------------------------------------------------------------------
// lineage
// ---------------------------------------------------------------------------

// A session transcript's records are FEED ROWS. Reading the vendor's parentUuid
// chain as containment would nest an entire conversation inside its first line
// and collapse a whole session into one page slot.
func TestSessionTranscriptRecordIsAFeedRow(t *testing.T) {
	// Arrange.
	c := testConverter(t)
	record := map[string]any{
		"type": "user", "uuid": "u2", "parentUuid": "u1",
		"message": map[string]any{"content": "second thing I said"},
	}

	// Act.
	message := single(t, c.Line(record, testAttribution(), nil)).GetExternal().GetMessage()

	// Assert.
	if message.GetParent().GetRoot() == nil {
		t.Fatal("parent is not root; a sequential reply was read as containment")
	}
	if message.GetTopLevelMessageId() != message.GetMessageId() {
		t.Fatalf("top_level_message_id = %q, want its own id %q", message.GetTopLevelMessageId(), message.GetMessageId())
	}
}

// A sidechain's records sit INSIDE the card the subagent runs as, so a page of
// ten rows counts the subagent once however long its conversation runs.
func TestSidechainRecordSitsInsideItsCard(t *testing.T) {
	// Arrange.
	c := testConverter(t)
	at := testAttribution()
	at.Container = DetachedWorkMessageID("agent-77")
	record := map[string]any{"type": "user", "uuid": "u9", "message": map[string]any{"content": "do the thing"}}

	// Act.
	message := single(t, c.Line(record, at, nil)).GetExternal().GetMessage()

	// Assert.
	if got := message.GetParent().GetInside().GetMessageId(); got != "dw:agent-77" {
		t.Fatalf("parent inside = %q, want %q", got, "dw:agent-77")
	}
	if got := message.GetTopLevelMessageId(); got != "dw:agent-77" {
		t.Fatalf("top_level_message_id = %q, want the card %q", got, "dw:agent-77")
	}
}

// A sidechain line read from the SESSION transcript names its own agent, so it
// lands in the same card as the sidechain file's own records without the two
// readers sharing anything.
func TestSidechainFlagOnASessionLineNamesItsCard(t *testing.T) {
	// Arrange.
	c := testConverter(t)
	record := map[string]any{
		"type": "assistant", "uuid": "a5", "isSidechain": true, "agentId": "agent-77",
		"message": map[string]any{"id": "m5", "content": []any{}},
	}

	// Act.
	message := single(t, c.Line(record, testAttribution(), nil)).GetExternal().GetMessage()

	// Assert.
	if got := message.GetTopLevelMessageId(); got != "dw:agent-77" {
		t.Fatalf("top_level_message_id = %q, want %q", got, "dw:agent-77")
	}
}

// The card's message id is a PURE FUNCTION of the task id. That is what lets the
// spool, the sidechain, the launch and the staleness sweep name one card with
// nothing correlated between them and nothing recovered after a restart.
func TestDetachedWorkMessageIDIsDerivedFromTheTaskID(t *testing.T) {
	tests := []struct {
		name   string
		taskID string
		want   string
	}{
		{name: "agent task", taskID: "agent-1", want: "dw:agent-1"},
		{name: "workflow run", taskID: "wf_abc", want: "dw:wf_abc"},
		{name: "no task identity", taskID: "", want: ""},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act.
			got := DetachedWorkMessageID(tc.taskID)

			// Assert.
			if got != tc.want {
				t.Fatalf("DetachedWorkMessageID(%q) = %q, want %q", tc.taskID, got, tc.want)
			}
		})
	}
}

// ---------------------------------------------------------------------------
// author
// ---------------------------------------------------------------------------

// Who a message is FROM is resolved by the producer, never inferred by a reader
// from which payload arm is set — a tool result is authored by the agent whose
// message it updates, and the vendor files it under the user.
func TestAuthorIsResolvedByTheProducer(t *testing.T) {
	tests := []struct {
		name    string
		record  map[string]any
		wantArm string
		setup   func(*Converter)
	}{
		{
			name:    "a person's prompt is from the user",
			record:  map[string]any{"type": "user", "uuid": "u1", "message": map[string]any{"content": "hi"}},
			wantArm: "user",
		},
		{
			name:    "a response is from the agent",
			record:  map[string]any{"type": "assistant", "uuid": "a1", "message": map[string]any{"id": "m1", "content": []any{}}},
			wantArm: "agent",
		},
		{
			name: "a tool result stays with the agent that called the tool",
			setup: func(c *Converter) {
				c.toolCallOwner["call-1"] = "m1"
			},
			record: map[string]any{"type": "user", "uuid": "u2", "message": map[string]any{"content": []any{
				map[string]any{"type": "tool_result", "tool_use_id": "call-1", "content": "output"},
			}}},
			wantArm: "agent",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			c := testConverter(t)
			if tc.setup != nil {
				tc.setup(c)
			}

			// Act.
			entries := c.Line(tc.record, testAttribution(), nil)

			// Assert.
			author := entries[0].GetExternal().GetMessage().GetAuthor()
			var got string
			switch {
			case author.GetUser() != nil:
				got = "user"
			case author.GetAgent() != nil:
				got = "agent"
			case author.GetDetachedAgent() != nil:
				got = "detached_agent"
			}
			if got != tc.wantArm {
				t.Fatalf("author arm = %q, want %q", got, tc.wantArm)
			}
		})
	}
}

// Inside detached work the agent is the DETACHED one, named by the work it runs
// as, so its emissions route to the card without a second correlation.
func TestDetachedAgentNamesItsOwnWork(t *testing.T) {
	// Arrange.
	c := testConverter(t)
	at := testAttribution()
	at.Container = DetachedWorkMessageID("agent-3")
	record := map[string]any{"type": "assistant", "uuid": "a1", "message": map[string]any{"id": "m1", "content": []any{}}}

	// Act.
	author := single(t, c.Line(record, at, nil)).GetExternal().GetMessage().GetAuthor()

	// Assert.
	if got := author.GetDetachedAgent().GetDetachedWorkMessageId(); got != "dw:agent-3" {
		t.Fatalf("detached_work_message_id = %q, want %q", got, "dw:agent-3")
	}
}

// ---------------------------------------------------------------------------
// tool results
// ---------------------------------------------------------------------------

// A tool result folds onto the message that MADE the call, which is what spares
// every consumer a correlation pass and costs the result no page slot.
func TestToolResultFoldsOntoTheCallingMessage(t *testing.T) {
	// Arrange.
	c := testConverter(t)
	c.Line(map[string]any{
		"type": "assistant", "uuid": "a1",
		"message": map[string]any{"id": "m1", "content": []any{
			map[string]any{"type": "tool_use", "id": "call-1", "name": "Bash", "input": map[string]any{"command": "ls"}},
		}},
	}, testAttribution(), nil)

	// Act.
	entries := c.Line(map[string]any{
		"type": "user", "uuid": "u1",
		"message": map[string]any{"content": []any{
			map[string]any{"type": "tool_result", "tool_use_id": "call-1", "content": "a b c"},
		}},
	}, testAttribution(), nil)

	// Assert.
	message := entries[0].GetExternal().GetMessage()
	if got := message.GetMessageId(); got != "m1" {
		t.Fatalf("message_id = %q, want the calling message %q", got, "m1")
	}
	if got := message.GetToolReturned().GetToolCallId(); got != "call-1" {
		t.Fatalf("tool_call_id = %q, want %q", got, "call-1")
	}
}

// A result whose calling message this reader never saw has no legal parent, and
// MessageParent has no arm for an unresolved one. It is stored whole rather than
// given an invented root that would become a phantom feed row.
func TestUnresolvableToolResultIsStoredUnconverted(t *testing.T) {
	// Arrange.
	c := testConverter(t)
	record := map[string]any{
		"type": "user", "uuid": "u1",
		"message": map[string]any{"content": []any{
			map[string]any{"type": "tool_result", "tool_use_id": "call-unseen", "content": "output"},
		}},
	}

	// Act.
	entry := single(t, c.Line(record, testAttribution(), nil))

	// Assert.
	if entry.GetExternal() != nil {
		t.Fatal("an unresolvable tool result reached the daemon with an invented parent")
	}
	if entry.GetInternal().GetUnknown() == nil {
		t.Fatal("an unresolvable tool result was not stored as an unknown record")
	}
}

// The vendor also names the assistant LINE a result answers, which resolves the
// owner when the call id alone does not.
func TestToolResultResolvesViaTheNamedAssistantLine(t *testing.T) {
	// Arrange.
	c := testConverter(t)
	c.Line(map[string]any{
		"type": "assistant", "uuid": "a1",
		"message": map[string]any{"id": "m1", "content": []any{}},
	}, testAttribution(), nil)

	// Act.
	entries := c.Line(map[string]any{
		"type": "user", "uuid": "u1", "sourceToolAssistantUUID": "a1",
		"message": map[string]any{"content": []any{
			map[string]any{"type": "tool_result", "tool_use_id": "call-x", "content": "output"},
		}},
	}, testAttribution(), nil)

	// Assert.
	if got := entries[0].GetExternal().GetMessage().GetMessageId(); got != "m1" {
		t.Fatalf("message_id = %q, want %q", got, "m1")
	}
}

// A tool that FAILED and a tool that returned nothing are different things to
// render, so the failure is a fact of its own rather than an empty body.
func TestToolFailureIsStatedRatherThanImpliedByEmptyContent(t *testing.T) {
	// Arrange.
	c := testConverter(t)
	c.toolCallOwner["call-1"] = "m1"
	record := map[string]any{
		"type": "user", "uuid": "u1",
		"message": map[string]any{"content": []any{
			map[string]any{"type": "tool_result", "tool_use_id": "call-1", "content": "", "is_error": true},
		}},
	}

	// Act.
	entries := c.Line(record, testAttribution(), nil)

	// Assert.
	if !entries[0].GetExternal().GetMessage().GetToolReturned().GetIsError() {
		t.Fatal("is_error is false for a tool result the vendor flagged as an error")
	}
}

// A user record that carries only tool results is not something a person said,
// so it must not also produce a UserSaid with empty content.
func TestToolResultCarrierIsNotAlsoAUserMessage(t *testing.T) {
	// Arrange.
	c := testConverter(t)
	c.toolCallOwner["call-1"] = "m1"
	record := map[string]any{
		"type": "user", "uuid": "u1",
		"message": map[string]any{"content": []any{
			map[string]any{"type": "tool_result", "tool_use_id": "call-1", "content": "out"},
		}},
	}

	// Act.
	entries := c.Line(record, testAttribution(), nil)

	// Assert.
	for _, entry := range entries {
		if entry.GetExternal().GetMessage().GetUserSaid() != nil {
			t.Fatal("a tool-result carrier produced a UserSaid the person never typed")
		}
	}
}

// ---------------------------------------------------------------------------
// agent responses
// ---------------------------------------------------------------------------

// The vendor's three disjoint input counters map onto the one canonical shape,
// and the mapping is fixed rather than a judgment call: reading any single
// counter as "the cost" is the mistake the nesting exists to prevent.
func TestUsageMapsOntoTheCanonicalTokenShape(t *testing.T) {
	// Arrange.
	c := testConverter(t)
	record := map[string]any{
		"type": "assistant", "uuid": "a1",
		"message": map[string]any{"id": "m1", "content": []any{}, "usage": map[string]any{
			"cache_read_input_tokens":     float64(900),
			"cache_creation_input_tokens": float64(30),
			"input_tokens":                float64(7),
			"output_tokens":               float64(12),
		}},
	}

	// Act.
	usage := single(t, c.Line(record, testAttribution(), nil)).GetExternal().GetMessage().GetAgentSaid().GetUsage()

	// Assert.
	if got := usage.GetInputHits().GetRead(); got != 900 {
		t.Fatalf("input_hits.read = %d, want 900", got)
	}
	if got := usage.GetInputMisses().GetWritten(); got != 30 {
		t.Fatalf("input_misses.written = %d, want 30", got)
	}
	if got := usage.GetInputMisses().GetUnwritten(); got != 7 {
		t.Fatalf("input_misses.unwritten = %d, want 7", got)
	}
	if got := usage.GetOutputTokens(); got != 12 {
		t.Fatalf("output_tokens = %d, want 12", got)
	}
}

// A response the vendor reported no usage for must not read as a response that
// cost nothing.
func TestAbsentUsageIsNotAZeroBill(t *testing.T) {
	// Arrange.
	c := testConverter(t)
	record := map[string]any{"type": "assistant", "uuid": "a1", "message": map[string]any{"id": "m1", "content": []any{}}}

	// Act.
	usage := single(t, c.Line(record, testAttribution(), nil)).GetExternal().GetMessage().GetAgentSaid().GetUsage()

	// Assert.
	if usage != nil {
		t.Fatal("absent vendor usage produced a zero TokenUsage, which reads as a free response")
	}
}

// A stop reason the schema does not model is STATED as unsupported. Defaulting
// it to end_turn would report a truncated response as a complete one.
func TestStopReasonIsStatedRatherThanDefaulted(t *testing.T) {
	tests := []struct {
		name   string
		vendor string
		check  func(*conversationv1.StopReason) bool
	}{
		{name: "end turn", vendor: "end_turn", check: func(s *conversationv1.StopReason) bool { return s.GetEndTurn() != nil }},
		{name: "waiting on a tool", vendor: "tool_use", check: func(s *conversationv1.StopReason) bool { return s.GetToolCall() != nil }},
		{name: "hit the output ceiling", vendor: "max_tokens", check: func(s *conversationv1.StopReason) bool { return s.GetMaxTokens() != nil }},
		{name: "unmodeled vendor reason", vendor: "some_new_reason", check: func(s *conversationv1.StopReason) bool {
			return s.GetUnsupported().GetReason() == "some_new_reason"
		}},
		{name: "vendor said nothing", vendor: "", check: func(s *conversationv1.StopReason) bool { return s == nil }},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			c := testConverter(t)
			record := map[string]any{"type": "assistant", "uuid": "a1", "message": map[string]any{
				"id": "m1", "content": []any{}, "stop_reason": tc.vendor,
			}}

			// Act.
			got := single(t, c.Line(record, testAttribution(), nil)).GetExternal().GetMessage().GetAgentSaid().GetStopReason()

			// Assert.
			if !tc.check(got) {
				t.Fatalf("stop reason for vendor %q resolved to %v", tc.vendor, got)
			}
		})
	}
}

// The vendor's own recorded API failure is a MESSAGE, because a reader scrolling
// back must see that the turn failed rather than find it merely absent.
func TestVendorApiErrorBecomesAFailureCard(t *testing.T) {
	// Arrange.
	c := testConverter(t)
	record := map[string]any{
		"type": "assistant", "uuid": "a1", "isApiErrorMessage": true, "error": "429 rate limited",
		"message": map[string]any{"id": "m1", "content": []any{}},
	}

	// Act.
	message := single(t, c.Line(record, testAttribution(), nil)).GetExternal().GetMessage()

	// Assert.
	if got := message.GetFailureRaised().GetSummary(); got != "429 rate limited" {
		t.Fatalf("failure summary = %q, want the vendor's own text", got)
	}
	if message.GetAgentSaid() != nil {
		t.Fatal("an API error also produced an empty agent response")
	}
}

// ---------------------------------------------------------------------------
// content blocks
// ---------------------------------------------------------------------------

// The vendor's call-shaped block kinds are the same fact wearing three names, so
// all three become a tool call and the tool's own name carries the rest.
func TestEveryCallShapedBlockKindBecomesAToolCall(t *testing.T) {
	tests := []struct {
		name string
		kind string
	}{
		{name: "client tool", kind: "tool_use"},
		{name: "server-run tool", kind: "server_tool_use"},
		{name: "MCP tool", kind: "mcp_tool_use"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			c := testConverter(t)
			record := map[string]any{"type": "assistant", "uuid": "a1", "message": map[string]any{
				"id": "m1", "content": []any{map[string]any{"type": tc.kind, "id": "c1", "name": "Search"}},
			}}

			// Act.
			blocks := single(t, c.Line(record, testAttribution(), nil)).GetExternal().GetMessage().GetAgentSaid().GetContent().GetBlocks()

			// Assert.
			if len(blocks) != 1 || blocks[0].GetToolCall() == nil {
				t.Fatalf("block kind %q did not convert to a tool call: %v", tc.kind, blocks)
			}
			if got := blocks[0].GetToolCall().GetToolName(); got != "Search" {
				t.Fatalf("tool_name = %q, want %q", got, "Search")
			}
		})
	}
}

// Reasoning the vendor WITHHELD is stated as redacted, so a client can say
// "reasoning was hidden" rather than showing nothing and implying none happened.
func TestRedactedThinkingIsStatedRatherThanShownAsEmpty(t *testing.T) {
	// Arrange.
	c := testConverter(t)
	record := map[string]any{"type": "assistant", "uuid": "a1", "message": map[string]any{
		"id": "m1", "content": []any{map[string]any{"type": "redacted_thinking", "data": "opaque"}},
	}}

	// Act.
	blocks := single(t, c.Line(record, testAttribution(), nil)).GetExternal().GetMessage().GetAgentSaid().GetContent().GetBlocks()

	// Assert.
	if !blocks[0].GetThinking().GetRedacted() {
		t.Fatal("redacted reasoning was not flagged, so it is indistinguishable from no reasoning")
	}
}

// A block kind this schema does not model is kept WHOLE, so the decision not to
// model it stays reversible from stored data.
func TestUnmodeledBlockIsKeptVerbatim(t *testing.T) {
	// Arrange.
	c := testConverter(t)
	record := map[string]any{"type": "assistant", "uuid": "a1", "message": map[string]any{
		"id": "m1", "content": []any{map[string]any{"type": "container_upload", "file_id": "f1"}},
	}}

	// Act.
	blocks := single(t, c.Line(record, testAttribution(), nil)).GetExternal().GetMessage().GetAgentSaid().GetContent().GetBlocks()

	// Assert.
	unsupported := blocks[0].GetUnsupported()
	if unsupported.GetKind() != "container_upload" {
		t.Fatalf("unsupported kind = %q, want %q", unsupported.GetKind(), "container_upload")
	}
	if unsupported.GetRaw().GetFields()["file_id"].GetStringValue() != "f1" {
		t.Fatal("the unmodeled block's own fields were not preserved")
	}
}

// A person's message spelled as a bare string and one spelled as blocks are the
// same thing said two ways, and the runtime JSON type is the only discriminator
// the vendor gives.
func TestUserContentAcceptsBothVendorSpellings(t *testing.T) {
	tests := []struct {
		name    string
		content any
	}{
		{name: "bare string", content: "hello there"},
		{name: "block array", content: []any{map[string]any{"type": "text", "text": "hello there"}}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			c := testConverter(t)
			record := map[string]any{"type": "user", "uuid": "u1", "message": map[string]any{"content": tc.content}}

			// Act.
			blocks := single(t, c.Line(record, testAttribution(), nil)).GetExternal().GetMessage().GetUserSaid().GetContent().GetBlocks()

			// Assert.
			if len(blocks) != 1 || blocks[0].GetText().GetText() != "hello there" {
				t.Fatalf("blocks = %v, want one text block", blocks)
			}
		})
	}
}

// ---------------------------------------------------------------------------
// context cuts
// ---------------------------------------------------------------------------

// The harness never writes the literal prompt "/clear"; it writes the expanded
// command envelope, so anything matching raw text misses every replayed session.
func TestClearIsDetectedThroughTheExpandedEnvelope(t *testing.T) {
	tests := []struct {
		name    string
		content string
		want    bool
	}{
		{name: "expanded envelope", content: "<command-name>/clear</command-name><command-message>clear</command-message><command-args></command-args>", want: true},
		{name: "bare command text", content: "/clear", want: true},
		{name: "envelope with an argument", content: "<command-name>/clear</command-name><command-args>everything</command-args>", want: false},
		{name: "a different command", content: "<command-name>/compact</command-name><command-args></command-args>", want: false},
		{name: "a prompt merely quoting the envelope", content: "look: <command-name>/clear</command-name> is what I ran", want: false},
		{name: "ordinary prose", content: "please clear the context", want: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			c := testConverter(t)
			record := map[string]any{"type": "user", "uuid": "u1", "message": map[string]any{"content": tc.content}}

			// Act.
			message := single(t, c.Line(record, testAttribution(), nil)).GetExternal().GetMessage()

			// Assert.
			got := message.GetContextCut().GetCleared() != nil
			if got != tc.want {
				t.Fatalf("cleared = %t for content %q, want %t", got, tc.content, tc.want)
			}
		})
	}
}

// The boundary and its summary are paired in FILE order. The harness composes
// the summary BEFORE writing the boundary that announces it, so the summary's
// timestamp is earlier and a timestamp-ordered pairing gets every pair wrong.
func TestCompactionTakesItsSummaryFromTheFollowingLine(t *testing.T) {
	// Arrange.
	c := testConverter(t)
	boundary := map[string]any{
		"type": "system", "uuid": "s1", "subtype": "compact_boundary",
		"compactMetadata": map[string]any{"preTokens": float64(150000), "postTokens": float64(20000)},
	}
	summary := map[string]any{
		"type": "user", "uuid": "u1", "isCompactSummary": true,
		"message": map[string]any{"content": "we were doing X"},
	}

	// Act.
	compacted := single(t, c.Line(boundary, testAttribution(), summary)).
		GetExternal().GetMessage().GetContextCut().GetCompacted()

	// Assert.
	if got := compacted.GetSummary().GetBlocks()[0].GetText().GetText(); got != "we were doing X" {
		t.Fatalf("summary = %q, want the following line's text", got)
	}
	if compacted.GetTokensBefore() != 150000 || compacted.GetTokensAfter() != 20000 {
		t.Fatalf("tokens before/after = %d/%d, want 150000/20000", compacted.GetTokensBefore(), compacted.GetTokensAfter())
	}
}

// A boundary with no summary after it is still a real cut and is still emitted:
// a reader must see WHERE the conversation was cut rather than merely find the
// history shorter than they left it.
func TestCompactionWithoutASummaryIsStillACut(t *testing.T) {
	// Arrange.
	c := testConverter(t)
	boundary := map[string]any{"type": "system", "uuid": "s1", "subtype": "compact_boundary"}

	// Act.
	message := single(t, c.Line(boundary, testAttribution(), nil)).GetExternal().GetMessage()

	// Assert.
	if message.GetContextCut().GetCompacted() == nil {
		t.Fatal("a summary-less boundary produced no context cut")
	}
}

// The summary line is the harness's own prose standing in for discarded history.
// Emitting it as a user message would render it twice and attribute the
// harness's text to the person.
func TestCompactionSummaryIsNotAlsoAUserMessage(t *testing.T) {
	// Arrange.
	c := testConverter(t)
	record := map[string]any{
		"type": "user", "uuid": "u1", "isCompactSummary": true,
		"message": map[string]any{"content": "we were doing X"},
	}

	// Act.
	entries := c.Line(record, testAttribution(), nil)

	// Assert.
	for _, entry := range entries {
		if entry.GetExternal().GetMessage().GetUserSaid() != nil {
			t.Fatal("the compaction summary was emitted as something the person said")
		}
	}
}

// IsCompactBoundary is what the reader consults to decide whether to defer a
// batch's last frame, so it must recognize exactly that record and no other.
func TestIsCompactBoundaryRecognizesOnlyTheBoundary(t *testing.T) {
	tests := []struct {
		name   string
		record map[string]any
		want   bool
	}{
		{name: "the boundary", record: map[string]any{"type": "system", "subtype": "compact_boundary"}, want: true},
		{name: "a different system subtype", record: map[string]any{"type": "system", "subtype": "turn_duration"}, want: false},
		{name: "a user line", record: map[string]any{"type": "user"}, want: false},
		{name: "the summary that follows it", record: map[string]any{"type": "user", "isCompactSummary": true}, want: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act.
			got := IsCompactBoundary(tc.record)

			// Assert.
			if got != tc.want {
				t.Fatalf("IsCompactBoundary = %t, want %t", got, tc.want)
			}
		})
	}
}

// ---------------------------------------------------------------------------
// detached work
// ---------------------------------------------------------------------------

// A launch opens a card that is a FEED ROW naming itself, so a page of ten rows
// is ten bounded things rather than ten trees.
func TestLaunchOpensAFeedRowNamingItself(t *testing.T) {
	tests := []struct {
		name       string
		result     map[string]any
		wantID     string
		wantKindOK func(*conversationv1.DetachedWorkKind) bool
	}{
		{
			name:       "background agent",
			result:     map[string]any{"isAsync": true, "agentId": "agent-1", "description": "do research"},
			wantID:     "dw:agent-1",
			wantKindOK: func(k *conversationv1.DetachedWorkKind) bool { return k.GetAgent() != nil },
		},
		{
			name:       "workflow run",
			result:     map[string]any{"runId": "wf_1", "summary": "build"},
			wantID:     "dw:wf_1",
			wantKindOK: func(k *conversationv1.DetachedWorkKind) bool { return k.GetWorkflow() != nil },
		},
		{
			name:       "background shell",
			result:     map[string]any{"stdout": "", "backgroundTaskId": "b-9"},
			wantID:     "dw:b-9",
			wantKindOK: func(k *conversationv1.DetachedWorkKind) bool { return k.GetShell() != nil },
		},
		{
			name:       "skill invocation",
			result:     map[string]any{"commandName": "deploy", "success": true},
			wantID:     "dw:call-7",
			wantKindOK: func(k *conversationv1.DetachedWorkKind) bool { return k.GetSkill().GetSkillName() == "deploy" },
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			c := testConverter(t)
			record := map[string]any{
				"type": "user", "uuid": "u1", "sourceToolUseID": "call-7",
				"toolUseResult": tc.result,
				"message":       map[string]any{"content": "ok"},
			}

			// Act.
			entries := c.Line(record, testAttribution(), nil)

			// Assert.
			var started *conversationv1.MessageEntry
			for _, entry := range entries {
				if entry.GetExternal().GetMessage().GetDetachedWorkStarted() != nil {
					started = entry.GetExternal().GetMessage()
				}
			}
			if started == nil {
				t.Fatal("the launch opened no detached-work card")
			}
			if started.GetMessageId() != tc.wantID {
				t.Fatalf("message_id = %q, want %q", started.GetMessageId(), tc.wantID)
			}
			if started.GetParent().GetRoot() == nil {
				t.Fatal("the card is not a feed row, so it cannot own a page slot")
			}
			if started.GetTopLevelMessageId() != tc.wantID {
				t.Fatalf("top_level_message_id = %q, want its own id", started.GetTopLevelMessageId())
			}
			if !tc.wantKindOK(started.GetDetachedWorkStarted().GetKind()) {
				t.Fatalf("detached kind is wrong: %v", started.GetDetachedWorkStarted().GetKind())
			}
			if got := started.GetDetachedWorkStarted().GetOriginToolCallId(); got != "call-7" {
				t.Fatalf("origin_tool_call_id = %q, want %q", got, "call-7")
			}
		})
	}
}

// A launch the harness did not name would open a card nothing could ever update,
// so it is stored whole instead.
func TestUnnamedLaunchDoesNotOpenACard(t *testing.T) {
	// Arrange.
	c := testConverter(t)
	record := map[string]any{
		"type": "user", "uuid": "u1",
		"toolUseResult": map[string]any{"isAsync": true},
		"message":       map[string]any{"content": "ok"},
	}

	// Act.
	entries := c.Line(record, testAttribution(), nil)

	// Assert.
	for _, entry := range entries {
		if entry.GetExternal().GetMessage().GetDetachedWorkStarted() != nil {
			t.Fatal("a launch with no task identity opened a card")
		}
	}
}

// A stop is CANCELLED, which is a different thing from either outcome the work
// might have reached on its own.
func TestTaskStopEndsTheWorkAsCancelled(t *testing.T) {
	// Arrange.
	c := testConverter(t)
	record := map[string]any{
		"type": "user", "uuid": "u1",
		"toolUseResult": map[string]any{"command": "stop", "taskType": "agent", "taskId": "agent-4", "message": "stopped"},
		"message":       map[string]any{"content": "ok"},
	}

	// Act.
	entries := c.Line(record, testAttribution(), nil)

	// Assert.
	var ended *conversationv1.DetachedWorkEnded
	for _, entry := range entries {
		if e := entry.GetExternal().GetMessage().GetDetachedWorkEnded(); e != nil {
			ended = e
		}
	}
	if ended.GetCancelled() == nil {
		t.Fatalf("a deliberate stop resolved to %v, want cancelled", ended)
	}
}

// A zero exit is a success and a non-zero exit is a failure, both read from the
// one structured byte a shell spool has.
func TestExitCodeDecidesTheOutcome(t *testing.T) {
	tests := []struct {
		name        string
		code        int
		wantSuccess bool
	}{
		{name: "clean exit", code: 0, wantSuccess: true},
		{name: "non-zero exit", code: 2, wantSuccess: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act.
			ended := DetachedExited(testAttribution(), "b-1", tc.code).
				GetExternal().GetMessage().GetDetachedWorkEnded()

			// Assert.
			if (ended.GetSucceeded() != nil) != tc.wantSuccess {
				t.Fatalf("outcome for exit %d = %v, want success=%t", tc.code, ended, tc.wantSuccess)
			}
		})
	}
}

// LOST is its own outcome and never failure: we do not know the work died, only
// that we cannot see it any more.
func TestLostIsNeverReportedAsFailure(t *testing.T) {
	// Arrange / Act.
	ended := DetachedLost(testAttribution(), "agent-1", "silence-timeout").
		GetExternal().GetMessage().GetDetachedWorkEnded()

	// Assert.
	if ended.GetFailed() != nil {
		t.Fatal("a LOST inference was reported as a failure the sidecar never observed")
	}
	if got := ended.GetLost().GetInference(); got != "silence-timeout" {
		t.Fatalf("inference = %q, want %q", got, "silence-timeout")
	}
}

// ---------------------------------------------------------------------------
// skills
// ---------------------------------------------------------------------------

// A skill's body arrives as a SEPARATE record after the call that opened the
// work, and it is resolved onto the skill's own card rather than left for a
// consumer to correlate.
func TestSkillBodyResolvesOntoItsOwnCard(t *testing.T) {
	// Arrange.
	c := testConverter(t)
	c.Line(map[string]any{
		"type": "user", "uuid": "u1", "sourceToolUseID": "call-1",
		"toolUseResult": map[string]any{"commandName": "deploy"},
		"message":       map[string]any{"content": "ok"},
	}, testAttribution(), nil)

	// Act.
	entries := c.Line(map[string]any{
		"type": "attachment", "uuid": "x1",
		"attachment": map[string]any{"type": "invoked_skills", "skills": []any{
			map[string]any{"name": "deploy", "content": "# Deploy\nsteps"},
		}},
	}, testAttribution(), nil)

	// Assert.
	message := single(t, entries).GetExternal().GetMessage()
	if message.GetMessageId() != "dw:call-1" {
		t.Fatalf("message_id = %q, want the skill's card %q", message.GetMessageId(), "dw:call-1")
	}
	if got := message.GetSkillBodyResolved().GetBody(); got != "# Deploy\nsteps" {
		t.Fatalf("body = %q, want the skill file verbatim", got)
	}
}

// A body naming a skill this reader never saw invoked has no card to resolve
// onto, so it is stored whole rather than attached to an invented one.
func TestUnresolvableSkillBodyIsStoredUnconverted(t *testing.T) {
	// Arrange.
	c := testConverter(t)
	record := map[string]any{
		"type": "attachment", "uuid": "x1",
		"attachment": map[string]any{"type": "invoked_skills", "skills": []any{
			map[string]any{"name": "never-seen", "content": "body"},
		}},
	}

	// Act.
	entry := single(t, c.Line(record, testAttribution(), nil))

	// Assert.
	if entry.GetExternal() != nil {
		t.Fatal("an unresolvable skill body was resolved onto an invented card")
	}
}

// ---------------------------------------------------------------------------
// write identity
// ---------------------------------------------------------------------------

// The write identity is minted once and NEVER regenerated — not for a retry, not
// for a replay after the store bounced. Deriving it from the file position is
// what makes that true with no durable state to lose.
func TestWriteIdentityIsStableAcrossReconversion(t *testing.T) {
	// Arrange.
	record := map[string]any{"type": "user", "uuid": "u1", "message": map[string]any{"content": "hello"}}

	// Act: two independent converters, as a restart would produce.
	first := single(t, testConverter(t).Line(record, testAttribution(), nil))
	second := single(t, testConverter(t).Line(record, testAttribution(), nil))

	// Assert.
	if first.GetInternal().GetWriteId() != second.GetInternal().GetWriteId() {
		t.Fatal("a replayed record minted a new write identity, so the store would write it twice")
	}
	if first.GetInternal().GetWriteId() == "" {
		t.Fatal("the record carries no write identity, so it is not replay-idempotent")
	}
}

// Two records read at DIFFERENT positions are two records, and must not collide
// on one write identity — which would silently drop the second at the store.
func TestWriteIdentityDistinguishesPositions(t *testing.T) {
	// Arrange.
	record := map[string]any{"type": "user", "uuid": "u1", "message": map[string]any{"content": "hello"}}
	first, second := testAttribution(), testAttribution()
	second.Offset = 500

	// Act.
	a := single(t, testConverter(t).Line(record, first, nil))
	b := single(t, testConverter(t).Line(record, second, nil))

	// Assert.
	if a.GetInternal().GetWriteId() == b.GetInternal().GetWriteId() {
		t.Fatal("two records at different offsets share one write identity")
	}
}

// Several records produced from ONE line are still several records, so they must
// not collide either.
func TestWriteIdentityDistinguishesRecordsFromOneLine(t *testing.T) {
	// Arrange.
	c := testConverter(t)
	c.toolCallOwner["call-1"] = "m1"
	c.toolCallOwner["call-2"] = "m1"
	record := map[string]any{
		"type": "user", "uuid": "u1",
		"message": map[string]any{"content": []any{
			map[string]any{"type": "tool_result", "tool_use_id": "call-1", "content": "a"},
			map[string]any{"type": "tool_result", "tool_use_id": "call-2", "content": "b"},
		}},
	}

	// Act.
	entries := c.Line(record, testAttribution(), nil)

	// Assert.
	seen := map[string]bool{}
	for _, entry := range entries {
		id := entry.GetInternal().GetWriteId()
		if seen[id] {
			t.Fatalf("two records from one line share write identity %q", id)
		}
		seen[id] = true
	}
}

// ---------------------------------------------------------------------------
// plane
// ---------------------------------------------------------------------------

// Every record this process writes was read from disk, so every one records the
// FILE plane. Attribution is the shim's own business and never leaves.
func TestEveryRecordIsAttributedToTheFilePlane(t *testing.T) {
	// Arrange.
	c := testConverter(t)
	record := map[string]any{"type": "user", "uuid": "u1", "message": map[string]any{"content": "hi"}}

	// Act.
	entry := single(t, c.Line(record, testAttribution(), nil))

	// Assert.
	if entry.GetInternal().GetPlane().GetFile() == nil {
		t.Fatal("a record read from disk was not attributed to the file plane")
	}
}

// ---------------------------------------------------------------------------
// unparsed records
// ---------------------------------------------------------------------------

// A record we could not READ is a failure rather than a gap, and the fields exist
// so it is investigable rather than merely counted.
func TestUnparsedRecordCarriesItsEvidence(t *testing.T) {
	// Arrange.
	at := testAttribution()
	cause := errors.New("unexpected end of JSON input")

	// Act.
	unparsed := UnparsedEntry(at, []byte(`{"type":"user"`), cause).GetInternal().GetUnparsed()

	// Assert.
	if unparsed.GetSource() != at.Path {
		t.Fatalf("source = %q, want %q", unparsed.GetSource(), at.Path)
	}
	if unparsed.GetOffset() != uint64(at.Offset) {
		t.Fatalf("offset = %d, want %d", unparsed.GetOffset(), at.Offset)
	}
	if unparsed.GetParseError() != cause.Error() {
		t.Fatalf("parse_error = %q, want %q", unparsed.GetParseError(), cause.Error())
	}
	if unparsed.GetRaw() != `{"type":"user"` {
		t.Fatalf("raw = %q, want the bytes verbatim", unparsed.GetRaw())
	}
}

// A corrupt line is evidence, not a payload: an unbounded copy of a multi-megabyte
// one would be written to the store on every re-read.
func TestUnparsedRawIsBounded(t *testing.T) {
	// Arrange.
	raw := make([]byte, maxUnparsedRaw+4096)

	// Act.
	unparsed := UnparsedEntry(testAttribution(), raw, errors.New("boom")).GetInternal().GetUnparsed()

	// Assert.
	if len(unparsed.GetRaw()) != maxUnparsedRaw {
		t.Fatalf("raw length = %d, want it capped at %d", len(unparsed.GetRaw()), maxUnparsedRaw)
	}
}

// ---------------------------------------------------------------------------
// journal records
// ---------------------------------------------------------------------------

// A journal record is the OUTPUT of a run, so it accumulates into the run's own
// card rather than becoming a feed row of its own.
func TestJournalRecordAppendsToTheRunsCard(t *testing.T) {
	// Arrange.
	c := testConverter(t)
	record := map[string]any{"type": "started", "key": "build"}

	// Act.
	message := single(t, c.JournalRecord(record, testAttribution(), "wf_1")).GetExternal().GetMessage()

	// Assert.
	if message.GetMessageId() != "dw:wf_1" {
		t.Fatalf("message_id = %q, want the run's card %q", message.GetMessageId(), "dw:wf_1")
	}
	if message.GetDetachedWorkProgressed() == nil {
		t.Fatal("a journal step did not become progress on the run")
	}
}

// Without the run's identity there is no card to append to, and no arm for
// progress that names no work.
func TestJournalRecordWithoutARunIsStoredUnconverted(t *testing.T) {
	// Arrange.
	c := testConverter(t)

	// Act.
	entry := single(t, c.JournalRecord(map[string]any{"type": "started"}, testAttribution(), ""))

	// Assert.
	if entry.GetExternal() != nil {
		t.Fatal("a journal record with no run reached the daemon")
	}
}

// A journal record type this reader does not know is stored whole rather than
// rendered as a blank step, because only one of the two is reversible.
func TestUnknownJournalRecordTypeIsStoredUnconverted(t *testing.T) {
	// Arrange.
	c := testConverter(t)

	// Act.
	entry := single(t, c.JournalRecord(map[string]any{"type": "brand-new"}, testAttribution(), "wf_1"))

	// Assert.
	if entry.GetExternal() != nil {
		t.Fatal("an unmodeled journal record was rendered into the run's output")
	}
	if got := entry.GetInternal().GetUnknown().GetDiscriminator(); got != "brand-new" {
		t.Fatalf("discriminator = %q, want %q", got, "brand-new")
	}
}
