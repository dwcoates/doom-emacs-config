package convert

// assistant_test.go — one API response becoming several units, and where the
// response's accounting rides.

import (
	"bytes"
	"io"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
)

const ts1 = "2026-07-22T19:58:36.000Z"

func TestAssistantMessageBecomesOneUnitPerContentBlock(t *testing.T) {
	// Arrange. Collapsing an assistant message into one row is the single most
	// damaging thing this converter could do: the last block would be the only
	// one stored.
	c := newTestConverter(t)
	blocks := `{"type":"thinking","thinking":"reasoning"},{"type":"text","text":"prose"},` +
		toolCall("toolu_a", "Read", `{"file_path":"/f.go"}`)

	// Act.
	entries := convertLines(t, c, assistantWith("a1", "msg_1", ts1, blocks))

	// Assert: three distinct units under three distinct keys.
	if len(entries) != 3 {
		t.Fatalf("entries = %d, want 3: keys=%v", len(entries), allKeys(entries))
	}
	entryByKey(t, entries, ActivityKey(BlockActivityID("msg_1", 0)))
	entryByKey(t, entries, ActivityKey(BlockActivityID("msg_1", 1)))
	entryByKey(t, entries, ActivityKey("toolu_a"))
}

func TestBlockOrdinalsContinueAcrossLinesSharingOneMessageId(t *testing.T) {
	// Arrange. The vendor splits ONE response over several lines. Counting per
	// line would restart at zero and mint the same activity id for different
	// blocks; the ordinal must equal the block's position in the API MESSAGE, so
	// it matches the stream plane's content_block_start.index.
	c := newTestConverter(t)
	first := assistantWith("a1", "msg_1", ts1, `{"type":"thinking","thinking":"r"}`)
	second := assistantWith("a2", "msg_1", ts1, `{"type":"text","text":"p"}`)

	// Act.
	entries := convertLines(t, c, first, second)

	// Assert: block 0 then block 1, never block 0 twice.
	entryByKey(t, entries, ActivityKey(BlockActivityID("msg_1", 0)))
	entryByKey(t, entries, ActivityKey(BlockActivityID("msg_1", 1)))
}

func TestBlockOrdinalsRestartWhenTheResponseChanges(t *testing.T) {
	// Arrange.
	c := newTestConverter(t)
	first := assistantWith("a1", "msg_1", ts1, `{"type":"text","text":"one"}`)
	second := assistantWith("a2", "msg_2", ts1, `{"type":"text","text":"two"}`)

	// Act.
	entries := convertLines(t, c, first, second)

	// Assert.
	entryByKey(t, entries, ActivityKey(BlockActivityID("msg_1", 0)))
	entryByKey(t, entries, ActivityKey(BlockActivityID("msg_2", 0)))
}

func TestExactlyOneUnitPerResponseCarriesUsage(t *testing.T) {
	// Arrange. A producer stamping usage on each unit would make any consumer
	// summing units over-count the bill by the number of blocks.
	c := newTestConverter(t)
	blocks := `{"type":"text","text":"a"},{"type":"text","text":"b"},{"type":"text","text":"c"}`
	line := `{"type":"assistant","uuid":"a1","isSidechain":false,"timestamp":"` + ts1 +
		`","message":{"id":"msg_1","role":"assistant","content":[` + blocks +
		`],"usage":{"input_tokens":2,"cache_creation_input_tokens":14001,"cache_read_input_tokens":17028,"output_tokens":120}}}`

	// Act.
	entries := convertLines(t, c, line)

	// Assert.
	carrying := 0
	for _, e := range entries {
		if activityOf(e).GetUsage() != nil {
			carrying++
		}
	}
	if carrying != 1 {
		t.Fatalf("units carrying usage = %d, want exactly 1", carrying)
	}
	first := entryByKey(t, entries, ActivityKey(BlockActivityID("msg_1", 0)))
	if activityOf(first).GetUsage() == nil {
		t.Fatal("usage must ride the response's FIRST content block's unit")
	}
}

func TestUsageIsOrganizedByEconomicsNotByVendorFieldNames(t *testing.T) {
	// Arrange. Reading any one vendor input counter as "the cost" is the mistake
	// the canonical shape makes unrepresentable: cache reads are the cheap
	// bucket, and BOTH miss buckets are expensive.
	c := newTestConverter(t)
	line := `{"type":"assistant","uuid":"a1","isSidechain":false,"timestamp":"` + ts1 +
		`","message":{"id":"msg_1","role":"assistant","content":[{"type":"text","text":"a"}],` +
		`"usage":{"input_tokens":11,"cache_creation_input_tokens":22,"cache_read_input_tokens":33,` +
		`"output_tokens":44,"output_thinking_tokens":5}}}`

	// Act.
	entries := convertLines(t, c, line)

	// Assert.
	usage := activityOf(entries[0]).GetUsage()
	if got := usage.GetInputHits().GetRead(); got != 33 {
		t.Fatalf("input_hits.read = %d, want the vendor's cache_read_input_tokens (33)", got)
	}
	if got := usage.GetInputMisses().GetWritten(); got != 22 {
		t.Fatalf("input_misses.written = %d, want cache_creation_input_tokens (22)", got)
	}
	if got := usage.GetInputMisses().GetUnwritten(); got != 11 {
		t.Fatalf("input_misses.unwritten = %d, want input_tokens (11)", got)
	}
	if got := usage.GetOutputTokens(); got != 44 {
		t.Fatalf("output_tokens = %d, want 44", got)
	}
	if got := usage.GetOutputThinkingTokens(); got != 5 {
		t.Fatalf("output_thinking_tokens = %d, want 5 (a PARTITION of output_tokens, never an addition)", got)
	}
}

func TestEffortRidesTheSameUnitAsUsage(t *testing.T) {
	// Arrange.
	c := newTestConverter(t)
	line := `{"type":"assistant","uuid":"a1","isSidechain":false,"effort":"high","timestamp":"` + ts1 +
		`","message":{"id":"msg_1","role":"assistant","content":[{"type":"text","text":"a"},{"type":"text","text":"b"}]}}`

	// Act.
	entries := convertLines(t, c, line)

	// Assert.
	first := entryByKey(t, entries, ActivityKey(BlockActivityID("msg_1", 0)))
	if got := activityOf(first).GetEffort(); got != conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_HIGH {
		t.Fatalf("effort = %v, want HIGH", got)
	}
	second := entryByKey(t, entries, ActivityKey(BlockActivityID("msg_1", 1)))
	if activityOf(second).Effort != nil {
		t.Fatal("only one unit per response may carry effort")
	}
}

func TestUnreportedEffortStaysUnsetRatherThanLow(t *testing.T) {
	// Arrange. UNSET means the producer reported no level, NEVER that it was low:
	// a consumer that draws it must draw nothing.
	c := newTestConverter(t)

	// Act.
	entries := convertLines(t, c, assistantWith("a1", "msg_1", ts1, `{"type":"text","text":"a"}`))

	// Assert.
	if activityOf(entries[0]).Effort != nil {
		t.Fatal("an absent effort must stay UNSET, not default to a level")
	}
}

func TestWithheldReasoningSettlesWithheldRatherThanAsEmptyText(t *testing.T) {
	// Arrange. The model emits a signature and no text; a consumer draws NOTHING
	// once it settles, which is different from drawing an empty card.
	c := newTestConverter(t)

	// Act.
	entries := convertLines(t, c, assistantWith("a1", "msg_1", ts1, `{"type":"thinking","signature":"sig"}`))

	// Assert.
	thinking := activityOf(entries[0]).GetThinking().GetSuccess()
	if thinking.GetWithheld() == nil {
		t.Fatal("reasoning with no text must settle on the withheld arm")
	}
	if thinking.GetText() != nil {
		t.Fatal("a withheld block must not carry an empty text arm")
	}
}

func TestProseSettlesAsFromModelByDefault(t *testing.T) {
	// Arrange.
	c := newTestConverter(t)

	// Act.
	entries := convertLines(t, c, assistantWith("a1", "msg_1", ts1, `{"type":"text","text":"the answer"}`))

	// Assert.
	success := activityOf(entries[0]).GetResponse().GetSuccess()
	if success.GetFromModel() == nil {
		t.Fatal("ordinary prose must be attributed to the model")
	}
	if got := success.GetProse().GetMarkdown(); got != "the answer" {
		t.Fatalf("prose = %q, want the text verbatim", got)
	}
}

func TestVendorSynthesizedNoticeIsNotDrawnAsTheAgentsAnswer(t *testing.T) {
	// Arrange. The vendor synthesizes error notices AS assistant prose, so a
	// consumer that trusts the shape draws an outage as something the agent said.
	// Only the producer sees the markers.
	c := newTestConverter(t)
	line := `{"type":"assistant","uuid":"a1","isSidechain":false,"isApiErrorMessage":true,"timestamp":"` + ts1 +
		`","message":{"id":"msg_1","role":"assistant","content":[{"type":"text","text":"API Error: overloaded"}]}}`

	// Act.
	entries := convertLines(t, c, line)

	// Assert.
	success := activityOf(entries[0]).GetResponse().GetSuccess()
	if success.GetSynthesizedNotice() == nil {
		t.Fatal("a vendor notice must be marked as synthesized, never as the model's answer")
	}
	if success.GetFromModel() != nil {
		t.Fatal("a synthesized notice must not claim model authorship")
	}
}

func TestSynthesizedNoticeSubjectsTable(t *testing.T) {
	// Arrange. WHAT the notice is about decides whether a surface can route it:
	// an exhausted allowance is actionable, a transient warning is not.
	tests := []struct {
		name    string
		text    string
		usage   bool
		warning bool
	}{
		{name: "spend limit is an exhausted allowance", text: "You've hit your monthly spend limit", usage: true},
		{name: "approaching is a warning, not a block", text: "API Error: approaching your limit", warning: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			c := newTestConverter(t)
			line := `{"type":"assistant","uuid":"a1","isSidechain":false,"isApiErrorMessage":true,"timestamp":"` + ts1 +
				`","message":{"id":"msg_1","role":"assistant","content":[{"type":"text","text":"` + tc.text + `"}]}}`

			// Act.
			entries := convertLines(t, c, line)

			// Assert.
			notice := activityOf(entries[0]).GetResponse().GetSuccess().GetSynthesizedNotice()
			if tc.usage && notice.GetUsageLimit() == nil {
				t.Fatalf("text %q must classify as a usage limit", tc.text)
			}
			if tc.warning && notice.GetUsageWarning() == nil {
				t.Fatalf("text %q must classify as a usage warning", tc.text)
			}
		})
	}
}

func TestSyntheticNoResponseRecordIsWithheld(t *testing.T) {
	// Arrange. No model wrote these words; it is the vendor's own placeholder for
	// a turn that produced nothing.
	c := newTestConverter(t)

	// Act.
	entries := convertLines(t, c, assistantWith("a1", "msg_1", ts1, `{"type":"text","text":"No response requested."}`))

	// Assert.
	if len(entries) != 1 {
		t.Fatalf("entries = %d, want 1", len(entries))
	}
	if got := vendorKindOf(entries[0]); got != "assistant/no_response_requested" {
		t.Fatalf("kind = %q, want the withheld placeholder", got)
	}
}

func TestUnmodeledContentBlockIsNotDrawnAsProse(t *testing.T) {
	// Arrange. A block kind this schema does not model must not be turned into
	// prose the agent did not write.
	c := newTestConverter(t)

	// Act.
	entries := convertLines(t, c, assistantWith("a1", "msg_1", ts1, `{"type":"fallback","note":"x"}`))

	// Assert.
	if got := vendorKindOf(entries[0]); got != "content_block/fallback" {
		t.Fatalf("kind = %q, want content_block/fallback", got)
	}
}

// loggedConverter is a converter whose records are readable, for the subjects
// that are ABOUT what the log says.
func loggedConverter(t *testing.T) (*Converter, *bytes.Buffer) {
	t.Helper()
	sink := &bytes.Buffer{}
	return New(logging.New(io.Discard, sink).With(logging.Context{Component: "test"})), sink
}

// TestASpawnOnlyResponseNamesTheDeferredAnnounce covers the shape the reader
// misdescribed on the owner's machine: a response whose only block is a
// subagent SPAWN produces no units because the spawn announces at its RESULT,
// which is a decision this converter took — not the exempt set, which it did
// not.
func TestASpawnOnlyResponseNamesTheDeferredAnnounce(t *testing.T) {
	// Arrange.
	c, sink := loggedConverter(t)

	// Act.
	entries := convertLines(t, c, assistantWith("a1", "msg_1", ts1,
		toolCall("toolu_spawn", "Agent", `{"subagent_type":"Explore","prompt":"look"}`)))

	// Assert.
	if len(entries) != 0 {
		t.Fatalf("a spawn-only response minted %d entry(ies), want the unit deferred to the launch's answer", len(entries))
	}
	if !strings.Contains(sink.String(), "announces at its result") {
		t.Errorf("the no-units record does not name the deferred announce: %s", sink.String())
	}
	if strings.Contains(sink.String(), "exempt set") {
		t.Errorf("the no-units record blames the exempt set for a deferred announce: %s", sink.String())
	}
}

// TestAnExemptOnlyResponseNamesTheExemptSet covers the other cause, so the two
// stay distinguishable in the log rather than collapsing back onto one
// sentence.
func TestAnExemptOnlyResponseNamesTheExemptSet(t *testing.T) {
	// Arrange.
	c, sink := loggedConverter(t)

	// Act.
	entries := convertLines(t, c, assistantWith("a1", "msg_1", ts1,
		toolCall("toolu_search", "ToolSearch", `{"query":"select:Read"}`)))

	// Assert.
	if len(entries) != 0 {
		t.Fatalf("an exempt-only response minted %d entry(ies), want none", len(entries))
	}
	if !strings.Contains(sink.String(), "the tool is in the exempt set") {
		t.Errorf("the no-units record does not name the exempt set: %s", sink.String())
	}
}

// TestAResponseWithBothCausesNamesBoth covers the mixed record: naming only the
// first cause would send a reader looking for a bug in the wrong half.
func TestAResponseWithBothCausesNamesBoth(t *testing.T) {
	// Arrange.
	c, sink := loggedConverter(t)

	// Act.
	entries := convertLines(t, c, assistantWith("a1", "msg_1", ts1,
		toolCall("toolu_search", "ToolSearch", `{"query":"select:Read"}`)+","+
			toolCall("toolu_spawn", "Agent", `{"subagent_type":"Explore","prompt":"look"}`)))

	// Assert.
	if len(entries) != 0 {
		t.Fatalf("the response minted %d entry(ies), want none", len(entries))
	}
	logged := sink.String()
	if !strings.Contains(logged, "the tool is in the exempt set") || !strings.Contains(logged, "announces at its result") {
		t.Errorf("the no-units record names only one of the two causes: %s", logged)
	}
}

// TestDescribeNoUnitsDoesNotRepeatOneCause covers the rendering: five exempt
// blocks are one reason, not five.
func TestDescribeNoUnitsDoesNotRepeatOneCause(t *testing.T) {
	// Arrange / Act.
	got := describeNoUnits([]noUnitReason{reasonExempt, reasonExempt, reasonExempt})

	// Assert.
	if strings.Count(got, string(reasonExempt)) != 1 {
		t.Errorf("describeNoUnits = %q, want one cause stated once", got)
	}
}

// TestDescribeNoUnitsCallsAnUnnamedCauseAModellingGap covers the case no block
// owner accounted for: a record producing nothing for a reason nobody decided
// is a gap, and saying so is the point of the record.
func TestDescribeNoUnitsCallsAnUnnamedCauseAModellingGap(t *testing.T) {
	// Arrange / Act.
	got := describeNoUnits(nil)

	// Assert.
	if !strings.Contains(got, "modelling gap") {
		t.Errorf("describeNoUnits(nil) = %q, want it named as a modelling gap", got)
	}
}
