package convert

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

// toolReturn converts one tool result through the full path, which is the only
// way a ToolResultContent is ever built.
func toolReturn(t *testing.T, content any) *conversationv1.ToolResultContent {
	t.Helper()
	c := testConverter(t)
	c.toolCallOwner["call-1"] = "m1"
	entries := c.Line(map[string]any{
		"type": "user", "uuid": "u1",
		"message": map[string]any{"content": []any{
			map[string]any{"type": "tool_result", "tool_use_id": "call-1", "content": content},
		}},
	}, testAttribution(), nil)
	return entries[0].GetExternal().GetMessage().GetToolReturned().GetContent()
}

// A tool's output is its own union, separate from the user's, precisely because
// the vendor delivers results inside user-role records — reusing the user's
// union here would preserve in a new schema the accident it exists to erase.
func TestToolResultContentAcceptsBothVendorSpellings(t *testing.T) {
	tests := []struct {
		name    string
		content any
	}{
		{name: "bare string", content: "the output"},
		{name: "block array", content: []any{map[string]any{"type": "text", "text": "the output"}}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act.
			blocks := toolReturn(t, tc.content).GetBlocks()

			// Assert.
			if len(blocks) != 1 || blocks[0].GetText().GetText() != "the output" {
				t.Fatalf("blocks = %v, want one text block", blocks)
			}
		})
	}
}

// An image a tool produced — a screenshot, a rendered chart — is a block a
// client draws, not text.
func TestToolResultImageIsCarriedAsAnImage(t *testing.T) {
	// Arrange / Act.
	blocks := toolReturn(t, []any{map[string]any{
		"type":   "image",
		"source": map[string]any{"type": "base64", "media_type": "image/png", "data": "iVBOR"},
	}}).GetBlocks()

	// Assert.
	if got := blocks[0].GetImage().GetMediaType(); got != "image/png" {
		t.Fatalf("media_type = %q, want %q", got, "image/png")
	}
}

// ImageBlock carries an image BY REFERENCE. The vendor's transcript carries
// base64 bytes inline, so there is no reference to give — the media type is
// stated and the source left empty rather than inventing a path or inlining
// megabytes into a record replayed on every page load.
func TestInlineImageBytesYieldNoSource(t *testing.T) {
	// Arrange / Act.
	image := toolReturn(t, []any{map[string]any{
		"type":   "image",
		"source": map[string]any{"type": "base64", "media_type": "image/png", "data": "iVBOR"},
	}}).GetBlocks()[0].GetImage()

	// Assert.
	if image.GetSource() != "" {
		t.Fatalf("source = %q; inline bytes were passed off as a reference", image.GetSource())
	}
}

// A URL-sourced image is the one case the vendor DOES give a reference for, and
// it is carried as one.
func TestUrlSourcedImageKeepsItsReference(t *testing.T) {
	// Arrange / Act.
	image := toolReturn(t, []any{map[string]any{
		"type":   "image",
		"source": map[string]any{"type": "url", "media_type": "image/png", "url": "https://example.test/a.png"},
	}}).GetBlocks()[0].GetImage()

	// Assert.
	if image.GetSource() != "https://example.test/a.png" {
		t.Fatalf("source = %q, want the URL", image.GetSource())
	}
}

// An MCP or server tool result whose payload is one object is kept WHOLE rather
// than flattened, so its shape stays recoverable from stored data.
func TestObjectShapedToolResultIsKeptWhole(t *testing.T) {
	// Arrange / Act.
	blocks := toolReturn(t, map[string]any{"error_code": "unavailable"}).GetBlocks()

	// Assert.
	unsupported := blocks[0].GetUnsupported()
	if unsupported == nil {
		t.Fatalf("blocks = %v, want the object kept as an unsupported block", blocks)
	}
	if unsupported.GetRaw().GetFields()["error_code"].GetStringValue() != "unavailable" {
		t.Fatal("the object's own fields were not preserved")
	}
}

// A block kind a tool produced that this schema does not model is kept whole
// too, for the same reason.
func TestUnmodeledToolResultBlockIsKeptWhole(t *testing.T) {
	// Arrange / Act.
	blocks := toolReturn(t, []any{map[string]any{"type": "resource_link", "uri": "file:///x"}}).GetBlocks()

	// Assert.
	if got := blocks[0].GetUnsupported().GetKind(); got != "resource_link" {
		t.Fatalf("unsupported kind = %q, want %q", got, "resource_link")
	}
}

// A person pastes images, which is why UserContent is not a bare text block.
func TestUserImageIsCarriedAsAnImage(t *testing.T) {
	// Arrange.
	c := testConverter(t)
	record := map[string]any{"type": "user", "uuid": "u1", "message": map[string]any{"content": []any{
		map[string]any{"type": "image", "source": map[string]any{"media_type": "image/jpeg"}},
	}}}

	// Act.
	blocks := single(t, c.Line(record, testAttribution(), nil)).
		GetExternal().GetMessage().GetUserSaid().GetContent().GetBlocks()

	// Assert.
	if got := blocks[0].GetImage().GetMediaType(); got != "image/jpeg" {
		t.Fatalf("media_type = %q, want %q", got, "image/jpeg")
	}
}

// A block kind a person's message carries that this schema does not model is
// kept rather than dropped, so the decision stays reversible.
func TestUnmodeledUserBlockIsKeptWhole(t *testing.T) {
	// Arrange.
	c := testConverter(t)
	record := map[string]any{"type": "user", "uuid": "u1", "message": map[string]any{"content": []any{
		map[string]any{"type": "document", "title": "spec"},
	}}}

	// Act.
	blocks := single(t, c.Line(record, testAttribution(), nil)).
		GetExternal().GetMessage().GetUserSaid().GetContent().GetBlocks()

	// Assert.
	if got := blocks[0].GetUnsupported().GetKind(); got != "document" {
		t.Fatalf("unsupported kind = %q, want %q", got, "document")
	}
}

// The vendor's own recorded API error is a conversation record because the CLI
// made it durable: a reader scrolling back must see the turn failed rather than
// find it merely absent.
func TestSystemApiErrorBecomesAFailureCard(t *testing.T) {
	// Arrange.
	c := testConverter(t)
	record := map[string]any{
		"type": "system", "uuid": "s1", "subtype": "api_error",
		"error":     map[string]any{"message": "overloaded", "formatted": "529 overloaded_error"},
		"retryInMs": float64(2000),
	}

	// Act.
	failure := single(t, c.Line(record, testAttribution(), nil)).
		GetExternal().GetMessage().GetFailureRaised()

	// Assert.
	if failure.GetSummary() != "overloaded" {
		t.Fatalf("summary = %q, want the vendor's own message", failure.GetSummary())
	}
	if failure.GetDetail() != "529 overloaded_error" {
		t.Fatalf("detail = %q, want the vendor's formatted error", failure.GetDetail())
	}
	if failure.GetRetryInMs() != 2000 {
		t.Fatalf("retry_in_ms = %d, want 2000", failure.GetRetryInMs())
	}
}

// Zero means the vendor said NOTHING about retrying, not that it will retry
// immediately, so an absent value must not be invented.
func TestApiErrorWithNoRetryHintReportsZero(t *testing.T) {
	// Arrange.
	c := testConverter(t)
	record := map[string]any{
		"type": "system", "uuid": "s1", "subtype": "api_error",
		"error": map[string]any{"message": "bad request"},
	}

	// Act.
	failure := single(t, c.Line(record, testAttribution(), nil)).
		GetExternal().GetMessage().GetFailureRaised()

	// Assert.
	if failure.GetRetryInMs() != 0 {
		t.Fatalf("retry_in_ms = %d, want 0 for a vendor that said nothing", failure.GetRetryInMs())
	}
}

// A compaction summary spelled as blocks is the same summary spelled as a
// string, and a boundary must find it either way.
func TestCompactionSummaryIsReadFromEitherSpelling(t *testing.T) {
	tests := []struct {
		name    string
		content any
	}{
		{name: "bare string", content: "we were doing X"},
		{name: "block array", content: []any{map[string]any{"type": "text", "text": "we were doing X"}}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			c := testConverter(t)
			boundary := map[string]any{"type": "system", "uuid": "s1", "subtype": "compact_boundary"}
			summary := map[string]any{
				"type": "user", "uuid": "u1", "isCompactSummary": true,
				"message": map[string]any{"content": tc.content},
			}

			// Act.
			compacted := single(t, c.Line(boundary, testAttribution(), summary)).
				GetExternal().GetMessage().GetContextCut().GetCompacted()

			// Assert.
			if got := compacted.GetSummary().GetBlocks()[0].GetText().GetText(); got != "we were doing X" {
				t.Fatalf("summary = %q, want the text either spelling carries", got)
			}
		})
	}
}

// A line following a boundary that is NOT a summary settles the boundary without
// one, rather than being read as a summary because it happened to be next.
func TestALineThatIsNotASummaryDoesNotBecomeOne(t *testing.T) {
	// Arrange.
	c := testConverter(t)
	boundary := map[string]any{"type": "system", "uuid": "s1", "subtype": "compact_boundary"}
	next := map[string]any{"type": "user", "uuid": "u1", "message": map[string]any{"content": "an actual prompt"}}

	// Act.
	compacted := single(t, c.Line(boundary, testAttribution(), next)).
		GetExternal().GetMessage().GetContextCut().GetCompacted()

	// Assert.
	if compacted.GetSummary() != nil {
		t.Fatalf("summary = %v; an ordinary prompt was read as the compaction summary", compacted.GetSummary())
	}
}

// A negative counter must not wrap into an astronomically large unsigned count,
// which would read as a catastrophic bill.
func TestNegativeTokenCounterIsClampedRatherThanWrapped(t *testing.T) {
	// Arrange / Act.
	got := counter(float64(-5))

	// Assert.
	if got != 0 {
		t.Fatalf("counter(-5) = %d, want 0", got)
	}
}

// A harness-injected user record with no prose is not something a person said —
// a system reminder, an attachment carrier — and must not be rendered as one.
func TestMetaUserRecordWithNoProseIsNotAMessage(t *testing.T) {
	// Arrange.
	c := testConverter(t)
	record := map[string]any{
		"type": "user", "uuid": "u1", "isMeta": true,
		"message": map[string]any{"content": ""},
	}

	// Act.
	entry := single(t, c.Line(record, testAttribution(), nil))

	// Assert.
	if entry.GetExternal() != nil {
		t.Fatal("a harness-injected record was rendered as something the person said")
	}
}

// A meta record that DOES carry prose is still a message: the flag says who
// injected it, not whether there is anything to read.
func TestMetaUserRecordWithProseIsStillAMessage(t *testing.T) {
	// Arrange.
	c := testConverter(t)
	record := map[string]any{
		"type": "user", "uuid": "u1", "isMeta": true,
		"message": map[string]any{"content": "continue where you left off"},
	}

	// Act.
	entry := single(t, c.Line(record, testAttribution(), nil))

	// Assert.
	if entry.GetExternal().GetMessage().GetUserSaid() == nil {
		t.Fatal("a meta record carrying prose was withheld from the feed")
	}
}
