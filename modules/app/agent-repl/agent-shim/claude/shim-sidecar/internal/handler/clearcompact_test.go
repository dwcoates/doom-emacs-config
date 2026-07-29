package handler

import (
	"encoding/json"
	"fmt"
	"io"
	"strings"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

// --- fixtures ---------------------------------------------------------------

// clearEnvelope is how the harness actually records a slash command: the
// expanded envelope, never the typed text.
func clearEnvelope(name, args string) string {
	return fmt.Sprintf("<command-name>%s</command-name>\n            <command-message>%s</command-message>\n            <command-args>%s</command-args>",
		name, strings.TrimPrefix(name, "/"), args)
}

// userLineJSON builds a transcript `user` line carrying content.
func userLineJSON(uuid, content string) string {
	obj := map[string]any{
		"type":       "user",
		"uuid":       uuid,
		"parentUuid": "parent-of-" + uuid,
		"sessionId":  "s1",
		"timestamp":  "2026-07-07T22:01:31.000Z",
		"message":    map[string]any{"role": "user", "content": content},
	}
	b, _ := json.Marshal(obj)
	return string(b)
}

// summaryLineJSON builds the compaction SUMMARY line: typed `user`, flagged
// isCompactSummary, and (per the real transcript) stamped one millisecond
// BEFORE the boundary it belongs to.
func summaryLineJSON(uuid, boundaryUUID, timestamp, text string) string {
	obj := map[string]any{
		"type":                      "user",
		"uuid":                      uuid,
		"parentUuid":                boundaryUUID,
		"sessionId":                 "s1",
		"timestamp":                 timestamp,
		"isCompactSummary":          true,
		"isVisibleInTranscriptOnly": true,
		"message":                   map[string]any{"role": "user", "content": text},
	}
	b, _ := json.Marshal(obj)
	return string(b)
}

// boundaryLineJSON builds a `compact_boundary` system line.
func boundaryLineJSON(uuid, timestamp, trigger string, pre, post, durMs int64) string {
	obj := map[string]any{
		"type":       "system",
		"subtype":    "compact_boundary",
		"uuid":       uuid,
		"parentUuid": nil,
		"sessionId":  "s1",
		"timestamp":  timestamp,
		"content":    "Conversation compacted",
		"level":      "info",
		"compactMetadata": map[string]any{
			"trigger":    trigger,
			"preTokens":  pre,
			"postTokens": post,
			"durationMs": durMs,
		},
	}
	b, _ := json.Marshal(obj)
	return string(b)
}

// framesFor turns raw JSONL lines into decoded frames, preserving FILE ORDER.
func framesFor(t *testing.T, lines ...string) []tail.Frame {
	t.Helper()
	out := make([]tail.Frame, 0, len(lines))
	for i, ln := range lines {
		var obj map[string]any
		if err := json.Unmarshal([]byte(ln), &obj); err != nil {
			t.Fatalf("fixture line %d is not JSON: %v", i, err)
		}
		out = append(out, tail.Frame{Obj: obj, Raw: []byte(ln), Offset: int64(i)})
	}
	return out
}

func findCleared(evs []*corev1.Event) *corev1.Event {
	for _, e := range evs {
		if e.GetContextCleared() != nil {
			return e
		}
	}
	return nil
}

func findCompacted(evs []*corev1.Event) *corev1.Event {
	for _, e := range evs {
		if e.GetContextCompacted() != nil {
			return e
		}
	}
	return nil
}

func countCompacted(evs []*corev1.Event) int {
	n := 0
	for _, e := range evs {
		if e.GetContextCompacted() != nil {
			n++
		}
	}
	return n
}

// collectLog captures the loud-log lines a handler emits.
func collectLog(dst *[]string) *logging.Bound {
	log := logging.New(io.Discard, io.Discard).With(logging.Context{Component: "test"})
	log.SetDiagnosticSink(func(d logging.Diagnostic) { *dst = append(*dst, d.Message) })
	return log
}

// --- clear detection --------------------------------------------------------

func TestIsClearCommand(t *testing.T) {
	tests := []struct {
		name    string
		content string
		want    bool
	}{
		{name: "expanded envelope", content: clearEnvelope("/clear", ""), want: true},
		{name: "bare text", content: "/clear", want: true},
		{name: "bare text with surrounding whitespace", content: "  /clear\n", want: true},
		{name: "envelope with whitespace-only args", content: clearEnvelope("/clear", "   "), want: true},
		{name: "envelope with arguments", content: clearEnvelope("/clear", "all"), want: false},
		{name: "bare text with arguments", content: "/clear all", want: false},
		{name: "a different command", content: clearEnvelope("/compact", ""), want: false},
		{name: "envelope without the leading slash", content: clearEnvelope("clear", ""), want: false},
		{name: "envelope quoted inside prose", content: "run this: " + clearEnvelope("/clear", ""), want: false},
		{name: "envelope with trailing prose", content: clearEnvelope("/clear", "") + " and then stop", want: false},
		{name: "unterminated command-name tag", content: "<command-name>/clear", want: false},
		{name: "empty content", content: "", want: false},
		{name: "prose mentioning clear", content: "please clear the context", want: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			msg := &datav1.ApiUserMessage{
				Content: &datav1.ApiUserMessage_ContentString{ContentString: tc.content},
			}
			// Act
			got := isClearCommand(msg)
			// Assert
			if got != tc.want {
				t.Fatalf("isClearCommand(%q) = %v, want %v", tc.content, got, tc.want)
			}
		})
	}
}

func TestIsClearCommandEnvelopeWithoutArgsElement(t *testing.T) {
	// Arrange: older harness versions write no <command-args> element at all; a
	// missing element is an empty argument list, not a malformed envelope.
	msg := &datav1.ApiUserMessage{Content: &datav1.ApiUserMessage_ContentString{
		ContentString: "<command-name>/clear</command-name>\n<command-message>clear</command-message>",
	}}
	// Act
	got := isClearCommand(msg)
	// Assert
	if !got {
		t.Fatal("an envelope with no <command-args> element was not read as a clear")
	}
}

func TestIsClearCommandEnvelopeTagsOutOfOrder(t *testing.T) {
	// Arrange: the elements are matched by name, so their order on disk is not
	// part of the contract.
	msg := &datav1.ApiUserMessage{Content: &datav1.ApiUserMessage_ContentString{
		ContentString: "<command-args></command-args><command-message>clear</command-message><command-name>/clear</command-name>",
	}}
	// Act
	got := isClearCommand(msg)
	// Assert
	if !got {
		t.Fatal("a reordered command envelope was not read as a clear")
	}
}

func TestIsClearCommandBlocksContentIsNeverAClear(t *testing.T) {
	// Arrange: a blocks-form user message whose single text block says "/clear".
	msg := &datav1.ApiUserMessage{Content: &datav1.ApiUserMessage_ContentBlocks{
		ContentBlocks: &datav1.ApiContentBlocks{Blocks: []*datav1.ContentBlock{{
			Block: &datav1.ContentBlock_Text{Text: &datav1.TextBlock{Text: "/clear"}},
		}}},
	}}
	// Act
	got := isClearCommand(msg)
	// Assert
	if got {
		t.Fatal("a blocks-form message was read as a clear; only the string form carries a command envelope")
	}
}

func TestIsClearCommandNilMessageIsNotAClear(t *testing.T) {
	// Arrange / Act
	got := isClearCommand(nil)
	// Assert
	if got {
		t.Fatal("a line with no message was read as a clear")
	}
}

// --- clear emission ---------------------------------------------------------

func TestSessionHandlerEnvelopeClearEmitsContextCleared(t *testing.T) {
	// Arrange
	h := NewSessionTranscriptHandler(quietLog)
	frames := framesFor(t, userLineJSON("u-clear", clearEnvelope("/clear", "")))
	// Act
	evs := h.Handle(frames, &Context{SessionID: "s1"})
	// Assert
	if findCleared(evs) == nil {
		t.Fatalf("no ContextCleared emitted for the expanded /clear envelope; got %d events", len(evs))
	}
}

func TestSessionHandlerClearDedupKeyIsClearColonUUID(t *testing.T) {
	// Arrange
	h := NewSessionTranscriptHandler(quietLog)
	frames := framesFor(t, userLineJSON("u-clear", clearEnvelope("/clear", "")))
	// Act
	ev := findCleared(h.Handle(frames, &Context{SessionID: "s1"}))
	// Assert: the sidecar is the sole producer, so the key names this line for
	// this producer alone.
	if ev.GetDedupKey() != "clear:u-clear" {
		t.Fatalf("dedup_key = %q, want %q", ev.GetDedupKey(), "clear:u-clear")
	}
}

func TestSessionHandlerBareTextClearEmitsContextCleared(t *testing.T) {
	// Arrange
	h := NewSessionTranscriptHandler(quietLog)
	frames := framesFor(t, userLineJSON("u-bare", "/clear"))
	// Act
	evs := h.Handle(frames, &Context{SessionID: "s1"})
	// Assert
	if findCleared(evs) == nil {
		t.Fatal("no ContextCleared emitted for a bare-text /clear prompt")
	}
}

func TestSessionHandlerClearWithArgumentsEmitsNoClear(t *testing.T) {
	// Arrange
	h := NewSessionTranscriptHandler(quietLog)
	frames := framesFor(t, userLineJSON("u-args", clearEnvelope("/clear", "history")))
	// Act
	evs := h.Handle(frames, &Context{SessionID: "s1"})
	// Assert
	if findCleared(evs) != nil {
		t.Fatal("a /clear carrying arguments cleared the context; arguments mean it is not a clear")
	}
}

func TestSessionHandlerClearStillEmitsItsVendorTwin(t *testing.T) {
	// Arrange: the total-ingestion mandate — the neutral event never replaces the
	// vendor record of the line.
	h := NewSessionTranscriptHandler(quietLog)
	frames := framesFor(t, userLineJSON("u-clear", clearEnvelope("/clear", "")))
	// Act
	evs := h.Handle(frames, &Context{SessionID: "s1"})
	// Assert
	vendors := 0
	for _, e := range evs {
		if e.GetVendor() != nil {
			vendors++
		}
	}
	if vendors != 1 {
		t.Fatalf("vendor twins = %d, want 1", vendors)
	}
}

func TestSessionHandlerClearIsPersistent(t *testing.T) {
	// Arrange
	h := NewSessionTranscriptHandler(quietLog)
	frames := framesFor(t, userLineJSON("u-clear", clearEnvelope("/clear", "")))
	// Act
	ev := findCleared(h.Handle(frames, &Context{SessionID: "s1"}))
	// Assert: a ContextCleared must survive for replay.
	if ev.GetClass() != corev1.EventClass_EVENT_CLASS_PERSISTENT {
		t.Fatalf("class = %v, want PERSISTENT", ev.GetClass())
	}
}

func TestSessionHandlerClearIsOnTheFilePlane(t *testing.T) {
	// Arrange: the sidecar is the SOLE producer, and it only ever reads files.
	h := NewSessionTranscriptHandler(quietLog)
	frames := framesFor(t, userLineJSON("u-clear", clearEnvelope("/clear", "")))
	// Act
	ev := findCleared(h.Handle(frames, &Context{SessionID: "s1"}))
	// Assert
	if ev.GetPlane() != corev1.Plane_PLANE_FILE {
		t.Fatalf("plane = %v, want PLANE_FILE", ev.GetPlane())
	}
}

func TestSessionHandlerClearWithoutUUIDLoudLogsAndCarriesNoKey(t *testing.T) {
	// Arrange
	var logs []string
	h := NewSessionTranscriptHandler(collectLog(&logs))
	frames := framesFor(t, userLineJSON("", clearEnvelope("/clear", "")))
	// Act
	ev := findCleared(h.Handle(frames, &Context{SessionID: "s1"}))
	// Assert: the event is still emitted, keyless and loudly, never under a
	// fabricated key.
	if ev == nil {
		t.Fatal("a uuid-less clear was dropped")
	}
	if ev.GetDedupKey() != "" {
		t.Fatalf("dedup_key = %q, want empty", ev.GetDedupKey())
	}
	if !strings.Contains(strings.Join(logs, "\n"), "no uuid") {
		t.Fatalf("missing loud log for the uuid-less clear; got %v", logs)
	}
}

// --- compaction -------------------------------------------------------------

func TestSessionHandlerCompactBoundaryWithSummaryCoalesces(t *testing.T) {
	// Arrange: boundary at file line N, summary at N+1.
	h := NewSessionTranscriptHandler(quietLog)
	frames := framesFor(t,
		boundaryLineJSON("b1", "2026-07-07T22:01:31.660Z", "manual", 435029, 8639, 194511),
		summaryLineJSON("s1u", "b1", "2026-07-07T22:01:31.659Z", "the summary text"),
	)
	// Act
	cc := findCompacted(h.Handle(frames, &Context{SessionID: "s1"})).GetContextCompacted()
	// Assert
	if cc.GetSummary() != "the summary text" {
		t.Fatalf("summary = %q, want %q", cc.GetSummary(), "the summary text")
	}
}

func TestSessionHandlerCompactBoundaryCarriesMetadata(t *testing.T) {
	// Arrange
	h := NewSessionTranscriptHandler(quietLog)
	frames := framesFor(t,
		boundaryLineJSON("b1", "2026-07-07T22:01:31.660Z", "manual", 435029, 8639, 194511),
		summaryLineJSON("s1u", "b1", "2026-07-07T22:01:31.659Z", "the summary text"),
	)
	// Act
	cc := findCompacted(h.Handle(frames, &Context{SessionID: "s1"})).GetContextCompacted()
	// Assert
	if cc.GetPreTokens() != 435029 || cc.GetPostTokens() != 8639 || cc.GetDurationMs() != 194511 {
		t.Fatalf("pre/post/duration = %d/%d/%d, want 435029/8639/194511",
			cc.GetPreTokens(), cc.GetPostTokens(), cc.GetDurationMs())
	}
}

func TestSessionHandlerCompactDedupKeyIsCompactColonBoundaryUUID(t *testing.T) {
	// Arrange
	h := NewSessionTranscriptHandler(quietLog)
	frames := framesFor(t,
		boundaryLineJSON("b1", "2026-07-07T22:01:31.660Z", "auto", 1, 2, 3),
		summaryLineJSON("s1u", "b1", "2026-07-07T22:01:31.659Z", "text"),
	)
	// Act
	ev := findCompacted(h.Handle(frames, &Context{SessionID: "s1"}))
	// Assert: the BOUNDARY's uuid, not the summary's.
	if ev.GetDedupKey() != "compact:b1" {
		t.Fatalf("dedup_key = %q, want %q", ev.GetDedupKey(), "compact:b1")
	}
}

func TestSessionHandlerCompactTrigger(t *testing.T) {
	tests := []struct {
		name string
		disk string
		want corev1.ContextCompactTrigger
	}{
		{name: "manual", disk: "manual", want: corev1.ContextCompactTrigger_CONTEXT_COMPACT_TRIGGER_MANUAL},
		{name: "auto", disk: "auto", want: corev1.ContextCompactTrigger_CONTEXT_COMPACT_TRIGGER_AUTO},
		{name: "absent", disk: "", want: corev1.ContextCompactTrigger_CONTEXT_COMPACT_TRIGGER_UNSPECIFIED},
		{name: "unmodeled", disk: "sideways", want: corev1.ContextCompactTrigger_CONTEXT_COMPACT_TRIGGER_UNSPECIFIED},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := NewSessionTranscriptHandler(quietLog)
			frames := framesFor(t, boundaryLineJSON("b1", "2026-07-07T22:01:31.660Z", tc.disk, 1, 2, 3))
			// Act
			cc := findCompacted(h.Handle(frames, &Context{SessionID: "s1"})).GetContextCompacted()
			// Assert
			if cc.GetTrigger() != tc.want {
				t.Fatalf("trigger = %v, want %v", cc.GetTrigger(), tc.want)
			}
		})
	}
}

func TestSessionHandlerUnmodeledCompactTriggerLoudLogs(t *testing.T) {
	// Arrange
	var logs []string
	h := NewSessionTranscriptHandler(collectLog(&logs))
	frames := framesFor(t, boundaryLineJSON("b1", "2026-07-07T22:01:31.660Z", "sideways", 1, 2, 3))
	// Act
	h.Handle(frames, &Context{SessionID: "s1"})
	// Assert
	if !strings.Contains(strings.Join(logs, "\n"), "unmodeled trigger") {
		t.Fatalf("missing loud log for the unmodeled trigger; got %v", logs)
	}
}

func TestSessionHandlerCompactBoundaryFollowedByAnOrdinaryLineHasNoSummary(t *testing.T) {
	// Arrange: the line after the boundary is an ordinary prompt, not a summary.
	h := NewSessionTranscriptHandler(quietLog)
	frames := framesFor(t,
		boundaryLineJSON("b1", "2026-07-07T22:01:31.660Z", "auto", 1, 2, 3),
		userLineJSON("u1", "carry on"),
	)
	// Act
	cc := findCompacted(h.Handle(frames, &Context{SessionID: "s1"})).GetContextCompacted()
	// Assert: emitted, with no summary — never the neighbor's text.
	if cc.GetSummary() != "" {
		t.Fatalf("summary = %q, want empty", cc.GetSummary())
	}
}

func TestSessionHandlerCompactBoundaryAtBatchEndIsHeldForRedelivery(t *testing.T) {
	// Arrange: nothing follows the boundary in this batch, and the reader has
	// promised to hand back whatever the handler defers.
	h := NewSessionTranscriptHandler(quietLog)
	frames := framesFor(t, boundaryLineJSON("b1", "2026-07-07T22:01:31.660Z", "manual", 10, 5, 7))
	ctx := &Context{SessionID: "s1", Redelivers: true}
	// Act
	evs := h.Handle(frames, ctx)
	// Assert: the summary is the NEXT line in the file and may simply not be
	// written yet, so the boundary waits instead of emitting without it — and
	// the hold is recorded so the reader parks its cursor before the line.
	if findCompacted(evs) != nil {
		t.Fatal("a batch-terminal boundary emitted immediately; it must be held until the file says what follows it")
	}
	if ctx.HeldDeliveries != 1 || ctx.HeldOffset != frames[0].Offset {
		t.Fatalf("hold = offset %d after %d deliveries, want offset %d after 1",
			ctx.HeldOffset, ctx.HeldDeliveries, frames[0].Offset)
	}
}

func TestSessionHandlerMissingSummaryLoudLogs(t *testing.T) {
	// Arrange: the boundary is redelivered unchanged after being held, so the
	// silence bound is spent and it emits bare.
	var logs []string
	h := NewSessionTranscriptHandler(collectLog(&logs))
	frames := framesFor(t, boundaryLineJSON("b1", "2026-07-07T22:01:31.660Z", "manual", 10, 5, 7))
	ctx := &Context{SessionID: "s1", Redelivers: true}
	h.Handle(frames, ctx)
	// Act
	h.Handle(frames, ctx)
	// Assert
	if !strings.Contains(strings.Join(logs, "\n"), "not followed by a compaction summary") {
		t.Fatalf("missing loud log for the summary-less boundary; got %v", logs)
	}
}

func TestSessionHandlerCompactPairsByFileOrderNotTimestamp(t *testing.T) {
	// Arrange: two compactions whose summaries are each stamped ONE MILLISECOND
	// BEFORE their boundary — the first pair is the stamp pair recorded in the
	// real transcript (boundary .654, summary .653). A timestamp-ordered
	// assembly pairs them backwards; file order is the only sequence that pairs
	// them correctly.
	h := NewSessionTranscriptHandler(quietLog)
	frames := framesFor(t,
		boundaryLineJSON("b1", "2026-07-07T18:05:50.654Z", "manual", 1, 2, 3),
		summaryLineJSON("s-1", "b1", "2026-07-07T18:05:50.653Z", "first summary"),
		boundaryLineJSON("b2", "2026-07-07T23:05:50.654Z", "auto", 4, 5, 6),
		summaryLineJSON("s-2", "b2", "2026-07-07T23:05:50.653Z", "second summary"),
	)
	// Act
	evs := h.Handle(frames, &Context{SessionID: "s1"})
	// Assert
	got := map[string]string{}
	for _, e := range evs {
		if cc := e.GetContextCompacted(); cc != nil {
			got[e.GetDedupKey()] = cc.GetSummary()
		}
	}
	want := map[string]string{"compact:b1": "first summary", "compact:b2": "second summary"}
	for k, v := range want {
		if got[k] != v {
			t.Fatalf("%s summary = %q, want %q (pairing must follow file order)", k, got[k], v)
		}
	}
}

func TestSessionHandlerCompactIsPersistent(t *testing.T) {
	// Arrange
	h := NewSessionTranscriptHandler(quietLog)
	frames := framesFor(t, boundaryLineJSON("b1", "2026-07-07T22:01:31.660Z", "manual", 1, 2, 3))
	// Act
	ev := findCompacted(h.Handle(frames, &Context{SessionID: "s1"}))
	// Assert
	if ev.GetClass() != corev1.EventClass_EVENT_CLASS_PERSISTENT {
		t.Fatalf("class = %v, want PERSISTENT", ev.GetClass())
	}
}

func TestSessionHandlerCompactIsOnTheFilePlane(t *testing.T) {
	// Arrange
	h := NewSessionTranscriptHandler(quietLog)
	frames := framesFor(t, boundaryLineJSON("b1", "2026-07-07T18:05:50.654Z", "manual", 1, 2, 3))
	// Act
	ev := findCompacted(h.Handle(frames, &Context{SessionID: "s1"}))
	// Assert
	if ev.GetPlane() != corev1.Plane_PLANE_FILE {
		t.Fatalf("plane = %v, want PLANE_FILE", ev.GetPlane())
	}
}

func TestSessionHandlerCompactBoundaryFollowedBySystemLineHasNoSummary(t *testing.T) {
	// Arrange: only a user line flagged isCompactSummary is a summary; a system
	// line following the boundary supplies nothing.
	h := NewSessionTranscriptHandler(quietLog)
	frames := framesFor(t,
		boundaryLineJSON("b1", "2026-07-07T18:05:50.654Z", "auto", 1, 2, 3),
		boundaryLineJSON("b2", "2026-07-07T18:05:51.000Z", "auto", 4, 5, 6),
	)
	// Act
	evs := h.Handle(frames, &Context{SessionID: "s1"})
	// Assert
	for _, e := range evs {
		if cc := e.GetContextCompacted(); cc != nil && cc.GetSummary() != "" {
			t.Fatalf("%s summary = %q, want empty", e.GetDedupKey(), cc.GetSummary())
		}
	}
}

func TestSessionHandlerBoundaryWithoutUUIDCarriesNoKey(t *testing.T) {
	// Arrange
	var logs []string
	h := NewSessionTranscriptHandler(collectLog(&logs))
	frames := framesFor(t, boundaryLineJSON("", "2026-07-07T22:01:31.660Z", "manual", 1, 2, 3))
	// Act
	ev := findCompacted(h.Handle(frames, &Context{SessionID: "s1"}))
	// Assert
	if ev == nil || ev.GetDedupKey() != "" {
		t.Fatalf("want an emitted, keyless ContextCompacted; got %v", ev)
	}
}

// --- the summary must never read as a prompt --------------------------------

func TestSessionHandlerSummaryLineEmitsNoClearOrCompact(t *testing.T) {
	// Arrange: the summary alone, as the batch's only frame.
	h := NewSessionTranscriptHandler(quietLog)
	frames := framesFor(t, summaryLineJSON("s-1", "b1", "2026-07-07T22:01:31.659Z", "the summary text"))
	// Act
	evs := h.Handle(frames, &Context{SessionID: "s1"})
	// Assert: it is the harness's text, never the user's prompt, and it never
	// clears or compacts anything on its own.
	if findCleared(evs) != nil || findCompacted(evs) != nil {
		t.Fatalf("a compaction summary line produced a clear or compact of its own; got %d events", len(evs))
	}
}

func TestSessionHandlerSummaryLineStillEmitsItsVendorTwin(t *testing.T) {
	// Arrange: the boundary consumes the summary's TEXT, but the total-ingestion
	// mandate still requires the summary LINE's own vendor record.
	h := NewSessionTranscriptHandler(quietLog)
	frames := framesFor(t,
		boundaryLineJSON("b1", "2026-07-07T22:01:31.660Z", "manual", 1, 2, 3),
		summaryLineJSON("s-1", "b1", "2026-07-07T22:01:31.659Z", "the summary text"),
	)
	// Act
	evs := h.Handle(frames, &Context{SessionID: "s1"})
	// Assert: one vendor twin per frame.
	vendors := 0
	for _, e := range evs {
		if e.GetVendor() != nil {
			vendors++
		}
	}
	if vendors != 2 {
		t.Fatalf("vendor twins = %d, want 2 (one per transcript line)", vendors)
	}
}

func TestSessionHandlerSummarySpelledAsAClearIsNotAClear(t *testing.T) {
	// Arrange: a summary line whose text happens to be exactly "/clear". The
	// isCompactSummary flag, not the text, decides what the line is.
	h := NewSessionTranscriptHandler(quietLog)
	frames := framesFor(t, summaryLineJSON("s-1", "b1", "2026-07-07T22:01:31.659Z", "/clear"))
	// Act
	evs := h.Handle(frames, &Context{SessionID: "s1"})
	// Assert
	if findCleared(evs) != nil {
		t.Fatal("a compaction summary was read as a /clear prompt")
	}
}

func TestSessionHandlerBoundaryConsumesEachSummaryExactlyOnce(t *testing.T) {
	// Arrange
	h := NewSessionTranscriptHandler(quietLog)
	frames := framesFor(t,
		boundaryLineJSON("b1", "2026-07-07T22:01:31.660Z", "manual", 1, 2, 3),
		summaryLineJSON("s-1", "b1", "2026-07-07T22:01:31.659Z", "text"),
	)
	// Act
	evs := h.Handle(frames, &Context{SessionID: "s1"})
	// Assert: one boundary, one ContextCompacted.
	if n := countCompacted(evs); n != 1 {
		t.Fatalf("ContextCompacted count = %d, want 1", n)
	}
}

// --- dedup key spellings ----------------------------------------------------

func TestClearAndCompactDedupKeys(t *testing.T) {
	tests := []struct {
		name string
		got  string
		want string
	}{
		{name: "clear", got: clearDedupKey("abc"), want: "clear:abc"},
		{name: "compact", got: compactDedupKey("abc"), want: "compact:abc"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act happen in the table; Assert:
			if tc.got != tc.want {
				t.Fatalf("key = %q, want %q", tc.got, tc.want)
			}
		})
	}
}

// --- corpus grounding -------------------------------------------------------

func TestSessionHandlerCorpusCompactBoundaryEmitsContextCompacted(t *testing.T) {
	// Arrange: the real recorded boundary line from the golden corpus.
	h := NewSessionTranscriptHandler(quietLog)
	f := frameFor(t, "transcript-lines/system-compact_boundary.jsonl")
	// Act
	cc := findCompacted(h.Handle([]tail.Frame{f}, &Context{SessionID: "s1"})).GetContextCompacted()
	// Assert
	if cc.GetTrigger() != corev1.ContextCompactTrigger_CONTEXT_COMPACT_TRIGGER_MANUAL || cc.GetPreTokens() != 435029 {
		t.Fatalf("corpus boundary converted to %+v", cc)
	}
}
