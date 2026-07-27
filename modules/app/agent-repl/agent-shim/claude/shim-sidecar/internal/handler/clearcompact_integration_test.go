package handler

// INTEGRATION coverage for the two first-class context cuts, driven at the
// outermost seam this module offers: a REAL `<uuid>.jsonl` on disk, classified
// by discover exactly as the sidecar classifies a discovered transcript, framed
// by the JSONL codec, cursored by the tailer, and converted by the session
// handler. clearcompact_test.go covers the same logic at the function/batch
// level; nothing here re-asserts that — these tests exist for the facts only a
// file and a cursor can express: where the session id comes from (the PATH),
// what a scan boundary does to a boundary/summary pair, and whether re-reading
// the same bytes names them the same way.

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/shim-claude-sidecar/internal/discover"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

// --- rig --------------------------------------------------------------------

// ccintQuietLog discards loud logs for tests that do not inspect them.
func ccintQuietLog(string, ...any) {}

// ccintRig is one live session transcript plus the tailer that reads it, wired
// the way the sidecar wires a discovered file: discover classifies the path
// (which is what supplies the session id), the target picks the codec, and the
// session handler converts the frames.
type ccintRig struct {
	t    *testing.T
	tr   *tail.Tailer
	ctx  *tail.Context
	path string
	// uuid is the VENDOR session uuid the transcript is filed under — the file's
	// own basename, and the only session identifier in the system.
	uuid string
}

// ccintNewRig lays down an empty transcript for vendorUUID under a temp config
// root and opens a tailer on it.
func ccintNewRig(t *testing.T, vendorUUID string) *ccintRig {
	t.Helper()
	root := filepath.Join(t.TempDir(), "config")
	dir := filepath.Join(root, "projects", "-Users-someone-workspace")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("creating the project dir: %v", err)
	}
	path := filepath.Join(dir, vendorUUID+".jsonl")
	if err := os.WriteFile(path, nil, 0o644); err != nil {
		t.Fatalf("creating the transcript: %v", err)
	}
	tgt, ok := discover.New([]string{root}, filepath.Join(root, "spool"), ccintQuietLog).Classify(path)
	if !ok {
		t.Fatalf("discover did not classify %s as a tailable target", path)
	}
	r := &ccintRig{
		t:    t,
		ctx:  &tail.Context{SessionID: tgt.SessionID, Path: tgt.Path, Kind: tgt.Kind},
		path: path,
		uuid: vendorUUID,
	}
	r.tr = tail.New(path, tgt.Codec(), NewSessionTranscriptHandler(ccintQuietLog), r.ctx, ccintQuietLog)
	return r
}

// write appends JSONL lines to the live transcript, in file order.
func (r *ccintRig) write(lines ...string) {
	r.t.Helper()
	f, err := os.OpenFile(r.path, os.O_APPEND|os.O_WRONLY, 0o644)
	if err != nil {
		r.t.Fatalf("opening %s for append: %v", r.path, err)
	}
	defer f.Close()
	for _, ln := range lines {
		if _, err := f.WriteString(ln + "\n"); err != nil {
			r.t.Fatalf("appending to %s: %v", r.path, err)
		}
	}
}

// scan runs ONE poll — read appended bytes, convert, commit the cursor — which
// is the unit the sidecar's poll loop works in.
func (r *ccintRig) scan() []*corev1.Event {
	r.t.Helper()
	res, err := r.tr.Poll()
	if err != nil {
		r.t.Fatalf("polling %s: %v", r.path, err)
	}
	r.tr.Commit(res)
	return res.Events
}

// scanAll polls until the file stops yielding, for tests that only care about
// the whole file's output rather than where the scan boundaries fell.
func (r *ccintRig) scanAll() []*corev1.Event {
	r.t.Helper()
	var out []*corev1.Event
	for {
		res, err := r.tr.Poll()
		if err != nil {
			r.t.Fatalf("polling %s: %v", r.path, err)
		}
		r.tr.Commit(res)
		out = append(out, res.Events...)
		if !res.Changed {
			return out
		}
	}
}

// reopen replaces the tailer with a fresh one holding NO cursor, which is what
// a store that has no cursor for this path leaves behind: the file is read
// again from byte 0.
func (r *ccintRig) reopen() {
	r.t.Helper()
	r.tr = tail.New(r.path, tail.JSONLCodec{}, NewSessionTranscriptHandler(ccintQuietLog), r.ctx, ccintQuietLog)
}

// --- fixtures ---------------------------------------------------------------

// ccintCommandEnvelope is how the harness records a slash command on disk: the
// expanded envelope, never the text the user typed.
func ccintCommandEnvelope(name, args string) string {
	return fmt.Sprintf("<command-name>%s</command-name>\n            <command-message>%s</command-message>\n            <command-args>%s</command-args>",
		name, strings.TrimPrefix(name, "/"), args)
}

func ccintJSON(obj map[string]any) string {
	b, _ := json.Marshal(obj)
	return string(b)
}

// ccintUserLine builds a transcript `user` line carrying content.
func ccintUserLine(uuid, sessionID, content string) string {
	return ccintJSON(map[string]any{
		"type":       "user",
		"uuid":       uuid,
		"parentUuid": "parent-of-" + uuid,
		"sessionId":  sessionID,
		"timestamp":  "2026-07-21T20:13:00.000Z",
		"message":    map[string]any{"role": "user", "content": content},
	})
}

// ccintAssistantLine builds a transcript `assistant` line whose single text
// block says text.
func ccintAssistantLine(uuid, sessionID, text string) string {
	return ccintJSON(map[string]any{
		"type":       "assistant",
		"uuid":       uuid,
		"parentUuid": "parent-of-" + uuid,
		"sessionId":  sessionID,
		"timestamp":  "2026-07-21T20:13:30.000Z",
		"message": map[string]any{
			"role":    "assistant",
			"type":    "message",
			"id":      "msg_" + uuid,
			"model":   "claude-opus-4-8",
			"content": []any{map[string]any{"type": "text", "text": text}},
		},
	})
}

// ccintBoundaryLine builds a `compact_boundary` system line.
func ccintBoundaryLine(uuid, sessionID, timestamp, trigger string, pre, post, durMs int64) string {
	return ccintJSON(map[string]any{
		"type":       "system",
		"subtype":    "compact_boundary",
		"uuid":       uuid,
		"parentUuid": nil,
		"sessionId":  sessionID,
		"timestamp":  timestamp,
		"content":    "Conversation compacted",
		"level":      "info",
		"compactMetadata": map[string]any{
			"trigger":    trigger,
			"preTokens":  pre,
			"postTokens": post,
			"durationMs": durMs,
		},
	})
}

// ccintSummaryLine builds the compaction SUMMARY line: typed `user`, flagged
// isCompactSummary, and stamped BEFORE the boundary it belongs to (the harness
// composes the summary before writing the boundary that announces it).
func ccintSummaryLine(uuid, boundaryUUID, sessionID, timestamp, text string) string {
	return ccintJSON(map[string]any{
		"type":                      "user",
		"uuid":                      uuid,
		"parentUuid":                boundaryUUID,
		"sessionId":                 sessionID,
		"timestamp":                 timestamp,
		"isCompactSummary":          true,
		"isVisibleInTranscriptOnly": true,
		"message":                   map[string]any{"role": "user", "content": text},
	})
}

// --- selectors --------------------------------------------------------------

func ccintClears(evs []*corev1.Event) []*corev1.Event {
	var out []*corev1.Event
	for _, e := range evs {
		if e.GetContextCleared() != nil {
			out = append(out, e)
		}
	}
	return out
}

func ccintCompactions(evs []*corev1.Event) []*corev1.Event {
	var out []*corev1.Event
	for _, e := range evs {
		if e.GetContextCompacted() != nil {
			out = append(out, e)
		}
	}
	return out
}

// ccintCutKeys lists the dedup keys of the clear/compact events in emission
// order — the identity the store deduplicates on.
func ccintCutKeys(evs []*corev1.Event) []string {
	var out []string
	for _, e := range evs {
		if e.GetContextCleared() != nil || e.GetContextCompacted() != nil {
			out = append(out, e.GetDedupKey())
		}
	}
	return out
}

// --- clear ------------------------------------------------------------------

func TestIntegrationEnvelopedClearEmitsContextClearedKeyedByLineUUID(t *testing.T) {
	// Arrange: the transcript's only line is the expanded /clear envelope, whose
	// sole non-whitespace content is the command.
	r := ccintNewRig(t, "f77ed5b2-5a0f-4f66-9c6c-9f4f50574caa")
	r.write(ccintUserLine("u-clear", r.uuid, ccintCommandEnvelope("/clear", "")))
	// Act
	got := ccintClears(r.scan())
	// Assert
	if len(got) != 1 {
		t.Fatalf("ContextCleared count = %d, want 1", len(got))
	}
	if key := got[0].GetDedupKey(); key != "clear:u-clear" {
		t.Fatalf("dedup_key = %q, want %q", key, "clear:u-clear")
	}
}

func TestIntegrationClearWithTrailingTextEmitsNothing(t *testing.T) {
	// Arrange: /clear leads, but something follows it — the user asked for
	// something else and merely started with the word.
	r := ccintNewRig(t, "f77ed5b2-5a0f-4f66-9c6c-9f4f50574caa")
	r.write(ccintUserLine("u-trailing", r.uuid, "/clear the build cache"))
	// Act
	got := ccintClears(r.scan())
	// Assert
	if len(got) != 0 {
		t.Fatalf("ContextCleared count = %d, want 0 (only a bare /clear clears)", len(got))
	}
}

func TestIntegrationClearSpokenByTheAssistantEmitsNothing(t *testing.T) {
	// Arrange: the spoof guard — the assistant writes the literal text "/clear"
	// into its reply. Only the USER's line can invoke a command.
	r := ccintNewRig(t, "f77ed5b2-5a0f-4f66-9c6c-9f4f50574caa")
	r.write(ccintAssistantLine("a-spoof", r.uuid, "/clear"))
	// Act
	got := ccintClears(r.scan())
	// Assert
	if len(got) != 0 {
		t.Fatalf("ContextCleared count = %d, want 0 (an assistant message cannot clear the context)", len(got))
	}
}

func TestIntegrationEveryClearInAFileGetsItsOwnKey(t *testing.T) {
	// Arrange: two clears in one session, one scan.
	r := ccintNewRig(t, "f77ed5b2-5a0f-4f66-9c6c-9f4f50574caa")
	r.write(
		ccintUserLine("u-first", r.uuid, ccintCommandEnvelope("/clear", "")),
		ccintUserLine("u-work", r.uuid, "carry on"),
		ccintUserLine("u-second", r.uuid, ccintCommandEnvelope("/clear", "")),
	)
	// Act
	got := ccintCutKeys(r.scan())
	// Assert: one event per line uuid, each named for its own line.
	want := []string{"clear:u-first", "clear:u-second"}
	if strings.Join(got, ",") != strings.Join(want, ",") {
		t.Fatalf("dedup keys = %v, want %v", got, want)
	}
}

// --- compact command --------------------------------------------------------

func TestIntegrationCompactCommandProducesExactlyOneEventFromItsBoundary(t *testing.T) {
	// Arrange: the boundary line is the SOLE producer of a compaction — it alone
	// carries the trigger, token counts, and summary. The /compact command echo
	// preceding it must contribute nothing, or a manual compaction double-emits.
	r := ccintNewRig(t, "f77ed5b2-5a0f-4f66-9c6c-9f4f50574caa")
	r.write(
		ccintUserLine("u-compact", r.uuid, ccintCommandEnvelope("/compact", "keep the parser discussion")),
		ccintBoundaryLine("b-cmd", r.uuid, "2026-07-21T18:05:50.654Z", "manual", 435029, 8639, 194511),
		ccintSummaryLine("s-cmd", "b-cmd", r.uuid, "2026-07-21T18:05:50.653Z", "kept the parser discussion"),
	)
	// Act
	got := ccintCompactions(r.scan())
	// Assert
	if len(got) != 1 {
		t.Fatalf("ContextCompacted count = %d, want 1 (the boundary, never the command echo)", len(got))
	}
	if trig := got[0].GetContextCompacted().GetTrigger(); trig != corev1.ContextCompactTrigger_CONTEXT_COMPACT_TRIGGER_MANUAL {
		t.Fatalf("trigger = %v, want MANUAL (the user asked for it)", trig)
	}
}

func TestIntegrationCompactNotFirstEmitsNothing(t *testing.T) {
	// Arrange: the command is quoted mid-prompt, not invoked.
	r := ccintNewRig(t, "f77ed5b2-5a0f-4f66-9c6c-9f4f50574caa")
	r.write(ccintUserLine("u-quoted", r.uuid, "when you are done, run /compact"))
	// Act
	got := ccintCompactions(r.scan())
	// Assert
	if len(got) != 0 {
		t.Fatalf("ContextCompacted count = %d, want 0 (/compact must lead the prompt)", len(got))
	}
}

// --- compaction coalescing --------------------------------------------------

func TestIntegrationBoundaryAndSummaryCoalesceDespiteInvertedTimestamps(t *testing.T) {
	// Arrange: the pair as the harness really writes it — the summary is file
	// line N+1 but stamped one millisecond EARLIER than the boundary at N.
	r := ccintNewRig(t, "f77ed5b2-5a0f-4f66-9c6c-9f4f50574caa")
	r.write(
		ccintBoundaryLine("b-1", r.uuid, "2026-07-21T18:05:50.654Z", "manual", 435029, 8639, 194511),
		ccintSummaryLine("s-1", "b-1", r.uuid, "2026-07-21T18:05:50.653Z", "what the discarded history said"),
	)
	// Act
	got := ccintCompactions(r.scan())
	// Assert: one event, carrying the summary that FOLLOWS it in the file.
	if len(got) != 1 {
		t.Fatalf("ContextCompacted count = %d, want 1 (the pair coalesces into one event)", len(got))
	}
	if s := got[0].GetContextCompacted().GetSummary(); s != "what the discarded history said" {
		t.Fatalf("summary = %q, want the summary line's text", s)
	}
}

func TestIntegrationBoundarySplitAcrossScansStillCoalesces(t *testing.T) {
	// Arrange: the scan boundary falls between the pair — the boundary is the
	// last line the first scan sees, and its summary is appended afterwards.
	r := ccintNewRig(t, "f77ed5b2-5a0f-4f66-9c6c-9f4f50574caa")
	r.write(ccintBoundaryLine("b-1", r.uuid, "2026-07-21T18:05:50.654Z", "auto", 400000, 9000, 120000))
	// Act
	first := r.scan()
	r.write(ccintSummaryLine("s-1", "b-1", r.uuid, "2026-07-21T18:05:50.653Z", "the summary that arrived late"))
	second := r.scan()
	// Assert: never emitted summary-less just because the file had not been
	// written that far yet, and never emitted twice once it had.
	if n := len(ccintCompactions(first)); n != 0 {
		t.Fatalf("ContextCompacted count on the first scan = %d, want 0 (the summary line had not been written yet)", n)
	}
	got := ccintCompactions(append(append([]*corev1.Event{}, first...), second...))
	if len(got) != 1 {
		t.Fatalf("ContextCompacted count across both scans = %d, want 1", len(got))
	}
	if s := got[0].GetContextCompacted().GetSummary(); s != "the summary that arrived late" {
		t.Fatalf("summary = %q, want the late summary line's text", s)
	}
}

func TestIntegrationAutoCompactionCarriesTheAutoTrigger(t *testing.T) {
	// Arrange: the harness compacted on its own, not on a user command.
	r := ccintNewRig(t, "f77ed5b2-5a0f-4f66-9c6c-9f4f50574caa")
	r.write(
		ccintBoundaryLine("b-auto", r.uuid, "2026-07-21T18:05:50.654Z", "auto", 400000, 9000, 120000),
		ccintSummaryLine("s-auto", "b-auto", r.uuid, "2026-07-21T18:05:50.653Z", "summary"),
	)
	// Act
	got := ccintCompactions(r.scan())
	// Assert
	if len(got) != 1 {
		t.Fatalf("ContextCompacted count = %d, want 1", len(got))
	}
	if trig := got[0].GetContextCompacted().GetTrigger(); trig != corev1.ContextCompactTrigger_CONTEXT_COMPACT_TRIGGER_AUTO {
		t.Fatalf("trigger = %v, want AUTO", trig)
	}
}

func TestIntegrationCompactionCarriesTokenCountsAndDuration(t *testing.T) {
	// Arrange: the metadata the boundary line supplies is what makes a compaction
	// measurable rather than a bare marker.
	r := ccintNewRig(t, "f77ed5b2-5a0f-4f66-9c6c-9f4f50574caa")
	r.write(
		ccintBoundaryLine("b-1", r.uuid, "2026-07-21T18:05:50.654Z", "manual", 435029, 8639, 194511),
		ccintSummaryLine("s-1", "b-1", r.uuid, "2026-07-21T18:05:50.653Z", "summary"),
	)
	// Act
	got := ccintCompactions(r.scan())
	// Assert
	if len(got) != 1 {
		t.Fatalf("ContextCompacted count = %d, want 1", len(got))
	}
	cc := got[0].GetContextCompacted()
	if cc.GetPreTokens() != 435029 || cc.GetPostTokens() != 8639 || cc.GetDurationMs() != 194511 {
		t.Fatalf("pre/post/duration = %d/%d/%d, want 435029/8639/194511",
			cc.GetPreTokens(), cc.GetPostTokens(), cc.GetDurationMs())
	}
}

// --- identity and re-reads --------------------------------------------------

func TestIntegrationCutsAreAttributedToTheVendorSessionUUID(t *testing.T) {
	// Arrange: the transcript's basename IS the session id. A daemon-side s_… id
	// never reaches this plane, and nothing here may invent one.
	r := ccintNewRig(t, "f77ed5b2-5a0f-4f66-9c6c-9f4f50574caa")
	r.write(
		ccintUserLine("u-clear", r.uuid, ccintCommandEnvelope("/clear", "")),
		ccintBoundaryLine("b-1", r.uuid, "2026-07-21T18:05:50.654Z", "manual", 1, 2, 3),
		ccintSummaryLine("s-1", "b-1", r.uuid, "2026-07-21T18:05:50.653Z", "summary"),
	)
	// Act
	evs := r.scan()
	// Assert
	cuts := append(ccintClears(evs), ccintCompactions(evs)...)
	if len(cuts) == 0 {
		t.Fatal("no clear or compact events to attribute")
	}
	for _, e := range cuts {
		if e.GetSessionId() != r.uuid {
			t.Fatalf("session_id = %q, want the vendor uuid %q", e.GetSessionId(), r.uuid)
		}
	}
}

func TestIntegrationRereadingTheFileYieldsTheSameDedupKeys(t *testing.T) {
	// Arrange: a restart with no recovered cursor re-reads the whole transcript.
	// Key stability across that re-read is the entire basis for the store's
	// dedup absorbing the overlap.
	r := ccintNewRig(t, "f77ed5b2-5a0f-4f66-9c6c-9f4f50574caa")
	r.write(
		ccintUserLine("u-clear", r.uuid, ccintCommandEnvelope("/clear", "")),
		ccintBoundaryLine("b-1", r.uuid, "2026-07-21T18:05:50.654Z", "manual", 1, 2, 3),
		ccintSummaryLine("s-1", "b-1", r.uuid, "2026-07-21T18:05:50.653Z", "summary"),
	)
	// Act
	first := ccintCutKeys(r.scanAll())
	r.reopen()
	second := ccintCutKeys(r.scanAll())
	// Assert
	if strings.Join(first, ",") != strings.Join(second, ",") {
		t.Fatalf("re-read keys = %v, want the first read's %v", second, first)
	}
}
