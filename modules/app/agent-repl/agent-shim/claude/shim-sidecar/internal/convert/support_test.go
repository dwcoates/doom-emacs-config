package convert

// support_test.go — the convert suite's shared arrangement.
//
// Tests here drive the converter DIRECTLY with decoded records, so a failure
// names the conversion rather than the reader. Nothing touches a vendor.

import (
	"bytes"
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"io"
	"os"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
)

func TestMain(m *testing.M) {
	if err := os.Setenv("AGENT_REPL_FORBID_VENDOR_CALLS", "1"); err != nil {
		panic(err)
	}
	// Verbose records are exercised too: a verbose-only branch that panicked
	// would otherwise stay invisible until production enabled it.
	os.Exit(m.Run())
}

// newTestConverter builds a converter logging nowhere. Both sinks are supplied
// because the logger requires both — nil-ing one would stop testing the
// production logging contract.
func newTestConverter(t *testing.T) *Converter {
	t.Helper()
	return New(logging.New(io.Discard, io.Discard).With(logging.Context{Component: "test"}))
}

// testAttribution is the file position and identities a record is read under.
func testAttribution(offset int64) Attribution {
	return Attribution{
		VendorSessionID: "session-uuid",
		MainAgentID:     "session-uuid",
		AgentID:         "session-uuid",
		Path:            "/p/projects/proj/session-uuid.jsonl",
		FileID:          "dev:1",
		Offset:          offset,
	}
}

// decode turns one JSON line into the record the converter consumes.
func decode(t *testing.T, line string) map[string]any {
	t.Helper()
	var record map[string]any
	if err := json.Unmarshal([]byte(line), &record); err != nil {
		t.Fatalf("decode %q: %v", line, err)
	}
	return record
}

// convertLines runs several lines through ONE converter in file order, which is
// what the joins need: a result finds its call only because the call went through
// the same converter first.
func convertLines(t *testing.T, c *Converter, lines ...string) []*storev1.StoreEntry {
	t.Helper()
	records := make([]map[string]any, len(lines))
	for i, line := range lines {
		records[i] = decode(t, line)
	}
	var out []*storev1.StoreEntry
	for i, record := range records {
		var next map[string]any
		if i+1 < len(records) {
			next = records[i+1]
		}
		out = append(out, c.Line(record, testAttribution(int64(i*1000)), next)...)
	}
	return out
}

// convertLinesFrom is convertLines with the reader JOINING THE FILE at
// startOffset — the cursor a restarted reader resumes from. A converter that
// starts at byte 0 saw the whole file; one that starts anywhere else did not,
// and several branches turn on that difference.
func convertLinesFrom(t *testing.T, c *Converter, startOffset int64, lines ...string) []*storev1.StoreEntry {
	t.Helper()
	records := make([]map[string]any, len(lines))
	for i, line := range lines {
		records[i] = decode(t, line)
	}
	var out []*storev1.StoreEntry
	for i, record := range records {
		var next map[string]any
		if i+1 < len(records) {
			next = records[i+1]
		}
		out = append(out, c.Line(record, testAttribution(startOffset+int64(i*1000)), next)...)
	}
	return out
}

// ---- readers, so an assertion reads as a claim about the produced shape ----

func pageLine(e *storev1.StoreEntry) *storev1.StorePageLine {
	return e.GetAgentUpdate().GetServeableFrame()
}

func frameOf(e *storev1.StoreEntry) *conversationv1.AgentFrame {
	return pageLine(e).GetAgentItem().GetAgentFrame()
}

func activityOf(e *storev1.StoreEntry) *conversationv1.AgentActivity {
	return frameOf(e).GetUpdate().GetActivity()
}

// entryByKey finds the ONE entry under an upsert key, failing when the count is
// not exactly one: a duplicated key is the bug these tests exist to catch, so
// "the first match" would hide it.
func entryByKey(t *testing.T, entries []*storev1.StoreEntry, key string) *storev1.StoreEntry {
	t.Helper()
	var found *storev1.StoreEntry
	count := 0
	for _, e := range entries {
		if e.GetUpsertKey() == key {
			found = e
			count++
		}
	}
	if count != 1 {
		t.Fatalf("upsert_key %q: got %d entries, want exactly 1 (keys present: %v)", key, count, allKeys(entries))
	}
	return found
}

func allKeys(entries []*storev1.StoreEntry) []string {
	keys := make([]string, 0, len(entries))
	for _, e := range entries {
		keys = append(keys, e.GetUpsertKey())
	}
	return keys
}

func vendorKindOf(e *storev1.StoreEntry) string {
	return e.GetAgentUpdate().GetUnservedItem().GetVendorSpecific().GetKind()
}

// ---- record builders, so each test states only what it is about ----

// promptOriginUnspecified is the origin an adopted external prompt carries — the
// daemon draws it as the plain "You" author label.
const promptOriginUnspecified = conversationv1.PromptOrigin_PROMPT_ORIGIN_UNSPECIFIED

// sdkPromptLine builds a prompt agent-repl submitted through its own SDK
// (entrypoint "sdk-cli"), which is withheld as vendor_specific (R15).
func sdkPromptLine(uuid, text string) string {
	return externalPromptLine(uuid, "sdk-cli", text)
}

// externalPromptLine builds a genuine human prompt stamped with a given
// entrypoint: "sdk-cli" for agent-repl's own, "cli" for an adopted interactive
// session.
func externalPromptLine(uuid, entrypoint, text string) string {
	return `{"type":"user","uuid":"` + uuid + `","isSidechain":false,"entrypoint":"` + entrypoint +
		`","timestamp":"` + ts1 +
		`","message":{"role":"user","content":[{"type":"text","text":` + quote(text) + `}]}}`
}

// peerLineOf reads the PeerMessage arm of a served page line.
func peerLineOf(e *storev1.StoreEntry) *conversationv1.PeerMessage {
	return pageLine(e).GetAgentItem().GetPeerMessage()
}

// peerMessageLine builds a message another Claude session sent in: a user record
// whose origin.kind is "peer". `from` is the sender; `body`, when non-empty, is
// the vendor-stated origin.body (else the record text is the body). `handback`
// marks a subagent hand-back, which is a peer message all the same.
func peerMessageLine(uuid, from, body, text string, handback bool) string {
	origin := `"kind":"peer","from":` + quote(from) + `,"senderTaskId":` + quote(from)
	if body != "" {
		origin += `,"body":` + quote(body)
	}
	if handback {
		origin += `,"handback":true`
	}
	return `{"type":"user","uuid":"` + uuid + `","isSidechain":false,"isMeta":true,"promptSource":"system",` +
		`"origin":{` + origin + `},"timestamp":"` + ts1 +
		`","message":{"role":"user","content":[{"type":"text","text":` + quote(text) + `}]}}`
}

// assistantWith builds an assistant line carrying the given content blocks.
func assistantWith(uuid, messageID, timestamp, blocks string) string {
	return `{"type":"assistant","uuid":"` + uuid + `","isSidechain":false,"timestamp":"` + timestamp +
		`","message":{"id":"` + messageID + `","role":"assistant","content":[` + blocks + `]}}`
}

// toolCall builds a tool_use block.
func toolCall(id, name, input string) string {
	return `{"type":"tool_use","id":"` + id + `","name":"` + name + `","input":` + input + `}`
}

// toolResultLine builds the user record the vendor files a tool result under.
func toolResultLine(uuid, callID, timestamp, content, toolUseResult string) string {
	return toolResultLineWithError(uuid, callID, timestamp, content, toolUseResult, false)
}

// toolResultLineWithError builds a result the vendor MARKED AN ERROR FOR THE
// MODEL, which is a different fact from the call having failed.
func toolResultLineWithError(uuid, callID, timestamp, content, toolUseResult string, isError bool) string {
	errorField := ""
	if isError {
		errorField = `,"is_error":true`
	}
	line := `{"type":"user","uuid":"` + uuid + `","isSidechain":false,"timestamp":"` + timestamp +
		`","message":{"role":"user","content":[{"type":"tool_result","tool_use_id":"` + callID +
		`","content":` + content + errorField + `}]}}`
	if toolUseResult == "" {
		return line
	}
	// Splice the toolUseResult in beside the message.
	return line[:len(line)-1] + `,"toolUseResult":` + toolUseResult + `}`
}

// apiKindName names the api-failure arm a record landed on, so a table test
// states the arm it expects rather than switching in every case.
func apiKindName(failed *conversationv1.ApiRequestFailed) string {
	switch {
	case failed.GetRateLimited() != nil:
		return "rate_limited"
	case failed.GetOverloaded() != nil:
		return "overloaded"
	case failed.GetAuthenticationFailed() != nil:
		return "authentication_failed"
	case failed.GetPermissionDenied() != nil:
		return "permission_denied"
	case failed.GetInvalidRequest() != nil:
		return "invalid_request"
	case failed.GetRequestTooLarge() != nil:
		return "request_too_large"
	case failed.GetNotFound() != nil:
		return "not_found"
	case failed.GetInternal() != nil:
		return "internal"
	case failed.GetBillingError() != nil:
		return "billing_error"
	case failed.GetOauthOrgNotAllowed() != nil:
		return "oauth_org_not_allowed"
	case failed.GetMaxOutputTokens() != nil:
		return "max_output_tokens"
	case failed.GetUnmodeled() != nil:
		return "unmodeled"
	default:
		return "UNSET"
	}
}

// sha256Hex recomputes the documented write-identity digest, so the test asserts
// the RECIPE rather than a recorded value nobody can check against the ruling.
func sha256Hex(s string) string {
	sum := sha256.Sum256([]byte(s))
	return hex.EncodeToString(sum[:])
}

// levelForMessage returns the level of the ONE captured record whose message
// contains substring, failing unless exactly one does. It reads a
// loggedConverter's JSONL sink, so a test can pin the SEVERITY a conversion
// recorded at — the subject of the benign-re-scan reclassifications — rather
// than only the message text.
func levelForMessage(t *testing.T, sink *bytes.Buffer, substring string) string {
	t.Helper()
	var level string
	count := 0
	for _, line := range strings.Split(strings.TrimSpace(sink.String()), "\n") {
		if line == "" {
			continue
		}
		var rec map[string]any
		if err := json.Unmarshal([]byte(line), &rec); err != nil {
			t.Fatalf("captured log line is not JSON: %v: %q", err, line)
		}
		if msg, _ := rec["message"].(string); strings.Contains(msg, substring) {
			level, _ = rec["level"].(string)
			count++
		}
	}
	if count != 1 {
		t.Fatalf("message mentioning %q: got %d records, want exactly 1; log:\n%s", substring, count, sink.String())
	}
	return level
}
