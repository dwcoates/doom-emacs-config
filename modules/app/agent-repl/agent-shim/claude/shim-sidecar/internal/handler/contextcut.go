package handler

// Context cuts on the file plane. A cut is one of the two ways a conversation's
// history stops informing the agent — CLEAR and COMPACT — and both are read out
// of the session transcript here.
//
// CLEAR. The harness never writes the literal prompt "/clear" to the
// transcript. It writes the EXPANDED command envelope:
//
//	<command-name>/clear</command-name>
//	            <command-message>clear</command-message>
//	            <command-args></command-args>
//
// so anything matching the raw prompt text against "/clear" misses every
// replayed or rehydrated session. Detection therefore unwraps the envelope
// first and then requires "/clear" to be the ONLY non-whitespace content left:
// an argument, or prose around the envelope, means the user asked for something
// other than a cut.
//
// COMPACT. The file plane is the ONLY plane that carries the compaction
// SUMMARY, so the sidecar is the sole producer of ContextCompacted and
// coalesces the vendor's two records HERE rather than leaving the correlation
// to a consumer. The `compact_boundary` system line supplies trigger, token
// counts and duration; the line after it supplies the summary text.
//
// ORDERING IS FILE ORDER, NEVER TIMESTAMP ORDER. Verified against a real
// transcript: the boundary is file line N stamped 22:01:31.660 and its summary
// is line N+1 stamped 22:01:31.659 — the summary is one millisecond EARLIER,
// because the harness composes the summary before writing the boundary that
// announces it. Any timestamp-ordered assembly gets the pair backwards and, in
// a session with several compactions, pairs each boundary with the wrong
// summary. The byte order on disk is the only correct sequence, and it is what
// this file reads.

import (
	"strings"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
)

// clearCommand is the exact command name a cleared context is spelled with.
const clearCommand = "/clear"

// compactResultSuccess is the only outcome the file plane can attest to. The
// harness writes a `compact_boundary` line when a compaction has COMPLETED, so
// the line's existence is the success record; a compaction that failed leaves
// no boundary behind and is reported (with its reason) on the stream plane's
// status message, which the sidecar does not read. `error` is therefore always
// empty here — the sidecar never guesses an outcome it cannot observe.
const compactResultSuccess = "success"

// commandEnvelopeTags are the elements the harness expands a slash command into,
// in the order it writes them.
var commandEnvelopeTags = []string{"command-name", "command-message", "command-args"}

// isClearCommand reports whether a user message is the /clear command and
// nothing else.
func isClearCommand(m *datav1.ApiUserMessage) bool {
	// A clear is always plain text. A blocks-form user message is a tool result
	// or a composed prompt, never a command envelope.
	s, ok := m.GetContent().(*datav1.ApiUserMessage_ContentString)
	if !ok {
		return false
	}
	name, args, ok := unwrapCommandEnvelope(s.ContentString)
	return ok && name == clearCommand && args == ""
}

// unwrapCommandEnvelope reduces a user prompt to the command it invokes.
//
// Text with no <command-name> element is returned verbatim as the name, so a
// prompt whose entire content is "/clear" is recognized as one. Text carrying
// the envelope must consist of NOTHING but the envelope's elements: leftover
// non-whitespace outside them means the prompt merely quotes a command (a
// pasted transcript, a tool result echoing one) rather than invoking it, and ok
// is false.
func unwrapCommandEnvelope(s string) (name, args string, ok bool) {
	if !strings.Contains(s, "<"+commandEnvelopeTags[0]+">") {
		return strings.TrimSpace(s), "", true
	}
	rest := s
	found := map[string]string{}
	for _, tag := range commandEnvelopeTags {
		// command-args is absent on some harness versions; a missing element is
		// not a malformed envelope, only an empty one.
		if inner, remainder, present := cutTag(rest, tag); present {
			found[tag] = inner
			rest = remainder
		}
	}
	if strings.TrimSpace(rest) != "" {
		return "", "", false
	}
	name, present := found[commandEnvelopeTags[0]]
	if !present {
		return "", "", false
	}
	// command-message is the command's display label, redundant with the name;
	// it is consumed above and deliberately not returned.
	return strings.TrimSpace(name), strings.TrimSpace(found[commandEnvelopeTags[2]]), true
}

// cutTag removes the first <tag>…</tag> element from s, returning its inner text
// and s without it. An unterminated open tag is not an element and is left in
// place, so the caller's leftover check rejects the string.
func cutTag(s, tag string) (inner, rest string, found bool) {
	open, closing := "<"+tag+">", "</"+tag+">"
	i := strings.Index(s, open)
	if i < 0 {
		return "", s, false
	}
	start := i + len(open)
	j := strings.Index(s[start:], closing)
	if j < 0 {
		return "", s, false
	}
	return s[start : start+j], s[:i] + s[start+j+len(closing):], true
}

// clearedEvent builds the ContextCleared cut for a transcript line.
//
// The dedup key is `clear:<uuid>` off the TRANSCRIPT LINE's uuid, and that
// spelling is a hard cross-producer contract: the shim sees the same clear on
// the stream plane and emits the identical key, so the store collapses the twins
// into one cut instead of cutting the conversation twice.
func clearedEvent(sessionID, lineUUID string, log Logf) *corev1.Event {
	e := base(sessionID, corev1.Plane_PLANE_FILE)
	if lineUUID == "" {
		// Without the uuid the key cannot match the shim's twin. The cut itself
		// is still real and is never dropped; it is emitted keyless and loudly,
		// rather than under an invented key that would silently fail to merge.
		log("transcript: /clear line carries no uuid; emitting ContextCleared with no dedup key (the stream-plane twin will not merge)")
	} else {
		e.DedupKey = clearDedupKey(lineUUID)
	}
	e.Payload = &corev1.Event_ContextCleared{ContextCleared: &corev1.ContextCleared{}}
	return e
}

// compactedEvent builds the COALESCED ContextCompacted cut for a boundary line,
// taking its summary from next — the line that follows the boundary IN THE FILE
// (see this file's header on why file order, not timestamp order, is the pairing
// sequence).
//
// The dedup key is `compact:<uuid>` off the BOUNDARY line's uuid.
func compactedEvent(sessionID string, env *datav1.LineEnvelope, cb *datav1.CompactBoundaryLine, next *datav1.TranscriptLine, log Logf) *corev1.Event {
	uuid := env.GetUuid()
	md := cb.GetCompactMetadata()
	summary := compactSummary(next)
	if summary == "" {
		log("transcript: compact_boundary uuid=%q is not followed by a compaction summary line; emitting the cut without a summary", uuid)
	}
	e := base(sessionID, corev1.Plane_PLANE_FILE)
	if uuid == "" {
		log("transcript: compact_boundary carries no uuid; emitting ContextCompacted with no dedup key")
	} else {
		e.DedupKey = compactDedupKey(uuid)
	}
	e.Payload = &corev1.Event_ContextCompacted{ContextCompacted: &corev1.ContextCompacted{
		Trigger:    compactTrigger(md.GetTrigger(), uuid, log),
		PreTokens:  md.GetPreTokens(),
		PostTokens: md.GetPostTokens(),
		DurationMs: md.GetDurationMs(),
		Summary:    summary,
		Result:     compactResultSuccess,
	}}
	return e
}

// compactSummary returns the compaction summary text carried by the line
// FOLLOWING a boundary, or "" when that line is not a summary (or there is no
// following line in this batch).
//
// The summary line is typed `user`, which is why it is identified by the
// envelope's is_compact_summary flag and never by its type: it is the harness's
// own text standing in for the discarded history, not the user's prompt, and
// rendering it as one is a bug.
func compactSummary(next *datav1.TranscriptLine) string {
	u := next.GetUser()
	if u == nil || !u.GetEnvelope().GetIsCompactSummary() {
		return ""
	}
	return u.GetMessage().GetContentString()
}

// compactTrigger maps the disk's trigger string onto the neutral enum. An
// unmodeled value stays UNSPECIFIED and is loud-logged rather than guessed.
func compactTrigger(s, uuid string, log Logf) corev1.ContextCompactTrigger {
	switch s {
	case "manual":
		return corev1.ContextCompactTrigger_CONTEXT_COMPACT_TRIGGER_MANUAL
	case "auto":
		return corev1.ContextCompactTrigger_CONTEXT_COMPACT_TRIGGER_AUTO
	case "":
		return corev1.ContextCompactTrigger_CONTEXT_COMPACT_TRIGGER_UNSPECIFIED
	default:
		log("transcript: compact_boundary uuid=%q has unmodeled trigger %q; leaving the trigger UNSPECIFIED", uuid, s)
		return corev1.ContextCompactTrigger_CONTEXT_COMPACT_TRIGGER_UNSPECIFIED
	}
}

// clearDedupKey is the cross-producer dedup key for a clear cut (§6.4).
func clearDedupKey(lineUUID string) string { return "clear:" + lineUUID }

// compactDedupKey is the dedup key for a compaction cut, off the boundary uuid.
func compactDedupKey(boundaryUUID string) string { return "compact:" + boundaryUUID }
