package handler

// CLEAR and COMPACT on the file plane: the two ways a conversation's history
// stops informing the agent. Both are read out of the session transcript here.
//
// THE FILE PLANE IS THE ONLY PRODUCER OF BOTH. There is no stream-plane twin
// and there will never be one:
//
//   - Clear. The harness recognizes `/clear` itself and handles it entirely
//     inside the CLI. It never yields the command envelope back on the stream
//     plane, and it mints a FRESH transcript uuid for the recognized command,
//     so a stream-plane producer can neither observe the clear nor name it with
//     the uuid the transcript recorded it under. The shim half of this work was
//     cancelled on that evidence.
//   - Compact. Only the transcript carries the compaction SUMMARY; the stream
//     plane's boundary message has no summary text to give.
//
// So the sidecar coalesces the vendor's records HERE rather than leaving the
// correlation to a consumer, and the dedup keys below are PER-PRODUCER identity
// — a stable name for a line the sidecar may re-read after a restart or a
// cursor rewind — rather than a cross-producer merge contract.
//
// Session identity is not at risk in this attribution, but NOT for the reason
// this comment used to give. It claimed shim-driven and SDK sessions keep ONE
// vendor session id across a clear, and that only an interactive TUI session
// rotates it. That is false: a shim-driven `/clear` rotates the uuid too, live
// and every time (the shim's own store client re-keys on it, see the shim's
// StoreClient.rotateStoreKey). What holds regardless is the weaker and
// sufficient statement — the line is attributed to the transcript path it was
// READ from, whichever uuid that path names — so a clear recorded under a fresh
// uuid is filed under that fresh uuid, which is the identity the daemon has
// rotated its own subscription onto.
//
// WHAT THAT COSTS, AND WHO PAYS IT. The daemon must not depend on this event to
// learn that a dispatched `/clear` finished: this producer is a separate,
// launchd-managed process reading a file, and a sidecar that is down, behind, or
// has not yet discovered the new uuid's transcript emits nothing at all. The
// daemon closes its clearing axis on the ROTATION instead (ssm
// closeClearingLocked) and treats a ContextCleared arriving afterwards as the
// no-op it is. This event remains the only producer of the cleared BUBBLE and
// of the conversation's replay floor.
//
// CLEAR DETECTION. The harness never writes the literal prompt "/clear" to the
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
// else.
//
// COMPACT COALESCING. The `compact_boundary` system line supplies trigger,
// token counts and duration; the line after it supplies the summary text.
//
// ORDERING IS FILE ORDER, NEVER TIMESTAMP ORDER. Verified against a real
// transcript: the boundary is file line N stamped 18:05:50.654 and its summary
// is line N+1 stamped 18:05:50.653 — the summary is one millisecond EARLIER,
// because the harness composes the summary before writing the boundary that
// announces it. Any timestamp-ordered assembly gets the pair backwards and, in
// a session with several compactions, pairs each boundary with the wrong
// summary. The byte order on disk is the only correct sequence, and it is what
// this file reads.
//
// THE PAIR STRADDLES SCANS. The two lines are written ~1ms apart against a ~1s
// poll, so sooner or later a scan lands between them and the boundary is the
// last frame of its batch. That is NOT a summary-less compaction, it is a
// compaction whose summary has not been written yet, and emitting it bare
// throws the summary away for good. So a batch-terminal boundary is HELD:
// left unconverted, with the reader's cursor parked before it (holdCount
// below), until the file says what follows it. The next line in FILE ORDER
// settles it — its summary coalesces, anything else emits it bare — and one
// silent scan (maxHoldDeliveries) ends the wait either way, because a session
// that genuinely stops at a boundary must still render its truncation.

import (
	"strings"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

// clearCommand is the exact command name a cleared context is spelled with.
const clearCommand = "/clear"

// maxHoldDeliveries is the SILENCE BOUND on a held compaction boundary: the
// number of further deliveries the handler waits for a summary that never
// arrives before emitting the boundary bare.
//
// It is ONE. The two lines are written about a millisecond apart against a
// poll interval three orders of magnitude longer, so a single further scan is
// already an enormous margin over the gap being covered; every additional
// scan of holding only delays the truncation render for the sessions that
// genuinely stop at the boundary, and buys nothing for the ones that do not.
const maxHoldDeliveries = 1

// holdCount returns how many of frames may be converted NOW, and records on ctx
// whatever it deferred (see tail.Context "deferred frames").
//
// It defers exactly one frame, the batch's last, and only when that frame is a
// compaction boundary — the sole record in this transcript whose meaning
// depends on a line that may not be written yet. Everything else is settled by
// its own bytes and is converted the moment it is read.
//
// A deferral is only ever taken when the READER promises to hand the frame back
// (ctx.Redelivers). A handler given one standalone batch has no next delivery,
// so holding there would drop the compaction silently; that caller gets the
// bare emit, loud log and all.
func (h *SessionTranscriptHandler) holdCount(memo []*converted, frames []tail.Frame, ctx *Context) int {
	n := len(frames)
	// What was deferred LAST delivery. Both fields are rewritten below on every
	// call, so a stale hold can never outlive the batch that took it.
	heldOffset, heldFor := ctx.HeldOffset, ctx.HeldDeliveries
	ctx.HeldOffset, ctx.HeldDeliveries = 0, 0
	if n == 0 || !ctx.Redelivers {
		return n
	}
	last := frames[n-1]
	if last.ParseErr != nil {
		// An unparsable frame is settled: it becomes an UnparsedEvent now, and
		// no later line can change that.
		return n
	}
	c := h.convertAt(memo, frames, n-1)
	if c.err != nil || c.line.GetSystem().GetCompactBoundary() == nil {
		return n
	}
	if heldOffset == last.Offset && heldFor >= maxHoldDeliveries {
		// Held once already and the file still says nothing after it. Stop
		// waiting: the caller emits it bare, and compactedEvent loud-logs the
		// missing summary.
		return n
	}
	if heldOffset == last.Offset {
		ctx.HeldDeliveries = heldFor + 1
	} else {
		ctx.HeldDeliveries = 1
	}
	ctx.HeldOffset = last.Offset
	return n - 1
}

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
		if inner, remainder, present := takeTag(rest, tag); present {
			found[tag] = inner
			rest = remainder
		}
	}
	if strings.TrimSpace(rest) != "" {
		return "", "", false
	}
	// A retained guard, not dead weight: today the leftover check above already
	// rejects every string that carries an unconsumable <command-name>, so this
	// branch is not reachable — but it is the one thing standing between a
	// future change to that check and a nameless envelope being read as a valid
	// command, so it stays.
	name, present := found[commandEnvelopeTags[0]]
	if !present {
		return "", "", false
	}
	// command-message is the command's display label, redundant with the name;
	// it is consumed above and deliberately not returned.
	return strings.TrimSpace(name), strings.TrimSpace(found[commandEnvelopeTags[2]]), true
}

// takeTag removes the first <tag>…</tag> element from s, returning its inner
// text and s without it. An unterminated open tag is not an element and is left
// in place, so the caller's leftover check rejects the string.
func takeTag(s, tag string) (inner, rest string, found bool) {
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

// clearedEvent builds the ContextCleared event for a transcript line.
//
// The dedup key is `clear:<uuid>` off the TRANSCRIPT LINE's uuid: the sidecar
// is the sole producer, so the key names this line for THIS producer and keeps
// a re-read of the same line (restart, cursor rewind) from clearing twice.
func clearedEvent(sessionID, lineUUID string, log *logging.Bound) *corev1.Event {
	e := base(sessionID, corev1.Plane_PLANE_FILE)
	if lineUUID == "" {
		// Without the uuid there is nothing stable to name the line by. The
		// clear itself is real and is never dropped; it is emitted keyless and
		// loudly, rather than under an invented key that would collide with
		// another line's.
		log.With(logging.Context{Operation: "context-cleared", Session: sessionID, Level: "warn"}).Log(
			"/clear line carries no uuid; emitting ContextCleared with no dedup key (a re-read cannot be deduplicated)")
	} else {
		e.DedupKey = clearDedupKey(lineUUID)
	}
	e.Payload = &corev1.Event_ContextCleared{ContextCleared: &corev1.ContextCleared{}}
	return e
}

// compactedEvent builds the COALESCED ContextCompacted event for a boundary
// line, taking its summary from next — the line that follows the boundary IN
// THE FILE (see this file's header on why file order, not timestamp order, is
// the pairing sequence).
//
// The dedup key is `compact:<uuid>` off the BOUNDARY line's uuid.
func compactedEvent(sessionID string, env *datav1.LineEnvelope, cb *datav1.CompactBoundaryLine, next *datav1.TranscriptLine, log *logging.Bound) *corev1.Event {
	uuid := env.GetUuid()
	md := cb.GetCompactMetadata()
	summary := compactSummary(next)
	if summary == "" {
		log.With(logging.Context{Operation: "context-compacted", Session: sessionID, Level: "warn"}).Log(
			"compact_boundary uuid=%q is not followed by a compaction summary line; emitting without a summary", uuid)
	}
	e := base(sessionID, corev1.Plane_PLANE_FILE)
	if uuid == "" {
		log.With(logging.Context{Operation: "context-compacted", Session: sessionID, Level: "warn"}).Log(
			"compact_boundary carries no uuid; emitting with no dedup key")
	} else {
		e.DedupKey = compactDedupKey(uuid)
	}
	e.Payload = &corev1.Event_ContextCompacted{ContextCompacted: &corev1.ContextCompacted{
		Trigger:    compactTrigger(md.GetTrigger(), uuid, sessionID, log),
		PreTokens:  md.GetPreTokens(),
		PostTokens: md.GetPostTokens(),
		DurationMs: md.GetDurationMs(),
		Summary:    summary,
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
func compactTrigger(s, uuid, sessionID string, log *logging.Bound) corev1.ContextCompactTrigger {
	switch s {
	case "manual":
		return corev1.ContextCompactTrigger_CONTEXT_COMPACT_TRIGGER_MANUAL
	case "auto":
		return corev1.ContextCompactTrigger_CONTEXT_COMPACT_TRIGGER_AUTO
	case "":
		return corev1.ContextCompactTrigger_CONTEXT_COMPACT_TRIGGER_UNSPECIFIED
	default:
		log.With(logging.Context{Operation: "compact-trigger", Session: sessionID, Level: "warn"}).Log(
			"compact_boundary uuid=%q has unmodeled trigger %q; leaving trigger UNSPECIFIED", uuid, s)
		return corev1.ContextCompactTrigger_CONTEXT_COMPACT_TRIGGER_UNSPECIFIED
	}
}

// clearDedupKey is the dedup key for a clear, off the transcript line's uuid
// (§6.4).
func clearDedupKey(lineUUID string) string { return "clear:" + lineUUID }

// compactDedupKey is the dedup key for a compaction, off the boundary uuid.
func compactDedupKey(boundaryUUID string) string { return "compact:" + boundaryUUID }
