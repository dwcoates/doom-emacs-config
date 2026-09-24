package convert

// contextcut.go — THE CONVERSATION WAS CUT HERE, and the reader must see where.
//
// Two producers, one carrier. `/clear` is detected by UNWRAPPING the vendor's
// expanded command envelope — the literal "/clear" NEVER appears on disk, so
// anything matching raw prompt text against it misses every real session.
// Compaction COALESCES the boundary record with the FOLLOWING summary line in
// FILE ORDER, never timestamp order: the harness composes the summary before
// writing the boundary that announces it, so the summary's timestamp is EARLIER
// and a timestamp-ordered assembly pairs every boundary with the wrong summary
// in a session that compacted more than once.
//
// Both land as a page line of the MAIN AGENT'S book, keyed
// `session:context_cut:<the cut's identity>` — the boundary's uuid for a
// compaction, and for a clear THE SESSION IT ROTATED TO, because those are the
// spellings the STREAM plane can mint for the same cut. See `clearCutIdentity`.

import (
	"strings"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
)

// contextCleared stores that history was discarded outright.
//
// A CLEAR CARRIES NO TOKEN DELTA: the vendor's conversation-reset record states
// none, so none is claimed. Clearing does not empty the context either — the
// system prompt, skills and memory files are reloaded — which is exactly why
// fabricating a zero here would be a lie a reader could see.
func (c *Converter) contextCleared(record map[string]any, at Attribution, env envelope, agent string) *storev1.StoreEntry {
	identity := clearCutIdentity(at, env)
	if identity == env.uuid {
		// The file the envelope lives in does not name its session, which is
		// the one identity the stream plane can also spell. The cut is still
		// recorded — a reader must see WHERE the conversation was cut — but on
		// a key the other plane cannot reach, so the divider may be drawn
		// twice, and that is said out loud rather than discovered in the feed.
		c.log.With(at.ctxWarn("context-cleared")).With(logging.Context{UpsertKey: SessionKey("context_cut", identity)}).
			Log("context clear: the transcript names no session, so the cut is keyed on its own record uuid and the stream plane's write cannot collapse onto it")
	}
	c.log.With(at.ctxFor("context-cleared")).With(logging.Context{UpsertKey: SessionKey("context_cut", identity)}).
		Log("context clear: history discarded outright, with no token delta the vendor stated")
	return c.contextCutEntry(at, identity, agent, &conversationv1.ContextCut{
		Cut: &conversationv1.ContextCut_Cleared{Cleared: &conversationv1.ContextCleared{}},
	})
}

// clearCutIdentity is WHICH CUT a `/clear` is, in the one spelling the STREAM
// plane can also mint.
//
// THE TWO PLANES SEE DIFFERENT RECORDS FOR A CLEAR. This reader's only evidence
// is the expanded `/clear` command envelope; the shim's is the SDK's
// `conversation_reset`, and the two carry unrelated uuids in unrelated places
// (`testdata/captures/identity-rotation-clear`: `04f97c00-…` here,
// `cc07c2a0-…` there, and a `new_conversation_id` nothing ever uses). Keying on
// this record's own uuid therefore left ONE clear as TWO store rows and TWO
// "context cleared" dividers in the feed.
//
// What both planes DO hold is THE SESSION THE CLEAR ROTATED TO. The vendor
// writes the envelope into the NEW transcript, so it is this file's own session
// uuid; the shim learns the same id from the `system:init` that follows the
// reset. `Attribution.VendorSessionID` is that file's uuid — deliberately the
// BASENAME and never the per-record `sessionId` field, which diverges from the
// runtime's answer in ~22% of records.
func clearCutIdentity(at Attribution, env envelope) string {
	if at.VendorSessionID != "" {
		return at.VendorSessionID
	}
	return env.uuid
}

// NoSummaryWritten is what the cut says when the vendor wrote no summary for
// it. IT IS A STATEMENT, NOT A SUMMARY: an empty `summary` renders as a hole,
// and a reader looking at a hole cannot tell "the history was discarded and
// nothing stood in for it" from "this feed lost something". The sentence says
// which, in the reader's own words, and is replaced the moment a real summary
// arrives.
const NoSummaryWritten = "context compacted; no summary was written"

// contextCompacted stores that history was replaced by a summary of itself.
func (c *Converter) contextCompacted(record map[string]any, at Attribution, env envelope, agent string, next map[string]any) *storev1.StoreEntry {
	metadata := obj(record["compactMetadata"])
	summary := compactSummaryText(env.uuid, next)

	compacted := &conversationv1.ContextCompacted{
		Summary:    &conversationv1.AgentResponseProse{Markdown: summary},
		Tokens:     compactTokens(metadata),
		DurationMs: uint64(number(metadata["durationMs"])),
	}
	if str(metadata["trigger"]) == "auto" || str(metadata["trigger"]) == "automatic" {
		compacted.Trigger = &conversationv1.ContextCompacted_Automatic{Automatic: &conversationv1.ContextCompactionAutomatic{}}
	} else if str(metadata["trigger"]) != "" {
		compacted.Trigger = &conversationv1.ContextCompacted_Requested{Requested: &conversationv1.ContextCompactionRequested{}}
	}

	if summary == "" {
		// THE CUT IS REAL AND IS DRAWN EITHER WAY, but never as a hole: the
		// placeholder stands in for the discarded history so the reader is TOLD
		// there was no summary rather than left to guess. It is INFO because
		// nothing here is degraded any more — the condition is stated on the
		// wire, and the vendor writing no summary is the vendor's business.
		//
		// AND THE SUMMARY MAY STILL BE COMING. It is not always the very next
		// line: a real transcript put a `system/scheduled_task_fire` between a
		// boundary and its summary, and the summary named the boundary as its
		// parent. The boundary is remembered here so that summary, whenever it
		// lands and however many lines or batches later, supersedes this
		// placeholder on the cut's own key (attachCompactSummary).
		compacted.Summary = &conversationv1.AgentResponseProse{Markdown: NoSummaryWritten}
		c.pendingCut = &pendingCompaction{boundaryUUID: env.uuid, agent: agent, compacted: compacted}
		c.log.With(at.ctxFor("context-compacted")).With(logging.Context{UpsertKey: SessionKey("context_cut", env.uuid)}).
			Log("compact boundary carries no summary on the following line; the cut is drawn with the stated placeholder %q, and a summary naming this boundary later supersedes it", NoSummaryWritten)
	} else {
		c.pendingCut = nil
		c.log.With(at.ctxFor("context-compacted")).With(logging.Context{UpsertKey: SessionKey("context_cut", env.uuid)}).
			Log("compaction trigger=%q tokens %d->%d coalesced with its summary (%d characters)",
				str(metadata["trigger"]), compacted.GetTokens().GetTokensBefore(), compacted.GetTokens().GetTokensAfter(), len(summary))
	}

	// A COMPACTION IS ONE VENDOR RECORD ON BOTH PLANES — the stream's
	// `compact_boundary` and this one share a uuid — so the boundary's own uuid
	// IS the cut's identity, and the two writes collapse onto one row.
	return c.contextCutEntry(at, env.uuid, agent, &conversationv1.ContextCut{
		Cut: &conversationv1.ContextCut_Compacted{Compacted: compacted},
	})
}

// compactTokens reads what the cut did to the context's size.
//
// CONCERN, STATED IN THE DATA: the vendor's boundary carries `preTokens` and
// `postTokens`, and both are read here. A boundary that carries neither yields
// zeros — the message declares both fields non-optional, so absence cannot be
// expressed, and the log above prints what was read so a zero is attributable.
func compactTokens(metadata map[string]any) *conversationv1.ContextTokenDelta {
	return &conversationv1.ContextTokenDelta{
		TokensBefore: int64(number(metadata["preTokens"])),
		TokensAfter:  int64(number(metadata["postTokens"])),
	}
}

// contextCutEntry lands a cut as a page line of the MAIN agent's book.
//
// THE MAIN AGENT'S BOOK SPECIFICALLY: a cut is a fact about the session's
// context, not about whichever subagent happened to be running, and the feed
// draws it as the separation divider in the main conversation.
//
// `identity` IS THE CUT, not the record: the two planes see one record for a
// compaction and two unrelated records for a clear, so each arm states the one
// spelling the other plane can also mint.
func (c *Converter) contextCutEntry(at Attribution, identity, agent string, cut *conversationv1.ContextCut) *storev1.StoreEntry {
	book := firstNonEmpty(at.MainAgentID, agent)
	frame := updateFrame(book, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_ContextCut{ContextCut: cut},
	})
	return c.landFrame(at, book, SessionKey("context_cut", identity), "context_cut", frame)
}

// IsCompactBoundary reports whether a record is a compaction boundary — the one
// record in a transcript whose meaning depends on a line that may not be written
// yet, and therefore the one legitimate hold.
func IsCompactBoundary(record map[string]any) bool {
	return str(record["type"]) == "system" && str(record["subtype"]) == "compact_boundary"
}

// pendingCompaction is a cut this converter drew with the placeholder, kept so
// the summary that names it can supersede that draw whenever it arrives.
//
// ONE, NOT A MAP. A session compacts one context at a time and the boundary is
// the only record that can be waiting; a second boundary means the first one's
// summary is never coming.
type pendingCompaction struct {
	boundaryUUID string
	agent        string
	compacted    *conversationv1.ContextCompacted
}

// attachCompactSummary supersedes a placeholder cut with the summary that named
// its boundary, and answers nil for a summary that names none of ours.
//
// THE KEY IS THE CUT'S, THE POSITION IS THIS RECORD'S. Re-emitting under
// `session:context_cut:<boundary uuid>` is what makes the store replace the
// placeholder row rather than stand a second cut beside it, and attributing at
// the SUMMARY's offset is what makes it a later write rather than a replay of
// the first.
func (c *Converter) attachCompactSummary(record map[string]any, at Attribution) *storev1.StoreEntry {
	pending := c.pendingCut
	if pending == nil {
		return nil
	}
	if str(record["parentUuid"]) != pending.boundaryUUID {
		return nil
	}
	summary := summaryProse(record)
	if summary == "" {
		return nil
	}
	c.pendingCut = nil
	pending.compacted.Summary = &conversationv1.AgentResponseProse{Markdown: summary}
	c.log.With(at.ctxFor("context-compacted")).With(logging.Context{UpsertKey: SessionKey("context_cut", pending.boundaryUUID)}).
		Log("the compact boundary's summary arrived on a later line (%d characters); it supersedes the placeholder on the cut's own key", len(summary))
	return c.contextCutEntry(at, pending.boundaryUUID, pending.agent, &conversationv1.ContextCut{
		Cut: &conversationv1.ContextCut_Compacted{Compacted: pending.compacted},
	})
}

// compactSummaryText returns the summary carried by the line FOLLOWING a
// boundary, or "" when that line is not a summary.
//
// The summary line is typed `user`, which is why it is identified by the
// envelope's flag and never by its type: it is the harness's own text standing in
// for the discarded history, not the user's prompt.
//
// POSITION IS THE WHOLE CLAIM HERE, and it is why this asks for no parent link:
// the line immediately after a boundary is that boundary's summary by file
// order, which is the rule two compactions in one file are told apart by. A
// summary further away has to name its boundary instead — see
// attachCompactSummary.
func compactSummaryText(boundaryUUID string, next map[string]any) string {
	if next == nil {
		return ""
	}
	if parent := str(next["parentUuid"]); parent != "" && parent != boundaryUUID {
		// It names a DIFFERENT boundary, so file order is not what it looks
		// like and this cut's summary is elsewhere.
		return ""
	}
	return summaryProse(next)
}

// summaryProse reads the harness's standing-in text off a compaction summary
// record, or "" for a record that is not one.
func summaryProse(next map[string]any) string {
	if str(next["type"]) != "user" || !boolean(next["isCompactSummary"]) {
		return ""
	}
	content := obj(next["message"])["content"]
	if s, ok := content.(string); ok {
		return s
	}
	var parts []string
	for _, raw := range list(content) {
		block := obj(raw)
		if block != nil && str(block["type"]) == "text" {
			parts = append(parts, str(block["text"]))
		}
	}
	return strings.Join(parts, "\n")
}

// ---------------------------------------------------------------------------
// clear detection
// ---------------------------------------------------------------------------

// clearCommand is the command name a cleared context is spelled with.
const clearCommand = "/clear"

// commandEnvelopeTags are the elements the harness expands a slash command into,
// in the order it writes them.
var commandEnvelopeTags = []string{"command-name", "command-message", "command-args"}

// isClearCommand reports whether a user message is `/clear` and nothing else.
// It is IsClearCommand, the one spelling of the rule the reader's rotation hold
// shares, so the converter and the hold can never disagree about what a clear is.
func (c *Converter) isClearCommand(message map[string]any) bool {
	return IsClearCommand(message)
}

// IsClearCommand reports whether a `user` record's message is `/clear` and
// nothing else.
//
// Detection unwraps the envelope first and then requires the command to be the
// ONLY non-whitespace content left: an argument, or prose around the envelope,
// means the user asked for something else.
func IsClearCommand(message map[string]any) bool {
	// A clear is always plain text. A blocks-form user message is a tool result
	// or a composed prompt, never a command envelope.
	text, ok := message["content"].(string)
	if !ok {
		return false
	}
	name, args, ok := unwrapCommandEnvelope(text)
	return ok && name == clearCommand && args == ""
}

// unwrapCommandEnvelope reduces a user prompt to the command it invokes.
//
// Text with no <command-name> element is returned verbatim as the name, so a
// prompt whose entire content is "/clear" is recognized as one. Text carrying the
// envelope must consist of NOTHING but the envelope's elements: leftover
// non-whitespace outside them means the prompt merely QUOTES a command — a pasted
// transcript, a tool result echoing one — rather than invoking it.
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
	// A retained guard, not dead weight: the leftover check above already rejects
	// every string carrying an unconsumable <command-name>, so this branch is not
	// reachable today — but it is the one thing standing between a future change
	// to that check and a nameless envelope being read as a valid command.
	name, present := found[commandEnvelopeTags[0]]
	if !present {
		return "", "", false
	}
	return strings.TrimSpace(name), strings.TrimSpace(found[commandEnvelopeTags[2]]), true
}

// takeTag removes the first <tag>…</tag> element from s, returning its inner text
// and s without it. An unterminated open tag is not an element and is left in
// place, so the caller's leftover check rejects the string.
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
