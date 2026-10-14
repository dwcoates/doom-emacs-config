package convert

// contextcut.go — THE CONVERSATION WAS CUT HERE, and the reader must see where.
//
// Two producers, one carrier. `/clear` is detected by UNWRAPPING the vendor's
// expanded command envelope — the literal "/clear" NEVER appears on disk, so
// anything matching raw prompt text against it misses every real session.
// Compaction COALESCES the boundary record with ITS summary: the first summary
// record AFTER it in FILE ORDER whose parent links lead back to it — never
// timestamp order, because the harness composes the summary before writing the
// boundary that announces it, so the summary's timestamp is EARLIER and a
// timestamp-ordered assembly pairs every boundary with the wrong summary in a
// session that compacted more than once. The summary is not always the next
// line: the harness writes `instructions`, `session_context` and `date`
// attachments between them, each the previous one's child (owner's report,
// 2026-10-14: 32 of 169 compactions on disk were written that way).
//
// Both land as a page line of the MAIN AGENT'S book, keyed
// `session:context_cut:<the cut's identity>` — the boundary's uuid for a
// compaction, and for a clear THE SESSION IT ROTATED TO, because those are the
// spellings the STREAM plane can mint for the same cut. See `clearCutIdentity`.

import (
	"strings"

	"google.golang.org/protobuf/proto"

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
func (c *Converter) contextCompacted(record map[string]any, at Attribution, env envelope, agent string, following []map[string]any) *storev1.StoreEntry {
	metadata := obj(record["compactMetadata"])
	summary := summaryProse(compactSummaryAfter(env.uuid, following))

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
		// AND THE SUMMARY MAY STILL BE COMING: it was not in this batch. The
		// boundary is remembered, with every record that descends from it, so
		// that summary, whenever it lands and however many lines or batches
		// later, supersedes this placeholder on the cut's own key
		// (attachCompactSummary).
		compacted.Summary = &conversationv1.AgentResponseProse{Markdown: NoSummaryWritten}
		c.pendingCut = &pendingCompaction{boundaryUUID: env.uuid, agent: agent, compacted: compacted,
			descendants: newLineage(env.uuid)}
		c.log.With(at.ctxFor("context-compacted")).With(logging.Context{UpsertKey: SessionKey("context_cut", env.uuid)}).
			Log("compact boundary's summary is not in its batch; the cut is drawn with the stated placeholder %q, and the summary descending from this boundary supersedes it when it arrives (records after it in the batch: %d)", NoSummaryWritten, len(following))
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
	// descendants are the boundary and every record since it that descends
	// from it: the records the summary's parent link may name.
	descendants lineage
}

// extend records RECORD as one of the boundary's descendants when its parent
// is one. A nil cut waits for nothing, so it learns nothing.
func (p *pendingCompaction) extend(record map[string]any) {
	if p == nil {
		return
	}
	p.descendants.extend(str(record["uuid"]), str(record["parentUuid"]))
}

// descendsFrom reports whether RECORD's parent is one of the boundary's
// descendants.
func (p *pendingCompaction) descendsFrom(record map[string]any) bool {
	return p.descendants.has(str(record["parentUuid"]))
}

// attachCompactSummary supersedes a placeholder cut with the summary that
// descends from its boundary, and answers nil for a summary that does not.
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
	if !pending.descendsFrom(record) {
		return nil
	}
	summary := summaryProse(record)
	if summary == "" {
		return nil
	}
	c.pendingCut = nil
	// A COPY, NEVER THE PLACEHOLDER'S OWN MESSAGE: the placeholder's entry
	// already left with that message, and writing the summary into it would
	// rewrite an entry this converter no longer owns.
	compacted := proto.Clone(pending.compacted).(*conversationv1.ContextCompacted)
	compacted.Summary = &conversationv1.AgentResponseProse{Markdown: summary}
	c.log.With(at.ctxFor("context-compacted")).With(logging.Context{UpsertKey: SessionKey("context_cut", pending.boundaryUUID)}).
		Log("the compact boundary's summary arrived on a later line (%d characters); it supersedes the placeholder on the cut's own key", len(summary))
	return c.contextCutEntry(at, pending.boundaryUUID, pending.agent, &conversationv1.ContextCut{
		Cut: &conversationv1.ContextCut_Compacted{Compacted: compacted},
	})
}

// compactSummaryAfter returns the summary record of the boundary BOUNDARYUUID
// from FOLLOWING, the records after it in file order, or nil when the batch
// does not hold it.
//
// The summary is the first summary record whose parent links lead back to the
// boundary through the records between them — or, POSITION BEING ITS OWN
// CLAIM, the very next record when that summary names no parent at all. A
// summary naming a parent that does not descend from the boundary is never
// this boundary's, however close it stands. Reaching another boundary first
// ends the search: a second boundary means the first one's summary is never
// coming. A summary descending from something else is not this boundary's,
// and the search walks on past it.
//
// The summary line is typed `user`, which is why it is identified by the
// envelope's flag and never by its type: it is the harness's own text standing in
// for the discarded history, not the user's prompt.
func compactSummaryAfter(boundaryUUID string, following []map[string]any) map[string]any {
	chain := newLineage(boundaryUUID)
	for i, record := range following {
		if record == nil {
			// An unparsable line names no parent and is no summary.
			continue
		}
		if IsCompactBoundary(record) {
			return nil
		}
		adjacentOrphan := i == 0 && str(record["parentUuid"]) == ""
		if summaryProse(record) != "" && (adjacentOrphan || chain.has(str(record["parentUuid"]))) {
			return record
		}
		chain.extend(str(record["uuid"]), str(record["parentUuid"]))
	}
	return nil
}

// AwaitsCompactSummary returns the index in RECORDS of the batch's compaction
// boundary whose summary the batch does not hold, or -1 when every boundary in
// it is settled. Only the LAST boundary can be waiting: a later boundary means
// an earlier one's summary is never coming.
func AwaitsCompactSummary(records []map[string]any) int {
	for i := len(records) - 1; i >= 0; i-- {
		if records[i] == nil || !IsCompactBoundary(records[i]) {
			continue
		}
		if compactSummaryAfter(str(records[i]["uuid"]), records[i+1:]) != nil {
			return -1
		}
		return i
	}
	return -1
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
