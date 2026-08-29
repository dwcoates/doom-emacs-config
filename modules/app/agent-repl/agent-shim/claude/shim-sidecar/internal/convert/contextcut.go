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
// `session:context_cut:<record uuid>`.

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
	c.log.With(at.ctxFor("context-cleared")).With(logging.Context{UpsertKey: SessionKey("context_cut", env.uuid)}).
		Log("context clear: history discarded outright, with no token delta the vendor stated")
	return c.contextCutEntry(at, env, agent, &conversationv1.ContextCut{
		Cut: &conversationv1.ContextCut_Cleared{Cleared: &conversationv1.ContextCleared{}},
	})
}

// contextCompacted stores that history was replaced by a summary of itself.
func (c *Converter) contextCompacted(record map[string]any, at Attribution, env envelope, agent string, next map[string]any) *storev1.StoreEntry {
	metadata := obj(record["compactMetadata"])
	summary := compactSummaryText(next)

	if summary == "" {
		// A compaction with no summary renders as a hole where the discarded
		// history was. It is emitted anyway — the cut is REAL and a reader must
		// see WHERE — but never silently.
		c.log.With(at.ctxWarn("context-compacted")).With(logging.Context{UpsertKey: SessionKey("context_cut", env.uuid)}).
			Log("compact boundary is not followed by a summary line; the cut renders with nothing in place of the discarded history")
	}

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

	c.log.With(at.ctxFor("context-compacted")).With(logging.Context{UpsertKey: SessionKey("context_cut", env.uuid)}).
		Log("compaction trigger=%q tokens %d->%d coalesced with its summary (%d characters)",
			str(metadata["trigger"]), compacted.GetTokens().GetTokensBefore(), compacted.GetTokens().GetTokensAfter(), len(summary))

	return c.contextCutEntry(at, env, agent, &conversationv1.ContextCut{
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
func (c *Converter) contextCutEntry(at Attribution, env envelope, agent string, cut *conversationv1.ContextCut) *storev1.StoreEntry {
	book := firstNonEmpty(at.MainAgentID, agent)
	frame := updateFrame(book, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_ContextCut{ContextCut: cut},
	})
	return c.landFrame(at, book, SessionKey("context_cut", env.uuid), "context_cut", frame)
}

// IsCompactBoundary reports whether a record is a compaction boundary — the one
// record in a transcript whose meaning depends on a line that may not be written
// yet, and therefore the one legitimate hold.
func IsCompactBoundary(record map[string]any) bool {
	return str(record["type"]) == "system" && str(record["subtype"]) == "compact_boundary"
}

// compactSummaryText returns the summary carried by the line FOLLOWING a
// boundary, or "" when that line is not a summary.
//
// The summary line is typed `user`, which is why it is identified by the
// envelope's flag and never by its type: it is the harness's own text standing in
// for the discarded history, not the user's prompt.
func compactSummaryText(next map[string]any) string {
	if next == nil || str(next["type"]) != "user" || !boolean(next["isCompactSummary"]) {
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
//
// Detection unwraps the envelope first and then requires the command to be the
// ONLY non-whitespace content left: an argument, or prose around the envelope,
// means the user asked for something else.
func (c *Converter) isClearCommand(message map[string]any) bool {
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
