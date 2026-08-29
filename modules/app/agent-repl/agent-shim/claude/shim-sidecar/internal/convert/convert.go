// Package convert reads the Claude harness's on-disk JSON records and turns
// them into the store.v1 entries the sidecar writes.
//
// THE CONVERSION HALF IS UNPORTED, AND THAT IS STATED RATHER THAN HIDDEN. This
// package used to target conversation.v1's MessageEntry record model — UserSaid,
// AgentSaid, ToolReturned, ContextCut, FailureRaised, SkillBodyResolved and the
// DetachedWork* lifecycle, each with a MessageAuthor and a MessageParent. The
// redesigned contract DELETED that model outright and replaced it with the
// Agent* protocol model (AgentFrame / AgentPrompt / AgentActivity, keyed by
// AgentId and AgentActivityId). That is a different structure, not a rename, and
// populating it from Claude's JSONL is a design decision no reconciliation may
// take on its own.
//
// THE OUTCOMES A RECORD CAN HAVE, and nothing else happens to one:
//
//   - UNPORTED. The record was understood well enough to name the conversion it
//     used to get, and that conversion no longer exists. It is stored WHOLE via
//     UnportedEntry, and the failure is logged at error level with the deleted
//     type named. Every such record is queryable by StoreUnknown's
//     discriminator_field == UnportedField.
//   - UNSERVED. The record is understood and deliberately not carried
//     (VendorSpecificEntry), parsed and not modeled (UnknownEntry), or not
//     readable at all (UnparsedEntry).
//   - Nothing else. There is no drop. The total-ingestion mandate is that every
//     JSON object on disk ends up in the store as a protobuf shape, and it still
//     holds: an unported record is still a record in the database.
//
// THE VENDOR-SIDE READING IS KEPT INTACT — envelope parsing, tool-call/result
// correlation, skill-name resolution, launch classification, compaction
// coalescing, /clear envelope unwrapping. None of it depends on the deleted
// protos, and all of it is knowledge that would have to be rebuilt from scratch.
package convert

import (
	"fmt"
	"strings"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
)

// Converter holds the per-file correlation a conversion needs beyond the record
// in front of it. It is NOT safe for concurrent use; each tailer owns one.
type Converter struct {
	log *logging.Bound

	// toolCallOwner maps a vendor tool_call_id to the message that MADE the
	// call. A tool result carries the id of the call it answers and nothing
	// else, and ToolReturned must name the message the result folds onto — so
	// this is the only thing standing between a result and a correlation pass
	// in a consumer.
	toolCallOwner map[string]string

	// lineOwner maps an assistant LINE's uuid to the message id it belongs to.
	// The harness splits one response across several lines sharing one
	// message.id, and a tool result names the assistant LINE it answers
	// (`sourceToolAssistantUUID`), so resolving that name needs this second hop.
	lineOwner map[string]string

	// skillMessage maps a skill's invoked name to the detached-work message that
	// opened it, so the body the harness writes as a SEPARATE record later can
	// be resolved onto the skill's own card.
	skillMessage map[string]string
}

// New builds a Converter.
func New(log *logging.Bound) *Converter {
	log.With(logging.Context{Operation: "convert-new"}).LogVerbose("constructing converter")
	return &Converter{
		log:           log,
		toolCallOwner: map[string]string{},
		lineOwner:     map[string]string{},
		skillMessage:  map[string]string{},
	}
}

// envelope is the common header on user/assistant/system/attachment lines,
// reduced to the fields lineage and attribution actually need.
type envelope struct {
	uuid        string
	parentUUID  string
	logicalUUID string
	isSidechain bool
	agentID     string
	isMeta      bool
	isSummary   bool
	toolUseID   string
	assistUUID  string
	apiError    bool
	errorText   string
}

func readEnvelope(obj map[string]any) envelope {
	return envelope{
		uuid:        str(obj["uuid"]),
		parentUUID:  str(obj["parentUuid"]),
		logicalUUID: str(obj["logicalParentUuid"]),
		isSidechain: boolean(obj["isSidechain"]),
		agentID:     str(obj["agentId"]),
		isMeta:      boolean(obj["isMeta"]),
		isSummary:   boolean(obj["isCompactSummary"]),
		toolUseID:   str(obj["sourceToolUseID"]),
		assistUUID:  str(obj["sourceToolAssistantUUID"]),
		apiError:    boolean(obj["isApiErrorMessage"]),
		errorText:   str(obj["error"]),
	}
}

// callBlockTypes are the vendor's spellings of "the model called a tool".
var callBlockTypes = map[string]bool{
	"tool_use":        true,
	"server_tool_use": true,
	"mcp_tool_use":    true,
}

// resultBlockTypes are the vendor's spellings of "a tool answered".
var resultBlockTypes = map[string]bool{
	"tool_result":            true,
	"web_search_tool_result": true,
	"mcp_tool_result":        true,
}

// blockType reads a content block's discriminator.
func blockType(block map[string]any) string {
	return str(block["type"])
}

func str(v any) string   { s, _ := v.(string); return s }
func boolean(v any) bool { b, _ := v.(bool); return b }
func obj(v any) map[string]any {
	m, _ := v.(map[string]any)
	return m
}

// container returns the detached-work message this record sits inside, and
// whether there is one.
//
// A sidechain line names its own agent, which is how a subagent's records land
// inside the card that spawned it even when they are read from a different file
// than the launch was.
func (c *Converter) container(at Attribution, env envelope) string {
	if at.Container != "" {
		return at.Container
	}
	if env.isSidechain && env.agentID != "" {
		return DetachedWorkMessageID(env.agentID)
	}
	return ""
}

// Line converts one decoded transcript line into the records it implies.
//
// `next` is the line that FOLLOWS this one IN THE FILE, or nil at the end of a
// batch. It exists for exactly one record: a compaction boundary, whose summary
// the harness writes as the following line.
//
// It NEVER returns zero entries for a line it was given. A line it cannot place
// is stored unconverted, because the ingestion mandate binds this package
// absolutely: irrelevance to a reader is a consumption-side judgment, never a
// reason to leave a record out of the database.
func (c *Converter) Line(record map[string]any, at Attribution, next map[string]any) []*storev1.StoreEntry {
	kind := str(record["type"])
	c.log.With(logging.Context{Operation: "convert-line", Path: at.Path, VendorSessionID: at.SessionID}).
		LogVerbose("converting line type=%q offset=%d keys=%d", kind, at.Offset, len(record))

	switch kind {
	case "":
		// No discriminator at all. It parsed, so it is not unparsed; we simply
		// cannot say what it is, which is exactly what UnknownEntry means.
		c.log.With(logging.Context{Operation: "convert-line", Path: at.Path, VendorSessionID: at.SessionID, Level: "warn"}).
			Log("transcript line at offset=%d carries no %q field; stored unconverted with no path to the daemon", at.Offset, "type")
		return []*storev1.StoreEntry{UnknownEntry(at, "", "type", record)}
	case "user":
		return c.userLine(record, at)
	case "assistant":
		return c.assistantLine(record, at)
	case "system":
		return c.systemLine(record, at, next)
	case "attachment":
		return c.attachmentLine(record, at)
	default:
		if knownMetadataLines[kind] {
			// A metadata line the harness writes about its own bookkeeping. We
			// know exactly what each one is and have decided not to carry it,
			// which is a different situation from not knowing — so it is
			// vendor_specific, and the follow-up it asks for is a CONVERTER, if
			// one of them turns out to be portable after all.
			return []*storev1.StoreEntry{VendorSpecificEntry(at, kind, record)}
		}
		// A top-level type this reader has never seen. We parsed it and do not
		// model it, so the follow-up it asks for is a MODEL — and filing it as
		// vendor_specific would claim an understanding nobody has.
		c.log.With(logging.Context{Operation: "convert-line", Path: at.Path, VendorSessionID: at.SessionID, Level: "warn"}).
			Log("transcript line type=%q at offset=%d is not modeled; stored unconverted with no path to the daemon", kind, at.Offset)
		return []*storev1.StoreEntry{UnknownEntry(at, kind, "type", record)}
	}
}

// knownMetadataLines are the top-level line types the harness writes about its
// OWN bookkeeping rather than about the conversation.
//
// THE LIST IS WHAT SEPARATES "understood and not carried" FROM "not
// understood". Without it every unmodeled type would be filed as
// vendor_specific, which asserts we know what a brand-new line means — and the
// two arms exist precisely so the follow-up each one needs is distinguishable: a
// converter for the first, a model for the second.
var knownMetadataLines = map[string]bool{
	"mode":                  true,
	"permission-mode":       true,
	"queue-operation":       true,
	"last-prompt":           true,
	"ai-title":              true,
	"pr-link":               true,
	"file-history-snapshot": true,
	"file-history-delta":    true,
	"frame-link":            true,
	"attribution-snapshot":  true,
}

// ---------------------------------------------------------------------------
// user lines
// ---------------------------------------------------------------------------

// userLine converts a `user` record, which is FOUR different things wearing one
// type tag: a person's prompt, a tool's result the vendor filed under the user,
// the harness's own compaction summary, and the expanded `/clear` envelope.
func (c *Converter) userLine(record map[string]any, at Attribution) []*storev1.StoreEntry {
	env := readEnvelope(record)
	container := c.container(at, env)
	message := obj(record["message"])

	var out []*storev1.StoreEntry

	// A launch result opens a detached-work card. It rides on a user record
	// because the vendor files tool results there, and it is emitted BEFORE the
	// result itself so the card exists by the time anything updates it.
	out = append(out, c.launchRecords(record, at, env, container)...)

	// Tool results are lifted out of the user's content and become updates to
	// the message that MADE the call.
	results, orphans := c.toolReturns(message, at, env, container)
	out = append(out, results...)

	switch {
	case env.isSummary:
		// The harness's own prose standing in for discarded history. It is
		// consumed by the compaction boundary that precedes it (systemLine), so
		// emitting it here as a user message would render the summary twice and
		// attribute the harness's text to the person.
		c.log.With(logging.Context{Operation: "compact-summary", Path: at.Path, VendorSessionID: at.SessionID}).
			LogVerbose("compaction summary at offset=%d folded into its boundary", at.Offset)
	case c.isClearCommand(message):
		out = append(out, c.contextCleared(at, env, container))
	case len(orphans) > 0 && !hasUserProse(message):
		// The record carried tool results whose calling message we never saw and
		// nothing else. There is no legal parent to state, so it is stored whole
		// rather than given an invented one.
		out = append(out, UnknownEntry(at, "tool_result", "message.content[].type", record))
	case hasToolResults(message) && !hasUserProse(message):
		// A pure carrier for tool results, which have already been lifted out
		// above. The vendor files results under the user's role; emitting an
		// empty UserSaid alongside them would put words in a person's mouth and
		// spend a page slot on a message nobody wrote.
	case env.isMeta && !hasUserProse(message):
		// A harness-injected user record with no prose: a system reminder, a
		// skill body, an attachment carrier. Not something a person said.
		out = append(out, VendorSpecificEntry(at, "user.meta", record))
	default:
		out = append(out, c.userSaid(record, at, env, container))
	}

	if len(out) == 0 {
		// Every branch above is either an emit or a documented fold, so this is
		// only reachable if one stops emitting. It fails LOUD and stores the
		// record rather than letting a line vanish silently.
		c.log.With(logging.Context{Operation: "convert-line", Path: at.Path, VendorSessionID: at.SessionID, Level: "warn"}).
			Log("user line at offset=%d produced no record; stored unconverted so it is not lost", at.Offset)
		out = append(out, UnknownEntry(at, "user", "type", record))
	}
	return out
}

// hasToolResults reports whether a user message carries any tool result at all,
// which is what makes it a carrier rather than something a person said.
func hasToolResults(message map[string]any) bool {
	blocks, ok := message["content"].([]any)
	if !ok {
		return false
	}
	for _, el := range blocks {
		block, ok := el.(map[string]any)
		if ok && resultBlockTypes[blockType(block)] {
			return true
		}
	}
	return false
}

// hasUserProse reports whether a user message carries anything a person could
// have typed, as opposed to being a pure carrier for tool results.
func hasUserProse(message map[string]any) bool {
	switch content := message["content"].(type) {
	case string:
		return strings.TrimSpace(content) != ""
	case []any:
		for _, el := range content {
			block, ok := el.(map[string]any)
			if !ok {
				return true
			}
			if !resultBlockTypes[blockType(block)] {
				return true
			}
		}
	}
	return false
}

// userSaid stores a person's prompt.
//
// UNPORTED. Its target was conversation.v1 UserSaid carried on a MessageEntry
// with a MessageAuthor and a MessageParent, all of which the redesign deleted.
// UserSaid survives as a type, but it is now an AgentInput arm on the shim's
// request path, not a producer-written record on a store entry — a different
// structure, not a rename. The prompt is stored whole instead.
func (c *Converter) userSaid(record map[string]any, at Attribution, env envelope, container string) *storev1.StoreEntry {
	_, _ = env, container
	c.log.With(logging.Context{Operation: "user-said", Path: at.Path, VendorSessionID: at.SessionID, Level: "error"}).
		Log("user prompt at offset=%d has NO conversion under the redesigned conversation.v1: "+
			"MessageEntry/MessageAuthor/MessageParent were deleted and UserSaid is now an AgentInput arm. Record stored unported", at.Offset)
	return UnportedEntry(at, "user_said", record)
}

// toolReturns finds every tool result in a user record.
//
// It returns the stored records and the ids of the results it could NOT place.
// The RESOLUTION is kept — it is correlation over the vendor's own ids, and
// losing it would lose the only knowledge of which call a result answers — but
// the conversion is UNPORTED: conversation.v1 ToolReturned was deleted, and the
// successor (an AgentActivity settling on its AgentActivityId) is keyed
// differently. A result whose owner is unresolved is stored unconverted, never
// given an invented parent.
func (c *Converter) toolReturns(message map[string]any, at Attribution, env envelope, container string) ([]*storev1.StoreEntry, []string) {
	_ = container
	blocks, ok := message["content"].([]any)
	if !ok {
		return nil, nil
	}
	var out []*storev1.StoreEntry
	var orphans []string
	for _, el := range blocks {
		block, ok := el.(map[string]any)
		if !ok || !resultBlockTypes[blockType(block)] {
			continue
		}
		callID := str(block["tool_use_id"])
		owner, resolved := c.callOwner(callID, env)
		if !resolved {
			// The call this answers was read before this process's cursor, so
			// the message it folds onto is not derivable from the record. There
			// is no arm for an unresolved owner and none is invented.
			c.log.With(logging.Context{Operation: "tool-return", Path: at.Path, VendorSessionID: at.SessionID, Level: "warn"}).
				Log("tool result tool_call_id=%q at offset=%d names no message this reader observed; stored unconverted rather than given an invented parent", callID, at.Offset)
			orphans = append(orphans, callID)
			continue
		}
		c.log.With(logging.Context{Operation: "tool-return", Path: at.Path, VendorSessionID: at.SessionID, Level: "error"}).
			Log("tool result tool_call_id=%q owner=%q at offset=%d has NO conversion under the redesigned conversation.v1: "+
				"ToolReturned was deleted and AgentActivity settles by AgentActivityId. Record stored unported", callID, owner, at.Offset)
		out = append(out, UnportedEntry(at, "tool_returned:"+callID, block))
	}
	return out, orphans
}

// callOwner resolves the message that made a tool call, by the call's own id
// first and by the assistant LINE the vendor named second.
func (c *Converter) callOwner(callID string, env envelope) (string, bool) {
	if owner, ok := c.toolCallOwner[callID]; ok && owner != "" {
		return owner, true
	}
	if owner, ok := c.lineOwner[env.assistUUID]; ok && owner != "" {
		return owner, true
	}
	return "", false
}

// ---------------------------------------------------------------------------
// assistant lines
// ---------------------------------------------------------------------------

// assistantLine reads an `assistant` record and records the call ids it
// contains so the results that follow can find it.
//
// THE INDEXING IS KEPT AND THE CONVERSION IS UNPORTED. rememberCalls is pure
// vendor correlation and still runs; AgentSaid, FailureRaised and StopReason —
// everything the record used to become — were deleted, and the Agent* model's
// AgentResponse/AgentFailure are reached through an AgentFrame keyed by AgentId,
// which this reader has no mapping to mint.
func (c *Converter) assistantLine(record map[string]any, at Attribution) []*storev1.StoreEntry {
	env := readEnvelope(record)
	message := obj(record["message"])

	messageID := str(message["id"])
	if messageID == "" {
		// A response the vendor did not name. The LINE's own uuid is the only
		// stable identity left, and it is a real one — it just cannot fold two
		// lines of one response together.
		messageID = env.uuid
	}
	c.rememberCalls(message, env, messageID)

	// The vendor records its own API failures as assistant records flagged on
	// the envelope. The distinction is PRESERVED in the stored discriminator so
	// a reader can still tell a failed turn from an absent one.
	conversion := "agent_said"
	if env.apiError {
		conversion = "failure_raised"
	}
	c.log.With(logging.Context{Operation: "assistant-line", Path: at.Path, VendorSessionID: at.SessionID, Level: "error"}).
		Log("assistant record (%s, message_id=%q) at offset=%d has NO conversion under the redesigned conversation.v1: "+
			"AgentSaid/FailureRaised/StopReason were deleted and AgentResponse rides an AgentFrame keyed by AgentId. Record stored unported",
			conversion, messageID, at.Offset)
	return []*storev1.StoreEntry{UnportedEntry(at, conversion, record)}
}

// rememberCalls indexes every call this response made, under both names a later
// result can arrive with.
func (c *Converter) rememberCalls(message map[string]any, env envelope, messageID string) {
	if env.uuid != "" {
		c.lineOwner[env.uuid] = messageID
	}
	blocks, ok := message["content"].([]any)
	if !ok {
		return
	}
	for _, el := range blocks {
		block, ok := el.(map[string]any)
		if !ok || !callBlockTypes[blockType(block)] {
			continue
		}
		if id := str(block["id"]); id != "" {
			c.toolCallOwner[id] = messageID
		}
	}
}

func firstNonEmpty(values ...string) string {
	for _, v := range values {
		if v != "" {
			return v
		}
	}
	return ""
}

// ---------------------------------------------------------------------------
// system lines
// ---------------------------------------------------------------------------

// systemLine converts a `system` record by its subtype. Two subtypes have a
// vendor-agnostic reading; the rest are the harness narrating itself.
func (c *Converter) systemLine(record map[string]any, at Attribution, next map[string]any) []*storev1.StoreEntry {
	env := readEnvelope(record)
	container := c.container(at, env)
	subtype := str(record["subtype"])
	switch subtype {
	case "":
		c.log.With(logging.Context{Operation: "convert-line", Path: at.Path, VendorSessionID: at.SessionID, Level: "warn"}).
			Log("system line at offset=%d carries no %q field; stored unconverted", at.Offset, "subtype")
		return []*storev1.StoreEntry{UnknownEntry(at, "", "subtype", record)}
	case "compact_boundary":
		return []*storev1.StoreEntry{c.contextCompacted(record, at, env, container, next)}
	case "api_error":
		return []*storev1.StoreEntry{c.apiErrorFailure(record, at, env, container)}
	default:
		return []*storev1.StoreEntry{VendorSpecificEntry(at, "system."+subtype, record)}
	}
}

// apiErrorFailure stores the vendor's own recorded API error.
//
// UNPORTED. conversation.v1 FailureRaised was deleted. api.proto's
// ApiRequestFailed is the nearest successor, but it is an AgentFailure arm on an
// AgentFrame with a typed kind oneof, not a free-text summary/detail/retry
// record — mapping the vendor's error object onto those arms is a design
// decision, so the record is stored whole.
func (c *Converter) apiErrorFailure(record map[string]any, at Attribution, env envelope, container string) *storev1.StoreEntry {
	_, _ = env, container
	c.log.With(logging.Context{Operation: "api-error", Path: at.Path, VendorSessionID: at.SessionID, Level: "error"}).
		Log("vendor api_error at offset=%d has NO conversion under the redesigned conversation.v1: "+
			"FailureRaised was deleted and ApiRequestFailed is a typed AgentFailure arm. Record stored unported", at.Offset)
	return UnportedEntry(at, "failure_raised", record)
}

func number(v any) float64 {
	f, _ := v.(float64)
	return f
}

// ---------------------------------------------------------------------------
// attachment lines
// ---------------------------------------------------------------------------

// attachmentLine converts an `attachment` record. One attachment type carries a
// conversation fact — the body of a skill that was invoked — and the rest are
// context the harness injected, which no vendor-agnostic feed shows.
func (c *Converter) attachmentLine(record map[string]any, at Attribution) []*storev1.StoreEntry {
	attachment := obj(record["attachment"])
	if attachment == nil {
		c.log.With(logging.Context{Operation: "convert-line", Path: at.Path, VendorSessionID: at.SessionID, Level: "warn"}).
			Log("attachment line at offset=%d carries no %q object; stored unconverted", at.Offset, "attachment")
		return []*storev1.StoreEntry{UnknownEntry(at, "", "attachment", record)}
	}
	kind := str(attachment["type"])
	if kind != "invoked_skills" {
		return []*storev1.StoreEntry{VendorSpecificEntry(at, "attachment."+kind, record)}
	}
	env := readEnvelope(record)
	container := c.container(at, env)
	entries := c.skillBodies(attachment, at, container)
	if len(entries) == 0 {
		// Every skill in the attachment named a card this reader never opened.
		// The bodies are stored whole rather than resolved onto an invented one.
		return []*storev1.StoreEntry{VendorSpecificEntry(at, "attachment.invoked_skills", record)}
	}
	return entries
}

// skillBodies resolves each invoked skill's file contents onto the skill it
// belongs to, and stores each body unported.
//
// UNPORTED. conversation.v1 SkillBodyResolved was deleted; AgentSkillUse in the
// Agent* model is an AgentActivity arm, not a record a file reader mints. The
// NAME RESOLUTION is kept, so a body whose invocation this reader never saw is
// still refused rather than attached to an invented card.
func (c *Converter) skillBodies(attachment map[string]any, at Attribution, container string) []*storev1.StoreEntry {
	_ = container
	skills, ok := attachment["skills"].([]any)
	if !ok {
		return nil
	}
	var out []*storev1.StoreEntry
	for _, el := range skills {
		skill, ok := el.(map[string]any)
		if !ok {
			continue
		}
		name := str(skill["name"])
		messageID, resolved := c.skillMessage[name]
		if !resolved || messageID == "" {
			c.log.With(logging.Context{Operation: "skill-body", Path: at.Path, VendorSessionID: at.SessionID, Level: "warn"}).
				Log("skill body name=%q at offset=%d names no skill invocation this reader observed; not resolved onto a card", name, at.Offset)
			continue
		}
		c.log.With(logging.Context{Operation: "skill-body", Path: at.Path, VendorSessionID: at.SessionID, Level: "error"}).
			Log("skill body name=%q message_id=%q at offset=%d has NO conversion under the redesigned conversation.v1: "+
				"SkillBodyResolved was deleted and AgentSkillUse is an AgentActivity arm. Record stored unported", name, messageID, at.Offset)
		out = append(out, UnportedEntry(at, "skill_body:"+name, skill))
	}
	return out
}

// ---------------------------------------------------------------------------
// context cuts
// ---------------------------------------------------------------------------

// contextCleared stores that history was discarded outright.
//
// UNPORTED. conversation.v1 ContextCut survives as a type but no longer has a
// producer-written home: MessageEntry, the record that carried it, is gone.
func (c *Converter) contextCleared(at Attribution, env envelope, container string) *storev1.StoreEntry {
	_ = container
	c.log.With(logging.Context{Operation: "context-cleared", Path: at.Path, VendorSessionID: at.SessionID, Level: "error"}).
		Log("context clear at offset=%d uuid=%q has NO conversion under the redesigned conversation.v1: "+
			"MessageEntry, ContextCut's only carrier, was deleted. Record stored unported", at.Offset, env.uuid)
	return UnportedEntry(at, "context_cleared", map[string]any{
		"uuid":               env.uuid,
		"__unported_because": "conversation.v1 MessageEntry (ContextCut's carrier) was deleted",
	})
}

// contextCompacted stores that history was replaced by a summary of itself,
// COALESCING the boundary with the summary line that follows it in the file.
//
// FILE ORDER, NEVER TIMESTAMP ORDER. The harness composes the summary before
// writing the boundary that announces it, so the summary's timestamp is EARLIER
// than the boundary's — a timestamp-ordered assembly pairs every boundary with
// the wrong summary in a session that compacted more than once. The coalescing
// is therefore kept even though the conversion is UNPORTED: it is the reason the
// deferral machinery in tail.Context exists, and dropping it would strand it.
func (c *Converter) contextCompacted(record map[string]any, at Attribution, env envelope, container string, next map[string]any) *storev1.StoreEntry {
	_ = container
	metadata := obj(record["compactMetadata"])
	summary := compactSummaryText(next)
	if summary == "" {
		// A compaction with no summary renders as a hole where the discarded
		// history was. It is emitted anyway — the cut is real and a reader must
		// see WHERE — but never silently.
		c.log.With(logging.Context{Operation: "context-compacted", Path: at.Path, VendorSessionID: at.SessionID, Level: "warn"}).
			Log("compact boundary uuid=%q at offset=%d is not followed by a summary line; the cut renders with nothing in place of the discarded history", env.uuid, at.Offset)
	}
	c.log.With(logging.Context{Operation: "context-compacted", Path: at.Path, VendorSessionID: at.SessionID, Level: "error"}).
		Log("compact boundary uuid=%q at offset=%d has NO conversion under the redesigned conversation.v1: "+
			"MessageEntry, ContextCut's only carrier, was deleted. Record stored unported", env.uuid, at.Offset)
	return UnportedEntry(at, "context_compacted", map[string]any{
		"uuid":               env.uuid,
		"tokens_before":      number(metadata["preTokens"]),
		"tokens_after":       number(metadata["postTokens"]),
		"summary":            summary,
		"__unported_because": "conversation.v1 MessageEntry (ContextCut's carrier) was deleted",
	})
}

// IsCompactBoundary reports whether a record is a compaction boundary, which is
// the only record in a transcript whose meaning depends on a line that may not
// be written yet.
func IsCompactBoundary(record map[string]any) bool {
	return str(record["type"]) == "system" && str(record["subtype"]) == "compact_boundary"
}

// compactSummaryText returns the summary carried by the line FOLLOWING a
// boundary, or "" when that line is not a summary.
//
// The summary line is typed `user`, which is why it is identified by the
// envelope's flag and never by its type: it is the harness's own text standing
// in for the discarded history, not the user's prompt.
func compactSummaryText(next map[string]any) string {
	if next == nil || str(next["type"]) != "user" || !boolean(next["isCompactSummary"]) {
		return ""
	}
	content := obj(next["message"])["content"]
	if s, ok := content.(string); ok {
		return s
	}
	var parts []string
	blocks, _ := content.([]any)
	for _, el := range blocks {
		block, ok := el.(map[string]any)
		if !ok || blockType(block) != "text" {
			continue
		}
		parts = append(parts, str(block["text"]))
	}
	return strings.Join(parts, "\n")
}

// ---------------------------------------------------------------------------
// clear detection
// ---------------------------------------------------------------------------

// clearCommand is the command name a cleared context is spelled with.
const clearCommand = "/clear"

// commandEnvelopeTags are the elements the harness expands a slash command
// into, in the order it writes them.
var commandEnvelopeTags = []string{"command-name", "command-message", "command-args"}

// isClearCommand reports whether a user message is `/clear` and nothing else.
//
// The harness NEVER writes the literal prompt "/clear" to the transcript. It
// writes the expanded command envelope, so anything matching raw prompt text
// against "/clear" misses every replayed or rehydrated session. Detection
// therefore unwraps the envelope first and then requires the command to be the
// only non-whitespace content left: an argument, or prose around the envelope,
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
// prompt whose entire content is "/clear" is recognized as one. Text carrying
// the envelope must consist of NOTHING but the envelope's elements: leftover
// non-whitespace outside them means the prompt merely QUOTES a command — a
// pasted transcript, a tool result echoing one — rather than invoking it.
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

// ---------------------------------------------------------------------------
// producer diagnostics
// ---------------------------------------------------------------------------

// ProducerDiagnostic states something about the READER rather than the read.
//
// UNPORTED. protocol.v1 BookkeepingEntry and its ProducerDiagnostic arm were
// deleted outright, and store.v1 StoreEntry has no bookkeeping arm at all —
// there is nowhere on the new contract for a producer to say something about
// itself. The diagnostic is still WRITTEN, whole, so a session's own log can
// still be reconstructed from stored data; it simply no longer has a typed home.
func ProducerDiagnostic(at Attribution, name, operation, detail string) *storev1.StoreEntry {
	return SyntheticUnportedEntry(at, name, "producer_diagnostic", map[string]any{
		"operation":          operation,
		"detail":             detail,
		"session_id":         at.SessionID,
		"path":               at.Path,
		"produced_at_ms":     float64(at.ProducedAtMs),
		"__unported_because": "protocol.v1 BookkeepingEntry/ProducerDiagnostic were deleted and store.v1 StoreEntry has no bookkeeping arm",
	})
}

// Describe renders why a record was stored unserved, for a log line that has to
// say what was not carried.
func Describe(entry *storev1.StoreEntry) string {
	item := entry.GetAgentUpdate().GetUnservedItem()
	switch arm := item.GetUnservedItem().(type) {
	case *storev1.StoreUnservedItem_VendorSpecific:
		return fmt.Sprintf("vendor_specific kind=%q", arm.VendorSpecific.GetKind())
	case *storev1.StoreUnservedItem_Unknown:
		return fmt.Sprintf("unknown discriminator=%q field=%q", arm.Unknown.GetDiscriminator(), arm.Unknown.GetDiscriminatorField())
	case *storev1.StoreUnservedItem_Unparsed:
		return fmt.Sprintf("unparsed offset=%d error=%q", arm.Unparsed.GetOffset(), arm.Unparsed.GetParseError())
	default:
		return ""
	}
}
