// Package convert reads the Claude harness's on-disk JSON records and turns them
// into the store.v1 entries the sidecar writes.
//
// THE OUTCOMES A RECORD CAN HAVE, and nothing else happens to one:
//
//   - A PAGE LINE. The record is a conversation fact with a book: an agent's
//     frame, upserting its unit whole.
//   - A RUN FRAME. A detached shell run's delta or terminal, wrapped with the
//     spawning call's unit id. Never paginatable.
//   - AN UNSERVED ITEM. A keep-alive turn's item (no book), something one vendor
//     does that no vendor-agnostic feed can show (vendor_specific), a record we
//     parsed and do not model (unknown), or one we could not parse (unparsed).
//   - A DROP, for the EXEMPT SET alone: built-ins deliberately not carried. A
//     drop is never residue and never AgentUnmodeled.
//
// THE NO-VARIABLE-STATE PRINCIPLE BINDS THIS PACKAGE. Every join is a single
// indexed lookup: a tool return finds its call by tool_use_id, a skill document
// finds its call by sourceToolUseID, IDE diagnostics find their change by one
// remembered adjacency value. Nothing walks lineage and nothing accumulates a
// stack, a queue or a tree.
//
// INSTANTS COME FROM THE FILE, NEVER FROM A CLOCK HERE. Every started_at and
// settled_at is the record's own timestamp, so re-reading a file after a restart
// mints byte-identical frames under byte-identical write_ids, and the store
// absorbs the replay as a no-op.
package convert

import (
	"strings"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
)

// Observer receives the facts a conversion learns that OWNER RESOLUTION needs
// and the converter itself has no use for: which spool belongs to which call.
//
// A CALLBACK RATHER THAN A SHARED MAP, deliberately. Owner resolution lives in
// the root package (it is IO and policy); the converter is pure. One call per
// observation keeps the two packages from sharing mutable state across a
// boundary neither of them owns.
type Observer interface {
	// TaskSpawned reports a launch read off a tool result: the vendor task id,
	// the call that spawned it, the agent whose book the spawn happened in, and
	// the spool path the vendor named (empty when it named none).
	//
	// THE CREATED AGENT'S ID IS NOT A PARAMETER because it is not a separate
	// fact: a subagent's AgentId IS the spawning call's tool_use_id, so a reader
	// holding toolUseID already holds it.
	TaskSpawned(taskID, toolUseID, ownerAgentID, outputPath string)

	// TaskStopped reports a TaskStop result: a person stopped this task.
	//
	// THE TERMINAL IS NOT MINTED HERE. A cancelled shell run's terminal has to
	// carry the OUTPUT the run produced, and those bytes live in the spool —
	// which this converter is not the reader of. So the fact travels and the
	// reader mints the terminal through the spool's own handler, the same way a
	// LOST conclusion does.
	TaskStopped(taskID string)
}

// noopObserver is the default: a converter with nobody listening still converts.
type noopObserver struct{}

func (noopObserver) TaskSpawned(string, string, string, string) {}
func (noopObserver) TaskStopped(string)                         {}

// openCall is what a tool RETURN needs to settle its unit, remembered from the
// call. One entry per OPEN call, deleted the moment the call settles — the map
// is bounded by concurrent in-flight calls, never by transcript length.
type openCall struct {
	name       string
	input      map[string]any
	startedAt  int64
	activityID string
	agentID    string
}

// Converter holds the per-file correlation a conversion needs beyond the record
// in front of it. NOT safe for concurrent use; each tailer owns one.
type Converter struct {
	log      *logging.Bound
	observer Observer

	// openCalls maps a vendor tool_use_id to the call it names. A result carries
	// that id and nothing else, so this is the single lookup that lets a settled
	// frame restate the call it settles.
	openCalls map[string]openCall

	// openSkills maps a skill invocation's tool_use_id to its call, so the
	// DOCUMENT that lands afterwards settles the right unit. The join is the
	// vendor's own `sourceToolUseID` — direct and structural, never a skill-name
	// map matched against whatever arrives next.
	openSkills map[string]openCall

	// lastChangeUnit is the write/edit unit an IDE diagnostics attachment joins
	// to, and lastChangeWasEdit is which arm that unit carries. ONE REMEMBERED
	// PAIR, sanctioned at the schema: the vendor's diagnostics record carries no
	// tool-call id, so the join is by ADJACENCY.
	lastChangeUnit    string
	lastChangeWasEdit bool

	// currentMessageID and nextBlockOrdinal number an API response's content
	// blocks ACROSS THE SEVERAL TRANSCRIPT LINES that share one `message.id`.
	//
	// THE ORDINAL IS THE BLOCK'S POSITION IN THE API MESSAGE, not its position in
	// the line that happened to carry it: the vendor splits one response over
	// several lines, usually one block each. Counting per line would restart at
	// zero on every line and mint the same activity id for different blocks. This
	// ordinal equals the SDK's `content_block_start.index` the shim sees, so both
	// planes mint the SAME identity for the same block — which is the whole point
	// of a shared upsert-key space.
	//
	// ONE REMEMBERED COUNTER, reset when message.id changes. Nothing accumulates.
	currentMessageID string
	nextBlockOrdinal int

	// spawnedRuns maps a vendor task id to the tool_use_id of the call that
	// launched it. ONE ENTRY PER OPEN LAUNCH, written where the launch result is
	// read and consulted where a TaskStop result must name the run it cancelled.
	//
	// IT IS THE SAME FACT THE READER GETS THROUGH THE OBSERVER, kept here because
	// a TaskStop result arrives on THIS file's stream and must be converted from
	// what this file already said — the converter cannot ask the reader anything.
	spawnedRuns map[string]string

	// keepalive marks every record converted while a keep-alive turn is open.
	// ONE REMEMBERED BOOL per file, cleared by the next non-keepalive prompt.
	keepalive bool
}

// New builds a Converter with no observer installed.
func New(log *logging.Bound) *Converter {
	log.With(logging.Context{Operation: "convert-new"}).LogVerbose("constructing converter producer=%s", Producer)
	return &Converter{
		log:         log,
		observer:    noopObserver{},
		openCalls:   map[string]openCall{},
		openSkills:  map[string]openCall{},
		spawnedRuns: map[string]string{},
	}
}

// SetObserver installs the owner-resolution listener. A nil observer is refused
// rather than silently ignored: a caller that meant to listen and passed nil
// would otherwise lose every spool attribution with no signal.
func (c *Converter) SetObserver(o Observer) {
	if o == nil {
		panic("convert: SetObserver requires an observer; pass the no-op explicitly to opt out")
	}
	c.log.With(logging.Context{Operation: "convert-observer"}).LogVerbose("owner-resolution observer installed")
	c.observer = o
}

// KeepaliveMarker opens a prompt whose turn produces NOTHING servable.
//
// PENDING RULING, DEFAULT IMPLEMENTED (confirmed by the shim lead): the marker
// is this literal at the very start of the prompt's first text block.
const KeepaliveMarker = "<!--agent-repl:keepalive-->"

// Line converts one decoded transcript line into the entries it implies.
//
// `next` is the line that FOLLOWS this one IN THE FILE, or nil at the end of a
// batch. It exists for exactly one record: a compaction boundary, whose summary
// the harness writes as the following line.
//
// It NEVER returns zero entries for a line it was given, except for the EXEMPT
// SET, which is dropped deliberately and loudly. Total ingestion binds this
// package: irrelevance to a reader is a consumption-side judgment, never a
// reason to leave a record out of the database.
func (c *Converter) Line(record map[string]any, at Attribution, next map[string]any) []*storev1.StoreEntry {
	kind := str(record["type"])
	c.log.With(at.ctxFor("convert-line")).
		LogVerbose("converting line type=%q", kind)

	switch kind {
	case "":
		// It PARSED, so it is not unparsed; we simply cannot say what it is,
		// which is exactly what the unknown arm means.
		c.log.With(at.ctxWarn("convert-line")).
			Log("transcript line carries no %q field; stored as unknown residue with no path to a page", "type")
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
		if kind, withheld := withheldLineKind(kind); withheld {
			// CLI bookkeeping the harness writes about itself. We know exactly
			// what each one is and have decided not to carry it, which is a
			// different situation from not knowing.
			c.log.With(at.ctxFor("withhold")).
				LogVerbose("line type=%q withheld as vendor_specific", kind)
			return []*storev1.StoreEntry{VendorSpecificEntry(at, kind, record)}
		}
		c.log.With(at.ctxWarn("convert-line")).
			Log("transcript line type=%q is not modeled; stored as unknown residue", kind)
		return []*storev1.StoreEntry{UnknownEntry(at, kind, "type", record)}
	}
}

// withheldLineKind reports whether a top-level line type is CLI
// bookkeeping/machinery that must never become a feed row, and the kind string
// it is filed under.
//
// THE LIST IS WHAT SEPARATES "understood and not carried" FROM "not understood".
// Without it every unmodeled type would be filed as vendor_specific, which
// asserts we know what a brand-new line means — and the two arms exist precisely
// so the follow-up each needs is distinguishable: a converter for the first, a
// model for the second.
func withheldLineKind(kind string) (string, bool) {
	switch kind {
	case "mode", "permission-mode", "queue-operation", "last-prompt", "ai-title",
		"pr-link", "frame-link", "file-history-snapshot", "file-history-delta",
		"attribution-snapshot", "summary":
		return kind, true
	default:
		return kind, false
	}
}

// ---------------------------------------------------------------------------
// the record envelope
// ---------------------------------------------------------------------------

// envelope is the common header on user/assistant/system/attachment lines,
// reduced to the fields attribution and the joins actually need.
type envelope struct {
	uuid        string
	isMeta      bool
	isSummary   bool
	sourceTool  string
	timestampMs int64
}

func readEnvelope(obj map[string]any) envelope {
	return envelope{
		uuid:        str(obj["uuid"]),
		isMeta:      boolean(obj["isMeta"]),
		isSummary:   boolean(obj["isCompactSummary"]),
		sourceTool:  str(obj["sourceToolUseID"]),
		timestampMs: parseInstant(str(obj["timestamp"])),
	}
}

// frameAgent resolves WHOSE frame a record produces.
//
// THE READER'S IDENTITY WINS, ALWAYS. A record's own `agentId` is the vendor's
// LOCATOR for a sidechain file, not an AgentId: under the cross-plane minting
// rule a subagent's identity is the tool_use_id of the call that spawned it,
// which the reader read out of the agent's meta.json and put on the context.
// Reading the record field as an identity would file this agent's frames under
// a second, file-plane-only name that the stream plane never uses and no
// consumer could join to.
//
// The per-record `sessionId` is deliberately never consulted either: it diverges
// from the runtime's answer in ~22% of records and that divergence must not ride
// the wire.
func (c *Converter) frameAgent(at Attribution, env envelope) string {
	if at.AgentID != "" {
		return at.AgentID
	}
	return at.MainAgentID
}

// ---------------------------------------------------------------------------
// small readers over decoded JSON
// ---------------------------------------------------------------------------

func str(v any) string   { s, _ := v.(string); return s }
func boolean(v any) bool { b, _ := v.(bool); return b }

func obj(v any) map[string]any {
	m, _ := v.(map[string]any)
	return m
}

func list(v any) []any {
	l, _ := v.([]any)
	return l
}

func number(v any) float64 {
	f, _ := v.(float64)
	return f
}

// optionalString returns a pointer for a present, non-empty value and nil
// otherwise — PRESENCE, NEVER SENTINELS, at every optional proto field.
func optionalString(v any) *string {
	s := str(v)
	if s == "" {
		return nil
	}
	return &s
}

// optionalUint32 returns a pointer for a present numeric value.
func optionalUint32(o map[string]any, key string) *uint32 {
	raw, ok := o[key]
	if !ok || raw == nil {
		return nil
	}
	f, ok := raw.(float64)
	if !ok {
		return nil
	}
	v := uint32(f)
	return &v
}

// optionalInt64 returns a pointer for a present numeric value.
func optionalInt64(o map[string]any, key string) *int64 {
	raw, ok := o[key]
	if !ok || raw == nil {
		return nil
	}
	f, ok := raw.(float64)
	if !ok {
		return nil
	}
	v := int64(f)
	return &v
}

func firstNonEmpty(values ...string) string {
	for _, v := range values {
		if v != "" {
			return v
		}
	}
	return ""
}

// has reports whether an object carries a key, matching the vendor's camelCase
// and snake_case spellings of one name (the disk carries both).
func has(o map[string]any, key string) bool {
	if _, ok := o[key]; ok {
		return true
	}
	want := canon(key)
	for k := range o {
		if canon(k) == want {
			return true
		}
	}
	return false
}

// pick reads a value under any of the vendor's spellings of one name.
func pick(o map[string]any, keys ...string) any {
	for _, key := range keys {
		if v, ok := o[key]; ok {
			return v
		}
	}
	for _, key := range keys {
		want := canon(key)
		for k, v := range o {
			if canon(k) == want {
				return v
			}
		}
	}
	return nil
}

// canon folds a field name to its case- and separator-insensitive form, so the
// disk's `toolUseID`, `tool_use_id` and `toolUseId` all collide.
func canon(s string) string {
	var b strings.Builder
	for i := 0; i < len(s); i++ {
		ch := s[i]
		switch {
		case ch == '_' || ch == '-':
			continue
		case ch >= 'A' && ch <= 'Z':
			b.WriteByte(ch + ('a' - 'A'))
		default:
			b.WriteByte(ch)
		}
	}
	return b.String()
}
