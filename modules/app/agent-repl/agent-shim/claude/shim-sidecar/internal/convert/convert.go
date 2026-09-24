// Package convert reads the Claude harness's on-disk JSON records and turns them
// into the store.v1 entries the sidecar writes.
//
// THE OUTCOMES A RECORD CAN HAVE, and nothing else happens to one:
//
//   - A PAGE LINE. The record is a conversation fact with a book: an agent's
//     frame, upserting its unit whole.
//   - A RUN FRAME. A detached shell run's delta or terminal, wrapped with the
//     spawning call's unit id. Never paginatable.
//   - AN UNSERVED ITEM. Something one vendor does that no vendor-agnostic feed
//     can show (vendor_specific), a record we parsed and do not model
//     (unknown), or one we could not parse (unparsed).
//   - A DROP, in three named cases and no others: the EXEMPT SET (built-ins
//     deliberately not carried; a drop there is never residue and never
//     AgentUnmodeled), the NEVER-PERSISTED RESIDUE KINDS (neverpersist.go),
//     which are classified exactly as before and then not written, and a
//     KEEP-ALIVE TURN'S RECORDS (keepalive.go), converted and then not stored.
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

	conversationv1 "agentrepl/proto/conversation/v1"
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
	// the call that spawned it, the agent whose book the spawn happened in, the
	// spool path the vendor named (empty when it named none), and whether the
	// spawn went to the BACKGROUND.
	//
	// THE CREATED AGENT'S ID IS NOT A PARAMETER because it is not a separate
	// fact: a subagent's AgentId IS the spawning call's tool_use_id, so a reader
	// holding toolUseID already holds it.
	//
	// `backgrounded` IS reported, because nothing downstream can derive it. It
	// is read off the launch result's own signature (an `isAsync` launch, and
	// only that one), and it is what decides `top_level`: a backgrounded
	// subagent's stream outlives the spawning turn, so the subagent is its own
	// top level rather than the session's main agent.
	TaskSpawned(taskID, toolUseID, ownerAgentID, outputPath string, backgrounded bool)

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

func (noopObserver) TaskSpawned(string, string, string, string, bool) {}
func (noopObserver) TaskStopped(string)                               {}

// openCall is what a tool RETURN needs to settle its unit, remembered from the
// call. One entry per OPEN call, deleted the moment the call settles — the map
// is bounded by concurrent in-flight calls, never by transcript length.
type openCall struct {
	name       string
	input      map[string]any
	startedAt  int64
	activityID string
	agentID    string
	// retainedAllowedTools carries what a skill's ACKNOWLEDGEMENT declared, kept
	// for the document record that actually settles the unit. The allowances
	// ride the acknowledgement and nothing else — the call's input names only
	// the skill and its args, and the document states no allowances of its own.
	// NIL means the acknowledgement declared none, which the proto distinguishes
	// from an empty declared set.
	retainedAllowedTools *conversationv1.AgentSkillAllowedTools

	// inherited marks a call that this transcript merely QUOTES from a parent —
	// a tool_use block of a fork's copied context. Its producer already booked
	// the settled unit under the producing agent, so this reader must not
	// settle it a second time under its own book (which the store would refuse
	// as a book move). The result is kept as residue instead; see settle.go.
	inherited bool
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

	// foreignSpawns maps a vendor task id to the SESSION that launched it, for
	// the runs whose launch was written to a different transcript than this one
	// (foreignspawn.go). ONE ENTRY PER OBSERVED FOREIGN NOTIFICATION.
	//
	// IT IS THE THIRD ANSWER TO "WHY IS THERE NO LAUNCH". Beside "there is a
	// real gap" and "the launch predates my window" stands "the launch was
	// never in this file at all", which is what a background agent surviving a
	// `/clear` looks like from here.
	foreignSpawns map[string]string

	// keepaliveScope is which of this file's records a keep-alive turn
	// produced, by the transcript's own prompt and parent links
	// (keepalive.go). None of them is ever stored.
	keepaliveScope keepaliveScope

	// joined records WHERE IN THE FILE this converter started reading, and
	// whether it has started at all. ONE OFFSET, written once.
	//
	// IT IS THE DIFFERENCE BETWEEN "NOT THERE" AND "BEFORE MY TIME". Everything
	// this converter correlates — an open call, a launched run — it learned from
	// a line it read itself, because a converter cannot ask the reader anything.
	// So a correlation that comes up empty means one of two entirely different
	// things, and only this offset separates them: a converter that read the file
	// from byte 0 and still has no launch for a task is looking at a real gap,
	// while one that resumed at the cursor the store held is simply looking
	// before its own window. Without it, every restart on a large transcript
	// reported the second as the first.
	joined       bool
	joinedOffset int64

	// pendingCut is a compaction drawn with the placeholder because its summary
	// was not the line after the boundary, kept until the summary that names
	// that boundary arrives (contextcut.go). Nil whenever no cut is waiting.
	pendingCut *pendingCompaction
}

// New builds a Converter with no observer installed.
func New(log *logging.Bound) *Converter {
	log.With(logging.Context{Operation: "convert-new"}).LogVerbose("constructing converter producer=%s", Producer)
	return &Converter{
		log:            log,
		observer:       noopObserver{},
		openCalls:      map[string]openCall{},
		openSkills:     map[string]openCall{},
		spawnedRuns:    map[string]string{},
		foreignSpawns:  map[string]string{},
		keepaliveScope: newKeepaliveScope(),
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

// Line converts one decoded transcript line into the entries it implies.
//
// `next` is the line that FOLLOWS this one IN THE FILE, or nil at the end of a
// batch. It exists for exactly one record: a compaction boundary, whose summary
// the harness writes as the following line.
//
// It NEVER returns zero entries for a line it was given, except for the EXEMPT
// SET, the NEVER-PERSISTED RESIDUE KINDS and a KEEP-ALIVE TURN'S RECORDS, all
// of which are dropped deliberately and loudly. Total ingestion still binds the
// READ: every line is decoded and classified, and irrelevance to a reader is
// never a reason to stop reading one — only, for the ruled kinds, a reason not
// to store it.
func (c *Converter) Line(record map[string]any, at Attribution, next map[string]any) []*storev1.StoreEntry {
	// SET ONCE, HERE, FOR THE WHOLE RECORD. Attribution travels by value, so
	// every conversion this record fans out to carries the vendor's uuid without
	// thirty call sites having to pass it — and residue minted anywhere in that
	// fan-out keys on the same record the other plane keys on.
	at.RecordUUID = str(record["uuid"])
	// THE KEEP-ALIVE QUESTION IS ASKED ONCE PER RECORD, BEFORE ANYTHING IS
	// CONVERTED, and answered by the record's own links (keepalive.go). A
	// per-record answer is what makes the skip exact: every entry this record
	// fans out to — a unit, a settle, a prompt, residue — is withheld together,
	// whichever branch minted it.
	keepalive := c.keepaliveScope.classify(keepaliveFactsOf(record))
	// THE LINE IS CLASSIFIED IN FULL, and the READER decides what is written:
	// residue is withheld at the sidecar's single write path (neverpersist.go,
	// cycle.go withholdResidue), so every branch here still runs and still
	// states what the vendor recorded. A keep-alive's record is converted too,
	// so the joins it opens or settles stay exactly as warm as any other's.
	entries := c.lineEntries(record, at, next)
	if !keepalive {
		return entries
	}
	// IT ANNOUNCES NO ROW, so it carries no upsert_key: nothing was stored.
	c.log.With(at.ctxFor("keepalive-skip")).LogVerbose(
		"a keep-alive turn's %s record is never stored; converted to %d entrie(s), stored none",
		str(record["type"]), len(entries))
	return nil
}

// lineEntries is the conversion itself.
func (c *Converter) lineEntries(record map[string]any, at Attribution, next map[string]any) []*storev1.StoreEntry {
	if !c.joined {
		c.joined = true
		c.joinedOffset = at.Offset
	}
	// WHERE A RUN'S LAUNCH LIVES IS LEARNED BEFORE IT IS NEEDED. The record that
	// says so is a notification about the run, which the vendor writes long
	// before any stop; reading it here — on every line, ahead of the type
	// switch — is what makes the fact available to the stop when it arrives.
	c.noteForeignSpawn(record, at)
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
		// BENIGN FORWARD-COMPAT — debug, not warn. A line whose type is present
		// but not yet modelled is stored WHOLE as residue: that residue IS the
		// coverage, and it re-converts the day the type is modelled. A vendor
		// adding a new line type is expected, not a fault, so a cold re-scan
		// must not flood the strict harvest with one warn per such line. The
		// residue record is unchanged; only the severity drops. (A line missing
		// its type discriminator entirely stays warn above — that is malformed,
		// not merely unmodelled.)
		c.log.With(at.ctxFor("convert-line")).
			LogVerbose("transcript line type=%q is not modeled; stored as unknown residue", kind)
		return []*storev1.StoreEntry{UnknownEntry(at, kind, "type", record)}
	}
}

// resumedMidFile reports that this converter started reading the file somewhere
// other than its beginning, so a correlation it never saw may simply predate its
// window rather than be missing.
func (c *Converter) resumedMidFile() bool { return c.joined && c.joinedOffset > 0 }

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
	// attributionAgent is the TYPE of the agent that actually produced this
	// record, which the vendor stamps on every sidechain assistant record and
	// PRESERVES across a fork's copy of the parent's conversation. It is how a
	// quoted record is told from a produced one; empty on records the vendor
	// does not attribute (a session transcript's, a non-sidechain's).
	attributionAgent string
	// originKind is the vendor's `origin.kind` — "peer" for a message another
	// Claude session sent in (an inter-session peer, or a subagent hand-back
	// with `origin.handback` set). Empty on records with no origin.
	originKind string
	// peerSender is the sender label of a peer message: `origin.from`, or
	// `origin.senderTaskId` when `from` is absent. Empty on non-peer records.
	peerSender string
	// peerBody is the vendor-stated body of a peer message (`origin.body`).
	// Empty when the vendor states none, in which case the record's own text is
	// the body instead.
	peerBody string
}

func readEnvelope(rec map[string]any) envelope {
	origin := obj(rec["origin"])
	sender := str(origin["from"])
	if sender == "" {
		sender = str(origin["senderTaskId"])
	}
	return envelope{
		uuid:             str(rec["uuid"]),
		isMeta:           boolean(rec["isMeta"]),
		isSummary:        boolean(rec["isCompactSummary"]),
		sourceTool:       str(rec["sourceToolUseID"]),
		timestampMs:      parseInstant(str(rec["timestamp"])),
		attributionAgent: str(rec["attributionAgent"]),
		originKind:       str(origin["kind"]),
		peerSender:       sender,
		peerBody:         str(origin["body"]),
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
