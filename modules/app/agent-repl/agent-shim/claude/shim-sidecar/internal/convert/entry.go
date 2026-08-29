package convert

// entry.go — THE STORE ENVELOPE. Every StoreEntry the sidecar writes is built
// here, so the four envelope duties (plane, write_id, upsert_key, agent_info
// arm) are discharged in exactly one place and cannot drift per record kind.

import (
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"strconv"

	"agentrepl/shim-claude-sidecar/internal/logging"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"google.golang.org/protobuf/types/known/structpb"
)

// Producer is the fixed WriteBatch producer identity for the sidecar.
const Producer = "shim-claude-sidecar"

// maxUnparsedRaw bounds the verbatim bytes a StoreUnparsed carries. A record we
// could not read is EVIDENCE, not a payload, and an unbounded copy of a corrupt
// multi-megabyte line would be re-written to the store on every re-read.
const maxUnparsedRaw = 64 << 10

// Attribution names where a record was read from and whose book it lands in. It
// is everything a conversion needs that is NOT in the record itself.
//
// INSTANTS ARE NEVER TAKEN FROM HERE. Every started_at/settled_at comes from the
// file record's own timestamp, so a re-read after a restart mints the identical
// frame and the identical write_id. There is deliberately no producer clock on
// this struct.
type Attribution struct {
	// VendorSessionID is the transcript FILE's session uuid — the basename of
	// `<session>.jsonl`, never the per-record `sessionId` field, which diverges
	// from the runtime's answer in ~22% of records.
	VendorSessionID string

	// MainAgentID is the AgentId.value of the session's main agent (R9 default:
	// the file's session uuid). Every sidechain record in this session resolves
	// its top_level to this unless the spawn was backgrounded.
	MainAgentID string

	// AgentID is the agent whose book this file's records belong to: the main
	// agent for a session transcript, the vendor `agentId` for a subagent
	// transcript. Empty means the file itself does not name one and the record
	// must.
	AgentID string

	// Backgrounded reports that this file's agent was spawned into the
	// background, which makes the agent its OWN top_level rather than the
	// session's main agent.
	Backgrounded bool

	// Path and Offset locate the record on disk. They are the write identity's
	// only inputs besides the discriminator, which is what makes that identity
	// survive a restart unchanged.
	Path   string
	Offset int64

	// FileID is the tailed file's stable "dev:inode" identity, so a log record
	// names the file the same way the cursor does even across a rename.
	FileID string

	// TaskID is the vendor task id of a spool file (b*/a*/w*), empty for a
	// transcript. It never crosses the contract; it is logging and owner
	// resolution only.
	TaskID string
}

// ctxFor is the CORRELATION BASE every log record in this package starts from.
//
// THE KEYS LIVE IN FIELDS, NEVER IN MESSAGE TEXT. A record whose identifiers are
// interpolated into a sentence cannot be filtered, joined or aggregated by the
// integration loop that reads these logs — so producer, path, file id, offset,
// agent and task all ride dedicated keys here, and a call site adds only what is
// specific to its branch (activity id, upsert key, write id).
func (at Attribution) ctxFor(operation string) logging.Context {
	return logging.Context{
		Operation:       operation,
		Producer:        Producer,
		Path:            at.Path,
		FileID:          at.FileID,
		Offset:          logging.Off(at.Offset),
		AgentID:         at.AgentID,
		VendorSessionID: at.VendorSessionID,
		TaskID:          at.TaskID,
	}
}

// ctxWarn is ctxFor at warning level: degraded, unexpected, but handled.
func (at Attribution) ctxWarn(operation string) logging.Context {
	c := at.ctxFor(operation)
	c.Level = "warn"
	return c
}

// ctxError is ctxFor at error level: a failure or an invariant violation.
func (at Attribution) ctxError(operation string) logging.Context {
	c := at.ctxFor(operation)
	c.Level = "error"
	return c
}

// writeID mints the STABLE write identity for one record.
//
// DETERMINISTIC ON PURPOSE, and the exact ruled recipe: hex sha256 of
// "shim-claude-sidecar|" + path + "|" + offset + "|" + discriminator. The
// discriminator separates the several entries one record can mint (a block
// index, "terminal", "diag"). Randomness is forbidden — replay idempotence at
// the store rests entirely on the same bytes minting the same id.
func writeID(at Attribution, discriminator string) string {
	sum := sha256.Sum256([]byte(Producer + "|" + at.Path + "|" + strconv.FormatInt(at.Offset, 10) + "|" + discriminator))
	return hex.EncodeToString(sum[:])
}

// WriteID exposes the write-identity recipe so tests can assert determinism
// against the rule rather than against a recorded digest.
func WriteID(at Attribution, discriminator string) string { return writeID(at, discriminator) }

// filePlane is the observation plane of every record this process writes.
func filePlane() *storev1.Plane {
	return &storev1.Plane{Plane: &storev1.Plane_File{File: &storev1.PlaneFile{}}}
}

// agentID wraps a raw identifier as the typed identity, or nil for an empty one
// so an unresolvable attribution stays UNSET rather than becoming a sentinel.
func agentID(value string) *conversationv1.AgentId {
	if value == "" {
		return nil
	}
	return &conversationv1.AgentId{Value: value}
}

// activityID wraps a unit identity.
func activityID(value string) *conversationv1.AgentActivityId {
	return &conversationv1.AgentActivityId{Value: value}
}

// topLevel resolves the nearest non-sync ancestor for a record read from this
// file.
//
// UNSET ONLY WHEN GENUINELY UNRESOLVABLE, which is residue that names no agent
// at all. A backgrounded subagent is its OWN top_level because its stream
// outlives the turn that spawned it; every other subagent's work belongs to the
// session's main agent.
func topLevel(at Attribution, frameAgent string) *conversationv1.AgentId {
	if at.Backgrounded && frameAgent != "" {
		return agentID(frameAgent)
	}
	if at.MainAgentID != "" {
		return agentID(at.MainAgentID)
	}
	return agentID(frameAgent)
}

// entry assembles the envelope around one agent_info arm.
func entry(at Attribution, discriminator, upsertKey string, frameAgent string, info isAgentInfo) *storev1.StoreEntry {
	update := &storev1.StoreAgentUpdate{TopLevel: topLevel(at, frameAgent)}
	info(update)
	return &storev1.StoreEntry{
		Plane:     filePlane(),
		WriteId:   writeID(at, discriminator),
		UpsertKey: upsertKey,
		Entry:     &storev1.StoreEntry_AgentUpdate{AgentUpdate: update},
	}
}

// isAgentInfo sets exactly one agent_info arm on the update.
type isAgentInfo func(*storev1.StoreAgentUpdate)

// PageLine stores a servable frame as a line in `pageAgent`'s book.
//
// THE PRODUCER DECIDES PAGEABILITY, so this is the only constructor that can
// produce a page line and every caller must name the book explicitly.
func PageLine(at Attribution, discriminator, upsertKey, pageAgent string, frame *conversationv1.AgentFrame) *storev1.StoreEntry {
	return entry(at, discriminator, upsertKey, pageAgent, func(u *storev1.StoreAgentUpdate) {
		u.AgentInfo = &storev1.StoreAgentUpdate_ServeableFrame{ServeableFrame: &storev1.StorePageLine{
			PageAgentId: agentID(pageAgent),
			AgentItem: &storev1.StoreAgentItem{
				Item: &storev1.StoreAgentItem_AgentFrame{AgentFrame: frame},
			},
		}}
	})
}

// BashRun stores a detached shell run's frame, wrapped with the unit id the
// spawning stream announced so a reader holding the announcement resolves it.
// NOT paginatable by construction.
func BashRun(at Attribution, discriminator, upsertKey, run string, frame *conversationv1.AgentBash) *storev1.StoreEntry {
	return entry(at, discriminator, upsertKey, at.AgentID, func(u *storev1.StoreAgentUpdate) {
		u.AgentInfo = &storev1.StoreAgentUpdate_Bash{Bash: &storev1.StoreAgentBash{
			Run:   activityID(run),
			Frame: frame,
		}}
	})
}

// Keepalive stores a well-formed conversation fact that has NO book: the turn
// was marked keep-alive, so nothing it produced may ever reach a page.
func Keepalive(at Attribution, discriminator, upsertKey string, frameAgent string, item *storev1.StoreAgentItem) *storev1.StoreEntry {
	return unservedEntry(at, discriminator, upsertKey, frameAgent, &storev1.StoreUnservedItem{
		UnservedItem: &storev1.StoreUnservedItem_Keepalive{Keepalive: item},
	})
}

// VendorSpecificEntry stores a record we UNDERSTAND and have decided not to
// carry into a vendor-agnostic feed. The follow-up it asks for is a CONVERTER.
func VendorSpecificEntry(at Attribution, kind string, raw map[string]any) *storev1.StoreEntry {
	return unservedEntry(at, "vendor:"+kind, "", at.AgentID, &storev1.StoreUnservedItem{
		UnservedItem: &storev1.StoreUnservedItem_VendorSpecific{VendorSpecific: &storev1.StoreVendorSpecific{
			Kind: kind,
			Raw:  rawStruct(raw),
		}},
	})
}

// UnknownEntry stores a record we PARSED but do not MODEL. The follow-up it asks
// for is a MODEL, which is why it is a different arm from vendor_specific.
//
// A RECOGNIZABLE MODELED KIND REACHING HERE IS A PRODUCER DEFECT — the
// golden-corpus test asserts the set of discriminators reaching this arm is
// EMPTY, so a regression in any mapping fails the suite rather than degrading
// quietly into residue.
func UnknownEntry(at Attribution, discriminator, discriminatorField string, raw map[string]any) *storev1.StoreEntry {
	return unservedEntry(at, "unknown:"+discriminatorField+":"+discriminator, "", at.AgentID, &storev1.StoreUnservedItem{
		UnservedItem: &storev1.StoreUnservedItem_Unknown{Unknown: &storev1.StoreUnknown{
			Discriminator:      discriminator,
			DiscriminatorField: discriminatorField,
			Raw:                rawStruct(raw),
		}},
	})
}

// UnparsedEntry stores a record we could not READ at all — a FAILURE rather than
// a gap, carrying enough evidence to be investigated rather than merely counted.
func UnparsedEntry(at Attribution, raw []byte, cause error) *storev1.StoreEntry {
	if len(raw) > maxUnparsedRaw {
		raw = raw[:maxUnparsedRaw]
	}
	return unservedEntry(at, "unparsed", "", at.AgentID, &storev1.StoreUnservedItem{
		UnservedItem: &storev1.StoreUnservedItem_Unparsed{Unparsed: &storev1.StoreUnparsed{
			Source:     at.Path,
			Offset:     uint64(at.Offset),
			ParseError: cause.Error(),
			Raw:        string(raw),
		}},
	})
}

// unservedEntry wraps an unserved arm. Residue with no upsert key of its own is
// keyed by its write identity, so a re-read supersedes its own row rather than
// appending a second copy of the same bytes.
func unservedEntry(at Attribution, discriminator, upsertKey string, frameAgent string, item *storev1.StoreUnservedItem) *storev1.StoreEntry {
	if upsertKey == "" {
		upsertKey = "residue:" + writeID(at, discriminator)
	}
	return entry(at, discriminator, upsertKey, frameAgent, func(u *storev1.StoreAgentUpdate) {
		u.AgentInfo = &storev1.StoreAgentUpdate_UnservedItem{UnservedItem: item}
	})
}

// rawStruct converts a decoded JSON object into the Struct the residue arms
// carry it in.
//
// A CONVERSION FAILURE IS NOT A DROP. structpb rejects values encoding/json
// cannot produce, so this cannot fail for a decoded JSONL object — but if it
// ever did, the record would still be stored, carrying the failure in place of
// the body rather than vanishing.
func rawStruct(raw map[string]any) *structpb.Struct {
	s, err := structpb.NewStruct(raw)
	if err == nil {
		return s
	}
	fallback, fallbackErr := structpb.NewStruct(map[string]any{"__raw_struct_error": err.Error()})
	if fallbackErr != nil {
		return nil
	}
	return fallback
}

// Describe renders why a record was stored unserved, for a log line that has to
// say what was not carried.
func Describe(e *storev1.StoreEntry) string {
	item := e.GetAgentUpdate().GetUnservedItem()
	switch arm := item.GetUnservedItem().(type) {
	case *storev1.StoreUnservedItem_Keepalive:
		return "keepalive"
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
