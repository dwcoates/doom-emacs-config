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
	WorkspaceDir    string
	WorkspaceID     string
	ClaudeSessionID string
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

	// AgentType is the subagent TYPE this file's agent IS ("general-purpose", a
	// plugin name, "fork", ...), read from the companion meta file; EMPTY for a
	// session transcript, which has none.
	//
	// IT DECIDES WHETHER AN ASSISTANT RECORD WAS PRODUCED HERE OR MERELY QUOTED.
	// A fork transcript copies the parent's conversation ahead of its own work,
	// and the vendor keeps each copied record's true producer in
	// `attributionAgent` while stamping the fork's OWN records with the fork's
	// type. A record whose `attributionAgent` names a DIFFERENT type is inherited
	// context, already booked under its producer, and re-booking it under this
	// agent would ask the store to move a row between books (assistant.go). It is
	// a TYPE, never an identity, so it is never a book and never reaches the wire.
	AgentType string

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

	// RecordUUID is the vendor's own uuid for the record being converted, set
	// once per record at the converter's entry point.
	//
	// IT IS THE RESIDUE KEY, AND THAT IS WHY IT EXISTS. Both planes see the same
	// vendor record and both may store it as residue; keying it by the vendor's
	// uuid is what makes the two writes collapse onto ONE row instead of
	// standing beside each other as two copies of one unconvertible line.
	// Empty for a record with no uuid — an unparsed line, a spool's bytes —
	// which falls back to the file coordinates.
	RecordUUID string

	// TaskID is the vendor task id of a spool file (b*/a*/w*), empty for a
	// transcript. It never crosses the contract; it is logging and owner
	// resolution only.
	TaskID string

	// WriteScope is an alternative identity for the write, used when the record
	// has NO FILE POSITION at all.
	//
	// A TERMINAL THE READER INFERRED IS THE ONLY SUCH RECORD. A run swept up at
	// boot, or one whose spool was never readable, is concluded from the
	// ABSENCE of a file rather than from a byte in one — so there is no file id
	// and no offset to digest, and every such terminal would otherwise share
	// one write identity and absorb its siblings at the store. Its identity is
	// therefore scoped to the RUN, which is unique by construction and is
	// already what its upsert key names.
	//
	// It is deliberately NOT a fallback for a record that was read off a file:
	// FileID wins whenever it is set, and a record with neither is a defect.
	WriteScope string
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
		WorkspaceDir:    at.WorkspaceDir,
		WorkspaceID:     at.WorkspaceID,
		ClaudeSessionID: at.ClaudeSessionID,
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
// DETERMINISTIC ON PURPOSE, and the exact ruled recipe (R-S1) with the
// conversion version in it: hex sha256 of "shim-claude-sidecar|v<N>|" +
// FILE_ID + "|" + offset + "|" + discriminator. THE VERSION IS PART OF THE
// IDENTITY (versionTag): the same bytes re-read under a new conversion are a
// new write the ledger does not absorb, so their re-derived content lands. The
// discriminator separates the several entries one record can mint (a block
// index, "terminal", "diag"). Randomness is forbidden — replay idempotence at
// the store rests entirely on the same bytes minting the same id.
//
// IT DIGESTS THE FILE ID, NEVER THE PATH, and the two are not
// interchangeable. The cursor is keyed by "dev:inode", so a RENAMED file keeps
// its cursor and is resumed from it — but a path-derived write identity would
// mint a brand new id for every record replayed after that rename, and the
// store's absorption (a replayed batch whose write_ids all landed before is the
// SUCCESS arm) would silently store the whole re-read turn a second time. The
// cursor's identity and the write's identity have to be the same identity, or
// rename-proof resumption and replay absorption contradict each other.
//
// A FILE WITH NO ID IS A READER DEFECT, and it is stated as one rather than
// quietly digesting an empty string, which would collapse every file's records
// onto one identity space keyed only by offset.
func writeID(at Attribution, discriminator string) string {
	if at.FileID != "" {
		sum := sha256.Sum256([]byte(Producer + "|" + versionTag + "|" + at.FileID + "|" + strconv.FormatInt(at.Offset, 10) + "|" + discriminator))
		return hex.EncodeToString(sum[:])
	}
	if at.WriteScope != "" {
		// A record with no file position — an inferred terminal — is identified
		// by its run instead. The offset is deliberately absent rather than
		// zero: there is no position, and digesting one would claim there was.
		sum := sha256.Sum256([]byte(Producer + "|" + versionTag + "|" + at.WriteScope + "|" + discriminator))
		return hex.EncodeToString(sum[:])
	}
	panic("convert: write_id requires either the file's dev:inode identity or a run-scoped WriteScope; the reader supplied neither")
}

// RunScope spells the WriteScope of a record inferred ABOUT a run rather than
// read out of a file, so the spelling lives in one place.
func RunScope(run string) string { return "run:" + run }

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
			Book: &storev1.StorePageLine_PageAgentId{PageAgentId: agentID(pageAgent)},
			AgentItem: &storev1.StoreAgentItem{
				Item: &storev1.StoreAgentItem_AgentFrame{AgentFrame: frame},
			},
		}}
	})
}

// PromptLine stores a served PROMPT as a line in `pageAgent`'s book — the same
// StorePageLine envelope PageLine builds, carrying the AgentPrompt arm of the
// item oneof rather than the AgentFrame arm.
//
// THE STORE ALREADY SERVES THIS. route.go routes a page line whose item is an
// agent_prompt (validating the recipient and that the book matches it), the shim
// reader maps it to HistoryEntry.userPrompt, and the daemon draws it as a user
// prompt bubble — so a file-plane prompt reaches the feed by exactly the path a
// stream-plane prompt does. It is used only for an ADOPTED external transcript,
// whose prompts were never submitted through agent-repl and so were never minted
// or drawn by the daemon; agent-repl's own prompts are still withheld (R15).
func PromptLine(at Attribution, discriminator, upsertKey, pageAgent string, prompt *conversationv1.AgentPrompt) *storev1.StoreEntry {
	return entry(at, discriminator, upsertKey, pageAgent, func(u *storev1.StoreAgentUpdate) {
		u.AgentInfo = &storev1.StoreAgentUpdate_ServeableFrame{ServeableFrame: &storev1.StorePageLine{
			Book: &storev1.StorePageLine_PageAgentId{PageAgentId: agentID(pageAgent)},
			AgentItem: &storev1.StoreAgentItem{
				Item: &storev1.StoreAgentItem_AgentPrompt{AgentPrompt: prompt},
			},
		}}
	})
}

// PeerLine stores a served PEER MESSAGE as a line in `pageAgent`'s book — the
// same StorePageLine envelope PromptLine builds, carrying the PeerMessage arm of
// the item oneof.
//
// THE STORE SERVES IT BY THE SAME PATH A PROMPT TAKES. route.go routes a page
// line whose item is a peer_message (validating the recipient and the book
// match), the shim reader maps it to HistoryEntry.peerMessage, and the daemon
// draws it as the abbreviated peer bubble. Used for an ADOPTED transcript's peer
// messages, which the live stream would emit itself on a running session; the
// two planes carry the SAME vendor record uuid so their rows collapse to one.
func PeerLine(at Attribution, discriminator, upsertKey, pageAgent string, peer *conversationv1.PeerMessage) *storev1.StoreEntry {
	return entry(at, discriminator, upsertKey, pageAgent, func(u *storev1.StoreAgentUpdate) {
		u.AgentInfo = &storev1.StoreAgentUpdate_ServeableFrame{ServeableFrame: &storev1.StorePageLine{
			Book: &storev1.StorePageLine_PageAgentId{PageAgentId: agentID(pageAgent)},
			AgentItem: &storev1.StoreAgentItem{
				Item: &storev1.StoreAgentItem_PeerMessage{PeerMessage: peer},
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
		upsertKey = ResidueKey(at)
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
