package convert

import (
	"crypto/sha256"
	"encoding/hex"
	"fmt"

	storev1 "agentrepl/proto/store/v1"
	"google.golang.org/protobuf/types/known/structpb"
)

// Producer is the fixed WriteBatchRequest producer identity for the sidecar.
const Producer = "shim-claude-sidecar"

// maxUnparsedRaw bounds the verbatim bytes a StoreUnparsed carries. A record we
// could not read is evidence, not a payload, and an unbounded copy of a corrupt
// multi-megabyte line would be written to the store on every re-read.
const maxUnparsedRaw = 64 << 10

// Attribution names where a record was read from and what it sits inside. It is
// everything the conversion needs that is NOT in the record itself.
//
// SessionID and ProducedAtMs NO LONGER REACH THE STORE. Their carrier was
// protocol.v1 ExternalEntry, which the redesigned contract deleted without a
// successor on store.v1 StoreEntry — so they survive here as attribution the
// sidecar logs by, and the gap is reported rather than papered over.
type Attribution struct {
	// SessionID is the conversation the record belongs to, taken from the file
	// PATH (the transcript IS the session's record) or from the launch that
	// announced a spool. Never invented.
	SessionID string
	// Path and Offset locate the record on disk. They are the write identity's
	// only inputs, which is what makes that identity survive a restart.
	Path   string
	Offset int64
	// Container is the detached-work message every record in this file sits
	// INSIDE. Empty for a session transcript, whose records are feed rows.
	//
	// It is derived from the task id rather than remembered across files
	// (DetachedWorkMessageID), so an agent sidechain read after a restart lands
	// under the same card as one read before it, with nothing to recover.
	Container string
	// ProducedAtMs is the producer's wall clock at observation.
	ProducedAtMs int64
}

// DetachedWorkMessageID is the message id of the feed row a detached task owns.
//
// IT IS A PURE FUNCTION OF THE TASK ID, and that is load-bearing. The card is
// opened by the transcript that announced the launch and updated by a spool, a
// sidechain and a staleness sweep — four producers in three files and two
// processes' lifetimes. Deriving the id lets each of them name the card without
// a correlation table, so nothing has to be recovered after a restart for a
// subagent's output to land on the work that spawned it.
func DetachedWorkMessageID(taskID string) string {
	if taskID == "" {
		return ""
	}
	return "dw:" + taskID
}

// writeID mints the STABLE write identity for one record: a digest of the file
// position it was read at plus the discriminator separating several records
// produced from that one position.
//
// DETERMINISTIC ON PURPOSE. StoreEntry.write_id must be minted once by the
// producer when the record is first handed to a store write, and never
// regenerated — not for a retry, not for a replay after the store bounced.
// A digest of the position satisfies that without any durable state: the same
// bytes at the same offset in the same file always mint the same id, so a
// replay after a restart is a no-op at the store rather than a duplicate.
func writeID(at Attribution, discriminator string) string {
	sum := sha256.Sum256([]byte(fmt.Sprintf("%s\x00%s\x00%d\x00%s", Producer, at.Path, at.Offset, discriminator)))
	return hex.EncodeToString(sum[:])
}

// syntheticWriteID mints the write identity for a record the sidecar INFERRED
// rather than read, which therefore has no file position to be named by. The
// caller supplies a name that is stable for the inference (a task's LOST
// verdict is one fact however many sweeps observe it).
func syntheticWriteID(name string) string {
	sum := sha256.Sum256([]byte(Producer + "\x00synthetic\x00" + name))
	return hex.EncodeToString(sum[:])
}

// filePlane is the observation plane of every record this process writes: the
// sidecar reads what the vendor wrote to disk.
func filePlane() *storev1.Plane {
	return &storev1.Plane{Plane: &storev1.Plane_File{File: &storev1.PlaneFile{}}}
}

// unserved wraps an unserved item as the StoreEntry the sidecar writes.
func unserved(writeID string, item *storev1.StoreUnservedItem) *storev1.StoreEntry {
	return &storev1.StoreEntry{
		Plane:   filePlane(),
		WriteId: writeID,
		Entry: &storev1.StoreEntry_AgentUpdate{AgentUpdate: &storev1.StoreAgentUpdate{
			AgentInfo: &storev1.StoreAgentUpdate_UnservedItem{UnservedItem: item},
		}},
	}
}

// VendorSpecificEntry stores a record we UNDERSTAND and have decided not to
// carry into a vendor-agnostic feed.
func VendorSpecificEntry(at Attribution, kind string, raw map[string]any) *storev1.StoreEntry {
	return unserved(writeID(at, "vendor:"+kind), &storev1.StoreUnservedItem{
		UnservedItem: &storev1.StoreUnservedItem_VendorSpecific{VendorSpecific: &storev1.StoreVendorSpecific{
			Kind: kind,
			Raw:  rawStruct(raw),
		}},
	})
}

// UnknownEntry stores a record we PARSED but do not MODEL.
func UnknownEntry(at Attribution, discriminator, discriminatorField string, raw map[string]any) *storev1.StoreEntry {
	return unserved(writeID(at, "unknown:"+discriminatorField+":"+discriminator), &storev1.StoreUnservedItem{
		UnservedItem: &storev1.StoreUnservedItem_Unknown{Unknown: &storev1.StoreUnknown{
			Discriminator:      discriminator,
			DiscriminatorField: discriminatorField,
			Raw:                rawStruct(raw),
		}},
	})
}

// UnportedField is the discriminator field UnportedEntry files a record under.
// It is deliberately unmistakable: nothing in the vendor's own JSON can produce
// it, so a query for it enumerates exactly the conversions this reconciliation
// left unwritten.
const UnportedField = "__unported_conversion"

// UnportedEntry stores a record whose CONVERSION no longer exists.
//
// The redesigned conversation.v1 deleted the whole MessageEntry record model
// (MessageEntry, AgentSaid, ToolCallBlock, ToolReturned, DetachedWork*,
// ContextCut, FailureRaised, SkillBodyResolved, MessageAuthor, MessageParent,
// StopReason) and replaced it with the Agent* protocol model (AgentFrame,
// AgentPrompt, AgentActivity) whose population from Claude's JSONL is a
// DESIGN DECISION, not a rename. Rather than invent that mapping, the record is
// stored WHOLE and the failure to convert it is stated in the data and logged.
//
// This preserves the total-ingestion mandate — the JSON object still reaches
// the store — while refusing to claim it was converted. Every caller is a
// conversion the redesign left unported; see the reconciliation report.
func UnportedEntry(at Attribution, conversion string, raw map[string]any) *storev1.StoreEntry {
	return unserved(writeID(at, "unported:"+conversion), &storev1.StoreUnservedItem{
		UnservedItem: &storev1.StoreUnservedItem_Unknown{Unknown: &storev1.StoreUnknown{
			Discriminator:      conversion,
			DiscriminatorField: UnportedField,
			Raw:                rawStruct(raw),
		}},
	})
}

// SyntheticUnportedEntry is UnportedEntry for a record the sidecar INFERRED
// rather than read, whose write identity therefore comes from the inference.
func SyntheticUnportedEntry(at Attribution, name, conversion string, raw map[string]any) *storev1.StoreEntry {
	return unserved(syntheticWriteID(name), &storev1.StoreUnservedItem{
		UnservedItem: &storev1.StoreUnservedItem_Unknown{Unknown: &storev1.StoreUnknown{
			Discriminator:      conversion,
			DiscriminatorField: UnportedField,
			Raw:                rawStruct(raw),
		}},
	})
}

// UnparsedEntry stores a record we could not READ at all — a failure rather
// than a gap.
func UnparsedEntry(at Attribution, raw []byte, cause error) *storev1.StoreEntry {
	if len(raw) > maxUnparsedRaw {
		raw = raw[:maxUnparsedRaw]
	}
	return unserved(writeID(at, "unparsed"), &storev1.StoreUnservedItem{
		UnservedItem: &storev1.StoreUnservedItem_Unparsed{Unparsed: &storev1.StoreUnparsed{
			Source:     at.Path,
			Offset:     uint64(at.Offset),
			ParseError: cause.Error(),
			Raw:        string(raw),
		}},
	})
}

// rawStruct converts a decoded JSON object into the Struct the unserved arms
// carry it in.
//
// A CONVERSION FAILURE IS NOT A DROP. structpb rejects values encoding/json
// cannot produce, so this cannot fail for a decoded JSONL object — but if it
// ever did, the record would still be stored, carrying the failure in place of
// the body rather than vanishing.
func rawStruct(raw map[string]any) *structpb.Struct {
	s, err := structpb.NewStruct(raw)
	if err != nil {
		fallback, fallbackErr := structpb.NewStruct(map[string]any{
			"__raw_struct_error": err.Error(),
		})
		if fallbackErr != nil {
			return nil
		}
		return fallback
	}
	return s
}
