package convert

import (
	"crypto/sha256"
	"encoding/hex"
	"fmt"

	agentshimv1 "agentrepl/proto/agentshim/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	protocolv1 "agentrepl/proto/protocol/v1"
	"google.golang.org/protobuf/types/known/structpb"
)

// Producer is the fixed StoreEntryWrite producer identity for the sidecar.
const Producer = "shim-claude-sidecar"

// maxUnparsedRaw bounds the verbatim bytes an UnparsedEntry carries. A record
// we could not read is evidence, not a payload, and an unbounded copy of a
// corrupt multi-megabyte line would be written to the store on every re-read.
const maxUnparsedRaw = 64 << 10

// Attribution names where a record was read from and what it sits inside. It is
// everything the conversion needs that is NOT in the record itself.
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
// DETERMINISTIC ON PURPOSE. InternalEntry.write_id must be "minted once by the
// producer when the record is first handed to a store write, and never
// regenerated — not for a retry, not for a replay after the store bounced".
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
func filePlane() *agentshimv1.Plane {
	return &agentshimv1.Plane{Plane: &agentshimv1.Plane_File{File: &agentshimv1.PlaneFile{}}}
}

// external builds the half of a record that may leave the shim.
func external(at Attribution) *protocolv1.ExternalEntry {
	return &protocolv1.ExternalEntry{SessionId: at.SessionID, ProducedAtMs: at.ProducedAtMs}
}

// MessageEntry wraps a converted conversation record as a stored Entry.
func MessageEntry(at Attribution, discriminator string, m *conversationv1.MessageEntry) *agentshimv1.Entry {
	ext := external(at)
	ext.Entry = &protocolv1.ExternalEntry_Message{Message: m}
	return &agentshimv1.Entry{
		Internal: &agentshimv1.InternalEntry{Plane: filePlane(), WriteId: writeID(at, discriminator)},
		External: ext,
	}
}

// SyntheticMessageEntry wraps a conversation record the sidecar INFERRED, whose
// write identity therefore comes from the inference rather than a file offset.
func SyntheticMessageEntry(at Attribution, name string, m *conversationv1.MessageEntry) *agentshimv1.Entry {
	ext := external(at)
	ext.Entry = &protocolv1.ExternalEntry_Message{Message: m}
	return &agentshimv1.Entry{
		Internal: &agentshimv1.InternalEntry{Plane: filePlane(), WriteId: syntheticWriteID(name)},
		External: ext,
	}
}

// BookkeepingEntry wraps a fact ABOUT the session as a stored Entry.
func BookkeepingEntry(at Attribution, discriminator string, b *protocolv1.BookkeepingEntry) *agentshimv1.Entry {
	ext := external(at)
	ext.Entry = &protocolv1.ExternalEntry_Bookkeeping{Bookkeeping: b}
	return &agentshimv1.Entry{
		Internal: &agentshimv1.InternalEntry{Plane: filePlane(), WriteId: writeID(at, discriminator)},
		External: ext,
	}
}

// SyntheticBookkeepingEntry wraps a fact the sidecar states about itself rather
// than reads: a diagnostic, an outage report. It has no file position.
func SyntheticBookkeepingEntry(at Attribution, name string, b *protocolv1.BookkeepingEntry) *agentshimv1.Entry {
	ext := external(at)
	ext.Entry = &protocolv1.ExternalEntry_Bookkeeping{Bookkeeping: b}
	return &agentshimv1.Entry{
		Internal: &agentshimv1.InternalEntry{Plane: filePlane(), WriteId: syntheticWriteID(name)},
		External: ext,
	}
}

// VendorSpecificEntry stores a record we UNDERSTAND and have decided not to
// carry into a vendor-agnostic feed. It has NO external half, so it has no path
// to the daemon at all.
func VendorSpecificEntry(at Attribution, kind string, raw map[string]any) *agentshimv1.Entry {
	return &agentshimv1.Entry{Internal: &agentshimv1.InternalEntry{
		Plane:   filePlane(),
		WriteId: writeID(at, "vendor:"+kind),
		Unconverted: &agentshimv1.InternalEntry_VendorSpecific{VendorSpecific: &agentshimv1.VendorSpecificEntry{
			Kind: kind,
			Raw:  rawStruct(raw),
		}},
	}}
}

// UnknownEntry stores a record we PARSED but do not MODEL.
func UnknownEntry(at Attribution, discriminator, discriminatorField string, raw map[string]any) *agentshimv1.Entry {
	return &agentshimv1.Entry{Internal: &agentshimv1.InternalEntry{
		Plane:   filePlane(),
		WriteId: writeID(at, "unknown:"+discriminatorField+":"+discriminator),
		Unconverted: &agentshimv1.InternalEntry_Unknown{Unknown: &agentshimv1.UnknownEntry{
			Discriminator:      discriminator,
			DiscriminatorField: discriminatorField,
			Raw:                rawStruct(raw),
		}},
	}}
}

// UnparsedEntry stores a record we could not READ at all — a failure rather
// than a gap.
func UnparsedEntry(at Attribution, raw []byte, cause error) *agentshimv1.Entry {
	if len(raw) > maxUnparsedRaw {
		raw = raw[:maxUnparsedRaw]
	}
	return &agentshimv1.Entry{Internal: &agentshimv1.InternalEntry{
		Plane:   filePlane(),
		WriteId: writeID(at, "unparsed"),
		Unconverted: &agentshimv1.InternalEntry_Unparsed{Unparsed: &agentshimv1.UnparsedEntry{
			Source:     at.Path,
			Offset:     uint64(at.Offset),
			ParseError: cause.Error(),
			Raw:        string(raw),
		}},
	}}
}

// rawStruct converts a decoded JSON object into the Struct the unconverted arms
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
