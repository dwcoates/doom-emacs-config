// residue.go is the sidecar's LAST RESORT INGESTION: bytes that reach it are
// bytes whose conversion could not be selected at all — a spool whose task id
// carries no a/b/w kind prefix, or a spool no spawning call ever claimed.
//
// IT IS NOT A FALLBACK. A fallback would let a recognizable kind quietly take a
// lesser path; this handler is only ever reached after the failure to classify
// has ALREADY been stated at error or warning level by discovery or by the hold.
// What it adds is the other half of total ingestion: the bytes land in the store,
// whole and investigable, rather than being dropped because nothing knew what
// they meant.
//
// The entries it mints are ENVELOPE-level only — StoreUnservedItem.unparsed
// inside a file-plane StoreEntry. It reads nothing out of the bytes and models
// nothing about them, which is exactly why it can never be mistaken for a
// converter.
package main

import (
	"crypto/sha256"
	"encoding/hex"
	"fmt"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/convert"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/storeclient"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

// residueHandler turns whatever it is given into unparsed residue.
type residueHandler struct {
	// reason names why conversion could not be selected, so the stored record
	// says what a human needs to know without anyone reading the log.
	reason string
	log    *logging.Bound
}

func newResidueHandler(reason string, log *logging.Bound) *residueHandler {
	return &residueHandler{reason: reason, log: log}
}

// Handle implements tail.Handler.
func (h *residueHandler) Handle(frames []tail.Frame, ctx *tail.Context) []*storev1.StoreEntry {
	out := make([]*storev1.StoreEntry, 0, len(frames))
	for _, frame := range frames {
		out = append(out, residueEntry(ctx.FileID, ctx.Path, frame.Offset, h.reason, frame.Raw))
		h.log.With(logging.Context{
			Operation: "residue", Path: ctx.Path, TaskID: ctx.TaskID, Offset: logging.Off(frame.Offset),
		}).LogVerbose("ingested %d byte(s) as residue: %s", len(frame.Raw), h.reason)
	}
	return out
}

// declaredResidueHandler ingests a file whose kind IS recognized but whose
// conversion deliberately does not exist yet — the w* workflow spool, with
// workflow kicked for this wave (R-S4).
//
// IT IS NOT residueHandler. That one states "no conversion could be selected",
// which is a failure to classify; this one states a DECLARED kind, so a reader
// can tell the two apart without reading prose: the bytes land as
// `vendor_specific{kind}` rather than as `unparsed`, and the day workflow
// ingestion lands, every one of these rows is findable by that kind.
type declaredResidueHandler struct {
	kind string
	log  *logging.Bound
}

func newDeclaredResidueHandler(kind string, log *logging.Bound) *declaredResidueHandler {
	return &declaredResidueHandler{kind: kind, log: log}
}

// Handle implements tail.Handler.
func (h *declaredResidueHandler) Handle(frames []tail.Frame, ctx *tail.Context) []*storev1.StoreEntry {
	out := make([]*storev1.StoreEntry, 0, len(frames))
	for _, frame := range frames {
		at := convert.Attribution{
			Path:   ctx.Path,
			FileID: ctx.FileID,
			Offset: frame.Offset,
			TaskID: ctx.TaskID,
		}
		// The attribution carries no RecordUUID — a spool's bytes are a byte
		// range, not a record — so convert.ResidueKey keys it
		// `residue:file:<path>:<offset>`, which is exactly what R-S4 pins.
		out = append(out, convert.VendorSpecificEntry(at, h.kind, map[string]any{
			"path":   ctx.Path,
			"offset": float64(frame.Offset),
			"output": string(frame.Raw),
		}))
		h.log.With(logging.Context{
			Operation: "declared-residue", Path: ctx.Path, TaskID: ctx.TaskID,
			FileID: ctx.FileID, Offset: logging.Off(frame.Offset),
		}).LogVerbose("ingested %d byte(s) as declared residue kind=%s", len(frame.Raw), h.kind)
	}
	return out
}

// residueEntry mints one unparsed record.
//
// The write id is DETERMINISTIC — a digest of the write's source coordinates —
// so the same bytes re-read after a restart mint the same id and the store
// absorbs the replay as success instead of storing the residue twice. The
// upsert key is the same coordinates, so a re-read supersedes the row whole
// rather than growing a second one beside it.
// R-S1: the digest is over the FILE ID, never the path, so a renamed file
// re-read from its (inode-keyed) cursor mints the identical write ids and the
// store absorbs the replay instead of storing the residue a second time. The
// upsert key still names the PATH, because that is the human-facing coordinate
// a residue row is investigated by and it is the same key convert.ResidueKey
// uses for a uuid-less record.
func residueEntry(fileID, path string, offset int64, reason string, raw []byte) *storev1.StoreEntry {
	if fileID == "" {
		panic("sidecar: residue write_id requires the file's dev:inode identity; the reader supplied none")
	}
	coordinates := fmt.Sprintf("%s|%s|%d|residue", storeclient.Producer, fileID, offset)
	digest := sha256.Sum256([]byte(coordinates))
	return &storev1.StoreEntry{
		Plane:   &storev1.Plane{Plane: &storev1.Plane_File{File: &storev1.PlaneFile{}}},
		WriteId: hex.EncodeToString(digest[:]),
		// A spool's bytes carry no vendor uuid — there is no record, only a byte
		// range — so they key on where they live. `residue:file:` is the same
		// space convert.ResidueKey uses for a uuid-less record, kept visibly
		// apart from the uuid space so no path can collide with a uuid.
		UpsertKey: fmt.Sprintf("residue:file:%s:%d", path, offset),
		Entry: &storev1.StoreEntry_AgentUpdate{AgentUpdate: &storev1.StoreAgentUpdate{
			// top_level is genuinely unresolvable here: residue names no agent,
			// which is the one case the field is documented to be unset for.
			AgentInfo: &storev1.StoreAgentUpdate_UnservedItem{UnservedItem: &storev1.StoreUnservedItem{
				UnservedItem: &storev1.StoreUnservedItem_Unparsed{Unparsed: &storev1.StoreUnparsed{
					Source:     path,
					Offset:     uint64(offset),
					ParseError: reason,
					Raw:        string(raw),
				}},
			}},
		}},
	}
}
