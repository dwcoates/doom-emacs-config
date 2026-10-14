// Package storewrite is the store's validation of a store.v1 WriteBatchRequest,
// held once: the store's server refuses with it, and every fake store a
// producer's suite runs against validates with the same function, so a fake
// can never accept a write the real store refuses, or refuse one it accepts.
package storewrite

import (
	"fmt"

	storev1 "agentrepl/proto/store/v1"
)

// The refusal sites this validation names. Each is a stable machine name the
// store records a refusal under; Field is what a producer's own logs quote.
const (
	SiteProducerEmpty       = "producer_empty"
	SiteWriteClassUnset     = "write_class_unset"
	SiteBatchMissing        = "batch_missing"
	SiteBatchEmpty          = "batch_empty"
	SiteEntryPlaneUnset     = "entry_plane_unset"
	SiteEntryWriteIDEmpty   = "entry_write_id_empty"
	SiteEntryUpsertKeyEmpty = "entry_upsert_key_empty"
	SiteEntryArmUnset       = "entry_arm_unset"
	SiteCursorFileIDEmpty   = "cursor_file_id_empty"
	SiteShapeUnset          = "shape_unset"
	SiteShapeHashEmpty      = "shape_hash_empty"
	SiteShapeKindEmpty      = "shape_kind_empty"
	SiteShapeStructureEmpty = "shape_structure_empty"
	SiteShapeSeenMsUnset    = "shape_seen_ms_unset"
)

// Refusal is one malformed write: the site it is recorded under, the field at
// fault, and the human detail. Detail is for people and logs, never switched
// on; Field is what the store's invalid_request arm carries.
type Refusal struct {
	Site   string
	Field  string
	Detail string
}

func refuse(site, field, format string, args ...any) *Refusal {
	return &Refusal{Site: site, Field: field, Detail: fmt.Sprintf(format, args...)}
}

// ValidateWriteBatchRequest answers why the store refuses req, or nil when it
// is well-formed. It opens only envelopes: the frames inside are the storage
// layer's to route.
func ValidateWriteBatchRequest(req *storev1.WriteBatchRequest) *Refusal {
	if req.GetProducer() == "" {
		return refuse(SiteProducerEmpty, "producer", "producer: the write names no producer, so nothing can be attributed")
	}
	// THE CLASS IS REQUIRED AND NEVER DEFAULTED. It decides which queue the
	// write takes into the one writer, and a guessed class is how a bulk copy
	// ends up ahead of a live turn.
	if req.GetWriteClass().GetWriteClass() == nil {
		return refuse(SiteWriteClassUnset, "write_class", "write_class: the write states neither interactive nor bulk, and the store never guesses which queue a write belongs in")
	}
	if ref := ValidateEntryBatch(req.GetBatch(), len(req.GetShapes()) > 0); ref != nil {
		return ref
	}
	// THE CATALOG IS PART OF THE WRITE, so a malformed observation refuses the
	// whole request here rather than reaching a transaction the records share.
	for i, shape := range req.GetShapes() {
		if ref := ValidateShapeObservation(shape, i); ref != nil {
			return ref
		}
	}
	return nil
}

// ValidateEntryBatch is the validation of store.v1.EntryBatch: every entry's
// envelope well-formed, a cursor advance (when sent) that names a file, and AT
// LEAST ONE thing stated.
//
// A CURSOR-ONLY BATCH IS LEGAL. A sidecar that read bytes yielding no entries
// must still make its file position durable. What is empty is a batch carrying
// no entry, no cursor advance, no agent locator and no shell run claim, in a
// request with no shape observation: that states nothing at all.
func ValidateEntryBatch(b *storev1.EntryBatch, carriesShapes bool) *Refusal {
	if b == nil {
		return refuse(SiteBatchMissing, "batch", "batch: the request carries no EntryBatch")
	}
	if len(b.GetEntries()) == 0 && b.GetCursorAdvance() == nil && len(b.GetAgentLocators()) == 0 && len(b.GetShellRunClaims()) == 0 && !carriesShapes {
		return refuse(SiteBatchEmpty, "batch", "batch: the EntryBatch carries neither entries nor a cursor advance nor an agent locator nor a shell run claim, and the request carries no shape observation either")
	}
	for i, entry := range b.GetEntries() {
		if ref := ValidateStoreEntry(entry, i); ref != nil {
			return ref
		}
	}
	if b.GetCursorAdvance() != nil {
		if ref := ValidateCursorState(b.GetCursorAdvance(), "cursor_advance"); ref != nil {
			return ref
		}
	}
	return nil
}

// ValidateStoreEntry is the validation of one write's ENVELOPE, the only part
// of a StoreEntry this layer opens.
func ValidateStoreEntry(e *storev1.StoreEntry, index int) *Refusal {
	what := fmt.Sprintf("entries[%d]", index)
	if e == nil {
		return refuse(SiteEntryArmUnset, what, "%s: no entry", what)
	}
	if ref := ValidatePlane(e.GetPlane(), what); ref != nil {
		return ref
	}
	if e.GetWriteId() == "" {
		return refuse(SiteEntryWriteIDEmpty, what+".write_id", "%s: write_id is empty, so the write cannot be deduped on replay", what)
	}
	if e.GetUpsertKey() == "" {
		return refuse(SiteEntryUpsertKeyEmpty, what+".upsert_key", "%s (write_id %s): upsert_key is empty, so the write identifies no row", what, e.GetWriteId())
	}
	if e.GetEntry() == nil {
		return refuse(SiteEntryArmUnset, what+".entry", "%s (write_id %s): the entry oneof names neither agent_update nor session_update", what, e.GetWriteId())
	}
	return nil
}

// ValidatePlane is the validation of store.v1.Plane: the oneof arm must be set.
// The store never guesses a producer.
func ValidatePlane(p *storev1.Plane, what string) *Refusal {
	if p == nil || p.GetPlane() == nil {
		return refuse(SiteEntryPlaneUnset, what+".plane", "%s: plane names neither stream nor file", what)
	}
	return nil
}

// ValidateCursorState is the validation of store.v1.CursorState. The file's
// stable identity is the row key, so an empty one is unset; path, offset and
// carry are payload with no presence rule (offset zero is a legitimate start).
func ValidateCursorState(c *storev1.CursorState, what string) *Refusal {
	if c.GetFileId() == "" {
		return refuse(SiteCursorFileIDEmpty, what+".file_id", "%s: a CursorState with no file_id keys no row", what)
	}
	return nil
}

// ValidateShapeObservation refuses a catalog observation that names no shape.
// Every field but the example is required: a hash, a kind, a rendering and an
// observation instant are what make a row matchable, filterable, readable and
// orderable. The example alone is optional, because a line can legitimately be
// empty bytes.
func ValidateShapeObservation(s *storev1.ShapeObservation, index int) *Refusal {
	what := fmt.Sprintf("shapes[%d]", index)
	if s == nil {
		return refuse(SiteShapeUnset, what, "%s: the shape observation is unset", what)
	}
	if s.GetShapeHash() == "" {
		return refuse(SiteShapeHashEmpty, what+".shape_hash", "%s: shape_hash is empty, and it is the catalog's primary key", what)
	}
	if s.GetKind() == "" {
		return refuse(SiteShapeKindEmpty, what+".kind", "%s (shape %s): kind is empty, so the observation names no residue kind", what, s.GetShapeHash())
	}
	if s.GetKeyStructure() == "" {
		return refuse(SiteShapeStructureEmpty, what+".key_structure", "%s (shape %s): key_structure is empty, so nothing says what was hashed", what, s.GetShapeHash())
	}
	if s.GetSeenMs() <= 0 {
		return refuse(SiteShapeSeenMsUnset, what+".seen_ms", "%s (shape %s): seen_ms is %d, not a positive unix millis; first_seen and last_seen come from the observer's clock", what, s.GetShapeHash(), s.GetSeenMs())
	}
	return nil
}
