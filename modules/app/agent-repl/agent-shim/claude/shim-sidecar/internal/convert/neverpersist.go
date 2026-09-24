package convert

import (
	"fmt"

	storev1 "agentrepl/proto/store/v1"
)

// neverpersist.go — RESIDUE IS NEVER PERSISTED (owner ruling 2026-09-13,
// docs/STORE-VOLUME-PROPOSAL.md item 1).
//
// THE DISCOVERY MANDATE IS UNTOUCHED. Every line is still READ, framed and
// CLASSIFIED by the same converter branch it always was, and every classifier
// still states what the vendor recorded. What changes is only the last step: an
// entry that carries a RESIDUE arm is counted and not handed to the store.
//
// WHY: NOBODY READS IT. `vendor_specific`, `unknown` and `unparsed` have zero
// readers anywhere downstream — not in the daemon, not in the webapp, not in
// the editor. The measured store (1.64 GB, 612,214 rows) is overwhelmingly
// those three arms, so the whole volume was stored for nothing. Storage is not
// coverage: what the reader SAW is evidence, and that evidence is the per-line
// debug record and the per-file withheld-count summary, both of which survive.
//
// THE FORWARD-COMPAT ARGUMENT MOVES TO THE COUNTS. `unknown` used to be kept on
// the grounds that its stored row IS the coverage for a vendor behavior nobody
// has modelled yet. The count and the classification are that coverage now: a
// new vendor line type still reaches the `unknown` arm, still names its own
// discriminator in the withheld tally, and the day it is modelled the durable
// FILE is re-read — the sidecar's sources are the vendor's own files, which is
// exactly why it holds no retry buffer and spills nothing.
//
// KEEPALIVE IS NOT RESIDUE, and is not withheld here: the converter drops a
// keep-alive's record itself, before any entry leaves it (keepalive.go), so
// nothing on the `unserved_item.keepalive` arm reaches this rule at all.

// IsResidue reports whether an entry carries one of the three RESIDUE arms —
// the ones no reader anywhere consumes.
//
// IT KEYS ON THE ARM THE CONVERTER MINTED, never on a kind string or a path, so
// no producer can route around it by inventing a new residue kind.
func IsResidue(e *storev1.StoreEntry) bool {
	switch e.GetAgentUpdate().GetUnservedItem().GetUnservedItem().(type) {
	case *storev1.StoreUnservedItem_VendorSpecific,
		*storev1.StoreUnservedItem_Unknown,
		*storev1.StoreUnservedItem_Unparsed:
		return true
	default:
		return false
	}
}

// ResidueLabel names WHAT was withheld, for the tally and the per-line record.
// It is the arm plus the arm's own discriminator, so a count says which vendor
// behavior produced the volume rather than merely how much there was. Empty for
// an entry that is not residue.
func ResidueLabel(e *storev1.StoreEntry) string {
	switch arm := e.GetAgentUpdate().GetUnservedItem().GetUnservedItem().(type) {
	case *storev1.StoreUnservedItem_VendorSpecific:
		return "vendor_specific/" + arm.VendorSpecific.GetKind()
	case *storev1.StoreUnservedItem_Unknown:
		return fmt.Sprintf("unknown/%s:%s", arm.Unknown.GetDiscriminatorField(), arm.Unknown.GetDiscriminator())
	case *storev1.StoreUnservedItem_Unparsed:
		return "unparsed"
	default:
		return ""
	}
}
