package convert

import (
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
)

// neverpersist.go — RESIDUE KINDS NEVER PERSISTED (owner ruling 2026-09-13,
// docs/STORE-VOLUME-PROPOSAL.md item 1, first two kinds).
//
// THE DISCOVERY MANDATE IS UNTOUCHED. Every line is still READ, framed, and
// CLASSIFIED by the same converter branch it always was — hook attachments still
// run through hookAttachment and still state what the vendor recorded. What
// changes is only the last step: an entry whose vendor_specific kind is named
// here is not handed to the store.
//
// WHY A DROP RATHER THAN A NARROWER READ. The measured store (1.64 GB,
// 612,214 rows) is 37% `attachment/hook_success` and 9%
// `attachment/total_tokens_reminder` — 286,389 rows and 263 MB that NO reader is
// ever served:
//
//   - `attachment/hook_success` is the transcript's copy of a hook firing. The
//     STREAM plane owns the served hook row (keys.go, ruling 2026-09-04), and the
//     two planes hold disjoint identity material, so this copy can never be
//     joined to the row a reader sees. It is a second, poorer, unjoinable copy.
//   - `attachment/total_tokens_reminder` is the vendor's per-turn
//     `<total_tokens>N tokens left</total_tokens>` line. It carries one bare
//     `text` field, reaches no arm of the conversation vocabulary, and is read by
//     nothing.
//
// AN UNKNOWN KIND STAYS PERSISTED. This is a NAMED list and never a predicate: a
// residue kind nobody has ruled on is exactly the kind whose stored record IS
// the coverage, and dropping it by pattern would silently lose the evidence that
// a new vendor behavior exists. Only the kinds spelled here are dropped, and
// adding one is an owner ruling — recorded in the sidecar's AGENTS.md under
// "residue kinds never persisted".
var neverPersistedResidueKinds = map[string]bool{
	"attachment/hook_success":          true,
	"attachment/total_tokens_reminder": true,
}

// IsNeverPersistedResidue reports whether a vendor_specific residue kind is on
// the never-persisted list.
func IsNeverPersistedResidue(kind string) bool { return neverPersistedResidueKinds[kind] }

// NeverPersistedResidueKinds returns the named list, for the doc and the tests
// that assert the two spellings have not drifted.
func NeverPersistedResidueKinds() []string {
	out := make([]string, 0, len(neverPersistedResidueKinds))
	for kind := range neverPersistedResidueKinds {
		out = append(out, kind)
	}
	sortStrings(out)
	return out
}

// sortStrings is an insertion sort over the handful of names above; the package
// deliberately carries no sort import for a two-element list.
func sortStrings(s []string) {
	for i := 1; i < len(s); i++ {
		for j := i; j > 0 && s[j] < s[j-1]; j-- {
			s[j], s[j-1] = s[j-1], s[j]
		}
	}
}

// ---------------------------------------------------------------------------
// the drop, and the count of it
// ---------------------------------------------------------------------------

// dropNeverPersisted removes the never-persisted residue kinds from a line's
// converted entries, tallies each drop by kind, and states it at DEBUG.
//
// DEBUG PER LINE, NEVER INFO. A dropped kind is the STEADY STATE — 37% of every
// transcript's records are hook attachments — so one INFO per line would be the
// same inverted pyramid the catch-up window exists to level, in perpetuity
// rather than only at boot. The decision is still written per line, so any one
// record can be traced; the SUMMARY is what a reader watches, and the reader
// states it once per file at catch-up end (cycle.go, summarizeDroppedResidue).
//
// IT FILTERS ENTRIES RATHER THAN SHORT-CIRCUITING A BRANCH, so the drop is
// decided from the residue kind the converter itself minted — the one string the
// ruling names — and no future producer of these kinds can route around it.
func (c *Converter) dropNeverPersisted(at Attribution, entries []*storev1.StoreEntry) []*storev1.StoreEntry {
	kept := entries[:0]
	for _, e := range entries {
		kind := e.GetAgentUpdate().GetUnservedItem().GetVendorSpecific().GetKind()
		if kind == "" || !neverPersistedResidueKinds[kind] {
			kept = append(kept, e)
			continue
		}
		c.droppedResidue[kind]++
		c.log.With(at.ctxFor("residue-drop")).With(logging.Context{UpsertKey: e.GetUpsertKey()}).
			LogVerbose("residue kind %q is on the never-persisted list; the line was read and classified and is not stored", kind)
	}
	return kept
}

// DroppedResidue returns this file's never-persisted counts by kind, as a copy.
// Nil when nothing was dropped, so a caller states a summary only for a file
// that had one.
func (c *Converter) DroppedResidue() map[string]int {
	if len(c.droppedResidue) == 0 {
		return nil
	}
	out := make(map[string]int, len(c.droppedResidue))
	for kind, count := range c.droppedResidue {
		out[kind] = count
	}
	return out
}
