package registry

import (
	"strings"

	"claude-repld/internal/protocmd"
)

// placeholderModel is what the Claude CLI reports as its model when it is not
// running a real, nameable one. It is a marker, not a model id.
//
// READ OFF THE SCHEMA, NEVER SPELLED HERE. The literal used to be written out
// once per runtime — twice in Go, once in the shim, and three times inline in
// the webapp — with nothing comparing the copies. A vendor that renamed the
// marker would have been adopted in some of them and not the others, and the
// ones left behind would have begun treating the placeholder as a selectable
// model. The one definition is now the MODEL_MARKER_SYNTHETIC enum value's
// option, and this reads it back.
//
// Resolved at package init, which is the loudest and earliest this can fail.
// The value is fixed the moment the bindings are generated, so a schema that
// disagrees with them is a broken build rather than a runtime condition, and
// protocmd panics on it. Failing at init means it can never be observed as a
// wrong model instead.
var placeholderModel = protocmd.SyntheticModelLiteral()

// IsPlaceholderModel reports whether model is the CLI's placeholder rather than
// a real model id.
//
// The predicate identifies the literal marker. Call NormalizeModel at model
// boundaries to give it the SAME downstream semantics as an empty model: pin
// nothing and let the shim/CLI choose.
//
// WHY IT MATTERS HERE. The placeholder was being adopted as a session's model at
// create, persisted to the registry, and then replayed onto the spawn argv on
// every respawn thereafter. Twelve records in one live registry held it. A
// session's model must be either a real id or honestly absent.
func IsPlaceholderModel(model string) bool {
	return strings.TrimSpace(model) == placeholderModel
}

// NormalizeModel returns the durable/spawnable representation of model.
//
// Empty is the canonical "no model override" representation. The CLI marker
// carries exactly that meaning, so it is normalized to empty before request
// coalescing, persistence, or spawn argv construction can distinguish them.
// Real model ids are preserved byte-for-byte.
func NormalizeModel(model string) string {
	if IsPlaceholderModel(model) {
		return ""
	}
	return model
}

// Model is a model value that HAS ALREADY BEEN NORMALIZED, carried as a type
// rather than as a convention.
//
// THE RUNG THIS CLIMBS. NormalizeModel is a shared helper, and a shared helper
// achieves agreement rather than enforcement: every call site is expected to
// remember to call it, a new one compiles perfectly well without doing so, and
// three webapp sites had already drifted into hand-inlined comparisons instead.
// A value that carries its own guarantee inverts that — a consumer added later
// inherits the rule without knowing it exists, and an unnormalized or
// placeholder model becomes a COMPILE ERROR rather than a discipline lapse.
//
// The field is unexported, so NewModel below is the only way to build a
// non-empty one and the normalizer is the only way through it. The zero value
// is the honest absence — "pin nothing, let the shim choose" — which is
// exactly what the normalizer maps the placeholder to, so a forgotten
// initialization degrades to the safe answer rather than to a wrong model.
type Model struct{ value string }

// NewModel is THE constructor, and it normalizes.
//
// It takes the raw string a vendor, a frontend, or a record handed over, and
// returns the value every downstream consumer may rely on: a real model id, or
// empty. The placeholder cannot survive it.
func NewModel(raw string) Model { return Model{value: NormalizeModel(raw)} }

// String is the wire/spawn representation: the real id, or empty for absence.
func (m Model) String() string { return m.value }

// Empty reports whether the model pins nothing.
//
// True for an absent model AND for the placeholder, which is the whole point:
// the two mean the same thing downstream, and a caller asking this question
// cannot accidentally treat the marker as a selection.
func (m Model) Empty() bool { return m.value == "" }
