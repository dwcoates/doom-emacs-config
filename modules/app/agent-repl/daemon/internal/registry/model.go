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
