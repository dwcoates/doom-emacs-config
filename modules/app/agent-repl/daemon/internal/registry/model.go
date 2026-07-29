package registry

import "strings"

// placeholderModel is what the Claude CLI reports as its model when it is not
// running a real, nameable one. It is a marker, not a model id.
const placeholderModel = "<synthetic>"

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
