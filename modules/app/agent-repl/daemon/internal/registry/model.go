package registry

import "strings"

// placeholderModel is what the Claude CLI reports as its model when it is not
// running a real, nameable one. It is a marker, not a model id: nothing can be
// spawned under it, and `claude --model '<synthetic>'` is not a command that
// means anything.
const placeholderModel = "<synthetic>"

// IsPlaceholderModel reports whether model is the CLI's placeholder rather than
// a real model id.
//
// AN EMPTY MODEL IS NOT A PLACEHOLDER, and the distinction is the whole reason
// this is a named predicate rather than an inline comparison. Empty means "pin
// nothing and let the CLI choose its configured default", which is a legitimate
// and common state that the spawn path already handles by omitting `--model`.
// The placeholder means "the CLI told us something that is not an answer", which
// must never be written down and never be spawned with.
//
// WHY IT MATTERS HERE. The placeholder was being adopted as a session's model at
// create, persisted to the registry, and then replayed onto the spawn argv on
// every respawn thereafter. Twelve records in one live registry held it. A
// session's model must be either a real id or honestly absent.
func IsPlaceholderModel(model string) bool {
	return strings.TrimSpace(model) == placeholderModel
}
