package e2e

import (
	"agentrepl/e2e/kit/stack"
	"encoding/json"
)

// decodeBool reads an elisp truth value back. `json-encode' writes nil as
// `null' and t as `true', so anything but a JSON null is truth.
func decodeBool(raw json.RawMessage) bool {
	if stack.IsJSONNull(raw) {
		return false
	}
	return string(raw) != "false"
}
