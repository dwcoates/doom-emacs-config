package sessioncontroller

import (
	"crypto/rand"
	"encoding/hex"
	"fmt"
)

// newSecureControllerGenerationID mints an opaque identifier that remains
// distinct from generations persisted by earlier daemon processes.
func newSecureControllerGenerationID() (string, error) {
	var raw [16]byte
	if _, err := rand.Read(raw[:]); err != nil {
		return "", fmt.Errorf("session-controller: mint controller generation id: %w", err)
	}
	return "cg_" + hex.EncodeToString(raw[:]), nil
}
