package prompthandler

import (
	"crypto/rand"
	"encoding/hex"
	"fmt"
)

// MintKey mints the idempotency key a DAEMON-BORN prompt is submitted under:
// PREFIX, a hyphen, and 32 random hex characters. Every such prompt is its own
// submission (a link's question, a merge's handback, a retarget's notice), so
// a key is never reused, and the prefix names which kind it was in the
// handler's claim records.
func MintKey(prefix string) (string, error) {
	var b [16]byte
	if _, err := rand.Read(b[:]); err != nil {
		return "", fmt.Errorf("mint a %s idempotency key: %w", prefix, err)
	}
	return prefix + "-" + hex.EncodeToString(b[:]), nil
}
