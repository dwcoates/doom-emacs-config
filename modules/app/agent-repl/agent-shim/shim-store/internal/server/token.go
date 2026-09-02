package server

import (
	"crypto/rand"
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"sync"
)

// tokenBytes is the minted token's entropy: 128 bits, hex-encoded.
const tokenBytes = 16

// tokenEntry is what a minted token resolves to: the book the watch follows,
// and the global write ordinal the opening page was read at.
type tokenEntry struct {
	agentID string
	pinSeq  uint64
}

// tokenRegistry mints and consumes watch tokens.
//
// IN MEMORY AND SINGLE-USE, BOTH DELIBERATELY. A token is a capability minted
// at one OpenAgentSession and spent by exactly one WatchAgentSession: a second
// watch on the same token, or any token from a previous store process, is
// refused, and the caller re-opens. Nothing here survives a restart, which is
// what makes "the store restarted" and "you already used this" the same
// recovery for the caller.
type tokenRegistry struct {
	mu      sync.Mutex
	entries map[string]tokenEntry
}

func newTokenRegistry() *tokenRegistry {
	return &tokenRegistry{entries: map[string]tokenEntry{}}
}

// mint returns a fresh random token bound to agentID at pinSeq.
func (r *tokenRegistry) mint(agentID string, pinSeq uint64) (string, error) {
	raw := make([]byte, tokenBytes)
	if _, err := rand.Read(raw); err != nil {
		return "", fmt.Errorf("shim-store server: minting a watch token: %w", err)
	}
	token := hex.EncodeToString(raw)
	r.mu.Lock()
	defer r.mu.Unlock()
	r.entries[token] = tokenEntry{agentID: agentID, pinSeq: pinSeq}
	return token, nil
}

// consume resolves a token and RETIRES it in the same critical section, so two
// concurrent watches on one token cannot both succeed.
func (r *tokenRegistry) consume(token string) (tokenEntry, bool) {
	r.mu.Lock()
	defer r.mu.Unlock()
	entry, ok := r.entries[token]
	if !ok {
		return tokenEntry{}, false
	}
	delete(r.entries, token)
	return entry, true
}

// outstanding is the number of minted, unspent tokens. Diagnostics only.
func (r *tokenRegistry) outstanding() int {
	r.mu.Lock()
	defer r.mu.Unlock()
	return len(r.entries)
}

// tokenHash is the only form of a token that may be logged: a sha256 prefix.
// The token itself is a capability, and a log is not a place to publish one.
func tokenHash(token string) string {
	sum := sha256.Sum256([]byte(token))
	return hex.EncodeToString(sum[:])[:12]
}
