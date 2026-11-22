package main

import (
	"agent-repld/integration/fakeshim/shimfacts"
	"crypto/rand"
	"encoding/hex"
	"fmt"
)

// The fake's default session facts live in shimfacts, which the in-process
// scenario projection serves too; these names keep the fake's own call sites.
const (
	DefaultBuildSHA          = shimfacts.DefaultBuildSHA
	DefaultModel             = shimfacts.DefaultModel
	SDKVersion               = shimfacts.SDKVersion
	MainAgentID              = shimfacts.MainAgentID
	DefaultColdContextTokens = shimfacts.DefaultColdContextTokens
)

var (
	DefaultCatalog                = shimfacts.DefaultCatalog
	HealthyDiagnostics            = shimfacts.HealthyDiagnostics
	UnhealthyDiagnostics          = shimfacts.UnhealthyDiagnostics
	NetworkUnreachableDiagnostics = shimfacts.NetworkUnreachableDiagnostics
	DefaultContextUsage           = shimfacts.DefaultContextUsage
	EmptyFloorPage                = shimfacts.EmptyFloorPage
)

// mintID produces a fresh vendor session id.
func mintID() string {
	var b [16]byte
	if _, err := rand.Read(b[:]); err != nil {
		panic("fakeshim: no entropy for a session id: " + err.Error())
	}
	return hex.EncodeToString(b[:])
}

// pointerAt builds a stable history pointer for a pushed frame.
func pointerAt(agent string, seq int) string {
	if agent == "" {
		agent = MainAgentID
	}
	return fmt.Sprintf("%s:%d", agent, seq)
}

func sprintf(format string, args ...any) string { return fmt.Sprintf(format, args...) }
