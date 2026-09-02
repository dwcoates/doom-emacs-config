package main

import (
	"crypto/rand"
	"encoding/hex"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"
)

// The fake's default session facts. They are the ones SPEC.md fixes, so a
// test that does not care about them can leave them alone.
const (
	// DefaultBuildSHA is duplicated as harness.FakeShimDefaultBuildSHA, which
	// the daemon's own SHIM_BUILD_SHA is set from; the two move together.
	DefaultBuildSHA = "fake"
	DefaultModel    = "opus"
	MainAgentID     = "main"
)

// DefaultCatalog is the model catalog a fresh StartSession answers with.
func DefaultCatalog() []*conversationv1.ModelOption {
	return []*conversationv1.ModelOption{
		{Model: &conversationv1.AgentModel{Name: "opus"}, DisplayName: "Opus", Description: "most capable"},
		{Model: &conversationv1.AgentModel{Name: "sonnet"}, DisplayName: "Sonnet", Description: "balanced"},
		{Model: &conversationv1.AgentModel{Name: "haiku"}, DisplayName: "Haiku", Description: "fastest"},
	}
}

// HealthyDiagnostics is the readiness push: the first healthy diagnostics
// arm on WatchSession is what gates the daemon's readiness.
func HealthyDiagnostics() *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_Diagnostics{Diagnostics: &conversationv1.SessionDiagnostics{
			Health: &conversationv1.SessionDiagnostics_Healthy{Healthy: &conversationv1.SessionHealthy{}},
		}},
	}
}

// EmptyFloorPage is a history page with no entries that states it is the
// bottom of the transcript.
func EmptyFloorPage() *conversationv1.HistoryPage {
	return &conversationv1.HistoryPage{
		Boundary: &conversationv1.HistoryPage_Floor{Floor: &conversationv1.HistoryFloor{}},
	}
}

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
