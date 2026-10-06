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
	// SDKVersion is the Agent SDK version the fake shim reports on every
	// session start, as the real shim reports its installed package's.
	SDKVersion  = "0.0.0-fakeshim"
	MainAgentID = "main"
	// DefaultColdContextTokens is the context size the fake measures a model
	// switch against, matching DefaultContextUsage's own figure. A switch is
	// refused `cold` when the caller's stated threshold is BELOW it, which is
	// the real shim's own rule.
	DefaultColdContextTokens = 1000
)

// DefaultCatalog is the model catalog a fresh StartSession answers with.
func DefaultCatalog() []*conversationv1.ModelOption {
	return []*conversationv1.ModelOption{
		{Model: &conversationv1.AgentModel{Name: "opus"}, DisplayName: "Opus", Description: "most capable",
			Capabilities: effortCapabilities(
				conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_LOW,
				conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_MEDIUM,
				conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_HIGH,
			)},
		{Model: &conversationv1.AgentModel{Name: "sonnet"}, DisplayName: "Sonnet", Description: "balanced"},
		{Model: &conversationv1.AgentModel{Name: "haiku"}, DisplayName: "Haiku", Description: "fastest",
			Capabilities: &conversationv1.ModelCapabilities{
				EffortSupport: &conversationv1.ModelCapabilities_EffortUnsupported{
					EffortUnsupported: &conversationv1.ModelEffortUnsupported{},
				},
			}},
	}
}

// effortCapabilities states a catalog row that accepts LEVELS, the shape the
// effort selector is served from. Sonnet states no capability block at all,
// so the catalog carries each of the three effort standings.
func effortCapabilities(levels ...conversationv1.AgentEffortLevel) *conversationv1.ModelCapabilities {
	return &conversationv1.ModelCapabilities{
		EffortSupport: &conversationv1.ModelCapabilities_EffortSupported{
			EffortSupported: &conversationv1.ModelEffortSupported{Levels: levels},
		},
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

// UnhealthyDiagnostics is the opening push of a shim standing on a fault it
// does not clear: an ANSWER, and never readiness.
func UnhealthyDiagnostics(detail string) *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_Diagnostics{Diagnostics: &conversationv1.SessionDiagnostics{
			Health: &conversationv1.SessionDiagnostics_Unhealthy{Unhealthy: &conversationv1.SessionUnhealthy{
				Faults: []*conversationv1.SessionFault{{
					Component: "store client",
					Detail:    detail,
					Kind: &conversationv1.SessionFault_StoreUnreachable{
						StoreUnreachable: &conversationv1.SessionFaultStoreUnreachable{},
					},
				}},
			}},
		}},
	}
}

// NetworkUnreachableDiagnostics is the push of a shim that saw this machine
// unable to reach the network, with what it observed as the detail.
func NetworkUnreachableDiagnostics(detail string) *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_Diagnostics{Diagnostics: &conversationv1.SessionDiagnostics{
			Health: &conversationv1.SessionDiagnostics_Unhealthy{Unhealthy: &conversationv1.SessionUnhealthy{
				Faults: []*conversationv1.SessionFault{{
					Component: "vendor",
					Detail:    detail,
					Kind: &conversationv1.SessionFault_NetworkUnreachable{
						NetworkUnreachable: &conversationv1.SessionFaultNetworkUnreachable{},
					},
				}},
			}},
		}},
	}
}

// DefaultContextUsage is the opening context-usage push. The real shim states
// the session's usage at its own cadence starting at the session's start, and
// the topbar draws NO view at all until it has one, so the fake states it
// alongside the readiness push. A test asserting on particular figures pushes
// its own, which supersedes this.
func DefaultContextUsage() *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_ContextUsage{ContextUsage: &conversationv1.SessionContextUsage{
			TotalTokens:  1000,
			MaxTokens:    200000,
			RawMaxTokens: 200000,
			Percentage:   1,
			Model:        DefaultModel,
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
