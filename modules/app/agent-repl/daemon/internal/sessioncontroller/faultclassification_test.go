package sessioncontroller

import (
	"fmt"
	"strings"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"

	"claude-repld/internal/ssm"
)

func TestEveryDegradedProducerHasAnExplicitFaultImpact(t *testing.T) {
	want := map[string]ssm.FaultImpact{
		"shim-store-client":              ssm.FaultImpactConnectivity,
		"shim-store":                     ssm.FaultImpactConnectivity,
		"store-client":                   ssm.FaultImpactConnectivity,
		"store":                          ssm.FaultImpactConnectivity,
		"shim-connection":                ssm.FaultImpactConnectivity,
		"claude-shim-model-catalog":      ssm.FaultImpactFeature,
		"daemon-model-catalog":           ssm.FaultImpactFeature,
		"shim-claude-sidecar-store-link": ssm.FaultImpactFeature,
		"claude-shim-sdk":                ssm.FaultImpactTurnTerminal,
		"claude-shim-interrupt":          ssm.FaultImpactCommand,
		"claude-shim-permission-mode":    ssm.FaultImpactCommand,
		"claude-shim":                    ssm.FaultImpactFeature,
	}
	if len(faultClassifications) != len(want) {
		t.Fatalf("classification count = %d, want %d: %+v", len(faultClassifications), len(want), faultClassifications)
	}
	for component, impact := range want {
		got, ok := faultClassifications[component]
		if !ok || got.faultType == "" || got.impact != impact {
			t.Fatalf("classification[%q] = %+v/%v, want non-empty type and impact %q",
				component, got, ok, impact)
		}
	}
}

func TestUnknownDegradedProducerFailsLoudlyWithoutStateMutation(t *testing.T) {
	applier := &fakeApplier{}
	var logs []string
	c := newConsumer(
		"ws", "s1", &fakePusher{}, applier, nil, newFakeClearCompactStore(), emptyTurnAccountingStore{},
		func(format string, args ...any) { logs = append(logs, fmt.Sprintf(format, args...)) },
		nil, nil, nil, nil, nil,
	)
	c.generationID = "g1"

	c.Degraded("s1", nil, &corev1.DegradedState{
		Component: "new-unclassified-component",
		Reason:    "opaque failure",
	})

	if len(applier.faultEdges) != 0 {
		t.Fatalf("unknown producer wrote runtime fault edges: %+v", applier.faultEdges)
	}
	found := false
	for _, line := range logs {
		if strings.Contains(line, "runtime fault REJECTED") &&
			strings.Contains(line, `component="new-unclassified-component"`) &&
			strings.Contains(line, "branch=unknown_component") {
			found = true
		}
	}
	if !found {
		t.Fatalf("unknown producer rejection missing identity-complete log: %v", logs)
	}
}
