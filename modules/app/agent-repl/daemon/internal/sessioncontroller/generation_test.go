package sessioncontroller

import (
	"errors"
	"strings"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
)

func TestControllerGenerationIsStableAcrossEveryEdge(t *testing.T) {
	spawner := &fakeSpawner{}
	m, _ := newTestManager(t, fakeLocator{m: map[string]string{"ws": "s1"}}, spawner)
	m.newControllerGenerationID = func() (string, error) { return "g_fixed", nil }

	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	d, err := m.existing("ws")
	if err != nil {
		t.Fatalf("existing: %v", err)
	}
	if d.generationID != "g_fixed" || d.consumer.generationID != "g_fixed" {
		t.Fatalf("controller identities = controller:%q consumer:%q, want g_fixed",
			d.generationID, d.consumer.generationID)
	}
	edges := m.cfg.SSM.(*fakeApplier).connectivityEdges
	if len(edges) != 1 || edges[0].generationID != "g_fixed" {
		t.Fatalf("connectivity edges = %+v, want one g_fixed edge", edges)
	}
}

func TestControllerGenerationMintFailureAbortsBeforeSpawnOrMutation(t *testing.T) {
	spawner := &fakeSpawner{}
	m, _ := newTestManager(t, fakeLocator{m: map[string]string{"ws": "s1"}}, spawner)
	mintErr := errors.New("entropy unavailable")
	m.newControllerGenerationID = func() (string, error) { return "", mintErr }
	var logs []string
	m.logf = func(format string, args ...any) { logs = append(logs, format) }

	err := m.Ensure("ws")
	if !errors.Is(err, mintErr) {
		t.Fatalf("Ensure error = %v, want %v", err, mintErr)
	}
	if len(spawner.calls) != 0 || m.Live("ws") {
		t.Fatalf("mint failure mutated runtime: spawn_calls=%v live=%v", spawner.calls, m.Live("ws"))
	}
	if edges := m.cfg.SSM.(*fakeApplier).connectivityEdges; len(edges) != 0 {
		t.Fatalf("mint failure wrote connectivity edges: %+v", edges)
	}
	if len(logs) != 1 ||
		!strings.Contains(logs[0], "controller generation mint FAILED") ||
		!strings.Contains(logs[0], "decision=abort_before_spawn") {
		t.Fatalf("mint failure log = %v", logs)
	}
}

func TestRetiredGenerationOfSameSessionCannotMutateReplacement(t *testing.T) {
	m, _ := newTestManager(t, fakeLocator{m: map[string]string{"ws": "s1"}}, &fakeSpawner{})
	m.newControllerGenerationID = func() (string, error) { return "g_current", nil }
	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	applier := m.cfg.SSM.(*fakeApplier)
	before := len(applier.connectivityEdges)

	if err := m.onHandshakeForGeneration(
		"ws", "s1", "g_retired", &corev1.ShimHello{SessionId: "s1"},
	); err == nil {
		t.Fatal("retired generation handshake succeeded")
	}
	m.onConnectedForGeneration("ws", "s1", "g_retired", &corev1.ShimHello{SessionId: "s1"})
	m.onLinkLostForGeneration("ws", "s1", "g_retired", errors.New("late EOF"))

	if after := len(applier.connectivityEdges); after != before {
		t.Fatalf("retired generation mutated connectivity: before=%d after=%d edges=%+v",
			before, after, applier.connectivityEdges)
	}
}
