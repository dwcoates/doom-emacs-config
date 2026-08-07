package sessioncontroller

import (
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
)

// A SUPERSEDED SESSION'S DEATH CARD CLOSES WHEN ITS SUCCESSOR IS UP, AND THE
// BRING-UP GATE IS THE ONLY THING THAT MAY SAY SO.
//
// The supersede itself runs BEFORE the replacement session id is even minted,
// so nothing there could honestly close the card. The one edge that means "the
// newer session genuinely has this workspace" is the same one that writes
// OPERATIONAL, which is why the resolution is delivered from there and from
// nowhere weaker.

// operationalRig is newWiredRig with a registrar attached, since the fleet's
// default harness wires none.
func operationalRig(t *testing.T) (*Manager, *fakeRegistrar) {
	t.Helper()
	m, applier, _ := newWiredRig(t)
	reg := &fakeRegistrar{}
	m.cfg.Registrar = reg
	if _, err := m.bringUp("ws"); err != nil {
		t.Fatalf("bringUp: %v", err)
	}
	waitForWirings(applier, 1)
	return m, reg
}

func TestOperationalReportsTheWorkspaceHandoverIsComplete(t *testing.T) {
	// Arrange.
	m, reg := operationalRig(t)

	// Act — the bring-up gate closes.
	m.onConnected("ws", "s1", &corev1.ShimHello{})

	// Assert.
	got := reg.operationalEdges()
	if len(got) != 1 || got[0] != "ws=s1" {
		t.Fatalf("operational edges = %v, want exactly [ws=s1]", got)
	}
}

func TestABringUpBeforeItsGateResolvesNothing(t *testing.T) {
	// Arrange — the shim is spawned and the axis says `starting`, which is
	// precisely the window in which a fresh supersede must still show its card.
	_, reg := operationalRig(t)

	// Act — nothing; no ShimReady has arrived.

	// Assert.
	if got := reg.operationalEdges(); len(got) != 0 {
		t.Fatalf("operational edges = %v before the gate closed, want none", got)
	}
}

func TestAStaleShimReadyResolvesNothing(t *testing.T) {
	// Arrange — a ShimReady from a RETIRED controller generation. It proves
	// nothing about the workspace's current session, so it must not close a
	// card on that session's behalf.
	m, reg := operationalRig(t)

	// Act.
	m.onConnectedForGeneration("ws", "s1", "g_retired", &corev1.ShimHello{SessionId: "s1"})

	// Assert.
	if got := reg.operationalEdges(); len(got) != 0 {
		t.Fatalf("operational edges = %v for a retired generation, want none", got)
	}
}
