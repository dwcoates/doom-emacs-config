package rollout

import (
	"context"
	"errors"
	"testing"
	"time"

	"agent-repld/internal/ids"
)

// outgoingPrevious is the incumbent writeHandoverManifest names.
const outgoingPrevious = ids.InstanceID("daemon-outgoing-previous")

// carryParticipants writes a carry for ws sealed by sealedBy with the given
// occupancy, as an incumbent's transfer does before it releases the row.
func carryParticipants(t *testing.T, h *harness, ws ids.WorkspaceID, sealedBy ids.InstanceID, live Participants) {
	t.Helper()
	if err := h.c.writeCarry(Carry{
		Workspace:    ws,
		Daemon:       sealedBy,
		Participants: &CarriedParticipants{Host: live.Host, Web: live.Web},
	}); err != nil {
		t.Fatalf("writeCarry: %v", err)
	}
}

// releaseServing clears the serving row the harness workspace starts with, as
// the incumbent's transfer does.
func releaseServing(t *testing.T, h *harness, ws ids.WorkspaceID) {
	t.Helper()
	owner, err := h.db.Serving(context.Background(), ws)
	if err != nil {
		t.Fatalf("Serving: %v", err)
	}
	if owner == nil {
		return
	}
	if err := h.db.ReleaseServing(context.Background(), ws, *owner); err != nil {
		t.Fatalf("ReleaseServing: %v", err)
	}
}

// expectedOf answers the rendezvous's expectations for ws.
func expectedOf(t *testing.T, h *harness, ws ids.WorkspaceID) Participants {
	t.Helper()
	h.c.mu.Lock()
	defer h.c.mu.Unlock()
	e, armed := h.c.rendezvous[ws]
	if !armed {
		t.Fatalf("workspace %q is not armed", ws)
	}
	return e.expected
}

// cancelled answers a context that is already done, so a rendezvous call
// that would wait answers at once instead.
func cancelled() context.Context {
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	return ctx
}

func TestAHostCallAdoptsOnceTheTransferCarriedNoPage(t *testing.T) {
	// Arrange: the announcement saw a page, and the transfer found it gone.
	h := newHarness(t)
	ws, _ := h.workspace(t)
	arm(t, h, ws, Participants{Host: true, Web: true})
	carryParticipants(t, h, ws, outgoingPrevious, Participants{Host: true})

	// Act
	err := h.c.AdoptHost(context.Background(), ws)

	// Assert
	if err != nil {
		t.Fatalf("AdoptHost: %v, want the adoption to land without the page that went away", err)
	}
	if got := h.fleet.Adoptions(); len(got) != 1 || got[0] != ws {
		t.Fatalf("adoptions = %v, want the workspace adopted on the host's call alone", got)
	}
}

func TestACarryFromAnotherDaemonIsNotFollowed(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	arm(t, h, ws, Participants{Host: true, Web: true})
	carryParticipants(t, h, ws, ids.InstanceID("daemon-someone-else"), Participants{Host: true})

	// Act
	err := h.c.AdoptHost(cancelled(), ws)

	// Assert
	if !errors.Is(err, ErrNotYetAdopted) {
		t.Fatalf("AdoptHost = %v, want not_yet_adopted while the page is still owed", err)
	}
	if got := expectedOf(t, h, ws); got != (Participants{Host: true, Web: true}) {
		t.Fatalf("expected = %+v, want the announcement's host and web", got)
	}
}

func TestARefusedCarryIsNotFollowed(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	arm(t, h, ws, Participants{Host: true, Web: true})
	carryParticipants(t, h, ws, outgoingPrevious, Participants{Host: true})
	if err := h.c.writeRefusal(ws, "refused for the test"); err != nil {
		t.Fatalf("writeRefusal: %v", err)
	}

	// Act
	h.c.AdoptHost(cancelled(), ws)

	// Assert
	if got := expectedOf(t, h, ws); got != (Participants{Host: true, Web: true}) {
		t.Fatalf("expected = %+v, want the announcement's host and web", got)
	}
}

func TestATransferWithNoParticipantIsClaimedHeadlessOnceItsRowIsReleased(t *testing.T) {
	// Arrange: armed without Join, so no follower runs beside the look.
	h := newHarness(t)
	ws, _ := h.workspace(t)
	writeHandoverManifest(t, h, ws, Participants{Host: true, Web: true})
	if err := h.c.armFromManifest(); err != nil {
		t.Fatalf("armFromManifest: %v", err)
	}
	releaseServing(t, h, ws)
	carryParticipants(t, h, ws, outgoingPrevious, Participants{})

	// Act
	headless := h.c.followCarriesOnce()

	// Assert
	if len(headless) != 1 || headless[0] != ws {
		t.Fatalf("claimed headless = %v, want the workspace nobody is left to call about", headless)
	}
}

// TestAWorkspaceWhoseRowIsHeldIsStillClaimedForTheDaemonsAdoption: the
// daemon claims every armed workspace whatever its participants, and the
// adoption it runs waits on the serving release and arbitrates the claim
// (adopt), so a row the incumbent holds is never adopted from under it.
func TestAWorkspaceWhoseRowIsHeldIsStillClaimedForTheDaemonsAdoption(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	writeHandoverManifest(t, h, ws, Participants{Host: true, Web: true})
	if err := h.c.armFromManifest(); err != nil {
		t.Fatalf("armFromManifest: %v", err)
	}
	releaseServing(t, h, ws)
	if err := h.db.ClaimServing(context.Background(), ws, outgoingPrevious); err != nil {
		t.Fatalf("ClaimServing: %v", err)
	}
	carryParticipants(t, h, ws, outgoingPrevious, Participants{})

	// Act
	headless := h.c.followCarriesOnce()

	// Assert
	if len(headless) != 1 || headless[0] != ws {
		t.Fatalf("claimed = %v, want the armed workspace claimed for the daemon's own adoption", headless)
	}
	if got := expectedOf(t, h, ws); got != (Participants{Host: true, Web: true}) {
		t.Fatalf("expected = %+v, want the announcement's host and web", got)
	}
}

func TestAJoiningSuccessorAdoptsATransferThatLeftNobodyToCall(t *testing.T) {
	// Arrange: armed with participants, then transferred with none left.
	adopted := make(chan ids.WorkspaceID, 4)
	h := harnessSignallingAdoptions(t, adopted)
	ws, _ := h.workspace(t)
	arm(t, h, ws, Participants{Host: true, Web: true})
	releaseServing(t, h, ws)

	// Act
	carryParticipants(t, h, ws, outgoingPrevious, Participants{})

	// Assert
	select {
	case got := <-adopted:
		if got != ws {
			t.Fatalf("adopted %q, want %q", got, ws)
		}
	case <-time.After(10 * time.Second):
		t.Fatal("the workspace nobody was left to call about was never adopted")
	}
}
