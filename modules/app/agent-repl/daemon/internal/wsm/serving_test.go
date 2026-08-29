package wsm

import (
	"context"
	"errors"
	"testing"
)

func TestClaimServingRecordsTheOwner(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	instance := NewInstanceID()

	// Act
	if err := s.ClaimServing(context.Background(), ws.ID, instance); err != nil {
		t.Fatalf("ClaimServing: %v", err)
	}
	got, err := s.Serving(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("Serving: %v", err)
	}
	if got == nil || *got != instance {
		t.Fatalf("serving = %v, want %q", got, instance)
	}
}

func TestClaimServingTransfersOwnership(t *testing.T) {
	// Arrange — the handover's per-workspace transfer: the successor claims a
	// workspace the incumbent still names.
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	incumbent := NewInstanceID()
	successor := NewInstanceID()
	if err := s.ClaimServing(context.Background(), ws.ID, incumbent); err != nil {
		t.Fatalf("ClaimServing: %v", err)
	}

	// Act
	if err := s.ClaimServing(context.Background(), ws.ID, successor); err != nil {
		t.Fatalf("ClaimServing: %v", err)
	}

	// Assert
	got, err := s.Serving(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("Serving: %v", err)
	}
	if got == nil || *got != successor {
		t.Fatalf("serving = %v, want %q", got, successor)
	}
}

func TestClaimServingRefusesAnEmptyInstance(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	err := s.ClaimServing(context.Background(), ws.ID, InstanceID(""))

	// Assert
	if err == nil {
		t.Fatalf("ClaimServing with no instance succeeded")
	}
	if !loggedOperation(log, "daemon.wsm.claim_serving", "error") {
		t.Fatalf("the refusal was not logged at error: %v", log.Records())
	}
}

func TestClaimServingRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	err := s.ClaimServing(context.Background(), WorkspaceID("absent"), NewInstanceID())

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("ClaimServing = %v, want ErrNotFound", err)
	}
}

func TestServingReportsNoneWhenUnclaimed(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	got, err := s.Serving(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("Serving: %v", err)
	}
	if got != nil {
		t.Fatalf("serving = %q, want none", *got)
	}
}

func TestServingRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	_, err := s.Serving(context.Background(), WorkspaceID("absent"))

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("Serving = %v, want ErrNotFound", err)
	}
}

func TestReleaseServingGivesUpOwnership(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	instance := NewInstanceID()
	if err := s.ClaimServing(context.Background(), ws.ID, instance); err != nil {
		t.Fatalf("ClaimServing: %v", err)
	}

	// Act
	if err := s.ReleaseServing(context.Background(), ws.ID, instance); err != nil {
		t.Fatalf("ReleaseServing: %v", err)
	}

	// Assert
	got, err := s.Serving(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("Serving: %v", err)
	}
	if got != nil {
		t.Fatalf("serving = %q after release, want none", *got)
	}
}

func TestReleaseServingRefusesABystander(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s)
	owner := NewInstanceID()
	bystander := NewInstanceID()
	if err := s.ClaimServing(context.Background(), ws.ID, owner); err != nil {
		t.Fatalf("ClaimServing: %v", err)
	}

	// Act
	err := s.ReleaseServing(context.Background(), ws.ID, bystander)

	// Assert
	var refusal *ServingError
	if !errors.As(err, &refusal) {
		t.Fatalf("ReleaseServing = %v, want a *ServingError", err)
	}
	if refusal.Holder != owner || refusal.Claimant != bystander {
		t.Fatalf("refusal = %+v, want the real owner named", refusal)
	}
	if !loggedOperation(log, "daemon.wsm.release_serving", "error") {
		t.Fatalf("the refusal was not logged at error: %v", log.Records())
	}
}

func TestReleaseServingRefusesAnUnservedWorkspace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	err := s.ReleaseServing(context.Background(), ws.ID, NewInstanceID())

	// Assert
	var refusal *ServingError
	if !errors.As(err, &refusal) || refusal.Holder != "" {
		t.Fatalf("ReleaseServing = %v, want a *ServingError naming no holder", err)
	}
}
