package wsm

import (
	"context"
	"testing"
	"time"
)

func TestClaimResumptionsTakesWhatAStandDownOwed(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s).ID
	ctx := context.Background()
	for _, turn := range []TurnID{"t-a", "t-b"} {
		if err := s.OweResumption(ctx, ws, turn, time.Unix(100, 0)); err != nil {
			t.Fatalf("OweResumption(%s): %v", turn, err)
		}
	}

	// Act
	claimed, err := s.ClaimResumptions(ctx, ws)

	// Assert
	if err != nil || claimed != 2 {
		t.Fatalf("ClaimResumptions = %d, %v; want the 2 owed", claimed, err)
	}
}

func TestASecondClaimFindsNothingOwed(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s).ID
	ctx := context.Background()
	if err := s.OweResumption(ctx, ws, "t-a", time.Unix(100, 0)); err != nil {
		t.Fatalf("OweResumption: %v", err)
	}
	if _, err := s.ClaimResumptions(ctx, ws); err != nil {
		t.Fatalf("first ClaimResumptions: %v", err)
	}

	// Act
	claimed, err := s.ClaimResumptions(ctx, ws)

	// Assert
	if err != nil || claimed != 0 {
		t.Fatalf("second ClaimResumptions = %d, %v; want 0, the debt is paid once", claimed, err)
	}
}

func TestOwingTheSameTurnTwiceIsOneDebt(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s).ID
	ctx := context.Background()
	for i := 0; i < 2; i++ {
		if err := s.OweResumption(ctx, ws, "t-a", time.Unix(int64(100+i), 0)); err != nil {
			t.Fatalf("OweResumption #%d: %v", i, err)
		}
	}

	// Act
	claimed, err := s.ClaimResumptions(ctx, ws)

	// Assert
	if err != nil || claimed != 1 {
		t.Fatalf("ClaimResumptions = %d, %v; want 1", claimed, err)
	}
}

func TestClaimResumptionsTakesOnlyItsOwnWorkspaces(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	mine, other := testWorkspace(t, s).ID, testWorkspace(t, s).ID
	ctx := context.Background()
	if err := s.OweResumption(ctx, other, "t-other", time.Unix(100, 0)); err != nil {
		t.Fatalf("OweResumption: %v", err)
	}

	// Act
	claimed, err := s.ClaimResumptions(ctx, mine)

	// Assert
	if err != nil || claimed != 0 {
		t.Fatalf("ClaimResumptions(mine) = %d, %v; want 0", claimed, err)
	}
	if n, err := s.ClaimResumptions(ctx, other); err != nil || n != 1 {
		t.Fatalf("ClaimResumptions(other) = %d, %v; want its 1 left standing", n, err)
	}
}

func TestOwedTurnsAnswersWhatIsOwedWithoutClaimingIt(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s).ID
	ctx := context.Background()
	if err := s.OweResumption(ctx, ws, "t-a", time.Unix(100, 0)); err != nil {
		t.Fatalf("OweResumption: %v", err)
	}

	// Act
	owed, err := s.OwedTurns(ctx, ws)

	// Assert
	if err != nil || len(owed) != 1 || owed[0] != "t-a" {
		t.Fatalf("OwedTurns = %v, %v; want [t-a]", owed, err)
	}
	if n, err := s.ClaimResumptions(ctx, ws); err != nil || n != 1 {
		t.Fatalf("ClaimResumptions after the read = %d, %v; want the debt still standing", n, err)
	}
}
