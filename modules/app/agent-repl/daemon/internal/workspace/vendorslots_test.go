package workspace

import (
	"context"
	"errors"
	"testing"
)

func TestVendorSlotsLetNoMoreStartsInFlightThanTheirCap(t *testing.T) {
	// Arrange: both slots taken.
	slots := newVendorSlots(2)
	for i := 0; i < 2; i++ {
		if _, err := slots.acquire(context.Background()); err != nil {
			t.Fatalf("acquire %d: %v", i, err)
		}
	}
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	third := make(chan error, 1)

	// Act: a third start asks for a slot.
	go func() {
		_, err := slots.acquire(ctx)
		third <- err
	}()

	// Assert: it is still waiting while both slots are held.
	select {
	case err := <-third:
		t.Fatalf("a third start took a slot (%v) while two were in flight", err)
	default:
	}
	cancel()
	if err := <-third; !errors.Is(err, context.Canceled) {
		t.Fatalf("the waiting start ended on %v, want its context's end", err)
	}
}

func TestAReleasedVendorSlotAdmitsTheNextStart(t *testing.T) {
	// Arrange
	slots := newVendorSlots(1)
	release, err := slots.acquire(context.Background())
	if err != nil {
		t.Fatal(err)
	}
	next := make(chan error, 1)
	go func() {
		_, err := slots.acquire(context.Background())
		next <- err
	}()

	// Act
	release()

	// Assert
	if err := <-next; err != nil {
		t.Fatalf("the next start = %v, want a slot once one was released", err)
	}
}

func TestAWaitForAVendorSlotEndsWithTheRestartsCause(t *testing.T) {
	// Arrange
	slots := newVendorSlots(1)
	if _, err := slots.acquire(context.Background()); err != nil {
		t.Fatal(err)
	}
	ctx, cancel := context.WithCancelCause(context.Background())

	// Act
	cancel(ErrVendorStartCancelled)
	_, err := slots.acquire(ctx)

	// Assert
	if !errors.Is(err, ErrVendorStartCancelled) {
		t.Fatalf("acquire = %v, want the restart's cause", err)
	}
}

func TestThereAreAlwaysAtLeastTwoVendorStartSlots(t *testing.T) {
	// Act
	got := vendorStartSlots()

	// Assert
	if got < 2 {
		t.Fatalf("vendorStartSlots() = %d, want at least 2", got)
	}
}
