package prompthandler

import (
	"context"
	"errors"
	"testing"

	"claude-repld/internal/ids"
)

// TestInflightAcquireTakesAFreeKeyAtOnce pins the ordinary case: nothing else
// holds the key, so it is taken without waiting.
func TestInflightAcquireTakesAFreeKeyAtOnce(t *testing.T) {
	tests := []struct {
		name     string
		otherWS  ids.WorkspaceID
		otherKey string
	}{
		{name: "no other key is held"},
		{name: "another key is held in the workspace", otherWS: theWorkspace, otherKey: "key-2"},
		{name: "the same key is held in another workspace", otherWS: "ws-2", otherKey: "key-1"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newInflight()
			if tc.otherKey != "" {
				release, _, err := f.acquire(context.Background(), tc.otherWS, tc.otherKey)
				if err != nil {
					t.Fatalf("arrange acquire: %v", err)
				}
				defer release()
			}

			// Act
			release, waited, err := f.acquire(context.Background(), theWorkspace, "key-1")

			// Assert
			if err != nil {
				t.Fatalf("acquire: %v", err)
			}
			defer release()
			if waited {
				t.Fatal("acquire waited for a key nobody held")
			}
		})
	}
}

// TestInflightAcquireWaitsForTheHolder pins that a second submission of a key
// takes it only once the first releases it.
func TestInflightAcquireWaitsForTheHolder(t *testing.T) {
	// Arrange
	f := newInflight()
	first, _, err := f.acquire(context.Background(), theWorkspace, "key-1")
	if err != nil {
		t.Fatalf("arrange acquire: %v", err)
	}
	type result struct {
		release func()
		err     error
	}
	second := make(chan result, 1)
	go func() {
		release, _, err := f.acquire(context.Background(), theWorkspace, "key-1")
		second <- result{release: release, err: err}
	}()

	// Act
	first()
	got := <-second

	// Assert
	if got.err != nil {
		t.Fatalf("acquire after the release: %v", got.err)
	}
	got.release()
}

// TestInflightAcquireGivesUpWithItsCaller pins that a waiter whose context ends
// answers that context's error and takes nothing.
func TestInflightAcquireGivesUpWithItsCaller(t *testing.T) {
	// Arrange
	f := newInflight()
	first, _, err := f.acquire(context.Background(), theWorkspace, "key-1")
	if err != nil {
		t.Fatalf("arrange acquire: %v", err)
	}
	defer first()
	gaveUp, cancel := context.WithCancel(context.Background())
	cancel()

	// Act
	_, waited, err := f.acquire(gaveUp, theWorkspace, "key-1")

	// Assert
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("err = %v, want the caller's cancellation", err)
	}
	if !waited {
		t.Fatal("waited = false for a key another submission held")
	}
}

// TestClaimDoneWithoutAKeyIsANoOp pins that a claim that took no key -- a
// keyless submission, or a model change that mints nothing -- releases
// nothing.
func TestClaimDoneWithoutAKeyIsANoOp(t *testing.T) {
	// Arrange
	c := claim{turn: "turn-1"}

	// Act / Assert: a nil release must not panic.
	c.done()
}
