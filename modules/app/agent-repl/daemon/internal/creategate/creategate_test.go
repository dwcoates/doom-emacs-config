package creategate

import (
	"context"
	"errors"
	"testing"
	"time"
)

func closed(ch <-chan struct{}) bool {
	select {
	case <-ch:
		return true
	default:
		return false
	}
}

func TestAdmit(t *testing.T) {
	cases := []struct {
		name          string
		close         []string
		wantRefused   bool
		wantSuccessor string
	}{
		{name: "an open gate admits"},
		{name: "a gate closed for a successor refuses naming it", close: []string{"127.0.0.1:9"}, wantRefused: true, wantSuccessor: "127.0.0.1:9"},
		{name: "a gate closed with none listening refuses naming none", close: []string{""}, wantRefused: true},
		{name: "a second close keeps the first successor", close: []string{"127.0.0.1:9", ""}, wantRefused: true, wantSuccessor: "127.0.0.1:9"},
		{name: "a close names a successor a closed gate lacked", close: []string{"", "127.0.0.1:9"}, wantRefused: true, wantSuccessor: "127.0.0.1:9"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			g := New()
			for _, successor := range tc.close {
				g.Close(successor)
			}

			// Act
			_, release, err := g.Admit(context.Background())
			defer release()

			// Assert
			var refused *ClosedError
			if got := errors.As(err, &refused); got != tc.wantRefused {
				t.Fatalf("Admit error = %v, want refused %v", err, tc.wantRefused)
			}
			if tc.wantRefused {
				if refused.Successor != tc.wantSuccessor || g.InFlight() != 0 {
					t.Fatalf("refused with successor %q and %d in flight, want %q and none", refused.Successor, g.InFlight(), tc.wantSuccessor)
				}
				return
			}
			if g.InFlight() != 1 {
				t.Fatalf("InFlight = %d, want 1", g.InFlight())
			}
		})
	}
}

func TestReleaseCountsOnce(t *testing.T) {
	// Arrange
	g := New()
	_, release, _ := g.Admit(context.Background())

	// Act
	release()
	release()

	// Assert
	if g.InFlight() != 0 {
		t.Fatalf("InFlight = %d, want 0", g.InFlight())
	}
}

func TestATicketedContextIsNotEnteredTwice(t *testing.T) {
	cases := []struct {
		name  string
		enter func(g *Gate, ctx context.Context) (context.Context, func(), error)
	}{
		{name: "admit", enter: (*Gate).Admit},
		{name: "hold", enter: (*Gate).Hold},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: a ticket, then the gate closes.
			g := New()
			ticketed, release, _ := g.Admit(context.Background())
			defer release()
			g.Close("")

			// Act
			_, nested, err := tc.enter(g, ticketed)
			nested()

			// Assert: the create already admitted runs on, counted once.
			if err != nil || g.InFlight() != 1 {
				t.Fatalf("nested enter = %v with %d in flight, want no error and 1", err, g.InFlight())
			}
		})
	}
}

func TestHoldOnAClosedGateEndsWithItsContext(t *testing.T) {
	// Arrange
	g := New()
	g.Close("")
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act
	_, _, err := g.Hold(ctx)

	// Assert: nothing entered, and the cause is the context's.
	if !errors.Is(err, context.Canceled) || g.InFlight() != 0 {
		t.Fatalf("Hold = %v with %d in flight, want context.Canceled and none", err, g.InFlight())
	}
}

func TestHoldStartsOnceTheGateReopens(t *testing.T) {
	// Arrange
	g := New()
	g.Close("127.0.0.1:9")
	entered := make(chan error, 1)
	go func() {
		_, _, err := g.Hold(context.Background())
		entered <- err
	}()

	// Act
	g.Reopen()

	// Assert
	if err := <-entered; err != nil || g.InFlight() != 1 {
		t.Fatalf("Hold = %v with %d in flight, want entered", err, g.InFlight())
	}
}

func TestReopenAdmitsAgainWithNoSuccessor(t *testing.T) {
	// Arrange
	g := New()
	g.Close("127.0.0.1:9")

	// Act
	g.Reopen()
	_, release, err := g.Admit(context.Background())
	defer release()
	g.Close("")
	_, _, refused := g.Admit(context.Background())

	// Assert: admitted while open, and the next close names no stale successor.
	var closedErr *ClosedError
	if err != nil || !errors.As(refused, &closedErr) || closedErr.Successor != "" {
		t.Fatalf("Admit after reopen = %v, then %v; want admitted, then refused naming none", err, refused)
	}
}

func TestIdle(t *testing.T) {
	// Arrange
	g := New()
	if !closed(g.Idle()) {
		t.Fatal("Idle is open with nothing in flight, want closed")
	}
	_, release, _ := g.Admit(context.Background())
	idle := g.Idle()
	if closed(idle) {
		t.Fatal("Idle is closed with a create in flight, want open")
	}

	// Act
	release()

	// Assert
	if !closed(idle) {
		t.Fatal("Idle stayed open after the last create left")
	}
}

func TestAwaitIdle(t *testing.T) {
	cases := []struct {
		name      string
		inFlight  bool
		cancel    bool
		wantErr   error
		wantNamed bool
	}{
		{name: "nothing in flight answers at once", wantErr: nil},
		{name: "a create in flight is named on the cadence and awaited", inFlight: true, wantNamed: true},
		{name: "an ended context ends the wait", inFlight: true, cancel: true, wantErr: context.Canceled},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			g := New()
			release := func() {}
			if tc.inFlight {
				_, release, _ = g.Admit(context.Background())
			}
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			if tc.cancel {
				cancel()
			}
			ticks := make(chan time.Time, 1)
			asked := make(chan struct{}, 4)
			after := func(time.Duration) <-chan time.Time { asked <- struct{}{}; return ticks }
			var named []int
			done := make(chan error, 1)

			// Act
			go func() {
				done <- g.AwaitIdle(ctx, time.Minute, after, func(warnings, inFlight int) { named = append(named, warnings, inFlight) })
			}()
			if tc.wantNamed {
				<-asked
				ticks <- time.Time{}
				<-asked
				release()
			}
			err := <-done
			release()

			// Assert
			if !errors.Is(err, tc.wantErr) && err != tc.wantErr {
				t.Fatalf("AwaitIdle = %v, want %v", err, tc.wantErr)
			}
			if tc.wantNamed && (len(named) != 2 || named[0] != 1 || named[1] != 1) {
				t.Fatalf("holdout told %v, want (1 warning, 1 in flight)", named)
			}
		})
	}
}
