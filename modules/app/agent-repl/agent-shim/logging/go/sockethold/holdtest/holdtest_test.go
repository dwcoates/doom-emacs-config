package holdtest

import (
	"context"
	"errors"
	"net"
	"os"
	"sync"
	"testing"

	"agentrepl/logging/sockethold"
)

func TestStartGrantsTheSocketsItHolds(t *testing.T) {
	// Arrange
	f, address := TCP(t)
	control := Start(t, map[string]*os.File{"daemon": f}, nil)

	// Act
	ln, err := sockethold.Take(context.Background(), control, "daemon")

	// Assert
	if err != nil || ln.Addr().String() != address {
		t.Fatalf("Take = (%v, %v), want the held listener at %s", ln, err, address)
	}
	ln.Close()
}

func TestStartRefusesASocketItDoesNotHoldAndTellsTheGrant(t *testing.T) {
	// Arrange
	f, _ := TCP(t)
	var mu sync.Mutex
	var grants []sockethold.Grant
	control := Start(t, map[string]*os.File{"daemon": f}, func(g sockethold.Grant) {
		mu.Lock()
		defer mu.Unlock()
		grants = append(grants, g)
	})

	// Act
	_, err := sockethold.Take(context.Background(), control, "store")

	// Assert
	mu.Lock()
	defer mu.Unlock()
	if !errors.Is(err, sockethold.ErrRefused) || len(grants) != 1 || grants[0].Name != "store" || grants[0].Err == nil {
		t.Fatalf("Take = %v, grants %+v; want the refusal, told once", err, grants)
	}
}

func TestTCPAnswersAListeningDescriptorAtItsAddress(t *testing.T) {
	// Arrange
	f, address := TCP(t)
	defer f.Close()

	// Act
	ln, err := net.FileListener(f)

	// Assert
	if err != nil || ln.Addr().String() != address {
		t.Fatalf("FileListener = (%v, %v), want a listener at %s", ln, err, address)
	}
	ln.Close()
}
