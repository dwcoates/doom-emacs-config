package main

import (
	"context"
	"errors"
	"net"
	"strings"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/server"
)

// advertisingClaim records what the shared take advertises.
type advertisingClaim struct{ advertised []string }

func (c *advertisingClaim) Advertise(address string) { c.advertised = append(c.advertised, address) }

func newPrivateAccept(t *testing.T) *server.SharedAccept {
	t.Helper()
	private, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	a := server.NewSharedAccept(private)
	t.Cleanup(func() { a.Close() })
	return a
}

func TestSharedTakeWithNoHolderDoesNothing(t *testing.T) {
	// Arrange
	claim := &advertisingClaim{}
	s := &sharedTake{accept: newPrivateAccept(t), claim: claim, log: &spyLogger{},
		takeFn: func(context.Context, string, string) (net.Listener, error) {
			t.Fatal("took with no holder")
			return nil, nil
		}}

	// Act
	err := s.take()

	// Assert
	if err != nil || len(claim.advertised) != 0 {
		t.Fatalf("take = %v, advertised %v; want nothing done", err, claim.advertised)
	}
}

func TestSharedTakeAdvertisesAndAcceptsOnTheHoldersListenerOnce(t *testing.T) {
	// Arrange
	shared, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	claim := &advertisingClaim{}
	accept := newPrivateAccept(t)
	log := &spyLogger{}
	takes := 0
	s := &sharedTake{holder: "/h/daemon-holder.sock", accept: accept, claim: claim, log: log,
		takeFn: func(_ context.Context, control, name string) (net.Listener, error) {
			takes++
			if control != "/h/daemon-holder.sock" || name != holderDaemonSocket {
				t.Fatalf("took %q from %q", name, control)
			}
			return shared, nil
		}}

	// Act
	first, second := s.take(), s.take()
	client, dialErr := net.Dial("tcp", shared.Addr().String())
	conn, acceptErr := accept.Accept()

	// Assert
	if first != nil || second != nil || takes != 1 {
		t.Fatalf("takes = %d (%v, %v), want one", takes, first, second)
	}
	if len(claim.advertised) != 1 || claim.advertised[0] != shared.Addr().String() {
		t.Fatalf("advertised %v, want the shared address once", claim.advertised)
	}
	if dialErr != nil || acceptErr != nil {
		t.Fatalf("a connect to the shared address = %v, %v; want it accepted", dialErr, acceptErr)
	}
	client.Close()
	conn.Close()
	if _, ok := log.message(dlog.LevelInfo); !ok {
		t.Fatalf("records = %+v, want the take recorded", log.records)
	}
}

func TestSharedTakeFailsWhenTheHolderCannotGrant(t *testing.T) {
	// Arrange
	claim := &advertisingClaim{}
	log := &spyLogger{}
	s := &sharedTake{holder: "/h/daemon-holder.sock", accept: newPrivateAccept(t), claim: claim, log: log,
		takeFn: func(context.Context, string, string) (net.Listener, error) {
			return nil, errors.New("connection refused")
		}}

	// Act
	err := s.take()

	// Assert
	if err == nil || len(claim.advertised) != 0 {
		t.Fatalf("take = %v, advertised %v; want the failure and nothing advertised", err, claim.advertised)
	}
	if msg, ok := log.message(dlog.LevelError); !ok || !strings.Contains(msg, "shared listener") {
		t.Fatalf("records = %+v, want an ERROR naming the shared listener", log.records)
	}
}

func TestSharedTakeAnswersItsAddressOnlyOnceTaken(t *testing.T) {
	// Arrange
	shared, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	s := &sharedTake{holder: "/h/daemon-holder.sock", accept: newPrivateAccept(t), claim: &advertisingClaim{}, log: &spyLogger{},
		takeFn: func(context.Context, string, string) (net.Listener, error) { return shared, nil }}

	// Act
	before := s.address()
	takeErr := s.take()
	after := s.address()

	// Assert
	if before != "" || takeErr != nil || after != shared.Addr().String() {
		t.Fatalf("address before %q, after %q (%v); want none, then the shared address", before, after, takeErr)
	}
	if (*sharedTake)(nil).address() != "" {
		t.Fatal("no take answers no address")
	}
}
