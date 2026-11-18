package main

import (
	"context"
	"errors"
	"net"
	"path/filepath"
	"strings"
	"testing"

	"agent-repld/internal/daemonaddr"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
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

// yieldingClaim records whether it was yielded.
type yieldingClaim struct{ yielded bool }

func (c *yieldingClaim) Yield() error { c.yielded = true; return nil }

func TestYieldStopsTheSharedAcceptBeforeItYieldsTheClaim(t *testing.T) {
	// Arrange
	shared, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	accept := newPrivateAccept(t)
	accept.AddShared(shared)
	claim := &yieldingClaim{}
	s := &sharedTake{accept: accept, log: &spyLogger{}}

	// Act
	err = yieldBootClaim(claim, s)
	_, dialErr := net.Dial("tcp", shared.Addr().String())

	// Assert: the claim is yielded, and this daemon accepts on the shared
	// listener no more (its only copy here is closed, so the connect fails).
	if err != nil || !claim.yielded {
		t.Fatalf("yield = %v, yielded %v; want the claim yielded", err, claim.yielded)
	}
	if dialErr == nil {
		t.Fatal("the shared listener still accepts after the yield, want it stopped")
	}
}

// fakeServingRows answers one serving row.
type fakeServingRows struct {
	owner *ids.InstanceID
	err   error
}

func (f fakeServingRows) Serving(context.Context, ids.WorkspaceID) (*ids.InstanceID, error) {
	return f.owner, f.err
}

func TestServingElsewhereFollowsTheServingRow(t *testing.T) {
	// Arrange: a state root where instance "elder" lives and records its address.
	addrPath := filepath.Join(t.TempDir(), "daemon.addr")
	life, err := daemonaddr.HoldLife(addrPath, "elder")
	if err != nil {
		t.Fatal(err)
	}
	defer life.Release()
	if err := life.RecordAddress("127.0.0.1:5000"); err != nil {
		t.Fatal(err)
	}
	instance := func(s string) *ids.InstanceID { id := ids.InstanceID(s); return &id }
	cases := []struct {
		name    string
		rows    fakeServingRows
		want    string
		wantOK  bool
		wantErr string
	}{
		{name: "a released row is served here", rows: fakeServingRows{}},
		{name: "a row naming this daemon is served here", rows: fakeServingRows{owner: instance("self")}},
		{name: "a row naming a live elder is forwarded to its address", rows: fakeServingRows{owner: instance("elder")}, want: "127.0.0.1:5000", wantOK: true},
		{name: "a row naming a daemon not running is a violation", rows: fakeServingRows{owner: instance("gone")}, wantErr: "not running"},
		{name: "a row that cannot be read is a failure", rows: fakeServingRows{err: errors.New("database is locked")}, wantErr: "database is locked"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, ok, err := servingElsewhere(context.Background(), tc.rows, addrPath, "self", "ws-1")

			// Assert
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("servingElsewhere = %v, want an error naming %q", err, tc.wantErr)
				}
				return
			}
			if err != nil || ok != tc.wantOK || got != tc.want {
				t.Fatalf("servingElsewhere = (%q, %v, %v), want (%q, %v)", got, ok, err, tc.want, tc.wantOK)
			}
		})
	}
}
