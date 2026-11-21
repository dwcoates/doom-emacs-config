package main

import (
	"context"
	"errors"
	"go/ast"
	"go/token"
	"io"
	"net"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"

	"agent-repld/internal/daemonaddr"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/server"
	"agent-repld/internal/sourcescan"
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

// holderServes is an ensure that finds the holder serving.
func holderServes(context.Context, string) error { return nil }

func TestSharedTakeEnsuresItsHolderBeforeItTakes(t *testing.T) {
	tests := []struct {
		name      string
		ensureErr error
		wantTakes int
		wantErr   bool
	}{
		{name: "a holder the ensure finds serving is taken from", wantTakes: 1},
		{name: "a holder the ensure cannot bring up is never taken from", ensureErr: errors.New("install failed"), wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			shared, err := net.Listen("tcp", "127.0.0.1:0")
			if err != nil {
				t.Fatal(err)
			}
			t.Cleanup(func() { shared.Close() })
			var order []string
			claim := &advertisingClaim{}
			s := &sharedTake{holder: "/h/daemon-holder.sock", subscribeFn: noReturns, accept: newPrivateAccept(t), claim: claim, log: &spyLogger{},
				ensure: func(_ context.Context, control string) error {
					order = append(order, "ensure "+control)
					return tt.ensureErr
				},
				takeFn: func(context.Context, string, string) (net.Listener, error) {
					order = append(order, "take")
					return shared, nil
				}}

			// Act
			err = s.take()

			// Assert
			takes := 0
			for _, o := range order {
				if o == "take" {
					takes++
				}
			}
			if (err != nil) != tt.wantErr || takes != tt.wantTakes || len(order) == 0 || order[0] != "ensure /h/daemon-holder.sock" {
				t.Fatalf("take = %v, order %v; want the ensure first and %d takes", err, order, tt.wantTakes)
			}
			if tt.wantErr && len(claim.advertised) != 0 {
				t.Fatalf("advertised %v, want nothing advertised", claim.advertised)
			}
		})
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
	s := &sharedTake{holder: "/h/daemon-holder.sock", subscribeFn: noReturns, accept: accept, claim: claim, log: log, ensure: holderServes,
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
	s := &sharedTake{holder: "/h/daemon-holder.sock", subscribeFn: noReturns, accept: newPrivateAccept(t), claim: claim, log: log, ensure: holderServes,
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

func TestAnAcquiredSharedListenerNamesItsAddressAndIsServedOnlyOnceTaken(t *testing.T) {
	// Arrange
	shared, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	claim := &advertisingClaim{}
	s := &sharedTake{holder: "/h/daemon-holder.sock", subscribeFn: noReturns, accept: newPrivateAccept(t), claim: claim, log: &spyLogger{}, ensure: holderServes,
		takeFn: func(context.Context, string, string) (net.Listener, error) { return shared, nil }}

	// Act
	acquireErr := s.acquire()
	acquired, advertisedBefore := s.address(), len(claim.advertised)
	takeErr := s.take()

	// Assert
	if acquireErr != nil || acquired != shared.Addr().String() || advertisedBefore != 0 {
		t.Fatalf("acquire = %v, address %q, advertised %d; want the shared address known and nothing advertised before the take", acquireErr, acquired, advertisedBefore)
	}
	if takeErr != nil || len(claim.advertised) != 1 {
		t.Fatalf("take = %v, advertised %v; want the shared address advertised once taken", takeErr, claim.advertised)
	}
}

func TestSharedTakeAnswersItsAddressOnlyOnceTaken(t *testing.T) {
	// Arrange
	shared, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	s := &sharedTake{holder: "/h/daemon-holder.sock", subscribeFn: noReturns, accept: newPrivateAccept(t), claim: &advertisingClaim{}, log: &spyLogger{}, ensure: holderServes,
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
	accept.AddShared(shared, newFakeReturned(), &spyLogger{})
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

func TestInstanceAddressAnswersWhereARunningDaemonIs(t *testing.T) {
	// Arrange: a state root where "elder" runs and records its address, and
	// "mute" runs and records none.
	addrPath := filepath.Join(t.TempDir(), "daemon.addr")
	elder, err := daemonaddr.HoldLife(addrPath, "elder")
	if err != nil {
		t.Fatal(err)
	}
	defer elder.Release()
	if err := elder.RecordAddress("127.0.0.1:5000"); err != nil {
		t.Fatal(err)
	}
	mute, err := daemonaddr.HoldLife(addrPath, "mute")
	if err != nil {
		t.Fatal(err)
	}
	defer mute.Release()
	cases := []struct {
		name      string
		instance  ids.InstanceID
		want      string
		wantAlive bool
		wantErr   string
	}{
		{name: "a daemon that runs answers its recorded address", instance: "elder", want: "127.0.0.1:5000", wantAlive: true},
		{name: "a daemon that no longer runs answers not alive, and no error", instance: "gone"},
		{name: "a daemon that runs and records no address is a violation", instance: "mute", wantErr: "records no address"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, alive, err := instanceAddress(addrPath, tc.instance)

			// Assert
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("instanceAddress = %v, want an error naming %q", err, tc.wantErr)
				}
				return
			}
			if err != nil || got != tc.want || alive != tc.wantAlive {
				t.Fatalf("instanceAddress = (%q, %v, %v), want (%q, %v)", got, alive, err, tc.want, tc.wantAlive)
			}
		})
	}
}

// TestOnlyInstanceAddressResolvesADaemonsPrivateAddress keeps the lookup of
// another daemon's private address in one place: a second site that probes
// and reads it by hand would judge "runs" and "records an address" apart from
// the one every forward rests on.
func TestOnlyInstanceAddressResolvesADaemonsPrivateAddress(t *testing.T) {
	// Act
	sites := sourcescan.Sites(t, "daemonaddr.InstanceAddress(")

	// Assert
	if len(sites) != 1 || sites[0] != "sharedtake.go" {
		t.Fatalf("daemonaddr.InstanceAddress is called from %v, want only instanceAddress in sharedtake.go", sites)
	}
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

// TestTheDaemonsHolderTakeIsSocketholdsTimedTake pins the one bounded, timed holder take: the daemon calls
// sockethold.TakeTimed and keeps no take bound of its own.
func TestTheDaemonsHolderTakeIsSocketholdsTimedTake(t *testing.T) {
	// Arrange
	calls, ownBound := 0, ""

	// Act
	sourcescan.EachSourceIn(t, ".", false, func(fset *token.FileSet, f *ast.File) {
		ast.Inspect(f, func(n ast.Node) bool {
			if v, ok := n.(*ast.ValueSpec); ok {
				for _, id := range v.Names {
					if strings.Contains(id.Name, "TakeBound") || strings.Contains(id.Name, "takeBound") {
						ownBound = fset.Position(id.Pos()).String()
					}
				}
			}
			if sourcescan.IsSelector(n, "sockethold", "TakeTimed") {
				calls++
			}
			return true
		})
	})

	// Assert
	if ownBound != "" || calls != 1 {
		t.Fatalf("own take bound at %q, %d sockethold.TakeTimed calls; want none of its own and exactly one call", ownBound, calls)
	}
}

// fakeReturned is a holder's accept stream the test feeds: Next answers each
// connection put on conns, then io.EOF once the stream is stopped.
type fakeReturned struct {
	conns chan net.Conn
	stop  chan struct{}
	once  sync.Once
}

func newFakeReturned() *fakeReturned {
	return &fakeReturned{conns: make(chan net.Conn, 4), stop: make(chan struct{})}
}

func (f *fakeReturned) Next() (net.Conn, error) {
	select {
	case c := <-f.conns:
		return c, nil
	case <-f.stop:
		return nil, io.EOF
	}
}

func (f *fakeReturned) Stop() error { f.once.Do(func() { close(f.stop) }); return nil }

func (f *fakeReturned) Close() error { return f.Stop() }

// noReturns opens a fake accept stream that hands back nothing.
func noReturns(context.Context, string, string) (server.Returned, error) {
	return newFakeReturned(), nil
}

// acceptedShared answers a client and the connection a SharedAccept took for
// it from the shared address, as a daemon's serve sees it.
func acceptedShared(t *testing.T) (net.Conn, net.Conn) {
	t.Helper()
	shared, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	accept := newPrivateAccept(t)
	accept.AddShared(shared, newFakeReturned(), &spyLogger{})
	client, err := net.Dial("tcp", shared.Addr().String())
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { client.Close() })
	conn, err := accept.Accept()
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { conn.Close() })
	return client, conn
}

func TestTheExitHandsBackAnUnreadSharedConnection(t *testing.T) {
	cases := []struct {
		name       string
		giveErr    error
		wantLevel  string
		wantMsg    string
		requestles bool
	}{
		{name: "the holder takes it", wantLevel: dlog.LevelInfo,
			wantMsg: "handed an unread connection of the shared address back to the holder, for the daemon accepting next"},
		{name: "the holder refuses it", giveErr: errors.New("the holder refused"), wantLevel: dlog.LevelError,
			wantMsg: "an unread connection of the shared address could not be handed back; it was closed", requestles: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: an accepted shared connection that delivered nothing.
			_, conn := acceptedShared(t)
			log := &spyLogger{}
			var gave int
			conns := &connStates{byConn: map[net.Conn]http.ConnState{}, born: map[net.Conn]time.Time{}, handed: map[net.Conn]bool{}, log: log,
				handBack: func(*os.File) error { gave++; return tc.giveErr }}
			conns.track(conn, http.StateNew)

			// Act
			conns.closeRequestless()
			conns.track(conn, http.StateClosed)

			// Assert
			if gave != 1 {
				t.Fatalf("handed back %d time(s), want once", gave)
			}
			if msg, ok := log.message(tc.wantLevel); !ok || msg != tc.wantMsg {
				t.Fatalf("records = %+v, want %q at %s", log.records, tc.wantMsg, tc.wantLevel)
			}
			recorded := false
			for _, r := range log.records {
				if r.operation == "daemon.server.conn_requestless" {
					recorded = true
				}
			}
			if recorded != tc.requestles {
				t.Fatalf("requestless recorded = %v, want %v: a handed-back connection is no lost call", recorded, tc.requestles)
			}
		})
	}
}

func TestTheExitClosesAPrivateConnectionThatDeliveredNothing(t *testing.T) {
	// Arrange
	ln, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	defer ln.Close()
	client, err := net.Dial("tcp", ln.Addr().String())
	if err != nil {
		t.Fatal(err)
	}
	defer client.Close()
	conn, err := ln.Accept()
	if err != nil {
		t.Fatal(err)
	}
	var gave int
	conns := &connStates{byConn: map[net.Conn]http.ConnState{}, born: map[net.Conn]time.Time{}, handed: map[net.Conn]bool{}, log: &spyLogger{},
		handBack: func(*os.File) error { gave++; return nil }}
	conns.track(conn, http.StateNew)

	// Act
	conns.closeRequestless()

	// Assert
	if gave != 0 {
		t.Fatalf("handed back %d private connection(s), want none", gave)
	}
	if _, err := conn.Read(make([]byte, 1)); err == nil {
		t.Fatal("the private connection is still open, want it closed")
	}
}

func TestTakeFailsWhenTheHoldersStreamCannotBeOpened(t *testing.T) {
	// Arrange
	shared, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { shared.Close() })
	log := &spyLogger{}
	claim := &advertisingClaim{}
	s := &sharedTake{holder: "/h/daemon-holder.sock", accept: newPrivateAccept(t), claim: claim, log: log, ensure: holderServes,
		takeFn: func(context.Context, string, string) (net.Listener, error) { return shared, nil },
		subscribeFn: func(context.Context, string, string) (server.Returned, error) {
			return nil, errors.New("sockethold: the holder refused the request")
		}}

	// Act
	err = s.take()

	// Assert
	if err == nil {
		t.Fatal("take = nil, want the stream's failure")
	}
	if msg, ok := log.message(dlog.LevelError); !ok || msg != "the holder's stream of handed-back connections could not be opened" {
		t.Fatalf("records = %+v, want the failure at ERROR", log.records)
	}
}

func TestHandBackAsksTheDaemonsHolder(t *testing.T) {
	// Arrange
	var control, name string
	s := &sharedTake{holder: "/h/daemon-holder.sock", log: &spyLogger{},
		returnFn: func(_ context.Context, c, n string, _ *os.File) error { control, name = c, n; return nil }}

	// Act
	err := s.handBack(os.Stdin)

	// Assert
	if err != nil || control != "/h/daemon-holder.sock" || name != holderDaemonSocket {
		t.Fatalf("handBack = %v to (%q, %q), want the daemon's socket at its holder", err, control, name)
	}
}
