package daemonaddr

import (
	"context"
	"errors"
	"net"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
	"time"
)

// newAddrPath is a daemon.addr path inside a fresh temp state root.
func newAddrPath(t *testing.T) string {
	t.Helper()
	return filepath.Join(t.TempDir(), "daemon.addr")
}

// mustBind binds a claim and closes it at the end of the test.
func mustBind(t *testing.T, addrPath string) Claim {
	t.Helper()
	c, err := Bind(addrPath, 0)
	if err != nil {
		t.Fatalf("Bind: %v", err)
	}
	t.Cleanup(func() { c.Close() })
	return c
}

// readAddrFile reads daemon.addr's advertised address directly, without
// going through any production parsing helper, for tests that verify
// Publish's on-disk side effect rather than a reader's own behavior. The
// address is the first line; a "pid=<n>" line may follow it.
func readAddrFile(t *testing.T, addrPath string) string {
	t.Helper()
	raw, err := os.ReadFile(addrPath)
	if err != nil {
		t.Fatalf("read %s: %v", addrPath, err)
	}
	first, _, _ := strings.Cut(string(raw), "\n")
	return strings.TrimSpace(first)
}

func TestBindListensOnLoopback(t *testing.T) {
	// Arrange, Act.
	c := mustBind(t, newAddrPath(t))

	// Assert.
	host, port, err := net.SplitHostPort(c.Address())
	if err != nil {
		t.Fatalf("SplitHostPort(%q): %v", c.Address(), err)
	}
	if host != LoopbackHost {
		t.Fatalf("host = %q, want %q", host, LoopbackHost)
	}
	if port == "0" {
		t.Fatalf("Address = %q, want the actually-bound port", c.Address())
	}
	if c.Listener().Addr().String() != c.Address() {
		t.Fatalf("Listener addr %q != Address %q", c.Listener().Addr(), c.Address())
	}
}

func TestBindPublishesNothingByItself(t *testing.T) {
	// Arrange.
	addrPath := newAddrPath(t)

	// Act.
	mustBind(t, addrPath)

	// Assert: a joining daemon writes daemon.addr only once it owns every
	// workspace, so binding must not advertise.
	if _, err := os.Stat(addrPath); !os.IsNotExist(err) {
		t.Fatalf("Bind published daemon.addr (stat err = %v)", err)
	}
}

func TestSecondBindLosesTheClaim(t *testing.T) {
	// Arrange.
	addrPath := newAddrPath(t)
	mustBind(t, addrPath)

	// Act: no wait, because the incumbent here is not departing -- the
	// production bound is exercised in lock_test.go.
	loser, err := BindWithin(addrPath, 0, 0)

	// Assert.
	if err == nil {
		loser.Close()
		t.Fatalf("an unflagged second daemon took the claim")
	}
	if !errors.Is(err, ErrClaimed) {
		t.Fatalf("error = %v, want ErrClaimed", err)
	}
}

func TestALosingBindLeavesTheIncumbentsListenerAlone(t *testing.T) {
	// Arrange.
	addrPath := newAddrPath(t)
	incumbent := mustBind(t, addrPath)

	// Act.
	if _, err := BindWithin(addrPath, 0, 0); err == nil {
		t.Fatalf("the second bind was supposed to lose")
	}

	// Assert: the incumbent still serves.
	conn, err := net.Dial("tcp", incumbent.Address())
	if err != nil {
		t.Fatalf("the incumbent's listener was disturbed: %v", err)
	}
	conn.Close()
}

func TestALosingBindLeavesTheIncumbentsAdvertisementAlone(t *testing.T) {
	// Arrange.
	addrPath := newAddrPath(t)
	incumbent := mustBind(t, addrPath)
	if err := incumbent.Publish(); err != nil {
		t.Fatalf("Publish: %v", err)
	}

	// Act.
	if _, err := BindWithin(addrPath, 0, 0); err == nil {
		t.Fatalf("the second bind was supposed to lose")
	}

	// Assert.
	got := readAddrFile(t, addrPath)
	if got != incumbent.Address() {
		t.Fatalf("daemon.addr = %q, want the incumbent's %q", got, incumbent.Address())
	}
}

func TestPublishWritesTheAddressAndPid(t *testing.T) {
	// Arrange.
	addrPath := newAddrPath(t)
	c := mustBind(t, addrPath)

	// Act.
	if err := c.Publish(); err != nil {
		t.Fatalf("Publish: %v", err)
	}

	// Assert: the address on the first line, this daemon's pid on the second.
	raw, err := os.ReadFile(addrPath)
	if err != nil {
		t.Fatalf("read: %v", err)
	}
	want := c.Address() + "\npid=" + strconv.Itoa(os.Getpid()) + "\n"
	if string(raw) != want {
		t.Fatalf("daemon.addr = %q, want %q", raw, want)
	}
}

// TestReadAdvertisementParsesTheAddressAndPid pins the reader against what
// Publish writes: both the address and the advertiser's pid come back.
func TestReadAdvertisementParsesTheAddressAndPid(t *testing.T) {
	// Arrange.
	addrPath := newAddrPath(t)
	c := mustBind(t, addrPath)
	if err := c.Publish(); err != nil {
		t.Fatalf("Publish: %v", err)
	}

	// Act.
	adv, err := ReadAdvertisement(addrPath)
	if err != nil {
		t.Fatalf("ReadAdvertisement: %v", err)
	}

	// Assert.
	if adv.Address != c.Address() {
		t.Fatalf("Address = %q, want %q", adv.Address, c.Address())
	}
	if !adv.PIDKnown {
		t.Fatal("PIDKnown = false, want the pid Publish wrote")
	}
	if adv.PID != os.Getpid() {
		t.Fatalf("PID = %d, want %d", adv.PID, os.Getpid())
	}
}

// TestReadAdvertisementReadsALegacyBareAddressAsPidUnknown pins the forward
// compatibility: a file a legacy daemon wrote -- a bare host:port -- reads as
// an address whose advertiser is unknown, never a guessed pid.
func TestReadAdvertisementReadsALegacyBareAddressAsPidUnknown(t *testing.T) {
	// Arrange.
	addrPath := newAddrPath(t)
	if err := os.MkdirAll(filepath.Dir(addrPath), 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	if err := os.WriteFile(addrPath, []byte("127.0.0.1:58161\n"), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}

	// Act.
	adv, err := ReadAdvertisement(addrPath)
	if err != nil {
		t.Fatalf("ReadAdvertisement: %v", err)
	}

	// Assert.
	if adv.Address != "127.0.0.1:58161" {
		t.Fatalf("Address = %q, want the advertised one", adv.Address)
	}
	if adv.PIDKnown {
		t.Fatalf("PIDKnown = true (pid %d), want a legacy file to name no pid", adv.PID)
	}
}

// TestReadAdvertisementSurfacesAReadError pins the one failure ReadAdvertisement
// can have: an absent file is a read error the caller must see, never an empty
// advertisement it might mistake for a named one.
func TestReadAdvertisementSurfacesAReadError(t *testing.T) {
	// Arrange, Act.
	_, err := ReadAdvertisement(newAddrPath(t))

	// Assert.
	if err == nil {
		t.Fatal("ReadAdvertisement of an absent file = nil error, want the read error surfaced")
	}
}

func TestPublishReplacesAStaleAdvertisement(t *testing.T) {
	// Arrange.
	addrPath := newAddrPath(t)
	if err := os.MkdirAll(filepath.Dir(addrPath), 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	if err := os.WriteFile(addrPath, []byte("127.0.0.1:1\n"), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	c := mustBind(t, addrPath)

	// Act.
	if err := c.Publish(); err != nil {
		t.Fatalf("Publish: %v", err)
	}

	// Assert.
	got := readAddrFile(t, addrPath)
	if got != c.Address() {
		t.Fatalf("daemon.addr = %q, want %q", got, c.Address())
	}
}

func TestPublishLeavesNoTemporaryFileBehind(t *testing.T) {
	// Arrange.
	addrPath := newAddrPath(t)
	c := mustBind(t, addrPath)

	// Act.
	if err := c.Publish(); err != nil {
		t.Fatalf("Publish: %v", err)
	}

	// Assert.
	entries, err := os.ReadDir(filepath.Dir(addrPath))
	if err != nil {
		t.Fatalf("readdir: %v", err)
	}
	for _, e := range entries {
		if strings.HasPrefix(e.Name(), ".daemon.addr.") {
			t.Fatalf("temporary file %q was left behind", e.Name())
		}
	}
}

func TestWithdrawRemovesTheAdvertisement(t *testing.T) {
	// Arrange.
	addrPath := newAddrPath(t)
	c := mustBind(t, addrPath)
	if err := c.Publish(); err != nil {
		t.Fatalf("Publish: %v", err)
	}

	// Act.
	withdrawn, err := c.Withdraw()
	if err != nil {
		t.Fatalf("Withdraw: %v", err)
	}

	// Assert.
	if !withdrawn {
		t.Fatal("Withdraw reported nothing removed, want its own advertisement withdrawn")
	}
	if _, err := os.Stat(addrPath); !os.IsNotExist(err) {
		t.Fatalf("daemon.addr survived Withdraw (stat err = %v)", err)
	}
}

func TestWithdrawingAnAbsentAdvertisementIsSuccess(t *testing.T) {
	// Arrange: an orderly exit that never published.
	c := mustBind(t, newAddrPath(t))

	// Act.
	withdrawn, err := c.Withdraw()

	// Assert.
	if err != nil {
		t.Fatalf("Withdraw = %v, want success", err)
	}
	if withdrawn {
		t.Fatal("Withdraw reported a removal, want none: nothing was ever published")
	}
}

// TestWithdrawLeavesASuccessorsAdvertisementAlone pins the handover's end: the
// successor publishes its own address into this same path and the incumbent
// exits afterwards, so an unconditional remove here would leave the state root
// naming no daemon while a daemon serves.
func TestWithdrawLeavesASuccessorsAdvertisementAlone(t *testing.T) {
	// Arrange.
	addrPath := newAddrPath(t)
	incumbent := mustBind(t, addrPath)
	if err := incumbent.Publish(); err != nil {
		t.Fatalf("Publish: %v", err)
	}
	if err := os.WriteFile(addrPath, []byte("127.0.0.1:65000\n"), 0o644); err != nil {
		t.Fatalf("write the successor's advertisement: %v", err)
	}

	// Act.
	withdrawn, err := incumbent.Withdraw()

	// Assert.
	if err != nil {
		t.Fatalf("Withdraw = %v, want success", err)
	}
	if withdrawn {
		t.Fatal("Withdraw removed an address it does not own")
	}
	if got := readAddrFile(t, addrPath); got != "127.0.0.1:65000" {
		t.Fatalf("daemon.addr = %q, want the successor's address", got)
	}
}

// TestStaleAdvertisementNamesAnAddressNobodyAnswers pins the boot's evidence:
// a predecessor that exited without withdrawing left an address the next
// client would dial and time out on.
func TestStaleAdvertisementNamesAnAddressNobodyAnswers(t *testing.T) {
	// Arrange.
	addrPath := newAddrPath(t)
	if err := os.WriteFile(addrPath, []byte("127.0.0.1:58161\n"), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}

	// Act.
	addr, stale := staleAdvertisement(addrPath, func(string) error { return errors.New("connection refused") })

	// Assert.
	if !stale {
		t.Fatal("staleAdvertisement reported a live advertisement, want it stale")
	}
	if addr != "127.0.0.1:58161" {
		t.Fatalf("stale address = %q, want the advertised one", addr)
	}
}

// TestAnAnsweringAdvertisementIsNotStale is the other arm: an address a
// listener answers on is never reported as stale.
func TestAnAnsweringAdvertisementIsNotStale(t *testing.T) {
	// Arrange.
	addrPath := newAddrPath(t)
	if err := os.WriteFile(addrPath, []byte("127.0.0.1:58161\n"), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}

	// Act.
	addr, stale := staleAdvertisement(addrPath, func(string) error { return nil })

	// Assert.
	if stale {
		t.Fatal("staleAdvertisement reported an answering address stale")
	}
	if addr != "127.0.0.1:58161" {
		t.Fatalf("address = %q, want the advertised one", addr)
	}
}

// TestAnAbsentAdvertisementIsNotStale pins the ordinary first boot: there is
// nothing to warn about, so nothing is reported.
func TestAnAbsentAdvertisementIsNotStale(t *testing.T) {
	// Arrange, Act.
	addr, stale := staleAdvertisement(newAddrPath(t), func(string) error {
		t.Fatal("the probe was called for an absent advertisement")
		return nil
	})

	// Assert.
	if stale || addr != "" {
		t.Fatalf("staleAdvertisement of an absent file = (%q, %v), want (\"\", false)", addr, stale)
	}
}

// TestAnEmptyAdvertisementIsNotStale pins the half-written file the atomic
// publish cannot produce but a foreign writer can: there is no address to
// probe, so nothing is reported rather than an empty one.
func TestAnEmptyAdvertisementIsNotStale(t *testing.T) {
	// Arrange.
	addrPath := newAddrPath(t)
	if err := os.WriteFile(addrPath, []byte("\n"), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}

	// Act.
	addr, stale := staleAdvertisement(addrPath, func(string) error {
		t.Fatal("the probe was called for an empty advertisement")
		return nil
	})

	// Assert.
	if stale || addr != "" {
		t.Fatalf("staleAdvertisement of an empty file = (%q, %v), want (\"\", false)", addr, stale)
	}
}

func TestCloseDoesNotWithdraw(t *testing.T) {
	// Arrange: a handover closes the incumbent's listener while the
	// successor's advertisement already stands.
	addrPath := newAddrPath(t)
	c, err := Bind(addrPath, 0)
	if err != nil {
		t.Fatalf("Bind: %v", err)
	}
	if err := c.Publish(); err != nil {
		t.Fatalf("Publish: %v", err)
	}

	// Act.
	if err := c.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Assert.
	if _, err := os.Stat(addrPath); err != nil {
		t.Fatalf("Close withdrew the advertisement: %v", err)
	}
}

func TestCloseEndsTheClaim(t *testing.T) {
	// Arrange.
	addrPath := newAddrPath(t)
	first, err := Bind(addrPath, 0)
	if err != nil {
		t.Fatalf("Bind: %v", err)
	}

	// Act.
	if err := first.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}
	second, err := Bind(addrPath, 0)

	// Assert.
	if err != nil {
		t.Fatalf("the successor could not take the freed claim: %v", err)
	}
	second.Close()
}

func TestCloseStopsTheListener(t *testing.T) {
	// Arrange.
	c, err := Bind(newAddrPath(t), 0)
	if err != nil {
		t.Fatalf("Bind: %v", err)
	}
	address := c.Address()

	// Act.
	if err := c.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Assert.
	conn, err := net.Dial("tcp", address)
	if err == nil {
		conn.Close()
		t.Fatalf("the listener still accepts connections after Close")
	}
}

func TestBindRefusesAnEmptyAddrPath(t *testing.T) {
	// Arrange, Act.
	c, err := Bind("", 0)

	// Assert.
	if err == nil {
		c.Close()
		t.Fatalf("Bind accepted an empty daemon.addr path")
	}
}

func TestBindRefusesAnOutOfRangePort(t *testing.T) {
	tests := []struct {
		name string
		port int
	}{
		{name: "negative", port: -1},
		{name: "above the range", port: 65536},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			c, err := Bind(newAddrPath(t), tc.port)

			// Assert.
			if err == nil {
				c.Close()
				t.Fatalf("Bind accepted port %d", tc.port)
			}
		})
	}
}

// TestAJoiningBindDoesNotTakeTheBootClaim covers the successor's whole
// premise: the INCUMBENT holds the claim for as long as it serves, so a
// successor that raced for it would lose to its own predecessor and exit.
func TestAJoiningBindDoesNotTakeTheBootClaim(t *testing.T) {
	// Arrange: an incumbent holding the claim.
	path := filepath.Join(t.TempDir(), "daemon.addr")
	incumbent, err := Bind(path, 0)
	if err != nil {
		t.Fatalf("Bind: %v", err)
	}
	defer incumbent.Close()

	// Act.
	successor, err := BindJoining(path, 0)

	// Assert.
	if err != nil {
		t.Fatalf("BindJoining alongside an incumbent = %v, want a bound listener", err)
	}
	defer successor.Close()
	if successor.Address() == incumbent.Address() {
		t.Fatalf("the successor bound %q, the incumbent's own address", successor.Address())
	}
}

// TestAJoiningClaimTakesTheBootClaimWhenItAdvertises covers the other half: a
// successor becomes the daemon of the state root at Publish, and taking the
// claim is what that means.
func TestAJoiningClaimTakesTheBootClaimWhenItAdvertises(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "daemon.addr")
	successor, err := BindJoining(path, 0)
	if err != nil {
		t.Fatalf("BindJoining: %v", err)
	}
	defer successor.Close()

	// Act.
	if err := successor.Publish(); err != nil {
		t.Fatalf("Publish: %v", err)
	}

	// Assert: nobody else can take the claim now.
	if _, err := BindWithin(path, 0, 0); !errors.Is(err, ErrClaimed) {
		t.Fatalf("Bind after the successor advertised = %v, want ErrClaimed", err)
	}
}

func TestVerify(t *testing.T) {
	cases := []struct {
		name    string
		joining bool
		publish bool
		// mutate changes the state root after the claim is bound (and
		// published, when publish is set).
		mutate func(t *testing.T, c Claim, addrPath string)
		// want is a substring of the loss; empty means Verify must pass.
		want string
	}{
		{
			name:    "an intact published claim verifies",
			publish: true,
			mutate:  func(*testing.T, Claim, string) {},
		},
		{
			name:    "a removed state root is a loss",
			publish: true,
			mutate: func(t *testing.T, _ Claim, addrPath string) {
				if err := os.RemoveAll(filepath.Dir(addrPath)); err != nil {
					t.Fatalf("RemoveAll: %v", err)
				}
			},
			want: "the state root",
		},
		{
			name:    "a state root recreated at the same path is a loss",
			publish: true,
			mutate: func(t *testing.T, _ Claim, addrPath string) {
				dir := filepath.Dir(addrPath)
				if err := os.RemoveAll(dir); err != nil {
					t.Fatalf("RemoveAll: %v", err)
				}
				if err := os.Mkdir(dir, 0o755); err != nil {
					t.Fatalf("Mkdir: %v", err)
				}
			},
			want: "was replaced by another directory",
		},
		{
			name: "a removed daemon.lock is a loss",
			mutate: func(t *testing.T, _ Claim, addrPath string) {
				if err := os.Remove(LockPath(addrPath)); err != nil {
					t.Fatalf("Remove: %v", err)
				}
			},
			want: "daemon.lock",
		},
		{
			name: "a daemon.lock replaced by another file is a loss",
			mutate: func(t *testing.T, _ Claim, addrPath string) {
				if err := os.Remove(LockPath(addrPath)); err != nil {
					t.Fatalf("Remove: %v", err)
				}
				if err := os.WriteFile(LockPath(addrPath), nil, 0o644); err != nil {
					t.Fatalf("WriteFile: %v", err)
				}
			},
			want: "was replaced by another file",
		},
		{
			name:    "a removed daemon.addr is a loss while published",
			publish: true,
			mutate: func(t *testing.T, _ Claim, addrPath string) {
				if err := os.Remove(addrPath); err != nil {
					t.Fatalf("Remove: %v", err)
				}
			},
			want: "daemon.addr",
		},
		{
			name:    "a daemon.addr naming another address is a loss while published",
			publish: true,
			mutate: func(t *testing.T, _ Claim, addrPath string) {
				if err := os.WriteFile(addrPath, []byte("127.0.0.1:1\n"), 0o644); err != nil {
					t.Fatalf("WriteFile: %v", err)
				}
			},
			want: "127.0.0.1:1",
		},
		{
			name:   "an unpublished claim owes no daemon.addr",
			mutate: func(*testing.T, Claim, string) {},
		},
		{
			name:    "a withdrawn claim owes no daemon.addr",
			publish: true,
			mutate: func(t *testing.T, c Claim, _ string) {
				if _, err := c.Withdraw(); err != nil {
					t.Fatalf("Withdraw: %v", err)
				}
			},
		},
		{
			name:    "a joining claim owes no daemon.lock before it publishes",
			joining: true,
			mutate:  func(*testing.T, Claim, string) {},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			addrPath := newAddrPath(t)
			bind := Bind
			if tc.joining {
				bind = BindJoining
			}
			c, err := bind(addrPath, 0)
			if err != nil {
				t.Fatalf("bind: %v", err)
			}
			t.Cleanup(func() { c.Close() })
			if tc.publish {
				if err := c.Publish(); err != nil {
					t.Fatalf("Publish: %v", err)
				}
			}
			tc.mutate(t, c, addrPath)

			// Act
			err = c.Verify()

			// Assert
			if tc.want == "" {
				if err != nil {
					t.Fatalf("Verify = %v, want nil", err)
				}
				return
			}
			if !errors.Is(err, ErrVanished) || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("Verify = %v, want an ErrVanished naming %q", err, tc.want)
			}
		})
	}
}

// awaitResult runs AwaitBootClaim on its own goroutine and answers its result.
func awaitResult(ctx context.Context, c Claim) <-chan error {
	out := make(chan error, 1)
	go func() { out <- c.AwaitBootClaim(ctx) }()
	return out
}

// awaitBound bounds a wait the kernel answers at once once the incumbent lets
// go: a failure ceiling, never a delay a passing test pays.
const awaitBound = 5 * time.Second

func TestAClaimThatHoldsTheBootClaimIsAnsweredAtOnce(t *testing.T) {
	// Arrange
	c, err := Bind(filepath.Join(t.TempDir(), "daemon.addr"), 0)
	if err != nil {
		t.Fatalf("Bind: %v", err)
	}
	defer c.Close()

	// Act
	err = c.AwaitBootClaim(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("AwaitBootClaim = %v, want nil for a claim that holds it", err)
	}
}

func TestASuccessorTakesTheBootClaimTheMomentTheIncumbentLetsGo(t *testing.T) {
	// Arrange: an incumbent holds the claim; the successor waits for it.
	path := filepath.Join(t.TempDir(), "daemon.addr")
	incumbent, err := Bind(path, 0)
	if err != nil {
		t.Fatalf("Bind: %v", err)
	}
	successor, err := BindJoining(path, 0)
	if err != nil {
		t.Fatalf("BindJoining: %v", err)
	}
	defer successor.Close()
	waiters := []<-chan error{awaitResult(context.Background(), successor), awaitResult(context.Background(), successor)}

	// Act: the incumbent exits.
	if err := incumbent.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Assert: every waiter is answered, and the successor advertises under
	// the claim it now holds.
	for i, w := range waiters {
		select {
		case err := <-w:
			if err != nil {
				t.Fatalf("waiter %d = %v, want nil", i, err)
			}
		case <-time.After(awaitBound):
			t.Fatalf("waiter %d was not answered once the incumbent let go", i)
		}
	}
	if err := successor.Publish(); err != nil {
		t.Fatalf("Publish under the awaited claim: %v", err)
	}
	if _, err := BindWithin(path, 0, 0); !errors.Is(err, ErrClaimed) {
		t.Fatalf("Bind after the takeover = %v, want ErrClaimed", err)
	}
}

func TestAWaiterWhoseContextEndsIsAnsweredWithItsCause(t *testing.T) {
	// Arrange: the incumbent never lets go.
	path := filepath.Join(t.TempDir(), "daemon.addr")
	incumbent, err := Bind(path, 0)
	if err != nil {
		t.Fatalf("Bind: %v", err)
	}
	defer incumbent.Close()
	successor, err := BindJoining(path, 0)
	if err != nil {
		t.Fatalf("BindJoining: %v", err)
	}
	defer successor.Close()
	ctx, cancel := context.WithCancel(context.Background())
	waiter := awaitResult(ctx, successor)

	// Act
	cancel()

	// Assert
	select {
	case err := <-waiter:
		if !errors.Is(err, context.Canceled) {
			t.Fatalf("AwaitBootClaim = %v, want the context's cancellation", err)
		}
	case <-time.After(awaitBound):
		t.Fatal("a cancelled waiter was not answered")
	}
}

func TestAWaiterIsAnsweredWhenPublishTakesTheClaimFirst(t *testing.T) {
	// Arrange: an incumbent holds the claim, and a waiter is parked on it.
	path := filepath.Join(t.TempDir(), "daemon.addr")
	incumbent, err := Bind(path, 0)
	if err != nil {
		t.Fatalf("Bind: %v", err)
	}
	successor, err := BindJoining(path, 0)
	if err != nil {
		t.Fatalf("BindJoining: %v", err)
	}
	defer successor.Close()
	waiter := awaitResult(context.Background(), successor)

	// Act: the incumbent exits and the successor's own Publish takes the
	// claim, which the parked wait may or may not have reached first.
	if err := incumbent.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}
	select {
	case err := <-waiter:
		if err != nil {
			t.Fatalf("AwaitBootClaim = %v, want nil", err)
		}
	case <-time.After(awaitBound):
		t.Fatal("the waiter was not answered once the claim was free")
	}

	// Assert: the claim is the successor's either way.
	if err := successor.Publish(); err != nil {
		t.Fatalf("Publish: %v", err)
	}
}

func TestAWaitThatCannotOpenTheLockSurfacesTheFailure(t *testing.T) {
	// Arrange: a successor whose state root vanished under it.
	root := t.TempDir()
	successor, err := BindJoining(filepath.Join(root, "daemon.addr"), 0)
	if err != nil {
		t.Fatalf("BindJoining: %v", err)
	}
	defer successor.Close()
	if err := os.RemoveAll(root); err != nil {
		t.Fatalf("RemoveAll: %v", err)
	}

	// Act
	err = successor.AwaitBootClaim(context.Background())

	// Assert
	if err == nil || !strings.Contains(err.Error(), "open the boot lock") {
		t.Fatalf("AwaitBootClaim = %v, want the lock's open failure", err)
	}
}

func TestAClaimClosedWhileItWaitsReleasesTheClaimItLaterTakes(t *testing.T) {
	// Arrange: a successor waits on an incumbent, then is closed.
	path := filepath.Join(t.TempDir(), "daemon.addr")
	incumbent, err := Bind(path, 0)
	if err != nil {
		t.Fatalf("Bind: %v", err)
	}
	successor, err := BindJoining(path, 0)
	if err != nil {
		t.Fatalf("BindJoining: %v", err)
	}
	waiter := awaitResult(context.Background(), successor)
	if err := successor.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Act: the incumbent exits, and the closed successor's wait lands.
	if err := incumbent.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Assert: the waiter is told, and the claim is free for the next daemon.
	select {
	case err := <-waiter:
		if err == nil || !strings.Contains(err.Error(), "the claim was closed") {
			t.Fatalf("AwaitBootClaim = %v, want the closed claim named", err)
		}
	case <-time.After(awaitBound):
		t.Fatal("the closed successor's waiter was not answered")
	}
	next, err := BindWithin(path, 0, 0)
	if err != nil {
		t.Fatalf("Bind after the closed successor = %v, want the claim free", err)
	}
	next.Close()
}
