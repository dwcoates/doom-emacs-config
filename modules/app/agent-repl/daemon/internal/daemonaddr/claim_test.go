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
	loser, _, err := BindWithin(addrPath, 0, 0, BootOrdinary)

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
	if _, _, err := BindWithin(addrPath, 0, 0, BootOrdinary); err == nil {
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
	if _, _, err := BindWithin(addrPath, 0, 0, BootOrdinary); err == nil {
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

// TestAClaimStandingDownPublishesNothing pins the incident of 2026-11-22: a
// successor whose adoption finished after it began standing down published
// daemon.addr. A claim that departed, yielded or closed writes nothing and
// answers ErrDeparted, and Departed says so first.
func TestAClaimStandingDownPublishesNothing(t *testing.T) {
	tests := []struct {
		name      string
		standDown func(c Claim) error
	}{
		{name: "after Depart", standDown: func(c Claim) error { return c.Depart() }},
		{name: "after Yield", standDown: func(c Claim) error { return c.Yield() }},
		{name: "after Close", standDown: func(c Claim) error { return c.Close() }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			addrPath := newAddrPath(t)
			c := mustBind(t, addrPath)
			if err := tt.standDown(c); err != nil {
				t.Fatalf("stand down: %v", err)
			}

			// Act
			departed := c.Departed()
			err := c.Publish()

			// Assert
			if !departed {
				t.Fatal("Departed = false, want true once the claim stood down")
			}
			if !errors.Is(err, ErrDeparted) {
				t.Fatalf("Publish = %v, want ErrDeparted", err)
			}
			if _, statErr := os.Stat(addrPath); !os.IsNotExist(statErr) {
				t.Fatalf("daemon.addr was written by a claim standing down (stat err = %v)", statErr)
			}
		})
	}
}

// TestAClaimPublishedBeforeItDepartsIsWithdrawnByTheDeparture pins the other
// ordering of a publish racing a departure: the publish lands first, and the
// departure's withdrawal takes it down, so neither ordering leaves an address.
func TestAClaimPublishedBeforeItDepartsIsWithdrawnByTheDeparture(t *testing.T) {
	// Arrange
	addrPath := newAddrPath(t)
	c := mustBind(t, addrPath)
	if c.Departed() {
		t.Fatal("Departed = true before any departure")
	}
	if err := c.Publish(); err != nil {
		t.Fatalf("Publish before Depart = %v, want the address written", err)
	}
	if got := readAddrFile(t, addrPath); got != c.Address() {
		t.Fatalf("daemon.addr = %q, want %q", got, c.Address())
	}

	// Act
	if err := c.Depart(); err != nil {
		t.Fatalf("Depart: %v", err)
	}
	withdrawal, err := c.Withdraw()

	// Assert
	if err != nil {
		t.Fatalf("Withdraw: %v", err)
	}
	if !withdrawal.Withdrawn {
		t.Fatal("Withdraw removed nothing, want the published address withdrawn")
	}
	if _, statErr := os.Stat(addrPath); !os.IsNotExist(statErr) {
		t.Fatalf("daemon.addr survived the departure (stat err = %v)", statErr)
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
	withdrawal, err := c.Withdraw()
	if err != nil {
		t.Fatalf("Withdraw: %v", err)
	}

	// Assert.
	if !withdrawal.Withdrawn {
		t.Fatal("Withdraw reported nothing removed, want its own advertisement withdrawn")
	}
	if _, err := os.Stat(addrPath); !os.IsNotExist(err) {
		t.Fatalf("daemon.addr survived Withdraw (stat err = %v)", err)
	}
}

// TestWithdrawAnswersWhatItRead pins the Withdrawal every exit record is
// built from: this claim's own advertisement, and what daemon.addr held.
func TestWithdrawAnswersWhatItRead(t *testing.T) {
	const shared = "127.0.0.1:41000"
	other := os.Getpid() + 1
	tests := []struct {
		name string
		// file is daemon.addr's content before the withdrawal; "" for none,
		// "own" for this claim's own publish.
		file        string
		joining     bool
		wantRead    bool
		wantPresent bool
		wantFile    Advertisement
		wantRemoved bool
	}{
		{name: "its own advertisement is read and removed", file: "own", wantRead: true, wantPresent: true,
			wantFile: Advertisement{Address: shared, PID: os.Getpid(), PIDKnown: true}, wantRemoved: true},
		{name: "another daemon's advertisement is read and left", file: shared + "\npid=" + strconv.Itoa(other) + "\nprivate=127.0.0.1:5001\n",
			wantRead: true, wantPresent: true, wantFile: Advertisement{Address: shared, PID: other, PIDKnown: true, Private: "127.0.0.1:5001"}},
		{name: "no file is read as absent", wantRead: true},
		{name: "a claim holding no boot claim reads nothing", file: shared + "\npid=" + strconv.Itoa(other) + "\n", joining: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			addrPath := newAddrPath(t)
			var c Claim
			if tt.joining {
				// The incumbent holds the boot claim; the successor holds none.
				mustBind(t, addrPath)
				var err error
				if c, err = BindJoining(addrPath, 0); err != nil {
					t.Fatalf("BindJoining: %v", err)
				}
				t.Cleanup(func() { c.Close() })
			} else {
				c = mustBind(t, addrPath)
			}
			c.Advertise(shared)
			switch tt.file {
			case "":
			case "own":
				if err := c.Publish(); err != nil {
					t.Fatalf("Publish: %v", err)
				}
			default:
				if err := os.WriteFile(addrPath, []byte(tt.file), 0o644); err != nil {
					t.Fatal(err)
				}
			}
			if tt.file == "own" {
				tt.wantFile.Private = c.Address()
			}

			// Act
			got, err := c.Withdraw()

			// Assert
			if err != nil {
				t.Fatalf("Withdraw: %v", err)
			}
			want := Withdrawal{Address: shared, PID: os.Getpid(), Private: c.Address(),
				Read: tt.wantRead, Present: tt.wantPresent, File: tt.wantFile, Withdrawn: tt.wantRemoved}
			if got != want {
				t.Fatalf("Withdraw = %+v, want %+v", got, want)
			}
		})
	}
}

func TestWithdrawingAnAbsentAdvertisementIsSuccess(t *testing.T) {
	// Arrange: an orderly exit that never published.
	c := mustBind(t, newAddrPath(t))

	// Act.
	withdrawal, err := c.Withdraw()

	// Assert.
	if err != nil {
		t.Fatalf("Withdraw = %v, want success", err)
	}
	if withdrawal.Withdrawn {
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
	withdrawal, err := incumbent.Withdraw()

	// Assert.
	if err != nil {
		t.Fatalf("Withdraw = %v, want success", err)
	}
	if withdrawal.Withdrawn {
		t.Fatal("Withdraw removed an address it does not own")
	}
	if got := readAddrFile(t, addrPath); got != "127.0.0.1:65000" {
		t.Fatalf("daemon.addr = %q, want the successor's address", got)
	}
}

// TestWithdrawLeavesAnAdvertisementNotNamingThisProcess pins the incident of
// 2026-11-22: every daemon advertises the same shared address, so an
// incumbent's exit that matched on the address alone removed its successor's
// advertisement, and the successor stood down.
func TestWithdrawLeavesAnAdvertisementNotNamingThisProcess(t *testing.T) {
	const shared = "127.0.0.1:41000"
	cases := []struct {
		name string
		raw  string
	}{
		{name: "this claim's address and another pid", raw: shared + "\npid=" + strconv.Itoa(os.Getpid()+1) + "\nprivate=127.0.0.1:5001\n"},
		{name: "this claim's address and no pid", raw: shared + "\n"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			addrPath := newAddrPath(t)
			c := mustBind(t, addrPath)
			c.Advertise(shared)
			if err := c.Publish(); err != nil {
				t.Fatalf("Publish: %v", err)
			}
			if err := os.WriteFile(addrPath, []byte(tc.raw), 0o644); err != nil {
				t.Fatalf("write the other advertisement: %v", err)
			}

			// Act
			withdrawal, err := c.Withdraw()

			// Assert
			if err != nil || withdrawal.Withdrawn {
				t.Fatalf("Withdraw = (%v, %v), want (false, nil)", withdrawal.Withdrawn, err)
			}
			if raw, err := os.ReadFile(addrPath); err != nil || string(raw) != tc.raw {
				t.Fatalf("daemon.addr = %q (%v), want it left as %q", raw, err, tc.raw)
			}
		})
	}
}

func TestAnAdvertisementNamesAClaimByItsAddressAndPid(t *testing.T) {
	c := &claim{address: "127.0.0.1:5000", advertised: "127.0.0.1:41000", pid: 7}
	cases := []struct {
		name string
		ad   Advertisement
		want bool
	}{
		{name: "its advertised address and its pid", ad: Advertisement{Address: "127.0.0.1:41000", PID: 7, PIDKnown: true}, want: true},
		{name: "its advertised address and another pid", ad: Advertisement{Address: "127.0.0.1:41000", PID: 8, PIDKnown: true}},
		{name: "its advertised address and no pid", ad: Advertisement{Address: "127.0.0.1:41000"}},
		{name: "another address and its pid", ad: Advertisement{Address: "127.0.0.1:41001", PID: 7, PIDKnown: true}},
		{name: "its own private address, not the advertised one", ad: Advertisement{Address: "127.0.0.1:5000", PID: 7, PIDKnown: true}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			c.mu.Lock()
			got := c.namesLocked(tc.ad)
			c.mu.Unlock()

			// Assert
			if got != tc.want {
				t.Fatalf("namesLocked(%+v) = %v, want %v", tc.ad, got, tc.want)
			}
		})
	}
}

// TestStandingAdvertisement pins what the boot replaces: under the boot
// claim, an advertisement standing with an address, and nothing else.
func TestStandingAdvertisement(t *testing.T) {
	tests := []struct {
		name         string
		raw          string
		wantStanding bool
		wantAd       Advertisement
	}{
		{name: "a shared address with its pid and private address stands",
			raw:          "127.0.0.1:58161\npid=61885\nprivate=127.0.0.1:58200\n",
			wantStanding: true,
			wantAd:       Advertisement{Address: "127.0.0.1:58161", PID: 61885, PIDKnown: true, Private: "127.0.0.1:58200"}},
		{name: "a legacy bare address stands",
			raw:          "127.0.0.1:58161\n",
			wantStanding: true,
			wantAd:       Advertisement{Address: "127.0.0.1:58161"}},
		{name: "an empty file is none", raw: "\n"},
		{name: "a file naming no address is none", raw: "\npid=61885\n",
			wantAd: Advertisement{PID: 61885, PIDKnown: true}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			addrPath := newAddrPath(t)
			if err := os.WriteFile(addrPath, []byte(tt.raw), 0o644); err != nil {
				t.Fatalf("write: %v", err)
			}

			// Act
			ad, standing, err := StandingAdvertisement(addrPath, os.Getpid())

			// Assert
			if err != nil || standing != tt.wantStanding || ad != tt.wantAd {
				t.Fatalf("StandingAdvertisement = (%+v, %v, %v), want (%+v, %v, nil)", ad, standing, err, tt.wantAd, tt.wantStanding)
			}
		})
	}
}

// TestAnAnsweringSharedAddressIsStillStanding pins that nothing is dialed:
// the shared address the socket holder keeps answers a connect whether or
// not any daemon lives, so a standing advertisement is answered whatever
// answers there.
func TestAnAnsweringSharedAddressIsStillStanding(t *testing.T) {
	// Arrange: a listener on the advertised address, as the holder keeps one.
	ln, err := net.Listen("tcp", net.JoinHostPort(LoopbackHost, "0"))
	if err != nil {
		t.Fatalf("listen: %v", err)
	}
	t.Cleanup(func() { _ = ln.Close() })
	addrPath := newAddrPath(t)
	raw := ln.Addr().String() + "\npid=61885\nprivate=127.0.0.1:58200\n"
	if err := os.WriteFile(addrPath, []byte(raw), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}

	// Act
	ad, standing, err := StandingAdvertisement(addrPath, os.Getpid())

	// Assert
	if err != nil || !standing || ad.Address != ln.Addr().String() || ad.PID != 61885 {
		t.Fatalf("StandingAdvertisement = (%+v, %v, %v), want the answering address reported standing", ad, standing, err)
	}
}

// TestAnAbsentAdvertisementIsNoneStanding pins the ordinary first boot, and
// every boot after an orderly exit withdrew the file.
func TestAnAbsentAdvertisementIsNoneStanding(t *testing.T) {
	// Arrange, Act
	ad, standing, err := StandingAdvertisement(newAddrPath(t), os.Getpid())

	// Assert
	if err != nil || standing || ad != (Advertisement{}) {
		t.Fatalf("StandingAdvertisement of an absent file = (%+v, %v, %v), want none standing", ad, standing, err)
	}
}

// TestAnUnreadableAdvertisementIsAnError pins that a file which cannot be read
// is surfaced, never read as none standing.
func TestAnUnreadableAdvertisementIsAnError(t *testing.T) {
	// Arrange: a directory where daemon.addr belongs.
	addrPath := newAddrPath(t)
	if err := os.Mkdir(addrPath, 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}

	// Act
	_, standing, err := StandingAdvertisement(addrPath, os.Getpid())

	// Assert
	if err == nil || standing || !strings.Contains(err.Error(), addrPath) {
		t.Fatalf("StandingAdvertisement of a directory = (%v, %v), want an error naming %s", standing, err, addrPath)
	}
}

// TestAnAdvertisementNamingThisProcessIsAnError pins the invariant guard: a
// boot that has not yet published cannot have written the file, so one
// naming its own pid is a contradiction surfaced, not an advertisement
// standing from an earlier daemon.
func TestAnAdvertisementNamingThisProcessIsAnError(t *testing.T) {
	// Arrange
	addrPath := newAddrPath(t)
	if err := os.WriteFile(addrPath, []byte("127.0.0.1:58161\npid="+strconv.Itoa(os.Getpid())+"\n"), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}

	// Act
	_, standing, err := StandingAdvertisement(addrPath, os.Getpid())

	// Assert
	if err == nil || standing {
		t.Fatalf("StandingAdvertisement of this process's own pid = (%v, %v), want an error and none standing", standing, err)
	}
}

// TestAYieldedClaimNeverWithdrawsTheAdvertisement pins the handover's end
// after a yield: the boot claim is the successor's, so it may publish at any
// instant, and only the claim's holder may remove daemon.addr. The file left
// naming the incumbent is exactly what a read-then-remove raced on.
func TestAYieldedClaimNeverWithdrawsTheAdvertisement(t *testing.T) {
	// Arrange: the incumbent published and yielded; the file still names it.
	addrPath := newAddrPath(t)
	incumbent := mustBind(t, addrPath)
	if err := incumbent.Publish(); err != nil {
		t.Fatalf("Publish: %v", err)
	}
	if err := incumbent.Yield(); err != nil {
		t.Fatalf("Yield: %v", err)
	}

	// Act.
	withdrawal, err := incumbent.Withdraw()

	// Assert.
	if err != nil {
		t.Fatalf("Withdraw = %v, want success", err)
	}
	if withdrawal.Withdrawn {
		t.Fatal("a yielded claim removed daemon.addr, want it left for the boot claim's holder")
	}
	if got := readAddrFile(t, addrPath); got != incumbent.Address() {
		t.Fatalf("daemon.addr = %q, want it untouched (%q)", got, incumbent.Address())
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
	if _, _, err := BindWithin(path, 0, 0, BootOrdinary); !errors.Is(err, ErrClaimed) {
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
			name:    "a daemon.addr naming this address and another pid is a loss while published",
			publish: true,
			mutate: func(t *testing.T, c Claim, addrPath string) {
				if err := os.WriteFile(addrPath, []byte(c.Address()+"\npid=1\n"), 0o644); err != nil {
					t.Fatalf("WriteFile: %v", err)
				}
			},
			want: "names pid 1, not this daemon's " + strconv.Itoa(os.Getpid()),
		},
		{
			name:    "a daemon.addr naming this address and no pid is a loss while published",
			publish: true,
			mutate: func(t *testing.T, c Claim, addrPath string) {
				if err := os.WriteFile(addrPath, []byte(c.Address()+"\n"), 0o644); err != nil {
					t.Fatalf("WriteFile: %v", err)
				}
			},
			want: "names no pid",
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
	if _, _, err := BindWithin(path, 0, 0, BootOrdinary); !errors.Is(err, ErrClaimed) {
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

// TestAnAnsweredWaitTouchesNothingInTheStateDirectory pins where the wait's
// lock file is opened: on the caller's goroutine. Opened by the waiting
// goroutine instead, a late-scheduled one created daemon.lock after its caller
// had answered, inside a directory already being removed.
func TestAnAnsweredWaitTouchesNothingInTheStateDirectory(t *testing.T) {
	// Arrange: a cancelled waiter, and the lock file removed after it answered.
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
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	if err := successor.AwaitBootClaim(ctx); !errors.Is(err, context.Canceled) {
		t.Fatalf("AwaitBootClaim = %v, want the cancellation", err)
	}
	if err := os.Remove(LockPath(path)); err != nil {
		t.Fatalf("remove the lock file: %v", err)
	}

	// Act: the incumbent lets go, so the parked wait completes.
	incumbent.Close()
	settled := awaitResult(context.Background(), successor)
	select {
	case err := <-settled:
		if err != nil {
			t.Fatalf("AwaitBootClaim = %v, want the claim", err)
		}
	case <-time.After(awaitBound):
		t.Fatal("the parked wait never completed")
	}

	// Assert.
	if _, err := os.Stat(LockPath(path)); !os.IsNotExist(err) {
		t.Fatalf("stat the lock file = %v, want it never recreated", err)
	}
}

func TestAWaitWhoseLockFileCannotBeOpenedIsAnsweredWithTheCause(t *testing.T) {
	// Arrange: the lock file's path is a directory, which cannot be opened
	// for writing.
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
	if err := os.Remove(LockPath(path)); err != nil {
		t.Fatalf("remove the lock file: %v", err)
	}
	if err := os.Mkdir(LockPath(path), 0o700); err != nil {
		t.Fatalf("Mkdir: %v", err)
	}

	// Act.
	err = successor.AwaitBootClaim(context.Background())

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "open the boot lock") {
		t.Fatalf("AwaitBootClaim = %v, want the open's failure", err)
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
	next, _, err := BindWithin(path, 0, 0, BootOrdinary)
	if err != nil {
		t.Fatalf("Bind after the closed successor = %v, want the claim free", err)
	}
	next.Close()
}

func TestPublishLeavesTheClaimToAWaitAlreadyUnderWay(t *testing.T) {
	// Arrange: a successor is waiting for the claim, and the incumbent exits.
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
	ctx, cancel := context.WithCancel(context.Background())
	parked := awaitResult(ctx, successor)
	cancel()
	<-parked
	if err := incumbent.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Act: Publish is asked before the wait is seen to land, then after.
	if err := successor.AwaitBootClaim(context.Background()); err != nil {
		t.Fatalf("AwaitBootClaim: %v", err)
	}
	err = successor.Publish()

	// Assert: the claim the wait took is the one advertised under.
	if err != nil {
		t.Fatalf("Publish under the awaited claim = %v, want nil", err)
	}
	if _, _, err := BindWithin(path, 0, 0, BootOrdinary); !errors.Is(err, ErrClaimed) {
		t.Fatalf("Bind after the takeover = %v, want ErrClaimed", err)
	}
}

func TestPublishWhileAWaitIsUnderWayIsAHeldClaim(t *testing.T) {
	// Arrange: the incumbent holds the claim and a successor's wait is queued.
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
	parked := awaitResult(ctx, successor)
	cancel()
	<-parked

	// Act
	err = successor.Publish()

	// Assert
	if !errors.Is(err, ErrClaimed) || !strings.Contains(err.Error(), "a wait for it is under way") {
		t.Fatalf("Publish = %v, want ErrClaimed naming the wait under way", err)
	}
}

func TestAWaitAbandonedBeforeCloseCreatesNoLockFileAfterIt(t *testing.T) {
	// Arrange: a successor's wait is begun and abandoned, the successor is
	// closed, and its state root's lock file is then taken away -- what a
	// teardown removing the state root does.
	path := filepath.Join(t.TempDir(), "daemon.addr")
	incumbent, err := Bind(path, 0)
	if err != nil {
		t.Fatalf("Bind: %v", err)
	}
	successor, err := BindJoining(path, 0)
	if err != nil {
		t.Fatalf("BindJoining: %v", err)
	}
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	if err := successor.AwaitBootClaim(ctx); !errors.Is(err, context.Canceled) {
		t.Fatalf("AwaitBootClaim = %v, want context.Canceled", err)
	}
	if err := successor.Close(); err != nil {
		t.Fatalf("successor Close: %v", err)
	}
	if err := os.Remove(LockPath(path)); err != nil {
		t.Fatalf("remove the lock file: %v", err)
	}

	// Act: the incumbent lets go, so the abandoned wait lands and settles.
	if err := incumbent.Close(); err != nil {
		t.Fatalf("incumbent Close: %v", err)
	}
	<-successor.(*claim).bootWait

	// Assert: the wait never named the path again, so nothing recreated it.
	if _, err := os.Stat(LockPath(path)); !os.IsNotExist(err) {
		t.Fatalf("stat the lock file after the wait settled = %v, want it absent", err)
	}
}

func TestAwaitBootClaimOnAClosedClaimStartsNoWait(t *testing.T) {
	// Arrange: a successor closed before it ever waited.
	path := filepath.Join(t.TempDir(), "daemon.addr")
	successor, err := BindJoining(path, 0)
	if err != nil {
		t.Fatalf("BindJoining: %v", err)
	}
	if err := successor.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Act
	err = successor.AwaitBootClaim(context.Background())

	// Assert: refused, and the lock file was never created.
	if err == nil || !strings.Contains(err.Error(), "the claim was closed") {
		t.Fatalf("AwaitBootClaim on a closed claim = %v, want the closed refusal", err)
	}
	if _, err := os.Stat(LockPath(path)); !os.IsNotExist(err) {
		t.Fatalf("stat the lock file = %v, want it absent", err)
	}
}

// requireServing asserts whether the serving lock beside addrPath is held.
func requireServing(t *testing.T, addrPath string, want bool) {
	t.Helper()
	held, err := servingHeld(ServingPath(addrPath))
	if err != nil {
		t.Fatalf("servingHeld: %v", err)
	}
	if held != want {
		t.Fatalf("serving lock held = %v, want %v", held, want)
	}
}

func TestBindTakesTheServingLock(t *testing.T) {
	// Arrange.
	addrPath := newAddrPath(t)

	// Act.
	mustBind(t, addrPath)

	// Assert.
	requireServing(t, addrPath, true)
}

func TestASecondBindBesideAServingClaimIsRefusedAtOnce(t *testing.T) {
	// Arrange.
	addrPath := newAddrPath(t)
	mustBind(t, addrPath)

	// Act: the production bound, which a serving incumbent must not cost.
	loser, verdict, err := BindWithin(addrPath, 0, ClaimWaitBound, BootOrdinary)

	// Assert.
	if err == nil {
		loser.Close()
		t.Fatalf("a second daemon took the claim of a serving incumbent")
	}
	if !errors.Is(err, ErrClaimed) || verdict != VerdictIncumbentServing {
		t.Fatalf("BindWithin = (%q, %v), want (%q, ErrClaimed)", verdict, err, VerdictIncumbentServing)
	}
}

func TestDepartReleasesTheServingLockAndKeepsTheBootClaim(t *testing.T) {
	// Arrange.
	addrPath := newAddrPath(t)
	c := mustBind(t, addrPath)

	// Act.
	if err := c.Depart(); err != nil {
		t.Fatalf("Depart: %v", err)
	}

	// Assert.
	requireServing(t, addrPath, false)
	if _, _, err := BindWithin(addrPath, 0, 0, BootOrdinary); !errors.Is(err, ErrClaimed) {
		t.Fatalf("BindWithin after Depart = %v, want ErrClaimed: the boot claim is held until exit", err)
	}
}

func TestCloseReleasesTheServingLock(t *testing.T) {
	// Arrange.
	addrPath := newAddrPath(t)
	c, err := Bind(addrPath, 0)
	if err != nil {
		t.Fatalf("Bind: %v", err)
	}

	// Act.
	if err := c.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Assert.
	requireServing(t, addrPath, false)
}

func TestAJoiningClaimTakesTheServingLockWhenItAdvertises(t *testing.T) {
	// Arrange.
	addrPath := newAddrPath(t)
	successor, err := BindJoining(addrPath, 0)
	if err != nil {
		t.Fatalf("BindJoining: %v", err)
	}
	defer successor.Close()
	requireServing(t, addrPath, false)

	// Act.
	if err := successor.Publish(); err != nil {
		t.Fatalf("Publish: %v", err)
	}

	// Assert.
	requireServing(t, addrPath, true)
}

func TestASuccessorTakesTheServingLockWithTheAwaitedClaim(t *testing.T) {
	// Arrange.
	addrPath := newAddrPath(t)
	incumbent, err := Bind(addrPath, 0)
	if err != nil {
		t.Fatalf("Bind: %v", err)
	}
	successor, err := BindJoining(addrPath, 0)
	if err != nil {
		t.Fatalf("BindJoining: %v", err)
	}
	defer successor.Close()
	waiter := awaitResult(context.Background(), successor)

	// Act.
	if err := incumbent.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}
	select {
	case err := <-waiter:
		if err != nil {
			t.Fatalf("AwaitBootClaim = %v, want nil", err)
		}
	case <-time.After(awaitBound):
		t.Fatalf("the successor was not answered once the incumbent let go")
	}

	// Assert.
	requireServing(t, addrPath, true)
}

func TestAClaimThatDepartedTakesNoServingLockWithALaterClaim(t *testing.T) {
	// Arrange: a successor that departs before it ever takes over.
	addrPath := newAddrPath(t)
	successor, err := BindJoining(addrPath, 0)
	if err != nil {
		t.Fatalf("BindJoining: %v", err)
	}
	defer successor.Close()
	if err := successor.Depart(); err != nil {
		t.Fatalf("Depart: %v", err)
	}

	// Act.
	if err := successor.Publish(); err != nil {
		t.Fatalf("Publish: %v", err)
	}

	// Assert.
	requireServing(t, addrPath, false)
}

func TestAServingLockThatCannotBeTakenRefusesTheBind(t *testing.T) {
	// Arrange: the serving lock unopenable, the boot claim free.
	if os.Geteuid() == 0 {
		t.Fatal("this test needs a non-root user: root opens a mode-000 file")
	}
	addrPath := newAddrPath(t)
	if err := os.WriteFile(ServingPath(addrPath), nil, 0); err != nil {
		t.Fatalf("write an unopenable serving lock: %v", err)
	}

	// Act.
	c, verdict, err := BindWithin(addrPath, 0, 0, BootOrdinary)

	// Assert: refused, and the boot claim it took first was released.
	if err == nil {
		c.Close()
		t.Fatalf("BindWithin took a claim it could not mark serving")
	}
	if !errors.Is(err, os.ErrPermission) || verdict != VerdictUndecided {
		t.Fatalf("BindWithin = (%q, %v), want (%q, a permission error)", verdict, err, VerdictUndecided)
	}
	if err := ProbeBootClaim(addrPath); err != nil {
		t.Fatalf("ProbeBootClaim after the refused bind = %v, want the claim free", err)
	}
}

func TestPublishWhoseServingLockCannotBeTakenAdvertisesNothing(t *testing.T) {
	// Arrange.
	if os.Geteuid() == 0 {
		t.Fatal("this test needs a non-root user: root opens a mode-000 file")
	}
	addrPath := newAddrPath(t)
	successor, err := BindJoining(addrPath, 0)
	if err != nil {
		t.Fatalf("BindJoining: %v", err)
	}
	defer successor.Close()
	if err := os.WriteFile(ServingPath(addrPath), nil, 0); err != nil {
		t.Fatalf("write an unopenable serving lock: %v", err)
	}

	// Act.
	err = successor.Publish()

	// Assert: refused, nothing advertised, and the boot claim released.
	if !errors.Is(err, os.ErrPermission) {
		t.Fatalf("Publish = %v, want the serving lock's permission error", err)
	}
	if _, statErr := os.Stat(addrPath); !os.IsNotExist(statErr) {
		t.Fatalf("daemon.addr after a refused Publish: stat err = %v, want it absent", statErr)
	}
	if err := ProbeBootClaim(addrPath); err != nil {
		t.Fatalf("ProbeBootClaim after the refused Publish = %v, want the claim free", err)
	}
}

func TestAnAwaitedClaimWhoseServingLockCannotBeTakenIsAnsweredWithTheCause(t *testing.T) {
	// Arrange: the successor waits; the serving lock becomes unopenable
	// while the incumbent still holds it open.
	if os.Geteuid() == 0 {
		t.Fatal("this test needs a non-root user: root opens a mode-000 file")
	}
	addrPath := newAddrPath(t)
	incumbent, err := Bind(addrPath, 0)
	if err != nil {
		t.Fatalf("Bind: %v", err)
	}
	successor, err := BindJoining(addrPath, 0)
	if err != nil {
		t.Fatalf("BindJoining: %v", err)
	}
	defer successor.Close()
	waiter := awaitResult(context.Background(), successor)
	if err := os.Chmod(ServingPath(addrPath), 0); err != nil {
		t.Fatalf("chmod the serving lock: %v", err)
	}

	// Act.
	if err := incumbent.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Assert: the wait is answered with the cause, and holds no claim.
	select {
	case err := <-waiter:
		if !errors.Is(err, os.ErrPermission) {
			t.Fatalf("AwaitBootClaim = %v, want the serving lock's permission error", err)
		}
	case <-time.After(awaitBound):
		t.Fatalf("the successor was not answered once the incumbent let go")
	}
	if err := ProbeBootClaim(addrPath); err != nil {
		t.Fatalf("ProbeBootClaim after the failed takeover = %v, want the claim free", err)
	}
}

func TestAYieldedClaimHandsTheBootClaimToItsSuccessor(t *testing.T) {
	// Arrange: an incumbent advertising, and its successor waiting.
	path := filepath.Join(t.TempDir(), "daemon.addr")
	incumbent, err := Bind(path, 0)
	if err != nil {
		t.Fatalf("Bind: %v", err)
	}
	defer incumbent.Close()
	if err := incumbent.Publish(); err != nil {
		t.Fatalf("Publish: %v", err)
	}
	successor, err := BindJoining(path, 0)
	if err != nil {
		t.Fatalf("BindJoining: %v", err)
	}
	defer successor.Close()
	waited := awaitResult(context.Background(), successor)

	// Act
	err = incumbent.Yield()

	// Assert: the successor's wait takes the claim, and it advertises.
	if err != nil {
		t.Fatalf("Yield = %v", err)
	}
	if err := <-waited; err != nil {
		t.Fatalf("the successor's AwaitBootClaim = %v, want the claim", err)
	}
	if err := successor.Publish(); err != nil {
		t.Fatalf("the successor's Publish = %v", err)
	}
}

func TestAYieldedClaimIsStillWhole(t *testing.T) {
	tests := []struct {
		name string
		act  func(c Claim) error
		want bool
	}{
		{name: "it verifies while its successor advertises", act: func(c Claim) error { return c.Verify() }, want: true},
		{name: "it never waits for the boot claim again", act: func(c Claim) error { return c.AwaitBootClaim(context.Background()) }},
		{name: "it never advertises again", act: func(c Claim) error { return c.Publish() }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the incumbent yielded, and its successor advertises.
			path := filepath.Join(t.TempDir(), "daemon.addr")
			incumbent, err := Bind(path, 0)
			if err != nil {
				t.Fatalf("Bind: %v", err)
			}
			defer incumbent.Close()
			if err := incumbent.Publish(); err != nil {
				t.Fatalf("Publish: %v", err)
			}
			if err := incumbent.Yield(); err != nil {
				t.Fatalf("Yield: %v", err)
			}
			successor, err := BindJoining(path, 0)
			if err != nil {
				t.Fatalf("BindJoining: %v", err)
			}
			defer successor.Close()
			if err := successor.Publish(); err != nil {
				t.Fatalf("the successor's Publish: %v", err)
			}

			// Act
			err = tt.act(incumbent)

			// Assert
			if (err == nil) != tt.want {
				t.Fatalf("got %v, want success %v", err, tt.want)
			}
		})
	}
}

func TestAnAdvertisedSharedAddressIsPublishedVerifiedAndWithdrawn(t *testing.T) {
	// Arrange.
	addrPath := newAddrPath(t)
	c := mustBind(t, addrPath)
	c.Advertise("127.0.0.1:41000")

	// Act.
	publishErr := c.Publish()
	raw, _ := os.ReadFile(addrPath)
	verifyErr := c.Verify()
	removed, withdrawErr := c.Withdraw()

	// Assert: daemon.addr names the shared address, not this claim's own.
	if publishErr != nil {
		t.Fatalf("Publish: %v", publishErr)
	}
	if want := "127.0.0.1:41000\npid=" + strconv.Itoa(os.Getpid()) + "\nprivate=" + c.Address() + "\n"; string(raw) != want {
		t.Fatalf("daemon.addr = %q, want %q", raw, want)
	}
	if verifyErr != nil {
		t.Fatalf("Verify of the advertised address: %v", verifyErr)
	}
	if withdrawErr != nil || !removed.Withdrawn {
		t.Fatalf("Withdraw = %+v, %v; want the advertisement removed", removed, withdrawErr)
	}
}

func TestAnAdvertisementNamesItsDaemonByItsPrivateAddress(t *testing.T) {
	tests := []struct {
		name string
		raw  string
		want string
	}{
		{name: "a shared address names its daemon's private one", raw: "127.0.0.1:41000\npid=7\nprivate=127.0.0.1:5000\n", want: "127.0.0.1:5000"},
		{name: "a daemon's own address names itself", raw: "127.0.0.1:5000\npid=7\n", want: "127.0.0.1:5000"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := ParseAdvertisement(tt.raw).Daemon()

			// Assert
			if got != tt.want {
				t.Fatalf("Daemon() = %q, want %q", got, tt.want)
			}
		})
	}
}
