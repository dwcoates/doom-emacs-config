package daemonaddr

import (
	"errors"
	"net"
	"os"
	"path/filepath"
	"strings"
	"testing"
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

	// Act.
	loser, err := Bind(addrPath, 0)

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
	if _, err := Bind(addrPath, 0); err == nil {
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
	if _, err := Bind(addrPath, 0); err == nil {
		t.Fatalf("the second bind was supposed to lose")
	}

	// Assert.
	got, err := Read(addrPath)
	if err != nil {
		t.Fatalf("Read: %v", err)
	}
	if got != incumbent.Address() {
		t.Fatalf("daemon.addr = %q, want the incumbent's %q", got, incumbent.Address())
	}
}

func TestPublishWritesTheAddressLine(t *testing.T) {
	// Arrange.
	addrPath := newAddrPath(t)
	c := mustBind(t, addrPath)

	// Act.
	if err := c.Publish(); err != nil {
		t.Fatalf("Publish: %v", err)
	}

	// Assert.
	raw, err := os.ReadFile(addrPath)
	if err != nil {
		t.Fatalf("read: %v", err)
	}
	if string(raw) != c.Address()+"\n" {
		t.Fatalf("daemon.addr = %q, want %q", raw, c.Address()+"\n")
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
	got, err := Read(addrPath)
	if err != nil {
		t.Fatalf("Read: %v", err)
	}
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
	if err := c.Withdraw(); err != nil {
		t.Fatalf("Withdraw: %v", err)
	}

	// Assert.
	if _, err := os.Stat(addrPath); !os.IsNotExist(err) {
		t.Fatalf("daemon.addr survived Withdraw (stat err = %v)", err)
	}
}

func TestWithdrawingAnAbsentAdvertisementIsSuccess(t *testing.T) {
	// Arrange: an orderly exit that never published.
	c := mustBind(t, newAddrPath(t))

	// Act.
	err := c.Withdraw()

	// Assert.
	if err != nil {
		t.Fatalf("Withdraw = %v, want success", err)
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

func TestReadReturnsThePublishedAddress(t *testing.T) {
	// Arrange.
	addrPath := newAddrPath(t)
	c := mustBind(t, addrPath)
	if err := c.Publish(); err != nil {
		t.Fatalf("Publish: %v", err)
	}

	// Act.
	got, err := Read(addrPath)

	// Assert: the trailing newline is not part of the address.
	if err != nil {
		t.Fatalf("Read: %v", err)
	}
	if got != c.Address() {
		t.Fatalf("Read = %q, want %q", got, c.Address())
	}
}

func TestReadRefusesAnAbsentOrUnusableFile(t *testing.T) {
	tests := []struct {
		name     string
		contents *string
	}{
		{name: "missing", contents: nil},
		{name: "empty", contents: strPtr("")},
		{name: "whitespace", contents: strPtr("\n \n")},
		{name: "no port", contents: strPtr("127.0.0.1\n")},
		{name: "non-numeric port", contents: strPtr("127.0.0.1:http\n")},
		{name: "no host", contents: strPtr(":8080\n")},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			addrPath := newAddrPath(t)
			if err := os.MkdirAll(filepath.Dir(addrPath), 0o755); err != nil {
				t.Fatalf("mkdir: %v", err)
			}
			if tc.contents != nil {
				if err := os.WriteFile(addrPath, []byte(*tc.contents), 0o644); err != nil {
					t.Fatalf("write: %v", err)
				}
			}

			// Act.
			got, err := Read(addrPath)

			// Assert: never an empty address reported as success.
			if err == nil {
				t.Fatalf("Read returned %q, want an error", got)
			}
			if got != "" {
				t.Fatalf("Read returned %q alongside its error", got)
			}
		})
	}
}

func strPtr(s string) *string { return &s }
