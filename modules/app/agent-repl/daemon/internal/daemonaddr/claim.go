package daemonaddr

import (
	"fmt"
	"net"
	"os"
	"path/filepath"
	"strconv"
)

// LoopbackHost is the only interface the daemon ever binds. The daemon serves
// a local editor and a local browser; a routable bind would expose every
// workspace on the machine to the network.
const LoopbackHost = "127.0.0.1"

// claim is the Claim implementation: the boot lock, the bound listener, and
// the advertisement it may publish.
type claim struct {
	lock     *bootLock
	ln       net.Listener
	addrPath string
	address  string
}

// bind takes the boot claim and binds the listener, in that order. The lock
// comes FIRST so a second daemon loses before it has bound anything: it never
// creates a listener, never writes daemon.addr, and so cannot disturb the
// incumbent on its way out.
func bind(addrPath string, port int) (Claim, error) { return bindWith(addrPath, port, true) }

// bindJoining binds WITHOUT the boot claim, for a successor.
//
// A successor does not race for exclusivity and must not: the INCUMBENT holds
// the claim for as long as it is still serving, and a successor that tried for
// it would lose to its own predecessor and exit -- which is exactly what
// happened, so no handover ever completed. It takes the claim when it takes
// over, at Publish, by which point the incumbent has stood down.
func bindJoining(addrPath string, port int) (Claim, error) { return bindWith(addrPath, port, false) }

func bindWith(addrPath string, port int, claimBoot bool) (Claim, error) {
	if addrPath == "" {
		return nil, fmt.Errorf("daemon.addr path is empty")
	}
	if port < 0 || port > 65535 {
		return nil, fmt.Errorf("port %d is out of range", port)
	}
	dir := filepath.Dir(addrPath)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return nil, fmt.Errorf("create the state root %q: %w", dir, err)
	}
	var lock *bootLock
	if claimBoot {
		var err error
		if lock, err = acquireBootLock(LockPath(addrPath)); err != nil {
			return nil, err
		}
	}
	release := func() {
		if lock != nil {
			lock.release()
		}
	}
	ln, err := net.Listen("tcp", net.JoinHostPort(LoopbackHost, strconv.Itoa(port)))
	if err != nil {
		release()
		return nil, fmt.Errorf("bind the daemon listener on %s:%d: %w", LoopbackHost, port, err)
	}
	bound, ok := ln.Addr().(*net.TCPAddr)
	if !ok {
		ln.Close()
		release()
		return nil, fmt.Errorf("bound listener address %q is not TCP", ln.Addr())
	}
	return &claim{
		lock:     lock,
		ln:       ln,
		addrPath: addrPath,
		address:  net.JoinHostPort(LoopbackHost, strconv.Itoa(bound.Port)),
	}, nil
}

// Listener implements Claim.
func (c *claim) Listener() net.Listener { return c.ln }

// Address implements Claim.
func (c *claim) Address() string { return c.address }

// Publish implements Claim. The write is atomic — a temporary file in the
// same directory, then a rename — so a reader either sees the previous
// address or this one, never a half-written line.
func (c *claim) Publish() error {
	// A SUCCESSOR TAKES THE BOOT CLAIM WHEN IT TAKES OVER. It bound without
	// one -- the incumbent held it -- and publishing daemon.addr is the moment
	// it becomes the daemon of this state root. Failing to take it here is a
	// refusal, never a publish that advertises an address nobody claims.
	if c.lock == nil {
		lock, err := acquireBootLock(LockPath(c.addrPath))
		if err != nil {
			return fmt.Errorf("take the boot claim before advertising %q: %w", c.addrPath, err)
		}
		c.lock = lock
	}
	dir := filepath.Dir(c.addrPath)
	tmp, err := os.CreateTemp(dir, "."+filepath.Base(c.addrPath)+".*")
	if err != nil {
		return fmt.Errorf("create a temporary daemon.addr beside %q: %w", c.addrPath, err)
	}
	name := tmp.Name()
	if _, err := tmp.WriteString(c.address + "\n"); err != nil {
		tmp.Close()
		os.Remove(name)
		return fmt.Errorf("write the daemon address to %q: %w", name, err)
	}
	if err := tmp.Sync(); err != nil {
		tmp.Close()
		os.Remove(name)
		return fmt.Errorf("flush %q: %w", name, err)
	}
	if err := tmp.Close(); err != nil {
		os.Remove(name)
		return fmt.Errorf("close %q: %w", name, err)
	}
	if err := os.Chmod(name, 0o644); err != nil {
		os.Remove(name)
		return fmt.Errorf("set the mode of %q: %w", name, err)
	}
	if err := os.Rename(name, c.addrPath); err != nil {
		os.Remove(name)
		return fmt.Errorf("atomically replace %q: %w", c.addrPath, err)
	}
	return nil
}

// Withdraw implements Claim. Removing an absent file is success: an orderly
// exit that never published still withdraws.
func (c *claim) Withdraw() error {
	if err := os.Remove(c.addrPath); err != nil && !os.IsNotExist(err) {
		return fmt.Errorf("remove %q: %w", c.addrPath, err)
	}
	return nil
}

// Close implements Claim: it closes the listener and releases the boot claim,
// which is the end of this daemon's exclusivity. It deliberately does NOT
// withdraw the advertisement — a blue-green handover closes the incumbent's
// listener while the successor's daemon.addr already stands.
func (c *claim) Close() error {
	var firstErr error
	if err := c.ln.Close(); err != nil {
		firstErr = fmt.Errorf("close the daemon listener: %w", err)
	}
	// A SUCCESSOR THAT NEVER TOOK OVER holds no boot claim to release.
	if c.lock != nil {
		if err := c.lock.release(); err != nil && firstErr == nil {
			firstErr = err
		}
	}
	return firstErr
}
