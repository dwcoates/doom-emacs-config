package daemonaddr

import (
	"context"
	"fmt"
	"net"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"time"
)

// LoopbackHost is the only interface the daemon ever binds. The daemon serves
// a local editor and a local browser; a routable bind would expose every
// workspace on the machine to the network.
const LoopbackHost = "127.0.0.1"

// claim is the Claim implementation: the boot lock, the bound listener, and
// the advertisement it may publish.
type claim struct {
	ln       net.Listener
	addrPath string
	address  string
	pid      int
	// root is the state root's identity at bind, so Verify can tell a
	// directory recreated at the same path from the one this claim lives in.
	root os.FileInfo

	// mu guards what Publish, Withdraw and Verify share across goroutines.
	mu sync.Mutex
	// lock is the held boot claim; nil for a successor until it publishes.
	lock *bootLock
	// published is true from a successful Publish until a Withdraw.
	published bool
	// closed is true once Close ran: a boot claim that lands after it is
	// released at once, never kept by a closed claim.
	closed bool
	// taken is closed the moment this claim holds the boot claim, by
	// whichever path took it (bind, Publish, or the wait below).
	taken chan struct{}
	// bootWait is the one blocking wait for the boot claim, closed when it
	// settles; nil until AwaitBootClaim first runs. bootWaitErr is how it
	// settled when it did not take the claim.
	bootWait    chan struct{}
	bootWaitErr error
}

// holdLocked makes LOCK this claim's boot claim and tells every waiter. The
// caller holds c.mu and c.lock is nil.
func (c *claim) holdLocked(lock *bootLock) {
	c.lock = lock
	close(c.taken)
}

// pidLinePrefix marks the second line of a daemon.addr advertisement, which
// carries the advertising daemon's process id. See ReadAdvertisement for the
// on-disk format.
const pidLinePrefix = "pid="

// Advertisement is the parsed content of a daemon.addr file: the loopback
// address a daemon serves on, and the process id of the daemon that wrote it.
//
// The file's format is forward-compatible. The first line is the bare
// "host:port" address, exactly as a legacy daemon wrote it; an optional
// second line "pid=<n>" names the advertiser. A reader that finds no pid line
// -- a file a legacy daemon wrote -- reports PIDKnown false rather than
// guessing a pid, so a client can tell "this advertiser is dead" apart from
// "this file does not say who the advertiser is."
type Advertisement struct {
	// Address is the "host:port" the daemon serves on, or "" when the file
	// names none (absent first line).
	Address string
	// PID is the advertising daemon's process id, meaningful only when
	// PIDKnown is true.
	PID int
	// PIDKnown is false for a legacy bare-address file, which names no pid.
	PIDKnown bool
}

// ParseAdvertisement parses the daemon.addr payload. It never fails: a file
// that names no address answers an empty Address, and one that names no pid
// answers PIDKnown false, because both are states a reader must handle rather
// than errors it can act on.
func ParseAdvertisement(raw string) Advertisement {
	var adv Advertisement
	for i, line := range strings.Split(raw, "\n") {
		trimmed := strings.TrimSpace(line)
		if i == 0 {
			adv.Address = trimmed
			continue
		}
		if rest, ok := strings.CutPrefix(trimmed, pidLinePrefix); ok {
			if pid, err := strconv.Atoi(strings.TrimSpace(rest)); err == nil {
				adv.PID = pid
				adv.PIDKnown = true
			}
		}
	}
	return adv
}

// ReadAdvertisement reads and parses a daemon.addr file. A read error is
// surfaced to the caller; the parse itself never fails.
func ReadAdvertisement(addrPath string) (Advertisement, error) {
	raw, err := os.ReadFile(addrPath)
	if err != nil {
		return Advertisement{}, fmt.Errorf("read the daemon advertisement %q: %w", addrPath, err)
	}
	return ParseAdvertisement(string(raw)), nil
}

// bind takes the boot claim and binds the listener, in that order. The lock
// comes FIRST so a second daemon loses before it has bound anything: it never
// creates a listener, never writes daemon.addr, and so cannot disturb the
// incumbent on its way out.
func bind(addrPath string, port int, wait time.Duration) (Claim, error) {
	return bindWith(addrPath, port, true, wait)
}

// bindJoining binds WITHOUT the boot claim, for a successor.
//
// A successor does not race for exclusivity and must not: the INCUMBENT holds
// the claim for as long as it is still serving, and a successor that tried for
// it would lose to its own predecessor and exit -- which is exactly what
// happened, so no handover ever completed. It takes the claim when it takes
// over, at Publish, by which point the incumbent has stood down.
func bindJoining(addrPath string, port int) (Claim, error) {
	// A SUCCESSOR TAKES NO CLAIM HERE, so there is nothing for it to wait on.
	return bindWith(addrPath, port, false, 0)
}

func bindWith(addrPath string, port int, claimBoot bool, wait time.Duration) (Claim, error) {
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
		// THE HELD CLAIM IS WAITED ON, not read as a live incumbent on sight:
		// an outgoing daemon holds it until its process ends, and its
		// replacement is spawned into exactly that window. See ClaimWaitBound.
		if lock, err = acquireBootLockWithin(LockPath(addrPath), wait, nil); err != nil {
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
	root, err := os.Stat(dir)
	if err != nil {
		ln.Close()
		release()
		return nil, fmt.Errorf("stat the state root %q: %w", dir, err)
	}
	taken := make(chan struct{})
	if lock != nil {
		close(taken)
	}
	return &claim{
		lock:     lock,
		taken:    taken,
		ln:       ln,
		addrPath: addrPath,
		address:  net.JoinHostPort(LoopbackHost, strconv.Itoa(bound.Port)),
		pid:      os.Getpid(),
		root:     root,
	}, nil
}

// Listener implements Claim.
func (c *claim) Listener() net.Listener { return c.ln }

// Address implements Claim.
func (c *claim) Address() string { return c.address }

// advertisement is the daemon.addr payload this claim publishes: the bare
// address on the first line, this daemon's pid on the second. The first line
// alone is what a legacy daemon wrote, so a legacy reader that takes only the
// first line still reads a valid address.
func (c *claim) advertisement() string {
	return c.address + "\n" + pidLinePrefix + strconv.Itoa(c.pid) + "\n"
}

// Publish implements Claim. The write is atomic — a temporary file in the
// same directory, then a rename — so a reader either sees the previous
// address or this one, never a half-written advertisement. The payload is the
// address followed by "pid=<n>"; see ReadAdvertisement for the format.
func (c *claim) Publish() error {
	c.mu.Lock()
	defer c.mu.Unlock()
	// A SUCCESSOR TAKES THE BOOT CLAIM WHEN IT TAKES OVER. It bound without
	// one -- the incumbent held it -- and publishing daemon.addr is the moment
	// it becomes the daemon of this state root. Failing to take it here is a
	// refusal, never a publish that advertises an address nobody claims.
	if c.lock == nil {
		// A WAIT FOR THE CLAIM IS ALREADY QUEUED IN THE KERNEL, and the
		// claim is ITS to take: a second, single-shot take on another
		// descriptor would win the claim and leave the wait blocked on this
		// very process's own lock for the rest of its life. The caller
		// awaits the claim (AwaitBootClaim) and publishes once it holds it.
		if c.bootWait != nil {
			return fmt.Errorf("take the boot claim before advertising %q: %w (a wait for it is under way)", c.addrPath, ErrClaimed)
		}
		lock, err := acquireBootLock(LockPath(c.addrPath))
		if err != nil {
			return fmt.Errorf("take the boot claim before advertising %q: %w", c.addrPath, err)
		}
		c.holdLocked(lock)
	}
	dir := filepath.Dir(c.addrPath)
	tmp, err := os.CreateTemp(dir, "."+filepath.Base(c.addrPath)+".*")
	if err != nil {
		return fmt.Errorf("create a temporary daemon.addr beside %q: %w", c.addrPath, err)
	}
	name := tmp.Name()
	if _, err := tmp.WriteString(c.advertisement()); err != nil {
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
	c.published = true
	return nil
}

// Withdraw implements Claim. It removes daemon.addr ONLY while the file still
// names THIS claim's address, and answers whether it removed anything.
//
// THE ADVERTISEMENT IS NOT REMOVED BY WHOEVER EXITS LAST. A blue-green
// handover ends with the SUCCESSOR publishing its own address into this same
// path and the incumbent exiting afterwards; an unconditional remove here took
// the successor's advertisement with it and left the state root naming no
// daemon at all, while a daemon was serving. Reading the file first makes the
// withdrawal this daemon's OWN, which is the only one it is entitled to.
//
// Removing an absent file is success: an orderly exit that never published
// still withdraws.
func (c *claim) Withdraw() (bool, error) {
	c.mu.Lock()
	defer c.mu.Unlock()
	// THE ADVERTISEMENT IS NO LONGER THIS CLAIM'S TO VERIFY from the first
	// call on, whatever the file then says: an orderly exit is taking it down,
	// or a successor has already replaced it.
	c.published = false
	raw, err := os.ReadFile(c.addrPath)
	if os.IsNotExist(err) {
		return false, nil
	}
	if err != nil {
		return false, fmt.Errorf("read %q before withdrawing it: %w", c.addrPath, err)
	}
	if ParseAdvertisement(string(raw)).Address != c.address {
		return false, nil
	}
	if err := os.Remove(c.addrPath); err != nil && !os.IsNotExist(err) {
		return false, fmt.Errorf("remove %q: %w", c.addrPath, err)
	}
	return true, nil
}

// ProbeBound is how long the staleness probe waits for a loopback connect. It
// is a connect to a port on this machine's own loopback, which either answers
// or is refused immediately; the bound covers only a kernel that is busy, and
// it is paid in full ONLY by an advertised address whose listener is gone
// without the port being closed, which a dead process's port cannot be.
const ProbeBound = 250 * time.Millisecond

// StaleAdvertisement reads daemon.addr and reports the address it names when
// NOTHING ANSWERS there.
//
// It is the boot's evidence, not its permission: an incumbent holds the boot
// claim, so a daemon that reached this point is the only one entitled to the
// state root, and an advertisement it did not write is by definition its
// predecessor's. The predecessor that exited without withdrawing -- SIGKILLed,
// or killed with its whole session -- left Emacs an address to probe and time
// out on, so the boot says so out loud before overwriting it.
func StaleAdvertisement(addrPath string) (string, bool) {
	return staleAdvertisement(addrPath, dialLoopback)
}

// staleAdvertisement is StaleAdvertisement's test seam: the probe is injected
// so a test never depends on a port being free.
func staleAdvertisement(addrPath string, dial func(addr string) error) (string, bool) {
	raw, err := os.ReadFile(addrPath)
	if err != nil {
		return "", false
	}
	addr := ParseAdvertisement(string(raw)).Address
	if addr == "" {
		return "", false
	}
	if dial(addr) == nil {
		return addr, false
	}
	return addr, true
}

// dialLoopback is the staleness probe: a bounded TCP connect, closed at once.
func dialLoopback(addr string) error {
	conn, err := net.DialTimeout("tcp", addr, ProbeBound)
	if err != nil {
		return err
	}
	return conn.Close()
}

// Close implements Claim: it closes the listener and releases the boot claim,
// which is the end of this daemon's exclusivity. It deliberately does NOT
// withdraw the advertisement — a blue-green handover closes the incumbent's
// listener while the successor's daemon.addr already stands.
func (c *claim) Close() error {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.closed = true
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

// AwaitBootClaim implements Claim.
func (c *claim) AwaitBootClaim(ctx context.Context) error {
	c.mu.Lock()
	if c.lock != nil {
		c.mu.Unlock()
		return nil
	}
	if c.bootWait == nil {
		// A CLOSED CLAIM STARTS NO WAIT. Close is the end of this claim's
		// footprint on the state root; a wait begun after it would open (and
		// so create) the lock file once its owner had already let go.
		if c.closed {
			c.mu.Unlock()
			return fmt.Errorf("await the boot claim beside %q: the claim was closed", c.addrPath)
		}
		// THE LOCK FILE IS OPENED UNDER c.mu, before the wait goes to its
		// goroutine, so Close -- which takes c.mu -- either precedes the
		// open (and the wait never starts) or follows it. A goroutine that
		// opened the file itself was free to do so after Close had returned
		// and the state root was being removed, recreating daemon.lock in a
		// directory its owner was tearing down.
		f, err := openBootLock(LockPath(c.addrPath))
		if err != nil {
			c.mu.Unlock()
			return fmt.Errorf("await the boot claim beside %q: %w", c.addrPath, err)
		}
		c.bootWait = make(chan struct{})
		go c.waitForBootClaim(f, c.bootWait)
	}
	settled, taken := c.bootWait, c.taken
	c.mu.Unlock()
	select {
	case <-taken:
		return nil
	case <-settled:
		c.mu.Lock()
		defer c.mu.Unlock()
		if c.lock != nil {
			return nil
		}
		return c.bootWaitErr
	case <-ctx.Done():
		return fmt.Errorf("await the boot claim beside %q: %w", c.addrPath, ctx.Err())
	}
}

// waitForBootClaim blocks in the kernel for the boot claim on F, the lock
// file AwaitBootClaim already opened, and keeps it as
// this claim's, then closes SETTLED. A claim taken some other way meanwhile
// (Publish's own single-shot attempt) or closed meanwhile releases the one
// this wait took.
func (c *claim) waitForBootClaim(f *os.File, settled chan struct{}) {
	lock, err := blockForBootLock(f, LockPath(c.addrPath))
	c.mu.Lock()
	defer c.mu.Unlock()
	defer close(settled)
	if err != nil {
		c.bootWaitErr = err
		return
	}
	if c.lock != nil || c.closed {
		if rerr := lock.release(); rerr != nil {
			c.bootWaitErr = rerr
			return
		}
		if c.closed && c.lock == nil {
			c.bootWaitErr = fmt.Errorf("await the boot claim beside %q: the claim was closed", c.addrPath)
		}
		return
	}
	c.holdLocked(lock)
}

// Verify implements Claim.
func (c *claim) Verify() error {
	c.mu.Lock()
	defer c.mu.Unlock()
	dir := filepath.Dir(c.addrPath)
	root, err := os.Stat(dir)
	switch {
	case os.IsNotExist(err):
		return fmt.Errorf("%w: the state root %q is gone", ErrVanished, dir)
	case err != nil:
		return fmt.Errorf("stat the state root %q: %w", dir, err)
	case !os.SameFile(root, c.root):
		return fmt.Errorf("%w: the state root %q was replaced by another directory", ErrVanished, dir)
	}
	if c.lock != nil {
		if err := c.lock.verify(); err != nil {
			return err
		}
	}
	if !c.published {
		return nil
	}
	raw, err := os.ReadFile(c.addrPath)
	switch {
	case os.IsNotExist(err):
		return fmt.Errorf("%w: daemon.addr %q is gone", ErrVanished, c.addrPath)
	case err != nil:
		return fmt.Errorf("read daemon.addr %q: %w", c.addrPath, err)
	}
	if named := ParseAdvertisement(string(raw)).Address; named != c.address {
		return fmt.Errorf("%w: daemon.addr %q names %q, not this daemon's %q", ErrVanished, c.addrPath, named, c.address)
	}
	return nil
}
