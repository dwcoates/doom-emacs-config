package daemonaddr

import (
	"context"
	"errors"
	"fmt"
	"net"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"time"

	"agent-repld/internal/atomicfile"
	"agent-repld/internal/inflight"
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
	// advertised is the address daemon.addr names: the shared address the
	// daemon's socket holder keeps, once Advertise named it, and this claim's
	// own address otherwise.
	advertised string
	pid        int
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
	// departed is true once Depart ran: a boot claim taken after it takes no
	// serving lock, because this daemon is no longer serving.
	departed bool
	// yielded is true once Yield ran: the boot claim went to the successor,
	// and this claim never takes it again.
	yielded bool
	// taken is closed the moment this claim holds the boot claim, by
	// whichever path took it (bind, Publish, or the wait below).
	taken chan struct{}
	// bootWait is the one blocking wait for the boot claim, closed when it
	// settles; nil until AwaitBootClaim first runs. bootWaitErr is how it
	// settled when it did not take the claim.
	bootWait    chan struct{}
	bootWaitErr error
}

// holdLocked makes LOCK this claim's boot claim, takes the serving lock
// beside it unless this daemon has already departed, and tells every waiter.
// The caller holds c.mu and c.lock is nil. A serving lock that cannot be
// taken releases the boot claim and is the answer: a claim held without its
// serving lock would read as departing to every booting daemon.
func (c *claim) holdLocked(lock *bootLock) error {
	if !c.departed {
		if err := lock.holdServing(); err != nil {
			return err
		}
	}
	c.lock = lock
	close(c.taken)
	return nil
}

// pidLinePrefix marks the second line of a daemon.addr advertisement, which
// carries the advertising daemon's process id. See ReadAdvertisement for the
// on-disk format.
const pidLinePrefix = "pid="

// privateLinePrefix starts the line naming the advertising daemon's own
// private address, written when daemon.addr names a shared one: a daemon
// telling the newest daemon apart from itself or its successor reads it,
// since the shared address names every daemon alike.
const privateLinePrefix = "private="

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
	// Private is the advertising daemon's own private address when Address
	// is a shared one the daemons' socket holder keeps; empty otherwise.
	Private string
}

// Daemon answers the advertising daemon's own address: Private when the
// advertisement names a shared address, Address otherwise.
func (a Advertisement) Daemon() string {
	if a.Private != "" {
		return a.Private
	}
	return a.Address
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
		if rest, ok := strings.CutPrefix(trimmed, privateLinePrefix); ok {
			adv.Private = strings.TrimSpace(rest)
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
func bind(addrPath string, port int, wait time.Duration, mode BootMode) (Claim, ClaimVerdict, error) {
	return bindWith(addrPath, port, true, wait, mode)
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
	c, _, err := bindWith(addrPath, port, false, 0, BootOrdinary)
	return c, err
}

func bindWith(addrPath string, port int, claimBoot bool, wait time.Duration, mode BootMode) (Claim, ClaimVerdict, error) {
	verdict := VerdictFree
	if addrPath == "" {
		return nil, VerdictUndecided, fmt.Errorf("daemon.addr path is empty")
	}
	if port < 0 || port > 65535 {
		return nil, VerdictUndecided, fmt.Errorf("port %d is out of range", port)
	}
	dir := filepath.Dir(addrPath)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return nil, VerdictUndecided, fmt.Errorf("create the state root %q: %w", dir, err)
	}
	var lock *bootLock
	if claimBoot {
		var err error
		// A HELD CLAIM IS REFUSED AT ONCE ONLY WHILE ITS HOLDER SERVES; a
		// departing holder is waited for, because an outgoing daemon holds the
		// claim until its process ends and its replacement is spawned into
		// exactly that window. See acquireBootLockWithin and ClaimWaitBound.
		if lock, verdict, err = acquireBootLockWithin(LockPath(addrPath), wait, mode, nil); err != nil {
			return nil, verdict, err
		}
		// THE SERVING LOCK IS TAKEN BEFORE ANYTHING IS BOUND: from here on a
		// booting daemon reads this one as serving.
		if err := lock.holdServing(); err != nil {
			return nil, VerdictUndecided, err
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
		return nil, verdict, fmt.Errorf("bind the daemon listener on %s:%d: %w", LoopbackHost, port, err)
	}
	bound, ok := ln.Addr().(*net.TCPAddr)
	if !ok {
		ln.Close()
		release()
		return nil, verdict, fmt.Errorf("bound listener address %q is not TCP", ln.Addr())
	}
	root, err := os.Stat(dir)
	if err != nil {
		ln.Close()
		release()
		return nil, verdict, fmt.Errorf("stat the state root %q: %w", dir, err)
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
	}, verdict, nil
}

// Listener implements Claim.
func (c *claim) Listener() net.Listener { return c.ln }

// Address implements Claim.
func (c *claim) Address() string { return c.address }

// Advertise implements Claim.
func (c *claim) Advertise(address string) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.advertised = address
}

// advertisedLocked is the address daemon.addr names; c.mu is held.
func (c *claim) advertisedLocked() string {
	if c.advertised != "" {
		return c.advertised
	}
	return c.address
}

// advertisement is the daemon.addr payload this claim publishes: the bare
// address on the first line, this daemon's pid on the second. The first line
// alone is what a legacy daemon wrote, so a legacy reader that takes only the
// first line still reads a valid address.
func (c *claim) advertisement() string {
	payload := c.advertisedLocked() + "\n" + pidLinePrefix + strconv.Itoa(c.pid) + "\n"
	if c.advertised != "" && c.advertised != c.address {
		payload += privateLinePrefix + c.address + "\n"
	}
	return payload
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
	if c.yielded {
		return fmt.Errorf("advertise %q: the boot claim was yielded to a successor, so this daemon advertises nothing", c.addrPath)
	}
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
		if err := c.holdLocked(lock); err != nil {
			return fmt.Errorf("take the boot claim before advertising %q: %w", c.addrPath, err)
		}
	}
	if err := atomicfile.Replace(c.addrPath, []byte(c.advertisement()), atomicfile.Options{Mode: 0o644, Sync: true}); err != nil {
		return fmt.Errorf("advertise the daemon address in %q: %w", c.addrPath, err)
	}
	c.published = true
	return nil
}

// WithdrawOutcome says what a Withdraw found at daemon.addr and did with it.
type WithdrawOutcome string

const (
	// WithdrawRemoved: the file named this daemon and was removed under the
	// boot claim.
	WithdrawRemoved WithdrawOutcome = "removed"
	// WithdrawAbsent: there was no file to remove.
	WithdrawAbsent WithdrawOutcome = "absent"
	// WithdrawNamesAnother: the file names another daemon and was left.
	WithdrawNamesAnother WithdrawOutcome = "names_another"
	// WithdrawHoldsNoClaim: this claim never held the boot claim (a successor
	// that never took over), so the file was not touched.
	WithdrawHoldsNoClaim WithdrawOutcome = "holds_no_claim"
	// WithdrawClaimHeld: this claim had yielded the boot claim and another
	// process holds it now, so the file is the holder's and was not read.
	WithdrawClaimHeld WithdrawOutcome = "claim_held"
)

// Withdrawal is what a Withdraw did, for the caller's record.
type Withdrawal struct {
	Outcome WithdrawOutcome
	// Yielded is true when the claim had yielded the boot claim, so the
	// withdrawal re-took it to judge the file (or found it held).
	Yielded bool
	// Path is the daemon.addr path judged.
	Path string
	// PID is this daemon's process id.
	PID int
	// Named is what the file named when it was read and present.
	Named Advertisement
	// Holder is the process the kernel named holding the boot claim, for
	// WithdrawClaimHeld; its PID is zero when the kernel named none.
	Holder Holder
}

// Removed answers whether the withdrawal removed daemon.addr.
func (w Withdrawal) Removed() bool { return w.Outcome == WithdrawRemoved }

// Withdraw implements Claim. It removes daemon.addr ONLY while the file still
// names THIS claim's address, and answers what it did.
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
//
// ONLY THE BOOT CLAIM'S HOLDER TOUCHES THE FILE. Every Publish holds the boot
// claim, so while this claim holds it no other daemon can write daemon.addr
// between the read below and the remove. A claim that yielded, closed, or
// never took the claim does not hold it: a successor may publish at any
// instant, and a read-then-remove there deleted the successor's fresh
// advertisement when it landed between the two (2026-11-17), leaving the
// successor to read its own state as vanished and stand down.
//
// A CLAIM THAT YIELDED RE-TAKES THE BOOT CLAIM to withdraw; see
// withdrawAfterYieldLocked.
func (c *claim) Withdraw() (Withdrawal, error) {
	c.mu.Lock()
	defer c.mu.Unlock()
	// THE ADVERTISEMENT IS NO LONGER THIS CLAIM'S TO VERIFY from the first
	// call on, whatever the file then says: an orderly exit is taking it down,
	// or a successor has already replaced it.
	c.published = false
	w := Withdrawal{Path: c.addrPath, PID: c.pid, Yielded: c.yielded}
	if c.yielded {
		return c.withdrawAfterYieldLocked(w)
	}
	if c.lock == nil {
		w.Outcome = WithdrawHoldsNoClaim
		return w, nil
	}
	raw, err := os.ReadFile(c.addrPath)
	if os.IsNotExist(err) {
		w.Outcome = WithdrawAbsent
		return w, nil
	}
	if err != nil {
		return w, fmt.Errorf("read %q before withdrawing it: %w", c.addrPath, err)
	}
	w.Named = ParseAdvertisement(string(raw))
	if w.Named.Address != c.advertisedLocked() {
		w.Outcome = WithdrawNamesAnother
		return w, nil
	}
	if err := os.Remove(c.addrPath); err != nil && !os.IsNotExist(err) {
		return w, fmt.Errorf("remove %q: %w", c.addrPath, err)
	}
	w.Outcome = WithdrawRemoved
	return w, nil
}

// withdrawAfterYieldLocked is Withdraw for a claim that YIELDED the boot
// claim. c.mu is held.
//
// THE INVARIANT: when a daemon that advertised exits orderly, either it
// removed its own advertisement while holding the boot claim, or another
// process holds the claim at that moment and so owns the file. A booting
// daemon reads a standing advertisement as its predecessor's death
// (PredecessorAdvertisement), so a yielded incumbent that left its file for a
// successor that never took the claim up -- the successor was shut down
// before its wait (AwaitBootClaim) took the lock -- was recorded dead though
// nobody crashed (2026-11-21).
//
// So the yielded claim re-takes the boot claim NON-BLOCKING, on the same
// kernel lock (acquireBootLock), and never its serving lock: this daemon is
// no longer serving. Taken, nobody else holds it, so the file is this
// daemon's to judge: it is removed only while it still names THIS PROCESS'S
// pid, and the claim is released at once. Refused, another process holds it
// and owns the file, which is left unread.
//
// Every interleaving with the successor (S) against this incumbent's exit
// (I), each covered by the kernel's arbitration of the one lock:
//
//   - S takes the claim BEFORE I's exit, and publishes: I's attempt is
//     refused while S holds it (left to S, which already rewrote the file);
//     after S released it, I takes it and the file names S's pid (left).
//   - S takes the claim before I's exit and abandons before publishing: I's
//     attempt is refused while S holds it (S owns the file; see below), or,
//     once S released it, I takes it and removes its own file.
//   - S takes the claim AFTER I's exit: I took it, removed its own file and
//     released it before S could hold it; S's Publish then writes a fresh
//     file under the claim.
//   - S abandons its wait BEFORE I's exit: nobody holds the claim, I takes it
//     and removes its own file -- the 2026-11-21 sequence.
//   - S abandons its wait AFTER I's exit: I took it and removed its own file
//     first; S's later abandon touches nothing.
//   - S is BLOCKED in its wait while I briefly re-holds the claim: S's wait
//     is a blocking kernel record lock (blockForBootLock, raced only against
//     the daemon's lifetime, never a bound), so it simply acquires on I's
//     release; the re-hold is one read and one remove. Its Publish (or
//     retryAdvertise, whose 30s advertiseRefusedBound is only an ERROR said
//     while the wait goes on) is not failed by it. An ORDINARY boot that
//     collides with the re-hold finds the serving lock free and waits for
//     the release within ClaimWaitBound (acquireBootLockWithin), and then
//     reads the file already gone.
//   - A third, ordinary boot holds the claim at I's exit: I's attempt is
//     refused, and the holder owns the file.
//
// The one case the holder's ownership does not cover by itself: S takes the
// claim, I exits while S holds it, and S then exits orderly WITHOUT
// publishing. S's own Withdraw removes the file only when it names S's
// advertised address (the shared one, once advertised), so I's file can be
// left behind. Closing it needs S to tell I's orderly exit from I's death,
// which nothing on disk says today; it is not closed here.
//
// A CLOSED CLAIM DOES NOT RE-TAKE: Close is the end of this claim's footprint
// on the state root (as in AwaitBootClaim), and the exit withdraws before it
// closes, so a withdrawal after it is an ordering defect, surfaced.
func (c *claim) withdrawAfterYieldLocked(w Withdrawal) (Withdrawal, error) {
	if c.closed {
		return w, fmt.Errorf("withdraw %q after a yield: the claim was closed, so the boot claim is not re-taken to judge the file", c.addrPath)
	}
	lock, err := acquireBootLock(LockPath(c.addrPath))
	if err != nil {
		var held *HeldError
		if errors.As(err, &held) {
			w.Outcome = WithdrawClaimHeld
			w.Holder = held.Holder
			return w, nil
		}
		return w, fmt.Errorf("re-take the yielded boot claim to withdraw %q: %w", c.addrPath, err)
	}
	w, judgeErr := c.removeOwnPIDLocked(w)
	if rerr := lock.release(); rerr != nil {
		return w, errors.Join(judgeErr, fmt.Errorf("release the boot claim re-taken to withdraw %q: %w", c.addrPath, rerr))
	}
	return w, judgeErr
}

// removeOwnPIDLocked removes daemon.addr when it names this process's pid,
// under a boot claim the caller holds. A shared address names every daemon
// alike, so the pid is what makes the advertisement this daemon's own.
func (c *claim) removeOwnPIDLocked(w Withdrawal) (Withdrawal, error) {
	raw, err := os.ReadFile(c.addrPath)
	if os.IsNotExist(err) {
		w.Outcome = WithdrawAbsent
		return w, nil
	}
	if err != nil {
		return w, fmt.Errorf("read %q before withdrawing it after a yield: %w", c.addrPath, err)
	}
	w.Named = ParseAdvertisement(string(raw))
	if !w.Named.PIDKnown || w.Named.PID != c.pid {
		w.Outcome = WithdrawNamesAnother
		return w, nil
	}
	if err := os.Remove(c.addrPath); err != nil && !os.IsNotExist(err) {
		return w, fmt.Errorf("remove %q after a yield: %w", c.addrPath, err)
	}
	w.Outcome = WithdrawRemoved
	return w, nil
}

// Depart implements Claim. It releases the serving lock and keeps the boot
// claim, and is idempotent.
func (c *claim) Depart() error {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.departed = true
	if c.lock == nil {
		return nil
	}
	if err := c.lock.depart(); err != nil {
		return fmt.Errorf("depart from the state root beside %q: %w", c.addrPath, err)
	}
	return nil
}

// Yield implements Claim.
func (c *claim) Yield() error {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.departed, c.yielded = true, true
	c.published = false
	if c.lock == nil {
		return nil
	}
	lock := c.lock
	c.lock = nil
	if err := lock.release(); err != nil {
		return fmt.Errorf("yield the boot claim beside %q: %w", c.addrPath, err)
	}
	return nil
}

// PredecessorAdvertisement reads daemon.addr at a boot that HOLDS THE BOOT
// CLAIM and has not yet published, and answers the advertisement standing
// there and whether it is a predecessor's.
//
// THE ADVERTISEMENT'S PRESENCE IS THE EVIDENCE; nothing is probed. The boot
// claim is kernel-enforced, so no other daemon is alive on this state root,
// and every orderly exit withdraws its advertisement (Withdraw), so one that
// still stands and names an address was left by a daemon that died without
// its orderly exit: SIGKILLed, killed with its whole session, or panicked.
// A dial cannot tell: the advertised address is the SHARED one the socket
// holder keeps listening, so a connect to it lands in the holder's backlog
// and succeeds whether or not any daemon lives (2026-11-21, a deploy's
// panic went unrecorded because the dead daemon's address "answered").
//
// An absent file, or one naming no address, is no predecessor. A read that
// fails for any other reason is an error, never read as no predecessor. An
// advertisement naming OWNPID is not a predecessor's: this process has not
// published yet, so the file cannot be its own, and the contradiction is an
// error for the caller to record.
func PredecessorAdvertisement(addrPath string, ownPID int) (Advertisement, bool, error) {
	raw, err := os.ReadFile(addrPath)
	if os.IsNotExist(err) {
		return Advertisement{}, false, nil
	}
	if err != nil {
		return Advertisement{}, false, fmt.Errorf("read the predecessor's advertisement %q: %w", addrPath, err)
	}
	ad := ParseAdvertisement(string(raw))
	if ad.Address == "" {
		return ad, false, nil
	}
	if ad.PIDKnown && ad.PID == ownPID {
		return ad, false, fmt.Errorf("the advertisement %q names this process (pid %d) before it published: it is no predecessor's", addrPath, ownPID)
	}
	return ad, true, nil
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
		// A YIELDED CLAIM GAVE THE BOOT CLAIM AWAY, and never takes it back.
		if c.yielded {
			c.mu.Unlock()
			return fmt.Errorf("await the boot claim beside %q: the claim was yielded to a successor", c.addrPath)
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
		bootWait := c.bootWait
		inflight.Standing(func() { c.waitForBootClaim(f, bootWait) })
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
	if c.lock != nil || c.closed || c.yielded {
		if rerr := lock.release(); rerr != nil {
			c.bootWaitErr = rerr
			return
		}
		if c.closed && c.lock == nil {
			c.bootWaitErr = fmt.Errorf("await the boot claim beside %q: the claim was closed", c.addrPath)
		}
		return
	}
	if err := c.holdLocked(lock); err != nil {
		c.bootWaitErr = err
	}
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
	if named := ParseAdvertisement(string(raw)).Address; named != c.advertisedLocked() {
		return fmt.Errorf("%w: daemon.addr %q names %q, not this daemon's %q", ErrVanished, c.addrPath, named, c.advertisedLocked())
	}
	return nil
}
