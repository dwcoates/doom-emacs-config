// Package seats rations a host resource between every run on the host: a
// Pool is N seats, each a lock file, and a run holds a seat for as long as it
// holds the file's lock.
//
// THE KERNEL IS THE ARBITER. A run that finds every seat held waits on the
// kernel's own lock rather than a poll, and a seat whose holder died is free at
// once, because the kernel drops the lock with its holder. No seat is ever
// leaked by a crashed run, and no two runs ever hold the same seat.
package seats

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"sync"
	"syscall"
)

// Pool is N seats under Dir, seat i being the lock file <Dir>/<Prefix>-<i>.lock.
// What decides whether two runs share a pool is the directory and prefix alone.
type Pool struct {
	Dir    string
	Prefix string
	N      int
	// What names the seats in errors ("RAM disk", "core").
	What string
}

// Path is seat i's lock file.
func (p Pool) Path(i int) string {
	return filepath.Join(p.Dir, fmt.Sprintf("%s-%d.lock", p.Prefix, i))
}

// Open opens seat i's lock file, creating it as needed, without locking it.
func (p Pool) Open(i int) (*os.File, error) {
	if err := os.MkdirAll(p.Dir, 0o755); err != nil {
		return nil, fmt.Errorf("make the %s seats directory: %w", p.What, err)
	}
	f, err := os.OpenFile(p.Path(i), os.O_CREATE|os.O_RDWR, 0o644)
	if err != nil {
		return nil, fmt.Errorf("open the %s seat %s: %w", p.What, p.Path(i), err)
	}
	return f, nil
}

// Take holds one of the pool's seats and answers its open lock file, which the
// caller closes to give the seat back, and the seat's index. It takes a free
// seat at once; when every seat is held it says so through waiting, then
// waits, unbounded, for the first seat its holder lets go.
//
// THE WAIT IS THE KERNEL'S. One goroutine per seat blocks in flock on it; the
// first to hold one wins, and every other, whenever it gets its seat, lets it
// go again at once, so a waiter never holds more than the one seat it won.
func (p Pool) Take(waiting func(held int)) (*os.File, int, error) {
	if p.N < 1 {
		return nil, 0, fmt.Errorf("the %s seat pool %s has %d seats; it needs at least one", p.What, p.Dir, p.N)
	}
	for i := range p.N {
		f, err := p.TryTake(i)
		if err != nil {
			return nil, 0, err
		}
		if f != nil {
			return f, i, nil
		}
	}
	waiting(p.N)
	type won struct {
		f   *os.File
		i   int
		err error
	}
	first := make(chan won, 1)
	var once sync.Once
	for i := range p.N {
		go func() {
			f, err := p.Open(i)
			if err == nil {
				err = syscall.Flock(int(f.Fd()), syscall.LOCK_EX)
				if err != nil {
					err = errors.Join(fmt.Errorf("lock the %s seat %s: %w", p.What, p.Path(i), err), f.Close())
				}
			}
			taken := false
			once.Do(func() {
				taken = true
				first <- won{f: f, i: i, err: err}
			})
			if !taken && err == nil {
				// Another seat won: this one goes back at once. Closing the
				// file drops the lock, so a failed close holds nothing.
				_ = f.Close()
			}
		}()
	}
	w := <-first
	if w.err != nil {
		return nil, 0, w.err
	}
	return w.f, w.i, nil
}

// Held is the seats one TakeN holds; Release gives every one of them back.
type Held []*os.File

// Release closes every held seat's lock file, which drops its lock. Every
// file is closed whatever the others answer, so a failed close never keeps a
// later seat held.
func (h Held) Release() error {
	var errs []error
	for _, f := range h {
		if err := f.Close(); err != nil {
			errs = append(errs, fmt.Errorf("close the seat %s: %w", f.Name(), err))
		}
	}
	return errors.Join(errs...)
}

// turnstilePath is the lock every TakeN of the pool holds while it gathers
// its seats.
func (p Pool) turnstilePath() string {
	return filepath.Join(p.Dir, p.Prefix+".turnstile")
}

// lanePath is the lock every TakeN of rank holds, shared, while it waits.
func (p Pool) lanePath(rank int) string {
	return filepath.Join(p.Dir, fmt.Sprintf("%s.lane-%d", p.Prefix, rank))
}

// lock opens path and flocks it with how, answering the open file, whose
// close drops the lock. With LOCK_NB, a lock held elsewhere answers
// syscall.EWOULDBLOCK and no file.
func (p Pool) lock(path string, how int) (*os.File, error) {
	f, err := os.OpenFile(path, os.O_CREATE|os.O_RDWR, 0o644)
	if err != nil {
		return nil, fmt.Errorf("open the %s seats' lock %s: %w", p.What, path, err)
	}
	if err := syscall.Flock(int(f.Fd()), how); err != nil {
		closeErr := f.Close()
		if errors.Is(err, syscall.EWOULDBLOCK) && closeErr == nil {
			return nil, err
		}
		return nil, errors.Join(fmt.Errorf("lock the %s seats' lock %s: %w", p.What, path, err), closeErr)
	}
	return f, nil
}

// waiterAbove answers the first rank below rank (a higher priority) that has
// a taker waiting, or -1 when none has.
func (p Pool) waiterAbove(rank int) (int, error) {
	for q := range rank {
		f, err := p.lock(p.lanePath(q), syscall.LOCK_EX|syscall.LOCK_NB)
		if errors.Is(err, syscall.EWOULDBLOCK) {
			return q, nil
		}
		if err != nil {
			return 0, err
		}
		if err := f.Close(); err != nil {
			return 0, fmt.Errorf("close the %s seats' lane %s: %w", p.What, p.lanePath(q), err)
		}
	}
	return -1, nil
}

// TakeN holds n of the pool's seats at once for a taker of rank (zero is the
// highest priority), calling waiting the first time (and only the first time)
// it has to wait for them.
//
// ONE GATHERER AT A TIME, SO NO TWO TAKERS CAN DEADLOCK. A taker gathers its
// seats one by one while holding the pool's turnstile, so at most one taker
// ever holds part of what it needs; every other held seat belongs to a taker
// that has all of its seats, which gives them back when its work ends. Two
// takers each holding half of a pool and each waiting for the other's half is
// therefore unrepresentable, however many runs overlap.
//
// A HIGHER RANK NEVER GATHERS WHILE A LOWER ONE WAITS. Every taker holds its
// rank's lane, shared, from the moment it asks until it has its seats. A
// taker that wins the turnstile first tries every lane above its own; finding
// one held, it lets the turnstile go and waits, on the kernel's lock, until
// that lane is empty. So the merge queue's head never queues behind another
// run's units for a seat; at most it waits for the one gather already under
// way when it asked. Lanes are only ever probed upward in priority, so no two
// takers ever wait on each other's lanes.
func (p Pool) TakeN(n, rank int, waiting func()) (Held, error) {
	if n < 1 || n > p.N {
		return nil, fmt.Errorf("take %d of the %s pool %s's %d seats: a taker needs from one to all of them", n, p.What, p.Dir, p.N)
	}
	if rank < 0 {
		return nil, fmt.Errorf("take %s seats at rank %d: a rank is zero or more", p.What, rank)
	}
	var once sync.Once
	wait := func() { once.Do(waiting) }
	if err := os.MkdirAll(p.Dir, 0o755); err != nil {
		return nil, fmt.Errorf("make the %s seats directory: %w", p.What, err)
	}
	lane, err := p.lock(p.lanePath(rank), syscall.LOCK_SH)
	if err != nil {
		return nil, err
	}
	// Closing the lane's file drops the taker's share of it: the deferred
	// close is the one release, on every path.
	defer lane.Close()
	for {
		turnstile, err := p.lock(p.turnstilePath(), syscall.LOCK_EX|syscall.LOCK_NB)
		if errors.Is(err, syscall.EWOULDBLOCK) {
			wait()
			turnstile, err = p.lock(p.turnstilePath(), syscall.LOCK_EX)
		}
		if err != nil {
			return nil, err
		}
		above, err := p.waiterAbove(rank)
		if err != nil {
			return nil, errors.Join(err, turnstile.Close())
		}
		if above < 0 {
			held, err := p.gather(n, wait)
			return held, errors.Join(err, turnstile.Close())
		}
		if err := turnstile.Close(); err != nil {
			return nil, fmt.Errorf("let the %s seats' turnstile go: %w", p.What, err)
		}
		wait()
		empty, err := p.lock(p.lanePath(above), syscall.LOCK_EX)
		if err != nil {
			return nil, err
		}
		if err := empty.Close(); err != nil {
			return nil, fmt.Errorf("close the %s seats' lane %s: %w", p.What, p.lanePath(above), err)
		}
	}
}

// gather takes n seats one at a time; on any failure it gives back what it
// already took.
func (p Pool) gather(n int, wait func()) (Held, error) {
	held := make(Held, 0, n)
	for range n {
		f, _, err := p.Take(func(int) { wait() })
		if err != nil {
			return nil, errors.Join(err, held.Release())
		}
		held = append(held, f)
	}
	return held, nil
}

// TryTake holds seat i if it is free right now, answering its open lock file
// (which the caller closes to give it back), or nil when another holder has
// it. It never waits.
func (p Pool) TryTake(i int) (*os.File, error) {
	f, err := p.Open(i)
	if err != nil {
		return nil, err
	}
	err = syscall.Flock(int(f.Fd()), syscall.LOCK_EX|syscall.LOCK_NB)
	if err == nil {
		return f, nil
	}
	closeErr := f.Close()
	if errors.Is(err, syscall.EWOULDBLOCK) {
		if closeErr != nil {
			return nil, fmt.Errorf("close the held %s seat %s: %w", p.What, p.Path(i), closeErr)
		}
		return nil, nil
	}
	return nil, errors.Join(fmt.Errorf("lock the %s seat %s: %w", p.What, p.Path(i), err), closeErr)
}

// TryTurnstile holds the pool's turnstile if nobody holds it right now,
// answering its open lock file, or nil when a taker is gathering. While it is
// held no taker of the pool can begin to gather: every TakeN waits for it.
func (p Pool) TryTurnstile() (*os.File, error) {
	if err := os.MkdirAll(p.Dir, 0o755); err != nil {
		return nil, fmt.Errorf("make the %s seats directory: %w", p.What, err)
	}
	f, err := p.lock(p.turnstilePath(), syscall.LOCK_EX|syscall.LOCK_NB)
	if errors.Is(err, syscall.EWOULDBLOCK) {
		return nil, nil
	}
	return f, err
}

// Turnstile holds the pool's turnstile, waiting for a gather under way to end.
func (p Pool) Turnstile() (*os.File, error) {
	if err := os.MkdirAll(p.Dir, 0o755); err != nil {
		return nil, fmt.Errorf("make the %s seats directory: %w", p.What, err)
	}
	return p.lock(p.turnstilePath(), syscall.LOCK_EX)
}

// Exclusive holds the lock <Dir>/<Prefix>.<name>, waiting for any other
// holder: a role one process on the host plays at a time, handed to the next
// waiter when its holder exits.
func (p Pool) Exclusive(name string) (*os.File, error) {
	if err := os.MkdirAll(p.Dir, 0o755); err != nil {
		return nil, fmt.Errorf("make the %s seats directory: %w", p.What, err)
	}
	return p.lock(filepath.Join(p.Dir, p.Prefix+"."+name), syscall.LOCK_EX)
}

// Held reports whether seat i is held right now, by any taker on the host. It
// probes with a lock it lets go at once, so it never holds the seat itself.
func (p Pool) Held(i int) (bool, error) {
	f, err := p.TryTake(i)
	if err != nil || f == nil {
		return f == nil && err == nil, err
	}
	if err := f.Close(); err != nil {
		return false, fmt.Errorf("let the probed %s seat %s go: %w", p.What, p.Path(i), err)
	}
	return false, nil
}
