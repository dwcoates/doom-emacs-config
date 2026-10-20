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
		f, err := p.Open(i)
		if err != nil {
			return nil, 0, err
		}
		err = syscall.Flock(int(f.Fd()), syscall.LOCK_EX|syscall.LOCK_NB)
		if err == nil {
			return f, i, nil
		}
		closeErr := f.Close()
		if !errors.Is(err, syscall.EWOULDBLOCK) {
			return nil, 0, errors.Join(fmt.Errorf("lock the %s seat %s: %w", p.What, p.Path(i), err), closeErr)
		}
		if closeErr != nil {
			return nil, 0, fmt.Errorf("close the held %s seat %s: %w", p.What, p.Path(i), closeErr)
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
