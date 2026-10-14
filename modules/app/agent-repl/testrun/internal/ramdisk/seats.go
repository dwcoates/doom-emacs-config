package ramdisk

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"sync"
	"syscall"
	"time"
)

// A RUN'S RAM DISK TAKES A MEMORY SEAT (owner ruling, 2026-10-14). A RAM
// disk's memory is wired: nothing pages it out, and `nice 19`, which keeps
// test runs off the owner's CPU, does nothing for memory. So however many runs
// overlap, their RAM disks together hold at most half the host's memory: there
// are MemorySeats seats, each a lock file under the registry, and a run holds
// one from before its disk is attached until it is released. A run that finds
// every seat held waits for one, on the kernel's own lock rather than a poll,
// and a seat whose holder died is free at once, because the kernel drops the
// lock with its holder.

// MemoryShare is the share of the host's memory every run's RAM disk together
// may hold.
const MemoryShare = 2 // one half

// MemorySeats is how many RAM disks of sizeMiB fit in the host's share: never
// fewer than one, so a host smaller than one disk's share still runs, one run
// at a time.
func MemorySeats(memBytes uint64, sizeMiB int) int {
	n := int(memBytes / MemoryShare / (uint64(sizeMiB) << 20))
	return max(n, 1)
}

// seatPath is seat i's lock file, in the registry's seats directory: never
// beside the runs' records, which the registry reads as every *.lock in it.
func seatPath(registry string, i int) string {
	return filepath.Join(registry, "seats", fmt.Sprintf("seat-%d.lock", i))
}

// takeSeat holds one of n seats under registry and answers its open lock
// file, which the caller closes to give the seat back. It takes a free seat
// at once; when every seat is held it says so through waiting, then waits,
// unbounded, for the first seat its holder lets go.
//
// THE WAIT IS THE KERNEL'S. One goroutine per seat blocks in flock on it; the
// first to hold one wins, and every other, whenever it gets its seat, lets it
// go again at once, so a waiter never holds more than the one seat it won.
func takeSeat(registry string, n int, waiting func(held int)) (*os.File, int, error) {
	for i := range n {
		f, err := openSeat(registry, i)
		if err != nil {
			return nil, 0, err
		}
		err = syscall.Flock(int(f.Fd()), syscall.LOCK_EX|syscall.LOCK_NB)
		if err == nil {
			return f, i, nil
		}
		closeErr := f.Close()
		if !errors.Is(err, syscall.EWOULDBLOCK) {
			return nil, 0, errors.Join(fmt.Errorf("lock the RAM disk seat %s: %w", seatPath(registry, i), err), closeErr)
		}
		if closeErr != nil {
			return nil, 0, fmt.Errorf("close the held RAM disk seat %s: %w", seatPath(registry, i), closeErr)
		}
	}
	waiting(n)
	type won struct {
		f   *os.File
		i   int
		err error
	}
	first := make(chan won, 1)
	var once sync.Once
	for i := range n {
		go func() {
			f, err := openSeat(registry, i)
			if err == nil {
				err = syscall.Flock(int(f.Fd()), syscall.LOCK_EX)
				if err != nil {
					err = errors.Join(fmt.Errorf("lock the RAM disk seat %s: %w", seatPath(registry, i), err), f.Close())
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

// openSeat opens seat i's lock file, creating it as needed.
func openSeat(registry string, i int) (*os.File, error) {
	if err := os.MkdirAll(filepath.Dir(seatPath(registry, i)), 0o755); err != nil {
		return nil, fmt.Errorf("make the RAM disk seats directory: %w", err)
	}
	f, err := os.OpenFile(seatPath(registry, i), os.O_CREATE|os.O_RDWR, 0o644)
	if err != nil {
		return nil, fmt.Errorf("open the RAM disk seat %s: %w", seatPath(registry, i), err)
	}
	return f, nil
}

// seat takes this run's memory seat for a disk of sizeMiB, saying how it
// went on the run's log.
func (m Manager) seat(sizeMiB int) (*os.File, error) {
	mem, err := m.Memory()
	if err != nil {
		return nil, fmt.Errorf("read the host's memory to size the RAM disk seats: %w", err)
	}
	n := MemorySeats(mem, sizeMiB)
	var start time.Time
	waited := false
	f, i, err := takeSeat(m.Registry, n, func(held int) {
		waited, start = true, m.Now()
		m.Log.Infof("RAM disk seat: all %d are held (half of the host's %d MiB, %d MiB each); waiting for one", held, mem>>20, sizeMiB)
	})
	if err != nil {
		return nil, err
	}
	if waited {
		m.Log.Infof("RAM disk seat: took seat %d of %d after %s", i, n, m.Now().Sub(start).Round(time.Millisecond))
	} else {
		m.Log.Infof("RAM disk seat: took seat %d of %d", i, n)
	}
	return f, nil
}
