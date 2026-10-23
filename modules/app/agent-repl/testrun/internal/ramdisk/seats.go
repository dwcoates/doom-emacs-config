package ramdisk

import (
	"fmt"
	"os"
	"path/filepath"
	"time"

	"agentrepl/testrun/internal/seats"
)

// A RUN'S RAM DISK TAKES A MEMORY SEAT (owner ruling, 2026-10-14). A RAM
// disk's memory is wired: nothing pages it out, and `nice 19`, which keeps
// test runs off the owner's CPU, does nothing for memory. So however many runs
// overlap, their RAM disks together hold at most half the host's memory: there
// are MemorySeats seats, each a lock file under the registry, and a run holds
// one from before its disk is attached until it is released. A run that finds
// every seat held waits for one, on the kernel's own lock rather than a poll,
// and a seat whose holder died is free at once, because the kernel drops the
// lock with its holder (package seats).

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

// seatPool is the registry's n memory seats: their lock files live in the
// registry's seats directory, never beside the runs' records, which the
// registry reads as every *.lock in it.
func seatPool(registry string, n int) seats.Pool {
	return seats.Pool{Dir: filepath.Join(registry, "seats"), Prefix: "seat", N: n, What: "RAM disk"}
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
	f, i, err := seatPool(m.Registry, n).Take(func(held int) {
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
