package ramdisk

import (
	"errors"
	"fmt"
	"path/filepath"
	"slices"
	"time"

	"agentrepl/testrun/internal/seats"
	"agentrepl/testrun/seatwait"
)

// A RUN'S RAM DISK TAKES A MEMORY SEAT. A RAM disk's memory is wired: nothing
// pages it out, and `nice 19`, which keeps test runs off the owner's CPU, does
// nothing for memory. So every run's RAM disk holds a seat from before it is
// attached until it is released, and a seat is granted under two rules:
//
//   - A CEILING: every run's RAM disk together wires at most CeilingMiB, a
//     share of the host's memory (CeilingNum/CeilingDen), so there are
//     MemorySeats seats.
//   - DYNAMIC ADMISSION below it: a seat is granted only while FloorMiB stays
//     available once every seated disk, this one included, has filled
//     (Admits).
//
// Seats go to waiting runs in RANK order, the merge queue's head first, as
// core seats do (cores.Rank). Each seat is a lock file under the registry
// naming its holder (seats.Pool.Admit), and the kernel drops a dead holder's
// seat with it.
//
// HOW A WAITING RUN WAKES. A run refused for memory blocks on the kernel's
// locks of the held seats, waking the moment one is let go, and re-reads the
// memory every MemoryRecheck in between, since the rest of the host frees
// memory with no seat let go. A run behind a full ceiling wakes only on a seat
// let go; a run behind a better rank, only once that run has its seat. Never
// a poll of the seats.
//
// RULING HISTORY.
//   - 2026-10-14: the RAM disk takes a memory seat, half the host's memory.
//   - 2026-10-23: cut to an eighth: three runs' 8 GiB disks held 24 GiB of a
//     48 GiB host while it swapped 20 GiB, and one process wedged in execve
//     under that paging hung every ps on the host.
//   - 2026-11-09: an eighth was one 4 GiB seat on a 48 GiB host, and a merge
//     queue gate waited 2m10s for it behind ordinary workspace runs while the
//     host had about 60% of its memory free. The owner ruled "I'm fine with 20
//     gigs being used if we've got 30 free": a 20 GiB ceiling (five 4 GiB
//     disks) with admission keeping 10 GiB available after the disk, in rank
//     order.

// CeilingNum/CeilingDen is the share of the host's memory every run's RAM
// disk together may wire: five twelfths, 20 GiB of the 48 GiB host the
// ruling was made on. A SHARE, not a fixed 20 GiB: on a smaller host a fixed
// 20 GiB clamped to its memory would leave the host nothing, and on a larger
// one a share grows with it; the absolute FloorMiB, which admission keeps,
// is what protects every host from the disks.
const (
	CeilingNum = 5
	CeilingDen = 12
)

// FloorMiB is the memory that must stay available once every seated disk has
// filled: 10 GiB, the owner's "30 free, 20 used" leaving 10.
const FloorMiB = 10 << 10

// MemoryRecheck is how often a run refused for memory reads the memory again
// while no seat is let go.
const MemoryRecheck = 10 * time.Second

// CeilingMiB is the most every run's RAM disk together may wire on a host of
// totalBytes.
func CeilingMiB(totalBytes uint64) uint64 {
	return (totalBytes >> 20) * CeilingNum / CeilingDen
}

// MemorySeats is how many RAM disks of sizeMiB fit under the ceiling of a
// host of totalBytes; zero when not even one does.
func MemorySeats(totalBytes uint64, sizeMiB int) int {
	return int(CeilingMiB(totalBytes) / uint64(sizeMiB))
}

// Admits reports whether a disk of sizeMiB may be seated while the seated
// disks may wire wiredMiB and availableMiB is available: whether FloorMiB
// stays available once every seated disk, this one included, has filled.
//
// A seated disk is counted at its whole size, though the reading already
// lacks what it wrote so far: a disk's memory is wired as its blocks are first
// written, so the reading less every seated disk's whole size is a lower bound
// on what stays available once they fill, and the floor holds then too.
func Admits(availableMiB, wiredMiB uint64, sizeMiB int) bool {
	return availableMiB >= wiredMiB+uint64(sizeMiB)+FloorMiB
}

// seatPool is the registry's n memory seats: their lock files live in the
// registry's seats directory, never beside the runs' records, which the
// registry reads as every *.lock in it. Their prefix is "ram": each names its
// holder and keeps a watch lock, which the single "seat" seat of the runners
// before 2026-11-09 did not, so the two never share a file.
func seatPool(registry string, n int) seats.Pool {
	return seats.Pool{Dir: filepath.Join(registry, "seats"), Prefix: "ram", N: n, What: "RAM disk"}
}

// wiredMiB is what the held seats' disks may wire together.
func wiredMiB(held []seats.Holder) uint64 {
	var sum uint64
	for _, h := range held {
		sum += uint64(h.Size)
	}
	return sum
}

// seatHolders is the held seats as the seat lines name them.
func seatHolders(held []seats.Holder) []seatwait.Holder {
	out := make([]seatwait.Holder, len(held))
	for i, h := range held {
		out[i] = seatwait.Holder{Workspace: h.Workspace, Pid: h.Pid}
	}
	return out
}

// reasons is each seats wait as the seat lines name it.
var reasons = map[seats.Why]seatwait.Reason{
	seats.Outranked: seatwait.Outranked,
	seats.Full:      seatwait.Ceiling,
	seats.Refused:   seatwait.Memory,
}

// seat takes this run's memory seat for a disk of sizeMiB, stating every
// wait and the grant on the run's log (seatwait's lines, which the merge
// gate draws in the bubble's tests tab).
func (m Manager) seat(sizeMiB int) (*seats.Seat, error) {
	mem, err := m.Memory()
	if err != nil {
		return nil, fmt.Errorf("read the host's memory to size the RAM disk seats: %w", err)
	}
	ceiling := CeilingMiB(mem.TotalBytes)
	n := MemorySeats(mem.TotalBytes, sizeMiB)
	if n < 1 {
		return nil, fmt.Errorf("the host's %d MiB allow RAM disks %d MiB together, less than one %d MiB disk", mem.TotalBytes>>20, ceiling, sizeMiB)
	}
	if !Admits(mem.TotalBytes>>20, 0, sizeMiB) {
		return nil, fmt.Errorf("the host's %d MiB can never seat a %d MiB RAM disk and keep %d MiB available", mem.TotalBytes>>20, sizeMiB, FloorMiB)
	}
	reading := func(available uint64, held []seats.Holder) seatwait.Reading {
		return seatwait.Reading{Holders: seatHolders(held), AvailableMiB: available, WiredMiB: wiredMiB(held),
			CeilingMiB: ceiling, FloorMiB: FloorMiB, Rank: m.Rank}
	}
	latest := mem
	var start time.Time
	var last *seatwait.Wait
	seat, err := seatPool(m.Registry, n).Admit(seats.Admission{
		Me: seats.Holder{Pid: m.Pid, Workspace: m.Workspace, Rank: m.Rank, Size: sizeMiB},
		Check: func(held []seats.Holder) (bool, error) {
			if latest, err = m.Memory(); err != nil {
				return false, fmt.Errorf("read the host's memory to admit a RAM disk seat: %w", err)
			}
			return Admits(latest.AvailableBytes>>20, wiredMiB(held), sizeMiB), nil
		},
		Waiting: func(w seats.Wait) error {
			if w.Why != seats.Refused {
				// The check did not run this turn, so the line's reading is
				// read now.
				if latest, err = m.Memory(); err != nil {
					return fmt.Errorf("read the host's memory to state the RAM disk seat wait: %w", err)
				}
			}
			wait := seatwait.Wait{Reason: reasons[w.Why], Reading: reading(latest.AvailableBytes>>20, w.Held)}
			if last == nil {
				start = m.Now()
			}
			// A wait is stated when it begins and whenever its reason or its
			// holders change; a recheck that changes neither says nothing.
			if last == nil || last.Reason != wait.Reason || !slices.Equal(last.Holders, wait.Holders) {
				m.Log.Infof("%s", wait.Line())
			}
			last = &wait
			return nil
		},
		Recheck: m.Recheck,
		Cut:     m.Cut,
	})
	if errors.Is(err, seats.ErrCut) {
		m.Log.Infof("RAM disk seat: the run was cut after waiting %s; it takes no seat and attaches no disk", m.Now().Sub(start).Round(time.Millisecond))
		return nil, err
	}
	if err != nil {
		if last != nil {
			m.Log.Infof("%s", seatwait.FailedLine(err))
		}
		return nil, err
	}
	waited := ""
	if last != nil {
		waited = m.Now().Sub(start).Round(time.Millisecond).String()
	}
	held := append(slices.Clone(seat.Held), seats.Holder{Pid: m.Pid, Workspace: m.Workspace, Rank: m.Rank, Size: sizeMiB})
	m.Log.Infof("%s", seatwait.TookLine(seat.Index, n, waited, reading(latest.AvailableBytes>>20, held)))
	return seat, nil
}
