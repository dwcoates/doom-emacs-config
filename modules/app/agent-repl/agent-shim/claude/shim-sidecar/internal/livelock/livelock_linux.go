package livelock

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"syscall"

	"golang.org/x/sys/unix"
)

// procLocks is the kernel's table of every held file lock.
const procLocks = "/proc/locks"

// platformHeld reads /proc/locks ONCE and answers every path from it with one
// stat each. Linux keeps flock(2) apart from POSIX locks, so F_GETLK cannot see
// the shim's flock; the lock table can, and reading it does not take anything.
func platformHeld(paths []string) (map[string]bool, map[string]error) {
	held := make(map[string]bool, len(paths))
	errs := map[string]error{}
	table, err := os.Open(procLocks)
	if err != nil {
		err = fmt.Errorf("livelock: open %s: %w", procLocks, err)
		for _, path := range paths {
			errs[path] = err
		}
		return held, errs
	}
	locked, parseErr := parseFlocks(table)
	closeErr := table.Close()
	if err := errors.Join(parseErr, closeErr); err != nil {
		err = fmt.Errorf("livelock: read %s: %w", procLocks, err)
		for _, path := range paths {
			errs[path] = err
		}
		return held, errs
	}
	for _, path := range paths {
		var st syscall.Stat_t
		if err := syscall.Stat(path, &st); err != nil {
			if errors.Is(err, fs.ErrNotExist) {
				held[path] = false
				continue
			}
			errs[path] = fmt.Errorf("livelock: stat %q: %w", path, err)
			continue
		}
		dev := uint64(st.Dev)
		held[path] = locked[fileKey{major: uint64(unix.Major(dev)), minor: uint64(unix.Minor(dev)), inode: st.Ino}]
	}
	return held, errs
}
