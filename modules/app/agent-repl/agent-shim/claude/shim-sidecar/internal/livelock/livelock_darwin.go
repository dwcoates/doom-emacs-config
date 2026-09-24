package livelock

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"syscall"
)

// platformHeld asks F_GETLK about each path. One open, one fcntl and one close
// per path: on the owner's machine that is 132 workspace locks, measured at
// ~1.5ms for the whole set (2026-09-24), which is what a poll tick pays to know
// which workspaces are live.
func platformHeld(paths []string) (map[string]bool, map[string]error) {
	held := make(map[string]bool, len(paths))
	errs := map[string]error{}
	for _, path := range paths {
		locked, err := heldByAnyone(path)
		if err != nil {
			errs[path] = err
			continue
		}
		held[path] = locked
	}
	return held, errs
}

func heldByAnyone(path string) (locked bool, err error) {
	// READ-ONLY AND NEVER CREATED. The probe must not change the file it asks
	// about, and a lock file nobody created cannot be held.
	file, err := os.Open(path)
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			return false, nil
		}
		return false, fmt.Errorf("livelock: open %q: %w", path, err)
	}
	defer func() {
		if closeErr := file.Close(); closeErr != nil && err == nil {
			err = fmt.Errorf("livelock: close %q: %w", path, closeErr)
		}
	}()
	query := syscall.Flock_t{Type: syscall.F_WRLCK, Whence: 0, Start: 0, Len: 0}
	if err := syscall.FcntlFlock(file.Fd(), syscall.F_GETLK, &query); err != nil {
		return false, fmt.Errorf("livelock: F_GETLK %q: %w", path, err)
	}
	return query.Type != syscall.F_UNLCK, nil
}
