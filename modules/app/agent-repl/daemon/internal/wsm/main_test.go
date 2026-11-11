package wsm

import (
	"fmt"
	"os"
	osexec "os/exec"
	"strconv"
	"strings"
	"testing"

	"golang.org/x/sys/unix"

	"agent-repld/internal/tempdirs/tempdirstest"
)

// lockProbeEnv names the database a re-executed test binary probes for
// SQLite's shared lock (sharedLockHolder), instead of running the tests.
const lockProbeEnv = "WSM_TEST_SHARED_LOCK_PROBE"

// TestMain runs this package's tests under ONE exempt temporary root
// (tempdirstest.Main), and makes a handle opened without WithTemporaryGuard
// exempt that root: this package opens handles at dozens of sites, and the
// default guard is the production one.
func TestMain(m *testing.M) {
	if path := os.Getenv(lockProbeEnv); path != "" {
		os.Exit(probeSharedLock(path))
	}
	productionTemporaryGuard = tempdirstest.RunGuard
	os.Exit(tempdirstest.Main(m))
}

// SQLite's shared byte range on a database file (os_unix.c SHARED_FIRST,
// SHARED_SIZE).
const (
	sqliteSharedFirst = 0x40000002
	sqliteSharedSize  = 510
)

// probeSharedLock prints the pid holding a lock that conflicts with a write
// lock on PATH's shared range, 0 for none. It must run in its own process:
// F_GETLK never reports the asking process's own locks.
func probeSharedLock(path string) int {
	fd, err := unix.Open(path, unix.O_RDWR|unix.O_CLOEXEC, 0)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		return 2
	}
	defer unix.Close(fd)
	lk := unix.Flock_t{Type: unix.F_WRLCK, Whence: 0, Start: sqliteSharedFirst, Len: sqliteSharedSize}
	if err := unix.FcntlFlock(uintptr(fd), unix.F_GETLK, &lk); err != nil {
		fmt.Fprintln(os.Stderr, err)
		return 2
	}
	if lk.Type == unix.F_UNLCK {
		fmt.Println(0)
	} else {
		fmt.Println(lk.Pid)
	}
	return 0
}

// sharedLockHolder answers the pid holding SQLite's shared range on PATH, 0
// for none, asked from a re-executed test binary.
func sharedLockHolder(path string) (int, error) {
	cmd := osexec.Command(os.Args[0])
	cmd.Env = append(os.Environ(), lockProbeEnv+"="+path)
	out, err := cmd.Output()
	if err != nil {
		return 0, fmt.Errorf("probe the shared lock on %s: %w", path, err)
	}
	return strconv.Atoi(strings.TrimSpace(string(out)))
}
