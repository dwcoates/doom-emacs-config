package logsites

import (
	"fmt"
	"os"

	"golang.org/x/sys/unix"
)

// identityOf answers the file identity of PATH, whose stat INFO is: its
// device, inode and birth instant. The birth instant tells a new file on a
// reused inode from the file the inode last held, so a cursor never carries
// over to it. A filesystem that records no birth instant cannot key a
// cursor, and is refused.
func identityOf(path string, info os.FileInfo) (string, error) {
	var stx unix.Statx_t
	if err := unix.Statx(unix.AT_FDCWD, path, unix.AT_SYMLINK_NOFOLLOW, unix.STATX_INO|unix.STATX_BTIME, &stx); err != nil {
		return "", fmt.Errorf("logsites: statx %s: %w", path, err)
	}
	if stx.Mask&unix.STATX_BTIME == 0 {
		return "", fmt.Errorf("logsites: the filesystem of %s records no birth instant, so its cursor cannot be keyed", path)
	}
	dev := unix.Mkdev(stx.Dev_major, stx.Dev_minor)
	return formatIdentity(dev, stx.Ino, stx.Btime.Sec, int64(stx.Btime.Nsec)), nil
}
