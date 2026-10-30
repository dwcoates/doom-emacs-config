package tail

import (
	"fmt"
	"syscall"

	"golang.org/x/sys/unix"
)

// volumeKey names the volume a file lives on by its MOUNT POINT.
//
// st_dev IS NOT A VOLUME'S NAME ON DARWIN. APFS hands each mounted volume a
// device number at mount time, so the same data volume is 16777233 one boot
// and 16777232 the next (measured 2026-10-17, -23 and -30). A cursor keyed by
// it stopped matching its file at every reboot, and the first activation of a
// conversation afterwards re-read every transcript in its book from byte 0 —
// a month of history re-written into the store and replayed as live rows.
// The mount point is the same every boot.
func volumeKey(path string, _ *syscall.Stat_t) (string, error) {
	var fs unix.Statfs_t
	if err := unix.Statfs(path, &fs); err != nil {
		return "", fmt.Errorf("tail: reading the volume of %s: %w", path, err)
	}
	mount := unix.ByteSliceToString(fs.Mntonname[:])
	if mount == "" {
		return "", fmt.Errorf("tail: the volume of %s reports no mount point", path)
	}
	return mount, nil
}
