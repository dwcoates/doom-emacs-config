//go:build !darwin

package tail

import (
	"fmt"
	"syscall"
)

// volumeKey names the volume a file lives on by its device number, which off
// darwin is the block device's major:minor and the same every boot.
func volumeKey(_ string, st *syscall.Stat_t) (string, error) {
	return fmt.Sprintf("%d", uint64(st.Dev)), nil
}
