package logsites

import (
	"fmt"
	"os"
	"syscall"
)

// identityOf answers the file identity of PATH, whose stat INFO is: its
// device, inode and birth instant. The birth instant tells a new file on a reused inode from the file
// the inode last held, so a cursor never carries over to it.
func identityOf(path string, info os.FileInfo) (string, error) {
	st, ok := info.Sys().(*syscall.Stat_t)
	if !ok {
		return "", fmt.Errorf("logsites: %s carries no stat record", path)
	}
	return formatIdentity(uint64(st.Dev), st.Ino, st.Birthtimespec.Sec, st.Birthtimespec.Nsec), nil
}
