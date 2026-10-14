//go:build linux

package shimclient

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
)

// procExiting answers whether pid's process has begun exiting: PF_EXITING, a
// zombie or dead state, or gone, read from /proc/<pid>/stat.
func procExiting(pid int) (bool, error) {
	raw, err := os.ReadFile(fmt.Sprintf("/proc/%d/stat", pid))
	if errors.Is(err, fs.ErrNotExist) {
		return true, nil
	}
	if err != nil {
		return false, fmt.Errorf("read /proc/%d/stat: %w", pid, err)
	}
	return parseProcStatExiting(string(raw))
}
