//go:build linux

package hostcpu

import "os"

// Read reads the host's CPU times from /proc/stat.
func Read() (Times, error) {
	data, err := os.ReadFile("/proc/stat")
	if err != nil {
		return Times{}, err
	}
	return parseProcStat(data)
}
