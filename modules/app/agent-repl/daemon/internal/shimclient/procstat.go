package shimclient

import (
	"fmt"
	"strconv"

	"agent-repld/internal/procgroup"
)

// linuxPFExiting is PF_EXITING (include/linux/sched.h), set as do_exit
// begins, before the process's descriptors close.
const linuxPFExiting = 0x4

// parseProcStatExiting reads the state (field 3) and flags (field 9) of one
// /proc/<pid>/stat line (procgroup.ParseStat).
func parseProcStatExiting(stat string) (bool, error) {
	line, err := procgroup.ParseStat(stat)
	if err != nil {
		return false, err
	}
	fields := line.Fields
	if len(fields) < 7 {
		return false, fmt.Errorf("/proc stat line is short: %q", stat)
	}
	state := fields[0]
	flags, err := strconv.ParseUint(fields[6], 10, 64)
	if err != nil {
		return false, fmt.Errorf("/proc stat flags %q: %w", fields[6], err)
	}
	return state == "Z" || state == "X" || flags&linuxPFExiting != 0, nil
}
