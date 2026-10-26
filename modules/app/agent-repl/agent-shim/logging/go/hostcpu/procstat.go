package hostcpu

import (
	"bytes"
	"fmt"
	"strconv"
	"strings"
)

// procStatTicksPerSecond is USER_HZ, the unit of /proc/stat's times, which
// Linux fixes at 100 for user space whatever the kernel's own tick rate.
const procStatTicksPerSecond = 100

// parseProcStat reads the aggregate "cpu" line of a /proc/stat. Time spent
// servicing interrupts, and time stolen by a hypervisor, is time the host was
// not free to run anything else, so it counts as system time; time waiting
// for I/O is idle time.
func parseProcStat(data []byte) (Times, error) {
	line, _, _ := bytes.Cut(data, []byte("\n"))
	fields := strings.Fields(string(line))
	// cpu user nice system idle iowait irq softirq steal ...
	if len(fields) < 9 || fields[0] != "cpu" {
		return Times{}, fmt.Errorf("hostcpu: /proc/stat's first line %q is not the aggregate cpu line with at least eight times", line)
	}
	ticks := make([]float64, 8)
	for i := range ticks {
		n, err := strconv.ParseUint(fields[i+1], 10, 64)
		if err != nil {
			return Times{}, fmt.Errorf("hostcpu: /proc/stat's cpu time %d %q: %w", i+1, fields[i+1], err)
		}
		ticks[i] = float64(n) / procStatTicksPerSecond
	}
	user, nice, system, idle, iowait, irq, softirq, steal := ticks[0], ticks[1], ticks[2], ticks[3], ticks[4], ticks[5], ticks[6], ticks[7]
	return Times{User: user, Nice: nice, System: system + irq + softirq + steal, Idle: idle + iowait}, nil
}
