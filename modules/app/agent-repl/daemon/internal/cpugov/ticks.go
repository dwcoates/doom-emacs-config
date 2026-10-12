package cpugov

import (
	"fmt"
	"strconv"
	"strings"
)

// foldTicks answers the busy and total ticks between two readings of the
// macOS per-state counters (user, system, idle, nice), each a 32-bit counter
// that wraps: the difference is taken modulo 2^32. A first reading (primed
// false) answers nothing.
func foldTicks(prev, now [4]uint32, primed bool) (busy, total uint64) {
	if !primed {
		return 0, 0
	}
	var delta [4]uint64
	for i := range now {
		delta[i] = uint64(now[i] - prev[i])
	}
	busy = delta[0] + delta[1] + delta[3]
	return busy, busy + delta[2]
}

// parseProcStat reads the aggregate "cpu" line of Linux's /proc/stat: user,
// nice, system, idle, iowait, irq, softirq, steal. Idle and iowait are idle;
// every other state is busy. A file without that line is refused.
func parseProcStat(text string) (busy, total uint64, err error) {
	for _, line := range strings.Split(text, "\n") {
		fields := strings.Fields(line)
		if len(fields) < 5 || fields[0] != "cpu" {
			continue
		}
		var values []uint64
		for _, f := range fields[1:] {
			v, err := strconv.ParseUint(f, 10, 64)
			if err != nil {
				return 0, 0, fmt.Errorf("cpugov: reading /proc/stat's cpu line %q: %w", line, err)
			}
			values = append(values, v)
		}
		// guest and guest_nice are already counted in user and nice.
		if len(values) > 8 {
			values = values[:8]
		}
		for i, v := range values {
			total += v
			if i != 3 && i != 4 {
				busy += v
			}
		}
		return busy, total, nil
	}
	return 0, 0, fmt.Errorf("cpugov: /proc/stat carries no aggregate cpu line")
}
