package grantedclock

import (
	"fmt"
	"os"
	"strconv"
	"strings"
	"time"
)

// cgroupCPUPressurePath is the cgroup's own CPU pressure file.
//
// The kernel keeps the cumulative "some" stall of a cgroup's CPU pressure
// (PSI): the time at least one task in it sat runnable without a CPU. It
// counts every cause alike: the cgroup's own quota throttling it, and a Docker
// VM too busy to give it the quota at all. MEASURED on the sandbox's kernel
// (7.0.12-linuxkit), three spinners under a 0.5-CPU cap: "some" grew 2.53s of
// a 3.0s wall.
//
// IT IS THE CONTAINER'S STALL, NOT ONE PROCESS'S, BY DESIGN. An answer can
// wait on any process of a scenario -- Xvfb for every X request, WebKit, the
// daemon, the shim -- and a stall charged per process would have to name every
// one of them to never charge starvation to the one waited on. The union over
// the whole container cannot miss one. What it costs is only the other
// direction: a real hang is reported later while the container is saturated,
// never sooner, and never not at all once it is not.
const cgroupCPUPressurePath = "/sys/fs/cgroup/cpu.pressure"

// CgroupCPUStall is the container's cumulative CPU stall: the total time some
// task in it was runnable without a CPU.
func CgroupCPUStall() (time.Duration, error) {
	body, err := os.ReadFile(cgroupCPUPressurePath)
	if err != nil {
		return 0, err
	}
	stall, err := parseCPUPressureSome(string(body))
	if err != nil {
		return 0, fmt.Errorf("%s: %w", cgroupCPUPressurePath, err)
	}
	return stall, nil
}

// parseCPUPressureSome reads the cumulative "some" total, in microseconds,
// out of a PSI pressure file:
//
//	some avg10=0.00 avg60=0.00 avg300=0.00 total=160255
//	full avg10=0.00 avg60=0.00 avg300=0.00 total=160255
func parseCPUPressureSome(body string) (time.Duration, error) {
	for line := range strings.SplitSeq(strings.TrimSpace(body), "\n") {
		fields := strings.Fields(line)
		if len(fields) == 0 || fields[0] != "some" {
			continue
		}
		for _, f := range fields[1:] {
			raw, ok := strings.CutPrefix(f, "total=")
			if !ok {
				continue
			}
			usec, err := strconv.ParseInt(raw, 10, 64)
			if err != nil {
				return 0, fmt.Errorf("the some line's total %q: %w", raw, err)
			}
			return time.Duration(usec) * time.Microsecond, nil
		}
		return 0, fmt.Errorf("the some line carries no total: %q", line)
	}
	return 0, fmt.Errorf("no some line in %q", body)
}
