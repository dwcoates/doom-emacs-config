//go:build darwin

package cpugov

/*
#include <mach/mach_host.h>
#include <mach/host_info.h>
#include <mach/mach_init.h>

// The host port is taken once: every mach_host_self() call adds a send right.
static mach_port_t cpugov_host = MACH_PORT_NULL;

static kern_return_t cpugov_ticks(unsigned int ticks[CPU_STATE_MAX]) {
	if (cpugov_host == MACH_PORT_NULL) {
		cpugov_host = mach_host_self();
	}
	host_cpu_load_info_data_t info;
	mach_msg_type_number_t count = HOST_CPU_LOAD_INFO_COUNT;
	kern_return_t kr = host_statistics(cpugov_host, HOST_CPU_LOAD_INFO, (host_info_t)&info, &count);
	if (kr != KERN_SUCCESS) {
		return kr;
	}
	for (int i = 0; i < CPU_STATE_MAX; i++) {
		ticks[i] = info.cpu_ticks[i];
	}
	return KERN_SUCCESS;
}
*/
import "C"

import (
	"fmt"
	"sync"

	"golang.org/x/sys/unix"
)

// Host is the real Sampler on macOS: host_statistics' HOST_CPU_LOAD_INFO, the
// kernel's own count of every processor's ticks in user, system, idle and nice
// time -- what top reports busy from. The counters are 32-bit and wrap, so
// Host folds each reading's difference into 64-bit totals of its own.
type Host struct {
	mu     sync.Mutex
	prev   [4]uint32
	primed bool
	busy   uint64
	total  uint64
}

// Ticks reads the host's processor ticks.
func (h *Host) Ticks() (Ticks, error) {
	var raw [C.CPU_STATE_MAX]C.uint
	if kr := C.cpugov_ticks(&raw[0]); kr != C.KERN_SUCCESS {
		return Ticks{}, fmt.Errorf("cpugov: host_statistics(HOST_CPU_LOAD_INFO) answered %d", int(kr))
	}
	cpus, err := unix.SysctlUint32("hw.logicalcpu")
	if err != nil {
		return Ticks{}, fmt.Errorf("cpugov: reading hw.logicalcpu: %w", err)
	}
	now := [4]uint32{uint32(raw[C.CPU_STATE_USER]), uint32(raw[C.CPU_STATE_SYSTEM]), uint32(raw[C.CPU_STATE_IDLE]), uint32(raw[C.CPU_STATE_NICE])}
	h.mu.Lock()
	defer h.mu.Unlock()
	busy, total := foldTicks(h.prev, now, h.primed)
	h.busy += busy
	h.total += total
	h.prev, h.primed = now, true
	return Ticks{Busy: h.busy, Total: h.total, CPUs: int(cpus)}, nil
}
