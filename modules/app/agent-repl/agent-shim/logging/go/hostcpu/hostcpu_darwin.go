//go:build darwin

package hostcpu

/*
#include <mach/mach.h>
#include <mach/mach_host.h>

static int hostcpu_load(host_cpu_load_info_data_t *out) {
	mach_msg_type_number_t count = HOST_CPU_LOAD_INFO_COUNT;
	mach_port_t host = mach_host_self();
	kern_return_t kr = host_statistics(host, HOST_CPU_LOAD_INFO, (host_info_t)out, &count);
	mach_port_deallocate(mach_task_self(), host);
	return kr;
}
*/
import "C"

import "fmt"

// ticksPerSecond is CLK_TCK on macOS: the unit of host_statistics' ticks.
const ticksPerSecond = 100

// Read reads the host's CPU times from the kernel (host_statistics).
func Read() (Times, error) {
	var load C.host_cpu_load_info_data_t
	if kr := C.hostcpu_load(&load); kr != C.KERN_SUCCESS {
		return Times{}, fmt.Errorf("hostcpu: host_statistics(HOST_CPU_LOAD_INFO) answered %d", int(kr))
	}
	return Times{
		User:   float64(load.cpu_ticks[C.CPU_STATE_USER]) / ticksPerSecond,
		System: float64(load.cpu_ticks[C.CPU_STATE_SYSTEM]) / ticksPerSecond,
		Idle:   float64(load.cpu_ticks[C.CPU_STATE_IDLE]) / ticksPerSecond,
		Nice:   float64(load.cpu_ticks[C.CPU_STATE_NICE]) / ticksPerSecond,
	}, nil
}
