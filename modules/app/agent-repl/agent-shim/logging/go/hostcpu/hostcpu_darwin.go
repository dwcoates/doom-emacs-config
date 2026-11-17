//go:build darwin

package hostcpu

/*
#include <mach/mach.h>
#include <mach/mach_host.h>
#include <mach/processor_info.h>

// hostcpu_load sums every processor's ticks into OUT. host_processor_info is
// read rather than host_statistics(HOST_CPU_LOAD_INFO), which the kernel
// rate-limits per task and answers from a cached copy past the limit.
static int hostcpu_load(unsigned long long out[CPU_STATE_MAX]) {
	natural_t count = 0;
	processor_info_array_t info = NULL;
	mach_msg_type_number_t infoCount = 0;
	mach_port_t host = mach_host_self();
	kern_return_t kr = host_processor_info(host, PROCESSOR_CPU_LOAD_INFO, &count, &info, &infoCount);
	mach_port_deallocate(mach_task_self(), host);
	if (kr != KERN_SUCCESS) {
		return kr;
	}
	processor_cpu_load_info_t loads = (processor_cpu_load_info_t)info;
	for (natural_t cpu = 0; cpu < count; cpu++) {
		for (int state = 0; state < CPU_STATE_MAX; state++) {
			out[state] += loads[cpu].cpu_ticks[state];
		}
	}
	vm_deallocate(mach_task_self(), (vm_address_t)info, infoCount * sizeof(integer_t));
	return KERN_SUCCESS;
}
*/
import "C"

import "fmt"

// ticksPerSecond is CLK_TCK on macOS: the unit of host_processor_info's ticks.
const ticksPerSecond = 100

// Read reads the host's CPU times from the kernel (host_processor_info),
// summed over every processor.
func Read() (Times, error) {
	var load [C.CPU_STATE_MAX]C.ulonglong
	if kr := C.hostcpu_load(&load[0]); kr != C.KERN_SUCCESS {
		return Times{}, fmt.Errorf("hostcpu: host_processor_info(PROCESSOR_CPU_LOAD_INFO) answered %d", int(kr))
	}
	return Times{
		User:   float64(load[C.CPU_STATE_USER]) / ticksPerSecond,
		System: float64(load[C.CPU_STATE_SYSTEM]) / ticksPerSecond,
		Idle:   float64(load[C.CPU_STATE_IDLE]) / ticksPerSecond,
		Nice:   float64(load[C.CPU_STATE_NICE]) / ticksPerSecond,
	}, nil
}
