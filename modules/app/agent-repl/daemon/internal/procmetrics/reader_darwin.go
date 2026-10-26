//go:build darwin

package procmetrics

/*
#include <errno.h>
#include <libproc.h>
#include <mach/mach.h>
#include <mach/mach_host.h>
#include <mach/mach_time.h>
#include <sys/resource.h>
#include <sys/sysctl.h>

// Not in a public header, but exported by libSystem: the process macOS holds
// responsible for pid (the app that launched it, for an XPC service the app
// that asked for it).
extern int responsibility_get_pid_responsible_for_pid(pid_t);

static int pm_responsible(pid_t pid) { return responsibility_get_pid_responsible_for_pid(pid); }

static int pm_rusage(pid_t pid, struct rusage_info_v6 *out) {
	return proc_pid_rusage(pid, RUSAGE_INFO_V6, (rusage_info_t *)out);
}

static int pm_bsdinfo(pid_t pid, struct proc_bsdinfo *out) {
	return proc_pidinfo(pid, PROC_PIDTBSDINFO, 0, out, sizeof(*out));
}

static int pm_taskinfo(pid_t pid, struct proc_taskinfo *out) {
	return proc_pidinfo(pid, PROC_PIDTASKINFO, 0, out, sizeof(*out));
}

static int pm_fdbytes(pid_t pid) {
	return proc_pidinfo(pid, PROC_PIDLISTFDS, 0, NULL, 0);
}

static int pm_vm(vm_statistics64_data_t *out, vm_size_t *page) {
	mach_msg_type_number_t count = HOST_VM_INFO64_COUNT;
	mach_port_t host = mach_host_self();
	kern_return_t kr = host_page_size(host, page);
	if (kr == KERN_SUCCESS) {
		kr = host_statistics64(host, HOST_VM_INFO64, (host_info64_t)out, &count);
	}
	mach_port_deallocate(mach_task_self(), host);
	return kr;
}
*/
import "C"

import (
	"bytes"
	"errors"
	"fmt"
	"sync"
	"syscall"
	"unsafe"

	"golang.org/x/sys/unix"

	"agentrepl/logging/hostcpu"
)

// Kernel is the real Reader.
type Kernel struct{}

var (
	timebaseOnce sync.Once
	timebaseNum  float64
	timebaseDen  float64
)

// machToSeconds converts rusage's mach absolute time units to seconds.
func machToSeconds(t C.uint64_t) float64 {
	timebaseOnce.Do(func() {
		var tb C.mach_timebase_info_data_t
		C.mach_timebase_info(&tb)
		timebaseNum, timebaseDen = float64(tb.numer), float64(tb.denom)
	})
	return float64(t) * timebaseNum / timebaseDen / 1e9
}

// PIDs implements Reader.
func (Kernel) PIDs() ([]int, error) {
	n, err := C.proc_listallpids(nil, 0)
	if n <= 0 {
		return nil, fmt.Errorf("procmetrics: count the processes: %v", err)
	}
	buf := make([]C.int, int(n)+64)
	n, err = C.proc_listallpids(unsafe.Pointer(&buf[0]), C.int(len(buf))*C.int(unsafe.Sizeof(buf[0])))
	if n <= 0 {
		return nil, fmt.Errorf("procmetrics: list the processes: %v", err)
	}
	pids := make([]int, 0, n)
	for _, p := range buf[:n] {
		if p > 0 {
			pids = append(pids, int(p))
		}
	}
	return pids, nil
}

// gone answers whether errno says the process no longer exists.
func gone(err error) bool {
	return errors.Is(err, syscall.ESRCH)
}

// Identity implements Reader.
func (Kernel) Identity(pid int) (Identity, error) {
	var bsd C.struct_proc_bsdinfo
	if n, err := C.pm_bsdinfo(C.pid_t(pid), &bsd); n <= 0 {
		if gone(err) || n == 0 {
			return Identity{}, ErrGone
		}
		return Identity{}, fmt.Errorf("procmetrics: read pid %d: %v", pid, err)
	}
	name := C.GoString(&bsd.pbi_name[0])
	if name == "" {
		name = C.GoString(&bsd.pbi_comm[0])
	}
	return Identity{
		PID:             pid,
		PPID:            int(bsd.pbi_ppid),
		Name:            name,
		Responsible:     int(C.pm_responsible(C.pid_t(pid))),
		StartUnixMicros: int64(bsd.pbi_start_tvsec)*1e6 + int64(bsd.pbi_start_tvusec),
	}, nil
}

// Usage implements Reader.
func (Kernel) Usage(pid int) (Usage, error) {
	var ru C.struct_rusage_info_v6
	if rc, err := C.pm_rusage(C.pid_t(pid), &ru); rc != 0 {
		if gone(err) {
			return Usage{}, ErrGone
		}
		return Usage{}, fmt.Errorf("procmetrics: read the usage of pid %d: %v", pid, err)
	}
	var task C.struct_proc_taskinfo
	threads := 0
	if n, _ := C.pm_taskinfo(C.pid_t(pid), &task); n > 0 {
		threads = int(task.pti_threadnum)
	}
	fds := 0
	if n, _ := C.pm_fdbytes(C.pid_t(pid)); n > 0 {
		fds = int(n) / int(C.sizeof_struct_proc_fdinfo)
	}
	return Usage{
		CPUSeconds:     machToSeconds(ru.ri_user_time) + machToSeconds(ru.ri_system_time),
		ResidentBytes:  uint64(ru.ri_resident_size),
		FootprintBytes: uint64(ru.ri_phys_footprint),
		DiskReadBytes:  uint64(ru.ri_diskio_bytesread),
		DiskWriteBytes: uint64(ru.ri_diskio_byteswritten),
		EnergyJoules:   float64(ru.ri_energy_nj) / 1e9,
		Threads:        threads,
		OpenFDs:        fds,
	}, nil
}

// Args implements Reader: kern.procargs2 is argc, the exec path, then argv.
func (Kernel) Args(pid int) ([]string, error) {
	raw, err := unix.SysctlRaw("kern.procargs2", pid)
	if err != nil {
		if errors.Is(err, syscall.ESRCH) || errors.Is(err, syscall.EINVAL) {
			return nil, ErrGone
		}
		return nil, fmt.Errorf("procmetrics: read the argv of pid %d: %w", pid, err)
	}
	if len(raw) < 4 {
		return nil, fmt.Errorf("procmetrics: the argv of pid %d is %d bytes", pid, len(raw))
	}
	argc := int(*(*int32)(unsafe.Pointer(&raw[0])))
	rest := raw[4:]
	// Skip the exec path and the NULs that pad it.
	if i := bytes.IndexByte(rest, 0); i >= 0 {
		rest = rest[i:]
	}
	rest = bytes.TrimLeft(rest, "\x00")
	args := make([]string, 0, argc)
	for len(args) < argc && len(rest) > 0 {
		i := bytes.IndexByte(rest, 0)
		if i < 0 {
			args = append(args, string(rest))
			break
		}
		args = append(args, string(rest[:i]))
		rest = rest[i+1:]
	}
	return args, nil
}

// Host implements Reader.
func (Kernel) Host(path string) (Host, error) {
	load, err := hostcpu.Read()
	if err != nil {
		return Host{}, fmt.Errorf("procmetrics: %w", err)
	}
	cpu := map[string]float64{"user": load.User, "system": load.System, "idle": load.Idle, "nice": load.Nice}
	var vm C.vm_statistics64_data_t
	var page C.vm_size_t
	if kr := C.pm_vm(&vm, &page); kr != C.KERN_SUCCESS {
		return Host{}, fmt.Errorf("procmetrics: host_statistics64(HOST_VM_INFO64) answered %d", int(kr))
	}
	p := uint64(page)
	memory := map[string]uint64{
		"free":       uint64(vm.free_count) * p,
		"active":     uint64(vm.active_count) * p,
		"inactive":   uint64(vm.inactive_count) * p,
		"wired":      uint64(vm.wire_count) * p,
		"compressed": uint64(vm.compressor_page_count) * p,
	}
	total, err := unix.SysctlUint64("hw.memsize")
	if err != nil {
		return Host{}, fmt.Errorf("procmetrics: read hw.memsize: %w", err)
	}
	pressure, err := unix.SysctlUint32("kern.memorystatus_vm_pressure_level")
	if err != nil {
		return Host{}, fmt.Errorf("procmetrics: read kern.memorystatus_vm_pressure_level: %w", err)
	}
	var fs unix.Statfs_t
	if err := unix.Statfs(path, &fs); err != nil {
		return Host{}, fmt.Errorf("procmetrics: statfs %s: %w", path, err)
	}
	return Host{
		CPUSeconds:       cpu,
		MemoryBytes:      memory,
		MemoryTotalBytes: total,
		MemoryPressure:   int(pressure),
		DiskFreeBytes:    fs.Bavail * uint64(fs.Bsize),
		DiskTotalBytes:   fs.Blocks * uint64(fs.Bsize),
	}, nil
}
