package proctable

import (
	"bytes"
	"encoding/binary"
	"fmt"
	"syscall"
	"time"
	"unsafe"

	"golang.org/x/sys/unix"
)

// proc_info(2), the call libproc's proc_listallpids and proc_pidinfo make
// (sys/proc_info.h).
const (
	sysProcInfo      = 336 // SYS_proc_info
	callListPids     = 1   // PROC_INFO_CALL_LISTPIDS
	callPidInfo      = 2   // PROC_INFO_CALL_PIDINFO
	allPids          = 1   // PROC_ALL_PIDS
	flavorBSDInfo    = 3   // PROC_PIDTBSDINFO
	flavorBSDShort   = 13  // PROC_PIDT_SHORTBSDINFO
	sizeofBSDInfo    = 136 // struct proc_bsdinfo
	sizeofBSDShort   = 64  // struct proc_bsdshortinfo
	listPidsHeadroom = 64  // pids born between sizing the list and reading it
)

// bsdInfo is struct proc_bsdinfo.
type bsdInfo struct {
	Flags, Status, Xstatus, Pid, Ppid       uint32
	UID, GID, RUID, RGID, SVUID, SVGID, Rfu uint32
	Comm                                    [16]byte
	Name                                    [32]byte
	Nfiles, Pgid, Pjobc, Tdev, Tpgid        uint32
	Nice                                    int32
	StartSec, StartUsec                     uint64
}

// bsdShort is struct proc_bsdshortinfo.
type bsdShort struct {
	Pid, Ppid, Pgid, Status                        uint32
	Comm                                           [16]byte
	Flags, UID, GID, RUID, RGID, SVUID, SVGID, Rfu uint32
}

// List answers every process on the host.
func List() ([]Entry, error) {
	return kernel{listPids: listPids, record: record, shortRecord: shortRecord, procArgs: procArgs}.list()
}

func procInfo(call, pid, flavor int, buf []byte) (int, error) {
	var p unsafe.Pointer
	if len(buf) > 0 {
		p = unsafe.Pointer(&buf[0])
	}
	r, _, e := unix.Syscall6(sysProcInfo, uintptr(call), uintptr(pid), uintptr(flavor), 0, uintptr(p), uintptr(len(buf)))
	if e != 0 {
		return 0, e
	}
	return int(r), nil
}

func listPids() ([]int, error) {
	need, err := procInfo(callListPids, allPids, 0, nil)
	if err != nil {
		return nil, err
	}
	raw := make([]byte, need+listPidsHeadroom*4)
	n, err := procInfo(callListPids, allPids, 0, raw)
	if err != nil {
		return nil, err
	}
	pids := make([]int, 0, n/4)
	for i := 0; i+4 <= n; i += 4 {
		pids = append(pids, int(int32(nativeEndian.Uint32(raw[i:]))))
	}
	return pids, nil
}

// pidInfo reads one flavor of a process's record into out. A process the
// kernel no longer reports (it answers no bytes) is gone, ESRCH.
func pidInfo(pid, flavor, size int, out any) error {
	buf := make([]byte, size)
	n, err := procInfo(callPidInfo, pid, flavor, buf)
	switch {
	case err != nil:
		return err
	case n == 0:
		return syscall.ESRCH
	case n != size:
		return fmt.Errorf("%d bytes, want %d", n, size)
	}
	return binary.Read(bytes.NewReader(buf), nativeEndian, out)
}

func record(pid int) (procRecord, error) {
	var bi bsdInfo
	if err := pidInfo(pid, flavorBSDInfo, sizeofBSDInfo, &bi); err != nil {
		return procRecord{}, err
	}
	return procRecord{
		Ppid: int(bi.Ppid), Pgid: int(bi.Pgid), Status: int8(bi.Status),
		Start: time.Unix(int64(bi.StartSec), int64(bi.StartUsec)*1000),
		Comm:  unix.ByteSliceToString(bi.Comm[:]),
	}, nil
}

func shortRecord(pid int) (procRecord, error) {
	var si bsdShort
	if err := pidInfo(pid, flavorBSDShort, sizeofBSDShort, &si); err != nil {
		return procRecord{}, err
	}
	return procRecord{Ppid: int(si.Ppid), Pgid: int(si.Pgid), Status: int8(si.Status), Comm: unix.ByteSliceToString(si.Comm[:])}, nil
}

func procArgs(pid int) ([]byte, error) { return unix.SysctlRaw("kern.procargs2", pid) }
