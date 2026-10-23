// Package proctable reads the host's process table IN PROCESS, through the
// kernel's proc_info call and kern.procargs2, never by running ps.
//
// WHY NOT ps (2026-10-23). A `timeout 10 docker info` child wedged inside
// execve (state U, unkillable even by SIGKILL) while the host was swapping 20
// GiB. Every `ps` that listed all processes then blocked in the kernel on that
// one process, unkillably, and so did whatever waited on it: a cancelled
// testrun's reap (`ps -axE`) never returned, so the run never ended and never
// detached its 8 GiB RAM disk, which kept the host swapping. A bound on the ps
// child cannot help: killing a process stuck in the kernel does nothing, and
// os/exec's Wait blocks until it exits. pgrep answered throughout.
//
// What blocks is any read of a wedged task's memory map: ps's task info, and
// the kern.proc.all sysctl too (its record carries the process's vmspace),
// which hung as ps did (measured 2026-10-23). proc_info's BSD records and
// kern.procargs2, the reads libproc and pgrep make, answered for every
// process within a millisecond, the wedged one included. This package reads
// those alone.
package proctable

import (
	"strings"
	"time"
)

// Entry is one process.
type Entry struct {
	Pid  int
	Ppid int
	Pgid int
	// State is ps's first state letter: 'I' idle, 'R' runnable, 'S'
	// sleeping, 'T' stopped, 'Z' a zombie.
	State byte
	// Start is when it started, to the microsecond; zero for another user's
	// process, whose start the kernel does not give this user.
	Start time.Time
	// Comm is the kernel's short name for it.
	Comm string
	// Argv and Env are its command line and environment, as it was started.
	// Both are empty when the kernel will not hand them over (another user's
	// process, a zombie, a system process).
	Argv []string
	Env  []string
}

// Line is the process's command line with its environment after it, as
// `ps -E -o args=` prints it; a process whose arguments are unreadable is its
// short name in parentheses, as ps prints it.
func (e Entry) Line() string {
	if len(e.Argv) == 0 {
		return "(" + e.Comm + ")"
	}
	return strings.Join(append(append([]string(nil), e.Argv...), e.Env...), " ")
}

// stateLetter is ps's letter for the kernel's p_stat (sys/proc.h): SIDL 1,
// SRUN 2, SSLEEP 3, SSTOP 4, SZOMB 5.
func stateLetter(stat int8) byte {
	switch stat {
	case 1:
		return 'I'
	case 2:
		return 'R'
	case 3:
		return 'S'
	case 4:
		return 'T'
	case 5:
		return 'Z'
	}
	return '?'
}

// ParseProcArgs reads a kern.procargs2 buffer: a native-endian int32 argc, the
// executable's path, NUL padding, then argc arguments and the environment,
// each NUL-terminated.
func ParseProcArgs(buf []byte) (argv, env []string, err error) {
	if len(buf) < 4 {
		return nil, nil, errShort(len(buf))
	}
	argc := int(int32(nativeEndian.Uint32(buf[:4])))
	rest := buf[4:]
	// The executable's path, then the padding after it.
	i := indexNUL(rest)
	if i < 0 {
		return nil, nil, errNoPath
	}
	rest = rest[i:]
	for len(rest) > 0 && rest[0] == 0 {
		rest = rest[1:]
	}
	var strs []string
	for len(rest) > 0 {
		j := indexNUL(rest)
		if j < 0 {
			strs = append(strs, string(rest))
			break
		}
		if j == 0 {
			// The environment ends at the first empty string.
			if len(strs) >= argc {
				break
			}
		}
		strs = append(strs, string(rest[:j]))
		rest = rest[j+1:]
	}
	if len(strs) < argc {
		// A process that rewrote its own title (node's process.title, which
		// `npm exec` sets) overwrote its argument area in place, so argc no
		// longer counts what is there (measured 2026-10-23: argc 298, 214
		// strings). ps shows what is there; so does this, with no
		// environment, since where argv ends is no longer known.
		return strs, nil, nil
	}
	return strs[:argc], strs[argc:], nil
}

func indexNUL(b []byte) int {
	for i, c := range b {
		if c == 0 {
			return i
		}
	}
	return -1
}
