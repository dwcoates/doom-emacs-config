package harness

import (
	"bytes"
	"encoding/binary"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"strings"
	"syscall"
)

// procEntry is one process-table entry: its pid and its argv joined by
// spaces, the shape `ps -o args=` prints.
type procEntry struct {
	pid  int
	args string
}

// THE PROCESS TABLE IS READ IN-PROCESS, NOT BY EXEC'ING ps. A stray sweep
// lists the table four or five times per teardown (the freeze re-lists until
// stable, a second round confirms, the leak check lists again), and one `ps
// -Ao pid=,args=` costs ~25ms on an idle 740-process host and more under a
// full run: measured across a full run, ReapStrays averaged 131ms over 1,038
// teardowns. Reading the table through the kernel's own interface
// (proctable_darwin.go, proctable_linux.go) answers the same pids and argv
// without a process spawn per listing.

// exitedMidRead answers whether a read of a process's record failed only
// because the process left the table during the listing. On linux that is
// ENOENT when it is gone before the open, and ESRCH when it exits between
// the open of /proc/<pid>/cmdline and the read (seen under a full run: "read
// /proc/1425/cmdline: no such process"). Either way it can no longer run, so
// it is no stray.
func exitedMidRead(err error) bool {
	return errors.Is(err, fs.ErrNotExist) || errors.Is(err, os.ErrProcessDone) || errors.Is(err, syscall.ESRCH)
}

// parseProcArgs2 decodes a darwin `kern.procargs2` buffer: a native-endian
// int32 argc, the executable path NUL-terminated and NUL-padded, then argc
// NUL-terminated arguments (the environment follows and is ignored). It
// answers the arguments joined by single spaces.
func parseProcArgs2(buf []byte) (string, error) {
	if len(buf) < 4 {
		return "", fmt.Errorf("procargs2 buffer of %d bytes holds no argc", len(buf))
	}
	argc := int(int32(binary.LittleEndian.Uint32(buf[:4])))
	if argc < 0 {
		return "", fmt.Errorf("procargs2 argc %d is negative", argc)
	}
	rest := buf[4:]
	end := bytes.IndexByte(rest, 0)
	if end < 0 {
		return "", fmt.Errorf("procargs2 executable path is unterminated")
	}
	rest = rest[end:]
	for len(rest) > 0 && rest[0] == 0 {
		rest = rest[1:]
	}
	args := make([]string, 0, argc)
	for i := 0; i < argc; i++ {
		end := bytes.IndexByte(rest, 0)
		if end < 0 {
			// A REWRITTEN ARGV RUNS OUT OF STRINGS. A process that sets its
			// title (npm's "npm exec vitest run <files>", vitest's "node
			// (vitest)") writes it over its own argument memory, joining its
			// arguments into fewer strings, while the kernel keeps the argc
			// of the exec: the strings run into the environment and then out,
			// with every string the buffer holds complete. A buffer that ends
			// mid-string is an exec still writing it, and is refused, as is
			// one holding no argument at all.
			if len(rest) > 0 || i == 0 {
				return "", fmt.Errorf("procargs2 argument %d of %d is unterminated", i, argc)
			}
			break
		}
		args = append(args, string(rest[:end]))
		rest = rest[end+1:]
	}
	return strings.Join(args, " "), nil
}

// parseProcCmdline decodes a linux `/proc/<pid>/cmdline`: NUL-terminated
// arguments, answered joined by single spaces.
func parseProcCmdline(raw []byte) string {
	return strings.Join(strings.Split(strings.TrimRight(string(raw), "\x00"), "\x00"), " ")
}
