package harness

import (
	"bytes"
	"encoding/binary"
	"fmt"
	"strings"
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
			return "", fmt.Errorf("procargs2 argument %d of %d is unterminated", i, argc)
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
