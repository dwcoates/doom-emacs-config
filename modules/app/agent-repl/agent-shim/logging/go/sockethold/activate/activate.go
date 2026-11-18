// Package activate takes the sockets a service manager created for the
// holder: launchd's Sockets entries on macOS (launch_activate_socket), systemd
// socket units on Linux (LISTEN_FDS). The service manager owns each socket
// for as long as the holder's service is loaded, so not even a crash of the
// holder closes it.
//
// It is its own package because the macOS half needs cgo, which only the
// holder program pays for.
package activate

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

// Socket is one socket the service manager handed the holder.
type Socket struct {
	// Name is the socket's name in the service definition: launchd's Sockets
	// key, or systemd's FileDescriptorName.
	Name string
	// File is the socket's descriptor.
	File *os.File
}

// listenFDsStart is the first descriptor systemd passes (SD_LISTEN_FDS_START).
const listenFDsStart = 3

// fromSystemdEnv reads the sockets systemd passed this process: LISTEN_PID
// names it, LISTEN_FDS counts the descriptors from 3, and LISTEN_FDNAMES
// names them, colon-separated. Every name is required: the holder hands a
// socket out by name.
func fromSystemdEnv(getenv func(string) string, pid int, newFile func(fd uintptr, name string) *os.File) ([]Socket, error) {
	if got := getenv("LISTEN_PID"); got != strconv.Itoa(pid) {
		return nil, fmt.Errorf("activate: LISTEN_PID is %q, not this process %d: systemd passed no sockets to it", got, pid)
	}
	count, err := strconv.Atoi(getenv("LISTEN_FDS"))
	if err != nil || count < 1 {
		return nil, fmt.Errorf("activate: LISTEN_FDS is %q, want a count of at least one socket", getenv("LISTEN_FDS"))
	}
	names := strings.Split(getenv("LISTEN_FDNAMES"), ":")
	if len(names) != count {
		return nil, fmt.Errorf("activate: LISTEN_FDNAMES names %d sockets, LISTEN_FDS counts %d", len(names), count)
	}
	sockets := make([]Socket, 0, count)
	for i, name := range names {
		if name == "" {
			return nil, fmt.Errorf("activate: socket %d has no FileDescriptorName", i)
		}
		fd := uintptr(listenFDsStart + i)
		sockets = append(sockets, Socket{Name: name, File: newFile(fd, name)})
	}
	return sockets, nil
}
