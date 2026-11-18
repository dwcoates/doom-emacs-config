//go:build darwin

package activate

/*
#include <launch.h>
#include <stdlib.h>
*/
import "C"

import (
	"fmt"
	"os"
	"syscall"
	"unsafe"
)

// Sockets answers the sockets launchd created under each of NAMES (the
// holder plist's Sockets keys). launchd hands each name exactly one
// descriptor for a listening socket.
func Sockets(names []string) ([]Socket, error) {
	sockets := make([]Socket, 0, len(names))
	for _, name := range names {
		cname := C.CString(name)
		var fds *C.int
		var count C.size_t
		rc := C.launch_activate_socket(cname, &fds, &count)
		C.free(unsafe.Pointer(cname))
		if rc != 0 {
			return nil, fmt.Errorf("activate: launchd holds no socket named %q for this job: %w", name, syscall.Errno(rc))
		}
		list := unsafe.Slice(fds, int(count))
		if len(list) != 1 {
			C.free(unsafe.Pointer(fds))
			return nil, fmt.Errorf("activate: launchd handed %d descriptors for %q, want one", len(list), name)
		}
		sockets = append(sockets, Socket{Name: name, File: os.NewFile(uintptr(list[0]), name)})
		C.free(unsafe.Pointer(fds))
	}
	return sockets, nil
}
