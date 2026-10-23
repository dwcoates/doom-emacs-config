package daemonaddr

import (
	"fmt"

	"agent-repld/internal/recordlock"
)

// THE BOOT CLAIM AND THE SERVING LOCK ARE RECORD LOCKS (recordlock), held by
// the daemon's process and never by a child it forked, so a dead daemon holds
// neither, whatever descriptors its children still have.

// Holder is the process the kernel names as holding a lock (recordlock.Holder).
type Holder = recordlock.Holder

// LockHolders answers the processes the kernel names as holding the boot
// claim and the serving lock beside addrPath right now, without taking
// either. A zero PID is a lock nobody holds, or one held by a flock (see
// Holder). A holder in this process is named as this process.
func LockHolders(addrPath string) (boot, serving Holder, err error) {
	if boot, err = recordlock.HolderOf(LockPath(addrPath)); err != nil {
		return Holder{}, Holder{}, fmt.Errorf("name the boot claim's holder beside %q: %w", addrPath, err)
	}
	if serving, err = recordlock.HolderOf(ServingPath(addrPath)); err != nil {
		return Holder{}, Holder{}, fmt.Errorf("name the serving lock's holder beside %q: %w", addrPath, err)
	}
	return boot, serving, nil
}
