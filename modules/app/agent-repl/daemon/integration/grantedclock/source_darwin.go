package grantedclock

import (
	"os"
	"sync"
)

// Source answers the stall this binary's bounds are kept on. On darwin it is
// this process's tree (processTreeStall): the host keeps no stall for a group
// of processes, and every process a test binary's waits depend on is the
// binary itself or one it started.
//
// ONE TREE PER BINARY: every clock reads the same running total, so two bounds
// open at once never read the tree into two diverging origins.
var Source = sync.OnceValues(func() (Stall, error) {
	return processTreeStall(os.Getpid())
})
