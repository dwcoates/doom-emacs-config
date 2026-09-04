package shimsocket

import "syscall"

// syscallECONNREFUSED is the errno a dial to a bound-but-unserved AF_UNIX path
// returns. It is named here so the probe's classification reads as one word.
const syscallECONNREFUSED = syscall.ECONNREFUSED
