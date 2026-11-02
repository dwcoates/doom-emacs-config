//go:build darwin

package persistentwifi

// #include <stdint.h>
import "C"

import "runtime/cgo"

// persistentwifiLidHeard is the IOKit callback's way back into Go: the handle
// names the heard func of the Watch that subscribed. It lives apart from
// lid_darwin.go because a file with an //export may only declare C.
//
//export persistentwifiLidHeard
func persistentwifiLidHeard(handle C.uintptr_t, closed C.int) {
	heard := cgo.Handle(handle).Value().(func(Lid))
	if closed != 0 {
		heard(LidClosed)
		return
	}
	heard(LidOpen)
}
