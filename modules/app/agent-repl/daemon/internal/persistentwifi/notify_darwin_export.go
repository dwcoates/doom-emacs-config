//go:build darwin

package persistentwifi

// #include <stdint.h>
import "C"

import "runtime/cgo"

// persistentwifiLidHeard is the lid's IOKit callback's way back into Go: the handle
// names the heard func of the Watch that subscribed. It lives apart from
// notify_darwin.go because a file with an //export may only declare C.
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

// persistentwifiPowerSourceHeard is the power source's IOKit callback, as
// persistentwifiLidHeard is the lid's.
//
//export persistentwifiPowerSourceHeard
func persistentwifiPowerSourceHeard(handle C.uintptr_t, battery C.int) {
	cgo.Handle(handle).Value().(func(PowerSource))(powerSourceOf(battery != 0))
}
