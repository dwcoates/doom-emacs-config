//go:build !darwin && !linux

package cpugov

import (
	"fmt"
	"runtime"
)

// Host has no tick reader on this platform: every reading is refused, loudly,
// and no speculative gate is ever admitted (the head's gate is never governed).
type Host struct{}

// Ticks refuses.
func (*Host) Ticks() (Ticks, error) {
	return Ticks{}, fmt.Errorf("cpugov: no host processor tick reader on %s", runtime.GOOS)
}

// Power refuses: the band decides every speculative gate.
func (*Host) Power() (Power, error) {
	return PowerUnknown, fmt.Errorf("cpugov: no host power source reader on %s", runtime.GOOS)
}
