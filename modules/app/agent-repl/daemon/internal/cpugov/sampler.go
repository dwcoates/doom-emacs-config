package cpugov

import (
	"encoding/binary"
	"fmt"
)

// loadavgSize is darwin's `struct loadavg`: three fixed-point uint32 averages,
// four bytes of alignment, and the int64 fixed-point scale.
const loadavgSize = 24

// parseLoadavg reads the one-minute average out of a raw `vm.loadavg` value.
// A value of any other size, or with no scale, is refused: a load this
// governor cannot read is never read as zero.
func parseLoadavg(raw []byte) (float64, error) {
	if len(raw) != loadavgSize {
		return 0, fmt.Errorf("cpugov: vm.loadavg is %d bytes, want %d", len(raw), loadavgSize)
	}
	load1 := binary.LittleEndian.Uint32(raw[0:4])
	scale := int64(binary.LittleEndian.Uint64(raw[16:24]))
	if scale <= 0 {
		return 0, fmt.Errorf("cpugov: vm.loadavg carries the scale %d", scale)
	}
	return float64(load1) / float64(scale), nil
}
