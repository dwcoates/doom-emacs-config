//go:build !darwin

package audiooutput

import (
	"fmt"
	"runtime"
)

// Read refuses: only macOS's CoreAudio is read here.
func Read() (Device, error) {
	return Device{}, fmt.Errorf("audiooutput: the audio output is not read on %s", runtime.GOOS)
}
