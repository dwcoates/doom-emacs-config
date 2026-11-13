package cores

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"syscall"
)

// THE GOVERNOR IS WOKEN BY THE LEDGER, NOT BY A CLOCK ALONE. A unit that
// states itself (asks for seats, starts, or gives its seats back) writes a
// byte to the pool's wake FIFO, and the governor, which alone reads it,
// decides again at once on the reads it already has. So a unit waits for the
// governor's decision for as long as the decision takes, not until the next
// Tick. The FIFO carries no data; a full one already holds a wake.

// WakeFile is the FIFO the governor is woken through.
const WakeFile = "core.wake"

// listenWake makes the pool's wake FIFO, as needed, and answers a channel
// that receives once for every wake (several pending wakes being one), and
// the function that stops listening. The governor holds the FIFO open for
// writing too, so the read never ends while no unit has it open.
func listenWake(dir string) (<-chan struct{}, func() error, error) {
	path := filepath.Join(dir, WakeFile)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return nil, nil, fmt.Errorf("make the core seats directory: %w", err)
	}
	if err := syscall.Mkfifo(path, 0o644); err != nil && !errors.Is(err, syscall.EEXIST) {
		return nil, nil, fmt.Errorf("make the governor's wake FIFO %s: %w", path, err)
	}
	st, err := os.Stat(path)
	if err != nil {
		return nil, nil, fmt.Errorf("read the governor's wake FIFO %s: %w", path, err)
	}
	if st.Mode()&os.ModeNamedPipe == 0 {
		return nil, nil, fmt.Errorf("the governor's wake FIFO %s is not a FIFO", path)
	}
	f, err := os.OpenFile(path, os.O_RDWR, 0)
	if err != nil {
		return nil, nil, fmt.Errorf("open the governor's wake FIFO %s: %w", path, err)
	}
	wake := make(chan struct{}, 1)
	go func() {
		buf := make([]byte, 64)
		for {
			if _, err := f.Read(buf); err != nil {
				// Closed by stop: the governor no longer listens.
				return
			}
			select {
			case wake <- struct{}{}:
			default:
			}
		}
	}()
	return wake, f.Close, nil
}

// wakeGovernor wakes the governor listening on dir's FIFO. No FIFO, or no
// governor listening on it, is no failure: the governor that next takes the
// role reads the ledger at its first decision. A full FIFO already holds a
// wake.
func wakeGovernor(dir string) error {
	path := filepath.Join(dir, WakeFile)
	f, err := os.OpenFile(path, os.O_WRONLY|syscall.O_NONBLOCK, 0)
	if errors.Is(err, os.ErrNotExist) || errors.Is(err, syscall.ENXIO) {
		return nil
	}
	if err != nil {
		return fmt.Errorf("open the governor's wake FIFO %s: %w", path, err)
	}
	_, werr := f.Write([]byte{1})
	if errors.Is(werr, syscall.EAGAIN) || errors.Is(werr, syscall.EPIPE) {
		// Full, or its governor stopped listening since the open.
		werr = nil
	}
	if werr != nil {
		werr = fmt.Errorf("wake the governor through %s: %w", path, werr)
	}
	return errors.Join(werr, f.Close())
}
