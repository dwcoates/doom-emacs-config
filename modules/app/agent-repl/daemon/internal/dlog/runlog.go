package dlog

import (
	"fmt"

	"agentrepl/logging"
)

// RunLogBackups is how many prior size generations are retained beside the
// current run log. Opening the daemon no longer consumes a generation; only
// reaching the size cap does.
const RunLogBackups = logging.DefaultBackups

// runLog is the daemon's global sink. It appends across process restarts and
// rotates only at the size cap through the shared Go logging implementation.
type runLog struct {
	path string
	file *logging.RotatingFile
}

func openRunLog(path string, backups int) (*runLog, error) {
	return openRunLogSized(path, CapBytes, backups)
}

// openRunLogSized is the test seam for the cap. Production always supplies
// the contract's 64 MiB value through openRunLog.
func openRunLogSized(path string, capBytes int64, backups int) (*runLog, error) {
	file, err := logging.OpenRotating(path, capBytes, backups)
	if err != nil {
		return nil, fmt.Errorf("open the daemon run log: %w", err)
	}
	return &runLog{path: path, file: file}, nil
}

func (r *runLog) write(line []byte) error {
	n, err := r.file.Write(line)
	if err != nil {
		return fmt.Errorf("%w: append to run log %q: %w", ErrPoisoned, r.path, err)
	}
	if n != len(line) {
		return fmt.Errorf("%w: append to run log %q: wrote %d of %d bytes", ErrPoisoned, r.path, n, len(line))
	}
	return nil
}

func (r *runLog) close() error {
	if err := r.file.Close(); err != nil {
		return fmt.Errorf("close run log %q: %w", r.path, err)
	}
	return nil
}
