package dlog

import (
	"errors"
	"fmt"

	"agentrepl/logging"
)

// RunLogBackups is how many prior size generations are retained beside the
// current run log. Opening the daemon no longer consumes a generation; only
// reaching the size cap does.
const RunLogBackups = logging.DefaultBackups

// runLog is the daemon's global sink: the run log and its warnings file
// (agentrepl/logging warnings.go), a warn or error record going only to the
// second. Each appends across process restarts and rotates only at the size
// cap through the shared Go logging implementation.
type runLog struct {
	path     string
	file     *logging.RotatingFile
	warnings *logging.RotatingFile
}

func openRunLog(path string, backups int) (*runLog, error) {
	return openRunLogSized(path, CapBytes, backups)
}

// openRunLogSized is the test seam for the cap. Production always supplies
// the contract's 64 MiB value through openRunLog.
func openRunLogSized(path string, capBytes int64, backups int) (*runLog, error) {
	pair, err := logging.OpenRotatingPair(path, capBytes, backups)
	if err != nil {
		return nil, fmt.Errorf("open the daemon run log: %w", err)
	}
	return &runLog{path: path, file: pair.Log, warnings: pair.Warnings}, nil
}

func (r *runLog) write(level string, line []byte) error {
	file := r.file
	if logging.IsWarningsLevel(level) {
		file = r.warnings
	}
	n, err := file.Write(line)
	if err != nil {
		return fmt.Errorf("%w: append to run log %q: %w", ErrPoisoned, file.Path(), err)
	}
	if n != len(line) {
		return fmt.Errorf("%w: append to run log %q: wrote %d of %d bytes", ErrPoisoned, file.Path(), n, len(line))
	}
	return nil
}

func (r *runLog) close() error {
	var errs []error
	for _, file := range []*logging.RotatingFile{r.file, r.warnings} {
		if err := file.Close(); err != nil {
			errs = append(errs, fmt.Errorf("close run log %q: %w", file.Path(), err))
		}
	}
	return errors.Join(errs...)
}
