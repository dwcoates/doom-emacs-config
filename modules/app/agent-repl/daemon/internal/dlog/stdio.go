package dlog

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"time"

	"agentrepl/logging"
)

// StdioLogName is the file under the state root's logs directory that Emacs's
// detacher appends the daemon's stdout and stderr to (lisp/daemon.el,
// `agent-repl-daemon--stdio-log-path').
const StdioLogName = "daemon.stdio.log"

// stdioLog is the daemon's stdout and stderr when they are the stdio log: a
// RotatingFile on that path whose descriptor is duplicated onto fds 1 and 2.
//
// WHY THE DAEMON TAKES IT OVER. The detacher's `/bin/sh` opens the file for
// append and execs the daemon, and every rollout successor inherits that
// descriptor, so nothing ever closed it, rolled it or aged it: 6 GB by
// 2026-10-19. Re-opening the path as a RotatingFile and pointing fds 1 and 2
// at it gives the file the same cap, generations, shared lock and retention as
// every other daemon log, without changing what Emacs reads (the path's last
// line) or what a successor inherits (fds 1 and 2).
type stdioLog struct {
	path string
	file *logging.RotatingFile
	// fds are the descriptors pointed at the current file: 1 and 2 in
	// production, test-owned descriptors in a test.
	fds []int
	// dup is dup2(2).
	dup func(oldfd, newfd int) error
}

// adoptStdio takes over std's file when std IS the stdio log under logsDir,
// and answers nil when it is not (a terminal, /dev/null, a harness's pipe or
// file): the daemon owns that file only when Emacs's detacher chose it.
func adoptStdio(logsDir string, std *os.File, fds []int, dup func(oldfd, newfd int) error) (*stdioLog, error) {
	path := filepath.Join(logsDir, StdioLogName)
	stdInfo, err := std.Stat()
	if err != nil {
		return nil, fmt.Errorf("stat the daemon's stderr to tell whether it is the stdio log %q: %w", path, err)
	}
	if !stdInfo.Mode().IsRegular() {
		return nil, nil
	}
	pathInfo, err := os.Stat(path)
	if errors.Is(err, fs.ErrNotExist) {
		return nil, nil
	}
	if err != nil {
		return nil, fmt.Errorf("stat the stdio log %q: %w", path, err)
	}
	if !os.SameFile(stdInfo, pathInfo) {
		return nil, nil
	}
	file, err := logging.OpenRotating(path, CapBytes, logging.DefaultBackups)
	if err != nil {
		return nil, fmt.Errorf("open the stdio log %q: %w", path, err)
	}
	l := &stdioLog{path: path, file: file, fds: fds, dup: dup}
	if err := l.point(); err != nil {
		if closeErr := file.Close(); closeErr != nil {
			return nil, errors.Join(err, closeErr)
		}
		return nil, err
	}
	return l, nil
}

// point duplicates the current file's descriptor onto every one of fds.
func (l *stdioLog) point() error {
	f := l.file.File()
	if f == nil {
		return fmt.Errorf("point the daemon's stdio at %q: the rotating file is closed", l.path)
	}
	for _, fd := range l.fds {
		if err := l.dup(int(f.Fd()), fd); err != nil {
			return fmt.Errorf("point fd %d at the stdio log %q: %w", fd, l.path, err)
		}
	}
	return nil
}

// maintain rolls the stdio log at the cap and at expiry, re-points fds at the
// fresh file after either roll, and removes the generations past the
// retention. The cap is read from the file, because every byte reaches it
// through fds 1 and 2 and none through the RotatingFile's own Write.
func (l *stdioLog) maintain(now time.Time) (logging.ExpireResult, error) {
	f := l.file.File()
	if f == nil {
		return logging.ExpireResult{}, fmt.Errorf("maintain the stdio log %q: the rotating file is closed", l.path)
	}
	info, err := f.Stat()
	if err != nil {
		return logging.ExpireResult{}, fmt.Errorf("stat the stdio log %q for its cap: %w", l.path, err)
	}
	rolled := false
	if info.Size() >= CapBytes {
		if err := l.file.Roll(); err != nil {
			return logging.ExpireResult{}, fmt.Errorf("roll the stdio log %q at the cap: %w", l.path, err)
		}
		rolled = true
	}
	result, expireErr := l.file.Expire(now)
	result.RolledCurrent = result.RolledCurrent || rolled
	if result.RolledCurrent {
		if err := l.point(); err != nil {
			return result, errors.Join(expireErr, err)
		}
	}
	return result, expireErr
}

func (l *stdioLog) close() error {
	if err := l.file.Close(); err != nil {
		return fmt.Errorf("close the stdio log %q: %w", l.path, err)
	}
	return nil
}
