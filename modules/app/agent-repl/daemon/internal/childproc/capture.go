package childproc

import (
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
)

// Capture holds a child's standard output and standard error in files rather
// than pipes.
//
// A PIPE OUTLIVES ITS WRITER. When a child's output is a pipe, exec copies it
// in goroutines and Wait returns only once every copy of the pipe's write end
// is closed, and every process the child started inherits one. A git whose
// reference-transaction hook left a `ps` stuck in the kernel exited, yet its
// Wait never returned, because the stuck `ps` still held git's stderr open:
// every create waiting on that git hung with it, for good (2026-10-23). An
// *os.File is handed to the child as its own descriptor, nothing copies it,
// and Wait returns the moment the child exits, whatever its descendants hold.
//
// The files are unlinked as soon as they are made, so nothing is left on disk
// whatever happens to the daemon.
type Capture struct {
	stdout *os.File
	stderr *os.File
}

// CaptureOutput points cmd's standard output and standard error at fresh
// unlinked files. The caller runs cmd and calls Finish after Wait, or Close
// when cmd never started.
func CaptureOutput(cmd *exec.Cmd) (*Capture, error) {
	stdout, err := unlinkedFile("stdout")
	if err != nil {
		return nil, err
	}
	stderr, err := unlinkedFile("stderr")
	if err != nil {
		_ = stdout.Close()
		return nil, err
	}
	cmd.Stdout = stdout
	cmd.Stderr = stderr
	return &Capture{stdout: stdout, stderr: stderr}, nil
}

// Finish answers everything the child wrote to each stream and releases the
// files. It is called after Wait: the child has exited, so what it wrote is
// all in the files. Any error means neither stream can be trusted.
func (c *Capture) Finish() (stdout, stderr string, err error) {
	stdout, outErr := readAll(c.stdout)
	stderr, errErr := readAll(c.stderr)
	if err := errors.Join(outErr, errErr, c.Close()); err != nil {
		return "", "", err
	}
	return stdout, stderr, nil
}

// Close releases both files, for a child that never started.
func (c *Capture) Close() error {
	return errors.Join(c.stdout.Close(), c.stderr.Close())
}

// unlinkedFile makes a temporary file and removes its name at once, leaving
// only the open descriptor.
func unlinkedFile(stream string) (*os.File, error) {
	f, err := os.CreateTemp("", "agent-repld-child-"+stream+"-*")
	if err != nil {
		return nil, fmt.Errorf("childproc: make the child's %s file: %w", stream, err)
	}
	if err := os.Remove(f.Name()); err != nil {
		return nil, errors.Join(fmt.Errorf("childproc: unlink the child's %s file %s: %w", stream, f.Name(), err), f.Close())
	}
	return f, nil
}

func readAll(f *os.File) (string, error) {
	if _, err := f.Seek(0, io.SeekStart); err != nil {
		return "", fmt.Errorf("childproc: rewind the child's output: %w", err)
	}
	b, err := io.ReadAll(f)
	if err != nil {
		return "", fmt.Errorf("childproc: read the child's output: %w", err)
	}
	return string(b), nil
}
