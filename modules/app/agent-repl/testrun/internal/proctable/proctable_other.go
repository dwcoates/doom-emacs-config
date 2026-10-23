//go:build !darwin

package proctable

import (
	"fmt"
	"runtime"
	"syscall"
)

// List is darwin's alone; anywhere else it fails loudly.
func List() ([]Entry, error) {
	return nil, fmt.Errorf("reading the process table is not implemented on %s: %w", runtime.GOOS, syscall.ENOSYS)
}
