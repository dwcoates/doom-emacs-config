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

// NewMarked is darwin's alone, like List; anywhere else it fails loudly.
func NewMarked(string) (*Marked, error) {
	return nil, fmt.Errorf("reading the CPU of marked processes is not implemented on %s: %w", runtime.GOOS, syscall.ENOSYS)
}
