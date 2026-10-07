// Package atomicfile replaces a file's whole content so a reader sees the old
// content or the new, never half of either: the body goes to a temporary in
// the file's own directory, and a rename puts it in place.
//
// Every daemon site that replaces a file this way goes through Replace, so
// the order of the steps and the cleanup of a failed temporary are stated
// once rather than per site.
package atomicfile

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
)

// Options are the ways one site's replace differs from another's.
type Options struct {
	// Pattern names the temporary, as os.CreateTemp takes it. Empty names it
	// `.<base>.*` beside the file.
	Pattern string
	// Mode is the replaced file's permissions. Zero leaves os.CreateTemp's
	// 0600.
	Mode fs.FileMode
	// Sync fsyncs the temporary before it is renamed into place, for a file
	// that must survive a crash, not only a concurrent reader.
	Sync bool
}

// Replace writes body to path atomically. A failure at any step removes the
// temporary, and a removal that fails too is joined to the failure rather than
// dropped. The directory must exist: creating it is the caller's decision.
func Replace(path string, body []byte, opts Options) error {
	pattern := opts.Pattern
	if pattern == "" {
		pattern = "." + filepath.Base(path) + ".*"
	}
	tmp, err := os.CreateTemp(filepath.Dir(path), pattern)
	if err != nil {
		return fmt.Errorf("create a temporary beside %s: %w", path, err)
	}
	name := tmp.Name()
	discard := func(step error) error {
		return errors.Join(step, tmp.Close(), os.Remove(name))
	}
	if _, err := tmp.Write(body); err != nil {
		return discard(fmt.Errorf("write %s: %w", name, err))
	}
	if opts.Mode != 0 {
		if err := tmp.Chmod(opts.Mode); err != nil {
			return discard(fmt.Errorf("set the mode of %s: %w", name, err))
		}
	}
	if opts.Sync {
		if err := tmp.Sync(); err != nil {
			return discard(fmt.Errorf("fsync %s: %w", name, err))
		}
	}
	if err := tmp.Close(); err != nil {
		return errors.Join(fmt.Errorf("close %s: %w", name, err), os.Remove(name))
	}
	if err := os.Rename(name, path); err != nil {
		return errors.Join(fmt.Errorf("rename %s to %s: %w", name, path, err), os.Remove(name))
	}
	return nil
}
