// Package atomicfile replaces a file's whole contents in one step: the new
// contents go to a temp file beside it, which is then renamed over it. A
// concurrent reader sees the old file or the new one, never half of either,
// and a writer that dies midway leaves the old file untouched.
//
// It is the runner's one way to rewrite a file other processes read: the
// host's timing history, its pass records and test_time.csv.
package atomicfile

import (
	"fmt"
	"os"
	"path/filepath"
)

// Replace writes data to path through a temp file named by pattern
// (os.CreateTemp) in path's directory. The temp file never outlives the call.
func Replace(path, pattern string, data []byte) error {
	tmp, err := os.CreateTemp(filepath.Dir(path), pattern)
	if err != nil {
		return fmt.Errorf("create a temp file beside %s: %w", path, err)
	}
	defer os.Remove(tmp.Name())
	if _, err := tmp.Write(data); err != nil {
		tmp.Close()
		return fmt.Errorf("write %s: %w", tmp.Name(), err)
	}
	if err := tmp.Close(); err != nil {
		return fmt.Errorf("close %s: %w", tmp.Name(), err)
	}
	if err := os.Rename(tmp.Name(), path); err != nil {
		return fmt.Errorf("replace %s: %w", path, err)
	}
	return nil
}
