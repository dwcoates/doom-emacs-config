package testenv

import (
	"bufio"
	"encoding/json"
	"errors"
	"fmt"
	"os"
)

// appendJSONLine appends v to the file at path as one JSON line, in a single
// append-mode write, so the tests of one process never interleave theirs.
// what names the file's contents, plural ("log record counts"), for errors.
func appendJSONLine(path, what string, v any) error {
	line, err := json.Marshal(v)
	if err != nil {
		return fmt.Errorf("testenv: encode a line of the %s: %w", what, err)
	}
	f, err := os.OpenFile(path, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0o644)
	if err != nil {
		return fmt.Errorf("testenv: open the %s %s: %w", what, path, err)
	}
	if _, err := f.Write(append(line, '\n')); err != nil {
		return errors.Join(fmt.Errorf("testenv: write the %s %s: %w", what, path, err), f.Close())
	}
	if err := f.Close(); err != nil {
		return fmt.Errorf("testenv: close the %s %s: %w", what, path, err)
	}
	return nil
}

// readJSONLines hands each line of the file at path to handle, with its
// number (from 1). A file nobody wrote reads as no lines, and ok is false;
// the first error handle returns ends the read.
func readJSONLines(path, what string, handle func(n int, line []byte) error) (ok bool, err error) {
	f, err := os.Open(path)
	if errors.Is(err, os.ErrNotExist) {
		return false, nil
	}
	if err != nil {
		return false, fmt.Errorf("testenv: open the %s %s: %w", what, path, err)
	}
	defer f.Close()
	scan := bufio.NewScanner(f)
	scan.Buffer(nil, 64<<20)
	for n := 1; scan.Scan(); n++ {
		if err := handle(n, scan.Bytes()); err != nil {
			return true, err
		}
	}
	if err := scan.Err(); err != nil {
		return true, fmt.Errorf("testenv: read the %s %s: %w", what, path, err)
	}
	return true, nil
}
