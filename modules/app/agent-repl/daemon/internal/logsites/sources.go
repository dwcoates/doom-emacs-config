package logsites

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"sort"
)

// A Dir is one directory whose log files the pass reads: every regular file
// in it whose base Owned matches. The pass reads each sink's current file and
// its rotated generations alike, never a warnings twin (`*.warn.log`, a copy
// of its sink's warnings and errors, which would count them twice) or a
// file that is not JSONL (a service's raw stdout and stderr).
type Dir struct {
	Path  string
	Owned func(base string) bool
}

// generation is a rotated generation's suffix (`.1` through `.N`).
const generation = `(\.[1-9][0-9]*)?`

// stateLogFile matches every JSONL log under the state root's logs
// directory (logging-contract.md "Persistence layout"): the run logs, the
// Emacs central log, and every workspace target of every runtime.
var stateLogFile = regexp.MustCompile(`^((daemon|thaw-guard)\.run\.log|emacs\.central\.log|agent-repl-[0-9a-f]+-(daemon|shim|webapp|sidecar|emacs)-[A-Za-z0-9]+\.log)` + generation + `$`)

// cacheLogFile matches every agent-repl service's own JSONL log under the
// cache root's log directory: the store, the sidecar, zettel, the test
// service and the socket holders.
var cacheLogFile = regexp.MustCompile(`^(shim-store|shim-claude-sidecar|zettel|tests|sockethold-[a-z]+)\.log` + generation + `$`)

// StateLogs is the state root's logs directory, as a Dir.
func StateLogs(dir string) Dir { return Dir{Path: dir, Owned: stateLogFile.MatchString} }

// CacheLogs is the cache root's log directory, as a Dir.
func CacheLogs(dir string) Dir { return Dir{Path: dir, Owned: cacheLogFile.MatchString} }

// A source is one file the pass reads.
type source struct {
	path string
	info os.FileInfo
}

// listSources answers every file of DIRS the pass reads, sorted by path. A
// directory that does not exist yet holds nothing to read. A file that goes
// between its listing and its stat was pruned, and is not read.
func listSources(dirs []Dir) ([]source, error) {
	var out []source
	for _, d := range dirs {
		entries, err := os.ReadDir(d.Path)
		if errors.Is(err, fs.ErrNotExist) {
			continue
		}
		if err != nil {
			return nil, fmt.Errorf("logsites: list %s: %w", d.Path, err)
		}
		for _, e := range entries {
			if !d.Owned(e.Name()) {
				continue
			}
			path := filepath.Join(d.Path, e.Name())
			info, err := os.Lstat(path)
			if errors.Is(err, fs.ErrNotExist) {
				continue
			}
			if err != nil {
				return nil, fmt.Errorf("logsites: stat %s: %w", path, err)
			}
			if !info.Mode().IsRegular() {
				continue
			}
			out = append(out, source{path: path, info: info})
		}
	}
	sort.Slice(out, func(i, j int) bool { return out[i].path < out[j].path })
	return out, nil
}

// formatIdentity is a file's identity as its cursor is keyed.
func formatIdentity(dev, ino uint64, bornSec, bornNsec int64) string {
	return fmt.Sprintf("%d:%d:%d.%09d", dev, ino, bornSec, bornNsec)
}
