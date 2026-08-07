package discover

import (
	"os"
	"path/filepath"

	"agentrepl/shim-claude-sidecar/internal/logging"
	"github.com/fsnotify/fsnotify"
)

// Watcher wraps fsnotify to supply low-latency change notifications over the
// discovery roots (§7.1). fsnotify is not recursive, so the Watcher watches
// every existing directory under the roots and adds a watch whenever a new
// directory is created. It is the latency path; a periodic Discoverer.Scan is
// the completeness backstop (watch events drop; files appear while down).
type Watcher struct {
	fsw *fsnotify.Watcher
	log *logging.Bound
}

// NewWatcher creates a Watcher and adds a recursive set of directory watches
// rooted at each dir in roots that exists.
func NewWatcher(roots []string, log *logging.Bound) (*Watcher, error) {
	fsw, err := fsnotify.NewWatcher()
	if err != nil {
		return nil, err
	}
	w := &Watcher{fsw: fsw, log: log}
	for _, r := range roots {
		w.addTree(r)
	}
	return w, nil
}

// addTree adds a watch on root and every existing subdirectory. Missing roots
// are skipped (they may appear later; Scan + create-events pick them up).
func (w *Watcher) addTree(root string) {
	filepath.WalkDir(root, func(path string, d os.DirEntry, err error) error {
		if err != nil {
			return nil // unreadable subtree: skip, don't abort the walk
		}
		if d.IsDir() {
			if aerr := w.fsw.Add(path); aerr != nil {
				// No watch on this subtree: change detection there falls back
				// to the slow periodic scan, so new bytes are picked up late.
				w.log.With(logging.Context{Operation: "watch-add", Path: path, Level: "warn"}).Log("watch add failed, falling back to the periodic scan for this subtree: %v", aerr)
			}
		}
		return nil
	})
}

// Events returns the underlying event channel. The consumer should, on a
// directory-create event, call AddDir so newly-created project/session dirs are
// watched, and classify file events into targeted polls.
func (w *Watcher) Events() <-chan fsnotify.Event { return w.fsw.Events }

// Errors returns the underlying error channel.
func (w *Watcher) Errors() <-chan error { return w.fsw.Errors }

// AddDir starts watching a directory (e.g. a freshly-created session dir).
func (w *Watcher) AddDir(dir string) {
	if fi, err := os.Stat(dir); err == nil && fi.IsDir() {
		if aerr := w.fsw.Add(dir); aerr != nil {
			// A new session dir with no watch loses its low-latency pickup.
			w.log.With(logging.Context{Operation: "watch-add", Path: dir, Level: "warn"}).Log("watch add failed, falling back to the periodic scan for this directory: %v", aerr)
		}
	}
}

// Close stops the watcher.
func (w *Watcher) Close() error { return w.fsw.Close() }
