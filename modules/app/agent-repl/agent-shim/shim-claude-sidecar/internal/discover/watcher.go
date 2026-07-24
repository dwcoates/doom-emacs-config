package discover

import (
	"os"
	"path/filepath"

	"github.com/fsnotify/fsnotify"
)

// Watcher wraps fsnotify to supply low-latency change notifications over the
// discovery roots (§7.1). fsnotify is not recursive, so the Watcher watches
// every existing directory under the roots and adds a watch whenever a new
// directory is created. It is the latency path; a periodic Discoverer.Scan is
// the completeness backstop (watch events drop; files appear while down).
type Watcher struct {
	fsw *fsnotify.Watcher
	log Logf
}

// NewWatcher creates a Watcher and adds a recursive set of directory watches
// rooted at each dir in roots that exists.
func NewWatcher(roots []string, log Logf) (*Watcher, error) {
	if log == nil {
		log = func(string, ...any) {}
	}
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
				w.log("discover/watch: add %s failed: %v", path, aerr)
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
			w.log("discover/watch: add %s failed: %v", dir, aerr)
		}
	}
}

// Close stops the watcher.
func (w *Watcher) Close() error { return w.fsw.Close() }
