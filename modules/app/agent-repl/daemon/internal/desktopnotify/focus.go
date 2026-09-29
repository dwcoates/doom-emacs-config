// Package desktopnotify is the daemon's desktop notification: it decides
// whether a banner is posted, posts it through the platform's banner program,
// reads the click back, and asks Emacs to select the clicked workspace.
//
// THE DAEMON OWNS EVERY DESKTOP BANNER. Emacs contributes exactly two facts:
// whether it is focused (agentrepl.v1.EditorFocus, on its WatchDaemon request
// and by ReportEditorFocus), and the tab selection a click asks for
// (WatchHostWorkspaceResponse.notification_clicked). The rule is Emacs-wide:
// a focused Emacs, whatever workspace is open in it, gets no banner; an
// unfocused one does.
package desktopnotify

import (
	"errors"
	"sync"

	"claude-repld/internal/dlog"
)

// opFocus is the operation Emacs's focus records carry.
const opFocus = "daemon.desktopnotify.focus"

// ErrNoEmacsStream is a focus report that arrived with no Emacs WatchDaemon
// stream standing: there is no connection for it to belong to.
var ErrNoEmacsStream = errors.New("no Emacs WatchDaemon stream stands on this daemon")

// Focus is Emacs's desktop focus, SCOPED TO ITS WatchDaemon STREAM. The stream
// attaches it with the focus its request carried, reports move it, and the
// stream's end releases it, after which Emacs reads as unfocused. So a banner
// is never decided on a focus older than the connection that reported it.
//
// A LATER STREAM SUPERSEDES AN EARLIER ONE. Emacs opens one WatchDaemon, but a
// reconnect can open the next before the previous one's end is observed; each
// Attach takes a new generation, and only the current generation's release
// clears the focus, so a stale stream ending cannot erase its successor's.
type Focus struct {
	log dlog.Logger

	mu sync.Mutex
	// generation numbers the attached streams; attached is whether the
	// current one still stands.
	generation uint64
	attached   bool
	focused    bool
}

// NewFocus builds an unattached focus: Emacs reads as unfocused until a stream
// attaches.
func NewFocus(log dlog.Logger) *Focus {
	if log == nil {
		panic("desktopnotify: NewFocus requires a logger")
	}
	return &Focus{log: log}
}

// Attach stands an Emacs stream's focus, answering the release its stream
// calls when it ends.
func (f *Focus) Attach(focused bool) (release func()) {
	f.mu.Lock()
	f.generation++
	generation := f.generation
	superseded := f.attached
	f.attached = true
	f.focused = focused
	f.mu.Unlock()
	f.log.Info(opFocus, "an Emacs stream attached its focus", dlog.Context{
		"focused": focused, "generation": generation, "superseded": superseded,
	})
	return func() { f.release(generation) }
}

// release ends one stream's focus, unless a later stream already superseded it.
func (f *Focus) release(generation uint64) {
	f.mu.Lock()
	current := f.attached && f.generation == generation
	if current {
		f.attached = false
		f.focused = false
	}
	f.mu.Unlock()
	if !current {
		f.log.Debug(opFocus, "a superseded Emacs stream ended; the current stream's focus stands", dlog.Context{
			"generation": generation,
		})
		return
	}
	f.log.Info(opFocus, "the Emacs stream ended; Emacs reads as unfocused", dlog.Context{
		"generation": generation,
	})
}

// Report moves the standing stream's focus. ErrNoEmacsStream when none stands.
func (f *Focus) Report(focused bool) error {
	f.mu.Lock()
	attached := f.attached
	if attached {
		f.focused = focused
	}
	generation := f.generation
	f.mu.Unlock()
	if !attached {
		return ErrNoEmacsStream
	}
	f.log.Info(opFocus, "Emacs reported its focus", dlog.Context{
		"focused": focused, "generation": generation,
	})
	return nil
}

// Focused reports whether a standing Emacs stream reports Emacs focused.
func (f *Focus) Focused() bool {
	f.mu.Lock()
	defer f.mu.Unlock()
	return f.attached && f.focused
}
