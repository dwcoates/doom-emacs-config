package login

import (
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"sync"

	"github.com/creack/pty"

	"claude-repld/internal/dlog"
)

// Default geometry for a freshly spawned login terminal.
//
// 400 columns rather than the conventional 80, and the width is not cosmetic:
// the TUI hard-wraps at the column count and the OAuth URL runs roughly 350
// characters, so a default-width terminal splits the one thing the user has to
// copy across five lines. A viewer replaces this with its real geometry the
// moment it attaches, but the child may have printed the URL before anyone was
// watching, which is why the DAEMON owns the default (endpoint_open_login.proto
// says so in as many words).
const (
	defaultRows uint16 = 60
	defaultCols uint16 = 400
)

// scrollbackCap bounds the retained output replayed to a late viewer.
// Overflow drops from the FRONT, which can bisect an escape sequence —
// harmless here, because a viewer sends its geometry on attach and the
// resulting repaint redraws the screen from scratch.
const scrollbackCap = 256 << 10

// readChunk is the pty read buffer size.
const readChunk = 32 << 10

// session is one running login, keyed by the account root it logs into.
type session struct {
	configDir string
	cmd       *exec.Cmd
	ptmx      *os.File
	log       dlog.Logger
	onExit    func(configDir string)

	mu     sync.Mutex
	scroll []byte
	subs   map[*subscriber]struct{}
	exited bool
}

// newSession wraps an already-started child and its pty.
func newSession(configDir string, cmd *exec.Cmd, ptmx *os.File, log dlog.Logger, onExit func(string)) *session {
	return &session{
		configDir: configDir,
		cmd:       cmd,
		ptmx:      ptmx,
		log:       log,
		onExit:    onExit,
		subs:      map[*subscriber]struct{}{},
	}
}

// Exited reports whether the login child is gone.
func (s *session) Exited() bool {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.exited
}

// attach registers a viewer and hands it the scrollback FIRST, under the same
// lock the reader broadcasts under.
//
// The lock is what makes "replay then live, never missing and never
// duplicating" structural rather than likely: a chunk read between the
// snapshot and the registration cannot exist, because the reader cannot take
// the lock until both have happened.
//
// Attaching to an already-exited login still replays: the final screen is
// exactly what the user needs to read. The terminal frame follows it at once.
func (s *session) attach() *subscriber {
	sub := newSubscriber()

	s.mu.Lock()
	defer s.mu.Unlock()

	if len(s.scroll) > 0 {
		replay := make([]byte, len(s.scroll))
		copy(replay, s.scroll)
		sub.push(Output{Bytes: replay})
	}
	if s.exited {
		sub.push(Output{Closed: true})
		sub.end()
		s.log.Debug("daemon.login.watch", "attached to an exited login; replayed the final screen", dlog.Context{
			"config_dir":   s.configDir,
			"replay_bytes": len(s.scroll),
			"branch":       "attach-exited",
		})
		return sub
	}

	s.subs[sub] = struct{}{}
	s.log.Debug("daemon.login.watch", "login viewer attached", dlog.Context{
		"config_dir":   s.configDir,
		"replay_bytes": len(s.scroll),
		"viewers":      len(s.subs),
		"branch":       "attach",
	})
	return sub
}

// detach unregisters a viewer whose stream ended (its context was cancelled).
func (s *session) detach(sub *subscriber) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if _, ok := s.subs[sub]; !ok {
		return
	}
	delete(s.subs, sub)
	sub.end()
	s.log.Debug("daemon.login.watch", "login viewer detached", dlog.Context{
		"config_dir": s.configDir,
		"viewers":    len(s.subs),
		"branch":     "detach",
	})
}

// write sends keystrokes to the login child.
func (s *session) write(p []byte) error {
	if s.Exited() {
		err := fmt.Errorf("login: the login session under %s has exited", s.configDir)
		s.log.Error("daemon.login.send_keystrokes", "keystrokes refused", dlog.Context{
			"config_dir": s.configDir,
			"bytes":      len(p),
			"branch":     "exited",
			"error":      err.Error(),
		})
		return err
	}
	if _, err := s.ptmx.Write(p); err != nil {
		wrapped := fmt.Errorf("login: writing to the pty under %s: %w", s.configDir, err)
		s.log.Error("daemon.login.send_keystrokes", "keystroke write failed", dlog.Context{
			"config_dir": s.configDir,
			"bytes":      len(p),
			"branch":     "write-error",
			"error":      wrapped.Error(),
		})
		return wrapped
	}
	// Successful writes are per-keystroke and would drown every lifecycle
	// record in the sink, so only the branches that matter are recorded.
	return nil
}

// resize reports the viewer's terminal geometry to the child.
func (s *session) resize(size Resize) error {
	if size.Rows <= 0 || size.Cols <= 0 {
		err := fmt.Errorf("login: refusing a %dx%d geometry (rows and columns must be positive)", size.Rows, size.Cols)
		s.log.Error("daemon.login.send_resize", "resize refused", dlog.Context{
			"config_dir": s.configDir,
			"rows":       size.Rows,
			"cols":       size.Cols,
			"branch":     "invalid-geometry",
			"error":      err.Error(),
		})
		return err
	}
	if s.Exited() {
		err := fmt.Errorf("login: the login session under %s has exited", s.configDir)
		s.log.Error("daemon.login.send_resize", "resize refused", dlog.Context{
			"config_dir": s.configDir,
			"branch":     "exited",
			"error":      err.Error(),
		})
		return err
	}
	if err := pty.Setsize(s.ptmx, size.Winsize()); err != nil {
		wrapped := fmt.Errorf("login: resizing the pty under %s: %w", s.configDir, err)
		s.log.Error("daemon.login.send_resize", "resize failed", dlog.Context{
			"config_dir": s.configDir,
			"rows":       size.Rows,
			"cols":       size.Cols,
			"branch":     "setsize-error",
			"error":      wrapped.Error(),
		})
		return wrapped
	}
	s.log.Debug("daemon.login.send_resize", "login terminal resized", dlog.Context{
		"config_dir": s.configDir,
		"rows":       size.Rows,
		"cols":       size.Cols,
		"branch":     "resized",
	})
	return nil
}

// close kills the login child. The reader sees the resulting EOF and tears the
// session down, so this never blocks on the child.
//
// ORDER MATTERS: the login TUI never exits on its own, so closing the pty
// alone would leave it parked on a read forever.
func (s *session) close() error {
	s.log.Debug("daemon.login.close", "closing a login terminal", dlog.Context{
		"config_dir": s.configDir,
		"branch":     "closing",
	})
	var failure error
	if s.cmd.Process != nil {
		if err := s.cmd.Process.Kill(); err != nil && !errors.Is(err, os.ErrProcessDone) {
			failure = fmt.Errorf("login: killing the login child under %s: %w", s.configDir, err)
			s.log.Error("daemon.login.close", "could not kill the login child", dlog.Context{
				"config_dir": s.configDir,
				"branch":     "kill-error",
				"error":      failure.Error(),
			})
		}
	}
	if err := s.ptmx.Close(); err != nil && !errors.Is(err, os.ErrClosed) {
		wrapped := fmt.Errorf("login: closing the pty under %s: %w", s.configDir, err)
		s.log.Error("daemon.login.close", "could not close the login pty", dlog.Context{
			"config_dir": s.configDir,
			"branch":     "pty-close-error",
			"error":      wrapped.Error(),
		})
		if failure == nil {
			failure = wrapped
		}
	}
	return failure
}

// pump reads the terminal until the child is gone, retaining output for replay
// and fanning it out to every viewer.
func (s *session) pump() {
	buf := make([]byte, readChunk)
	for {
		n, err := s.ptmx.Read(buf)
		if n > 0 {
			chunk := make([]byte, n)
			copy(chunk, buf[:n])
			s.broadcast(chunk)
		}
		if err == nil {
			continue
		}
		// EOF is the ordinary end of a login: the child exited, or close
		// killed it. On a pty a vanished child also surfaces as EIO, which is
		// the same fact wearing a different errno. Anything else is worth a
		// record, but the teardown is identical either way.
		if !errors.Is(err, io.EOF) && !isPtyHangup(err) {
			s.log.Error("daemon.login.read", "login terminal read failed", dlog.Context{
				"config_dir": s.configDir,
				"branch":     "read-error",
				"error":      err.Error(),
			})
		}
		s.finish()
		return
	}
}

// broadcast retains chunk and hands it to every attached viewer.
func (s *session) broadcast(chunk []byte) {
	s.mu.Lock()
	defer s.mu.Unlock()

	s.scroll = append(s.scroll, chunk...)
	if over := len(s.scroll) - scrollbackCap; over > 0 {
		s.scroll = s.scroll[over:]
	}
	for sub := range s.subs {
		sub.push(Output{Bytes: chunk})
	}
}

// finish marks the login over, sends every viewer the terminal frame, and
// reaps the child.
func (s *session) finish() {
	s.mu.Lock()
	s.exited = true
	subs := make([]*subscriber, 0, len(s.subs))
	for sub := range s.subs {
		subs = append(subs, sub)
	}
	s.subs = map[*subscriber]struct{}{}
	s.mu.Unlock()

	for _, sub := range subs {
		sub.push(Output{Closed: true})
		sub.end()
	}

	// Reap outside the lock: Wait blocks until the child is collected and
	// nothing about that needs the session held.
	err := s.cmd.Wait()
	if err != nil {
		s.log.Info("daemon.login.child_exit", "login child exited with a non-zero status", dlog.Context{
			"config_dir": s.configDir,
			"viewers":    len(subs),
			"branch":     "exit-error",
			"error":      err.Error(),
		})
	} else {
		s.log.Info("daemon.login.child_exit", "login child exited cleanly", dlog.Context{
			"config_dir": s.configDir,
			"viewers":    len(subs),
			"branch":     "exit-clean",
		})
	}
	s.onExit(s.configDir)
}
