// Package login runs the interactive Claude login under a daemon-owned
// pty and fans its terminal out to attached viewers.
//
// WHY THE DAEMON, AND NOT EMACS
//
// The login is a full-screen TUI gated behind stateful prompts (theme
// onboarding on a fresh config root, folder trust on an untrusted cwd)
// before it ever reaches the OAuth step. Nothing can reliably SCRAPE
// that, which is why the old design handed the whole problem to an Emacs
// vterm and let a human read the screen. This package keeps the human in
// the loop and moves the terminal instead: the daemon owns the pty, the
// webapp renders it, and no code anywhere parses the TUI.
//
// WHY ONE LOGIN PER ACCOUNT
//
// A login is scoped to the account it logs INTO, never to the caller
// that asked for it. Two workspaces on the same account must land on the
// same terminal, or a second click would race a second OAuth flow against
// the first. Two DIFFERENT accounts are genuinely independent and run
// concurrently, which is what lets a ~/.claude login and a
// ~/.claude-chesscom login be open at the same time.
package login

import (
	"fmt"
	"io"
	"os"
	"os/exec"
	"sync"

	"claude-repld/internal/pty"
)

// Default geometry for a freshly spawned login terminal.
//
// 400 columns rather than the conventional 80: the TUI hard-wraps its
// output at the column count and the OAuth URL runs ~350 characters, so a
// default-width terminal splits it across five lines. A viewer replaces
// this with its real geometry the moment it attaches, but the child may
// have printed the URL before anyone was watching.
const (
	DefaultRows uint16 = 60
	DefaultCols uint16 = 400
)

// clientBuffer bounds one viewer's outbound queue. A viewer that falls
// this far behind is dropped rather than stalling the reader for
// everyone: the frames it missed would leave its screen corrupt anyway,
// and reconnecting replays the scrollback.
const clientBuffer = 256

// scrollbackCap bounds the retained output replayed to a late viewer.
// Overflow drops from the FRONT, which can bisect an escape sequence —
// harmless here, because a viewer sends its geometry on attach and the
// resulting repaint redraws the screen from scratch.
const scrollbackCap = 128 << 10

// Proc is the live login child: a terminal to read and write, a window to
// resize, and a process to reap. An interface so tests can drive a fake
// terminal instead of spawning a real CLI.
type Proc interface {
	io.ReadWriteCloser
	Resize(rows, cols uint16) error
	Wait() error
}

// StartFunc spawns the login child for ACCOUNT, which is a
// CLAUDE_CONFIG_DIR path ("" meaning the daemon's own default account).
type StartFunc func(account string) (Proc, error)

// Client is one attached terminal viewer.
type Client struct {
	// Out carries raw pty bytes. Closed by the session when the viewer
	// detaches, when it falls too far behind, or when the login exits.
	Out chan []byte
}

// NewClient returns a viewer with the standard outbound buffer.
func NewClient() *Client {
	return &Client{Out: make(chan []byte, clientBuffer)}
}

// Session is one running login, keyed by the account it logs into.
type Session struct {
	account string
	proc    Proc
	logf    func(string, ...any)
	onExit  func(account string)

	mu      sync.Mutex
	scroll  []byte
	clients map[*Client]struct{}
	exited  bool
}

// Account returns the CLAUDE_CONFIG_DIR this login targets.
func (s *Session) Account() string { return s.account }

// Exited reports whether the login child is gone.
func (s *Session) Exited() bool {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.exited
}

// Attach registers a viewer and replays the terminal so far, so a viewer
// that arrives after the child has already drawn its screen sees it.
//
// Attaching to an already-exited login still replays: the final screen is
// exactly what the user needs to read. The channel is closed immediately
// afterwards so the viewer sees the login is over.
func (s *Session) Attach(c *Client) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.clients[c] = struct{}{}
	if len(s.scroll) > 0 {
		replay := make([]byte, len(s.scroll))
		copy(replay, s.scroll)
		c.Out <- replay
	}
	if s.exited {
		delete(s.clients, c)
		close(c.Out)
	}
}

// Detach unregisters a viewer; its Out channel is closed.
func (s *Session) Detach(c *Client) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.detachLocked(c)
}

func (s *Session) detachLocked(c *Client) {
	if _, ok := s.clients[c]; ok {
		delete(s.clients, c)
		close(c.Out)
	}
}

// Write sends keystrokes to the login child.
func (s *Session) Write(p []byte) error {
	if s.Exited() {
		return fmt.Errorf("login: %s has exited", s.describe())
	}
	if _, err := s.proc.Write(p); err != nil {
		return fmt.Errorf("login: write to %s: %w", s.describe(), err)
	}
	return nil
}

// Resize reports the viewer's terminal geometry to the child.
func (s *Session) Resize(rows, cols uint16) error {
	if s.Exited() {
		return fmt.Errorf("login: %s has exited", s.describe())
	}
	if err := s.proc.Resize(rows, cols); err != nil {
		return fmt.Errorf("login: resize %s: %w", s.describe(), err)
	}
	return nil
}

// Close kills the login child. The reader goroutine sees the resulting
// EOF and tears the session down, so this never blocks on the child.
func (s *Session) Close() error {
	return s.proc.Close()
}

// describe names the account for error text, since "" is a real account
// (the CLI's own default root) and reads as nothing at all.
func (s *Session) describe() string {
	if s.account == "" {
		return "the default account"
	}
	return s.account
}

// pump reads the terminal until the child is gone, retaining output for
// replay and fanning it out to every viewer.
func (s *Session) pump() {
	buf := make([]byte, 32<<10)
	for {
		n, err := s.proc.Read(buf)
		if n > 0 {
			chunk := make([]byte, n)
			copy(chunk, buf[:n])
			s.broadcast(chunk)
		}
		if err != nil {
			// EOF is the ordinary end of a login: the child exited, or
			// Close killed it. Anything else is worth a line in the log,
			// but the teardown is identical either way.
			if err != io.EOF {
				s.logf("login: %s: terminal read ended: %v", s.describe(), err)
			}
			s.finish()
			return
		}
	}
}

// broadcast retains CHUNK and hands it to every attached viewer.
func (s *Session) broadcast(chunk []byte) {
	s.mu.Lock()
	defer s.mu.Unlock()

	s.scroll = append(s.scroll, chunk...)
	if over := len(s.scroll) - scrollbackCap; over > 0 {
		s.scroll = s.scroll[over:]
	}

	for c := range s.clients {
		select {
		case c.Out <- chunk:
		default:
			// Too far behind to catch up. Drop it rather than stall every
			// other viewer and the reader with it.
			s.logf("login: %s: dropping a viewer that fell behind", s.describe())
			s.detachLocked(c)
		}
	}
}

// finish marks the login over and releases every viewer.
func (s *Session) finish() {
	s.mu.Lock()
	s.exited = true
	for c := range s.clients {
		delete(s.clients, c)
		close(c.Out)
	}
	s.mu.Unlock()

	// Reap outside the lock: Wait blocks until the child is collected and
	// nothing about that needs the session held.
	if err := s.proc.Wait(); err != nil {
		s.logf("login: %s: child exited: %v", s.describe(), err)
	}
	s.onExit(s.account)
}

// Manager owns the running logins, at most one per account.
type Manager struct {
	start StartFunc
	logf  func(string, ...any)

	mu       sync.Mutex
	sessions map[string]*Session
}

// Config assembles a Manager.
type Config struct {
	// Start spawns the login child. Defaults to Spawn(DefaultCommand).
	Start StartFunc
	Logf  func(string, ...any)
}

// DefaultCommand drops straight into the OAuth flow rather than landing
// in a plain session the user would have to drive by hand.
var DefaultCommand = []string{"claude", "/login"}

// NewManager returns a Manager.
func NewManager(cfg Config) *Manager {
	logf := cfg.Logf
	if logf == nil {
		logf = func(string, ...any) {}
	}
	start := cfg.Start
	if start == nil {
		start = Spawn(DefaultCommand)
	}
	return &Manager{
		start:    start,
		logf:     logf,
		sessions: map[string]*Session{},
	}
}

// Open returns ACCOUNT's live login, starting one if none is running.
//
// Idempotent by design: a second click, or a second workspace on the same
// account, joins the terminal already open rather than racing a second
// OAuth flow against it.
func (m *Manager) Open(account string) (*Session, error) {
	m.mu.Lock()
	defer m.mu.Unlock()

	if sess, ok := m.sessions[account]; ok && !sess.Exited() {
		return sess, nil
	}

	proc, err := m.start(account)
	if err != nil {
		return nil, err
	}
	sess := &Session{
		account: account,
		proc:    proc,
		logf:    m.logf,
		onExit:  m.forget,
		clients: map[*Client]struct{}{},
	}
	m.sessions[account] = sess
	go sess.pump()
	m.logf("login: opened terminal for %s", sess.describe())
	return sess, nil
}

// Get returns ACCOUNT's live login, or nil when none is running.
func (m *Manager) Get(account string) *Session {
	m.mu.Lock()
	defer m.mu.Unlock()
	if sess, ok := m.sessions[account]; ok && !sess.Exited() {
		return sess
	}
	return nil
}

// Close ends ACCOUNT's login. Closing one that is not running is a no-op:
// the caller wanted it gone and it is.
func (m *Manager) Close(account string) error {
	if sess := m.Get(account); sess != nil {
		return sess.Close()
	}
	return nil
}

// CloseAll ends every running login (daemon shutdown).
func (m *Manager) CloseAll() {
	m.mu.Lock()
	running := make([]*Session, 0, len(m.sessions))
	for _, sess := range m.sessions {
		running = append(running, sess)
	}
	m.mu.Unlock()

	for _, sess := range running {
		if err := sess.Close(); err != nil {
			m.logf("login: %s: close: %v", sess.describe(), err)
		}
	}
}

// forget drops an exited login so the next Open starts a fresh one.
func (m *Manager) forget(account string) {
	m.mu.Lock()
	defer m.mu.Unlock()
	if sess, ok := m.sessions[account]; ok && sess.Exited() {
		delete(m.sessions, account)
	}
}

// Spawn returns a StartFunc running COMMAND under a real pty.
func Spawn(command []string) StartFunc {
	return func(account string) (Proc, error) {
		if len(command) == 0 {
			return nil, fmt.Errorf("login: empty login command")
		}
		cmd := exec.Command(command[0], command[1:]...)
		cmd.Env = childEnv(account)
		// Run from the temp dir, NOT from any workspace. The login CLI
		// fires the global Claude Code hooks like any other invocation, and
		// those hooks key on cwd — running it inside a workspace would
		// attribute its session_start/stop sentinels to that workspace and
		// walk its tab through agent states belonging to a login prompt.
		// The account is selected by CLAUDE_CONFIG_DIR, never by cwd, so
		// nothing is lost.
		cmd.Dir = os.TempDir()

		master, err := pty.Start(cmd)
		if err != nil {
			return nil, err
		}
		if err := pty.Setsize(master, DefaultRows, DefaultCols); err != nil {
			_ = master.Close()
			_ = cmd.Process.Kill()
			return nil, err
		}
		return &ptyProc{master: master, cmd: cmd}, nil
	}
}

// childEnv assembles the login child's environment.
//
// CLAUDE_CONFIG_DIR is the account selector, exactly as it is for a
// session shim. An empty account leaves the daemon's own value inherited
// rather than exporting an empty override, which the CLI would read as a
// config root literally named "".
//
// TERM is mandatory rather than decorative: the login is an Ink TUI and
// renders nothing a viewer could read without a terminal type to target.
func childEnv(account string) []string {
	env := append(os.Environ(), "TERM=xterm-256color")
	if account != "" {
		env = append(env, "CLAUDE_CONFIG_DIR="+account)
	}
	return env
}

// ptyProc is a Proc backed by a real pty and child process.
type ptyProc struct {
	master *os.File
	cmd    *exec.Cmd
}

func (p *ptyProc) Read(b []byte) (int, error)  { return p.master.Read(b) }
func (p *ptyProc) Write(b []byte) (int, error) { return p.master.Write(b) }

func (p *ptyProc) Resize(rows, cols uint16) error { return pty.Setsize(p.master, rows, cols) }

func (p *ptyProc) Wait() error { return p.cmd.Wait() }

// Close kills the child before closing the terminal. Order matters: the
// login TUI never exits on its own, so closing the master alone would
// leave it parked on a read forever.
func (p *ptyProc) Close() error {
	if p.cmd.Process != nil {
		_ = p.cmd.Process.Kill()
	}
	return p.master.Close()
}
