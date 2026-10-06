package login

import (
	"context"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"sync"

	"github.com/creack/pty"

	"claude-repld/internal/dlog"
	"claude-repld/internal/envc"
	"claude-repld/internal/ids"
)

// guardSite is the name the vendor guard records for this call site.
const guardSite = "login"

// manager is the Manager.
type manager struct {
	guard        envc.VendorGuard
	vendorBin    string
	configDirFor ConfigDirFunc
	log          dlog.Logger
	observer     Observer

	mu       sync.Mutex
	sessions map[string]*session
}

// newManager resolves the vendor binary and builds the manager.
func newManager(guard envc.VendorGuard, vendorBin string, configDirFor ConfigDirFunc, log dlog.Logger, observer Observer) *manager {
	m := &manager{
		guard:        guard,
		vendorBin:    vendorBin,
		configDirFor: configDirFor,
		log:          log,
		observer:     observer,
		sessions:     map[string]*session{},
	}
	if m.vendorBin == "" {
		m.vendorBin = os.Getenv(EnvClaudeBin)
	}
	if m.vendorBin == "" {
		m.vendorBin = DefaultVendorBin
	}
	m.log.Debug("daemon.login.new", "login manager built", dlog.Context{
		"vendor_bin": m.vendorBin,
		"guarded":    m.vendorBin == DefaultVendorBin,
	})
	return m
}

// Open implements Manager: start the account root's login, or JOIN the one
// already running for it.
func (m *manager) Open(ctx context.Context, ws ids.WorkspaceID) (string, error) {
	if err := ctx.Err(); err != nil {
		return "", err
	}
	configDir, err := m.route(ws, "open")
	if err != nil {
		return "", err
	}

	m.mu.Lock()
	defer m.mu.Unlock()

	if live, ok := m.sessions[configDir]; ok {
		if !live.Exited() {
			m.log.Debug("daemon.login.open", "joined the login already running for this account root", dlog.Context{
				"workspace":  string(ws),
				"config_dir": configDir,
				"branch":     "joined",
			})
			return configDir, nil
		}
		// AN EXITED FLOW NOT YET FORGOTTEN ENDS HERE, before its successor
		// opens: its exit's forget will find the new flow standing and leave
		// it, so this is the one place its ending can be told.
		delete(m.sessions, configDir)
		m.observer.LoginEnded(configDir)
	}

	sess, err := m.spawn(configDir)
	if err != nil {
		m.log.Error("daemon.login.open", "could not start the login terminal", dlog.Context{
			"workspace":  string(ws),
			"config_dir": configDir,
			"vendor_bin": m.vendorBin,
			"branch":     "spawn-error",
			"error":      err.Error(),
		})
		return "", err
	}
	m.sessions[configDir] = sess
	// Told BEFORE the pump starts, under the lock the exit's forget takes, so
	// the observer always hears the opening before the ending.
	m.observer.LoginOpened(configDir)
	go sess.pump()

	m.log.Info("daemon.login.open", "login terminal opened", dlog.Context{
		"workspace":  string(ws),
		"config_dir": configDir,
		"vendor_bin": m.vendorBin,
		"branch":     "opened",
	})
	return configDir, nil
}

// spawn runs `<vendor bin> /login` on a fresh pty under configDir.
//
// THE GUARD REFUSES ONLY THE DEFAULT BINARY. AGENT_REPL_FORBID_VENDOR_CALLS
// forbids invoking the vendor, and `claude` on PATH is the vendor; an explicit
// path (from the constructor or $AGENT_REPL_CLAUDE_BIN) is by construction
// something else — a fake script in a test — so refusing it would forbid the
// very thing the knob exists to allow. AGENTS.md spells the same rule as
// "login pty with the default binary".
func (m *manager) spawn(configDir string) (*session, error) {
	if m.vendorBin == DefaultVendorBin {
		if err := m.guard.Check(guardSite); err != nil {
			return nil, err
		}
	}

	cmd := exec.Command(m.vendorBin, LoginArg) //nolint:gosec // daemon-configured binary, never client input
	cmd.Env = childEnv(configDir)
	// Run from the temp dir, NOT from any workspace. The login CLI fires the
	// global vendor hooks like any other invocation, and those hooks key on
	// cwd — running it inside a workspace would attribute its session
	// sentinels to that workspace and walk its tab through agent states
	// belonging to a login prompt. The account is selected by
	// CLAUDE_CONFIG_DIR, never by cwd, so nothing is lost.
	cmd.Dir = os.TempDir()

	ptmx, err := pty.Start(cmd)
	if err != nil {
		return nil, fmt.Errorf("login: starting %s %s under %s: %w", m.vendorBin, LoginArg, configDir, err)
	}
	if err := pty.Setsize(ptmx, &pty.Winsize{Rows: defaultRows, Cols: defaultCols}); err != nil {
		_ = ptmx.Close()
		if cmd.Process != nil {
			_ = cmd.Process.Kill()
		}
		_ = cmd.Wait()
		return nil, fmt.Errorf("login: setting the default geometry under %s: %w", configDir, err)
	}
	return newSession(configDir, cmd, ptmx, m.log, m.forget), nil
}

// childEnv assembles the login child's environment.
//
// CLAUDE_CONFIG_DIR is the account selector, exactly as it is for a session
// shim. TERM is mandatory rather than decorative: the login is an Ink TUI and
// renders nothing a viewer could read without a terminal type to target.
func childEnv(configDir string) []string {
	return append(os.Environ(), "TERM=xterm-256color", "CLAUDE_CONFIG_DIR="+configDir)
}

// Watch implements Manager.
func (m *manager) Watch(ctx context.Context, ws ids.WorkspaceID) (<-chan Output, error) {
	if err := ctx.Err(); err != nil {
		return nil, err
	}
	sess, configDir, err := m.session(ws, "watch")
	if err != nil {
		return nil, err
	}
	sub := sess.attach()
	out := sub.start(ctx, func() { sess.detach(sub) })
	m.log.Debug("daemon.login.watch", "login terminal stream opened", dlog.Context{
		"workspace":  string(ws),
		"config_dir": configDir,
		"branch":     "watching",
	})
	return out, nil
}

// SendKeystrokes implements Manager.
func (m *manager) SendKeystrokes(ctx context.Context, ws ids.WorkspaceID, data []byte) error {
	if err := ctx.Err(); err != nil {
		return err
	}
	sess, _, err := m.session(ws, "send_keystrokes")
	if err != nil {
		return err
	}
	return sess.write(data)
}

// SendResize implements Manager.
func (m *manager) SendResize(ctx context.Context, ws ids.WorkspaceID, size Resize) error {
	if err := ctx.Err(); err != nil {
		return err
	}
	sess, _, err := m.session(ws, "send_resize")
	if err != nil {
		return err
	}
	return sess.resize(size)
}

// Close implements Manager. Closing an absent login is SUCCESS: the desired
// state already holds, which is what endpoint_close_login.proto says.
func (m *manager) Close(ctx context.Context, ws ids.WorkspaceID) error {
	if err := ctx.Err(); err != nil {
		return err
	}
	configDir, err := m.route(ws, "close")
	if err != nil {
		return err
	}

	m.mu.Lock()
	sess, ok := m.sessions[configDir]
	m.mu.Unlock()

	if !ok || sess.Exited() {
		m.log.Debug("daemon.login.close", "no login standing for this account root; already closed", dlog.Context{
			"workspace":  string(ws),
			"config_dir": configDir,
			"branch":     "already-absent",
		})
		return nil
	}
	return sess.close()
}

// CloseAll implements Manager.
func (m *manager) CloseAll(_ context.Context) {
	m.mu.Lock()
	running := make([]*session, 0, len(m.sessions))
	for _, sess := range m.sessions {
		running = append(running, sess)
	}
	m.mu.Unlock()

	m.log.Info("daemon.login.close_all", "closing every login terminal", dlog.Context{
		"count":  len(running),
		"branch": "closing-all",
	})
	for _, sess := range running {
		// session.close owns both the attempt and its canonical error record.
		_ = sess.close()
	}
}

// route resolves the workspace's account root, which is what a login is keyed
// by. A workspace whose account cannot be determined has no login to address.
func (m *manager) route(ws ids.WorkspaceID, verb string) (string, error) {
	configDir, err := m.configDirFor(ws)
	if err != nil {
		wrapped := fmt.Errorf("login: resolving the account root for workspace %q: %w", ws, err)
		m.log.Error("daemon.login."+verb, "could not resolve the workspace's account root", dlog.Context{
			"workspace": string(ws),
			"branch":    "route-error",
			"error":     wrapped.Error(),
		})
		return "", wrapped
	}
	if configDir == "" {
		err := fmt.Errorf("login: workspace %q resolved to an empty account root", ws)
		m.log.Error("daemon.login."+verb, "workspace resolved to an empty account root", dlog.Context{
			"workspace": string(ws),
			"branch":    "empty-config-dir",
			"error":     err.Error(),
		})
		return "", err
	}
	return configDir, nil
}

// ErrNoSession is the refusal when a verb addresses a login that is not
// standing. It is typed so the transport can answer the intended arm rather
// than string-matching a message.
var ErrNoSession = errors.New("login: no login session is standing for this account root")

// session resolves the workspace's live login, refusing when none stands.
func (m *manager) session(ws ids.WorkspaceID, verb string) (*session, string, error) {
	configDir, err := m.route(ws, verb)
	if err != nil {
		return nil, "", err
	}

	m.mu.Lock()
	sess, ok := m.sessions[configDir]
	m.mu.Unlock()

	if !ok {
		// A LANDED TYPED REFUSAL IS AN ORDINARY ANSWER. SendLoginInputError
		// carries `no_login_open` and WatchLoginTerminal's refused open is
		// transport-closed by ruling, so neither is a warning: asking about a
		// login nobody opened is a state the contract spells, not a fault.
		m.log.Info("daemon.login."+verb, "no login session standing for this account root", dlog.Context{
			"workspace":  string(ws),
			"config_dir": configDir,
			"branch":     "no-session",
			"error":      ErrNoSession.Error(),
		})
		return nil, "", fmt.Errorf("%w (%s)", ErrNoSession, configDir)
	}
	return sess, configDir, nil
}

// forget drops an exited login so the next Open starts a fresh one rather than
// joining a corpse.
func (m *manager) forget(configDir string) {
	m.mu.Lock()
	defer m.mu.Unlock()
	if sess, ok := m.sessions[configDir]; ok && sess.Exited() {
		delete(m.sessions, configDir)
		m.log.Debug("daemon.login.forget", "removed an exited login terminal", dlog.Context{
			"config_dir": configDir,
			"branch":     "removed",
		})
		m.observer.LoginEnded(configDir)
	}
}
