package harness

import (
	"bytes"
	"context"
	"crypto/tls"
	"errors"
	"fmt"
	"net"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"syscall"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/integration/fakegit"

	"connectrpc.com/connect"
	"golang.org/x/net/http2"
)

// DefaultTimeout bounds every wait the harness performs. It is a failure
// bound, never a synchronization device.
const DefaultTimeout = 30 * time.Second

// pollInterval is how often a file-existence wait re-checks. Nothing in the
// harness sleeps to let another party make progress.
const pollInterval = 5 * time.Millisecond

// Opts configures one daemon process.
type Opts struct {
	// StateDir overrides the state root; empty mints a fresh temp one.
	StateDir string
	// Joining, when set, starts the daemon in joining mode against the address.
	Joining string
	// IdleCutoff sets the hibernation idle cutoff via --idle-cutoff.
	IdleCutoff time.Duration
	// IdleCutoffMS compresses the same cutoff via
	// AGENT_REPL_HIBERNATE_IDLE_CUTOFF_MS, the spelling the hibernation tests
	// use so the cutoff can be a handful of milliseconds.
	IdleCutoffMS int
	// Pprof sets the profiling listener address; empty leaves it off.
	Pprof string
	// SelfRepo names the daemon's own checkout via AGENT_REPL_SELF_REPO_DIR,
	// so a merge target can be recognized as the emacs repo. The daemon keeps
	// the self-reload trigger OFF under this override.
	SelfRepo string
	// MultiRepoRoot is the tree whose workspaces use the multi-repo account.
	MultiRepoRoot string
	// DefaultAccountEmail is written into the default config root's
	// .claude.json; empty leaves the root logged out.
	DefaultAccountEmail string
	// MultiRepoAccountEmail is written into the multi-repo config root.
	MultiRepoAccountEmail string
	// JSONCodec dials the daemon with the JSON codec instead of binary.
	JSONCodec bool
	// ExpectEarlyExit stops the harness from failing when the daemon exits on
	// its own, for the tests whose subject is a refusal to boot.
	ExpectEarlyExit bool
	// ExtraArgs and ExtraEnv are appended verbatim.
	ExtraArgs []string
	ExtraEnv  []string
}

// Daemon is one running claude-repld process and the client dialed to it.
type Daemon struct {
	// StateDir is the daemon's state root.
	StateDir string
	// Addr is the loopback address it serves on, read from daemon.addr.
	Addr string
	// ProfileDir holds the fake shim's per-workspace startup profiles.
	ProfileDir string
	// LockDir is the redirected kernel-lock directory.
	LockDir string
	// Browser, Deploy record what the daemon invoked.
	Browser *Recorder
	Deploy  *Recorder
	// PromptsDir is the copy of prompts/ the daemon reads its briefs from.
	PromptsDir string
	// WebappDir is the served dist.
	WebappDir string
	// DefaultConfigDir and MultiRepoConfigDir are the two account roots.
	DefaultConfigDir   string
	MultiRepoConfigDir string
	// StoreSocket is the store path nothing listens on.
	StoreSocket string
	// Git is the fake git world every scripted `git` answers from.
	Git *GitWorld

	t      *testing.T
	ctx    context.Context
	cmd    *exec.Cmd
	stderr *syncBuffer
	client agentreplv1connect.AgentReplClient
	http   *http.Client

	mu            sync.Mutex
	workspaceDirs []string
	exited        bool
	exitErr       error
	waitOnce      sync.Once
	expected      map[string]bool
	shims         map[string]*ShimControl
}

// installFakeGit copies the scripted `git` into the directory that leads the
// daemon's PATH.
func installFakeGit(t *testing.T, dir string) {
	t.Helper()
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("harness: mkdir %s: %v", dir, err)
	}
	body, err := os.ReadFile(FakeGitBinary(t))
	if err != nil {
		t.Fatalf("harness: read the fake git: %v", err)
	}
	if err := os.WriteFile(filepath.Join(dir, "git"), body, 0o755); err != nil {
		t.Fatalf("harness: install the fake git: %v", err)
	}
}

// gitEnvKeys are the repository-selecting variables that must never be
// inherited by a git child: a hook-leaked GIT_DIR is a real, previously
// observed source of bogus work-tree errors.
var gitEnvKeys = []string{
	"GIT_DIR", "GIT_WORK_TREE", "GIT_INDEX_FILE", "GIT_COMMON_DIR",
	"GIT_PREFIX", "GIT_OBJECT_DIRECTORY", "GIT_ALTERNATE_OBJECT_DIRECTORIES",
}

func cleanGitEnv(env []string) []string {
	out := make([]string, 0, len(env))
	for _, kv := range env {
		key, _, _ := strings.Cut(kv, "=")
		drop := false
		for _, bad := range gitEnvKeys {
			if key == bad {
				drop = true
				break
			}
		}
		if !drop {
			out = append(out, kv)
		}
	}
	return out
}

// syncBuffer collects a process's stderr without racing the reader.
type syncBuffer struct {
	mu  sync.Mutex
	buf bytes.Buffer
}

func (b *syncBuffer) Write(p []byte) (int, error) {
	b.mu.Lock()
	defer b.mu.Unlock()
	return b.buf.Write(p)
}

func (b *syncBuffer) String() string {
	b.mu.Lock()
	defer b.mu.Unlock()
	return b.buf.String()
}

// StartDaemon lays out a hermetic environment, starts the daemon, waits for
// daemon.addr, and dials it. Every process it starts is killed on cleanup.
func StartDaemon(t *testing.T, opts Opts) *Daemon {
	t.Helper()
	binary := DaemonBinary(t)
	root := t.TempDir()

	d := &Daemon{
		StateDir:           opts.StateDir,
		ProfileDir:         filepath.Join(root, "shim-profiles"),
		LockDir:            filepath.Join(root, "locks"),
		PromptsDir:         CopyPrompts(t, filepath.Join(root, "prompts")),
		WebappDir:          NewFakeWebappDist(t, filepath.Join(root, "dist")),
		DefaultConfigDir:   NewConfigRoot(t, filepath.Join(root, "config-default"), accountEmail(opts.DefaultAccountEmail, "default@example.invalid")),
		MultiRepoConfigDir: NewConfigRoot(t, filepath.Join(root, "config-multi"), accountEmail(opts.MultiRepoAccountEmail, "multi@example.invalid")),
		StoreSocket:        filepath.Join(root, "store.sock"),
		Browser:            NewFakeBrowser(t, filepath.Join(root, "bin")),
		Deploy:             NewFakeDeployScript(t, filepath.Join(root, "bin")),
		t:                  t,
		stderr:             &syncBuffer{},
		expected:           map[string]bool{},
		shims:              map[string]*ShimControl{},
	}
	if d.StateDir == "" {
		d.StateDir = filepath.Join(root, "state")
		if err := os.MkdirAll(d.StateDir, 0o755); err != nil {
			t.Fatalf("harness: mkdir state root: %v", err)
		}
	}
	for _, dir := range []string{d.ProfileDir, d.LockDir} {
		if err := os.MkdirAll(dir, 0o755); err != nil {
			t.Fatalf("harness: mkdir %s: %v", dir, err)
		}
	}
	mainJS := filepath.Join(root, "main.js")
	writeFile(t, mainJS, "// placeholder shim module\n")
	fakeBin := filepath.Join(root, "bin")
	fakeClaude := NewFakeClaude(t, fakeBin)
	// The scripted `git` goes first on the daemon's PATH, so every git fact the
	// daemon reads comes out of this test's fixture file and the real binary is
	// never reached.
	d.Git = World(t)
	installFakeGit(t, fakeBin)

	multiRoot := opts.MultiRepoRoot
	if multiRoot == "" {
		multiRoot = filepath.Join(root, "multi")
		if err := os.MkdirAll(multiRoot, 0o755); err != nil {
			t.Fatalf("harness: mkdir multi root: %v", err)
		}
	}

	ctx, cancel := context.WithTimeout(context.Background(), DefaultTimeout)
	t.Cleanup(cancel)
	d.ctx = ctx

	args := []string{
		"--state-dir", d.StateDir,
		"--fake",
		"--node", FakeShimBinary(t),
		"--shim-main", mainJS,
		"--webapp-dist", d.WebappDir,
		"--store-socket", d.StoreSocket,
		"--prompts-dir", d.PromptsDir,
		"--default-config-dir", d.DefaultConfigDir,
		"--multi-repo-config-dir", d.MultiRepoConfigDir,
	}
	if opts.Joining != "" {
		args = append(args, "--joining", opts.Joining)
	}
	if opts.IdleCutoff > 0 {
		args = append(args, "--idle-cutoff", opts.IdleCutoff.String())
	}
	if opts.Pprof != "" {
		args = append(args, "--pprof", opts.Pprof)
	}
	args = append(args, opts.ExtraArgs...)

	env := append(os.Environ(),
		"AGENT_REPL_STATE_DIR="+d.StateDir,
		"AGENT_REPL_FORBID_VENDOR_CALLS=1",
		"AGENT_REPL_LOCK_DIR="+d.LockDir,
		"AGENT_REPL_STORE_SOCKET="+filepath.Join(root, "unused-store.sock"),
		"MULTI_REPO_ROOT="+multiRoot,
		"AGENT_REPL_BROWSER_CMD="+d.Browser.Path,
		"AGENT_REPL_DEPLOY_SCRIPT="+d.Deploy.Path,
		"AGENT_REPL_CLAUDE_BIN="+fakeClaude,
		fakegit.EnvStateFile+"="+d.Git.StateFile,
		"FAKESHIM_PROFILE_DIR="+d.ProfileDir,
		"HOME="+root,
		"PATH="+fakeBin+string(os.PathListSeparator)+os.Getenv("PATH"),
	)
	if opts.SelfRepo != "" {
		env = append(env, "AGENT_REPL_SELF_REPO_DIR="+opts.SelfRepo)
	}
	if opts.IdleCutoffMS > 0 {
		env = append(env, "AGENT_REPL_HIBERNATE_IDLE_CUTOFF_MS="+strconv.Itoa(opts.IdleCutoffMS))
	}
	env = append(env, opts.ExtraEnv...)

	cmd := exec.Command(binary, args...)
	cmd.Dir = root
	cmd.Env = cleanGitEnv(env)
	cmd.Stderr = d.stderr
	cmd.Stdout = d.stderr
	cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
	if err := cmd.Start(); err != nil {
		t.Fatalf("harness: start daemon: %v", err)
	}
	d.cmd = cmd
	t.Cleanup(d.Kill)

	if opts.ExpectEarlyExit {
		return d
	}
	if opts.Joining == "" {
		d.Addr = d.AwaitAddrFile()
		d.dial(opts.JSONCodec)
	}
	return d
}

func accountEmail(given, fallback string) string {
	if given == LoggedOut {
		return ""
	}
	if given != "" {
		return given
	}
	return fallback
}

// LoggedOut asks StartDaemon for an account root with no .claude.json.
const LoggedOut = "\x00logged-out"

// AddrFile is the state root's daemon.addr path.
func (d *Daemon) AddrFile() string { return filepath.Join(d.StateDir, "daemon.addr") }

// AwaitAddrFile waits for daemon.addr to appear and answers its address,
// asserting the contracted `127.0.0.1:<port>\n` shape.
func (d *Daemon) AwaitAddrFile() string {
	d.t.Helper()
	raw := d.awaitFile(d.AddrFile())
	if !strings.HasSuffix(raw, "\n") {
		d.t.Fatalf("daemon.addr = %q, want a trailing newline", raw)
	}
	addr := strings.TrimSuffix(raw, "\n")
	host, _, err := net.SplitHostPort(addr)
	if err != nil {
		d.t.Fatalf("daemon.addr = %q, want 127.0.0.1:<port>: %v", raw, err)
	}
	if host != "127.0.0.1" {
		d.t.Fatalf("daemon.addr host = %q, want the loopback address", host)
	}
	return addr
}

// awaitFile polls for a file, bounded by the daemon's context.
func (d *Daemon) awaitFile(path string) string {
	d.t.Helper()
	ticker := time.NewTicker(pollInterval)
	defer ticker.Stop()
	for {
		if body, err := os.ReadFile(path); err == nil && len(body) > 0 {
			return string(body)
		}
		if d.Exited() && !d.expectedExit() {
			d.t.Fatalf("daemon exited before writing %s (exit %v)\nstderr:\n%s", filepath.Base(path), d.exitErr, d.Stderr())
		}
		select {
		case <-ticker.C:
		case <-d.ctx.Done():
			d.t.Fatalf("waiting for %s: %v\nstderr:\n%s", path, d.ctx.Err(), d.Stderr())
		}
	}
}

// AwaitFileGone waits for a path to disappear.
func (d *Daemon) AwaitFileGone(path string) {
	d.t.Helper()
	ticker := time.NewTicker(pollInterval)
	defer ticker.Stop()
	for {
		if _, err := os.Stat(path); errors.Is(err, os.ErrNotExist) {
			return
		}
		select {
		case <-ticker.C:
		case <-d.ctx.Done():
			d.t.Fatalf("waiting for %s to be removed: %v", path, d.ctx.Err())
		}
	}
}

func (d *Daemon) expectedExit() bool {
	d.mu.Lock()
	defer d.mu.Unlock()
	return d.exited && d.exitErr == nil
}

// dial builds the Connect client. The daemon serves HTTP/1.1 and h2c on one
// origin; the harness uses h2c so a server stream is not head-of-line blocked.
func (d *Daemon) dial(jsonCodec bool) {
	d.t.Helper()
	d.http = &http.Client{
		Transport: &http2.Transport{
			AllowHTTP: true,
			DialTLSContext: func(ctx context.Context, network, addr string, _ *tls.Config) (net.Conn, error) {
				var dialer net.Dialer
				return dialer.DialContext(ctx, network, addr)
			},
		},
	}
	var opts []connect.ClientOption
	if jsonCodec {
		opts = append(opts, connect.WithProtoJSON())
	}
	d.client = agentreplv1connect.NewAgentReplClient(d.http, "http://"+d.Addr, opts...)
}

// Client is the Connect client dialed to this daemon.
func (d *Daemon) Client() agentreplv1connect.AgentReplClient {
	d.t.Helper()
	if d.client == nil {
		d.t.Fatal("harness: the daemon has no client (joining mode, or an expected early exit)")
	}
	return d.client
}

// Dial builds a second, independent client connection, for the tests whose
// subject is per-connection state (a feed page walk).
func (d *Daemon) Dial() agentreplv1connect.AgentReplClient {
	d.t.Helper()
	client := &http.Client{
		Transport: &http2.Transport{
			AllowHTTP: true,
			DialTLSContext: func(ctx context.Context, network, addr string, _ *tls.Config) (net.Conn, error) {
				var dialer net.Dialer
				return dialer.DialContext(ctx, network, addr)
			},
		},
	}
	return agentreplv1connect.NewAgentReplClient(client, "http://"+d.Addr)
}

// HTTP is a plain client against the daemon's asset origin.
func (d *Daemon) HTTP() *http.Client { return &http.Client{} }

// Ctx is the context every call in the test is bounded by.
func (d *Daemon) Ctx() context.Context { return d.ctx }

// PID is the daemon process's id.
func (d *Daemon) PID() int { return d.cmd.Process.Pid }

// Stop sends SIGTERM and waits for the process to leave.
func (d *Daemon) Stop() {
	d.t.Helper()
	d.signal(syscall.SIGTERM)
	d.Wait()
}

// Kill ends the process group without warning, for crash simulation and for
// the cleanup every test gets.
func (d *Daemon) Kill() {
	if d.cmd == nil || d.cmd.Process == nil {
		return
	}
	syscall.Kill(-d.cmd.Process.Pid, syscall.SIGKILL)
	d.waitOnce.Do(d.wait)
}

func (d *Daemon) signal(sig syscall.Signal) {
	d.t.Helper()
	if err := d.cmd.Process.Signal(sig); err != nil && !errors.Is(err, os.ErrProcessDone) {
		d.t.Fatalf("harness: signal %v: %v", sig, err)
	}
}

// Wait blocks until the process has left and answers its exit status.
func (d *Daemon) Wait() int {
	d.waitOnce.Do(d.wait)
	d.mu.Lock()
	defer d.mu.Unlock()
	var exit *exec.ExitError
	if errors.As(d.exitErr, &exit) {
		return exit.ExitCode()
	}
	return 0
}

func (d *Daemon) wait() {
	err := d.cmd.Wait()
	d.mu.Lock()
	d.exited, d.exitErr = true, err
	d.mu.Unlock()
}

// Exited reports whether the process has already left.
func (d *Daemon) Exited() bool {
	if d.cmd == nil || d.cmd.Process == nil {
		return true
	}
	d.mu.Lock()
	already := d.exited
	d.mu.Unlock()
	if already {
		return true
	}
	// Signal 0 probes liveness without disturbing the process.
	return d.cmd.Process.Signal(syscall.Signal(0)) != nil
}

// AwaitExit waits for the process to leave and answers its exit status,
// failing the test if it outlives the context.
func (d *Daemon) AwaitExit() int {
	d.t.Helper()
	done := make(chan int, 1)
	go func() { done <- d.Wait() }()
	select {
	case code := <-done:
		return code
	case <-d.ctx.Done():
		d.t.Fatalf("daemon did not exit: %v\nstderr:\n%s", d.ctx.Err(), d.Stderr())
		return -1
	}
}

// Stderr is everything the daemon wrote to its terminal mirror.
func (d *Daemon) Stderr() string { return d.stderr.String() }

// Register registers a repository's worktree and answers the minted ref.
func Register(t *testing.T, d *Daemon, dir string) *workspacev1.WorkspaceRef {
	t.Helper()
	resp, err := d.Client().RegisterWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.RegisterWorkspaceRequest{Dir: dir}))
	if err != nil {
		t.Fatalf("RegisterWorkspace(%s) = error %v, want a minted id", dir, err)
	}
	ref := resp.Msg.GetSuccess().GetWorkspace()
	if ref.GetId() == "" {
		t.Fatalf("RegisterWorkspace(%s) = %v, want a success carrying a workspace id", dir, resp.Msg)
	}
	return ref
}

// SocketPath is the per-workspace shim UDS the daemon spawns against.
func (d *Daemon) SocketPath(ws *workspacev1.WorkspaceRef) string {
	return filepath.Join(d.StateDir, "sock", ws.GetId()+".sock")
}

// WriteShimProfile files a fake-shim startup profile for a workspace
// directory, to be read when the daemon spawns the shim there.
func (d *Daemon) WriteShimProfile(dir string, profile any) {
	d.t.Helper()
	writeJSON(d.t, filepath.Join(d.ProfileDir, profileFileName(dir)), profile)
}

// WriteDefaultShimProfile files the profile every unprofiled workspace uses.
func (d *Daemon) WriteDefaultShimProfile(profile any) {
	d.t.Helper()
	writeJSON(d.t, filepath.Join(d.ProfileDir, "default.json"), profile)
}

func (d *Daemon) String() string { return fmt.Sprintf("daemon(%s)", d.Addr) }

// ExpectFileUnchanged asserts a file still holds exactly `want` after the
// probe window. It is a negative assertion, so it necessarily waits out a
// bound rather than synchronizing on an event.
func (d *Daemon) ExpectFileUnchanged(path, want string, probe time.Duration) {
	d.t.Helper()
	deadline := time.Now().Add(probe)
	ticker := time.NewTicker(pollInterval)
	defer ticker.Stop()
	for time.Now().Before(deadline) {
		body, err := os.ReadFile(path)
		if err != nil {
			if want == "" {
				<-ticker.C
				continue
			}
			d.t.Fatalf("read %s: %v, want it to still hold %q", path, err, want)
		}
		if string(body) != want {
			d.t.Fatalf("%s = %q, want it unchanged at %q", path, body, want)
		}
		<-ticker.C
	}
}
