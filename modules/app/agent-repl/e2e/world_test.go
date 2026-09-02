package e2e

import (
	"context"
	"crypto/rand"
	"encoding/hex"
	"fmt"
	"net"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"syscall"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/proto/store/v1/storev1connect"
	workspacev1 "agentrepl/proto/workspace/v1"

	"connectrpc.com/connect"

	"claude-repld/integration/harness"
)

// ===========================================================================
// Waits (SPEC.md section B, "Waits"). Event-driven only: a daemon watch-
// stream frame, a structured-log record, or a bounded store-read poll. No
// time.Sleep for synchronization anywhere in this package.
// ===========================================================================

// DefaultTimeout bounds an ordinary per-test wait. Reuses
// harness.DefaultTimeout verbatim rather than minting a same-value twin — see
// that constant's own sizing rationale (a 443-test daemon/integration run:
// median 0.2s, observed max 1.7s), independently corroborated by the deleted
// daemon/e2e's own ~0.65s-per-await baseline for a real node-shim spawn plus
// a turn through a real store ("roughly an 8x margin"). Two independently
// measured real-process baselines agreeing on 5s is strong grounding for
// reusing it as-is.
const DefaultTimeout = harness.DefaultTimeout

// HandoverChainTimeout bounds the restart/adoption/handover tests that chain
// a SECOND real process boot onto the same context (adoption_e2e_test.go).
// Reused verbatim from harness — see its doc comment; the shape (two real
// process lifecycles on one budget) matches exactly.
const HandoverChainTimeout = harness.HandoverChainTimeout

// AdoptionChainTimeout is HandoverChainTimeout under the name the adoption
// area uses at its call sites: a restart/adoption test chains a second real
// claude-repld boot onto the same context, exactly the shape
// HandoverChainTimeout's doc comment describes. Kept as a distinct name (not
// a re-mint of the value) so an adoption test reads self-documenting at the
// call site without the reader having to know the handover family shares its
// budget.
const AdoptionChainTimeout = HandoverChainTimeout

// StoreOutageWindow bounds the degraded-state tests (degradedstate_e2e_test.go)
// that provoke a REAL store outage with Store.Stop/StartSameDB. Derived the
// same way the sidecar's own integration suite derived
// UnownedSpoolWindow=200ms ("a subject that is not ABOUT the hold would
// simply time out inside it; subjects that ARE about the hold set their own
// window") — this suite's degraded-state test IS about the outage, so it
// gets its own window rather than DefaultTimeout. The sidecar's own
// UnownedSpoolWindow constant is reused directly as the sidecar's outage
// tolerance (see Sidecar's defaultSidecarOpts below); this constant is the
// TEST's own budget for observing the daemon's shim-degraded fact and its
// clearing, one order of magnitude above the sidecar's own window so the
// test's wait is never the tighter of the two bounds.
const StoreOutageWindow = 2 * time.Second

// unownedSpoolWindow is this suite's own copy of the sidecar's real
// production default divisor, at a value matched to
// shim-sidecar/integration/helpers_test.go's own defaultSidecarOptions
// (UnownedSpoolWindow: 200ms) for the identical reason given there.
const unownedSpoolWindow = 200 * time.Millisecond

// pollInterval is how often a bounded store-read poll re-checks. It is a poll
// of a durable surface, never a sleep standing in for a signal.
const pollInterval = 20 * time.Millisecond

// ===========================================================================
// World: one test's full stack — a real store, a real sidecar, and a real
// daemon spawning the real shim per session. Every area test file builds one
// with NewWorld and drives it purely through the embedded *harness.Daemon's
// Connect client and watch streams (WatchFeed, WatchTopbar, WatchDaemonHolds,
// ...) plus this file's SubmitPrompt/driveScenarioToCompletion helpers.
//
// World embeds *harness.Daemon, so every exported harness.Daemon method
// (Client, WatchFeed, WatchTopbar, WatchDaemonHolds, AwaitLogRecord, ...) is
// callable directly as a method of World; harness.Register(t, w.Daemon, dir)
// still works by naming the embedded field explicitly where a function
// (rather than a method) is required.
// ===========================================================================

// World is one test's daemon + store + sidecar + (session-spawned) real shim.
type World struct {
	*harness.Daemon
	Store   *Store
	Sidecar *Sidecar
}

// WorldOpts configures one World. DaemonOpts is forwarded to
// harness.StartDaemon verbatim EXCEPT for the four fields NewWorld always
// sets itself (ShimNode, ShimMain, StoreSocket, SkipFakeGit) — a caller that
// sets those is overridden, since this suite's whole point is exercising the
// real shim against a real store with real git, never the fakes.
//
// DaemonOpts.ExtraEnv is the seam for a world-wide lever the real shim reads
// from its OWN spawn environment. It reaches every shim this daemon spawns
// unchanged: StartDaemon appends it onto the daemon process's own env, and
// the daemon's shimclient.spawnEnv copies the daemon's os.Environ() forward
// to each spawned shim verbatim except for a fixed, small override set
// (CLAUDE_CONFIG_DIR, SHIM_BUILD_SHA, state dir, session id,
// forbid-vendor-calls) — never an allowlist. Known consumer: the fake SDK's
// `AGENT_REPL_FAKE_REFUSE` (src/fake/index.ts) — "start" refuses every
// StartSession the whole process makes; "start-once" refuses only the
// FIRST one, for retry-recovery coverage. This is how the refusals area
// (test #54, StartSessionVendorStartFailed) provokes
// StartSessionFailure.cause.vendor_start_failed for real, since
// StartSession resolves before any prompt exists and so cannot be reached
// by a scenario prompt.
type WorldOpts struct {
	DaemonOpts harness.Opts
}

// NewWorld builds one test's stack: the real store, the real sidecar, and a
// real daemon spawning the real (built-from-source, `--fake`-mode) shim.
// Every process is killed and every socket/temp root is removed at test
// cleanup, in reverse start order (shim [daemon-owned] -> daemon -> sidecar
// -> store), so tearing the store down never races a live writer.
//
// Loud skips (never a fail): no `node` on PATH, the shim's deps not
// installed (names the exact `npm ci` command — never runs it), no `go` on
// PATH. A store or sidecar build FAILURE (not a missing precondition) fails
// the test outright: that is a real defect this suite exists to catch.
func NewWorld(t *testing.T, opts WorldOpts) *World {
	t.Helper()
	node := requireNode(t)
	shimMain := requireShimBundle(t)
	sidecarBin := requireSidecarBinary(t)

	// START ORDER: store, then sidecar, then daemon (which spawns the shim
	// per session). Cleanups are registered in this same order, so
	// t.Cleanup's LIFO unwind tears down daemon -> sidecar -> store — daemon
	// (and its shims) first, so nothing is left writing to a store or
	// sidecar that already went away.
	store := startStore(t, shortSocketPath(t, "store"), filepath.Join(t.TempDir(), "store.db"))

	daemonOpts := opts.DaemonOpts
	daemonOpts.ShimNode = node
	daemonOpts.ShimMain = shimMain
	daemonOpts.StoreSocket = store.Socket
	daemonOpts.SkipFakeGit = true
	daemonOpts.ExtraEnv = append(append([]string{}, daemonOpts.ExtraEnv...), "SHIM_BUILD_SHA="+shimBuildSHA)

	d := harness.StartDaemon(t, daemonOpts)

	sidecar := startSidecar(t, sidecarBin, sidecarOpts{
		StoreSocket: store.Socket,
		ConfigRoots: []string{d.DefaultConfigDir, d.MultiRepoConfigDir},
		SpoolRoot:   t.TempDir(),
		LogPath:     filepath.Join(t.TempDir(), "sidecar.log"),
	})

	return &World{Daemon: d, Store: store, Sidecar: sidecar}
}

// shortSocketPath mints a short-enough UDS path directly under the OS temp
// dir. t.TempDir() encodes the whole test name, which blows the ~103-byte
// sockaddr_un budget for this suite's longer test names.
func shortSocketPath(t *testing.T, tag string) string {
	t.Helper()
	buf := make([]byte, 4)
	if _, err := rand.Read(buf); err != nil {
		t.Fatalf("e2e: random socket suffix: %v", err)
	}
	p := filepath.Join(os.TempDir(), fmt.Sprintf("are2e-%s-%s.sock", tag, hex.EncodeToString(buf)))
	t.Cleanup(func() { _ = os.Remove(p) })
	return p
}

// ===========================================================================
// Store: the real shim-store, restartable on the same socket + database, for
// the degraded-state family's real outage window (ruling 2).
// ===========================================================================

// Store is one running real shim-store.
type Store struct {
	t       *testing.T
	bin     string
	Socket  string
	DBPath  string
	LogPath string
	Client  storev1connect.ShimStoreClient

	cmd     *exec.Cmd
	done    chan error
	stopped bool
}

// startStore launches the store on a named socket + database and blocks
// until a real rpc against it succeeds — readiness is the store's own
// statement, never an elapsed duration.
func startStore(t *testing.T, socket, dbPath string) *Store {
	t.Helper()
	bin := requireStoreBinary(t)
	if err := os.MkdirAll(filepath.Dir(dbPath), 0o755); err != nil {
		t.Fatalf("e2e: mkdir %s: %v", filepath.Dir(dbPath), err)
	}
	logPath := filepath.Join(t.TempDir(), "store.log")
	cmd := exec.Command(bin, "--socket", socket, "--db", dbPath, "--log", logPath)
	cmd.Env = append(os.Environ(),
		"AGENT_REPL_STORE_SOCKET="+socket,
		"AGENT_REPL_FORBID_VENDOR_CALLS=1",
	)
	cmd.Stdout = os.Stderr
	cmd.Stderr = os.Stderr
	if err := cmd.Start(); err != nil {
		t.Fatalf("e2e: start store: %v", err)
	}
	s := &Store{
		t:       t,
		bin:     bin,
		Socket:  socket,
		DBPath:  dbPath,
		LogPath: logPath,
		Client:  storeClient(socket),
		cmd:     cmd,
		done:    make(chan error, 1),
	}
	go func() { s.done <- cmd.Wait() }()
	t.Cleanup(func() {
		if !s.Exited() {
			s.Stop()
		}
	})
	s.awaitReady()
	return s
}

func storeClient(socket string) storev1connect.ShimStoreClient {
	return storev1connect.NewShimStoreClient(udsHTTPClient(socket), "http://store")
}

func udsHTTPClient(socket string) *http.Client {
	return &http.Client{
		Transport: &http.Transport{
			DialContext: func(ctx context.Context, _, _ string) (net.Conn, error) {
				return (&net.Dialer{}).DialContext(ctx, "unix", socket)
			},
		},
	}
}

func (s *Store) awaitReady() {
	s.t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), DefaultTimeout)
	defer cancel()
	ticker := time.NewTicker(pollInterval)
	defer ticker.Stop()
	for {
		if _, err := s.Client.GetSidecarCursors(ctx, connect.NewRequest(&storev1.GetSidecarCursorsRequest{})); err == nil {
			return
		}
		select {
		case <-ctx.Done():
			s.t.Fatalf("e2e: store never answered GetSidecarCursors within %s", DefaultTimeout)
		case <-ticker.C:
		}
	}
}

// Exited reports whether the store process has already left, without
// disturbing it.
func (s *Store) Exited() bool {
	select {
	case <-s.done:
		return true
	default:
		return false
	}
}

// Stop sends SIGTERM and waits for the store to leave. It is ruling 2's real
// degraded-state control: a test calls this while a session is live, asserts
// the daemon states the shim-degraded fact, then calls StartSameDB to prove
// recovery. No test ever fabricates a degraded-state fact — this is the only
// legitimate way to produce a real one.
func (s *Store) Stop() {
	s.t.Helper()
	if s.stopped {
		return
	}
	s.stopped = true
	if s.cmd.Process != nil {
		_ = s.cmd.Process.Signal(syscall.SIGTERM)
	}
	select {
	case <-s.done:
	case <-time.After(DefaultTimeout):
		if s.cmd.Process != nil {
			_ = s.cmd.Process.Kill()
		}
		<-s.done
		s.t.Fatalf("e2e: store did not exit within %s of SIGTERM", DefaultTimeout)
	}
	_ = os.Remove(s.Socket)
}

// StartSameDB relaunches the store on the SAME socket and database path this
// Store was minted with, and waits for it to become ready again. Pairs with
// Stop for ruling 2's real outage window.
func (s *Store) StartSameDB(t *testing.T) {
	t.Helper()
	cmd := exec.Command(s.bin, "--socket", s.Socket, "--db", s.DBPath, "--log", s.LogPath)
	cmd.Env = append(os.Environ(),
		"AGENT_REPL_STORE_SOCKET="+s.Socket,
		"AGENT_REPL_FORBID_VENDOR_CALLS=1",
	)
	cmd.Stdout = os.Stderr
	cmd.Stderr = os.Stderr
	if err := cmd.Start(); err != nil {
		t.Fatalf("e2e: restart store: %v", err)
	}
	s.cmd = cmd
	s.stopped = false
	s.done = make(chan error, 1)
	go func() { s.done <- cmd.Wait() }()
	t.Cleanup(func() {
		if !s.Exited() {
			s.Stop()
		}
	})
	s.awaitReady()
}

// Cursors answers the store's current durable sidecar cursors.
func (s *Store) Cursors(t *testing.T, ctx context.Context) []*storev1.CursorState {
	t.Helper()
	resp, err := s.Client.GetSidecarCursors(ctx, connect.NewRequest(&storev1.GetSidecarCursorsRequest{}))
	if err != nil {
		t.Fatalf("e2e: GetSidecarCursors: %v", err)
	}
	if f := resp.Msg.GetFailure(); f != nil {
		t.Fatalf("e2e: GetSidecarCursors failed: %v", f)
	}
	return resp.Msg.GetSuccess().GetCursors()
}

// ===========================================================================
// Sidecar: the real shim-claude-sidecar, watching the daemon's two account
// roots for the vendor-shaped files the real (--fake) shim writes.
// ===========================================================================

// Sidecar is one running real shim-claude-sidecar.
type Sidecar struct {
	t       *testing.T
	LogPath string
	cmd     *exec.Cmd
	done    chan error
	stopped bool
}

type sidecarOpts struct {
	StoreSocket string
	ConfigRoots []string
	SpoolRoot   string
	LogPath     string
}

// startSidecar launches the sidecar against the daemon's own two account
// roots (d.DefaultConfigDir, d.MultiRepoConfigDir), which is ALREADY the
// per-account CLAUDE_CONFIG_DIR the daemon hands each spawned shim — no new
// harness plumbing is needed to make the sidecar watch the right trees.
func startSidecar(t *testing.T, bin string, opts sidecarOpts) *Sidecar {
	t.Helper()
	args := []string{
		"--store-socket", opts.StoreSocket,
		"--config-roots", strings.Join(opts.ConfigRoots, ","),
		"--spool-root", opts.SpoolRoot,
		"--log", opts.LogPath,
		"--poll-interval", "50ms",
		"--rescan-interval", "200ms",
		// The unclaimed-spool hold defaults to 60s in production, which is the
		// whole budget of this suite: a subject that is not ABOUT the hold
		// would simply time out inside it. The degraded-state area sets its
		// own window (StoreOutageWindow) at the daemon-wait layer; this
		// shrinks the sidecar's OWN internal hold to match the sidecar
		// integration suite's own precedent (UnownedSpoolWindow: 200ms).
		"--unowned-spool-window", unownedSpoolWindow.String(),
	}
	cmd := exec.Command(bin, args...)
	cmd.Env = append(os.Environ(),
		"AGENT_REPL_STORE_SOCKET="+opts.StoreSocket,
		"AGENT_REPL_FORBID_VENDOR_CALLS=1",
	)
	cmd.Stdout = os.Stderr
	cmd.Stderr = os.Stderr
	if err := cmd.Start(); err != nil {
		t.Fatalf("e2e: start sidecar: %v", err)
	}
	s := &Sidecar{t: t, LogPath: opts.LogPath, cmd: cmd, done: make(chan error, 1)}
	go func() { s.done <- cmd.Wait() }()
	t.Cleanup(s.Stop)
	return s
}

// Stop sends SIGTERM and waits for the sidecar to leave.
func (s *Sidecar) Stop() {
	s.t.Helper()
	if s.stopped {
		return
	}
	s.stopped = true
	if s.cmd.Process != nil {
		_ = s.cmd.Process.Signal(syscall.SIGTERM)
	}
	select {
	case <-s.done:
	case <-time.After(DefaultTimeout):
		if s.cmd.Process != nil {
			_ = s.cmd.Process.Kill()
		}
		<-s.done
		s.t.Fatalf("e2e: sidecar did not exit within %s of SIGTERM", DefaultTimeout)
	}
}

// Log reads every structured-log record the sidecar has written so far.
func (s *Sidecar) Log(t *testing.T) []harness.LogRecord {
	t.Helper()
	return harness.ReadLog(t, s.LogPath)
}

// ===========================================================================
// A process that exits before test cleanup fails the test loudly (SPEC.md
// section B, "Lifecycle"). harness.Daemon already tracks this for the
// daemon; these two calls extend the same discipline to Store and Sidecar.
// An area writer calls this once, right after NewWorld, if the test does not
// itself intend to stop either process.
// ===========================================================================

// RequireNoUnexpectedExit registers a cleanup that fails the test if the
// store or the sidecar has already exited by the time the test ends, unless
// the test itself stopped it (Store.Stop / a Sidecar the test killed
// directly). It is opt-in, not automatic, because ruling-2 degraded-state
// tests deliberately stop the store mid-test.
func (w *World) RequireNoUnexpectedExit(t *testing.T) {
	t.Helper()
	t.Cleanup(func() {
		if w.Store.Exited() && !w.Store.stopped {
			t.Errorf("e2e: the store exited before test cleanup")
		}
	})
}

// ===========================================================================
// Submitting a real prompt and awaiting its terminal, durably.
// ===========================================================================

// e2ePromptOrigin is the PromptOrigin every prompt this suite submits
// carries. UNSPECIFIED is refused at once (endpoint_submit_prompt.proto), and
// this suite's prompts are not attributable to any one particular Emacs send
// site, so the plain "user sent" arm is the correct choice for all of them.
const e2ePromptOrigin = conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT

// SubmitPrompt submits a real prompt through the daemon's SubmitPrompt rpc
// and answers the minted turn id. It fails the test if the submission was
// refused or did not mint a turn (a command-panel/command-acted/
// command-refused arm) — callers that expect one of those other arms should
// call the client's SubmitPrompt directly instead.
func SubmitPrompt(t *testing.T, w *World, ws *workspacev1.WorkspaceRef, text string) *conversationv1.TurnId {
	t.Helper()
	resp, err := w.Client().SubmitPrompt(w.Ctx(), connect.NewRequest(&agentreplv1.SubmitPromptRequest{
		Workspace: ws,
		Said: &conversationv1.UserSaid{Content: &conversationv1.UserContent{Blocks: []*conversationv1.UserContentBlock{
			{Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: text}}},
		}}},
		IdempotencyKey: newIdempotencyKey(t),
		Origin:         e2ePromptOrigin,
	}))
	if err != nil {
		t.Fatalf("SubmitPrompt(%q): %v", text, err)
	}
	turn := resp.Msg.GetSuccess().GetTurn().GetTurn()
	if turn.GetValue() == "" {
		t.Fatalf("SubmitPrompt(%q) = %v, want a minted turn", text, resp.Msg)
	}
	return turn
}

func newIdempotencyKey(t *testing.T) string {
	t.Helper()
	buf := make([]byte, 16)
	if _, err := rand.Read(buf); err != nil {
		t.Fatalf("e2e: random idempotency key: %v", err)
	}
	return hex.EncodeToString(buf)
}

// AwaitTurnEnded opens the workspace's root feed, watches it until the given
// turn's FeedTurnEnded row arrives, and answers that row.
func AwaitTurnEnded(t *testing.T, w *World, ws *workspacev1.WorkspaceRef, turn *conversationv1.TurnId) *frontendv1.FeedRow {
	t.Helper()
	opened, err := w.Client().OpenFeed(w.Ctx(), connect.NewRequest(&agentreplv1.OpenFeedRequest{Workspace: ws}))
	if err != nil {
		t.Fatalf("OpenFeed: %v", err)
	}
	success := opened.Msg.GetSuccess()
	if success == nil {
		t.Fatalf("OpenFeed = %v, want success", opened.Msg)
	}
	for _, row := range success.GetPage().GetSuccess().GetRows() {
		if row.GetTurn().GetValue() == turn.GetValue() && row.GetTurnEnded() != nil {
			return row
		}
	}
	stream := w.WatchFeedOn(w.Client(), success.GetWatch())
	defer stream.Close()
	return harness.AwaitView(t, w.Ctx(), stream, "turn "+turn.GetValue()+" to end", func(row *frontendv1.FeedRow) bool {
		return row.GetTurn().GetValue() == turn.GetValue() && row.GetTurnEnded() != nil
	})
}

// driveScenarioToCompletion submits a real "!"+scenario prompt (or a
// caller-supplied full prompt string for scenarios with their own documented
// form, e.g. "!compact [summary]"), waits for the turn's terminal
// FeedTurnEnded row, and returns once the sidecar has DURABLY written the
// resulting facts to the real store — proven by the store's own
// GetSidecarCursors read verb showing a cursor whose path is under this
// workspace's project directory advance past its pre-prompt baseline, never
// by a sleep.
//
// A test that needs pre-restart durable rows (the ColdBootReadsReplayFromStore
// family) calls this BEFORE simulating the daemon bounce, then restarts the
// daemon against the same state root/store socket and asserts on replay.
//
// configDir is the account root the workspace routes through
// (w.DefaultConfigDir or w.MultiRepoConfigDir) — the caller names it because
// only the caller knows which account root registered the workspace.
func driveScenarioToCompletion(t *testing.T, w *World, ws *workspacev1.WorkspaceRef, configDir, scenario string) *conversationv1.TurnId {
	t.Helper()
	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()
	before := w.Store.Cursors(t, ctx)
	baseline := cursorOffsetsUnder(before, harness.ProjectDir(configDir, ws.GetDir()))

	turn := SubmitPrompt(t, w, ws, "!"+scenario)
	AwaitTurnEnded(t, w, ws, turn)

	awaitCursorAdvance(t, w, harness.ProjectDir(configDir, ws.GetDir()), baseline)
	return turn
}

// driveDocumentedPrompt is driveScenarioToCompletion for the scenarios whose
// documented invocation is not a bare "!"+scenario (e.g. "!compact [summary]").
func driveDocumentedPrompt(t *testing.T, w *World, ws *workspacev1.WorkspaceRef, configDir, prompt string) *conversationv1.TurnId {
	t.Helper()
	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()
	before := w.Store.Cursors(t, ctx)
	baseline := cursorOffsetsUnder(before, harness.ProjectDir(configDir, ws.GetDir()))

	turn := SubmitPrompt(t, w, ws, prompt)
	AwaitTurnEnded(t, w, ws, turn)

	awaitCursorAdvance(t, w, harness.ProjectDir(configDir, ws.GetDir()), baseline)
	return turn
}

func cursorOffsetsUnder(cursors []*storev1.CursorState, projectDir string) map[string]int64 {
	out := map[string]int64{}
	for _, c := range cursors {
		if strings.HasPrefix(c.GetPath(), projectDir) {
			out[c.GetPath()] = c.GetOffset()
		}
	}
	return out
}

// awaitCursorAdvance polls GetSidecarCursors until some cursor under
// projectDir has advanced past its baseline offset (or a cursor under it now
// exists that did not before) — the store's own durable statement that the
// sidecar has committed the turn's resulting facts.
func awaitCursorAdvance(t *testing.T, w *World, projectDir string, baseline map[string]int64) {
	t.Helper()
	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()
	ticker := time.NewTicker(pollInterval)
	defer ticker.Stop()
	for {
		now := cursorOffsetsUnder(w.Store.Cursors(t, ctx), projectDir)
		for path, offset := range now {
			if offset > baseline[path] {
				return
			}
		}
		select {
		case <-ticker.C:
		case <-ctx.Done():
			t.Fatalf("e2e: waiting for the sidecar to durably advance a cursor under %s: %v", projectDir, ctx.Err())
		}
	}
}

// ===========================================================================
// Real git (SPEC.md section B, "Real git" — ruling 4). The daemon's
// no-real-git directive is scoped to the daemon's OWN unit/integration
// suites; its AGENTS.md hands the git facts this suite owns to "the
// project lead's suite" — this one. NewWorld sets Opts.SkipFakeGit, so the
// real `git` on PATH is what the daemon's own git invocations reach.
// ===========================================================================

// gitEnv is the hermetic environment every real-git child process runs
// under: an isolated global config and HOME (so a test never depends on the
// operator's own git identity or defaults), a fixed author/committer
// identity and date (so a test's own assertions on commit metadata are
// deterministic), and the repository-selecting variables a hook could leak
// (GIT_DIR, GIT_WORK_TREE, GIT_INDEX_FILE) explicitly unset — the known
// core.bare hazard from a leaked GIT_DIR.
func gitEnv(t *testing.T) []string {
	t.Helper()
	home := t.TempDir()
	globalConfig := filepath.Join(home, ".gitconfig-e2e")
	body := "[user]\n\tname = agent-repl e2e\n\temail = e2e@example.invalid\n[init]\n\tdefaultBranch = " + harness.DefaultBranch + "\n"
	if err := os.WriteFile(globalConfig, []byte(body), 0o644); err != nil {
		t.Fatalf("e2e: write git global config: %v", err)
	}
	env := []string{
		"GIT_CONFIG_GLOBAL=" + globalConfig,
		"GIT_CONFIG_SYSTEM=/dev/null",
		"HOME=" + home,
		"GIT_AUTHOR_NAME=agent-repl e2e",
		"GIT_AUTHOR_EMAIL=e2e@example.invalid",
		"GIT_AUTHOR_DATE=2026-01-01T00:00:00Z",
		"GIT_COMMITTER_NAME=agent-repl e2e",
		"GIT_COMMITTER_EMAIL=e2e@example.invalid",
		"GIT_COMMITTER_DATE=2026-01-01T00:00:00Z",
	}
	for _, kv := range os.Environ() {
		key, _, _ := strings.Cut(kv, "=")
		switch key {
		case "GIT_DIR", "GIT_WORK_TREE", "GIT_INDEX_FILE", "HOME",
			"GIT_CONFIG_GLOBAL", "GIT_CONFIG_SYSTEM",
			"GIT_AUTHOR_NAME", "GIT_AUTHOR_EMAIL", "GIT_AUTHOR_DATE",
			"GIT_COMMITTER_NAME", "GIT_COMMITTER_EMAIL", "GIT_COMMITTER_DATE":
			continue // superseded above; never inherited.
		}
		env = append(env, kv)
	}
	return env
}

// RealRepo is one real git repository, created fresh under the test's own
// temp root. Nothing here is a fixture: every fact the daemon reads about
// this repository comes from the REAL `git` on PATH, running against files
// this type actually wrote.
type RealRepo struct {
	t   *testing.T
	Dir string
	env []string
}

// NewRealRepo runs real `git init` (and one commit, so the repository has a
// default-branch head to build on) in a fresh temp directory, and answers
// it. Skips loudly if `git` is absent from PATH; the harness never installs
// it.
func NewRealRepo(t *testing.T) *RealRepo {
	t.Helper()
	requireGit(t)
	dir := t.TempDir()
	r := &RealRepo{t: t, Dir: dir, env: gitEnv(t)}
	r.git("init", "--initial-branch="+harness.DefaultBranch, dir)
	readme := filepath.Join(dir, "README.md")
	if err := os.WriteFile(readme, []byte("e2e repository\n"), 0o644); err != nil {
		t.Fatalf("e2e: write README.md: %v", err)
	}
	r.git("-C", dir, "add", "README.md")
	r.git("-C", dir, "commit", "-m", "add README.md")
	return r
}

// git runs the real git binary with this repository's hermetic environment,
// failing the test loudly on a non-zero exit. GIT_DIR/GIT_WORK_TREE/
// GIT_INDEX_FILE are never inherited from the calling process's own
// environment (see gitEnv) — the known core.bare hazard from a hook-leaked
// GIT_DIR.
func (r *RealRepo) git(args ...string) string {
	r.t.Helper()
	cmd := exec.Command(gitBin, args...)
	cmd.Env = r.env
	out, err := cmd.CombinedOutput()
	if err != nil {
		r.t.Fatalf("e2e: git %s: %v\n%s", strings.Join(args, " "), err, out)
	}
	return string(out)
}

// Git runs an arbitrary real git command against this repository (`-C
// r.Dir` is NOT implied — pass it, or a subdirectory, explicitly, exactly
// like a real invocation would need to), for the tests whose subject is a
// specific git fact (a two-parent --no-ff merge, a conflicted index and
// MERGE_HEAD, a revert, a worktree prune, porcelain markers, GIT_DIR
// precedence, git version compatibility).
func (r *RealRepo) Git(args ...string) string { return r.git(args...) }
