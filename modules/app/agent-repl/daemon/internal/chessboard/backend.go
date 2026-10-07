package chessboard

import (
	"bytes"
	"context"
	"crypto/sha256"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
	"sync"

	"claude-repld/internal/dlog"
)

// The operations this package's records carry.
const (
	opBuild   = "daemon.chessboard.build"
	opServe   = "daemon.chessboard.serve"
	opBoard   = "daemon.chessboard.board"
	opSquare  = "daemon.chessboard.square"
	opBundle  = "daemon.chessboard.bundle"
	opBackend = "daemon.chessboard.backend"
)

// Runner runs one command in a directory and answers its combined output and
// exit code; an error is a run that could not be classified at all
// (internal/scriptrunner's contract, which production binds).
type Runner interface {
	Run(ctx context.Context, dir string, argv []string) (string, int, error)
}

// The steps a board shows while its backend is being readied, as the reader
// reads them.
const (
	stepGettingReady    = "Getting the chess widget ready…"
	stepBuildingWidget  = "Building the chess widget…"
	stepBuildingBackend = "Building the chess widget's backend…"
	stepStartingBackend = "Starting the chess widget's backend…"
	stepLoadingGame     = "Loading the game…"
)

// failure is a backend step that failed: reason is the one line the board
// shows; the full account is already in the log.
type failure struct {
	reason string
}

// serving is a backend ready to answer: cee-webapp's base URL and the widget
// build the board's bundle URLs name.
type serving struct {
	baseURL     string
	widgetStamp string
	widgetDist  string
}

// backend owns the widget's backend: one readying run at a time, shared by
// every board waiting on it.
type backend struct {
	log    dlog.Logger
	run    Runner
	getenv func(string) string
	// pluginDir is the gns cee plugin's install directory, where cee-webapp
	// is installed beside the plugin that spawns it.
	pluginDir string
	// onStep is told each step a readying run enters.
	onStep func(string)
	// life bounds every readying run: a run outlives the board that started
	// it, because every waiting board shares it.
	life context.Context

	mu sync.Mutex
	// inflight is the readying run under way, nil when none is.
	inflight *readying
	// built is the widget build the last readying run served, which the
	// bundle route serves while a later run rebuilds.
	built serving
}

// readying is one readying run, which every concurrent caller waits on.
type readying struct {
	done    chan struct{}
	serving serving
	failed  *failure
}

// errBuildFailed marks a build or start command that ran and failed.
var errBuildFailed = errors.New("chessboard: a backend command failed")

// fail logs a failed step at ERROR with its cause and answers the failure the
// board shows.
func (b *backend) fail(stage, reason string, cause error, ctx dlog.Context) *failure {
	fields := dlog.Context{"stage": stage, "cause": cause.Error(), "reason": reason}
	for k, v := range ctx {
		fields[k] = v
	}
	b.log.Error(opBackend, "a step readying the chess widget's backend failed", fields)
	return &failure{reason: reason}
}

// step tells waiting boards the step a readying run entered.
func (b *backend) step(text string) {
	b.onStep(text)
}

// runStep runs one command of a step; a command that fails answers a failure
// whose reason is prefix and the command's last output line.
func (b *backend) runStep(ctx context.Context, stage, prefix, dir string, argv []string) *failure {
	output, code, err := b.run.Run(ctx, dir, argv)
	if err != nil {
		return b.fail(stage, prefix+": "+err.Error()+".", err, dlog.Context{"dir": dir, "argv": strings.Join(argv, " ")})
	}
	if code != 0 {
		last := lastLine(output)
		return b.fail(stage, prefix+": "+last, fmt.Errorf("%w: %s exited %d", errBuildFailed, strings.Join(argv, " "), code),
			dlog.Context{"dir": dir, "argv": strings.Join(argv, " "), "exit_code": code, "output_tail": tail(output, 2000)})
	}
	return nil
}

// ensure readies the backend: the checkout, the widget, cee-webapp's binary
// and the cee-webapp singleton, in that order. Concurrent callers share one
// run. Every call starts a fresh run once the previous one is done, so a
// source change is built and a stopped cee-webapp is restarted by the next
// board; each step is a no-op when its work is already done.
func (b *backend) ensure(ctx context.Context) (serving, *failure) {
	run := b.join()
	select {
	case <-run.done:
		return run.serving, run.failed
	case <-ctx.Done():
		return serving{}, &failure{reason: "The daemon stopped before the chess widget's backend was ready."}
	}
}

// join answers the readying run under way, starting one when none is.
func (b *backend) join() *readying {
	b.mu.Lock()
	defer b.mu.Unlock()
	if b.inflight == nil {
		b.inflight = &readying{done: make(chan struct{})}
		go b.ready(b.life, b.inflight)
	}
	return b.inflight
}

// ready performs one readying run and releases its waiters.
func (b *backend) ready(ctx context.Context, run *readying) {
	run.serving, run.failed = b.readyOnce(ctx)
	b.mu.Lock()
	b.inflight = nil
	if run.failed == nil {
		b.built = run.serving
	}
	b.mu.Unlock()
	close(run.done)
}

// readyOnce performs the steps of one readying run.
func (b *backend) readyOnce(ctx context.Context) (serving, *failure) {
	b.step(stepGettingReady)
	checkout, err := resolveCheckout(b.getenv)
	if err != nil {
		return serving{}, b.fail("checkout", fmt.Sprintf("The explanation-engine checkout was not found: set %s or %s.", EngineDirEnv, MultiRepoRootEnv), err, nil)
	}
	cli := filepath.Join(checkout, cliDir)
	stamp, f := b.ensureWidget(ctx, cli)
	if f != nil {
		return serving{}, f
	}
	if f := b.ensureWebappBinary(ctx, cli); f != nil {
		return serving{}, f
	}
	b.step(stepStartingBackend)
	url, f := b.startWebapp(ctx, checkout)
	if f != nil {
		return serving{}, f
	}
	return serving{baseURL: url, widgetStamp: stamp, widgetDist: widgetDist(cli)}, nil
}

// webappBinary is cee-webapp's installed name in the plugin directory, where
// `gns cee debug webapp` spawns it from.
const webappBinary = "cee-webapp"

// webappBuildName is the binary a build writes before it is installed.
const webappBuildName = ".cee-webapp.agent-repl-build"

// ensureWebappBinary builds cee-webapp from the checkout and installs it when
// it differs from the installed one, stopping the running singleton so the
// next start serves the new build. Go's build cache makes an unchanged build a
// near no-op, and the build is byte-reproducible, so "differs" is exact.
func (b *backend) ensureWebappBinary(ctx context.Context, cli string) *failure {
	installed := filepath.Join(b.pluginDir, webappBinary)
	if _, err := os.Stat(b.pluginDir); err != nil {
		return b.fail("webapp_install", "The gns cee plugin is not installed, so the chess widget's backend cannot run.", err, dlog.Context{"plugin_dir": b.pluginDir})
	}
	built := filepath.Join(b.pluginDir, webappBuildName)
	b.step(stepBuildingBackend)
	if f := b.runStep(ctx, "webapp_build", "Building the chess widget's backend failed", cli,
		[]string{"go", "build", "-o", built, "./cmd/cee-webapp"}); f != nil {
		return f
	}
	same, err := sameFile(built, installed)
	if err != nil {
		return b.fail("webapp_install", "The chess widget's backend was built, but could not be compared with the installed one.", err, dlog.Context{"built": built, "installed": installed})
	}
	if same {
		if err := os.Remove(built); err != nil {
			return b.fail("webapp_install", "The chess widget's backend build could not be cleaned up.", err, dlog.Context{"built": built})
		}
		b.log.Debug(opBuild, "the installed cee-webapp is the checkout's build", dlog.Context{"installed": installed})
		return nil
	}
	// THE RUNNING SINGLETON SERVES THE OLD BINARY until it exits, and `gns cee
	// debug webapp` reuses a live one, so it is stopped before the new binary
	// is installed. pkill answers 1 when nothing matched: no singleton ran.
	output, code, err := b.run.Run(ctx, cli, []string{"pkill", "-f", installed})
	if err != nil || (code != 0 && code != 1) {
		cause := err
		if cause == nil {
			cause = fmt.Errorf("%w: pkill exited %d: %s", errBuildFailed, code, lastLine(output))
		}
		return b.fail("webapp_install", "The running chess widget backend could not be stopped for its rebuild.", cause, dlog.Context{"installed": installed})
	}
	if err := os.Rename(built, installed); err != nil {
		return b.fail("webapp_install", "The chess widget's backend was built, but could not be installed.", err, dlog.Context{"built": built, "installed": installed})
	}
	b.log.Info(opBuild, "installed the checkout's cee-webapp build, stopping the singleton that ran the previous one", dlog.Context{
		"installed": installed, "singleton_stopped": code == 0,
	})
	return nil
}

// sameFile reports whether two files hold the same bytes; a missing second
// file is simply different.
func sameFile(a, b string) (bool, error) {
	ha, err := fileHash(a)
	if err != nil {
		return false, err
	}
	hb, err := fileHash(b)
	if errors.Is(err, os.ErrNotExist) {
		return false, nil
	}
	if err != nil {
		return false, err
	}
	return bytes.Equal(ha, hb), nil
}

// fileHash answers a file's sha256.
func fileHash(path string) ([]byte, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	h := sha256.New()
	if _, err := io.Copy(h, f); err != nil {
		return nil, err
	}
	return h.Sum(nil), nil
}

// startWebapp ensures the cee-webapp singleton through the CEE CLI's own
// lifecycle (`gns cee debug webapp`: it reuses a live singleton and spawns one
// otherwise) and answers its base URL. The checkout is stated to it
// explicitly, so cee-webapp serves the tree the daemon built.
func (b *backend) startWebapp(ctx context.Context, checkout string) (string, *failure) {
	argv := []string{"env", EngineDirEnv + "=" + checkout, "gns", "cee", "debug", "webapp"}
	output, code, err := b.run.Run(ctx, checkout, argv)
	if err != nil || code != 0 {
		cause := err
		if cause == nil {
			cause = fmt.Errorf("%w: gns cee debug webapp exited %d", errBuildFailed, code)
		}
		return "", b.fail("webapp_start", "The chess widget's backend could not be started: "+lastLine(output), cause,
			dlog.Context{"checkout": checkout, "output_tail": tail(output, 2000)})
	}
	var answer struct {
		URL string `json:"url"`
	}
	if err := json.Unmarshal([]byte(strings.TrimSpace(output)), &answer); err != nil || answer.URL == "" {
		cause := err
		if cause == nil {
			cause = errors.New("the answer names no url")
		}
		return "", b.fail("webapp_start", "The chess widget's backend started, but did not say where it listens.", cause,
			dlog.Context{"checkout": checkout, "output_tail": tail(output, 2000)})
	}
	b.log.Debug(opServe, "the cee-webapp singleton serves", dlog.Context{"url": answer.URL})
	return strings.TrimRight(answer.URL, "/"), nil
}

// webappURL answers the live singleton's URL for a square click, ensuring it
// the same way a board does but without the builds: a click reaches a board
// that was ready, so its builds are already done.
func (b *backend) webappURL(ctx context.Context) (string, *failure) {
	checkout, err := resolveCheckout(b.getenv)
	if err != nil {
		return "", b.fail("checkout", fmt.Sprintf("The explanation-engine checkout was not found: set %s or %s.", EngineDirEnv, MultiRepoRootEnv), err, nil)
	}
	return b.startWebapp(ctx, checkout)
}

// builtWidget answers the widget build the last successful readying run
// served, and whether one has.
func (b *backend) builtWidget() (serving, bool) {
	b.mu.Lock()
	defer b.mu.Unlock()
	return b.built, b.built.widgetStamp != ""
}

// lastLine answers output's last non-blank line, the one a failed command
// most often explains itself in.
func lastLine(output string) string {
	lines := strings.Split(strings.TrimSpace(output), "\n")
	for i := len(lines) - 1; i >= 0; i-- {
		if line := strings.TrimSpace(lines[i]); line != "" {
			return line
		}
	}
	return "it printed nothing."
}

// tail answers output's last n bytes, for a log record.
func tail(output string, n int) string {
	if len(output) <= n {
		return output
	}
	return output[len(output)-n:]
}
