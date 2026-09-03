// webapplayer_e2e_test.go — WEBAPP-LAYER-SPEC.md.
//
// THE WEBAPP IN THE LOOP. Every other file in this package dials the daemon's
// Connect API directly, one layer below the webapp (SPEC.md section B,
// "Frontends: none"). This file closes that gap without duplicating a byte of
// bring-up: it builds an ordinary World — real store, real sidecar, real
// claude-repld, real shim over the fake SDK, scripted fake git — and then
// hands that daemon's own loopback address to a vitest child process which
// mounts the REAL webapp in jsdom against it.
//
// The chain under test is therefore:
//
//	fake SDK -> real shim -> real store + real sidecar -> real claude-repld
//	         -> real webapp
//
// WHY THE LIFECYCLE STAYS HERE: bring-up in this suite is not "start four
// processes", it is NewWorld — the store's short socket and log, the one spool
// root the fake SDK writes and the sidecar globs, the forced
// ShimNode/ShimMain/StoreSocket, buildIdentityEnv's one sha in both roles,
// resolveConfigRoots' symlink resolution, assertOneSpoolRoot,
// preserveLogsOnFailure, and a LIFO teardown whose order is load-bearing. Two
// of those invariants fail SILENTLY when broken (SPEC.md section B). A
// TypeScript bring-up would have to re-derive all of it; this file adds zero
// process management on the TypeScript side. The rejected alternatives are
// recorded in WEBAPP-LAYER-SPEC.md section A.
package e2e

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"

	"claude-repld/integration/harness"
)

// WebappLayerTimeout bounds the vitest child process end to end.
//
// MEASURED: the first green run's child reported 1.41s wall
// (transform 406ms, collect 584ms, environment 435ms, tests 233ms) for one
// file and one scenario, and the whole Go test took 2.73s including a full
// world bring-up.
//
// SET DELIBERATELY ABOVE ~3x THAT, and the reason is stated rather than
// smuggled: the dominant term is a COLD vitest+jsdom start — a fresh Vite
// transform of the whole webapp `src/` tree plus jsdom construction — and the
// 406ms measured above is a WARM transform cache. Nothing else in this repo
// pays that cost inside a test (the webapp's own projects pay it once,
// OUTSIDE their 300ms / 900ms per-test bounds), so there is no measurement of
// the cold case to derive a 3x from, and a bound sized off the warm one would
// be a race on a cold checkout.
//
// It bounds a HANG, not a synchronization wait: nothing here sleeps, the
// child's exit is awaited on its own channel, and the child's own per-site
// budgets (BOOT_BUDGET_MS, TURN_BUDGET_MS in test/webapp-layer/) are what
// actually bound the work — a stuck turn fails there in 5s, not here in 20s.
// Re-measure against a cold transform cache and tighten then.
const WebappLayerTimeout = 300 * time.Second

// npmOnce/npmBin resolve `npm` once per run, the same shape requireNode uses.
var (
	npmOnce sync.Once
	npmBin  string
	npmErr  error
)

// wlRequireNPM answers the `npm` binary on PATH, skipping the calling test
// loudly if there is none.
func wlRequireNPM(t *testing.T) string {
	t.Helper()
	npmOnce.Do(func() {
		npmBin, npmErr = exec.LookPath("npm")
	})
	if npmErr != nil {
		t.Skip("e2e/webapp-layer: npm not found on PATH; the webapp layer needs it to run the vitest child")
	}
	return npmBin
}

// wlRequireWebappDeps answers the webapp directory, skipping the calling test
// loudly when its node_modules is absent.
//
// THE HARNESS INSTALLS NOTHING, exactly as main_test.go's own builders do not:
// a network `npm ci` inside an e2e test is not this suite's business, so the
// skip names the command and directory that supply the prerequisite instead of
// running it. (The webapp's own `pretest` hooks self-bootstrap; the layer's
// `test:webapp-layer` script deliberately has no such hook.)
func wlRequireWebappDeps(t *testing.T) string {
	t.Helper()
	webappDir := filepath.Join(repo.repoDir, "webapp")
	if _, err := os.Stat(webappDir); err != nil {
		t.Skipf("e2e/webapp-layer: webapp not found at %s: %v", webappDir, err)
	}
	if _, err := os.Stat(filepath.Join(webappDir, "node_modules")); err != nil {
		t.Skipf("e2e/webapp-layer: %s/node_modules is absent; run `npm ci --prefix %s` first",
			webappDir, webappDir)
	}
	return webappDir
}

// ONE GO TEST PER AREA (project-lead ruling). Each area gets its OWN world,
// so its artifacts are preserved on its own failure, a red run names the area,
// and the areas parallelize; the ~2.7s world cost per area is acceptable at
// nine areas. Each area's Go test names exactly one vitest file, and that
// file is the area's scenario list from WEBAPP-LAYER-SPEC.md section F.

// TestWebappLayer is section F1: proof of life.
func TestWebappLayer(t *testing.T) {
	wlDriveArea(t, "proof-of-life.layer.test.ts")
}

// TestWebappLayerFeedFamilies is section F2: one drawn row family per test.
func TestWebappLayerFeedFamilies(t *testing.T) {
	wlDriveArea(t, "feed-families.layer.test.ts")
}

// TestWebappLayerSubfeeds is section F3: sub-feed open/collapse lifecycle.
func TestWebappLayerSubfeeds(t *testing.T) {
	wlDriveArea(t, "subfeeds.layer.test.ts")
}

// TestWebappLayerCards is section F4: permission and question cards.
func TestWebappLayerCards(t *testing.T) {
	wlDriveArea(t, "cards.layer.test.ts")
}

// TestWebappLayerSurfaces is section F5: footer and topbar surfaces.
func TestWebappLayerSurfaces(t *testing.T) {
	wlDriveArea(t, "surfaces.layer.test.ts")
}

// TestWebappLayerPanels is section F6: daemon-answered command panels.
func TestWebappLayerPanels(t *testing.T) {
	wlDriveArea(t, "panels.layer.test.ts")
}

// TestWebappLayerRefusals is section F8: refusal wording and placement.
func TestWebappLayerRefusals(t *testing.T) {
	wlDriveArea(t, "refusals.layer.test.ts")
}

// wlDriveArea builds one world and drives one of the layer's vitest files
// against its real daemon.
func wlDriveArea(t *testing.T, vitestFile string) {
	t.Helper()
	npm := wlRequireNPM(t)
	webappDir := wlRequireWebappDeps(t)

	w := NewWorld(t, WorldOpts{})
	repoFixture := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repoFixture.Dir)

	// THE HOST PARTICIPANT, WHICH EMACS WOULD BE.
	//
	// The footer's connectivity truth is the PAIR of participants
	// (server.holdParticipant fires on the open/close edges of
	// WatchHostWorkspace and WatchWebWorkspace). The real webapp supplies the
	// web hop itself — that is this layer's whole point — but Emacs is an
	// external system this suite mocks, so nothing holds the host hop and the
	// footer correctly draws `disconnected` forever. A disconnected footer
	// CLOSES the composer gate, so the page's second submission and every one
	// after it is refused by the app itself: the area files above the first
	// turn all fail, and the fault is not theirs.
	//
	// This is the same participant-gating World.WatchFooter does for every Go
	// footer wait in this suite, held here for exactly the child's lifetime.
	host := w.Daemon.WatchHost(ws)
	defer host.Close()

	// The daemon's serving address, as the daemon itself published it. This is
	// the WHOLE handoff: a reachable loopback origin, so the page's transport
	// needs no socket dispatcher of its own.
	if w.Addr == "" {
		t.Fatal("e2e/webapp-layer: the daemon published no address to hand the webapp")
	}

	env := append(os.Environ(),
		"AGENT_REPL_WEBAPP_LAYER=1",
		"AGENT_REPL_E2E_DAEMON_URL=http://"+w.Addr,
		"AGENT_REPL_E2E_WORKSPACE_ID="+ws.GetId(),
		"AGENT_REPL_E2E_WORKSPACE_DIR="+ws.GetDir(),
		// The same standing tripwire the vitest config sets, in case the child
		// is ever run through a different script: the vendor in this chain is
		// the fake SDK inside the real shim, never a network one.
		"AGENT_REPL_FORBID_VENDOR_CALLS=1",
		// vitest's own CI mode: no watch, no interactive reporter.
		"CI=1",
	)

	if err := wlRunVitest(t, npm, webappDir, vitestFile, env); err != nil {
		t.Fatalf("e2e/webapp-layer: %s failed: %v", vitestFile, err)
	}

	w.RequireNoUnexpectedExit(t)
}

// wlRunVitest runs the layer's vitest project as a child of this test,
// streaming its output into the test log so a red child is read here rather
// than hunted for, and answers its failure (if any).
//
// The child's exit is awaited on its own channel with a select against
// WebappLayerTimeout — never a sleep, and a timeout kills the process group
// rather than leaking a vitest that outlives the test.
func wlRunVitest(t *testing.T, npm, webappDir, vitestFile string, env []string) error {
	t.Helper()

	// The file filter is positional after `--`: one area's world drives one
	// area's file, never the whole layer.
	cmd := exec.Command(npm, "run", "--silent", "test:webapp-layer", "--",
		filepath.Join("test", "webapp-layer", vitestFile))
	cmd.Dir = webappDir
	cmd.Env = env
	stdout, err := cmd.StdoutPipe()
	if err != nil {
		return fmt.Errorf("vitest stdout pipe: %w", err)
	}
	stderr, err := cmd.StderrPipe()
	if err != nil {
		return fmt.Errorf("vitest stderr pipe: %w", err)
	}
	if err := cmd.Start(); err != nil {
		return fmt.Errorf("starting `npm run test:webapp-layer` in %s: %w", webappDir, err)
	}

	var mirrored sync.WaitGroup
	var tail struct {
		sync.Mutex
		lines []string
	}
	mirror := func(name string, r io.Reader) {
		defer mirrored.Done()
		scanner := bufio.NewScanner(r)
		scanner.Buffer(make([]byte, 0, 64*1024), 1024*1024)
		for scanner.Scan() {
			line := scanner.Text()
			t.Logf("webapp-layer %s | %s", name, line)
			tail.Lock()
			tail.lines = append(tail.lines, line)
			if len(tail.lines) > wlTailLines {
				tail.lines = tail.lines[len(tail.lines)-wlTailLines:]
			}
			tail.Unlock()
		}
	}
	mirrored.Add(2)
	go mirror("out", stdout)
	go mirror("err", stderr)

	done := make(chan error, 1)
	go func() {
		mirrored.Wait()
		done <- cmd.Wait()
	}()

	select {
	case waitErr := <-done:
		if waitErr == nil {
			return nil
		}
		tail.Lock()
		defer tail.Unlock()
		return fmt.Errorf("vitest exited %v; last output:\n%s", waitErr, strings.Join(tail.lines, "\n"))
	case <-time.After(WebappLayerTimeout):
		// The child is killed, not abandoned: a leaked vitest holds this
		// daemon's streams open and the world's teardown would then observe
		// state the test never caused.
		if cmd.Process != nil {
			_ = cmd.Process.Kill()
		}
		tail.Lock()
		defer tail.Unlock()
		return fmt.Errorf("vitest did not exit within %s; last output:\n%s",
			WebappLayerTimeout, strings.Join(tail.lines, "\n"))
	}
}

// wlTailLines is how much of the child's output a failure quotes inline. The
// full stream is already in the test log via t.Logf; this is the excerpt that
// rides the failure message itself.
const wlTailLines = 40
