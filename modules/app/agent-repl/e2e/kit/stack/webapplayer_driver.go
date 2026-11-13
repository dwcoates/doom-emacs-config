package stack

import (
	"agent-repld/integration/grantedclock"
	"agent-repld/integration/harness"
	"agentrepl/e2e/kit"
	workspacev1 "agentrepl/proto/workspace/v1"
	"bufio"
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"syscall"
	"testing"
	"time"
)

// WebappLayerTimeout bounds the vitest CHILD PROCESS end to end.
//
// MEASURED UNDER THE LOAD THE PACKAGE ACTUALLY RUNS AT, then set at ~3x the
// observed max, the same way every other bound in this suite is derived.
//
// ITS EARLIER DERIVATION WAS TAKEN FROM THE WRONG NUMBER AND THE WRONG
// CONDITIONS, and that is why it kept failing green areas. It read "per-area
// vitest child durations: 1.21s ... 3.16s; 3x the 3.16s max is ~9.5s, so 10s",
// but those are vitest's own `Duration` summary — which begins after npm's
// wrapper and node's boot and ends before the worker pool's teardown — taken
// with the layer alone on the box. Nothing ever reported the child's own wall,
// which is what this constant bounds; it does now, on every run, green or red.
//
// The wall, per area, over fourteen full-package runs at `-parallel 8`
// (sorted; the last figure in each row is that area's observed maximum):
//
//	cards            3.22 3.47 3.51 3.51 3.55 3.64 3.71 3.92 3.99 4.07 4.13 4.17 5.07 9.23
//	merge-tabs       1.82 2.07 2.08 2.09 2.32 2.42 2.45 2.58 2.71 2.77 2.93 3.15 4.59 9.11
//	subfeeds         3.27 3.43 3.44 3.45 3.52 3.61 3.62 3.86 4.06 4.13 4.21 4.62 4.69 8.80
//	panels           2.22 2.28 2.29 2.35 2.38 2.41 2.49 2.61 2.63 2.79 3.08 4.17 4.48 8.21
//	refusals         2.30 2.35 2.40 2.45 2.48 2.55 2.66 2.67 2.73 2.90 3.15 3.90 4.55 7.99
//	roster           2.44 2.44 2.46 2.55 2.67 2.77 2.96 3.07 3.18 3.29 3.31 5.27      7.91
//	feed-families    6.21 6.83 7.00 7.11 7.49 7.49 7.74 7.78 7.78 7.79 7.98 8.01 8.42
//	surfaces         2.54 2.57 2.61 2.61 2.65 2.66 2.71 2.75 2.75 2.80 2.85 4.17 4.53 7.69
//	query-death      2.81 2.85 2.87 2.96 3.04 3.09 3.09 3.13 3.22 3.30 3.34 3.50 3.73 7.05
//	client-log       3.76 3.83 3.96 3.98 4.07 4.13 4.31 4.34 4.35 4.45 4.62 4.63 4.65 4.84
//	proof-of-life    3.67 3.67 3.82 3.89 3.92 3.93 4.08 4.10 4.13 4.30 4.41 4.61 4.63 4.72
//
// So the median area runs in about three seconds and every one of them has a
// tail into the eights on a busy run — 10s was ~1.1x the observed max for
// half the roster, not the ~3x this suite sets its bounds at, and areas kept
// dying on it while doing nothing wrong. 30s was ~3.2x the 9.23s maximum.
//
// RE-MEASURED UNDER testrun (2026-10-06), which is how the package runs now:
// one e2e chunk per core slot beside every other suite, not `-parallel 8`
// alone. Three green full-suite runs put the functional areas' walls at
// 5.0-14.4s (client-log 14.43, proof-of-life 14.21, feed-families 13.33);
// a heavier full run the same day had walls of 12.8-28.2s and three areas
// killed at 30s while their children were still passing. 30s was ~2.1x the
// green maximum. 45s is ~3.1x the 14.4s green maximum.
//
// FEED-FAMILIES NO LONGER NEEDS A BOUND OF ITS OWN, and the loaded figures are
// why: it is the area with the longest MEDIAN (24 real turns in one child) but
// not the longest maximum — cards and merge-tabs both spike higher. One
// measured default covers them all, and a per-area constant that the data does
// not support is a number nobody can check.
//
// It still bounds a HANG, not a synchronization wait: nothing here sleeps, the
// child's exit is awaited on its own channel, and the child's own per-site
// budgets (BOOT_BUDGET_MS and TURN_BUDGET_MS in test/webapp-layer/drive.ts)
// fail a stuck assertion long before this fires.
//
// THE WARM DRIVER DOES NOT USE IT: an area run there is bounded by
// WebappLayerSilence, between finished tests.
//
// AN AREA WHOSE CHILD IS STRUCTURALLY LONGER THAN A FUNCTIONAL ONE STILL DOES
// NOT RELAX THIS CONSTANT; it passes its own bound to wlChild.WaitFor. The
// restart-handover area is the one such caller (WebappLayerHandoverTimeout),
// and its child is two whole process lifecycles rather than a slow area.
const WebappLayerTimeout = 45 * time.Second

// WebappLayerSilence bounds how long an area on the warm driver may go without
// finishing a test (wlServer.Run): every finished test restarts it.
//
// A BOUND ON THE WHOLE RUN FAILED AREAS THAT WERE STILL PASSING. A coverage run
// at a host load near 70 (2026-10-14) stretched feed-families' 27 tests past
// 45s with the area's world serving it throughout and no per-site budget
// (drive.ts) tripping: it was starved, not hung. Only an area that stopped
// finishing tests trips this, however long a loaded host makes the whole run.
//
// MEASURED 2026-10-14, the longest silence per area: 1.0-2.7s with the layer
// alone, 1.7-7.9s in a full run, 3.9-8.2s in a full coverage run (host load
// 40-50). 20s is just over twice the 8.2s maximum, and above every per-site
// budget, so a stuck assertion still fails on its own budget first.
const WebappLayerSilence = 20 * time.Second

// WebappLayerHandoverTimeout bounds the RESTART-HANDOVER area's child, which
// is structurally longer than a functional area's by two whole process
// lifecycles: a merge landing, the rollout trigger, the incumbent re-execing
// itself, a second real agent-repld's full boot, the adoption rendezvous, and
// only then the `transferred` push the page waits on.
//
// The child's own budget for that is HANDOVER_TEST_MS in
// `webapp/test/webapp-layer/drive.ts` — BOOT + HANDOVER + BOOT = 25s, where
// the handover term is `harness.HandoverChainTimeout` inherited verbatim from
// the Go driver. This bound must outlive the child's own, or the Go side would
// kill a page that was still inside a budget the Go side gave it; 60s is ~2.4x
// that 25s, the same "a bound outlives the thing it bounds" rule
// wlDriveArea's participant hold is written under.
const WebappLayerHandoverTimeout = 60 * time.Second

// WebappLayerColdMountTimeout bounds a COLD vitest child's boot up to its
// page's mount: npm, node, vite's cold transform and collect of the module
// graph, then the page's own boot. The first three are exactly the warm
// driver's warm-up, so it is wlWarmupBound plus the page's kit.WlBootBudget.
//
// WebappLayerTimeout DOES NOT BOUND IT: that constant is sized for an area run
// whose cold transform the warm driver already paid. A full gate run
// (2026-10-17) warmed its driver in 41s, and the restart-handover child, cold
// in the same run, had not yet collected its file when the 45s bound fired.
const WebappLayerColdMountTimeout = wlWarmupBound + kit.WlBootBudget

// npmOnce/npmBin resolve `npm` once per run, the same shape requireNode uses.
var (
	npmOnce sync.Once
	npmBin  string
	npmErr  error
)

// wlRequireNPM answers the `npm` binary on PATH, failing the calling test
// loudly if there is none (a skip only under the opt-out; see
// precondition_test.go).
func WlRequireNPM(t *testing.T) string {
	t.Helper()
	npmOnce.Do(func() {
		npmBin, npmErr = exec.LookPath("npm")
	})
	if npmErr != nil {
		requireDependency(t, "e2e/webapp-layer: npm not found on PATH; the webapp layer needs it to run the vitest child")
	}
	return npmBin
}

// wlRequireWebappDeps answers the webapp directory, failing the calling test
// loudly when its node_modules is absent.
//
// THE HARNESS INSTALLS NOTHING, exactly as main_test.go's own builders do not:
// a network `npm ci` inside an e2e test is not this suite's business, so the
// failure names the command and directory that supply the prerequisite instead
// of running it. It is a failure rather than a skip for the reason vitest's own
// webapp-layer config states: a run that found nothing must never look like a
// pass (precondition_test.go carries the opt-out). (The webapp's own `pretest` hooks self-bootstrap; the layer's
// `test:webapp-layer` script deliberately has no such hook.)
func WlRequireWebappDeps(t *testing.T) string {
	t.Helper()
	webappDir := filepath.Join(Repo.repoDir, "webapp")
	if _, err := os.Stat(webappDir); err != nil {
		requireDependency(t, "e2e/webapp-layer: webapp not found at %s: %v", webappDir, err)
	}
	if _, err := os.Stat(filepath.Join(webappDir, "node_modules")); err != nil {
		requireDependency(t, "e2e/webapp-layer: %s/node_modules is absent; run `npm ci --prefix %s` first",
			webappDir, webappDir)
	}
	wlRequireWebappWritable(t, webappDir)
	return webappDir
}

// wlRequireWebappWritable fails the calling test, by name and before a world
// is built, when the vitest child could not write what it must write.
//
// WHY THIS EXISTS, and it is a defect this suite already paid for once. The
// e2e sandbox links `webapp/node_modules` at a READ-ONLY image layer (the
// trees are baked once and shared, because materializing them per run cost
// ~730 MiB of tmpfs and OOM-killed concurrent sandboxes). Vite's default
// `cacheDir` is `node_modules/.vite`, so vitest's first act failed with a
// filesystem error and ALL ELEVEN areas of this layer went red — inside the
// container, which is the environment this layer exists for, while staying
// green on the host. Eleven confusing failures named the symptom eleven times
// and the cause not once.
//
// The cache now lives in the webapp package directory instead
// (webapp/vite-cache.ts; webapp/test/vite-cache.test.ts holds every config to
// it). That directory being writable is what the whole layer rests on, so it
// is CHECKED HERE rather than assumed: if it stops being writable — a
// read-only mount, a staging change, a sandbox that starts linking the package
// directory too — the run says the layer cannot run, instead of eleven areas
// failing somewhere deep in a node process.
//
// It is a hard failure, never a skip and never the missing-dependency opt-out:
// no command the reader could run supplies this, and a layer that could not
// run must never read as one that did.
func wlRequireWebappWritable(t *testing.T, webappDir string) {
	t.Helper()
	if err := kit.WlWebappWritable(webappDir); err != nil {
		t.Fatalf("e2e/webapp-layer: THE WEBAPP LAYER CANNOT RUN HERE.\n"+
			"  %v\n"+
			"  vite and vitest keep their cache in that directory (webapp/vite-cache.ts),\n"+
			"  so the vitest child cannot start and no area of this layer would exercise anything.\n"+
			"  In the e2e sandbox this is what a read-only working copy looks like; on the host,\n"+
			"  check the permissions on the checkout.", err)
	}
}

// wlDriveArea builds one world and drives one of the layer's vitest files
// against its real daemon.
func WlDriveArea(t *testing.T, vitestFile string, expectWarnings ...string) {
	t.Helper()
	WlDriveAreaWorld(t, vitestFile, expectWarnings...)
}

// wlDriveAreaWorld is wlDriveArea, answering the world and the workspace it
// drove so an area whose subject is the daemon's OWN artifacts (the client-log
// area reads the workspace's log sinks) can assert against them after the
// child has exited. Every other area wants neither and calls wlDriveArea.
func WlDriveAreaWorld(t *testing.T, vitestFile string, expectWarnings ...string) (*World, *workspacev1.WorkspaceRef) {
	t.Helper()
	WlHoldAreaSlot(t)
	npm := WlRequireNPM(t)
	webappDir := WlRequireWebappDeps(t)

	w := NewWorld(t, WorldOpts{})
	if len(expectWarnings) > 0 {
		w.ExpectWarnings(expectWarnings...)
	}
	ws := w.NewWorkspace(t)

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
	//
	// ON THE CHILD'S BOUND, NEVER THE HARNESS'S. `Daemon.WatchHost` runs on
	// d.ctx, which expires at harness.DefaultTimeout — five seconds, sized for
	// one wait. A vitest file runs far longer than that, so the hold used to
	// expire mid-file: the daemon dropped `host_stream` to false, the footer
	// resolved `disconnected/severed`, the real page closed its composer gate,
	// and every later `send()` in drive.ts became a silent no-op — observed in
	// run 10 as six feed-family scenarios timing out five seconds apart with
	// no SubmitPrompt reaching the daemon at all. The hold therefore runs on
	// the bound of the thing it is held for, and is DRAINED so the harness's
	// own pump cannot wedge on a buffer nobody reads.
	//
	// AND IT IS RELEASED BY ITS HOLDER, NEVER BY A CLOCK OF ITS OWN. The file
	// waits for the shared vitest server (wlServer.Run takes one file at a
	// time) BEFORE its own WebappLayerTimeout starts, so a hold timed from
	// here expired while the file was still queued: run 2 of the
	// held-prompt-merge-flush gate (2026-10-13) dropped `host_stream` 45.004s
	// after this line, the instant roster.layer.test.ts began, and the page's
	// footer drew `severed` and closed the composer for both tray scenarios.
	// The child's run is what is bounded (wlServer.Run's deadline); the hold
	// lives exactly as long as this area does.
	holdCtx, releaseHold := context.WithCancel(context.Background())
	defer releaseHold()
	host := w.Daemon.WatchHostFor(holdCtx, ws)
	host.Drain()
	defer host.Close()

	// The daemon's serving address, as the daemon itself published it. This is
	// the WHOLE handoff: a reachable loopback origin, so the page's transport
	// needs no socket dispatcher of its own.
	if w.Addr == "" {
		t.Fatal("e2e/webapp-layer: the daemon published no address to hand the webapp")
	}

	if err := wlRunLayerFile(t, npm, webappDir, vitestFile, w, ws); err != nil {
		t.Fatalf("e2e/webapp-layer: %s failed: %v", vitestFile, err)
	}

	w.RequireNoUnexpectedExit(t)
	return w, ws
}

// wlChildEnv is the environment one vitest child is handed: the gate, the
// daemon's own published address, the workspace the page is addressed to, and
// the webapp build the daemon serves.
func WlChildEnv(t *testing.T, w *World, ws *workspacev1.WorkspaceRef) []string {
	t.Helper()
	if w.Addr == "" {
		t.Fatal("e2e/webapp-layer: the daemon published no address to hand the webapp")
	}
	return append(os.Environ(),
		"AGENT_REPL_WEBAPP_LAYER=1",
		"AGENT_REPL_E2E_DAEMON_URL=http://"+w.Addr,
		"AGENT_REPL_E2E_WORKSPACE_ID="+ws.GetId(),
		"AGENT_REPL_E2E_WORKSPACE_DIR="+ws.GetDir(),
		// The build the daemon's served dist names, which the page states as
		// its own through its entry tag: a page stating any other build is
		// one a deploy would push a reload.
		"AGENT_REPL_E2E_WEBAPP_BUILD="+harness.FakeWebappEntry,
		// The same standing tripwire the vitest config sets, in case the child
		// is ever run through a different script: the vendor in this chain is
		// the fake SDK inside the real shim, never a network one.
		"AGENT_REPL_FORBID_VENDOR_CALLS=1",
		// vitest's own CI mode: no watch, no interactive reporter.
		"CI=1",
	)
}

// wlChild is a vitest child still running, for the ONE area whose Go side must
// act WHILE the page is mounted (§F9 #39, the restart handover). Every other
// area starts the child and immediately waits, which is wlRunVitest.
type wlChild struct {
	t    *testing.T
	cmd  *exec.Cmd
	done chan error
	// file names the area file this child runs, and started is when it was
	// spawned, so WaitFor can report the CHILD'S OWN WALL. Reported on every
	// run, green or red, because that wall is the only measurement
	// WebappLayerTimeout is derivable from and it was previously being
	// confused with the vitest-internal `Duration` line, which excludes the
	// npm wrapper, node's own boot and the pool's teardown.
	file     string
	started  time.Time
	tailOnly struct {
		sync.Mutex
		lines []string
	}
}

// wlAreaSlots CAPS HOW MANY WEBAPP-LAYER AREAS RUN AT ONCE, across the whole
// parallel suite.
//
// Every test in this package runs with t.Parallel(), so `-parallel N` bounds
// how many WORLDS stand at once — and a world is cheap next to a webapp-layer
// area. A world is three small Go processes plus a node shim; an area is that
// PLUS a whole vitest run (its own node process, an esbuild transform of the
// app's sources, and a jsdom document), and there are ten areas. Left uncapped,
// ten of those land together and the box saturates: measured at `-parallel 8`
// with no cap, the feed-families area went from 7.4s to 29.1s and four
// unrelated Go tests failed on bounds sized for an unloaded machine.
//
// The cap is a SEPARATE, TIGHTER bound than `-parallel` because the two things
// being bounded have wildly different weights; one number cannot size both.
//
// IT IS TAKEN BEFORE THE WORLD IS BUILT, not around the vitest child alone.
// A world's own context starts ticking at harness.StartDaemon — a queued area
// that had already built its world would sit there burning its budget and its
// four processes while it waited for a slot, which is both the load the cap
// exists to prevent and a bound expiring for a reason the test is not about.
// The slot therefore covers the whole area, and is released at test cleanup,
// after the world's own teardown has run.
//
// SIZED AT TWO, and the figure below is the measurement it is sized on.
// Against its neighbours at `-parallel 8` on a 16-core host: cap 2 took 31.2s,
// cap 3 took 20.7s, and cap 4 at `-parallel 12` lost a test. The faster cap 3
// later let a vitest worker's source-transform RPC time out during a full
// package run while the child itself exited after 10.8s. Two keeps the heavy
// node worker pools below that saturation point; the extra package wall is
// queueing before a world's own context starts, as required above.
const wlMaxConcurrentAreas = 2

var wlAreaSlots = make(chan struct{}, wlMaxConcurrentAreas)

// wlHoldAreaSlot blocks until this area may run, and gives the slot back at
// test cleanup. Registered before anything else the area builds, so a queued
// area holds nothing but its place in line.
func WlHoldAreaSlot(t *testing.T) {
	t.Helper()
	wlAreaSlots <- struct{}{}
	t.Cleanup(func() { <-wlAreaSlots })
}

// wlStartVitest starts the layer's vitest project as a child of this test and
// answers the running child, its output already being mirrored into the test
// log.
func WlStartVitest(t *testing.T, npm, webappDir, vitestFile string, env []string) (*wlChild, error) {
	t.Helper()

	// The file filter is positional after `--`: one area's world drives one
	// area's file, never the whole layer.
	cmd := exec.Command(npm, "run", "--silent", "test:webapp-layer", "--",
		filepath.Join("test", "webapp-layer", vitestFile))
	cmd.Dir = webappDir
	cmd.Env = env
	// THE CHILD LEADS ITS OWN PROCESS GROUP, so a kill reaches the whole tree.
	// `npm run` is a wrapper: it spawns vitest, which spawns a POOL of node
	// workers. Signalling cmd.Process alone signals npm and nothing else, and
	// every worker is reparented to init and keeps running — CPU-bound, on a
	// box that is by then running the rest of a `-parallel 8` suite. Its own
	// group is what makes wlKillTree's negative-pid kill possible.
	cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
	stdout, err := cmd.StdoutPipe()
	if err != nil {
		return nil, fmt.Errorf("vitest stdout pipe: %w", err)
	}
	stderr, err := cmd.StderrPipe()
	if err != nil {
		return nil, fmt.Errorf("vitest stderr pipe: %w", err)
	}
	if err := cmd.Start(); err != nil {
		return nil, fmt.Errorf("starting `npm run test:webapp-layer` in %s: %w", webappDir, err)
	}

	child := &wlChild{t: t, cmd: cmd, done: make(chan error, 1), file: vitestFile, started: time.Now()}
	// UNCONDITIONAL, so no path out of this test leaves a worker pool behind:
	// the Go side failing its own assertion, a t.Fatal on the way to Wait, a
	// panic, or the bound firing. Killing an already-exited group is a no-op
	// (ESRCH), which is exactly what the ordinary path wants.
	t.Cleanup(child.killTree)
	tail := &child.tailOnly

	var mirrored sync.WaitGroup
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

	go func() {
		mirrored.Wait()
		child.done <- cmd.Wait()
	}()

	return child, nil
}

// Wait awaits the child's exit and answers its failure (if any).
//
// The exit is awaited on its own channel with a select against
// WebappLayerTimeout — never a sleep, and a timeout kills the child rather
// than leaking a vitest that outlives the test.
func (c *wlChild) Wait() error {
	c.t.Helper()
	return c.WaitFor(WebappLayerTimeout)
}

// WaitFor is Wait on a caller-supplied bound, for an area whose child is
// structurally longer than a functional one. The bound is the caller's because
// only the caller knows what its child does; WebappLayerTimeout is sized for a
// functional area and says so.
func (c *wlChild) WaitFor(bound time.Duration) error {
	c.t.Helper()
	tail := &c.tailOnly
	// THE BOUND IS GRANTED TIME (grantedclock): on 2026-10-22 two layer files
	// ran past their wall bounds on a host carrying a second full run.
	expired, stop := grantedclock.WithBound(context.Background(), bound)
	defer stop()
	select {
	case waitErr := <-c.done:
		wall := time.Since(c.started)
		c.t.Logf("e2e/webapp-layer: %s child wall %s (bound %s)", c.file, wall.Round(time.Millisecond), bound)
		if waitErr == nil {
			return nil
		}
		tail.Lock()
		defer tail.Unlock()
		return fmt.Errorf("vitest exited %v; last output:\n%s", waitErr, strings.Join(tail.lines, "\n"))
	case <-expired.Done():
		// The child is killed, not abandoned: a leaked vitest holds this
		// daemon's streams open and the world's teardown would then observe
		// state the test never caused. THE WHOLE GROUP GOES, not the npm
		// wrapper alone — see killTree.
		c.killTree()
		tail.Lock()
		defer tail.Unlock()
		c.t.Logf("e2e/webapp-layer: %s child wall EXCEEDED the %s bound", c.file, bound)
		return fmt.Errorf("vitest did not exit within %s; last output:\n%s",
			bound, strings.Join(tail.lines, "\n"))
	}
}

// Kill stops a still-running child, for the Go side failing before the page's
// own assertions could conclude. A killed child's output is already in the
// test log.
func (c *wlChild) Kill() { c.killTree() }

// killTree SIGKILLs the child's whole process group.
//
// `npm run` is a wrapper around vitest, and vitest runs its files in a POOL of
// forked node workers. Killing cmd.Process kills npm and leaves every one of
// those workers alive, reparented to init, spinning on a box that is still
// running the rest of a `-parallel 8` suite — the load that then expires
// bounds in unrelated tests. wlStartVitest puts the child in its own group
// precisely so the negative pid below reaches all of them.
//
// Idempotent and quiet on an already-dead group: ESRCH is the ordinary answer
// on the clean path, where the child exited on its own and this runs at
// cleanup. Any OTHER error is a fault to report, never one to swallow.
func (c *wlChild) killTree() {
	if c.cmd.Process == nil {
		return
	}
	// The group id IS the child's pid: Setpgid with no Pgid makes the child a
	// group leader.
	pgid := c.cmd.Process.Pid
	err := syscall.Kill(-pgid, syscall.SIGKILL)
	switch {
	case err == nil, errors.Is(err, syscall.ESRCH):
		return
	case errors.Is(err, syscall.EPERM):
		// DARWIN ANSWERS EPERM FOR A GROUP HOLDING A ZOMBIE: a worker that
		// exited and launchd has not reaped yet cannot be signalled, and the
		// group-wide kill then reports the whole group refused (measured in a
		// loaded e2e run, 2026-10-03: TestWebappLayerQueryDeath's cleanup,
		// after its vitest child had exited on its own). What matters is that
		// no LIVING member survives, so each one is killed by pid and any
		// failure there is reported.
		live, listErr := wlLiveGroupMembers(pgid)
		if listErr != nil {
			c.t.Errorf("killing the vitest process group %d: %v, and its members could not be listed: %v", pgid, err, listErr)
			return
		}
		for _, pid := range live {
			if killErr := syscall.Kill(pid, syscall.SIGKILL); killErr != nil && !errors.Is(killErr, syscall.ESRCH) {
				c.t.Errorf("killing vitest process %d of group %d: %v (it may still be running)", pid, pgid, killErr)
			}
		}
	default:
		c.t.Errorf("killing the vitest process group %d: %v (its worker pool may still be running)", pgid, err)
	}
}

// wlLiveGroupMembers answers the members of process group PGID that are not
// zombies, read from ps.
func wlLiveGroupMembers(pgid int) ([]int, error) {
	out, err := exec.Command("ps", "-A", "-o", "pid=,pgid=,stat=").Output()
	if err != nil {
		return nil, fmt.Errorf("ps: %w", err)
	}
	return kit.WlParseLiveGroupMembers(string(out), pgid)
}

// wlTailLines is how much of the child's output a failure quotes inline. The
// full stream is already in the test log via t.Logf; this is the excerpt that
// rides the failure message itself.
const wlTailLines = 40

// wlAwaitPidFile reads the pid the stand-in child wrote, polling a file the
// child creates rather than sleeping for a fixed guess at its startup.
func wlAwaitPidFile(t *testing.T, path string) int {
	t.Helper()
	deadline := time.Now().Add(DefaultTimeout)
	for {
		body, err := os.ReadFile(path)
		if err == nil {
			if pid, convErr := strconv.Atoi(strings.TrimSpace(string(body))); convErr == nil && pid > 0 {
				return pid
			}
		} else if !os.IsNotExist(err) {
			t.Fatalf("read %s: %v", path, err)
		}
		if time.Now().After(deadline) {
			t.Fatalf("the stand-in child never wrote its grandchild's pid to %s within %s", path, DefaultTimeout)
		}
		time.Sleep(PollInterval)
	}
}
