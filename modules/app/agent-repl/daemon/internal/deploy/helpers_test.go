package deploy

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"

	"agentrepl/logging/buildreport"

	"claude-repld/internal/bounce"
	"claude-repld/internal/buildid"
	"claude-repld/internal/deployprogress"
	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/rollout"
	"claude-repld/internal/wsm"
)

// instant is the fixed instant every test's arithmetic starts from.
var instant = time.Date(2026, 9, 23, 12, 0, 0, 0, time.UTC)

// stepClock is a clock whose waits ELAPSE AT ONCE in simulated time: every
// After advances Now by the duration and fires immediately. A poll loop driven
// by it runs its whole schedule deterministically, and nothing waits.
type stepClock struct {
	mu  sync.Mutex
	now time.Time
}

func newStepClock() *stepClock { return &stepClock{now: instant} }

func (c *stepClock) Now() time.Time {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.now
}

func (c *stepClock) After(d time.Duration) <-chan time.Time {
	c.mu.Lock()
	c.now = c.now.Add(d)
	now := c.now
	c.mu.Unlock()
	ch := make(chan time.Time, 1)
	ch <- now
	return ch
}

// records filters a test logger's records by operation.
func records(log *dlog.TestSurfaces, operation string) []dlog.Record {
	var out []dlog.Record
	for _, r := range log.Records() {
		if r.Operation == operation {
			out = append(out, r)
		}
	}
	return out
}

// logged reports whether a record at level under operation contains substr.
func logged(log *dlog.TestSurfaces, level, operation, substr string) bool {
	for _, r := range records(log, operation) {
		if r.Level == level && strings.Contains(r.Message, substr) {
			return true
		}
	}
	return false
}

// artifacts are the bytes one fake build stages, per artifact.
type artifacts struct {
	shim, webappEntry, daemon, store, sidecar, lock string
}

// stageInto writes a build's artifacts in build-frontend's staging layout.
func (a artifacts) stageInto(t *testing.T, staging string) {
	t.Helper()
	s := Staged{Dir: staging}
	writeFile(t, s.ShimMain(), a.shim)
	writeFile(t, filepath.Join(filepath.Dir(s.ShimMain()), ".built-sha"), "sha-"+a.shim)
	writeFile(t, filepath.Join(s.WebappDist(), "index.html"), `<script type="module" src="/assets/index-`+a.webappEntry+`.js"></script>`)
	writeFile(t, filepath.Join(s.WebappDist(), "assets", "index-"+a.webappEntry+".js"), "bundle "+a.webappEntry)
	writeFile(t, s.DaemonBin(), a.daemon)
	writeFile(t, s.CacheBin(buildreport.ServiceStore), a.store)
	writeFile(t, s.CacheBin(buildreport.ServiceSidecar), a.sidecar)
	writeFile(t, s.CacheBin("shim-lock"), a.lock)
}

// installInto writes the same artifacts at their live locations.
func (a artifacts) installInto(t *testing.T, live Live) {
	t.Helper()
	writeFile(t, live.ShimMain(), a.shim)
	writeFile(t, filepath.Join(live.WebappDist(), "index.html"), `<script type="module" src="/assets/index-`+a.webappEntry+`.js"></script>`)
	writeFile(t, live.DaemonBin(), a.daemon)
	writeFile(t, live.CacheBinPath(buildreport.ServiceStore), a.store)
	writeFile(t, live.CacheBinPath(buildreport.ServiceSidecar), a.sidecar)
	writeFile(t, live.CacheBinPath("shim-lock"), a.lock)
}

func writeFile(t *testing.T, path, content string) {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, []byte(content), 0o755); err != nil {
		t.Fatal(err)
	}
}

func readFile(t *testing.T, path string) string {
	t.Helper()
	raw, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read %s: %v", path, err)
	}
	return string(raw)
}

func hashOf(t *testing.T, content string) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), "h")
	writeFile(t, path, content)
	h, err := buildid.File(path)
	if err != nil {
		t.Fatal(err)
	}
	return h
}

// fakeBuilder stages a scripted build, or fails a step.
type fakeBuilder struct {
	mu    sync.Mutex
	t     *testing.T
	build artifacts
	// stageNothing makes the build succeed and stage no artifact at all.
	stageNothing bool
	fail         *BuildFailed
	builds       int
	started      chan struct{}
	gate         chan struct{}
}

func (b *fakeBuilder) Build(_ context.Context, staging string) error {
	b.mu.Lock()
	b.builds++
	fail, build, gate, started, nothing := b.fail, b.build, b.gate, b.started, b.stageNothing
	b.mu.Unlock()
	if started != nil {
		started <- struct{}{}
	}
	if gate != nil {
		<-gate
	}
	if fail != nil {
		return fail
	}
	if nothing {
		return nil
	}
	build.stageInto(b.t, staging)
	return nil
}

func (b *fakeBuilder) count() int {
	b.mu.Lock()
	defer b.mu.Unlock()
	return b.builds
}

// fakeRollout records what the deploy asked of the rollout.
type fakeRollout struct {
	mu         sync.Mutex
	joining    bool
	rolling    []ids.WorkspaceID
	handovers  []bool
	handErr    error
	restarts   []bool
	restartErr error
	checks     []bool
	stale      map[ids.WorkspaceID]bool
	checkErr   map[ids.WorkspaceID]error
	// registered are the stale workspaces whose bounce the registry
	// REGISTERS behind their work rather than taking now.
	registered map[ids.WorkspaceID]bool
	acceptance rollout.HandoverAcceptance
}

func (r *fakeRollout) HandOver(_ context.Context, force bool) (rollout.HandoverAcceptance, error) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.handovers = append(r.handovers, force)
	acc := r.acceptance
	acc.Forced = force
	return acc, r.handErr
}

func (r *fakeRollout) Restart(_ context.Context, force bool) (rollout.HandoverAcceptance, error) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.restarts = append(r.restarts, force)
	acc := r.acceptance
	acc.Forced = force
	return acc, r.restartErr
}

func (r *fakeRollout) CheckStaleness(_ context.Context, ws ids.WorkspaceID, force bool) (rollout.StaleCheck, error) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.checks = append(r.checks, force)
	if err := r.checkErr[ws]; err != nil {
		return rollout.StaleCheck{}, err
	}
	if !r.stale[ws] {
		return rollout.StaleCheck{Reported: "fresh", Installed: "fresh"}, nil
	}
	now := force || !r.registered[ws]
	return rollout.StaleCheck{Stale: true, Reported: "old", Installed: "fresh", Bounce: bounce.Decision{Now: now, Forced: force}}, nil
}

func (r *fakeRollout) Joining() bool {
	r.mu.Lock()
	defer r.mu.Unlock()
	return r.joining
}

func (r *fakeRollout) RollingOut() ([]ids.WorkspaceID, bool) {
	r.mu.Lock()
	defer r.mu.Unlock()
	return r.rolling, len(r.rolling) > 0
}

// fakeClients are the connected clients, with what they reported.
type fakeClients struct {
	mu           sync.Mutex
	emacs        []EmacsClient
	webviews     map[ids.WorkspaceID][]string
	elispPushes  [][]string
	elispRoot    string
	elispBuild   string
	webappPushes []ids.WorkspaceID
}

func (c *fakeClients) EmacsBuilds() []EmacsClient {
	c.mu.Lock()
	defer c.mu.Unlock()
	return append([]EmacsClient(nil), c.emacs...)
}

func (c *fakeClients) PushReloadElisp(streams []string, root, build string) int {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.elispPushes = append(c.elispPushes, streams)
	c.elispRoot, c.elispBuild = root, build
	return len(streams)
}

func (c *fakeClients) WebviewBuilds() map[ids.WorkspaceID][]string {
	c.mu.Lock()
	defer c.mu.Unlock()
	out := make(map[ids.WorkspaceID][]string, len(c.webviews))
	for ws, builds := range c.webviews {
		out[ws] = append([]string(nil), builds...)
	}
	return out
}

func (c *fakeClients) PushReloadWebapp(ws ids.WorkspaceID) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.webappPushes = append(c.webappPushes, ws)
}

// fakeServices records the restarts.
type fakeServices struct {
	mu         sync.Mutex
	calls      []string
	storeErr   error
	sidecarErr error
}

func (s *fakeServices) RestartStore(context.Context) error {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.calls = append(s.calls, "store")
	return s.storeErr
}

func (s *fakeServices) RestartSidecar(context.Context) error {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.calls = append(s.calls, "sidecar")
	return s.sidecarErr
}

func (s *fakeServices) Calls() []string {
	s.mu.Lock()
	defer s.mu.Unlock()
	return append([]string(nil), s.calls...)
}

// harness is one Deployer over a temp checkout, a temp cache-bin, and fakes.
type harness struct {
	d         *Deployer
	live      Live
	builder   *fakeBuilder
	rollout   *fakeRollout
	clients   *fakeClients
	services  *fakeServices
	reportDir string
	log       *dlog.TestSurfaces
	alive     map[int]bool
	fresh     artifacts
	elisp     string
	// freshLayout is the state layout the staged daemon answers, and
	// layoutErr fails the question; layoutAsked records the binaries asked.
	freshLayout int
	layoutErr   error
	layoutAsked []string
	progress    *fakeProgress
	faults      *fakeFaults
}

// fakeFaults is the state client's fault table: every fault ever opened, in
// order, with whether it is still open.
type fakeFaults struct {
	mu       sync.Mutex
	recorded []wsm.Fault
	closed   map[ids.FaultID]bool
	openErr  error
	readErr  error
	closeErr error
}

func newFakeFaults() *fakeFaults { return &fakeFaults{closed: map[ids.FaultID]bool{}} }

func (f *fakeFaults) OpenFault(_ context.Context, fault wsm.Fault) (ids.FaultID, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.openErr != nil {
		return "", f.openErr
	}
	fault.ID = ids.FaultID(fmt.Sprintf("fault-%d", len(f.recorded)+1))
	f.recorded = append(f.recorded, fault)
	return fault.ID, nil
}

func (f *fakeFaults) OpenFaults(_ context.Context, scope wsm.FaultScope) ([]wsm.Fault, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.readErr != nil {
		return nil, f.readErr
	}
	var out []wsm.Fault
	for _, fault := range f.recorded {
		if !f.closed[fault.ID] && (scope.Kind == "" || fault.Kind == scope.Kind) {
			out = append(out, fault)
		}
	}
	return out, nil
}

func (f *fakeFaults) CloseFault(_ context.Context, id ids.FaultID, _ time.Time) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.closeErr != nil {
		return f.closeErr
	}
	f.closed[id] = true
	return nil
}

// standing answers the open faults of every kind.
func (f *fakeFaults) standing(t *testing.T) []wsm.Fault {
	t.Helper()
	open, err := f.OpenFaults(context.Background(), wsm.FaultScope{})
	if err != nil {
		t.Fatalf("read the standing faults: %v", err)
	}
	return open
}

// seed records a standing fault as an earlier deploy or daemon left it.
func (f *fakeFaults) seed(t *testing.T, fault wsm.Fault) ids.FaultID {
	t.Helper()
	id, err := f.OpenFault(context.Background(), fault)
	if err != nil {
		t.Fatalf("seed a fault: %v", err)
	}
	return id
}

// fakeProgress records every statement the deploy made on the update line,
// nil (a clear) included.
type fakeProgress struct {
	mu     sync.Mutex
	stated []*deployprogress.Progress
}

func (f *fakeProgress) SetDeployProgress(p *deployprogress.Progress) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.stated = append(f.stated, p)
}

// phases names every statement in order, "cleared" for a clear.
func (f *fakeProgress) phases() []string {
	f.mu.Lock()
	defer f.mu.Unlock()
	out := make([]string, 0, len(f.stated))
	for _, p := range f.stated {
		if p == nil {
			out = append(out, "cleared")
			continue
		}
		out = append(out, p.Phase.String())
	}
	return out
}

// last is the newest statement.
func (f *fakeProgress) last() *deployprogress.Progress {
	f.mu.Lock()
	defer f.mu.Unlock()
	if len(f.stated) == 0 {
		return nil
	}
	return f.stated[len(f.stated)-1]
}

// runningLayout is the state layout the harness's running daemon writes.
const runningLayout = 11

// theFresh is the build every harness stages; theOld is what is installed and
// running before it.
var (
	theFresh = artifacts{shim: "shim-2", webappEntry: "Web2", daemon: "daemon-2", store: "store-2", sidecar: "sidecar-2", lock: "lock-2"}
	theOld   = artifacts{shim: "shim-1", webappEntry: "Web1", daemon: "daemon-1", store: "store-1", sidecar: "sidecar-1", lock: "lock-1"}
)

func newHarness(t *testing.T) *harness {
	t.Helper()
	root := t.TempDir()
	writeFile(t, filepath.Join(root, "config.el"), "(agent-repl--load-module \"core\")\n")
	writeFile(t, filepath.Join(root, "lisp", "core.el"), "(provide 'core)\n")
	elisp, err := buildid.Elisp(root)
	if err != nil {
		t.Fatal(err)
	}
	live := Live{ModuleRoot: root, CacheBin: filepath.Join(t.TempDir(), "bin")}
	theOld.installInto(t, live)
	h := &harness{
		live:      live,
		builder:   &fakeBuilder{t: t, build: theFresh},
		rollout:   &fakeRollout{stale: map[ids.WorkspaceID]bool{}, checkErr: map[ids.WorkspaceID]error{}},
		clients:   &fakeClients{webviews: map[ids.WorkspaceID][]string{}},
		services:  &fakeServices{},
		reportDir: t.TempDir(),
		log:       dlog.NewTestSurfaces(),
		alive:     map[int]bool{},
		fresh:     theFresh,
		elisp:     elisp,

		freshLayout: runningLayout,
		progress:    &fakeProgress{},
		faults:      newFakeFaults(),
	}
	// Both services run the FRESH build unless a test says otherwise.
	h.report(t, buildreport.ServiceStore, 101, hashOf(t, theFresh.store))
	h.report(t, buildreport.ServiceSidecar, 102, hashOf(t, theFresh.sidecar))
	d, err := New(Deps{
		Live:        live,
		StagingRoot: filepath.Join(t.TempDir(), "staging"),
		Builder:     h.builder,
		Bundle:      buildid.NewShimBundle(live.ShimMain(), ""),
		DaemonBuild: hashOf(t, theFresh.daemon),
		Rollout:     h.rollout,
		Workspaces: func(context.Context) ([]ids.WorkspaceID, error) {
			return []ids.WorkspaceID{"ws-a", "ws-b"}, nil
		},
		Clients:   h.clients,
		Services:  h.services,
		ReportDir: h.reportDir,
		Alive: func(pid int) bool {
			return h.alive[pid]
		},
		Clock:    newStepClock(),
		Progress: h.progress,
		Faults:   h.faults,
		Log:      h.log,
		StateLayout: func(_ context.Context, bin string) (int, error) {
			h.layoutAsked = append(h.layoutAsked, bin)
			return h.freshLayout, h.layoutErr
		},
		RunningLayout: runningLayout,
	})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	h.d = d
	return h
}

// report writes a service's build report and marks its pid alive.
func (h *harness) report(t *testing.T, service string, pid int, build string) {
	t.Helper()
	if err := buildreport.Write(h.reportDir, service, buildreport.Report{PID: pid, Build: build}); err != nil {
		t.Fatal(err)
	}
	h.alive[pid] = true
}
