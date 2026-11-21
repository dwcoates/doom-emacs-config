package deploy

import (
	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"context"
	"fmt"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"sync"
	"testing"
	"time"

	"agentrepl/logging/buildreport"

	"agent-repld/internal/bounce"
	"agent-repld/internal/buildid"
	"agent-repld/internal/deployprogress"
	"agent-repld/internal/dlog"
	"agent-repld/internal/holderensure"
	"agent-repld/internal/ids"
	"agent-repld/internal/restarttiming"
	"agent-repld/internal/restarttiming/restarttimingtest"
	"agent-repld/internal/rollout"
	"agent-repld/internal/wsm"
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

// boundClock is a clock whose failure bounds never elapse unless a test fires
// them: a wait under it ends on its event, or on a bound the test fired.
type boundClock struct {
	mu    sync.Mutex
	fired map[time.Duration]bool
}

func newBoundClock() *boundClock { return &boundClock{fired: map[time.Duration]bool{}} }

func (c *boundClock) Now() time.Time { return instant }

// After answers a channel that fires at once for a fired bound, and never
// for any other.
func (c *boundClock) After(d time.Duration) <-chan time.Time {
	c.mu.Lock()
	defer c.mu.Unlock()
	if !c.fired[d] {
		return nil
	}
	ch := make(chan time.Time, 1)
	ch <- instant.Add(d)
	return ch
}

// fire makes every wait on the bound d elapse at once.
func (c *boundClock) fire(d time.Duration) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.fired[d] = true
}

// records filters a test logger's records by operation.
func records(log *dlog.TestSurfaces, operation string) []dlog.Record {
	return dlog.FilterRecords(log.Records(), dlog.RecordQuery{Operation: operation})
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
	shim, webappEntry, daemon, store, sidecar, zettel, tests, lock, holder string
	// wifi is the Wi-Fi helper app's source hash; empty stages and installs
	// no helper.
	wifi string
	// launcher is the launcher's executable, staged and installed only when
	// set (a macOS deploy, newLauncherHarness).
	launcher string
}

// writeWifiApp writes a Wi-Fi helper app at APP built from the sources HASH
// names, with a signature file whose bytes say which build it is.
func writeWifiApp(t *testing.T, app, hash string) {
	t.Helper()
	writeFile(t, filepath.Join(app, "Contents", "Resources", "source-sha256"), hash+"\n")
	writeFile(t, filepath.Join(app, "Contents", "_CodeSignature", "CodeResources"), "signed "+hash)
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
	writeFile(t, filepath.Join(filepath.Dir(s.CacheBin(buildreport.ServiceStore)), ".shim-store.source-tree"), "tree-"+a.store)
	writeFile(t, s.CacheBin(buildreport.ServiceSidecar), a.sidecar)
	writeFile(t, s.CacheBin(buildreport.ServiceZettel), a.zettel)
	writeFile(t, s.CacheBin(buildreport.ServiceTests), a.tests)
	writeFile(t, s.CacheBin("shim-lock"), a.lock)
	writeFile(t, s.CacheBin(HolderBinary), a.holder)
	if a.wifi != "" {
		writeWifiApp(t, s.WifiApp(), a.wifi)
	}
	if a.launcher != "" {
		writeFile(t, filepath.Join(s.CacheBin(LauncherBundle), LauncherExecutable), a.launcher)
		writeFile(t, filepath.Join(s.CacheBin(LauncherBundle), "Contents", "Info.plist"), "plist "+a.launcher)
	}
}

// installInto writes the same artifacts at their live locations.
func (a artifacts) installInto(t *testing.T, live Live) {
	t.Helper()
	writeFile(t, live.ShimMain(), a.shim)
	writeFile(t, filepath.Join(live.WebappDist(), "index.html"), `<script type="module" src="/assets/index-`+a.webappEntry+`.js"></script>`)
	writeFile(t, live.DaemonBin(), a.daemon)
	writeFile(t, live.CacheBinPath(buildreport.ServiceStore), a.store)
	writeFile(t, live.CacheBinPath(buildreport.ServiceSidecar), a.sidecar)
	writeFile(t, live.CacheBinPath(buildreport.ServiceZettel), a.zettel)
	writeFile(t, live.CacheBinPath(buildreport.ServiceTests), a.tests)
	writeFile(t, live.CacheBinPath("shim-lock"), a.lock)
	writeFile(t, live.CacheBinPath(HolderBinary), a.holder)
	if live.WifiApp != "" && a.wifi != "" {
		writeWifiApp(t, live.WifiApp, a.wifi)
	}
	if a.launcher != "" {
		writeFile(t, filepath.Join(live.LauncherApp(), LauncherExecutable), a.launcher)
	}
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
	mu      sync.Mutex
	joining bool
	// tookOver is the takeover latch; takeOver closes it.
	tookOver chan struct{}
	// tookOverAsked, when set, closes on the first TookOver call, so a test
	// knows a landing's deploy is waiting on the takeover.
	tookOverAsked chan struct{}
	rolling       []ids.WorkspaceID
	handovers     []bool
	// asks is every handover's and restart's ask, in order.
	asks    []rollout.Ask
	handErr error
	// onHandOver runs inside the handover, before it answers.
	onHandOver func()
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

func (r *fakeRollout) HandOver(_ context.Context, ask rollout.Ask) (rollout.HandoverAcceptance, error) {
	force := ask.Force
	r.mu.Lock()
	defer r.mu.Unlock()
	r.handovers = append(r.handovers, force)
	r.asks = append(r.asks, ask)
	if r.onHandOver != nil {
		r.onHandOver()
	}
	acc := r.acceptance
	acc.Forced = force
	return acc, r.handErr
}

func (r *fakeRollout) Restart(_ context.Context, ask rollout.Ask) (rollout.HandoverAcceptance, error) {
	force := ask.Force
	r.mu.Lock()
	defer r.mu.Unlock()
	r.restarts = append(r.restarts, force)
	r.asks = append(r.asks, ask)
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

// TookOver closes once takeOver ends the joining.
func (r *fakeRollout) TookOver() <-chan struct{} {
	r.mu.Lock()
	defer r.mu.Unlock()
	if r.tookOver == nil {
		r.tookOver = make(chan struct{})
	}
	if r.tookOverAsked != nil {
		close(r.tookOverAsked)
		r.tookOverAsked = nil
	}
	return r.tookOver
}

// takeOver ends the joining and closes the takeover latch, as the rollout's
// becomeIncumbent does.
func (r *fakeRollout) takeOver() {
	r.mu.Lock()
	r.joining = false
	if r.tookOver == nil {
		r.tookOver = make(chan struct{})
	}
	ch := r.tookOver
	r.mu.Unlock()
	close(ch)
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
	webappBuilds []string
	webhosts     []*agentreplv1.WebhostPage
	webhostPush  []string
	// webhostReach is how many Emacs streams a host reload reaches.
	webhostReach int
}

// hostPages are standing web view host page streams on BUILDS, stating no
// reload generation.
func hostPages(builds ...string) []*agentreplv1.WebhostPage {
	var out []*agentreplv1.WebhostPage
	for _, build := range builds {
		out = append(out, &agentreplv1.WebhostPage{WebappBuild: build})
	}
	return out
}

// hostPageAt is a standing web view host page stream on BUILD at reload
// GENERATION.
func hostPageAt(build string, generation uint32) *agentreplv1.WebhostPage {
	return &agentreplv1.WebhostPage{WebappBuild: build, ReloadGeneration: &generation}
}

func (c *fakeClients) WebhostPages() []*agentreplv1.WebhostPage {
	c.mu.Lock()
	defer c.mu.Unlock()
	return append([]*agentreplv1.WebhostPage(nil), c.webhosts...)
}

func (c *fakeClients) PushReloadWebhost(build string) int {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.webhostPush = append(c.webhostPush, build)
	return c.webhostReach
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

func (c *fakeClients) PushReloadWebappBuild(ws ids.WorkspaceID, build string) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.webappPushes = append(c.webappPushes, ws)
	c.webappBuilds = append(c.webappBuilds, build)
}

// fakeServices records the restarts.
type fakeServices struct {
	mu         sync.Mutex
	calls      []string
	storeErr   error
	sidecarErr error
	// standaloneErr fails a standalone service's restart, by component.
	standaloneErr map[Component]error
	launcherErr   error
	// recovers makes a failing restart fail ONCE: every later restart of
	// that service succeeds.
	recovers bool
	// holderErr fails every socket holder's restart.
	holderErr error
	// atHolder, when set, runs at each holder's restart: what the deploy had
	// done by then.
	atHolder func()
}

func (s *fakeServices) RestartStore(context.Context) error {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.calls = append(s.calls, "store")
	err := s.storeErr
	if s.recovers {
		s.storeErr = nil
	}
	return err
}

func (s *fakeServices) RestartSidecar(context.Context) error {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.calls = append(s.calls, "sidecar")
	err := s.sidecarErr
	if s.recovers {
		s.sidecarErr = nil
	}
	return err
}

func (s *fakeServices) RestartStandalone(_ context.Context, sv Standalone) error {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.calls = append(s.calls, string(sv.Component))
	err := s.standaloneErr[sv.Component]
	if s.recovers {
		delete(s.standaloneErr, sv.Component)
	}
	return err
}

func (s *fakeServices) RestartHolder(_ context.Context, h holderensure.Holder) error {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.calls = append(s.calls, "holder "+h.ID)
	if s.atHolder != nil {
		s.atHolder()
	}
	return s.holderErr
}

func (s *fakeServices) Calls() []string {
	s.mu.Lock()
	defer s.mu.Unlock()
	return append([]string(nil), s.calls...)
}

// harness is one Deployer over a temp checkout, a temp cache-bin, and fakes.
type harness struct {
	foldSweep *fakeFoldSweep
	hotReload *fakeHotReload
	restarts  *restarttimingtest.Recorder
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
	// migrationKind is what the staged daemon says its steps are (breaking
	// unless a test says otherwise), migrationErr fails the question, and
	// migrationAsked records the running layout each question named.
	migrationKind  wsm.MigrationKind
	migrationErr   error
	migrationAsked []int
	progress       *fakeProgress
	faults         *fakeFaults
	reports        *fakeReports
	// deployed counts the Deployed hook's calls.
	deployed int
	// wifiAsks answers StopWifiAsks.
	wifiAsks *fakeWifiAsks
}

// fakeWifiAsks answers the deploy's StopWifiAsks with stopped and err after
// moving the deploy's clock by took, and counts its calls.
type fakeWifiAsks struct {
	calls   int
	stopped bool
	err     error
	took    time.Duration
	clock   *stepClock
}

func (f *fakeWifiAsks) stop(context.Context) (bool, error) {
	f.calls++
	f.clock.mu.Lock()
	f.clock.now = f.clock.now.Add(f.took)
	f.clock.mu.Unlock()
	return f.stopped, f.err
}

// fakeReports records every cover and payment of the owed landing reports, in
// order, with how many builds had started at each.
type fakeReports struct {
	mu      sync.Mutex
	builder *fakeBuilder
	calls   []string
	// buildsAtCover is how many builds had started when the reports were
	// covered.
	buildsAtCover int
}

func (r *fakeReports) CoverOwed(context.Context) error {
	r.builder.mu.Lock()
	builds := r.builder.builds
	r.builder.mu.Unlock()
	r.mu.Lock()
	defer r.mu.Unlock()
	r.calls = append(r.calls, "cover")
	r.buildsAtCover = builds
	return nil
}

func (r *fakeReports) PayOwed(_ context.Context, coveredOnly bool, deployErr error) {
	r.mu.Lock()
	defer r.mu.Unlock()
	call := fmt.Sprintf("pay covered_only=%v", coveredOnly)
	if deployErr != nil {
		call += " failed"
	}
	r.calls = append(r.calls, call)
}

// made answers every call, in order.
func (r *fakeReports) made() []string {
	r.mu.Lock()
	defer r.mu.Unlock()
	return append([]string(nil), r.calls...)
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

// fakeHotReload records every report the deploy tells the hot reload, as
// "event" or "event:argument", in order.
// fakeFoldSweep records when the sweep ran, into the hot reload's event
// order, and how many builds had started by then.
type fakeFoldSweep struct {
	hotReload *fakeHotReload
	// sawBegin is whether the hot reload had been told it began.
	sawBegin bool
	builder  *fakeBuilder
	err      error
	// buildsAtSweep is the builder's count when the sweep ran.
	buildsAtSweep int
	sweeps        int
}

func (f *fakeFoldSweep) Sweep(context.Context) error {
	f.sweeps++
	f.sawBegin = slices.Contains(f.hotReload.told(), "begin")
	f.builder.mu.Lock()
	f.buildsAtSweep = f.builder.builds
	f.builder.mu.Unlock()
	return f.err
}

type fakeHotReload struct {
	mu     sync.Mutex
	events []string
}

func (f *fakeHotReload) record(event string) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.events = append(f.events, event)
}

// told answers every recorded event, in order.
func (f *fakeHotReload) told() []string {
	f.mu.Lock()
	defer f.mu.Unlock()
	return append([]string(nil), f.events...)
}

func (f *fakeHotReload) Begin(context.Context, time.Time) { f.record("begin") }
func (f *fakeHotReload) Built(fresh map[string]string)    { f.record(fmt.Sprintf("built:%d", len(fresh))) }
func (f *fakeHotReload) InstallBegan()                    { f.record("install_began") }
func (f *fakeHotReload) InstallEnded()                    { f.record("install_ended") }
func (f *fakeHotReload) ServiceRestarting(service string) { f.record("restarting:" + service) }
func (f *fakeHotReload) ServiceRestarted(service string)  { f.record("restarted:" + service) }
func (f *fakeHotReload) ServiceCurrent(service string)    { f.record("current:" + service) }
func (f *fakeHotReload) DaemonCurrent()                   { f.record("daemon_current") }
func (f *fakeHotReload) Handoff() []byte                  { f.record("handoff"); return []byte("card") }
func (f *fakeHotReload) HandoverFailed(string)            { f.record("handover_failed") }
func (f *fakeHotReload) ShimsJudging()                    { f.record("shims_judging") }
func (f *fakeHotReload) ShimsJudged()                     { f.record("shims_judged") }
func (f *fakeHotReload) WebhostReloading()                { f.record("webhost_reloading") }
func (f *fakeHotReload) WebappJudged()                    { f.record("webapp_judged") }
func (f *fakeHotReload) ElispPushed(recipients int) {
	f.record(fmt.Sprintf("elisp_pushed:%d", recipients))
}
func (f *fakeHotReload) ElispCurrent() { f.record("elisp_current") }
func (f *fakeHotReload) Failed(error)  { f.record("failed") }
func (f *fakeHotReload) ArtifactInstalled(service string, primary bool, _ string, replaced bool) {
	f.record(fmt.Sprintf("installed:%s:primary=%t:replaced=%t", service, primary, replaced))
}
func (f *fakeHotReload) DaemonMoving(_ context.Context, words string, stale []ids.WorkspaceID) {
	f.record(fmt.Sprintf("daemon_moving:%s:%v", words, stale))
}
func (f *fakeHotReload) WebviewReloading(_ context.Context, ws ids.WorkspaceID) {
	f.record("webview_reloading:" + string(ws))
}

// runningLayout is the state layout the harness's running daemon writes.
const runningLayout = 11

// theFresh is the build every harness stages; theOld is what is installed and
// running before it.
var (
	theFresh = artifacts{shim: "shim-2", webappEntry: "Web2", daemon: "daemon-2", store: "store-2", sidecar: "sidecar-2", zettel: "zettel-2", tests: "tests-2", lock: "lock-2", holder: "holder-2", wifi: "wifi-2"}
	theOld   = artifacts{shim: "shim-1", webappEntry: "Web1", daemon: "daemon-1", store: "store-1", sidecar: "sidecar-1", zettel: "zettel-1", tests: "tests-1", lock: "lock-1", holder: "holder-1", wifi: "wifi-1"}
)

// theHolders are the harness's declared socket holders.
var theHolders = []holderensure.Holder{
	{ID: "socket-holder-daemon", Label: "com.agentrepl.sockethold.daemon", Service: "sockethold-daemon"},
	{ID: "socket-holder-store", Label: "com.agentrepl.sockethold.store", Service: "sockethold-store"},
}

// withWifiHelper is a harness on the platform with a Wi-Fi helper: its live
// layout names the app (Live.WifiApp), and the old build installed one.
func withWifiHelper(t *testing.T) func(*Live) {
	return func(l *Live) { l.WifiApp = filepath.Join(t.TempDir(), "AgentReplWifi.app") }
}

func newHarness(t *testing.T, opts ...func(*Live)) *harness {
	t.Helper()
	root := t.TempDir()
	writeFile(t, filepath.Join(root, "config.el"), "(agent-repl--load-module \"core\")\n")
	writeFile(t, filepath.Join(root, "lisp", "core.el"), "(provide 'core)\n")
	elisp, err := buildid.Elisp(root)
	if err != nil {
		t.Fatal(err)
	}
	live := Live{ModuleRoot: root, CacheBin: filepath.Join(t.TempDir(), "bin")}
	for _, opt := range opts {
		opt(&live)
	}
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

		freshLayout:   runningLayout,
		migrationKind: wsm.MigrationBreaking,
		progress:      &fakeProgress{},
		faults:        newFakeFaults(),
		restarts:      &restarttimingtest.Recorder{},
		wifiAsks:      &fakeWifiAsks{clock: newStepClock()},
		hotReload:     &fakeHotReload{},
	}
	h.foldSweep = &fakeFoldSweep{hotReload: h.hotReload, builder: h.builder}
	h.reports = &fakeReports{builder: h.builder}
	// Both services run the FRESH build unless a test says otherwise.
	h.report(t, buildreport.ServiceStore, 101, hashOf(t, theFresh.store))
	h.report(t, buildreport.ServiceSidecar, 102, hashOf(t, theFresh.sidecar))
	h.report(t, buildreport.ServiceZettel, 103, hashOf(t, theFresh.zettel))
	h.report(t, buildreport.ServiceTests, 104, hashOf(t, theFresh.tests))
	d, err := New(Deps{
		Live:        live,
		StagingRoot: filepath.Join(t.TempDir(), "staging"),
		Builder:     h.builder,
		Bundle:      buildid.NewShimBundle(live.ShimMain(), ""),
		DaemonBuild: hashOf(t, theFresh.daemon),
		Rollout:     h.rollout,
		Reports:     h.reports,
		Workspaces: func(context.Context) ([]ids.WorkspaceID, error) {
			return []ids.WorkspaceID{"ws-a", "ws-b"}, nil
		},
		Clients:      h.clients,
		Services:     h.services,
		Holders:      theHolders,
		StopWifiAsks: h.wifiAsks.stop,
		ReportDir:    h.reportDir,
		Alive: func(pid int) bool {
			return h.alive[pid]
		},
		Clock:    h.wifiAsks.clock,
		Progress: h.progress,
		Faults:   h.faults,
		Log:      h.log,
		StateLayout: func(_ context.Context, bin string) (int, error) {
			h.layoutAsked = append(h.layoutAsked, bin)
			return h.freshLayout, h.layoutErr
		},
		MigrationKind: func(_ context.Context, _ string, from int) (wsm.MigrationKind, error) {
			h.migrationAsked = append(h.migrationAsked, from)
			return h.migrationKind, h.migrationErr
		},
		RunningLayout: runningLayout,
		Deployed:      func() { h.deployed++ },
		Restarts:      h.restarts,
		Reconnects:    restarttiming.NewReconnects(h.restarts, restarttiming.NoWindows{}, h.wifiAsks.clock.Now),
		HotReload:     h.hotReload,
		FoldSweep:     h.foldSweep,
	})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	h.d = d
	// EVERY HOLDER RUNS THE FRESH BUILD unless a test says otherwise, as on a
	// machine no deploy has yet moved past its holders.
	for i, holder := range theHolders {
		h.report(t, holder.Service, 900+i, hashOf(t, theFresh.holder))
	}
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

func (s *fakeServices) RestartLauncher(context.Context) error {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.calls = append(s.calls, "launcher")
	err := s.launcherErr
	if s.recovers {
		s.launcherErr = nil
	}
	return err
}

// launcherPID is the pid a launcher harness's build report names.
const launcherPID = 104

// newLauncherHarness is a macOS deploy's harness: the launcher is built,
// installed in an Applications directory of the test's, and judged on its
// build report, which names RUNNING (no report when running is empty).
func newLauncherHarness(t *testing.T, running string) *harness {
	t.Helper()
	h := newHarness(t)
	deps := h.d.deps
	deps.Live.Applications = t.TempDir()
	deps.Launcher = true
	old, fresh := theOld, theFresh
	old.launcher, fresh.launcher = "launcher-1", "launcher-2"
	old.installInto(t, deps.Live)
	h.builder.build = fresh
	if running != "" {
		h.report(t, buildreport.ServiceLauncher, launcherPID, running)
	}
	d, err := New(deps)
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	h.d, h.live = d, deps.Live
	return h
}
