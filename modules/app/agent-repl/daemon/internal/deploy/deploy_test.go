package deploy

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"testing"

	"agentrepl/logging/buildreport"

	"claude-repld/internal/gitclient"
	"claude-repld/internal/ids"
	"claude-repld/internal/rollout"
)

// outcome answers one component's decision out of a result.
func outcome(t *testing.T, r Result, c Component) Outcome {
	t.Helper()
	for _, o := range r.Outcomes {
		if o.Component == c {
			return o
		}
	}
	t.Fatalf("result %+v has no %s outcome", r, c)
	return Outcome{}
}

func TestABuildFailureDeploysNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.builder.fail = &BuildFailed{Step: "webapp", Detail: "tsc: 3 errors", Log: "/logs/build.log"}
	h.rollout.stale["ws-a"] = true
	h.clients.emacs = []EmacsClient{{ID: "emacs-1", Build: "older"}}

	// Act
	_, err := h.d.Deploy(context.Background(), false)

	// Assert
	var failed *BuildFailed
	if !errors.As(err, &failed) || failed.Step != "webapp" {
		t.Fatalf("Deploy = %v, want the webapp BuildFailed", err)
	}
	if got := readFile(t, h.live.ShimMain()); got != theOld.shim {
		t.Fatalf("installed shim = %q, want the old build untouched", got)
	}
	if calls := h.services.Calls(); len(calls) != 0 {
		t.Fatalf("services restarted = %v, want none", calls)
	}
	if len(h.rollout.handovers) != 0 || len(h.rollout.checks) != 0 || len(h.clients.elispPushes) != 0 {
		t.Fatalf("a failed build acted: handovers %v, checks %v, elisp %v", h.rollout.handovers, h.rollout.checks, h.clients.elispPushes)
	}
	if !logged(h.log, "error", opDeploy, "NOTHING WAS DEPLOYED") {
		t.Fatalf("records = %+v, want the failure at ERROR", h.log.Records())
	}
}

func TestADeployInstallsOnlyWhatDiffers(t *testing.T) {
	// Arrange: the store is already the fresh build on disk.
	h := newHarness(t)
	writeFile(t, h.live.CacheBinPath(buildreport.ServiceStore), theFresh.store)
	storeBefore, err := os.Stat(h.live.CacheBinPath(buildreport.ServiceStore))
	if err != nil {
		t.Fatal(err)
	}

	// Act
	if _, err := h.d.Deploy(context.Background(), false); err != nil {
		t.Fatalf("Deploy: %v", err)
	}

	// Assert
	for path, want := range map[string]string{
		h.live.ShimMain():  theFresh.shim,
		h.live.DaemonBin(): theFresh.daemon,
		h.live.CacheBinPath(buildreport.ServiceSidecar):              theFresh.sidecar,
		h.live.CacheBinPath("shim-lock"):                             theFresh.lock,
		filepath.Join(filepath.Dir(h.live.ShimMain()), ".built-sha"): "sha-" + theFresh.shim,
	} {
		if got := readFile(t, path); got != want {
			t.Fatalf("%s = %q, want %q", path, got, want)
		}
	}
	web, err := os.ReadFile(filepath.Join(h.live.WebappDist(), "assets", "index-Web2.js"))
	if err != nil || string(web) != "bundle Web2" {
		t.Fatalf("webapp entry = %q (%v), want the fresh tree installed", web, err)
	}
	storeAfter, err := os.Stat(h.live.CacheBinPath(buildreport.ServiceStore))
	if err != nil {
		t.Fatal(err)
	}
	if !os.SameFile(storeBefore, storeAfter) {
		t.Fatalf("the store binary was reinstalled although its content was already the fresh build")
	}
}

func TestServiceStalenessIsTheReportedBuildAgainstTheFreshOne(t *testing.T) {
	tests := []struct {
		name      string
		arrange   func(t *testing.T, h *harness)
		wantCalls []string
		wantStore OutcomeKind
		wantSide  OutcomeKind
		wantError string
	}{
		{
			name:      "both report the fresh build: nothing restarts",
			arrange:   func(*testing.T, *harness) {},
			wantStore: UpToDate, wantSide: UpToDate,
		},
		{
			name: "the store reports an older build: the pair restarts in the safe order",
			arrange: func(t *testing.T, h *harness) {
				h.report(t, buildreport.ServiceStore, 101, hashOf(t, theOld.store))
			},
			wantCalls: []string{"store"}, wantStore: Restarted, wantSide: Restarted,
		},
		{
			name: "only the sidecar is older: the sidecar alone restarts",
			arrange: func(t *testing.T, h *harness) {
				h.report(t, buildreport.ServiceSidecar, 102, hashOf(t, theOld.sidecar))
			},
			wantCalls: []string{"sidecar"}, wantStore: UpToDate, wantSide: Restarted,
		},
		{
			name: "a store that reports nothing is restarted",
			arrange: func(t *testing.T, h *harness) {
				if err := os.Remove(buildreport.Path(h.reportDir, buildreport.ServiceStore)); err != nil {
					t.Fatal(err)
				}
			},
			wantCalls: []string{"store"}, wantStore: Restarted, wantSide: Restarted,
		},
		{
			name: "a report whose process is gone is restarted",
			arrange: func(t *testing.T, h *harness) {
				h.alive[101] = false
			},
			wantCalls: []string{"store"}, wantStore: Restarted, wantSide: Restarted,
		},
		{
			name: "an unreadable report is restarted, loudly",
			arrange: func(t *testing.T, h *harness) {
				writeFile(t, buildreport.Path(h.reportDir, buildreport.ServiceSidecar), "{not json")
			},
			wantCalls: []string{"sidecar"}, wantStore: UpToDate, wantSide: Restarted,
			wantError: "unreadable",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			tc.arrange(t, h)

			// Act
			result, err := h.d.Deploy(context.Background(), false)

			// Assert
			if err != nil {
				t.Fatalf("Deploy: %v", err)
			}
			if got := h.services.Calls(); len(got) != len(tc.wantCalls) || (len(got) > 0 && got[0] != tc.wantCalls[0]) {
				t.Fatalf("restarts = %v, want %v", got, tc.wantCalls)
			}
			if got := outcome(t, result, ComponentStore).Kind; got != tc.wantStore {
				t.Fatalf("store = %s, want %s", got, tc.wantStore)
			}
			if got := outcome(t, result, ComponentSidecar).Kind; got != tc.wantSide {
				t.Fatalf("sidecar = %s, want %s", got, tc.wantSide)
			}
			if tc.wantError != "" && !logged(h.log, "error", opDecide, tc.wantError) {
				t.Fatalf("records = %+v, want an ERROR naming %q", h.log.Records(), tc.wantError)
			}
		})
	}
}

func TestAFailedServiceRestartIsTheDeploysAnswer(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.report(t, buildreport.ServiceStore, 101, hashOf(t, theOld.store))
	h.services.storeErr = errors.New("the store died before its socket appeared")

	// Act
	_, err := h.d.Deploy(context.Background(), false)

	// Assert
	var failed *ServiceRestartFailed
	if !errors.As(err, &failed) || failed.Component != ComponentStore {
		t.Fatalf("Deploy = %v, want the store's ServiceRestartFailed", err)
	}
	if len(h.rollout.handovers) != 0 || len(h.rollout.checks) != 0 {
		t.Fatalf("the deploy went on to the daemon and the shims after the store failed")
	}
	if !logged(h.log, "error", opDecide, "the store restart failed") {
		t.Fatalf("records = %+v, want the restart failure at ERROR", h.log.Records())
	}
}

func TestElispIsPushedOnlyToTheEmacsOnOlderElisp(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.clients.emacs = []EmacsClient{{ID: "current", Build: h.elisp}, {ID: "stale", Build: "older"}}

	// Act
	result, err := h.d.Deploy(context.Background(), false)

	// Assert
	if err != nil {
		t.Fatalf("Deploy: %v", err)
	}
	got := outcome(t, result, ComponentElisp)
	if got.Kind != ReloadPushed || got.Recipients != 1 {
		t.Fatalf("elisp = %+v, want one reload pushed", got)
	}
	if len(h.clients.elispPushes) != 1 || len(h.clients.elispPushes[0]) != 1 || h.clients.elispPushes[0][0] != "stale" {
		t.Fatalf("pushes = %v, want the stale stream alone", h.clients.elispPushes)
	}
	if h.clients.elispRoot != h.live.ModuleRoot || h.clients.elispBuild != h.elisp {
		t.Fatalf("pushed root %q build %q, want the checkout's", h.clients.elispRoot, h.clients.elispBuild)
	}
}

func TestElispIsUpToDateWhenEveryEmacsRunsIt(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.clients.emacs = []EmacsClient{{ID: "current", Build: h.elisp}}

	// Act
	result, err := h.d.Deploy(context.Background(), false)

	// Assert
	if err != nil {
		t.Fatalf("Deploy: %v", err)
	}
	if got := outcome(t, result, ComponentElisp); got.Kind != UpToDate || len(h.clients.elispPushes) != 0 {
		t.Fatalf("elisp = %+v, pushes %v; want up to date", got, h.clients.elispPushes)
	}
}

func TestAStaleDaemonIsHandedOverAndItsSuccessorTakesTheRest(t *testing.T) {
	tests := []struct {
		name  string
		force bool
	}{
		{name: "an unforced deploy hands over at freeness"},
		{name: "a forced deploy hands over at once", force: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: this daemon runs the OLD build.
			h := newHarness(t)
			h.d.deps.DaemonBuild = hashOf(t, theOld.daemon)
			h.rollout.acceptance = rollout.HandoverAcceptance{Workspaces: 2, Busy: 1}

			// Act
			result, err := h.d.Deploy(context.Background(), tc.force)

			// Assert
			if err != nil {
				t.Fatalf("Deploy: %v", err)
			}
			if len(h.rollout.handovers) != 1 || h.rollout.handovers[0] != tc.force {
				t.Fatalf("handovers = %v, want one with force=%v", h.rollout.handovers, tc.force)
			}
			daemon := outcome(t, result, ComponentDaemon)
			if daemon.Kind != HandingOver || daemon.Handover.Workspaces != 2 || daemon.Handover.Forced != tc.force {
				t.Fatalf("daemon = %+v, want the handover's acceptance", daemon)
			}
			for _, c := range []Component{ComponentShim, ComponentWebapp} {
				if got := outcome(t, result, c).Kind; got != DeferredToSuccessor {
					t.Fatalf("%s = %s, want deferred to the successor", c, got)
				}
			}
			if len(h.rollout.checks) != 0 || len(h.clients.webappPushes) != 0 {
				t.Fatalf("the incumbent judged shims or pushed reloads its successor owns")
			}
		})
	}
}

func TestARefusedHandoverIsTheDeploysAnswer(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.d.deps.DaemonBuild = hashOf(t, theOld.daemon)
	h.rollout.handErr = &rollout.ErrAlreadyRollingOut{WaitingOn: []ids.WorkspaceID{"ws-a"}}

	// Act
	_, err := h.d.Deploy(context.Background(), false)

	// Assert
	var inFlight *rollout.ErrAlreadyRollingOut
	if !errors.As(err, &inFlight) {
		t.Fatalf("Deploy = %v, want the handover's refusal", err)
	}
}

func TestStaleShimsGoToTheBounceRegistry(t *testing.T) {
	tests := []struct {
		name  string
		force bool
	}{
		{name: "unforced"},
		{name: "forced", force: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.rollout.stale["ws-b"] = true

			// Act
			result, err := h.d.Deploy(context.Background(), tc.force)

			// Assert
			if err != nil {
				t.Fatalf("Deploy: %v", err)
			}
			shims := outcome(t, result, ComponentShim)
			if shims.Kind != ShimsBouncing || len(shims.Shims) != 1 || shims.Shims[0].Workspace != "ws-b" {
				t.Fatalf("shims = %+v, want ws-b bouncing", shims)
			}
			if shims.Shims[0].Decision.Forced != tc.force {
				t.Fatalf("decision = %+v, want forced=%v passed through", shims.Shims[0].Decision, tc.force)
			}
			for _, forced := range h.rollout.checks {
				if forced != tc.force {
					t.Fatalf("checks = %v, want every one forced=%v", h.rollout.checks, tc.force)
				}
			}
		})
	}
}

func TestAShimThatCannotBeJudgedDoesNotStopTheOthers(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.rollout.checkErr["ws-a"] = errors.New("the installed bundle is unreadable")
	h.rollout.stale["ws-b"] = true

	// Act
	result, err := h.d.Deploy(context.Background(), false)

	// Assert
	if err != nil {
		t.Fatalf("Deploy: %v", err)
	}
	if shims := outcome(t, result, ComponentShim); len(shims.Shims) != 1 {
		t.Fatalf("shims = %+v, want ws-b still bounced", shims)
	}
	if !logged(h.log, "error", opDecide, "a workspace's shim could not be judged") {
		t.Fatalf("records = %+v, want the unjudged workspace at ERROR", h.log.Records())
	}
}

func TestTheWebappReloadGoesOnlyToWebviewsOnAnOlderBuild(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.clients.webviews["ws-a"] = []string{"Web2"}
	h.clients.webviews["ws-b"] = []string{"Web2", "Web1"}

	// Act
	result, err := h.d.Deploy(context.Background(), false)

	// Assert
	if err != nil {
		t.Fatalf("Deploy: %v", err)
	}
	if got := outcome(t, result, ComponentWebapp); got.Kind != ReloadPushed || got.Recipients != 1 {
		t.Fatalf("webapp = %+v, want one reload", got)
	}
	if len(h.clients.webappPushes) != 1 || h.clients.webappPushes[0] != "ws-b" {
		t.Fatalf("pushes = %v, want ws-b alone", h.clients.webappPushes)
	}
}

func TestADeployIsRefused(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
		want    func(error) bool
	}{
		{
			name:    "by a successor still joining",
			arrange: func(h *harness) { h.rollout.joining = true },
			want:    func(err error) bool { return errors.Is(err, rollout.ErrJoining) },
		},
		{
			name:    "while a handover is in flight",
			arrange: func(h *harness) { h.rollout.rolling = []ids.WorkspaceID{"ws-a"} },
			want: func(err error) bool {
				var inFlight *rollout.ErrAlreadyRollingOut
				return errors.As(err, &inFlight) && len(inFlight.WaitingOn) == 1
			},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			tc.arrange(h)

			// Act
			_, err := h.d.Deploy(context.Background(), false)

			// Assert
			if !tc.want(err) {
				t.Fatalf("Deploy = %v", err)
			}
			if h.builder.count() != 0 {
				t.Fatalf("a refused deploy built")
			}
		})
	}
}

func TestASecondDeployIsRefusedWhileOneRuns(t *testing.T) {
	// Arrange: the first deploy is held inside its build.
	h := newHarness(t)
	h.builder.started = make(chan struct{}, 1)
	h.builder.gate = make(chan struct{})
	first := make(chan error, 1)
	go func() {
		_, err := h.d.Deploy(context.Background(), false)
		first <- err
	}()
	<-h.builder.started

	// Act
	_, err := h.d.Deploy(context.Background(), false)

	// Assert
	if !errors.Is(err, ErrAlreadyDeploying) {
		t.Fatalf("second Deploy = %v, want ErrAlreadyDeploying", err)
	}
	close(h.builder.gate)
	if err := <-first; err != nil {
		t.Fatalf("first Deploy: %v", err)
	}
}

func commitsOf(n int) []gitclient.Commit {
	out := make([]gitclient.Commit, n)
	for i := range out {
		out[i] = gitclient.Commit{SHA: string(rune('a' + i))}
	}
	return out
}

func TestALandingDeploysOnceWhateverItsCommits(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act: one merge that landed five commits.
	h.d.Landed(context.Background(), commitsOf(5))
	h.d.WaitLandings()

	// Assert
	if got := h.builder.count(); got != 1 {
		t.Fatalf("builds = %d, want exactly one deploy for the landing", got)
	}
}

func TestLandingsDuringADeployGetOneFollowUp(t *testing.T) {
	// Arrange: a deploy is running, held in its build.
	h := newHarness(t)
	h.builder.started = make(chan struct{}, 4)
	h.builder.gate = make(chan struct{})
	first := make(chan error, 1)
	go func() {
		_, err := h.d.Deploy(context.Background(), false)
		first <- err
	}()
	<-h.builder.started

	// Act: two merges land while it runs.
	h.d.Landed(context.Background(), commitsOf(1))
	h.d.Landed(context.Background(), commitsOf(3))
	close(h.builder.gate)
	if err := <-first; err != nil {
		t.Fatalf("first Deploy: %v", err)
	}
	h.d.WaitLandings()

	// Assert
	if got := h.builder.count(); got != 2 {
		t.Fatalf("builds = %d, want the running deploy and ONE follow-up for both landings", got)
	}
	if !logged(h.log, "info", opLanding, "one deploy follows it") {
		t.Fatalf("records = %+v, want the deferred landing recorded", h.log.Records())
	}
}

func TestNewRefusesAMissingCollaborator(t *testing.T) {
	// Arrange
	h := newHarness(t)
	deps := h.d.deps
	deps.Builder = nil

	// Act
	_, err := New(deps)

	// Assert
	if err == nil {
		t.Fatalf("New accepted a Deployer with no builder")
	}
}
