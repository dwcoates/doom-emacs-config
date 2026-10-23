package deploy

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
	"time"

	"agentrepl/logging/buildreport"
	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/deployprogress"
	"agent-repld/internal/dlog"
	"agent-repld/internal/gitclient"
	"agent-repld/internal/ids"
	"agent-repld/internal/restarttiming"
	"agent-repld/internal/rollout"
	"agent-repld/internal/wsm"
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

// TestAStaleDaemonAcrossALayoutChangeIsRestartedNotHandedOver is invariant B:
// a joining successor cannot carry an older state layout forward, so a fresh
// build that writes a different layout is rolled out stop-then-start. On
// 2026-09-27 it was handed over, and the successor died at boot.
func TestAStaleDaemonAcrossALayoutChangeIsRestartedNotHandedOver(t *testing.T) {
	tests := []struct {
		name  string
		force bool
	}{
		{name: "an unforced deploy restarts at freeness"},
		{name: "a forced deploy restarts at once", force: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.d.deps.DaemonBuild = hashOf(t, theOld.daemon)
			h.freshLayout = runningLayout + 1
			h.rollout.acceptance = rollout.HandoverAcceptance{Workspaces: 2, Busy: 1}

			// Act
			result, err := h.d.Deploy(context.Background(), tc.force)

			// Assert
			if err != nil {
				t.Fatalf("Deploy: %v", err)
			}
			if len(h.rollout.handovers) != 0 || len(h.rollout.restarts) != 1 || h.rollout.restarts[0] != tc.force {
				t.Fatalf("handovers %v, restarts %v: want one restart with force=%v and no handover", h.rollout.handovers, h.rollout.restarts, tc.force)
			}
			daemon := outcome(t, result, ComponentDaemon)
			if daemon.Kind != RestartingAcrossLayout || daemon.Layouts != (LayoutChange{Running: runningLayout, Fresh: runningLayout + 1}) {
				t.Fatalf("daemon = %+v, want the restart naming both layouts", daemon)
			}
			for _, c := range []Component{ComponentShim, ComponentWebapp} {
				if got := outcome(t, result, c).Kind; got != DeferredToSuccessor {
					t.Fatalf("%s = %s, want deferred to the replacement", c, got)
				}
			}
		})
	}
}

// AN ADDITIVE LAYOUT CHANGE IS HANDED OVER (owner ruling, 2026-09-30): the
// joining successor applies the additive steps while the incumbent serves, so
// a schema bump no longer takes every workspace through a restart.
func TestAnAdditiveLayoutChangeIsHandedOver(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.d.deps.DaemonBuild = hashOf(t, theOld.daemon)
	h.freshLayout = runningLayout + 1
	h.migrationKind = wsm.MigrationAdditive

	// Act
	result, err := h.d.Deploy(context.Background(), false)

	// Assert
	if err != nil {
		t.Fatalf("Deploy: %v", err)
	}
	if len(h.rollout.handovers) != 1 || len(h.rollout.restarts) != 0 {
		t.Fatalf("handovers %v, restarts %v: want one handover and no restart", h.rollout.handovers, h.rollout.restarts)
	}
	if got := outcome(t, result, ComponentDaemon).Kind; got != HandingOver {
		t.Fatalf("daemon = %s, want handing over", got)
	}
	if len(h.migrationAsked) != 1 || h.migrationAsked[0] != runningLayout {
		t.Fatalf("migration kind asked from %v, want the running layout", h.migrationAsked)
	}
}

func TestAnOlderFreshLayoutIsRestartedWithoutAskingWhatItsStepsAre(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.d.deps.DaemonBuild = hashOf(t, theOld.daemon)
	h.freshLayout = runningLayout - 1

	// Act
	if _, err := h.d.Deploy(context.Background(), false); err != nil {
		t.Fatalf("Deploy: %v", err)
	}

	// Assert
	if len(h.rollout.restarts) != 1 || len(h.migrationAsked) != 0 {
		t.Fatalf("restarts %v, migration asked %v: want a restart and no question", h.rollout.restarts, h.migrationAsked)
	}
}

func TestTheSameLayoutIsHandedOverWithoutAskingWhatItsStepsAre(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.d.deps.DaemonBuild = hashOf(t, theOld.daemon)

	// Act
	if _, err := h.d.Deploy(context.Background(), false); err != nil {
		t.Fatalf("Deploy: %v", err)
	}

	// Assert
	if len(h.rollout.handovers) != 1 || len(h.migrationAsked) != 0 {
		t.Fatalf("handovers %v, migration asked %v: want a handover and no question", h.rollout.handovers, h.migrationAsked)
	}
}

func TestADaemonWhoseMigrationKindCannotBeReadIsNeitherHandedOverNorRestarted(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.d.deps.DaemonBuild = hashOf(t, theOld.daemon)
	h.freshLayout = runningLayout + 1
	h.migrationErr = errors.New("exit status 2")

	// Act
	_, err := h.d.Deploy(context.Background(), false)

	// Assert
	if err == nil {
		t.Fatal("Deploy succeeded with the migration kind unknown")
	}
	if len(h.rollout.handovers) != 0 || len(h.rollout.restarts) != 0 {
		t.Fatalf("handovers %v, restarts %v: want neither on a guess", h.rollout.handovers, h.rollout.restarts)
	}
	if !logged(h.log, "error", opDecide, "the fresh daemon could not say whether its migrations are additive; the daemon is neither handed over nor restarted") {
		t.Fatalf("records = %+v, want the unread kind at ERROR", h.log.Records())
	}
}

func TestParseMigrationKind(t *testing.T) {
	tests := []struct {
		name    string
		answer  string
		want    wsm.MigrationKind
		wantErr bool
	}{
		{name: "additive", answer: "additive", want: wsm.MigrationAdditive},
		{name: "breaking", answer: "breaking", want: wsm.MigrationBreaking},
		{name: "unmarked names no kind", answer: "unmarked", wantErr: true},
		{name: "empty names no kind", answer: "", wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := parseMigrationKind("agent-repld", tt.answer)

			// Assert
			if (err != nil) != tt.wantErr || (!tt.wantErr && got != tt.want) {
				t.Fatalf("parseMigrationKind(%q) = (%v, %v), want (%v, err %v)", tt.answer, got, err, tt.want, tt.wantErr)
			}
		})
	}
}

func TestTheLayoutQuestionIsAskedOfTheStagedDaemon(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.d.deps.DaemonBuild = hashOf(t, theOld.daemon)

	// Act
	if _, err := h.d.Deploy(context.Background(), false); err != nil {
		t.Fatalf("Deploy: %v", err)
	}

	// Assert
	if len(h.layoutAsked) != 1 || !strings.HasSuffix(h.layoutAsked[0], filepath.Join("daemon", "bin", "agent-repld")) || strings.HasPrefix(h.layoutAsked[0], h.live.ModuleRoot) {
		t.Fatalf("layout asked of %v, want the one staged daemon binary", h.layoutAsked)
	}
}

func TestADaemonWhoseFreshLayoutCannotBeReadIsNeitherHandedOverNorRestarted(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.d.deps.DaemonBuild = hashOf(t, theOld.daemon)
	h.layoutErr = errors.New("exit status 2")

	// Act
	_, err := h.d.Deploy(context.Background(), false)

	// Assert
	if err == nil {
		t.Fatal("Deploy succeeded with the fresh layout unknown")
	}
	if len(h.rollout.handovers) != 0 || len(h.rollout.restarts) != 0 {
		t.Fatalf("handovers %v, restarts %v: want neither on a guess", h.rollout.handovers, h.rollout.restarts)
	}
	if !logged(h.log, "error", opDecide, "the fresh daemon's state layout could not be read") {
		t.Fatalf("records = %+v, want the unread layout at ERROR", h.log.Records())
	}
}

func TestARefusedRestartIsTheDeploysAnswer(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.d.deps.DaemonBuild = hashOf(t, theOld.daemon)
	h.freshLayout = runningLayout + 1
	h.rollout.restartErr = &rollout.ErrAlreadyRollingOut{WaitingOn: []ids.WorkspaceID{"ws-a"}}

	// Act
	_, err := h.d.Deploy(context.Background(), false)

	// Assert
	var inFlight *rollout.ErrAlreadyRollingOut
	if !errors.As(err, &inFlight) {
		t.Fatalf("Deploy = %v, want the restart's refusal", err)
	}
	if !logged(h.log, "error", opDecide, "the restart was not accepted") {
		t.Fatalf("records = %+v, want the refused restart at ERROR", h.log.Records())
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
	if !logged(h.log, "error", opDecide, "the handover was not accepted") {
		t.Fatalf("records = %+v, want the refused handover at ERROR", h.log.Records())
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

func TestTheWebappReloadNamesTheBuildItBrings(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.clients.webviews["ws-a"] = []string{"Web1"}
	h.clients.webviews["ws-b"] = []string{"Web1"}

	// Act
	if _, err := h.d.Deploy(context.Background(), false); err != nil {
		t.Fatalf("Deploy: %v", err)
	}

	// Assert
	if want := []string{"Web2", "Web2"}; !slices.Equal(h.clients.webappBuilds, want) {
		t.Fatalf("builds = %v, want %v", h.clients.webappBuilds, want)
	}
}

func TestTheWebviewHostOnAnOlderBuildIsReloadedOnce(t *testing.T) {
	tests := []struct {
		name      string
		hosts     []string
		reach     int
		wantPush  []string
		wantKind  OutcomeKind
		wantLevel string
	}{
		{name: "a host on the fresh build is left alone", hosts: []string{"Web2"}, reach: 1, wantKind: UpToDate},
		{name: "a host on an older build is reloaded onto the fresh one", hosts: []string{"Web1"}, reach: 1, wantPush: []string{"Web2"}, wantKind: ReloadPushed, wantLevel: "info"},
		{name: "two stale host streams are one reload", hosts: []string{"Web1", "Web1"}, reach: 1, wantPush: []string{"Web2"}, wantKind: ReloadPushed, wantLevel: "info"},
		{name: "a reload no Emacs took is an error", hosts: []string{"Web1"}, reach: 0, wantPush: []string{"Web2"}, wantKind: UpToDate, wantLevel: "error"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.clients.webhosts = tt.hosts
			h.clients.webhostReach = tt.reach

			// Act
			result, err := h.d.Deploy(context.Background(), false)

			// Assert
			if err != nil {
				t.Fatalf("Deploy: %v", err)
			}
			if !slices.Equal(h.clients.webhostPush, tt.wantPush) {
				t.Fatalf("host reloads = %v, want %v", h.clients.webhostPush, tt.wantPush)
			}
			if got := outcome(t, result, ComponentWebapp); got.Kind != tt.wantKind {
				t.Fatalf("webapp = %+v, want %s", got, tt.wantKind)
			}
			if tt.wantLevel == "info" && !logged(h.log, "info", opDecide, "the web view host runs an older webapp; pushed its reload") {
				t.Fatalf("records = %+v, want the reload at INFO", h.log.Records())
			}
			if tt.wantLevel == "error" && !logged(h.log, "error", opDecide, "the web view host runs an older webapp, but no Emacs stream took its reload") {
				t.Fatalf("records = %+v, want the unreached reload at ERROR", h.log.Records())
			}
		})
	}
}

func TestADeployIsRefused(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
		want    func(error) bool
		wantLog string
	}{
		{
			name:    "by a successor still joining",
			arrange: func(h *harness) { h.rollout.joining = true },
			want:    func(err error) bool { return errors.Is(err, rollout.ErrJoining) },
			wantLog: "refused a deploy asked of a successor still joining",
		},
		{
			name:    "while a handover is in flight",
			arrange: func(h *harness) { h.rollout.rolling = []ids.WorkspaceID{"ws-a"} },
			want: func(err error) bool {
				var inFlight *rollout.ErrAlreadyRollingOut
				return errors.As(err, &inFlight) && len(inFlight.WaitingOn) == 1
			},
			wantLog: "refused a deploy while a handover is in flight",
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
			if calls := h.reports.made(); len(calls) != 0 {
				t.Fatalf("a refused deploy touched the owed landing reports: %v", calls)
			}
			if !logged(h.log, "info", opDeploy, tc.wantLog) {
				t.Fatalf("records = %+v, want %q at INFO", h.log.Records(), tc.wantLog)
			}
		})
	}
}

func TestAStagingDirectoryThatCannotBeMadeBuildsNothing(t *testing.T) {
	// Arrange: the staging root's parent is a file.
	h := newHarness(t)
	blocker := filepath.Join(t.TempDir(), "file")
	writeFile(t, blocker, "x")
	h.d.deps.StagingRoot = filepath.Join(blocker, "staging")

	// Act
	_, err := h.d.Deploy(context.Background(), false)

	// Assert
	var failed *BuildFailed
	if !errors.As(err, &failed) || failed.Step != "setup" {
		t.Fatalf("Deploy = %v, want the setup step's failure", err)
	}
	if h.builder.count() != 0 {
		t.Fatalf("a deploy with no staging directory built")
	}
	if !logged(h.log, "error", opDeploy, "could not create the staging directory") {
		t.Fatalf("records = %+v, want the failure at ERROR", h.log.Records())
	}
}

func TestABuildThatStagedNoArtifactDeploysNothing(t *testing.T) {
	// Arrange: the build "succeeds" and stages nothing at all.
	h := newHarness(t)
	h.builder.build = artifacts{}
	h.builder.stageNothing = true

	// Act
	_, err := h.d.Deploy(context.Background(), false)

	// Assert
	var failed *BuildFailed
	if !errors.As(err, &failed) || failed.Step != "shim" {
		t.Fatalf("Deploy = %v, want the first unhashable artifact named", err)
	}
	if got := readFile(t, h.live.ShimMain()); got != theOld.shim {
		t.Fatalf("installed shim = %q, want the old one untouched", got)
	}
	if !logged(h.log, "error", opDeploy, "a staged artifact could not be hashed; NOTHING WAS DEPLOYED") {
		t.Fatalf("records = %+v, want the failure at ERROR", h.log.Records())
	}
}

func TestAnUnlistableWorkspaceSetIsTheDeploysAnswer(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.d.deps.Workspaces = func(context.Context) ([]ids.WorkspaceID, error) {
		return nil, errors.New("state client closed")
	}

	// Act
	_, err := h.d.Deploy(context.Background(), false)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "state client closed") {
		t.Fatalf("Deploy = %v, want the listing failure", err)
	}
	if !logged(h.log, "error", opDecide, "could not list the workspaces whose shims are judged") {
		t.Fatalf("records = %+v, want the failure at ERROR", h.log.Records())
	}
}

func TestAnInstallFailureRestartsNothing(t *testing.T) {
	// Arrange: the live daemon binary's directory is read-only, so the fresh
	// daemon cannot be installed beside it.
	h := newHarness(t)
	dir := filepath.Dir(h.live.DaemonBin())
	if err := os.Chmod(dir, 0o555); err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = os.Chmod(dir, 0o755) })

	// Act
	_, err := h.d.Deploy(context.Background(), false)

	// Assert
	var failed *InstallFailed
	if !errors.As(err, &failed) || failed.Component != ComponentDaemon {
		t.Fatalf("Deploy = %v, want the daemon's install failure", err)
	}
	if len(h.services.Calls()) != 0 || len(h.rollout.handovers) != 0 {
		t.Fatalf("services %v, handovers %v: want nothing restarted after a failed install", h.services.Calls(), h.rollout.handovers)
	}
	if !logged(h.log, "error", opInstall, "could not install the fresh build") ||
		!logged(h.log, "error", opDeploy, "nothing was restarted") {
		t.Fatalf("records = %+v, want the install failure and the deploy's stop at ERROR", h.log.Records())
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

// joiningHarness is a harness whose daemon is a successor still joining, with
// a latch that closes once a landing's deploy waits on the takeover.
func joiningHarness(t *testing.T) (*harness, <-chan struct{}) {
	t.Helper()
	h := newHarness(t)
	asked := make(chan struct{})
	h.rollout.joining = true
	h.rollout.tookOverAsked = asked
	return h, asked
}

// noErrors fails t when any deploy record was written at ERROR.
func noErrors(t *testing.T, h *harness) {
	t.Helper()
	for _, op := range []string{opLanding, opDeploy} {
		for _, r := range records(h.log, op) {
			if r.Level == "error" {
				t.Fatalf("an ERROR record was written: %+v", r)
			}
		}
	}
}

func TestALandingWhileJoiningDeploysOnceTheSuccessorTakesOver(t *testing.T) {
	// Arrange: the change lands while this successor is still joining.
	h, asked := joiningHarness(t)
	h.d.Landed(context.Background(), commitsOf(1))
	<-asked
	if got := h.builder.count(); got != 0 {
		t.Fatalf("builds = %d before the takeover, want none", got)
	}

	// Act
	h.rollout.takeOver()
	h.d.WaitLandings()

	// Assert
	if got := h.builder.count(); got != 1 {
		t.Fatalf("builds = %d, want the landing deployed once after the takeover", got)
	}
	if !logged(h.log, "info", opLanding, "the landing is deployed") {
		t.Fatalf("records = %+v, want the landing's deploy recorded", h.log.Records())
	}
	noErrors(t, h)
}

func TestALandingDuringTheTakeoverWaitIsCoveredByTheWaitingDeploy(t *testing.T) {
	// Arrange: one landing's deploy waits on the takeover.
	h, asked := joiningHarness(t)
	h.d.Landed(context.Background(), commitsOf(1))
	<-asked

	// Act: a second change lands, then the successor takes over.
	h.d.Landed(context.Background(), commitsOf(2))
	h.rollout.takeOver()
	h.d.WaitLandings()

	// Assert
	if got := h.builder.count(); got != 1 {
		t.Fatalf("builds = %d, want ONE deploy covering both landings", got)
	}
	if !logged(h.log, "info", opLanding, "that deploy covers it") {
		t.Fatalf("records = %+v, want the covered landing recorded", h.log.Records())
	}
	noErrors(t, h)
}

func TestALandingWhoseDaemonStopsBeforeTheTakeoverIsNotAFailure(t *testing.T) {
	// Arrange: the daemon's lifetime ends while the landing waits.
	h, asked := joiningHarness(t)
	lifetime, stop := context.WithCancel(context.Background())
	h.d.deps.Lifetime = lifetime
	h.d.Landed(context.Background(), commitsOf(1))
	<-asked

	// Act
	stop()
	h.d.WaitLandings()

	// Assert
	if got := h.builder.count(); got != 0 {
		t.Fatalf("builds = %d, want none from a daemon that never took over", got)
	}
	if !logged(h.log, "info", opLanding, "stopped before it took over") {
		t.Fatalf("records = %+v, want the abandoned wait recorded", h.log.Records())
	}
	noErrors(t, h)
}

func TestADeployRefusedWhileJoiningLeavesTheLandingOwed(t *testing.T) {
	// Arrange: a landing is owed while a deploy runs, so the landing's own
	// deploy is the follow-up; the running deploy is then refused because
	// this successor is joining.
	h := newHarness(t)
	h.rollout.joining = true

	// Act: a refused deploy must not consume the landing.
	if _, err := h.d.Deploy(context.Background(), false); !errors.Is(err, rollout.ErrJoining) {
		t.Fatalf("Deploy = %v, want ErrJoining", err)
	}
	h.d.mu.Lock()
	h.d.landed = true
	h.d.mu.Unlock()
	if _, err := h.d.Deploy(context.Background(), false); !errors.Is(err, rollout.ErrJoining) {
		t.Fatalf("Deploy = %v, want ErrJoining", err)
	}

	// Assert
	h.d.mu.Lock()
	owed, running := h.d.landed, h.d.running
	h.d.mu.Unlock()
	if !owed || running {
		t.Fatalf("landed = %v, running = %v; want the landing still owed and the slot free", owed, running)
	}
	if got := h.builder.count(); got != 0 {
		t.Fatalf("builds = %d, want none: a refused deploy starts no follow-up", got)
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

// ---- the update line --------------------------------------------------------

func TestADeployThatStaysStatesEveryPhaseAndEndsUpdated(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, h *harness)
		want    []string
	}{
		{"nothing to restart", func(*testing.T, *harness) {}, []string{"building", "installing", "updated"}},
		{"a stale store", func(t *testing.T, h *harness) {
			h.report(t, buildreport.ServiceStore, 101, hashOf(t, theOld.store))
		}, []string{"building", "installing", "restarting_services", "updated"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			tc.arrange(t, h)

			// Act
			if _, err := h.d.Deploy(context.Background(), false); err != nil {
				t.Fatalf("Deploy: %v", err)
			}

			// Assert
			if got := strings.Join(h.progress.phases(), ","); got != strings.Join(tc.want, ",") {
				t.Fatalf("phases = %s, want %s", got, strings.Join(tc.want, ","))
			}
		})
	}
}

func TestTheBuildingLineNamesTheBuiltComponents(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	if _, err := h.d.Deploy(context.Background(), false); err != nil {
		t.Fatalf("Deploy: %v", err)
	}

	// Assert
	first := h.progress.stated[0]
	if first.Phase != deployprogress.Building || len(first.Components) != len(builtComponents) {
		t.Fatalf("first statement = %+v, want building naming %v", first, builtComponents)
	}
}

func TestAStoreRestartNamesTheStoreAndTheSidecar(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.report(t, buildreport.ServiceStore, 101, hashOf(t, theOld.store))

	// Act
	if _, err := h.d.Deploy(context.Background(), false); err != nil {
		t.Fatalf("Deploy: %v", err)
	}

	// Assert
	restart := h.progress.stated[2]
	if restart.Phase != deployprogress.RestartingServices || len(restart.Components) != 2 ||
		restart.Components[0] != deployprogress.Store || restart.Components[1] != deployprogress.Sidecar {
		t.Fatalf("restart statement = %+v, want the store then the sidecar", restart)
	}
}

func TestAShimRegisteredBehindItsWorkIsNotedOnItsWorkspace(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.rollout.stale["ws-a"] = true
	h.rollout.stale["ws-b"] = true
	h.rollout.registered = map[ids.WorkspaceID]bool{"ws-b": true}

	// Act
	if _, err := h.d.Deploy(context.Background(), false); err != nil {
		t.Fatalf("Deploy: %v", err)
	}

	// Assert
	notes := h.progress.last().Notes
	if len(notes["ws-a"]) != 0 || len(notes["ws-b"]) != 1 || notes["ws-b"][0] != deployprogress.ShimWhenIdle {
		t.Fatalf("notes = %v, want shim_when_idle on ws-b alone", notes)
	}
}

func TestAHandoverLeavesUpdatedToItsSuccessor(t *testing.T) {
	tests := []struct {
		name         string
		force        bool
		wantDraining bool
	}{
		{name: "an unforced handover drains", wantDraining: true},
		{name: "a forced handover does not", force: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.d.deps.DaemonBuild = hashOf(t, theOld.daemon)

			// Act
			if _, err := h.d.Deploy(context.Background(), tc.force); err != nil {
				t.Fatalf("Deploy: %v", err)
			}

			// Assert
			last := h.progress.last()
			if last.Phase != deployprogress.HandingOver || last.Draining != tc.wantDraining {
				t.Fatalf("last statement = %+v, want handing_over with draining=%v", last, tc.wantDraining)
			}
		})
	}
}

func TestALayoutRestartStatesTheDaemonRestart(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.d.deps.DaemonBuild = hashOf(t, theOld.daemon)
	h.freshLayout = runningLayout + 1

	// Act
	if _, err := h.d.Deploy(context.Background(), false); err != nil {
		t.Fatalf("Deploy: %v", err)
	}

	// Assert
	last := h.progress.last()
	if last.Phase != deployprogress.RestartingServices || len(last.Components) != 1 ||
		last.Components[0] != deployprogress.Daemon || !last.Draining {
		t.Fatalf("last statement = %+v, want the daemon restarting at freeness", last)
	}
}

func TestAFailedDeployClearsItsLine(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, h *harness)
	}{
		{"a failed build", func(t *testing.T, h *harness) {
			h.builder.fail = &BuildFailed{Step: "webapp", Detail: "tsc", Log: "/logs"}
		}},
		{"a refused handover", func(t *testing.T, h *harness) {
			h.d.deps.DaemonBuild = hashOf(t, theOld.daemon)
			h.rollout.handErr = errors.New("the successor never proved it was serving")
		}},
		{"a failed service restart", func(t *testing.T, h *harness) {
			h.report(t, buildreport.ServiceStore, 101, hashOf(t, theOld.store))
			h.services.storeErr = errors.New("launchctl refused")
		}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			tc.arrange(t, h)

			// Act
			_, err := h.d.Deploy(context.Background(), false)

			// Assert
			if err == nil {
				t.Fatalf("Deploy succeeded, want the failure")
			}
			if h.progress.last() != nil {
				t.Fatalf("last statement = %+v, want the line cleared", h.progress.last())
			}
		})
	}
}

func TestARefusedDeployLeavesTheLineAlone(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.rollout.joining = true

	// Act
	_, err := h.d.Deploy(context.Background(), false)

	// Assert
	if err == nil {
		t.Fatalf("Deploy succeeded, want the refusal")
	}
	if got := h.progress.phases(); len(got) != 0 {
		t.Fatalf("phases = %v, want none: a refused deploy owns no line", got)
	}
}

func TestNewRefusesAMissingProgressSink(t *testing.T) {
	// Arrange
	h := newHarness(t)
	deps := h.d.deps
	deps.Progress = nil

	// Act
	_, err := New(deps)

	// Assert
	if err == nil {
		t.Fatalf("New accepted a Deployer with no progress sink")
	}
}

func TestAComponentIsNamedOnTheWireByItsOwnArm(t *testing.T) {
	tests := []struct {
		component Component
		want      agentreplv1.DeployComponent
	}{
		{ComponentDaemon, agentreplv1.DeployComponent_DEPLOY_COMPONENT_DAEMON},
		{ComponentShim, agentreplv1.DeployComponent_DEPLOY_COMPONENT_SHIM},
		{ComponentWebapp, agentreplv1.DeployComponent_DEPLOY_COMPONENT_WEBAPP},
		{ComponentStore, agentreplv1.DeployComponent_DEPLOY_COMPONENT_STORE},
		{ComponentSidecar, agentreplv1.DeployComponent_DEPLOY_COMPONENT_SIDECAR},
		{ComponentElisp, agentreplv1.DeployComponent_DEPLOY_COMPONENT_ELISP},
		{Component("nothing"), agentreplv1.DeployComponent_DEPLOY_COMPONENT_UNSPECIFIED},
	}
	for _, tt := range tests {
		t.Run(string(tt.component), func(t *testing.T) {
			// Arrange in the table. Act.
			got := tt.component.Arm()

			// Assert.
			if got != tt.want {
				t.Fatalf("%q.Arm() = %v, want %v", tt.component, got, tt.want)
			}
		})
	}
}

func TestServiceStalenessJudgesTheReportAgainstTheFreshBuild(t *testing.T) {
	tests := []struct {
		name      string
		report    *buildreport.Report
		raw       string
		alive     bool
		wantStale bool
		wantLevel string
	}{
		{name: "the fresh build, still running, is current", report: &buildreport.Report{PID: 7, Build: "fresh"}, alive: true, wantStale: false, wantLevel: "debug"},
		{name: "an older build is stale", report: &buildreport.Report{PID: 7, Build: "old"}, alive: true, wantStale: true, wantLevel: "info"},
		{name: "a report whose process is gone is stale", report: &buildreport.Report{PID: 7, Build: "fresh"}, alive: false, wantStale: true, wantLevel: "info"},
		{name: "no report is stale", wantStale: true, wantLevel: "info"},
		{name: "an unreadable report is stale, loudly", raw: "{not json", wantStale: true, wantLevel: "error"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			dir := t.TempDir()
			if tc.report != nil {
				if err := buildreport.Write(dir, buildreport.ServiceStore, *tc.report); err != nil {
					t.Fatal(err)
				}
			}
			if tc.raw != "" {
				writeFile(t, buildreport.Path(dir, buildreport.ServiceStore), tc.raw)
			}
			log := dlog.NewTestLogger()
			judge := serviceStaleness{reportDir: dir, alive: func(int) bool { return tc.alive }, log: log, op: "test"}

			// Act
			stale := judge.stale(ComponentStore, buildreport.ServiceStore, "fresh")

			// Assert
			if stale != tc.wantStale {
				t.Fatalf("stale = %v, want %v", stale, tc.wantStale)
			}
			records := log.Records()
			if len(records) != 1 || records[0].Level != tc.wantLevel {
				t.Fatalf("records = %+v, want one at %s", records, tc.wantLevel)
			}
		})
	}
}

// TestDeployNamesTheLockHolderOnlyThroughTheSharedConstant guards the
// extraction: deploy.go spells the lock holder's binary name only as
// sessionlock.HolderBinary, so the name it installs and the name the traffic
// measurement recognizes cannot drift apart.
func TestDeployNamesTheLockHolderOnlyThroughTheSharedConstant(t *testing.T) {
	// Arrange.
	src, err := os.ReadFile("deploy.go")
	if err != nil {
		t.Fatalf("read deploy.go: %v", err)
	}

	// Act.
	literal := strings.Count(string(src), `"shim-lock"`)
	shared := strings.Count(string(src), "sessionlock.HolderBinary")

	// Assert.
	if literal != 0 || shared == 0 {
		t.Fatalf("deploy.go spells the lock holder %d time(s) as a literal and %d through sessionlock.HolderBinary; want only the constant", literal, shared)
	}
}

func TestDrainLandingsAnswersAtOnceWithNoLandingDeploy(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	drained := h.d.DrainLandings(time.Second)

	// Assert
	if !drained {
		t.Fatal("DrainLandings = false with no landing deploy running, want true")
	}
}

func TestDrainLandingsWaitsForALandingDeployToEnd(t *testing.T) {
	// Arrange: a landing's deploy held in its build.
	h := newHarness(t)
	h.builder.started = make(chan struct{}, 1)
	h.builder.gate = make(chan struct{})
	h.d.Landed(context.Background(), commitsOf(1))
	<-h.builder.started
	drained := make(chan bool, 1)
	go func() { drained <- h.d.DrainLandings(10 * time.Second) }()

	// Act
	close(h.builder.gate)

	// Assert
	if !<-drained {
		t.Fatal("DrainLandings = false once the landing's deploy ended, want true")
	}
	if !logged(h.log, "info", "daemon.deploy.landing", "the landing is deployed") {
		t.Fatal("the landing's deploy did not record how it ended before the drain answered")
	}
}

func TestDrainLandingsAnswersFalseForALandingDeployThatOutlivesItsBound(t *testing.T) {
	// Arrange: a landing's deploy held in its build for the whole wait.
	h := newHarness(t)
	h.builder.started = make(chan struct{}, 1)
	h.builder.gate = make(chan struct{})
	t.Cleanup(func() {
		close(h.builder.gate)
		h.d.WaitLandings()
	})
	h.d.Landed(context.Background(), commitsOf(1))
	<-h.builder.started

	// Act
	drained := h.d.DrainLandings(50 * time.Millisecond)

	// Assert
	if drained {
		t.Fatal("DrainLandings = true while the landing's deploy still runs, want false")
	}
}

// ---- the landing reports a deploy owes ----

func TestADeployCoversTheOwedReportsBeforeItBuilds(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	if _, err := h.d.Deploy(context.Background(), false); err != nil {
		t.Fatalf("Deploy: %v", err)
	}

	// Assert: covered once, before the build read the checkout.
	h.reports.mu.Lock()
	defer h.reports.mu.Unlock()
	if len(h.reports.calls) == 0 || h.reports.calls[0] != "cover" || h.reports.buildsAtCover != 0 {
		t.Fatalf("calls %v with %d build(s) at the cover, want the cover first and before any build", h.reports.calls, h.reports.buildsAtCover)
	}
}

func TestADeployThatStaysPaysTheReportsItCoveredOnceItHasRun(t *testing.T) {
	// Arrange: the daemon already runs the fresh build, so it stays.
	h := newHarness(t)

	// Act
	if _, err := h.d.Deploy(context.Background(), false); err != nil {
		t.Fatalf("Deploy: %v", err)
	}

	// Assert
	if got := h.reports.made(); !slices.Equal(got, []string{"cover", "pay covered_only=true"}) {
		t.Fatalf("calls = %v, want the cover and then the covered reports paid", got)
	}
}

func TestADeployThatHandsOverLeavesTheReportsToItsSuccessor(t *testing.T) {
	// Arrange: an older daemon, so the deploy hands it over.
	h := newHarness(t)
	h.d.deps.DaemonBuild = hashOf(t, theOld.daemon)

	// Act
	if _, err := h.d.Deploy(context.Background(), false); err != nil {
		t.Fatalf("Deploy: %v", err)
	}

	// Assert
	if len(h.rollout.handovers) != 1 {
		t.Fatalf("handovers = %v, want one", h.rollout.handovers)
	}
	if got := h.reports.made(); !slices.Equal(got, []string{"cover"}) {
		t.Fatalf("calls = %v, want the reports covered and left for the successor", got)
	}
}

func TestADeployThatFailsPaysTheReportsItCoveredSayingSo(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.builder.fail = &BuildFailed{Step: "build", Detail: "webapp: tsc failed"}

	// Act
	_, err := h.d.Deploy(context.Background(), false)

	// Assert
	if err == nil {
		t.Fatal("Deploy succeeded with a failing build")
	}
	if got := h.reports.made(); !slices.Equal(got, []string{"cover", "pay covered_only=true failed"}) {
		t.Fatalf("calls = %v, want the covered reports paid as failed", got)
	}
}

func TestNewRefusesADeployWithNoOwedReports(t *testing.T) {
	// Arrange: every other dependency in place.
	h := newHarness(t)
	deps := h.d.deps
	deps.Reports = nil

	// Act
	_, err := New(deps)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "the owed landing reports are required") {
		t.Fatalf("New = %v, want the missing reports refused", err)
	}
}

func TestTheDeployedHookHearsOnlyADeployThatGotThrough(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
		want    int
	}{
		{name: "a deploy that got all the way through", arrange: func(*harness) {}, want: 1},
		{name: "a failed build", arrange: func(h *harness) {
			h.builder.fail = &BuildFailed{Step: "webapp", Detail: "tsc: 3 errors", Log: "/logs/build.log"}
		}, want: 0},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			tc.arrange(h)

			// Act
			_, _ = h.d.Deploy(context.Background(), false)

			// Assert
			if h.deployed != tc.want {
				t.Fatalf("Deployed heard %d deploys, want %d", h.deployed, tc.want)
			}
		})
	}
}

// ---- restart timing ---------------------------------------------------------

// deployPhases answers the deploy phases timed, each as phase:outcome.
func deployPhases(h *harness) []string {
	var out []string
	for _, o := range h.restarts.Of(restarttiming.Deploy) {
		out = append(out, o.Phase()+":"+string(o.Outcome()))
	}
	return out
}

func TestEachDeployPhaseIsTimed(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, h *harness)
		want    []string
	}{
		{
			name:    "a deploy that stays is timed to its end",
			arrange: func(*testing.T, *harness) {},
			want:    []string{"build:ok", "install:ok", "restart_services:ok", "decide:ok", "total:ok"},
		},
		{
			name:    "a deploy that hands over leaves its total to the successor",
			arrange: func(t *testing.T, h *harness) { h.d.deps.DaemonBuild = hashOf(t, theOld.daemon) },
			want:    []string{"build:ok", "install:ok", "restart_services:ok", "decide:ok"},
		},
		{
			name: "a failed build is timed to its failure",
			arrange: func(_ *testing.T, h *harness) {
				h.builder.fail = &BuildFailed{Step: "webapp", Detail: "tsc: 3 errors"}
			},
			want: []string{"build:failed", "total:failed"},
		},
		{
			name: "a refused handover fails the decision and the deploy",
			arrange: func(t *testing.T, h *harness) {
				h.d.deps.DaemonBuild = hashOf(t, theOld.daemon)
				h.rollout.handErr = errors.New("refused")
			},
			want: []string{"build:ok", "install:ok", "restart_services:ok", "decide:failed", "total:failed"},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			tc.arrange(t, h)

			// Act
			_, _ = h.d.Deploy(context.Background(), false)

			// Assert
			if got := strings.Join(deployPhases(h), ","); got != strings.Join(tc.want, ",") {
				t.Fatalf("timed = %s, want %s", got, strings.Join(tc.want, ","))
			}
		})
	}
}

func TestAHandoverCarriesTheDeploysStart(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, h *harness)
	}{
		{"a handover", func(*testing.T, *harness) {}},
		{"a restart across a layout", func(_ *testing.T, h *harness) { h.freshLayout = runningLayout + 1 }},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.d.deps.DaemonBuild = hashOf(t, theOld.daemon)
			tc.arrange(t, h)

			// Act
			if _, err := h.d.Deploy(context.Background(), true); err != nil {
				t.Fatalf("Deploy: %v", err)
			}

			// Assert
			if len(h.rollout.asks) != 1 || !h.rollout.asks[0].DeployStartedAt.Equal(instant) || !h.rollout.asks[0].Force {
				t.Fatalf("asks = %+v, want one forced ask carrying the deploy's start %v", h.rollout.asks, instant)
			}
		})
	}
}

func TestNewRefusesAMissingRestartRecorder(t *testing.T) {
	// Arrange
	h := newHarness(t)
	deps := h.d.deps
	deps.Restarts = nil

	// Act
	_, err := New(deps)

	// Assert
	if err == nil {
		t.Fatalf("New accepted a Deployer with no restart recorder")
	}
}

func TestAReloadedWebviewIsExpectedBackFromItsPush(t *testing.T) {
	// Arrange: only ws-b's webview is on an older build.
	h := newHarness(t)
	h.clients.webviews["ws-a"] = []string{"Web2"}
	h.clients.webviews["ws-b"] = []string{"Web2", "Web1"}

	// Act
	if _, err := h.d.Deploy(context.Background(), false); err != nil {
		t.Fatalf("Deploy: %v", err)
	}
	h.d.deps.Reconnects.Connected("ws-a", restarttiming.ReconnectWeb)
	h.d.deps.Reconnects.Connected("ws-b", restarttiming.ReconnectWeb)

	// Assert
	got := h.restarts.Of(restarttiming.WorkspaceReconnect)
	if len(got) != 1 || got[0].Workspace() != "ws-b" || got[0].Cause() != "webapp_reload" {
		t.Fatalf("reconnects = %+v, want ws-b's reloaded webview alone", got)
	}
}

func TestNewRefusesAMissingReconnectTracker(t *testing.T) {
	// Arrange
	h := newHarness(t)
	deps := h.d.deps
	deps.Reconnects = nil

	// Act
	_, err := New(deps)

	// Assert
	if err == nil {
		t.Fatalf("New accepted a Deployer with no reconnect tracker")
	}
}
