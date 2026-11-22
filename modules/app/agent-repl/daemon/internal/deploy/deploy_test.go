package deploy

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"reflect"
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
	"agent-repld/internal/sourcescan"
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

func TestADeployMovesAnUnchangedArtifactsStampsToTheFreshSource(t *testing.T) {
	// Arrange: the store is already the fresh build, stamped from older source.
	h := newHarness(t)
	writeFile(t, h.live.CacheBinPath(buildreport.ServiceStore), theFresh.store)
	stamp := filepath.Join(h.live.CacheBin, ".shim-store.source-tree")
	writeFile(t, stamp, "tree-the-older-source")

	// Act
	if _, err := h.d.Deploy(context.Background(), false); err != nil {
		t.Fatalf("Deploy: %v", err)
	}

	// Assert
	if got := readFile(t, stamp); got != "tree-"+theFresh.store {
		t.Fatalf("store source stamp = %q, want the fresh source's %q", got, "tree-"+theFresh.store)
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

func TestAMovingDaemonLeavesTheElispReloadToItsSuccessor(t *testing.T) {
	// Arrange: this daemon runs the OLD build, and an Emacs runs older elisp.
	h := newHarness(t)
	h.d.deps.DaemonBuild = hashOf(t, theOld.daemon)
	h.rollout.acceptance = rollout.HandoverAcceptance{Workspaces: 1}
	h.clients.emacs = []EmacsClient{{ID: "stale", Build: "older"}}

	// Act
	result, err := h.d.Deploy(context.Background(), false)

	// Assert
	if err != nil {
		t.Fatalf("Deploy: %v", err)
	}
	if got := outcome(t, result, ComponentElisp); got.Kind != DeferredToSuccessor || len(h.clients.elispPushes) != 0 {
		t.Fatalf("elisp = %+v, pushes %v; want the reload left to the successor", got, h.clients.elispPushes)
	}
	if !logged(h.log, "info", opDecide, "its successor decides each Emacs's elisp as its stream connects") {
		t.Fatalf("records = %+v, want the deferral recorded", h.log.Records())
	}
}

func TestElispVerdictDecidesAConnectingEmacsAgainstTheCheckout(t *testing.T) {
	tests := []struct {
		name      string
		running   func(h *harness) string
		wantStale bool
		wantLog   string
	}{
		{name: "an Emacs on older elisp is stale", running: func(*harness) string { return "older" },
			wantStale: true, wantLog: "an Emacs runs older elisp; it is pushed the reload"},
		{name: "an Emacs on the checkout's elisp is current", running: func(h *harness) string { return h.elisp },
			wantLog: "an Emacs runs the checkout's elisp"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			running := tt.running(h)

			// Act
			verdict, err := h.d.ElispVerdict("s1", running)

			// Assert
			if err != nil {
				t.Fatalf("ElispVerdict: %v", err)
			}
			want := ElispVerdict{Fresh: h.elisp, ModuleRoot: h.live.ModuleRoot, Stale: tt.wantStale}
			if verdict != want {
				t.Fatalf("verdict = %+v, want %+v", verdict, want)
			}
			assertElispDecisionLogged(t, h, tt.wantLog, dlog.Context{
				"stream": "s1", "running": running, "fresh": h.elisp, "trigger": ElispOnConnect,
			})
		})
	}
}

func TestElispVerdictOfAnUnreadableBuildIsAnErrorAndSaysWhy(t *testing.T) {
	// Arrange: the module root names no elisp at all.
	h := newHarness(t)
	h.d.deps.Live.ModuleRoot = filepath.Join(t.TempDir(), "gone")

	// Act
	_, err := h.d.ElispVerdict("s1", "older")

	// Assert
	if err == nil {
		t.Fatal("ElispVerdict = nil error, want the unreadable build refused")
	}
	recs := records(h.log, opDecide)
	if len(recs) != 1 || recs[0].Level != "error" || !strings.Contains(recs[0].Message, "the checkout's elisp build could not be read") {
		t.Fatalf("records = %+v, want the unreadable build alone at ERROR", recs)
	}
	for key, want := range map[string]any{"stream": "s1", "running": "older", "trigger": ElispOnConnect, "module_root": h.d.deps.Live.ModuleRoot} {
		if recs[0].Context[key] != want {
			t.Fatalf("context[%s] = %v, want %v (record %+v)", key, recs[0].Context[key], want, recs[0])
		}
	}
}

func TestADeployRecordsEachStandingEmacsDecisionWithItsTrigger(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.clients.emacs = []EmacsClient{{ID: "current", Build: h.elisp}, {ID: "stale", Build: "older"}}

	// Act
	if _, err := h.d.Deploy(context.Background(), false); err != nil {
		t.Fatalf("Deploy: %v", err)
	}

	// Assert
	assertElispDecisionLogged(t, h, "an Emacs runs the checkout's elisp", dlog.Context{
		"stream": "current", "running": h.elisp, "fresh": h.elisp, "trigger": ElispOnDeploy,
	})
	assertElispDecisionLogged(t, h, "an Emacs runs older elisp; it is pushed the reload", dlog.Context{
		"stream": "stale", "running": "older", "fresh": h.elisp, "trigger": ElispOnDeploy,
	})
}

// assertElispDecisionLogged fails unless one INFO decision record carries
// MESSAGE and every field of WANT.
func assertElispDecisionLogged(t *testing.T, h *harness, message string, want dlog.Context) {
	t.Helper()
	for _, r := range records(h.log, opDecide) {
		if r.Level != "info" || r.Message != message || r.Context["stream"] != want["stream"] {
			continue
		}
		for key, value := range want {
			if r.Context[key] != value {
				t.Fatalf("context[%s] = %v, want %v (record %+v)", key, r.Context[key], value, r)
			}
		}
		return
	}
	t.Fatalf("records = %+v, want an INFO %q for stream %v", h.log.Records(), message, want["stream"])
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
		// AMENDED (2026-10-24): an Emacs attaching carries a reload no stream
		// stood to take (webhost.go), so it is awaited at INFO, not an error.
		{name: "a reload no Emacs took awaits one", hosts: []string{"Web1"}, reach: 0, wantPush: []string{"Web2"}, wantKind: UpToDate, wantLevel: "awaited"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.clients.webhosts = hostPages(tt.hosts...)
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
			if tt.wantLevel == "info" && !logged(h.log, "info", opWebhost, "the web view host runs another webapp than the installed one; pushed its reload") {
				t.Fatalf("records = %+v, want the reload at INFO", h.log.Records())
			}
			if tt.wantLevel == "awaited" && !logged(h.log, "info", opWebhost, "a web view host runs another webapp and no Emacs stream stands to take its reload; it is pushed when one attaches") {
				t.Fatalf("records = %+v, want the awaited reload at INFO", h.log.Records())
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

func TestAStaleStandaloneRestartsAloneAndIsReported(t *testing.T) {
	cases := []struct {
		name  string
		stale bool
		want  OutcomeKind
	}{
		{name: "reports the fresh build: nothing restarts", want: UpToDate},
		{name: "reports an older build: it alone restarts", stale: true, want: Restarted},
	}
	builds := map[Component][2]string{ComponentZettel: {theOld.zettel, theFresh.zettel}, ComponentTests: {theOld.tests, theFresh.tests}}
	for _, sv := range Standalones {
		for _, tc := range cases {
			t.Run(sv.Short+" "+tc.name, func(t *testing.T) {
				// Arrange
				h := newHarness(t)
				if tc.stale {
					h.report(t, sv.Service, 200, hashOf(t, builds[sv.Component][0]))
				}

				// Act
				result, err := h.d.Deploy(context.Background(), false)

				// Assert
				if err != nil {
					t.Fatalf("Deploy: %v", err)
				}
				var wantCalls []string
				if tc.stale {
					wantCalls = []string{string(sv.Component)}
				}
				if got := h.services.Calls(); !reflect.DeepEqual(got, wantCalls) {
					t.Fatalf("restarts = %v, want %v", got, wantCalls)
				}
				if got := outcome(t, result, sv.Component).Kind; got != tc.want {
					t.Fatalf("%s = %s, want %s", sv.Short, got, tc.want)
				}
				if got := readFile(t, h.live.CacheBinPath(sv.Service)); got != builds[sv.Component][1] {
					t.Fatalf("installed %s = %q, want the fresh build", sv.Short, got)
				}
			})
		}
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

func TestAWebappReloadExpectsEveryReloadedWebviewBackAsOneWave(t *testing.T) {
	// Arrange: three webviews run an older build, one the fresh one.
	h := newHarness(t)
	h.clients.webviews["ws-a"] = []string{"Web1"}
	h.clients.webviews["ws-b"] = []string{"Web2", "Web1"}
	h.clients.webviews["ws-c"] = []string{"Web1"}
	h.clients.webviews["ws-d"] = []string{"Web2"}
	if _, err := h.d.Deploy(context.Background(), false); err != nil {
		t.Fatalf("Deploy: %v", err)
	}

	// Act: the reloaded webviews come back 1s, 3s and 7s after the deploy
	// ended, and the fresh one's stream opens too, expected by nothing.
	h.wifiAsks.clock.After(time.Second)
	h.d.deps.Reconnects.Connected("ws-b", restarttiming.ReconnectWeb)
	h.d.deps.Reconnects.Connected("ws-d", restarttiming.ReconnectWeb)
	h.wifiAsks.clock.After(2 * time.Second)
	h.d.deps.Reconnects.Connected("ws-a", restarttiming.ReconnectWeb)
	h.wifiAsks.clock.After(4 * time.Second)
	h.d.deps.Reconnects.Connected("ws-c", restarttiming.ReconnectWeb)

	// Assert: ONE web wave holding the three reloaded webviews, its spread
	// the 6s between its first and last arrival.
	got := h.restarts.Of(restarttiming.ReconnectWave)
	var marks []string
	for _, o := range got {
		marks = append(marks, o.Phase()+" "+o.Cause())
	}
	if want := []string{"web_first webapp_reload", "web_last webapp_reload", "web_spread webapp_reload"}; !slices.Equal(marks, want) {
		t.Fatalf("wave marks = %v, want %v", marks, want)
	}
	if got[2].Seconds() != 6 || got[1].Seconds()-got[0].Seconds() != 6 {
		t.Fatalf("wave = %v, want a 6s spread between first and last", h.restarts.WaveMarks())
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

func TestAFailedStandaloneRestartIsTheDeploysAnswer(t *testing.T) {
	old := map[Component]string{ComponentZettel: theOld.zettel, ComponentTests: theOld.tests}
	for _, sv := range Standalones {
		t.Run(sv.Short, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.report(t, sv.Service, 200, hashOf(t, old[sv.Component]))
			h.services.standaloneErr = map[Component]error{sv.Component: errors.New("kickstart refused")}

			// Act
			_, err := h.d.Deploy(context.Background(), false)

			// Assert
			var failed *ServiceRestartFailed
			if !errors.As(err, &failed) || failed.Component != sv.Component {
				t.Fatalf("Deploy = %v, want %s's ServiceRestartFailed", err, sv.Short)
			}
			if !logged(h.log, "error", opDecide, "the "+sv.Short+" restart failed") {
				t.Fatalf("records = %+v, want the failure at ERROR", h.log.Records())
			}
		})
	}
}

// wifiHash reads the live Wi-Fi helper's source hash.
func (h *harness) wifiHash(t *testing.T) string {
	t.Helper()
	hash, err := wifiSourceHash(h.live.WifiApp)
	if err != nil {
		t.Fatalf("the live Wi-Fi helper: %v", err)
	}
	return hash
}

func TestADeployInstallsAWifiHelperBuiltFromOtherSources(t *testing.T) {
	// Arrange
	h := newHarness(t, withWifiHelper(t))

	// Act
	if _, err := h.d.Deploy(context.Background(), false); err != nil {
		t.Fatalf("Deploy: %v", err)
	}

	// Assert
	if got := h.wifiHash(t); got != theFresh.wifi {
		t.Fatalf("live helper sources = %q, want the fresh %q", got, theFresh.wifi)
	}
	if got := readFile(t, filepath.Join(h.live.WifiApp, "Contents", "_CodeSignature", "CodeResources")); got != "signed "+theFresh.wifi {
		t.Fatalf("live helper signature = %q, want the fresh build's whole app", got)
	}
	installed := false
	for _, r := range records(h.log, opInstall) {
		installed = installed || (r.Message == "installed the fresh build" && r.Context["artifact"] == "wifi-helper")
	}
	if !installed {
		t.Fatalf("records = %+v, want the helper's install recorded", records(h.log, opInstall))
	}
}

func TestADeployThatInstallsAWifiHelperStopsTheReplacedBuildsAsks(t *testing.T) {
	cases := []struct {
		name        string
		stopped     bool
		wantMessage string
	}{
		{name: "an ask that was waiting is stopped and recorded with the stop's time", stopped: true,
			wantMessage: "stopped the replaced Wi-Fi helper's ask waiting on the owner; the next scan asks again from the installed helper"},
		{name: "no ask waiting is recorded with the stop's time", stopped: false,
			wantMessage: "no ask of the replaced Wi-Fi helper was waiting on the owner"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t, withWifiHelper(t))
			h.wifiAsks.stopped = tc.stopped
			h.wifiAsks.took = 23 * time.Millisecond

			// Act
			if _, err := h.d.Deploy(context.Background(), false); err != nil {
				t.Fatalf("Deploy: %v", err)
			}

			// Assert
			if h.wifiAsks.calls != 1 {
				t.Fatalf("StopWifiAsks calls = %d, want one after the helper's install", h.wifiAsks.calls)
			}
			var stops []map[string]any
			for _, r := range records(h.log, opInstall) {
				if r.Level == "info" && r.Message == tc.wantMessage {
					stops = append(stops, r.Context)
				}
			}
			if len(stops) != 1 || stops[0]["app"] != h.live.WifiApp || stops[0]["took_ms"] != int64(23) {
				t.Fatalf("stop records = %+v, want one %q naming the helper and took_ms 23", stops, tc.wantMessage)
			}
		})
	}
}

func TestADeployThatLeavesTheWifiHelperStopsNoAsk(t *testing.T) {
	// Arrange: the live helper is already the fresh build.
	h := newHarness(t, withWifiHelper(t))
	writeWifiApp(t, h.live.WifiApp, theFresh.wifi)

	// Act
	if _, err := h.d.Deploy(context.Background(), false); err != nil {
		t.Fatalf("Deploy: %v", err)
	}

	// Assert: an ask of the standing build is the one the owner may answer.
	if h.wifiAsks.calls != 0 {
		t.Fatalf("StopWifiAsks calls = %d, want none for a helper left standing", h.wifiAsks.calls)
	}
}

func TestAWifiAskThatCannotBeStoppedFailsTheInstallAndPutsTheHelperBack(t *testing.T) {
	// Arrange
	h := newHarness(t, withWifiHelper(t))
	h.wifiAsks.err = errors.New("pkill exited 2: bad pattern")
	h.wifiAsks.took = 41 * time.Millisecond

	// Act
	_, err := h.d.Deploy(context.Background(), false)

	// Assert
	var failed *InstallFailed
	if !errors.As(err, &failed) || failed.Component != ComponentDaemon || !strings.Contains(failed.Detail, "stop the replaced Wi-Fi helper's waiting asks: pkill exited 2: bad pattern (after 41ms)") {
		t.Fatalf("Deploy = %v, want the helper's install failure naming the stop", err)
	}
	if got := h.wifiHash(t); got != theOld.wifi {
		t.Fatalf("live helper sources = %q, want the previous %q restored", got, theOld.wifi)
	}
	if !logged(h.log, "error", opInstall, "could not install the fresh build") {
		t.Fatalf("records = %+v, want the failure at ERROR", h.log.Records())
	}
	if len(h.services.Calls()) != 0 || len(h.rollout.handovers) != 0 {
		t.Fatalf("services %v, handovers %v: want nothing restarted after a failed install", h.services.Calls(), h.rollout.handovers)
	}
}

func TestNewRefusesAWifiHelperWithNoAskStopper(t *testing.T) {
	// Arrange
	h := newHarness(t, withWifiHelper(t))
	deps := h.d.deps
	deps.StopWifiAsks = nil

	// Act
	_, err := New(deps)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "ask stopper is required") {
		t.Fatalf("New = %v, want the missing ask stopper refused", err)
	}
}

func TestADeployLeavesAWifiHelperBuiltFromTheSameSources(t *testing.T) {
	// Arrange: the live helper is built from the fresh sources but signed
	// apart, as a rebuild would sign it.
	h := newHarness(t, withWifiHelper(t))
	writeWifiApp(t, h.live.WifiApp, theFresh.wifi)
	signature := filepath.Join(h.live.WifiApp, "Contents", "_CodeSignature", "CodeResources")
	writeFile(t, signature, "the signature macOS allowed")

	// Act
	if _, err := h.d.Deploy(context.Background(), false); err != nil {
		t.Fatalf("Deploy: %v", err)
	}

	// Assert: the allowed signature still stands.
	if got := readFile(t, signature); got != "the signature macOS allowed" {
		t.Fatalf("live helper signature = %q, want the allowed app untouched", got)
	}
}

func TestAStagedWifiHelperNamingNoSourcesDeploysNothing(t *testing.T) {
	// Arrange: the build staged no helper at all.
	h := newHarness(t, withWifiHelper(t))
	h.builder.build.wifi = ""

	// Act
	_, err := h.d.Deploy(context.Background(), false)

	// Assert
	var failed *BuildFailed
	if !errors.As(err, &failed) || failed.Step != "wifi" {
		t.Fatalf("Deploy = %v, want the helper's step named", err)
	}
	if got := h.wifiHash(t); got != theOld.wifi {
		t.Fatalf("live helper sources = %q, want the old %q untouched", got, theOld.wifi)
	}
	if got := readFile(t, h.live.ShimMain()); got != theOld.shim {
		t.Fatalf("installed shim = %q, want nothing installed", got)
	}
	if !logged(h.log, "error", opDeploy, "a staged artifact could not be hashed; NOTHING WAS DEPLOYED") {
		t.Fatalf("records = %+v, want the failure at ERROR", h.log.Records())
	}
}

func TestAPlatformWithNoWifiHelperNeitherHashesNorInstallsOne(t *testing.T) {
	// Arrange: no live helper is named, and the build stages none.
	h := newHarness(t)
	h.builder.build.wifi = ""

	// Act
	if _, err := h.d.Deploy(context.Background(), false); err != nil {
		t.Fatalf("Deploy: %v", err)
	}

	// Assert
	for _, r := range records(h.log, opInstall) {
		if r.Context["artifact"] == "wifi-helper" {
			t.Fatalf("record %+v: a platform with no helper installed one", r)
		}
	}
}

func TestAFailureAfterTheInstallPutsThePreviousWifiHelperBack(t *testing.T) {
	// Arrange: the store restart fails after every artifact was installed.
	h := newHarness(t, withWifiHelper(t))
	h.report(t, buildreport.ServiceStore, 101, hashOf(t, theOld.store))
	h.services.storeErr = errors.New("launchctl refused")
	h.services.recovers = true

	// Act
	_, err := h.d.Deploy(context.Background(), false)

	// Assert
	if err == nil {
		t.Fatal("Deploy succeeded, want the store restart's failure")
	}
	if got := h.wifiHash(t); got != theOld.wifi {
		t.Fatalf("live helper sources = %q, want the previous %q restored", got, theOld.wifi)
	}
}

func TestUpdatedSaysWhetherTheDeployChangedNothing(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
		want    bool
	}{
		{name: "every component up to date", want: true},
		{name: "a stale shim bounced", arrange: func(h *harness) { h.rollout.stale["ws-a"] = true }},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			if tc.arrange != nil {
				tc.arrange(h)
			}

			// Act
			result, err := h.d.Deploy(context.Background(), false)

			// Assert
			last := h.progress.last()
			if err != nil || last.Phase != deployprogress.Updated || last.Unchanged != tc.want {
				t.Fatalf("Deploy = (%+v, %v), last statement %+v; want updated with unchanged=%v", result, err, last, tc.want)
			}
		})
	}
}

func TestUnchangedIsEveryOutcomeUpToDate(t *testing.T) {
	tests := []struct {
		name     string
		outcomes []Outcome
		want     bool
	}{
		{name: "every component up to date", outcomes: []Outcome{{Component: ComponentStore, Kind: UpToDate}, {Component: ComponentElisp, Kind: UpToDate}}, want: true},
		{name: "one component restarted", outcomes: []Outcome{{Component: ComponentStore, Kind: UpToDate}, {Component: ComponentSidecar, Kind: Restarted}}},
		{name: "no component decided"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := unchanged(Result{Outcomes: tc.outcomes})

			// Assert
			if got != tc.want {
				t.Fatalf("unchanged = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestADeployThatInstallsTheFirstWifiHelperStopsNoAsk(t *testing.T) {
	// Arrange: no helper stands where the fresh one installs.
	h := newHarness(t, withWifiHelper(t))
	if err := os.RemoveAll(h.live.WifiApp); err != nil {
		t.Fatal(err)
	}

	// Act
	if _, err := h.d.Deploy(context.Background(), false); err != nil {
		t.Fatalf("Deploy: %v", err)
	}

	// Assert: installed, with no ask of a replaced build to stop.
	if got := h.wifiHash(t); got != theFresh.wifi {
		t.Fatalf("live helper sources = %q, want the fresh %q", got, theFresh.wifi)
	}
	if h.wifiAsks.calls != 0 {
		t.Fatalf("StopWifiAsks calls = %d, want none when no helper stood", h.wifiAsks.calls)
	}
	if !logged(h.log, "info", opInstall, "no Wi-Fi helper stood before this install, so no ask of a replaced build can be waiting") {
		t.Fatalf("records = %+v, want the skipped stop recorded", records(h.log, opInstall))
	}
}

func TestPathStands(t *testing.T) {
	dir := t.TempDir()
	file := filepath.Join(dir, "file")
	if err := os.WriteFile(file, nil, 0o644); err != nil {
		t.Fatal(err)
	}
	tests := []struct {
		name    string
		path    string
		want    bool
		wantErr bool
	}{
		{name: "a file stands", path: file, want: true},
		{name: "a directory stands", path: dir, want: true},
		{name: "nothing stands", path: filepath.Join(dir, "absent")},
		{name: "a path under a file cannot be read", path: filepath.Join(file, "child"), wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := pathStands(tt.path)

			// Assert
			if got != tt.want || (err != nil) != tt.wantErr {
				t.Fatalf("pathStands(%s) = %v, %v; want %v, error %v", tt.path, got, err, tt.want, tt.wantErr)
			}
		})
	}
}

func TestEveryLiveAbsenceIsReadThroughPathStands(t *testing.T) {
	// Act
	sites := sourcescan.Sites(t, "os.Lstat(")

	// Assert: pathStands' own read is the only one.
	if !slices.Equal(sites, []string{"deploy.go"}) {
		t.Fatalf("os.Lstat sites = %v, want only pathStands' in deploy.go", sites)
	}
}

// ---- the hot reload ---------------------------------------------------------

// toldAll reports whether every one of WANT was told, in order, among TOLD.
func toldAll(told, want []string) bool {
	i := 0
	for _, event := range told {
		if i < len(want) && event == want[i] {
			i++
		}
	}
	return i == len(want)
}

func TestADeployTellsItsHotReloadEveryStep(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, h *harness)
		want    []string
	}{
		{"a deploy whose daemon stays", func(*testing.T, *harness) {}, []string{
			"begin", "built:8", "install_began", "install_ended",
			"current:store", "current:sidecar", "current:zettel", "current:test service",
			"daemon_current", "shims_judging", "shims_judged", "webapp_judged",
		}},
		{"a stale store restarts the sidecar with it", func(t *testing.T, h *harness) {
			h.report(t, buildreport.ServiceStore, 101, hashOf(t, theOld.store))
		}, []string{"restarting:store", "restarting:sidecar", "restarted:store", "restarted:sidecar"}},
		{"a stale sidecar alone", func(t *testing.T, h *harness) {
			h.report(t, buildreport.ServiceSidecar, 102, hashOf(t, theOld.sidecar))
		}, []string{"current:store", "restarting:sidecar", "restarted:sidecar"}},
		{"a stale standalone service", func(t *testing.T, h *harness) {
			h.report(t, buildreport.ServiceZettel, 103, hashOf(t, theOld.zettel))
		}, []string{"restarting:zettel", "restarted:zettel"}},
		{"a stale webview", func(_ *testing.T, h *harness) {
			h.clients.webviews["ws-a"] = []string{"web-old"}
		}, []string{"webview_reloading:ws-a", "webapp_judged"}},
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
			if told := h.hotReload.told(); !toldAll(told, tc.want) {
				t.Fatalf("told %v, want %v in order", told, tc.want)
			}
		})
	}
}

func TestADeployTellsItsHotReloadEachInstalledArtifact(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	if _, err := h.d.Deploy(context.Background(), false); err != nil {
		t.Fatalf("Deploy: %v", err)
	}

	// Assert
	want := []string{"installed:shim:primary=true:replaced=true", "installed:shim:primary=false:replaced=true"}
	if told := h.hotReload.told(); !toldAll(told, want) {
		t.Fatalf("told %v, want the shim bundle as its build and shim-lock as a rider", told)
	}
}

func TestAMovingDaemonHandsItsHotReloadToTheSuccessor(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, h *harness)
		words   string
	}{
		{"a handover", func(*testing.T, *harness) {}, "handing over every workspace"},
		{"a restart across a layout change", func(_ *testing.T, h *harness) { h.freshLayout = runningLayout + 1 }, "restarting across a state layout change"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.d.deps.DaemonBuild = hashOf(t, theOld.daemon)
			h.clients.webviews["ws-b"] = []string{"web-old"}
			tc.arrange(t, h)

			// Act
			if _, err := h.d.Deploy(context.Background(), false); err != nil {
				t.Fatalf("Deploy: %v", err)
			}

			// Assert
			if told := h.hotReload.told(); !toldAll(told, []string{"daemon_moving:" + tc.words + ":[ws-b]", "handoff"}) {
				t.Fatalf("told %v, want the move with the stale webview, then the handoff", told)
			}
			if asks := h.rollout.asks; len(asks) != 1 || string(asks[0].HotReload) != "card" {
				t.Fatalf("asks %+v, want the card in the rollout's ask", asks)
			}
		})
	}
}

func TestARefusedMoveFailsItsHotReload(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
	}{
		{"a refused handover", func(h *harness) { h.rollout.handErr = errors.New("busy") }},
		{"a refused restart", func(h *harness) { h.freshLayout = runningLayout + 1; h.rollout.restartErr = errors.New("busy") }},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.d.deps.DaemonBuild = hashOf(t, theOld.daemon)
			tc.arrange(h)

			// Act
			_, err := h.d.Deploy(context.Background(), false)

			// Assert
			if err == nil || !toldAll(h.hotReload.told(), []string{"handoff", "handover_failed", "failed"}) {
				t.Fatalf("Deploy = %v, told %v; want the move's failure told", err, h.hotReload.told())
			}
		})
	}
}

func TestAFailedBuildFailsItsHotReload(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.builder.fail = &BuildFailed{Step: "shim", Detail: "boom"}

	// Act
	_, err := h.d.Deploy(context.Background(), false)

	// Assert
	if err == nil || strings.Join(h.hotReload.told(), ",") != "begin,failed" {
		t.Fatalf("Deploy = %v, told %v; want begin then failed", err, h.hotReload.told())
	}
}

func TestElispTellsItsHotReload(t *testing.T) {
	tests := []struct {
		name  string
		emacs []EmacsClient
		want  string
	}{
		{"an Emacs on older elisp", []EmacsClient{{ID: "e1", Build: "old"}}, "elisp_pushed:1"},
		{"every Emacs current", nil, "elisp_current"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.clients.emacs = tc.emacs

			// Act
			if _, err := h.d.Deploy(context.Background(), false); err != nil {
				t.Fatalf("Deploy: %v", err)
			}

			// Assert
			if told := h.hotReload.told(); !toldAll(told, []string{tc.want}) {
				t.Fatalf("told %v, want %s", told, tc.want)
			}
		})
	}
}

func TestNewRefusesAMissingHotReload(t *testing.T) {
	// Arrange
	deps := newHarness(t).d.deps
	deps.HotReload = nil

	// Act
	_, err := New(deps)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "hot reload tracker is required") {
		t.Fatalf("New = %v, want the missing hot reload refused", err)
	}
}

func TestHotReloadServiceNamesEveryComponent(t *testing.T) {
	for _, c := range []Component{ComponentStore, ComponentSidecar, ComponentZettel, ComponentTests, ComponentLauncher, ComponentDaemon, ComponentShim, ComponentWebapp, ComponentElisp} {
		t.Run(string(c), func(t *testing.T) {
			// Act
			got := hotReloadService(c)

			// Assert
			if got == "" {
				t.Fatalf("component %s has no hot reload service", c)
			}
		})
	}
}

func TestHotReloadServiceRefusesAnUnknownComponent(t *testing.T) {
	// Arrange
	defer func() {
		if recover() == nil {
			t.Fatal("an unknown component did not panic")
		}
	}()

	// Act
	hotReloadService("nonsense")
}

func TestADeploySweepsTheSidebarAfterBeginAndBeforeTheBuild(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	if _, err := h.d.Deploy(context.Background(), false); err != nil {
		t.Fatalf("Deploy: %v", err)
	}

	// Assert
	if h.foldSweep.sweeps != 1 {
		t.Fatalf("sweeps = %d, want one per deploy", h.foldSweep.sweeps)
	}
	if h.foldSweep.buildsAtSweep != 0 {
		t.Fatalf("builds started before the sweep = %d, want none", h.foldSweep.buildsAtSweep)
	}
	if !h.foldSweep.sawBegin {
		t.Fatal("the sweep ran before the hot reload was told the deploy began")
	}
}

func TestAFailedSweepNeverFailsTheDeploy(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.foldSweep.err = errors.New("fold subsection \"sub\": holds_current")

	// Act
	_, err := h.d.Deploy(context.Background(), false)

	// Assert
	if err != nil {
		t.Fatalf("Deploy = %v, want a failed sweep to leave the deploy standing", err)
	}
	if h.builder.builds != 1 {
		t.Fatalf("builds = %d, want the deploy to go on to build", h.builder.builds)
	}
}

func TestADeployWithNoSweepStillDeploys(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.d.deps.FoldSweep = nil

	// Act
	_, err := h.d.Deploy(context.Background(), false)

	// Assert
	if err != nil || h.foldSweep.sweeps != 0 {
		t.Fatalf("Deploy = %v with %d sweeps, want a deploy that sweeps nothing", err, h.foldSweep.sweeps)
	}
}

func TestASocketHolderOnAnOlderBuildRestartsBeforeTheDaemonRollsOut(t *testing.T) {
	cases := []struct {
		name     string
		report   func(t *testing.T, h *harness)
		restarts []string
	}{
		{name: "a holder on the fresh build is left alone", report: func(*testing.T, *harness) {}},
		{name: "a holder on an older build restarts", report: func(t *testing.T, h *harness) {
			h.report(t, "sockethold-daemon", 300, hashOf(t, theOld.holder))
		}, restarts: []string{"holder socket-holder-daemon"}},
		{name: "a holder that reports no build restarts", report: func(t *testing.T, h *harness) {
			if err := os.Remove(buildreport.Path(h.reportDir, "sockethold-store")); err != nil {
				t.Fatal(err)
			}
		}, restarts: []string{"holder socket-holder-store"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			tc.report(t, h)
			asksAtRestart := -1
			h.services.atHolder = func() {
				h.rollout.mu.Lock()
				asksAtRestart = len(h.rollout.asks)
				h.rollout.mu.Unlock()
			}

			// Act
			_, err := h.d.Deploy(context.Background(), false)

			// Assert: the holder restarted, if at all, before the daemon's
			// rollout asked anything.
			if err != nil {
				t.Fatalf("Deploy: %v", err)
			}
			var holders []string
			for _, c := range h.services.Calls() {
				if strings.HasPrefix(c, "holder ") {
					holders = append(holders, c)
				}
			}
			if !reflect.DeepEqual(holders, tc.restarts) {
				t.Fatalf("holder restarts = %v, want %v", holders, tc.restarts)
			}
			if tc.restarts != nil && asksAtRestart != 0 {
				t.Fatalf("the rollout had asked %d time(s) when the holder restarted, want none", asksAtRestart)
			}
			if got := readFile(t, h.live.CacheBinPath(HolderBinary)); got != theFresh.holder {
				t.Fatalf("installed %s = %q, want the fresh build", HolderBinary, got)
			}
		})
	}
}

func TestAFailedSocketHolderRestartStopsTheDeployBeforeTheRollout(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.report(t, "sockethold-daemon", 300, hashOf(t, theOld.holder))
	h.services.holderErr = errors.New("launchctl kickstart exited 1")

	// Act
	_, err := h.d.Deploy(context.Background(), false)

	// Assert
	var failed *ServiceRestartFailed
	if !errors.As(err, &failed) || failed.Component != ComponentDaemon {
		t.Fatalf("Deploy = %v, want the holder's restart failure", err)
	}
	h.rollout.mu.Lock()
	asks := len(h.rollout.asks)
	h.rollout.mu.Unlock()
	if asks != 0 {
		t.Fatalf("the rollout was asked %d time(s), want none after the holder failed", asks)
	}
	if _, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: dlog.LevelError, Message: "the socket holder's restart failed; the daemon is not rolled out"}); !ok {
		t.Fatalf("records = %+v, want the failure at ERROR", h.log.Records())
	}
}
