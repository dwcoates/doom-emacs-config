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

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"connectrpc.com/connect"

	"claude-repld/integration/harness"
)

// WebappLayerTimeout bounds the vitest child process end to end.
//
// MEASURED, then set at ~3x the observed max, the same way every other bound
// in this suite was derived.
//
// Two full runs of all the areas (2026-09-03), per-area vitest child
// durations: 1.21s, 1.30s, 1.38s, 1.44s, 1.48s, 1.50s, 1.74s, 3.16s — the
// slowest is the feed-families area (22 real turns in one child). The Go
// tests wrapping them ran 1.58-3.77s including a full world bring-up each.
// 3x the 3.16s max is ~9.5s, so 10s.
//
// AN EARLIER DRAFT CARRIED 60s, THEN 20s, on the theory that a cold Vite
// transform dominated and could not be measured. That theory was wrong: the
// transform is ~400ms on every run, cache or no cache (clearing
// node_modules/.vite changes nothing — vitest transforms sources per run),
// so there is no hidden cold-start term to leave headroom for.
//
// It bounds a HANG, not a synchronization wait: nothing here sleeps, the
// child's exit is awaited on its own channel, and the child's own per-site
// budgets (BOOT_BUDGET_MS and TURN_BUDGET_MS in test/webapp-layer/drive.ts)
// fail a stuck assertion long before this fires.
//
// THE CONSTANT SAID 300s UNTIL 2026-09-04, WHICH THE COMMENT ABOVE AND
// WEBAPP-LAYER-SPEC.md §E BOTH CONTRADICTED — reported as PERF-SPEC.md §H
// finding 2 and fixed here to the value both of them state and the
// measurement above supports. 300s is not a hang bound: it is thirty times
// the slowest observed child, so a genuinely wedged area would have held its
// world, its slot and its four processes for five minutes before saying so.
//
// AN AREA WHOSE CHILD IS STRUCTURALLY LONGER THAN A FUNCTIONAL ONE DOES NOT
// RELAX THIS CONSTANT; it passes its own bound to wlChild.WaitFor. The perf
// area is the first such caller (WebappLayerPerfTimeout).
const WebappLayerTimeout = 10 * time.Second

// WebappLayerHandoverTimeout bounds the RESTART-HANDOVER area's child, which
// is structurally longer than a functional area's by two whole process
// lifecycles: a merge landing, the rollout trigger, the incumbent re-execing
// itself, a second real claude-repld's full boot, the adoption rendezvous, and
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

// TestWebappLayerParticipantHoldOutlivesTheWaitBound pins the bound the host
// participant hold runs on.
//
// The hold exists so the footer reads CONNECTED for the whole vitest child
// (internal/resolve/footer/status.go refuses `connected` while either
// participant stream is down). Bounding it by harness.DefaultTimeout — which
// is what `Daemon.WatchHost` does, correctly, for a WAIT — expired the hold
// five seconds into a child that runs far longer: the footer dropped to
// `disconnected`, the real page closed its composer gate, and every later
// submission was swallowed by the app itself with nothing reaching the daemon.
// A hold is bounded by the thing it is held FOR, so this asserts the two
// bounds are the ones the arrangement needs and not each other.
func TestWebappLayerParticipantHoldOutlivesTheWaitBound(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name  string
		bound time.Duration
		floor time.Duration
	}{
		{
			name:  "a functional area's bound outlives the harness's wait bound",
			bound: WebappLayerTimeout,
			floor: harness.DefaultTimeout,
		},
		{
			// ONE BOUND PER AREA, since 2026-09-04: WebappLayerTimeout used to
			// carry 300s so that the handover area fitted under it too, which
			// made every OTHER area's hang bound thirty times its slowest
			// observed child (PERF-SPEC.md §H finding 2). The areas whose child
			// is structurally longer now pass their own bound to
			// wlChild.WaitFor, and each such bound is pinned here.
			name:  "the handover area's bound outlives the handover chain it drives",
			bound: WebappLayerHandoverTimeout,
			floor: harness.HandoverChainTimeout,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act: the constants themselves are the subject.
			got := tc.bound

			// Assert.
			if got <= tc.floor {
				t.Errorf("the participant hold's bound = %s, want more than %s", got, tc.floor)
			}
		})
	}
}

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
// ten areas. Each area's Go test names exactly one vitest file, and that
// file is the area's scenario list from WEBAPP-LAYER-SPEC.md section F.

// TestWebappLayer is section F1: proof of life.
func TestWebappLayer(t *testing.T) {
	t.Parallel()
	wlDriveArea(t, "proof-of-life.layer.test.ts")
}

// TestWebappLayerFeedFamilies is section F2: one drawn row family per test.
//
// THE DECLARED FAULT IS THE AREA'S OWN SUBJECT: the file's last test drives
// `!query-eof`, and a vendor query dying under a turn is exactly what
// `daemon.health.open_fault` records.
func TestWebappLayerFeedFamilies(t *testing.T) {
	t.Parallel()
	// The query-death records beside it are the SAME act: the death is
	// routed at the watcher and drawn as the turn's terminal.
	wlDriveArea(t, "feed-families.layer.test.ts", "daemon.health.open_fault",
		"daemon.sessionwatcher.query_died", "daemon.feed.query_died")
}

// TestWebappLayerQueryDeath is section F2's SECOND query-death cause arm.
//
// ITS OWN AREA, AND THE REASON IS STRUCTURAL: a dead vendor query ends its
// session (every later StartTurn is refused `query_dead`), and one area file
// drives one session, so one file can exercise exactly one query death.
// `!query-eof` is feed-families' last test; `!query-fail` is this one's only
// test.
func TestWebappLayerQueryDeath(t *testing.T) {
	t.Parallel()
	// THE DECLARED FAULT IS THIS AREA'S WHOLE SUBJECT: the vendor query dies,
	// and `daemon.health.open_fault` is the daemon recording exactly that.
	wlDriveArea(t, "query-death.layer.test.ts", "daemon.health.open_fault",
		"daemon.sessionwatcher.query_died", "daemon.feed.query_died")
}

// TestWebappLayerSubfeeds is section F3: sub-feed open/collapse lifecycle.
func TestWebappLayerSubfeeds(t *testing.T) {
	t.Parallel()
	wlDriveArea(t, "subfeeds.layer.test.ts")
}

// TestWebappLayerCards is section F4: permission and question cards.
func TestWebappLayerCards(t *testing.T) {
	t.Parallel()
	wlDriveArea(t, "cards.layer.test.ts")
}

// TestWebappLayerSurfaces is section F5: footer and topbar surfaces.
func TestWebappLayerSurfaces(t *testing.T) {
	t.Parallel()
	// The area drives `unmodeled` to prove the warning dropdown is an
	// unmodeled tool's only home, so the watcher's own record of that
	// activity is this file's evidence, not a surprise.
	wlDriveArea(t, "surfaces.layer.test.ts", "daemon.sessionwatcher.unmodeled_activity")
}

// TestWebappLayerPanels is section F6: daemon-answered command panels.
func TestWebappLayerPanels(t *testing.T) {
	t.Parallel()
	wlDriveArea(t, "panels.layer.test.ts")
}

// TestWebappLayerRefusals is section F8: refusal wording and placement.
func TestWebappLayerRefusals(t *testing.T) {
	t.Parallel()
	wlDriveArea(t, "refusals.layer.test.ts")
}

// TestWebappLayerRoster is section F9: tray, sidebar and lifecycle banner.
//
// THE TWO DECLARED FAULTS ARE THIS AREA'S OWN SUBJECTS, not incidental noise:
//
//   - `daemon.wsm.acquire_lease: refused the write` — the tray tests HOLD a
//     prompt on purpose, by submitting behind a turn that parks. The refused
//     lease acquisition IS the hold.
//   - `daemon.drain.fire` (a workspace's lease is already held) — the drain
//     test SCHEDULES a real drain through the app's own verb, and the daemon
//     acts on that standing schedule while a lease is still held.
//
// Declaring them is what keeps the area honest: an undeclared warning fails
// the run, so a NEW fault here cannot hide behind these two.
func TestWebappLayerRoster(t *testing.T) {
	t.Parallel()
	wlDriveArea(t, "roster.layer.test.ts", "daemon.wsm.acquire_lease", "daemon.drain.fire")
}

// TestWebappLayerMergeTabs is section F7: the merge bubble's tab strip.
//
// THIS AREA NEEDS A DIFFERENT WORLD from every other one: a merge needs a
// REPOSITORY the daemon knows and a CHILD workspace to merge into it, so the
// page is addressed to the child rather than to a plain registered worktree.
// The scripted fake git (harness.NewRepo plus a test-all script) is the only
// git involved — no real repository, exactly as SPEC.md section B requires.
func TestWebappLayerMergeTabs(t *testing.T) {
	t.Parallel()
	wlHoldAreaSlot(t)
	npm := wlRequireNPM(t)
	webappDir := wlRequireWebappDeps(t)

	repo := harness.NewRepo(t)
	// THE MERGE TEST GATE. The self-repo method runs `bash bin/test-all.sh` in
	// the merge target worktree; these scripted repos have none, so without a
	// provided gate the merge exits 127 and NEVER reaches a terminal — which
	// showed up here as a merge bubble that sometimes never appeared at all.
	// Provided exactly as daemon/integration/merge_test.go and the merge-queue
	// area do, and named through the env var the daemon reads.
	script := harness.NewTestAllScript(t, repo.Dir)
	script.SetExitCode(0)
	script.SetStdout("webapp layer: passed in 1s\n")

	w := NewWorld(t, WorldOpts{DaemonOpts: harness.Opts{
		SelfRepo: repo.Dir,
		ExtraEnv: []string{"AGENT_REPL_TEST_ALL_SCRIPT=" + script.Path},
	}})
	repoRef := wlRepositoryRef(t, w, repo)
	ws := wlCreateChild(t, w, repoRef, "wl-merge")

	// A SCRIPTED CONFLICT, so the merge PARKS instead of landing.
	//
	// A landed merge tears the child workspace's worktree down and releases
	// its lease — which would pull the page's own workspace out from under it
	// mid-file. A conflict keeps the workspace alive, opens the merge tab, and
	// gives the strip its agentic tabs (conflicts, and PARKED), which is what
	// section F7 is about. The conflict is scripted into the FAKE git's state;
	// no real repository is involved.
	repo.ScriptConflict(repo.Dir, filepath.Base(ws.GetDir()), "conflict.txt")
	// The parked conflict is scripted above ON PURPOSE, so the merge parking
	// on it is this area's own subject rather than an unexplained warning.
	w.ExpectWarnings("daemon.gitclient.merge_no_ff", "daemon.merge.merge_tab", "daemon.merge.conflicts")

	host := w.Daemon.WatchHost(ws)
	defer host.Close()

	if err := wlRunVitest(t, npm, webappDir, "merge-tabs.layer.test.ts",
		wlChildEnv(t, w, ws)); err != nil {
		t.Fatalf("e2e/webapp-layer: merge-tabs.layer.test.ts failed: %v", err)
	}

	w.RequireNoUnexpectedExit(t)
}

// wlPageMountedOperation is the operation the vitest child logs through the
// daemon's own ClientLog rpc once its page is mounted and its streams are
// standing. IT IS THE RENDEZVOUS: this Go side may not replace the daemon
// before that record exists, or the `transferred` push would have no
// subscriber to reach.
//
// Matched VERBATIM against MOUNTED_MARKER in
// `webapp/test/webapp-layer/restart-handover.layer.test.ts`; the two constants
// are documented on each other and move together.
const wlPageMountedOperation = "webapp-layer.handover.page-mounted"

// TestWebappLayerRestartHandover is section F9 #39: THE RESTART HANDOVER, the
// one area whose Go side acts WHILE the page is mounted.
//
// THE SHAPE, and why it is this shape:
//
//   - The child is STARTED, not awaited: the page must be standing when the
//     daemon under it is replaced. It is awaited at the end, so a red page is
//     still this test's failure.
//   - The rendezvous is the child's own ClientLog marker, awaited with
//     AwaitLogRecord on the workspace's `webapp` sink (the sink ClientLog
//     persists a webview's records to, harness.ClientLogPath). No sleep, and
//     no assumption about how long a vitest boot takes.
//   - The handover itself is the SUITE'S EXISTING MACHINERY, reused verbatim
//     from adoption_e2e_test.go: adSelfRepoWorld (a fake-git SelfRepo with a
//     passing merge gate), adTriggerSelfMergeRollout (a real landed commit
//     firing the real blue-green self-rollout), adDial (reaching the
//     successor at the address the announcement carried). There is no second
//     way to replace a daemon in this package.
//   - THE ADOPTS ARE ISSUED FROM HERE, not from the page, and that is the
//     contract: "THE WEBAPP DOES NOT REDIAL (project lead, final) ...
//     re-pointing the webview is Emacs's job" (webapp/src/lifecycle/
//     lifecycle.ts:12-21). Emacs is the external system this suite mocks, so
//     this test plays the lagging client for BOTH hops exactly as
//     TestRefusalOrderingDuringHandover does — concurrently, because "EVERY
//     EXPECTED PARTICIPANT SUCCEEDS TOGETHER" (daemon/internal/rollout/
//     adopt.go:222-251). The page's own web participant is its standing
//     WatchWebWorkspace, which is what then receives `transferred`.
//   - The page's recovery is its OWN fresh boot at the successor's address —
//     the reload Emacs performs — so the successor's AdoptWebWorkspace on the
//     recovered page is issued by the real app.
//
// THE DECLARED WARNING is this test's own subject, as in the adoption area:
// the standing unlanded-arm refusal record is the evidence of the refusal a
// handover deliberately provokes.
func TestWebappLayerRestartHandover(t *testing.T) {
	t.Parallel()
	wlHoldAreaSlot(t)
	npm := wlRequireNPM(t)
	webappDir := wlRequireWebappDeps(t)

	// The handover world, verbatim from the adoption area: its daemon's own
	// checkout is a fake-git repository with a passing merge gate, and its ONE
	// context carries the whole chain on HandoverChainTimeout.
	selfRepo, w := adSelfRepoWorld(t)
	w.ExpectWarnings("daemon.refusal.unlanded_arm.standing")

	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)

	// THE HOST PARTICIPANT, WHICH EMACS WOULD BE (see wlDriveArea): the
	// footer's connectivity truth is the PAIR of participants, and this hop is
	// also the host half of the adoption rendezvous below.
	host := w.WatchHost(ws)
	defer host.Close()
	harness.AwaitNext(t, w.Ctx(), host, "the fresh host push")

	daemonStream := w.WatchDaemonStream()
	defer daemonStream.Close()

	child, err := wlStartVitest(t, npm, webappDir, "restart-handover.layer.test.ts",
		wlChildEnv(t, w, ws))
	if err != nil {
		t.Fatalf("e2e/webapp-layer: starting the handover child: %v", err)
	}
	defer child.Kill()

	// Arrange: the rendezvous. The page is mounted and its streams stand.
	w.Daemon.AwaitLogRecord(harness.ClientLogPath(ws),
		"the page's own mounted marker, logged through ClientLog",
		func(r harness.LogRecord) bool { return r.Operation == wlPageMountedOperation })

	// Act: a real landed commit on the daemon's own checkout fires the real
	// self-merge rollout.
	adTriggerSelfMergeRollout(t, w, selfRepo, adSelfMergeTriggerPath)
	announced := harness.AwaitView(t, w.Ctx(), daemonStream, "shutdown_announced",
		func(r *agentreplv1.WatchDaemonResponse) bool { return r.GetShutdownAnnounced() != nil },
	).GetShutdownAnnounced()
	addr := announced.GetAddress()
	if addr == "" {
		t.Fatal("shutdown_announced.address is unset, want the successor's address for a handover")
	}
	successor := adDial(addr)

	// Act: complete the rendezvous on the successor, CONCURRENTLY — one after
	// the other blocks the first inside the rendezvous until its own context
	// expires (adopt.go:222-251).
	adopts := make(chan error, 2)
	go func() {
		_, err := successor.AdoptHostWorkspace(w.Ctx(),
			connect.NewRequest(&agentreplv1.AdoptHostWorkspaceRequest{Workspace: ws}))
		adopts <- err
	}()
	go func() {
		_, err := successor.AdoptWebWorkspace(w.Ctx(),
			connect.NewRequest(&agentreplv1.AdoptWebWorkspaceRequest{Workspace: ws}))
		adopts <- err
	}()
	for i := 0; i < 2; i++ {
		if err := <-adopts; err != nil {
			t.Fatalf("adopting the workspace on the successor = error %v, want a success (both adopts succeed together)", err)
		}
	}

	// Act: KEEP THE HOST HOP ARRIVING, because the page is about to reload.
	//
	// A fresh page adopts at boot (lifecycle.ts:23-28), so the recovered page
	// issues its OWN AdoptWebWorkspace against the successor — and the
	// successor RE-ARMS the rendezvous from the stand-down intent manifest
	// after the first arming was already satisfied (observed here:
	// `daemon.rollout.join` "armed the adopt rendezvous from the intent
	// manifest" resets host_called/web_called), so that second web caller
	// waits for a host participant all over again. In production the host is
	// Emacs, which re-points the webview AND re-adopts the host hop; this
	// suite mocks Emacs, so the host half of that reload is issued here. The
	// loop keeps a host caller arriving until one is accepted, so whenever the
	// recovered page's web adopt lands there is a partner in the rendezvous.
	// IT KEEPS CALLING FOR AS LONG AS THE PAGE IS RUNNING, rather than
	// stopping at its first success: a host adopt that returns "the workspace
	// is already adopted; the call succeeds at once" satisfies nothing for a
	// rendezvous that is re-armed a millisecond later, which is exactly the
	// order observed (host accepted at once, the manifest re-armed after it,
	// then the page's web adopt waiting alone until it gave up).
	stopReadopting := make(chan struct{})
	readopted := make(chan int, 1)
	go func() {
		accepted := 0
		for {
			if _, err := successor.AdoptHostWorkspace(w.Ctx(),
				connect.NewRequest(&agentreplv1.AdoptHostWorkspaceRequest{Workspace: ws})); err == nil {
				accepted++
			}
			select {
			case <-time.After(pollInterval):
			case <-stopReadopting:
				readopted <- accepted
				return
			case <-w.Ctx().Done():
				readopted <- accepted
				return
			}
		}
	}()

	// Assert (the Go half): the transfer really happened and names the
	// successor — the barrier the page's own `transferred` push rides on.
	harness.AwaitView(t, w.Ctx(), host, "transferred",
		func(r *agentreplv1.WatchHostWorkspaceResponse) bool { return r.GetTransferred() != nil })
	if code := w.AwaitExit(); code != 0 {
		t.Fatalf("the incumbent's exit code = %d, want an orderly 0 after the handover", code)
	}
	adAwaitAddrFileChange(t, w.Daemon, addr)

	// Assert (the page's half): every assertion in
	// restart-handover.layer.test.ts — the moved notice naming this same
	// successor, the quiesced page's local refusal, and the fresh page's own
	// boot and adoption at the new address.
	waitErr := child.WaitFor(WebappLayerHandoverTimeout)
	close(stopReadopting)
	accepted := <-readopted
	if waitErr != nil {
		t.Fatalf("e2e/webapp-layer: restart-handover.layer.test.ts failed: %v", waitErr)
	}

	// And the host half of that reload really was accepted: a page recovering
	// beside a host hop that never re-adopted would be a different scenario.
	if accepted == 0 {
		t.Fatal("no AdoptHostWorkspace call on the successor was ever accepted, so the page's own recovery adopt had no partner in the rendezvous")
	}
}

// wlRepositoryRef registers a repository's main worktree and reads the
// daemon-minted RepositoryRef back off the roster stream.
func wlRepositoryRef(t *testing.T, w *World, repo *harness.Repo) *workspacev1.RepositoryRef {
	t.Helper()
	harness.Register(t, w.Daemon, repo.Dir)
	roster := w.WatchRoster()
	defer roster.Close()
	got := harness.AwaitView(t, w.Ctx(), roster, "the repository's roster section",
		func(r *frontendv1.WorkspaceRoster) bool { return wlFindRepo(r, repo.Dir) != nil })
	ref := wlFindRepo(got, repo.Dir)
	if ref == nil {
		t.Fatalf("e2e/webapp-layer: no roster repository section for %s", repo.Dir)
	}
	return ref
}

// wlFindRepo finds a repository section by its worktree dir. A repository is
// keyed by its COMMON DIR, which for an ordinary checkout is
// `<worktree>/.git`, so both spellings are accepted.
func wlFindRepo(r *frontendv1.WorkspaceRoster, dir string) *workspacev1.RepositoryRef {
	for _, section := range r.GetRepository().GetSections() {
		switch section.GetKey().GetRepository().GetDir() {
		case dir, filepath.Join(dir, ".git"):
			return section.GetKey().GetRepository()
		}
	}
	return nil
}

// wlCreateChild creates a top-level child workspace of a repository, which is
// what a merge merges.
func wlCreateChild(t *testing.T, w *World, repoRef *workspacev1.RepositoryRef, name string) *workspacev1.WorkspaceRef {
	t.Helper()
	resp, err := w.Client().CreateWorkspace(w.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Repository: repoRef,
		Form: &agentreplv1.CreateWorkspaceRequest_Standard{Standard: &agentreplv1.CreateWorkspaceStandard{
			Name: &name,
		}},
	}))
	if err != nil {
		t.Fatalf("CreateWorkspace(%s) = error %v, want a success", name, err)
	}
	ws := resp.Msg.GetSuccess().GetWorkspace()
	if ws.GetId() == "" {
		t.Fatalf("CreateWorkspace(%s) = %v, want a success carrying a workspace ref", name, resp.Msg)
	}
	return ws
}

// wlDriveArea builds one world and drives one of the layer's vitest files
// against its real daemon.
func wlDriveArea(t *testing.T, vitestFile string, expectWarnings ...string) {
	t.Helper()
	wlHoldAreaSlot(t)
	npm := wlRequireNPM(t)
	webappDir := wlRequireWebappDeps(t)

	w := NewWorld(t, WorldOpts{})
	if len(expectWarnings) > 0 {
		w.ExpectWarnings(expectWarnings...)
	}
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
	holdCtx, releaseHold := context.WithTimeout(context.Background(), WebappLayerTimeout)
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

	env := wlChildEnv(t, w, ws)

	if err := wlRunVitest(t, npm, webappDir, vitestFile, env); err != nil {
		t.Fatalf("e2e/webapp-layer: %s failed: %v", vitestFile, err)
	}

	w.RequireNoUnexpectedExit(t)
}

// wlChildEnv is the environment one vitest child is handed: the gate, the
// daemon's own published address, and the workspace the page is addressed to.
func wlChildEnv(t *testing.T, w *World, ws *workspacev1.WorkspaceRef) []string {
	t.Helper()
	if w.Addr == "" {
		t.Fatal("e2e/webapp-layer: the daemon published no address to hand the webapp")
	}
	return append(os.Environ(),
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
	child, err := wlStartVitest(t, npm, webappDir, vitestFile, env)
	if err != nil {
		return err
	}
	return child.Wait()
}

// wlChild is a vitest child still running, for the ONE area whose Go side must
// act WHILE the page is mounted (§F9 #39, the restart handover). Every other
// area starts the child and immediately waits, which is wlRunVitest.
type wlChild struct {
	t        *testing.T
	cmd      *exec.Cmd
	done     chan error
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
// Sized at 2 from measurement on a 16-core host: one area alone runs in
// 2.9-7.4s (its vitest child is 1.2-3.2s of that — WebappLayerTimeout's own
// note), two together stay inside that spread, and the Go areas' own
// DefaultTimeout waits keep their measured headroom.
const wlMaxConcurrentAreas = 3

var wlAreaSlots = make(chan struct{}, wlMaxConcurrentAreas)

// wlHoldAreaSlot blocks until this area may run, and gives the slot back at
// test cleanup. Registered before anything else the area builds, so a queued
// area holds nothing but its place in line.
func wlHoldAreaSlot(t *testing.T) {
	t.Helper()
	wlAreaSlots <- struct{}{}
	t.Cleanup(func() { <-wlAreaSlots })
}

// wlStartVitest starts the layer's vitest project as a child of this test and
// answers the running child, its output already being mirrored into the test
// log.
func wlStartVitest(t *testing.T, npm, webappDir, vitestFile string, env []string) (*wlChild, error) {
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

	child := &wlChild{t: t, cmd: cmd, done: make(chan error, 1)}
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
	select {
	case waitErr := <-c.done:
		if waitErr == nil {
			return nil
		}
		tail.Lock()
		defer tail.Unlock()
		return fmt.Errorf("vitest exited %v; last output:\n%s", waitErr, strings.Join(tail.lines, "\n"))
	case <-time.After(bound):
		// The child is killed, not abandoned: a leaked vitest holds this
		// daemon's streams open and the world's teardown would then observe
		// state the test never caused. THE WHOLE GROUP GOES, not the npm
		// wrapper alone — see killTree.
		c.killTree()
		tail.Lock()
		defer tail.Unlock()
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
	err := syscall.Kill(-c.cmd.Process.Pid, syscall.SIGKILL)
	switch {
	case err == nil, errors.Is(err, syscall.ESRCH):
		return
	default:
		c.t.Errorf("killing the vitest process group %d: %v (its worker pool may still be running)", c.cmd.Process.Pid, err)
	}
}

// wlTailLines is how much of the child's output a failure quotes inline. The
// full stream is already in the test log via t.Logf; this is the excerpt that
// rides the failure message itself.
const wlTailLines = 40

// TestWlKillTreeReapsAGrandchild pins the reason wlStartVitest puts its child
// in its own process group.
//
// `npm run` is a wrapper: the process the Go side holds is not the one doing
// the work, and vitest's own worker pool is a further generation down. A kill
// aimed at cmd.Process alone left every worker alive and reparented to init,
// spinning on the box for the rest of the suite. This drives the exact shape
// with a shell standing in for npm: the parent exits immediately, and only a
// group-wide kill can reach the grandchild it left behind.
func TestWlKillTreeReapsAGrandchild(t *testing.T) {
	t.Parallel()
	// Arrange: a shell that spawns a long-lived grandchild, writes its pid,
	// and waits — the npm/vitest shape.
	pidFile := filepath.Join(t.TempDir(), "grandchild.pid")
	cmd := exec.Command("/bin/sh", "-c",
		fmt.Sprintf("sleep 600 & echo $! > %s; wait", pidFile))
	cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
	if err := cmd.Start(); err != nil {
		t.Fatalf("start the stand-in child: %v", err)
	}
	child := &wlChild{t: t, cmd: cmd, done: make(chan error, 1)}
	go func() { child.done <- cmd.Wait() }()

	grandchild := wlAwaitPidFile(t, pidFile)
	if err := syscall.Kill(grandchild, 0); err != nil {
		t.Fatalf("the grandchild %d is not running before the kill: %v", grandchild, err)
	}

	// Act.
	child.killTree()

	// Assert: the grandchild is gone, not merely its parent.
	deadline := time.Now().Add(DefaultTimeout)
	for {
		if err := syscall.Kill(grandchild, 0); errors.Is(err, syscall.ESRCH) {
			return
		}
		if time.Now().After(deadline) {
			_ = syscall.Kill(grandchild, syscall.SIGKILL)
			t.Fatalf("the grandchild %d survived killTree for %s; the kill reached the parent only", grandchild, DefaultTimeout)
		}
		time.Sleep(pollInterval)
	}
}

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
		time.Sleep(pollInterval)
	}
}
