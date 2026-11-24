// webapplayer_e2e_test.go — WEBAPP-LAYER-SPEC.md.
//
// THE WEBAPP IN THE LOOP. Every other file in this package dials the daemon's
// Connect API directly, one layer below the webapp (SPEC.md section B,
// "Frontends: none"). This file closes that gap without duplicating a byte of
// bring-up: it builds an ordinary World — real store, real sidecar, real
// agent-repld, real shim over the fake SDK, scripted fake git — and then
// hands that daemon's own loopback address to a vitest child process which
// mounts the REAL webapp in jsdom against it.
//
// The chain under test is therefore:
//
//	fake SDK -> real shim -> real store + real sidecar -> real agent-repld
//	         -> real webapp
//
// WHY THE LIFECYCLE STAYS HERE: bring-up in this suite is not "start four
// processes", it is NewWorld — the store's short socket and log, the one spool
// root the fake SDK writes and the sidecar globs, the forced
// ShimNode/ShimMain/StoreSocket, the real services reporting their builds
// into the daemon's lock dir,
// resolveConfigRoots' symlink resolution, assertOneSpoolRoot,
// preserveLogsOnFailure, and a LIFO teardown whose order is load-bearing. Two
// of those invariants fail SILENTLY when broken (SPEC.md section B). A
// TypeScript bring-up would have to re-derive all of it; this file adds zero
// process management on the TypeScript side. The rejected alternatives are
// recorded in WEBAPP-LAYER-SPEC.md section A.
package e2e

import (
	"sort"
	"testing"

	"agentrepl/e2e/kit/stack"
	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"connectrpc.com/connect"

	"agent-repld/integration/harness"
)

// ONE GO TEST PER AREA (project-lead ruling). Each area gets its OWN world,
// so its artifacts are preserved on its own failure, a red run names the area,
// and the areas parallelize; the ~2.7s world cost per area is acceptable at
// ten areas. Each area's Go test names exactly one vitest file, and that
// file is the area's scenario list from WEBAPP-LAYER-SPEC.md section F.

// wlClientLogOperation is the operation the client-log area's page plants.
//
// SHARED WITH THE PAGE BY VALUE: `OPERATION` in
// `webapp/test/webapp-layer/client-log.layer.test.ts` is this same string. It
// is how this driver finds the ONE record the page emitted on purpose among
// the boot's own fifty-odd.
const wlClientLogOperation = "webapp-layer.client-log-round-trip"

// testWebappLayerClientLog is the five-runtime JOIN the logging contract
// promises, asserted where it is real: a record emitted in the browser lands
// in the daemon's own `webapp.log` carrying the identity that ties it to the
// daemon's records and the shim's for the same session.
//
// WHY THIS AREA EXISTS AT ALL. `ClientLog` is the console-less client's only
// evidence path — a page inside an xwidget has no console anybody reads — and
// until this area the round trip was proven only against the FAKE daemon. The
// layer's own page forwarded nothing, because the layer installed a no-op
// sink; so the one place that runs the real page against the real daemon could
// not prove the thing the whole contract exists for. The page now mounts with
// production's own sink (its cost per file is measured in
// `webapp/test/webapp-layer/setup.ts`), plants one record, and this side reads
// the files back.
//
// THE ASSERTION IS A JOIN, NOT AN ECHO. The page's own file-level assertions
// (that its record arrived, in the webapp runtime, with both identities
// promoted) are the half a browser can see. What only this side can see is
// that those identities are the SAME strings the other runtimes wrote for the
// same session — which is what makes one incident readable across five logs.
//
// Seam: webapp -> daemon, a browser record joined to the daemon's and the shim's records for its session
func testWebappLayerClientLog(t *testing.T) {
	w, ws := stack.WlDriveAreaWorld(t, "client-log.layer.test.ts")

	// The record the page planted, in the workspace's own webapp sink. The
	// page already waited for it, so this reads rather than races; awaiting it
	// keeps the failure legible if the sink ever moved.
	planted := w.Daemon.AwaitLogRecord(harness.ClientLogPath(ws), "the page's planted client-log record",
		func(r harness.LogRecord) bool { return r.Operation == wlClientLogOperation })

	if planted.Runtime != "webapp" {
		t.Errorf("the forwarded record's runtime = %q, want webapp: a forwarded record keeps the SENDING runtime's name", planted.Runtime)
	}
	// A FORWARDED RECORD CARRIES NO PID, deliberately: stamping the daemon's
	// would attribute a browser's record to the daemon process.
	if planted.PID != 0 {
		t.Errorf("the forwarded record carries pid %d, want none: the daemon's pid would misattribute a browser's record", planted.PID)
	}
	if planted.AgentReplSessionID == "" || planted.ClaudeSessionID == "" {
		t.Fatalf("the forwarded record carries agent_repl_session_id=%q claude_session_id=%q, want both: without them the record joins to the workspace and no further",
			planted.AgentReplSessionID, planted.ClaudeSessionID)
	}
	// THE WORKSPACE ID IS THE DAEMON'S OWN, NOT THE REF'S. ClientLog stamps
	// the routing identity the daemon resolved the record's workspace to
	// (internal/dlog/surfaces.go merges its own dir and id over whatever the
	// client sent), which is a different string from the ref's id — so the
	// assertion that means something is that it joins to the daemon's records
	// for this workspace, not that it echoes the ref.
	wlRequireJoin(t, "the daemon's own records", w.Daemon.WorkspaceLog(ws.GetDir(), "daemon"),
		"workspace_id", planted.WorkspaceID,
		func(r harness.LogRecord) string { return r.WorkspaceID })

	// THE JOIN, ARM ONE: the daemon's OWN records for this session. The
	// workspace's session-scoped logger binds agent_repl_session_id
	// (internal/workspace/sessions.go), so the page's id must be a string the
	// daemon itself wrote.
	wlRequireJoin(t, "the daemon's own records", w.Daemon.WorkspaceLog(ws.GetDir(), "daemon"),
		"agent_repl_session_id", planted.AgentReplSessionID,
		func(r harness.LogRecord) string { return r.AgentReplSessionID })

	// THE JOIN, ARM TWO: the shim's records for the same vendor conversation.
	// The shim binds claude_session_id once the vendor names the session, so
	// the page's id must be a string the shim itself wrote — the browser and
	// the process talking to the vendor agreeing on one conversation.
	wlRequireJoin(t, "the shim's records", w.Daemon.WorkspaceLog(ws.GetDir(), "shim"),
		"claude_session_id", planted.ClaudeSessionID,
		func(r harness.LogRecord) string { return r.ClaudeSessionID })
}

// wlRequireJoin fails unless some record in `records` carries `want` in the
// field `field` reads, naming what it did see so a mismatch is read here
// rather than diffed by hand out of two log files.
func wlRequireJoin(t *testing.T, whose string, records []harness.LogRecord, field, want string, idOf func(harness.LogRecord) string) {
	t.Helper()
	seen := map[string]bool{}
	for _, r := range records {
		if got := idOf(r); got != "" {
			if got == want {
				return
			}
			seen[got] = true
		}
	}
	values := make([]string, 0, len(seen))
	for v := range seen {
		values = append(values, v)
	}
	sort.Strings(values)
	t.Errorf("the page's %s=%q appears in no record of %s (%d records, ids seen: %v); the webapp record does not join to them",
		field, want, whose, len(records), values)
}

// testWebappLayerCards is section F4: permission and question cards.
//
// Seam: webapp -> daemon, the permission and question cards' answers from the real page
func testWebappLayerCards(t *testing.T) {
	stack.WlDriveArea(t, "cards.layer.test.ts")
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

// testWebappLayerRestartHandover is section F9 #39: THE RESTART HANDOVER, the
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
//     passing merge gate), adTriggerDeploy (a real landed commit whose one
//     deploy finds the daemon stale and hands it over blue-green), adDial (reaching the
//     successor at the address the announcement carried). There is no second
//     way to replace a daemon in this package.
//   - THE HOST ADOPT IS ISSUED FROM HERE, playing Emacs, the external system
//     this suite mocks; THE WEB ADOPT IS THE PAGE'S OWN. No handover reloads
//     a page (owner ruling, 2026-11-22): on `transferred` the mounted page
//     adopts its workspace in place through the shared address, which the
//     incumbent forwards to the successor, keeps its streams until the
//     incumbent ends its page stream at the promotion, and attaches once at
//     the successor (webapp/src/lifecycle/lifecycle.ts `adoptInPlace`,
//     docs/protobuf-design/feed-resume.md). Its adopt is the
//     rendezvous's web participant, so the host adopt below succeeds only
//     together with it.
//
// THE DECLARED WARNING is this test's own subject, as in the adoption area:
// the standing unlanded-arm refusal record is the evidence of the refusal a
// handover deliberately provokes.
//
// Seam: successor daemon -> webapp, the mounted page carried across a daemon handover
func testWebappLayerRestartHandover(t *testing.T) {
	stack.WlHoldAreaSlot(t)
	npm := stack.WlRequireNPM(t)
	webappDir := stack.WlRequireWebappDeps(t)

	// The handover world, verbatim from the adoption area: its daemon's own
	// checkout is a fake-git repository with a passing merge gate, and its ONE
	// context carries the whole chain on HandoverChainTimeout.
	selfRepo, w := adSelfRepoWorld(t)
	w.ExpectWarnings("daemon.refusal.unlanded_arm.standing")

	ws := w.NewWorkspace(t)

	// THE PAGE ON SCREEN IS THE SELECTION, as Emacs shows only the selected
	// workspace's page: selected, the workspace stays visible through the
	// deploy's fold sweep (which never folds over the selection), so the
	// successor serves its page (wsm.Visibility).
	if resp, err := w.Client().SelectWorkspace(w.Ctx(),
		connect.NewRequest(&agentreplv1.SelectWorkspaceRequest{Workspace: ws})); err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("SelectWorkspace = (%v, %v), want success", resp, err)
	}

	// THE HOST PARTICIPANT, WHICH EMACS WOULD BE (see wlDriveArea): the
	// footer's connectivity truth is the PAIR of participants, and this hop is
	// also the host half of the adoption rendezvous below.
	host := w.WatchHost(ws)
	defer host.Close()
	harness.AwaitNext(t, w.Ctx(), host, "the fresh host push")

	daemonStream := w.WatchDaemonStream()
	defer daemonStream.Close()

	child, err := stack.WlStartVitest(t, npm, webappDir, "restart-handover.layer.test.ts",
		stack.WlChildEnv(t, w, ws))
	if err != nil {
		t.Fatalf("e2e/webapp-layer: starting the handover child: %v", err)
	}
	defer child.Kill()

	// Arrange: the rendezvous. The page is mounted and its streams stand.
	//
	// THE MARKER WAITS ON THE VITEST CHILD'S OWN BOOT, not on a daemon step:
	// npm, node, vite's transform and collect, then the page's mount. Bounded
	// by one daemon wait (HandoverChainTimeout, 15s) it failed under a full
	// suite run while the child was still booting -- its siblings' walls that
	// run reached 17s -- so it takes the bound of a cold child's boot.
	w.Daemon.AwaitLogRecordWithin(harness.ClientLogPath(ws),
		"the page's own mounted marker, logged through ClientLog", stack.WebappLayerColdMountTimeout,
		func(r harness.LogRecord) bool { return r.Operation == wlPageMountedOperation })

	// Act: a real landed commit on the daemon's own checkout fires the real
	// self-merge rollout.
	adTriggerDeploy(t, w, selfRepo, harness.DeployStaleDaemon)
	announced := harness.AwaitView(t, w.Ctx(), daemonStream, "shutdown_announced",
		func(r *agentreplv1.WatchDaemonResponse) bool { return r.GetShutdownAnnounced() != nil },
	).GetShutdownAnnounced()
	addr := announced.GetAddress()
	if addr == "" {
		t.Fatal("shutdown_announced.address is unset, want the successor's address for a handover")
	}
	successor := adDial(addr)

	// Act: the host half of the rendezvous on the successor. Its web half is
	// the mounted page's own in-place adoption, and "EVERY EXPECTED
	// PARTICIPANT SUCCEEDS TOGETHER" (daemon/internal/rollout/adopt.go), so
	// this returning is the page having adopted too.
	adopted := make(chan error, 1)
	go func() {
		_, err := successor.AdoptHostWorkspace(w.Ctx(),
			connect.NewRequest(&agentreplv1.AdoptHostWorkspaceRequest{Workspace: ws}))
		adopted <- err
	}()

	// Assert (the Go half): the transfer really happened and names the
	// successor — the barrier the page's own `transferred` push rides on.
	harness.AwaitView(t, w.Ctx(), host, "transferred",
		func(r *agentreplv1.WatchHostWorkspaceResponse) bool { return r.GetTransferred() != nil })
	if code := w.AwaitExit(); code != 0 {
		t.Fatalf("the incumbent's exit code = %d, want an orderly 0 after the handover", code)
	}
	adAwaitAddrFileChange(t, w.Daemon, addr)

	if err := <-adopted; err != nil {
		t.Fatalf("adopting the workspace's host on the successor = error %v, want a success beside the page's own adopt", err)
	}

	// Assert (the page's half): every assertion in
	// restart-handover.layer.test.ts — the page re-homed in place, never quiet
	// and never reloaded, its views drawn again from the successor.
	if err := child.WaitFor(stack.WebappLayerHandoverTimeout); err != nil {
		t.Fatalf("e2e/webapp-layer: restart-handover.layer.test.ts failed: %v", err)
	}
}
